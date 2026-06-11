     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4075RAL
     H*    DESCRIPTION: ADJUDICATION (ALABAMA RULES - 05/19/95)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the Alabama State
     H* Fee Schedule Reimbursement Manual issued on 05/19/95.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provider of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 05/19/95.
     H*****************************************************************
     H*  Created by:  C:05  06/25/95
     H*****************************************************************
     H*  Specialty Codes used in this program:
     H*
     H*  ANES Subroutine
     H*          ANE
     H*          RNA
     H*          CRNA
     H*
     H*  SURG Subroutine
     H*          AS
     H*          LAS
     H******************************************************************
     H*  Revisions:                                                    *
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  06/19/99 C:79 PROJ: AL035
     H*                Modified to work with Provider Bills
     H*                                                               *
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
     H*
     H*  09/04/96   C:59    REQ/PROJ: AL029
     H*                     1. ADD MODIFIER '0' TO CALCULATE PAYMENT AT
     H*                        12.5%
     H*                     2. PERFORM MODIFIER CALCULATIONS ATER
     H*                        MULTIPLE SURGERY REDUCTIONS HAVE BEEN
     H*                        MADE.
     H*
     H*  07/16/96   C:32    REQ/PROJ: AL028                            *
     H*                     1. ADDED ENDING DATE RANGE FOR PROCESSING  *
     H*                        OF RECORDS,(960519)                     *
     H*                     2. ADDED AN EFFECTIVE DATE TO MODIFIER     *
     H*                        PROCESSING OF: D1-D5,T1-T3,L1-L2,H1-H4, *
     H*                        ET,CT (DUE TO RATE CHG ON EFF DATE 960215)
     H*                     3. DDS - REMOVED USE OF PR92N4 AS DENTAL    MINUTES
     H*                        AMT DENTAL CODES ARE NOW CODED          RSEMENT:
     H*                        'Dnnnn' W AN MRA
     H*
     H*  09/05/95   C:32    REQ/PROJ: AL025                            *
     H*                     1. ADD COMPILE TIME ARRAY TO HOLD MRA AMTS *
     H*                        FOR CHIROPRATIC CPT-4 PRIOR TO 950801   *
     H*                     2. ADD COMPILE TIME ARRAY TO HOLD MRA AMTS *
     H*                        FOR PHYSICAL THERAPY PRIOR TO 950801    *
     H*                     3. ADD ERROR MSG FOR CPT-4 99456 WHICH HAS *
     H*                        TWO PAYMENT AMOUNTS                     *
     H*                     4. ADD HOME HEALTH CARE IN OTHE SUBROUTINE *
     H*
     H*  07/26/95   C:20    REQ/PROJ: AL024                            *
     H*                     1. CHANGES ANE CONVERSION FACTOR FROM 31.78*
     H*                        TO 32.67.                               *
     H******************************************************************
     H*  Indicator Usage:
     H*  54   CHAIN/DOWEQ on PCA01 - Detail Charge Primary File
     H*  60   General Usage
     H*  70   LOKUP   Usage
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPRLAL   IF   E           K DISK
     FFTABLAL   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FFDCALH    UF   E           K DISK
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFMRCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFACAPAL   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT1)
     FFACALAL   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT2)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
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
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D CDMD            S              7    DIM(10) CTDATA PERRCD(1)             CODE/MOD 02/01/95
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D NC1             S              5    DIM(76) CTDATA PERRCD(10)            NO CUT ON THESE
     D PART            S              2    DIM(13) CTDATA PERRCD(1)             PHYS PART VS. MRA
     D PMRA            S              9  2 DIM(13) ALT(PART)
     D*** BEGIN ADD *** 09/05/95 **************************************
     D CCODE           S              5    DIM(41) CTDATA PERRCD(1)             CPT4 CHIRO /  MRA
     D CMRA            S              9  2 DIM(41) ALT(CCODE)
     D PCODE           S              7    DIM(31) CTDATA PERRCD(1)             CPT4 PHYS THERAPY
     D TMRA            S              9  2 DIM(31) ALT(PCODE)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** END   ADD *** 09/05/95 **************************************
     I*
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
     I* Record Format for SURGERY Workfile FACALAL
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
     C     CAKY1B        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    HLDSEQ
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
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKYA3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    P2DSEQ
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
     C     CAKEYC        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
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
     C*** Geographical Zip Area FILE
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#
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
     C* - Initialization Section --------------------------------------
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C***********          MOVELSRDTO     SYDATE  60
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END CHANGE ************** 11/18/97 *****************************
     C*
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C     *LIKE         DEFINE    CATOTA        HLTOTA
     C     *LIKE         DEFINE    CAPAID        HLTOTP
     C*
     C     *LIKE         DEFINE    CAFYR         QQFYR
     C     *LIKE         DEFINE    CACASE        QQCASE
     C     *LIKE         DEFINE    CMSS#         QQSS#
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    PDSPCD        SVSPCD
     C     *LIKE         DEFINE    DHUNTS        SVUNTS
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C*
     C     *LIKE         DEFINE    PR1A1         SX1A1
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
     C     *LIKE         DEFINE    PRPROF        SXPROF
     C     *LIKE         DEFINE    PRRPT         SXRPT
     C     *LIKE         DEFINE    PRSUPP        SXSUPP
     C     *LIKE         DEFINE    PRSURG        SXSURG
     C     *LIKE         DEFINE    PR92N2        SX92N2
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CACHRG        HLTOTC
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C*
     C     *LIKE         DEFINE    DHPRCD        XXCPCD
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    DHAREA        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAFDAT        XXYMD
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
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C*
     C                   Z-ADD     0             O                 5 0          Counter for CDMD/OTH
     C                   Z-ADD     0             Q                 5 0          Counter for Err Msgs
     C                   Z-ADD     0             S                 5 0          Counter for Srv Dts
     C*** BEGIN ADD *** 09/05/95 *******************************************
     C                   Z-ADD     0             X                 5 0          CNTR FOR TBL LOKUPS
     C*** END   ADD *** 09/05/95 *******************************************
     C                   MOVE      '0'           XX25EM            1
     C*** BEGIN CHG *** 07/26/95 ***********************************************
     C**********           Z-ADD31.78     XXANES  92       ANESTHESIA CONV FCT
     C                   Z-ADD     32.67         XXANES            9 2          ANESTHESIA CONV FCT
     C*** END CHG *** 07/26/95 *************************************************
     C                   Z-ADD     0             XXWORK            6 2
     C                   Z-ADD     0             XXANMN            5 2
     C                   Z-ADD     0             COUNT1            5 0
     C                   Z-ADD     0             COUNT2            5 0
     C                   Z-ADD     0             COUNT3            5 0
     C                   Z-ADD     0             COUNT4            5 0
     C                   Z-ADD     0             COUNT5            5 0
     C                   Z-ADD     0             ZZMRA             9 2
     C                   MOVE      *BLANKS       ZZPART            2
     C                   MOVE      *BLANK        OPNFLG            1
     C                   MOVE      *BLANK        OVRFLG            1
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
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
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
     C     CAFDAT        IFGE      0950519
     C     CAFDAT        ANDLT     0960519
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
     C* ---------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
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
     C* ---------------------------------------------------------------
     C* --- NOT OVERRIDE ----------------------------------------------
     C*
     C* If override flag is not equal to 'Y', then process this record
     C*    Else do not adjudicate.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C* ---------------------------------------------------------------
     C*-- R&C ---------------------------------------------------------
     C* Check to see if this record is a Reasonable/Customary record
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
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
     C     RCKY          SETGT     PRC01
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
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '207'         XPEOMB
     C* EOMB Code 207: "This charge has been paid per R&C fees"
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   Z-ADD     RCMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE
     C                   GOTO      NXRC1
     C                   END
     C                   END
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*
     C*
     C*----------------------------------------------------------------
     C*   Check for level break (Provider)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*----------------------------------------------------------------
     C*------ HOSPITAL TYPE ------------------------------------------
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C*----------------------------------------------------------------
     C*  Test for Fee Schedule Reimbursement - 'Y'
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
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C*
     C*----------------------------------------------------------------
     C*-- FS Reimbursement - "N" --------------------------------------
     C*
     C*  If fee schedule reimbursement equals 'N', but the MRA is equal
     C*     to zero, take the information as is.
     C     PRFSR         IFEQ      'N'
     C*
     C*  Flag to display Fee Schedule non-reimburseable Error Message
     C                   MOVE      '1'           XX25EM
     C*
     C*-------------------------------------------------------------------------
     C*
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '203'         XPEOMB
     C* EOMB Code 203: "Charges exceed Reasonable & Customary"
     C   54              Z-ADD     PRMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   Z-ADD     PRMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- FS Reimbursement - "Y" --------------------------------------
     C*
     C*  If Fee Schedule Reimbursement equals 'Y', use MRA
     C*
     C*  Save the procedure code file values.
     C     PRFSR         IFEQ      'Y'
     C     SVSPCD        IFEQ      'CHI'
     C*** BEGIN ADD *** 09/05/95 *******************************************
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLE 950731
     C     CAFDAT        IFLE      0950731
     C*** END   CHANGE************* 11/18/97 *****************************
     C                   Z-ADD     1             X
     C     DHPRCD        LOOKUP    CCODE(X)                               70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     CMRA(X)       SXMRA
     C                   ELSE
     C                   Z-ADD     PRMRA         SXMRA
     C                   END
     C                   ELSE
     C*** END   ADD *** 09/05/95 *******************************************
     C     PR92N1        IFNE      *ZEROS
     C                   Z-ADD     PR92N1        SXMRA
     C                   ELSE                                                   ELSE PR92N1
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END PR92N1
     C                   END                                                    ADD 09/05/95
     C                   ELSE                                                   ELSE EQ'CHI'
     C     SVSPCD        IFEQ      'OD '
     C     PR92N2        IFNE      *ZEROS
     C                   Z-ADD     PR92N2        SXMRA
     C                   ELSE                                                   ELSE PR92N2
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END PR92N2
     C                   ELSE                                                   ELSE EQ'OD'
     C     SVSPCD        IFEQ      'PHT'
     C     PRPROF        IFNE      *ZEROS
     C                   Z-ADD     PRPROF        SXMRA
     C                   ELSE                                                   ELSE PRPROF
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END PRPROF
     C                   ELSE                                                   ELSE EQ'PHT'
     C     SVSPCD        IFEQ      'ASC'
     C     PR92N3        IFNE      *ZEROS
     C                   Z-ADD     PR92N3        SXMRA
     C                   ELSE                                                   ELSE PR92N3
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END PR92N3
     C                   ELSE                                                   ELSE EQ'ASC'
     C*** BEGIN DELETE *** 07/16/96 ***********************************
     C********** SVSPCD    IFEQ 'DDS'
     C********** PR92N4    IFNE *ZEROS
     C**********           Z-ADDPR92N4    SXMRA
     C**********           ELSE                            ELSE PR92N4
     C**********           Z-ADDPRMRA     SXMRA
     C**********           END                             END PR92N4
     C**********           ELSE                            ELSE EQ'DDS'
     C*** END   DELETE *** 07/16/96 ***********************************
     C     SVSPCD        IFEQ      'DIA'
     C     PR92N5        IFNE      *ZEROS
     C                   Z-ADD     PR92N5        SXMRA
     C                   ELSE                                                   ELSE PR92N5
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END PR92N5
     C                   ELSE                                                   ELSE EQ'DIA'
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END EQ 'DIA'
     C*** DEL 07/16/96 *** END                             END EQ 'DDS'
     C                   END                                                    END EQ 'ASC'
     C                   END                                                    END EQ 'PHT'
     C                   END                                                    END EQ 'OD '
     C                   END                                                    END EQ 'CHI'
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PR1A1         SX1A1
     C                   Z-ADD     PR92N2        SX92N2
     C*
     C*----------------------------------------------------------------
     C*-- By Report ---------------------------------------------------
     C*
     C     PRRPT         IFEQ      'Y'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'Y'
     C*
     C*----------------------------------------------------------------
     C*-- ANE/Amount Not Established ----------------------------------
     C*
     C     PRRPT         IFEQ      'A'
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
     C                   MOVE      *ZEROS        XPDIFF
     C     DHPRCD        IFGE      'V2020'
     C     DHPRCD        ANDLE     'V2780'
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 201: "Maximum allowable fee not established by state"
     C                   END                                                    END DHPRCD
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'A'
     C*
     C*----------------------------------------------------------------
     C*-- SV/Service Item ---------------------------------------------
     C*
     C     PRRPT         IFEQ      'S'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '52'          QQC(Q)                         SV MESSAGE
     C* Err Msg 52: "The item is SV - requires further inspection
     C*             by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'S'
     C*
     C*----------------------------------------------------------------
     C*-- NC/Not Covered ----------------------------------------------
     C*
     C     PRRPT         IFEQ      'X'
     C     DHPRCD        IFEQ      '92340'
     C     SVSPCD        ANDEQ     'OPT'
     C     DHPRCD        OREQ      '92341'
     C     SVSPCD        ANDEQ     'OPT'
     C     DHPRCD        OREQ      '92342'
     C     SVSPCD        ANDEQ     'OPT'
     C     DHPRCD        OREQ      '92370'
     C     SVSPCD        ANDEQ     'OPT'
     C     DHPRCD        OREQ      '92371'
     C     SVSPCD        ANDEQ     'OPT'
     C     DHPRCD        OREQ      'Q103'
     C     DHPRCD        OREQ      'Q104'
     C     DHPRCD        OREQ      'Q0103'
     C     DHPRCD        OREQ      'Q0104'
     C     DHPRCD        OREQ      '90900'
     C     DHPRCD        OREQ      '97799'
     C                   GOTO      PASRPT
     C                   END
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C                   EXSR      UPDAT
     C*
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C     PASRPT        TAG
     C                   END                                                    END RPT EQ 'X'
     C*
     C*----------------------------------------------------------------
     C*-- Normal Processing of Procedure Code -------------------------
     C*   Execute all Subroutines here.
     C*
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '01999'
     C                   EXSR      ANES                                         Anesthesia
     C                   ELSE
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69979'
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALAL
     C                   CLOSE     FACAPAL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURGERY Codes
     C                   CALL      'CRTFACAL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALAL
     C                   OPEN      FACAPAL
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
     C                   EXSR      SURG2                                        ADD 03/09/93
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALAL
     C                   CLOSE     FACAPAL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACAL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALAL
     C                   OPEN      FACAPAL
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
     C                   EXSR      SURG                                         Surgery
     C                   ELSE
     C     DHPRCD        IFGE      '70002'
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      RADI                                         Radiology
     C                   ELSE
     C     DHPRCD        IFGE      '80003'
     C     DHPRCD        ANDLE     '89399'
     C                   EXSR      PATH                                         Pathology
     C                   ELSE
     C     DHPRCD        IFGE      '90010'
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '99080'
     C     DHPRCD        ANDLE     '99081'
     C     DHPRCD        ORGE      'M0702'
     C     DHPRCD        ANDLE     'M0730'
     C                   EXSR      MEDI                                         Medicine
     C                   ELSE
     C     DHPRCD        IFGE      '97000'
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      'Q103'
     C     DHPRCD        ANDLE     'Q104'
     C     DHPRCD        ORGE      'Q0103'
     C     DHPRCD        ANDLE     'Q0104'
     C                   EXSR      PHYS                                         Phys Therapy
     C                   ELSE
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C                   ELSE
     C                   EXSR      OTHE                                         Other Codes
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- ELSE IS OVERRIDE --------------------------------------------
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
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C                   Z-ADD     0             XPTOTA
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C                   MOVE      *ZEROS        CADIFF
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
     C*----------------------------------------------------------------
     C*-- Process next record -----------------------------------------
     C     NXTRCD        TAG
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C*
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD                         CHG 12/28/92
     C                   END
     C                   END
     C*
     C                   ENDDO
     C*
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2
     C                   END
     C*
     C*----------------------------------------------------------------
     C* --- End the program -------------------------------------------
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALAL
     C                   CLOSE     FACAPAL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPAL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             SXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  ANESTHESIA SUBROUTINE CODES 00100 - 01999               A N E S *
     C********************************************************************
     C     ANES          BEGSR
     C*
     C*--------------------------------------------------------------------
     C*--- Valid suffixes: 6 ----------------------------------------------
     C     XPPMOD        IFNE      *BLANK
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '6 '
     C     XPPMOD        ORNE      'P1'
     C     XPPMOD        ORNE      'P2'
     C     XPPMOD        ORNE      'P3'
     C     XPPMOD        ORNE      'P4'
     C     XPPMOD        ORNE      'P5'
     C     XPPMOD        ORNE      'P6'
     C     XPPMOD        ORNE      '22'
     C     XPPMOD        ORNE      '23'
     C     XPPMOD        ORNE      '32'
     C     XPPMOD        ORNE      '51'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END NE '6 '
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- CALCULATE ARA FOR ALL VALID PROC/MODIFIERS ---------------------
     C     DHUNTS        ADD       SXANAM        XXWORK
     C     XXWORK        MULT      XXANES        SXMRA
     C*
     C*-------------------------------------------------------------------
     C*-- VERIFY BODY PART AND LEVEL OF SERVICE MATCH --------------------
     C*
     C     DHAREA        IFEQ      'A1'
     C     SVSPCD        ANDEQ     'RNA'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty Code and level of service selected do not
     C***              match, please verify."
     C                   END                                                    END DHAREA EQ 'A1'
     C*
     C     DHAREA        IFEQ      'A4'
     C     SVSPCD        IFNE      'RNA'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty Code and level of service selected do not
     C***              match, please verify."
     C                   END                                                    END SVSPCD NE 'RNA'
     C                   END                                                    END DHAREA EQ 'A4'
     C*
     C*-------------------------------------------------------------------
     C*-- VERIFY BODY PART AND PAY ACCORDINGLY ---------------------------
     C*
     C     DHAREA        IFEQ      'A1'
     C     SXMRA         MULT      1.00          SXMRA
     C                   ELSE                                                   ELSE DHAREA EQ 'A1'
     C     DHAREA        IFEQ      'A2'
     C     SVSPCD        IFEQ      'RNA'
     C     SXMRA         MULT      .40           SXMRA
     C                   ELSE                                                   ELSE SVSPCD EQ 'RNA'
     C     SXMRA         MULT      .60           SXMRA
     C                   END                                                    END SVSPCD EQ 'RNA'
     C                   ELSE                                                   ELSE DHAREA EQ 'A2'
     C     DHAREA        IFEQ      'A3'
     C     SVSPCD        IFEQ      'RNA'
     C     SXMRA         MULT      0.00          SXMRA
     C                   ELSE                                                   ELSE SVSPCD EQ 'RNA'
     C     SXMRA         MULT      1.00          SXMRA
     C                   END                                                    END SVSPCD EQ 'RNA'
     C                   ELSE                                                   ELSE DHAREA EQ 'A3'
     C     DHAREA        IFEQ      'A4'
     C     SXMRA         MULT      .65           SXMRA
     C                   END                                                    END DHAREA EQ 'A4'
     C                   END                                                    END DHAREA EQ 'A3'
     C                   END                                                    END CAPAERT EQ 'A2'
     C                   END                                                    END DHAREA EQ 'A1'
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C*-----------------------------------------------------------------------
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
     C*  Medicine Subroutine          Codes M0702 - M0730        M E D I *
     C*                                     90010 - 96999
     C*                                     99080 - 99081
     *********************************************************************
     C*
     C     MEDI          BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Valid Suffixes: 6, 9, A, B, C, D, E, M, N, P, Q, V, W, X  ------
     C*---                22, 24, 25, 26, 32, 51, 52, 55, 56, 76, 77,
     C*---                78, 79, 90, 99
     C     XPPMOD        IFNE      *BLANK
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '6 '
     C     XPPMOD        ANDNE     '9 '
     C     XPPMOD        ANDNE     'A '
     C     XPPMOD        ANDNE     'B '
     C     XPPMOD        ANDNE     'C '
     C     XPPMOD        ANDNE     'D '
     C     XPPMOD        ANDNE     'E '
     C     XPPMOD        ANDNE     'M '
     C     XPPMOD        ANDNE     'N '
     C     XPPMOD        ANDNE     'P '
     C     XPPMOD        ANDNE     'Q '
     C     XPPMOD        ANDNE     'V '
     C     XPPMOD        ANDNE     'W '
     C     XPPMOD        ANDNE     'X '
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C     XPPMOD        ANDNE     'D1'
     C     XPPMOD        ANDNE     'D2'
     C     XPPMOD        ANDNE     'D3'
     C     XPPMOD        ANDNE     'D4'
     C     XPPMOD        ANDNE     'D5'
     C     XPPMOD        ANDNE     'T1'
     C     XPPMOD        ANDNE     'T2'
     C     XPPMOD        ANDNE     'T3'
     C     XPPMOD        ANDNE     'L1'
     C     XPPMOD        ANDNE     'L2'
     C     XPPMOD        ANDNE     'H1'
     C     XPPMOD        ANDNE     'H2'
     C     XPPMOD        ANDNE     'H3'
     C     XPPMOD        ANDNE     'H4'
     C     XPPMOD        ANDNE     'CT'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END NE '6 '
     C     XPPMOD        IFEQ      'D1'
     C     XPPMOD        OREQ      'D2'
     C     XPPMOD        OREQ      'D3'
     C     XPPMOD        OREQ      'D4'
     C     XPPMOD        OREQ      'D5'
     C     DHPRCD        IFNE      '90784'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AD'          QQC(Q)
     C* Err Msg AD: "Invoice is required for payment."
     C                   END                                                    END EQ 90784
     C                   END                                                    END EQ D1
     C*
     C     XPPMOD        IFEQ      'T1'
     C     XPPMOD        OREQ      'T2'
     C     XPPMOD        OREQ      'T3'
     C     XPPMOD        OREQ      'L1'
     C     XPPMOD        OREQ      'L2'
     C     XPPMOD        OREQ      'H1'
     C     XPPMOD        OREQ      'H2'
     C     XPPMOD        OREQ      'H3'
     C     XPPMOD        OREQ      'H4'
     C     DHPRCD        IFNE      '90784'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END EQ 90784
     C                   END                                                    END EQ T1
     C*
     C     XPPMOD        IFEQ      'CT'
     C     DHPRCD        IFNE      '96410'
     C     DHPRCD        ANDNE     '96414'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AD'          QQC(Q)
     C* Err Msg AD: "Invoice is required for payment."
     C                   END                                                    END EQ 96410
     C                   END                                                    END EQ CT
     C*
     C* Professional Component
     C                   EXSR      PMOD
     C*
     C                   END                                                    END NE *BLANKS
     C*
     C* IF CODE IS 65222, 82948, 90000-99999
     C* AND SPECAILTY CODE IS 'OPT' THEN USE OPT AMOUNT PR92N2
     C*
     C     DHPRCD        IFEQ      '65222'
     C     DHPRCD        OREQ      '82948'
     C     DHPRCD        ORGE      '90000'
     C     DHPRCD        ANDLE     '99999'
     C*
     C********** CAFDAT    IFGE 930915                     DEL 02/22/95
     C     SVSPCD        IFEQ      'OPT'                                        CHG 02/22/95
     C                   Z-ADD     SX92N2        SXMRA
     C                   END
     C*
     C                   END
     C*
     C*-------------------------------------------------------------------
     C* Look up this Code/Suffix (modifier) to see if it should get its
     C*    MRA value from one of the "OTHER MRA" fields
     C*
     C                   MOVE      *BLANKS       OTHMRA
     C                   MOVE      *BLANKS       OTCODE
     C                   MOVEL     DHPRCD        OTCODE
     C                   MOVE      XPPMOD        OTCODE
     C*
     C                   Z-ADD     1             O
     C     OTCODE        LOOKUP    CDMD(O)                                60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AC'          QQC(Q)
     C* Err Msg AC: "Review Fee Schedule for MRA value on these code/
     C*             suffixes."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END 1 *IN60
     C*
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C     XPPMOD        IFNE      'D1'
     C     XPPMOD        ANDNE     'D2'
     C     XPPMOD        ANDNE     'D3'
     C     XPPMOD        ANDNE     'D4'
     C     XPPMOD        ANDNE     'D5'
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C                   END
     C*
     C                   EXSR      UPDAT
     C*-------------------------------------------------------------------
     C*
     C     MEDIE         ENDSR
     *********************************************************************
     C*  NEW PROVIDER SUBROUTINE (LEVEL BREAK)               N E W P R O *
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
     C     CAKY1B        CHAIN     PCA01                              54
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
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** OFFICE VISITS     SUBROUTINE CODES 99201 - 99499        O F F I *
     *********************************************************************
     C*
     C     OFFI          BEGSR
     C*
     C* IF CODE IS 65222, 82948, 90000-99999
     C* AND SPECAILTY CODE IS 'OPT' THEN USE OPT AMOUNT PR92N2
     C*
     C     DHPRCD        IFEQ      '65222'
     C     DHPRCD        OREQ      '82948'
     C     DHPRCD        ORGE      '90000'
     C     DHPRCD        ANDLE     '99999'
     C*
     C     SVSPCD        IFEQ      'OPT'
     C                   Z-ADD     SX92N2        SXMRA
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 33-34, B6
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
     C     DHPRCD        OREQ      '99241'
     C     DHPRCD        OREQ      '99242'
     C     DHPRCD        OREQ      '99243'
     C     DHPRCD        OREQ      '99244'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99251'
     C     DHPRCD        OREQ      '99252'
     C     DHPRCD        OREQ      '99253'
     C     DHPRCD        OREQ      '99254'
     C     DHPRCD        OREQ      '99255'
     C     DHPRCD        OREQ      '99271'
     C     DHPRCD        OREQ      '99272'
     C     DHPRCD        OREQ      '99273'
     C     DHPRCD        OREQ      '99274'
     C     DHPRCD        OREQ      '99275'
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C     DHPRCD        OREQ      '99301'
     C     DHPRCD        OREQ      '99302'
     C     DHPRCD        OREQ      '99303'
     C     DHPRCD        OREQ      '99311'
     C     DHPRCD        OREQ      '99312'
     C     DHPRCD        OREQ      '99313'
     C     DHPRCD        OREQ      '99321'
     C     DHPRCD        OREQ      '99322'
     C     DHPRCD        OREQ      '99323'
     C     DHPRCD        OREQ      '99341'
     C     DHPRCD        OREQ      '99342'
     C     DHPRCD        OREQ      '99343'
     C     DHPRCD        OREQ      '99381'
     C     DHPRCD        OREQ      '99382'
     C     DHPRCD        OREQ      '99383'
     C     DHPRCD        OREQ      '99384'
     C     DHPRCD        OREQ      '99385'
     C     DHPRCD        OREQ      '99386'
     C     DHPRCD        OREQ      '99387'
     C     DHPRCD        OREQ      '99401'
     C     DHPRCD        OREQ      '99402'
     C     DHPRCD        OREQ      '99403'
     C     DHPRCD        OREQ      '99404'
     C     DHPRCD        OREQ      '99411'
     C     DHPRCD        OREQ      '99412'
     C     DHPRCD        OREQ      '99420'
     C     DHPRCD        OREQ      '99429'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN1        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69999'
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
     C*** EOMB 216: "Payment is made for code 99025 instead of O/V per Fee
     C**             Schedule"
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
     C                   MOVE      PR1A1         SX1A1
     C                   Z-ADD     PR92N2        SX92N2
     C*
     C                   END
     C*
     C                   ELSE
     C                   GOTO      AGAIN1
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 33-34, B6
     C*** When a starred
     C*** procedure performed at the time of a follow-up office visit,
     C*** the follow -up visit will be disallowed
     C*
     C     DHPRCD        IFEQ      '99211'
     C     DHPRCD        OREQ      '99212'
     C     DHPRCD        OREQ      '99213'
     C     DHPRCD        OREQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C     DHPRCD        OREQ      '99221'
     C     DHPRCD        OREQ      '99222'
     C     DHPRCD        OREQ      '99223'
     C     DHPRCD        OREQ      '99231'
     C     DHPRCD        OREQ      '99232'
     C     DHPRCD        OREQ      '99233'
     C     DHPRCD        OREQ      '99241'
     C     DHPRCD        OREQ      '99242'
     C     DHPRCD        OREQ      '99243'
     C     DHPRCD        OREQ      '99244'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99251'
     C     DHPRCD        OREQ      '99252'
     C     DHPRCD        OREQ      '99253'
     C     DHPRCD        OREQ      '99254'
     C     DHPRCD        OREQ      '99255'
     C     DHPRCD        OREQ      '99261'
     C     DHPRCD        OREQ      '99262'
     C     DHPRCD        OREQ      '99263'
     C     DHPRCD        OREQ      '99271'
     C     DHPRCD        OREQ      '99272'
     C     DHPRCD        OREQ      '99273'
     C     DHPRCD        OREQ      '99274'
     C     DHPRCD        OREQ      '99275'
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C     DHPRCD        OREQ      '99301'
     C     DHPRCD        OREQ      '99302'
     C     DHPRCD        OREQ      '99303'
     C     DHPRCD        OREQ      '99311'
     C     DHPRCD        OREQ      '99312'
     C     DHPRCD        OREQ      '99313'
     C     DHPRCD        OREQ      '99331'
     C     DHPRCD        OREQ      '99332'
     C     DHPRCD        OREQ      '99333'
     C     DHPRCD        OREQ      '99351'
     C     DHPRCD        OREQ      '99352'
     C     DHPRCD        OREQ      '99353'
     C     DHPRCD        OREQ      '99391'
     C     DHPRCD        OREQ      '99392'
     C     DHPRCD        OREQ      '99393'
     C     DHPRCD        OREQ      '99394'
     C     DHPRCD        OREQ      '99395'
     C     DHPRCD        OREQ      '99396'
     C     DHPRCD        OREQ      '99397'
     C     DHPRCD        OREQ      '99401'
     C     DHPRCD        OREQ      '99402'
     C     DHPRCD        OREQ      '99403'
     C     DHPRCD        OREQ      '99404'
     C     DHPRCD        OREQ      '99411'
     C     DHPRCD        OREQ      '99412'
     C     DHPRCD        OREQ      '99420'
     C     DHPRCD        OREQ      '99429'
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
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '90'          QQC(Q)
     C*** Err Msg 90: "Adjustor should review charges for significant
     C***              identifiable services"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C*
     C                   END
     C                   ELSE
     C                   GOTO      AGAIN2
     C*
     C                   END
     C                   END
     C*** BEGIN ADD *** 09/05/95 **************************************
     C*** Per 1995 Updates
     C*** CPT-4 Code 99456 has two MRA amounts, $200 for 1st hour,
     C*** $50 for each additional 30 minutes
     C*** Involves exam by other than treating physican for work
     C*** related or medical disability.
     C*
     C     DHPRCD        IFEQ      '99456'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BT'          QQC(Q)
     C*** Err Msg BT: "For CPT4 99456 - Pay at $200 for 1st hr; $50
     C***              for each addtl 30 min"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      *BLANKS       XPEOMB
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C*** END   ADD *** 09/05/95 **************************************
     C*
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C*----------------------------------------------------------------
     C*   If Professional Component:
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFEQ      'DM'
     C     XPPMOD        OREQ      'ET'
     C     DHPRCD        IFNE      '99070'
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
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AD'          QQC(Q)
     C* Err Msg AD: "Invoice is required for payment."
     C                   END                                                    END EQ 99070
     C                   END                                                    END EQ DM,ET
     C                   EXSR      PMOD
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- RX PAIN MANAGEMENT PM001 ---------------------------------------
     C     DHPRCD        IFEQ      'PM001'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AD'          QQC(Q)
     C* Err Msg AD: "Invoice is required for payment."
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   138.38        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   142.26        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   ADD       CACHRG        SXMRA
     C                   Z-ADD     SXMRA         CACHRG
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- RX PAIN MANAGEMENT PM002 ---------------------------------------
     C     DHPRCD        IFEQ      'PM002'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   35.88         SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   36.89         SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- RX PAIN MANAGEMENT PM003 ---------------------------------------
     C     DHPRCD        IFEQ      'PM003'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   46.13         SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   47.42         SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   END
     C*
     C* IF CODE IS 65222, 82948, 90000-99999
     C* AND SPECAILTY CODE IS 'OPT' THEN USE OPT AMOUNT PR92N2
     C*
     C     DHPRCD        IFEQ      '65222'
     C     DHPRCD        OREQ      '82948'
     C     DHPRCD        ORGE      '90000'
     C     DHPRCD        ANDLE     '99999'
     C*
     C     SVSPCD        IFEQ      'OPT'
     C                   Z-ADD     SX92N2        SXMRA
     C                   END
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 09/05/95 **************************************
     C* Per pg. 72: Home Health Care
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C                   Z-ADD     0             XXUNTS            7 4
     C     DHPRCD        IFEQ      'RNH  '
     C     DHPRCD        OREQ      'LPNH '
     C     DHPRCD        OREQ      'CNAH '
     C     DHPRCD        OREQ      'SITH '
     C     DHUNTS        DIV(H)    60            XXUNTS
     C*
     C* Determine "MRA" for Hourly Charges
     C     XXUNTS        MULT(H)   SXANAM        SXMRA
     C*
     C                   END                                                    END EQ RNH...
     C*
     C     DHPRCD        IFEQ      'SNCV '
     C     DHPRCD        OREQ      'PHTV '
     C     DHPRCD        OREQ      'SPCV '
     C     DHPRCD        OREQ      'OCTV '
     C     DHPRCD        OREQ      'MSSV '
     C     DHPRCD        OREQ      'HHAV '
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        SXMRA
     C                   END
     C*
     C                   END                                                    END EQ SNCV..
     C*** END   ADD *** 09/05/95 **************************************
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     C*  Pathology Subroutine         Codes 80003 - 89399        P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Valid Suffixes: 8, P, T ----------------------------------------
     C     XPPMOD        IFNE      *BLANK
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '8 '
     C     XPPMOD        ANDNE     'P '
     C     XPPMOD        ANDNE     'T '
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '90'
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END NE '8 '
     C*
     C* Professional Component
     C                   EXSR      PMOD
     C*
     C                   END                                                    END NE *BLANKS
     C*-------------------------------------------------------------------
     C* Look up this Code/Suffix (modifier) to see if it should get its
     C*    MRA value from one of the "OTHER MRA" fields
     C*
     C                   MOVE      *BLANKS       OTHMRA
     C                   MOVE      *BLANKS       OTCODE
     C                   MOVEL     DHPRCD        OTCODE
     C                   MOVE      XPPMOD        OTCODE
     C*
     C                   Z-ADD     1             O
     C     OTCODE        LOOKUP    CDMD(O)                                60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AC'          QQC(Q)
     C* Err Msg AC: "Review Fee Schedule for MRA value on these code/
     C*             suffixes."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*
     C* IF CODE IS 82948, 90000-99999
     C* AND SPECAILTY CODE IS 'OPT' THEN USE OPT AMOUNT PR92N2
     C*
     C     DHPRCD        IFEQ      '82948'
     C*
     C     SVSPCD        IFEQ      'OPT'
     C                   Z-ADD     SX92N2        SXMRA
     C                   END
     C*
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*                                              >
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*-------------------------------------------------------------------
     C*
     C     PATHE         ENDSR
     *********************************************************************
     C*  Physical Medicine Subroutine Codes 97000 - 97799        P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Valid Suffixes:  D, N ------------------------------------------
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      'D '
     C     XPPMOD        ANDNE     'N '
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 10/19/92
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END NE 'D '
     C*
     C* Professional Component
     C                   EXSR      PMOD
     C*
     C                   END                                                    END NE *BLANKS
     C*
     C*
     C* IF SERVICE DATE >= 01/01/95 USE PHT MRA AMOUNT FROM PRPROF IF
     C* PR1A1 = T
     C*
     C     SVSPCD        IFNE      'CHI'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C********   CAFDAT    IFGE 950801                     ADD 09/05/95
     C     CAFDAT        IFGE      0950801
     C*** END   CHANGE************* 11/18/97 *****************************
     C     SX1A1         ANDEQ     'T'
     C                   Z-ADD     SXPROF        SXMRA
     C******************** END                             DEL 09/05/95
     C*** BEGIN ADD *** 09/05/95 *******************************************
     C                   ELSE                                                   ELSE SVSPCD EQ 'CHI'
     C     SX1A1         IFEQ      'T'
     C                   Z-ADD     1             X
     C     DHPRCD        CAT       DHAREA:0      CPTPRT            7
     C     CPTPRT        LOOKUP    PCODE(X)                               70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     TMRA(X)       SXMRA
     C                   ELSE                                                   ELSE *IN70
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END *IN70
     C                   END                                                    END SX1A1 EQ 'T'
     C                   END                                                    END CAFDAT 950801
     C                   END                                                    END SVSPCD NE CHI
     C*** END   ADD *** 09/05/95 *******************************************
     C*
     C* MAXIMUM OF 1 HOUR OF ADDITIONAL TIME ALLOWED FOR 97221 OR 97241
     C*
     C     DHPRCD        IFEQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     DHPRCD        XXCPT4
     C                   Z-ADD     *ZEROS        CNTADD
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        IFNE      CADSEQ
     C                   ADD       1             CNTADD
     C                   END                                                    END XRDSEQ
     C                   END                                                    END XRTOTP
     C     CAKEY8        READE     PCA09                                  60
     C                   END                                                    END DOWEQ '0'
     C     CNTADD        IFGE      4
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "Exceeds number of allowable procedures in the
     C***            reimbursement manual."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END CNTADD
     C                   END                                                    END EQ '97221'
     C*
     C* IF SERVICE DATE >= 01/01/95 AND CODE IS 97752 LOOK UP TABLE
     C* VALUE FOR CORRECT MRA FOR THIS BODY PART
     C*
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C********   CAFDAT    IFGE 950101
     C     CAFDAT        IFGE      0950101
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHPRCD        ANDEQ     '97752'
     C*
     C                   Z-ADD     1             O
     C     DHAREA        LOOKUP    PART(O)                                70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     PMRA(O)       SXMRA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 06: "A copy of the justification report is required
     C*             for reimbursement"
     C*
     C                   END                                                    END *IN70
     C*
     C* MAXIMUM OF 2 AREAS UNLESS DOCUMENTED MULTIPLE INJURIES
     C*
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     DHPRCD        XXCPT4
     C                   Z-ADD     *ZEROS        CNTPRT
     C     CAKEYC        CHAIN     PCA0C                              60
     C     *IN60         DOWEQ     '0'
     C     XCTOTP        IFGT      *ZEROS
     C     XCDSEQ        IFNE      CADSEQ
     C     XCPART        IFNE      DHAREA
     C                   ADD       1             CNTPRT
     C                   END                                                    END XRPART
     C                   END                                                    END XRDSEQ
     C                   END                                                    END XRTOTP
     C     CAKEYC        READE     PCA0C                                  60
     C                   END                                                    END DOWEQ '0'
     C     CNTPRT        IFGE      2
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AM'          QQC(Q)
     C* Err Msg 06: "Only two areas allowed unless documentation of
     C*             multiple injuries"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END CNTPRT
     C*
     C                   END                                                    END CAFDAT
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*                                              >
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C*-------------------------------------------------------------------
     C*
     C*
     C     PHYSE         ENDSR
     C********************************************************************
     C*  PMOD - Procedure Code Modifier Subroutine               P M O D *
     C********************************************************************
     C*  CALCULATE MRA BASED ON MODIFIER ENTERED
     C*       P  - "Professional Component"
     C*            Pay per PC Unit Value
     C********************************************************************
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C* -----------------------------------------------------------------------
     C* - P/Professional Component ---------------------------------
     C*
     C     XPPMOD        IFEQ      '26'                                         ADD 05/10/93
     C     XPPMOD        OREQ      'P'                                          CHG 05/10/93
     C                   Z-ADD     SXPROF        SXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C*  0 MODIFIER              -----------------------------------
     C*
     C     XPPMOD        IFEQ      '0 '                                         ADD 09/04/96
     C     SXMRA         MULT(H)   .125          SXMRA                          ADD 09/04/96
     C                   GOTO      PMODE                                        ADD 09/04/96
     C                   END                                                    ADD 09/04/96
     C*
     C* -----------------------------------------------------------------------
     C* -----------------------------------------------------------------------
     C* - 51 MULTIPLE PROCEDURES -----------------------------------
     C*
     C     XPPMOD        IFEQ      '51'
     C     SXMRA         MULT(H)   .50           SXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - ASSISTING SURGEON ----------------------------------------
     C*
     C     XPPMOD        IFEQ      '80'
     C     SXMRA         MULT      .25           SXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - CHEMOTHERAPY ---------------------------------------------
     C*
     C     XPPMOD        IFEQ      'CT'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHPRCD        IFEQ      '96410'
     C     DHUNTS        MULT(H)   46.13         SXMRA
     C                   ELSE
     C     DHUNTS        MULT(H)   92.25         SXMRA
     C                   END
     C*** BEGIN ADD *** 07/16/96 *******************************************
     C                   END
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFGE 960215                     ADD 07/16/96
     C     CAFDAT        IFGE      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHPRCD        IFEQ      '96410'
     C     DHUNTS        MULT(H)   47.42         SXMRA
     C                   ELSE
     C     DHUNTS        MULT(H)   94.83         SXMRA
     C                   END
     C                   END
     C*** END   ADD *** 07/16/96 *******************************************
     C                   ADD       CACHRG        SXMRA
     C                   Z-ADD     SXMRA         CACHRG
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - DURABLE MEDICAL EQUIPMENT --------------------------------
     C*
     C     XPPMOD        IFEQ      'DM'
     C     CACHRG        MULT(H)   1.333         SXMRA
     C                   Z-ADD     SXMRA         CACHRG
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - ENTERAL THERAPY ------------------------------------------
     C*
     C     XPPMOD        IFEQ      'ET'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   21.53         SXMRA
     C*** BEGIN ADD *** 07/16/96 *******************************************
     C                   ELSE
     C     DHUNTS        MULT(H)   22.13         SXMRA
     C                   END
     C*** END   ADD *** 07/16/96 *******************************************
     C                   ADD       CACHRG        SXMRA
     C                   Z-ADD     SXMRA         CACHRG
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - HYDRATION H1 ---------------------------------------------
     C*
     C     XPPMOD        IFEQ      'H1'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   87.13         SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   89.57         SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - HYDRATION H2 ---------------------------------------------
     C*
     C     XPPMOD        IFEQ      'H2'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   102.50        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   105.37        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - HYDRATION H3 ---------------------------------------------
     C*
     C     XPPMOD        IFEQ      'H3'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   138.38        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   142.26        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - HYDRATION H4 ---------------------------------------------
     C*
     C     XPPMOD        IFEQ      'H4'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   174.25        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   179.13        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - TOTAL PARENTERAL NUTRITION L1 ----------------------------
     C*
     C     XPPMOD        IFEQ      'L1'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   66.63         SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   68.50         SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - TOTAL PARENTERAL NUTRITION L2 ----------------------------
     C*
     C     XPPMOD        IFEQ      'L2'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   87.13         SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   89.57         SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - TOTAL PARENTERAL NUTRITION T1 ----------------------------
     C*
     C     XPPMOD        IFEQ      'T1'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   261.38        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   268.70        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - TOTAL PARENTERAL NUTRITION T2 ----------------------------
     C*
     C     XPPMOD        IFEQ      'T2'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   317.75        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   326.65        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - TOTAL PARENTERAL NUTRITION T3 ----------------------------
     C*
     C     XPPMOD        IFEQ      'T3'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   353.63        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   363.53        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - PARENTERAL/ENTERAL D1 ------------------------------------
     C*
     C     XPPMOD        IFEQ      'D1'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   153.75        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   158.06        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C*
     C     CABRTC        IFGT      *ZEROS
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
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - PARENTERAL/ENTERAL D2 ------------------------------------
     C*
     C     XPPMOD        IFEQ      'D2'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   174.25        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   179.13        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C*
     C     CABRTC        IFGT      *ZEROS
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
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - PARENTERAL/ENTERAL D3 ------------------------------------
     C*
     C     XPPMOD        IFEQ      'D3'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   194.75        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   200.20        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C*
     C     CABRTC        IFGT      *ZEROS
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
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - PARENTERAL/ENTERAL D4 ------------------------------------
     C*
     C     XPPMOD        IFEQ      'D4'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   246.00        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   252.89        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C*
     C     CABRTC        IFGT      *ZEROS
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
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - PARENTERAL/ENTERAL D5 ------------------------------------
     C*
     C     XPPMOD        IFEQ      'D5'
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C*********  CAFDAT    IFLT 960215                     ADD 07/16/96
     C     CAFDAT        IFLT      0960215
     C*** END   CHANGE************* 11/18/97 *****************************
     C     DHUNTS        MULT(H)   307.50        SXMRA
     C                   ELSE                                                   ADD 07/16/96
     C     DHUNTS        MULT(H)   316.11        SXMRA                          ADD 07/16/96
     C                   END                                                    ADD 07/16/96
     C*
     C     CABRTC        IFGT      *ZEROS
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
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - Any other modifier falls thru to here: --------------------
     C*
     C     SXMRA         MULT(H)   XXPERC        SXMRA
     C                   GOTO      PMODE
     C*
     C*
     C     PMODE         ENDSR
     *********************************************************************
     C*  RADIOLOGY SUBROUTINE  CODES 70002 - 79999               R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C* ----------------------------------------------------------------
     C* --- Valid Suffixes: 5, E, P ------------------------------------
     C*
     C     XPPMOD        IFNE      *BLANKS
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '5 '
     C     XPPMOD        ANDNE     'E '
     C     XPPMOD        ANDNE     'P '
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '62'
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END NE '5 '
     C*
     C                   EXSR      PMOD                                         ADD 08/06/93
     C                   END                                                    ENDNE *BLANKS
     C*
     C* ----------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C* ----------------------------------------------------------------
     C*
     C     RADIE         ENDSR
     *********************************************************************
     C*  SURGERY SUBROUTINE    CODES 10000 - 69979               S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*-------------------------------------------------------------------
     C* Look up this Code/Suffix (modifier) to see if it should get its
     C*    MRA value from one of the "OTHER MRA" fields
     C*
     C                   MOVE      *BLANKS       OTHMRA
     C                   MOVE      *BLANKS       OTCODE
     C                   MOVEL     DHPRCD        OTCODE
     C                   MOVE      XPPMOD        OTCODE
     C*
     C                   Z-ADD     1             O
     C     OTCODE        LOOKUP    CDMD(O)                                60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AC'          QQC(Q)
     C* Err Msg AC: "Review Fee Schedule for MRA value on these code/
     C*             suffixes."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END 1 *IN60
     C*
     C*
     C* ----------------------------------------------------------------
     C* --- Valid Suffixes: 2 ------------------------------------------
     C*
     C     XPPMOD        IFNE      *BLANKS
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '2 '
     C     XPPMOD        ANDNE     '0 '                                         ADD 09/04/96
     C     XPPMOD        ANDNE     '20'
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
     C     XPPMOD        ANDNE     '82'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END NE '2 '
     C*
     C                   EXSR      PMOD                                         ADD 06/08/93
     C*
     C                   END                                                    ENDNE *BLANKS
     C*
     C* ----------------------------------------------------------------
     C*
     C* PER PG 32-33, B3
     C* IF SPECIALTY CODE IS FOR AN ASSISTANT SURGEON TAKE 25% OF FULL
     C* CODE VALUE FOR ASSISTANT SURGEON VALUE.
     C*
     C     QQSPCD        IFEQ      'AS '
     C     XPPMOD        ANDNE     '80'
     C     SXMRA         MULT      .25           SXMRA
     C                   END                                                    END QQSPCD=AS
     C*
     C* ----------------------------------------------------------------
     C*
     C* PER PG 33, B4
     C* IF SPECIALTY CODE IS FOR A LICENSED SURGEON'S ASSISTANT TAKE
     C* 25% OF FULL CODE VALUE MULTIPLIED BY 50% FOR LSA VALUE.
     C*
     C     QQSPCD        IFEQ      'LSA'
     C     SXMRA         MULT      .25           SXMRA
     C     SXMRA         MULT      .50           SXMRA
     C                   END                                                    END QQSPCD=AS
     C*
     C* IF CODE IS 65222
     C* AND SPECAILTY CODE IS 'OPT' THEN USE OPT AMOUNT PR92N2
     C*
     C     DHPRCD        IFEQ      '65222'
     C*
     C     SVSPCD        IFEQ      'OPT'                                        CHG 02/22/95
     C                   Z-ADD     SX92N2        SXMRA
     C                   END
     C*
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*
     C*  PER PG 32, B2
     C*  Surgical procedures (10000-69999) pay highest $ amount at
     C*  100% , all others pay 50%. THIS IS FOR EACH BODY PART
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE STARRED PROCEDURES - THESE
     C*  WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   GOTO      NOCUT
     C                   END
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE PROCEDURES FOUND IN TABLE NC1
     C*  THESE WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C*  If the procedure code is between 10000 and 69999 and the
     C*  modifier is zero, write to work file
     C*
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C     XPPMOD        ANDNE     '51'                                         ADD 09/04/96
     C********** XPPMOD    ANDEQ*BLANK                     DEL 09/04/96
     C                   MOVE      'S'           CATYPE
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   WRITE     PCAPT1
     C                   GOTO      SURGE
     C                   END
     C*
     C     NOCUT         TAG
     C*
     C* ----------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54SXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 07/29/93
     C*
     C                   EXSR      UPDAT
     C* ----------------------------------------------------------------
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
     C                   Z-ADD     0             COUNT1
     C                   Z-ADD     0             COUNT2
     C                   Z-ADD     0             COUNT3
     C                   Z-ADD     0             COUNT4
     C                   Z-ADD     0             COUNT5
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or 50% -- FOR EACH BODY PART
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     P2MRA         ZZMRA                          Chrg's MRA
     C*
     C     P2PART        IFEQ      'S1'
     C                   ADD       1             COUNT1
     C     COUNT1        IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END GT 1
     C                   ELSE
     C     P2PART        IFEQ      'S2'
     C                   ADD       1             COUNT2
     C     COUNT2        IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END GT 1
     C                   ELSE
     C     P2PART        IFEQ      'S3'
     C                   ADD       1             COUNT3
     C     COUNT3        IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END GT 1
     C                   ELSE
     C     P2PART        IFEQ      'S4'
     C                   ADD       1             COUNT4
     C     COUNT4        IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END GT 1
     C                   ELSE
     C     P2PART        IFEQ      'S5'
     C                   ADD       1             COUNT5
     C     COUNT5        IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END GT 1
     C                   END                                                    END EQ 'S5'
     C                   END                                                    END EQ 'S4'
     C                   END                                                    END EQ 'S3'
     C                   END                                                    END EQ 'S2'
     C                   END                                                    END EQ 'S1'
     C*
     C     P2TOTC        IFGT      ZZMRA
     C                   Z-ADD     ZZMRA         XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 10/19/92
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   Z-ADD     ZZMRA         XPBRTC                         ADD 07/29/93
     C                   ELSE                                                   ELSE CACHRG GT L9750
     C                   Z-ADD     P2TOTC        XPTOTA
     C     ZZMRA         SUB       XPTOTA        XPDIFF                          ADD 10/19/92
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     ZZMRA         XPBRTC                         ADD 07/29/93
     C                   END                                                    END CACHRG GT L97503
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
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C* ---------------------------------------------------------------
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
     C* ---------------------------------------------------------------
     C*
     C*  Only process the record if not an override type
     C                   Z-ADD     CACHRG        XPTOTC                         ADD 08/11/93
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTC        CACHRG                         ADD 08/11/93
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C**********           MOVELXPPMOD    XPPMOD           DEL 09/04/96
     C                   MOVE      XPEOMB        CAEOMB
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 07/29/93
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
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C*
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY6         CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C* Err Msg 25: "This item is non-reimburseable under the Fee Sched
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN54
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 07/29/93
     C*
     C     UPDAE         ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR SURG  FILE FACAPAL             U P D A T2 *
     C********************************************************************
     C     UPDAT2        BEGSR
     C*
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
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
     C  N60              Z-ADD     30            Q
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
     C*  Using FACAPAL record, chain to FDCAP record to update
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
     C                   Z-ADD     XPDIFF        XRDIFF                         ADD 10/19/92
     C**********           MOVELXPPMOD    XRPMOD           DEL 09/04/96
     C                   MOVE      XPEOMB        XREOMB
     C                   Z-ADD     XPBRTC        XRBRTC                         ADD 07/29/93
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
     C                   Z-ADD     P2FEIN        CAFEIN
     C                   Z-ADD     P2MOD         CAMOD
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVEL     P2PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C     CAKY8         CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
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
     C                   Z-ADD     *ZERO         XPDIFF                         ADD 10/19/92
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 07/29/93
     C*
     C     UPDAE2        ENDSR
     C********************************************************************
     OPCA01     E            ECA01
**  CDMD: LOKUP Code/Mod (suffix) to find out what "OTHER MRA" field to use
622793
92083M
92083N
92502M
93010A
93010N
93014A
93014N
95830A
95830N
**  QCMDEXC TABLE
DLTOVR FILE(FACALAL)
OVRDBF FILE(FACALAL) TOFILE(QTEMP/FACALAL)
DLTOVR FILE(FACAPAL)
OVRDBF FILE(FACAPAL) TOFILE(QTEMP/FACAPAL)
**  NC1 CODES NOT TO CUT LIST 1
11001111011120111701117111173115261157871700117002
17101171022258522650261252686126863273584111444955
47001586115941259525630666307663078630826308663088
64787648306483264837648596487211732119221510115121
15201152211524117201173071900119340221452214822230
27692333503353033830356813621836248611066113061712
61795630356304863057630916330864443647276477864783
6487464876649016490267335
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
**  CPT4 CHIRO / MRA
72010000008879
72040000005691
72050000008764
72052000009903
72070000006487
72100000007398
72110000009789
72114000015138
99201000003850
99202000004783
99203000006767
99204000008167
99205000014467
99211000002683
99212000003616
99213000004200
99214000005483
99215000008049
99244000014467
99245000015983
97010000001743
97012000002460
97014000002563
97016000003075
97018000002768
97020000001230
97022000002665
97024000002153
97026000001230
97028000002153
97110000003075
97112000003075
97114000002460
97116000003075
97118000002665
97120000002870
97122000002460
97124000002153
97145000001538
97260000002968
97261000001538
**  CPT4 PHYSICAL THERAPY / MRA
97010  000001743
97012  000002460
97014  000002563
97016  000003075
97018  000002768
97020  000001230
97022  000002665
97024  000002153
97026  000001230
97028  000002153
97110  000003075
97112  000003075
97114  000002460
97116  000003075
97118  000002665
97120  000002870
97122  000002460
97124  000002153
97260  000002768
97261  000001538
97500  000003895
97501  000002050
97520  000003588
97521  000001845
97530  000003895
97531  000002050
97540  000003485
97541  000002050
Q0103  000005125
Q0104  000003075
90900  000004100
