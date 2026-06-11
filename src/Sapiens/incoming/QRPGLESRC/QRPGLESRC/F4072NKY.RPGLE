     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4072RKY
     H*    DESCRIPTION: ADJUDICATION (KENTUCKY RULES - 02/01/97)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the KENTUCKY State
     H* Fee Schedule Reimbursment Manual issued on 02/01/97
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 02/01/97 and before 01/01/99
     H*****************************************************************
     H*  CREATED: 02/24/97   C:59   PROJECT: KY008
     H***************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*
     H*  CHI - CHIROPRACTOR
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  02/03/00 C:79 Support#: 21500                                *
     H*               If override and no percent do not update        *
     H*                                                               *
     H*  7/29/99 C:79 Support#: 18829                                 *
     H*               Use correct DSEQ when writing poss dup message  *
     H*                                                               *
     H*  5/24/99 C:79 PROJ:KY011                                      *
     H*               Modified to work with provider bills            *
     H*                                                               *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*
     H*  01/21/99 C:78  Project: KY009
     H*                 Program should be use for any service provided
     H*                 before 01/01/99 also.
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  08/24/98 C:79  Project: MC082 Support:13381
     H*         If modifier 47 is billed, then disallow with message
     H*         that code is BY REPORT and further inspection by an
     H*         adjuster is needed.
     H*
     H*  08/21/98 C:79  Project: MC082 Support:13350
     H*         If modifier 80 is billed, then do not apply the
     H*         multiple surgery rule.
     H*
     H*  11/18/97 C:86  Project: MC145
     H*         CHECK ALL DATE FIELDS AND ROUTINES TO SEE IF THEY ARE
     H*         CORRECT FOR 7 DIGIT FILE FIELDS & 8 DIGIT SCREEN FIELDS
     H*
     H*  11/03/97 C:59  PROJ: MC082/#9178
     H*           1.    MODIFIER -50 HAS BEEN CHANGED TO NOT GO THROUGH
     H*                 MULTIPLE SURGICAL RULE.  ADDED S1 - S6 BODY
     H*                 PARTS TO SURG2 TO SEPERATE PROCEDURES
     H*
     H*  10/01/97 C:59  PROJ: MC082/#8791
     H*           1.    MADE CORRECTION TO 99025 CONVERSION FACTOR
     H*                 CORRECT AMOUNT IS 40.43 NOT SURGERY AMOUNT
     H*                 NEED TO MAKE CORRECTIONS TO CPT4 FILE CODE
     H*                 CODE 99025, AMOUNT IS .76 NOT .35
     H*                 ADD PG400 #1- ADD .35 TO SURGERY VALUE AND
     H*                 AND DENY CODES 99211-99215
     H*
     H*                 PROJ: MC082/#8796
     H*           2.    REMOVED MODIFIER -51 WHEN DEALING WITH
     H*                 SURGERY CODES.  ALL SURGICAL CODES WILL PASS
     H*                 THROUGH MULTIPLE SURGICAL RULE.
     H*
     H*  07/03/97 C:59  PROJ: MC082/#8025
     H*                 MADE CORRECTION TO ANES CONVERSION FACTOR
     H*                 CORRECT AMOUNT IS 29.16
     H*                 NEED TO MAKE CORRECTIONS TO CPT4 FILE
     H*                 CODES 99100,99116,99135,99140 NEED TO BE SETUP
     H*                 LIKE ANESTHESIA CODES.  THEY WERE PUT INTO ANES
     H*                 SUBROUTINE.
     H*
     H*           2.    PROJ: MC082/#8050
     H*                 97XXX RANGE WERE PAYING AT A 30 MINUTE CONVERSION
     H*                 TIME.  CORRECT AMOUNT IS 15 FOR EFFECTIVE 2/1/97
     H*
     H*  07/20/97 C:59  PROJ: MC082/#8231
     H*                 MADE CORRECTION TO ANES CALCULATION
     H*                 ROUND UP ALL FRACTIONS IF >=.3
     H*
     H*  08/05/97 C:59  PROJ: MC082/#8246
     H*                 MADE CORRECTION TO OFFICE VISIT
     H*                 CODE 99211 WAS DISPLAYING AN ERROR FOR
     H*                 EVERY PT CODE INSTEAD OF JUST ONE.
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                        SUBR         CODES
     H*  Evaluation & Management                        EVAL    99201 - 99499
     H*  Anesthesia                                     ANES    00100 - 01999
     H*                                                         02000 - 02020
     H*  Surgery                                        SURG    10000 - 69999
     H*                                                 SURG2   10000 - 69999
     H*  Radiology                                      RADI    70010 - 79999
     H*                                                         99065 - 99066
Q    H*                                                 RADI2   70010 - 79999
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
     H*                                                 PHYS2   97000 - 97999
     H*****************************************************************
     F*
     FFACAPKY   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALKY   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFACAL1KY  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK3)
     FFACAL2KY  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK4)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLKY   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLKY   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK
     FFDCALH    UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0F)
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
     D DAT             S              7  0 DIM(7)                               ARRAY FOR 7 DAYS
     D*** END   CHANGE ********* 11/18/97 ********************************
     D XDAT            S              2  0 DIM(7)                               ARRAY FOR 7 DAYS
     D*
     D*** BEGIN ADD *** 11/04/97 **************************************
     D*
     D*   SETTING UP COUNTER FOR SURG 2
     D CNT             S              1  0 DIM(10)
     D*
     D*** END ADD ***** 11/04/97 **************************************
     D*
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D SUR1            S              5    DIM(363) CTDATA PERRCD(1)            NO CUT/MAJOR-MIN
     D SUR2            S              5    DIM(363) ALT(SUR1)
     D PHY1            S              5    DIM(32) CTDATA PERRCD(1)             PHYS MAJOR/MINOR
     D PHY2            S              5    DIM(32) ALT(PHY1)
     D*
     D SEPP            S              5    DIM(269) CTDATA PERRCD(1)            SEPARATE PROCEDURE
     D NC1             S              5    DIM(102) CTDATA PERRCD(1)            NO CUT ON THESE
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

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
     IPCA0F
     I              CACO#                       PXCO#
     I              CAFND                       PXFND
     I              CAFYR                       PXFYR
     I              CACASE                      PXCASE
     I              CACSCD                      PXCSCD
     I              CAFDAT                      PXFDAT
     I              CATDAT                      PXTDAT
     I              CARDAT                      PXRDAT
     I              CAIPOP                      PXIPOP
     I              CAFEIN                      PXFEIN
     I              CAMOD                       PXMOD
     I              CAPRTY                      PXPRTY
     I              CATRCD                      PXTRCD
     I              CASVCD                      PXSVCD
     I              CAPMOD                      PXPMOD
     I              CACPT4                      PXCPT4
     I              CAICD9                      PXICD9
     I              CATOTA                      PXTOTA
     I              CAEOMB                      PXEOMB
     I              CADSEQ                      PXDSEQ
     I              CADELF                      PXDELF
     I              CAFLAG                      PXFLAG
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CAPART                      PXPART
     I              CAP1                        PXP1
     I              CASS#                       PXSS#
     I              CAACDT                      PXACDT
     I              CASTAT                      PXSTAT
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CATIME                      PXTIME
     I              CADT                        PXDT
     I              CAIUSR                      PXIUSR
     I              CAIDT                       PXIDT
     I              CADXR                       PXDXR
     I              CADCS#                      PXDCS#
     I              CALTR#                      PXLTR#
     I              CARCFL                      PXRCFL
     I              CASUB#                      PXSUB#
     I              CAOTHR                      PXOTHR
     I              CANDIS                      PXNDIS
     I              CARDTP                      PXRDTP
     I              CABRSV                      PXBRSV
     I              CABRPM                      PXBRPM
     I              CABRPC                      PXBRPC
     I              CABRAM                      PXBRAM
     I              CABRUN                      PXBRUN
     I              CABRTC                      PXBRTC
     I              CAZIPA                      PXZIPA
     I              CAAST                       PXAST
     I              CADIFF                      PXDIFF
     I              CAIPGM                      PXIPGM
     I              CACHRG                      PXTOTC
     I              CAMRA                       PXTOTD
     I              CAOTH2                      PXOTH2
     I              CAPAID                      PXTOTP
     I              CARCN                       PXRCN
     I              CALIN#                      PXLIN#
     I              CARULE                      PXRULE
     I              CAOT1C                      PXOT1C
     I              CAOT2C                      PXOT2C
     I              CAMRAA                      PXMRAA
     I              CAITIM                      PXITIM
     I              CAMNAA                      PXMNAA
     I* Record Format for SURG2 Workfile FACALKY
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
     I* Record Format for PHYS2 Workfile FACAL1KY
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
     I* Record Format for RADI2 Workfile FACAL2KY
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

     C*
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
     C*** Used with PCA08 (FDCAL8) in SURG2/PHYS2/RADI2
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
     C*** Used with PCA09 (FDCAL9) in SURG2/PHYS2/RADI2
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
     C*** Used with PCA09 (FDCAL9) in SURG2/PHYS2/RADI2
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
      *
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
     C     *LIKE         DEFINE    CAPART        HLPART
     C     *LIKE         DEFINE    CAPART        HLDPRT
     C     *LIKE         DEFINE    CATOTA        HLTOTA
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    PRMRA         HLDMRA
     C     *LIKE         DEFINE    CAANMN        XGT240
     C     *LIKE         DEFINE    CAANMN        XLE240
     C     *LIKE         DEFINE    CACPT4        XXSMCD
     C     *LIKE         DEFINE    CACPT4        ZZCPT4
     C*
     C     *LIKE         DEFINE    CACPT4        PPCPT4
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
     C     *LIKE         DEFINE    PRMRA         SAVMRA
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
     C********** *LIKE     DEFN CAFDAT    XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    PRMRA         XTMRA
     C     *LIKE         DEFINE    CAPART        XXPART
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
     C     *LIKE         DEFINE    PRMRA         X1MRA
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C********   *LIKE     DEFN CAFDAT    WKFDAT NEEDS TO BE 8
     C     *LIKE         DEFINE    CAFDAT        XXPAST
     C     *LIKE         DEFINE    CAFDAT        NEWPST
     C     *LIKE         DEFINE    CAFDAT        XXCHK
     C     *LIKE         DEFINE    CAFDAT        XXFUTR
     C*** END   CHANGE ********* 11/18/97 ********************************
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             Y                 5 0
     C                   Z-ADD     0             I                 3 0
     C                   Z-ADD     0             P                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             COUNT1            5 0
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          CAANMN for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   Z-ADD     0             H1ANTV            3 0
     C                   Z-ADD     0             XXANTV            5 2          Anes Time Value
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      ' '           XFIRST            1            First time Wrkf buil
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
     C                   Z-ADD     *ZEROS        SRDTE             8 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C*
     C                   Z-ADD     40.43         XXMEDI            5 2           MEDI CONVERSION
     C                   Z-ADD     29.16         XXANES            5 2           ANES CONVERSION
     C                   Z-ADD     59.94         XXSURG            5 2           SURG CONVERSION
     C                   Z-ADD     40.43         XXRADI            5 2           RADI CONVERSION
     C                   Z-ADD     40.43         XXPATH            5 2           PATH CONVERSION
     C                   Z-ADD     40.43         XXPHYS            5 2           PHYS CONVERSION
     C                   Z-ADD     43.20         XXEVAL            5 2           E/M CONVERSION
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
     C*
     C                   Z-ADD     QQACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
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
     C     CAFDAT        IFGE      0970201
     C     CAFDAT        ANDLT     0990101
     C     XXRCN         ANDEQ     CARCN
     C*
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C                   MOVE      '0'           OFCPD
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DHUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C                   MOVE      'N'           EDT400            1            EDIT pg 400
     C*
     C                   MOVE      QQSPCD        SVSPCD
      *
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
     C*** Do on each date brought in ********************************
     C*
     C***  XPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
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
      *
     C     DHMOD1        IFNE      *BLANKS
     C     DHMOD2        ORNE      *BLANKS
     C     DHMOD3        ORNE      *BLANKS
     C     DHMOD4        ORNE      *BLANKS
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
     C  N60              Z-ADD     30            Q
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
     C  N60              Z-ADD     30            Q
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
     C   60              MOVE      '210'         XPEOMB
     C* EOMB Code 210: "Charges exceed Reasonable & Customary"
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
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     C* Work conditioning is required for work conditioning, work
     c* hardening and work conditioning services 97545,and 97546.
     c*
     C     CAFDAT        IFGE      960801
     C*
     C     DHPRCD        IFEQ      '97545'                                      RANGE CHECK
     C     DHPRCD        OREQ      '97546'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EV'          QQC(Q)
     C*** Err Msg EV: "Pre-certification is required"
     C                   ENDIF
     C                   ENDIF
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
     C*   If modifier exists check the code through the Modifier Subr
      *
     C     DHMOD1        IFNE      *BLANKS
     C     DHMOD2        ORNE      *BLANKS
     C     DHMOD3        ORNE      *BLANKS
     C     DHMOD4        ORNE      *BLANKS
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
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'S'
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    END XXEND EQ '99'
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '99201'                                      EVAL/MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      EVAL
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        ORGE      '02000'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '02020'
     C                   EXSR      ANES
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69999'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99065'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '99066'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   EXSR      PHYS2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   END
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0F                              61
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
     C*** BEG CHG *** 07/29/99 ****************************************
     C******************** MOVELPXDSEQ    QQS,Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C*** END CHG *** 07/29/99 ****************************************
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
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
      *
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C*** BEG ADD *** 02/03/00 ****************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C                   UPDATE    PCA01
     C*** END ADD *** 02/03/00 ****************************************
     C                   END
     C*
     C*** BEG DEL *** 02/03/00 ****************************************
     C****BEGIN*ADD*****09/04/98 **************************************
     C******************** Z-ADDXXMRA     CAMRAA
     C****END*ADD*****09/04/98 ****************************************
     C******************** UPDATPCA01
     C*** END DEL *** 02/03/00 ****************************************
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
7    C*
     C*   Before next record is read, save the key fields needed to
     C*   retrieve the physical therapy records to be updated in the
     C*   PHYS2 routine which is executed on a provider level break...
     C                   Z-ADD     CADSEQ        PTDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        PTFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         PTMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        PTPRTY
     C                   Z-ADD     CAEOB#        PTEOB#
     C                   Z-ADD     CACHK#        PTCHK#
     C*                                                      TO PROCESS
     C                   ENDIF
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*
     C                   ENDDO
     C*
     C*** Execute SURG2/PHYS2 if a SURG/PHYS2 Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   EXSR      PHYS2                                        Process the PHYS cod
     C                   EXSR      RADI2                                        Process the RADI cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*****************************************************************
     C* THE FOLLOWING CODE WAS DUPLICATED TO CATCH ERRORS NOT FOUND
     C* ON FIRST PASS OF DATA.
     C*****************************************************************
     C*  DUP OF MAIN CODE !!
     C                   MOVE      ' '           XFIRST
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '2'           XXPASS            1
     C*
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
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
     C     CAFDAT        IFGE      0970201
     C     CAFDAT        ANDLT     0990101
     C     XXRCN         ANDEQ     CARCN
     C*
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C                   MOVE      '0'           OFCPD
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DHUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C                   MOVE      'N'           EDT400            1            EDIT pg 400
     C*
     C                   MOVE      QQSPCD        SVSPCD
      *
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
     C     NXRC2         TAG
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
     C                   GOTO      NXTRC2
     C*
     C                   ELSE                                                   ELSE XPPMOD NE *BLAN
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2
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
     C                   GOTO      NXRC2
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2
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
     C                   GOTO      NXTRC2
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
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     CACHRG        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C   60              MOVE      '210'         XPEOMB
     C* EOMB Code 210: "Charges exceed Reasonable & Customary"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
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
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     C* Work conditioning is required for work conditioning, work
     c* hardening and work conditioning services 97545,and 97546.
     c*
     C     CAFDAT        IFGE      960801
     C*
     C     DHPRCD        IFEQ      '97545'                                      RANGE CHECK
     C     DHPRCD        OREQ      '97546'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EV'          QQC(Q)
     C*** Err Msg EV: "Pre-certification is required"
     C                   ENDIF
     C                   ENDIF
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
     C*
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
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
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
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC2                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'S'
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    END XXEND EQ '99'
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '99201'                                      EVAL/MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      EVAL
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        ORGE      '02000'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '02020'
     C*** BEGIN ADD *** 07/16/97 **************************************
     C** SUPPORT: #8105  PAY CODES 99100,99116,99135,99140 SAME AS ANES
     C     DHPRCD        OREQ      '99100'                                      ANESTHESIA
     C     DHPRCD        OREQ      '99116'
     C     DHPRCD        OREQ      '99135'                                      ANESTHESIA
     C     DHPRCD        OREQ      '99140'
     C*** END ADD ***** 07/16/97 **************************************
     C                   EXSR      ANES
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69999'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99065'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '99066'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   EXSR      PHYS2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACKY'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL2KY
     C                   OPEN      FACAL1KY
     C                   OPEN      FACALKY
     C                   OPEN      FACAPKY
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
     C                   END
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0F                              61
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
     C*** BEG CHG *** 07/29/99 ****************************************
     C******************** MOVELPXDSEQ    QQS,Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C*** END CHG *** 07/29/99 ****************************************
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
     C*** BEG ADD *** 02/03/00 ****************************************
     C     CAOTH2        ANDEQ     0
     C*** END ADD *** 02/03/00 ****************************************
     C     CANDIS        ANDEQ     0
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
     C*** BEG ADD *** 02/03/00 ****************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C                   UPDATE    PCA01
     C*** END ADD *** 02/03/00 ****************************************
     C                   END
     C*
     C*** BEG DEL *** 02/03/00 ****************************************
     C****BEGIN*ADD*****09/04/98 **************************************
     C******************** Z-ADDXXMRA     CAMRAA
     C****END*ADD*****09/04/98 ****************************************
     C******************** UPDATPCA01
     C*** END DEL *** 02/03/00 ****************************************
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRC2        TAG
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
     C                   ENDIF
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
     C*
     C*** Execute SURG2/PHYS2 if a SURG/PHYS2 Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   EXSR      PHYS2                                        Process the PHYS cod
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
     C                   CLOSE     FACAL2KY
     C                   CLOSE     FACAL1KY
     C                   CLOSE     FACALKY
     C                   CLOSE     FACAPKY
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL2KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACALKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKY
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C*
     C* Write the adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** Anesthesia     Codes: 00100 - 01999 02000 - 02020       A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per ANES, pg. 2-4
     C     XPPMOD        IFNE      'P1'
     C     XPPMOD        ANDNE     'P2'
     C     XPPMOD        ANDNE     'P3'
     C     XPPMOD        ANDNE     'P4'
     C     XPPMOD        ANDNE     'P5'
     C     XPPMOD        ANDNE     'P6'
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '33'
     C     XPPMOD        ANDNE     '35'
     C     XPPMOD        ANDNE     '37'
     C     XPPMOD        ANDNE     '38'
     C     XPPMOD        ANDNE     '39'
     C     XPPMOD        ANDNE     '47'
     C     XPPMOD        ANDNE     '49'
     C     XPPMOD        ANDNE     '51'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   ELSE                                                   ELSE NE P1...
     C                   EXSR      PMOD
     C                   END                                                    END NE P1...
     C                   END                                                    END NE *BLANK
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
     C*
     C*================================================================
     C* PER PAGE 35, PROBLEM #4
     C* Separate or Multiple Procedures:  It is appropriate to designate
     C* multiple procedures that are rendered at the same time session by
     C* separate entries.
     C*
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '02020'
     C*
     C                   MOVEL     '00100'       XXCPT4
     C     CAKEY8        SETLL     PCA09                              60
     C     CAKEY7        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '02020'
     C*
     C*  USING CALCULATION TO DETERMINE WHICH IS THE HIGHEST MRA TO PAY
     C*
     C                   Z-ADD     *ZEROS        HLANTV
     C                   Z-ADD     *ZEROS        H1ANTV
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof under 4 hours
     C     DHUNTS        DIV       15            HLANMN           10 7
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C*** BEGIN DEL *** 07/30/97 *************************************
     C**********           MVR            HLADDT  77
     C**********           ADD  HLANMN    HLADDT
     C********** HLANTV    MULT 15        XXANTV
     C********** CAANMN    SUB  XXANTV    XXANTV
     C********** XXANTV    IFGT 5
     C*** END DEL ***** 07/30/97 *************************************
     C                   Z-ADD     HLANMN        HLANTV            3 0          Anes Time Value
     C                   Z-ADD     HLANMN        HLADDT            7 7          Anes Time Value
     C     HLADDT        IFGE      .3                                           ADD 07/30/97
     C                   ADD       1             HLANTV
     C                   END
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C     SXANAM        ADD       HLANTV        XXWORK           10 7
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C     XRTOTP        IFGT      SXMRA
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
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  60
     C                   END                                                    END DO
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C* Calculate ARA
     C* Per ANES pg. 1
     C* User will have entered total minutes, we must now convert those
     C* minutes to TIME VALUE UNITS
     C*
     C                   Z-ADD     *ZEROS        HLANTV
     C                   Z-ADD     *ZEROS        H1ANTV
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof under 4 hours
     C     DHUNTS        DIV       15            HLANMN           10 7
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C*** BEGIN DEL *** 07/30/97 *************************************
     C**********           MVR            HLADDT  77
     C**********           ADD  HLANMN    HLADDT
     C********** HLANTV    MULT 15        XXANTV
     C********** CAANMN    SUB  XXANTV    XXANTV
     C********** XXANTV    IFGT 5
     C*** END DEL ***** 07/30/97 *************************************
     C                   Z-ADD     HLANMN        HLANTV            3 0          Anes Time Value
     C                   Z-ADD     HLANMN        HLADDT            7 7          Anes Time Value
     C     HLADDT        IFGE      .3                                           ADD 07/30/97
     C                   ADD       1             HLANTV
     C                   END
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C     SXANAM        ADD       HLANTV        XXWORK           10 7
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
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
     *** Evaluation/Management      Codes: 99201 - 99499         E V A L *
     *********************************************************************
     C     EVAL          BEGSR
     C*
     C     SXMRA         MULT      XXEVAL        SXMRA
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
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '57'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   ELSE                                                   ELSE EQ 21...
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
     C                   END                                                    END EQ 21...
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* =================================================================
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
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
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
     C* ===============================================================
     C* PER P. 33 PROBLEM 1
     C* The reporting of anesthesia services is appropriate by or under the
     c* responsibe supervision of a physician.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   MOVEL     '00100'       XXCPT4
     C*
     C     CAKYA         SETLL     PCA0A
     C     CAKYA         READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '00100'
     C     XACPT4        ANDLE     '02020'
     C     XADSEQ        ANDNE     CADSEQ
     C     XATOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END                                                    END XACPT4
     C     CAKYA         READE     PCA0A                                  60
     C                   ENDDO
     C*
     C                   END                                                    END DHPRCD
     C*==================================================================
     C*** Per SURG pg. 2
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
     C                   MOVE      '211'         XPEOMB
     C*** EOMB 211: "Reimbursement made for an office visit with a starred(*)
     C***            procedcure."
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
     C                   MOVE      *BLANK        SX1A1
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
     C     SXMRA         MULT      XXSURG        SXMRA
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
     C* Per SURG pg. 2
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
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   ELSE
     C                   GOTO      AGAIN2
     C                   END
     C*
     C                   END
     C*
     C                   END
     C* ===============================================================
     C* PER P. 4
     C* An Initial evaluation and management code is allowed for the
     c* initial assessment and evaluation of the patient.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C*
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKYA         SETLL     PCA0A
     C     CAKYA         READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '99201'
     C     XACPT4        ANDLE     '99215'
     C     XADSEQ        ANDNE     CADSEQ
     C     XATOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '212'         XPEOMB
     C* EOMB Code 212: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END                                                    END XACPT4
     C     CAKYA         READE     PCA0A                                  60
     C                   ENDDO
     C*
     C                   END                                                    END DHPRCD
     C* ===============================================================
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     C* Evaluation and management codes which may be allowed for follow
     C* up established visits when the above conditions apply re 99211-99214
     C*
     C     CAFDAT        IFGE      960801
     C*** NOTE: CODE RANGE 97010 - 97145 INCLUDED IN THIS RANGE
     C*** (PER SH)
     C*
     C     DHPRCD        IFGE      '99211'                                      MANIPULATION
     C     DHPRCD        ANDLE     '99214'
     C                   MOVE      '97010'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*** BEGIN CHG *** 08/05/97 **************************************
     C********** *IN60     DOWEQ'0'
     C     *IN60         IFEQ      '0'
     C*** END CHG ***** 08/05/97 **************************************
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97145'
     C     XRTOTP        IFGT      0
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FL'          QQC(Q)
     C* Err Msg FL: "Follow up visit with PT, Verify medical necessity."
     C*
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C*
     C*** BEGIN DEL *** 08/05/97 **************************************
     C********** CAKY9A    READEPCA09                    60
     C*** END DEL ***** 08/05/97 **************************************
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99211
     C                   ENDIF                                                  END EFF. DATE
     C*
     C* ===============================================================
     C* PAGE 55, #2
     C* DISALLOW DURING FOLLOW UP DAYS
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
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
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '99201'
     C*
     C*** BEGIN ADD *** 10/01/97 **************************************
     C* ====================================================================
     C*  PER PG 400 #1
     C*  If a starred surgical code is billed, with a code from 99211
     C*  to 99215 (mod -25), Disallow codes 99211-99215
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     XPPMOD        ANDEQ     '25'
     C     XXPASS        ANDEQ     '2'
     C*
     C                   MOVEL     '10040'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        ANDGE     '10040'
     C     XRCPT4        ANDLE     '64979'
     C*
     C     EDT400        ANDEQ     'Y'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C     CAKEY7        READE     PCA09                                  54
     C                   ENDDO
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ====================================================================
     C*** END ADD ***** 10/01/97 **************************************
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
     C*=====================================================================
     C*
     C     SXMRA         MULT      XXANES        SXMRA
     C*
     C*=====================================================================
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
     C*
     C* 99025 IS USED IF INITIAL OFFICE VISIT IS ENTERED WITH A STARRED PROCEDUR
     C* IF THIS IS THE CASE THE OFFICE VISIT IS NOT PAID BUT AN ADDITIONAL .15
     C* SURGICAL UNITS IS REIMBURSED.  ISTEAD OF ADDING THIS .15 TO THE SURGERY
     C* CODE WE WILL PAY 99025 AT .15 UNITS MULTIPLIED BY THE SURGICAL CONVERSIO
     C* FACTOR.
     C*
     C*** BEGIN DEL *** 10/01/97 **************************************
     C*********  DHPRCD    IFNE '99025'
     C*********  SXMRA     MULT XXMEDI    SXMRA
     C*********            ELSE
     C*********  SXMRA     MULT XXSURG    SXMRA
     C*********            END
     C*** END DEL   *** 10/01/97 **************************************
     C*
     C     SXMRA         MULT      XXMEDI        SXMRA                          ADD 10/01/97
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per MEDI, pg. 2-3
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* PER P. 33 PROBLEM 1
     C* The reporting of anesthesia services is appropriate by or under the
     c* responsibe supervision of a physician.
     C*
     C     DHPRCD        IFGE      '93000'
     C     DHPRCD        ANDLE     '94799'
     C*
     C                   MOVE      '00100'       XXCPT4
     C*
     C     CAKYA         SETLL     PCA0A
     C     CAKYA         READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '00100'
     C     XACPT4        ANDLE     '02020'
     C     XADSEQ        ANDNE     CADSEQ
     C     XATOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END XACPT4
     C     CAKYA         READE     PCA0A                                  60
     C                   ENDDO
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C* PAGE 55, PROBLEM 2
     C* Listed Surgical Procedures:                                    or
     C* These include the ooperation per se, local infiltration,       n,
     C* metacarpal/digital block or topical anesthesia when used, and  or
     C* the normal, uncomplicated follow -up care.  This concept is
     C* referred to as a "Package" for surgical procedures.  To report 0502
     C* a postoperative follow-up visit for documentation purposes onlye
     C* use 99024.
     C*
     C     DHPRCD        IFEQ      '99070'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        IFGT      0
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99070
     C*
     C*
     C* =================================================================
     C* 99030 IS A MILEAGE CODE ... WILL USE ANE/MI/HR/RNT AMT
     C*
     C     DHPRCD        IFEQ      '99030'
     C     SXANAM        MULT      SXMRA         SXMRA
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
     C* SPECIAL SERVICES AND BILLING PROCEDURES PG 5 & 6 THESE SERVICES
     C* DEPENDING ON CODE WILL BE MULTIPLIED BY MEDICAL, PATHOLOGICAL,
     C* RADIOLOGICAL OR SURGICAL CONVERSIUON FACTORS
     C*
     C     DHPRCD        IFGE      '99007'
     C     DHPRCD        ANDLE     '99026'
     C     SXMRA         MULT      XXPATH        SXMRA
     C*
     C                   ELSE                                                   ELSE DHPRCD EQ 99007
     C*
     C     DHPRCD        IFGE      '99027'
     C     DHPRCD        ANDLE     '99054'
     C*
     C     DHPRCD        IFEQ      '99030'
     C     DHPRCD        OREQ      '99039'
     C     DHPRCD        OREQ      '99040'
     C                   Z-ADD     DHUNTS        HLANMN
     C     HLANMN        MULT      SXANAM        SXMRA
     C     SXMRA         MULT      XXMEDI        SXMRA
     C                   ELSE                                                   ELSE DHPRCD EQ 99030
     C     SXMRA         MULT      XXMEDI        SXMRA
     C                   END                                                    END DHPRCD EQ '99030
     C*
     C                   ELSE                                                   ELSE DHPRCD GE 99027
     C*
     C     DHPRCD        IFGE      '99065'
     C     DHPRCD        ANDLE     '99095'
     C     SXMRA         MULT      XXRADI        SXMRA
     C                   END                                                    END DHPRCD GE '99065
     C                   END                                                    END DHPRCD GE '99027
     C                   END                                                    END DHPRCD GE '99007
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
     ***                                   99007 - 99023                 *
     ***                                   99026 - 99026                 *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*=====================================================================
     C     SXMRA         MULT      XXPATH        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PATH, pg. 4
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C     XPPMOD        OREQ      '27'
     C     SXPROF        ANDEQ     *ZEROS
     C*
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END NE 22
     C                   END                                                    END EQ 26
     C*
     C                   EXSR      PMOD
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* PAGE 344, PROBLEM 12
     C* REVIEW OF DIAGNOSTIC STUDIES: No separate charge is warranted for
     C* prior studies reviewed in conjunction with a visit, consultation,
     C* record review, or other evaluation by the medical practitioner or
     C* other medical personnel, and neither the professional component
     C* value modifier -26 no the pathologyc consultation codes 80500&80502
     C* reimbursable.  The review of Diagnostic tests is included in the
     C* Evaluation and Management codes.
     C*
     C*
     C     DHPRCD        IFGE      '80002'
     C     DHPRCD        ANDLE     '80549'
     C     XPPMOD        ANDEQ     '26'
     C     DHPRCD        OREQ      '80501'
     C     XPPMOD        ANDEQ     '26'
     C     DHPRCD        ORGE      '80551'
     C     DHPRCD        ANDLE     '89399'
     C     XPPMOD        ANDEQ     '26'
     C     DHPRCD        OREQ      '80500'
     C     XPPMOD        ANDEQ     '  '
     C     DHPRCD        OREQ      '80502'
     C     XPPMOD        ANDEQ     '  '
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99499'
     C     XRTOTP        IFGT      0
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 80002
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
     C     PATHE         ENDSR
     *********************************************************************
     *** Physical Medicine          Codes: 97000 - 97999         P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C     SXMRA         MULT      XXPHYS        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PHYS, pg. 2-3
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
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
     C* ADDITIONAL MINUTES
     C     DHPRCD        IFEQ      '97101'
     C     DHPRCD        OREQ      '97201'
     C     DHPRCD        OREQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97531'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97701'
     C     DHPRCD        OREQ      '97706'
     C     DHPRCD        OREQ      '97709'
     C     DHPRCD        OREQ      '97721'
     C     DHPRCD        OREQ      '97741'
     C*
     C* Disallow any additional minutes code that doesn't have the proper
     C* major code associated with it
     C*
     C     DHPRCD        IFEQ      '97101'
     C                   MOVE      '97100'       XXCPT4
     C                   END
     C*
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
     C     DHPRCD        IFEQ      '97706'
     C                   MOVE      '97705'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97709'
     C                   MOVE      '97708'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97721'
     C                   MOVE      '97720'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97741'
     C                   MOVE      '97740'       XXCPT4
     C                   END
     C*
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   END                                                    END EQ 97221, ETC.
     C*
     C* FREQUENCY OF TREATMENT : Except as justified by report, no more
     C* than three treatments are to be rendered within a period 7 days
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     C*
     C     CAFDAT        IFGE      960801
     C*
     C     DHPRCD        IFGE      '97010'                                      RANGE CHECK
     C     DHPRCD        ANDLE     '97752'
     C                   MOVE      DHPRCD        XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      CAFDAT        XXFUTR
     C                   MOVE      CAFDAT        XXCHK
     C* FIND DATE 7 DAYS PRIOR
     C                   Z-ADD     6             SRPST
     C                   Z-ADD     CAFDAT        SRDTI
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
     C*
     C*** FIND THE FIRST DATE THAT FALLS WITHIN THE 14 DAY WINDOW
     *** BEG CHG *** 11/19/98 *****************************************
     C******************** MOVE SRDTE     XXPAST
     C******************** Z-ADDXXPAST    SRDTI
     C                   Z-ADD     SRDTE         SRDTI
     *** END CHG *** 11/19/98 *****************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C                   MOVE      XXFUTR        NEWPST
     C     CAKYA         SETLL     PCA0A
     C     CAKYA         READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C     XACPT4        IFGE      '97010'
     C     XACPT4        ANDLE     '97752'
     C     XAFDAT        ANDGE     XXPAST
     C     XAFDAT        ANDLT     XXFUTR
     C     XAFDAT        ANDLT     NEWPST
     C                   MOVE      XAFDAT        NEWPST
     C                   ENDIF
     C     CAKYA         READE     PCA0A                                  60
     C                   ENDDO
     C                   MOVE      NEWPST        XXPAST
     C                   MOVE      XXPAST        DAT(1)
     C* FIND DATE 7 DAYS FUTURE OF PAST DATE
     C                   Z-ADD     6             SRFUT
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRFUTR'
     C                   CALL      'SRFUTR8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR
     C* SETTING UP ARRAY OF 7 TO HOLD 7 INDEPENDENT DAYS
     C                   Z-ADD     1             Y
     C                   DO        6             X
     C* FIND THE 7 INDEPENDENT DAYS
     C                   Z-ADD     1             SRFUT
     C                   Z-ADD     DAT(X)        SRDTI
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
     C     X             ADD       1             Y
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAT(Y)
     C                   ENDDO
     C***  ZERO OUT COUNTERS FOR DATES
     C                   CLEAR                   XDAT
     C*                    Z-ADD*ZEROS    XDAT,1
     C*                    Z-ADD*ZEROS    XDAT,2
     C*                    Z-ADD*ZEROS    XDAT,3
     C**                   Z-ADD*ZEROS    XDAT,4
     C*                    Z-ADD*ZEROS    XDAT,5
     C*                    Z-ADD*ZEROS    XDAT,6
     C*                    Z-ADD*ZEROS    XDAT,7
     C***  READS ALL CPT4 CODES IN SPECIFIC RANGE AND INCREMENTS
     C***  PROPER COUNTER DETERMINED BY THE DAY
     C     CAKYA         SETLL     PCA0A
     C     CAKYA         READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C     XACPT4        IFGE      '97010'
     C     XACPT4        ANDLE     '97752'
     C     CADSEQ        ANDNE     XADSEQ
     C     XATOTP        ANDNE     *ZEROS
     C                   SELECT
     C     XAFDAT        WHENEQ    DAT(1)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(1)
     C     XAFDAT        WHENEQ    DAT(2)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(2)
     C     XAFDAT        WHENEQ    DAT(3)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(3)
     C     XAFDAT        WHENEQ    DAT(4)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(4)
     C     XAFDAT        WHENEQ    DAT(5)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(5)
     C     XAFDAT        WHENEQ    DAT(6)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(6)
     C     XAFDAT        WHENEQ    DAT(7)
     C     XAFDAT        ANDNE     XXCHK
     C                   ADD       1             XDAT(7)
     C                   ENDSL
     C                   ENDIF                                                  END DHPRCD 97010
     C     CAKYA         READE     PCA0A                                  60
     C                   ENDDO                                                  END LOOP FOR 7 DAYS
     C                   Z-ADD     *ZEROS        COUNT
     C                   DO        7             X
     C     XDAT(X)       IFGE      1
     C                   ADD       1             COUNT
     C                   ENDIF                                                  END COUNTER
     C                   ENDDO
     C***  CHECK IF DAYS ADD UP TO MORE THAN 2, IF THEY DO DON'T PAY
     C***  ELSE IT FALLS OUT.
     C     COUNT         IFGE      3
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GO'          QQC(Q)
     C*** Err Msg FE: "Only 3 visits in 7 days.  Written report
     C***              required"
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '213'         XPEOMB
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*                    END                             END CHK#/EOB#
     C                   END                                                    END COUNTER
     C                   ENDIF                                                  END RANGE CHECK
     C                   ENDIF                                                  END EFF. DATE
     C* ===============================================================
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     c* Tests and Measurements :  Test and measurement codes are includ
     c* in the value of an evaluation and management service when
     c* performed on the same day as test and meas. services.
     C     CAFDAT        IFGE      960801
     C*** NOTE: CODE RANGE 99201 - 99499 INCLUDED IN THIS RANGE
     C*** (PER SH)
     C*
     C     DHPRCD        IFGE      '97703'                                      MANIPULATION
     C     DHPRCD        ANDLE     '97750'
     C                   MOVE      '99201'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99499'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FM'          QQC(Q)
     C*** Err Msg FM: "Test and Measurements are included in the
     C***              value of Eval and Management"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END DHPRCD 97260
     C                   ENDIF                                                  END EFF. DATE
     C* ===============================================================
     C* Pg. 32
     C* 97221 MAXIMUM ALLOWED TIME IS ONE HOUR
     C*
     C     DHPRCD        IFEQ      '97221'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97221'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        ANDEQ     '97221'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFNE      *ZEROS
     C                   ADD       1             COUNT
     C                   END
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGT      3
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END COUNT GT 3
     C*
     C                   END                                                    END DHPRCD EQ 97221
     C*
     C* ===============================================================
     C* Pg. 32
     C* 97241 MAXIMUM ALLOWED TIME IS ONE HOUR
     C*
     C     DHPRCD        IFEQ      '97241'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97241'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        ANDEQ     '97241'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFNE      *ZEROS
     C                   ADD       1             COUNT
     C                   END
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGT      3
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END COUNT GT 3
     C*
     C                   END                                                    END DHPRCD EQ 97241
     C* ===============================================================
     C* Per PHYS, pg. 4
     C* Will only need to do processing in workfile if XPPMOD is
     C*          51 = MULTIPLE PROCEDURES
     C* AND DHPRCD ne 97260 or 97261
     C*
     C     CAFDAT        IFLE      960801
     C     XPPMOD        IFEQ      '51'
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPKY Workfile
     C* Only load "current charges" into the FACAPKY Workfile
     C*
     C     DHPRCD        IFNE      '97260'
     C     DHPRCD        ANDNE     '97261'
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     SXMRA         CAMRA
     C*
     C                   MOVE      'P'           CATYPE
     C                   WRITE     PCAWK1
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END XPPMOD EQ '51'
     C*
     C                   END                                                    ENDNE 97260/97261
     C                   ENDIF                                                  ENDGE 08/01/96
     C*
     C*   If current code exists in table, must have associated code else
     C*     error has occurred
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVE      '0'           *IN54
     C     PHYLOK        TAG
     C     DHPRCD        LOOKUP    PHY1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      PHY2(R)       XXCPT4
     C     CAKY9         CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C     R             IFLT      32
     C                   ADD       1             R
     C                   GOTO      PHYLOK
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C*
     C                   END
     C                   ELSE
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C                   END
     C*
     C* ===============================================================
     C* ===============================================================
     C* ADJUDICATE IF DATE GREATER THAN 08/01/96
     C* DETERMINE CAPACITY OF 4 OR TOTAL TIME OF ONE HOUR
     C     CAFDAT        IFGE      960801                                        EFF. DAT 08/01/96
     C                   Z-ADD     0             XXTOT             2 0
     C                   Z-ADD     0             XXMIN             3 0
     C                   Z-ADD     0             XXSL              2 0
     C*SETTING CPT4 RANGES
     C*
     C     DHPRCD        IFEQ      '97500'
     C     DHPRCD        OREQ      '97520'
     C*
     C                   Z-ADD     30            XXMIN             3 0          ADDING 30 TO TOTAL
     C                   Z-ADD     1             XXSL                              TIME
     C*
     C                   ELSE
     C*
     C     DHPRCD        IFGE      '97032'                                      DETERMINE 15 MIN COD
     C     DHPRCD        ANDLE     '97036'
     C     DHPRCD        ORGE      '97110'                                      DETERMINE 15 MIN COD
     C     DHPRCD        ANDLE     '97139'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97530'
     C     DHPRCD        OREQ      '97535'
     C     DHPRCD        OREQ      '97537'
     C     DHPRCD        OREQ      '97542'
     C     DHPRCD        OREQ      '97703'
     C     DHPRCD        OREQ      '97750'
     C     DHPRCD        OREQ      '97770'
     C*
     C*** BEGIN CHG *** 07/16/97 **************************************
     C* SUPPORT #8050 -- CORRECT TIME UNIT FOR CODES AS OF 2/1/97 IS 15
     C                   Z-ADD     15            XXMIN             3 0          ADDING 15 TO TOTAL
     C**********           Z-ADD30        XXMIN   30       ADDING 30 TO TOTAL
     C*** END CHG ***** 07/16/97 **************************************
     C                   Z-ADD     1             XXSL                              TIME
     C                   ELSE
     C*
     C     DHPRCD        IFGE      '97010'                                      DETERMINE NON TIMED
     C     DHPRCD        ANDLE     '97028'                                      CODE
     C     DHPRCD        OREQ      '97150'                                      CODE
     C     DHPRCD        OREQ      '97250'
     C     DHPRCD        OREQ      '97260'
     C     DHPRCD        OREQ      '97261'
     C     DHPRCD        OREQ      '97265'
     C     DHPRCD        OREQ      '97799'
     C                   Z-ADD     1             XXSL                           SETTING TO PROCESS
     C*
     C                   ENDIF                                                  DHPRCD GE 97010
     C*
     C                   ENDIF                                                  DHPRCD GE 97110
     C*
     C                   ENDIF                                                  DHPRCD=97500
     C*
     C* ===============================================================
     C     XXSL          IFEQ      1                                            IF XXSL = 1 YOUR IN
     C     CAKY9A        SETLL     PCA09                                        PROCESSING RANGE
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XRTOTP        IFNE      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C     XRCPT4        IFEQ      '97500'
     C     XRCPT4        OREQ      '97520'
     C*
     C                   ADD       30            XXMIN             3 0          PREVIOUS MIN
     C                   ADD       1             XXTOT                          TOTAL UNITS
     C*
     C                   ELSE
     C*
     C     XRCPT4        IFGE      '97032'
     C     XRCPT4        ANDLE     '97036'
     C     XRCPT4        ORGE      '97110'
     C     XRCPT4        ANDLE     '97139'
     C     XRCPT4        OREQ      '97501'
     C     XRCPT4        OREQ      '97521'
     C     XRCPT4        OREQ      '97530'
     C     XRCPT4        OREQ      '97535'
     C     XRCPT4        OREQ      '97537'
     C     XRCPT4        OREQ      '97542'
     C     XRCPT4        OREQ      '97703'
     C     XRCPT4        OREQ      '97750'
     C     XRCPT4        OREQ      '97770'
     C*** BEGIN CHG *** 07/16/97 **************************************
     C* SUPPORT #8050 -- CORRECT TIME UNIT FOR CODES AS OF 2/1/97 IS 15
     C                   ADD       15            XXMIN             3 0          PREVIOUS MIN
     C**********           ADD  30        XXMIN   30       PREVIOUS MIN
     C*** END CHG ***** 07/16/97 **************************************
     C                   ADD       1             XXTOT                          TOTAL UNITS
     C                   ELSE
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97028'
     C     XRCPT4        OREQ      '97150'
     C     XRCPT4        OREQ      '97250'
     C     XRCPT4        OREQ      '97260'
     C     XRCPT4        OREQ      '97261'
     C     XRCPT4        OREQ      '97265'
     C     XRCPT4        OREQ      '97799'
     C                   ADD       1             XXTOT
     C                   ENDIF                                                   XRCPT4 GE 97010
     C                   ENDIF                                                   XRCPT4 GE 97110
     C                   ENDIF                                                   XRCPT4 EQ 97500
     C                   ENDIF                                                   XRTOTP NE 0
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO
     C     XXTOT         IFGE      4                                             TOTAL UNITS ALLOWED
     C                   EXSR      CPTERR                                        ARE 4
     C                   GOTO      PHYSE
     C                   ELSE
     C     XXMIN         IFGT      60                                            TOTAL TIME  ALLOWED
     C                   EXSR      CPTERR                                        IS 60 MIN
     C                   GOTO      PHYSE
     C                   ENDIF                                                   IF CPTSL =  S
     C                   ENDIF                                                   CPTTOT >=4
     C                   ENDIF                                                   XXSL=1
     C                   ENDIF                                                   EFF. DATE
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
     C     PHYSE         ENDSR
     C********************************************************************
     C** Process PHYS Codes for DIFFERENT PERCENTAGES          P H Y S 2 *
     C*  This subroutine is called at each date, provider level break    *
     C********************************************************************
     C     CPTERR        BEGSR
     C********************************************************************
     C*  This subroutine is called at each Error #54 and 204 EOMB        *
     C********************************************************************
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C                   EXSR      UPDAT
     C     CPTEER        ENDSR
     C     PHYS2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the PHYS Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C*
     C                   Z-ADD     0             COUNT
     C                   MOVE      *BLANKS       HLPART
     C     *LOVAL        SETLL     PCAWK3
     C                   READ      PCAWK3                                 60
     C     *IN60         IFEQ      '0'
     C* Read history to determine if a PHYS code has already been paid
     C                   Z-ADD     P3FDAT        PPFDAT
     C                   Z-ADD     P3FEIN        PPFEIN
     C                   Z-ADD     P3MOD         PPMOD
     C                   MOVE      '97000'       PPCPT4
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        ANDLE     '97999'
     C     XRTOTP        IFGT      *ZEROS
     C     XRPMOD        IFEQ      '51'
     C                   ADD       1             COUNT
     C                   END                                                    END EQ '51'
     C                   END                                                    END NE *ZEROS
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Per PHYS, pg. 4
     C*  Highest value 100% - HIGHEST WILL NOT INCLUDE MOD 51 SO IT WILL
     C*                       NOT BE INCLUDED IN THE WORKFILE
     C*  2nd            75%
     C*  3rd            50%
     C*  4th            25%
     C*  5th +          Message Review
     C*
     C     P3PMOD        IFEQ      '51'
     C*
     C     COUNT         IFLE      0
     C                   Z-ADD     .75           P3BRP                          75%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      1
     C                   Z-ADD     .50           P3BRP                          50%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFEQ      2
     C                   Z-ADD     .25           P3BRP                          25%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 2
     C     COUNT         IFGT      1
     C                   Z-ADD     0             P3BRP                          0%
     C                   ADD       1             COUNT
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      'BL'          QQC(Q)
     C* Err Msg BL: "For 5 or more Phys Med procedures, review
     C                   END                                                    END  GT 1
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   END                                                    END EQ '51'
     C*
     C     P3MRA         MULT      P3BRP         P3MRA
     C*
     C     P3TOTC        COMP      P3MRA                              54        >>>>>
     C   54              Z-ADD     P3MRA         P3TOTA
     C   54              Z-ADD     *ZEROS        P3DIFF
     C  N54              Z-ADD     P3TOTC        P3TOTA
     C  N54P3MRA         SUB       P3TOTA        P3DIFF
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C*
     C*  Using FACAPKY record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P3FDAT        PPFDAT
     C                   Z-ADD     P3FEIN        PPFEIN
     C                   Z-ADD     P3MOD         PPMOD
     C                   MOVE      P3CPT4        PPCPT4
     C                   Z-ADD     P3DSEQ        PPDSEQ
     C     CAKY8A        CHAIN     PCA08                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              62
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     P3TOTA        XRTOTA
     C                   Z-ADD     P3TOTD        XRTOTD
     C                   Z-ADD     P3TOTP        XRTOTP
     C                   MOVEL     P3PMOD        XRPMOD
     C     P3TOTD        IFGT      *ZERO
     C     P3TOTP        ANDGT     *ZEROS
     C                   MOVE      '202'         P3EOMB
     C* EOMB Code 202: "Charges exceed maximum allowance"
     C                   END                                                    END P3TOTD GT *ZERO
     C*
     C                   MOVE      P3EOMB        XREOMB
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
      *
     C                   UPDATE    PCA08
      *
     C* Set the sequence number key field to write the EOMB code
     C     P3EOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     P3DSEQ        KKDSEQ
     C                   MOVE      XREOMB        XPEOMB
     C                   EXSR      EOMB
      *
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   EXCEPT    ECA01
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   ENDIF                                                  *IN61 = OFF
     C*
     C                   READ      PCAWK3                                 60
     C*
     C                   END                                                    END DOWEQ 0 *IN60
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
     C*
     C* ===============================================================
     C* Modifier -21 = Prolonged E/M Service
     C     XPPMOD        IFEQ      '21'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the Justification Report is required for
     C***              this procedure."
     C                   END                                                    END EQ '21'
     C*
     C* ===============================================================
     C* Modifier -22 = Unusual Circumstances
     C     XPPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the Justification Report is required for
     C***              this procedure."
     C                   END                                                    END EQ '22'
     C*
     C* ===============================================================
     C* Modifier -26 = Professional Component
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDNE     *ZEROS
     C     SXPROF        DIV       100           SXPROF
     C     SXPROF        MULT(H)   SXMRA         SXMRA
     C                   END                                                    END EQ '26'
     C*
     C* ===============================================================
     C* Modifier -27 = TECHNICAL Component
     C     XPPMOD        IFEQ      '27'
     C     SXPROF        ANDNE     *ZEROS
     C     100           SUB       SXPROF        SXPROF
     C     SXPROF        DIV       100           SXPROF
     C     SXPROF        MULT(H)   SXMRA         SXMRA
     C                   END                                                    END EQ '27'
     C*
     C* ===============================================================
     C* Modifier -28 = Debridement and/or decontamination
     C     XPPMOD        IFEQ      '28'
     C     SXMRA         MULT      1.10          SXMRA
     C                   END                                                    END EQ '28'
     C*
     C*===================================================================
     C*-- 33 - "Anesthesia complicated by total body hypothermia"---------
     C     XPPMOD        IFEQ      '33'
     C* Per ANES pg. 3 - add 5.0 to basic unit value
     C     SXANAM        ADD       5.0           SXANAM
     C     SXANAM        MULT      SXMRA         SXMRA
     C                   END                                                    END EQ '33'
     C*
     C*===================================================================
     C*-- 35 - "Anesthesia complicated by extra corporeal---------
     C     XPPMOD        IFEQ      '35'
     C* Per ANES pg. 3 - add 10.0 to basic unit value
     C     SXANAM        ADD       10.0          SXANAM
     C     SXANAM        MULT      SXMRA         SXMRA
     C                   END                                                    END EQ '35'
     C*
     C*===================================================================
     C*-- 37 - "Anesthesia risk" -----------------------------------------
     C     XPPMOD        IFEQ      '37'
     C* Per ANES pg. 3 - add 2.0 to basic unit value
     C     SXANAM        ADD       2.0           SXANAM
     C     SXANAM        MULT      SXMRA         SXMRA
     C                   END                                                    END EQ '37'
     C*
     C* ===============================================================
     C*-- 38 - "Anesthesia risk for emergency surgery/systemic" - give message
     C     XPPMOD        IFEQ      '38'
     C* Per ANES pg. 3 - add 3.0 to basic unit value
     C     SXANAM        ADD       3.0           SXANAM
     C     SXANAM        MULT      SXMRA         SXMRA
     C                   END                                                    END EQ '38'
     C*
     C* ===============================================================
     C*-- 39 - "Anesthesia risk for emergency surgery/moribund" - give message
     C     XPPMOD        IFEQ      '39'
     C* Per ANES pg. 3 - add 5.0 to basic unit value
     C     SXANAM        ADD       5.0           SXANAM
     C     SXANAM        MULT      SXMRA         SXMRA
     C                   END                                                    END EQ '39'
     C*
     C* ===============================================================
     C* Modifier -49 = Multiple Anesthesia
     C     XPPMOD        IFEQ      '49'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the Justification Report is required for
     C***              this procedure."
     C                   END                                                    END EQ '49'
     C*
     C* ===============================================================
     C* Modifier -50 = Bilateral procedures
     C     XPPMOD        IFEQ      '50'
     C     SXMRA         MULT      .50           SXMRA
     C                   END                                                    END EQ '50'
     C*
     C* ===============================================================
     C* Modifier -53 = Primary emergenct services
     C     XPPMOD        IFEQ      '53'
     C     SXMRA         MULT      .70           SXMRA
     C                   END                                                    END EQ '53'
     C*
     C* ===============================================================
     C* Modifier -80 = ASSISTANT SURGEON
     C     XPPMOD        IFEQ      '80'
     C     SXMRA         MULT      .20           SXMRA
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C* Modifier -81 = MINIMUM ASSISTANT SURGEON
     C     XPPMOD        IFEQ      '81'
     C     1.1           MULT      XXSURG        SXMRA
     C                   END                                                    END EQ '81'
     C*
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C     SXMRA         MULT      XXRADI        SXMRA
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
     C     XPPMOD        ANDNE     '58'
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
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C*
     C* ===============================================================
     C* PAGE 307, PROBLEM 10
     C* REVIEW OF DIAGNOSTIC STUDIES: No separate charge is warranted for
     C* prior studies reviewed in conjunction with a visit, consultation,
     C* record review, or other evaluation by the medical practitioner or
     C* other medical personnel, and neither the professional component
     C* value modifier -26 no hte radiologic consultation code 76140 is
     C* reimbursable.  The review of Diagnostic tests is included in the
     C* Evaluation and Management codes.
     C*
     C*
     C     DHPRCD        IFGE      '70010'
     C     DHPRCD        ANDLE     '76139'
     C     XPPMOD        ANDEQ     '26'
     C     DHPRCD        ORGE      '76141'
     C     DHPRCD        ANDLE     '79999'
     C     XPPMOD        ANDEQ     '26'
     C     DHPRCD        OREQ      '76140'
     C     XPPMOD        ANDEQ     '  '
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99499'
     C     XRTOTP        IFGT      0
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 70010
     C*
     C*
     C* ===============================================================
     C     XPPMOD        IFEQ      '26'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     SX1A1         IFEQ      'A'
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
     C                   GOTO      RADIE
     C*
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- By Report -------------------------------------------------------
     C     SX1A1         IFEQ      'B'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END PRRPT EQ 'B'
     C*
     C                   END                                                    END XPPMOD EQ '26'
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*
     C*
     C* ===============================================================
     C* Per RADI, pg. 4
     C* Will only need to do processing in workfile if XPPMOD is
     C*          51 = MULTIPLE PROCEDURES
     C*
     C     XPPMOD        IFEQ      '51'
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPKY Workfile
     C* Only load "current charges" into the FACAPKY Workfile
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     SXMRA         CAMRA
     C*
     C                   MOVE      'R'           CATYPE
     C                   WRITE     PCAWK1
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END XPPMOD EQ '51'
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
     C********************************************************************
     C** Process RADI Codes for DIFFERENT PERCENTAGES          R A D I 2 *
     C*  This subroutine is called at each date, provider level break    *
     C********************************************************************
     C     RADI2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the RADI Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C*
     C                   Z-ADD     0             COUNT
     C                   MOVE      *BLANKS       HLPART
     C     *LOVAL        SETLL     PCAWK4
     C                   READ      PCAWK4                                 60
     C     *IN60         IFEQ      '0'
     C* Read history to determine if a RADI code has already been paid
     C                   Z-ADD     P4FDAT        PPFDAT
     C                   Z-ADD     P4FEIN        PPFEIN
     C                   Z-ADD     P4MOD         PPMOD
     C                   MOVE      '70010'       PPCPT4
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        ANDLE     '79999'
     C     XRTOTP        IFGT      *ZEROS
     C     XRPMOD        IFEQ      '51'
     C                   ADD       1             COUNT
     C                   END                                                    END EQ '51'
     C                   END                                                    END NE *ZEROS
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Per RADI, pg. 3
     C*  Highest value 100% - HIGHEST WILL NOT INCLUDE MOD 51 SO IT WILL
     C*                       NOT BE INCLUDED IN THE WORKFILE
     C*  2nd            50%
     C*  3rd            25%
     C*  4th +          Message Review
     C*
     C     P4PMOD        IFEQ      '51'
     C*
     C     COUNT         IFLE      0
     C                   Z-ADD     .50           P4BRP                          50%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      1
     C                   Z-ADD     .25           P4BRP                          25%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFGT      1
     C                   Z-ADD     0             P4BRP                          0%
     C                   ADD       1             COUNT
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      'BM'          QQC(Q)
     C* Err Msg BM: "For 4 or more Radi Med procedures, review
     C                   END                                                    END  GT 1
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   END                                                    END EQ '51'
     C*
     C     P4MRA         MULT      P4BRP         P4MRA
     C*
     C     P4TOTC        COMP      P4MRA                              54        >>>>>
     C   54              Z-ADD     P4MRA         P4TOTA
     C   54              Z-ADD     *ZEROS        P4DIFF
     C  N54              Z-ADD     P4TOTC        P4TOTA
     C  N54P4MRA         SUB       P4TOTA        P4DIFF
     C     P4TOTC        SUB       P4TOTA        P4TOTD
     C                   Z-ADD     P4TOTA        P4TOTP
     C*
     C*  Using FACAPKY record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P4FDAT        PPFDAT
     C                   Z-ADD     P4FEIN        PPFEIN
     C                   Z-ADD     P4MOD         PPMOD
     C                   MOVE      P4CPT4        PPCPT4
     C                   Z-ADD     P4DSEQ        PPDSEQ
     C     CAKY8A        CHAIN     PCA08                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              62
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     P4TOTA        XRTOTA
     C                   Z-ADD     P4TOTD        XRTOTD
     C                   Z-ADD     P4TOTP        XRTOTP
     C                   MOVEL     P4PMOD        XRPMOD
     C     P4TOTD        IFGT      *ZERO
     C     P4TOTP        ANDGT     *ZEROS
     C                   MOVE      '202'         P4EOMB
     C* EOMB Code 202: "Charges exceed maximum allowance"
     C                   END                                                    END P3TOTD GT *ZERO
     C*
     C                   MOVE      P4EOMB        XREOMB
     C*
     C* Set the sequence number key field to write the EOMB code
     C     P4EOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     P4DSEQ        KKDSEQ
     C                   MOVE      XREOMB        XPEOMB
     C                   EXSR      EOMB
      *
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA08
     C*
     C                   ELSE
     C*
     C                   EXCEPT    ECA01
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C                   READ      PCAWK4                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C     RADI2E        ENDSR
     *********************************************************************
     *** Surgery                    Codes: 10000 - 69999         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C     SXMRA         MULT(H)   XXSURG        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per SURG, pg. 5-8
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C*
     C*** BEG CHG *** 08/24/98 ****************************************
     C********** XPPMOD    IFNE '20'
     C     XPPMOD        ORNE      '20'
     C*** END CHG *** 08/24/98 ****************************************
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '47'
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '62'
     C     XPPMOD        ANDNE     '64'
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '68'
     C     XPPMOD        ANDNE     '75'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*** BEG ADD *** 08/24/98 ****************************************
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "Item is BY REPORT and requires further inspection
     C*             by an adjuster."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN62
     C*** END ADD *** 08/24/98 ****************************************
     C*
     C     XPPMOD        IFEQ      '62'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C  N60              Z-ADD     30            Q
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
     C  N60              Z-ADD     30            Q
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
     C  N60              Z-ADD     30            Q
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
     C*** BEG DEL *** 08/24/98 ****************************************
     C******************** END                             END XPPMOD NE 20...
     C*** END DEL *** 08/24/98 ****************************************
     C                   END                                                    END XPPMOD EQ 26...
     C                   END                                                    END XPPMOD NE *BLANK
     C* ===============================================================
     C*
     C* Modifier -80 = ASSISTANT SURGEON
     C     XPPMOD        IFEQ      '80'
     C     PRSURG        IFEQ      'N'
     C     SVSPCD        OREQ      'CSA'
     C     SVSPCD        OREQ      'ST '
     C     SVSPCD        OREQ      'AS '
     C     SVSPCD        OREQ      'AS1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END PRSUPP
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C* Modifier -81 = MINIMUM ASSISTANT SURGEON
     C     XPPMOD        IFEQ      '81'
     C     PRSURG        IFEQ      'N'
     C     SVSPCD        OREQ      'CSA'
     C     SVSPCD        OREQ      'ST '
     C     SVSPCD        OREQ      'AS '
     C     SVSPCD        OREQ      'AS1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END PRSUPP
     C                   END                                                    END EQ '81'
     C*
     C* ====================================================================
     C* SKIP IF MODIFIER 50, CALCULATIONS FOR -50 IS ON PAGE 50
     C     XPPMOD        IFNE      '50'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C* ====================================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '69970'
     C     XPPMOD        ANDEQ     '50'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C*                    MOVEL*BLANKS   XPPMOD
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ====================================================================
     C*  PER PG 60,  Modifier -50 paying 150%
     C*  Bilateral Procedures:  Unless otherwise identified in the listings
     C*  bilateral procedures that are performed at the same operative
     C*  session should be identified by the appropriate five digit code
     C*  describing the first procedure.
     C*
     C*  If the code is billed twice on the same day with modifier -50
     C*  and they don't contain the same surgical sites, the first will
     C*  pay 100% and the second will pay 80% else a single code will
     C*  pay 150% of the MRA.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1
     C*
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '69970'
     C     XPPMOD        ANDEQ     '50'
     C*
     C*  Only looking for each specific code, so we use logical with
     C*  XXCPT4 pre-determined as the search code and no others.
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C*  We want to look through all the paid codes that match our code
     C*  and contain a modifier -50, and has a different surgical part.
     C*
     C     *IN54         DOWEQ     '0'
      *
     C*** BEGIN CHG *** 11/03/97 **************************************
      *   Change lookup to look at one specific code not the range
      *
     C     XRCPT4        IFEQ      DHPRCD
     C*********  XRCPT4    IFGE '10040'
     C*********  XRCPT4    ANDLE'69970'
      *
     C*** END CHG ***** 11/03/97 **************************************
     C     DHAREA        ANDEQ     XRPART                                       ADD 11/03/97
     C     CAFDAT        ANDEQ     XRFDAT                                       ADD 11/03/97
     C                   ADD       1             COUNT
     C*
     C     XRTOTP        IFGT      0
     C     CADSEQ        ANDNE     XRDSEQ
     C                   ADD       1             COUNT1
     C                   ENDIF
     C*
     C                   ENDIF
     C     CAKEY7        READE     PCA09                                  54
     C                   ENDDO
     C*
     C*  Count determines how many CPT4 codes matched the search.
     C*  If = 0 pay 150%, if = 1 pay 50%, else pay nothing.
     C     COUNT         IFLE      1
     C     COUNT1        ANDEQ     0
     C*** BEGIN CHG *** 11/03/97 **************************************
     C*   REMOVED PAYMENT FOR 150% -- NOW ERROR MESSAGE AND EOMB 203
      *
     C*********  SXMRA     MULT 1.5       SXMRA
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C* Err Msg 16: "A major code must be present to pay this item."
      *
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C*** END CHG ***** 11/03/97 **************************************
     C                   ELSE
     C*
     C     COUNT         IFGE      2
     C     COUNT1        ANDEQ     0
     C     SXMRA         MULT      1.0           SXMRA
     C                   ELSE
     C*
     C     COUNT         IFGE      2
     C     COUNT1        ANDEQ     1
     C     SXMRA         MULT      .5            SXMRA
     C                   ELSE
     C*** PAY NOTHING
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "Exceeds number of allowable procedures in the
     C***            reimbursement manual."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  COUNT = 1
     C                   ENDIF                                                  COUNT = 1
     C                   ENDIF                                                  COUNT = 0
     C                   ENDIF                                                  DHPRCD GE
     C*
     C* ====================================================================
     C* PAGE 55, PROBLEM 2
     C* Listed Surgical Procedures:                                    or
     C* These include the ooperation per se, local infiltration,       n,
     C* metacarpal/digital block or topical anesthesia when used, and  or
     C* the normal, uncomplicated follow -up care.  This concept is
     C* referred to as a "Package" for surgical procedures.  To report 0502
     C* a postoperative follow-up visit for documentation purposes onlye
     C* use 99024.
     C*
     C     DHPRCD        IFGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C     DHPRCD        OREQ      '62273'
     C     DHPRCD        ANDLE     '62279'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C*** BEGIN ADD *** 07/03/97 **************************************
     C*   VERIFY THAT IT IS NOT COMPARING AGAINST ITSELF
     C     XRDSEQ        ANDNE     CADSEQ
     C*** END ADD ***** 07/03/97 **************************************
     C     XRTOTP        IFGT      0
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "This procedure was paid under a complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 64400
     C*
     C*
     C* ===============================================================
     C* PER P. 6, 1  Separate procedures.
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
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  60
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
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C                   END                                                    END XRCPT4
     C     CAKEY8        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END *IN60 EQ 1
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
     C* ====================================================================
     C* WILL ONLY NEED TO DO PROCESSING IN WORKFILE IF XPPMOD IS
     C* 51 = MULTIPLE PROCEDURES
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
     C*
     C*********  XPPMOD    IFEQ '51'
     C*** BEGIN ADD *** 11/03/97 **************************************
      *   MODIFIER 50, BILATERAL PROCEDURES DO NOT GO THROUGH SURG 2
      *
     C     XPPMOD        IFNE      '50'
      *
     C*** END ADD ***** 11/03/97 **************************************
     C*** END DEL ***** 10/01/97 **************************************
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPKY Workfile
     C* Only load "current charges" into the FACAPKY Workfile
     C*
     C* Exceptions are found in SUR1/SUR2 table and cannot be cut
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    SUR1(T)                                60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     SXMRA         CAMRA
     C                   MOVE      'S'           CATYPE
     C                   WRITE     PCAWK1
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
     C**********           END                             END XPPMOD EQ '51'
     C*** END DEL ***** 10/01/97 **************************************
     C*** BEGIN ADD *** 11/03/97 **************************************
      *   MODIFIER 50, BILATERAL PROCEDURES DO NOT GO THROUGH SURG 2
      *
     C                   END                                                    END XPPMOD EQ '50'
      *
     C*** END ADD ***** 11/03/97 **************************************
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
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
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
     C                   MOVE      '203'         XPEOMB
     C* EOMB Code 203: "A major code must be present to pay this item"
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
     C*** BEGIN ADD *** 11/03/97 **************************************
     C                   CLEAR                   CNT
     C*** END ADD ***** 11/03/97 **************************************
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
     C     XRCPT4        ANDLE     '69999'
     C     XRTOTP        IFGT      *ZEROS
     C     XREOB#        ANDNE     QQEOB#
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
      *
     C********** XRPMOD    IFEQ '51'
     C*** END DEL ***** 10/01/97 **************************************
     C*** BEGIN ADD *** 11/03/97 **************************************
     C     XRPART        ANDNE     '50'
     C*** END ADD ***** 11/03/97 **************************************
      *
     C*** BEG ADD ***** 08/21/98 **************************************
      *   Do not count codes billed with mod. 80 (assistant surgeon)
      *
     C     XRPMOD        ANDNE     '80'
     C*** END ADD ***** 08/21/98 **************************************
      *
     C*** BEGIN ADD *** 11/03/97 **************************************
     C     P2PART        IFNE      'S1'
     C     P2PART        ANDNE     'S2'
     C     P2PART        ANDNE     'S3'
     C     P2PART        ANDNE     'S4'
     C     P2PART        ANDNE     'S5'
     C*** END ADD ***** 11/03/97 **************************************
     C                   ADD       1             COUNT
     C*** BEGIN ADD *** 11/03/97 **************************************
     C                   ENDIF                                                   END P2PART
     C*** END ADD ***** 11/03/97 **************************************
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
      *
     C**********           END                             END EQ '51'
     C*** END DEL ***** 10/01/97 **************************************
     C                   END                                                    END NE *ZEROS
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Per pg. 8
     C*  Highest value 100% - HIGHEST WILL NOT INCLUDE MOD 51 SO IT WILL
     C*                       NOT BE INCLUDED IN THE WORKFILE
     C*  2nd            50%
     C*  3rd            25%
     C*  4th +          Message Review for Global Fee
     C*
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
      *
     C*********  P2PMOD    IFEQ '51'
     C*** END DEL ***** 10/01/97 **************************************
     C*
     C*** BEGIN ADD *** 11/03/97 **************************************
     C     P2PART        IFNE      'S1'
     C     P2PART        ANDNE     'S2'
     C     P2PART        ANDNE     'S3'
     C     P2PART        ANDNE     'S4'
     C     P2PART        ANDNE     'S5'
     C*** END ADD ***** 11/03/97 **************************************
     C*
     C     COUNT         IFEQ      0
     C                   Z-ADD     1.0           P2BRP                          100%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      1
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      2
     C                   Z-ADD     .25           P2BRP                          25%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFGE      3
     C                   Z-ADD     0             P2BRP                          0%
     C                   ADD       1             COUNT
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      'BF'          QQC(Q)
     C* Err Msg BF: "For 4 or more Surgical procedures, review for global fee
     C*
     C                   Z-ADD     *ZERO         P2TOTP
     C     P2TOTC        SUB       P2TOTP        P2TOTD
     C                   Z-ADD     P2TOTP        P2TOTA
     C******************** Z-ADDCACHRG    P2TOTC
     C                   MOVE      '212'         P2EOMB
     C* EOMB Code 212: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C*
     C                   END                                                    END  GT 3
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  EQ 0
     C*** BEGIN ADD *** 11/03/97 **************************************
     C                   ELSE
     C*   IF BODY PART WAS ENTERED
     C*
     C                   SELECT
     C*
     C     P2PART        WHENEQ    'S1'
     C                   Z-ADD     1             X
     C                   ADD       1             CNT(X)
     C*
     C     P2PART        WHENEQ    'S2'
     C                   Z-ADD     2             X
     C                   ADD       1             CNT(X)
     C*
     C     P2PART        WHENEQ    'S3'
     C                   Z-ADD     3             X
     C                   ADD       1             CNT(X)
     C*
     C     P2PART        WHENEQ    'S4'
     C                   MOVEL     4             X
     C                   ADD       1             CNT(X)
     C*
     C     P2PART        WHENEQ    'S5'
     C                   Z-ADD     5             X
     C                   ADD       1             CNT(X)
     C*
     C                   ENDSL
     C*
     C                   SELECT
     C     CNT(X)        WHENEQ    1
     C                   Z-ADD     1.0           P2BRP                          100%
      *
     C     CNT(X)        WHENEQ    2
     C                   Z-ADD     .5            P2BRP                           50%
      *
     C     CNT(X)        WHENEQ    3
     C                   Z-ADD     .25           P2BRP                           25%
      *
     C     CNT(X)        WHENGE    4
     C                   Z-ADD     0             P2BRP                          0%
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      'BF'          QQC(Q)
     C* Err Msg BF: "For 4 or more Surgical procedures, review for global fee
     C*
     C                   Z-ADD     *ZERO         P2TOTP
     C     P2TOTC        SUB       P2TOTP        P2TOTD
     C                   Z-ADD     P2TOTP        P2TOTA
     C******************** Z-ADDCACHRG    P2TOTC
     C                   MOVE      '212'         P2EOMB
     C* EOMB Code 212: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C*
     C                   ENDSL                                                  END  SELECT
     C                   ENDIF                                                  END  P2PART
     C*** END ADD ***** 11/03/97 **************************************
     C*
     C*** BEGIN DEL *** 10/01/97 **************************************
      *   REMOVE MODIFIER 51 AS MULTIPLE SURGICAL RULE, ALL CODES FALL
      *   UNDER MULTIPLE SURGICAL RULE.
      *
     C**********           END                             END EQ '51'
     C*** END DEL ***** 10/01/97 **************************************
     C*
     C     P2MRA         MULT(H)   P2BRP         P2MRA
     C*
     C*** BEGIN ADD *** 10/01/97 **************************************
     C* ====================================================================
     C*  PER PG 400 #1
     C*  If a starred surgical code is billed, with a code from 99211
     C*  to 99215 (mod -25), add .35 to the srgery value.
     C*
     C     P2CPT4        IFGE      '10040'
     C     P2CPT4        ANDLE     '69970'
     C*
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVE      P2FDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     P2CPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C*
     C                   MOVEL     '99211'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        ANDGE     '99211'
     C     XRCPT4        ANDLE     '99215'
     C     XRPMOD        ANDEQ     '25'
     C     XXSURG        MULT(H)   .35           XXMRA
     C                   ADD       XXMRA         P2MRA
     C                   MOVE      'Y'           EDT400                         EDIT pg 400
     C*                    LEAVE
     C     CAKEY7        READE     PCA09                                  54
     C                   ENDDO
     C*
     C                   ENDIF                                                  END PRSUPP
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ====================================================================
     C*** END ADD ***** 10/01/97 **************************************
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C*
     C*  Using FACAPKY record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P2FDAT        PPFDAT
     C                   Z-ADD     P2FEIN        PPFEIN
     C                   Z-ADD     P2MOD         PPMOD
     C                   MOVE      P2CPT4        PPCPT4
     C                   Z-ADD     P2DSEQ        PPDSEQ
     C     CAKY8A        CHAIN     PCA08                              61
     C*
     C     *IN61         IFEQ      *OFF
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
     C     P2TOTP        ANDGT     *ZEROS
     C                   MOVE      '202'         P2EOMB
     C* EOMB Code 202: "Charges exceed maximum allowance"
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     C                   MOVE      P2EOMB        XREOMB
      *
     C* Set the sequence number key field to write the EOMB code
     C     P2EOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   MOVE      XREOMB        XPEOMB
     C                   EXSR      EOMB
      *
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA08
     C*
     C                   ELSE
     C                   EXCEPT    ECA08
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C                   READ      PCAWK2                                 60
     C*
     C                   END                                                    END DOWEQ 0 *IN60
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
     C*
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
     C*
     C                   MOVE      SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0F                              61
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
     C*** BEG CHG *** 07/29/99 ****************************************
     C******************** MOVELPXDSEQ    QQS,Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C*** END CHG *** 07/29/99 ****************************************
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
DLTOVR FILE(FACALKY)
OVRDBF FILE(FACALKY)  TOFILE(QTEMP/FACALKY)
DLTOVR FILE(FACAPKY)
OVRDBF FILE(FACAPKY)  TOFILE(QTEMP/FACAPKY)
DLTOVR FILE(FACAL1KY)
OVRDBF FILE(FACAL1KY)  TOFILE(QTEMP/FACAL1KY)
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
**  PHY1/PHY2: A MUST HAVE B AND A WILL NOT BE CUT
9714597010
9714597012
9714597014
9714597016
9714597018
9714597020
9714597022
9714597024
9714597026
9714597028
9714597039
9714597110
9714597112
9714597114
9714597116
9714597118
9714597120
9714597122
9714597124
9714597126
9714597128
9714597139
9722197220
9724197240
9726197260
9750197500
9752197520
9753197530
9754197540
9754697545
9770197700
9772197720
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
**  NC1 CODES NOT TO CUT LIST 1
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
