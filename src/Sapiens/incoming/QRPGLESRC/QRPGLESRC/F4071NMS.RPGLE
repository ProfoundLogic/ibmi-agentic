     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4071RMS
     H*    DESCRIPTION: ADJUDICATION (MISSISSIPPI RULES - 08/01/93)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the MISSISSIPPI State
     H* Fee Schedule Reimbursment Manual issued on 08/01/93
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 08/01/93 and before 01/01/98
     H*****************************************************************
     H*  CREATED: 9/21/93   C:20   PROJECT:
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update the XXANES conversion factor
     H*        (found in *LIKE section)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*  NP = NURSE PRACTITIONER
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  06/07/99 C:79 PROJ:MS007                                     *
     H*                Modified to work with provider bills           *
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
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  05/15/98 C:79  Project: MS005
     H*         Add date condition for 01/01/98
     H*
     H*  12/14/94  C:20     REQ/PROJ: 50043/MS002
     H*                     1. ONLY ALLOW 2 97145 IN ONE DAY
     H*                     2. IF SPLTY CODE IS NP(NURSE PRACTITIONER)
     H*                        PAY 75% OF THE MRA LISTED
     H*
     H*  01/10/97  C:59     REQ/PROJ: 214/MS004
     H*                     1. WHEN AN EMG IS PERFORMED ON THE SAME DAY
     H*                        AS AN FOLLOWUP OFF VISIT, DISALLOW
     H*                        UNLESS DOCUMENTATION OF MED NECESSITY
     H*                     2. SAME AS #1 FOR NERVE CONDUCTION STUDIES
     H*                     3. REIMBURSEMENT FOR H-REFLEX IS LIMITED TO
     H*                        (2) H-REFLEX CHARGES (1 FOR EACH LEG)
     H*                     4. REIMBURSEMENT FOR F-WAVE IS LIMITED TO
     H*                        (2) CHARGES PER EXTREMITY
     H*                     5. ADD PHYSICAL MED (MULTI PROCEDURE - 51)
     H*                        CODE TO PAY AT DIFF % IF PROCEDURES
     H*                        PERFORMED ON SAME DAY (DIFF BODY PARTS)
     H*
     H*  02/20/97  C:59     REQ/PROJ: MC048/6754
     H*                     1. REMOVED EDIT 'INITIAL TREATMENT FOR SOFT
     H*                        INJURIES MUST BE BILLED UNDER THE
     H*                        APPROPRIATE OFFICE VISIT CODE
     H*
     H*  04/30/97  C:59     REQ/PROJ: MC048/7066
     H*                     1. ADDED TABLE WHICH CONTAINED SURGERY CODES
     H*                        TO BE PROCESSED AS ANESTHESIA CODES
     H*
     H*  11/18/97 C:86  Project: MC145
     H*         CHECK ALL DATE FIELDS AND ROUTINES TO SEE IF THEY ARE
     H*         CORRECT FOR 7 DIGIT FILE FIELDS & 8 DIGIT SCREEN FIELDS
     H*
     H*  07/13/98  C:79     REQ/PROJ: MC095 SEQ: 12396
     H*                     Added new list of no-cut CPT-4 codes to NC1
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  56    Use to determine if SURG code is to be paid as ANES codes
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                        SUBR         CODES
     H*  Evaluation and Management Service              OFFI    99201 - 99499
     H*  Anesthesia                                     ANES    00100 - 01999
     H*                                                         99100 - 99140
     H*  Surgery                                        SURG    10040 - 69979
     H*                                                 SURG2   10040 - 69979
     H*  Radiology                                      RADI    70010 - 79999
     H*  Labratory and Pathology                        PATH    80002 - 89399
     H*  Medicine                                       MEDI    90701 - 99199
     H*  Physical Medicine                              PHYS    97010 - 97799
     H*                                                         Q0103 - Q0104
     H*****************************************************************
     F*
     FFACAPMS   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALMS   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALH    UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLMS   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLMS   IF   E           K DISK
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
     D PRT             S              2    DIM(20)
     D*
     D*** END ADD ***** 01/10/97 ***********************************************
     D*
     D* Compile-time Arrays
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES Px Modifiers
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
      *** BEG CHG *** 07/13/98 ****************************************
     D******************* NC1    10  80  5               NO CUT ON THES
     D NC1             S              5    DIM(187) CTDATA PERRCD(11)           NO CUT ON THES
      *** END CHG *** 07/13/98 ****************************************
     D SUR1            S              5    DIM(363) CTDATA PERRCD(1)            NO CUT/MAJOR-MIN
     D SUR2            S              5    DIM(363) ALT(SUR1)
     D*** BEGIN ADD *** 04/30/97 ****************************************
     D ANE             S              5    DIM(44) CTDATA PERRCD(1)             SUR/PAID AS ANES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** END ADD ***** 04/30/97 ****************************************
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
     IPCA0C
     I              CACPT4                      XCCPT4
     I              CACO#                       XCCO#
     I              CASUB#                      XCSUB#
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
     I              CAICD9                      XCICD9
     I              CAANMN                      XCANMN
     I              CAUNTS                      XCUNTS
     I              CATOTA                      XCTOTA
     I              CACHRG                      XCTOTC
     I              CAMRA                       XCTOTD
     I              CAPAID                      XCTOTP
     I              CADSEQ                      XCDSEQ
     I              CADELF                      XCDELF
     I              CAFLAG                      XCFLAG
     I              CASTAT                      XCSTAT
     I              CAP1                        XCP1
     I              CAPMOD                      XCPMOD
     I              CAUSER                      XCUSER
     I              CAPGM                       XCPGM
     I              CADT                        XCDT
     I              CATIME                      XCTIME
     I              CACHK#                      XCCHK#
     I              CAEOB#                      XCEOB#
     I              CAPART                      XCPART
     I              CAEOMB                      XCEOMB
     I              CARCFL                      XCRCFL
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
     I*
     I* Record Format for SURG2 Workfile FACALMS
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
     C     CAKY9E        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    X1YMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C*
     C     CAKY9F        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    X1YMD
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
     C                   KFLD                    HDYMD
     C                   KFLD                    HDFEIN
     C                   KFLD                    HDMOD
     C                   KFLD                    P2CPT4
     C                   KFLD                    P2DSEQ
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
     C* = Initialization Section ======================================
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAFEIN        HDFEIN                         ADD 06/23/93
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD                          ADD 06/23/93
     C     *LIKE         DEFINE    CAFDAT        SVYMD                          ADD 06/23/93
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD                          ADD 06/23/93
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
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
     C     *LIKE         DEFINE    DHUNTS        XXUNIT
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CAFDAT        X1YMD
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
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C********   *LIKE     DEFN CAFDAT    WKFDAT NEEDS TO BE 8
     C     *LIKE         DEFINE    CAFDAT        XXPAST
     C*** END   CHANGE ********* 11/18/97 ********************************
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             COUNTA            5 0          ADD 06/23/93
     C                   Z-ADD     0             CNTB1             5 0          ADD 06/23/93
     C                   Z-ADD     0             CNTB2             5 0          ADD 06/23/93
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          DHUNTS for 15 minute
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
     C                   MOVE      *BLANKS       QQTYPE            1            ADD 06/23/93
     C                   Z-ADD     0             XVUNTS            7 4
     C                   Z-ADD     0             XXUNTS            7 4
     C                   Z-ADD     0             X1ANMN            7 4
     C                   Z-ADD     0             XXUNT             7 4          ADD 06/23/93
     C                   MOVE      *BLANKS       X25CUT            1            ADD 07/13/93
     C                   MOVE      *BLANKS       X50CUT            1            ADD 07/13/93
     C                   Z-ADD     0             M95834            9 2          MRA for 95834
     C                   Z-ADD     0             FUTRCT            5 0
     C                   Z-ADD     0             PASTCT            5 0
     C                   Z-ADD     0             PHYSCT            5 0
     C                   Z-ADD     0             Q0103C            5 0
     C                   MOVE      *BLANKS       SRDTE             8
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C                   Z-ADD     0             T95831            9 2          Tot for 95831 series
     C*
     C                   Z-ADD     40.00         XXANES            5 2           ANES Conversion
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
     C*** BEGIN ADD *** 01/10/97 ***************************************
     C*
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
     C*
     C*** END   ADD *** 01/10/97 ***************************************
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
     C     CAFDAT        IFGE      930801
     C     CAFDAT        ANDLT     980101
     C     XXRCN         ANDEQ     CARCN
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
     C                   MOVEL     XPPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
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
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*** BEGIN ADD *** 04/30/97 ****************************************
     C* SURGERY CODES IF CHARGED WITH SPECIALTY ANE OR RNA WILL BE PAID
     C* THE SAME AS ANESTHESIA  CODES.
     C     DHPRCD        LOOKUP    ANE                                    56
     C*
     C*** END ADD ***** 04/30/97 ****************************************
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
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C*** BEGIN ADD *** 04/30/97 ****************************************
     C* SURGERY CODES IF CHARGED WITH SPECIALTY ANE OR RNA WILL BE PAID
     C* THE SAME AS ANESTHESIA  CODES.
     C     SVSPCD        OREQ      'ANE'
     C     *IN56         ANDEQ     *ON
     C     SVSPCD        OREQ      'RNA'
     C     *IN56         ANDEQ     *ON
     C*
     C*** END ADD ***** 04/30/97 ****************************************
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
     C                   CLOSE     FACALMS
     C                   CLOSE     FACAPMS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACMS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALMS
     C                   OPEN      FACAPMS
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
     C                   CLOSE     FACALMS
     C                   CLOSE     FACAPMS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACMS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALMS
     C                   OPEN      FACAPMS
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
     C     DHPRCD        ORGE      '99000'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      'Q0103'
     C     DHPRCD        ANDLE     'Q0104'
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C     DHPRCD        IFGE      '99201'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
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
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*** BEGIN ADD *** 01/10/97 **************************************
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
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
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
     C     CAFDAT        IFGE      930801
     C     CAFDAT        ANDLT     980101
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
     C                   MOVE      '0'           OFCPD
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
     C                   MOVE      930801        XXEFDT
     C*
     C     PRKY          SETGT     PPR01
     C     NXTPR3        TAG
     C                   READP     PPR01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     '95834'
     C     PRSTAT        CABEQ     'I'           NXTPR3
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
     C     NXRC3         TAG
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
     C                   GOTO      NXTRC5
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
     C                   GOTO      NXTRC5
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
     C                   GOTO      NXRC3
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC5
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
     C                   GOTO      NXTRC5
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
     C     NXTPR7        TAG
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
     C                   GOTO      NXTRC5
      *** END CHG *** 12/10/98 ****************************************
      *** END CHG *** 11/05/98 ****************************************
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR7
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
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
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
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
     C                   ELSE
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
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
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
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
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
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
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
     C                   MOVE      '005'         XPEOMB
     C* EOMB Code 005: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC5                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*
     C*** BEGIN ADD *** 04/30/97 ****************************************
     C* SURGERY CODES IF CHARGED WITH SPECIALTY ANE OR RNA WILL BE PAID
     C* THE SAME AS ANESTHESIA  CODES.
     C     DHPRCD        LOOKUP    ANE                                    56
     C*
     C*** END ADD ***** 04/30/97 ****************************************
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
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C*** BEGIN ADD *** 04/30/97 ****************************************
     C* SURGERY CODES IF CHARGED WITH SPECIALTY ANE OR RNA WILL BE PAID
     C* THE SAME AS ANESTHESIA  CODES.
     C     SVSPCD        OREQ      'ANE'
     C     *IN56         ANDEQ     *ON
     C     SVSPCD        OREQ      'RNA'
     C     *IN56         ANDEQ     *ON
     C*
     C*** END ADD ***** 04/30/97 ****************************************
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
     C                   CLOSE     FACALMS
     C                   CLOSE     FACAPMS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACMS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALMS
     C                   OPEN      FACAPMS
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
     C                   CLOSE     FACALMS
     C                   CLOSE     FACAPMS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACMS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALMS
     C                   OPEN      FACAPMS
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
     C     DHPRCD        ORGE      '99000'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      'Q0103'
     C     DHPRCD        ANDLE     'Q0104'
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C     DHPRCD        IFGE      '99201'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
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
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *
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
     C*
     C                   END                                                    END EQ OVRRD
     C*
     C                   ENDIF                                                  CAFDAT GE
     C*
     C*** Process next record *****************************************
     C     NXTRC5        TAG
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
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*
     C*
     C*** END ADD *** 01/10/97 **********************************************
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
     C                   CLOSE     FACALMS
     C                   CLOSE     FACAPMS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPMS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C* Write adjudication messages to FDMGP                           -----
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
     C* Per PG. 54-55
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '46'
     C     XPPMOD        ANDNE     '52'
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*----------------------------------------------------------------
     C* Per Chpt. 51, pg. 22-23....also Relative Value Guide, pg. vii
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
     C*** BEGIN ADD *** 04/30/97 **************************************
     C* MISSISSIPPI ADDENDUM CODES (63780 AND 64640) HAVE ZERO FACTOR
     C* CODES ARE TO GIVE ERROR MESSAGE.
     C     DHPRCD        IFEQ      '63780'
     C     DHPRCD        OREQ      '64640'
     C     SXANAM        IFEQ      *ZEROS
     C*
     C*ERROR MESSAGE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '90'          QQC(Q)
     C* Err Msg 90: "Adjustor should review charges for significant
     C*             identifiable services."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   ENDIF                                                     END SXANAM
     C                   ENDIF                                                     END DHPRCD
     C*** END ADD ***** 04/30/97 ****************************************
     C*================================================================
     C*=== Calculating Time Units =====================================
     C*
     C* The Time Units for all procedures is computed by allowing 1 unit
     C* for each 15 minutes of anesthesia time.  In each instance, 5 minutes
     C* or greater is considered a significant portion of a time unit.
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof
     C     DHUNTS        DIV       15            HLANMN
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
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
     C*
     C*===============================================================
     C* Calculate Payment
     C* Per Chpt. 51, pg. 24: The amount payable for anesthesia services will be
     C* the lesser of the actual charge or $40 times the total allowed units
     C* as determined by this schedule and the above guidance
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
     *** Medical                    Codes: 90701 - 96999         M E D I *
     ***                                   97800 - 99199                 *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 257
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '50'
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C*** BEGIN ADD *** 01/10/97 **************************************
     C*
     C* Per pg. 17,  2,3
     C*
     C* Reimbursement for an F-Wave is limited to two(2) F-Wave charges
     C* perextremity studied.
     C* Reimbusement for an H-Reflex is limited to one(1) charge for
     C* each leg for a total of two(2) H-Reflex charges.
     C*
     C     DHPRCD        IFEQ      '95935'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FE'          QQC(Q)
     C*** Err Msg FE: "F-Wave limit 2 per extremity, H-Reflex limit 1
     C***              per leg"
     C*
     C                   END                                                    END EQ 99090
     C*
     C* ===============================================================
     C*** END ADD ***** 01/10/97 **************************************
     C* Per pg. 277,F,4
     C*** Procedure code 97752 includes a printout of test results and
     C*** seperate reimbursement must not be made under CPT code 99090
     C*
     C     DHPRCD        IFEQ      '99090'
     C*
     C                   MOVE      '97752'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '005'         XPEOMB
     C* EOMB Code 005: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   END                                                    END EQ 99090
     C*
     C* ===============================================================
     C* Per pg. 277,F,1-5
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
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRPAST'
     C                   CALL      'SRPAST8'
     C*** END   CHANGE ********* 11/18/97 ********************************
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
     C* Per chpt. 51, pg. 36, d, 2
     C*** When two or more procedures from 95831-95853 are performed on the
     C*** same day, reimbursement may not exceed the MRA for Procedure Code
     C*** 95834, total evaluation of the body, including hands.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95853'
     C     DHPRCD        ANDNE     '95834'
     C                   MOVE      '95834'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C     DHPRCD        ANDLE     '95853'
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
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds Reimbursement Limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C* Per pg. 71, C, 2
     C* TISSUE OBTAINED FROM A CADAVER FOR GRAFTING MUST BE BILLED USING 99070
     C* AND ACCOMPANIED BY A REPORT TO ENSURE AN EQUITABLE REIMBURSEMENT BY
     C* CARRIER
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END CPT4 EQ 99070
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     *** Office Visits/Consults     Codes: 99201 - 99499         O F F I *
     *********************************************************************
     C     OFFI          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 47
     C     XPPMOD        IFNE      '21'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ELSE                                                   ELSE EQ 21...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 21
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*** BEGIN ADD *** 01/10/97 **************************************
     C*================================================================
     C*
     C* Per pg. 16, 10.1
     C* Physicians may be reimbursed for both an initial(new patient)
     C* visit or consultation and nerve conduction studies performed   tutes
     C* at the same visit.  An established patient visit will not be   ot
     C* reimbursed when a nervce conduction study is performed unless
     C* documentation of medical necessity substantiates the need for
     C* the additional visit service.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C*
     C                   MOVE      '95900'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '95900'
     C     XRCPT4        ANDLE     '95904'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C* EOMB Code 005: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C*================================================================
     C*
     C* Per pg. 16, 9.2
     C* When an EMG is performed on the same day as a followup visit
     C* or consultation, reimbursement may be made for the EMG only    tutes
     C* unless documnetation of medical necessity substantiates the    ot
     C* need for the additional visit service.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C     XRCPT4        ANDGE     '95860'
     C     XRCPT4        ANDLE     '95872'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C* EOMB Code 005: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C*** END   ADD *** 01/10/97 **************************************
     C* ===============================================================
     C*** The number in that column (FU) establishes the days during which no
     C*** additional reimbursement is allowed for the usual care provided
     C*** following surgery, absent complications or unusual circumstances.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99391'
     C     DHPRCD        ANDLE     '99397'
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
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C     DHPRCD        ORGE      '99218'
     C     DHPRCD        ANDLE     '99223'
     C     DHPRCD        ORGE      '99251'
     C     DHPRCD        ANDLE     '99255'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C     DHPRCD        ORGE      '99381'
     C     DHPRCD        ANDLE     '99387'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN4        TAG
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
     C*
     C*
     C                   ELSE
     C                   GOTO      AGAIN4
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
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C     DHPRCD        ORGE      '99391'
     C     DHPRCD        ANDLE     '99393'
     C*
     C* SETLL and read the file testing for an '*' procedure.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN1        TAG
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '43'          QQC(Q)
     C*** Err Msg 43: "Adjustor should review charges for an
     C***                    unscheduled, starred procedure"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN1
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* ===============================================================
     C*** Per pg.
     C*** Only allow initial visit with physical therapy
     C*
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C     DHPRCD        ORGE      '99391'
     C     DHPRCD        ANDLE     '99393'
     C*
     C                   MOVE      '97010'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '97010'
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
     C                   END                                                    END GE 99211...
     C*
     C* ===============================================================
     C* GLOBAL SURG - PAY OFFICE VISIT BUT GIVE ERROR MESSAGE THAT SERVICE
     C* FALLS WITHIN PRE-OPERATIVE POLICY
     C                   Z-ADD     1             SRFUTR
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDATE
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        X1YMD
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9E        SETLL     PCA09
     C*
     C     CAKY9F        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C     CAKY9F        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C*
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
     C  N60              Z-ADD     30            Q
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
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     C* ===============================================================
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     *** Pathology                  Codes: 70010 - 79999         P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 218-219
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* THE BILLING FOR PANEL TEST MUST INCLUDE THE DOCUMENTATION LISTING
     C* THE TEST IN THE PANEL
     C     DHPRCD        IFGE      '80050'
     C     DHPRCD        ANDLE     '80099'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END EQ '80050'
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     *** PHYSICAL MEDICINE SUBROUTINE CODES 97010 - 97799        P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C*----------------------------------------------------------------
     C*** BEGIN ADD *** 01/10/97 *******************************************
     C     XPPMOD        IFNE      '51'
     C     DHPRCD        ANDLT     '97010'
     C     XPPMOD        ORNE      '51'
     C     DHPRCD        ANDGT     '97701'
     C*** END   ADD *** 01/10/97 *******************************************
     C*
     C     XPPMOD        IFNE      *BLANK
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END NE *ZERO
     C*
     C*** BEGIN ADD *** 06/27/95 *******************************************
     C                   END                                                    END PMOD NE 51
     C*** END   ADD *** 06/27/95 *******************************************
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C*
     C* PER MS BULLETIN
     C*----------------------------------------------------------------
     C*
     C     DHPRCD        IFEQ      '97145'
     C*
     C                   MOVE      '97145'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97145'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END
     C                   END
     C                   END                                                    END GE '97145'
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGT      1
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END COUNT GT 1
     C*
     C                   END                                                    END EQ '97145'
     C*** END ADD *** 12/14/94 *************************************************
     C*** BEGIN DEL *** 12/14/94 ***********************************************
     C*
     C* =================================================================
     C*
     C*** Per pg. 276,D,1
     C*** Visits for therapy may not exceed one visit per day without prior
     C*** approval from payer
     C*
     C**********           MOVE DHPRCD    XXCPT4
     C**********           MOVE XPPMOD    XXPMOD
     C********** CAKYP     CHAINPCA0P                60
     C********** DUPCL3    TAG
     C*
     C********** *IN60     IFEQ '0'
     C********** CADSEQ    IFNE PXDSEQ
     C********** PXTOTP    ANDGT*ZEROS
     C********** CADSEQ    OREQ PXDSEQ
     C********** CAEOB#    ANDNEPXEOB#
     C********** PXTOTP    ANDGT*ZEROS
     C*
     C********** DHAREA    IFEQ PXPART
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CACHRG    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCACHRG    XPTOTC
     C**********           MOVE '009'     XPEOMB
     C**********09: "Exceeds reimbursement limitations"
     C*
     C**********           EXSR UPDAT
     C**********           GOTO PHYSE
     C**********           END
     C*
     C**********           ELSE
     C*
     C********** CAKYP     READEPCA0P                    60
     C**N60                GOTO DUPCL3
     C**********           END
     C**********           END
     C*
     C* =================================================================
     C*
     C*** Per pg.
     C*** Only 1 initial assesment is allowed per life of claim(Q0103)
     C*
     C*
     C     DHPRCD        IFEQ      'Q0103'
     C*
     C                   MOVE      'Q0103'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   Z-ADD     *ZEROS        Q0103C
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      'Q0103'
     C     XCTOTP        ANDGT     *ZEROS
     C                   ADD       1             Q0103C
     C     Q0103C        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END TQ0103
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END
     C*
     C                   END                                                    END GE Q0103...
     C*
     C* =================================================================
     C*
     C*** Per pg. 276,D,4
     C*** Reimbursement will be made for no more than two modalities and two
     C*** procedures for a total of 4, concurrently at the same visit.  if
     C*** multiple treatment areas, ad additional 2 modalities and 2 procedures
     C*** will be allwed
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97039'
     C*
     C                   MOVE      '97010'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   Z-ADD     *ZEROS        PHYSCT
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97039'
     C     XRPART        ANDEQ     DHAREA
     C     XRTOTP        ANDGT     *ZEROS
     C                   ADD       1             PHYSCT
     C     PHYSCT        IFGE      2
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END PHYSCT
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END GE 97010...
     C*
     C     DHPRCD        IFGE      '97110'
     C     DHPRCD        ANDLE     '97145'
     C*
     C                   MOVE      '97110'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   Z-ADD     *ZEROS        PHYSCT
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '97110'
     C     XRCPT4        ANDLE     '97145'
     C     XRPART        ANDEQ     DHAREA
     C     XRTOTP        ANDGT     *ZEROS
     C                   ADD       1             PHYSCT
     C     PHYSCT        IFGE      2
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END PHYSCT
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END GE 97110...
     C*
     C* =================================================================
     C*
     C*** Per pg. 277,F,1-5
     C*** Reimbursement for extremity testing, muscle testing and range
     C*** of motion measurements may be made only once in a 30-day period for
     C*** the same body area.
     C*
     C     DHPRCD        IFGE      '97720'
     C     DHPRCD        ANDLE     '97752'
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
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             PASTCT
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        ANDEQ     DHPRCD
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             PASTCT
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
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
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             FUTRCT
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        ANDEQ     DHPRCD
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             FUTRCT
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
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
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END
     C*
     C                   END
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
     C*
     C*** BEGIN ADD *** 01/10/97 ***********************************************
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97701'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   MOVEL     *BLANKS       XPPMOD
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C*
     C* PER PG. 19, 9
     C* When physical medicine therapies are provided to more than one
     C* body area, modifier -51 must be added tothe procedure code or
     C* codes billed for the additional body area and will be reimbursed
     C* according to the multiple procedure rule.
     C*
     C* THE CLEAR PRT IS TO CLEAR OUT THE TABLE FILES WHICH WILL HOLD THE
     C* BODY PARTS.
     C                   CLEAR                   PRT
     C*
     C*  LOOKING FOR CODE RANGING FROM 97010 - 97701
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97701'
     C*
     C*  MOVING CPT4 CODE INTO THE SEARCH CPT4 CODE ONLY BECAUSE WE WANT
     C*  ALL CPT4 CODES THAT MATCH THE SPECIFIC CPT4 CODE WE ARE PROCESSING.
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C                   Z-ADD     *ZEROS        X
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C*  WE FIRST LOOK THROUGH ALL THE CPT4 CODES THAT HAVE BEEN PROCESS
     C*  PRIOR AND GET THE BODY PART OFF THEM.  IF WE HAVE PROCESSED THE
     C*  SAME BODY PART WE FALL THROUGH AND GIVE AN EOMB WITHOUT PAYING.
     C*  ELSE WE PUT IN A TABLE.  DETERMINED BY THE AMOUNT IN THE TABLE
     C*  THEY WILL PAY A PERDETERMIND PERCENT.
     C*
     C     XRPART        LOOKUP    PRT                                    61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     XRPART        ANDNE     DHAREA
     C                   ADD       1             X
     C                   MOVEL     XRPART        PRT(X)
     C                   ELSE
     C*
     C* THE ELSE IS TO BYPASS ANY CPT4 CODES WITH DUPLICATE CPT4 CODES
     C* AND BODY PART.  IT WILL PAY NOTHING.  SETS X TO 6 FOR EOMB ON
     C* BOTTOM OF EDIT.
     C*
     C                   Z-ADD     6             X
     C                   LEAVE
     C                   ENDIF                                                  LOKUP PRT
     C                   ENDIF                                                  DHPRCD = XRCPT4
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     X             IFEQ      1
     C     SXMRA         MULT      .60           SXMRA
     C                   MOVEL     '51'          XPPMOD
     C                   ENDIF                                                  END X =1
     C*
     C     X             IFEQ      2
     C     SXMRA         MULT      .40           SXMRA
     C                   MOVEL     '51'          XPPMOD
     C                   ENDIF
     C*
     C     X             IFEQ      3
     C     X             OREQ      4
     C     SXMRA         MULT      .25           SXMRA
     C                   MOVEL     '51'          XPPMOD
     C                   ENDIF
     C*
     C     X             IFGE      5
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END X GE 5
     C                   END                                                    END EQ '97010'
     C*** END ADD ***** 01/10/97 ***********************************************
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*=====================================================================
     C* If modifier -20 MICROSURGERY IS used, inform user
     C* UP TO 20% OF THE FEE SCHEDULE ALLOWABLE MAY BE REIMBURSED
     C*
     C     XPPMOD        IFEQ      '20'
     C     SXMRA         MULT      .20           SXMRA
     C                   END                                                    END EQ '20'
     C*
     C* ===============================================================
     C* Modifier -26 = Professional Component
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     SXPROF        SXMRA
     C                   END                                                    END EQ '26'
     C*
     C* ===============================================================
     C* Modifier -27 = TECHNICAL Component
     C     XPPMOD        IFEQ      '27'
     C                   Z-ADD     SX92N1        SXMRA
     C                   END                                                    END EQ '27'
     C*
     C* ===============================================================
     C* Modifier -46 = ANESTHESIA BY CRNA
     C     XPPMOD        IFEQ      '46'
     C* REDUCE 20%
     C     SXMRA         MULT      .80           SXMRA
     C*
     C                   END                                                    END XPPMOD
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
     C*
     C* REDUCE 10%
     C     SXMRA         MULT      .10           SXMRA
     C*
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Radiology                  Codes: 80002 - 89399         R A D I *
     *********************************************************************
     C     RADI          BEGSR
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
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
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
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 73-77
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ELSE                                                   ELSE EQ 20...
     C*
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AH'          QQC(Q)
     C* Err Msg AH: "Refer to corresponding aneshtesia code for
     C*             base units."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END 47
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
     C                   END                                                    END 62
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
     C                   END                                                    END 66
     C*
     C     XPPMOD        IFEQ      '80'
     C*
     C* SETLL and read the file testing for an surgery code on same bill
     C* without mod 80
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRPMOD        ANDNE     '80'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AI'          QQC(Q)
     C* Err Msg AI: "Enter charge on serperate EOMB to properly
     C*             calculate."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END 80
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 20
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C*** BEGIN DEL *** 02/20/97 **************************************
     C*  EDIT WAS REMOVED DUE TO UPDATE FOR MISSISSIPPI
     C*
     C* Per pg. 71, A, 1
     C*** Replacement casting and strapping codes (29000-29799) must not
     C*** be used for initial care.
     C*********
     C*********  DHPRCD    IFGE '29000'
     C*********  DHPRCD    ANDLE'29799'
     C*********
     C**SETLL*and read the file testing for an initial visit.
     C*********            MOVE '99201'   XXCPT4
     C*********  CAKY9     SETLLPCA09
     C*********
     C*********  CAKY9A    READEPCA09                    60
     C*********
     C*********  *IN60     DOWEQ'0'
     C*********
     C*********  XRCPT4    IFGE '99201'
     C*********  XRCPT4    ANDLE'99205'
     C*********  XRCPT4    ORGE '99218'
     C*********  XRCPT4    ANDLE'99223'
     C*********  XRCPT4    ORGE '99251'
     C*********  XRCPT4    ANDLE'99255'
     C*********  XRCPT4    ORGE '99321'
     C*********  XRCPT4    ANDLE'99323'
     C*********  XRCPT4    ORGE '99341'
     C*********  XRCPT4    ANDLE'99343'
     C*********  XRCPT4    ORGE '99381'
     C*********  XRCPT4    ANDLE'99387'
     C*********
     C*********            Z-ADDCACHRG    XPTOTD
     C*********            Z-ADD*ZERO     XPTOTA
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            MOVE '005'     XPEOMB
     C****EOMB*005: "This procedure is included in the basic allowance
     C*********      of another procedure"
     C*********
     C*********            EXSR UPDAT
     C*********            GOTO SURGE
     C*********
     C*********            END
     C*********
     C*********  CAKY9A    READEPCA09                    60
     C*********
     C*********            END                             END 0 DOWEQ 60
     C*********            END                             END EQ 29000...
     C*
     C*** END DEL ***** 02/20/97 **************************************
     C* ===============================================================
     C*
     C* PER PG 67, I
     C* DIAGNOSTIC PROCEDURES DURING THE SAME SESSION AND ENTRY SITE ARE
     C* INCIDENTAL TO THE MAJOR PROCEDURE
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
     C* PER PG 67, I
     C* DIAGNOSTIC PROCEDURES FOLLOWED BY ANOTHER SURGERY CODE (NON ENDO
     C* SCOPIC) PAY DIAGNOSTIC AT 50%
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
     C*
     C* PER PG 72, D
     C* ARTHROSCOPIC PROCEDURES DURING THE SAME SESSION AND ENTRY SITE ARE
     C* INCIDENTAL TO THE MAJOR PROCEDURE
     C*
     C     DHPRCD        IFEQ      '29800'
     C     DHPRCD        OREQ      '29815'
     C     DHPRCD        OREQ      '29830'
     C     DHPRCD        OREQ      '29840'
     C     DHPRCD        OREQ      '29870'
     C*
     C* SETLL and read the file testing for another Arthroscopic procedure
     C                   MOVE      '29800'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '29801'
     C     XRCPT4        ANDLE     '29814'
     C     XRCPT4        ORGE      '29816'
     C     XRCPT4        ANDLE     '29829'
     C     XRCPT4        ORGE      '29831'
     C     XRCPT4        ANDLE     '29839'
     C     XRCPT4        ORGE      '29841'
     C     XRCPT4        ANDLE     '29869'
     C     XRCPT4        ORGE      '29871'
     C     XRCPT4        ANDLE     '29894'
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
     C                   END                                                    END EQ 29800...
     C*
     C* ===============================================================
     C*
     C* PER PG 72, d
     C* ARTHROSCOPIC PROCEDURES FOLLOWED BY ANOTHER SURGERY CODE (NON ARTHRO
     C* SCOPIC) PAY DIAGNOSTIC AT 50%
     C*
     C     DHPRCD        IFEQ      '29800'
     C     DHPRCD        OREQ      '29815'
     C     DHPRCD        OREQ      '29830'
     C     DHPRCD        OREQ      '29840'
     C     DHPRCD        OREQ      '29870'
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
     C                   END                                                    END EQ 29800...
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPMS Workfile
     C* Only load "current charges" into the FACAPMS Workfile
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE PROCEDURES FOUND IN TABLE NC1
     C*  THESE WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C*
     C                   Z-ADD     SXMRA         CAMRA
     C                   MOVE      'S'           CATYPE
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
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     SXMRA         MULT(H)   .75           SXMRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
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
     C                   Z-ADD     0             COUNTA                         ADD 06/23/93
     C                   Z-ADD     0             CNTB1                          ADD 06/23/93
     C                   Z-ADD     0             CNTB2                          ADD 06/23/93
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
     C*** BEGIN CHG *** 06/23/93 **************************************
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
     C*** END CHG *** 06/23/93 ****************************************
     C                   END                                                    END NE *ZEROS
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 06/23/93 **************************************
     C*
     C* ===============================================================
     C* Per Chpt. 51, per pg. 22, 3
     C* Pay specailty code "AS" at 20% of normal fee
     C*
     C     SVSPCD        IFEQ      'AS '
     C     P2MRA         MULT(H)   .20           P2MRA
     C                   END                                                    END EQ 'AS'
     C*
     C* Per pg. 21, D
     C* One operative site: Body Part "O"
     C*  Highest value 100%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th and 5th    25%
     C*  after 5th      requires report
     C*
     C     P2PART        IFEQ      'O'
     C*
     C     COUNT         IFLE      0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      1
     C                   Z-ADD     .60           P2BRP                          60%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFEQ      2
     C                   Z-ADD     .40           P2BRP                          40%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 2
     C     COUNT         IFGE      3
     C     COUNT         ANDLE     4
     C                   Z-ADD     .25           P2BRP                          25%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE GE 3/LE 4
     C     COUNT         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             COUNT
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3/LE 4
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'O'
     C* Per pg. 21, E
     C* Seperate Organ/Anatomical: Body Part "A"
     C*  Highest value 100%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th and 5th    25%
     C*  after 5th      requires report
     C*
     C     P2PART        IFEQ      'A'
     C*
     C     COUNTA        IFLE      0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE EQ 0
     C     COUNTA        IFEQ      1
     C                   Z-ADD     .60           P2BRP                          60%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE EQ 1
     C     COUNTA        IFEQ      2
     C                   Z-ADD     .40           P2BRP
     C                   ADD       1             COUNTA
     C                   ELSE
     C     COUNTA        IFGE      3
     C     COUNTA        ANDLE     4
     C                   Z-ADD     .25           P2BRP
     C                   ADD       1             COUNTA
     C                   ELSE
     C     COUNTA        IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             COUNTA
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C* Per pg. 21, F
     C* Bilateral: Body Part "B1" OR "B2"
     C*
     C     P2PART        IFEQ      'B1'
     C*
     C     X25CUT        IFEQ      'Y'
     C     CNTB1         IFGE      3
     C     CNTB1         ANDLE     4
     C                   Z-ADD     .25           P2BRP
     C                   ADD       1             CNTB1
     C                   END
     C                   ELSE
     C*
     C     CNTB1         IFLE      0
     C     CNTB2         IFGT      0
     C                   Z-ADD     .75           P2BRP
     C                   Z-ADD     1             CNTB1
     C                   ELSE
     C                   Z-ADD     1             P2BRP
     C                   ADD       1             CNTB1
     C                   END
     C                   ELSE
     C     CNTB1         IFEQ      1
     C                   Z-ADD     .60           P2BRP
     C                   ADD       1             CNTB1
     C                   ELSE
     C     CNTB1         IFEQ      2
     C                   Z-ADD     .40           P2BRP
     C                   ADD       1             CNTB1
     C                   MOVE      'Y'           X25CUT
     C                   ELSE
     C     CNTB1         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             CNTB1
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END GT 4
     C                   END                                                    END GE 2/LE 4
     C                   END                                                    END EQ 1
     C                   END                                                    END LE 0
     C*
     C                   END                                                    END EQ X25CUT
     C*
     C                   ELSE                                                   END EQ 'B1'
     C*
     C     P2PART        IFEQ      'B2'
     C*
     C     X50CUT        IFEQ      'Y'
     C     CNTB2         IFGE      3
     C     CNTB2         ANDLE     4
     C                   Z-ADD     .25           P2BRP
     C                   ADD       1             CNTB2
     C                   END
     C                   ELSE
     C*
     C     CNTB2         IFLE      0
     C     CNTB1         IFGT      0
     C                   Z-ADD     .75           P2BRP
     C                   ADD       1             CNTB2
     C                   ELSE
     C                   Z-ADD     1             P2BRP
     C                   ADD       1             CNTB2
     C                   END
     C                   ELSE
     C     CNTB2         IFEQ      1
     C                   Z-ADD     .60           P2BRP
     C                   ADD       1             CNTB2
     C                   ELSE
     C     CNTB2         IFEQ      2
     C                   Z-ADD     .40           P2BRP
     C                   ADD       1             CNTB2
     C                   MOVE      'Y'           X50CUT
     C                   ELSE
     C     CNTB2         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             CNTB2
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END GT 4
     C                   END                                                    END EQ 2
     C                   END                                                    END EQ 1
     C                   END                                                    END EQ 0
     C*
     C                   END                                                    END EQ X50CUT
     C*
     C                   END                                                    END EQ 'B2'
     C                   END                                                    END EQ 'B1'
     C                   END                                                    END EQ 'A'
     C                   END                                                    END EQ 'O'
     C*** END ADD *** 06/23/93 ****************************************
     C*
     C     P2MRA         MULT      P2BRP         P2MRA
     C*
     C     P2PART        IFEQ      'B'
     C     P2MRA         MULT      1.50          P2MRA
     C                   END                                                    END EQ 'B'
     C*** BEGIN ADD *** 12/14/94 ***********************************************
     C     QQSPCD        IFEQ      'NP '
     C     P2MRA         MULT(H)   .75           P2MRA
     C                   END                                                    END QQSPCD EQ 'NP '
     C*** END ADD *** 12/14/94 *************************************************
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
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
     C     *IN61         IFEQ      *ZERO
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
     C     P2TOTP        ANDGT     *ZEROS
     C                   MOVE      '002'         P2EOMB
     C* EOMB Code 002: "Charges exceed maximum allowance"
     C                   END                                                    END P2TOTD GT *ZERO
      *
     C* Set the sequence number key field to write the EOMB code
     C     P2EOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   MOVE      P2EOMB        XPEOMB
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C                   Z-ADD     P2FEIN        CAFEIN
     C                   Z-ADD     P2MOD         CAMOD
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVEL     P2PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
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
     C                   UPDATE    PCA08
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   ENDIF                                                  IN61=OFF
     C*
     C                   READ      PCAWK2                                 60
     C*
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
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
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
      *
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
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
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
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
DLTOVR FILE(FACALMS)
OVRDBF FILE(FACALMS)  TOFILE(QTEMP/FACALMS)
DLTOVR FILE(FACAPMS)
OVRDBF FILE(FACAPMS)  TOFILE(QTEMP/FACAPMS)
**  NC1 CODES NOT TO CUT LIST 1
1100111101112011170111711117311173211922151011512115201
1522115421152611578715936159371594615956159581604217001
1700217101171021720117310190011912619291193402069020692
2090020902209102091220920209222092420926221452214822230
2258522650228202612526861268632735827692333503351733518
3351933521335223352333530339613539035681357003621836248
3720637208381023874638747411144363544015441214413944955
4700147550495684990556606586115941259525611066113061609
6161061611616126171261795630356304863057630666307663078
6308263086630886309163308644436462364727647786478364787
6483064832648376485964872648746487664901649026732067331
6733267335673407430175774759647596275993759967612576350
7847878480829528691187088871068716387187872538828088283
8828588289883118831288313883148833290781929819298492996
9332593539935409354193542935439354493545935559355693623
9595795962964129642397261975019752197541975469770199292
993559935799359
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
**  ANES/SUG: SUGERY CODES WITH SPECIALTY ANE OR RNA TO BE PAID AS ANES
62270
62273
62274
62275
62276
62277
62278
62279
62280
62282
62288
62289
63780
64400
64402
64405
64408
64410
64412
64413
64415
64417
64418
64420
64421
64425
64430
64435
64440
64441
64445
64450
64505
64508
64510
64520
64530
64600
64605
64610
64620
64630
64640
64680
