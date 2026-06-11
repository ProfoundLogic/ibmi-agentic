     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*
     H* PROGRAMMER NOTE: THE MAIN SECTION HAS BEEN DUPLICATED
     H* IN ORDER TO MAKE TWO PASSES OF THE DATA
     H*
     H*    PROGRAM: F4071ROK                                          *
     H*    DESCRIPTION: ADJUDICATION (OKLAHOMA RULES - 1/96)          *
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Oklahoma State
     H* Fee Schedule Reimbursment Manual issued on 1/01/96.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 01/01/96 and before 01/01/98.                                 *
     H*                                                               *
     H*  NOTE:  960101 IS HARD CODED IN PROGRAM                       *
     H*****************************************************************
     H*  CREATED: 11/28/95 C:32    PROJECT: OK003                     *
     H*****************************************************************
     H*  Specialty Codes used in this program:
     H*
     H*  ANES Subroutine: (A - 1)
     H*      ANE       - Anesthesiologist
     H*      RNA       - Certified Registered Nurse Anesthetist
     H*
     H*  PHYS Subroutine: (PM - 1)
     H*      PHT       - Licensed Physical Therapist
     H*      OCT       - Occupational Therapy             ADD 01/19/96
     H*      OT        - Occupational Therapist           ADD 01/19/96
     H*
     H*****************************************************************
     H*                                     Conversion      FS Manual
     H*  Subroutines/Codes                    Factor          Pages
     H*      ANES     Anesthesia              $  34.45        A - 1
     H*
     H*      MEDI     Medicine / Eval and Mgmt                M - 1
     H*          90780-96999, 99001-99199 / 99201-99499
     H*
     H*      PATH     Pathology                               P - 1
     H*          80002-83999
     H*
     H*      PHYS     Physical Medicine                      PM - 1
     H*          97010-97799, 98925-98929
     H*
     H*      RADI     Radiology                               R - 1
     H*          70010-79999
     H*
     H*      SURG     Surgery                                 S - 1
     H*          10000-69979
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  06/08/99 C:79 PROJ: OK008
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
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  05/27/98 C:31  Support: 11772
     H*                 CPT4 Issue
     H*                 1. Change range of CPT4
     H*
     H*----------------------------------------------------------------
     H*  01/19/96 C:32   REQ/PROJ: OK003
     H*                  1. ADDED DIVISION BY 60 TO GET MIN RATE FOR
     H*                     SPECIFIC MEDICAL CODES.                    E
     H*                  2. CHGD PT EDIT FOR ONLY 4 VISITS ALLOWED
     H*                     IF PROCEDURES ARE ALL MODALITIES
     H*                  3. ADDED EDIT FOR 97700 (ONLY 1 ALLOWED
     H*                     IF SPEC = PHT OR OCT)
     H*                  4. ADDED OFFICE VISITS RANGES TO FUD
     H*                  5. ADDED 62289 TO EDIT FOR EPIDURAL INJ
     H*                  6. ADDED EDIT TO ALLOW FOR 21 VISITS
     H*                     INSTEAD OF 18, IF ANY PT PRIOR TO 960101
     H*
     H*  04/01/96 C:32   REQ/PROJ: OKMNT
     H*                  1. ADDED 63020 TO MAJOR/MINOR FOR 63035
     H*                  2. FUD - CHG TO EXCLUDE OFFICE VIS ON SAME
     H*                     DAY OF SURG (WAS USING GREATER THAN)
     H*                  3. ADD DUP OF MAIN CODE FOR 2 PASSES OF DATA
     H*                  4. CHG PART TABLE LOKUP TO NOT USE BLANKS
     H*                  5. RESTART COUNTER IF NON-MODALITIES PERFORM
     H*                     FOR 2 WEEK CNTR.
     H*                  6. CHG MOD -80 TO PAY 20% (ALWAYS)
     H*                  7. ADD MOD -81 TO PAY 10%
     H*                  8. CHG KEYS IN UPDAT3/UPDAT5 TO NOT USE CAFEIN
     H*                  9. CHG SURG2 & RADI2 TO ALWAYS LOAD XPPMOD
     H*                 10. ADD ERR MSG FOR SURG WITHIN ANOTHER SURG
     H*                     SURG FOLLOW-UP DAYS
     H*
     H*  05/21/96 C:32   REQ/PROJ: OKMNT
     H*                  1. CHG SURG2 / RADI2 TO COMPUTE 50% UPTO TWICE
     H*                     THE MAX MRA
     H*                  2. CHG TO INCLUDE ALL MODIFIERS IN SURG2/RADI2
     H*
     H*  06/04/96 C:32   REQ/PROJ: OKMNT
     H*                  1. ADD MSG FOR OFFICE VISIT CODES WHICH DO NOT
     H*                     SPECIFY NEW OR ESTABLISHIED WHICH OCCUR
     H*                     WITH AN ASTERISK PROC.
     H*                  2. ADD RANGE FOR PT CODES (4 COMBO EDIT)
     H*
     H*  11/25/96 C:59   REQ/PROJ: OKMNT
     H*                  MADE CORRECTIONS TO MOD -80 AND -81
     H*                  MULTIPLE MODIFIERS WERE BEING CUT TO MANY
     H*                  TIMES.
     H*****************************************************************
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*  54   CHAIN/DOWEQ on PCA01 - Detail Charge Primary File      **
     H*  60   General Usage                                          **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPRLOK   IF   E           K DISK
     FFTABLOK   IF   E           K DISK
     F                                     RENAME(PRC01:PRC0W)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALH    UF   E           K DISK
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFMRCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFACAPOK   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT1)
     FFACAL1OK  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT3)
     FFACAL3OK  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT5)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
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

     D*
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D* SAVE ENTRY ARRAYS
     D*
     D TABA            S              5    DIM(41) CTDATA PERRCD(1)             NO CUT/MAJOR-MIN
     D TABB            S              5    DIM(41) ALT(TABA)
     D*
     D*** BEGIN DEL *** 04/01/96 **************************************
     D*****************   PTSC       50  5  A            PT Summary Cde
     D*****************   PTSM       50  9 2             PT Summary MRA
     D*****************   PTSS       50 14  D            PT Summary
     D*** END   DEL *** 04/01/96 **************************************
     D MNPL            S              7  0 DIM(3)                               Manipulation Dates
     D QCA             S             55    DIM(10) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
     D PART            S              2    DIM(20)                              BODY PART TBL
     D PCNT            S              7  0 DIM(20)                              BODY PART CNTR
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*** BEGIN ADD *** 01/19/96 **************************************
     D VIST            S              7  0 DIM(40)                              # OF VISITS      s
     D*** END   ADD *** 01/19/96 **************************************
     D*** BEGIN ADD *** 11/25/96 *************************************
     D** MODIFIER ARRAY
     D MOD             S              2    DIM(100)
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

     I*** END ADD ***** 11/25/96 *************************************
     I*
     I*
     IPRC0W
     I              RCKEY                       RCKEYW
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
     I* Record Format for Physical Therapy Workfile FACAL1OK
     IPCAPT3
     I              CACPT4                      P3CPT4
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
     I              CAICD9                      P3ICD9
     I              CAANMN                      P3ANMN
     I              CAUNTS                      P3UNTS
     I              CATOTA                      P3TOTA
     I              CATOTC                      P3TOTC
     I              CATOTD                      P3TOTD
     I              CATOTP                      P3TOTP
     I              CADSEQ                      P3DSEQ
     I              CADELF                      P3DELF
     I              CAFLAG                      P3FLAG
     I              CASTAT                      P3STAT
     I              CAP1                        P3P1
     I              CAPMOD                      P3PMOD
     I              CAUSER                      P3USER
     I              CAPGM                       P3PGM
     I              CADT                        P3DT
     I              CATIME                      P3TIME
     I              CACHK#                      P3CHK#
     I              CAEOB#                      P3EOB#
     I              CAPART                      P3PART
     I              CAEOMB                      P3EOMB
     I              CARCFL                      P3RCFL
     I              CAMRA                       P3MRA
     I              CAMMRA                      P3MMRA
     I              CASMCD                      P3SMCD
     I              CADIFF                      P3DIFF
     I              CATYPE                      P3TYPE
     I* Record Format for Radiology Workfile FACAL3OK
     IPCAPT5
     I              CACPT4                      P5CPT4
     I              CACO#                       P5CO#
     I              CASUB#                      P5SUB#
     I              CAFND                       P5FND
     I              CAFYR                       P5FYR
     I              CACASE                      P5CASE
     I              CACSCD                      P5CSCD
     I              CAFDAT                      P5FDAT
     I              CATDAT                      P5TDAT
     I              CARDAT                      P5RDAT
     I              CAIPOP                      P5IPOP
     I              CAFEIN                      P5FEIN
     I              CAMOD                       P5MOD
     I              CAPRTY                      P5PRTY
     I              CATRCD                      P5TRCD
     I              CASVCD                      P5SVCD
     I              CAICD9                      P5ICD9
     I              CAANMN                      P5ANMN
     I              CAUNTS                      P5UNTS
     I              CATOTA                      P5TOTA
     I              CATOTC                      P5TOTC
     I              CATOTD                      P5TOTD
     I              CATOTP                      P5TOTP
     I              CADSEQ                      P5DSEQ
     I              CADELF                      P5DELF
     I              CAFLAG                      P5FLAG
     I              CASTAT                      P5STAT
     I              CAP1                        P5P1
     I              CAPMOD                      P5PMOD
     I              CAUSER                      P5USER
     I              CAPGM                       P5PGM
     I              CADT                        P5DT
     I              CATIME                      P5TIME
     I              CACHK#                      P5CHK#
     I              CAEOB#                      P5EOB#
     I              CAPART                      P5PART
     I              CAEOMB                      P5EOMB
     I              CARCFL                      P5RCFL
     I              CAMRA                       P5MRA
     I              CAMMRA                      P5MMRA
     I              CASMCD                      P5SMCD
     I              CADIFF                      P5DIFF
     I              CATYPE                      P5TYPE

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
     C     CAKY8A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    H1DSEQ
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
     C     CAKYA         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SXFEIN
     C                   KFLD                    SXMOD
     C*
     C     CAKYA3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    P3DSEQ
     C*
     C     CAKYA5        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    P5DSEQ
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
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*
     C     CAKYPX        KLIST
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
     C     CAKY6X        KLIST
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
     C*
     C*** END   ADD *** 04/01/96 **************************************
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
     C     RCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD
     C*** BEGIN DEL *** 08/09/02 **************************************
     C*** Geographical Zip Area FILE
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
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
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#
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
     C* - Initialization Section --------------------------------------
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    DHPRCD        CPTFR
     C     *LIKE         DEFINE    DHPRCD        CPTTO
     C     *LIKE         DEFINE    DHPRCD        PPCPT4
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAFEIN        PPFEIN
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAMOD         PPMOD
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        PPFDAT
     C     *LIKE         DEFINE    CADSEQ        H1DSEQ
     C     *LIKE         DEFINE    CADSEQ        PPDSEQ
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAFDAT        H3FDAT                         ADD 01/19/96
     C     *LIKE         DEFINE    DHPRCD        N2CPT4
     C     *LIKE         DEFINE    PRMRA         OFCMRA
     C     *LIKE         DEFINE    PRMRA         SURMRA
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C     *LIKE         DEFINE    DHPRCD        XHCPT4
     C     *LIKE         DEFINE    DHPRCD        ZZCPT4
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C     *LIKE         DEFINE    DHPRCD        HLPTSC
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
     C     *LIKE         DEFINE    PR1A2         SX1A2
     C     *LIKE         DEFINE    PR1A3         SX1A3                          TECHNICAL COMP FL
     C     *LIKE         DEFINE    PR1A5         SX1A5                          POUND FLAG
     C     *LIKE         DEFINE    PR1A6         SX1A6                          AMPERSAND FLAG
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
     C     *LIKE         DEFINE    PRPROF        SXPROF
     C     *LIKE         DEFINE    PR92N1        SX92N1                         TECHNICAL COMP
     C     *LIKE         DEFINE    PRRPT         SXRPT
     C     *LIKE         DEFINE    PRSUPP        SXSUPP
     C     *LIKE         DEFINE    PRSURG        SXSURG
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*
     C     *LIKE         DEFINE    DHPRCD        XXCPCD
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    DHAREA        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    DHPRCD        XXSMCD
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C*** BEGIN ADD *** 04/01/96 **************************************
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C*** END   ADD *** 04/01/96 **************************************
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C*
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     0             CT                3 0
     C                   Z-ADD     0             F                 1 0
     C                   Z-ADD     0             FC                1 0
     C                   Z-ADD     0             I                 3 0
     C                   Z-ADD     0             P                 5 0
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   MOVE      ' '           XFIRST            1            First time into pgm
     C                   MOVE      '0'           XX25EM            1
     C                   Z-ADD     0             PHIGH            10 2
     C                   Z-ADD     0             LHIGH            10 2
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             MAXADD            5 0
     C                   Z-ADD     0             MRA1              9 2
     C                   MOVE      *BLANK        OPNFLG            1
     C                   MOVE      *BLANK        OVRFLG            1
     C                   Z-ADD     0             XXALOW            9 2
     C                   Z-ADD     0             XXANMN            5 2
     C                   Z-ADD     0             XXMRA             9 2
     C                   Z-ADD     0             XXMODU            2 0
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XXPERC            5 2
     C                   Z-ADD     0             XXWORK            6 2
     C                   MOVE      *BLANK        XX50              1            Flag to pay PT at 50
     C                   Z-ADD     0             ZZMRA             9 2
     C                   Z-ADD     0             Z1MRA             9 2
     C                   Z-ADD     0             Z2MRA             9 2
     C*
     C* ---------------------------------------------------------------
     C* - Establish Conversion Factor --------------------------------
     C                   Z-ADD     34.45         XXANES            7 2
     C* ---------------------------------------------------------------
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
     C* ---------------------------------------------------------------
     C     CAFDAT        IFGE      960101
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
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C* If the Service Date is greater than the system date, give err message
     C*
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
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '207'         XPEOMB
     C* EOMB Code 207: "This charge has been paid per R&C fees"
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
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
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
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
     C*-- Supplies & Materials -------------------------------------------------
     C*
     C*  Per A -1 #6: 99070 Payment shall not exceed the cost of the
     C*              item(s) to the physician plus 25% not to exceed
     C*              $15.00 for a single item.
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '53'          QQC(Q)
     C*** Err Msg 53: "Payment for 99070 not to exceed cost of item +
     C***              25%, not to exceed $15/single item"
     C                   END
     C*
     C*
     C*-------------------------------------------------------------------------
     C*
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '205'         XPEOMB
     C* EOMB Code 205: "Charges exceed Reasonable & Customary"
     C   54              Z-ADD     PRMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
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
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   Z-ADD     PR92N1        SX92N1
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A5         SX1A5
     C                   MOVE      PR1A6         SX1A6
     C*
     C*
     C* WE ONLY WANT TO PROCESS THE BY-REPORT FIELD IF THIS CODE IS
     C* NOT SUPPOSED TO DROP DOWN AND PROCESS PER ANES CALCULATIONS
     C* THIS WILL BE INDICATED BY BODY PART 'AN'
     C     DHAREA        IFNE      'AN'
     C     DHAREA        OREQ      'AN'
     C     DHPRCD        ANDGE     '00100'
     C     DHPRCD        ANDLE     '01999'
     C     DHAREA        OREQ      'AN'
     C     SVSPCD        ANDNE     'ANE'
     C     SVSPCD        ANDNE     'RNA'
     C*----------------------------------------------------------------
     C*-- By Report ---------------------------------------------------
     C*
     C     PRRPT         IFEQ      'Y'
     C     PR1A6         ANDNE     'Y'                                          AMPERSAND 'Y'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = B, then it is BR
     C*** If Mod -27 and Tech Component Flag = B, then it is BR
     C     XPPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'B'
     C     XPPMOD        OREQ      '27'
     C     SX1A3         ANDEQ     'B'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    ENDEQ 26/'B'
     C*
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 06: "This item is BR  - requires further inspection
     C***              by an adjuster"
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF                                     2
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF                                     2
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- ANE/Amount Not Established ----------------------------------
     C*
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = A, then it is ANE
     C*** If Mod -27 and Tech Component Flag = A, then it is ANE
     C     XPPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'A'
     C     XPPMOD        OREQ      '27'
     C     SX1A3         ANDEQ     'A'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    ENDEQ 26/'A'
     C*
     C     XPPMOD        IFEQ      '55'
     C     XPPMOD        OREQ      '80'
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   END                                                    END EQ 55/80
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
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
     C                   MOVE      *ZEROS        XPDIFF
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- SV/Service Item ---------------------------------------------
     C*
     C     PRRPT         IFEQ      'S'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = S, then it is SV
     C     XPPMOD        IFEQ      'SV'
     C     SX1A1         ANDEQ     'S'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '52'          QQC(Q)                         SV MESSAGE
     C*** Err Msg 52: "This item is SV - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    ENDEQ 26/'S'
     C*
     C     XPPMOD        IFEQ      '27'
     C     XPPMOD        OREQ      '55'
     C     XPPMOD        OREQ      '80'
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '52'          QQC(Q)                         SV MESSAGE
     C*** Er  Msg 52: "This item is SV - requires further inspection
     C***              by an adjuster"
     C                   END                                                    END EQ 27/55/80
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
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
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- NC/Not Covered ----------------------------------------------
     C*
     C     PRRPT         IFEQ      'X'
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C                   EXSR      UPDAT
     C*
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'X'
     C*
     C                   END                                                    END DHAREA NE 'AN'
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
     C*
     C                   EXSR      SETFL2                                       Setup wrk fl
     C                   EXSR      SURG                                         Surgery
     C                   ELSE
     C     DHPRCD        IFGE      '70010'
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      SETFL2                                       Setup wrk fl
     C                   EXSR      RADI                                         Radiology
     C                   ELSE
     C     DHPRCD        IFGE      '80002'
     C     DHPRCD        ANDLE     '89399'
     C                   EXSR      PATH                                         Pathology
     C                   ELSE
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C     DHPRCD        ORGE      '90780'
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '99001'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI                                         Medicine
     C                   ELSE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C                   EXSR      SETFL2                                       Setup wrk fl
     C                   EXSR      PHYS                                         Phys Therapy
     C                   ELSE
     C                   EXSR      OTHE                                         Other Codes
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
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
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *
     C*                    Z-ADD*ZEROS    XPTOTA
      *
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C*
     C                   END                                                    END EQ OVRRD
     C*
     C                   ENDIF                                                  CAFDAT GE
     C*
     C*----------------------------------------------------------------
     C*-- Process next record -----------------------------------------
     C     NXTRCD        TAG
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C                   END
     C*
     C                   ENDDO                                                  *IN54 DOWEQ
     C*
     C*
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      RADI2
     C                   EXSR      SURG2
     C                   END
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*****************************************************************
     C* THE FOLLOWING CODE WAS DUPLICATED TO CATCH ERRORS NOT FOUND
     C* ON FIRST PASS OF DATA.
     C*****************************************************************
     C*  DUP OF MAIN CODE !!
     C*
     C                   MOVE      ' '           XFIRST
     C*
     C* INITIALIZE ADJUDICATION MESSAGE ARRAYS
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
     C     CAFDAT        IFGE      960101
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
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   EXSR      LODFU
     C*
     C* If the Service Date is greater than the system date, give err message
     C*
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
     C     RCKY          SETGT     PRC01
     C     N2RC1         TAG
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
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '207'         XPEOMB
     C* EOMB Code 207: "This charge has been paid per R&C fees"
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE
     C                   GOTO      N2RC1
     C                   END
     C                   END
     C*
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
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
     C                   GOTO      N2TRCD
     C                   END
     C*
     C*----------------------------------------------------------------
     C*  Test for Fee Schedule Reimbursement - 'Y'
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
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
     C*-- Supplies & Materials -------------------------------------------------
     C*
     C*  Per A -1 #6: 99070 Payment shall not exceed the cost of the
     C*              item(s) to the physician plus 25% not to exceed
     C*              $15.00 for a single item.
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '53'          QQC(Q)
     C*** Err Msg 53: "Payment for 99070 not to exceed cost of item +
     C***              25%, not to exceed $15/single item"
     C                   END
     C*
     C*
     C*-------------------------------------------------------------------------
     C*
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '205'         XPEOMB
     C* EOMB Code 205: "Charges exceed Reasonable & Customary"
     C   54              Z-ADD     PRMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
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
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   Z-ADD     PR92N1        SX92N1
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A5         SX1A5
     C                   MOVE      PR1A6         SX1A6
     C*
     C*
     C* WE ONLY WANT TO PROCESS THE BY-REPORT FIELD IF THIS CODE IS
     C* NOT SUPPOSED TO DROP DOWN AND PROCESS PER ANES CALCULATIONS
     C* THIS WILL BE INDICATED BY BODY PART 'AN'
     C     DHAREA        IFNE      'AN'
     C     DHAREA        OREQ      'AN'
     C     DHPRCD        ANDGE     '00100'
     C     DHPRCD        ANDLE     '01999'
     C     DHAREA        OREQ      'AN'
     C     SVSPCD        ANDNE     'ANE'
     C     SVSPCD        ANDNE     'RNA'
     C*----------------------------------------------------------------
     C*-- By Report ---------------------------------------------------
     C*
     C     PRRPT         IFEQ      'Y'
     C     PR1A6         ANDNE     'Y'                                          AMPERSAND 'Y'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = B, then it is BR
     C*** If Mod -27 and Tech Component Flag = B, then it is BR
     C     XPPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'B'
     C     XPPMOD        OREQ      '27'
     C     SX1A3         ANDEQ     'B'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    ENDEQ 26/'B'
     C*
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 06: "This item is BR  - requires further inspection
     C***              by an adjuster"
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF                                     2
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF                                     2
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF                                     2
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- ANE/Amount Not Established ----------------------------------
     C*
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = A, then it is ANE
     C*** If Mod -27 and Tech Component Flag = A, then it is ANE
     C     XPPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'A'
     C     XPPMOD        OREQ      '27'
     C     SX1A3         ANDEQ     'A'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    ENDEQ 26/'A'
     C*
     C     XPPMOD        IFEQ      '55'
     C     XPPMOD        OREQ      '80'
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   END                                                    END EQ 55/80
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C                   MOVE      *ZEROS        XPDIFF
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- SV/Service Item ---------------------------------------------
     C*
     C     PRRPT         IFEQ      'S'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C*** If Mod -26 and Prof Component Flag = S, then it is SV
     C     XPPMOD        IFEQ      'SV'
     C     SX1A1         ANDEQ     'S'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '52'          QQC(Q)                         SV MESSAGE
     C*** Err Msg 52: "This item is SV - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    ENDEQ 26/'S'
     C*
     C     XPPMOD        IFEQ      '27'
     C     XPPMOD        OREQ      '55'
     C     XPPMOD        OREQ      '80'
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '52'          QQC(Q)                         SV MESSAGE
     C*** Er  Msg 52: "This item is SV - requires further inspection
     C***              by an adjuster"
     C                   END                                                    END EQ 27/55/80
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
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
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*----------------------------------------------------------------
     C*-- NC/Not Covered ----------------------------------------------
     C*
     C     PRRPT         IFEQ      'X'
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C                   EXSR      UPDAT
     C*
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RPT EQ 'X'
     C*
     C                   END                                                    END DHAREA NE 'AN'
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
     C*
     C                   EXSR      SETFL2                                       Setup wrk fl
     C                   EXSR      SURG                                         Surgery
     C                   ELSE
     C     DHPRCD        IFGE      '70010'
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      SETFL2                                       Setup wrk fl
     C                   EXSR      RADI                                         Radiology
     C                   ELSE
     C     DHPRCD        IFGE      '80002'
     C     DHPRCD        ANDLE     '89399'
     C                   EXSR      PATH                                         Pathology
     C                   ELSE
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C     DHPRCD        ORGE      '90780'
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '99001'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI                                         Medicine
     C                   ELSE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C*** BEGIN DEL *** 04/01/96 *************************************
     C*****************    EXSR SETFL2                     Setup wrk fl
     C*** END   DEL *** 04/01/96 *************************************
     C                   EXSR      PHYS                                         Phys Therapy
     C                   ELSE
     C                   EXSR      OTHE                                         Other Codes
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
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
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
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
     C                   MOVE      *ZEROS        CADIFF
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C                   ENDIF                                                  CAFDAT GE
     C*
     C*----------------------------------------------------------------
     C*-- Process next record -----------------------------------------
     C     N2TRCD        TAG
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C                   END
     C*
     C                   ENDDO                                                  *IN54 IFEQ
     C*
     C*
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      RADI2
     C                   EXSR      SURG2
     C                   END
     C*
     C*** END   ADD *** 04/01/96 **************************************
     C*
     C*
     C*----------------------------------------------------------------
     C* --- End the program -------------------------------------------
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
     C                   CLOSE     FACAL1OK
     C                   CLOSE     FACAL3OK
     C                   CLOSE     FACAPOK
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL1OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL3OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPOK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
      *
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  ANESTHESIA SUBROUTINE CODES 00100 - 01999               A N E S *
     C*   ** Use with ASA Relative Value Guide 1991
     C*   ** Per FS Manual Pages A - 1, 2
     C********************************************************************
     C     ANES          BEGSR
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C*--------------------------------------------------------------------
     C*--- Procedure Codes with modifiers: NONE ---------------------------
     C     XPPMOD        IFNE      *BLANK
     C     XPPMOD        IFNE      '23'
     C     XPPMOD        ANDNE     '47'
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
     C                   END                                                    END NE 47
     C                   END                                                    END NE *ZERO
     C*
     C*----------------------------------------------------------------
     C*--- Procedure codes without modifiers----------------------------------
     C*
     C*--- Specialty Code conditioned -------------------------------------
     C*
     C*  ANE and RNA use the values set forth under the respective
     C*  anesthesia charge (as dictated by the ASA Relative Guide 1991)
     C*           ANE - Anesthesiologist
     C*           RNA - Certified Registered Nurse Anesthetist
     C*
     C     SVSPCD        IFEQ      'ANE'
     C     SVSPCD        OREQ      'RNA'
     C*
     C* Per pg. A - 1:                                                 -------
     C*   (Time Units * Conversion Factor) + Basic MRA Value           -------
     C*   Time units entered by user (not minutes).
     C*
     C                   Z-ADD     DHUNTS        XXANMN                         TIME UNITS
     C     XXANMN        MULT      XXANES        XXWORK
     C*
     C     SXANAM        ADD       XXWORK        XPTOTA
     C*
     C*
     C* Per pg. A - 1, #1:                                             -------
     C*  When anesthesia is administered by a CRNA, bill at 90% of     -------
     C*  calculated fee                                                -------
     C     SVSPCD        IFEQ      'RNA'
     C                   MULT      .90           XPTOTA
     C                   END                                                    END EQ RNA
     C*
     C     XPTOTA        IFLT      CACHRG
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Maximum Reimbursement Allowance"
     C                   ELSE
     C     XPTOTA        SUB       CACHRG        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   MOVE      *BLANKS       XPEOMB
     C                   END
     C*
     C                   ELSE                                                   ELS EQ ANE
     C*  Per pg. S - 1:
     C*  Regional and general anesthesia provided by the operating
     C*  surgeon can be charged for by using the corresponding Basic
     C*  Anesthesia Unit Value only.
     C*  FIX THIS !!  SHOULD THIS BE IN SURG OR MED SECTION??
     C*  SETLL and READ the file looking for Surgery Procedures
     C**                   MOVE '10000'   XXCPT4
     C**         CAKEY8    SETLLPCA09
     C**         AGAIN1    TAG
     C*
     C**         CAKEY7    READEPCA09                    54
     C*
     C**N54      XRCPT4    IFGE '10020'
     C**         XRCPT4    ANDLE'69979'
     C*
     C*  Get corresponding surgery code's anesthesia amount from FMPRP
     C**                   MOVE XRCPT4    XXCPT4
     C**                   MOVE XRFDAT    XXEFDT
     C**         PRKEY     SETGTPPR01
     C**                   READPPPR01                    54
     C**         *IN54     IFEQ '0'
     C**         CACO#     ANDEQPRCO#
     C**         XXCPT4    ANDEQPRCPT4
     C**
     C**
     C**                   Z-ADDPRANAM    SXANAM
     C**
     C**                   ELSE
     C**                   GOTO AGAIN1
     C**                   END
     C**
     C**                   END
     C*
     C*
     C                   Z-ADD     PRANAM        SXANAM
     C*  If anesthesia amount from the FMPRP file is zero, then give
     C*  error message that this anesthesia is BR or not established
     C     PRANAM        IFLE      *ZEROS
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '57'          QQC(Q)
     C* Err Msg 57: "Anesthesia amount is either BR or not established
     C*             - Please review"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      *BLANKS       XPEOMB
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELS LE *ZERO
     C*
     C                   Z-ADD     SXANAM        XPTOTA
     C*
     C     XPTOTA        IFLT      CACHRG
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Maximum Reimbursement Allowance"
     C                   ELSE
     C     XPTOTA        SUB       CACHRG        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   MOVE      *BLANKS       XPEOMB
     C                   END
     C                   END                                                    END LE *ZERO
     C*
     C                   END                                                    END EQ ANE
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
     *** LOAD FOLLOW UP ARRAYS SUBROUTINE                      L O D F U *
     *********************************************************************
     C*
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRAYS WITH FOLLOW UP DAYS
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
     C     PRKEY         SETGT     PPR01
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
     C*  Medicine Subroutine          Codes 90780 - 96999        M E D I *
     C*                                     99001 - 99199
     C*         Evaluation and Management:  99201 - 99499
     *********************************************************************
     C*
     C     MEDI          BEGSR
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C* DETERMINE ANES RATE FOR SURGEON
     C*  SURGEON DOES NOT GET ADDITIONAL TIME UNIT CONSIDERATION
     C*
     C     XPPMOD        IFEQ      '47'
     C     SXANAM        IFNE      *ZEROS
     C     SXANAM        MULT      XXANES        XXMRA
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE                                                   ELSE SXANAM = 0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AN'          QQC(Q)
     C*** Err Msg AN: "An anesthesia reimbursement value is not available
     C*                for this code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '217'         XPEOMB
     C* EOMB Code 217: "An anesthesia reimbursement value is not available
     C*                 for this code."
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END SXANAM  S
     C                   END                                                    END XPPMOD
     C*
     C     DHAREA        IFNE      'AN'
     C*----------------------------------------------------------------
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C                   ELSE                                                   ELSE EQ 'AN'
     C** COMPUTE ANESTHESIA RATE
     C     SVSPCD        IFEQ      'ANE'
     C     SVSPCD        OREQ      'RNA'
     C*
     C     SXANAM        IFNE      *ZEROS
     C     DHUNTS        ADD       SXANAM        XXMRA
     C     XXMRA         MULT      XXANES        XXMRA
     C*
     C* Per pg. A - 1, #1:                                             -------
     C*  When anesthesia is administered by a CRNA, bill at 90% of     -------
     C*  calculated fee                                                -------
     C     SVSPCD        IFEQ      'RNA'
     C                   MULT      .90           XXMRA
     C                   END                                                    END EQ RNA
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE                                                   ELSE SXANAM = 0
     C*
     C     SX1A2         IFNE      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AN'          QQC(Q)
     C*** Err Msg AN: "An anesthesia reimbursement value is not available
     C*                for this code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '217'         XPEOMB
     C* EOMB Code 217: "An anesthesia reimbursement value is not available
     C*                 for this code."
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSE SX1A2
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AO'          QQC(Q)
     C*** Err Msg AO: "This charge is an individual consideration."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '218'         XPEOMB
     C* EOMB Code 218: "This charge is an individual consideration."
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END SX1A2
     C*
     C                   END                                                    END SXANAM
     C*
     C                   ELSE                                                   ELSE EQ 'ANE'
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C                   END                                                    END EQ 'ANE'
     C*
     C                   END                                                    END EQ 'AN'
     C*
     C*----------------------------------------------------------------
     C*--- Procedure Codes with modifiers: 26 -----------------------------
     C     XPPMOD        IFNE      *BLANK
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
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
     C                   ELSE                                                   ELS NE 26
     C*
     C     XPPMOD        IFEQ      '26'
     C     PRPROF        IFEQ      *ZEROS
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
     C                   Z-ADD     PRPROF        XXMRA
     C                   END                                                    END PRPROF
     C                   END                                                    END EQ 26
     C*
     C     XPPMOD        IFEQ      '27'
     C     PRPROF        IFEQ      *ZEROS
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
     C     PRMRA         SUB       PRPROF        XXMRA
     C                   END                                                    END PRPROF
     C                   END                                                    END EQ 27
     C*
     C                   END                                                    END NE 26
     C                   END                                                    END NE *ZERO
     C*** BEGIN ADD *** 01/19/96 **************************************
     C* Per pg. M - 3 These codes are per minute in the Fee Sched
     C*     manual.  The value in FMPRPOK is set at the hourly rate.
     C*
     C     DHPRCD        IFEQ      '90855'
     C     DHPRCD        OREQ      '90880'
     C     DHPRCD        OREQ      '90900'
     C     DHPRCD        OREQ      '90902'
     C     DHPRCD        OREQ      '90904'
     C     DHPRCD        OREQ      '90906'
     C     DHPRCD        OREQ      '90908'
     C     DHPRCD        OREQ      '90910'
     C     DHPRCD        OREQ      '90911'
     C     SXANAM        DIV       60            XXMRA
     C     DHUNTS        IFNE      *ZEROS
     C     XXMRA         MULT      DHUNTS        XXMRA
     C                   END
     C                   END
     C*** END   ADD *** 01/19/96 **************************************
     C*
     C*
     C*** IF DHUNTS > *ZERO MULT UNITS * MRA CALCULATED
     C*
     C     DHUNTS        IFNE      *ZEROS
     C     XXMRA         MULT(H)   DHUNTS        XXMRA
     C                   END
     C*-------------------------------------------------------------------------
     C*-- Consultations --------------------------------------------------------
     C*
     C* Per pg E & M - 1 #5: Consultations always require a narrative
     C*        report from the consultant to the attending physician
     C*        requesting the opinion.
     C*
     C     DHPRCD        IFGE      '99241'
     C     DHPRCD        ANDLE     '99275'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '47'          QQC(Q)
     C* Err Msg 47: "A copy of the Consultation Report is required
     C*              for reimbursement"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "Consultation report required for payment"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C*
     C*
     C*-------------------------------------------------------------------------
     C*-- Special Service and Reports ------------------------------------------
     C*
     C*  Per pg. M - 1: Narrative reports requested by the insurance
     C*         (#8)    carrier or self-insured employer involving the
     C*                 review of medical data to clarify a patient's
     C*                 status should be billed using CPT code 99080.
     C*  Max reimbursement: 1st page $38.16, ea additl pg. $15.26
     C*                     not to exceed $76.32
     C*  Display error message if over the  MRA of (1st page value)
     C*
     C     DHPRCD        IFEQ      '99080'
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CE'          QQC(Q)
     C* Err Msg CE: "Narrative rept reimbursement: 1 pg. 38.16,
     C*              ea additl pg 15.26 (not to exceed 76.32)."
     C     *IN54         IFEQ      '1'
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Maximum Reimbursement Allowance"
     C                   END
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C**
     C*
     C*-------------------------------------------------------------------------
     C*-- ANE or RNA Specialty Code --------------------------------------------
     C*
     C*  Per pg. A - 2 #10 b.
     C*              A consultation fee is not payable to an ANE
     C*              examining the patient prior to administering
     C*              anesthesia to that patient.  No additional charge
     C*              is to be made for routine follow-up care and
     C*              observation
     C*
     C*  Counter on number of 99201-99275 between O/V - Err Msg
     C     SVSPCD        IFEQ      'ANE'
     C     DHPRCD        ANDGE     '99201'
     C     DHPRCD        ANDLE     '99275'
     C     SVSPCD        OREQ      'RNA'
     C     DHPRCD        ANDGE     '99201'
     C     DHPRCD        ANDLE     '99275'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '209'         XPEOMB
     C*** EOMB 209: "These services not reimbursable under the Fee Sch
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C*
     C*** Per pg. S - 1 #1
     C*** PACKAGE OR GLOBAL FEE CONCEPT: Listed allowables
     C*** for all surgical procedures include the surgery,
     C*** local infiltration, digital block and/or topical
     C*** anesthesia when used and the normal follow-up care
     C*** for the period indicated in days in the column headed
     C*** "Follow-up Days (FUD)".
     C*
     C     DHPRCD        IFGE      '99211'
     C*********  DHPRCD    ANDLE'99215'                    DEL 01/19/96
     C*********  DHPRCD    ORGE '99231'                    DEL 01/19/96
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99238'                                      ADD 01/19/96
     C     DHPRCD        ANDLE     '99255'                                      ADD 01/19/96
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99271'                                      ADD 01/19/96
     C     DHPRCD        ANDLE     '99275'                                      ADD 01/19/96
     C     DHPRCD        ORGE      '99281'                                      ADD 01/19/96
     C     DHPRCD        ANDLE     '99285'                                      ADD 01/19/96
     C     DHPRCD        ORGE      '99301'                                      ADD 01/19/96
     C     DHPRCD        ANDLE     '99303'                                      ADD 01/19/96
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C*
     C                   Z-ADD     1             X                 7 0
     C     X             DOWLE     500
     C     FUF(X)        ANDGT     *ZERO
     C*
     C*** BEGIN CHG *** 04/01/96 **************************************
     C********** CAFDAT    IFGT FUF,X
     C     CAFDAT        IFGE      FUF(X)
     C*** END   CHG *** 04/01/96 **************************************
     C     CAFDAT        ANDLE     FUT(X)
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '09'          QQC(Q)
     C*** Err Msg 09: "This visit occurred during another procedures
     C***              follow-up period"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   END
     C*
     C*-------------------------------------------------------------------
     C*-- Asterisk Visit with Initial Visit ---------------
     C*   Per pg. S - 5 #18 b.
     C*   When an asterisk(*) procedure is carried out at the time of
     C*   an initial visit (new patient) and this procedure constitutes
     C*   the major service at that visit, and the fee for the proc
     C*   is greater than the fee for the visit, the greater fee plus
     C*   one-half of the applicable visit fee is payable.  If the fee
     C*   asterisk(*) procedure is less than that for the applicable
     C*   visit, then the latter fee plus one-half the former is
     C*   payable.
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   Z-ADD     *ZEROS        COUNT
     C*** END   ADD *** 04/01/96 **************************************
     C* Office Medical Service - New Patient
     C     DHPRCD        IFEQ      '99201'
     C     DHPRCD        OREQ      '99202'
     C     DHPRCD        OREQ      '99203'
     C     DHPRCD        OREQ      '99204'
     C     DHPRCD        OREQ      '99205'
     C* Hospital Medical Service - New Patient
     C     DHPRCD        OREQ      '99221'
     C     DHPRCD        OREQ      '99222'
     C     DHPRCD        OREQ      '99223'
     C* Emergency Dept Service - New Patient
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C*
     C* SETLL and read the file testing for an '*' procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN2        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C* Determine if surgery code is an asterisk procedure (PRSUPP=Y)
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*** GET FIRST ASTERICK AMOUNT, BUT COUNT IF OTHER ASTERICK(S)
     C     COUNT         IFEQ      *ZEROS
     C*** END   ADD *** 04/01/96 **************************************
     C* Get MRA for Asterick Procedure
     C                   Z-ADD     PRMRA         SURMRA                         Surg MRA
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   END
     C                   ADD       1             COUNT
     C                   GOTO      AGAIN2
     C*** END   ADD *** 04/01/96 **************************************
     C                   ELSE                                                   ELS EQ Y
     C                   GOTO      AGAIN2
     C                   END                                                    END EQ Y
     C*
     C*** BEG DEL *** 04/01/96 ****************************************
     C* If surg proc MRA is greater, pay office visit at 50%
     C*********  SURMRA    IFGE XXMRA
     C*********            MULT .50       XXMRA     H
     C*********            END                             END GE XXMRA
     C*** END DEL *** 04/01/96 ****************************************
     C*
     C                   END                                                    END LE 69979
     C*
     C                   END                                                    END EQ 99201
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*
     C     COUNT         IFGT      *ZEROS
     C* If surg proc MRA is greater, pay office visit at 50%
     C     SURMRA        IFGE      XXMRA
     C                   MULT(H)   .50           XXMRA
     C                   END                                                    END GE XXMRA
     C                   END
     C*
     C     COUNT         IFGT      1
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'DL'          QQC(Q)
     C*** Err Msg DL: "There are multiple asterisk procedures on this
     C***              date verify payment'.
     C*
     C                   ENDIF
     C*** END   ADD *** 04/01/96 **************************************
     C*
     C*
     C*-------------------------------------------------------------------
     C*-- Asterisk Visit with Follow-up Visit -------------
     C*   Per pg. S - 5 #18 c.
     C*   When an asterisk(*) procedure is carried out at the time of
     C*   a follow-up visit and this procedure contitutes the major
     C*   service at that visit, then the greater fee, not both fees,
     C*   is payable.
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   Z-ADD     *ZEROS        COUNT
     C*** END   ADD *** 04/01/96 **************************************
     C* Office Medical Service - Established Patient
     C     DHPRCD        IFEQ      '99211'
     C     DHPRCD        OREQ      '99212'
     C     DHPRCD        OREQ      '99213'
     C     DHPRCD        OREQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C* Hospital Medical Service - Subsequent Hospital Care
     C     DHPRCD        OREQ      '99231'
     C     DHPRCD        OREQ      '99232'
     C     DHPRCD        OREQ      '99233'
     C***
     C* SETLL and read the file testing for an '*' procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN3        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C* Determine if surgery code is an asterisk procedure (PRSUPP=Y)
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C* Get MRA for Asterisk Procedure
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*** GET FIRST ASTERICK AMOUNT, BUT COUNT IF OTHER ASTERICK(S)
     C     COUNT         IFEQ      *ZEROS
     C*** END   ADD *** 04/01/96 **************************************
     C                   Z-ADD     PRMRA         SURMRA                         Surg MRA
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   END
     C                   ADD       1             COUNT
     C                   GOTO      AGAIN3
     C*** END   ADD *** 04/01/96 **************************************
     C                   ELSE                                                   ELS EQ Y
     C                   GOTO      AGAIN3
     C                   END                                                    END EQ Y
     C*
     C*** BEG DEL *** 04/01/96 ****************************************
     C* If surg proc MRA is greater, pay office visit at 0%
     C*********  SURMRA    IFGE XXMRA
     C*********            Z-ADD*ZEROS    XXMRA
     C*********            END                             END GE XXMRA
     C*** END DEL *** 04/01/96 ****************************************
     C*
     C                   END                                                    END LE 69979
     C*
     C                   END                                                    END EQ 99211
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C*
     C* If surg proc MRA is greater, pay office visit at 0%
     C     COUNT         IFGT      *ZEROS
     C     SURMRA        IFGE      XXMRA
     C                   Z-ADD     *ZEROS        XXMRA
     C                   END                                                    END GE XXMRA
     C                   END
     C*
     C     XXMRA         IFEQ      *ZEROS
     C                   MOVE      '212'         XPEOMB
     C*** EOMB 212: "This procedure is included in the basic allowance
     C***            of another procedure"
     C                   ENDIF
     C*
     C     COUNT         IFGT      1
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'DL'          QQC(Q)
     C*** Err Msg DL: "There are multiple asterisk procedures on this
     C***              date verify payment'.
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 04/01/96 **************************************
     C*** BEGIN ADD *** 06/04/96 **************************************
     C*-------------------------------------------------------------------
     C*-- Asterisk Visit - Unknown if Initial or Followup Patient
     C*
     C*   Display message for user to manually determine.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C* Office Medical Service
     C     DHPRCD        IFGE      '99217'
     C     DHPRCD        ANDLE     '99220'
     C     DHPRCD        ORGE      '99238'
     C     DHPRCD        ANDLE     '99275'
     C     DHPRCD        ORGE      '99288'
     C     DHPRCD        ANDLE     '99499'
     C***
     C* SETLL and read the file testing for an '*' procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN4        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C* Determine if surgery code is an asterisk procedure (PRSUPP=Y)
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C***  COUNT IF OTHER ASTERISK(S)
     C                   ADD       1             COUNT
     C                   GOTO      AGAIN4
     C                   ELSE                                                   ELS EQ Y
     C                   GOTO      AGAIN4
     C                   END                                                    END EQ Y
     C*
     C                   END                                                    END LE 69979
     C*
     C                   END                                                    END EQ 99217
     C*
     C     COUNT         IFGT      *ZEROS
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'ED'          QQC(Q)
     C*** Err Msg ED: "Asterisk Proc billed w/ E/M code (New/Estab?)
     C***              Apply Rule Manually'.
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 06/04/96 **************************************
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C*----------------------------------------------------------------
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     C*  Pathology Subroutine         Codes 80002 - 89399        P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*----------------------------------------------------------------
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C*----------------------------------------------------------------
     C*--- Procedure Codes with modifiers: 26, 27--------------------------
     C     XPPMOD        IFNE      *BLANK
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
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
     C*
     C                   ELSE                                                   ELS NE 26
     C*
     C*
     C     XPPMOD        IFEQ      '26'
     C*
     C*-- Check for previously paid Complete Procedure ---------------
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
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Load Professional Component amount into MRA field
     C                   Z-ADD     PRPROF        XXMRA
     C                   ELSE                                                   ELS EQ 26
     C*
     C     XPPMOD        IFEQ      '27'
     C*
     C*-- Check for previously paid Complete Procedure ---------------
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
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Load Technical Component amount
     C                   Z-ADD     PR92N1        XXMRA
     C                   END                                                    END EQ 27
     C                   END                                                    END EQ 26
     C*                                              >
     C     XXMRA         IFEQ      *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END EQ 26/27
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       CATOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END NE 26
     C                   END                                                    END NE *ZERO
     C*
     C*-------------------------------------------------------------------
     C*-- Determine Allowed/Disallowed/Paid ------------------------------
     C*                                              >
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C     XPPMOD        IFEQ      '22'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C*--- Procedure Codes with modifiers: 51   ---------------------------
     C     XPPMOD        IFNE      *BLANK
     C     XPPMOD        IFNE      '51'
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
     C                   GOTO      PHYSE
     C                   END                                                    END NE 51
     C                   END                                                    END NE *ZERO
     C*
     C*-------------------------------------------------------------------------
     C*- Additional minutes ---------------------------------------------------
     C     DHPRCD        IFEQ      '97261'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97701'
     C*
     C*   Disallow any additional minutes code that doesn't have the proper
     C*   major code associated with it
     C*
     C     DHPRCD        IFEQ      '97261'
     C                   MOVE      '97260'       XXCPT4
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
     C     DHPRCD        IFEQ      '97541'
     C                   MOVE      '97540'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97701'
     C                   MOVE      '97700'       XXCPT4
     C                   END
     C*
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CATOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '215'         XPEOMB
     C* EOMB Code 215: "No corresponding initial minutes is found
     C*                 for this procedure"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   END                                                    END EQ 97001, ETC.
     C*
     C* -----------------------------------------------------------------------
     C* Per pg. PM - 1 #2
     C*             No more than one adjustment/manipulation per day
     C*             will be paid, EXCEPT on the initial and next two
     C*             subsequent visits.
     C*             Documentation required for exceptions.
     C* Per Janice Wolfe at State: applies to code 97260 only
     C*
     C     DHPRCD        IFEQ      '97260'
     C*
     C* Determine Dates for initial and next two subsequent visit
     C                   MOVE      '97260'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  54
     C*
     C* Reset MNPL array to hold srv dates for manipulations
     C                   Z-ADD     0             MNPL
     C                   Z-ADD     1             FC
     C                   Z-ADD     1             F
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        ANDEQ     '97260'
     C     FC            IFLT      4
     C                   Z-ADD     1             F
     C     XCFDAT        LOOKUP    MNPL                                   60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             F
     C     *ZEROS        LOOKUP    MNPL(F)                                60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     4             FC
     C                   ELSE
     C                   Z-ADD     XCFDAT        MNPL(F)
     C                   ADD       1             FC
     C                   END                                                    END 0 *IN60
     C                   END                                                    END 0 *IN60
     C                   END                                                    END LT 4
     C     CAKYC2        READE     PCA0C                                  54
     C                   END                                                    END DOWEQ
     C*
     C* Read files to determine how many manipulations on this date
     C*          SETLL and READ the file for 97260 codes
     C                   MOVE      '97260'       XXCPT4
     C                   Z-ADD     *ZERO         COUNT
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        ANDEQ     '97260'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C* Check to see if this is one of the 1st three visits.
     C* If not, than only one code/visit/day is allowed.
     C                   Z-ADD     1             F
     C     CAFDAT        LOOKUP    MNPL                                   60
     C     *IN60         IFEQ      '0'
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*             and pay at MRA"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END GE 1
     C                   END                                                    END LOKUP
     C*
     C                   END                                                    END EQ 97260
     C*
     C*** BEGIN ADD *** 01/19/96 **************************************
     C* Per pg. PM - 2 #11
     C*        If examination is necessary, physician may charge for
     C*        and be reimbursed for an office visit at the initial
     C*        visit, after initial corrective treatment, and at the
     C*        final eval.  Licensed physical therapists and lic.
     C*        occupational therapists may use CPT code 97700 to
     C*        charge for an initial evaluation of the patient.
     C*
     C     SVSPCD        IFEQ      'PHT'
     C     SVSPCD        OREQ      'OCT'
     C     SVSPCD        OREQ      'OT'
     C     DHPRCD        IFGE      '97700'
     C*
     C                   MOVE      '97700'       XXCPT4
     C                   Z-ADD     *ZERO         COUNT
     C     CAKYC         SETLL     PCA0C
     C     CAKYC         READE     PCA0C                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        ANDEQ     '97700'
     C     XCDSEQ        ANDNE     CADSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C     CAKYC         READE     PCA0C                                  54
     C                   END                                                    END 0 DOWEQ
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CATOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END GE COUNT
     C*
     C                   END                                                    END EQ 97700, ETC.
     C                   END                                                    END EQ PHT  , ETC.
     C*** END   ADD *** 01/19/96 **************************************
     C*
     C*** BEGIN DEL *** 01/19/96 **************************************
     C* -----------------------------------------------------------------------
     C* Per pg. PM - 1 #3
     C*******
     C*******       Treatment beyond the first 18 visits must be
     C*******       authorized by the insurance carrier, the self-
     C*******       insured employer or the Court, unless previous
     C*******       authorization was for a greater number of visits.
     C*******    DHPRCD    IFGE '97010'
     C*******    DHPRCD    ANDLE'97799'
     C*******    DHPRCD    ORGE '98925'
     C*******    DHPRCD    ANDLE'98929'
     C*******    SETLL and READ the file for 97010-97799 codes
     C*******              MOVE '97010'   XXCPT4
     C*******              Z-ADD*ZERO     COUNT
     C*******              Z-ADD*ZERO     HLFDAT
     C*******              Z-ADD*ZERO     XXDSEQ
     C*******    CAKYC2    CHAINPCA0A                54
     C*******
     C*******    *IN54     DOWEQ'0'
     C*******
     C*******    XACPT4    IFGE '97010'
     C*******    XACPT4    ANDLE'97799'
     C*******    XAFDAT    ANDNEHLFDAT
     C*******    XATOTP    ANDGT*ZEROS
     C*******    XACPT4    ORGE '98925'
     C*******    XACPT4    ANDLE'98929'
     C*******    XAFDAT    ANDNEHLFDAT
     C*******    XATOTP    ANDGT*ZEROS
     C*******              ADD  1         COUNT
     C*******              Z-ADDXAFDAT    HLFDAT
     C**Need*to allow 2nd+ charge on the 18st day
     C*******    COUNT     IFEQ 3
     C*******              Z-ADDXAFDAT    H2FDAT
     C*******              END                             END COUNT EQ 18
     C*******              END                             END NE 97010
     C*******
     C*******    CAKYC2    READEPCA0A                    54
     C*******              END                             END 0 DOWEQ
     C*******
     C*******    COUNT     IFGT 3
     C*******    COUNT     OREQ 3
     C*******    CAFDAT    ANDNEH2FDAT
     C*******              Z-ADDCACHRG    XPTOTD
     C*******              Z-ADD*ZERO     XPTOTA
     C*******              Z-ADD*ZERO     XPTOTP
     C*******              Z-ADD*ZEROS    XPDIFF
     C*******              MOVE '204'     XPEOMB
     C**EOMB*Code 204: "Exceeds number of allowable procedures in
     C*******           Reimbursement Manual"
     C*******    CACHK#    IFEQ *ZERO
     C*******    CAEOB#    ANDEQ*ZERO
     C*******              Z-ADD1         Q       30
     C*******    '00000'   LOKUPQQS,Q                    60
     C**N60                Z-ADD30        Q
     C*******              MOVELCADSEQ    QQS,Q
     C*******              MOVE '27'      QQC,Q
     C**Err*Msg 27: "If the necessary documentation exists, override
     C*******       and pay at MRA"
     C*******              END
     C*******              EXSR UPDAT
     C*******              GOTO PHYSE
     C*******              END
     C*******              END
     C*******
     C*** END   DEL 01/19/96 ******************************************
     C*** BEGIN ADD 01/19/96 ******************************************
     C* Per pg. PM - 1 #3
     C*             Treatment beyond the first 18 visits must be
     C*             authorized by the insurance carrier, the self-
     C*             insured employer or the Court, unless previous
     C*             authorization was for a greater number of visits.
     C*** Per Janice Wolfe 01/22/96 - Gray area concerning overlap
     C*   of visits between new and old fee schedules.
     C*   Chg to use (counter of PT) to determine whether to use
     C*   18 or 21 as limit. If PT service < 960101, add to counter.
     C*   If counter greater than zero, allow 21 visits.
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C     DHPRCD        ORGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C*          SETLL and READ the file for 97010-97799 codes
     C                   MOVE      '97010'       XXCPT4
     C                   Z-ADD     *ZERO         XXVIST            6 0
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         HLFDAT
     C                   CLEAR                   VIST
     C     CAKYC2        CHAIN     PCA0A                              54
     C*** BEG DEL *** 04/01/96 ****************************************
     C**N54                Z-ADDXAFDAT    HLFDAT
     C*** END DEL *** 04/01/96 ****************************************
     C*
     C     *IN54         DOWEQ     '0'
     C* SKIP CURRENT RECORD
     C     XACPT4        IFEQ      DHPRCD
     C     XADSEQ        ANDEQ     CADSEQ
     C*
     C     XAFDAT        IFLT      960101
     C                   ADD       1             COUNT
     C                   END
     C*
     C                   GOTO      NXTPHT
     C                   END
     C*
     C     XACPT4        IFGE      '97010'
     C     XACPT4        ANDLE     '97799'
     C     XATOTP        ANDGT     *ZEROS
     C     XACPT4        ORGE      '98925'
     C     XACPT4        ANDLE     '98929'
     C     XATOTP        ANDGT     *ZEROS
     C*** BEG ADD *** 04/01/96 ****************************************
     C     HLFDAT        IFEQ      *ZEROS
     C                   Z-ADD     XAFDAT        HLFDAT
     C                   ENDIF
     C*** END ADD *** 04/01/96 ****************************************
     C* ACCUMULATE NUMBER OF VISITS
     C     XAFDAT        IFNE      HLFDAT
     C                   Z-ADD     1             X
     C     HLFDAT        LOOKUP    VIST(X)                                61
     C     *IN61         IFEQ      '0'
     C     *ZEROS        LOOKUP    VIST(X)                                61
     C  N61              Z-ADD     40            X
     C                   Z-ADD     HLFDAT        VIST(X)
     C                   ADD       1             XXVIST
     C                   END                                                    END *IN61
     C*
     C                   Z-ADD     XAFDAT        HLFDAT
     C                   END
     C                   END
     C*
     C     XAFDAT        IFLT      960101
     C                   ADD       1             COUNT
     C                   END
     C*
     C     NXTPHT        TAG
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C*** ACCUMULATE LAST RECORD PROCESSED
     C                   Z-ADD     1             X
     C     HLFDAT        LOOKUP    VIST(X)                                61
     C     *IN61         IFEQ      '0'
     C     *ZEROS        LOOKUP    VIST(X)                                61
     C  N61              Z-ADD     40            X
     C                   Z-ADD     HLFDAT        VIST(X)
     C                   ADD       1             XXVIST
     C                   END                                                    END *IN61
     C* ALLOW 18 VISITS IF ALL PT SERVICES ARE GE 960101 FS DATE
     C     CAFDAT        LOOKUP    VIST                                   60
     C     *IN60         IFEQ      '0'
     C     COUNT         IFEQ      *ZEROS
     C     XXVIST        IFGE      18
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*             and pay at MRA"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C                   END
     C                   END
     C*
     C* ALLOW 21 VISITS IF ANY PT SERVICES ARE PRIOR TO 960101
     C     *IN60         IFEQ      '0'
     C     COUNT         IFNE      *ZEROS
     C     XXVIST        IFGE      21
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*             and pay at MRA"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C                   END
     C                   END
     C*
     C                   END                                                    ADD 01/19/96
     C*** END   ADD 01/19/96 ******************************************
     C*** BEGIN ADD 01/19/96 ******************************************
     C* Per pg. PM - 2 #10
     C*       Not more than (4) visits should be compensated
     C*       if treatment consists of only modalities. This
     C*       includes CPT codes 97010 through 97039.
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97039'
     C                   Z-ADD     *ZERO         MODCNT
     C                   Z-ADD     *ZERO         XXOTHR            6 0
     C                   Z-ADD     *ZERO         XXVIST            6 0
     C                   Z-ADD     *ZERO         HLFDAT
     C                   Z-ADD     *ZERO         H2FDAT
     C                   CLEAR                   VIST
     C     CAKYC2        CHAIN     PCA0A                              54
     C  N54              Z-ADD     XAFDAT        HLFDAT
     C*
     C     *IN54         DOWEQ     '0'
     C* SKIP CURRENT RECORD
     C     XACPT4        IFEQ      DHPRCD
     C     XADSEQ        ANDEQ     CADSEQ
     C                   GOTO      NXTPHS
     C                   END
     C* ACCUMULATE NUMBER OF VISITS IF ONLY MODALITIES PERFORMED
     C     XAFDAT        IFNE      HLFDAT
     C     XXOTHR        IFEQ      *ZEROS
     C     MODCNT        ANDGT     *ZEROS
     C                   Z-ADD     1             X
     C     HLFDAT        LOOKUP    VIST(X)                                61
     C     *IN61         IFEQ      '0'
     C     *ZEROS        LOOKUP    VIST(X)                                61
     C  N61              Z-ADD     40            X
     C                   Z-ADD     HLFDAT        VIST(X)
     C                   ADD       1             XXVIST
     C                   END                                                    END *IN61
     C*
     C                   END
     C*** DEL 04/01/96 *** Z-ADDXAFDAT    HLFDAT
     C                   Z-ADD     *ZEROS        XXOTHR
     C                   Z-ADD     *ZEROS        MODCNT
     C                   END
     C*
     C     XACPT4        IFGE      '97010'
     C     XACPT4        ANDLE     '97039'
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             MODCNT
     C                   END                                                    END NE 97010
     C*
     C*********  XATOTP    IFGT *ZEROS
     C     XACPT4        IFLT      '97010'
     C     XACPT4        ORGT      '97039'
     C                   ADD       1             XXOTHR
     C                   END                                                    END NE 97010
     C*********            END                             END XATOTP
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C* RESTART COUNTER IF NON-MODALITY PERFORMED
     C     XAFDAT        IFNE      HLFDAT
     C     XXOTHR        IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        XXVIST
     C                   END
     C                   END
     C                   Z-ADD     XAFDAT        HLFDAT
     C*** END   ADD *** 04/01/96 **************************************
     C     NXTPHS        TAG
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C*** ACCUMULATE LAST RECORD PROCESSED
     C     XXOTHR        IFEQ      *ZEROS
     C     MODCNT        ANDGT     *ZEROS
     C                   Z-ADD     1             X
     C     HLFDAT        LOOKUP    VIST(X)                                61
     C     *IN61         IFEQ      '0'
     C     *ZEROS        LOOKUP    VIST(X)                                61
     C  N61              Z-ADD     40            X
     C                   Z-ADD     HLFDAT        VIST(X)
     C                   ADD       1             XXVIST
     C                   END                                                    END *IN61
     C                   END                                                    END XXOTHR
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C* RESTART COUNTER IF NON-MODALITY PERFORMED ON LAST REC READ
     C     XXOTHR        IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        XXVIST
     C                   END
     C*** END   ADD *** 04/01/96 **************************************
     C     CAFDAT        LOOKUP    VIST                                   60
     C     XXVIST        IFGE      4
     C     *IN60         ANDEQ     '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*             and pay at MRA"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C                   END
     C*** END   ADD 01/19/96 ******************************************
     C* Accumulate modalities and procedures, test, etc..                   es)
     C* Create a table of body parts.
     C*          SETLL and READ the file for 97010 series
     C*
     C                   MOVE      '97010'       XXCPT4
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         MODCNT            7 0
     C                   Z-ADD     *ZERO         PROCNT            7 0
     C                   CLEAR                   PART
     C                   CLEAR                   PCNT
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*** BEG ADD *** 06/04/96 ****************************************
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97799'
     C     XRCPT4        ORGE      '98925'
     C     XRCPT4        ANDLE     '98929'
     C*** END ADD *** 06/04/96 ****************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        ANDGT     *ZEROS
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97039'
     C                   ADD       1             MODCNT
     C                   ELSE
     C                   ADD       1             PROCNT
     C                   END                                                    END GE 97010
     C                   ADD       1             COUNT
     C*
     C*** BEGIN ADD *** 04/01/96 **************************************
     C     XRPART        IFEQ      *BLANKS
     C                   MOVE      '??'          XXPART
     C                   ELSE
     C                   MOVE      XRPART        XXPART
     C                   ENDIF
     C*** END   ADD *** 04/01/96 **************************************
     C                   Z-ADD     1             X
     C*** BEGIN CHG *** 04/01/96 **************************************
     C********** XRPART    LOKUPPART,X                   61
     C     XXPART        LOOKUP    PART(X)                                61
     C*** END   CHG *** 04/01/96 **************************************
     C     *IN61         IFEQ      '0'
     C     *BLANKS       LOOKUP    PART(X)                                61
     C  N61              Z-ADD     20            X
     C*** BEGIN CHG *** 04/01/96 **************************************
     C*****************    MOVE XRPART    PART,X
     C                   MOVE      XXPART        PART(X)
     C*** END   CHG *** 04/01/96 **************************************
     C                   Z-ADD     1             PCNT(X)
     C                   ELSE
     C                   ADD       1             PCNT(X)
     C                   END                                                    END *IN61
     C                   END                                                    END XRTOTP GT 0
     C*** BEG ADD *** 06/04/96 ****************************************
     C                   ENDIF                                                  END XRCPT4
     C*** END ADD *** 06/04/96 ****************************************
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C* -----------------------------------------------------------------------
     C*** BEGIN DEL 01/19/96 ******************************************
     C* Per pg. PM - 2 #10
     C*********     Not more than (4) visits should be compensated
     C*********     if treatment consists of only modalities. This
     C*********     includes CPT codes 97010 through 97039.
     C*********
     C*********  DHPRCD    IFGE '97010'
     C*********  DHPRCD    ANDLE'97039'
     C*********  PROCNT    IFEQ *ZEROS
     C*********  MODCNT    ANDGE4
     C*** DISALLOW CHARGES
     C*********
     C*********            Z-ADD*ZERO     XPTOTA
     C*********            Z-ADD*ZERO     XPTOTP
     C*********  CACHRG    SUB  XPTOTA    XPTOTD
     C*********            Z-ADDCACHRG    XPTOTC
     C*********            Z-ADD*ZEROS    XPDIFF
     C*********            MOVE '204'     XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C*********
     C*********            EXSR UPDAT
     C*********            GOTO PHYSE
     C*********
     C*********            END                             END PROCNT
     C*********            END                             END DHPRCD 97010
     C*********
     C*** END   DEL 01/19/96 ******************************************
     C* Per pg. PM - 1 #9
     C*     Any combination of four (4) modalities, procedures,
     C*     physical medicine activities, training codes, and
     C*     additional time codes is allowable per day.  An injury
     C*     resulting in treatment to more than one body area
     C*     must be substantiated by the appropriate diagnosis
     C*     codes.  An addition of two (2) physical medicine
     C*     codes per day will be allowed for treatment of more
     C*     than one body area.
     C*
     C     PCNT(2)       IFGT      *ZEROS
     C     COUNT         IFGE      6
     C*** DISALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END COUNT
     C                   END                                                    END PCNT,2
     C*
     C**  Only allow 4 per body area
     C*** BEGIN ADD *** 04/01/96 **************************************
     C     DHAREA        IFEQ      *BLANKS
     C                   MOVE      '??'          XXPART
     C                   ELSE
     C                   MOVE      DHAREA        XXPART
     C                   ENDIF
     C*** END   ADD *** 04/01/96 **************************************
     C                   Z-ADD     1             X
     C*** BEGIN CHG *** 04/01/96 **************************************
     C********** DHAREA    LOKUPPART,X                   61
     C     XXPART        LOOKUP    PART(X)                                61
     C*** END   CHG *** 04/01/96 **************************************
     C     *IN61         IFEQ      '1'
     C     PCNT(X)       IFGE      4
     C*** DISALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END PCNT,X
     C                   END                                                    END *IN61
     C*
     C* -----------------------------------------------------------------------
     C* Per pg. PM - 1 #8
     C*             When a physical therapist renders treatment in a
     C*             patient's home, add 50% to the listed unit value.
     C*             Justification report required
     C*
     C*   Chain to Provider file to see if specialty code = PHT
     C*
     C     SVSPCD        IFEQ      'PHT'
     C     DHPOS         ANDEQ     '4'
     C     XXMRA         MULT(H)   1.50          XXMRA
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required
     C*             for reimbursement"
     C                   END                                                    END EQ PHT
     C* -----------------------------------------------------------------------
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C*
     C     PHYSE         ENDSR
     C********************************************************************
     C*  PMOD - Procedure Code Modifier Subroutine               P M O D *
     C********************************************************************
     C*  CALCULATE MRA BASED ON MODIFIER ENTERED
     C*       22 - "Unusual Services"
     C*            Substantiate by report
     C*       26 - "Professional Component"
     C*            Pay per PC Unit Value
     C*       27 - "Technical Component"
     C*            Pay per Total value less the PC value
     C*       47 - "Anesthesia by Surgeon"
     C*       50 - "Bilateral Procedure"
     C*            Payment is for the procedure with the highest
     C*            allowance, plus half of the lesser procedure(s)
     C*            up to a total maximum allowance of twice the
     C*            highest fee
     C*       51 - "Multiple Procedures"
     C*       52 - "Reduced Services"
     C*       54 - "Surgical Care Only"
     C*       55 - "Postoperative Managment Only"
     C*       62 - "Two Surgeons"
     C*       66 - "Surgical Team"
     C*       75 - "Concurrent Care, Services rendered by more than
     C*             one Physician"
     C*       76 - "Repeat Procedure by Same Physician"
     C*       77 - "Repeat Procedure by Another Physician"
     C*       80 - "Assistant Surgeon/Physician's Assistant"
     C*       81 - "Minimum Assistance to Surgeon"
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
     C* -----------------------------------------------------------------------
     C* - 22/Unusual Services Modifier ------------------------------
     C     XPPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C* Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C*              amount to be paid."
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - 26/Professional Component ---------------------------------
     C* Per R - 1 #3, P - 1 # 4
     C*      The allowable for the professional component is entered
     C*      with modifier -26 in Fee Sched Reimbursement manual.
     C*
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     SXPROF        XXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C* - 27/Technical Component ------------------------------------
     C* Per R - 1 #3, P - 1 # 4
     C*      The allowable for the technical component is entered
     C*      with modifier -27 in Fee Sched Reimbursement manual.
     C* Per M - 1 #7
     C*      The allowable for the technical component is the
     C*      difference between the procedure value shown without
     C*      a modifier and the value shown for modifier -26.
     C     XPPMOD        IFEQ      '27'
     C     SX92N1        IFNE      *ZEROS
     C                   Z-ADD     SX92N1        XXMRA
     C                   GOTO      PMODE
     C                   ELSE
     C     SXPROF        IFNE      *ZEROS
     C     SXMRA         SUB       SXPROF        XXMRA
     C                   GOTO      PMODE
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END SXPROF
     C                   END                                                    END SX92N1
     C                   END                                                    END XPPMOD
     C*
     C     XPPMOD        IFEQ      '52'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced Services Modifier. Adjuster should verify
     C***              amount to be paid"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PMODE
     C*
     C                   END                                                    END EQ 52
     C*
     C* -----------------------------------------------------------------------
     C*** BEGIN CHG *** 04/01/96 **************************************
     C*
     C* Per Pg. S - 3 #12 b.: Identify surgery performed by code number
     C*            and description of services, bill at 20% of fee
     C* - 80/Assisting Surgeon --------------------------------------
     C     XPPMOD        IFEQ      '80'
     C                   Z-ADD     .20           XXPERC
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END EQ '80'
     C*
     C*********  SXSURG    IFEQ 'Y'
     C*
     C* Per pg. S - 4 #17: Physician's Assistant ...
     C*                    bill at 10% of fee
     C* - 81/Minimum Assistant Surgeon ------------------------------
     C     XPPMOD        IFEQ      '81'
     C                   Z-ADD     .10           XXPERC
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END EQ '81'
     C*
     C********** SVSPCD    IFEQ 'PA'
     C**********           Z-ADD.10       XXPERC
     C**********           END
     C**********
     C*            (20% loaded into FTPMPOK)
     C********** XXMRA     MULT XXPERC    XXMRA     H
     C**********           ELSE
     C**********           Z-ADD*ZERO     XXMRA
     C********** CAEOMB    IFEQ *BLANK
     C**********           MOVE '203'     CAEOMB
     C**********           MOVE '203'     XPEOMB
     C**EOMB*Code 203:Service is not reimburseable under the WC Program
     C**********           END
     C**********
     C**********           END
     C**********
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '71'      QQC,Q
     C**Err*Msg*71: "Item uses Assistant Surgeon Mod-80. Adjuster
     C**********     should verify usage."
     C**********
     C**********           GOTO PMODE
     C**********           END
     C*** END   CHG *** 04/01/96 **************************************
     C*
     C*
     C* -----------------------------------------------------------------------
     C* - Any other modifier falls thru to here: --------------------
     C*
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   GOTO      PMODE
     C*
     C*
     C     PMODE         ENDSR
     *********************************************************************
     C*  RADIOLOGY SUBROUTINE  CODES 70010 - 79999               R A D I *
     C*
     C*     Subsections
     C*        76500-76999     Diagnostic Ultrasound
     C*        77261-77999     Therapeutic Radiology
     C*        78000-79999     Nuclear Medicine
     *********************************************************************
     C     RADI          BEGSR
     C*----------------------------------------------------------------
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C* ----------------------------------------------------------------
     C* --- Valid Modifiers --------------------------------------------
     C*  Per pg. R - 1, 2: Modifiers - (22, 26, 27, 51, 52)
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELS NE 26/27
     C*
     C     XPPMOD        IFNE      '51'
     C                   EXSR      PMOD
     C                   END
     C*
     C     XPPMOD        IFEQ      '26'
     C     XXMRA         ANDEQ     *ZEROS
     C     XPPMOD        OREQ      '27'
     C     XXMRA         ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END EQ 26/27
     C                   END                                                    END NE 26/27
     C*
     C                   END                                                    ENDNE *BLANKS
     C*
     C*  Per pg. R - 2: Multiple services other than diagnostic
     C*  radiology:  When multiple or bilateral procedures or
     C*  services are provided at the same session, the highest valued
     C*  procedure will be reported as listed.  Reimbursement will
     C*  be made for the highest valued procedure and at 50% of the
     C*  lesser procedures(s) up to a total max allowance of twice
     C*  the highest valued procedure.
     C*
     C*** BEGIN CHG *** 05/27/98 **************************************
     C********   DHPRCD    IFGE '76500'
     C********   DHPRCD    ANDLE'79999'
     C     DHPRCD        IFGE      '77261'
     C     DHPRCD        ANDLE     '77799'
     C     DHPRCD        ORGE      '79000'
     C     DHPRCD        ANDLE     '79999'
     C*** END CHG *** 05/27/98 ****************************************
     C     XPPMOD        IFEQ      *BLANK
     C     XPPMOD        OREQ      '51'
     C                   MOVE      'R'           CATYPE
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     XXMRA         CAMRA
     C                   WRITE     PCAPT1
     C                   GOTO      RADIE
     C                   END
     C                   END
     C*
     C* ----------------------------------------------------------------
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C*
     C     RADIE         ENDSR
     C********************************************************************
     C** Process RADI2 Codes 76500-79999            R A D I 2          2 *
     C********************************************************************
     C     RADI2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the RADI Detail Charge Workfile --------------------
     C*
     C                   Z-ADD     0             COUNT
     C*** BEGIN ADD *** 05/21/96 **************************************
     C                   Z-ADD     0             XXTOT             9 2
     C                   Z-ADD     0             XXWRK             9 2
     C                   Z-ADD     0             XXDIF             9 2
     C*** END   ADD *** 05/21/96 **************************************
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or 50%
     C     *LOVAL        SETLL     PCAPT5
     C                   READ      PCAPT5                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      P5CPT4        XXCPT4
     C     PRKEY         SETGT     PPR01
     C     NXTPR5        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR5
     C                   ELSE
     C                   Z-ADD     0             PRMRA
     C                   END
     C*
     C                   ADD       1             COUNT
     C                   Z-ADD     PRMRA         ZZMRA                          Chrg's MRA
     C*
     C*  GET TWICE ALLOWED FOR THE HIGHEST MRA IN WORK FILE
     C*
     C     COUNT         IFEQ      1
     C     ZZMRA         MULT      2             Z2MRA                          2X Chrg MRA
     C                   END
     C*  GET 50% VALUE FOR ALL OTHERS RECORDS IN WORK FILE
     C     COUNT         IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END  GE 2
     C*
     C*** BEGIN ADD *** 05/21/96 **************************************
     C*
     C     XXTOT         IFEQ      Z2MRA
     C* REACHED MAX
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     P5TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C                   ELSE                                                   ELSE XXTOT
     C*
     C     ZZMRA         ADD       XXTOT         XXWRK
     C     XXWRK         IFLE      Z2MRA
     C     P5TOTC        IFLE      ZZMRA
     C* PAY P5TOTC
     C                   Z-ADD     P5TOTC        XPTOTA
     C     ZZMRA         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   ADD       P5TOTC        XXTOT
     C                   ELSE
     C* PAY ZZMRA
     C                   Z-ADD     ZZMRA         XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P5TOTC        SUB       XPTOTA        XPTOTD
     C                   ADD       ZZMRA         XXTOT
     C                   ENDIF
     C                   ELSE                                                   ELSE XXWRK
     C*
     C* COMPUTE DIFFERENCE TO PAY
     C     Z2MRA         SUB       XXTOT         XXDIF
     C     P5TOTC        IFLE      XXDIF
     C* PAY P5TOTC
     C                   Z-ADD     P5TOTC        XPTOTA
     C     XXDIF         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   ADD       P5TOTC        XXTOT
     C                   ELSE
     C* PAY XXDIF
     C                   Z-ADD     XXDIF         XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P5TOTC        SUB       XPTOTA        XPTOTD
     C                   ADD       XXDIF         XXTOT
     C                   ENDIF                                                  END P5TOTC
     C                   ENDIF                                                  END XXWRK
     C                   ENDIF                                                  END XXTOT
     C*** END   ADD *** 05/21/96 **************************************
     C*** BEGIN DEL *** 05/21/96 **************************************
     C*********  Z2MRA     SUB  ZZMRA     LHIGH
     C*********            Z-ADDLHIGH     Z2MRA
     C*********  LHIGH     IFLT *ZERO
     C*********            Z-ADD*ZERO     XPTOTA
     C*********            Z-ADD*ZERO     XPTOTP
     C*********  P5TOTC    SUB  XPTOTA    XPTOTD
     C*********            Z-ADD*ZEROS    XPDIFF
     C*********            MOVE '204'     XPEOMB
     C****EOMB*204: "Exceeds number of allowable procedures in reimburse manual"
     C*********            ELSE                            ELSE LHIGH  LE *ZERO
     C*********  P5TOTC    IFGT LHIGH
     C*********  LHIGH     COMP ZZMRA                54    >
     C***54****            Z-ADDZZMRA     XPTOTA
     C***54****            Z-ADD*ZEROS    XPDIFF
     C*********  *IN54     IFEQ '0'
     C*********  LHIGH     ANDNE*ZEROS
     C*********            Z-ADDLHIGH     XPTOTA
     C*********            ELSE
     C*********            Z-ADDZZMRA     XPTOTA
     C*********            END
     C**N54****  ZZMRA     SUB  XPTOTA    XPDIFF
     C*********            Z-ADDXPTOTA    XPTOTP
     C*********  P5TOTC    SUB  XPTOTA    XPTOTD
     C*********            MOVE '002'     XPEOMB
     C****EOMB*002: "Charges exceed Maximum Allowance found in reimburse manual"
     C*********            ELSE                            ELSE CACHRG GT L97503
     C*********  P5TOTC    COMP ZZMRA                54    >
     C***54****            Z-ADDZZMRA     XPTOTA
     C***54****            Z-ADD*ZEROS    XPDIFF
     C**N54****            Z-ADDP5TOTC    XPTOTA
     C**N54****  ZZMRA     SUB  XPTOTA    XPDIFF
     C*********            Z-ADDXPTOTA    XPTOTP
     C*********  P5TOTC    SUB  XPTOTA    XPTOTD
     C*********            END                             END CACHRG GT L97503
     C*********            END                             END LHIGH  LE *ZERO
     C*** END   DEL *** 05/21/96 **************************************
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   MOVEL     P5PMOD        XPPMOD
     C*** END   ADD *** 04/01/96 **************************************
     C                   EXSR      UPDAT5
     C*
     C                   READ      PCAPT5                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C*
     C     RADI2E        ENDSR
     *********************************************************************
     C*  SETFL2 SUBROUTINE                                 S E T F L 2 I *
     C*
     C*  Sets up the workfiles for SURG2 and RADI2.
     *********************************************************************
     C     SETFL2        BEGSR
     C*
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL1OK
     C                   CLOSE     FACAL3OK
     C                   CLOSE     FACAPOK
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL1OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C*     to FACAL3OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAPOK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACOK'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL1OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C*     FACAL3OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*
     C*     FACAPOK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL1OK
     C                   OPEN      FACAL3OK
     C                   OPEN      FACAPOK
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C*** BEGIN DEL *** 04/01/96 **************************************
     C*     then, reset Phys Medicine arrays
     C*************        MOVE *BLANKS   PTSC
     C*************        MOVE *ZEROS    PTSM
     C*************        MOVE *BLANKS   PTSS
     C*** END   DEL *** 04/01/96 **************************************
     C*
     C                   ELSE                                                   ELS XFIRST EQ ' '
     C*
     C* If this is a new date or new provider/modifier
     C     XXYMD         IFNE      HDYMD
     C     CAFEIN        ORNE      HDFEIN
     C     CAMOD         ORNE      HDMOD
     C*     then, Process all Phys Medicine Codes for the existing date/provider
     C                   EXSR      SURG2
     C                   EXSR      RADI2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL1OK
     C                   CLOSE     FACAL3OK
     C                   CLOSE     FACAPOK
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL1OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C*     to FACAL3OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAPOK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACOK'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL1OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL3OK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*     FACAPOK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL1OK
     C                   OPEN      FACAL3OK
     C                   OPEN      FACAPOK
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C*** BEGIN DEL *** 04/01/96 **************************************
     C*     then, reset Phys Medicine arrays
     C*************        MOVE *BLANKS   PTSC
     C*************        MOVE *ZEROS    PTSM
     C*************        MOVE *BLANKS   PTSS
     C*** END   DEL *** 04/01/96 **************************************
     C                   END
     C                   END                                                    END XFIRST EQ ' '
     C     SETFLN        ENDSR
     *********************************************************************
     C*  SURGERY SUBROUTINE    CODES 10000 - 69979               S U R G *
     C*      Subsections
     C*         20000-29799      Musculoskeletal
     C*         36000-36822      Vascular Radiology Injection Procedures
     C*         52000-53675      Endoscopy-Cystoscopy, Urethroscopy, Cystourethr
     C*         59000-59899      Maternity Care and Delivery
     C*         67311-68850      Ocular Adnexa-Extraocular Muscles
     *********************************************************************
     C     SURG          BEGSR
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C* DETERMINE ANES RATE FOR SURGEON
     C     XPPMOD        IFEQ      '47'
     C     SXANAM        IFNE      *ZEROS
     C     SXANAM        MULT      XXANES        XXMRA
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ELSE                                                   ELSE SXANAM = 0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AN'          QQC(Q)
     C*** Err Msg AN: "An anesthesia reimbursement value is not available
     C*                for this code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '217'         XPEOMB
     C* EOMB Code 217: "An anesthesia reimbursement value is not available
     C*                 for this code."
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END SXANAM  S
     C                   END                                                    END XPPMOD
     C*
     C     DHAREA        IFNE      'AN'
     C*----------------------------------------------------------------
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C                   ELSE                                                   ELSE EQ 'AN'
     C*
     C** COMPUTE ANESTHESIA RATE
     C     SVSPCD        IFEQ      'ANE'
     C     SVSPCD        OREQ      'RNA'
     C*
     C     SXANAM        IFNE      *ZEROS
     C*   (Time Units * Conversion Factor) + Basic MRA Value           -------
     C*
     C     DHUNTS        ADD       SXANAM        XXMRA
     C     XXMRA         MULT      XXANES        XXMRA
     C*
     C*
     C* Per pg. A - 1, #1:                                             -------
     C*  When anesthesia is administered by a CRNA, bill at 90% of     -------
     C*  calculated fee                                                -------
     C     SVSPCD        IFEQ      'RNA'
     C                   MULT      .90           XXMRA
     C                   END                                                    END EQ RNA
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              MOVE      *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ELSE                                                   ELSE SXANAM = 0
     C*
     C     SX1A2         IFNE      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AN'          QQC(Q)
     C*** Err Msg AN: "An anesthesia reimbursement value is not available
     C*                for this code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '217'         XPEOMB
     C* EOMB Code 217: "An anesthesia reimbursement value is not available
     C*                 for this code."
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ELSE                                                   ELSE SX1A2
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AO'          QQC(Q)
     C*** Err Msg AO: "This charge is an individual consideration."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *ZEROS        XPDIFF
     C                   MOVE      '218'         XPEOMB
     C* EOMB Code 218: "This charge is an individual consideration."
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END SX1A2
     C*
     C                   END                                                    END SXANAM
     C                   ELSE                                                   ELSE EQ 'ANE'
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C                   END                                                    END EQ 'ANE'
     C*
     C                   END                                                    END EQ 'AN'
     C*
     C* ----------------------------------------------------------------
     C*  Per pg. Appendix A: Modifiers - 22, 47, 50, 51, 52, 54,
     C*                                  55, 56, 57, 58,
     C*                                  62, 66, 76, 77, 78,
     C*                                  79, 80, 81, 82
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFNE      '22'
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
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '81'
     C     XPPMOD        ANDNE     '82'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    ELS NE 22...
     C                   END                                                    END NE *BLKS
     C* ------------------------------------------------------------
     C*   Per pg. S - 1
     C*   Handle modifider 50 & 51 to cut charges by 50%.
     C     XPPMOD        IFEQ      '50'
     C     XPPMOD        OREQ      '51'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     XXMRA         DIV(H)    2.00          XPTOTA
     C*
     C     CACHRG        IFLT      XPTOTA
     C     XPTOTA        SUB       CACHRG        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C* ------------------------------------------------------------
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C* ------------------------------------------------------------
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C     XPPMOD        IFEQ      '56'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BR'          QQC(Q)
     C* Err Msg BR: "Item is Pre-Oper Mgmt ONLY mod-56,
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C     XPPMOD        IFEQ      '57'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CH'          QQC(Q)
     C* Err Msg CH: "Item is Decision for Surgery mod-57,
     C*             Refer to fee sched for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C     XPPMOD        IFEQ      '58'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CN'          QQC(Q)
     C* Err Msg CN: "Staged procedure by the same phys Mod-58,
     C*             Refer to fee sched for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C* ------------------------------------------------------------
     C*   Per pg.      Under certain circumstances the skills of
     C*   Appendix A:  two surgeons (usually with different skills)
     C*                may be required in management of a specific
     C*                surgical problem...Total value may be increased
     C*                by 25% in lieu of assistant's charge
     C     XPPMOD        IFEQ      '62'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '65'          QQC(Q)
     C* Err Msg 65: "The item indicates Two Surgeons mod-62. Refer to
     C*             fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C* ------------------------------------------------------------
     C     XPPMOD        IFEQ      '66'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '66'          QQC(Q)
     C* Err Msg 66: "The item indicates Surgical Team mod-66
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C* ------------------------------------------------------------
     C     XPPMOD        IFEQ      '76'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '68'          QQC(Q)
     C* Err Msg 68: "The item indicates Proc by Same Phys  mod-76
     C*             Explanatory note required for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C* ------------------------------------------------------------
     C     XPPMOD        IFEQ      '77'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '69'          QQC(Q)
     C* Err Msg 69: "The item indicates Proc by another Phys  mod-77
     C*             Explanatory note required for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C     XPPMOD        IFEQ      '78'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CI'          QQC(Q)
     C* Err Msg CI: "Return to Oper Room for a related proc., mod-78,
     C*             Refer to fee sched for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C     XPPMOD        IFEQ      '79'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CJ'          QQC(Q)
     C* Err Msg CJ: "Unrelated proc by same Phys, mod-79,
     C*             Refer to fee sched for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*** BEGIN DEL *** 04/01/96 **************************************
     C********   XPPMOD    IFEQ '81'
     C********   CACHK#    IFEQ *ZERO
     C********   CAEOB#    ANDEQ*ZERO
     C********             Z-ADD1         Q
     C********   '00000'   LOKUPQQS,Q                    60
     C**N60***             Z-ADD30        Q
     C********             MOVELCADSEQ    QQS,Q
     C********             MOVED'CK'      QQC,Q
     C**Err*Msg CK: "Minimum-Assistant Surgeons, mod-81,
     C********      Refer to-fee sched for reimbursement"
     C********             END            Q
     C********             Z-ADDCACHRG    XPTOTD
     C********             Z-ADD*ZERO     XPTOTA
     C********             Z-ADD*ZERO     XPTOTP
     C********             Z-ADD*ZEROS    XPDIFF
     C********             EXSR UPDAT
     C********             GOTO SURGE
     C********             END
     C*** END   DEL *** 04/01/96 **************************************
     C*
     C*
     C     XPPMOD        IFEQ      '82'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CL'          QQC(Q)
     C* Err Msg CL: "Assistant Surgeon (when resident not avail)Mod-82,
     C*             Refer to fee sched for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C* ------------------------------------------------------------
     C*   Per pg. S - 4 #17
     C*       A Physician's Assistant with current certification
     C*       assisting in surgery identify surgery performed by
     C*       code number and description of procedure, bill at 10%
     C*       of the code fee.  The code(s) must coincide with
     C*       those of the surgeon.
     C*
     C     SVSPCD        IFEQ      'PA'
     C*** BEGIN ADD *** 04/01/96 **************************************
     C     XPPMOD        IFNE      '80'                                         ALREADY REDUCED
     C     XPPMOD        ANDNE     '81'
     C*** END   ADD *** 04/01/96 **************************************
     C     XXMRA         MULT      .10           XXMRA
     C* Chain to detail charge file, if you do NOT find a charge that
     C* was paid, then give an error message
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '1'
     C     X6TOTP        OREQ      *ZEROS
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '58'          QQC(Q)
     C*** Err Msg 58: "The surgeon's charge for this procedure has not
     C***              been paid - Please review"
     C                   END                                                    END 1 *IN60
     C*** BEGIN ADD 04/01/96 ******************************************
     C                   END                                                    END XPPMOD
     C*** END   ADD 04/01/96 ******************************************
     C                   END                                                    END EQ PA
     C*
     C* ------------------------------------------------------------
     C*   Per pg. S - 5 # 22
     C*   FOR DIAGNOSED RADICULOPATHY with or without low back pain
     C*   supported by appropriate diagnostic tests such as MRI,
     C*   CT scan and/or Myelogram, the number of epidural steroid
     C*   injections shall be limited to 3 per year.
     C*   Per Janice Wolfe: don't have CPT-4 codes for cervical
     C*   epidural steriod injections.  (The 2 per yr limit).
     C*   01/19/96 - Added CPT-4 code 62289 to list, but only an
     C*   error message is displayed for either the 2 per year limit
     C*   or the 3 per year limit.
     C*
     C     DHPRCD        IFEQ      '62275'
     C     DHPRCD        OREQ      '62278'
     C     DHPRCD        OREQ      '62279'
     C     DHPRCD        OREQ      '62281'
     C     DHPRCD        OREQ      '62282'
     C     DHPRCD        OREQ      '62289'                                      ADD 01/19/96
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CM'          QQC(Q)
     C* Err Msg CM: "Number of epidural steriod injections is 3 per yr /
     C*             (cervical 2 per year)"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C*
     C*-------------------------------------------------------------------
     C*-- Asterisk Visit with Initial Visit ---------------
     C*   Per pg. S - 4, S - 5 #18
     C*   When an asterisk(*) procedure is carried out at the time of
     C*   an initial visit (new patient) and this procedure constitutes
     C*   the major service at that visit, and the fee for the proc
     C*   is greater than the fee for the visit, the greater fee plus
     C*   one-half of the applicable visit fee is payable.  If the fee
     C*   asterisk(*) procedure is less than that for the applicable
     C*   visit, then the latter fee plus one-half the former is
     C*   payable.
     C*
     C*** BEGIN ADD 01/08/96 ******************************************
     C                   MOVE      'N'           XXSKIP            1
     C*** END   ADD 01/08/96 ******************************************
     C* Asterisk Procedure
     C     SXSUPP        IFEQ      'Y'
     C*
     C* SETLL and read the file testing for office visit
     C                   MOVE      '99201'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C* Office Medical Service - New Patient
     C  N54XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99205'
     C* Hospital Medical Service - New Patient
     C     XRCPT4        ORGE      '99221'
     C     XRCPT4        ANDLE     '99223'
     C* Emergency Dept Service - New Patient
     C     XRCPT4        ORGE      '99281'
     C     XRCPT4        ANDLE     '99285'
     C*
     C* Get MRA for office visit code
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C* Get MRA for Asterisk Procedure
     C                   Z-ADD     PRMRA         OFCMRA                         O/V MRA
     C                   END                                                    END 0 *IN54
     C*
     C* If O/V proc MRA is greater, pay surgery code at 50%
     C     OFCMRA        IFGE      XXMRA
     C                   MULT(H)   .50           XXMRA
     C*** BEGIN ADD 01/08/96 ******************************************
     C                   MOVE      'Y'           XXSKIP
     C*** END   ADD 01/08/96 ******************************************
     C                   END                                                    END GE XXMRA
     C*
     C                   END                                                    END EQ 99201
     C*
     C                   END                                                    END EQ Y
     C*
     C*-------------------------------------------------------------------
     C*-- Asterisk Visit with Follow-up Visit -------------
     C*   Per pg. S - 4, S - 5 #18
     C*   When an asterisk(*) procedure is carried out at the time of
     C*   a follow-up visit and this procedure contitutes the major
     C*   service at that visit, then the greater fee, not both fees,
     C*   is payable.
     C*
     C* Asterisk Procedure
     C     SXSUPP        IFEQ      'Y'
     C*
     C* SETLL and read the file testing for office visit
     C                   MOVE      '99211'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C* Office Medical Service - Established Patient
     C  N54XRCPT4        IFGE      '99211'
     C     XRCPT4        ANDLE     '99215'
     C* Hospital Medical Service - Subsequent Hospital Care
     C     XRCPT4        ORGE      '99231'
     C     XRCPT4        ANDLE     '99233'
     C*
     C* Get office visit MRA
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C                   Z-ADD     PRMRA         OFCMRA                         O/V MRA
     C                   END                                                    END 0 *IN54
     C*
     C* If ofc visit MRA is greater, pay surgery at 0%
     C     OFCMRA        IFGE      XXMRA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '212'         XPEOMB
     C*** EOMB 212: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END GE XXMRA
     C*
     C                   END                                                    END LE 99211
     C*
     C                   END                                                    END EQ Y
     C*
     C* ------------------------------------------------------------
     C*
     C*   Look for Major code in file, must have associated code else
     C*     error has occurred
     C     DHPRCD        IFEQ      '63035'
     C                   MOVE      '63030'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      '63031'       XXCPT4
     C*** NOTE CODES: 63031, 63021 HAVE BEEN DELETED IN 010196
     C*** LEFT HERE FOR 010191 UPDATE (NEED TO REMOVE FROM OLD TABLE)
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      '63021'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   MOVE      '63020'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C*** END   ADD *** 04/01/96 **************************************
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '216'         XPEOMB
     C* EOMB Code 216: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    ADD 04/01/96
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*   Look for Major code in file, must have associated code else
     C*     error has occurred
     C*
     C     DHPRCD        IFEQ      '63048'
     C                   MOVE      '63045'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      '63046'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      '63047'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '216'         XPEOMB
     C* EOMB Code 216: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*   If current code exists in table, must have associated code else
     C*     error has occurred
     C     DHPRCD        LOOKUP    TABA          TABB                     60
     C     *IN60         IFEQ      '1'
     C                   MOVE      TABB          XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '216'         XPEOMB
     C* EOMB Code 216: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 04/01/96 *************************************
     C*-------------------------------------------------------------------
     C*-- Multiple or bilateral procedures -------------
     C*   Per pg. S - 2
     C*   When more than one identifiable surgical proc is rendered
     C*   during the total follow-up period, an additional reimbur
     C*   may be warranted. The major proc at full value and 50%
     C*   for the lesser proce up to a total of twice of greater
     C*   value.
     C*   NOTE: Only an error message is given, since we cannot
     C*   tell if this is related to the major surg.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     1             X                 7 0
     C     X             DOWLE     500
     C     FUF(X)        ANDGT     *ZERO
     C*
     C     CAFDAT        IFGE      FUF(X)
     C     CAFDAT        ANDLE     FUT(X)
     C                   ADD       1             COUNT
     C                   ENDIF
     C*
     C                   ADD       1             X
     C*
     C                   ENDDO
     C*
     C* NEED TO DETERMINE IF THIS IS A MAJOR/MINOR CODE
     C* TO SET COUNT LIMIT TO 2 FOR EDIT
     C                   MOVE      'N'           XXMAJR            1
     C     DHPRCD        LOOKUP    TABA                                   60
     C     *IN60         IFEQ      '1'
     C                   MOVE      'Y'           XXMAJR
     C                   ENDIF
     C     DHPRCD        LOOKUP    TABB          TABA                     60
     C     *IN60         IFEQ      '1'
     C                   MOVE      'Y'           XXMAJR
     C                   MOVE      TABA          XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      'N'           XXMAJR
     C                   ENDIF
     C                   ENDIF
     C*
     C     DHPRCD        IFEQ      '63020'
     C     DHPRCD        OREQ      '63021'
     C     DHPRCD        OREQ      '63030'
     C     DHPRCD        OREQ      '63031'
     C                   MOVE      '63035'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      'Y'           XXMAJR
     C                   END
     C                   END
     C*
     C     DHPRCD        IFEQ      '63035'
     C     DHPRCD        OREQ      '63048'
     C                   MOVE      'Y'           XXMAJR
     C                   ENDIF
     C*
     C     DHPRCD        IFEQ      '63045'
     C     DHPRCD        OREQ      '63046'
     C     DHPRCD        OREQ      '63047'
     C                   MOVE      '63048'       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      'Y'           XXMAJR
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXMAJR        IFEQ      'Y'
     C     COUNT         ANDGT     2
     C     XXMAJR        OREQ      'N'
     C     COUNT         ANDGT     1
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'DM'          QQC(Q)
     C*** Err Msg DM: "If SURG proc is a follow-up to a previous major
     C***              SURG, pay at 50%"
     C                   ENDIF
     C*
     C*** END   ADD *** 04/01/96 *************************************
     C* ------------------------------------------------------------
     C*  Per Pg. S - 1 #5:  Multiple or Bilateral Procedures
     C*               When multiple procedures, unrelated to the major
     C*               procedure and adding significant item or
     C*               complexity are provided at the same operative
     C*               session, payment is for the procedure with the
     C*               highest allowance of twice the highest fee.  The
     C*               same rule applies for bilateral procedures when
     C*               such are not specifically identified in the sched
     C*  Surgical procedures (10000-69999) pay highest $ amount at
     C*  100% , all others pay 50% up to twice the highest amount
     C*
     C*                  Additional Fees...
     C*                  Multiple related procedures shall not warrant
     C*                  any additional fee(s) except in those areas
     C*                  where separate codes are listed.
     C*
     C*** BEGIN ADD *** 01/08/96 **************************************
     C     XXSKIP        IFEQ      'Y'
     C                   GOTO      NOCUT
     C                   ENDIF
     C*** END   ADD *** 01/08/96 **************************************
     C*
     C*
     C*  Per Pg. S - 5 #19:  POUND (#) Procedures:
     C*      Procedures flaged with a pound (#) sign are valued at
     C*      their appropriate level and Surgery Ground Rule 5
     C*      will not apply.
     C     SX1A5         IFEQ      'Y'
     C                   GOTO      NOCUT
     C                   ENDIF
     C*
     C*  If the procedure code is between 10000 and 69999 and the
     C*  modifier is zero, cut the charge if a larger one exists.
     C*  Write to work file.
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C*** BEGIN DEL *** 05/21/96 **************************************
     C********** XPPMOD    ANDEQ*BLANK
     C*** END   DEL *** 05/21/96 **************************************
     C                   MOVE      'S'           CATYPE
     C                   Z-ADD     XXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   WRITE     PCAPT1
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C*
     C*
     C     NOCUT         TAG
     C*
     C* ------------------------------------------------------------
     C*   All Other Codes To be handled here.
     C*   Perform the calculation for adjudciation.
     C*
     C*   If modifier exists, execute the Mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     CACHRG        XPTOTA
     C  N54XXMRA         SUB       XPTOTA        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
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
     C*** BEGIN ADD *** 11/25/96 **************************************
     C                   Z-ADD     0             XXCNT             3 0
     C                   Z-ADD     0             M                 2 0
     C                   CLEAR                   MOD
     C*** END   ADD *** 11/25/96 **************************************
     C*** BEGIN ADD *** 05/21/96 **************************************
     C                   Z-ADD     0             XXTOT             9 2
     C                   Z-ADD     0             XXWRK             9 2
     C                   Z-ADD     0             XXDIF             9 2
     C*** END   ADD *** 05/21/96 **************************************
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or 50%
     C     *LOVAL        SETLL     PCAPT3
     C                   READ      PCAPT3                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      P3CPT4        XXCPT4
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
     C                   Z-ADD     PRMRA         ZZMRA                          Chrg's MRA
     C*
     C*** BEGIN ADD *** 11/25/96 *************************************
     C*   SKIPPING IF MOD EQ -80 OR -81
     C     P3PMOD        IFNE      '80'
     C     P3PMOD        ANDNE     '81'
     C*** END ADD *** 11/25/96 ***************************************
     C                   ADD       1             COUNT
     C     COUNT         IFEQ      1
     C     ZZMRA         MULT      2             Z2MRA                          2X Chrg MRA
     C                   Z-ADD     ZZMRA         Z1MRA                          LARGEST MRA
     C                   END
     C*
     C*
     C     COUNT         IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C                   END                                                    END  GE 2
     C*** BEGIN ADD *** 11/25/96 *************************************
     C                   ENDIF                                                  END MOD 80,8
     C*** END ADD *** 11/25/96 ***************************************
     C*
     C*** Per Pg S - 5 #20:  AMPERSAND (&) PROCEDURES:
     C*       Procedures flagged with an ampersand (&) will be
     C*       reimbursed as follows:
     C*       61712: 25% of the primary procedure
     C*       64727: 25% of the primary procedure
     C*       64783: 25% of the primary procedure
     C*       64830: 50% of the nerve repair
     C*       64872: 25% of the primary procedure
     C*       64874: 25% of the nerve suture
     C*       64876: 25% of the nerve suture
     C*** Per LYNN use proceure with the highest dollar amount
     C***
     C     PR1A6         IFEQ      'Y'
     C     P3CPT4        IFEQ      '61712'
     C     P3CPT4        OREQ      '64727'
     C     P3CPT4        OREQ      '64783'
     C     P3CPT4        OREQ      '64872'
     C     P3CPT4        OREQ      '64874'
     C     P3CPT4        OREQ      '64876'
     C     Z1MRA         MULT      .25           ZZMRA
     C     P3TOTC        COMP      ZZMRA                              54        >
     C   54              Z-ADD     ZZMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     P3TOTC        XPTOTA
     C  N54ZZMRA         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   MOVEL     P3PMOD        XPPMOD
     C*** END   ADD *** 04/01/96 **************************************
     C                   EXSR      UPDAT3
     C                   READ      PCAPT3                                 60
     C                   ITER
     C                   END                                                    END P3CPT4  T L97503
     C*
     C     P3CPT4        IFEQ      '64830'
     C     Z1MRA         MULT      .50           ZZMRA
     C     P3TOTC        COMP      ZZMRA                              54        >
     C   54              Z-ADD     ZZMRA         XPTOTA
     C   54              Z-ADD     *ZEROS        XPDIFF
     C  N54              Z-ADD     P3TOTC        XPTOTA
     C  N54ZZMRA         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   MOVEL     P3PMOD        XPPMOD
     C*** END   ADD *** 04/01/96 **************************************
     C                   EXSR      UPDAT3
     C                   READ      PCAPT3                                 60
     C                   ITER
     C                   END                                                    END P3CPT4  T L97503
     C                   END                                                    END PR1A6   E *ZERO
     C***
     C*** BEGIN ADD *** 11/25/96 **************************************
     C     P3PMOD        IFNE      *BLANKS
     C     P3PMOD        LOOKUP    MOD                                    60
     C     *IN60         IFEQ      '0'
     C                   ADD       1             M
     C                   MOVE      P3PMOD        MOD(M)
     C                   Z-ADD     1             XXCNT
     C                   ELSE
     C                   Z-ADD     0             XXCNT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 11/25/96 **************************************
     C***
     C*** BEGIN ADD *** 05/21/96 **************************************
     C*
     C*
     C     P3PMOD        IFEQ      '80'
     C*
     C* PAYING MOD 80 AT 20% IF FIRST AND 10% IF MORE THAN 1
     C     XXCNT         IFEQ      1                                            ADD 11/25/96
     C                   Z-ADD     .20           XXPERC
     C                   ELSE                                                   ADD 11/25/96
     C                   Z-ADD     .10           XXPERC                         ADD 11/25/96
     C                   ENDIF                                                  ADD 11/25/96
     C     ZZMRA         MULT(H)   XXPERC        ZZMRA
     C                   END                                                    END EQ '80'
     C*
     C     SVSPCD        IFEQ      'PA'
     C                   Z-ADD     .10           XXPERC
     C     ZZMRA         MULT(H)   XXPERC        ZZMRA
     C                   END                                                    END EQ '81'
     C*
     C*********  P3PMOD    OREQ '81'                       CHG 11/25/96
     C     P3PMOD        IFEQ      '81'                                         CHG 11/25/96
     C* PAYING MOD 81 AT 10% IF FIRST AND 05% IF MORE THAN 1
     C     XXCNT         IFEQ      1                                            ADD 11/25/96
     C                   Z-ADD     .10           XXPERC
     C                   ELSE                                                   ADD 11/25/96
     C                   Z-ADD     .05           XXPERC                         ADD 11/25/96
     C                   ENDIF                                                  ADD 11/25/96
     C     ZZMRA         MULT(H)   XXPERC        ZZMRA
     C                   END                                                    END EQ '81'
     C*
     C     XXTOT         IFEQ      Z2MRA
     C* REACHED MAX
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C                   ELSE                                                   ELSE XXTOT
     C*
     C     ZZMRA         ADD       XXTOT         XXWRK
     C     XXWRK         IFLE      Z2MRA
     C     P3TOTC        IFLE      ZZMRA
     C* PAY P3TOTC
     C                   Z-ADD     P3TOTC        XPTOTA
     C     ZZMRA         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   ADD       P3TOTC        XXTOT
     C                   ELSE
     C* PAY ZZMRA
     C                   Z-ADD     ZZMRA         XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   ADD       ZZMRA         XXTOT
     C                   ENDIF
     C                   ELSE                                                   ELSE XXWRK
     C*
     C* COMPUTE DIFFERENCE TO PAY
     C     Z2MRA         SUB       XXTOT         XXDIF
     C     P3TOTC        IFLE      XXDIF
     C* PAY P3TOTC
     C                   Z-ADD     P3TOTC        XPTOTA
     C     XXDIF         SUB       XPTOTA        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   ADD       P3TOTC        XXTOT
     C                   ELSE
     C* PAY XXDIF
     C                   Z-ADD     XXDIF         XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   ADD       XXDIF         XXTOT
     C                   ENDIF                                                  END P3TOTC
     C                   ENDIF                                                  END XXWRK
     C                   ENDIF                                                  END XXTOT
     C*** END   ADD *** 05/21/96 **************************************
     C*** BEGIN DEL *** 05/21/96 **************************************
     C********   Z2MRA     SUB  ZZMRA     LHIGH                        LE *ZERO
     C********             Z-ADDLHIGH     Z2MRA                        LE *ZERO
     C********   LHIGH     IFLT *ZERO                                  LE *ZERO
     C********             Z-ADD*ZERO     XPTOTA                       LE *ZERO
     C********             Z-ADD*ZERO     XPTOTP                       LE *ZERO
     C********   P3TOTC    SUB  XPTOTA    XPTOTD                       LE *ZERO
     C********             Z-ADD*ZEROS    XPDIFF                       LE *ZERO
     C********             MOVE '204'     XPEOMB                       LE *ZERO
     C*** EOMB 204: "Exceeds number of allowable procedures in reimburse manual"
     C********                                                         LE *ZERO
     C********             ELSE                            ELSE LHIGH  LE *ZERO
     C********   P3TOTC    IFGT LHIGH
     C********   LHIGH     COMP ZZMRA                54    >
     C***54***             Z-ADDZZMRA     XPTOTA                       LE *ZERO
     C***54***             Z-ADD*ZEROS    XPDIFF                       LE *ZERO
     C********   *IN54     IFEQ '0'                                    LE *ZERO
     C********   LHIGH     ANDNE*ZEROS                                 LE *ZERO
     C********             Z-ADDLHIGH     XPTOTA                       LE *ZERO
     C********             ELSE                                        LE *ZERO
     C********             Z-ADDZZMRA     XPTOTA                       LE *ZERO
     C********             END                                         LE *ZERO
     C**N54***   ZZMRA     SUB  XPTOTA    XPDIFF                       LE *ZERO
     C********             Z-ADDXPTOTA    XPTOTP                       LE *ZERO
     C********   P3TOTC    SUB  XPTOTA    XPTOTD                       LE *ZERO
     C********             MOVE '002'     XPEOMB                       LE *ZERO
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C********             ELSE                            ELSE CACHRG GT L97503
     C********   P3TOTC    COMP ZZMRA                54    >           LE *ZERO
     C***54***             Z-ADDZZMRA     XPTOTA                       LE *ZERO
     C***54***             Z-ADD*ZEROS    XPDIFF                       LE *ZERO
     C**N54***             Z-ADDP3TOTC    XPTOTA                       LE *ZERO
     C**N54***   ZZMRA     SUB  XPTOTA    XPDIFF                       LE *ZERO
     C********             Z-ADDXPTOTA    XPTOTP                       LE *ZERO
     C********   P3TOTC    SUB  XPTOTA    XPTOTD                       LE *ZERO
     C********             END                             END CACHRG GTEL97503
     C********             END                             END LHIGH  LEE*ZEROO
     C*** END   DEL *** 05/21/96 **************************************
     C*** BEGIN ADD *** 04/01/96 **************************************
     C                   MOVEL     P3PMOD        XPPMOD
     C*** END   ADD *** 04/01/96 **************************************
     C                   EXSR      UPDAT3
     C*
     C                   READ      PCAPT3                                 60
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
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
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
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
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
     C*  Only process the record if not an override type
     C     CAKEYA        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C*** BEGIN DLT *** 04/01/96 **************************************
     C*****************    MOVELXPPMOD    XPPMOD
     C*** END   DTL *** 04/01/96 **************************************
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
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY8         CHAIN     PCA06                              54
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
     C*
     C     UPDAE         ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR SURG  FILE FACAPOK             U P D A T2 *
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
     C*** BEGIN CHG *** 04/01/96 **************************************
     C**********           Z-ADDP3FEIN    CAFEIN
     C**********           Z-ADDP3MOD     CAMOD
     C                   Z-ADD     P3FEIN        XXFEIN
     C                   Z-ADD     P3MOD         XXMOD
     C*** END   CHG *** 04/01/96 **************************************
     C                   MOVE      P3CPT4        XXCPT4
     C                   MOVEL     P3PMOD        XXPMOD
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
     C     PXFDAT        ANDEQ     P3FDAT
     C     PXFEIN        ANDEQ     P3FEIN
     C     PXDSEQ        ANDNE     P3DSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P3DSEQ        QQS(Q)
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
     C*  Using FACAPOK record, chain to FDCAP record to update
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
     C                   MOVEL     XPPMOD        XRPMOD
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
     C*** BEGIN CHG *** 04/01/96 **************************************
     C**********           Z-ADDP3FEIN    CAFEIN
     C**********           Z-ADDP3MOD     CAMOD
     C                   Z-ADD     P3FEIN        XXFEIN
     C                   Z-ADD     P3MOD         XXMOD
     C*** END   CHG *** 04/01/96 **************************************
     C                   MOVE      P3CPT4        XXCPT4
     C                   MOVEL     P3PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*** BEGIN CHG *** 04/01/96 **************************************
     C********** CAKY8     CHAINPCA06                54
     C     CAKY6X        CHAIN     PCA06                              54
     C*** END   CHG *** 04/01/96 **************************************
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE3        ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR RADI  FILE FACAPOK         U P D A T 5    *
     C********************************************************************
     C     UPDAT5        BEGSR
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
     C                   Z-ADD     P5DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C*** BEGIN CHG *** 04/01/96 **************************************
     C**********           Z-ADDP5FEIN    CAFEIN
     C**********           Z-ADDP5MOD     CAMOD
     C                   Z-ADD     P5FEIN        XXFEIN
     C                   Z-ADD     P5MOD         XXMOD
     C*** END   CHG *** 04/01/96 **************************************
     C                   MOVE      P5CPT4        XXCPT4
     C                   MOVEL     P5PMOD        XXPMOD
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
     C     PXFDAT        ANDEQ     P5FDAT
     C     PXFEIN        ANDEQ     P5FEIN
     C     PXDSEQ        ANDNE     P5DSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P5DSEQ        QQS(Q)
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
     C* ---------------------------------------------------------------
     C*  Using FACAPOK record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C     CAKYA5        CHAIN     PCA08                              54
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
     C                   MOVEL     XPPMOD        XRPMOD
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
     C*** BEGIN CHG *** 04/01/96 **************************************
     C**********           Z-ADDP5FEIN    CAFEIN
     C**********           Z-ADDP5MOD     CAMOD
     C                   Z-ADD     P5FEIN        XXFEIN
     C                   Z-ADD     P5MOD         XXMOD
     C*** END   CHG *** 04/01/96 **************************************
     C                   MOVE      P5CPT4        XXCPT4
     C                   MOVEL     P5PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*** BEGIN CHG *** 04/01/96 **************************************
     C********** CAKY8     CHAINPCA06                54
     C     CAKY6X        CHAIN     PCA06                              54
     C*** END   CHG *** 04/01/96 **************************************
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P5DSEQ        QQS(Q)
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
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE5        ENDSR
     C********************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
     O*
**  TABA/TABB: A MUST HAVE B AND A WILL NOT BE CUT 50 %
1100111000
1105111050
1105211051
1120111200
1170111700
1170211701
1171111710
1171211711
1173111730
1173211731
1510115100
1512115120
1512215121
1520115200
1520215201
1522115220
1522215221
1524115240
1524215241
1526115260
1526215261
2526125260
2526425263
2526625265
2527125270
2527325272
2527525274
2528125280
2529125290
2529625295
2612626124
2612726126
2617226710
2635126350
2635326352
2769227691
6307663075
6462364622
6477864776
6478364782
6483264831
**  QCMDEXC TABLE
DLTOVR FILE(FACALOK)
OVRDBF FILE(FACALOK) TOFILE(QTEMP/FACALOK)
DLTOVR FILE(FACAL1OK)
OVRDBF FILE(FACAL1OK) TOFILE(QTEMP/FACAL1OK)
DLTOVR FILE(FACAL2OK)
OVRDBF FILE(FACAL2OK) TOFILE(QTEMP/FACAL2OK)
DLTOVR FILE(FACAPOK)
OVRDBF FILE(FACAPOK) TOFILE(QTEMP/FACAPOK)
DLTOVR FILE(FACAL3OK)
OVRDBF FILE(FACAL3OK) TOFILE(QTEMP/FACAL3OK)
