     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4073RFL                                          *
     H*    DESCRIPTION: ADJUDICATION (1997 RULES)                     *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*   This program was derived from the Florida State Fee         *
     H*   Schedule Reimbursment Manual issued on 9/01/97.             *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  06/01/91.                                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  04/03/00 C:79 SUPPORT:MC044/22406                            *
     H*                1. Use PDH01 in only one place after reading   *
     H*                   line item                                   *
     H*                2. Don't move line item to paid array in PHYS  *
     H*                                                               *
     H*  11/23/99 C:79 SUPPORT:20789                                  *
     H*               Move P2EOMB to XPEOMB before call to EOMBP2     *
     H*                                                               *
     H*  11/04/99 C:79 SUPPORT:20348                                  *
     H*               Added DSEQ logic to Possible dup criteria       *
     H*                                                               *
     H*  4/15/99 C:79 PROJ:FL019                                      *
     H*               Modified to work with provider bills            *
     H*****************************************************************
     H*  NOTES: MOST OF THE OLD LOGICALS (PRE-PROVIDER BILLS) USED    *
     H*         CACPT4 EXTENSIVLY AND EVEN THOUGH THE FIELD IS NO     *
     H*         LONGER USED IT IS STILL BEING POPULATED IN THE NEW    *
     H*         PROVIDER BILLS SO THESE LOGICALS WERE NOT CHANGED.    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPRL     IF   E           K DISK
     FFTABLFL   IF   E           K DISK
     F                                     RENAME(PRC01:PRC0W)
     FFDCAL9    UF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALH    UF   E           K DISK
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFDDHL     IF   E           K DISK
     FFDDHL1    IF   E           K DISK
     F                                     RENAME(PDH01:PDH02)
     FFDDHL2    IF   E           K DISK
     F                                     RENAME(PDH01:PDH03)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     FFMRCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFACAPFL   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT1)
     FFACALFL   UF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT2)
     FFACAL1FL  UF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT3)
     FFACAL2FL  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT4)
     FFDCALF    UF   E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     F*
     F*

     D*
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
     D PT1             S              2    DIM(30)                              BODY PART CODE
     D PT2             S              7    DIM(30)                              PART/CPT4 CODE
     D PT3             S              5  0 DIM(30)                              SEQUENCE NO.
     D PT4             S              9  2 DIM(30)                              AMOUNT
     D PT5             S              2    DIM(30)                              BODY PART CODE
     D PT6             S              9  2 DIM(30)                              SUMMARY AMOUNT
     D PT9             S             16    DIM(30)                              CHK/EOB
     D PT7             S             11    DIM(30)                              SUM/AMOUNT-PAR
     D AJB             S             71    DIM(30)                              SUMMARY
     D AJ1             S              9  2 DIM(30)                              ADJUDE AMOUNT1
     D AJ2             S              9  2 DIM(30)                              ADJUDE AMOUNT2
     D AJ3             S              3  0 DIM(30)                              POINTER
     D AJC             S              1    DIM(30)                              HISTORY CODE
     D AJM             S              2    DIM(30)                              PROC MODIFIERS
     D PFN             S              9  0 DIM(30)                              PROVIDER FEIN
     D PMD             S              3  0 DIM(30)                              PROVIDER MOD
     D PTY             S              1    DIM(30)                              PROVIDER TYPE
     D*
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*
     D*
     D NC1             S              5    DIM(90) CTDATA PERRCD(10)            NO CUT ON THESE
     D NC2             S              5    DIM(50) CTDATA PERRCD(10)            NO CUT ON THESE 2
     D TABA            S              5    DIM(22) CTDATA PERRCD(1)             NO CUT/MAJOR-MIN
     D TABB            S              5    DIM(22) ALT(TABA)
     D RAD1            S              5    DIM(95) CTDATA PERRCD(1)             RADIOLOGY INCL/IN
     D RAD2            S              5    DIM(95) ALT(RAD1)
     D OFC1            S              5    DIM(135) CTDATA PERRCD(1)            Office Visit XREF
     D OFC2            S              5    DIM(135) ALT(OFC1)
     D*
     D IPT2            S              5    DIM(11) CTDATA PERRCD(1)             Initial PT for 97145
     D QCA             S             55    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D* ====== The following arrays are used in PHYS3 =======
     D* Workfile Arrays
     D WBPC            S              9  2 DIM(999)                             Chrg Amt
     D WBPO            S              3  0 DIM(999)                             Entry Order #
     D WBP             S              2    DIM(999)                             Body Part
     D WBP#            S              5  0 DIM(999)                             Detail Seq #
     D WBPM            S              2    DIM(999)                             Proc Code Mod
     D* The following makes up WBPS:  WBPC, WBPO, WBP, WBP#, WBPM
     D WBPS            S             21    DIM(999) DESCEND                     Summary (Descending)
     D*
     D* Misc Body Part Arrays
     D BPP             S              9  2 DIM(999)                             Paid Amt
     D BP              S              2    DIM(999) ASCEND                      Body Part
     D BPC             S              9  2 DIM(999)                             Chrg Amt
     D BP$             S              9  2 DIM(999)                             MRA Amt
     D* The following makes up BPS:  BPP, BP, BPC, BP$
     D BPS             S             29    DIM(999) DESCEND                     Summary (Descending)
     D*
     D* History Arrays
     D HBPP            S              2    DIM(999) ASCEND                      Hist BP Paid
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*  SORT ARRAY WORK DATA STRUCTURE.
     D*
     D                 DS
     D  DSSORT                 1     71
     D  DSAMT                  1      9  2
     D  DSDSEQ                10     14  0
     D  DSCHK#                15     21  0
     D  DSEOB#                22     30  0
     D  DSAJC                 31     31
     D  DSPART                32     33
     D  DSCPT4                34     38
     D  DSAJ1                 39     47  2
     D  DSAJ2                 48     56  2
     D  DSAJM                 57     58
     D  DSFEIN                59     67  0
     D  DSMOD                 68     70  0
     D  DSPRTY                71     71
     D*
     D* ====== The following Data Structures are used in PHYS3 ======
     D*
     D*  Workfile Body Part Sort Data Structure
     D                 DS
     D  WFSORT                 1     21
     D  WFTOTC                 1      9  2
     D  WFORDR                10     12  0
     D  WFPART                13     14
     D  WFDSEQ                15     19  0
     D  WFPMOD                20     21
     D*
     D*  Workfile Misc. Body Part Sort Data Structure
     D                 DS
     D  BPSORT                 1     29
     D  BPTOTP                 1      9  2
     D  BPPART                10     11
     D  BPTOTC                12     20  2
     D  BPMRA                 21     29  2

     I* =====================================================
     I*** END ADD ***** 11/09/92 **************************************
     I*
     I*
     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     IPRC0W
     I              RCKEY                       RCKEYW
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
     I              CAFLAG                      XRFLAG
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
     IPCA06
     I              CACO#                       X6CO#
     I              CASUB#                      X6SUB#
     I              CAFND                       X6FND
     I              CAFYR                       X6FYR
     I              CACASE                      X6CASE
     I              CAFDAT                      X6FDAT
     I              CADSEQ                      X6DSEQ
     I              CAFEIN                      X6FEIN
     I              CAMOD                       X6MOD
     I              CACPT4                      X6CPT4
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
     IPCA0F
     I*
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
     I* Record Format for Physical Therapy Workfile FACALFL
     IPCAPT2
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
     I              CATYPE                      P2TYPE
     I* Record Format for Physical Therapy Workfile FACAL1FL
     IPCAPT3
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
     I              CATYPE                      P3TYPE
     I* Record Format for Surgery Workfile FACAL2FL
     IPCAPT4
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
     I              CATYPE                      P4TYPE
     I*
     IPDH02
     I              DHCO#                       DXCO#
     I              DHSUB#                      DXSUB#
     I              DHFND                       DXFND
     I              DHFYR                       DXFYR
     I              DHCASE                      DXCASE
     I              DHSS#                       DXSS#
     I              DHACDT                      DXACDT
     I              DHEOB#                      DXEOB#
     I              DHDSEQ                      DXDSEQ
     I              DHPOS                       DXPOS
     I              DHTOS                       DXTOS
     I              DHPRCD                      DXPRCD
     I              DHPRDS                      DXPRDS
     I              DHMOD1                      DXMOD1
     I              DHMOD2                      DXMOD2
     I              DHMOD3                      DXMOD3
     I              DHMOD4                      DXMOD4
     I              DHDXR1                      DXDXR1
     I              DHDXR2                      DXDXR2
     I              DHDXR3                      DXDXR3
     I              DHDXR4                      DXDXR4
     I              DHUNTS                      DXUNTS
     I              DHAREA                      DXAREA
     I              DHPPLN                      DXPPLN
     I              DHSTAT                      DXSTAT
     I              DHUSER                      DXUSER
     I              DHPGM                       DXPGM
     I              DHTIME                      DXTIME
     I              DHDT                        DXDT
     I              DHIUSR                      DXIUSR
     I              DHIPGM                      DXIPGM
     I              DHITIM                      DXITIM
     I              DHIDT                       DXIDT
     I              DHRCFL                      DXRCFL
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
     I* =============================================================

     C*
     C/EJECT
     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE
     C     CMKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  CLAIM DETAIL FILE
     C     CAKYH         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    ZZYMD
     C*
     C     CAKYH1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     CAKY6A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    CAPMOD
     C                   KFLD                    XXPART            2
     C*
     C*  CLAIM DETAIL FILE
     C     CAKEY8        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4            5
     C*
     C*  CLAIM DETAIL FILE
     C     CAKY8         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    XXPMOD            2
     C*
     C     CAKY84        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    XXPMOD            2
     C*
     C*  CLAIM DETAIL FILE
     C     CAKYA         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SXFEIN
     C                   KFLD                    SXMOD
     C*
     C*  CLAIM DETAIL FILE
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
     C*  CLAIM DETAIL FILE
     C     CAKEY1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXDSEQ            5 0
     C*
     C*  CLAIM DETAIL FILE
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
     C*  CLAIM DETAIL FILE
     C     CAKYC2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*** Used with PCA06 (FDCAL6) in PHYS3
     C     CAKYP1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P2FDAT
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C*** Used with PCA09 (FDCAL9) in PHYS3
     C     CAKYP2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P2FDAT
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C                   KFLD                    XXCPT4
     C*
     C*** Used with PCAPT3 (FACAL1FL - Detail Charge Workfile) in PHYS3
     C     CAKYP3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXDSEQ
     C*
     C*** Used with PCA09 (FDCAL9) in PHYS3
     C     CAKYP4        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P2FDAT
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C*
     C*** Used with PCA09 (FDCAL9) in PHYS3
     C     CAKYA2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P2FDAT
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C                   KFLD                    P2CPT4
     C                   KFLD                    P2DSEQ
     C*                                                                *****
     C     CAKYA4        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    HDYMD
     C                   KFLD                    HDFEIN
     C                   KFLD                    HDMOD
     C                   KFLD                    P4CPT4
     C                   KFLD                    P4DSEQ
     C*
     C*
     C     DHKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     DHKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
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
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C*  EOMB CODE FILE MASTER (FDDEL4)
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
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
     C                   KFLD                    XXDSEQ
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
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C*
     C*  PROC CODE MODIFIER  FILE
     C     PMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  SERV CODE FILE
     C     SVKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASVCD
     C                   KFLD                    CAFDAT
     C*
     C*  PROCEDURE CODE FILE
     C     PRKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    XXEFDT            7 0
     C*
     C*  PROVIDER FILE
     C     PDKEY         KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*** REASONABLE & CUSTOMARY FILE
     C     RCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA                         ADD 02/07/92
     C                   KFLD                    XXCPCD            5
     C*** Geographical Zip Area FILE
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3            ADD 12/28/92
     C                   PARM                    QQEOB#            9 0
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
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
     C     *LIKE         DEFINE    PR1A1         SX1A1                          11/04/91 ADD
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA           05/13/93 ADD
     C     *LIKE         DEFINE    RCZIPA        XXZIPA                         05/13/93 ADD
     C*** END   CHG *** 08/09/02 **************************************
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP           05/13/93 ADD
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         ADD 08/22/94
     C     *LIKE         DEFINE    CAFDAT        SVYMD                          ADD 01/25/96
     C     *LIKE         DEFINE    CAFDAT        HDYMD                          ADD 01/25/96
     C     *LIKE         DEFINE    CAFDAT        ZZYMD                          ADD 01/25/96
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          ADD 01/25/96
     C     *LIKE         DEFINE    CAFEIN        HDFEIN                         ADD 01/25/96
     C     *LIKE         DEFINE    CAMOD         HDMOD                          ADD 01/25/96
     C     *LIKE         DEFINE    CAFEIN        XXFEIN                         ADD 04/29/96
     C     *LIKE         DEFINE    CAEOB#        XXEOB#                         ADD 04/29/96
     C     *LIKE         DEFINE    CACASE        XXCASE                         ADD 04/29/96
     C     *LIKE         DEFINE    CADSEQ        QQDSEQ                         ADD 04/29/96
     C     *LIKE         DEFINE    CAMOD         XXMOD                          ADD 04/29/96
     C     *LIKE         DEFINE    EORCN         XXRCN                          ADD 04/29/96
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CATOTC        XPTOTC
     C     *LIKE         DEFINE    CATOTP        XPTOTP
     C     *LIKE         DEFINE    CATOTD        XPTOTD
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAPMOD        H2PMOD                         ADD 12/03/92
     C     *LIKE         DEFINE    DHPRCD        HLBRPC                         Hold BR Proc Code
     C     *LIKE         DEFINE    DHAREA        HLBRPT                         Hold BR Part Code
     C     *LIKE         DEFINE    CAPMOD        HLBRPM                         Hold BR Proc Code Mo
     C*** NOTE:  BP stands for Body Part
     C***        BR stands for Best Reflects
     C                   Z-ADD     *ZEROS        B                 5 0          Misc Counter
     C                   Z-ADD     *ZEROS        H                 5 0          Misc Counter
     C                   Z-ADD     *ZEROS        I                 5 0          Misc Counter
     C                   Z-ADD     *ZEROS        W                 5 0          Misc Counter
     C                   Z-ADD     *ZEROS        WO                5 0          Entry Order Counter
     C                   Z-ADD     *ZEROS        CNTPRC            5 0          Misc Counter
     C                   MOVE      *BLANK        BPFRST            1            BP First Code Flg
     C                   MOVE      *BLANK        OPNFLG            1
     C                   MOVE      *BLANK        OVRFLG            1
     C                   MOVE      *BLANK        SPINE             1            97260/97261 Spine Fl
     C                   MOVE      *BLANK        XFIRST            1            1st creation of FACA
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
     C*
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C                   MOVE      '0'           XX25EM                         04/19/90 ADD
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)                         INVALID USER PARM
     C*
     C                   GOTO      TERM
     C                   END
     C*
     C                   Z-ADD     QQCO#         QQCO#
     C                   Z-ADD     QQSUB#        QQSUB#
     C                   Z-ADD     QQFND         QQFND
     C*
     C** GET THE CLAIM MASTER RECORD.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C*
     C                   GOTO      TERM
     C                   END
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
     C                   Z-ADD     *ZEROS        ZZYMD
     C*
     C     CAKYH         SETLL     PCA01
     C     CAKYH1        READE     PCA01                                  54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C     CAFDAT        IFGE      0910601
     C     CAFDAT        ANDLT     0970901
     C     XXRCN         ANDEQ     CARCN
     C*
     C*
     C                   Z-ADD     CADSEQ        XXDSEQ
     C                   Z-ADD     CAFDAT        XXYMD
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ            5 0          SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN            9 0          FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD             3 0          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY            1
     C                   Z-ADD     DHUNTS        SVUNTS            5 2
     C                   MOVEL     DHPRCD        XPCPT4            5
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#            9 0
     C                   Z-ADD     CACHK#        SVCHK#            7 0
     C*
     C                   MOVE      CAFEIN        SXFEIN            9 0          SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD             3 0          ARE PROCESSING
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*                                                    ARRAYS
     C*** GET MRA AMOUNT FOR SPECIAL RULE
     C                   MOVEL     '95834'       XXCPT4
     C                   MOVE      910601        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
     C     NXTPR1        TAG
     C                   READP     PPR01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     '95834'
     C     PRSTAT        CABEQ     'I'           NXTPR1
     C                   Z-ADD     PRMRA         MRA1              9 2
     C                   ELSE
     C                   Z-ADD     *ZERO         MRA1              9 2
     C                   END
     C                   Z-ADD     *ZERO         TOTA1             9 2
     C*
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END GT SYDATE
     C*
     C*** Set flag for FS Non-Reimburseable - Error Message #25
     C                   MOVE      '0'           XX25EM
     C*
     C** GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C     CSOVRD        IFNE      'Y'
     C*
     C*****  CHECK TO SEE IF THIS RECORD IS A REASONABLE/CUSTOMARY RECORD
     C*
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPBRTC                          ADD 08/22/94
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
     C     RCZIPA        ANDEQ     XXZIPA                                       ADD 05/13/93
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     RCRPT         IFEQ      'Y'
     C*
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '229'         XPEOMB
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE
     C                   GOTO      NXRC1
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*** HOSPITAL SUBROUTINE OR REGULAR PROVIDERS.
     C*
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD                                       ADD 04/28/92
     C                   ELSE
     C*
     C** TEST FOR FEE SCHEDULE REIMBURSEMENT = 'Y'
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DHPRCD
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)                         INVALID PROC CODE
     C*** Err Msg 98: "Invalid Procedure Code, See Programmer"
     C*
     C* Release the record lock on FDCALH
     C                   EXCEPT    ECA01
     C*
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C*
     C*
     C*
     C*
     C*
     C** IF FEE SCHEDULE REIMBURSEMENT EQUALS 'N' BUT THE MAX REIMB AMT
     C** IS EQUAL TO ZERO,  TAKE THE INFO AS IS.
     C*
B6   C     PRFSR         IFEQ      'N'
     C*
     C** FLAG TO DISPLAY FEE SCHEDULE NON-REIMBURSEABLE ERROR MESSAGE
     C                   MOVE      '1'           XX25EM            1            ## 4/17/89
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 77, XIII, F, 4
     C*** Per pg. 114, II, E, 4
     C*** Procedure Code 97752 includes a printout of test results and separate
     C*** reimbursement must not be made under Procedure code 99090.
     C*
     C     DHPRCD        IFEQ      '99090'
     C                   MOVE      '97752'       XXCPT4
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
B7   C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C**
 X7  C                   ELSE
     C**
     C** IF FEE SCHEDULE REIMBURSEMENT EQUALS 'N' BUT THE MAX REIMB AMT
     C** IS GREATER THAN ZERO, THIS CPT4 CODE HAS BEEN ASSIGNED A
     C** SPECIFIC MAX REIMB. AMT BY THE INSURANCE COMPANY.
     C**
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed reasonable and customary charges"
     C   54              Z-ADD     PRMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
E7   C                   END
E6   C                   END                                                    END PRFSR eq N
     C*
     C*
     C*
     C*
     C*
     C*
     C** IF FEE SCHEDULE REIMBURSEMENT EQUALS 'Y', USE MAX REIMB AMT
     C*
     C** SAVE THE PROCEDURE CODE FILE VALUES.
     C*
B6   C     PRFSR         IFEQ      'Y'
     C                   Z-ADD     PRMRA         SXMRA             9 2
     C                   Z-ADD     PRANAM        SXANAM            9 2
     C                   Z-ADD     PRPROF        SXPROF            9 2
     C                   MOVE      PRRPT         SXRPT             1
     C                   MOVE      PRSUPP        SXSUPP            1
     C                   MOVE      PRSURG        SXSURG            1
     C                   MOVE      PR1A1         SX1A1                          11/04/91 ADD
     C*
     C*  % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % *
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     PRRPT         IFEQ      'Y'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 155, for code 15000, must have a 15050-15400
     C*
     C     DHPRCD        IFEQ      '15000'
     C                   MOVE      '15050'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '1'
     C     XRCPT4        ORGT      '15400'
     C*
     C*** PAY ZERO MUST HAVE ONE OF THE OTHER CODES.
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C*** Err Msg 16: "A Major code must be present to pay this item"
     C*
     C                   END
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '15'          QQC(Q)
     C*** Err Msg 15: "This item is paid at 50% of the Major Codes MRA.
     C***              Please Key Amount"
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 198, for code 61712, must have a 61050-61570
     C*
     C     DHPRCD        IFEQ      '61712'
     C                   MOVE      '61050'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '1'
     C     XRCPT4        ORGT      '61570'
     C*
     C*** PAY ZERO MUST HAVE ONE OF THE OTHER CODES.
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C*** Err Msg 16: "A Major code must be present to pay this item"
     C*
     C                   END
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '13'          QQC(Q)
     C*** Err Msg 13: "This item is paid at 20% of the Major Codes MRA.
     C***              Please key amount"
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 202, for code 64830, must have a 64702-64818
     C*
     C     DHPRCD        IFEQ      '64830'
     C                   MOVE      '64702'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '1'
     C     XRCPT4        ORGT      '64818'
     C*
     C*** PAY ZERO MUST HAVE ONE OF THE OTHER CODES.
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C*** Err Msg 16: "A Major code must be present to pay this item"
     C*
     C                   END
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '13'          QQC(Q)
     C*** Err Msg 13: "This item is paid at 20% of the Major Codes MRA.
     C***              Please key amount"
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 202-203, for code 64872-65876, must have a 64831-64856
     C*
     C     DHPRCD        IFGE      '64872'
     C     DHPRCD        ANDLE     '64876'
     C                   MOVE      '64831'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '1'
     C     XRCPT4        ORGT      '64856'
     C*
     C*** PAY ZERO MUST HAVE ONE OF THE OTHER CODES.
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C*** Err Msg 16: "A Major code must be present to pay this item"
     C*
     C                   END
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '14'          QQC(Q)                         04/19/90 CHG
     C*** Err Msg 14: "This item is paid at 25% of the Major Codes MRA.
     C***              Please key amount"
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per Pg. 57-58, VI, E
     C*** The provider & carrier should agree on the amount of reimbursement
     C*** to be made for an IME.
     C     DHPRCD        IFEQ      '96320'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '46'          QQC(Q)
     C*** Err Msg 46: "For reimbursement of IME Code 96320, refer to
     C***              Provider/Carrier Agreement"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 55, V, 2, a
     C*** The pharmaceutical reimbursement formula for prescribed drugs
     C*** (medicines) by dispensing practitioners is:
     C*** Average Wholesale Price (AWP) x 1.2 + $4.18 = Reimbursement
     C*
     C     DHPRCD        IFEQ      '96370'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '45'          QQC(Q)
     C*** Err Msg 45: "Reimbursement = AWP x 1.2 + $4.18
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END EQ 96370
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     CAPMOD        IFNE      *BLANKS
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
     C                   MOVE      '005'         XPEOMB
     C* EOMB 013: "Medical Report required for Payment"
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
     C*** Err Msg 06: "The item is By Report - Requires futher inspection
     C***              by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD                         07/16/91 CHG
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
     C                   MOVE      '005'         XPEOMB
     C* EOMB 013: "Medical Report required for Payment"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END 'BR'
     C*
     C*  % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % *
     C*
     C*** IF PROCEDURE IS "ANE" FLAG IT AS SUCH AND UPDATE RECORD      .
     C*
     C     PRRPT         IFEQ      'A'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     DHPRCD        IFGE      '70000'
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        ORGE      '95819'
     C     DHPRCD        ANDLE     '95950'
     C*** Per pg. 83, XV, A, 4, c
     C*** Should the provider fail to add modifier -26 to a
     C*** radiology, pathology or neurology procedure
     C*** provided in a hospital, ambulatory surgical center or similiar
     C*** facility, reimbursement must only be made for the professional
     C*** component.
     C*
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      'B'
     C     CAPMOD        IFEQ      *BLANKS
     C                   MOVE      '26'          CAPMOD
     C                   MOVE      '236'         XPEOMB
     C*** EOMB 236: "Based upon provider's facility, reimbursement is made for
     C***            the professional component"
     C                   END                                                    END EQ BLANKS
     C                   END                                                    END EQ 1/2/B
     C                   END                                                    END GE/LE 95819/9595
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        ANDNE     '27'                                         ADD 10/10/95
     C                   Z-ADD     SXMRA         XXMRA                          ADD 03/03/92
     C                   EXSR      PMOD
     C*** If Mod -26 and Prof Component Flag = A, then it is ANE
     C     CAPMOD        IFEQ      '26'
     C     SXPROF        IFEQ      *ZEROS
     C     SX1A1         IFEQ      'A'
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   ELSE                                                   ELSE EQ 'A'
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    ENDEQ 'A'
     C                   END                                                    END SXPROF EQ *ZEROS
     C                   END                                                    END EQ '26'
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires futher inspection
     C***              by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD                         07/16/91 CHG
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END 'ANE'
     C*
     C*  % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % *
     C*
     C*** IF PROCEDURE IS "N/C" FLAG IT AS SUCH AND UPDATE RECORD      .
     C*
     C     PRRPT         IFEQ      'X'
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Worker's Comp Pgm"
     C                   EXSR      UPDAT
     C*
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END 'NC'
     C*
     C*  % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % *
     C*
     C*** EXECUTE ALL SUBROUTINES HERE.
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C                   EXSR      ANES
     C                   ELSE
     C     DHPRCD        IFGE      'D0110'                                      DENTAL
     C     DHPRCD        ANDLE     'D9999'
     C                   EXSR      DENT
     C                   ELSE
     C     DHPRCD        IFGE      'J0110'                                      INJECTIONS
     C     DHPRCD        ANDLE     'J9999'
     C                   EXSR      INJE
     C                   ELSE
     C     DHPRCD        IFGE      '10000'                                      SURGICAL
     C     DHPRCD        ANDLE     '69999'                                        10020-69646
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALFL
     C                   CLOSE     FACAL1FL
     C                   CLOSE     FACAL2FL                                     ADD 01/25/96
     C                   CLOSE     FACAPFL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACFL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALFL
     C                   OPEN      FACAL1FL
     C                   OPEN      FACAL2FL                                     ADD 01/25/96
     C                   OPEN      FACAPFL
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
     C                   EXSR      PHYS2
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALFL
     C                   CLOSE     FACAL1FL
     C                   CLOSE     FACAL2FL                                     ADD 01/25/96
     C                   CLOSE     FACAPFL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACFL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALFL
     C                   OPEN      FACAL1FL
     C                   OPEN      FACAL2FL                                     ADD 01/25/96
     C                   OPEN      FACAPFL
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END                                                    END HDYMD NE
     C                   END                                                    END XFIRST EQ ' '
     C                   EXSR      SURG
     C                   ELSE
     C     DHPRCD        IFGE      '70010'                                      RADIOLOGICAL
     C     DHPRCD        ANDLE     '78990'
     C                   EXSR      RADI
     C                   ELSE
     C     DHPRCD        IFGE      '80002'                                      PATHOLOGICAL
     C     DHPRCD        ANDLE     '89205'
     C                   EXSR      PATH
     C                   ELSE
     C     DHPRCD        IFGE      '90000'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '90654'
     C     DHPRCD        ORGE      '99201'                                      ADD 03/26/92S
     C     DHPRCD        ANDLE     '99353'                                      ADD 03/26/92
     C                   EXSR      OFFI
     C                   ELSE
     C     DHPRCD        IFGE      '90600'
     C     DHPRCD        ANDLE     '96912'
     C                   EXSR      MEDI
     C                   ELSE
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97752'
     C* We only want to build the Physical Therapy Workfile if we
     C* processing a Physical Therapy Code
     C*
     C* So the Workfile will be built here on the first Phys Ther charge
     C* then rebuilt each time PHYS2 subroutine is process
     C* 01/25/96 Added SURG2 Workfile
     C*
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALFL
     C                   CLOSE     FACAL1FL
     C                   CLOSE     FACAL2FL                                     ADD 01/25/96
     C                   CLOSE     FACAPFL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACFL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALFL
     C                   OPEN      FACAL1FL
     C                   OPEN      FACAL2FL                                     ADD 01/25/96
     C                   OPEN      FACAPFL
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
     C                   EXSR      PHYS2
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALFL
     C                   CLOSE     FACAL1FL
     C                   CLOSE     FACAL2FL                                     ADD 01/25/96
     C                   CLOSE     FACAPFL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACFL'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*     FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALFL
     C                   OPEN      FACAL1FL
     C                   OPEN      FACAL2FL                                     ADD 01/25/96
     C                   OPEN      FACAPFL
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END                                                    END HDYMD NE
     C                   END                                                    END XFIRST EQ ' '
     C                   EXSR      PHYS
     C                   ELSE
     C     DHPRCD        IFGE      '99000'                                      SPECIAL CODES
     C     DHPRCD        ANDLE     '99195'
     C                   EXSR      SPEC
     C                   ELSE
     C     DHPRCD        IFGE      '97801'                                      PHYS RECON CODES
     C     DHPRCD        ANDLE     '97851'
     C                   EXSR      RECON
     C                   ELSE
     C                   EXSR      OTHE                                         OTHER CODES
     C                   END                                                    ADD 09/27/94
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
     C                   END
     C                   END
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        XXDSEQ
     C*
     C     CAKYP3        CHAIN(N)  PCA0F                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXDSEQ        ANDNE     CADSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        ANDNE     '95834'
     C*
     C                   ADD       CAPAID        TOTA1                          KEEP AMT EVEN
     C*                                                    FOR AN OVR TYPE
     C                   END
     C*
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C     CSOVRP        IFGT      *ZERO
     C*** BEGIN CHG *** 09/25/01 **************************************
     C********** CSOVRP    DIV  100       XXOVRP  32
     C     CSOVRP        DIV       100           XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C     CACHRG        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CABRTC                         ADD 08/22/94
     C     CACHRG        SUB       CATOTA        CAMRA
     C                   Z-ADD     CATOTA        CAPAID
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C*
     C     NXTRCD        TAG
     C*
     C*** BEFORE NEXT RECORD IS READ, SAVE THE KEY FIELDS NEEDED TO
     C*** RETRIEVE THE PHYSICAL THERAPY RECORDS TO BE UPDATED IN THE
     C*** PHYS2 ROUTINE WHICH IS EXECUTED ON A PROVIDER LEVEL BREAK...
     C**
     C                   Z-ADD     CADSEQ        PTDSEQ            5 0          SAVE FIELD VALS
     C                   MOVE      CAFEIN        PTFEIN            9 0          FOR RECORD WE
     C                   MOVE      CAMOD         PTMOD             3 0          ARE PROCESSING
     C                   MOVE      CAPRTY        PTPRTY            1
     C                   Z-ADD     CAEOB#        PTEOB#            9 0
     C                   Z-ADD     CACHK#        PTCHK#            7 0
     C*
     C                   ELSE                                                               060
     C                   EXCEPT    ECA01
     C*
     C                   ENDIF                                                  CAFDAT GE 91060
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ            5 0          CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN            9 0
     C                   MOVE      CAMOD         SVMOD             3 0
     C                   MOVE      CAPRTY        SVPRTY            1
     C                   Z-ADD     DHUNTS        SVUNTS            5 2
     C                   MOVEL     DHPRCD        XPCPT4            5
     C                   MOVE      CAPMOD        XPPMOD            2
     C                   Z-ADD     CAEOB#        SVEOB#            9 0
     C                   Z-ADD     CACHK#        SVCHK#            7 0
     C                   END
     C*
     C                   ENDDO                                                  *IN54=OFF
     C*
     C                   EXSR      ONEOFC                                       EDIT FOR ONLY
     C*                                                    ONE OFFC VISIT
     C*                                                    IN A DAY
     C*
     C*
     C*** Execute PHYS2 if a Physical Therapy Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      PHYS2                                        PROCESS THE PHY
     C                   EXSR      SURG2                                        ADD 01/25/96
     C                   MOVE      ' '           XFIRST
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALFL
     C                   CLOSE     FACAL1FL
     C                   CLOSE     FACAL2FL                                     ADD 01/25/96
     C                   CLOSE     FACAPFL
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2FL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPFL
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
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
     *** ANESTHESIA SUBROUTINE CODES 00100 - 01999               A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*** VALID MODIFIERS ARE "22", "23", "47", AND "48".
     C*** VALID MODIFIERS ARE "76"
     C*
     C*** Per Pg. 124, III, B:
     C*** The Anesthesia Reimbursement Allowance (ARA) is calculated by
     C*** formula: ARA=(Basic value + Time Units) * Conversion Factor($26.60)
     C*
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '23'
     C     CAPMOD        ANDNE     '48'
     C     CAPMOD        ANDNE     '47'
     C     CAPMOD        ANDNE     '76'                                          7/26/89 ADD
     C*
     C** ERROR - INVALID MODIFIER ENTERED CLEAR THE CHARGE, SND ERROR
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE
     C*
     C*
     C*** ERROR MESSAGE '01' SHOULD ONLY APPEAR FOR PMOD 22 & 23
     C*** Pg. 128, IV, A, 1
     C     CAPMOD        IFEQ      '22'
     C     CAPMOD        OREQ      '23'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C*** Err Msg 01: "This bill requires further inspection by an adjuster"
     C                   END                                                    ENDEQ 22/23
     C*
     C*** CALCULATE ARA FOR ALL VALID PROC/MODIFIERS
     C*
     C*** Per pg. 124-127, III, A-G
     C*** User will have entered total minutes, we must now convert those
     C*** minutes to TIME VALUE UNITS
     C*
     C*** Determine provider's specialty
     C*** If ne ANE or RNA, then send message, but calculate per ANE
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     QQSPCD        IFNE      'ANE'
     C     QQSPCD        ANDNE     'RNA'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '49'          QQC(Q)
     C*** Err Msg 49: "Reimbursement was determined using the anesthesiologist
     C***              calculations.  Please verify.
     C                   END                                                    END EQ ANE/RNA
     C                   END                                                    END 0 *IN60
     C*
     C*** If eq RNA:
     C     QQSPCD        IFEQ      'RNA'                                        ADD 12/28/92
     C     DHUNTS        DIV       15            HLANMN           10 7
     C                   ELSE
     C     DHUNTS        DIV       10            HLANMN
     C                   END
     C     SXANAM        ADD       HLANMN        XXWORK           10 7
     C     XXWORK        MULT      27.34         XXMRA
     C*
     C*** CALCULATE NORMAL PROC MOD VALUES BASED ON ABOVE XPTOTA
     C                   EXSR      PMOD
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
     C*** UPDATE DETAIL CHARGE RECORD, THEN END SUBROUTINE
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END 22/23/48
     C*
     C                   END                                                    END NE *BLANKS
     C*
     C*** Per pg. 124-127, III, A-G
     C*** User will have entered total minutes, we must now convert those
     C*** minutes to TIME VALUE UNITS
     C*
     C*** Determine provider's specialty
     C*** If ne ANE or RNA, then send message, but calculate per ANE
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     QQSPCD        IFNE      'ANE'
     C     QQSPCD        ANDNE     'RNA'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '49'          QQC(Q)
     C*** Err Msg 49: "Reimbursement was determined using the anesthesiologist
     C***              calculations.  Please verify.
     C                   END                                                    END EQ ANE/RNA
     C                   END                                                    END 0 *IN60
     C*
     C*** If eq RNA:
     C     QQSPCD        IFEQ      'RNA'                                        ADD 12/28/92
     C     DHUNTS        DIV       15            HLANMN           10 7
     C                   ELSE
     C     DHUNTS        DIV       10            HLANMN
     C                   END
     C     SXANAM        ADD       HLANMN        XXWORK           10 7
     C*
     C     XXWORK        MULT      27.34         XXMRA
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
     C                   EXSR      UPDAT
     C*
     C     ANESE         ENDSR
     *********************************************************************
     *** DENTAL SUBROUTINE     CODES D0110 - D9999               D E N T *
     *********************************************************************
     C     DENT          BEGSR
     C*
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              62
     C   62              Z-ADD     XXMRA         XPTOTA
     C  N62              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     DENTE         ENDSR
     C/EJECT
     C********************************************************************
     C** CHECK FOR POSSIBLE DUPLICATE OFFICE VISITS          D U P O F C *
     C********************************************************************
     C     DUPOFC        BEGSR
     C*
     C                   Z-ADD     1             O                 5 0
     C*
     C* Look up table for cross-references office visit code
     C     OFCLOK        TAG
     C     DHPRCD        LOOKUP    OFC1(O)                                54
     C     *IN54         IFEQ      '1'
     C                   MOVE      OFC2(O)       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C*
     C     DUPCL3        TAG
     C     CADSEQ        IFNE      XRDSEQ
     C     CAFEIN        ANDEQ     XRFEIN
     C     XRFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '51'          QQC(Q)
     C*** Err Msg 51: "Possible Duplicate Claim exists for this"
     C***              Office Visit Code"
     C                   END                                                    END EQ X6DSQ
     C                   END                                                    END 0 *IN54
     C*
     C* Because some office visit codes cross-reference two other codes
     C* loop back through this process to check on the 2nd code
     C                   ADD       1             O
     C                   GOTO      OFCLOK
     C                   END                                                    END 1 *IN54
     C*
     C                   ENDSR
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
     C                   MOVE      'A'           QQESRC
     C                   MOVE      CADSEQ        QQDSEQ
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
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB FOR SURGERY CODES              E O M B 4 *
     *********************************************************************
     C     EOMB4         BEGSR
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
     C                   MOVE      'A'           QQESRC
     C                   MOVE      P4DSEQ        QQDSEQ
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
     C                   MOVE      P4CO#         DECO#
     C                   MOVE      P4SUB#        DESUB#
     C                   MOVE      P4FND         DEFND
     C                   MOVE      P4FYR         DEFYR
     C                   MOVE      P4CASE        DECASE
     C                   MOVE      P4SS#         DESS#
     C                   MOVE      P4ACDT        DEACDT
     C                   MOVE      P4EOB#        DEEOB#
     C                   MOVE      P4DSEQ        DEDSEQ
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMB4E        ENDSR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB FOR PHYS CODES            E O M B P T    *
     *********************************************************************
     C     EOMBP2        BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  P2EOMB        EBKEY
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
     C                   MOVE      'A'           QQESRC
     C                   MOVE      P2DSEQ        QQDSEQ
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
     C                   MOVE      P2CO#         DECO#
     C                   MOVE      P2SUB#        DESUB#
     C                   MOVE      P2FND         DEFND
     C                   MOVE      P2FYR         DEFYR
     C                   MOVE      P2CASE        DECASE
     C                   MOVE      P2SS#         DESS#
     C                   MOVE      P2ACDT        DEACDT
     C                   MOVE      P2EOB#        DEEOB#
     C                   MOVE      P2DSEQ        DEDSEQ
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOBP2E        ENDSR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB FOR PHYS CODES            E O M B P X    *
     *********************************************************************
     C     EOMBPX        BEGSR
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
     C                   MOVE      'A'           QQESRC
     C                   MOVE      PXDSEQ        QQDSEQ
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
     C                   MOVE      PXCO#         DECO#
     C                   MOVE      PXSUB#        DESUB#
     C                   MOVE      PXFND         DEFND
     C                   MOVE      PXFYR         DEFYR
     C                   MOVE      PXCASE        DECASE
     C                   MOVE      PXSS#         DESS#
     C                   MOVE      PXACDT        DEACDT
     C                   MOVE      PXEOB#        DEEOB#
     C                   MOVE      PXDSEQ        DEDSEQ
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOBPXE        ENDSR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB FOR PHYS CODES            E O M B P 3    *
     *********************************************************************
     C     EOMBP3        BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  P3EOMB        EBKEY
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
     C                   MOVE      'A'           QQESRC
     C                   MOVE      P3DSEQ        QQDSEQ
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
     C                   MOVE      P3CO#         DECO#
     C                   MOVE      P3SUB#        DESUB#
     C                   MOVE      P3FND         DEFND
     C                   MOVE      P3FYR         DEFYR
     C                   MOVE      P3CASE        DECASE
     C                   MOVE      P3SS#         DESS#
     C                   MOVE      P3ACDT        DEACDT
     C                   MOVE      P3EOB#        DEEOB#
     C                   MOVE      P3DSEQ        DEDSEQ
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOBPTE        ENDSR
     *********************************************************************
     *** INJECTIONS SUBROUTINE CODES J0120 - J7120               I N J E *
     *********************************************************************
     C     INJE          BEGSR
     C*
     C*** VALID MODIFIERS ARE "51".
     C*** VALID MODIFIERS ARE "76"
     C*
     C*** REIMBURSMENT FOR MULTIPLE DRUGS FROM SAME SYRINGE; 1ST AT
     C*** MRA (OR CHARGE WHICH EVER IS LESS) 2ND AND SUBSEQUENT REQUIRE
     C*** MODIFIER 51 AND WILL PAID AT 50% OF MRA OR CHARGE WHICH EVER IS
     C*** LESS.
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '51'
     C     CAPMOD        ANDNE     '76'                                          7/26/89 ADD
     C*
     C** ERROR - INVALID MODIFIER ENTERED CLEAR THE CHARGE, SND ERROR
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      INJEE
     C*
     C                   ELSE
     C*
     C     XXMRA         DIV(H)    2.00          XPTOTA
     C*
     C                   Z-ADD     XPTOTA        XPBRTC                          ADD 08/22/94
     C     CACHRG        IFLT      XPTOTA
     C                   Z-ADD     CACHRG        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      INJEE
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 600 VII, B, 1, c
     C*** When a starred procedure is performed at the time of "J" codes...
     C*** "The injectable therapeutic medications may be separately billed
     C*** using Procedure Code 99070"
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C     NXTINJ        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSTAT        CABEQ     'I'           NXTINJ
     C     PRSUPP        IFEQ      'Y'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '276'         XPEOMB
     C*** EOMB 276: "Invoice required for payment"
     C                   EXSR      UPDAT
     C                   GOTO      INJEE
     C                   END                                                     END EQ 'Y'
     C*
     C                   END                                                     END 0 *IN54
     C*
     C                   ENDIF                                                   XRCPT4 GE
     C*
     C     CAKEY7        READE(N)  PCA09                                  54     ADD 08/11/93
     C*
     C                   END                                                     END GE 10020...
     C*
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     INJEE         ENDSR
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
     *** MEDICINE PROCEDURES SUBROUTINE CODES 90620 - 96912      M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Per pg. 61, VIII, D, 2, c
     C*** A copy of a consultation report must be submitted with the bill in
     C*** order for reimbursement to be made.
     C*
     C     DHPRCD        IFEQ      '90600'
     C     DHPRCD        OREQ      '90605'
     C     DHPRCD        OREQ      '90610'
     C     DHPRCD        OREQ      '90620'
     C     DHPRCD        OREQ      '90630'
     C     DHPRCD        OREQ      '90640'
     C     DHPRCD        OREQ      '90641'
     C     DHPRCD        OREQ      '90642'
     C     DHPRCD        OREQ      '90643'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '237'         XPEOMB
     C*** EOMB 237: "Consultation report required for payment"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '47'          QQC(Q)
     C*** Err Msg 47: "Copy of consultation report required for
     C***              reimbursement"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END EQ Consultation
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 59, VII, A, 2, d (for 90701-90749)
     C*** The CPT Immunization Procdure Codes (90701-90749) must not be
     C*** reimbursed under this program.
     C*
     C     DHPRCD        IFGE      '90701'
     C     DHPRCD        ANDLE     '90749'
     C     DHPRCD        OREQ      '90782'
     C     DHPRCD        OREQ      '90788'
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '239'         XPEOMB
     C*** EOMB 239: "These services not reimbursable under the Fee Schedule"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 84, XV, A, 4, g
     C*** Certain radiology studies have procedure codes which list complete
     C*** body areas, as well as the individual sections of body areas,
     C*** as separate entities.  When x-rays of multiple sections of a body area
     C*** are billed separately, the total reimbursement must not exceed the
     C*** provider's charge or the MRA of the compelte body area study,
     C*** whichever is less.
     C*
     C                   Z-ADD     1             R                 5 0
     C     MEDLOK        TAG
     C     DHPRCD        LOOKUP    RAD1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      RAD2(R)       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C*
     C     R             IFLT      95
     C                   ADD       1             R
     C                   GOTO      MEDLOK
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 77, XIII, F, 2
     C*** Per pg. 113, II, E, 2
     C*** When two or more procedures from 95831-95852 are performed on the
     C*** same day, reimbursement may not exceed the MRA for Procedure Code
     C*** 95834, total evaluation of the body, including hands.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        ANDNE     '95834'
     C                   MOVE      '95834'       XXCPT4
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '17'          QQC(Q)
     C*** Err Msg 17: "The total of codes 95831-95852 cant exceed MRA
     C***              for code 95834"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C                   END
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 64-65, IX, B, 1, d, 2
     C*** Per pg. 77, XIII, F, 2
     C*** Per pg. 113, II, E, 2
     C*** When two or more procedures from 95831-95852 are performed on the
     C*** same day, reimbursement may not exceed the MRA for Procedure Code
     C*** 95834, total evaluation of the body, including hands.
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
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C                   ADD       XXMRA         TOTA1
     C*
     C     TOTA1         IFGT      MRA1
     C*
     C                   SUB       XXMRA         TOTA1
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '17'          QQC(Q)
     C*** Err Msg 17: "The total of codes 95831-95852 cant exceed MRA
     C***              for code 95834"
     C*
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 55, IV, D
     C*** Per pg. 107, I, C
     C*** Reimbursement for biofeedback training is limited to 12 visits.
     C*
     C     DHPRCD        IFGE      '90900'
     C     DHPRCD        ANDLE     '90915'
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         HLFDAT
     C                   Z-ADD     *ZERO         H2FDAT
     C*
     C     CAKYC2        CHAIN     PCA0A                              54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XACASE        XXCASE
     C                   Z-ADD     XAEOB#        XXEOB#
     C                   Z-ADD     XADSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XACPT4
     C*
     C     XACPT4        IFGE      '90900'
     C     XACPT4        ANDLE     '90915'
     C     XAFDAT        ANDNE     HLFDAT
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C                   Z-ADD     XAFDAT        HLFDAT
     C* Need to allow 2nd+ charge on the 21st day
     C     COUNT         IFEQ      12
     C                   Z-ADD     XAFDAT        H2FDAT
     C                   END                                                    END COUNT EQ 21
     C                   END                                                    END NE HLFDAT
     C*
     C     CAKYC2        READE     PCA0A                                  54
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 89, XVIII, B, 4
     C*** Thermographic tests must not be authorized unless the date of service
     C*** is at least 45 days after the date of accident...
     C*
     C     DHPRCD        IFEQ      '93760'
     C     DHPRCD        OREQ      '93762'
     C*
     C*
     C                   Z-ADD     QQACDT        SRDATE
     C                   Z-ADD     45            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKACDT            7 0
     C     CAFDAT        IFLE      WKACDT
     C** DISALLOW
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Workers Comp Program"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C*** Per pg. 90, XVIII, E, 1
     C*** Reimbursement is limited to ONE body area, either major or limited
     C*** unless an additional area(s) is medically necessary and documentation
     C*** of medical necessity is submitted to the carrier
     C*
     C                   Z-ADD     0             COUNT             5 0
     C*
     C     CAKYC2        CHAIN     PCA0A                              54
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XACASE        XXCASE
     C                   Z-ADD     XAEOB#        XXEOB#
     C                   Z-ADD     XADSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XACPT4
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
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        CACHRG
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END EQ 93760/93762
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 63, IX, B, 1, a, 1
     C*** When a visit/consultation is made for the purpose of an assessment
     C*** and evaluation of a patient, the visit/consultation may be reimbursed
     C*** at the appropriate level of service.  Extremity, muscle and ROM tests
     C*** and measurements performed during the visit must not be reimbursed
     C*** as separate entities.  As these tests are an integral part of the
     C*** visit/consultation, reimbursement for these tests and measurements
     C*** is indluded in the reimbursement for the visit/consultation.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C*
     C                   MOVE      '90000'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '90630'
     C*
     C*** DISALLOW THIS TESTING CODE.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '99353'
     C*
     C*** DISALLOW THIS TESTING CODE.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 110, II, C, 2, c
     C*** Assessment of the patient's status includes assessment of the
     C*** neuromuscular system.  Therefore, reimbursement must not be made
     C*** for neuromuscular testing codes, extremity testing codes and/or
     C*** range of motion codes in addition to 97700.  These codes may not be
     C*** used instead of 97700 when an initial assessment is performed.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95950'
     C*
     C                   MOVE      '97700'       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** DISALLOW THIS TESTING CODE.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 64, IX, B, 1, d, 1
     C*** Per pg. 77, XIII, F, 1
     C*** Reimbursement for extremity testing, muscle testing and range of
     C*** motion measurements may be made only once in a 30-day period for
     C*** the same body area.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C*
     C                   Z-ADD     30            SRPST             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             PASTCT            5 0
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFEQ      DHPRCD
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             PASTCT
     C                   END
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
     C                   END
     C*
     C                   Z-ADD     30            SRFUT             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM            1
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             FUTRCT            5 0
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFEQ      DHPRCD
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             FUTRCT
     C                   END
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 65, IX, B, 2, a
     C*** Reimbursement is limited to two EMGs in a 90-day period.
     C*
     C     DHPRCD        IFGE      '95860'
     C     DHPRCD        ANDLE     '95869'
     C*
     C                   Z-ADD     90            SRPST             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C     CAKYC2        READE     PCA0C                                  54
     C                   Z-ADD     0             PASTCT
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFLE      '95869'
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             PASTCT
     C                   END
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
     C                   END
     C*
     C                   Z-ADD     90            SRFUT             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM            1
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             FUTRCT            5 0
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFEQ      DHPRCD
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             FUTRCT
     C                   END
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
     C                   END
     C*
     C     PASTCT        IFGE      2
     C     FUTRCT        ORGE      2
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '239'         XPEOMB
     C*** EOMB 239: "These services not reimbursable under the Fee Schedule"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 65, IX, B, 3, c
     C*** Reimbursement for nerve conduction studies is limited to two studies
     C*** in a 90-day period.
     C*
     C     DHPRCD        IFGE      '95900'
     C     DHPRCD        ANDLE     '95904'
     C*
     C                   Z-ADD     90            SRPST             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '95900'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  54
     C                   Z-ADD     0             COUNT
     C                   MOVE      ' '           PDT1              1
     C                   MOVE      ' '           PDT2              1
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFLE      '95904'
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C*
     C     PDT1          IFEQ      *BLANK
     C                   MOVE      '1'           PDT1              1
     C                   MOVE      XCFDAT        SVDT              7 0
     C                   ELSE
     C     XCFDAT        IFNE      SVDT
     C                   MOVE      '1'           PDT2              1
     C                   END
     C                   END
     C                   END
     C***
     C                   END
     C     CAKYC         READE     PCA0C                                  54
     C                   END
     C*
     C                   Z-ADD     90            SRFUT             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM            1
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '95900'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  54
     C                   MOVE      ' '           FDT1              1
     C                   MOVE      ' '           FDT2              1
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFLE      '95904'
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C*
     C     FDT1          IFEQ      *BLANK
     C                   MOVE      '1'           FDT1              1
     C                   MOVE      XCFDAT        SVDT
     C                   ELSE
     C     XCFDAT        IFNE      SVDT
     C                   MOVE      '1'           FDT2              1
     C                   END
     C                   END
     C                   END
     C                   END
     C     CAKYC         READE     PCA0C                                  54
     C                   END
     C*
     C     PDT2          IFEQ      '1'
     C     FDT2          OREQ      '1'
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '239'         XPEOMB
     C*** EOMB 239: "These services not reimbursable under the Fee Schedule"
     C**** ADD CODE TO DISPLAY ERROR MESSAGE ** IF MORE THAN 2 EXIST IN
     C*** DAYS, "THESE CODES MIGHT BE ALLOWED IN CERTAIN SITUATIONS.",
     C*** "SEE ADJUSTOR..." ALSO.
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '26'          QQC(Q)
     C*** Err Msg 26: "Only 2 nerve studies are allowed in a 90-Day Period
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 96, II, E, 3, b
     C*** When a starred procedure is perfromed at the time of the initial
     C*** visit, and this procedure constitutes the major service rendered
     C*** during the intial visit, Procedure Code 99025 must be used instead
     C*** of the usual intial office visit code.
     C*
     C     DHPRCD        IFEQ      '92002'
     C     DHPRCD        OREQ      '92004'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN4        TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
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
     C                   MOVE      '99025'       XXCPT4
     C                   MOVE      '216'         XPEOMB
     C*** EOMB 216: "Payment is made for code 99025 instead of O/V per Fee Sche"
     C*
     C     PRKEY         SETGT     PPR01
     C     NXTPR4        TAG
     C*   NEED TO GET MRA FOR 99025 - CPT4 CODE WAS CHANGED ...
     C*
     C                   READP     PPR01                                  54
     C*
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR4
     C                   ELSE
     C                   Z-ADD     *ZERO         SXMRA             9 2
     C                   Z-ADD     *ZERO         SXANAM            9 2
     C                   Z-ADD     *ZERO         SXPROF            9 2
     C                   MOVE      *BLANK        SXRPT             1
     C                   MOVE      *BLANK        SXSUPP            1
     C                   MOVE      *BLANK        SXSURG            1
     C                   MOVE      *BLANK        SX1A1                          11/04/91 ADD
     C                   END
     C*
     C** SAVE THE PROCEDURE CODE FILE VALUES.
     C*
     C                   Z-ADD     PRMRA         SXMRA             9 2
     C                   Z-ADD     PRANAM        SXANAM            9 2
     C                   Z-ADD     PRPROF        SXPROF            9 2
     C                   MOVE      PRRPT         SXRPT             1
     C                   MOVE      PRSUPP        SXSUPP            1
     C                   MOVE      PRSURG        SXSURG            1
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
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 254
     C*** CHECK FOR MULTIPLE 95925 (ONLY ONE 95925 ALLOWED ON SAME DAY)
     C*** DISALLOW 2ND AND SUBSEQUENT CODES IN SAME DAY
     C*
     C     DHPRCD        IFEQ      '95925'
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      CAPMOD        XXPMOD
     C                   Z-ADD     0             COUNT             5 0
     C*
     C     CAKY8         CHAIN     PCA06                              54
     C     *IN54         DOWEQ     '0'
     C*
     C     CADSEQ        IFNE      X6DSEQ
     C     X6TOTP        ANDGT     *ZERO
     C                   ADD       1             COUNT
     C                   END                                                    END NE/EQ/NE
     C*
     C     CAKY8         READE     PCA06                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*** DISSALLOW CHARGES
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        CACHRG
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END EQ 95925
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 55, V, 2, a
     C*** The pharmaceutical reimbursement formula for prescribed drugs
     C*** (medicines) by dispensing practitioners is:
     C*** Average Wholesale Price (AWP) x 1.2 + $4.18 = Reimbursement
     C*
     C     DHPRCD        IFEQ      '96370'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '45'          QQC(Q)
     C*** Err Msg 45: "Reimbursement = AWP x 1.2 + $4.18
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END EQ 96370
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 66, IX, B, 4
     C*** An F-wave is considered a part of the study of a nerve.
     C*** Reimbursement must not be made separately from the nerve study
     C*** procedure(s).
     C*
     C     DHPRCD        IFEQ      '95900'
     C     DHPRCD        OREQ      '95904'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '48'          QQC(Q)
     C*** Err Msg 48: "An F-wave is considered part of the study of a nerve.
     C***              Please verify reimbursement"
     C*
     C                   END                                                    END EQ 95900/95904
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 63, IX, A, 6
     C*** When the diagnostic services are provided at a hospital or ASC,
     C*** the hospital or ASC bills for the technical services and the
     C*** physician bills for the professional component only, using Mod -26
     C*
     C     DHPRCD        IFGE      '95819'
     C     DHPRCD        ANDLE     '95950'                                      07/16/91 CHG
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      'B'
     C     CAPMOD        IFEQ      *BLANKS
     C                   MOVE      '26'          CAPMOD
     C                   MOVE      '236'         XPEOMB
     C*** EOMB 236: "Based upon provider's facility, reimbursement is made for
     C***            the professional component"
     C                   END                                                    END EQ BLANKS
     C                   END                                                    END EQ 1/2/B
     C                   END                                                    END GE 95819-95950
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     MEDIE         ENDSR
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
     C                   MOVE(P)   QQS(N)        XXDSEQ
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
     C                   Z-ADD     XXDSEQ        MGDSEQ
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
     C                   Z-ADD     CADSEQ        HLDSEQ            5 0          SAVE FIELD VALS
     C                   MOVE      CAFEIN        HLFEIN            9 0          FOR RECORD WE
     C                   MOVE      CAMOD         HLMOD             3 0          ARE PROCESSING
     C                   MOVE      CAPRTY        HLPRTY            1
     C                   Z-ADD     CAEOB#        HLEOB#            9 0
     C                   Z-ADD     CACHK#        HLCHK#            7 0
     C*
     C                   EXSR      ONEOFC
     C*
     C*
     C*
     C                   Z-ADD     HLDSEQ        CADSEQ            5 0          SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN            9 0          FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD             3 0          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY            1
     C                   Z-ADD     HLEOB#        CAEOB#            9 0
     C                   Z-ADD     HLCHK#        CACHK#            7 0
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ            5 0
     C                   MOVE      CAFEIN        SVFEIN            9 0             CURR REC.
     C                   MOVE      CAMOD         SVMOD             3 0
     C                   MOVE      CAPRTY        SVPRTY            1
     C                   Z-ADD     CAEOB#        SVEOB#            9 0
     C                   Z-ADD     CACHK#        SVCHK#            7 0
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C                   EXSR      LODFU
     C*
     C                   Z-ADD     0             TOTA1
     C     NEWPRE        ENDSR
     *********************************************************************
     *** OFFICE VISITS     SUBROUTINE CODES 90000 - 90654        O F F I *
     ***                                    99201 - 99353        O F F I *
     *********************************************************************
     C*
     C     OFFI          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Per State and CPT4 Manual:
     C*** If old Office Visit code (90000-90643), give EOMB Message
     C     DHPRCD        IFGE      '90000'
     C     DHPRCD        ANDLE     '90643'
     C                   MOVE      '275'         XPEOMB
     C*** EOMB 275: "This charge has been paid per the new CPT4 Office
     C***                 Visit Code"
     C                   END                                                    END GE 90000...
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Per Sylvia/State and CPT4 Manual:
     C*** Codes 90530, 90540, 90550, 90560, 90570 have been deleted from the
     C*** the CPT4 manual.  Do not pay if after 02/03/92 with EOMB Code 230
     C     DHPRCD        IFEQ      '90530'
     C     DHPRCD        OREQ      '90540'
     C     DHPRCD        OREQ      '90550'
     C     DHPRCD        OREQ      '90560'
     C     DHPRCD        OREQ      '90570'
     C     CAFDAT        IFGE      920203
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '230'         XPEOMB
     C*** EOMB 230: "No longer valid CPT4 Code. To report, see 99271-99275"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END GE 920203
     C                   END                                                    END EQ 90530...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Per pg. 61, VIII, D, 2, c
     C*** A copy of a consultation report must be submitted with the bill in
     C*** order for reimbursement to be made.
     C*
     C     DHPRCD        IFEQ      '90600'
     C     DHPRCD        OREQ      '90605'
     C     DHPRCD        OREQ      '90610'
     C     DHPRCD        OREQ      '90620'
     C     DHPRCD        OREQ      '90630'
     C     DHPRCD        OREQ      '90640'
     C     DHPRCD        OREQ      '90641'
     C     DHPRCD        OREQ      '90642'
     C     DHPRCD        OREQ      '90643'
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
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '237'         XPEOMB
     C*** EOMB 237: "Consultation report required for payment"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 96, II, E, 3, b
     C*** When a starred procdures is performed at the time of the initial
     C*** office visits, and this procedure constitutes the major service
     C*** rendered during the initial visit, Procedure Code 99025 must be
     C*** used instead of the usual initial office visit code.
     C*
     C     DHPRCD        IFEQ      '90000'
     C     DHPRCD        OREQ      '90010'
     C     DHPRCD        OREQ      '90015'
     C     DHPRCD        OREQ      '90017'
     C     DHPRCD        OREQ      '90020'
     C     DHPRCD        OREQ      '90100'
     C     DHPRCD        OREQ      '90110'
     C     DHPRCD        OREQ      '90115'
     C     DHPRCD        OREQ      '90200'
     C     DHPRCD        OREQ      '90215'
     C     DHPRCD        OREQ      '90220'
     C     DHPRCD        OREQ      '90300'
     C     DHPRCD        OREQ      '90315'
     C     DHPRCD        OREQ      '90320'
     C     DHPRCD        OREQ      '90400'
     C     DHPRCD        OREQ      '90415'
     C     DHPRCD        OREQ      '90420'
     C     DHPRCD        OREQ      '90500'
     C     DHPRCD        OREQ      '90505'
     C     DHPRCD        OREQ      '90510'
     C     DHPRCD        OREQ      '90515'
     C     DHPRCD        OREQ      '90517'
     C     DHPRCD        OREQ      '90620'
     C     DHPRCD        OREQ      '90630'
     C     DHPRCD        OREQ      '99201'
     C     DHPRCD        OREQ      '99202'
     C     DHPRCD        OREQ      '99203'
     C     DHPRCD        OREQ      '99204'
     C     DHPRCD        OREQ      '99205'
     C     DHPRCD        OREQ      '99341'
     C     DHPRCD        OREQ      '99342'
     C     DHPRCD        OREQ      '99343'
     C     DHPRCD        OREQ      '99221'
     C     DHPRCD        OREQ      '99222'
     C     DHPRCD        OREQ      '99223'
     C     DHPRCD        OREQ      '99301'
     C     DHPRCD        OREQ      '99302'
     C     DHPRCD        OREQ      '99303'
     C     DHPRCD        OREQ      '99321'
     C     DHPRCD        OREQ      '99322'
     C     DHPRCD        OREQ      '99323'
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C     DHPRCD        OREQ      '99244'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99254'
     C     DHPRCD        OREQ      '99255'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN         TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
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
     C                   MOVE      '99025'       XXCPT4
     C                   MOVE      '216'         XPEOMB
     C*** EOMB 216: "Payment is made for code 99025 instead of O/V per Fee Sche"
     C     PRKEY         SETGT     PPR01
     C     NXTPRB        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPRB
     C                   ELSE
     C                   Z-ADD     *ZERO         SXMRA             9 2
     C                   Z-ADD     *ZERO         SXANAM            9 2
     C                   Z-ADD     *ZERO         SXPROF            9 2
     C                   MOVE      *BLANK        SXRPT             1
     C                   MOVE      *BLANK        SXSUPP            1
     C                   MOVE      *BLANK        SXSURG            1
     C                   MOVE      *BLANK        SX1A1                          11/04/91 ADD
     C                   END
     C*
     C** SAVE THE PROCEDURE CODE FILE VALUES.
     C*
     C                   Z-ADD     PRMRA         SXMRA             9 2
     C                   Z-ADD     PRANAM        SXANAM            9 2
     C                   Z-ADD     PRPROF        SXPROF            9 2
     C                   MOVE      PRRPT         SXRPT             1
     C                   MOVE      PRSUPP        SXSUPP            1
     C                   MOVE      PRSURG        SXSURG            1
     C                   MOVE      PR1A1         SX1A1                          11/04/91 ADD
     C*
     C*
     C                   ELSE
     C                   GOTO      AGAIN
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 64, IX, B, 1, a, 2
     C*** When an office visit/consultation is made solely for the purpose
     C*** of performing tests and measurements, these testing procedures
     C*** may be reimbursed as separate entities.  Reimbursement must not be
     C*** made for a visit in addition to the test(s).
     C*
     C     DHPRCD        IFGE      '90000'
     C     DHPRCD        ANDLE     '90630'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99353'
     C*
     C                   MOVE      '97720'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '97752'
     C*
     C*** DISALLOW THIS OFFICE VISIT
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 67, X, A, 2
     C*** When using the CPT ophthalmology procedure codes, providers must
     C*** adhere to the specific CPT guidelines for these services, e.g.,
     C*** reporting of office services and definitions.
     C*   (OPHTHALMOLOGICAL SERVICES USE THEIR OWN OFFICE VISIT CODES)
     C*** IF CODES 92002 THRU 92014, OFFICE VISITS ARE INCLUDED, DISALLOW
     C*** AN OFFICE VISIT CODE ON SAME DAY.
     C*
     C     DHPRCD        IFGE      '90000'
     C     DHPRCD        ANDLE     '90630'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99353'
     C*
     C                   MOVE      '92002'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '92014'
     C*
     C*** DISALLOW THIS OFFICE VISIT
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 93, II, A, 1, d
     C*** The number in that column (FU) establishes the days during which no
     C*** additional reimbursement is allowed for the usual care provided
     C*** following surgery, absent complications or unusual circumstances.
     C*
     C*** PROCEDURE CODES 90000-90570 AND 90640-90643 REDUCE TO ZERO IF THE
     C*** VISIT FALLS WITHIN THE FOLLOW-UP DAYS OF A PREVIOUS PROCEDURE.
     C*** FOLLOW-UP VISITS ONLY.
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
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
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 87-88, XVI, B, 4
     C*** Per pg. 103, III, I, 2, d
     C*** Follow-up visit charges may be reimbursed in addition to replacement
     C*** casting and strapping procedures only when significant, identifiable,
     C*** further medical services are provided.
     C*
     C*** DISSALLOW FOLLOW UP VISIT IF CAST CODES 29000-29799 ARE PRESENT
     C*** THE SAME DAY.
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C*
     C* SETLL AND READ THE FILE TESTING FOR A 29000-29799 PROCEDURE.
     C*
     C                   MOVE      '29000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '29799'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 65, IX, B, 2, d
     C*** When an EMG is performed on the same day as a follow-up (established
     C*** patient) visit or consultation, reimbursement may be made for the
     C*** visit only when documentation of medical necessity substantiates
     C*** the need for the visit services in addition to the EMG.
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C*
     C* SETLL AND READ THE FILE TESTING FOR A 95860-95869 PROCEDURE.
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '95869'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 53, I, C:
     C*** Acupuncture must be billed under Procedure Code 96300, a unique
     C*** Workers' Compensation code listed in Chapter 6.
     C*
     C*** DISSALLOW THE FOLLOW-UP OFFICE VISIT OR CONSULTATION ON SAME DAY
     C*** AS ACUPUNCTURE.
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C*
     C* CHAIN FOR A 96300 ACUPUNCTURE CODE.
     C*
     C                   MOVE      '96300'       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 77, XIII, E, 1, a-d:
     C*** A follow-up office visit for a re-examination of an established
     C*** patient may be reimbursed in addition to physical medicine only when:
     C*** a. There is a definitive change in  the patient's condition.
     C*** b. The patient fails to respond to treatment.
     C*** c. The patient reaches maximum medical improvement (MMI) or is ready
     C***    for discharge.
     C*** d. It is medically necessary to provide evaluation services over and
     C***    above the evaluative services normally performed during physical
     C***    medicine treatments.
     C*
     C*** DISSALLOW THE FOLLOW-UP OFFICE VISIT OR CONSULTATION ON SAME DAY
     C*** AS CODES 97010-97752 (PHYS THERAPY
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C*
     C* SETLL AND READ THE FILE TESTING FOR A 97010-97752 PROCEDURE.
     C*
     C                   MOVE      '97010'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '97752'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 66, IX, B, 3, f
     C*** When a nerve conduction study is performed on the same day as a
     C*** follow-up (established patient) visit /consultation, reimbursement
     C*** for the visit/consultation may be made only when documentation
     C*** of medical necessity substantiates the need for the visit services
     C*** in addition to the nerve conduction study.
     C*
     C     DHPRCD        IFGE      '90030'
     C     DHPRCD        ANDLE     '90080'
     C     DHPRCD        ORGE      '90130'
     C     DHPRCD        ANDLE     '90170'
     C     DHPRCD        ORGE      '90240'
     C     DHPRCD        ANDLE     '90280'
     C     DHPRCD        ORGE      '90340'
     C     DHPRCD        ANDLE     '90370'
     C     DHPRCD        ORGE      '90440'
     C     DHPRCD        ANDLE     '90470'
     C     DHPRCD        ORGE      '90530'
     C     DHPRCD        ANDLE     '90570'
     C     DHPRCD        ORGE      '90640'
     C     DHPRCD        ANDLE     '90643'
     C     DHPRCD        ORGE      '99211'
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
     C*
     C* SETLL AND READ THE FILE TESTING FOR A 95900-95904 PROCEDURE.
     C*
     C                   MOVE      '95900'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '95904'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 62, VIII, E
     C*** Reimbursement must not be made for Hospital Discharge Day Management
     C*** Code 90292 in addition to another hospital visit billed by the same
     C*** physician on the same day for the same patient.
     C*
     C     DHPRCD        IFEQ      '90292'
     C     DHPRCD        OREQ      '99238'                                      ADD 03/26/92
     C*
     C* SETLL AND READ THE FILE TESTING FOR A 90200-90280 PROCEDURE.
     C*
     C                   MOVE      '90200'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN3        TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C  N54XRCPT4        IFGE      '90200'
     C     XRCPT4        ANDLE     '90280'
     C     XRCPT4        ORGE      '99221'
     C     XRCPT4        ANDLE     '99233'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN3
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 92, II, 1, a
     C*** Global reimbursement includes: the immediate preoperative care
     C*** including the INITIAL VISIT in the hospital or elsewhere to examine
     C*** the patient, complete hospital records and initiation of the
     C*** treatment program.
     C*
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     UGAIN         TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C  N54XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLT     '29010'
     C     XRCPT4        ORGT      '29580'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C*
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'N'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB            3
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   ELSE
     C*
     C                   GOTO      UGAIN
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*
     C*** Per pg. 96, II, E, 3, d
     C*** When a patient is scheduled to have a starred
     C*** procedure performed at the time of a follow-up office visit,
     C*** reimbursement is limited to the provider's usual charge or the MRA for
     C*** the starred procedure, whichever is less.
     C*
     C     DHPRCD        IFEQ      '90030'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90040'                                      CHG 01/21/93
     C     DHPRCD        OREQ      '90050'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90060'
     C     DHPRCD        OREQ      '90070'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90080'
     C     DHPRCD        OREQ      '90130'
     C     DHPRCD        OREQ      '90140'
     C     DHPRCD        OREQ      '90150'
     C     DHPRCD        OREQ      '90160'
     C     DHPRCD        OREQ      '90170'
     C     DHPRCD        OREQ      '90240'
     C     DHPRCD        OREQ      '90250'
     C     DHPRCD        OREQ      '90260'
     C     DHPRCD        OREQ      '90270'
     C     DHPRCD        OREQ      '90280'
     C     DHPRCD        OREQ      '90292'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90340'
     C     DHPRCD        OREQ      '90350'
     C     DHPRCD        OREQ      '90360'
     C     DHPRCD        OREQ      '90370'
     C     DHPRCD        OREQ      '90440'
     C     DHPRCD        OREQ      '90450'
     C     DHPRCD        OREQ      '90460'
     C     DHPRCD        OREQ      '90470'
     C     DHPRCD        OREQ      '90500'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90505'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90510'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90515'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90517'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90600'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90605'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90610'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90620'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90630'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '90801'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '92012'
     C     DHPRCD        OREQ      '92014'
     C     DHPRCD        OREQ      '99211'
     C     DHPRCD        OREQ      '99212'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99213'
     C     DHPRCD        OREQ      '99214'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99215'
     C     DHPRCD        OREQ      '99231'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99232'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99233'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99238'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99241'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99242'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99243'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99244'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99245'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99251'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99252'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99253'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99254'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99255'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99281'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99282'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99283'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99284'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99285'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99351'
     C     DHPRCD        OREQ      '99352'
     C     DHPRCD        OREQ      '99353'
     C     DHPRCD        OREQ      '99231'
     C     DHPRCD        OREQ      '99232'
     C     DHPRCD        OREQ      '99233'
     C     DHPRCD        OREQ      '99284'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99285'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99311'
     C     DHPRCD        OREQ      '99312'
     C     DHPRCD        OREQ      '99313'
     C     DHPRCD        OREQ      '99331'                                      ADD 01/21/93
     C     DHPRCD        OREQ      '99332'
     C     DHPRCD        OREQ      '99333'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN1        TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
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
     C                   Z-ADD     1             Q                 3 0
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB            3
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN1
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** ONLY ALLOW ONE OFFICE VISIT IN A DAY                O N E O F C *
     *********************************************************************
MG3  C*
     C     ONEOFC        BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 61, VIII, B
     C*** Reimbursement may be made only for one visit per physician per
     C*** patient per day at the highest level of care provided.
     C*
     C** IF THE PROCEDURE CODE IS 90000 TO 90630,CHECK FOR OTHERS.
     C*
     C                   MOVE      '90000'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  64
     C*
     C                   MOVE      *ZERO         XGDSEQ            5 0
     C                   MOVE      *ZERO         XGMRA             9 2
     C*
     C     *IN64         DOWEQ     '0'
     C     XRCPT4        ANDLE     '90630'
     C*
     C                   MOVE      XRCPT4        XXCPT4                         ADD 11/04/91
     C                   MOVE      XRFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C*
     C     XGDSEQ        IFNE      *ZERO
     C                   MOVE      '1'           OFCPD             1
     C                   END
     C*
     C     PRMRA         IFGT      XGMRA
     C                   MOVE      XRDSEQ        XGDSEQ
     C                   MOVE      PRMRA         XGMRA
     C                   MOVE      XREOB#        XGEOB#            9 0
     C                   MOVE      XRCHK#        XGCHK#            7 0
     C                   END
     C*
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  64
     C                   END
     C*
     C     OFCPD         IFEQ      '1'
     C                   MOVE      '90000'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  64
     C*
     C     *IN64         DOWEQ     '0'
     C     XRCPT4        ANDLE     '90630'
     C*
     C     XRDSEQ        IFEQ      XGDSEQ
     C*
     C     XRTOTC        IFLT      XGMRA
 |   C                   Z-ADD     XRTOTC        XRTOTA
     C                   ELSE
 |   C                   Z-ADD     XGMRA         XRTOTA
     C                   END
     C*
 |   C     XRTOTC        SUB       XRTOTA        XRTOTD
 |   C                   Z-ADD     XRTOTA        XRTOTP
     C     XRTOTD        IFGT      *ZERO
     C     XREOMB        ANDEQ     *BLANKS                                      11/29/90 ADD
     C                   MOVE      '002'         XREOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C                   ELSE
 |   C                   Z-ADD     *ZERO         XRTOTA
 |   C                   Z-ADD     XRTOTC        XRTOTD
 |   C                   Z-ADD     *ZERO         XRTOTP
 |   C                   MOVE      '006'         XREOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA09
     C*
     C     CAKEY7        READE     PCA09                                  64
     C                   END
     C*
     C  N64              EXCEPT    ECA09
MG7  C                   END
     C*
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  64
     C*
     C                   MOVE      *ZERO         XGDSEQ            5 0
     C                   MOVE      *ZERO         XGMRA             9 2
     C*
     C     *IN64         DOWEQ     '0'
     C     XRCPT4        ANDLE     '99353'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C*
     C     XGDSEQ        IFNE      *ZERO
     C                   MOVE      '1'           OFCPD             1
     C                   END
     C*
     C     PRMRA         IFGT      XGMRA
     C                   MOVE      XRDSEQ        XGDSEQ
     C                   MOVE      PRMRA         XGMRA
     C                   MOVE      XREOB#        XGEOB#            9 0
     C                   MOVE      XRCHK#        XGCHK#            7 0
     C                   END
     C*
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  64
     C                   END
     C*
     C     OFCPD         IFEQ      '1'
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  64
     C*
     C     *IN64         DOWEQ     '0'
     C     XRCPT4        ANDLE     '99353'
     C*
     C     XRDSEQ        IFEQ      XGDSEQ
     C*
     C     XRTOTC        IFLT      XGMRA
 |   C                   Z-ADD     XRTOTC        XRTOTA
     C                   ELSE
 |   C                   Z-ADD     XGMRA         XRTOTA
     C                   END
     C*
 |   C     XRTOTC        SUB       XRTOTA        XRTOTD
 |   C                   Z-ADD     XRTOTA        XRTOTP
     C     XRTOTD        IFGT      *ZERO
     C     XREOMB        ANDEQ     *BLANKS
     C                   MOVE      '002'         XREOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C                   ELSE
 |   C                   Z-ADD     *ZERO         XRTOTA
 |   C                   Z-ADD     XRTOTC        XRTOTD
 |   C                   Z-ADD     *ZERO         XRTOTP
 |   C                   MOVE      '006'         XREOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA09
     C*
     C     CAKEY7        READE     PCA09                                  64
     C                   END
     C*
     C  N64              EXCEPT    ECA09
MG7  C                   END
     C*
     C     ONEOFE        ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     *** PATHOLOGY AND LAB SUBROUTINE CODES 80002 - 89205-26     P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*** VALID MODIFIERS ARE "26","52",AND "90".
     C*** VALID MODIFIERS ARE "76"
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 68, XI, A, 2, b
     C*** SOME STUDIES HAVE CODES WHICH LIST COMPLETE BODY AREAS AS WELL
     C*** AS SEPERATE ENTITIES.  WHEN MULTIPLE SECTIONS OF AN AREA ARE
     C*** BILLED SEPERATLY, THE TOTAL PAID MUST NOT EXCEED THE MRA OF THE
     C*** COMPLETE BODY AREA STUDY.
     C*** Reimbursements for the total procedure must be the provider's
     C*** charge or the MRA, whichever is less.
     C*
     C                   Z-ADD     1             R                 5 0
     C     PATLOK        TAG
     C     DHPRCD        LOOKUP    RAD1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      RAD2(R)       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END
     C*
     C     R             IFLT      95
     C                   ADD       1             R
     C                   GOTO      PATLOK
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 68, XI, A, 2, c
     C*** When a physician provides only the professional component, such
     C*** as in a hospital or other facility, Modifier -26 must be added to
     C*** the appropriate laboratory 5-digit procedure code.
     C*
     C*** SAVE ORIGINAL MODIFIER
     C                   MOVE      CAPMOD        H2PMOD
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      'B'
     C                   MOVE      CAPMOD        H2PMOD                         ADD 12/03/92
     C                   MOVE      '26'          CAPMOD
     C                   MOVE      '236'         XPEOMB
     C*** EOMB 236: "Based upon provider's facility, reimbursement is made for
     C***            the professional component"
     C                   END                                                    END EQ 1/2/B
     C*** END ADD ***** 05/23/91 *************************************
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '26'
     C     CAPMOD        ANDNE     '52'
     C     CAPMOD        ANDNE     '90'
     C     CAPMOD        ANDNE     '76'                                         7/26/89 ADD
     C*
     C** ERROR - INVALID MODIFIER ENTERED CLEAR THE CHARGE, SND ERROR
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   ELSE
     C*
     C                   EXSR      PMOD
     C                   MOVE      H2PMOD        CAPMOD                         ADD 12/03/92
     C     CAPMOD        CABEQ     '90'          MOD90
     C*** If Mod -26 and Prof Component Flag = A, then it is ANE
     C     CAPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'A'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTD
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    ENDEQ 26/'A'
     C*
     C* CHAIN TO DETAIL CHARGE FILE, IF YOU FIND A CHARGE W/O MOD 26
     C* THAT HAS BEEN PAID, THEN DENY THIS (CURRENT) CHARGE AND PRINT
     C* MESSAGE (ERROR MSG #29, EOMB #222) THAT SAYS "THIS PROCEDURE
     C* WAS PREVIOUSLY PAID UNDER A COMPLETE CODE"
     C     CAPMOD        IFEQ      '26'
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '222'         XPEOMB
     C*** EOMB 222: "This procedure was previously paid under a complete code"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C                   END                                                    END CAPMOD=26
     C*
     C                   END
     C                   END
     C*
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
     C     MOD90         TAG
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     PATHE         ENDSR
     C********************************************************************
     C** PHYSICAL MEDICINE SUBROUTINE CODES 97010 - 97752        P H Y S *
     C********************************************************************
     C     PHYS          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Initialize all unique fields for FACAP
     C                   MOVE      *BLANKS       CABRPT
     C                   MOVE      *BLANKS       CADAFL
     C                   Z-ADD     *ZEROS        CAMRA
     C                   MOVE      *BLANKS       CABREC
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Give user error message if this is a "Unusual Services" Modifier
     C     CAPMOD        IFEQ      '22'
     C                   EXSR      PMOD
     C                   END                                                    END EQ '22'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 64, IX, B, 1, d, 1
     C*** Per pg. 77, XIII, F, 1
     C*** Per pg. 113, II, E, 1
     C*** Reimbursement for extremity testing, muscle testing and range
     C*** of motion measurements may be made only once in a 30-day period for
     C*** the same body area.
     C*
     C     DHPRCD        IFGE      '97720'
     C     DHPRCD        ANDLE     '97752'
     C*
     C                   Z-ADD     30            SRPST             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE             8 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
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
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFEQ      DHPRCD
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             PASTCT
     C                   END
     C                   END
     C     CAKYC2        READE     PCA0C                                  54
     C                   END
     C*
     C                   Z-ADD     30            SRFUT             5 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM            1
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
     C*
     C                   Z-ADD     XCCASE        XXCASE
     C                   Z-ADD     XCEOB#        XXEOB#
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XCCPT4
     C*
     C     XCCPT4        IFEQ      DHPRCD
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             FUTRCT
     C                   END
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 110, II, C, 2, c
     C*** Assessment of the patient's status includes assessment of the
     C*** neuromuscular system.  Therefore, reimbursement must not be made
     C*** for the neuromuscular testing codes, extremity testing codes,
     C*** and/or range of motion codes in addition to 97700.  These codes
     C*** may not be used instead of 97700 when an initial assessment is
     C*** performed.
     C*
     C     DHPRCD        IFGE      '97720'
     C     DHPRCD        ANDLE     '97752'
     C*
     C                   MOVE      '97700'       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** DISALLOW THIS TESTING CODE.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 76, XIII, E, 3, f
     C*** Per pg. 113, II, D, 3, f
     C*** Since the Hubbard Tank or Therapeutice Pool is designed for full
     C*** body immersion, unless full body immersion therapy is medically
     C*** necessary and prescribed, Procedure codes 97220, 97221, 97240,
     C*** and 97241 must not be reimbursed.
     C*
     C     DHPRCD        IFEQ      '97220'
     C     DHPRCD        OREQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C     DHPRCD        OREQ      '97701'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '228'         XPEOMB
     C*** EOMB 228: "This item requires a prescription and proof of
     C***            Medical Necessity"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 110, II, C, 2, a
     C*** Only one assessment per injury may be reimbursed.
     C*
     C     DHPRCD        IFEQ      '97700'
     C                   MOVE      '97700'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             COUNT             5 0
     C*
     C     *IN54         DOWEQ     '0'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END
     C     CAKYC         READE     PCA0C                                  54
     C                   END
     C*
     C     COUNT         IFGE      1
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '239'         XPEOMB
     C*** EOMB 239: "These services not reimbursable under the Fee Schedule"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C*
     C                   END
     C*
     C                   END
     C*
     C**=*=*=*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 73, XIII, D, 2, a
     C*** For Workers' Compensation, Procedure Code 97260 must be used when
     C*** billing for a manipulation of the spine. (Part Code 2)
     C*
     C     DHPRCD        IFEQ      '97260'
     C     DHAREA        ANDNE     '2 '
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '271'         XPEOMB
     C*** EOMB 271: "Procedure Code is inappropriate to diagnosis info presented
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END EQ 97260/NE 2
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 73, XIII, D, 3, b
     C*** For Workers' Compensation, Procedure Code 97261 must be used when
     C*** billing for a manipulation of an extremity. (Part Code 3-6)
     C*
     C     DHPRCD        IFEQ      '97261'
     C     DHAREA        IFNE      '3 '
     C     DHAREA        ANDNE     '4 '
     C     DHAREA        ANDNE     '5 '
     C     DHAREA        ANDNE     '6 '
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '271'         XPEOMB
     C*** EOMB 271: "Procedure Code is inappropriate to diagnosis info presented
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END NE 3/4/5/6
     C                   END                                                    END EQ 97261
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 73, XIII, D, 3, a
     C*** No more than one visit per day for the purpose of a  manipulation(s)
     C*** may be reimbursed. (Code 97260 or 97261)
     C*
     C*** Per pg. 73, XIII, D, 3, b
     C*** Reimbursement for manipulations is limited to two body areas
     C*** (a combination of the spine and an extremity, or two extremities)
     C*
     C* Code 97260
     C     DHPRCD        IFEQ      '97260'
     C     DHPRCD        OREQ      '97261'
     C                   MOVE      '97260'       XXCPT4
     C                   MOVE      ' '           SPINE
     C                   Z-ADD     0             COUNT
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '97261'
     C*
     C     CADSEQ        IFNE      XRDSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C*
     C* Don't allow a 2nd spine (i.e. Part Code '2 ')
     C     XRPART        IFEQ      '2 '
     C     XRPART        OREQ      '  '
     C                   MOVE      'Y'           SPINE
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C                   END
     C*
     C* If Two Body Parts have already been paid
     C* or this is a Spine charge and a spine has already been paid
     C* Then disallow this charge
     C     COUNT         IFGE      2
     C     DHAREA        OREQ      '2 '
     C     SPINE         ANDEQ     'Y '
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END GE 2 or EQ '2 '
     C*
     C                   END                                                    END EQ 97260/97261
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Check for Possible Physical Therapy Duplicates
     C*
     C     DHPRCD        IFNE      *BLANK
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      CADSEQ        H1DSEQ
     C                   MOVE      DHAREA        XXPART
     C                   Z-ADD     0             COUNT             5 0
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        XXDSEQ
     C*
     C     CAKYP3        CHAIN(N)  PCA0F                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXDSEQ        ANDNE     CADSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Process additional minutes codes
     C*** Per pg. 75, XIII, E, 2, c, 4   (97145,97221,97241,97501,97521,97541)
     C*** Per pg. 112, II, D, 2, c, 4    (97145,97221,97241,97501,97521,97541)
     C*** Per pg. 75, XIII, E, 2, b, 4   (97531)
     C*** Per pg. 112, II, D, 2, b, 4    (97531)
     C*
     C*
     c*** Procedure code 97145 should only be used with codes 97110-97139
     C*** Per pg. 74, XIII, e, 2, a, 4
     C*** Per pg. 111, II, D, 2, a, 4
     c*** Procedure code 97145 must not be reimbursed for additional time
     C*** for physical medicine modalities (Procedure Code 97010-97039)
     C*** as these codes are not billed and reimbursed on a time-oriented
     C*** basis.
     C*
     C     DHPRCD        IFEQ      '97145'
     C*
     C* Read thru entire 97100-type Phys Medicine Codes array to determine
     C* if there is a corresponding initial code
     C                   Z-ADD     1             I
     C     T97100        TAG
     C                   MOVE      IPT2(I)       XXCPT4
     C     CAKEY8        CHAIN(N)  PCA09                              60
     C     *IN60         IFEQ      '1'
     C     I             IFLT      11
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '274'         XPEOMB
     C* EOMB Code 274: "A major code must be present to pay this item"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                    ELS EQ 97145
     C*
     C* Process all other additional minutes codes
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
     C     DHPRCD        IFEQ      '97721'
     C                   MOVE      '97720'       XXCPT4
     C                   END
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '274'         XPEOMB
     C* EOMB Code 274: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   END                                                     END EQ 97145
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 74, XIII, E, 2, b, 3
     C*** Per pg. 111, II, D, 2, b, 4
     C*** For combination therapies which exceed 30 minutes of time, Procedure
     C*** code 97531 should have been used to bill for the additional time
     C*** Code 97145 is best reflected by 97531
     C*
     C     DHPRCD        IFEQ      '97145'
     C*
     C                   MOVE      '97110'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   Z-ADD     0             COUNT
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '97139'
     C*
     C     CADSEQ        IFNE      XRDSEQ
     C     DHAREA        ANDEQ     XRPART
     C                   ADD       1             COUNT
     C                   END
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C                   END
     C*
     C* If there is a combination of modalities or procedures, then 97531
     C* best reflects the code this charge should be paid as.
     C     COUNT         IFGT      1
     C                   MOVEL     '97531'       CABRPC
     C                   MOVE      '273'         CABREC
     C* And get MRA for 97531 for workfile
     C                   MOVE      '97531'       XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C     NXMRA2        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXMRA2
     C                   Z-ADD     PRMRA         CAMRA
     C                   ELSE
     C                   Z-ADD     0             CAMRA
     C                   END                                                    END 0 *IN60
     C                   END
     C*
     C                   END                                                    END EQ '97145'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* If there is history of code 97145; then count to determine
     C* if the adjustor should be notified that history has already been
     C* processed for code 97145 - the computer may not properly
     C* calculate the new 97145 charge, so the adjustor should
     C* be notified. (not dependent upon part)
     C*
     C     DHPRCD        IFEQ      '97145'
     C                   MOVE      '97145'       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C*
     C                   Z-ADD     0             CNTPRC
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   ADD       1             CNTPRC
     C*
     C     CAKEY8        READE(N)  PCA09                                  54
     C                   END
     C*
     C* If history has already been processed - send error message
     C     CNTPRC        IFGT      *ZERO
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '75'          QQC(Q)
     C*** Err Msg 75: "History exists for code 97145;
     C***              Review payment of this charge"
     C                   END                                                    END CNT GT *ZERO
     C*
     C                   END                                                    END 97145
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Only the following codes will be saved to the PT Array and Workfile
     C*
     C** PHYSICAL THERAPY PROCEDURE CODES 97010-97241 AND 97500-97541
     C** ARE ADJUSTED TO 100%,50%, AND 0%. THE HIGHEST REIMBURSEMENT
     C** AMOUNT IS PAID AT 100%, THE NEXT HIGHEST AT 50% AND ALL OTHERS
     C** ARE PAID AT 0%.
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97241'
     C     DHPRCD        ORGE      '97500'
     C     DHPRCD        ANDLE     '97541'
     C*
     C*
     C* SAVE THE NEEDED DATA INORDER TO SUMMERIZE LATER.
     C*
     C                   Z-ADD     1             I
     C*
     C     DHAREA        IFEQ      *BLANK
     C                   MOVE      'xx'          DHAREA
     C                   END
     C                   MOVEL     DHAREA        KEY7              7
     C                   MOVE      DHPRCD        KEY7
     C     '       '     LOOKUP    PT2(I)                                 60
     C*
     C                   MOVEL     KEY7          PT1(I)
     C                   MOVE      KEY7          PT2(I)
     C                   MOVE      CADSEQ        PT3(I)
     C                   Z-ADD     SXMRA         PT4(I)
     C                   MOVE      CAEOB#        PT9(I)
     C                   MOVEL     CACHK#        PT9(I)
     C                   Z-ADD     CAFEIN        PFN(I)
     C                   Z-ADD     CAMOD         PMD(I)
     C                   MOVE      CAPRTY        PTY(I)
     C                   Z-ADD     CAPAID        AJ1(I)
     C                   MOVE      'P'           AJC(I)
     C                   Z-ADD     1             I
     C*
     C     DHAREA        LOOKUP    PT5(I)                                 60
     C  N60              Z-ADD     1             I
     C  N60'  '          LOOKUP    PT5(I)                                 60
     C*
     C                   MOVE      DHAREA        PT5(I)
     C                   ADD       CACHRG        PT6(I)
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Code has passed all edits - Write to the FACAPFL Workfile
     C* Only load "current charges" into the FACAPFL Workfile
     C*
     C* If the "Best Reflects" fields are empty - then load the regular code
     C* Procedure Code Modifier
     C     CABRPM        IFEQ      *BLANKS
     C                   MOVE      CAPMOD        CABRPM
     C                   END
     C* Procedure Code
     C     CABRPC        IFEQ      *BLANKS
     C                   MOVE      DHPRCD        CABRPC
     C                   END
     C* Determine "Best Reflects" Total Charge
     C* Total Charge or MRA
     C                   Z-ADD     SXMRA         XXMRA
     C     CACHRG        COMP      XXMRA                              54        Greater Than
     C   54              Z-ADD     XXMRA         CABRTC
     C  N54              Z-ADD     CACHRG        CABRTC
     C*
     C* MRA
     C     CAMRA         IFEQ      *ZEROS
     C                   Z-ADD     SXMRA         CAMRA
     C                   END
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C                   MOVE      'P'           CATYPE                         ADD 01/25/96
     C                   WRITE     PCAPT1
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END GE 97010...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C     PHYSA         TAG
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     PHYSE         ENDSR
     C*
     C/EJECT
     *********************************************************************
     *** PHYSICAL THEREPY LEVEL BREAK (ACTUAL UPDATES)         P H Y S 2 *
     *********************************************************************
     C     PHYS2         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     PT1(1)        IFNE      *BLANK
     C*
     C                   MOVEL     PT6           PT7
     C                   MOVE      PT5           PT7
     C                   SORTA     PT7
     C*
     C** IF THERE ARE AT LEAST TWO BODY PARTS PERFORM SUMMARY
     C** BREAKDOWN, IF NOT PERFORM A DETAIL BREAKDOWN.
     C*
     C                   MOVE      PT7(29)       TEST              2
     C     TEST          IFEQ      *BLANK
MG2  C                   MOVE      '1'           *IN92
     C*
     C**  PERFORM A DETAIL BREAKDOWN. FIRST LOAD AN ARRAY WITH AMOUNT
     C**  AND SEQUENCE NUMBER. PAY 100% OF HIGHEST, 50% OF NEXT HIGHEST
     C**  AND 0% OF ALL AFTER THAT.
     C*
     C     1             DO        30            I
     C*
     C                   MOVE      PT2(I)        WRK5A             5
     C     WRK5A         IFEQ      '97145'
     C     WRK5A         OREQ      '97221'
     C     WRK5A         OREQ      '97241'
     C     WRK5A         OREQ      '97501'
     C     WRK5A         OREQ      '97521'
     C     WRK5A         OREQ      '97531'
     C     WRK5A         OREQ      '97541'
     C*
     C*** DISALLOW ANY ADDITIONAL MIN. CODES HERE THAT DON'T HAVE THE PROPER
     C*** MAJOR CODE ASSOCIATED WITH THEM.
     C*
     C                   MOVE      PT3(I)        PTDSEQ
     C                   MOVE      PT9(I)        PTEOB#
     C                   MOVE      PT9(I)        PTCHK#
     C                   MOVE      PFN(I)        PTFEIN
     C                   MOVE      PMD(I)        PTMOD
     C                   MOVE      PTY(I)        PTPRTY
     C* THIS SECTION OF CODE IS COMMENTED OUT BECAUSE MAJOR CODE AND
     C* ADDITIONAL TIME IS HANDLED IN PHYS AND PHYS3
     C*
     C*
     C*
     C                   MOVEL     *BLANKS       PT1(I)
     C                   MOVE      *BLANKS       PT2(I)
     C                   MOVE      *ZEROS        PT3(I)
     C                   MOVE      *ZEROS        PT4(I)
     C                   MOVE      *ALL'0'       PT9(I)
     C                   MOVE      ' '           AJC(I)
     C                   Z-ADD     *ZERO         AJ1(I)
     C                   Z-ADD     *ZERO         AJ2(I)
     C                   MOVE      *BLANKS       AJM(I)
     C                   MOVE      *ZEROS        PFN(I)
     C                   MOVE      *ZEROS        PMD(I)
     C                   MOVE      *BLANKS       PTY(I)
     C                   END
     C*
     C                   MOVEL     PT1(I)        DSPART
     C                   MOVE      PT2(I)        DSCPT4
     C                   MOVE      PT3(I)        DSDSEQ
     C                   MOVE      PT4(I)        DSAMT
     C                   MOVEL     PT9(I)        DSCHK#
     C                   MOVE      PT9(I)        DSEOB#
     C                   MOVE      AJC(I)        DSAJC
     C                   Z-ADD     AJ1(I)        DSAJ1
     C                   Z-ADD     AJ2(I)        DSAJ2
     C                   MOVE      AJM(I)        DSAJM
     C                   MOVE      PFN(I)        DSFEIN
     C                   MOVE      PMD(I)        DSMOD
     C                   MOVE      PTY(I)        DSPRTY
     C                   MOVE      DSSORT        AJB(I)
     C                   END
     C*
     C                   SORTA     AJB
     C*
     C     1             DO        30            I
     C                   MOVEL     AJB(I)        DSSORT
     C                   MOVEL     DSPART        PT1(I)
     C                   MOVEL     DSCPT4        PT2(I)
     C                   MOVEL     DSDSEQ        PT3(I)
     C                   MOVEL     DSAMT         PT4(I)
     C                   MOVEL     DSCHK#        PT9(I)
     C                   MOVE      DSEOB#        PT9(I)
     C                   Z-ADD     DSAJ1         AJ1(I)
     C                   Z-ADD     DSAJ2         AJ2(I)
     C                   MOVE      DSAJM         AJM(I)
     C                   MOVEL     DSAJC         AJC(I)
     C                   Z-ADD     DSFEIN        PFN(I)
     C                   Z-ADD     DSMOD         PMD(I)
     C                   MOVE      DSPRTY        PTY(I)
     C                   END
     C                   Z-ADD     1             XX                3 0
     C                   SETOFF                                         79
     C     30            DO        30            I
     C*
     C     I             IFGT      *ZERO
     C*
     C                   MOVE      PT3(I)        XXDSEQ
     C*
     C** ONLY PROCESS THE ELEMENT IF THE SEQUENCE NUMBER IS NOT ZERO.
     C*
     C     XXDSEQ        IFNE      *ZERO
     C*
     C     CAKEY1        CHAIN(N)  PCA0F                              64
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C*
     C**********           MOVELDHPRCD    XXCPT4
     C     DHKY          CHAIN     PDH02                              61
     C*
     C                   MOVEL     DXPRCD        XXCPT4
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      PXFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C*
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS >2.
     C     XX            IFGT      2
     C                   ADD       1             XX
     C                   Z-ADD     PRMRA         PXTOTA
     C                   END
     C*
     C* CALCULATE THE AMOUNT TO PAY. PAY 100 IF 'XX' IS 2.
     C     XX            IFEQ      2
     C                   ADD       1             XX
     C                   Z-ADD     PRMRA         PXTOTA
     C                   END
     C*
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS 1.
     C     XX            IFEQ      1
     C                   ADD       1             XX
     C                   Z-ADD     PRMRA         PXTOTA
     C                   END
     C*
     C     PXTOTC        IFLT      PXTOTA
     C                   Z-ADD     PXTOTC        PXTOTA
     C                   END
     C*
     C                   Z-ADD     PXTOTA        AJ2(I)
     C                   Z-ADD     XX            AJ3(I)
     C                   MOVE      PXPMOD        AJM(I)
     C*
     C                   END
     C                   ELSE
     C                   SETON                                        79
     C                   END
     C  N79              END       -1
     C*
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN92
     C*
     C**  PERFORM A SUMMARY BREAKDOWN. PROCESS THE PART ARRAY BY AMOUNT
     C**  PAY 100% OF HIGHEST, 50% OF NEXT HIGHEST
     C**  AND 0% OF ALL AFTER THAT.
     C*
     C                   Z-ADD     1             XX
     C                   SETOFF                                       79
     C     30            DO        30            I
     C*
     C     I             IFGT      *ZERO
     C*
     C                   MOVE      PT7(I)        PART              2
     C*
     C**  PERFORM A THE POSTING ONLY IF A BODY PART IS FOUND.
     C*
     C     PART          IFNE      *BLANK
     C*
     C     1             DO        30            N                 3 0
     C*
     C**  PROCESS THE ARRAY LOOKING ONLY FOR THE CURRENT BODY PART.
     C*
     C     PART          IFEQ      PT1(N)
     C*
     C** ONLY PROCESS THE ELEMENT IF THE SEQUENCE NUMBER IS NOT ZERO.
     C*
     C                   MOVE      PT3(N)        XXDSEQ
     C*
     C     CAKEY1        CHAIN(N)  PCA0F                              64
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C*
     C**********           MOVELDHPRCD    XXCPT4
     C     DHKY          CHAIN     PDH02                              61
     C*
     C                   MOVEL     DXPRCD        XXCPT4
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      PXFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C*
     C*** Per pg. 73, XIII, D, 3, b
     C*** Per pg. 76, XIII, E, 3, d, 2
     C*** Reimbursement for manipulations is limited to two body areas (a
     C*** combination of the spine and an extremity, or two extremities).
     C* CALCULATE THE AMOUNT TO PAY. PAY 0% IF 'XX' IS >2.
     C     XX            IFGT      2
     C                   Z-ADD     0             PXTOTA
     C                   END
     C*
     C*** Per pg. 75-76, XIII, E, 2, d, 2  /  3, d, 2
     C*** Per pg. 112, II, D, 2, d, 1  /  3, d, 2
     C*** When physical medicine therapies are provided to more than one body
     C*** area, Modifier -51 must be added to the procedure code(s) billed
     C*** for the addtional body area.
     C* CALCULATE THE AMOUNT TO PAY. PAY 50% IF 'XX' IS 2.
     C*
     C     XX            IFEQ      2
     C     PRMRA         DIV(H)    2             PXTOTA
     C                   END
     C*
     C*** Per pg. 76, XIII, E, 3, d, 1
     C*** Per pg. 112, II, D, 3, d, 1
     C*** Reimbursement for therapy to the first body area must be at the
     C*** provider's usual charge or the MRA for the applicable procedure
     C*** code, whichever is less.
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS 1.
     C*
     C     XX            IFEQ      1
     C                   Z-ADD     PRMRA         PXTOTA
     C                   END
     C*
     C     PXTOTC        IFLT      PXTOTA
     C                   Z-ADD     PXTOTC        PXTOTA
     C                   END
     C*
     C                   Z-ADD     PXTOTA        AJ2(N)
     C                   Z-ADD     XX            AJ3(N)
     C                   MOVE      PXPMOD        AJM(N)
     C*
     C** END OF MATCH ON PART.
     C*
     C                   END
     C*
     C** END OF LOOP FOR MATCHING BODY PART.
     C*
     C                   END
     C*
     C** INCREMENT THE INDICATOR TO REDUCE THE PERCENTAGE IN HALF.
     C*
     C                   ADD       1             XX
     C*
     C*
     C** END OF TEST FOR BLANK PART.
     C*
     C                   END
     C                   ELSE
     C                   SETON                                        79
     C                   END
     C*
     C** END OF DO LOOP FOR THE PROCESS OF BODY PART.
     C*
     C  N79              END       -1
     C*
     C** END OF SUMMARY LOOP.
     C*
     C                   END
     C*
     C** END OF PHYSICAL THERAPY CLOSE ROUTINE.
     C*
     C                   EXSR      UPPT
     C                   END
     C*
     C** AFTER A BREAK IN THE PROVIDER CLEAR THE TOTALS.
     C*
     C                   MOVE      *BLANK        PT1
     C                   MOVE      *BLANK        PT2
     C                   MOVE      *BLANK        PT3
     C                   MOVE      *BLANK        PT4
     C                   MOVE      *BLANK        PT5
     C                   MOVE      *BLANK        PT6
     C                   MOVE      *BLANK        PT7
     C                   MOVE      *BLANK        PT9
     C                   MOVE      *BLANK        AJ1
     C                   MOVE      *BLANK        AJ2
     C                   MOVE      *BLANKS       AJM
     C                   MOVE      *BLANK        AJ3
     C                   MOVE      *BLANK        AJB
     C                   MOVE      *BLANK        AJC                            11/29/90 ADD
     C                   MOVE      *BLANKS       PFN
     C                   MOVE      *BLANKS       PMD
     C                   MOVE      *BLANKS       PTY
     C*
     C     PHYS2E        ENDSR
     C*
     C********************************************************************
     C** PHYSICAL THERAPY (NEW PROCESSING)                     P H Y S 3 *
     c********************************************************************
     C     PHYS3         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Reading the Physical Therapy Detail Charge Workfile,
     C* Run all records through the following additional FS rules before
     C* final payment
     C*
     C* NOTE:  The "Best Reflects" field will be used to determine payment
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Read all charges with modifier -51:
     C*
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       P2EOMB
     C*
     C     P2PMOD        IFEQ      '51'
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKYP1        CHAIN     PCA06                              61
     C* See if there is the same code in either history or current
     C* without modifier -51. If not, then disallow the workfile record
     C     *IN61         IFEQ      '1'
     C* OR If there is the same code in either history or current
     C* without modifier -51, but it has the same body part as the
     C* workfile record, then disallow the workfile record.
     C     *IN61         OREQ      '0'
     C     P2FDAT        ANDEQ     CAFDAT                                       ADD 04/14/97
     C     P2DSEQ        ANDNE     X6DSEQ
     C     P2PART        ANDEQ     X6PART
     C* OR If there is the same code in either history or current
     C* without modifier -51, but it has the same body area (extremity) as the
     C* workfile record, then disallow the workfile record.
     C     *IN61         OREQ      '0'
     C     P2FDAT        ANDEQ     CAFDAT                                       ADD 04/14/97
     C     P2DSEQ        ANDNE     X6DSEQ
     C     P2PART        ANDGE     '3 '
     C     P2PART        ANDLE     '6 '
     C*  CALL TO WRONG RECORD
     C     X6PART        ANDGE     '3 '
     C     X6PART        ANDLE     '6 '
     C                   Z-ADD     *ZERO         P2TOTA
     C                   Z-ADD     *ZERO         P2TOTP
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTC        P2TOTC
     C                   Z-ADD     *ZEROS        P2BRTC
     C                   MOVE      '271'         P2EOMB
     C*** EOMB 271: "Procedure code is inappropriate to diagnosis info
     C***            presented"
     C                   MOVE      'Y'           P2DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT2
     C                   END                                                    END 1 *IN61...
     C                   END                                                    END EQ '51'
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     P2EOMB        IFNE      *BLANKS
     C*
     C*** BEG ADD *** 11/23/99 ****************************************
     C                   MOVE      P2EOMB        XPEOMB
     C*** END ADD *** 11/23/99 ****************************************
     C                   EXSR      EOMBP2
     C*
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   READ      PCAPT2                                 60
     C                   END                                                    END DOWEQ *IN60
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*  Read all history for this date/provider/mod/code (Best Reflects)
     C*       to determine what body parts have been paid
     C*
     C* Initialize Variables
     C                   Z-ADD     *ZEROS        WBPC                           Wrkf Chrg Array
     C                   Z-ADD     *ZEROS        WBPO                           Wrkf Entry Ord Array
     C                   MOVE      *BLANKS       WBP                            Wrkf Body Part Array
     C                   Z-ADD     *ZEROS        WBP#                           Wrkf Seq# Array
     C                   MOVE      *BLANKS       WBPM                           Wrkf Proc Mod Array
     C                   MOVE      *BLANKS       WFSORT                         Wrkf Sort Data Struc
     C                   Z-ADD     *ZEROS        WFTOTC                         Wrkf TOTC Data Struc
     C                   Z-ADD     *ZEROS        WFORDR                         Wrkf ORDR Data Struc
     C                   MOVE      *BLANKS       WFPART                         Wrkf PART Data Struc
     C                   Z-ADD     *ZEROS        WFDSEQ                         Wrkf DSEQ Data Struc
     C                   MOVE      *BLANKS       WFPMOD                         Wrkf PMOD Data Struc
     C                   MOVE      *BLANKS       WBPS                           Wrkf Summary Array
     C                   Z-ADD     0             W                              Wrkf Array Cntr
     C                   Z-ADD     1000          WO                             Wrkf Entry Order Cnt
     C                   MOVE      *BLANKS       HLBRPC                         Hold BR Proc Code
     C                   MOVE      *BLANKS       HLBRPT                         Hold BR Part Code
     C                   MOVE      *BLANKS       HLBRPM                         Hold BR Proc Code Mo
     C                   Z-ADD     0             H                              Hist Cd/BP Array Cnt
     C                   MOVE      ' '           BPFRST                         Determines if 1st
     C*                                                    chrg has been done
     C*                                                    Blank says yes
     C*
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C                   EXCEPT    ECAPT2
     C*
     C                   MOVE      *BLANKS       P2EOMB
     C*
     C* If this code had not been disallowed above, then process here
     C     P2DAFL        IFEQ      ' '
     C*
     C* If this is the first code read
     C     BPFRST        IFEQ      ' '
     C* Establish the code we are working with
     C* Load this code into a workfile array
     C                   ADD       1             W
     C                   SUB       1             WO
     C                   Z-ADD     P2BRTC        WBPC(W)
     C                   Z-ADD     WO            WBPO(W)
     C                   MOVE      P2PART        WBP(W)
     C                   Z-ADD     P2DSEQ        WBP#(W)
     C                   MOVE      P2PMOD        WBPM(W)
     C                   END                                                    END BPFRST EQ ' '
     C*
     C* If this is a new code we are working with:
     C     P2BRPC        IFNE      HLBRPC
     C     P2BRPM        ORNE      HLBRPM
     C*
     C* If this is not the first code we are working with:
     C     BPFRST        IFEQ      'Y'
     C*
     C* Process all workfile records for the "old" code first
     C* Reset counter
     C                   Z-ADD     0             W                              Wrkf Array Cntr
     C*
     C*   Move into data structure/summary BP array - preset for sort
     C     1             DO        999           W
     C     WBPC(W)       IFEQ      *ZERO
     C                   GOTO      SRT3A
     C                   END
     C                   Z-ADD     WBPC(W)       WFTOTC
     C                   Z-ADD     WBPO(W)       WFORDR
     C                   MOVEL     WBP(W)        WFPART
     C                   Z-ADD     WBP#(W)       WFDSEQ
     C                   MOVEL     WBPM(W)       WFPMOD
     C                   MOVE      WFSORT        WBPS(W)
     C                   END
     C*
     C*   Sort BP arrays to determine the highest charge amount
     C     SRT3A         TAG
     C                   SORTA     WBPS
     C*
     C*   Move back out into arrays
     C     1             DO        999           W
     C     WBPS(W)       IFEQ      *BLANKS
     C                   GOTO      NXT3A
     C                   END
     C                   MOVEL     WBPS(W)       WFSORT
     C                   Z-ADD     WFTOTC        WBPC(W)
     C                   Z-ADD     WFORDR        WBPO(W)
     C                   MOVEL     WFPART        WBP(W)
     C                   Z-ADD     WFDSEQ        WBP#(W)
     C                   MOVEL     WFPMOD        WBPM(W)
     C                   END
     C*
     C*   Determine highest paid
     C*   All records must be read so that they can each be assigned a
     C*   body part or disallowed, as applicable.
     C     NXT3A         TAG
     C     1             DO        999           W
     C*
     C     WBPC(W)       IFEQ      *ZEROS
     C                   GOTO      NXT3A1
     C                   END
     C                   Z-ADD     WBP#(W)       XXDSEQ
     C*
     C*
     C* Look for the body part in the History's array
     C* If found, then disallow this code
     C     WBP(W)        LOOKUP    HBPP                                   61
     C     *IN61         IFEQ      '1'
     C*
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     P3CPT4        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     P3CPT4        ORGE      '97010'
     C     P3CPT4        ANDLE     '97139'
     C     HLMRA         SUB       P97530        L97530
     C     L97530        IFLE      *ZERO
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   MOVE      'Y'           P3DAFL
     C                   ELSE                                                   ELSE L97530 LE *ZERO
     C     P3TOTC        IFGT      L97530                                       CHG 03/11/93
     C                   Z-ADD     L97530        P3TOTA
     C                   Z-ADD     L97530        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97530        P3BRTC
     C                   MOVE      '002'         XPEOMB
     C*   EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ADD       P3TOTP        P97530
     C                   END                                                    END P3TOTC GT L97503
     C                   END                                                    END L97503 LE *ZERO
     C                   ELSE                                                   ELSE P3BRPC
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C     HLMRA         SUB       P97531        L97531           10 2
     C     L97531        IFLE      *ZERO
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   MOVE      'Y'           P3DAFL
     C                   ELSE                                                   ELSE L97531 LE *ZERO
     C     P3TOTC        IFGT      L97531                                       CHG 03/11/93
     C                   Z-ADD     L97531        P3TOTA
     C                   Z-ADD     L97531        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97531        P3BRTC
     C                   MOVE      '002'         XPEOMB
     C*   EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ADD       P3TOTP        P97531           10 2
     C                   ELSE                                                   ELSE P3TOTC GT L9750
     C                   Z-ADD     P3TOTC        P3TOTA                         CHG 03/11/93
     C                   Z-ADD     P3TOTA        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97531        P3BRTC
     C                   MOVE      '273'         P3EOMB
     C*   EOMB 273: "Additional time for multiple procedures is reimbursed"
     C*             "as 97531 per RM"
     C                   ADD       P3TOTP        P97531
     C                   END                                                    END P3TOTC GT L97503
     C                   END                                                    END L97531 LE *ZERO
     C                   ELSE                                                   ELSE P3BRPC
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in
     C*              reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C                   END                                                    END P3BRPC
     C                   END                                                    END P3BRPC
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C*
     C                   ELSE                                                   ELSE 1 *IN61
     C*
     C* If not found, then determine what the next available body part is
     C                   Z-ADD     1             N
     C     *BLANKS       LOOKUP    HBPP(N)                                62
     C     *IN62         IFEQ      '1'
     C*
     C* Establish the Part Code available
     C     N             IFEQ      1
     C     N             OREQ      2
     C                   MOVE      'P1'          HLBRPT
     C                   ELSE                                                   ELSE N EQ 1/2
     C                   MOVE      'P2'          HLBRPT
     C                   END                                                    END N EQ 1/2
     C*
     C* If this is the TRUNK:
     C     WBP(W)        IFEQ      '2 '
     C*
     C* If HBPP,1 is not available, then move to HBPP,3
     C     N             IFEQ      2
     C                   Z-ADD     3             N
     C                   MOVE      'P2'          HLBRPT
     C*
     C* If HBPP,3 is available, then update workfile and array
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* Load both sections of body area
     C                   MOVE      'P2'          P3BRPT                         BR Part
     C                   MOVE      P3PART        HBPP(3)
     C                   MOVE      P3PART        HBPP(4)
     C*
     C* If this workfile code doesn't have a modifier -51, then give it EOB Code
     C     WBPM(W)       IFNE      '51'
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END NE '51'
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Pay at 50% of MRA
     C     P3MRA         MULT      .50           XXMRA
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA             9 2
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   ELSE                                                   ELSE EQ 2
     C*
     C* If HBPP,3 is not available, then disallow this charge
     C     N             IFEQ      4
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   ELSE                                                   ELSE N EQ 4
     C* If either HBPP,1 or HBPP,3 is available, then update workfile and array
     C*
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* If this workfile code has a modifier -51, then it must loaded into "2"
     C     WBPM(W)       IFEQ      '51'
     C                   MOVE      'P2'          P3BRPT                         BR Part
     C* Load both sections of body area
     C                   MOVE      P3PART        HBPP(3)
     C                   MOVE      P3PART        HBPP(4)
     C                   ELSE                                                   ELSE EQ '51'
     C* Else load into what is available
     C                   MOVE      HLBRPT        P3BRPT                         BR Part
     C*
     C* If this is loaded into the second Body Part, then give EOMB
     C     N             IFEQ      3
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END EQ 3
     C*
     C* Load both sections of body area
     C                   MOVE      P3PART        HBPP(N)
     C                   ADD       1             N
     C                   MOVE      P3PART        HBPP(N)
     C                   END                                                    END EQ '51'
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Determine whether to pay at 100% of MRA or 50% of MRA
     C     P3BRPT        IFEQ      'P1'
     C                   Z-ADD     P3MRA         XXMRA
     C                   ELSE                                                   ELSE EQ 'P1'
     C     P3BRPT        IFEQ      'P2'
     C     P3MRA         MULT      .50           XXMRA
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'P1'
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   END                                                    END N EQ 4
     C*
     C                   END                                                    END N EQ 2
     C*
     C                   ELSE                                                   ELSE WBP,W EQ '2 '
     C*
     C* If this is an EXTREMITY, then update workfile and array
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* If this workfile code has a modifier -51, then it must loaded into "2"
     C     WBPM(W)       IFEQ      '51'
     C*
     C                   MOVE      'P2'          P3BRPT
     C* If this array has first Part Two available, then load it there
     C     N             IFEQ      3
     C                   MOVE      P3PART        HBPP(3)
     C                   ELSE                                                   ELSE N EQ 3
     C* Else, load into second Part Two
     C                   MOVE      P3PART        HBPP(4)
     C                   END                                                    END N EQ 3
     C*
     C                   ELSE                                                   ELSE EQ '51'
     C* Else load into what is available
     C                   MOVE      HLBRPT        P3BRPT                         BR Part
     C*
     C* If this is loaded into the second Body Part, then give EOMB
     C     N             IFEQ      3
     C     N             OREQ      4
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END EQ 3
     C                   MOVE      P3PART        HBPP(N)
     C                   END                                                    END EQ '51'
     C*
     C* If this is second section of Body Part, then disallow
     C     N             IFEQ      2
     C     N             OREQ      4
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*
     C                   ELSE                                                   ELSE N EQ 2 or 4
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Determine whether to pay at 100% of MRA or 50% of MRA
     C     P3BRPT        IFEQ      'P1'
     C                   Z-ADD     P3MRA         XXMRA
     C                   ELSE                                                   ELSE EQ 'P1'
     C     P3BRPT        IFEQ      'P2'
     C     P3MRA         MULT      .50           XXMRA
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'P1'
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C                   END                                                    END N EQ 2 or 4
     C*
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   END                                                    END WBP,W EQ '2 '
     C*
     C                   ELSE                                                   ELSE 1 *IN62
     C* If there is no next available body part, then disallow this code
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C                   END                                                    END 1 *IN62
     C                   END                                                    END 1 *IN61
     C*
     C                   END                                                    1 DO 999 W
     C*
     C* Reset variables and arrays
     C     NXT3A1        TAG
     C                   Z-ADD     *ZEROS        WBPC
     C                   Z-ADD     *ZEROS        WBPO
     C                   MOVE      *BLANKS       WBP
     C                   Z-ADD     *ZEROS        WBP#
     C                   MOVE      *BLANKS       WBPM
     C                   MOVE      *BLANKS       WFSORT
     C                   Z-ADD     *ZEROS        WFTOTC
     C                   Z-ADD     *ZEROS        WFORDR
     C                   MOVE      *BLANKS       WFPART
     C                   Z-ADD     *ZEROS        WFDSEQ
     C                   MOVE      *BLANKS       WFPMOD
     C                   MOVE      *BLANKS       WBPS
     C                   Z-ADD     0             W
     C                   Z-ADD     1000          WO
     C*
     C                   END                                                     END BPFRST EQ 'Y'
     C*
     C* Set "First Record" flag
     C*
     C* Process all history:
     C* Only process history once for each code
     C* Reset variables and arrays
     C                   MOVE      P2BRPC        XXCPT4
     C                   Z-ADD     *ZEROS        B                              BP Array Counter
     C                   Z-ADD     *ZEROS        BPP                            BP Paid Amt Array
     C                   MOVE      *BLANKS       BP                             BP Array
     C                   Z-ADD     *ZEROS        BPC                            BP Chrg Amt Array
     C                   Z-ADD     *ZEROS        BP$                            BP MRA Amt Array
     C                   MOVE      *BLANKS       HBPP                           Hist BP paid
     C                   MOVE      *BLANKS       BPPART                         Body Part Data Struc
     C                   Z-ADD     *ZEROS        BPTOTP                         TOTP Data Struct
     C                   Z-ADD     *ZEROS        BPTOTC                         TOTC Data Struct
     C                   Z-ADD     *ZEROS        BPMRA                          MRA Data Struct
     C                   MOVE      *BLANKS       BPSORT                         Sort Data Struct
     C                   MOVE      *BLANKS       BPS                            BP Summary Array
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     P2EOMB        IFNE      *BLANKS
     C*
     C                   EXSR      EOMBP3
     C*
     C                   END                                                    END DOWEQ *IN60
     C*
     C     CAKYP2        CHAIN(N)  PCA09                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       1             B
     C                   Z-ADD     XRTOTP        BPP(B)
     C                   MOVE      XRPART        BP(B)
     C                   Z-ADD     XRTOTC        BPC(B)
     C* Get MRA for this code
     C                   MOVEL     XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
     C                   READP     PPR01                                  61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     PRMRA         BP$(B)
     C                   END                                                    END 0 *IN61
     C     XRBRPC        IFEQ      '97530'
     C     XRCPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       XRTOTP        P97530
     C                   END
     C     XRBRPC        IFEQ      '97531'
     C     XRCPT4        ANDNE     '97531'
     C                   ADD       XRTOTP        P97531
     C                   END
     C*
     C     CAKYP2        READE(N)  PCA09                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*   Move into data structure/summary BP array - preset for sort
     C     1             DO        999           B
     C     BPP(B)        IFEQ      *ZERO
     C                   GOTO      SRT3B
     C                   END
     C                   MOVE      BPP(B)        BPTOTP
     C                   MOVEL     BP(B)         BPPART
     C                   MOVE      BPC(B)        BPTOTC
     C                   MOVE      BP$(B)        BPMRA
     C                   MOVE      BPSORT        BPS(B)
     C                   END
     C*
     C*   Sort BP arrays to determine the highest paid amount
     C     SRT3B         TAG
     C                   SORTA     BPS
     C*
     C*   Move back out into arrays
     C     1             DO        999           B
     C     BPS(B)        IFEQ      *BLANKS
     C                   GOTO      NXT3B
     C                   END
     C                   MOVEL     BPS(B)        BPSORT
     C                   MOVEL     BPTOTP        BPP(B)
     C                   MOVEL     BPPART        BP(B)
     C                   MOVEL     BPTOTC        BPC(B)
     C                   MOVEL     BPMRA         BP$(B)
     C                   END
     C*
     C*   Determine highest paid and by what percentage
     C     NXT3B         TAG
     C     1             DO        4             B
     C*
     C     BPC(B)        IFNE      *ZEROS
     C*
     C*   If the Paid amount equals the Charge Amount or the MRA, then
     C*   this charge was paid at 100% and should be marked as Body Part 1
     C     BPP(B)        IFEQ      BPC(B)                                       100%
     C     BPP(B)        OREQ      BP$(B)                                       100%
     C*
     C*   If this was the TRUNK:
     C     BP(B)         IFEQ      '2 '
     C     BP(B)         OREQ      '  '
     C*
     C*   If the first Part One is free, then both Part Ones should be marked:
     C     HBPP(1)       IFEQ      '  '
     C                   MOVE      '2 '          HBPP(1)
     C                   MOVE      '2 '          HBPP(2)
     C                   END                                                    END HBPP,1 EQ ' '
     C*
     C                   ELSE                                                   ELSE BP,B EQ '2 '
     C*   Else, If this was an EXTREMITY:
     C*   If the first Part One is free, then just one Part One should be marked
     C     HBPP(1)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(1)
     C                   ELSE                                                   ELSE HBPP,1 EQ ' '
     C*   If the second Part One is free:
     C     HBPP(2)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(2)
     C                   END                                                    END HBPP,2 EQ ' '
     C                   END                                                    END HBPP,1 EQ ' '
     C*
     C                   END                                                    END BP,B  EQ '2 '
     C*
     C                   ELSE                                                   BPP EQ BPC/BPM
     C*
     C* Else, if the Paid amount is 50% of the Charge or MRA, then
     C* this should be marked as Body Part Two
     C     BPC(B)        MULT      .50           BPC(B)
     C     BP$(B)        MULT      .50           BP$(B)
     C     BPP(B)        IFEQ      BPC(B)                                       50%
     C     BPP(B)        OREQ      BP$(B)                                       50%
     C*
     C*   If this was the TRUNK:
     C     BP(B)         IFEQ      '2 '
     C     BP(B)         OREQ      '  '
     C*
     C*   If the first Part Two is free, then both Part Twos should be marked:
     C     HBPP(3)       IFEQ      '  '
     C                   MOVE      '2 '          HBPP(3)
     C                   MOVE      '2 '          HBPP(4)
     C                   END                                                    END HBPP,3 EQ ' '
     C*
     C                   ELSE                                                   ELSE BP,B EQ '2'
     C*   If this was an EXTREMITY:
     C*   If the first Part Two is free, then just one Part Two should be marked
     C     HBPP(3)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(3)
     C                   ELSE                                                   ELSE HBPP,3 EQ ' '
     C*   If the second Part Two is free:
     C     HBPP(4)       IFEQ      ' '
     C                   MOVE      BP(B)         HBPP(4)
     C                   END                                                    END HBPP,4 EQ ' '
     C                   END                                                    END HBPP,3 EQ ' '
     C*
     C                   END                                                    END BP,B EQ '2 '
     C*
     C                   END                                                    BPP EQ .50 BPC/BP$
     C*
     C                   END                                                    BPP EQ BPC/BP$
     C*
     C                   END                                                    END BPC,B NE *ZEROS
     C*
     C                   END                                                    END DO 4 B
     C*
     C                   MOVE      P2BRPC        HLBRPC
     C                   MOVE      P2BRPM        HLBRPM
     C                   END                                                    END P2BRPC NE HLBRPC
     C*
     C* Load this code into a workfile array, if it isn't the first code
     C     BPFRST        IFEQ      'Y'
     C                   ADD       1             W
     C                   SUB       1             WO
     C                   Z-ADD     P2BRTC        WBPC(W)
     C                   Z-ADD     WO            WBPO(W)
     C                   MOVE      P2PART        WBP(W)
     C                   Z-ADD     P2DSEQ        WBP#(W)
     C                   MOVE      P2PMOD        WBPM(W)
     C                   END                                                    END BRFRST EQ 'Y'
     C                   MOVE      'Y'           BPFRST
     C     P2BRPC        IFEQ      '97530'
     C     P2CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P2TOTP        P97530
     C                   Z-ADD     P2MRA         HLMRA
     C                   END
     C*
     C                   END                                                    END P2DAFL EQ ' '
     C*
     C                   READ      PCAPT2                                 60
     C                   END                                                    END DOWEQ *IN60
     C*
     C* Process all workfile records for the final code
     C* Reset counter
     C                   Z-ADD     0             W                              Wrkf Array Cntr
     C*
     C*   Move into data structure/summary BP array - preset for sort
     C     1             DO        999           W
     C     WBPC(W)       IFEQ      *ZEROS
     C                   GOTO      SRT3C
     C                   END
     C                   Z-ADD     WBPC(W)       WFTOTC
     C                   Z-ADD     WBPO(W)       WFORDR
     C                   MOVEL     WBP(W)        WFPART
     C                   Z-ADD     WBP#(W)       WFDSEQ
     C                   MOVEL     WBPM(W)       WFPMOD
     C                   MOVE      WFSORT        WBPS(W)
     C                   END
     C*
     C*   Sort BP arrays to determine the highest charge amount
     C     SRT3C         TAG
     C                   SORTA     WBPS
     C*
     C*   Move back out into arrays
     C     1             DO        999           W
     C     WBPS(W)       IFEQ      *BLANKS
     C                   GOTO      NXT3C
     C                   END
     C                   MOVEL     WBPS(W)       WFSORT
     C                   Z-ADD     WFTOTC        WBPC(W)
     C                   Z-ADD     WFORDR        WBPO(W)
     C                   MOVEL     WFPART        WBP(W)
     C                   Z-ADD     WFDSEQ        WBP#(W)
     C                   MOVEL     WFPMOD        WBPM(W)
     C                   END
     C*
     C*
     C*   Determine highest paid
     C*   All records must be read so that they can each be assigned a
     C*   body part or disallowed, as applicable.
     C     NXT3C         TAG
     C     1             DO        999           W
     C*
     C     WBPC(W)       IFEQ      *ZEROS
     C                   GOTO      NXT3C1
     C                   END
     C*
     C                   Z-ADD     WBP#(W)       XXDSEQ
     C*
     C* Look for the body part in the History's array
     C* If found, then disallow this code
     C     WBP(W)        LOOKUP    HBPP                                   61
     C     *IN61         IFEQ      '1'
     C*
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C     HLMRA         SUB       P97530        L97530
     C     L97530        IFLE      *ZERO
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   MOVE      'Y'           P3DAFL
     C                   ELSE                                                   ELSE L97530 LE *ZERO
     C     P3TOTC        IFGT      L97530                                       CHG 03/11/93
     C                   Z-ADD     L97530        P3TOTA
     C                   Z-ADD     L97530        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97530        P3BRTC
     C                   MOVE      '002'         XPEOMB
     C*   EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ADD       P3TOTP        P97530
     C********             ELSE                            ELSE P3TOTC GT L97503
     C********             Z-ADDP3TOTC    P3TOTA           CHG 03/11/93
     C********             Z-ADDP3TOTA    P3TOTP
     C********   P3TOTC    SUB  P3TOTA    P3TOTD           CHG 03/11/93
     C********             Z-ADDP3TOTC    P3TOTC
     C********             Z-ADDL97530    P3BRTC
     C********             MOVE '235'     P3EOMB
     C****EOMB 235: "Multiple Procedures (97010-97139) are reimbursed per FS"
     C********             ADD  P3TOTP    P97530
     C                   END                                                    END P3TOTC GT L97503
     C                   END                                                    END L97503 LE *ZERO
     C                   ELSE                                                   ELSE P3BRPC
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C     HLMRA         SUB       P97531        L97531           10 2
     C     L97531        IFLE      *ZERO
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   MOVE      'Y'           P3DAFL
     C                   ELSE                                                   ELSE L97531 LE *ZERO
     C     P3TOTC        IFGT      L97531                                       CHG 03/11/93
     C                   Z-ADD     L97531        P3TOTA
     C                   Z-ADD     L97531        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97531        P3BRTC
     C                   MOVE      '002'         XPEOMB
     C*   EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ADD       P3TOTP        P97531           10 2
     C                   ELSE                                                   ELSE P3TOTC GT L9750
     C                   Z-ADD     P3TOTC        P3TOTA                         CHG 03/11/93
     C                   Z-ADD     P3TOTA        P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     L97531        P3BRTC
     C                   MOVE      '273'         P3EOMB
     C*   EOMB 273: "Additional time for multiple procedures is reimbursed"
     C*             "as 97531 per RM"
     C                   ADD       P3TOTP        P97531
     C                   END                                                    END P3TOTC GT L97503
     C                   END                                                    END L97531 LE *ZERO
     C                   ELSE                                                   ELSE P3BRPC
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD                         CHG 03/11/93
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*   EOMB 009: "Exceeds number of allowable procedures in
     C*              reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C                   END                                                    END P3BRPC
     C                   END                                                    END P3BRPC
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C*
     C                   ELSE                                                   ELSE 1 *IN61
     C*
     C* If not found, then determine what the next available body part is
     C                   Z-ADD     1             N
     C     *BLANKS       LOOKUP    HBPP(N)                                62
     C     *IN62         IFEQ      '1'
     C*
     C* Establish the Part Code available
     C     N             IFEQ      1
     C     N             OREQ      2
     C                   MOVE      'P1'          HLBRPT
     C                   ELSE                                                   ELSE N EQ 1/2
     C                   MOVE      'P2'          HLBRPT
     C                   END                                                    END N EQ 1/2
     C*
     C* If this is the TRUNK:
     C     WBP(W)        IFEQ      '2 '
     C*
     C* If HBPP,1 is not available, then move to HBPP,3
     C     N             IFEQ      2
     C                   Z-ADD     3             N
     C                   MOVE      'P2'          HLBRPT
     C* If HBPP,3 is available, then update workfile and array
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* Load both sections of body area
     C                   MOVE      'P2'          P3BRPT                         BR Part
     C                   MOVE      P3PART        HBPP(3)
     C                   MOVE      P3PART        HBPP(4)
     C*
     C* If this workfile code doesn't have a modifier -51, then give it EOB Code
     C     WBPM(W)       IFNE      '51'
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END NE '51'
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Pay at 50% of MRA
     C     P3MRA         MULT      .50           XXMRA
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   ELSE                                                   ELSE EQ 2
     C*
     C* If HBPP,3 is not available, then disallow this charge
     C     N             IFEQ      4
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   ELSE                                                   ELSE N EQ 4
     C* If either HBPP,1 or HBPP,3 is available, then update workfile and array
     C*
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* If this workfile code has a modifier -51, then it must loaded into "2"
     C     WBPM(W)       IFEQ      '51'
     C                   MOVE      'P2'          P3BRPT                         BR Part
     C* Load both sections of body area
     C                   MOVE      P3PART        HBPP(3)
     C                   MOVE      P3PART        HBPP(4)
     C                   ELSE                                                   ELSE EQ '51'
     C* Else load into what is available
     C                   MOVE      HLBRPT        P3BRPT                         BR Part
     C*
     C* If this is loaded into the second Body Part, then give EOMB
     C     N             IFEQ      3
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END EQ 3
     C*
     C* Load both sections of body area
     C                   MOVE      P3PART        HBPP(N)
     C                   ADD       1             N
     C                   MOVE      P3PART        HBPP(N)
     C                   END                                                    END EQ '51'
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Determine whether to pay at 100% of MRA or 50% of MRA
     C     P3BRPT        IFEQ      'P1'
     C                   Z-ADD     P3MRA         XXMRA
     C                   ELSE                                                   ELSE EQ 'P1'
     C     P3BRPT        IFEQ      'P2'
     C     P3MRA         MULT      .50           XXMRA
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'P1'
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   END                                                    END N EQ 4
     C*
     C                   END                                                    END N EQ 2
     C*
     C                   ELSE                                                   ELSE WBP,W EQ '2 '
     C*
     C* If this is an EXTREMITY, then update workfile and array
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C*
     C* If this workfile code has a modifier -51, then it must loaded into "2"
     C     WBPM(W)       IFEQ      '51'
     C*
     C                   MOVE      'P2'          P3BRPT
     C* If this array has first Part Two available, then load it there
     C     N             IFEQ      3
     C                   MOVE      P3PART        HBPP(3)
     C                   ELSE                                                   ELSE N EQ 3
     C* Else, load into second Part Two
     C                   MOVE      P3PART        HBPP(4)
     C                   END                                                    END N EQ 3
     C*
     C*
     C                   ELSE                                                   ELSE EQ '51'
     C* Else load into what is available
     C                   MOVE      HLBRPT        P3BRPT                         BR Part
     C*
     C* If this is loaded into the second Body Part, then give EOMB
     C     N             IFEQ      3
     C     N             OREQ      4
     C                   MOVE      '272'         P3EOMB
     C*** EOMB Code 272: "Reimbursement for a second body area per
     C***                 reimbursement policy"
     C                   END                                                    END EQ 3
     C*
     C                   MOVE      P3PART        HBPP(N)
     C                   END                                                    END EQ '51'
     C*
     C* If this is second section of Body Part, then disallow
     C     N             IFEQ      2
     C     N             OREQ      4
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*
     C                   ELSE                                                   ELSE N EQ 2 or 4
     C*
     C                   MOVE      ' '           P3DAFL                         Disallowed Flag
     C*
     C* Determine whether to pay at 100% of MRA or 50% of MRA
     C     P3BRPT        IFEQ      'P1'
     C                   Z-ADD     P3MRA         XXMRA
     C                   ELSE                                                   ELSE EQ 'P1'
     C     P3BRPT        IFEQ      'P2'
     C     P3MRA         MULT      .50           XXMRA
     C                   END                                                    END EQ 'P2'
     C                   END                                                    END EQ 'P1'
     C*
     C     P3TOTC        COMP      XXMRA                              64
     C   64              Z-ADD     XXMRA         P3TOTA
     C  N64              Z-ADD     P3TOTC        P3TOTA
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     XXMRA         P3BRTC
     C     P3BRPC        IFEQ      '97530'
     C     P3CPT4        ANDNE     '97530'
     C     XRCPT4        ORGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C                   ADD       P3TOTP        P97530
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C     P3BRPC        IFEQ      '97531'
     C     P3CPT4        ANDNE     '97531'
     C                   ADD       P3TOTP        P97531
     C                   Z-ADD     P3MRA         HLMRA
     C                   END
     C*
     C                   END                                                    END N EQ 2 or 4
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C*
     C                   END                                                    END WBP,W EQ '2 '
     C*
     C                   ELSE                                                   ELSE 1 *IN62
     C* If there is no next available body part, then disallow this code
     C     CAKYP3        CHAIN     PCAPT3                             63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '005'         P3EOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in
     C***            reimbursement manual"
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN63
     C                   END                                                    END 1 *IN62
     C                   END                                                    END 1 *IN61
     C*
     C                   END                                                    1 DO 999 W
     C*
     C     NXT3C1        TAG
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Only 1 Additional Minutes Charge per body part is allowed
     C*
     C* Initialize Variables
     C                   Z-ADD     *ZEROS        B                              BP Array Counter
     C                   Z-ADD     *ZEROS        BPP                            BP Paid Amt Array
     C                   MOVE      *BLANKS       BP                             BP Array
     C                   Z-ADD     *ZEROS        BPC                            BP Chrg Amt Array
     C                   Z-ADD     *ZEROS        BP$                            BP MRA Amt Array
     C                   MOVE      *BLANKS       HBPP                           Hist BP paid
     C                   MOVE      *BLANKS       BPPART                         Body Part Data Struc
     C                   Z-ADD     *ZEROS        BPTOTP                         TOTP Data Struct
     C                   Z-ADD     *ZEROS        BPTOTC                         TOTC Data Struct
     C                   Z-ADD     *ZEROS        BPMRA                          MRA Data Struct
     C                   MOVE      *BLANKS       BPSORT                         Sort Data Struct
     C                   MOVE      *BLANKS       BPS                            BP Summary Array
     C*
     C                   Z-ADD     *ZEROS        WBPC                           Wrkf Chrg Array
     C                   MOVE      *BLANKS       WBP                            Wrkf Body Part Array
     C                   Z-ADD     *ZEROS        WBPO                           Wrkf Entry Ordr Arra
     C                   Z-ADD     *ZEROS        WBP#                           Wrkf Seq# Array
     C                   MOVE      *BLANKS       WBPM                           Wrkf Proc Mod Array
     C                   MOVE      *BLANKS       WFSORT                         Wrkf Sort Data Struc
     C                   Z-ADD     *ZEROS        WFTOTC                         Wrkf TOTC Data Struc
     C                   Z-ADD     *ZEROS        WFORDR                         Wrkf ORDR Data Struc
     C                   MOVE      *BLANKS       WFPART                         Wrkf PART Data Struc
     C                   Z-ADD     *ZEROS        WFDSEQ                         Wrkf DSEQ Data Struc
     C                   MOVE      *BLANKS       WFPMOD                         Wrkf PMOD Data Struc
     C                   MOVE      *BLANKS       WBPS                           Wrkf Summary Array
     C                   Z-ADD     0             W                              Wrkf Array Cntr
     C                   Z-ADD     1000          WO                             Wrkf Entry Order Cnt
     C                   MOVE      *BLANKS       HLBRPT
     C*
     C* Read History
     C                   MOVE      '97145'       XXCPT4
     C     CAKYP2        SETLL     PCA09
     C     CAKYP4        READE(N)  PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        ANDLE     '97541'
     C*
     C* If an additional time code:
     C     XRCPT4        IFEQ      '97145'
     C     XRCPT4        OREQ      '97531'
     C     XRCPT4        OREQ      '97541'
     C*
     C* If this is a paid history charge:
     C     XRTOTP        IFGT      *ZEROS
     C                   ADD       1             B
     C                   MOVE      XRPART        BP(B)
     C                   END                                                    END NE *ZEROS
     C*
     C                   END                                                    END EQ '97145'...
     C*
     C     CAKYP4        READE(N)  PCA09                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*   Sort BP array to determine what body parts have been paid
     C                   SORTA     BP
     C*
     C*   Determine what body parts have been paid
     C     1             DO        4             B
     C*
     C     BP(B)         IFNE      *BLANKS
     C*
     C*   If this was the TRUNK:
     C     BP(B)         IFEQ      '2 '
     C     BP(B)         OREQ      '  '
     C*
     C*   If the first Part One is free, then both Part Ones should be marked:
     C     HBPP(1)       IFEQ      '  '
     C                   MOVE      '2 '          HBPP(1)
     C                   MOVE      '2 '          HBPP(2)
     C                   ELSE                                                   ELSE HBPP,1 EQ ' '
     C     HBPP(3)       IFEQ      '  '
     C                   MOVE      '2 '          HBPP(3)
     C                   MOVE      '2 '          HBPP(4)
     C                   END                                                    END HBPP,3 EQ ' '
     C                   END                                                    END HBPP,1 EQ ' '
     C*
     C                   ELSE                                                   ELSE BP,B EQ '2 '
     C*   Else, If this was an EXTREMITY:
     C*   If the first Part One is free, then just one Part One should be marked
     C     HBPP(1)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(1)
     C                   ELSE                                                   ELSE HBPP,1 EQ ' '
     C*   If the second Part One is free:
     C     HBPP(2)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(2)
     C                   ELSE                                                   ELSE HBPP,2 EQ ' '
     C*   If the first Part Two is free:
     C     HBPP(3)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(3)
     C                   ELSE                                                   ELSE HBPP,3 EQ ' '
     C*   If the second Part Two is free:
     C     HBPP(4)       IFEQ      '  '
     C                   MOVE      BP(B)         HBPP(4)
     C                   END                                                    END HBPP,4 EQ ' '
     C                   END                                                    END HBPP,3 EQ ' '
     C                   END                                                    END HBPP,2 EQ ' '
     C                   END                                                    END HBPP,1 EQ ' '
     C*
     C                   END                                                    END BP,B  EQ '2 '
     C*
     C                   END                                                    END BP,B NE *BLANKS
     C*
     C                   END                                                    END DO 4 B
     C*
     C* Read the Workfile
     C     NXTAT         TAG
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C* Additional Minutes Codes only
     C     P2BRPC        IFEQ      '97145'
     C     P2BRPC        OREQ      '97531'
     C     P2BRPC        OREQ      '97541'
     C     P2DAFL        IFNE      'Y'
     C                   ADD       1             W
     C                   SUB       1             WO
     C                   Z-ADD     P2BRTC        WBPC(W)
     C                   Z-ADD     WO            WBPO(W)
     C                   MOVE      P2PART        WBP(W)
     C                   Z-ADD     P2DSEQ        WBP#(W)
     C                   MOVE      P2PMOD        WBPM(W)
     C                   END                                                     END P2DAFL NE 'Y'
     C*
     C                   END                                                     END EQ 97139...
     C*
     C                   READ      PCAPT2                                 60
     C                   END                                                     END DOWEQ 60
     C* Process all array records:
     C* Reset counter
     C                   Z-ADD     0             W                              Wrkf Array Cntr
     C*
     C*   Move into data structure/summary BP array - preset for sort
     C     1             DO        999           W
     C     WBPC(W)       IFEQ      *ZEROS
     C                   GOTO      SRT3D
     C                   END
     C                   Z-ADD     WBPC(W)       WFTOTC
     C                   Z-ADD     WBPO(W)       WFORDR
     C                   MOVEL     WBP(W)        WFPART
     C                   Z-ADD     WBP#(W)       WFDSEQ
     C                   MOVEL     WBPM(W)       WFPMOD
     C                   MOVE      WFSORT        WBPS(W)
     C                   END
     C*
     C*   Sort BP arrays by Body Part/Highest Charge or MRA
     C     SRT3D         TAG
     C                   SORTA     WBPS
     C*
     C*   Move back out into arrays
     C     1             DO        999           W
     C     WBPS(W)       IFEQ      *BLANKS
     C                   GOTO      NXT3D
     C                   END
     C                   MOVEL     WBPS(W)       WFSORT
     C                   Z-ADD     WFTOTC        WBPC(W)
     C                   Z-ADD     WFORDR        WBPO(W)
     C                   MOVEL     WFPART        WBP(W)
     C                   Z-ADD     WFDSEQ        WBP#(W)
     C                   MOVEL     WFPMOD        WBPM(W)
     C                   END
     C*
     C*   Only pay the first code from each body part
     C*   All records must be read so that they can each be paid
     C*   or disallowed, as applicable.
     C*
     C     NXT3D         TAG
     C     1             DO        999           W
     C*
     C     WBP(W)        IFEQ      *BLANKS
     C                   GOTO      NXT3D1
     C                   END
     C*
     C                   Z-ADD     WBP#(W)       XXDSEQ
     C*
     C*   Compare the workfile against what history has already paid
     C*
     C*   Determine if there are any body areas left to be paid
     C     *BLANKS       LOOKUP    HBPP                                   61
     C     *IN61         IFEQ      '0'
     C*
     C*   If both body areas have already been paid, then disallow
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     WBPM(W)       ANDEQ     P3PMOD                                       ADD 04/16/97
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '233'         P3EOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body part
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C*
     C                   ELSE                                                   ELSE 0 *IN61
     C*
     C*   If this is the trunk
     C     WBP(W)        IFEQ      '2 '
     C     WBP(W)        LOOKUP    HBPP                                   63
     C*   If the trunk has already been paid, then disallow this charge
     C     *IN63         IFEQ      '1'
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     WBPM(W)       ANDEQ     P3PMOD                                       ADD 04/16/97
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '233'         P3EOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body part
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C*
     C                   ELSE                                                   ELSE *IN63 EQ '1'
     C*
     C*   If the trunk has not already been paid, look to see if this
     C*   can be paid based upon body part available
     C                   Z-ADD     1             B
     C     *BLANKS       LOOKUP    HBPP(B)                                64
     C     *IN64         IFEQ      '1'
     C*   If this is the 2nd part of Body Area One, then go to 1st part
     C*   of Body Area Two.
     C     B             IFEQ      2
     C                   ADD       1             B
     C                   END                                                    END B EQ 2
     C     B             IFEQ      1
     C     B             OREQ      3
     C                   MOVE      WBP(W)        HBPP(B)
     C                   ADD       1             B
     C                   MOVE      WBP(W)        HBPP(B)
     C                   ELSE                                                   ELSE B EQ 1/3
     C*   If 2 sections of the body part are not available, then disallow
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     WBPM(W)       ANDEQ     P3PMOD                                       ADD 04/16/97
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '233'         P3EOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body part
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C                   END                                                    END B EQ 1/3
     C*
     C                   END                                                    END  *IN64 EQ '1'
     C*
     C                   END                                                    END *IN63 EQ '0'
     C*
     C                   ELSE                                                   ELSE WBP,W EQ '2 '
     C*
     C*   If this is an extremity:
     C     WBP(W)        LOOKUP    HBPP                                   63
     C*   If the extremity has already been paid, then disallow this charge
     C     *IN63         IFEQ      '1'
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     WBPM(W)       ANDEQ     P3PMOD                                       ADD 04/16/97
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '233'         P3EOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body part
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C*
     C                   ELSE                                                   ELSE *IN63 EQ '1'
     C*
     C*   If the extremity has not already been paid, look to see if this
     C*   can be paid based upon body part available
     C                   Z-ADD     1             B
     C     *BLANKS       LOOKUP    HBPP(B)                                64
     C     *IN64         IFEQ      '1'
     C                   MOVE      WBP(W)        HBPP(B)
     C                   END                                                    END  *IN64 EQ '1'
     C     B             IFEQ      2
     C     B             OREQ      4
     C*   If this is the 2nd section of the body area, then disallow
     C     CAKYP3        CHAIN     PCAPT3                             62
     C     *IN62         IFEQ      '0'
     C     WBPM(W)       ANDEQ     P3PMOD                                       ADD 04/16/97
     C                   Z-ADD     *ZERO         P3TOTA
     C                   Z-ADD     *ZERO         P3TOTP
     C     P3TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTC        P3TOTC
     C                   Z-ADD     *ZEROS        P3BRTC
     C                   MOVE      '233'         P3EOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body part
     C                   MOVE      'Y'           P3DAFL                         Disallowed Flag
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCAPT3
     C                   END                                                    END 0 *IN62
     C                   END                                                    END B EQ 2/4
     C*
     C                   END                                                    END *IN63 EQ '0'
     C*
     C                   END                                                    END WBP,W EQ '2 '
     C*
     C                   END                                                    END *IN61 EQ '0'
     C*
     C                   END                                                    END 1 DO 999 W
     C*
     C     NXT3D1        TAG
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*  Now that all workfile records have been processed and payment
     C*  has been determined...then update the "live" codes with the
     C*  values from the workfile.
     C*
     C*  The following fields should be set (or reset) because of
     C*  the potential errors in subroutines PHYS2, PTM, UPPT
     C*  (such as modifiers being changed in UPPT)
     C*         CATOTA
     C*         CAMRA
     C*         CAPAID
     C*         CAPMOD
     C*         CAEOMB
     C*
     C* Read entire workfile and update respective "live" record
     C*
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       P2EOMB
     C*
     C*  Using FACAPFL record, chain to FDCAP record to update
     C     CAKYA2        CHAIN     PCA09                              61
     C*
     C                   Z-ADD     P2TOTA        XRTOTA
     C                   Z-ADD     P2TOTD        XRTOTD
     C                   Z-ADD     P2TOTP        XRTOTP
     C******************** Z-ADDP2BRTC    XRBRTC
     C                   MOVEL     P2PMOD        XRPMOD
     C                   MOVEL     P2BRPC        XRBRPC
     C*
     C* Load "Best Reflects" EOMB Code into regular EOMB Code
     C     P2EOMB        IFLE      '199'
     C     P2BREC        ANDNE     *BLANKS
     C                   MOVE      P2BREC        P2EOMB
     C                   ELSE                                                   ELSE P2BREC NE *BLAN
     C* Otherwise, may have to establish as '002' EOMB Code
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C                   MOVE      '002'         P2EOMB
     C                   END                                                    END P2TOTD GT *ZERO
     C                   END                                                    END P2BREC NE *BLANK
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N61              UPDATE    PCA09
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     P2EOMB        IFNE      *BLANKS
     C*
     C*** BEG ADD *** 11/23/99 ****************************************
     C                   MOVE      P2EOMB        XPEOMB
     C*** END ADD *** 11/23/99 ****************************************
     C                   EXSR      EOMBP2
     C*
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   READ      PCAPT2                                 60
     C                   END                                                    END DOWEQ *IN60
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     PHYS3E        ENDSR
     C*
     *********************************************************************
     *** CALCULATE MRA BASED ON MODIFIER ENTERED                 P M O D *
     *********************************************************************
     C     PMOD          BEGSR
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CAPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01                              64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC            5 2
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHECK FOR ASSISTING SURGEON IF MODIFIER IS PRESENT.
     C*
     C     CAPMOD        IFEQ      '80'
     C*
     C** CHECK FOR ASSISTING SURGEON IF MODIFIER IS PRESENT.
     C*
     C     SXSURG        IFEQ      'Y'
     C*
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   ELSE
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C*
     C** SERVICE IS NOT REIMBURSEABLE UNDER THE WORKER'S COMP PROGRAM
     C*
     C     CAEOMB        IFEQ      *BLANK
     C                   MOVE      '057'         CAEOMB
     C                   MOVE      '057'         XPEOMB
     C*** EOMB 004: "Surgical assistant is not routinely allowed for this proc"
     C                   END
     C*
     C                   END
     C                   GOTO      PMODE
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** HANDLE THE PROFESSIONAL COMPONENT PROCESSING.
     C*
     C** IF THE MODIFER IS 26 CALCULATE THE CHARGE.
     C*
     C     CAPMOD        IFEQ      '26'
     C     DHPRCD        IFGE      '70000'
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        ORGE      '95819'
     C     DHPRCD        ANDLE     '95950'
     C*
     C***  Per pg. 84, XV, A, 4, d, 1
     C***  X-rays taken elsewhere are reviewed during an independent
     C***  medical examination (IME), medical visit or consultation.
     C***  Such review is included in the reimbursement for the evaluation
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN OFFICE VISITS.
     C*
     C                   MOVE      '90000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN2        TAG
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C  N54XRCPT4        IFEQ      '90000'
     C     XRCPT4        OREQ      '90010'
     C     XRCPT4        OREQ      '90015'
     C     XRCPT4        OREQ      '90017'
     C     XRCPT4        OREQ      '90020'
     C     XRCPT4        OREQ      '90030'
     C     XRCPT4        OREQ      '90040'
     C     XRCPT4        OREQ      '90050'
     C     XRCPT4        OREQ      '90060'
     C     XRCPT4        OREQ      '90070'
     C     XRCPT4        OREQ      '90080'
     C     XRCPT4        OREQ      '90100'
     C     XRCPT4        OREQ      '90110'
     C     XRCPT4        OREQ      '90115'
     C     XRCPT4        OREQ      '90130'
     C     XRCPT4        OREQ      '90140'
     C     XRCPT4        OREQ      '90150'
     C     XRCPT4        OREQ      '90160'
     C     XRCPT4        OREQ      '90170'
     C     XRCPT4        OREQ      '90200'
     C     XRCPT4        OREQ      '90215'
     C     XRCPT4        OREQ      '90220'
     C     XRCPT4        OREQ      '90240'
     C     XRCPT4        OREQ      '90250'
     C     XRCPT4        OREQ      '90260'
     C     XRCPT4        OREQ      '90270'
     C     XRCPT4        OREQ      '90280'
     C     XRCPT4        OREQ      '90300'
     C     XRCPT4        OREQ      '90315'
     C     XRCPT4        OREQ      '90320'
     C     XRCPT4        OREQ      '90340'
     C     XRCPT4        OREQ      '90350'
     C     XRCPT4        OREQ      '90360'
     C     XRCPT4        OREQ      '90370'
     C     XRCPT4        OREQ      '90400'
     C     XRCPT4        OREQ      '90415'
     C     XRCPT4        OREQ      '90420'
     C     XRCPT4        OREQ      '90440'
     C     XRCPT4        OREQ      '90450'
     C     XRCPT4        OREQ      '90460'
     C     XRCPT4        OREQ      '90470'
     C     XRCPT4        OREQ      '90500'
     C     XRCPT4        OREQ      '90505'
     C     XRCPT4        OREQ      '90510'
     C     XRCPT4        OREQ      '90515'
     C     XRCPT4        OREQ      '90517'
     C     XRCPT4        OREQ      '90530'
     C     XRCPT4        OREQ      '90540'
     C     XRCPT4        OREQ      '90550'
     C     XRCPT4        OREQ      '90560'
     C     XRCPT4        OREQ      '90570'
     C     XRCPT4        OREQ      '90620'
     C     XRCPT4        OREQ      '90635'
     C     XRCPT4        OREQ      '90640'
     C     XRCPT4        OREQ      '90641'
     C     XRCPT4        OREQ      '90642'
     C     XRCPT4        OREQ      '90643'
     C     XRCPT4        OREQ      '92002'
     C     XRCPT4        OREQ      '92004'
     C     XRCPT4        OREQ      '92012'
     C     XRCPT4        OREQ      '92014'
     C     XRCPT4        OREQ      '99080'
     C     XRCPT4        OREQ      '96320'
     C     XRCPT4        OREQ      '99201'
     C     XRCPT4        OREQ      '99202'
     C     XRCPT4        OREQ      '99203'
     C     XRCPT4        OREQ      '99204'
     C     XRCPT4        OREQ      '99205'
     C     XRCPT4        OREQ      '99211'
     C     XRCPT4        OREQ      '99212'
     C     XRCPT4        OREQ      '99213'
     C     XRCPT4        OREQ      '99214'
     C     XRCPT4        OREQ      '99215'
     C     XRCPT4        OREQ      '99341'
     C     XRCPT4        OREQ      '99342'
     C     XRCPT4        OREQ      '99343'
     C     XRCPT4        OREQ      '99351'
     C     XRCPT4        OREQ      '99352'
     C     XRCPT4        OREQ      '99353'
     C     XRCPT4        OREQ      '99221'
     C     XRCPT4        OREQ      '99222'
     C     XRCPT4        OREQ      '99223'
     C     XRCPT4        OREQ      '99231'
     C     XRCPT4        OREQ      '99232'
     C     XRCPT4        OREQ      '99233'
     C     XRCPT4        OREQ      '99301'
     C     XRCPT4        OREQ      '99302'
     C     XRCPT4        OREQ      '99303'
     C     XRCPT4        OREQ      '99311'
     C     XRCPT4        OREQ      '99312'
     C     XRCPT4        OREQ      '99313'
     C     XRCPT4        OREQ      '99321'
     C     XRCPT4        OREQ      '99322'
     C     XRCPT4        OREQ      '99323'
     C     XRCPT4        OREQ      '99331'
     C     XRCPT4        OREQ      '99332'
     C     XRCPT4        OREQ      '99333'
     C     XRCPT4        OREQ      '99281'
     C     XRCPT4        OREQ      '99282'
     C     XRCPT4        OREQ      '99283'
     C     XRCPT4        OREQ      '99284'
     C     XRCPT4        OREQ      '99285'
     C     XRCPT4        OREQ      '99244'
     C     XRCPT4        OREQ      '99254'
     C     XRCPT4        OREQ      '99261'
     C     XRCPT4        OREQ      '99262'
     C     XRCPT4        OREQ      '99263'
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C                   MOVE      '006'         XPEOMB            3
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
MG11 C                   GOTO      PMODE
     C*
     C                   ELSE
MG11 C                   GOTO      AGAIN2
     C                   END
     C*
     C*** Per pg. 63, IX, A, 5, c (for Neurology & Neuromuscular Services)
     C*** When a procedure code listed in Chpt 6 does not list a separate amount
     C*** for the professional component, reimbursement for the professional
     C*** component must not exceed one-third (1/3) of the total MRA.
     C     DHPRCD        IFGE      '95819'
     C     DHPRCD        ANDLE     '95950'
     C     SXPROF        IFEQ      *ZEROS
     C     SXMRA         DIV       3             XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GE/LE 95819/9595
     C*
     C                   Z-ADD     SXPROF        XXMRA                          CHG 07/26/91
     C                   GOTO      PMODE
     C                   ELSE
     C*
     C** ERROR - INVALID MODIFIER ENTERED CLEAR THE CHARGE, SND ERROR
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C                   GOTO      PMODE
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** HANDLE THE MICROSURGERY MODIFIER.
     C*
     C** IF THE MODIFER IS 20 INFORM USER.
     C*
     C     CAPMOD        IFEQ      '20'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '04'          QQC(Q)
     C*** Err Msg 04: "Thie item contains the microsurgery modifier"
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** HANDLE THE UNUSUAL SERVICES MODIFIER.
     C*
     C** IF THE MODIFER IS 22 INFORM USER.
     C*
     C     CAPMOD        IFEQ      '22'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C*
     C                   GOTO      PMODE
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C** IF THE MODIFER IS 52 PAY 50%
     C     CAPMOD        IFEQ      '52'
     C     SXMRA         MULT      .50           XXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHECK FOR 90 MODIFIER
     C*
     C     CAPMOD        IFEQ      '90'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     *ZERO         XPTOTD
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '23'          QQC(Q)
     C*** Err Msg 23: "Contains modifier 90, must pay doctors charges in full"
     C                   MOVE      CAEOMB        XPEOMB
     C*
     C                   GOTO      PMODE
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 13, VIII, B, 9, a, 2
     C*** Per pg. 63, IX, A, 5, b
     C*** Per pg. 83, XV, A, 3, d
     C*** Per pg. 84, XV, A, 4, e
     C*** The health care provider who performs the technical component bills
     C*** for the technical component by adding modifier -27 to the 5-digit
     C*** procedure code.  The reimbursement for the technical component is
     C*** the lesser of the provider's charge or the difference between the
     C*** MRA for the total procedure (MRA AMOUNT IN FMPRPFL)
     C*** and the MRA for the 5-digit procedure code plus modifier -26
     C*** (PROFESSIONAL COMPONENT AMOUNT IN FMPRPFL)
     C*
     C     CAPMOD        IFEQ      '27'
     C*
     C*** Per pg. 63, IX, A, 5, c (for Neurology & Neuromuscular Services)
     C*** When a procedure code listed in Chpt 6 does not list a separate amount
     C*** for the technical component, reimbursement for the technical
     C*** component must not exceed two-thirds (2/3) of the total MRA.
     C     DHPRCD        IFGE      '95819'
     C     DHPRCD        ANDLE     '95950'
     C     SXPROF        IFEQ      *ZEROS
     C     SXMRA         DIV       1.5           XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GE/LE 95819/9595
     C*
     C     SXMRA         SUB       SXPROF        XXMRA
     C                   GOTO      PMODE
     C                   END                                                    END EQ '27'
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C** IF THE MODIFER IS 48 PAY 50%
     C     CAPMOD        IFEQ      '48'
     C     XXMRA         MULT      .50           XXMRA
     C                   GOTO      PMODE
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** HANDLE ANY OTHER MODIFER.
     C*
     C*
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   GOTO      PMODE
     C*
     C*
     C     PMODE         ENDSR
     C*                                                                ***
     *********************************************************************
     *** DO ADDITIONAL MINUTES                                     P T M *
     *********************************************************************
     C*
     C     PTM           BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4   (97145,97221,97241,97501,97521,97541)
     C*** Per pg. 112, II, D, 2, c, 4    (97145,97221,97241,97501,97521,97541)
     C*** Per pg. 75, XIII, E, 2, b, 4   (97531)
     C*** Per pg. 112, II, D, 2, b, 4    (97531)
     C*
     C* HANDLE THE ADDITIONAL MINUTES FOR P/T.
     C*
     C     DHPRCD        IFGE      '97110'
     C     DHPRCD        ANDLE     '97139'
     C     DHPRCD        OREQ      '97500'
     C     DHPRCD        OREQ      '97520'
     C     DHPRCD        OREQ      '97530'
     C     DHPRCD        OREQ      '97540'
     C*
     C* SETUP THE KEY FOR ADDITIONAL MINUTES P/T.
     C*
     C     DHPRCD        IFGE      '97110'
     C     DHPRCD        ANDLE     '97139'
     C                   MOVE      '97145'       XXCPT4
     C                   END
     C*
     C*
     C     DHPRCD        IFEQ      '97500'
     C                   MOVE      '97501'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97520'
     C                   MOVE      '97521'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97530'
     C                   MOVE      '97531'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97540'
     C                   MOVE      '97541'       XXCPT4
     C                   END
     C     CAKEY8        CHAIN     PCA09                              64
     C*
MG3  C                   MOVE      '0'           *IN94
MG3  C     PXDSEQ        ADD       1             H1DSEQ            5 0          CHG 03/03/92
     C                   MOVE      PXFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     PMIND         TAG
     C*
     C     *IN64         IFEQ      '0'
     C*
     C** IF THE CURRENT TOTAL CHARGE IS GREATER THAN THE ONE FROM
     C** THE SEARCH - REDUCE THE SEARCH RECORD. IF THE SEARCH RECORD
     C** IS GREATER REDUCED THE CURRENT.
     C*
     C** GET THE NEW MRA
     C*
MG3  C*
MG3  C*** MUST ONLY PERFORM CALCULATIONS ON THE ADD. MINS. DIRECTLY AFTER
MG3  C*** THE PRINCIPAL PROCEDURE CODE.
MG3  C*
MG3  C     H1DSEQ        IFEQ      XRDSEQ                                       CHG 03/03/92
     C*
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS >3.
     C     XX            IFGT      3
     C                   Z-ADD     PRMRA         XRTOTA
     C                   END
     C*
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS 3.
     C     XX            IFEQ      3
     C                   Z-ADD     PRMRA         XRTOTA
     C                   END
     C* CALCULATE THE AMOUNT TO PAY. PAY 100% IF 'XX' IS 2.
     C*
     C     XX            IFEQ      2
     C                   Z-ADD     PRMRA         XRTOTA
     C                   END
     C*
     C     XRTOTC        IFLT      XRTOTA
     C                   Z-ADD     XRTOTC        XRTOTA
     C                   END
     C*
     C     XRTOTC        SUB       XRTOTA        XRTOTD
     C                   Z-ADD     XRTOTA        XRTOTP
     C******************** Z-ADDXRTOTA    XRBRTC           ADD 10/05/94
     C*
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XRTOTD        IFGT      *ZERO
     C     XREOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XREOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 76, XIII, E, 3, b:
     C*** Reimbursement for additional time is limited to 15 minutes for each
     C*** body area treated regardless of codes used
     C* Per Sharon at State - This is limited to 97110-97139 range w/ 97145
     C*
     C* DISALLOW ANY SECOND OR SUBSEQUENT ADDITIONAL MINUTES
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97145'       XXCPT4
     C     CAKEY8        SETLL     PCA06
     C     CAKEY7        READE     PCA06                                  64
     C*
     C     *IN64         DOWEQ     '0'
     C     X6CPT4        ANDEQ     '97145'
     C*
     C*
     C     X6TOTP        IFGT      *ZEROS
     C     X6DSEQ        ANDNE     XRDSEQ
     C                   ADD       1             COUNT
     C                   END
     C*
     C     CAKEY7        READE     PCA06                                  64
     C                   END
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XRTOTA
     C     XRTOTC        SUB       XRTOTA        XRTOTD
     C                   Z-ADD     XRTOTA        XRTOTP
     C******************** Z-ADD*ZEROS    XRBRTC
     C                   MOVE      '233'         XREOMB
     C*** EOMB 233: "Additional time is limited to 15 minutes for each body
     C***            area treated"
     C                   END                                                    END COUNT GE 1
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
     C                   UPDATE    PCA09
     C*
     C                   MOVE      '1'           *IN94
     C                   GOTO      PTMND
     C                   END
     C*
     C     CAKEY8        READE     PCA09                                  64
     C  N64              GOTO      PMIND
     C                   END
     C*
     C                   END
     C*
     C     PTMND         ENDSR
     *********************************************************************
     *** RADIOLOGY SUBROUTINE  CODES 70010 - 78990-26            R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C*** VALID MODIFIERS ARE "22","25","26","52","55","76", AND "77".
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 84, XV, A, 4, g
     C*** Certain radiology studies have procedure codes which list complete
     C*** body areas, as well as the individual sections of body areas, as
     C*** separate entities.  When X-rays of multiple sections of a body area
     C*** are billed separately, the total reimbursement must not exceed the
     C*** provider's charge or the MRA of the complete body area study,
     C*** whichever is less.
     C*
     C                   Z-ADD     1             R                 5 0
     C     RADLOK        TAG
     C     DHPRCD        LOOKUP    RAD1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      RAD2(R)       XXCPT4
     C*
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END
     C*
     C     R             IFLT      95
     C                   ADD       1             R
     C                   GOTO      RADLOK
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 80, XV, A, 2, c
     C*** Procedure Code 76140 may be used when there is a documented need for a
     C*** radiology consultation and must be used ONLY by radiologists or other
     C*** physicians who are certified to perform radiological services.
     C*
     C     DHPRCD        IFEQ      '76140'
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** Only allow 76140 if Provider's Specialty if 'RAD'
     C***   Else, disallow procedure with Err msg 01 and EOMB 238
     C     QQSPCD        IFNE      'RAD'                                        ADD 12/28/92
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C*** Err Msg 01: "This bill requires further inspection by an adjuster"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '238'         XPEOMB
     C*** EOMB 238: "This procedure may be used by a radiologist only"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END EQ 'RAD'
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   END                                                    END 76140
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 83, XV, A, 4, c
     C*** Should the provider fail to add modifier -26 to a radiology procedure
     C*** provided in a hospital, ambulatory surgical center or similiar
     C*** facility, reimbursement must only be made for the professional
     C*** component.
     C*
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      'B'
     C                   MOVE      CAPMOD        H2PMOD                         ADD 12/03/92
     C                   MOVE      '26'          CAPMOD
     C                   MOVE      '236'         XPEOMB
     C*** EOMB 236: "Based upon provider's facility, reimbursement is made for
     C***            the professional component"
     C                   END                                                    END EQ 1/2/B
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 81-83, XV, A, 2, e/f  and  3, b/c
     C*** CPT Modifiers commonly used by radiologists...
     C*
     C** CHECK FOR VALID MODIFIER
     C*
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '26'
     C     CAPMOD        ANDNE     '27'                                         ADD 5/23/91
     C     CAPMOD        ANDNE     '52'
     C     CAPMOD        ANDNE     '55'
     C     CAPMOD        ANDNE     '76'
     C     CAPMOD        ANDNE     '77'
     C*
     C** ERROR - INVALID MODIFIER ENTERED CLEAR THE CHARGE, SND ERROR
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELSEQ 22
     C*
     C     CAPMOD        IFEQ      '26'
     C*
     C*
     C*** Code 72241-26 should be paid as 72240
     C     DHPRCD        IFEQ      '72241'
     C                   MOVE      '72240'       XXCPT4
     C                   MOVE      '240'         XPEOMB
     C*** EOMB 240: "This procedure was paid as 72240"
     C     PRKEY         SETGT     PPR01
     C     NXTRD0        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD0
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72246
     C*
     C*** Code 72256-26 should be paid as 72255
     C     DHPRCD        IFEQ      '72256'
     C                   MOVE      '72255'       XXCPT4
     C                   MOVE      '241'         XPEOMB
     C*** EOMB 241: "This procedure was paid as 72255"
     C     PRKEY         SETGT     PPR01
     C     NXTRD1        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD1
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72256
     C*
     C*** Code 72266-26 should be paid as 72265
     C     DHPRCD        IFEQ      '72266'
     C                   MOVE      '72265'       XXCPT4
     C                   MOVE      '242'         XPEOMB
     C*** EOMB 242: "This procedure was paid as 72265"
     C     PRKEY         SETGT     PPR01
     C     NXTRD2        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD2
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72266
     C*
     C*** Code 72271-26 should be paid as 72270
     C     DHPRCD        IFEQ      '72271'
     C                   MOVE      '72270'       XXCPT4
     C                   MOVE      '243'         XPEOMB
     C*** EOMB 243: "This procedure was paid as 72270"
     C     PRKEY         SETGT     PPR01
     C     NXTRD3        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD3
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72270
     C*
     C*** Code 72286-26 should be paid as 72285
     C     DHPRCD        IFEQ      '72286'
     C                   MOVE      '72285'       XXCPT4
     C**********           MOVE '154'     XPEOMB
     C                   MOVE      '244'         XPEOMB
     C*** EOMB 244: "This procedure was paid as 72285"
     C     PRKEY         SETGT     PPR01
     C     NXTRD4        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD4
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72286
     C*
     C*** Code 72296-26 should be paid as 72295
     C     DHPRCD        IFEQ      '72296'
     C                   MOVE      '72295'       XXCPT4
     C                   MOVE      '245'         XPEOMB
     C*** EOMB 245: "This procedure was paid as 72295"
     C     PRKEY         SETGT     PPR01
     C     NXTRD5        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD5
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 72296
     C*
     C*** Code 73041-26 should be paid as 73040
     C     DHPRCD        IFEQ      '73041'
     C                   MOVE      '73040'       XXCPT4
     C**********           MOVE '156'     XPEOMB
     C                   MOVE      '246'         XPEOMB
     C*** EOMB 246: "This procedure was paid as 73040"
     C     PRKEY         SETGT     PPR01
     C     NXTRD6        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD6
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 73041
     C*
     C*** Code 73086-26 should be paid as 73085
     C     DHPRCD        IFEQ      '73086'
     C                   MOVE      '73085'       XXCPT4
     C                   MOVE      '247'         XPEOMB
     C*** EOMB 247: "This procedure was paid as 73085"
     C     PRKEY         SETGT     PPR01
     C     NXTRD7        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD7
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 73086
     C*
     C*** Code 73116-26 should be paid as 73115
     C     DHPRCD        IFEQ      '73116'
     C                   MOVE      '73115'       XXCPT4
     C                   MOVE      '248'         XPEOMB
     C*** EOMB 248: "This procedure was paid as 73115"
     C     PRKEY         SETGT     PPR01
     C     NXTRD8        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD8
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 73116
     C*
     C*** Code 73526-26 should be paid as 73525
     C     DHPRCD        IFEQ      '73526'
     C                   MOVE      '73525'       XXCPT4
     C                   MOVE      '249'         XPEOMB
     C*** EOMB 249: "This procedure was paid as 73525"
     C     PRKEY         SETGT     PPR01
     C     NXTRD9        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRD9
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 73526
     C*
     C*** Code 73581-26 should be paid as 73580
     C     DHPRCD        IFEQ      '73581'
     C                   MOVE      '73580'       XXCPT4
     C                   MOVE      '250'         XPEOMB
     C*** EOMB 250: "This procedure was paid as 73580"
     C     PRKEY         SETGT     PPR01
     C     NXTRDA        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRDA
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 76581
     C*
     C*** Code 73616-26 should be paid as 73615
     C     DHPRCD        IFEQ      '73616'
     C                   MOVE      '73615'       XXCPT4
     C                   MOVE      '251'         XPEOMB
     C*** EOMB 251: "This procedure was paid as 73615"
     C     PRKEY         SETGT     PPR01
     C     NXTRDB        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTRDB
     C                   ELSE                                                   ELSE 0 *IN54
     C                   Z-ADD     *ZERO         SXMRA
     C                   END                                                    END 0 *IN54
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C                   GOTO      RADUPD
     C                   END                                                    END EQ 73616
     C*
     C                   END                                                    END CAPMOD=26
     C*
     C                   EXSR      PMOD
     C*** If Mod -26 and Prof Component Flag = A, then it is ANE
     C     CAPMOD        IFEQ      '26'
     C     SX1A1         ANDEQ     'A'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C*** Err Msg 24: "This item is ANE - requires further inspection
     C***              by an adjuster"
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTD
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    ENDEQ 26/'A'
     C*
     C* CHAIN TO DETAIL CHARGE FILE, IF YOU FIND A CHARGE W/O MOD 26
     C* THAT HAS BEEN PAID, THEN DENY THIS (CURRENT) CHARGE AND PRINT
     C* MESSAGE (ERROR MSG #29, EOMB #222) THAT SAYS "THIS PROCEDURE
     C* WAS PREVIOUSLY PAID UNDER A COMPLETE CODE"
     C     CAPMOD        IFEQ      '26'
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
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '222'         XPEOMB
     C*** EOMB 222: "This procedure was previously paid under a complete code"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ 26
     C*** If Mod -26 and AND NO PROF COMP AMOUNT GIVE EOMB CODE
     C     CAPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      '277'         XPEOMB
     C*** EOMB 277: "Charge was submitted with incorrect procedure cod modifier
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    ENDEQ 26
     C*
     C                   END                                                    ENDNE 22
     C                   END                                                    ENDNE *ZERO
     C*
     C     RADUPD        TAG                                                    ADD 03/03/92
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     RADIE         ENDSR
     *********************************************************************
     *** PHYSICAL RECONITIONING CODE 97801-97851               R E C O N *
     *********************************************************************
     C     RECON         BEGSR
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Per pg. 3, 4
     C* PHYSICAL RECONDITIONING SHALL NOT BE AUTHORIZED UNLESS THE DATE OF
     C* SERVICE IS AT LEAST 30 DAYS AFTER THE ACCIDENT DATE
     C*
     C                   Z-ADD     QQACDT        SRDATE
     C                   Z-ADD     30            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKACDT            7 0
     C     CAFDAT        IFLE      WKACDT
     C* DISALLOW
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '278'         XPEOMB
     C*** EOMB 278: "Phys reconditioning must be within 30 to 180 days after the
     C***           accident."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RECONE
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Per pg. 3, 4
     C* PHYSICAL RECONDITIONING SHALL NOT BE AUTHORIZED UNLESS THE DATE OF
     C* SERVICE IS LESS THAN 180 DAYS AFTER THE ACCIDENT DATE
     C*
     C                   Z-ADD     QQACDT        SRDATE
     C                   Z-ADD     180           SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKACDT            7 0
     C     CAFDAT        IFGE      WKACDT
     C* DISALLOW
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '278'         XPEOMB
     C*** EOMB 278: "Phys reconditioning must be within 30 to 180 days after the
     C***           accident."
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RECONE
     C*
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     RECONE        ENDSR
     *********************************************************************
     *** SPECIAL SERVICES AND REPORTS CODES 99000 - 99195        S P E C *
     *********************************************************************
     C     SPEC          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C*
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C*
     C     SPECE         ENDSR
     *********************************************************************
     *** SURGERY SUBROUTINE    CODES 10020 - 69646               S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*
     C*** Per Pg. 129, VI, B, 1, b:
     C*** Reimburse either the provider's usual charge or the ARA listed
     C*** in TABLE 3 (page 133)
     C     CAPMOD        IFEQ      '47'
     C*
     C     DHAREA        IFEQ      'A1'
     C     DHAREA        OREQ      'A2'
     C     DHAREA        OREQ      'A3'
     C                   Z-ADD     133.00        XPTOTA
     C                   Z-ADD     XPTOTA        XPBRTC                          ADD 08/22/94
     C     XPTOTA        IFLT      CACHRG
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE LT CACHRG
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   END                                                    END LT CACHRG
     C*
     C                   ELSE                                                   ELSE EQ A1/A2/A3
     C     DHAREA        IFEQ      'A4'
     C     DHAREA        OREQ      'A5'
     C     DHAREA        OREQ      'A6'
     C                   Z-ADD     79.80         XPTOTA
     C                   Z-ADD     XPTOTA        XPBRTC                          ADD 08/22/94
     C     XPTOTA        IFLT      CACHRG
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE LT CACHRG
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   END                                                    END LT CACHRG
     C*
     C                   ELSE                                                   ELSE EQ A4/A5/A6
     C                   Z-ADD     CACHRG        XPTOTD                         07/16/91 CHG
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '   '         XPEOMB
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '18'          QQC(Q)
     C*** Err Msg 18: "See Table in Fee Schedule Manual for Allowed Amount"
     C                   END                                                    END EQ A4/A5/A6
     C                   END                                                    END EQ A1/A2/A3
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END EQ '47'
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 87, XVI, A, 3
     C*** Replacement casting and strapping codes (29000-29799) must not
     C*** be used for initial care.
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29799'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN INITIAL VISIT.
     C*
     C                   MOVE      '90000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '90630'
     C*
     C     XRCPT4        IFGE      '90000'
     C     XRCPT4        ANDLE     '90026'
     C     XRCPT4        ORGE      '90100'
     C     XRCPT4        ANDLE     '90115'
     C     XRCPT4        ORGE      '90200'
     C     XRCPT4        ANDLE     '90220'
     C     XRCPT4        ORGE      '90300'
     C     XRCPT4        ANDLE     '90320'
     C     XRCPT4        ORGE      '90400'
     C     XRCPT4        ANDLE     '90420'
     C     XRCPT4        ORGE      '90500'
     C     XRCPT4        ANDLE     '90517'
     C     XRCPT4        ORGE      '90600'                                      6/25/89 ADD
     C     XRCPT4        ANDLE     '90630'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   END
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFLE      '99353'
     C*
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99205'
     C     XRCPT4        ORGE      '99341'
     C     XRCPT4        ANDLE     '99342'
     C     XRCPT4        ORGE      '99221'
     C     XRCPT4        ANDLE     '99223'
     C     XRCPT4        ORGE      '99301'
     C     XRCPT4        ANDLE     '99303'
     C     XRCPT4        ORGE      '99321'
     C     XRCPT4        ANDLE     '99323'
     C     XRCPT4        ORGE      '99281'
     C     XRCPT4        ANDLE     '99285'
     C     XRCPT4        ORGE      '99241'
     C     XRCPT4        ANDLE     '99245'
     C     XRCPT4        ORGE      '99251'
     C     XRCPT4        ANDLE     '99255'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C     CAKEY7        READE(N)  PCA09                                  54
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 98, II, F, 1, b, 2
     C*** The reimbursement for the second additional or lesser procedure(s)
     C*** (modifier -51) must be the provider's usual charge or 50 percent
     C*** of MRA, whichever is less.
     C*
     C*** HANDLE MODIFIERS 50 & 51 TO CUT CHARGES BY 50%.
     C*
     C     CAPMOD        IFEQ      '50'
     C     CAPMOD        OREQ      '51'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA             9 2
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     XXMRA         DIV(H)    2.00          XPTOTA
     C*
     C                   Z-ADD     XPTOTA        XPBRTC                          ADD 08/22/94
     C     CACHRG        IFLT      XPTOTA
     C                   Z-ADD     CACHRG        XPTOTA
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
     C** SURGICAL PROCEDURES (10000-69999) PAY HIGHEST $ AMOUNT AT
     C** 100% , ALL OTHER PAY 50% AND FORCE MODIFIER 51.
     C*
     C*
     C*** EXCEPTIONS ARE IN THE 3 FOLLOWING TABLES NC1,NC2, AND TAB1/2.
     C*** IF THE CURRENT CODE IS FOUND IN ANY OF THESE TABLES, THE ITEM
     C*** CAN NOT BE CUT BY THIS RULE AND MUST BYPASS THIS SECTION.
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC2(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C     DHPRCD        LOOKUP    TABA                                   60
     C     *IN60         CABEQ     '1'           NOCUT
     C     DHPRCD        LOOKUP    TABB          TABA                     60
     C     *IN60         IFEQ      '1'
     C                   MOVE      TABA          XXCPT4
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         CABEQ     '0'           NOCUT
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg.
     C*
     C*** DISALLOW RE-APPLICATION OF CASTS IF ANY OTHER SURIGCAL PROCEDURE
     C*** IS PERFORMED ON SAME DAY. (EXCEPTION {OF COURSE} IS CODES 15510-
     C*** 15730 AND 29000-29790).
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29790'
     C*
     C                   MOVE      '10020'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLT     '15510'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C*
     C                   MOVE      '15731'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFGT      '15730'
     C     XRCPT4        ANDLT     '29000'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '29791'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE(N)  PCA09                                  54
     C     *IN54         IFEQ      '0'
     C*
     C                   Z-ADD     XRCASE        XXCASE
     C                   Z-ADD     XREOB#        XXEOB#
     C                   Z-ADD     XRDSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XRCPT4
     C*
     C     XRCPT4        IFGT      '29790'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 98, II, F, b, 2
     C*** The reimbursement for the second additional or lesser procedure(s)
     C*** (modifier -51) must be the provider's usual charge or 50 percent
     C*** of the MRA, whichever is less.
     C*
     C** IF THE PROCEDURE CODE IS BETWEEN 10000 AND 69999 AND THE
     C** MODIFIER IS ZERO CUT THE CHG IF A LARGER ONE EXISTS.
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg.
     C*
     C     NXTPR5        TAG
     C*                    READPPPR01                    60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR5
     C                   ELSE
     C                   Z-ADD     0             PRMRA
     C                   END
     C     NXTPR6        TAG
     C*                    READPPPR01                    60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR6
     C                   ELSE
     C                   Z-ADD     0             PRMRA
     C                   END
     C*
     C*
     C*  If the procedure code is between 10000 and 69999 and the
     C*  modifier is zero, write to work file
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C     CAPMOD        ANDEQ     *BLANK
     C                   MOVE      'S'           CATYPE
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   MOVE      *BLANKS       CAEOMB
     C                   WRITE     PCAPT1
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     NOCUT         TAG
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 90, XIX, C
     C*** Reimbursement for TNS is limited to four sessions
     C*
     C     DHPRCD        IFEQ      '64550'
     C                   MOVE      '64550'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              54
     C                   Z-ADD     0             COUNT             5 0
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END
     C*
     C     CAKYC         READE     PCA0C                                  54
     C                   END
     C*
     C     COUNT         IFGE      4
     C***    DISSALLOW CHARGES WHEN LIMIT HAS BEEN REACHED
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '239'         XPEOMB
     C*** EOMB 239: "These services not reimbursable under the Fee Schedule"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per codes in Chapter 6: "Each additional..."
     C*
     C*** IF CURRENT CODE EXISTS IN TABLE, MUST HAVE ASSOCIATED CODE ELSE
     C*** ERROR HAS OCCURED.
     C*
     C                   MOVE      ' '           NOMTCH            1
     C                   Z-ADD     1             T
     C*
     C                   DO        19            T
     C     DHPRCD        LOOKUP    TABA          TABB                     60
     C     *IN60         IFEQ      '1'
     C                   MOVE      TABB          XXCPT4
     C     CAKEY8        CHAIN(N)  PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   MOVE      'Y'           NOMTCH
     C                   ELSE                                                   ELS 1 *IN54
     C                   MOVE      ' '           NOMTCH
     C                   GOTO      MTCH
     C                   END                                                    END 1 *IN54
     C                   END                                                    END 1 *IN60
     C                   END                                                    END DO 19 T
     C*
     C*** If no match on other codes, pay zero for this code
     C     MTCH          TAG
     C     NOMTCH        IFEQ      'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C*** Err Msg 16: "A Major code must be present to pay this item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END NOMTCH = Y
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** All Other Codes To be handled here.
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     CAPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 08/22/94
     C*
MG11 C                   EXSR      UPDAT
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
     C*** Per pg. 98, II, F, b, 2
     C*** The reimbursement for the second additional or lesser procedure(s)
     C*** (modifier -51) must be the provider's usual charge or 50 percent
     C*** of the MRA, whichever is less.
     C*
     C** IF THE PROCEDURE CODE IS BETWEEN 10000 AND 69999 AND THE
     C** MODIFIER IS ZERO CUT THE CHG IF A LARGER ONE EXISTS.
     C*
     C*
     C                   Z-ADD     0             COUNT
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or 50%
     C     *LOVAL        SETLL     PCAPT4
     C                   READ      PCAPT4                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C                   Z-ADD     P4CASE        XXCASE
     C                   Z-ADD     P4EOB#        XXEOB#
     C                   Z-ADD     P4DSEQ        XXDSEQ
     C*
     C     DHKY1         CHAIN     PDH02                              61
     C*
     C                   MOVE      DXPRCD        XXCPT4
     C*
     C     PRKEY         SETGT     PPR01
     C     NXTPR7        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR7
     C                   ELSE
     C                   Z-ADD     0             PRMRA
     C                   END
     C*
     C                   ADD       1             COUNT
     C                   Z-ADD     PRMRA         ZZMRA             9 2          Chrg's MRA
     C*
     C     COUNT         IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      '88'          QQC(Q)
     C* Err Msg 88: "If this procedure is located at a different site,
     C*              override and pay at 100%"
     C                   END                                                    END  GE 2
     C*
     C     P4TOTC        IFGT      ZZMRA
     C                   Z-ADD     ZZMRA         XPTOTP
     C     P4TOTC        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE P4TOTC GT ZZMRA
     C                   Z-ADD     P4TOTC        XPTOTP
     C     P4TOTC        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   END                                                    END P4TOTC GT ZZMRA
     C                   MOVE      P4PMOD        XPPMOD                         ADD 04/29/96
     C                   Z-ADD     ZZMRA         XPBRTC
     C                   EXSR      UPDAT4
     C*
     C                   READ      PCAPT4                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C*
     C     SURG2E        ENDSR
     C*** END ADD *** 09/27/94 *************************************************
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** WHEN PROCEDURE CODE IS 99080 - REFER BILL TO ADJUSTER.
     C*
     C     DHPRCD        IFEQ      '99080'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C*** Err Msg 01: "This bill requires further inspection by an adjustor"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** WHEN PROCEDURE CODE 29000 - 29999, DISPLAY ERROR, MIGHT BE INITAL
     C** CASTS.  LET OPERATOR KNOW THIS.
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29799'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '07'          QQC(Q)
     C*** Err Msg 07: "The item could be an initial cast,
     C***              adjust the paid amount accordingly"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        XXDSEQ
     C*
     C     CAKYP3        CHAIN(N)  PCA0F                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C*** BEG ADD *** 11/04/99 ****************************************
     C     PXDSEQ        ANDNE     CADSEQ
     C*** END ADD *** 11/04/99 ****************************************
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C     DHPRCD        IFGE      '90000'
     C     DHPRCD        ANDLE     '90654'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99353'
     C                   EXSR      DUPOFC
     C                   END
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPBRTC        CABRTC                          ADD 08/22/94
     C                   MOVE      XPPMOD        CAPMOD
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*
     C                   UPDATE    PCA01
     C*** IF FEE SCHEDULE NON-REIMBURSEABLE ... DISPLAY ERROR MESSAGE
     C*** XX25EM FLAG IS SET AT BEGINNING OF PROGRAM WHERE PRFSR EQ 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY8         CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C*** Err Msg 25: "This item is non-reimburseable under the Fee Schedule"
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN54
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 08/22/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     C*
     C********************************************************************
     C** UPDATE SUBROUTINE FOR SURG  FILE FACAPFL             U P D A T4 *
     C********************************************************************
     C     UPDAT4        BEGSR
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
     C* Call subroutine to write EOMB to FDDEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB4
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C*
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C     P4CPT4        IFNE      *BLANK
     C                   Z-ADD     P4FEIN        XXFEIN
     C                   Z-ADD     P4MOD         XXMOD
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
     C     CAKYP3        CHAIN(N)  PCA0F                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     P4FDAT
     C     PXFEIN        ANDEQ     P4FEIN
     C     P4DSEQ        ANDNE     PXDSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C                   Z-ADD     SVYMD         XXYMD
     C*
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*  Using FACAPFL record, chain to FDCAP record to update
     C     CAKYA4        CHAIN     PCA09                              54
     C*
     C                   Z-ADD     XPTOTA        XRTOTA
     C                   Z-ADD     XPTOTD        XRTOTD
     C                   Z-ADD     XPTOTP        XRTOTP
     C******************   Z-ADDXPBRTC    XRBRTC
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
     C  N54              UPDATE    PCA09
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
     C     CAKY84        CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C* Err Msg 25: "This item is non-reimburseable under the Fee Sched
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN54
     C                   Z-ADD     SVYMD         XXYMD
     C                   END                                                    ENDEQ '1'
     C*
     C* ---------------------------------------------------------------
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPBRTC
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE4        ENDSR
     *********************************************************************
     *********************************************************************
     *** UPDATE PHYS THERAPY THROUGH PT ARRAYS                   U P P T *
     *********************************************************************
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST THE PT ARRAYS AND SEND MESSAGE IF AN ERROR OCCURED.
     C** IF NO ERROR HAS OCCURED UPDATE THE FILE.
     C*
     C     UPPT          BEGSR
     C*
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C**  PROCESS THE ARRAY AGAIN TO UPDATE.
     C*
     C     1             DO        30            N                 3 0
     C*
     C**  PROCESS THE ARRAY ELEMENTS WHICH HAVE BEEN PAID.
     C*
     C     AJC(N)        IFNE      'P'
     C*
     C*
     C                   Z-ADD     PT3(N)        XXDSEQ
     C*
     C     CAKEY1        CHAIN     PCA0F                              64
     C*
     C     *IN64         IFEQ      '0'
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C*
     C********** DHPRCD    IFNE '97145'
     C********** DHPRCD    ANDNE'97221'
     C********** DHPRCD    ANDNE'97241'
     C********** DHPRCD    ANDNE'97501'
     C********** DHPRCD    ANDNE'97521'
     C********** DHPRCD    ANDNE'97531'
     C********** DHPRCD    ANDNE'97541'
     C     DHKY          CHAIN     PDH02                              61
     C*
     C     DXPRCD        IFNE      '97145'
     C     DXPRCD        ANDNE     '97221'
     C     DXPRCD        ANDNE     '97241'
     C     DXPRCD        ANDNE     '97501'
     C     DXPRCD        ANDNE     '97521'
     C     DXPRCD        ANDNE     '97531'
     C     DXPRCD        ANDNE     '97541'
     C*** END CHG *** 04/03/00 ****************************************
     C*
     C                   Z-ADD     AJ2(N)        PXTOTA
     C                   Z-ADD     AJ2(N)        PXBRTC                         ADD 08/22/94
     C                   MOVE      AJM(N)        PXPMOD
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     PXTOTA        PXTOTP
     C*
     C     PXEOMB        IFEQ      *BLANK
     C     PXTOTD        ANDGT     *ZERO
     C                   MOVE      '002'         PXEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C                   MOVE      Q1USER        PXUSER
     C                   MOVE      Q1PGM         PXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   Z-ADD     DSTIME        PXTIME
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C* Per pg. 74, XIII, E, 2, b, 1
     C* Per pg. 111, II, d, 2, b, 1
     C* Workers' compensation has designated the use of Procedure Code 97530
     C* to bille for any combination (two or more) of the modalities or
     C* procedures (Procedure Codes 97010-97139) when applied to a single
     C* body area.
     C*
     C                   Z-ADD     *ZEROS        P97530           10 2          ACCUM 97530 PAID
     C                   Z-ADD     *ZEROS        L97530           10 2          AMT LEFT OF 97530
     C*
     C*** CHECK FOR MULTIPLE PHYS RECORDS.  (ONLY ONE 97530 ALLOWED ON SAME
     C*** DAY WHEN SAME BODY PART - PAY 0 FOR SECOND CHARGE ...
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97530'
     C**********           MOVE DHPRCD    XXCPT4
     C**********           MOVE DHAREA    XXPART
     C     DXPRCD        IFEQ      '97530'
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      DXAREA        XXPART
     C*** END CHG *** 04/03/00 ****************************************
     C                   Z-ADD     0             COUNT             5 0
     C*
     C     CAKY6A        CHAIN     PCA06                              54
     C     *IN54         DOWEQ     '0'
     C*
     C     PXDSEQ        IFNE      X6DSEQ
     C     X6TOTP        ANDGT     *ZERO
     C                   ADD       X6TOTP        P97530
     C                   END                                                    END NE/EQ/NE
     C*
     C     CAKY6A        READE     PCA06                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*** DISSALLOW CHARGES
     C*** GET MRA FOR PROCEDURE CODE
     C*** BEG CHG *** 04/03/00 ****************************************
     C******************** MOVELDHPRCD    XXCPT4
     C                   MOVEL     DXPRCD        XXCPT4
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      CAFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PXPMOD        IFEQ      '51'
     C     PRMRA         DIV       2             SXMRA
     C                   ELSE                                                   ELS EQ '51'
     C                   Z-ADD     PRMRA         SXMRA
     C                   END                                                    END EQ '51'
     C                   END                                                    END 0 *IN60
     C     SXMRA         SUB       P97530        L97530
     C     L97530        IFLE      *ZERO
     C                   Z-ADD     *ZERO         PXTOTA
     C                   Z-ADD     *ZERO         PXTOTP
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     PXTOTC        PXTOTC
     C                   Z-ADD     *ZEROS        PXBRTC
     C                   MOVE      '005'         PXEOMB
     C*** EOMB 009: "Exceeds number of allowable procedures in reimburse manual"
     C                   ELSE                                                   ELSE L97530 LE *ZERO
     C     PXTOTC        IFGT      L97530
     C                   Z-ADD     L97530        PXTOTA
     C                   Z-ADD     L97530        PXTOTP
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     PXTOTC        PXTOTC
     C                   Z-ADD     L97530        PXBRTC
     C                   MOVE      '002'         PXEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE CACHRG GT L9750
     C                   Z-ADD     PXTOTC        PXTOTA
     C                   Z-ADD     PXTOTA        PXTOTP
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     L97530        PXBRTC
     C                   Z-ADD     PXTOTC        PXTOTC
     C                   MOVE      *BLANKS       PXEOMB
     C                   END                                                    END CACHRG GT L97503
     C                   END                                                    END L97503 LE *ZERO
     C                   END                                                    END EQ 97530
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         PXMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA0F
     C*
     C                   ELSE
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C******************** MOVE DHAREA    CPTPRT  2
     C                   MOVE      DXAREA        CPTPRT            2
     C*** END CHG *** 04/03/00 ****************************************
     C*
     C* SETUP THE KEY FOR ADDITIONAL MINUTES P/T.
     C*
     C*** Per pg. 74, XIII, E, 2, a, 3
     C*** Per pg. 111, II, D, 2, a, 3
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97145'
     C     DXPRCD        IFEQ      '97145'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97110'       CPTFR             5
     C                   MOVE      '97139'       CPTTO             5
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4
     C*** Per pg. 112, II, D, 2, c, 4
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97221'
     C     DXPRCD        IFEQ      '97221'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97220'       CPTFR             5
     C                   MOVE      '97220'       CPTTO             5
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4
     C*** Per pg. 112, II, D, 2, c, 4
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97241'
     C     DXPRCD        IFEQ      '97241'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97240'       CPTFR
     C                   MOVE      '97240'       CPTTO
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4
     C*** Per pg. 112, II, D, 2, c, 4
     C     DHPRCD        IFEQ      '97501'
     C                   MOVE      '97500'       CPTFR
     C                   MOVE      '97500'       CPTTO
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4
     C*** Per pg. 112, II, D, 2, c, 4
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97521'
     C     DXPRCD        IFEQ      '97521'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97520'       CPTFR
     C                   MOVE      '97520'       CPTTO
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, b, 4
     C*** Per pg. 112, II, D, 2, b, 4
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97531'
     C     DXPRCD        IFEQ      '97531'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97530'       CPTFR
     C                   MOVE      '97530'       CPTTO
     C                   END
     C*
     C*** Per pg. 75, XIII, E, 2, c, 4
     C*** Per pg. 112, II, D, 2, c, 4
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFEQ '97541'
     C     DXPRCD        IFEQ      '97541'
     C*** END CHG *** 04/03/00 ****************************************
     C                   MOVE      '97540'       CPTFR
     C                   MOVE      '97540'       CPTTO
     C                   END
     C*
     C                   READP     PCA0F                                  60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     PXDSEQ        XXDSEQ
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C     DHKY          CHAIN     PDH02                              61
     C*** END CHG *** 04/03/00 ****************************************
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHPRCD    IFGE CPTFR
     C********** DHPRCD    ANDLECPTTO
     C********** DHAREA    ANDEQCPTPRT
     C     DXPRCD        IFGE      CPTFR
     C     DXPRCD        ANDLE     CPTTO
     C     DXAREA        ANDEQ     CPTPRT
     C*** END CHG *** 04/03/00 ****************************************
     C                   READ      PCA0F                                  60
     C*
     C                   Z-ADD     PXDSEQ        XXDSEQ
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C     DHKY          CHAIN     PDH02                              61
     C*** END CHG *** 04/03/00 ****************************************
     C*
     C                   Z-ADD     AJ2(N)        PXTOTA
     C                   MOVE      AJM(N)        PXPMOD
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     PXTOTA        PXTOTP
     C*
     C     PXEOMB        IFEQ      *BLANK
     C     PXTOTD        ANDGT     *ZERO
     C                   MOVE      '002'         PXEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C                   MOVE      Q1USER        PXUSER
     C                   MOVE      Q1PGM         PXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   Z-ADD     DSTIME        PXTIME
     C*
     C                   ELSE
     C*
     C                   READ      PCA0F                                  60
     C*
     C                   Z-ADD     PXDSEQ        XXDSEQ
     C*
     C*** BEG CHG *** 04/03/00 ****************************************
     C********** DHKY      CHAINPDH01                61
     C     DHKY          CHAIN     PDH02                              61
     C*** END CHG *** 04/03/00 ****************************************
     C*
     C                   Z-ADD     *ZERO         PXTOTA
     C     PXTOTC        SUB       PXTOTA        PXTOTD
     C                   Z-ADD     PXTOTA        PXTOTP
     C                   Z-ADD     *ZEROS        PXBRTC
     C*
     C                   MOVE      '001'         PXEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   MOVE      Q1USER        PXUSER
     C                   MOVE      Q1PGM         PXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   Z-ADD     DSTIME        PXTIME
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         PXMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA0F
     C                   END
     C*
     C*
     C                   END
     C                   Z-ADD     AJ3(N)        XX
MG2  C   92              EXSR      PTM
     C*
     C** END - OF CONDITIONAL MOVE ROUTINE.
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     PXEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMBPX
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C* After array has been processed, process FACAPFL Workfile
     C                   EXSR      PHYS3
     C*
     C*
     C* END OF THE UPDATE OF PT.
     C     UPEND         ENDSR
     C********************************************************************
     C*
     OPCA01     E            ECA01
     OPCA09     E            ECA09
     OPCAPT2    E            ECAPT2
     O*
**  NC1 CODES NOT TO CUT LIST 1
21800223052231022315252602526325270252722529025295
25310256302563525645260602608026110261352614026145
26170263502635226356263582641026412264182642026440
26442264452645026455264602647626480264852652026526
26530265312653526536265452660026605266072661026615
26720267252672726730267352674026742267442674626750
26755267602676526861276852766427681280802812628150
28160282002820828450284552846528470284752848028485
28510285152852028525
**  NC2 CODES NOT TO CUT LIST 2
20500206502066020670211002144021445252302617026180
26500265022709027685280602806228270282882831529815
29830298702988429890308053150531600320203641036620
38100387604383044005448504900049010495605200052310
6105061210622886580066030
**  TABA: MINOR CODE  TABB: MAJOR CODE
1100111000
1170111700
1171111710
1173111730
1173211731
1510115100
1524115240
6303563030
6304863045
6304863046
6304863047
6306663064
6307863075
6307863076
6307863077
6308663085
6308863087
6309163090
6444364442
6462364622
6477864776
6483264831
**  RAD1/RAD2 RADIOLOGY(&LAB) CODE1 IS INCLUDED IN CODE TWO.
7003070050
7004070050
7010070110
7012070130
7014070150
7021070220
7025070260
7031070320
7032870330
7045070470
7046070470
7048070482
7048170482
7049070492
7049170492
7102071030
7104071060
7110071101
7111071111
7125071270
7126071270
7204072050
7204072052
7207072072
7207072074
7210072110
7210072114
7212572127
7212672127
7212872130
7212972130
7213172133
7213272133
7217072190
7219272194
7219372194
7220072202
7224072241
7225572256
7226572266
7226562284
7227072271
7228572286
7229572296
7302073030
7304073041
7307073080
7308573086
7310073110
7311573116
7312073130
7320073202
7320173202
7350073510
7352573526
7356073562
7356073564
7356273564
7358073581
7360073610
7361573616
7362073630
7370073702
7370173702
7400074010
7400074020
7415074170
7416074170
7440074405
7441074415
7830078305
7830078306
7830578306
7860178606
7861078606
7870178707
7870478707
8100281000
8101581000
8830088302
9300593000
9301093000
9301793015
9301893015
9304193040
9304293040
9320893201
9354793549
9372293720
9470094705
9583195851
9583295851
9583395851
9583495851
9584295851
**  OFC1/OFC2 Office Visits XREF Codes
9000099201
9001099202
9001599203
9001799204
9002099205
9003099211
9004099211
9005099212
9006099213
9007099214
9008099215
9010099341
9011099341
9011599342
9011799343
9013099351
9014099351
9015099351
9016099352
9017099353
9020099221
9021599222
9022099223
9024099231
9025099231
9026099232
9027099233
9028099233
9029299238
9030099301
9031599302
9032099303
9034099311
9035099312
9036099312
9037099313
9040099321
9041599322
9042099323
9044099331
9045099332
9046099332
9047099333
9050099281
9050599282
9051099283
9051599284
9051799285
9060099241
9060099251
9060599242
9060599252
9061099243
9061099253
9062099244
9062099254
9063099245
9063099255
9064099261
9064199262
9064299262
9064399263
9920190000
9920290010
9920390015
9920490017
9920590020
9921190030
9921190040
9921290050
9921390060
9921490070
9921590080
9922190200
9922290215
9922390220
9923190240
9923190250
9923290260
9923390270
9923390280
9923890292
9924190600
9924199251
9924290605
9924299252
9924390610
9924399253
9924490620
9924499254
9924590630
9924599255
9925190600
9925199241
9925290605
9925299242
9925390610
9925399243
9925490620
9925499244
9925590630
9925599245
9926190640
9926290641
9926290642
9926390643
9928190500
9928290505
9928390510
9928490515
9928590517
9930190300
9930290315
9930390320
9931190340
9931290350
9931290360
9931390370
9932190400
9932290415
9932390420
9933190440
9933290450
9933290460
9933390470
9934190100
9934190110
9934290115
9934390117
9935190130
9935190140
9935190150
9935290160
9935390170
**  IPT2 - Initial Phys Therapy Codes for additional minutes code 97145
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
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALFL)
OVRDBF FILE(FACALFL)  TOFILE(QTEMP/FACALFL)
DLTOVR FILE(FACAL1FL)
OVRDBF FILE(FACAL1FL) TOFILE(QTEMP/FACAL1FL)
DLTOVR FILE(FACAPFL)
OVRDBF FILE(FACAPFL)  TOFILE(QTEMP/FACAPFL)
DLTOVR FILE(FACAL2FL)
OVRDBF FILE(FACAL2FL)  TOFILE(QTEMP/FACAL2FL)
