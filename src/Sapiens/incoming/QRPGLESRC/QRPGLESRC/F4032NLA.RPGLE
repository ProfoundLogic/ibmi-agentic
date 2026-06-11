     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4032RLA
     H*    DESCRIPTION: DENTAL ADJUDICATION (LOUISIANA RULES - 08/01/94)
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
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*****************************************************************
     H*  REVISIONS:
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
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                 CHPT   SUBR         CODES
     H*  Dental                                         DENT    Prefixed w "D"
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
     FFDCALH    UF   E           K DISK
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFACAPLA   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALLA   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFMCML1    IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMPRLLA   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLLA   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDDL1    IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)

     D*
     D* "Entry" Arrays
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
     D* Compile-time Arrays
     D DEN1            S              5    DIM(4) CTDATA PERRCD(1)              NO CUT/MAJOR-MIN
     D DEN2            S              5    DIM(4) ALT(DEN1)
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
     IPCA08
     I              CACPT4                      XRCPT4
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
     I              CATOTC                      XRTOTC
     I              CAMRA                       XRTOTD
     I              CATOTP                      XRTOTP
     I              CAPMOD                      XRPMOD
     I              CAUSER                      XRUSER
     I              CAPGM                       XRPGM
     I              CADT                        XRDT
     I              CATIME                      XRTIME
     I              CACHK#                      XRCHK#
     I              CAEOB#                      XREOB#
     I              CAPART                      XRPART
     I              CAEOMB                      XREOMB
     I              CACSCD                      XRCSCD
     I              CADIFF                      XRDIFF
     I              CADXR                       XRDXR
     I              CAFLAG                      XRFLAG
     I              CABRPC                      XRBRPC
     I              CABRTC                      XRBRTC
     I*
     IPCA09
     I              CACPT4                      XRCPT4
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
     I              CATOTC                      XRTOTC
     I              CAMRA                       XRTOTD
     I              CATOTP                      XRTOTP
     I              CAPMOD                      XRPMOD
     I              CAUSER                      XRUSER
     I              CAPGM                       XRPGM
     I              CADT                        XRDT
     I              CATIME                      XRTIME
     I              CACHK#                      XRCHK#
     I              CAEOB#                      XREOB#
     I              CAPART                      XRPART
     I              CAEOMB                      XREOMB
     I              CADIFF                      XRDIFF
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
     I              CATOTC                      PXTOTC
     I              CAMRA                       PXTOTD
     I              CATOTP                      PXTOTP
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
     I              CAMRA                       X6TOTD
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
     I*
     IPDD02
     I              DDCO#                       D1CO#
     I              DDSUB#                      D1SUB
     I              DDFND                       D1FND
     I              DDFYR                       D1FYR
     I              DDCASE                      D1CASE
     I              DDSS#                       D1SS#
     I              DDACDT                      D1ACDT
     I              DDEOB#                      D1EOB#
     I              DDDSEQ                      D1DSEQ
     I              DDPRCD                      D1PRCD
     I              DDPRDS                      D1PRDS
     I              DDSTAT                      D1STAT
     I              DDUSER                      D1USER
     I              DDPGM                       D1PGM
     I              DDTIME                      D1TIME
     I              DDDT                        D1DT
     I              DDIUSR                      D1IUSR
     I              DDIPGM                      D1PGM
     I              DDITIM                      D1ITIM
     I              DDIDT                       D1IDT
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
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKYPA        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P2FDAT
     C                   KFLD                    P2FEIN
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
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
     C*** Used with PCA08 (FDCAL8) in DENT2
     C     CAKY8A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    PPDSEQ
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
      *
     C*
     C     DDKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DDKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXPRCD
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
     C     *LIKE         DEFINE    CADSEQ        H1DSEQ
     C*
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAPMOD        H2PMOD
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
     C     *LIKE         DEFINE    DDUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    DDPRCD        XPCPT4
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*
     C     *LIKE         DEFINE    DDPRCD        XXCPT4
     C     *LIKE         DEFINE    DDPRCD        XXCPCD
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    CAPART        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAANMN        XXUNIT
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C     *LIKE         DEFINE    DDPRCD        XXPRCD
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C     *LIKE         DEFINE    DDPRCD        PPCPT4
     C     *LIKE         DEFINE    CADSEQ        PPDSEQ
     C     *LIKE         DEFINE    CAFDAT        PPFDAT
     C     *LIKE         DEFINE    CAFEIN        PPFEIN
     C     *LIKE         DEFINE    CAMOD         PPMOD
     C*
     C     *LIKE         DEFINE    CAFDAT        WKFDAT
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      *BLANKS       XXEND             2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XXPERC            5 2
     C                   Z-ADD     0             CNT50             5 0
     C                   Z-ADD     0             CNT51             5 0
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   MOVE      *BLANKS       QQTYPE            1            ADD 06/23/93
     C                   MOVE      ' '           XFIRST            1            First time Wrkf buil
     C*
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
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
     C     CAFDAT        IFGE      940801
     C     XXRCN         ANDEQ     CARCN
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DDKY          CHAIN     PDD01                              60
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DDUNTS        SVUNTS
     C                   MOVEL     DDPRCD        XPCPT4
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
     C                   MOVEL     DDPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     RCMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     XPPMOD        IFNE      *BLANK
     C*
     C                   EXSR      PMOD
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     XXMRA         XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C                   MOVEL     DDPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DDPRCD
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
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
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   EXSR      UPDAT
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
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
     C*
     C*-----------------------------------------------------------------
     C*-- Give Message for codes that end in '99' ---------------------
     C                   MOVE      DDPRCD        XXEND
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
     C                   Z-ADD     1             T
     C     1             SUBST     DDPRCD:T      DENTFL            1    65
     C     DENTFL        IFEQ      'D'
     C*
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
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
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
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
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
     C                   EXSR      DENT2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
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
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
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
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
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
     C*
     C                   EXSR      DENT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AE'          QQC(Q)
     C* Err Msg AE: "This code should be submitted on a HCFA form."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END                                                    END DENTFL
     C*
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C*  Test for a possible duplicate claim.
     C                   MOVE      DDPRCD        XXPRCD
     C*
     C     DDKY2         CHAIN     PDD02                              54
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
     C     DDKY2         READE     PDD02                                  54
     C*
     C                   ENDDO
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
     C*   If there is an override percentage, calculate
     C     CSOVRP        ANDGT     *ZERO
     C*
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   Z-ADD     *ZEROS        CADIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        CABRTC                          ADD 09/15/94
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
     C                   END                                                    END CAMRA=0
     C*
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
     C*
     C*** Execute DENT2 if a DENT Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      DENT2                                        Process the DENT cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*
     C*----------------------------------------------------------------------
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
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
     C*     to FACAPLA
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
     *** D E N T A L                Codes: PREFIXED WITH A "D"           *
     *********************************************************************
     C     DENT          BEGSR
     C*
     C*====================================================================
     C*--- Valid suffixes: ------------------------------------------------
     C     XPPMOD        IFNE      *BLANK
     C*
     C* Invalid Suffix
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '75'
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END                                                    END NE '22'
     C*
     C* ===============================================================
     C* XPPMOD IF EQ 52 REDUCED VALUES
     C     XPPMOD        IFEQ      '52'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C* Err Msg 01: "Adjustor should review charge."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END                                                    END EQ 52
     C*
     C* ===============================================================
     C* XPPMOD IF EQ 75 SERVICES RENDERED BY MORE THAN DENTIST
     C     XPPMOD        IFEQ      '75'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AF'          QQC(Q)
     C* Err Msg AF: "Pay 80% of MRA, if condition requires more than
     C*              one dentist."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END                                                    END EQ 75
     C*
     C* ===============================================================
     C* XPPMOD IF EQ 99 MULTIPLE MODIFIERS
     C     XPPMOD        IFEQ      '99'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override and
     C*              pay at MRA."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END                                                    END EQ 99
     C*
     C                   EXSR      PMOD
     C                   END                                                    END *BLANKS
     C*
     C* ===============================================================
     C* MINOR CODE MUST HAVE ASSOCIATED MAJOR CODE ELSE PAY ZERO
     C                   Z-ADD     1             T
     C     DDPRCD        LOOKUP    DEN1(T)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      DEN2(T)       XXCPT4
     C     CAKY9         CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   MOVE      '238'         XPEOMB
     C* EOMB Code 238: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END
     C                   END
     C*
     C* ===============================================================
     C* IF XPPMOD IS 50 OR 51 WRITE RECORD TO WORKFILE FOR PROCESSING IN DENT2
     C* 50 = BILATERAL OR MULTIPLE FIELD PROCEDURES
     C* 51 = MULTIPLE PROCEDURES
     C     XPPMOD        IFEQ      '50'
     C     XPPMOD        OREQ      '51'
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   Z-ADD     SXMRA         CAMRA
     C                   MOVE      'S'           CATYPE
     C                   WRITE     PCAWK1
     C                   GOTO      DENTE
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                          ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DENTE         ENDSR
     *********************************************************************
     *** D E N T A L 2              PROCESS DENTAL CODES IN WORKFILE     *
     *********************************************************************
     C     DENT2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the DENT Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C*
     C                   Z-ADD     0             CNT50
     C                   Z-ADD     0             CNT51
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* ===============================================================
     C* P2PMOD IFEQ 50
     C*  Highest value 100%
     C*  2nd +          70%
     C*
     C     P2PMOD        IFEQ      '50'
     C*
     C     CNT50         IFLE      0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             CNT50
     C                   ELSE                                                   ELSE EQ 0
     C     CNT50         IFGE      1
     C                   Z-ADD     .70           P2BRP                          50%
     C                   ADD       1             CNT50
     C                   END                                                    END  GE 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 50
     C* XPPMOD IFEQ 51
     C* Highest value 100%
     C* 2nd            50%
     C* 3rd            25%
     C* 4th            10%
     C* 5th             5%
     C* 6th +           BR
     C*
     C     P2PMOD        IFEQ      '51'
     C*
     C     CNT51         IFLE      0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             CNT51
     C                   ELSE                                                   ELSE EQ 0
     C     CNT51         IFEQ      1
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             CNT51
     C                   ELSE                                                   ELSE EQ 1
     C     CNT51         IFEQ      2
     C                   Z-ADD     .25           P2BRP
     C                   ADD       1             CNT51
     C                   ELSE                                                   ELSE EQ 2
     C     CNT51         IFEQ      3
     C                   Z-ADD     .10           P2BRP
     C                   ADD       1             CNT51
     C                   ELSE                                                   ELSE EQ 3
     C     CNT51         IFEQ      4
     C                   Z-ADD     .05           P2BRP
     C                   ADD       1             CNT51
     C                   ELSE                                                   ELSE EQ 4
     C     CNT51         IFGE      5
     C                   Z-ADD     0             P2BRP
     C                   ADD       1             CNT51
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GE 5
     C                   END                                                    END  EQ 4
     C                   END                                                    END  EQ 3
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   END                                                    END P2PMOD = 51
     C                   END                                                    END P2PMOD = 50
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
     C                   Z-ADD     *ZEROS        P2DIFF                          ADD 09/15/94
     C                   Z-ADD     P2MRA         P2BRTC                          ADD 09/15/94
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      DDPRCD        XXPRCD
     C*
     C     DDKY2         CHAIN     PDD02                              54
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
     C     DDKY2         READE     PDD02                                  54
     C*
     C                   ENDDO
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
     C                   Z-ADD     P2DIFF        XRDIFF                          ADD 09/15/94
     C                   Z-ADD     P2BRTC        XRBRTC                          ADD 09/15/94
     C                   MOVEL     P2PMOD        XRPMOD
     C     P2TOTD        IFGT      *ZERO
     C     P2TOTP        ANDGT     *ZERO
     C                   MOVE      '002'         P2EOMB
     C* EOMB Code 002: "Charges exceed maximum allowance"
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     C     P2EOMB        IFNE      *BLANKS
     C*
     C                   MOVE      P2EOMB        XPEOMB
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
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
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N61              UPDATE    PCA08
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   READ      PCAWK2                                 60
     C                   END                                                    END DOWEQ 0 *IN60
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
     C     NEWPRE        ENDSR
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*=====================================================================
     C* If modifier -22 is used, inform user
     C*
     C     XPPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END                                                    END EQ '22'
     C*
     C*
     C*=====================================================================
     C* If modifier -53 is used - PRIMARY EMERGENCY SERVICES
     C*
     C     XPPMOD        IFEQ      '53'
     C     SXMRA         MULT      .70           SXMRA
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C*
     C*=====================================================================
     C* If modifier -54 is used - SURGICAL PROCEDURE ONLY
     C*
     C     XPPMOD        IFEQ      '54'
     C     SXMRA         MULT      .70           SXMRA
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C*
     C*=====================================================================
     C* If modifier -55 is used - FOLLOW-UP CARE ONLY
     C*
     C     XPPMOD        IFEQ      '55'
     C     SXMRA         MULT      .30           SXMRA
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C*
     C*=====================================================================
     C* If modifier -56 is used - PRE-OPERATIVE CARE ONLY
     C*
     C     XPPMOD        IFEQ      '56'
     C     SXMRA         MULT      .30           SXMRA
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   GOTO      PMODE
     C                   END                                                    END EQ *ZEROS
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Update                                                U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* ===============================================================
     C* Establish default EOMB code, if necessary
     C     XPTOTD        IFGT      *ZERO
     C     XPTOTP        ANDGT     *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
      *
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
     C                   MOVE      DDPRCD        XXPRCD
     C*
     C     DDKY2         CHAIN     PDD02                              54
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
     C     DDKY2         READE     PDD02                                  54
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
     C                   Z-ADD     XPDIFF        CADIFF                          ADD 09/15/94
     C                   Z-ADD     XPBRTC        CABRTC                          ADD 09/15/94
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
     C                   MOVE      DDPRCD        XXCPT4
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
     C                   Z-ADD     *ZEROS        XPDIFF                          ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                          ADD 09/15/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     *********************************************************************
**  DEN1/DEN2: A MUST HAVE B
D0230D0220
D0260D0250
D0275D0274
D9221D9220
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALLA)
OVRDBF FILE(FACALLA)  TOFILE(QTEMP/FACALLA)
DLTOVR FILE(FACAPLA)
OVRDBF FILE(FACAPLA)  TOFILE(QTEMP/FACAPLA)
