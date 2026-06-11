     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4062RFL                                          *
     H*    DESCRIPTION: ADJUDICATION (1992 RULES)                     *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*   This program was derived from the Florida State Fee         *
     H*   Schedule Reimbursment Manual for Ambulatory Surgical        *
     H*   Centers issued on 12/31/92.                                 *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  12/31/92.                                                    *
     H*****************************************************************
     H*  Changes since 8/7/91 Manual:
     H*       *  Additional codes with MRAs put into CPT4 Proc Code File
     H*       *  Added err msg on mod-22 that report is required for payment
     H*       *  Codes in the PATH, RADI, MEDI range now added
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  11/23/94 C:20   REQ/PROJ: FLMNT
     H*                  ADD CODE TO LOAD CABRTC WITH MRA AMOUNT AFTER
     H*                  CALCULATED.
     H*
     H*  07/15/94 C:20   REQ/PROJ: FLMNT
     H*                  MODIFIED CODE TO PAY U&C BILLS AT 70%
     H*
     H*  10/29/93 C:20   REQ/PROJ: FLMNT
     H*                  ADD CODE TO PROCESS U&C BILLS - THIS IS DONE RIGHT
     H*                  AFTER CSOVRD IS CHECKED
     H*
     H*  03/19/93 C:05  Project: FS104
     H*                 1. Added Err Msg# 79 for Service Date > System Date
     H*
     H*  03/19/93 C:05  Project: FS102
     H*                 1. Remove Provider Modifier from key that checks for
     H*                    "Possible Duplicate".  Required adding new logical
     H*                    FDCALP
     H*                 2. Rmvd reference to FDCAL6/PCA06 (wasn't used anymore)
     H*
     H*  01/28/93 C:05   PROJ: FS081                                  *
     H*                  1. Added new parm to *ENTRY: Specialty Code  *
     H*                  2. Changed usage of SPCD from FMPDP to passed parm
     H*
     H*  01/22/93 C:20  Project: FS054
     H*                 1. ADDED CODE TO CHECK FOR POSSIBLE DUPLICATE
     H*                    CHARGE IF THE CLAIM STATUS CODE IS AN
     H*                    OVERRIDE TYPE
     H*                                                               *
     H*  01/21/93  C:20  PROJ: FS075                                  *
     H*                  1. CHANGED ADJUDICATION DO LOOP FROM 42 TO 50*
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     H*  FILE USAGE:                                                 **
     H*  FDCALP: (PCA0P)  (Uses 'PX' fields) For Possible Dup Processing
     H*          Fields:  CO,FND,FYR,CASE,FDAT,DSEQ,FEIN,MOD,CPT4,   **
     H*                   TOTA,TOTD,TOTP,TOTC,PMOD,USER,PGM,DT,TIME, **
     H*                   CHK#,EOB#,PART,DXR,FLAG                    **
     H*                   PART/DSEQ                                  **
     H*          KFLDs:   CO/FND/FYR/CASE/FDAT/FEIN/CPT4/PMOD/       **
     H*          S/O  :   FLAG NE 'P'                                **
     H*  FDCAL7: (PCA01)                                             **
     H*          KFLDs:   CO/FND/FYR/CASE/FDAT/FEIN/MOD/PRTY/DSEQ/   **
     H*                   EOB#/CHK#                                  **
     H*          S/O  :   FLAG NE 'P'                                **
     H*  FDCAL8: (PCA08)  (Uses 'XR' fields)                         **
     H*          Fields:  CO,FND,FYR,CASE,FDAT,DSEQ,FEIN,MOD,CPT4,   **
     H*                   TOTA,TOTD,TOTP,TOTC,PMOD,USER,PGM,DT,TIME, **
     H*                   CHK#,EOB#,PART,EOMB,CSCD,DXR,FLAG          **
     H*          KFLDs:   CO/FND/FYR/CASE/FDAT/FEIN/MOD/CPT4/DSEQ    **
     H*          S/O  :   CHK# EQ 0000000                            **
     H*          S/O  :   EOB# EQ 000000000                          **
     H*          S/O  :   FLAG NE 'P'                                **
     H*
     H*  09/10/93 C:20  REQ/PROJ: FSMNT
     H*                 1. OVR  - FIX TO SECTION OF CODE THAT CALCULATES
     H*                    THE TOTAL PAID AMOUNT FOR CODES THAT ARE OVERRIDE
     H*                    CLAIM STATUS, ONLY CALCULATE AMOUNT IF CATOTD,
     H*                    CAOTHR, CANDIS, CATOTA, CATOTP ALL EQUAL 0
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPRL     IF   E           K DISK
     F*** BEGIN CHG *** 04/13/92 **************************************
     F**********FTABL   IF  E           K        DISK
     F*** BEGIN CHG *** 10/29/93 **************************************
     FFTABLFL   IF   E           K DISK
     F                                     IGNORE(PRC01)
     F*** END CHG *** 10/29/93 ****************************************
     F*** END CHG *** 04/13/92 ****************************************
     FFDCAL7    UF   E           K DISK
     FFDCAL8    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     F*** BEGIN ADD *** 03/19/93 **************************************
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     F*** END ADD ***** 03/19/93 **************************************
     F*** BEGIN ADD *** 10/29/93 **************************************
     FFMGZL     IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     F*** END ADD *** 10/29/93 ****************************************
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*** END ADD ***** 03/19/93 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
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
     I              CATOTD                      XRTOTD
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
     I*** BEGIN ADD *** 10/27/92 **************************************
     I              CADIFF                      XRDIFF
     I*** END ADD *** 10/27/92 ****************************************
     I*
     I*** BEGIN ADD *** 03/19/93 **************************************
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
     I              CATOTA                      PXTOTA
     I              CATOTD                      PXTOTD
     I              CATOTP                      PXTOTP
     I              CATOTC                      PXTOTC
     I              CAPMOD                      PXPMOD
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CADT                        PXDT
     I              CATIME                      PXTIME
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CADIFF                      PXDIFF
     I              CAPART                      PXPART
     I              CADXR                       PXDXR
     I              CAFLAG                      PXFLAG

     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*** BEGIN ADD *** 10/29/93 **************************************
     C*
     C*** REASONABLE & CUSTOMARY FILE
     C     RCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD            5
     C*
     C*** Geographical Zip Area FILE
     C     GZKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXFRZP
     C*
     C*  PROVIDER FILE
     C     PDKEY         KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*** END ADD ***** 10/29/93 *************************************
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
     C     CAKEY9        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C*  CLAIM DETAIL FILE
     C     CAKEYA        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
     C                   KFLD                    SVCHK#
     C*
     C*  CLAIM DETAIL FILE - used in NEWPRO subroutine
     C     CAKEYZ        KLIST
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
     C                   KFLD                    XXCPT4
     C*
     C*  CLAIM DETAIL FILE - Use for Possible Dup Claims
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
     C*** BEGIN ADD *** 03/19/93 **************************************
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
     C*** END ADD ***** 03/19/93 **************************************
     C*
     C*  PROC CODE MODIFIER  FILE
     C     PMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  PROCEDURE CODE AUXILIARY FILE
     C     PRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3            ADD 01/28/93
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
     C*** BEGIN ADD *** 03/19/93 *****************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END ADD ***** 03/19/93 *****************************************
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CAFYR         QQFYR
     C     *LIKE         DEFINE    CACASE        QQCASE
     C     *LIKE         DEFINE    CASS#         QQSS#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    CAUNTS        SVUNTS
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PR92N1        SX92N1
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C     *LIKE         DEFINE    CACPT4        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTD        XPTOTA
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         ADD 11/23/94
     C     *LIKE         DEFINE    CATOTD        XPTOTD
     C     *LIKE         DEFINE    CATOTP        XPTOTP
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C********** *LIKE     DEFN PREFDT    XXDATE
     C     *LIKE         DEFINE    PREFDT        XXYMD
     C     *LIKE         DEFINE    PREFDT        XXEFDT
     C     *LIKE         DEFINE    PR92N1        XXMRA
     C     *LIKE         DEFINE    CSOVRP        XXOVRP
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA                         10/29/93 ADD
     C     *LIKE         DEFINE    GZFRZP        XXFRZP                         10/29/93 ADD
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
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
     C                   GOTO      TERM
     C                   END
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
     C                   GOTO      TERM
     C                   END
     C*
     C** DO THE ADJUDICATION FOR EACH POSSIBLE DATE ENTERED IF AFTER 6-1-91
     C*
     C*** BEGIN CHG *** 01/21/93 **************************************
     C**********           DO   42        S
     C                   DO        50            S
     C*** END CHG *** 01/21/93 ****************************************
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C**********           MOVELQQF,S     XXDATE
     C                   MOVE      QQF(S)        XXDATE            6 0
     C*** END   CHANGE************* 11/18/97 *****************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     0921231
     C     CAKEY9        CHAIN     PCA01                              54
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    ECA01
     C*
     C*** BEGIN ADD *** 03/19/93 *************************************
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GT SYDATE
     C*** END ADD ***** 03/19/93 *************************************
     C*
     C** GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C     CSOVRD        IFNE      'Y'
     C*** BEGIN ADD *** 10/29/93 *************************************
     C*****  CHECK TO SEE IF THIS RECORD IS A REASONABLE/CUSTOMARY RECORD
     C*
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     CACPT4        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPBRTC                         ADD 11/23/94
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C*
     C                   MOVE      *BLANKS       XXFRZP
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKEY         CHAIN     PPD01                              60
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
     C     RCKEY         SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA                                       ADD 05/13/93
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*** BEGIN CHG *** 07/15/94 ***********************************************
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C********** RCRPT     IFEQ 'Y'
     C*
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C********** CAPMOD    IFNE *BLANK
     C*
     C********** CATOTC    COMP XXMRA                54
     C***54                Z-ADDXXMRA     XPTOTA
     C**N54                Z-ADDCATOTC    XPTOTA
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C*
MG11 C**********           EXSR UPDAT
     C**********           GOTO NXTRCD                     GET NEXT RECORD
     C**********           ELSE
     C*
     C********** CACHK#    IFEQ *ZERO                      ## 4/20/89
     C********** CAEOB#    ANDEQ*ZERO                      ## 4/20/89
     C**********           Z-ADD1         Q       30
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60                Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '06'      QQC,Q
     C**********           END                             ## 4/20/89
     C*
     C**********           Z-ADDCATOTC    XPTOTD
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C*
MG11 C**********           EXSR UPDAT
     C**********           GOTO NXTRCD                     GET NEXT RECORD
     C*
     C**********           END
     C**********           ELSE                            ELSE RCRPT EQ 'Y'
     C********** CATOTC    COMP RCMRA                54
     C***54                MOVE '229'     XPEOMB
     C***54                Z-ADDRCMRA     XPTOTA
     C**N54                Z-ADDCATOTC    XPTOTA
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C*
     C**********           END                             END RCRPT EQ 'Y'
     C*
     C*   If procedure code is not found Fee Schedule Proc Code File:
     C*        (per pg. 13, C, 2)
     C*      then reimbursement is made at 70 percent of the charge
     C     CATOTC        MULT      .70           XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE
     C                   GOTO      NXRC1
     C                   END
     C                   END
     C***
     C********** CAKEYA    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
     C**********
     C**********           MOVE XPTOTA    CATOTA
     C**********           MOVE XPTOTD    CATOTD
     C**********           MOVE XPTOTP    CATOTP
     C**********           MOVE XPEOMB    CAEOMB
     C*
     C**********           UPDATPCA01
     C*
     C**********           Z-ADD*ZEROS    XPTOTA
     C**********           Z-ADD*ZEROS    XPTOTD
     C**********           Z-ADD*ZEROS    XPTOTP
     C**********           MOVE *BLANKS   XPPMOD
     C**********           MOVE *BLANKS   XPEOMB
     C*
     C**********           GOTO NXTRCD
     C*
     C**********           END
     C*** END CHG *** 07/15/94 *************************************************
     C                   END
     C*** END ADD ***** 10/29/93 *************************************
     C*
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*** BEGIN ADD *** 04/28/92 *************************************
     C* ===== HOSPITAL TYPE ==========================================
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* ===== NON-HOSPITAL TYPE ======================================
     C*** END ADD ***** 04/28/92 *************************************
     C*
     C                   MOVEL     CACPT4        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C*  GET PROCEDURE CODE AUXILIARY FILE
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      CACPT4
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)                         INVALID PROC CODE
     C*** Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C*
     C*
     C*
     C** SAVE THE PROCEDURE CODE AUXILIARY FILE VALUES.
     C*
     C                   Z-ADD     PR92N1        SX92N1
     C*
     C*
     C*  % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % *
     C*
     C*** EXECUTE ALL SUBROUTINES HERE.
     C*
     C     CACPT4        IFGE      '10000'                                      SURGICAL
     C     CACPT4        ANDLE     '69999'                                        10020-69646
     C                   EXSR      SURG
     C                   ELSE
     C     CACPT4        IFGE      '70010'                                      RADIOLOGICAL
     C     CACPT4        ANDLE     '78990'
     C                   EXSR      RADI
     C                   ELSE
     C     CACPT4        IFGE      '80002'                                      PATHOLOGICAL
     C     CACPT4        ANDLE     '89205'
     C                   EXSR      PATH
     C                   ELSE
     C     CACPT4        IFGE      '90600'
     C     CACPT4        ANDLE     '96912'
     C                   EXSR      MEDI
     C                   ELSE
     C                   EXSR      OTHE
     C                   END
     C                   END                                                    END EQ PATH
     C                   END                                                    END EQ RADI
     C                   END                                                    END EQ SURG
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*** BEGIN ADD *** 01/22/93 **************************************
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     CACPT4        IFNE      *BLANK
     C     CAEOB#        ANDEQ     *ZERO
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAPMOD        XXPMOD
     C********** CAKY8     CHAINPCA06                54    DEL 03/19/93
     C     CAKYP         CHAIN     PCA0P                              54        CHG 03/19/93
     C*
     C     DUPCL2        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 03/19/93 ****************************************
     C********** CADSEQ    IFNE X6DSEQ
     C********** CADSEQ    OREQ X6DSEQ
     C********** CAEOB#    ANDNEX6EOB#
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*** END CHG ***** 03/19/93 ****************************************
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C********** CAKY8     READEPCA06                    54 DEL 03/19/93
     C     CAKYP         READE     PCA0P                                  54     ADD 03/19/93
     C  N54              GOTO      DUPCL2
     C                   END
     C                   END
     C                   END
     C*** END ADD *** 01/22/93 ****************************************
     C*
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAEOB#        IFEQ      *ZERO
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
     C*
     C*** If override % entered in file, then pay at percentage
     C***     Else pay at 0 and user will change on SCR03 of F0070R
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END                                                    END CSOVRP GT *ZERO
     C*
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END                                                    END CAEOB# EQ *ZERO
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C                   END                                                    END 0 *IN54
     C*
     C                   END                                                    END OVERRIDE TYPE
     C*
     C     NXTRCD        TAG
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKEY9        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   END
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C     DUMMY         CHAIN     PCA08                              54
     C  N54              EXCEPT    ECA08
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
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
     C**** NOT NEEDED *************
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*** REGET RECORD.
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
     C     NEWPRE        ENDSR
     *********************************************************************
     *** MEDICINE SUBROUTINE    CODES 90600 - 96912              M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** (per pg. 36) - Valid Modifiers
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '52'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '101'         XPEOMB
     C*** EOMB 101: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSEQ valid modifier
     C*
     C     CAPMOD        IFEQ      '22'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the justification report is required for
     C***              reimbursement"
     C                   END
     C                   END                                                    ENDEQ '22'
     C*
     C     CAPMOD        IFEQ      '52'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '52'
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '013'         XPEOMB
     C*** EOMB 013: "Medical Report Required for Payment"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END NE 22/52
     C                   END                                                    ENDNE *BLANKS
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C*   (per pg. 13, C, 1, a)
     C*   If procedure code is found in Fee Schedule Proc Code File:
     C     SX92N1        IFNE      *ZERO
     C                   Z-ADD     SX92N1        XXMRA
     C*
     C*      then compare ASC's usual and customary charge to determine if it is
     C*      equal to or less than the MRA listed.
     C*
     C*        (per pg. 13, C, 1, a)
     C*        If so, reimbursement if made for 80% of charge or the MRA,
     C*        whichever is less.
     C     CATOTC        IFLE      XXMRA
     C     CATOTC        MULT      .80           XPTOTA
     C     XXMRA         IFLT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C*        (per pg. 13, C, 1, b)
     C*        If not, reimbursement if made for 65% of charge or the MRA,
     C*        whichever is greater.
     C                   ELSE                                                   ELSE TOTC LT MRA
     C     CATOTC        MULT      .65           XPTOTA
     C     XXMRA         IFGT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END  TOTC LT MRA
     C*
     C                   ELSE                                                   ELSE SX92N1 NE *ZERO
     C*
     C*   If procedure code is not found Fee Schedule Proc Code File:
     C*        (per pg. 13, C, 2)
     C*      then reimbursement is made at 70 percent of the charge
     C     CATOTC        MULT      .70           XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C     CATOTC        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                         ADD 11/23/94
     C*
     C                   EXSR      UPDAT
     C                   END                                                    END  SX92N1 NE *ZERO
     C*
     C     MEDIE         ENDSR
     *********************************************************************
     *** PATHOLOGY SUBROUTINE    CODES 80002 - 89205             P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** (per pg. 36) - Valid Modifiers
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '52'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '101'         XPEOMB
     C*** EOMB 101: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   ELSE                                                   ELSEQ valid modifier
     C*
     C     CAPMOD        IFEQ      '22'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the justification report is required for
     C***              reimbursement"
     C                   END
     C                   END                                                    ENDEQ '22'
     C*
     C     CAPMOD        IFEQ      '52'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '52'
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '013'         XPEOMB
     C*** EOMB 013: "Medical Report Required for Payment"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END NE 22/52
     C                   END                                                    ENDNE *BLANKS
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C*   (per pg. 13, C, 1, a)
     C*   If procedure code is found in Fee Schedule Proc Code Aux File:
     C     SX92N1        IFNE      *ZERO
     C                   Z-ADD     SX92N1        XXMRA
     C*
     C*      then compare ASC's usual and customary charge to determine if it is
     C*      equal to or less than the MRA listed.
     C*
     C*        (per pg. 13, C, 1, a)
     C*        If so, reimbursement if made for 80% of charge or the MRA,
     C*        whichever is less.
     C     CATOTC        IFLE      XXMRA
     C     CATOTC        MULT      .80           XPTOTA
     C     XXMRA         IFLT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C*        (per pg. 13, C, 1, b)
     C*        If not, reimbursement if made for 65% of charge or the MRA,
     C*        whichever is greater.
     C                   ELSE                                                   ELSE TOTC LT MRA
     C     CATOTC        MULT      .65           XPTOTA
     C     XXMRA         IFGT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END  TOTC LT MRA
     C*
     C                   ELSE                                                   ELSE SX92N1 NE *ZERO
     C*
     C*   If procedure code is not found Fee Schedule Proc Code Aux File:
     C*        (per pg. 13, C, 2)
     C*      then reimbursement is made at 70 percent of the charge
     C     CATOTC        MULT      .70           XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C     CATOTC        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                         ADD 11/23/94
     C*
     C                   EXSR      UPDAT
     C                   END                                                    END  SX92N1 NE *ZERO
     C*
     C     PATHE         ENDSR
     *********************************************************************
     *** RADIOLOGY SUBROUTINE    CODES 70010 - 78990             R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** (per pg. 36) - Valid Modifiers
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '52'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '101'         XPEOMB
     C*** EOMB 101: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELSEQ valid modifier
     C*
     C     CAPMOD        IFEQ      '22'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the justification report is required for
     C***              reimbursement"
     C                   END
     C                   END                                                    ENDEQ '22'
     C*
     C     CAPMOD        IFEQ      '52'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '52'
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '013'         XPEOMB
     C*** EOMB 013: "Medical Report Required for Payment"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END NE 22/52
     C                   END                                                    ENDNE *BLANKS
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C*   (per pg. 13, C, 1, a)
     C*   If procedure code is found in Fee Schedule Proc Code Aux File:
     C     SX92N1        IFNE      *ZERO
     C                   Z-ADD     SX92N1        XXMRA
     C*
     C*      then compare ASC's usual and customary charge to determine if it is
     C*      equal to or less than the MRA listed.
     C*
     C*        (per pg. 13, C, 1, a)
     C*        If so, reimbursement if made for 80% of charge or the MRA,
     C*        whichever is less.
     C     CATOTC        IFLE      XXMRA
     C     CATOTC        MULT      .80           XPTOTA
     C     XXMRA         IFLT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C*        (per pg. 13, C, 1, b)
     C*        If not, reimbursement if made for 65% of charge or the MRA,
     C*        whichever is greater.
     C                   ELSE                                                   ELSE TOTC LT MRA
     C     CATOTC        MULT      .65           XPTOTA
     C     XXMRA         IFGT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END  TOTC LT MRA
     C*
     C                   ELSE                                                   ELSE SX92N1 NE *ZERO
     C*
     C*   If procedure code is not found Fee Schedule Proc Code Aux File:
     C*        (per pg. 13, C, 2)
     C*      then reimbursement is made at 70 percent of the charge
     C     CATOTC        MULT      .70           XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C     CATOTC        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                         ADD 11/23/94
     C*
     C                   EXSR      UPDAT
     C                   END                                                    END  SX92N1 NE *ZERO
     C*
     C     RADIE         ENDSR
     *********************************************************************
     *** SURGERY SUBROUTINE    CODES 10020 - 69646               S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** (per pg. 36) - Valid Modifiers
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '52'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '101'         XPEOMB
     C*** EOMB 101: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ELSE                                                   ELSEQ valid modifier
     C*
     C     CAPMOD        IFEQ      '22'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C*** Err Msg 55: "A copy of the justification report is required for
     C***              reimbursement"
     C                   END
     C                   END                                                    ENDEQ '22'
     C*
     C     CAPMOD        IFEQ      '52'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '52'
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '013'         XPEOMB
     C*** EOMB 013: "Medical Report Required for Payment"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END NE 22/52
     C                   END                                                    ENDNE *BLANKS
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** (per conversation with Dawn Shelton at State)
     C*** Read all charges on bill to determine if this item has the greatest
     C***     charge amount
     C***           If yes, pay
     C***           If no, disallow and give EOMB and error message
     C*
     C*
     C* SETLL on surgery codes and READ the file testing for greatest charge
     C*   amount
     C                   MOVE      '10020'       XXCPT4
     C     CAKEY8        SETLL     PCA08
     C*
     C     CAKEY7        READE     PCA08                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C                   EXCEPT    ECA08
     C*
     C     XRCPT4        IFLE      '69646'
     C*
     C     XRPMOD        IFEQ      *BLANKS
     C*
     C     CATOTC        IFLT      XRTOTC
     C     XRTOTP        ORGT      *ZERO
     C     CADSEQ        ANDNE     XRDSEQ
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '103'         XPEOMB
     C*** EOMB 103: "This procedure is included in the basic surgical allowance
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C*** Err Msg 01: "This bill requires further inspection by an adjustor"
     C                   END
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END LT XRTOTC
     C*
     C                   END                                                    END PMOD EQ *BLANKS
     C*
     C                   END                                                    END LT 69646
     C*
     C     CAKEY7        READE     PCA08                                  54
     C*
     C                   END                                                    END 0 DOWEQ
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** PERFORM THE CALCULATION FOR ADJUDICATION.
     C*
     C*   (per pg. 13, C, 1, a)
     C*   If procedure code is found in Fee Schedule Proc Code Aux File:
     C     SX92N1        IFNE      *ZERO
     C                   Z-ADD     SX92N1        XXMRA
     C*
     C*      then compare ASC's usual and customary charge to determine if it is
     C*      equal to or less than the MRA listed.
     C*
     C*        (per pg. 13, C, 1, a)
     C*        If so, reimbursement if made for 80% of charge or the MRA,
     C*        whichever is less.
     C     CATOTC        IFLE      XXMRA
     C     CATOTC        MULT      .80           XPTOTA
     C     XXMRA         IFLT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C*        (per pg. 13, C, 1, b)
     C*        If not, reimbursement if made for 65% of charge or the MRA,
     C*        whichever is greater.
     C                   ELSE                                                   ELSE TOTC LT MRA
     C     CATOTC        MULT      .65           XPTOTA
     C     XXMRA         IFGT      XPTOTA
     C                   Z-ADD     XXMRA         XPTOTA
     C                   END
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END  TOTC LT MRA
     C*
     C                   ELSE                                                   ELSE SX92N1 NE *ZERO
     C*
     C*   If procedure code is not found Fee Schedule Proc Code Aux File:
     C*        (per pg. 13, C, 2)
     C*      then reimbursement is made at 70 percent of the charge
     C     CATOTC        MULT      .70           XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 11/23/94
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C     CATOTC        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXMRA         XPBRTC                         ADD 11/23/94
     C*
     C                   EXSR      UPDAT
     C                   END                                                    END  SX92N1 NE *ZERO
     C*
     C     SURGE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** (per pg. 36) - Valid Modifiers
     C     CAPMOD        IFNE      *BLANKS
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '52'
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '101'         XPEOMB
     C*** EOMB 101: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C*
     C                   ELSE                                                   ELSEQ valid modifier
     C*
     C     CAPMOD        IFEQ      '22'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '22'
     C*
     C     CAPMOD        IFEQ      '52'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '50'          QQC(Q)
     C*** Err Msg 50: "Reduced services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END
     C                   END                                                    ENDEQ '52'
     C*
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '013'         XPEOMB
     C*** EOMB 013: "Medical report required for payment"
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END VALID MODIFIER
     C                   END                                                    ENDNE *BLANKS
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Disallow all other charges, and give adjustor message to review
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTP
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C*** Err Msg 01: "This bill requires further inspection by an adjustor"
     C                   END
     C*
     C                   EXSR      UPDAT
     C*
     C     OTHEE         ENDSR
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
     C                   MOVE      '102'         XPEOMB
     C*** EOMB 102: "Charges exceed Maximum Reimbursement Allowance"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     CACPT4        IFNE      *BLANK
     C     CAEOB#        ANDEQ     *ZERO
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAPMOD        XXPMOD
     C*********  CAKY8     CHAINPCA06                54    DEL 03/19/93
     C     CAKYP         CHAIN     PCA0P                              54        ADD 03/19/93
     C*
     C     DUPCLM        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 03/19/93 *******************************************
     C********** CADSEQ    IFNE X6DSEQ
     C********** CADSEQ    OREQ X6DSEQ
     C********** CAEOB#    ANDNEX6EOB#
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*** END CHG ***** 03/19/93 *******************************************
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C********** CAKY8     READEPCA06                    54 DEL 03/19/93
     C     CAKYP         READE     PCA0P                                  54     ADD 03/19/93
     C  N54              GOTO      DUPCLM
     C                   END
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C     CAKEYA        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 11/23/94
     C                   Z-ADD     XPTOTP        CATOTP
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
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 11/23/94
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     C***********************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
