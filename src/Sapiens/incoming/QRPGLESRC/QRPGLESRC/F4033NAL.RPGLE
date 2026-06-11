     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4033RAL
     H*    DESCRIPTION: DENTAL ADJUDICATION (ALABAMA RULES - 03/15/97)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the Alabama State
     H* Fee Schedule Reimbursment Manual issued on 03/15/97
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 03/15/97 and before 03/15/98.
     H*****************************************************************
     H*  CREATED: 6/05/97   C:59   REQ/PROJECT: AL032
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*****************************************************************
     H*  REVISIONS:
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
     H*  09/14/98 C:79  Project: AL033
     H*                 Add ending date range so that line items
     H*                 between 3/15/97 and 3/15/98 are processed.
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
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLAL   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLAL   IF   E           K DISK
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*
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

     C*
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
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C***********          MOVELSRDTO     SYDATE  60
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END CHANGE ************** 11/18/97 *****************************
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
     C     *LIKE         DEFINE    CAUNTS        SVUNTS
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
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    CAPART        XXPART
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
     C     *LIKE         DEFINE    DDPRCD        XXPRCD
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
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
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      *BLANKS       QQTYPE            1            ADD 06/23/93
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
     C     CAFDAT        IFGE      970315
     C     CAFDAT        ANDLT     980315
     C     XXRCN         ANDEQ     CARCN
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C* Get corresponding Detail Charge Auxiliary record (FDDDP)
     C     DDKY          CHAIN     PDD01                              60
     C*
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DDPRCD        XPCPT4
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     RCMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C     RCRPT         IFEQ      'Y'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      '207'         XPEOMB
     C* EOMB Code 207: "This charge has been paid per R&C fees"
     C   61              Z-ADD     RCMRA         XPTOTA
     C  N61              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C   60              MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed Reasonable & Customary"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- Possible Bundled Codes ------------------------------------------
     C     PRRPT         IFEQ      'B'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '205'         XPEOMB
     C* EOMB Code 205: "This procedure is included in the basic allowance
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
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
     C     DDKY2         READE     PDD02                                  54
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
     C                   Z-ADD     *ZEROS        CADIFF
     C                   Z-ADD     *ZEROS        CABRTC
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
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
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
     C*
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*---------------------------------------------------------------------
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
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
     *** D E N T A L                Codes: PREFIXED WITH A "D"           *
     *********************************************************************
     C     DENT          BEGSR
     C*
     C*====================================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DENTE         ENDSR
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
     *** Update                                                U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* ===============================================================
     C* Establish default EOMB code, if necessary
     C     XPTOTD        IFGT      *ZERO
     C     XPTOTP        ANDGT     *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
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
     C     DDKY2         READE     PDD02                                  54
     C*
     C                   ENDDO
     C* ===============================================================
     C** Only process the record if not an override type
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    ENDIF *IN61
     C                   MOVE      '0'           XX25EM
     C                   END
     C*
     C* ===============================================================
     C*
     C** Clear the work fields
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     *********************************************************************
