     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4071RUC
     H*    DESCRIPTION: ADJUDICATION (USUAL & CUSTOMARY - 01/01/01)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.
     H*
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 01/01/01 and any state not specifically assigned state        *
     H* adjudication rules (such as FL - F4072RFL or GA - F4072RGA)   *
     H*****************************************************************
     H*  CREATED:  C:05  03/09/93  Project: FS099                     *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*                                                               *
     H*  09/21/98 C:79    1. Modify to work with provider bills
     H*
     H*  02/04/98 C:56    Project: MC087 seq# 9941
     H*                   1. Replace PDZIP with P1ZIP in determining
     H*                      Geo Zip.
     H*
     H*  03/19/93 C:05  Project: FS102
     H*                 1. Remove Provider Modifier from key that checks for
     H*                    "Possible Duplicate".  Required adding new logical
     H*                    FDCALP
     H*                 2. Rmvd reference to FDCAL6/PCA06 (wasn't used anymore)
     H*
     H*  03/19/93 C:05  Project: FS104
     H*                 1. Added Err Msg# 79 for Service Date > System Date
     H*
     H*  09/10/93 C:20  REQ/PROJ: FSMNT
     H*                 1. OVR  - FIX TO SECTION OF CODE THAT CALCULATES
     H*                    THE TOTAL PAID AMOUNT FOR CODES THAT ARE OVERRIDE
     H*                    CLAIM STATUS, ONLY CALCULATE AMOUNT IF CATOTD,
     H*                    CAOTHR, CANDIS, CATOTA, CATOTP ALL EQUAL 0
     H*
     H*  05/12/94 C:05  REQ/PROJ: UC001
     H*                 1. Revise program to be able to process FMPRPUC file
     H*                    if no match is found in the FMRCP file.
     H*                    If match is found in the FMPRPUC file, it will
     H*                    pay in full.
     H*
     H*  03/07/97 C:32  REQ/PROJ: FSMNT
     H*                 1. CHG TO BYPASS RECORDS WHICH ALREADY HAVE AN
     H*                    EOB#. THIS WAS CAUSING A PROBLEM IN PGM WHEN
     H*                    THE STATE WAS 'UC' AND ANOTHER REC PREVIOUSLY
     H*                    PROCESSED WAS NOT 'UC'. THE PGM GAVE NO ERRORS.
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMGZL     IF   E           K DISK
     F*** BEGIN CHG *** 02/04/98 *************************************
     F*FMPDL***IF**E***        K        DISK
     FFMP1L     IF   E           K DISK
     F*** END CHG *** 02/04/98 ***************************************
     FFMRCL     IF   E           K DISK
     FFTABLUC   IF   E           K DISK
     F                                     IGNORE(PRC01)
     F*** BEGIN DEL *** 09/21/99 *************************************
     F***********FDCAL7  UF  E           K        DISK
     F*** END   DEL *** 09/21/99 *************************************
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFMPRLUC   IF   E           K DISK
     F*
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*  FDCAL13 is the Primary File Read (PCA01)
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA13)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
      *** BEG ADD *** 09/21/98 ****************************************
     FFDDHL     IF   E           K DISK
      *** END ADD *** 09/21/98 ****************************************

     D QQS             S              5    DIM(9999)                            DETAIL SEQ #
     D QQC             S              2    DIM(9999)                            ERROR MESSAGES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
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
     C*  Claim Detail File
     C     CAKY1A        KLIST
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
     C*
     C     CAKY1B        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
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
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C*  Claim Master File
     C     CMKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
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
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C*
     C     CAKY2         KLIST
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
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C     DHKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
      *** END ADD *** 09/21/98 ****************************************
     C*  Dummy Key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*  Geographical Zip Area File
     C     GZKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXFRZP
     C*
     C*  Provider Master File
     C*** BEGIN CHG *** 02/04/98 **************************************
     C********** PDKEY     KLIST
     C     P1KEY         KLIST
     C*** END CHG *** 02/04/98 ***************************************
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  Procedure Code File
     C     PRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  Reasonable & Customary File
     C     RCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD
     C                   KFLD                    XXYMD
     C*
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*  Entry Parameter List
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
     C*** BEGIN ADD *** 03/19/93 *****************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE
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
     C     *LIKE         DEFINE    CAUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    RCANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    RCMRA         SXMRA
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    RCPROF        SXPROF
     C     *LIKE         DEFINE    RCRPT         SXRPT
     C     *LIKE         DEFINE    RCSUPP        SXSUPP
     C     *LIKE         DEFINE    RCSURG        SXSURG
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*
     C     *LIKE         DEFINE    DHPRCD        XXCPCD
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
     C     *LIKE         DEFINE    RCDT          XXYMD
     C     *LIKE         DEFINE    RCDT          SYDATE
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C*
     C* Initialize fields
     C                   MOVE      'XX00000'     XXMSG#
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
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
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      010101
     C     XXRCN         ANDEQ     CARCN
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     CADSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH01                              60
      *** END ADD *** 09/21/98 ****************************************
      *
     C                   Z-ADD     CAFDAT        XXYMD
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   Z-ADD     0             MRA1              9 2
     C*
     C*** BEGIN DEL *** 05/12/94 ************************************
     C*** BEGIN ADD *** 04/28/92 ************************************
     C*********  CARCFL    IFNE 'Y'
     C*********            GOTO NXTRCD
     C*********            END
     C*** END ADD ***** 04/28/92 ************************************
     C*** END DEL ***** 05/12/94 ************************************
     C*
      *** BEG DEL *** 09/21/99 ***************************************
     C******************** EXCPTECA01
      *** END DEL *** 09/21/99 ***************************************
     C*
     C*** BEGIN ADD *** 03/19/93 *************************************
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
     C*** END ADD ***** 03/19/93 *************************************
     C*
     C                   MOVE      '0'           XX25EM            1
     C*
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
     C                   MOVEL     DHPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C*
     C                   MOVE      *BLANKS       XXFRZP
     C                   MOVE      *BLANKS       XXZIPA
     C*** BEGIN CHG *** 02/04/98 **************************************
     C********** PDKEY     CHAINPPD01                60
     C     P1KEY         CHAIN     PP101                              60
     C*** END CHG *** 02/04/98 ****************************************
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN CHG *** 02/04/98 *************************************
     C**********           MOVELPDZIP     XXFRZP
     C                   MOVEL     P1ZIP         XXFRZP
     C*** END CHG *** 02/04/98 ****************************************
     C                   END                                                    END 0 *IN60 - PPD01
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXFRZP        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C                   END                                                    END 0 *IN60 - PGZ01
     C*
     C     RCKEY         SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
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
     C*** BEGIN DEL *** 05/12/94 *****************************************8
     C******************   EXSR UPDAT
     C******************   GOTO NXTRCD                     Get next record
     C*** END DEL ***** 05/12/94 *****************************************8
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '201'         XPEOMB
     C* EOMB Code 201: This Charge has been paid per Reasonable & Customary
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE                                                   ELSE RCSTAT NE 'I'
     C                   GOTO      NXRC1
     C                   END                                                    END RCSTAT NE 'I'
     C                   END                                                    END 0 *IN60 - PRC01
     C*
     C*** BEG DEL *** 09/21/99 *************************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
     C*** END DEL *** 09/21/99 *************************************************
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*** BEG DEL *** 09/21/99 *************************************************
     C******************** END                             END 0 *IN54 - PCA01
     C*** END DEL *** 09/21/99 *************************************************
     C*
     C*** BEGIN ADD *** 05/12/94 ***********************************************
     C                   ELSE                                                   ELSE CARCFL = Y
     C*
     C*** If no match in FMRCP file, then try FMPRPUC file
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR         TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*** A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DHPRCD
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)                         INVALID PROC CODE
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR
     C*
     C*** Pay charge at 100%, if MRA = zero
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C*
     C                   ELSE                                                   ELSE PRMRA = 0
     C*
     C*** If MRA > 0, this code has been assigned a specific Max. Reimbursement
     C*** amount by the insurance company and should be paid upto that amount
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '202'         XPEOMB
     C   54              Z-ADD     PRMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   END                                                    END  PRMRA = 0
     C*
     C*** Update record and go to next read
     C*** BEG DEL *** 09/21/99 *************************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
     C*** END DEL *** 09/21/99 *************************************************
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*** BEG DEL *** 09/21/99 *************************************************
     C******************** END                             END 0 *IN54 - PCA01
     C*** BEG DEL *** 09/21/99 *************************************************
     C*
     C*** END ADD ***** 05/12/94 ***********************************************
     C*
     C                   END                                                    END CARCFL EQ 'Y'
     C*
     C* ==================================================================
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C* ==================================================================
     C*
     C*** BEG DEL *** 09/21/99 *************************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
     C*** END DEL *** 09/21/99 *************************************************
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
     C*
     C                   Z-ADD     *ZEROS        XPTOTA
     C*
     C     CSOVRP        IFGT      *ZERO
     C*** BEGIN CHG *** 09/25/01 **************************************
     C********** CSOVRP    DIV  100       XXOVRP  32
     C     CSOVRP        DIV       100           XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C*
     C                   END
     C*
     C*** BEG CHG *** 09/21/99 *************************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
     C*** END CHG *** 09/21/99 *************************************************
     C*
     C                   END                                                    END 0 *IN54 - PCA01
     C                   END                                                    END OVERRIDE TYPE
     C*
     C     NXTRCD        TAG
     C*
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   END                                                    END 0 *IN54 - PCA01
     C*
     C                   END                                                    END 0 DOWEQ *IN54
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
     C*** BEG DEL *** 09/21/99 *************************************************
     C***********DUMMY     CHAINPCA01                54
     C**N54**************  EXCPTECA01
     C*** END DEL *** 09/21/99 *************************************************
     C*
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
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
     C                   Z-ADD     1             I                 2 0
      *
     C     QQC(I)        DOWNE     *ZEROS
     C     I             ANDLE     30
      *
      * Load the Key fields
      *
     C                   MOVE      QQS(I)        XXDSEQ
     C                   MOVE      QQC(I)        XXMSG#
      *
      * Is this message already in the file for this line item
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
     C                   ADD       1             I
      *
     C                   ENDDO                                                  QQC,I NE 0
      *
     C     MSGE          ENDSR
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
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: Charges exceed maximum allowance found in
     C*                reimbursement manual
     C                   END
     C*
     C* Write the EOMB code to FDDEP
     C     XPEOMB        IFNE      *BLANK
     C                   EXSR      EOMB
     C                   ENDIF
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     DHPRCD        IFNE      *BLANK
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVEL     CAPMOD        XXPMOD
     C********** CAKY6     CHAINPCA06                54    DEL 03/19/93
     C     CAKYP         CHAIN     PCA0P                              54        CHG 03/19/93
     C*
     C     DUPCLM        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 03/19/93 ****************************************
     C********** CADSEQ    IFNE X6DSEQ
     C********** CADSEQ    OREQ X6DSEQ
     C********** CAEOB#    ANDNEX6EOB#
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C*** END CHG ***** 03/19/93 ****************************************
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C                   ELSE
     C*
     C********** CAKY6     READEPCA06                    54 DEL 03/19/93
     C     CAKYP         READE     PCA0P                                  54     ADD 03/19/93
     C  N54              GOTO      DUPCLM
     C                   END
     C                   END
     C                   END
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*** BEG DEL *** 09/21/99 *************************************************
     C********** CAKY1A    CHAINPCA01                54
     C*** END DEL *** 09/21/99 *************************************************
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   MOVEL     XPPMOD        CAPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*** BEG CHG *** 09/21/99 *************************************************
     C**N54 ************** UPDATPCA01
     C                   UPDATE    PCA13
     C*** END CHG *** 09/21/99 *************************************************
     C                   END
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     C*************************************************************************
     C*** BEG DEL *** 09/21/99 *************************************************
     O*******PCA01   E                ECA01
     C*** END DEL *** 09/21/99 *************************************************
