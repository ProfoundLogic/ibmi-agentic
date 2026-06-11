     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H/TITLE F7075R - EOMB PRINT FOR CHECK REQUEST                    *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    TO RE-PRINT EOMBS WITH THE WCMS CHECKS                     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W6000C - WCMS DAILY BATCH PROCESS                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: 122/MC099           *
     H*  CREATED: 09/08/93                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  10/11/99  C:62  PROJECT: MC212
     H*                  ADD LOGIC FOR PBILLS.
     H*
     H*  03/09/99  C:94  PROJECT: MC152
     H*                  CHANGE FDCAL2 TO FDCALL AND CHANGE KEYLIST
     H*
     H*  01/14/99 C:94  PROJECT: 25MKL
     H*                 CHANGE FAEOL3 TO FAEOLG AND CHANGE KLIST
     H*
     H*  10/26/98 C:79  Support: 13503
     H*                 Add code to exclude FSMRA from being added to
     H*                 the Amount Billed column.
     H*
     H*  10/09/98 C:79  Support: 12953
     H*                 Clear procedure code print field XXCMSV
     H*                 before processing each service date to avoid
     H*                 printing unwanted values.
     H*
     H*  05/04/98 C:31  Support: 11001
     H*                 Base EOMB Print Issue - Medical Examiner
     H*                 1. Use EOIUSR for Medical Examiner
     H*
     H* ------------------------------------------------------------- *
     H*  10/29/93 C:05  PROJ: FSMNT                                   *
     H*                 1. Chgd FUND heading on EOMB back to 60A      *
     H*                                                               *
     H*  11/10/93 C:20  PROJ: FSMNT                                   *
     H*                 1. MOVE FARTHEST RIGHT HAND PRINTING IN 2 SPCS*
     H*                                                               *
     H*  11/29/93 C:20  PROJ: FSMNT                                   *
     H*                 1. Increased workfield lengths from 7.2 to 9.2*
     H*                 2. Decrease Code Desc from 40 to 34 to        *
     H*                    accomodate increase in workfields          *
     H*                                                               *
     H*  12/01/93 C:20   PROJ: FSMNT
     H*                  If CACPT4 = 'FSMRA', don't add into
     H*                     1.  Don't display as Charge field (XXABIL)
     H*                     2.  Don't add into Total Charge field (TTABIL)
     H*                     3.  Don't display as Reductions field (XXARED)
     H*                     4.  Don't add into Total Reductions field (TTARED)
     H*                                                              **
     H*  12/22/93 C:20     REQ/PROJ: 8/SY008                         **
     H*                    1. DO NOT USE "UP" VALUES BECAUSE PROGRAM **
     H*                       IS SUBMITTED                           **
     H*                                                              **
     H*  04/26/94 C:20    REQ/PROJ: 128/FS127                        **
     H*                   1. MODIFICATION TO GET THE CODE DESCRIPTION**
     H*                      BASED ON VALUE FOUND IN CARCFL:         **
     H*                      S = SERVICE CODE FMSVPxx                **
     H*                      Y = R&C CODE FMRCP                      **
     H*                      X = PHARMACY CODE FDHCP                 **
     H*                      N = NON-LISTED SERVICE CODE FDHCP       **
     H*                      C = PROCEDURE CODE FMPRPxx              **
     H*                                                              **
     H*  06/23/95 C:20    REQ/PROJ: NM001                            **
     H*                   1. IF NM ADJUDICATION STATE PRINT:         **
     H*                      "THIS CLAIM HAS BEEN REVIEWED IN ACCOR  **
     H*                      DANCE WITH THE NEW MEXICO WORKERS' COMP **
     H*                      ENSATION FEE SCHEDULE.  BALANCE BILLING **
     H*                      FOR TREATMENT FOR WORK COMP INJURY IS   **
     H*                      PROHIBITED."                            **
     H*                                                               *
     H*  01/24/96 C:41  PROJ: 48MAL                                   *
     H*                 1. INCREASE RIGHT/LEFT MARGIN FOR EOMB PRINT. *
     H*                                                               *
     H*  04/09/96 C:20  REQ/PROJ: 190/FS132                           *
     H*                 1. ALLOW DIAPLY OF NEGATIVE SIGN ON EOMB      *
     H*                    PAYMENT AMOUNT.                            *
     H*                                                               *
     H*  10/16/96 C:32  REQ/PROJ: OK005                               *
     H*                 1. CHG CRITERIA FOR PROCESSING 'FSMRA'        *
     H*                    FSMRA REC WAS CHGD TO HAVE A ZERO IN CATOTC*
     H*                    AND A NEGATIVE PAID AMT IN CATOTD (DENIED) *
     H*                  CROSS FOOT TOTL = 0 -(- CATOTD) = +REC PAYMNT*
     H*                 2. CHG TO PRINT DESC OF FSMRA                 *
     H*                                                               *
     H*  12/06/96 C:45  REQ/PROJ: 201/MC149                           *
     H*                 1. RE-ALLIGNED LEFT MARGINS TO ACCOMODATE ALL *
     H*                    PRINTERS.                                  *
     H*                 2. ADDED NETWORK DISCLAIMER.                  *
     H*                 3. REVISED XXRMSG PRINTING AT THE BOTTOM OF   *
     H*                    EOMB TO FIT NEW OUTPUT.                    *
     H*                 4. REVISED ROUTING HEADER PAGE LEFT MARGIN.   *
     H*                 5. SHORTEN DESCRIPTION ON THE NETWORK EOMB.   *
     H*                    ONLY.                                      *
     H*                                                               *
     H*  08/26/97 C:59  REQ/PROJ: MC087/8474                          *
     H*                 1. CHANGE PRINTING, SO DESCRIPTION OF SERVICE *
     H*                    CODE DISPLAYS WHEN ENTERED.  PRIOR EDITS   *
     H*                    WERE NOT DISPLAYING CORRECTLY BECAUSE      *
     H*                    EDIT LOOKS AT CPT4 IN SERVICE CODE AND     *
     H*                    USERS ARE NOW ALLOWED TO ENTER DUMMY CPT4  *
     H*                    CODES, CAUSES SERVICE CODE TO NOT FIND MATCH
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR  FUNCTION DESCRIPTION                            *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING                           *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY EOB FILE               *
     H*       51      GENERAL USAGE (CHAIN)                           *
     H*       52      READ/DOWEQ OF DETAIL CHARGE FILE                *
     H*       55      GENERAL USAGE (CHAIN)                           *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       74      NETWORK PPO IS INSTALLED/PRINT NETWORK EOMB     *
     H*       80      ALL COMPANIES TO BE PROCESSED                   *
     H*      N80      ONE COMPANY TO BE PROCESSED                     *
     H*       81      PROVIDER MODIFIER = 0                           *
     H*       82      LOST TIME WILL PRINT ON HEADING(I.E. EONLFL = N)*
     H*       86      NO RECORDS - ERROR MESSAGE                      *
     H*      N86      RECORDS TO BE PROCESSED                         *
     H*       87      FIRST TIME - GET ROUTING INFORMATION            *
     H*       91      HOSPITAL BILL CHARGE/SERVICE CODE               *
     H*      N91      PROCEDURE CODE                                  *
     H*       92      PROCEDURE CODE MODIFIER NOT EQUAL TO ZERO       *
     H*       93      PROVIDER MODIFIER NOT EQUAL TO ZERO             *
     H*       94      2ND ADDRESS NOT EQUAL TO BLANK                  *
     H*       95      EOB CODE 1  NOT EQUAL TO BLANK                  *
     H*       96      EOB CODE 2  NOT EQUAL TO BLANK                  *
     H*       97      EOB CODE 3  NOT EQUAL TO BLANK                  *
     H*       99      EOB CODE NOT 002 OR BLANK                       *
     H*****************************************************************
     FWACPL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     FFMC1L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFMF1L     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PRC01:PRC0W)
     F*** BEG CHG *** 03/09/99 ***************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 ***************************************
     FSMUPL     IF   E           K DISK
     FFMRCL1    IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFDHCL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFMRXL     IF   E           K DISK
     FFAE1L     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     FQSYSPRT   O    F  128        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER INFDS(PR1DS)
     F                                     OFLIND(*INOA)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D RM              S             70    DIM(4) CTDATA PERRCD(1)              RMSG
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D QCA             S             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  EOS                    1      9
     D                                     DIM(3)
     D  EOEOB1                 1      3
     D  EOEOB2                 4      6
     D  EOEOB3                 7      9
     D                 DS
     D  TTM                    1    210
     D                                     DIM(3)
     D  TTMSG1                 1     70
     D  TTMSG2                71    140
     D  TTMSG3               141    210
     D                 DS
     D  XSZIP                  1      9  0
     D  C1ZPP1                 1      5  0
     D  C1ZPP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND
     D  XSNAME                 1     40
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND
     D  FNNAME                 1     60
     D                 DS
     D  PRNM                   1     40
     D                                     DIM(40) ASCEND
     D  EOPNAM                 1     40
     D                 DS
     D  XF#                    1     20
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6
     D  XFFND                  7      9  0
     D  XFHYP2                10     10
     D  XFYR                  11     12  0
     D  XFHYP3                13     13
     D  XFCASE                14     20  0
     D                 DS
     D  XCPHN#                 1     14
     D  XXAREA                 2      4  0
     D  XXCPRN                 5      5
     D  XXPH1                  7      9  0
     D  XXHP1                 10     10
     D  XXPH2                 11     14  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D*
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD1                 1     55
     D  @@ST1                 40     41
     D*
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D*
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PR1DS           DS
     D  FILL3                  1    200
     D  FILL4                201    366
     D  X1LINE               367    368B 0
     D  X1PAGE               369    372B 0
     D*
     D PPOMS1          C                   CONST('* THIS BILL HAS BEEN-
     D                                      REVIEWED IN ACCORDA-
     D                                     NCE WITH YOUR AGREEM-
     D                                     ENT WITH THE')
     D*
     D PPOMS2          C                   CONST('WORKERS'' COMPENSATI-
     D                                     ON PREFERRED PROVIDE-
     D                                     R NETWORK.')
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPRC0W
     I              RCKEY                       RCKEYW
     ISCS01
     I              CSKEY                       CSKEYW
     ISRC01
     I              RCKEY                       RCKEYZ

     C*
     C*  CHECK PROCESSING KEY LIST
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*  EOMB TRANSACTION FILE
     C*
     C*** BEG CHG *** XX/XX/XX ***************************************
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C********** EOKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           RPEOB#
     C*** END CHG *** XX/XX/XX ***************************************
     C*
     C*  COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C     F1KY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  SUBSCRIBER MASTER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C*
     C*  PAYEE DEMOGRAPHICS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXEFDT                         07/17/92 KRR
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C     PRKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    CACPT4
     C                   KFLD                    XXEFDT
     C*
     C*
     C     RCKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    CACPT4
     C*
     C*  TABLE FILE KEY LIST
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  NDC DRUG CODE FILE
     C*
     C     RXKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    HCNDC
     C*
     C*  CASE DETAIL FILE.
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*********            KFLD           EOTRCD           DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARMETER FILE
     C*
     C     UPKEY         KLIST
     C*** BEGIN CHG *** 05/04/98 **************************************
     C********             KFLD           EOUSER
     C                   KFLD                    EOIUSR
     C*** END CHG *** 05/04/98 ****************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY1         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C*
     C*  NETWORK MASTER FILE
     C*
     C     NWKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXNWCD
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     *LIKE         DEFINE    FNDT          XXEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    CPPRDT        XXDATE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    CAANMN        XXANMN
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*** END   ADD *** 10/11/99 **************************************
     C                   MOVE      *BLANKS       XXDESC           24            ADD 04/26/94
     C                   MOVE      *BLANKS       XXDES2           20
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C* CHAIN OUT TO SYSTEM CONTROL FILE TO SEE IF NETWORK IS INSTALLED
     C* IF IT IS(I.E. FBFL06 = Y) MOVE '1' TO *IN74
     C*
     C                   MOVE      '0'           *IN74
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL06        IFEQ      'Y'
     C                   MOVE      '1'           *IN74
     C                   END                                                    END FBFL06
     C                   END                                                    END *IN60
     C*
     C     *IN74         IFEQ      '0'
     C                   MOVEL     RM(1)         XXRMS1           70
     C                   MOVEL     RM(2)         XXRMS2           70
     C                   END                                                    END *IN74
     C*
     C                   MOVE      '0'           *IN99
     C**** THIS STATEMENT IS PURPOSELY NEVER EXECUTED
     C   99
     CANN99UPCO#         CHAIN     PPR01                              60
     C*** BEGIN ADD *** 04/27/98 **************************************
     C*
     C* CHECK FMF1L TO SEE IF ALTERNATIVE ADDRESS SHOULD BE USED.
     C*
     C     F1KY          CHAIN     PF101                              99
     C*
     C     F1FLG1        IFEQ      'Y'
     C*
     C                   MOVE      F1AZIP        XSZIP
     C                   MOVE      F1ANME        XSNAME                         FOR EOB FORM
     C                   MOVE      F1ANME        XXCONM           40            FOR REPORT
     C                   MOVEL     '('           XCPHN#
     C                   Z-ADD     F1AARA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     F1APHN        XXPH1
     C                   MOVE      F1APHN        XXPH2
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C*
     C*
     C*** REMOVE LEADING BLANKS FROM THIS NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     F1AAD1        XCADR1           40
     C                   MOVEL     F1AAD2        XCADR2           40
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     F1AST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVEL     F1AZIP        C1ZPP1
     C                   MOVEL     F1AZIP        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C                   MOVE      F1AZIP        C1ZPP2
     C                   MOVE      F1AZIP        XXZP2             4
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     F1ACTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C*
     C                   ELSE
     C*** END   ADD *** 04/27/98 **************************************
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME                         FOR EOB FORM
     C                   MOVE      C1NAME        XXCONM           40            FOR REPORT
     C                   MOVEL     '('           XCPHN#
     C                   Z-ADD     C1AREA        XXAREA
     C                   MOVE      ')'           XXCPRN
     C                   MOVEL     C1PHN#        XXPH1
     C                   MOVE      C1PHN#        XXPH2
     C                   MOVE      '-'           XXHP1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C*
     C*
     C*** REMOVE LEADING BLANKS FROM THIS NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1           40
     C                   MOVEL     C1ADR2        XCADR2           40
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVEL     C1ZIP         C1ZPP1
     C                   MOVEL     C1ZIP         XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C                   MOVE      C1ZIP         C1ZPP2
     C                   MOVE      C1ZIP         XXZP2             4
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C                   END
     C*** BEGIN ADD *** 04/27/98 **************************************
     C                   ENDIF                                                  F1FLG1='Y'
     C*** END   ADD *** 04/27/98 **************************************
     C                   MOVEL     'FEE SCHD'    XX13A            13
     C                   MOVE      'USER'        XX13A
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C     CPKY          CHAIN     PCP01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE FLIPPED
     C*
     C     CPSTA0        IFEQ      'Y'
     C     CPSTA1        ANDEQ     'Y'
     C*
     C*  START READING ALL PAYMENT RECORDS IN PRINTED ORDER
     C*
     C     RPKY          CHAIN     PRP01                              51
     C     *IN51         DOWEQ     '0'
     C*
     C*  IF RECORD PROCESSED, READ EOMB FILE
     C*
     C     RPSTAT        IFEQ      'Y'
     C*
     C*  GET THE SUBSCRIBER #
     C*
     C     FNKY1         CHAIN     PFN02                              60
     C                   Z-ADD     FNSUB#        XXSUB#
     C*
     C     EOKY          CHAIN     PEO01                              52
     C     *IN52         IFEQ      '0'
     C     *IN74         IFEQ      '1'
     C                   SETOFF                                       82
     C     EONLFL        IFEQ      'N'                                          ADD 01/14/93
     C                   SETON                                        82
     C                   END                                                    END EOFLG5
     C                   END                                                    END *IN74
     C*
     C     EOSUB#        IFNE      XXSUB#                                       ADD 02/04/93
     C     EOFND         ORNE      XXFND                                        ADD 02/04/93
     C     XXSUB#        IFNE      *ZEROS                                       ADD 02/04/93
     C     XXFND         ORNE      *ZEROS                                       ADD 02/04/93
     C                   EXSR      RPT3
     C                   END                                                    ENDNE XXFND
     C                   EXSR      RPT1
     C                   END                                                    ENDNE *BLNKS
     C*
     C* * * GET PROVIDER INFORMATION.
     C*
     C                   MOVE      *BLANKS       XXPRNM           40
     C                   MOVEL     EOPNAM        XXPRNM
     C                   MOVE      EOFEIN        XXPID             9 0
     C                   MOVE      EOMOD         XXPIDM            3 0
     C                   MOVEL     EOADR1        XXPADD           40
     C                   MOVEL     EOCTY         XXPCTY           25
     C                   MOVEL     EOST          XXPST             2
     C                   MOVEL     EOZIP         XXPZP1            5 0
     C                   MOVE      EOZIP         XXPZP2            4 0
     C     XXPZP2        IFNE      *ZERO
     C                   MOVE      '1'           *IN66
     C                   ELSE
     C                   MOVE      '0'           *IN66
     C                   END
     C*                                                                *********
     C* LOAD MESSEAGE FOR PPO CONTRACTUAL AGREEMENT IF CSC DISCOUNT
     C* AMOUNT (E1AMT1) NOT EQUAL TO ZEROS.
     C*
     C                   MOVE      '0'           *IN71
     C*** BEGIN ADD *** 10/11/99 **************************************
     C     ##PRVB        IFNE      'Y'
     C*** END   ADD *** 10/11/99 **************************************
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             T                 2 0
     C     5             SUBST     E120A1:T      XXNWCD            5    65
     C     XXNWCD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN71
     C     NWKEY         CHAIN     PNW01                              60
     C     *IN60         IFEQ      '0'
     C     NWNAME        CAT       PPOMS2:1      PPOMSG           90
     C                   ENDIF                                                  END *IN60 EQ '0'
     C                   ENDIF                                                  XXNWCD EQ *BLANKS
     C                   ENDIF                                                  E1KEY = '0'
     C*** BEGIN ADD *** 10/11/99 **************************************
     C                   ELSE
     C*
     C     EONTWK        IFNE      *BLANKS
     C*
     C                   MOVE      '1'           *IN71
     C*
     C     NWKEY         CHAIN     PNW01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     NWNAME        CAT       PPOMS2:1      PPOMSG           90
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C                   ENDIF                                                  XXNWCD EQ *BLANKS
     C*
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C* * * GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   MOVE      CMSS#         XXSS#             9
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        XXSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     CMSS#         CHAIN     PCD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXPNAM           40
     C                   MOVEL     XXLAST        XZPNAM           25            LJD 8/25/88
     C                   ELSE
     C                   MOVEL     *BLANKS       XXPNAM           40
     C                   MOVEL     *BLANKS       XZPNAM                         LJD 8/25/88
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     0             XXACDT            6 0
     C                   MOVE      *BLANKS       XXSS#             9
     C                   MOVE      *BLANKS       XXCCLM           13
     C                   MOVEL     *BLANKS       XXPNAM           40
     C                   MOVEL     *BLANKS       XZPNAM                         LJD 8/25/88
     C                   END
     C*
     C                   Z-ADD     0             PAGE
     C                   MOVE      EOEOB#        XXEOB#
     C*
     C* FILE NUMBER
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C*
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRD             6 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTOD             6 0
     C*
     C                   MOVE      '0'           *IN67
     C     EOAST         IFEQ      'NM'
     C                   MOVE      '1'           *IN67
     C                   END
     C*
     C* * * GET ADJUSTOR NAME/NUMBER
     C*
     C                   MOVE      *BLANKS       XXADJ            40
     C     UPKEY         CHAIN     PUP01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXADJ
     C                   END
     C*
     C                   Z-ADD     0             TTABIL            9 2
     C                   Z-ADD     0             TTARED            9 2
     C                   Z-ADD     0             TTAALW            9 2
     C                   Z-ADD     0             TOTDUE            9 2
     C                   Z-ADD     0             TTORED            9 2
     C                   Z-ADD     0             TTNRED            9 2
     C                   Z-ADD     0             XXTNR
     C                   Z-ADD     0             XXTOTH
     C                   Z-ADD     0             XXTPAY
     C*
     C*  GET THE CASE DETAIL CLAIM RECORD.
     C*
     C     CAKEY         CHAIN     PCA01                              52
     C*
     C*  TEST FOR A HOSPTIAL PROVIDER TYPE.
     C*
     C     CAPRTY        COMP      '4'                                    91
     C     CARCFL        COMP      'X'                                    92
     C*
     C                   EXCEPT    HEAD
     C  N74              EXCEPT    HEAD1
     C   74              EXCEPT    HEAD2                                        ADD 10/14/92
     C*
     C*  READ ALL THE CASE RECORDS IN ORDER TO PRINT THEM NOW.
     C*
     C     *IN52         DOWEQ     '0'
      *** BEG ADD *** 10/09/98 ****************************************
      *
      * Initialize procedure code data structure
      *
     C                   CLEAR                   XXCMSV
      *** END ADD *** 10/09/98 ****************************************
     C*
     C*  CONVERT ALL THE DATES TO MM/DD/YY FORMAT.
     C*
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSVDT            6 0
     C*
     C*  GET THE CODE DESCRIPTION.
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   CLOSE     FTABL
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C* FMPRL FILE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C* FTABL FILE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C* FMSVL1 FILE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C* FMPRL FILE
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     EOAST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C* FTABL FILE
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @@CMD
     C                   MOVEL     EOAST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C* FMSVL1 FILE
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(6)        @@CMD1
     C                   MOVEL     EOAST         @@ST1
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD1
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   OPEN      FTABL
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C*
     C                   MOVE      *BLANK        XXDESC
     C                   MOVE      *BLANKS       XXDES2           20
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
      *** BEG DEL *** 10/26/98 ****************************************
     C********** CACPT4    OREQ 'FSMRA'                    ADD 10/16/96
      *** END DEL *** 10/26/98 ****************************************
     C                   MOVE      *BLANK        XXCODE           11
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C*** BEGIN CHG *** 08/26/97 **************************************
     C** SERVICE CODE DOES NOT USE CACPT4 CODES, CAUSES NOT TO FIND
     C*  CORRECT MATCH
     C                   MOVE      *BLANKS       XXHCPC            5
     C**********           MOVE CACPT4    XXHCPC  5
     C*** END CHG ***** 08/26/97 **************************************
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C                   Z-ADD     CAFDAT        XXEFDT
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  55
     C     *IN55         IFEQ      '0'
     C     SVCO#         ANDEQ     XXCO#
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C  N55              MOVEL     SVDESC        XXDESC
     C  N55              MOVEL     SVDESC        XXDES2
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C                   ELSE
     C     HCKY          CHAIN     PHC01                              55
     C                   MOVEL     'UNLISTED'    XXDESC
     C                   MOVEL     'UNLISTED'    XXDES2
     C     *IN55         IFEQ      '0'
     C                   MOVE      HCDESC        XXDESC
     C                   MOVE      HCDESC        XXDES2
     C                   END
     C                   END
     C                   MOVE      *BLANK        XXCODE
     C* Re-establish XXCODE for printing (will print correct UNITS)
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     CAUNTS        XXUNTS
     C                   MOVE      XXCMSV        XXCODE
     C                   ELSE
     C     CARCFL        IFNE      'X'
     C                   Z-ADD     CAFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  55
     C                   MOVE      *BLANK        XXDESC
     C                   MOVE      *BLANK        XXDES2
     C     *IN55         IFEQ      '0'
     C     PRCO#         ANDEQ     EOCO#
     C     PRCPT4        ANDEQ     CACPT4
     C                   MOVEL     PRDSC1        XXD100          100
     C                   MOVE      PRDSC2        XXD100
     C                   MOVEL     XXD100        XXDESC
     C                   MOVEL     XXD100        XXDES2
     C                   ELSE
     C*****************************************************
     C*** CHK REAS/CUST FILE FOR HIT
     C     RCKEY         SETGT     PRC01
     C     NXTRC5        TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     XXCO#
     C     RCCPT4        ANDEQ     CACPT4
     C     RCSTAT        IFNE      'I'
     C*
     C                   MOVEL     RCDSC1        XXD100          100
     C                   MOVE      RCDSC2        XXD100
     C                   MOVEL     XXD100        XXDESC
     C                   MOVEL     XXD100        XXDES2
     C*
     C                   ELSE
     C                   GOTO      NXTRC5
     C                   END
     C                   ELSE
     C                   MOVEL     *BLANK        XXDESC
     C                   MOVE      *BLANKS       XXDES2
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        XXCODE
     C                   MOVEL     CACPT4        XXCODE
     C                   ELSE                                                   CARCFL = X
     C     HCKY          CHAIN     PHC01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     HCNDC         XXCODE
     C     RXKEY         CHAIN     PRX01                              41
     C     *IN41         IFEQ      '0'
     C                   MOVEL     RXNAME        XXDESC
     C                   MOVEL     RXNAME        XXDES2
     C                   ELSE
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       XXDES2
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       XXCODE
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       XXDES2
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVEL     CAPMOD        XXPMOD            2
     C*** BEG CHG *** 10/26/98 ****************************************
     C******************** Z-ADDCATOTC    XXABIL  92
     C     CACPT4        IFNE      'FSMRA'
     C                   Z-ADD     CATOTC        XXABIL            9 2
     C                   ELSE                                                   ELSE NE 'FSMRA'
     C                   Z-ADD     *ZEROS        XXABIL
     C                   END                                                    END NE 'FSMRA'
     C*** END CHG *** 10/26/98 ****************************************
     C     *IN74         IFEQ      '0'
     C                   Z-ADD     CATOTD        XXARED            9 2
     C                   Z-ADD     CATOTP        XXAALW            9 2
     C                   END                                                    END *IN74
     C*
     C                   MOVE      CAEOMB        XXEOMB            3
     C*
     C     *IN74         IFEQ      '1'
     C                   Z-ADD     CANDIS        XXNRED            7 2          CHG 04/09/96
     C                   ADD       CANDIS        XXTNR            10 2
     C*
     C*** IF OTHER REDUCTIONS - CAOTHR - IS BLANK, SUBTRACT MRA DEDUCTIONS
     C*** IF OTHER REDUCTIONS - CAOTHR - IS NOT BLANK, SUB OTHER DEDUCTIONS
     C*** TO GET THE REC PAYMENT ...
     C*
     C                   Z-ADD     *ZEROS        XXRPAY
     C                   Z-ADD     *ZEROS        XXOTHR
     C                   Z-ADD     *ZEROS        XXARED
     C**
     C     CATOTC        SUB       CAOTHR        XXRPAY            9 2          CHG 11/23/93
     C                   Z-ADD     CAOTHR        XXOTHR            7 2          CHG 04/09/96
     C     XXRPAY        SUB       CATOTD        XXRPAY                         ADD 03/17/93
     C                   Z-ADD     CATOTD        XXARED
     C**
     C** NOW SUBTRACT THE NETWORK DISCOUNT
     C**
     C     XXRPAY        SUB       CANDIS        XXRPAY
     C*
     C** ACCUMULATE THE TOTALS FOR THIS EOB
     C*
     C     XXRPAY        ADD       XXTPAY        XXTPAY            9 2
     C     XXOTHR        ADD       XXTOTH        XXTOTH           10 2
     C     XXARED        ADD       TTARED        TTARED
     C                   END                                                    END *IN74
     C*
     C*
     C                   MOVE      *BLANK        XXEBDE                         ADD 02/11/93
     C                   MOVE      '0'           *IN99                          ADD 02/11/93
     C     CAEOMB        IFNE      '   '
     C*
     C*
     C*  GET THE EOMB CODE DESCRIPTION.
     C*         *** THIS WILL BE FOR ANY BUT 002!!!!! AND WILL OUTPUT ALSO
     C*
     C                   MOVEL     CAEOMB        XXKEY
     C     TBLKEY        CHAIN     PEB01                              55
     C   55              MOVE      *BLANK        XXEBDE
     C  N55              MOVEL     EBDESC        XXEBDE           70
     C*
     C                   Z-ADD     10            T                 2 0
     C     1             SUBST     EBPGM:T       XXFLAG            1    65
     C     XXFLAG        IFEQ      'Y'
     C                   MOVE      '0'           *IN99
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   END                                                    END XXFLAG
     C*
     C                   END                                                    END CAEOMB
     C*
     C*  ADD TOTALS FOR THE PROVIDER TOTALS LINE.
     C*
     C*** BEG CHG *** 10/26/98 ****************************************
     C******************** ADD  CATOTC    TTABIL  92
     C     CACPT4        IFNE      'FSMRA'
     C*
     C                   ADD       CATOTC        TTABIL
     C*
     C                   ENDIF                                                  END NE 'FSMRA'
     C*** END CHG *** 10/26/98 ****************************************
     C  N74              ADD       CATOTD        TTARED            9 2
     C  N74              ADD       CATOTP        TTAALW            9 2
     C   74              ADD       XXRPAY        TTAALW            9 2
     C   74              ADD       CAOTHR        TTORED            9 2
     C   74              ADD       CANDIS        TTNRED            9 2
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C  N74              EXCEPT    HEAD1
     C   74              EXCEPT    HEAD2                                        ADD 10/14/92
     C                   MOVE      '0'           *INOF
     C                   END
     C*** SET ANY CONTROLING INDICATORS HERE
     C*
     C     XXPMOD        COMP      *BLANKS                            9292
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C  N74              EXCEPT    DET
     C   74              EXCEPT    DET1                                         ADD 10/14/92
     C*
     C*  READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  52
     C*
     C*  END OF READE LOOP CONTROLLED BY *IN52.
     C*
     C                   END
     C*
     C                   EXSR      TOTEOB
     C*
     C                   END
     C*
     C                   END
     C     RPKY          READE     PRP01                                  51
     C                   END
     C*
     C                   END
     C     CPKY          READE     PCP01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '1'
     C     XXFND         ANDEQ     0
     C                   MOVE      '1'           *IN86                          NO RECORDS
     C                   EXSR      RPT1
     C                   EXCEPT    ERRMSG
     C                   END
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found and *in86 = '0';
          Except Route;
       Endif;
       xxPrt = '1';
       Chain Orkey Por01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C  N86              EXSR      RPT4
     C/EJECT
     **------------------------------------------------------------------*
     *** EOMB FORM TOTALS                                    T O T E O B *
     **------------------------------------------------------------------*
     C*
     C     TOTEOB        BEGSR
     C*
     C*
     C*  GET THE EOMB CODE DESCRIPTION.
     C                   DO        3             X                 5 0
     C     EOS(X)        IFNE      *BLANK
     C                   MOVEL     EOS(X)        XXKEY
     C     TBLKEY        CHAIN     PEB01                              55
     C*
     C     X             ADD       94            I                 5 0
     C     *IN55         IFEQ      '0'                                          DESCRIPTION IS
     C                   MOVEL     EBDESC        TTM(X)                         PRESENT
     C                   MOVE      '1'           *IN(I)
     C                   ELSE
     C                   MOVE      *BLANK        TTM(X)
     C                   MOVE      '0'           *IN(I)
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     XCADR2        COMP      *BLANKS                            9494
     C*
     C     XXLINE        IFGE      43                                           NOT ENOUGH ROOM
     C                   EXCEPT    HEAD                                         TO PRINT FOOT-
     C  N74              EXCEPT    HEAD1
     C   74              EXCEPT    HEAD2                                        ADD 10/14/92
     C                   END
     C*
     C  N74              EXCEPT    TOT
     C   74              EXCEPT    TOT1                                         ADD 10/14/92
     C*
     C                   EXSR      RPT2
     C*
     C                   MOVE      *BLANKS       TTM
     C*
     C                   Z-ADD     0             TTABIL            9 2
     C                   Z-ADD     0             TTORED            9 2
     C                   Z-ADD     0             TTNRED            9 2
     C                   Z-ADD     0             TTARED            9 2
     C                   Z-ADD     0             TTAALW            9 2
     C                   Z-ADD     0             TOTDUE            9 2
     C                   Z-ADD     0             #LIN
     C*
     C                   ENDSR
     C*****************************************************************
     C* RPT1 SUBROUTINE - REPORT HEADINGS                             *
     C*****************************************************************
     C     RPT1          BEGSR
     C*
     C*** GET ROUTING INFORMATION FIRST TIME ONLY (*IN87 = 0)
     C     *IN87         IFEQ      '0'
     C*** FOR EOB FORM
     C     *IN86         IFEQ      '0'
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              64
     C                   MOVE      ORTITL        X1TITL           60
     C                   END
     C*** FOR REPORT
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              65
     C                   MOVE      ORTITL        X2TITL           60
     C                   MOVE      '1'           *IN87
     C                   END
     C*
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C                   Z-ADD     EOSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   END
     C*
     C*** GET FUND INFORMATION
     C*
     C                   MOVE      EOFND         XXFND             3 0
     C                   MOVEL     *BLANKS       XZFNNM           40
     C                   MOVEL     *BLANKS       XXFNNM           60            CHG 10/29/93
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM                         FOR EOB FORM
     C* -----***REMOVE LEADING BLANKS FROM FUND NAME FOR REPORT        E
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XZFNNM                         FOR REPORT
     C                   END
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********     *IN64         IFEQ      '0'
     C********     *IN86         ANDEQ     '0'
     C********           EXCEPT    ROUTE
     C********           END
     C*
     C********     *IN65         IFEQ      '0'
     C********           EXCEPT    ROUTE1
     C********           END
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C     *IN86         IFEQ      '0'
     C                   EXCEPT    RPHD1
     C                   EXCEPT    RPHD2
     C                   END
     C*
     C                   ENDSR
     C****
     C*****************************************************************
     C* RPT2 SUBROUTINE - CALCULATE AMT SAVED,                        *
     C*                   ACCUMUALTE USER TOTALS, PRINT DETAIL        *
     C*****************************************************************
     C     RPT2          BEGSR
     C*
     C                   ADD       1             XXUEOB            4 0          USER # EOB
     C                   ADD       TTABIL        XXUBIL           10 2          USER BILLD
     C                   ADD       TTORED        XXURED           10 2          USER O RED
     C                   ADD       TTNRED        XXUNRD           10 2          USER N RED
     C                   ADD       TTAALW        XXUALW           10 2          USER ALLOW
     C     TTABIL        SUB       TTAALW        XXDSAV           10 2          DETL SAVED
     C     XXDSAV        SUB       TTORED        XXDSAV                         ADD 03/17/93
     C     XXDSAV        SUB       TTNRED        XXDSAV                         ADD 03/17/93
     C                   ADD       XXDSAV        XXUSAV           10 2          USER SAVED
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    RPHD1
     C                   EXCEPT    RPHD2
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    RPDETL
     C*
     C                   ENDSR
     C****
     C*****************************************************************
     C* RPT3 SUBROUTINE - ACCUMULATE FINAL TOTALS                     *
     C*    PRINT USER TOTAL LINE, PRINT NEW USER HEADINGS             *
     C*****************************************************************
     C     RPT3          BEGSR
     C*
     C                   ADD       XXUEOB        XXFEOB            5 0          FINAL # EOB
     C                   ADD       XXUBIL        XXFBIL           11 2          FINAL BILLD
     C                   ADD       XXUSAV        XXFSAV           11 2          FINAL SAVED
     C                   ADD       XXUALW        XXFALW           11 2          FINAL ALLOW
     C                   ADD       XXURED        XXFRED           11 2          FINAL O RED
     C                   ADD       XXUNRD        XXFNRD           11 2          FINAL N RED
     C*
     C                   EXCEPT    RPUTOT
     C*
     C                   ENDSR
     C****
     C*****************************************************************
     C* RPT4 SUBROUTINE - PRINT FINAL TOTALS AT LR                    *
     C*****************************************************************
     C     RPT4          BEGSR
     C*
     C                   EXSR      RPT3                                         USER TOTALS
     C*
     C                   EXCEPT    RPHD1
     C*
     C                   EXCEPT    RPFTOT
     C*
     C                   ENDSR
     C****
     O/EJECT
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                 13 'XXXXXXXX'
     O**********                                 25 'XXXXXX'
     O**********                                 39 'XX      XX'
     O**********                                 50 'XXXXXXXXXX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XXX     XX'
     O**********                                 82 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 14 'XXXXXXXXX'
     O**********                                 26 'XXXXXXXX'
     O**********                                 39 'XX      XX'
     O**********                                 50 'XXXXXXXXXX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XXXX    XX'
     O**********                                 83 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'XX      XX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XXXX    XX'
     O**********                                 84 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 15 'XX      XX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 45 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX XX   XX'
     O**********                                 76 'XX'
     O**********E            ROUTE          1
     O**********                                 14 'XXXXXXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX XX   XX'
     O**********                                 76 'XX'
     O**********E            ROUTE          1
     O**********                                 13 'XXXXXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX  XX  XX'
     O**********                                 76 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX  XX  XX'
     O**********                                 84 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX   XX XX'
     O**********                                 84 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                 11 'XX XXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX   XX XX'
     O**********                                 84 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 12 'XX  XXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX    XXXX'
     O**********                                 84 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 13 'XX   XXX'
     O**********                                 26 'XXXXXXXX'
     O**********                                 38 'XXXXXXXX'
     O**********                                 46 'XX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XX    XXXX'
     O**********                                 83 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                 14 'XX    XXX'
     O**********                                 25 'XXXXXX'
     O**********                                 37 'XXXXXX'
     O**********                                 46 'XX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XX     XXX'
     O**********                                 82 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     74      XXSUB#              27
     O**********     74                          29 '-'
     O**********     74      XXSBNM              70
     O**********E            ROUTE          1
     O**********             XXFND               27
     O**********                                 29 '-'
     O**********             XZFNNM              70
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               74      XXSUB#              27
     O               74                          29 '-'
     O               74      XXSBNM              70
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O          E            ROUTE          1
     O          E            ROUTE          2
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O******* E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                62 'INC. 1997            '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD             02
     O               74      XXSBNM              74
     O          E            HEAD             03
     O                       XXFNNM              94
     O                                          108 'PAGE:'
     O                       PAGE               117
     O          E            HEAD             04
     O                                           70 'EXPLANATION OF MEDICAL'
     O                                           79 'BENEFITS'
     O                                          108 'DATE:'
     O                       DSDATE             117 '  /  /  '
     O          E            HEAD             05
     O                                          108 'EOMB:'
     O                       XXEOB#        Z    121
     O          E            HEAD             08
     O                                           14 'PATIENT :'
     O                       XXPNAM              55
     O                                           82 'PROVIDER ID:'
     O                       XXPID         Z     92
     O                       XXPIDM        Z     96
     O          E            HEAD             09
     O                                           14 'SOC SEC :'
     O                       XXSS#               24
     O                                           82 'PROVIDER   :'
     O                       XXPRNM             123
     O          E            HEAD             10
     O                                           14 'ACC DT  :'
     O                       XXACDT              23 '  /  /  '
     O                       XXPADD             123
     O          E            HEAD             11
     O                                           14 'CASE #  :'
     O                       XF#                 35
     O                       XXPCTY             108
     O                       XXPST              111
     O                       XXPZP1             118
     O               66                         119 '-'
     O               66      XXPZP2             123
     O          E            HEAD             12
     O                                           14 'SRV DTS :'
     O                       XXFRD         Y     23
     O                                           25 '-'
     O                       XXTOD         Y     34
     O                                           82 'MEDICAL EXAMINER:'
     O                       XXADJ              123
     O          E            HEAD1            16
     O                                           13 'DATE OF'
     O                 N92N91                    25 'PROCEDURE'
     O                 N92 91                    25 ' SERVICE '
     O                  92N91                    25 '    NDC  '
     O                                           88 'AMOUNT'
     O                                          122 'EOMB'
     O          E            HEAD1          2 17
     O                    N92                    13 'SERVICE'
     O                     92                    13 '  FILL '
     O                                           25 '  CODE   '
     O                                           52 'DESCRIPTION OF SERVICES'
     O                                           89 'CHARGED'
     O                                          103 'DISALLOWED*'
     O                                          117 'ALLOWANCE'
     O                                          122 'CODE'
     O          E            DET            1
     O                       XXSVDT        Y     13
     O                       XXCODE              25
     O               92                          26 '-'
     O               92      XXPMOD              28
     O                       XXDESC              57
     O                       XXABIL        1     89
     O                       XXARED        J    103
     O                       XXAALW        1    117
     O                       XXEOMB             122
     O          E    99N98   DET            1
     O                                           22 '**'
     O                       XXEBDE              91
     O          E            TOT         2  1
     O                                           43 'TOTAL:'
     O                                           74 'AMOUNT CHARGED.....:'
     O                       TTABIL        J     89
     O          E            TOT            1
     O                                           74 'DISALLOWED.........:'
     O                       TTARED        J    103
     O          E            TOT            1
     O                                           74 'ALLOWANCE..........:'
     O                       TTAALW        1    118 '$'
     O          E            TOT            2
     O                                          119 '==============='
     O          E            TOT            1
     O                       XXRMS1             100
     O          E            TOT            2
     O                       XXRMS2             100
     O          E            TOT            1
     O                                           44 'IF YOU HAVE ANY QUESTION'
     O                                           67 'S REGARDING THIS REVIEW'
     O                                           90 ' PLEASE CALL OR MAIL A '
     O                                           97 'COPY OF'
     O          E            TOT            2
     O*
     O                                           40 'YOUR BILL AND THIS REVIE'
     O                                           42 'W.'
     O                                           65 'PLEASE REFERENCE THE '
     O                                           88 'MEDICAL EXAMINER ON ALL'
     O                                           99 'INQUIRIES.'
     O          E            TOT            1
     O                                           57 'TO:'
     O                       XCNAME             100
     O          E            TOT            1
     O                       XCADR1             100
     O          E    94      TOT            1
     O                       XCADR2             100
     O          E            TOT            1
     O                       XCCSZ              100
     O          E            TOT            1
     O                       XCPHN#              74
     O          E    95      TOT              60
     O                       TTMSG1              72
     O          E    96      TOT              61
     O                       TTMSG2              72
     O          E    97      TOT              62
     O                       TTMSG3              72
     O          E            HEAD2            16
     O                                          100 'NETWORK'
     O*
     O          E            HEAD2            17
     O                                           13 'DATE OF'
     O                 N92N91                    25 'PROCEDURE'
     O                 N92 91                    25 'SERVICE  '
     O                  92N91                    25 '   NDC   '
     O                                           62 'AMOUNT'
     O                                           76 ' "M.R.A."'
     O                                           88 'OTHER'
     O                                          102 'DISCOUNT'
     O                                          115 'RECOM'
     O                                          121 'EOMB'
     O          E            HEAD2          2 18
     O                    N92                    13 'SERVICE'
     O                     92                    13 '  FILL '
     O                                           21 'CODE'
     O                                           52 'DESCRIPTION OF SERVICES'
     O                                           62 'BILLED'
     O                                           76 'REDUCTIONS'
     O                                           89 'REDUCTIONS'
     O                                          102 'REDUCTION'
     O                                          116 'PAYMENT'
     O                                          121 'CODE'
     O          E            DET1           1
     O                       XXSVDT        Y     13
     O                       XXCODE              25
     O               92                          26 '-'
     O               92      XXPMOD              28
     O                       XXDES2              49
     O                       XXABIL        1     62
     O                       XXARED        J     76
     O                       XXOTHR        J     89
     O                       XXNRED        J    102
     O                       XXRPAY        J    117
     O                       XXEOMB             121
     O          E    99      DET1           1
     O                                           21 '**'
     O                       XXEBDE              94
     O          E            TOT1        2  1
     O                                           18 'TOTAL:'
     O                                           50 'AMOUNT BILLED .....:'
     O                       TTABIL        1     62
     O          E            TOT1           1
     O                                           50 '"MRA" REDUCTIONS ......:'
     O                       TTARED        J     76
     O          E            TOT1           1
     O                                           37 'OTHER REDUCTIONS'
     O                                           50 '...........:'
     O                       XXTOTH        J     89
     O          E            TOT1           1
     O                                           37 'NETWORK DISCOUNT'
     O                                           50 'REDUCTIONS :'
     O                       XXTNR         J    102
     O          E            TOT1           1
     O                                           37 'RECOMMENDED PAYM'
     O                                           50 'ENT ........:'
     O                       XXTPAY        J    117
     O          E            TOT1           1
     O                                          117 '_______________'
     O          E    67      TOT1        2  1
     O                                           24 'THIS CLAIM HAS BEEN REVI'
     O                                           48 'EWED IN ACCORDANCE WITH '
     O                                           71 'THE NEW MEXICO WORKERS'''
     O                                           96 'COMPENSATION FEE SCHEDUL'
     O                                          120 'E.  BALANCE BILLING FOR '
     O          E    67      TOT1
     O                                           24 'TREATMENT FOR WORK COMP '
     O                                           48 'INJURY IS PROHIBITED.   '
     O          E            TOT1        2  1
     O                       RM(3)              100
     O          E            TOT1           2
     O                       RM(4)              100
     O          E    71      TOT1           1
     O                       PPOMS1             102
     O          E    71      TOT1           2
     O                       PPOMSG             120
     O          E            TOT1           1
     O                                           44 'IF YOU HAVE ANY QUESTION'
     O                                           67 'S REGARDING THIS REVIEW'
     O                                           90 ' PLEASE CALL OR MAIL A '
     O                                           97 'COPY OF'
     O          E            TOT1           2
     O*
     O                                           40 'YOUR BILL AND THIS REVIE'
     O                                           42 'W.'
     O                                           65 'PLEASE REFERENCE THE '
     O                                           88 'MEDICAL EXAMINER ON ALL'
     O                                           99 'INQUIRIES.'
     O          E            TOT1           1
     O                                           57 'TO:'
     O                       XCNAME             100
     O          E            TOT1           1
     O                       XCADR1             100
     O          E    94      TOT1           1
     O                       XCADR2             100
     O          E            TOT1           1
     O                       XCCSZ              100
     O          E            TOT1           1
     O                       XCPHN#              74
     O          E    95      TOT1             60
     O                       TTMSG1              72
     O          E    96      TOT1             61
     O                       TTMSG2              72
     O          E    97      TOT1             62
     O                       TTMSG3              72
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     74      XXSUB#              27
     O**********     74                          29 '-'
     O**********     74      XXSBNM              70
     O**********E            ROUTE1         1
     O**********             XXFND               27
     O**********                                 29 '-'
     O**********             XZFNNM              70
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               74      XXSUB#              27
     O               74                          29 '-'
     O               74      XXSBNM              70
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O          E            ROUTE1         1
     O          E            ROUTE1         2
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                62 'INC. 1997            '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            RPHD1          1 06
     O                       DSDATE        Y      8
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            RPHD1          2
     O                                            6 'F7075R'
     O                       X2TITL              94
     O                       PAGE               127
     O                                          122 'PAGE:'
     O          E            RPHD2          2
     O                                           06 'SUBS:'
     O                       XXSUB#              13
     O                                           14 '-'
     O                       XXSBNM              55
     O          E            RPHD2          2
     O                                           06 'GROUP:'
     O                       XXFND               13
     O                                           14 '-'
     O                       XZFNNM              55
     O          E            RPHD2          1
     O               74                         139 'NETWORK'
     O          E            RPHD2          1
     O                                           54 'ACCIDENT'
     O               74                         106 'MRA'
     O               74                         123 'OTHER'
     O               74                         140 'DISCOUNT'
     O               74                         156 'RECOMMENDED'
     O          E            RPHD2          2
     O                                            3 'CO#'
     O                                           11 'FILE #'
     O                                           33 'EOB #'
     O                                           45 'SOC. SEC. #'
     O                                           51 'DATE'
     O                                           70 'CLIENT NAME'
     O                                           98 'AMT CHARGED'
     O               74                         112 'REDUCTIONS'
     O              N74                         112 'DISALLOWED'
     O               74                         126 'REDUCTIONS'
     O              N74                         126 'ALLOWANCE'
     O               74                         142 'REDUCTIONS'
     O               74                         154 'PAYMENT'
     O          E            RPDETL         1
     O                       EOCO#                3
     O                       XF#                 25
     O                       XXEOB#        Z     35
     O                       XXSS#               45
     O                       XXACDT        Y     54
     O                       XZPNAM              85
     O                       TTABIL        K     99
     O              N74      XXDSAV        J    113
     O               74      TTARED        J    113
     O              N74      TTAALW        J    127
     O               74      XXTOTH        J    127
     O               74      XXTNR         J    142
     O               74      XXTPAY        K    156
     O          E            RPUTOT      2  1
     O                                           13 'GROUP TOTALS:'
     O                                           34 'NUMBER EOBs PRINTED:'
     O                       XXUEOB        ZB    38
     O                       XXUBIL        KB    99
     O                       XXUSAV        JB   113
     O              N74      XXUALW        JB   127
     O               74      XXURED        JB   127
     O               74      XXUNRD        JB   142
     O               74      XXUALW        KB   156
     O          E            RPFTOT         2
     O                                           30 'ALL GROUP TOTALS'
     O          E            RPFTOT         2
     O                                           64 'TOTAL NUMBER EOBs PRI'
     O                                           69 'NTED:'
     O                       XXFEOB        ZB    84
     O          E            RPFTOT         2
     O                                           69 'TOTAL AMOUNT BILLED:'
     O                       XXFBIL        KB    85
     O          E            RPFTOT         2
     O              N74                          69 'TOTAL AMOUNT SAVED:'
     O               74                          69 'TOTAL MRA REDUCTIONS:'
     O                       XXFSAV        JB    85
     O          E    74      RPFTOT         2
     O               74                          69 'TOTAL OTHER REDUCTIONS:'
     O               74      XXFRED        JB    85
     O          E    74      RPFTOT         2
     O               74                          58 'TOTAL NETWORK DISCOUNT '
     O               74                          69 'REDUCTIONS:'
     O               74      XXFNRD        JB    85
     O          E            RPFTOT         2
     O              N74                          69 'TOTAL ALLOWANCE:'
     O               74                          67 'TOTAL RECOMMENDED PAYMEN'
     O               74                          69 'T:'
     O                       XXFALW        KB    85
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O**
** (RM)
* UNLESS OTHERWISE INDICATED ALL REDUCTIONS ARE DUE TO CHARGES
EXCEEDING MAXIMUM ALLOWANCE FOUND IN THE REIMBURSMENT MANUAL
* UNLESS OTHERWISE INDICATED ALL MRA REDUCTIONS ARE DUE TO CHARGES
EXCEEDING THE MAXIMUM ALLOWANCE FOUND IN THE REIMBURSMENT MANUAL.
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
