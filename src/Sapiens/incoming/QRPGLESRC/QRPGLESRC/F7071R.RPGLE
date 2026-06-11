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
     H*****************************************************************
     H/TITLE F7071R - PRINT EOMBS
     H*****************************************************************
     H*
     H*    >>------------------------------------------------------<<
     H*    >>>  WHENEVER A CHANGE IS MADE TO THE PRINTER OUTPUT   <<<
     H*    >>>  SPECS, THE SAME CHANGE MUST BE APPLIED TO F7200R  <<<
     H*    >>>  (EOMB REPRINT PROGRAM).                           <<<
     H*    >>------------------------------------------------------<<
     H*
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
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
     H*  12/11/98 C:31  Project: NC005
     H*                 Chg NC EOMB wording
     H*
     H*  12/07/98 C:31  Support: 15372   Project: MC145
     H*                 EOMB Heading issue
     H*                 1. Change F1K to use FAOEP values.
     H*
     H*  10/09/98 C:79  Support: 12953
     H*                 Clear procedure code print field XXCMSV
     H*                 before processing each service date to avoid
     H*                 printing unwanted values.
     H*
     H*  09/28/98 C:53  PROJ: MC145
     H*                 CORRECT AND ADD STATE WORDING
     H*
     H*  05/04/98 C:31  Support: 11001
     H*                 Base EOMB Print Issue - Medical Examiner
     H*                 1. Use EOIUSR for Medical Examiner
     H*
     H*  04/13/98 C:31  Support: 10937
     H*                 Base EOMB Print Issue
     H*                 1. Replace printer output specs
     H*                    OUTPUT SPECS HARD DELETED BUT CAN BE FOUND
     H*                    IN LIBRARY MCOLD.
     H* ------------------------------------------------------------- *
     H*  04/14/92 KRR   PROJ: FS004                                   *
     H*                 1. ADDED OVERRIDE TO USE THE CORRECT TABLE    *
     H*                    FILE                                       *
     H*                                                               *
     H*  04/28/92 KRR    PROJ:FS001                                   *
     H*                  1.CHANGE USE OF SERVICE CODE(4N) TO COMMON   *
     H*                    SERVICE CODE(11A)                          *
     H*                  2.ADDED OVERRIDE TO PROPER STATE FILE FOR    *
     H*                    FMSVP                                      *
     H*                                                               *
     H*  05/04/92 KRR    PROJ:FS001                                   *
     H*                  1.IF DESCRIPTION NOT FOUND IN SERVICE CODE   *
     H*                    FILE CHAIN TO FDHCL AND LOOK FOR DESCRIPT  *
     H*                                                               *
     H*  05/31/92 LJD    PROJ:FS001  (per TLB)                        *
     H*                  1.Chg SVKEY keyfield UNITS to always chain   *
     H*                     with *ZEROS                               *
     H*                                                               *
     H*  07/17/92 KRR    PROJ:                                        *
     H*                  1.CHG SVKEY TO CHAIN WITH THE EFFECTIVE DATE *
     H*                                                               *
     H*  08/26/92 C:05  PROJ: 99AAC                                   *
     H*                 Changed all instances of EOFLG1 to EOBTFL     *
     H*                 Changed all instances of EOFLG2 to EOHRFL     *
     H*                                                               *
     H*  10/14/92 KRR   PROJ: 99AAD                                   *
     H*                 INCORPORATED NETWORK PROCESSING               *
     H*                 1. ADDED HEAD1, DET1, TOT1 FOR NETWORK EOMB   *
     H*                 2. ADDED INDICATOR 74 TO INDICATE NETWORK     *
     H*                    INSTALLED                                  *
     H*                 3. CHANGED LENGTH OF XXDESC TO 40(WAS 52)     *
     H*                                                               *
     H*  01/14/93 C:20  PROJ: 99AAN                                   *
     H*                 CHANGED USE OF EOFLG5 FOR LOST TIME HEADING TO*
     H*                 EONLFL                                        *
     H*                                                               *
     H*  01/18/93 C:20  PROJ:                                         *
     H*                 1. MOVED EOMB CODE COLUMN TO BE THE LAST ONE  *
     H*                 2. ADDED MRA,OTHER AND NETWORK REDUCTIONS     *
     H*                    COLUMNS TO NETWORK SUMMARY REPORT          *
     H*                 3. CONDITIONED "002" MESSAGE FOR "202" ALSO   *
     H*                                                               *
     H*  02/03/93 C:20  PROJ: FS093                                   *
     H*                 1. CHANGED PRINTING OF BOTTOM DISCLAIMER      *
     H*                    DEPENDING UPON LAST BYTE OF EBPGM BEING    *
     H*                    EQUAL TO 'Y'                               *
     H*                 2. ADDED LEVEL BREAK AT SUBSCRIBER NUMBER     *
     H*                                                               *
     H*  02/11/93 C:20  PROJ:                                         *
     H*                 1. CHANGED CODE TO BLANKS OUT EOMB DESC       *
     H*                    IF NO EOMB CODE                            *
     H*                                                               *
     H*  02/17/93 C:20  PROJ:                                         *
     H*                 1. CHANGED CODE TO CORRECTLY PRINT ROUTE TO   *
     H*                    INFORMATION ON SUMMARY REPORT              *
     H*                                                               *
     H*  03/17/93 C:20  PROJ: 48/MC008                                *
     H*                 1. CHANGED CODE TO CORRECTLY HANDLE BOTH MRA  *
     H*                    AND OTHER REDUCTIONS OF NETWORK INSTALLED  *
     H*                                                               *
     H*  04/22/93 C:20  PROJ: FS088                                   *
     H*                 1. ADDED PHARMACY PROCESSING(FMRXP)           *
     H*                                                               *
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
      **                                                             **
      ** 12/22/93 C:20     REQ/PROJ: 8/SY008                         **
      **                   1. DO NOT USE "UP" VALUES BECAUSE PROGRAM **
      **                      IS SUBMITTED                           **
     H*                                                              **
     H*  03/28/95 C:20    REQ/PROJ: MCMNT                            **
     H*                   1. MODIFICATION TO GET THE CODE DESCRIPTION**
     H*                      BASED ON VALUE FOUND IN CARCFL:         **
     H*                      S = SERVICE CODE FMSVPxx                **
     H*                      Y = R&C CODE FMRCP                      **
     H*                      X = PHARMACY CODE FDHCP                 **
     H*                      N = NON-LISTED SERVICE CODE FDHCP       **
     H*                      C = PROCEDURE CODE FMPRPxx              **
     H*                                                              **
     H*  03/18/96  C:30  Project:  60ALS                             **
     H*                  Change Logical FAEOL2 to FAEOL4.            **
     H*                                                               *
     H*  05/21/96 C:53  REQ/PROJ: 25MDC                               *
     H*                 1. CHANGE SMUPL TO SMUPL2 TO RELEASE          *
     H*                    RESTRICTION OF JUST 1 USER                 *
     H*                 2. CONDITION FAEOP UPDATE ON MODE PARAMETER   *
     H*                                                              **
     H*  07/23/96 C:32    REQ/PROJ: 60ALS (MC060,#4532)              **
     H*                   1. ADD PRINT OF DIAGNOSIS CODE(S)          **
     H*                                                               *
     H*  06/19/97 C:59  REQ/PROJ: FS132                               *
     H*                 1. ALLOW DISPLAY OF NEGATIVE SIGN ON EOMB     *
     H*                    PAYMENT AMOUNT.                            *
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
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
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
     FFAEOL4    UF   E           K DISK
     FFMC1L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PRC01:PRC0W)
     FFDCAL2    IF   E           K DISK
     FSMUPL2    IF   E           K DISK
     FFMRCL1    IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFDHCL     IF   E           K DISK
     F*
     FSSYLF     IF   E           K DISK
     FFMRXL     IF   E           K DISK
     F*** BEGIN CHG *** 04/13/98 **************************************
     FFAE1L     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     F*** END CHG *** 04/13/98 ****************************************
     F*** BEG ADD *** 09/28/98 ****************************************
     FFMF1L     IF   E           K DISK
     F*** END ADD *** 09/28/98 ****************************************
     FQSYSPRT   O    F  128        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER INFDS(PR1DS)
     F                                     OFLIND(*INOA)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D RM              S             70    DIM(2) CTDATA PERRCD(1)              RMSG
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D QCA             S             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** BEGIN ADD *** 04/13/98 **************************************
     D NM              S             65    DIM(5) CTDATA PERRCD(1)              NM STATE MSG
     D*** BEG CHG *** 09/28/98 **************************************
     D SC              S             65    DIM(6) CTDATA PERRCD(1)              SC STATE MSG
     D*** END CHG *** 09/28/98 **************************************
     D*** END ADD *** 04/13/98 ****************************************
     D*** BEG ADD *** 09/28/98 **************************************
     D*** BEGIN CHG *** 12/11/98 **************************************
     D********            NC      1  22 65               NC STATE MSG
     D NC              S             65    DIM(24) CTDATA PERRCD(1)             NC STATE MSG
     D*** END CHG *** 12/11/98 ****************************************
     D FL              S             65    DIM(7) CTDATA PERRCD(1)              FL STATE MSG
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
     D*** BEGIN CHG *** 04/13/98 **************************************
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
     D*** END CHG *** 04/13/98 ****************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *** 09/28/98 **************************************
     IPRC0W
     I              RCKEY                       RCKEYW
     ISCS01
     I              CSKEY                       CSKEYW
     ISRC01
     I              RCKEY                       RCKEYZ

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             MOVE *BLANKS   XXDESC 34        ADD 11/29/93
     C                   MOVE      *BLANKS       XXDESC           28
     C*** END CHG *** 04/13/98 ****************************************
     C*
     C                   MOVE      QQCO#         XXCO#
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
     C*** BEGIN ADD *** 09/28/98 *************************************
     C*
     C*   FUND AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN CHG *** 12/07/98 **************************************
     C********             KFLD           XXSUB#
     C                   KFLD                    EOSUB#
     C********             KFLD           XXFND
     C                   KFLD                    EOFND
     C*** END CHG *** 12/07/98 ****************************************
     C*
     C*** END ADD ***** 09/28/98 *************************************
     C*** BEGIN ADD *** 09/19/92 **************************************
     C*
     C*  SUBSCRIBER MASTER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C*** END ADD *** 09/19/92 ****************************************
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
     C*** BEGIN CHG *** 04/28/92 **************************************
     C**********           KFLD           SCSVCD
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXEFDT                         07/17/92 KRR
     C*** END CHG *** 04/28/92 ****************************************
     C*** BEGIN ADD *** 05/04/92 *************************************
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*** END ADD ***** 05/04/92 *************************************
     C*
     C     PRKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    CACPT4
     C                   KFLD                    XXEFDT            7 0
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
     C*** BEGIN ADD *** 04/23/93 **************************************
     C*
     C*  NDC DRUG CODE FILE
     C*
     C     RXKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    HCNDC
     C*** END ADD *** 04/23/93 ****************************************
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
     C                   KFLD                    EOTRCD
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
     C*** BEGIN ADD *** 10/14/92 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*** BEGIN CHG *** 04/13/98 **************************************
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
     C*** END CHG *** 04/13/98 ****************************************
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
     C*** END ADD *** 10/14/92 ****************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN ADD *** 10/14/92 **************************************
     C     *IN74         IFEQ      '0'
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             MOVELRM,1      XXW140140
     C********             MOVE RM,2      XXW140
     C********             MOVELXXW140    XXRMSG124
     C                   MOVEL     RM(1)         XXRMS1           70
     C                   MOVEL     RM(2)         XXRMS2           70
     C*** END CHG *** 04/13/98 ****************************************
     C                   END                                                    END *IN74
     C*** END ADD *** 10/14/92 ****************************************
     C*
     C                   MOVE      '0'           *IN99
     C**** THIS STATEMENT IS PURPOSELY NEVER EXECUTED
     C   99
     CANN99UPCO#         CHAIN     PPR01                              60
     C*
     C*** BEG ADD *** 09/28/98 **************************************
     C*
     C*** READ EOB FILE
     C*
     C     *LOVAL        SETLL     PEO01
     C                   READ      PEO01                                  50
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN86                          NO RECORDS
     C                   EXSR      RPT1
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*** BEGIN DEL *** 12/07/98 **************************************
     C********             MOVELEOFND     XXFND
     C********             MOVELEOSUB#    XXSUB#
     C*** END DEL *** 12/07/98 ****************************************
      *
      *  IF THE ALTERNATE NAME FLAG IN FUND AUXILARY IS 'Y'; THE
      *  COMPANY NAME/ADDRESS WILL COME FROM THERE,OTHERWISE FROM THE
      *  COMPANY AUXILARY FILE
      *
     C                   MOVE      '0'           *IN84
     C     F1KY          CHAIN     PF101                              51
     C     *IN51         IFEQ      '0'
     C     F1FLG1        IFEQ      'Y'
     C                   MOVE      '1'           *IN84
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
      *
      *
      *** REMOVE LEADING BLANKS FROM THIS NAME
      *
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
      *
     C                   MOVEL     F1AAD1        XCADR1           40
     C                   MOVEL     F1AAD2        XCADR2           40
      *
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
      *
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
     C                   END
     C                   END
      *
     C     *IN84         IFEQ      '0'
     C*** END ADD *** 09/28/98 **************************************
      *
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
     C*** BEG ADD *** 09/28/98 **************************************
     C                   ENDIF                                                  END *IN84
     C*** END ADD *** 09/28/98 **************************************
     C*
     C                   MOVEL     'FEE SCHD'    XX13A            13
     C                   MOVE      'USER'        XX13A
     C*** END DEL *** 09/28/98 **************************************
     C*********
     C*** READ EOB FILE
     C*********
     C*********  *LOVAL    SETLLPEO01
     C*********            READ PEO01                    50
     C*********
     C*********  *IN50     IFEQ '1'
     C*********            MOVE '1'       *IN86            NO RECORDS
     C*********            EXSR RPT1
     C*********            EXCPTERRMSG
     C*********            GOTO TERM
     C*********            END
     C*********
     C*********  *IN50     DOWEQ'0'
     C*** END DEL *** 09/28/98 **************************************
     C*
     C*** BEGIN ADD *** 10/14/92 **************************************
     C     *IN74         IFEQ      '1'
     C                   SETOFF                                       82
     C***********EOFLG5    IFEQ 'N'                        DEL 01/14/93
     C     EONLFL        IFEQ      'N'                                          ADD 01/14/93
     C                   SETON                                        82
     C                   END                                                    END EOFLG5
     C                   END                                                    END *IN74
     C*** END ADD *** 10/14/92 ****************************************
     C*** BEGIN ADD *** 09/19/92 **************************************
     C**********
     C****GET THE SUBSCRIBER INFORMATION
     C**********
     C**********           Z-ADDEOSUB#    XXSUB#
     C********** SMKEY     CHAINPSM01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *BLANKS   XXSBNM 40
     C**********           ELSE
     C**********           MOVELSMNAME    XXSBNM 40
     C**********           END
     C*** END ADD *** 09/19/92 ****************************************
     C     EOSUB#        IFNE      XXSUB#                                       ADD 02/04/93
     C********** EOFND     IFNE XXFND                      DEL 02/04/93
     C     EOFND         ORNE      XXFND                                        ADD 02/04/93
     C     XXSUB#        IFNE      *ZEROS                                       ADD 02/04/93
     C***********XXFND     IFNE *ZEROS                     DEL 02/04/93
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
     C*** BEGIN CHG *** 04/13/98 **************************************
     C* LOAD MESSEAGE FOR PPO CONTRACTUAL AGREEMENT IF CSC DISCOUNT
     C* AMOUNT (E1AMT1) NOT EQUAL TO ZEROS.
     C*
     C                   MOVE      '0'           *IN71
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
     C*** END CHG *** 04/13/98 ****************************************
     C*
     C* * * GET CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT
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
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRD             6 0
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTOD             6 0
     C*** BEGIN ADD *** 04/13/98 **************************************
     C                   MOVE      EOAST         XXAST             2
     C*** END ADD *** 04/13/98 ****************************************
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
     C*** BEGIN ADD *** 10/14/92 **************************************
     C*** BEGIN ADD *** 01/18/93 **************************************
     C                   Z-ADD     0             TTORED            9 2
     C                   Z-ADD     0             TTNRED            9 2
     C*** END ADD *** 01/18/93 ****************************************
     C                   Z-ADD     0             XXTNR
     C                   Z-ADD     0             XXTOTH
     C                   Z-ADD     0             XXTPAY
     C*** END ADD *** 10/14/92 ****************************************
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
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSVDT            6 0
     C*
     C*  GET THE CODE DESCRIPTION.
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C*** BEGIN ADD *** 04/14/92 **************************************
     C                   CLOSE     FTABL
     C*** END ADD *** 04/14/92 ****************************************
     C*** BEGIN ADD *** 04/28/92 **************************************
     C                   CLOSE     FMSVL1
     C*** END ADD *** 04/28/92 ****************************************
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
     C*** BEGIN ADD *** 04/14/92 **************************************
     C* FTABL FILE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *** 04/14/92 ****************************************
     C*** BEGIN ADD *** 04/28/92 **************************************
     C* FMSVL1 FILE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *** 04/28/92 ****************************************
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
     C*** BEGIN ADD *** 04/14/92 **************************************
     C* FTABL FILE
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @@CMD
     C                   MOVEL     EOAST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*** END ADD *** 04/14/92 ****************************************
     C*** BEGIN ADD *** 04/28/92 **************************************
     C* FMSVL1 FILE
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(6)        @@CMD1
     C                   MOVEL     EOAST         @@ST1
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD1
     C                   PARM                    LENGTH           15 5
     C*** END ADD *** 04/28/92 ****************************************
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C*** BEGIN ADD *** 04/14/92 **************************************
     C                   OPEN      FTABL
     C*** END ADD *** 04/14/92 ****************************************
     C*** BEGIN ADD *** 04/28/92 **************************************
     C                   OPEN      FMSVL1
     C*** END ADD *** 04/28/92 ****************************************
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C*
     C                   MOVE      *BLANK        XXDESC
     C*** BEGIN CHG *** 03/28/95 ***********************************************
     C********** CAPRTY    IFEQ '4'
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C*** END CHG *** 03/28/95 *************************************************
     C                   MOVE      *BLANK        XXCODE           11
     C*** BEGIN DEL *** 04/28/92 **************************************
     C**********           MOVE CASVCD    XXCODE
     C*** END DEL *** 04/28/92 ****************************************
     C*** BEGIN ADD *** 04/28/92 **************************************
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C*** BEGIN CHG *** 08/26/97 **************************************
     C** SERVICE CODE DOES NOT USE CACPT4 CODES, CAUSES NOT TO FIND
     C*  CORRECT MATCH
     C                   MOVE      *BLANKS       XXHCPC            5
     C**********           MOVE CACPT4    XXHCPC  5
     C*** END CHG ***** 08/26/97 **************************************
     C******************   Z-ADDCAUNTS    XXUNTS  30       DEL 05/31/92
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C*** BEGIN ADD *** 07/17/92 **************************************
     C                   Z-ADD     CAFDAT        XXEFDT
     C*** END ADD *** 07/17/92 ****************************************
     C*** END ADD *** 04/28/92 ****************************************
     C********** SVKEY     CHAINPSV01                60
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  55
     C*** BEGIN ADD *** 05/04/92 **************************************
     C     *IN55         IFEQ      '0'
     C*** BEGIN CHG *** 12/22/93 **************************************
     C********** SVCO#     ANDEQUPCO#
     C     SVCO#         ANDEQ     XXCO#
     C*** END CHG *** 12/22/93 ****************************************
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C*** END ADD *** 05/04/92 ****************************************
     C  N55              MOVEL     SVDESC        XXDESC
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C                   ELSE
     C*** BEGIN ADD *** 05/04/92 **************************************
     C     HCKY          CHAIN     PHC01                              55
     C                   MOVEL     'UNLISTED'    XXDESC
     C     *IN55         IFEQ      '0'
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             MOVE HCDESC    XXDESC
     C                   MOVEL(P)  HCDESC        XXDESC
     C*** END CHG *** 04/13/98 ****************************************
     C                   END
     C                   END
     C*** BEGIN ADD *** 05/31/92 **************************************
     C                   MOVE      *BLANK        XXCODE
     C* Re-establish XXCODE for printing (will print correct UNITS)
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     CAUNTS        XXUNTS
     C                   MOVE      XXCMSV        XXCODE
     C*** END ADD *** 05/31/92 ****************************************
     C*** END ADD *** 05/04/92 ****************************************
     C                   ELSE
     C*** BEGIN CHG *** 04/23/93 **************************************
     C     CARCFL        IFNE      'X'
     C*** END CHG *** 04/23/93 ****************************************
     C                   Z-ADD     CAFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  55
     C                   MOVE      *BLANK        XXDESC
     C     *IN55         IFEQ      '0'
     C     PRCO#         ANDEQ     EOCO#
     C     PRCPT4        ANDEQ     CACPT4
     C                   MOVEL     PRDSC1        XXD100          100
     C                   MOVE      PRDSC2        XXD100
     C                   MOVEL     XXD100        XXDESC
     C                   ELSE
     C*****************************************************
     C*** CHK REAS/CUST FILE FOR HIT
     C     RCKEY         SETGT     PRC01
     C     NXTRC5        TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 12/22/93 **************************************
     C********** RCCO#     ANDEQUPCO#
     C     RCCO#         ANDEQ     XXCO#
     C*** END CHG *** 12/22/93 ****************************************
     C     RCCPT4        ANDEQ     CACPT4
     C     RCSTAT        IFNE      'I'
     C*
     C                   MOVEL     RCDSC1        XXD100          100
     C                   MOVE      RCDSC2        XXD100
     C                   MOVEL     XXD100        XXDESC
     C*
     C                   ELSE
     C                   GOTO      NXTRC5
     C                   END
     C                   ELSE
     C                   MOVEL     *BLANK        XXDESC
     C                   END
     C*****************************************************
     C                   END
     C*
     C                   MOVE      *BLANK        XXCODE
     C                   MOVEL     CACPT4        XXCODE
     C*** BEGIN ADD *** 04/23/93 **************************************
     C                   ELSE                                                   CARCFL = X
     C     HCKY          CHAIN     PHC01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     HCNDC         XXCODE
     C     RXKEY         CHAIN     PRX01                              41
     C     *IN41         IFEQ      '0'
     C                   MOVEL     RXNAME        XXDESC
     C                   ELSE
     C                   MOVE      *BLANKS       XXDESC
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       XXCODE
     C                   MOVE      *BLANKS       XXDESC
     C                   END
     C                   END
     C*** END ADD *** 04/23/93 ****************************************
     C                   END
     C*
     C                   MOVEL     CAPMOD        XXPMOD            2
     C*** BEGIN ADD *** 12/01/93 **************************************
     C     CACPT4        IFNE      'FSMRA'
     C                   Z-ADD     CATOTC        XXABIL            9 2
     C                   ELSE                                                   ELSE NE 'FSMRA'
     C                   Z-ADD     *ZEROS        XXABIL
     C                   END                                                    END NE 'FSMRA'
     C*** END ADD *** 12/01/93 ****************************************
     C*** BEGIN ADD *** 10/14/92 **************************************
     C     *IN74         IFEQ      '0'
     C*** BEGIN ADD *** 12/01/93 **************************************
     C     CACPT4        IFNE      'FSMRA'
     C                   Z-ADD     CATOTD        XXARED            9 2
     C                   ELSE                                                   ELSE NE 'FSMRA'
     C                   Z-ADD     *ZEROS        XXARED
     C                   END                                                    END NE 'FSMRA'
     C*** END ADD *** 12/01/93 ****************************************
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             Z-ADDCATOTP    XXAALW  92
     C*** NEW CALC FOR OTHER REDUCTIONS FIELD *****************8
     C********             Z-ADDCATOTP    XXAALW  92
     C                   Z-ADD     CATOTP        XXAALW            7 2
     C*** END CHG *** 04/13/98 ****************************************
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             Z-ADDCAOTHR    XXOTHR  92
     C                   Z-ADD     CAOTHR        XXOTHR            7 2
     C*** END CHG *** 04/13/98 ****************************************
     C*** NEW CALC FOR OTHER REDUCTIONS FIELD *****************8
     C     XXAALW        ADD       XXTPAY        XXTPAY            9 2
     C     XXOTHR        ADD       XXTOTH        XXTOTH           10 2
     C                   END                                                    END *IN74
     C*** END ADD *** 10/14/92 ****************************************
     C*
     C                   MOVE      CAEOMB        XXEOMB            3
     C*
     C*** BEGIN ADD *** 10/14/92 **************************************
     C     *IN74         IFEQ      '1'
     C********             Z-ADDCANDIS    XXNRED  92       CHG 11/23/93
     C                   Z-ADD     CANDIS        XXNRED            7 2          CHG 06/19/97
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
     C***********CAOTHR    IFNE *ZEROS                     DEL 03/17/93
     C     CATOTC        SUB       CAOTHR        XXRPAY            9 2          CHG 11/23/93
     C*** BEGIN CHG *** 04/13/98 **************************************
     C********             Z-ADDCAOTHR    XXOTHR  92       CHG 11/23/93
     C                   Z-ADD     CAOTHR        XXOTHR
     C*** END CHG *** 04/13/98 ****************************************
     C*********************ELSE                            DEL 03/17/93
     C***********CATOTC    SUB  CATOTD    XXRPAY           DEL 03/17/93
     C     XXRPAY        SUB       CATOTD        XXRPAY                         ADD 03/17/93
     C                   Z-ADD     CATOTD        XXARED
     C*********************END                             DEL 03/17/93
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
     C*** END ADD *** 10/14/92 ****************************************
     C*
     C*
     C*** BEGIN CHG *** 02/03/93 **************************************
     C********** CAEOMB    IFNE '002'
     C*** BEGIN ADD *** 01/18/93 **************************************
     C********** CAEOMB    ANDNE'202'
     C*** END ADD *** 01/18/93 ****************************************
     C********** CAEOMB    ANDNE'   '
     C                   MOVE      *BLANK        XXEBDE                         ADD 02/11/93
     C                   MOVE      '0'           *IN99                          ADD 02/11/93
     C     CAEOMB        IFNE      '   '
     C**********
     C**********           MOVE '1'       *IN99
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
     C*** END CHG *** 02/03/93 ****************************************
     C*
     C*** BEGIN OF ADD *** CONNIE/S0006 *** 8/22/89
     C*
     C                   MOVE      '0'           *IN98
     C     CADSEQ        IFEQ      *ZEROS
     C                   MOVE      *BLANK        XXDESC
     C                   MOVEL     XXEBDE        XXDESC
     C                   MOVE      '1'           *IN98
     C                   END
     C*
     C*** END OF ADD *** CONNIE/S0006 *** 8/22/89
     C*
     C*  ADD TOTALS FOR THE PROVIDER TOTALS LINE.
     C*
     C*** BEGIN ADD *** 10/14/92 **************************************
     C*** BEGIN ADD *** 12/01/92 **************************************
     C     CACPT4        IFNE      'FSMRA'
     C                   ADD       CATOTC        TTABIL            9 2
     C  N74              ADD       CATOTD        TTARED            9 2
     C                   END                                                    END NE 'FSMRA'
     C*** END ADD *** 12/01/93 ****************************************
     C  N74              ADD       CATOTP        TTAALW            9 2
     C   74              ADD       XXRPAY        TTAALW            9 2
     C*** BEGIN ADD *** 01/18/93 **************************************
     C   74              ADD       CAOTHR        TTORED            9 2
     C   74              ADD       CANDIS        TTNRED            9 2
     C*** END ADD *** 01/8/93 *****************************************
     C*** END ADD *** 10/14/92 ****************************************
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
     C                   READ      PEO01                                  50
     C*
     C                   END
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C  N86              EXSR      RPT4
     C/EJECT
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
     C*** BEG ADD *** 09/28/98 *************************************
     C     OVFLOW        BEGSR
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C  N74              EXCEPT    HEAD1
     C   74              EXCEPT    HEAD2                                        ADD 10/14/92
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ENDSR
     C*** END ADD *** 09/28/98 *************************************
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
     C*** BEGIN DEL *** 04/13/98 **************************************
     C***74***             EXCPTTOT1                       ADD 10/14/92
     C*** END DEL *** 04/13/98 ****************************************
     C*** BEGIN ADD *** 04/13/98 **************************************
     C*
     C*          If Network: Print network information
     C*          ------------------------------------------------------
     C     *IN74         IFEQ      *ON
     C                   EXCEPT    TOT1A
     C                   ENDIF                                                  ADD 09/28/98
     C*
     C*             If New Mexico: Print state unique message
     C*             ---------------------------------------------------
     C     XXAST         IFEQ      'NM'
     C                   DO        5             I1                3 0
     C*
     C                   EXSR      OVFLOW                                       ADD 09/28/98
     C*
     C                   MOVEL(P)  NM(I1)        XXSTM            65
     C                   EXCEPT    TOT1ST
     C                   ENDDO                                                  I1 <= 5
     C                   ENDIF                                                  AST = 'NM'
     C*
     C*             If South Carolina: Print state unique message
     C*             ---------------------------------------------------
     C     XXAST         IFEQ      'SC'
     C                   DO        6             I1                             CHG 09/28/98
     C*
     C                   EXSR      OVFLOW                                       ADD 09/28/98
     C*
     C                   MOVEL     SC(I1)        XXSTM
     C                   EXCEPT    TOT1ST
     C                   ENDDO                                                  I1 <= 6
     C                   ENDIF                                                  AST = 'SC'
     C*** BEG ADD *** 09/28/98 ****************************************
     C*
     C*             If North Carolina: Print state unique message
     C*             ---------------------------------------------------
     C     XXAST         IFEQ      'NC'
     C*** BEGIN CHG *** 12/11/98 **************************************
     C********             DO   22        I1
     C                   DO        24            I1
     C*** END CHG *** 12/11/98 ****************************************
     C*
     C                   EXSR      OVFLOW
     C*
     C                   MOVEL     NC(I1)        XXSTM
     C                   EXCEPT    TOT1ST
     C                   ENDDO                                                  I1 <= 22
     C                   ENDIF                                                  AST = 'SC'
     C*
     C*             If Florida: Print state unique message
     C*             ---------------------------------------------------
     C     XXAST         IFEQ      'FL'
     C                   DO        7             I1                3 0
     C*
     C                   EXSR      OVFLOW                                       ADD 09/28/98
     C*
     C                   MOVEL(P)  FL(I1)        XXSTM            65
     C                   EXCEPT    TOT1ST
     C                   ENDDO                                                  I1 <= 7
     C                   ENDIF                                                  AST = 'FL'
     C*
     C     *IN74         IFEQ      *ON
     C                   EXSR      OVFLOW
     C*** END ADD *** 09/28/98 ****************************************
     C                   EXCEPT    TOT1B
     C                   ENDIF                                                  *IN74 = On
     C*** END ADD *** 04/13/98 ****************************************
     C*
     C                   EXSR      RPT2
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      'P'           EOSTAT
     C                   MOVE      'H'           EOHRFL
     C** IF DETAIL BILL, FILL EOBTFL WITH "D"
     C   93              MOVE      'D'           EOBTFL
     C  N93              MOVE      'S'           EOBTFL
     C*** BEGIN ADD *** 05/21/96 ****************************************
     C     QQMODE        IFEQ      'B'
     C                   UPDATE    PEO01
     C                   ELSE
     C                   EXCEPT    REO01
     C                   ENDIF
     C*** END   ADD *** 05/21/96 ****************************************
     C*
     C                   MOVE      *BLANKS       TTM
     C*
     C                   Z-ADD     0             TTABIL            9 2
     C*** BEGIN ADD *** 01/18/93 **************************************
     C                   Z-ADD     0             TTORED            9 2
     C                   Z-ADD     0             TTNRED            9 2
     C*** END ADD *** 01/18/93 ****************************************
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
     C*** BEGIN ADD *** 02/03/93 **************************************
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
     C*** END ADD *** 02/03/93 ****************************************
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
     C*********     *IN64         IFEQ      '0'
     C*********     *IN86         ANDEQ     '0'
     C*********          EXCEPT    ROUTE
     C*********          END
     C*
     C*********     *IN65         IFEQ      '0'
     C*********                   EXCEPT    ROUTE1
     C*********                   END
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
     C*** BEGIN ADD *** 01/18/93 **************************************
     C                   ADD       TTORED        XXURED           10 2          USER O RED
     C                   ADD       TTNRED        XXUNRD           10 2          USER N RED
     C*** END ADD *** 01/18/93 ****************************************
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
     C*** BEGIN ADD *** 01/18/93 **************************************
     C                   ADD       XXURED        XXFRED           11 2          FINAL O RED
     C                   ADD       XXUNRD        XXFNRD           11 2          FINAL N RED
     C*** END ADD *** 01/18/93 ****************************************
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
     O*** BEGIN ADD *** 04/13/98 **************************************
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
     O**********D *** 04/13/98 ****************************************
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     74      XXSUB#              27
     O**********     74                          29 '-'
     O**********     74      XXSBNM              70
     O**********E            ROUTE          1
     O**********             XXFND               27
     O**********                                 29 '-'
     O**********             XZFNNM              70
     O**********E            ROUTE          1
     O**********E            ROUTE          1
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               74      XXSUB#              27
     O               74                          29 '-'
     O               74      XXSBNM              70
      ***** End Add ***** 08/05/15 ************************************
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
     O******  E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                58 'INC. 1993 - 1998'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*** BEGIN ADD *** 04/13/98 **************************************
     OQSYSPRT   E            HEAD             02
     O               74      XXSBNM              74
     O          E            HEAD             03
     O                       XXFNNM              94
     O                                          106 'PAGE:'
     O                       PAGE               115
     O          E            HEAD             04
     O                                           70 'EXPLANATION OF MEDICAL'
     O                                           79 'BENEFITS'
     O                                          106 'DATE:'
     O                       DSDATE             115 '  /  /  '
     O          E            HEAD             05
     O                                          106 'EOMB:'
     O                       XXEOB#        Z    115
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
     O*-------------------------------------------------------------------------
     O          E            HEAD             14
     O                                           14 'DIAG CD :'
     O                       EODIA1              20
     O                       EODIA2              27
     O                       EODIA3              34
     O                       EODIA4              41
     O*-------------------------------------------------------------------------
     O          E            HEAD1            16
     O                                           13 'DATE OF'
     O                 N92N91                    25 'PROCEDURE'
     O                 N92 91                    25 ' SERVICE '
     O                  92N91                    25 '    NDC  '
     O                                           71 'AMOUNT'
     O                                           99 'OTHER'
     O                                          122 'EOMB'
     O          E            HEAD1          2 17
     O                    N92                    13 'SERVICE'
     O                     92                    13 '  FILL '
     O                                           25 '  CODE   '
     O                                           52 'DESCRIPTION OF SERVICES'
     O                                           71 'CHARGED'
     O                                           86 'DISALLOWED*'
     O                                          101 'REDUCTIONS'
     O                                          116 'ALLOWANCE'
     O                                          122 'CODE'
     O*-------------------------------------------------------------------------
     O          E            DET            1
     O                       XXSVDT        Y     13
     O                       XXCODE              25
     O               92                          26 '-'
     O               92      XXPMOD              28
     O                       XXDESC              57
     O                       XXABIL        J     72
     O                       XXARED        J     87
     O                       XXOTHR        J    102
     O                       XXAALW        J    117
     O                       XXEOMB             122
     O          E    99N98   DET            1
     O                                           22 '**'
     O                       XXEBDE              94
     O*-------------------------------------------------------------------------
     O          E            TOT         2  1
     O                                           36 'TOTAL:'
     O                                           57 'AMOUNT CHARGED.....:'
     O                       TTABIL        J     72
     O          E            TOT            1
     O                                           57 'DISALLOWED.........:'
     O                       TTARED        J     87
     O          E            TOT            1
     O                                           57 'OTHER REDUCTIONS...:'
     O                       XXTOTH        J    102
     O          E            TOT            1
     O                                           57 'ALLOWANCE..........:'
     O                       TTAALW        J    117 '$'
     O          E            TOT            2
     O                                          118 '==============='
     O          E            TOT            1
     O                       XXRMS1             100
     O          E            TOT            2
     O                       XXRMS2             100
     O          E    71      TOT            1
     O                       PPOMS1             102
     O          E    71      TOT            2
     O                       PPOMSG             122
     O          E            TOT            1
     O                                           44 'IF YOU HAVE ANY QUESTION'
     O                                           67 'S REGARDING THIS REVIEW'
     O                                           90 ' PLEASE CALL OR MAIL A '
     O                                          102 'COPY OF YOUR'
     O          E            TOT            2
     O                                           41 'BILL AND THIS REVIEW.'
     O                                           62 'PLEASE REFERENCE THE'
     O                                           86 'MEDICAL EXAMINER ON ALL'
     O                                           97 'INQUIRIES.'
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
     O*-------------------------------------------------------------------------
     O          E            HEAD2            16
     O                                          102 'NETWORK'
     O          E            HEAD2            17
     O                                           13 'DATE OF'
     O                 N92N91                    25 'PROCEDURE'
     O                 N92 91                    25 'SERVICE  '
     O                  92N91                    25 '   NDC   '
     O                                           69 'AMOUNT'
     O                                           82 ' "M.R.A."'
     O                                           92 'OTHER'
     O                                          102 'DISCOUNT'
     O                                          114 'RECOM'
     O                                          120 'EOMB'
     O          E            HEAD2          2 18
     O                    N92                    13 'SERVICE'
     O                     92                    13 '  FILL '
     O                                           21 'CODE'
     O                                           52 'DESCRIPTION OF SERVICES'
     O                                           69 'BILLED'
     O                                           82 'REDUCTION'
     O                                           92 'REDUCTION'
     O                                          102 'REDUCTION'
     O                                          115 'PAYMENT'
     O                                          120 'CODE'
     O*-------------------------------------------------------------------------
     O          E            DET1           1
     O                       XXSVDT        Y     13
     O                       XXCODE              25
     O               92                          26 '-'
     O               92      XXPMOD              28
     O                       XXDESC              57
     O                       XXABIL        J     70
     O                       XXARED        J     83
     O                       XXOTHR        J     93
     O                       XXNRED        J    103
     O                       XXRPAY        J    116
     O                       XXEOMB             120
     O          E    99      DET1           1
     O                                           21 '**'
     O                       XXEBDE              94
     O*-------------------------------------------------------------------------
     O          E            TOT1A       2  1
     O                                           27 'TOTAL:'
     O                                           42 'AMOUNT BILLED.'
     O                                           56 '.............:'
     O                       TTABIL        J     70
     O          E            TOT1A          1
     O                                           42 '"MRA" REDUCTIO'
     O                                           56 'NS...........:'
     O                       TTARED        J     83
     O          E            TOT1A          1
     O                                           42 'OTHER REDUCTIO'
     O                                           56 'NS...........:'
     O                       XXTOTH        J     93
     O          E            TOT1A          1
     O                                           42 'NETWORK DISCOU'
     O                                           56 'NT REDUCTIONS:'
     O                       XXTNR         J    103
     O          E            TOT1A          1
     O                                           42 'RECOMMENDED PA'
     O                                           56 'YMENT........:'
     O                       XXTPAY        J    116
     O          E            TOT1A          1
     O                                          117 '==============='
     O          E            TOT1ST         1
     O                       XXSTM               95
     O*** BEG DEL *** 09/28/98 ***********************************
     O*****   E 21             TOT1B
     O*****                    RM,3     100
     O*****   E  2             TOT1B
     O*****                    RM,4     100
     O*** END DEL *** 09/28/98 ***********************************
     O          E    71      TOT1B          1
     O                       PPOMS1             102
     O          E    71      TOT1B          2
     O                       PPOMSG             122
     O          E            TOT1B          1
     O                                           44 'IF YOU HAVE ANY QUESTION'
     O                                           67 'S REGARDING THIS REVIEW'
     O                                           90 ' PLEASE CALL OR MAIL A '
     O                                          102 'COPY OF YOUR'
     O          E            TOT1B          2
     O                                           41 'BILL AND THIS REVIEW.'
     O                                           62 'PLEASE REFERENCE THE'
     O                                           86 'MEDICAL EXAMINER ON ALL'
     O                                           97 'INQUIRIES.'
     O          E            TOT1B          1
     O                                           57 'TO:'
     O                       XCNAME             100
     O          E            TOT1B          1
     O                       XCADR1             100
     O          E    94      TOT1B          1
     O                       XCADR2             100
     O          E            TOT1B          1
     O                       XCCSZ              100
     O          E            TOT1B          1
     O                       XCPHN#              74
     O          E    95      TOT1B            60
     O                       TTMSG1              78
     O          E    96      TOT1B            61
     O                       TTMSG2              78
     O          E    97      TOT1B            62
     O                       TTMSG3              78
     O*** END ADD *** 04/13/98 ****************************************
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
     O**********E            ROUTE1         1
     O**********E            ROUTE1         1
     O**********E            ROUTE1         2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 *********************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               74      XXSUB#              27
     O               74                          29 '-'
     O               74      XXSBNM              70
      ***** Beg Add ***** 08/05/15 *********************************************
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
     O***                                58 'INC. 1993 - 1996'
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
     O                                            6 'F7071R'
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
     O               74                         137 'NETWORK'
     O          E            RPHD2          1
     O                                           54 'ACCIDENT'
     O               74                         106 'MRA'
     O               74                         123 'OTHER'
     O               74                         138 'DISCOUNT'
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
     O               74                         140 'REDUCTIONS'
     O               74                         154 'PAYMENT'
     O          E            RPDETL         1
     O                       EOCO#                3
     O                       XF#                 25
     O                       XXEOB#        Z     35
     O                       XXSS#               45
     O                       XXACDT        Y     54
     O                       XZPNAM              85
     O                       TTABIL        K     99
     O              N74      XXDSAV        K    113
     O               74      TTARED        K    113
     O               74      TTARED        J    113
     O              N74      TTAALW        K    127
     O               74      XXTOTH        K    127
     O               74      XXTNR         K    140
     O               74      XXTPAY        K    154
     O          E            RPUTOT      2  1
     O                                           13 'GROUP TOTALS:'
     O                                           34 'NUMBER EOBs PRINTED:'
     O                       XXUEOB        ZB    38
     O                       XXUBIL        KB    99
     O                       XXUSAV        KB   113
     O              N74      XXUALW        KB   127
     O               74      XXURED        KB   127
     O               74      XXUNRD        KB   140
     O               74      XXUALW        KB   154
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
     O                       XXFSAV        KB    85
     O          E    74      RPFTOT         2
     O               74                          69 'TOTAL OTHER REDUCTIONS:'
     O               74      XXFRED        KB    85
     O          E    74      RPFTOT         2
     O               74                          58 'TOTAL NETWORK DISCOUNT '
     O               74                          69 'REDUCTIONS:'
     O               74      XXFNRD        KB    85
     O          E            RPFTOT         2
     O              N74                          69 'TOTAL ALLOWANCE:'
     O               74                          67 'TOTAL RECOMMENDED PAYMEN'
     O               74                          69 'T:'
     O                       XXFALW        KB    85
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O**
     O*** BEGIN ADD *** 05/21/96 ****************************************
     OPEO01     E            REO01
     O*** END   ADD *** 05/21/96 ****************************************
** (RM)
* UNLESS OTHERWISE INDICATED ALL REDUCTIONS ARE DUE TO CHARGES EXCEEDI
NG MAXIMUM ALLOWANCE FOUND IN THE REIMBURSMENT MANUAL
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
** (NM) NEW MEXICO MESSAGE -----------------------------------------------
                                                                 <  LINE 1
                                                                 <  LINE 2
THIS CLAIM HAS BEEN REVIEWED IN ACCORDANCE WITH THE NEW MEXICO   <  LINE 3
WORKERS' COMPENSATION FEE SCHEDULE. BALANCE BILLING FOR TREATMENT<  LINE 4
FOR WORK COMP INJURY IS PROHIBITED.                              <  LINE 5
** (SC) SOUTH CAROLINA MESSAGE -------------------------------------------
A PROVIDER WHO DISAGREES WITH THIS REVIEW MAY CONTACT THE        <  LINE 1
REVIEWER TO REQUEST RECONSIDERATION.  IF THE DISPUTE CANNOT BE   <  LINE 2
RESOLVED WITH CONTACTING THE REVIEWER, THE PROVIDER HAS THE RIGHT<  LINE 3
TO REQUEST AN ADMINISTRATIVE REVIEW BY THE MEDICAL SERVICES      <  LINE 4
DIVISION OF THE SOUTH CAROLINA WORKERS' COMPENSATION COMMISION,  <  LINE 5
P.O. BOX 1715. COLUMBIA, SC 29202.                               <  LINE 6
** (NC) NORTH CAROLINA MESSAGE -------------------------------------------
THIS BILL WAS CALCULATED PER THE NORTH CAROLINA INDUSTRIAL       <  LINE 1
COMMISSION WORKERS' COMPENSATION FEE SCHEDULE. IF YOU HAVE ANY   <  LINE 2
QUESTIONS REGARDING THE ANALYSIS PLEASE CALL (GIVE PHONE NO.,    <  LINE 3
PERSON'S NAME OR SECTION) OR SEND YOUR BILLS AND ANALYSIS TO     <  LINE 4
(GIVE ADDRESS, SECTION OR PERSON'S NAME) OR FAX TO (FAX NO.).    <  LINE 5
                                                                 <  LINE 6
THE NORTH CAROLINA INDUSTRIAL COMMISSION REQUIRES DISPUTES BE    <  LINE 7
RESOLVED BETWEEN PAYOR AND PAYEE. IF UNSUCCESSFUL, THE PAYEE     <  LINE 8
SHOULD REFER TO THE MEDICAL SECTION OF THE NORTH CAROLINA        <  LINE 9
INDUSTRIAL COMMISSION BY CALLING 919-733-5055. IF ANY MEDICAL    <  LINE 0
PROVIDER DISPUTES THE MEDICAL SECTION'S OPINION, RESOLUTION CAN  <  LINE 1
BE SOUGHT BY A FORMAL HEARING BY THE FILING OF FORM 33.          <  LINE 2
                                                                 <  LINE 3
IF ANY BILL FOR SERVICES PROVIDED UNDER G.S. 97-25 BY A MEDICAL  <  LINE 4
PROVIDER IS NOT PAID WITHIN 60 DAYS AFTER RECEIVED BY THE PAYOR, <  LINE 5
THE PAYOR IS SUBJECT TO A 10% PENALTY WHICH IS TO BE ADDED TO    <  LINE 6
THE APPROVED BILL. IF THE PAYOR DISPUTES THE 10% PENALTY, IT     <  LINE 7
SHALL NONETHELESS PAY THE REMAINDER OF THE BILL AND THE PAYEE    <  LINE 8
CAN REQUEST A HEARING BEFORE THE NORTH CAROLINA INDUSTRIAL       <  LINE 9
COMMISSION BY FILING A FORM 33.                                  <  LINE 0
                                                                 <  LINE 1
IT IS A CRIMINAL OFFENSE TO BILL ANY PERSON FOR THE DIFFERENCE   <  LINE 2
IN BILL AND REDUCED AMOUNT PER THE NORTH CAROLINA INDUSTRIAL     <  LINE 3
COMMISSION FEE SCHEDULE, G.S. 97-90.                             <  LINE 4
** (FL) FLORIDA MESSAGE ----------------------------------------       ---
* UNLESS OTHERWISE INDICATED ALL MRA REDUCTIONS ARE DUE TO       <  LINE 1
CHARGES EXCEEDING THE MAXIMUM ALLOWANCE FOUND IN THE REIMBURSMENT<  LINE 2
MANUAL.  ANY HEALTH CARE PROVIDER WHO ELECTS TO CONTEST THE      <  LINE 3
DISALLOWANCE OR ADJUSTMENT OF PAYMENT BY A CARRIER, MUST WITHIN  <  LINE 4
30 DAYS AFTER RECEIPT OF NOTICE OF DISALLOWANCE OR ADJUSTMENT OF <  LINE 5
PAYMENT, PETITION THE DIVISION TO RESOLVE THE DISPUTE.           <  LINE 6
SEE SECTION 440.13 (7) F.S.                                      <  LINE 7
