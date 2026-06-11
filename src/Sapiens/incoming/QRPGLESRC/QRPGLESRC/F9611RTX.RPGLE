     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H*    PROGRAM: F9611RTX                                          *
     H*    DESCRIPTION: CREATE WORKFILE AND REPORT                    *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      F9611CTX - CREATE WORKFILE                               *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  09/29/04  C:79  PROJECT: TX024                               *
     H*                  Use FAEHP/EHFLIC to report license number    *
     H*                                                               *
     H*  05/13/04  C:62  SUPPORT:MC025 42044                          *
     H*                  Ignore inactive and no print EOBs            *
     H*                                                               *
     H*  04/15/04  C:62  SUPPORT:MC025 42044                          *
     H*                  Add processing for Correct Coding EOBs       *
     H*                                                               *
     H*  09/17/03  C:79  SUPPORT:MC025 39076                          *
     H*                  Use the Check Mailing Date as Date Paid      *
     H*                                                               *
     H*  06/20/03  C:79  SUPPORT:MC025 37598                          *
     H*                  Report the 2nd modifier field                *
     H*                                                               *
     H*  09/06/02  C:79  SUPPORT:MC025 34034                          *
     H*                  On recons make sure Hosp bills report the    *
     H*                  ROOM code for inpatient bills                *
     H*                                                               *
     H*  07/11/02  C:79  SUPPORT:MC025 33348                          *
     H*                  1. Use DHBPRC only if not blank.             *
     H*                                                               *
     H*  03/21/02  C:07  SUPPORT:MC025 31715                          *
     H*                  1. Load EOB code not the note.               *
     H*                  2. Use sum of DACHRG at the recon level for  *
     H*                     reporting charges not CACHRG for Phy bills*
     H*                                                               *
     H*  02/08/02  C:062 PROJECT:TX017                                *
     H*                  ADD RESUBMISSION PROCESSING.                 *
     H*                                                               *
     H*  12/19/01  C:079 Support:MC025 30380                          *
     H*                  If bill is recon then use recon received date*
     H*                                                               *
     H*  10/11/01  C:079 Support:MC025 29089                          *
     H*                  If bill is recon then use recon received date*
     H*                  for the received date                        *
     H*                                                               *
     H*  08/27/01  C:062 PROJECT:TX015                                *
     H*                  -UPDATE FEECDE WITH DHBPRC                   *
     H*                                                               *
     H*  06/12/01  C:079 SUPPORT: MC025 27701                         *
     H*                  1. DO NOT UPDAT EOUSER                       *
     H*                                                               *
     H*  04/05/01  C:079 SUPPORT: MC025 25456                         *
     H*                  1. FOR UB92 USE 16A CONTROL NUMBER AND FOR   *
     H*                     FOR HCFA USE 22A CONTROL NUMBER           *
     H*                  2. IF INPATIENT HOSP THEN REQUIRE ROOM FOR   *
     H*                     X1 RECORD                                 *
     H*                  3. INITIALIZE ITIMIZED CHARGES 2,4,6 TO      *
     H*                     BLANKS                                    *
     H*                  4. USE MOVEL FOR MEDICARE # EUMCAR. STATE    *
     H*                     NEEDS 1ST 10 POSITIONS                    *
     H*                  5. Get header EOB at the recon level and read*
     H*                     through all sequence numbers              *
     H*                  6. Itemized charges 4,5,6,7 are only allowed *
     H*                     on acute care inpatient bills             *
     H*                                                               *
     H*                  7. Use EUPCM to get procedure code method.   *
     H*                     Do not substring to get it.               *
     H*                                                               *
     H*  12/11/00  C:117 PROJECT: TX013
     H*                  UPDATE EOCNTL WITH EOB#
     H*
     H*  06/29/00  C:117 PROJECT: TX011                               *
     H*                  ADDED NEW FIELDS AND REVISED SOME OF THE     *
     H*                  FILLER SPACES                                *
     H*                                                               *
     H*  07/07/00  C:79   Project: MCMNT                              *
     H*                   1. Change conversion of paid date so it     *
     H*                      prints on the report.                    *
     H*                                                               *
     H*  03/10/00  C:79   Support#: MC025/#22159                      *
     H*                   1. Check for Header w/o detail and don't    *
     H*                      include this EOB#                        *
     H*                   2. Remove duplicate logic as this can cause *
     H*                      a count mismatch                         *
     H*                   3. Change conversion of EAWCPD because it   *
     H*                      is now in CYMD in the database           *
     H*                                                               *
     H*  12/13/99  C:79   Project#: Y2K                               *
     H*                   1. Expand report dates to include century   *
     H*
     H*  09/10/99  C:79   Support#: 19005                             *
     H*                   1. Initialize itemized charges to blanks or *
     H*                      or zero as required                      *
     H*                                                               *
     H*  07/09/99  C:79   Support#: 17939                             *
     H*                   1. EOMB code lookup is truncating two       *
     H*                      places. Correct this                     *
     H*                                                               *
     H*  05/05/99  C:56   MC152                                       *
     H*                   Change FAEAL to FAEAL1, duplicate logicals  *
     H*                                                               *
     H*  01/06/99 C:79 PROJ:TX009/Support#: 17939                     *
     H*                1.  Modify to work with YR 2000 dates          *
     H*                2.  Modify to work with Provider Bills         *
     H*                3.  Initialize Itemized charge fields 2,4,6 to *
     H*                    zeros                                      *
     H*                3.  Chain to header auxiliary files when       *
     H*                    counting type 2 records                    *
     H*                4.  Remove code to get EOMB code for Hospital  *
     H*                    header. Add code in detail section         *
     H*                5.  Use CACHRG instead of DACHRG               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  40   -   PRINT ONE DETAIL 2 LINE ON HOSPITAL BILLS           *
     H*  60   -   GENERAL CHAIN                                       *
     H*  61   -   GENERAL CHAIN                                       *
     H*  62   -   GENERAL CHAIN                                       *
     H*  63   -   GENERAL CHAIN                                       *
     H*  64   -   GENERAL CHAIN                                       *
     H*  65   -   OUT/MISC/IR HIT; DO NOT PRINT MORE DETAIL RECORDS   *
     H*  76   -   OUT/MISC/IR DETAIL                                  *
     H*  77   -   FSMRA DETAIL                                        *
     H*  78   -   WRITE X-DETAIL TO REPORT                            *
     H*  79   -   MOVE BATCH CONTROL HEADER RECORD TO FILE            *
     H*  80   -   NO HIT ON FMTSP - ADD NEW SEQ# RECORD BY CO         *
     H* N80   -   HIT ON FMTSP - UPDATE SEQ# RECORD BY CO             *
     H*  88   -   NO EOMB FOR FUND WITHIN DATES                       *
     H*  89   -   PHYSICIAN                                           *
     H* N89   -   HOSPITAL                                            *
     H*  90   -   PHYSICIAN (CALLED FOR BY USER)                      *
     H* N90   -   HOSPITAL (CALLED FOR BY USER)                       *
     H*  91   -   ACUTE INPATIENT                                     *
     H*  92   -   ICU/CCU ACUTE INPAIENT                              *
     H*  93   -   IMPLANTABLES UNDER ACUTE INPATIENT                  *
     H*  94   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  95   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  96   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  97   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  98   -   OUT/MISC/IR FOR REPORT                              *
     H*******************************************************************
     H*  FILES:                                                         *
     H*  FAEOL KEY LIST: CO/SUB#/FND/FYR/CASE/TRDT/FEIN/MOD/EOB#        *
     H*        OPEN QUERY SELECTION:    QQFRDT GE EOTRDT LE QQTODT      *
     H*                                 EOTAPE EQ 'Y', EASUB# EQ QQSUB# *
     H*                                 IF QQFND NE 000, QQFND EQ EOFND *
     H*******************************************************************
     F*** BEG CHG *** 06/26/00 ****************************************
     F****FFATPPTX O   F      64            DISK                      A
     F*****FATHPTX O   F      64            DISK                      A
     FFATPPTX   O  A F   80        DISK
     FFATHPTX   O  A F   80        DISK
     F*** END CHG *** 06/26/00 ****************************************
     FSMCOL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   UF  E           K        DISK
     FFAEAL1    UF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     FFMFNL     IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*** BEGIN CHG *** 02/08/02 **************************************
     F**********FAEHL   IF  E           K        DISK
     F**********FAEUL   IF  E           K        DISK
     FFAEHL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     F*** END   CHG *** 02/08/02 **************************************
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDAL     IF   E           K DISK
     F*** BEG ADD *** 03/21/02 ****************************************
     FFDDAL2    IF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     F*** END ADD *** 03/21/02 ****************************************
     FFDDEL4    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     F*** BEG ADD *** 01/06/99 ****************************************
     F*** BEG CHG *** 04/05/01 ****************************************
     F*****FAEEL   IF  E           K        DISK
     FFAEEL3    IF   E           K DISK
     F*** END CHG *** 04/05/01 ****************************************
     F*** END ADD *** 01/06/99 ****************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FFMTSP     UF A E           K DISK
     FFTABLTX   IF   E           K DISK
     F                                     IGNORE(PTS01)
     F*** BEG ADD *** 10/11/01 ****************************************
     FFAESL3    IF   E           K DISK

     D*** END ADD *** 03/21/02 ****************************************
     D*
     D*** BEG ADD *** 09/10/99 ****************************************
     D FILL28          C                   CONST('  000000000000000000-
     D                                     000     ')
     D*** END ADD *** 09/10/99 ****************************************
     D                 DS
     D  EOBMED                 1      8
     D  EOB                    1      3
     D  MEDPD                  4      8
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXDT                   1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D*
     D* Year prior to current date's year; used to calc Julian date
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      8  0
     D*
     D* Data Structure for Claim Control Number
     D                 DS
     D  CNTL                   1     13
     D  CCC                    1      3
     D  YY                     4      5
     D  JJJ                    6      8
     D  SSSSS                  9     13
     D*
     D* TYPE OF SERVICE Data Structure
     D                 DS
     D  TYPE1                  1      3
     D  EUICN                  1      2
     D  EUDCN                  3      3
     D*
     D* FILLER DATA STRUCTURE
     D                 DS                  INZ
     D  FILL24                 1     24
     D  FLB2                   1      2
     D  FLN3                   3      5  0
     D  FLN72                  6     14  2
     D  FLN72A                15     23  2
     D  FLB1                  24     24
     D*** BEG ADD *** 06/29/00 ****************************************
     D*
     D*  PROVIDER'S LISCENSE NUMBER
     D*
     D                 DS
     D*** BEG CHG *** 09/29/04 ****************************************
     D*********************************       1   8 PRLIC#
     D*********************************       1   2 PREFIX
     D*********************************       3   8 PRNUM
     D  PRLIC#                 1     10
     D  PREFIX                 1      2
     D  PRNUM                  3     10
     D*** END CHG *** 09/29/04 ****************************************
     D*** END ADD *** 06/29/00 ****************************************
     D*
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*** END ADD *** 10/11/01 ****************************************

     I*** BEG ADD *** 03/21/02 ****************************************
     I*
     I*  Rename the duplicate field names.
     IPDA02
     I              DACO#                       D2CO#
     I              DASUB#                      D2SUB#
     I              DAFND                       D2FND
     I              DAFYR                       D2FYR
     I              DACASE                      D2CASE
     I              DASS#                       D2SS#
     I              DAACDT                      D2ACDT
     I              DAEOB#                      D2EOB#
     I              DADSEQ                      D2DSEQ
     I              DARCN#                      D2RCN#
     I              DATRDT                      D2TRDT
     I              DACHRG                      D2CHRG
     I              DAMRA                       D2MRA
     I              DAOTHR                      D2OTHR
     I              DAOTH2                      D2OTH2
     I              DANDIS                      D2NDIS
     I              DAPAID                      D2PAID
     I              DAOTY1                      D2OTY1
     I              DAOTY2                      D2OTY2
     I              DATRCD                      D2TRCD
     I              DASTAT                      D2STAT
     I              DAUSER                      D2USER
     I              DAPGM                       D2PGM
     I              DATIME                      D2TIME
     I              DADT                        D2DT
     I              DAIUSR                      D2IUSR
     I              DAIPGM                      D2IPGM
     I              DAITIM                      D2ITIM
     I              DAIDT                       D2IDT

     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQPROC            1
     C*** BEGIN ADD *** 02/08/02 **************************************
     C                   PARM                    QQRSUB            1
     C*** END   ADD *** 02/08/02 **************************************
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
     C*** BEG ADD *** 03/10/00 ****************************************
     C     DT1MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END ADD *** 03/10/00 ****************************************
     C*** BEG ADD *** 07/07/00 ****************************************
     C     DT2MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END ADD *** 07/07/00 ****************************************
     C*** BEG ADD *** 09/17/03 ****************************************
     C     DT3MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END ADD *** 09/17/03 ****************************************
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Company AUXILIARY Master File
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the FUND AUXILIARY Master File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Output Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the EOMB TRANSACTION FILE
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the SUBCRIBER   File
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* Key List for the Case Detail Charge File
     C*
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C* Key List for the EOMB AUXILIARY File
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     DHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     DUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     DAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C     DAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** BEG ADD *** 03/21/02 ****************************************
     C*
     C     DAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    X2DSEQ
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C     DEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXESRC
     C*
     C*** BEG ADD *** 01/06/99 ****************************************
     C     EEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEG CHG *** 04/05/01 ****************************************
     C******************** KFLD           KKESEQ
     C                   KFLD                    KKRCN
     C*** END CHG *** 04/05/01 ****************************************
     C*** END ADD *** 01/06/99 ****************************************
     C*
     C* Key List for the Claim Master File
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* Key List for the Claimant Demographics Master File
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C* Key List for the Provider Master File
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* Key List for the GROUP STATE INFORMATION FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C*
     C* Key List for the TAPE SUBMISSION SEQ# FILE
     C*
     C     TSKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA             5
     C                   KFLD                    XXLOST            1
     C*
     C* Key List for the PROVIDER AUXILIARY FILE
     C*
     C     P1KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C* Key List for the PROVIDER AUXILIARY FILE
     C*
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKEOB
     C*
     C*** BEG ADD *** 10/11/01 ****************************************
     C     ESKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C                   KFLD                    KKBLST
     C*** END ADD *** 10/11/01 ****************************************
     C*
     C* Set-up and Initialize
     C*
     C     *LIKE         DEFINE    CACHRG        SBCHRG
     C     *LIKE         DEFINE    CAPAID        SBPAID
     C*
     C     *LIKE         DEFINE    DEESRC        XXESRC
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EACASE        XXCASE
     C     *LIKE         DEFINE    EAEOB#        XXEOB#
     C     *LIKE         DEFINE    DADSEQ        XXDSEQ
     C     *LIKE         DEFINE    EARCN         XXRCN
     C     *LIKE         DEFINE    DHMOD1        XXPMOD
     C*** BEG ADD *** 03/21/02 ****************************************
     C     *LIKE         DEFINE    CACHRG        XXCHRG
     C     *LIKE         DEFINE    CACHRG        ZZCHRG
     C     *LIKE         DEFINE    EACHRG        ZZEOB#
     C     *LIKE         DEFINE    DADSEQ        X2DSEQ
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C     *LIKE         DEFINE    EBKEY         KKEOB
     C*** BEG ADD *** 01/06/99 ****************************************
     C     *LIKE         DEFINE    EESEQ#        KKESEQ
     C*** END ADD *** 01/06/99 ****************************************
     C*** BEG ADD *** 04/05/01 ****************************************
     C     *LIKE         DEFINE    EERCN         KKRCN
     C*** END ADD *** 04/05/01 ****************************************
     C*** BEG ADD *** 10/11/01 ****************************************
     C     *LIKE         DEFINE    EOBLST        KKBLST
     C*** END ADD *** 10/11/01 ****************************************
     C*
     C*** BEG ADD *** 09/17/03 ****************************************
     C                   EXSR      PGMSTR
     C*** END ADD *** 09/17/03 ****************************************
     C*
     C                   Z-ADD     0             COUNT             5 0          ADD 02/18/97
     C                   Z-ADD     0             X                 5 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XQFRDT            8 0
     C                   MOVE      QQTODT        XQTODT            8 0
     C                   Z-ADD     XQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     XQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C*** BEG CHG *** 12/13/99 ****************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXFROM  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*** END CHG *** 12/13/99 ****************************************
     C                   Z-ADD     XXTODT        SRDTI
     C*** BEG CHG *** 12/13/99 ****************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXTO    60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C*** END CHG *** 12/13/99 ****************************************
     C                   Z-ADD     0             CNTR              5 0
     C                   Z-ADD     *ZEROS        SBCHRG
     C                   Z-ADD     *ZEROS        SBPAID
     C*** BEG ADD *** 01/06/99 ****************************************
     C                   MOVE      1             KKESEQ
     C*** END ADD *** 01/06/99 ****************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQPROC        IFEQ      'P'
     C                   MOVEL     'PHYSI'       XXPROC            9
     C                   MOVE      'CIAN'        XXPROC
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C                   MOVEL     ' HOSPI'      XXPROC
     C                   MOVE      'TAL'         XXPROC
     C                   MOVE      '0'           *IN90
     C                   END
     C*
     C* Get the Julian Date
     C*
     C                   Z-ADD     DSDATE        XXDT
     C                   Z-ADD     12            ZZMM
     C                   Z-ADD     31            ZZDD
     C     XXYY          IFNE      0
     C     XXYY          SUB       1             ZZYY
     C                   ELSE
     C                   Z-ADD     99            ZZYY
     C                   END
     C*
     C* Call sub-program SRDDFF to Calc Julian Date
     C*
     C                   MOVE      ZZDT          SRFRDC            8 0
     C                   MOVE      XXDT          SRTODC            8 0
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C*
     C*  GET NEXT AVAILABLE SEQUENCE # TO BE USED IN CONTROL RECORD
     C*
     C     TSKEY         CHAIN     PTS01                              80
     C     *IN80         IFEQ      '1'
     C                   Z-ADD     0             CNTR
     C                   ELSE
     C                   MOVE      TSSEQ#        CNTR                            LAST # USED
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXCONM           40
     C                   ELSE
     C                   MOVEL     CONAME        XXCONM
     C                   END
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  GET COMPANY AUXILIARY RECORD FOR AUDIT NUMBERS
     C*  IF NON EXIST - PRINT ERROR MESSAGE AND END PROGRAM
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMS1
     C                   GOTO      TERM
     C                   END
     C*
     C* Get the Routing Information and Print Routing Page
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              61
     C     *IN61         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '0'           *IN88
     C*
     C* GET THE # OF TYPE 1A RECORDS IN THE BATCH
     C*
     C                   Z-ADD     *ZEROS        XXTP1A            5 0
     C*
     C                   READ(N)   PEA01                                  62
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     *IN62         IFEQ      *OFF
     C     QQRSUB        IFEQ      'Y'
     C*
     C                   Z-ADD     EASUB#        XXSUB#
     C                   Z-ADD     EAFND         XXFND
     C*
     C*  GET SUBCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C*
     C* Get the Fund Record
     C     FNKEY         CHAIN     PFN01                              63
     C     *IN63         IFEQ      '1'
     C                   MOVE      *BLANKS       XXFNNM           40
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNNM
     C                   END
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 02/08/02 **************************************
     C                   Z-ADD     EASUB#        XXSUB#
     C*** END   ADD *** 02/08/02 **************************************
     C                   Z-ADD     EAFND         XXFND
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EACASE        XXCASE
     C                   Z-ADD     EAEOB#        XXEOB#
     C*** BEG ADD *** 03/10/00 ****************************************
     C                   Z-ADD     EARCN         XXRCN
     C*** END ADD *** 03/10/00 ****************************************
     C*
     C*** BEGIN CHG *** 02/08/02 **************************************
     C********** EOKY1     CHAINPEO01                64
     C     EOKY1         CHAIN(N)  PEO01                              64
     C*** END   CHG *** 02/08/02 **************************************
     C*
     C     EAEDI         IFEQ      'Y'
     C     EOBTYP        ANDEQ     'N'
     C     EOAST         ANDEQ     'TX'
     C*
     C     EAEDI         OREQ      'Y'
     C     EOBTYP        ANDEQ     'H'
     C     EOAST         ANDEQ     'TX'
     C*** BEGIN ADD *** 02/08/02 **************************************
     C*
     C     EAEDI         OREQ      'R'
     C     EOBTYP        ANDEQ     'N'
     C     EOAST         ANDEQ     'TX'
     C*
     C     EAEDI         OREQ      'R'
     C     EOBTYP        ANDEQ     'H'
     C     EOAST         ANDEQ     'TX'
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C*** BEG ADD *** 03/10/00 ****************************************
     C* If there are no detail amount records don't count
     C     DAKY          CHAIN     PDA01                              59
     C     *IN59         IFEQ      *OFF
     C*** END ADD *** 03/10/00 ****************************************
     C*
     C     EASYS         IFEQ      'P'
     C                   MOVE      '1'           *IN89
     C                   ELSE
     C     EASYS         IFEQ      'H'
     C                   MOVE      '0'           *IN89
     C                   END                                                    EASYS = H
     C                   END                                                    EASYS = P
     C*
     C   89
     CAN 90XXTP1A        ADD       1             XXTP1A
     C  N89
     CANN90XXTP1A        ADD       1             XXTP1A
     C*
     C*** BEG ADD *** 03/10/00 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/10/00 ****************************************
     C*
     C                   ENDIF                                                  EAEDI = Y
     C*
     C                   READ(N)   PEA01                                  62
     C*
     C                   END                                                    DOW*IN62 = 0
     C*
     C                   MOVE      '1'           *IN79
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      *LOVAL        XXFND
     C                   MOVE      *LOVAL        XXFYR
     C*** BEG ADD *** 03/21/02 ****************************************
     C                   Z-ADD     0             ZZEOB#
     C*** END ADD *** 03/21/02 ****************************************
     C     EAKY1         SETLL     PEA01
     C*
     C**  BEGIN READING BATCH
     C*
     C                   READ      PEA01                                  62
     C*
     C     *IN62         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END                                                    *IN62 = 1
     C*
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C*
     C* INITIALIZE TOTAL CHARGES IF NEW EOB#
     C*
     C     ZZEOB#        IFNE      EAEOB#
     C                   Z-ADD     0             ZZCHRG
     C                   END                                                    ZZEOB NE
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C                   MOVE      *BLANK        RECORD            1
     C*** BEGIN ADD *** 02/08/02 **************************************
     C                   Z-ADD     EASUB#        XXSUB#
     C*** END   ADD *** 02/08/02 **************************************
     C                   Z-ADD     EAFND         XXFND
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EACASE        XXCASE
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN
     C*
     C     EOKY1         CHAIN     PEO01                              64
     C*
     C     EAEDI         IFEQ      'Y'
     C     EOBTYP        ANDEQ     'N'
     C     EOAST         ANDEQ     'TX'
     C*
     C     EAEDI         OREQ      'Y'
     C     EOBTYP        ANDEQ     'H'
     C     EOAST         ANDEQ     'TX'
     C*** BEGIN ADD *** 02/08/02 **************************************
     C*
     C     EAEDI         OREQ      'R'
     C     EOBTYP        ANDEQ     'N'
     C     EOAST         ANDEQ     'TX'
     C*
     C     EAEDI         OREQ      'R'
     C     EOBTYP        ANDEQ     'H'
     C     EOAST         ANDEQ     'TX'
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C*** BEG ADD *** 03/10/00 ****************************************
     C* If there are no detail amount records don't include EOB#
     C     DAKY          CHAIN     PDA01                              59
     C     *IN59         IFEQ      *OFF
     C*** END ADD *** 03/10/00 ****************************************
     C*
     C     EASYS         IFEQ      'P'
     C                   MOVE      '1'           *IN89
     C                   ELSE
     C     EASYS         IFEQ      'H'
     C                   MOVE      '0'           *IN89
     C                   END                                                    EOSYS = H
     C                   END                                                    EOSYS = P
     C*
     C* Set the flag for the record type, physician or hospital
     C   89
     CAN 90              MOVE      'P'           RECORD
     C  N89
     CANN90              MOVE      'H'           RECORD
     C*
     C     *IN89         IFEQ      *IN90
     C                   MOVE      '1'           *IN88
     C                   END                                                    *IN89=*IN90
     C*
     C* GET THE FUND AUXILIARY RECORD
     C     F1KEY         CHAIN     PF101                              63
     C*
     C* Get the Claim Master Record
     C     CMKEY         CHAIN     PCM01                              63
     C*
     C* Get the Claimant Demographic Record
     C     CDKEY         CHAIN     PCD01                              63
     C*
     C* Get the Provider Master Record
     C     PDKEY         CHAIN     PPD01                              63
     C*
     C* Get the GROUP STATE INFORMATION
     C     F2KEY         CHAIN     PF201                              63
     C*
     C* Get the PROVIDER AUXILARY RECORD
     C     P1KEY         CHAIN     PP101                              63
     C*
     C* Get the PHYSICAIN BILL HEADER RECORD
     C     RECORD        IFEQ      'P'
     C     EHKY          CHAIN     PEH01                              63
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     *IN63         IFEQ      *OFF
     C     EHEDI         IFEQ      'R'
     C                   MOVEL     'Y'           EHEDI
     C                   ENDIF
     C                   UPDATE    PEH01
     C                   ENDIF
     C*** END   ADD *** 02/08/02 **************************************
     C                   ENDIF                                                  RECORD=P
     C*
     C* Get the HOSPITAL BILL HEADER RECORD
     C     RECORD        IFEQ      'H'
     C     EUKY          CHAIN     PEU01                              63
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     *IN63         IFEQ      *OFF
     C     EUEDI         IFEQ      'R'
     C                   MOVEL     'Y'           EUEDI
     C                   ENDIF
     C                   UPDATE    PEU01
     C                   ENDIF
     C*** END   ADD *** 02/08/02 **************************************
     C                   ENDIF                                                  RECORD=P
     C*
     C* GET THE # OF TYPE 2 RECORDS AND FSRMA, OUT & IR TOTALS
     C*
     C                   MOVE      '0'           *IN75
     C                   MOVE      '0'           *IN76
     C                   MOVE      '0'           *IN77
     C                   Z-ADD     *ZEROS        XXTYP2            3 0
     C                   Z-ADD     *ZEROS        XXMRAC            9 2
     C*
     C* ACCUMULATE XXMRAP (CHARGES) UP TO THE RECON WE ARE ON. CLEAR
     C* IF THIS IS A NEW EOB#
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C     EAEOB#        IFNE      ZZEOB#
     C*** END ADD *** 03/21/02 ****************************************
     C                   Z-ADD     *ZEROS        XXMRAP            9 2
     C*** BEG ADD *** 03/21/02 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/21/02 ****************************************
     C                   Z-ADD     *ZEROS        SBCHRG
     C                   Z-ADD     *ZEROS        SBPAID
     C*
     C     DAKY          SETLL     PDA01
     C*** BEG CHG *** 03/21/02 ****************************************
     C********** DAKY1     READEPDA01                    63
     C     DAKY          READE     PDA01                                  63
     C*** END CHG *** 03/21/02 ****************************************
     C*
     C     *IN63         DOWEQ     '0'
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C*
     C* Accumulate charges for this EOB up to the recon you are on
     C*
     C                   ADD       DACHRG        ZZCHRG
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C                   Z-ADD     DADSEQ        XXDSEQ
     C*
     C*** BEG DEL *** 05/05/99 ****************************************
     C********** CAKY      CHAINPCA01                64
     C*** END DEL *** 05/05/99 ****************************************
     C*
     C* Get EOMB code
     C*
     C                   MOVE      'A'           XXESRC
     C                   MOVE      *BLANKS       XXEOB             1
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C* If there was a hit then move DEEOB to XXEOB
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DEEOB         XXEOB
     C                   ENDIF
     C*
     C* Do not include 'D' (duplicate) types in type 2 totals
     C*
     C*
     C**Add*total charge and paid for 'D' types
     C     XXEOB         IFEQ      'D'
     C****BEG*CHG *** 01/06/99 ****************************************
     C******************** ADD  DACHRG    SBCHRG
     C*** BEG ADD *** 03/21/02 ****************************************
     C******************** ADD  CACHRG    SBCHRG
     C                   ADD       DACHRG        SBCHRG
     C*** END ADD *** 03/21/02 ****************************************
     C****END*CHG *** 01/06/99 ****************************************
     C                   ADD       DAPAID        SBPAID
     C*
     C                   ELSE
     C*
     C     RECORD        IFEQ      'P'
     C     DHKY          CHAIN     PDH01                              60
     C                   ENDIF
     C*
     C     RECORD        IFEQ      'H'
     C     DUKY          CHAIN     PDU01                              60
     C                   ENDIF
     C*
     C   89
     CAN 90XXTYP2        ADD       1             XXTYP2
     C  N89
     CANN90DURATE        IFEQ      'FSMRA'
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** ADD  DACHRG    XXMRAC  92
     C*** BEG CHG *** 03/21/02 ****************************************
     C******************** ADD  CACHRG    XXMRAC  92
     C                   ADD       DACHRG        XXMRAC            9 2
     C*** END CHG *** 03/21/02 ****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C                   ADD       DAPAID        XXMRAP            9 2
     C                   MOVE      '1'           *IN77
     C                   END                                                    FSMRA
     C  N89
     CANN90DURATE        IFEQ      'OUT'
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** ADD  DACHRG    XXMRAC  92
     C*** BEG CHG *** 03/21/02 ****************************************
     C******************** ADD  CACHRG    XXMRAC  92
     C                   ADD       DACHRG        XXMRAC            9 2
     C*** END CHG *** 03/21/02 ****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C                   ADD       DAPAID        XXMRAP            9 2
     C                   MOVE      '1'           *IN75
     C                   END                                                    OUT
     C  N89
     CANN90DURATE        IFEQ      'IR'
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** ADD  DACHRG    XXMRAC  92
     C*** BEG CHG *** 03/21/02 ****************************************
     C******************** ADD  CACHRG    XXMRAC  92
     C                   ADD       DACHRG        XXMRAC            9 2
     C*** END CHG *** 03/21/02 ****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C                   ADD       DAPAID        XXMRAP            9 2
     C                   MOVE      '1'           *IN76
     C                   END                                                    IR
     C  N89
     CANN90DURATE        IFEQ      'MISC'
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** ADD  DACHRG    XXMRAC  92
     C*** BEG CHG *** 03/21/02 ****************************************
     C******************** ADD  CACHRG    XXMRAC  92
     C                   ADD       DACHRG        XXMRAC            9 2
     C*** END CHG *** 03/21/02 ****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C                   ADD       DAPAID        XXMRAP            9 2
     C                   MOVE      '1'           *IN74
     C                   END                                                    MISC
     C*
     C                   ENDIF                                                  DEEOB=D
     C*
     C     DAKY          READE     PDA01                                  63
     C*
     C                   END                                                    DOW*IN64 = 0
     C*
     C********************  HEADER SECTION  **********************************
     C* Clear Header record Fields
     C*
     C                   EXSR      CLRH
     C*
     C* Move information to Header fields
     C*
     C* RECORD TYPE
     C                   MOVEL     '0'           RECTYP
     C* CARRIER CODE
     C*******************  MOVE F292N1    CARCDE           DEL 11/07/95
     C                   MOVE      F29N01        CARCDE                         CHG 11/07/95
     C* LOCATION CODE
     C                   MOVE      C1LOC         LOCCDE
     C* YEAR OF SUBMISSION
     C*** BEG CHG *** 04/20/01 ****************************************
     C******************** MOVE XXYY      YRSUB
     C                   MOVE      DSDATE        YRSUB
     C*** END CHG *** 04/20/01 ****************************************
     C* JULIAN DATE
     C                   MOVE      SRDAYS        JULDT
     C* NUMBER OF TYPE 1A RECORDS
     C                   Z-ADD     XXTP1A        TYPE1A
     C* FILLER
     C*** BEG CHG *** 06/29/00 ****************************************
     C                   MOVE      *BLANKS       FILL38           38
     C                   MOVE      *BLANKS       FILL16           16
     C                   MOVE      *BLANKS       FILL22           22
     C*** END CHG *** 06/29/00 ****************************************
     C*
     C* Write the Header Record to the Work File ONCE
     C*
     C   90
     CAN 89
     CAN 79              EXCEPT    HEAD0
     C  N90
     CANN89
     CAN 79              EXCEPT    HED0
     C   90
     CAN 89
     CAN 79              MOVE      '0'           *IN79
     C  N90
     CANN89
     CAN 79              MOVE      '0'           *IN79
     C*
     C*
     C* RECORD TYPE
     C                   MOVEL     '1A'          RECTYP
     C* LAST NAME
     C                   MOVEL     CDLSN         LSNAME           20
     C* FIRST NAME
     C                   MOVEL     CDFSN         FSNAME           12
     C* MIDDLE INITIAL
     C                   MOVEL     CDMID         MID
     C                   MOVEL     MID           MID2              1
     C* SOCIAL SECURITY NUMBER
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         SSN               9 0
     C                   ELSE
     C                   MOVEL     '999'         SSN
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C                   END                                                    CMSS# = BLANK
     C* DATE OF INJURY
     C     CMACDT        IFNE      0
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         INJDT             6 0
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         INJDT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C                   ELSE
     C                   MOVE      XXYY          INJDT
     C                   MOVEL     '0000'        INJDT
     C                   END                                                    CMACDT NE 0
     C*** BEG CHG *** 06/29/00 ****************************************
     C* DATE FIRST CONSULTED NO LONGER NEEDED; CHANGING FIELD TO
     C* CARRIER CLAIM ID NUMBER
     C*
     C* DATE FIRST CONSULTED
     C     RECORD        IFEQ      'P'
     C*
     C*******              Z-ADDEHDFC     SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C*******              MOVELSRDTO     CONDT   60
     C*** BEG ADD *** 12/13/99 ****************************************
     C*******              CALL 'SRCVTDAT'DTMDYY
     C*******              MOVELSRDTO     CONDT1  80
     C*** END ADD *** 12/13/99 ****************************************
     C*** BEG CHG *** 04/05/01 ****************************************
     C******************** MOVELEOEOB#    CCID   22
     C* LOAD HCFA-1500 AND UB92 CLAIM ID NUMBERS WITH DIFF LENGTHS
     C                   MOVEL     EOEOB#        CCIDH            22
     C                   MOVEL     EOEOB#        CCIDU            16
     C*** END CHG *** 04/05/01 ****************************************
     C                   MOVEL     EOEOB#        CCID1             9
     C*
     C                   ENDIF                                                  RECORD=P
     C*** END CHG *** 06/29/00 ****************************************
     C*
     C* DATE OF CARRIER PAYMENT
     C                   Z-ADD     EAWCPD        SRDTI
     C*** BEG CHG *** 03/10/00 ****************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DT1MDY
     C*** END CHG *** 03/10/00 ****************************************
     C                   MOVEL     SRDTO         PAYDT             6 0
     C*** BEG CHG *** 07/07/00 ****************************************
     C******************** CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DT2MDY
     C*** END CHG *** 07/07/00 ****************************************
     C                   MOVEL     SRDTO         PAYDT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C*** BEG ADD *** 09/17/03 ****************************************
     C*
     C* USE THE CHECK MAILING DATE TO REPORT DATE PAID IF NOT BLANK
     C*
     C                   EXSR      GETMDT
     C*
     C*** END ADD *** 09/17/03 ****************************************
     C* ADMISSION DATE
     C     RECORD        IFEQ      'H'
     C*
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ADMDT             6 0
     C* SUBMISSION DATE
     C                   Z-ADD     EUSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SUBDT             6 0
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SUBDT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C*** BEG ADD *** 06/29/00 ****************************************
     C*** BEG CHG *** 04/05/01 ****************************************
     C******************** MOVELEOEOB#    CCID   22
     C* LOAD HCFA-1500 AND UB92 CLAIM ID NUMBERS WITH DIFF LENGTHS
     C                   MOVEL     EOEOB#        CCIDH            22
     C                   MOVEL     EOEOB#        CCIDU            16
     C*** END CHG *** 04/05/01 ****************************************
     C                   MOVEL     EOEOB#        CCID1             9
     C*** END ADD *** 06/29/00 ****************************************
     C*
     C                   ENDIF                                                  RECORD=H
     C* DATE BILL RECEIVED
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C*** BEG ADD *** 10/11/01 ****************************************
     C*
     C* If this is a recon get the received date from recon rec date
     C*
     C                   MOVE      'R'           KKBLST
     C*
     C     EARCN         IFGT      0
     C*
     C     ESKY          CHAIN     PES01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     ESRRDT        ANDNE     0
     C                   Z-ADD     ESRRDT        SRDTI
     C*
     C                   ELSE
     C                   Z-ADD     EOCRDT        SRDTI
     C*
     C                   ENDIF                                                  IN99=OFF
     C*
     C* DATE BILL RECEIVED
     C*
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C*
     C                   ENDIF                                                  EORCN GT 0
     C*** END ADD *** 10/11/01 ****************************************
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         RECDT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C*
     C* Write the Header Record to the Work File
     C*
     C   90
     CAN 89              EXCEPT    HEAD1A
     C  N90
     CANN89              EXCEPT    HED1A
     C*** BEGIN ADD *** 07/07/00 *********************************************
     C*  COUNTER IS USED TO DETERMIN THE NUMBER OF HEADERS THAT WERE PROCESSED
     C*  IN THE TAPE SUBMISSION.
     C*
     C  N90
     CANN89              ADD       1             COUNT
     C   90
     CAN 89              ADD       1             COUNT
     C*** END ADD ***** 07/07/00 *********************************************
     C* RECORD TYPE
     C                   MOVEL     '1B'          RECTYP
     C* DATE BILL RECEIVED
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C*** BEG ADD *** 10/11/01 ****************************************
     C*
     C* If this is a recon get the received date from recon rec date
     C*
     C     EARCN         IFGT      0
     C*
     C     ESKY          CHAIN     PES01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     ESRRDT        ANDNE     0
     C*
     C                   Z-ADD     ESRRDT        SRDTI
     C*
     C                   ELSE
     C                   Z-ADD     EOCRDT        SRDTI
     C*
     C                   ENDIF                                                  IN99=OFF
     C*
     C* DATE BILL RECEIVED
     C*
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C*
     C                   ENDIF                                                  EORCN GT 0
     C*** END ADD *** 10/11/01 ****************************************
     C* FILLER
     C                   MOVE      *BLANKS       FILL              1
     C* PRIMARY ICD-9
     C                   MOVE      'D'           XXLOST
     C*
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA1        XXDIA
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA1        XXDIA
     C                   ENDIF                                                  RECORD=P
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9              6
     C* SECONDARY ICD-9 1
     C*
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA2        XXDIA
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA2        XXDIA
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD91             6
     C* SECONDARY ICD-9 2
     C*
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA3        XXDIA
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA3        XXDIA
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD92             6
     C* SECONDARY ICD-9 3
     C*
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA4        XXDIA
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA4        XXDIA
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD93             6
     C*
     C* PROVIDER LICENSE NUMBER
     C*
     C     RECORD        IFEQ      'P'
     C*
     C*** BEG CHG *** 09/29/04 ****************************************
     C********** EHLIC#    IFEQ *BLANKS
     C     EHFLIC        IFEQ      *BLANKS
     C*** END CHG *** 09/29/04 ****************************************
     C     PDFLIC        IFNE      *BLANKS
     C                   MOVEL     PDFLIC        PRLIC#
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     '00000'       PRLIC#
     C                   MOVE      '0000'        PRLIC#
     C                   ENDIF                                                  END NE *BLANKS
     C                   ELSE                                                   ELSE EQ *BLANKS
     C*** BEG CHG *** 09/29/04 ****************************************
     C*******************  MOVELEHLIC#    PRLIC#
     C                   MOVEL     EHFLIC        PRLIC#
     C*** END CHG *** 09/29/04 ****************************************
     C                   END                                                    END EQ *BLANKS
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C* NUMBER OF TYPE 2 RECORDS
     C                   Z-ADD     XXTYP2        TYPE2
     C* TAX ID #
     C     EOFEIN        IFNE      0
     C                   MOVE      EOFEIN        TAXID#            9 0
     C                   END                                                    EOFEIN NE 0
     C*** BEG ADD *** 06/29/00 ****************************************
     C                   MOVEL     PDNAME        PRLSNM           19
     C*** END ADD *** 06/29/00 ****************************************
     C* SERVICE FROM DATE
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTF            6 0
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDTF1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C* SERVICE TO DATE
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTT            6 0
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDTT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C* TOTAL CHARGES SUMMED UP TO THE RECON WE ARE ON
     C     ZZCHRG        SUB       SBCHRG        TTLCHR
     C* ESTIMATED AMOUNT DUE FOR THIS RECON
     C     EAPAID        SUB       SBPAID        ESTDUE
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C*** BEG DEL *** 03/21/02 ****************************************
     C* TOTAL CHARGES
     C********** EOCHRG    SUB  SBCHRG    TTLCHR
     C* ESTIMATED AMOUNT DUE
     C********** EOPAID    SUB  SBPAID    ESTDUE
     C*** END DEL *** 03/21/02 ****************************************
     C*
     C*** BEG DEL *** 01/06/99 ****************************************
     C* PAYMENT EXCEPTION CODE
     C* HARD CODE AN S=SUPPLEMENTAL PAYMENT IF IT'S A RECON AND > 0
     C********** DARCN#    IFGT *ZERO
     C********** DAPAID    ANDGT*ZERO
     C**********
     C**********           MOVE 'S'       XXPCOD
     C**********
     C**********           ELSE
     C**********           EXSR EOMB
     C**********
     C**********           ENDIF                           DARCN GT 0
     C**********
     C**********           MOVE XXPCOD    EXCPT1
     C*** END DEL *** 01/06/99 ****************************************
     C*** BEG ADD *** 01/06/99 ****************************************
     C*** BEG DEL *** 04/05/01 ****************************************
     C**********
     C********** EEKY      CHAINPEE01                60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           MOVELEEEOB     EXCPT1
     C**********
     C**********           ENDIF
     C*** END DEL *** 04/05/01 ****************************************
     C*** END ADD *** 01/06/99 ****************************************
     C*** BEG ADD *** 04/05/01 ****************************************
     C                   Z-ADD     EARCN         KKRCN
     C*
     C     EEKY          CHAIN     PEE01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EEEOB         IFNE      *BLANK
     C                   MOVEL     EEEOB         EXCPT1
     C*** BEG ADD *** 12/19/01 ****************************************
     C*
     C* Use the cross referenced EOMB code if it is not blank
     C*
     C                   MOVEL     EEEOB         KKEOB
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EBXREF        IFNE      *BLANKS
     C                   MOVEL     EBXREF        EXCPT1
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       EXCPT1
     C                   ENDIF                                                  EBXREF NE BLANK
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*** END ADD *** 12/19/01 ****************************************
     C*
     C                   LEAVE
     C                   ENDIF
     C*
     C     EEKY          READE     PEE01                                  60
     C*
     C                   ENDDO
     C*** END ADD *** 04/05/01 ****************************************
     C*
     C* CLAIMANT SS#
     C                   MOVE      CMSS#         SSN
     C* INJURY DATE
     C*                    MOVE CMACDT    INJDT
     C* MEDICARE PROVIDER #
     C*** BEG DEL *** 04/05/01 ****************************************
     C**********
     C********** EUMCAR    IFNE *BLANKS
     C**********           MOVE EUMCAR    MEDPRO
     C**********
     C**********           ELSE
     C**********           MOVE P1MCR#    MEDPRO
     C**********
     C**********           ENDIF                           EUMCAR
     C*** END DEL *** 04/05/01 ****************************************
     C*** BEG ADD *** 04/05/01 ****************************************
     C*
     C     EUMCAR        IFNE      *BLANKS
     C                   MOVEL     EUMCAR        MEDPRO
     C*
     C                   ELSE
     C                   MOVEL     P1MCR#        MEDPRO
     C*
     C                   ENDIF                                                  EUMCAR
     C*** END ADD *** 04/05/01 ****************************************
     C* TYPE OF SERVICE FROM TYPE1 DATA STRUCTURE
     C                   MOVE      TYPE1         TYPSRV
     C*
     C* FILLER
     C                   MOVE      *BLANKS       FILL3             3
     C*
     C* Write the Header Record to the Work File
     C*
     C   90
     CAN 89              EXCEPT    HEAD1B
     C  N90
     CANN89              EXCEPT    HED1B
     C*
     C* Write the Header Record to the report
     C*
     C     *INOA         IFEQ      '1'
     C     XXFND         ORNE      HLDFND
     C                   EXCEPT    HEADER
     C                   Z-ADD     XXFND         HLDFND            3 0          TO DETERMINE LVL BRK
     C                   END
     C*
     C   90
     CAN 89              EXCEPT    HEADR1
     C  N90
     CANN89              EXCEPT    HEADR1
     C*** BEGIN ADD *** 02/18/97 *********************************************
     C*  COUNTER IS USED TO DETERMIN THE NUMBER OF HEADERS THAT WERE PROCESSED
     C*  IN THE TAPE SUBMISSION.
     C*
     C*** BEGIN DEL *** 07/07/00 *********************************************
     C******************** ADD  1         COUNT
     C*** END   DEL *** 07/07/00 *********************************************
     C*** END ADD ***** 02/18/00 *********************************************
     C*
     C********************  DETAIL SECTION  **********************************
     C* Get all Detail Charge Records that are associated with the
     C* EOB Master record
     C*
     C                   MOVE      '0'           *IN65
     C                   MOVEA     '0000000'     *IN(51)
     C                   MOVEA     '00000000'    *IN(91)
     C                   MOVE      '1'           *IN40
     C*** BEG DEL *** 09/10/99 ****************************************
     C******************** EXSR CLRX
     C*** END DEL *** 09/10/99 ****************************************
     C*
     C     DAKY          SETLL     PDA01
     C     DAKY1         READE     PDA01                                  64
     C*
     C*** BEG ADD *** 09/10/99 ****************************************
     C                   EXSR      CLRX
     C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 05/05/99 ****************************************
     C*
     C* If there are no detail amount records write an error message
     C     *IN64         IFEQ      *ON
     C                   EXCEPT    ERRMS3
     C                   ENDIF                                                  *in64=on
     C*** END ADD *** 05/05/99 ****************************************
     C*
     C     *IN64         DOWEQ     '0'
     C*
     C                   Z-ADD     DADSEQ        XXDSEQ
     C*
     C* Get the Detail Charge record
     C     CAKY          CHAIN     PCA01                              63
     C                   MOVE      'A'           XXESRC
     C                   MOVE      *BLANKS       XXEOB             1
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C* If there was a hit then move DEEOB to XXEOB
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DEEOB         XXEOB
     C                   ENDIF
     C*
     C* Do not include 'D' (duplicate) types in type 2 totals
     C     XXEOB         IFNE      'D'
     C*
     C* Get the PHYSICAIN BILL DETAIL RECORD
     C   89DHKY          CHAIN     PDH01                              63
     C*
     C* Get the HOSPITAL BILL DETAIL RECORD
     C  N89DUKY          CHAIN     PDU01                              63
     C*
     C     *IN63         IFEQ      *OFF
     C*
     C* Clear Detail record Fields
     C*
     C                   EXSR      CLRD
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     '2'           RECTYP
     C* SERVICE FROM DATE
     C     CAFDAT        IFEQ      0
     C                   MOVEL     '0000'        SRVDTF            6 0
     C                   MOVE      XXYY          SRVDTF
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   MOVEL     '0000'        SVDTF1
     C                   MOVE      XXYY          SVDTF1
     C*** END ADD *** 12/13/99 ****************************************
     C                   ELSE
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTF
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDTF1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C                   END
     C* SERVICE TO DATE
     C     CATDAT        IFEQ      0
     C                   MOVEL     '0000'        SRVDTT
     C                   MOVE      XXYY          SRVDTT
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   MOVEL     '0000'        SVDTT1
     C                   MOVE      XXYY          SVDTT1
     C*** END ADD *** 12/13/99 ****************************************
     C                   ELSE
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTT
     C*** BEG ADD *** 12/13/99 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SVDTT1            8 0
     C*** END ADD *** 12/13/99 ****************************************
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C                   END
     C* FEE GUIDELINE CODE
     C     RECORD        IFEQ      'P'
     C*
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL     DHBPRC        FEECDE
     C                   ELSE
     C                   MOVEL     DHPRCD        FEECDE
     C                   ENDIF                                                  DHBPRC NE BLANK
     C*
     C*** BEGIN CHG *** 08/27/01 **************************************
     C******************** MOVELDHPRCD    FEECDE
     C*** BEG DEL *** 07/11/02 ****************************************
     C******************** MOVELDHBPRC    FEECDE
     C*** END DEL *** 07/11/02 ****************************************
     C*** END   CHG *** 08/27/01 **************************************
     C* FIRST MODIFIER
     C     DHMOD1        IFNE      *BLANKS
     C                   MOVE      DHMOD1        MOD1
     C*
     C                   ELSE
     C*
     C     DHMOD2        IFNE      *BLANKS
     C                   MOVE      DHMOD2        MOD1
     C*
     C                   ELSE
     C*
     C     DHMOD3        IFNE      *BLANKS
     C                   MOVE      DHMOD3        MOD1
     C*
     C                   ELSE
     C*
     C     DHMOD4        IFNE      *BLANKS
     C                   MOVE      DHMOD4        MOD1
     C*
     C                   ENDIF                                                  DHMOD4 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD3 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD2 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD1 NE BLANK
     C*
     C*** BEG ADD *** 06/20/03 ****************************************
     C*
     C* SECOND MODIFIER
     C*
     C     DHMOD1        IFNE      *BLANKS
     C     DHMOD1        ANDNE     MOD1
     C                   MOVE      DHMOD1        MOD2
     C*
     C                   ELSE
     C*
     C     DHMOD2        IFNE      *BLANKS
     C     DHMOD2        ANDNE     MOD1
     C                   MOVE      DHMOD2        MOD2
     C*
     C                   ELSE
     C*
     C     DHMOD3        IFNE      *BLANKS
     C     DHMOD3        ANDNE     MOD1
     C                   MOVE      DHMOD3        MOD2
     C*
     C                   ELSE
     C*
     C     DHMOD4        IFNE      *BLANKS
     C     DHMOD4        ANDNE     MOD1
     C                   MOVE      DHMOD4        MOD2
     C*
     C                   ENDIF                                                  DHMOD4 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD3 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD2 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD1 NE BLANK
     C*
     C*** END ADD *** 06/20/03 ****************************************
     C*** BEG DEL *** 06/20/03 ****************************************
     C* SECOND MODIFIER (NOT USED AT THIS TIME)
     C*******************  MOVE *BLANKS   MOD2    2
     C*** END DEL *** 06/20/03 ****************************************
     C*
     C* SELECT ALL DIAGNOSIS CODE XREF NUMBERS
     C                   EXSR      DIAG
     C*
     C                   ENDIF                                                  RECORD=P
     C* UNITS
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   Z-ADD     DHUNTS        XXUNIT            3 0
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   Z-ADD     DUUNTS        XXUNIT
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C                   MOVE      XXUNIT        UNITS
     C* CHARGE AMOUNT
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** MOVE DACHRG    CHGAMT  92
     C*
     C* CALL SUBROUTINE TO ACCUMULATE CHARGES UP TO THE RECON LEVEL
     C* TWCC REJECTS WHEN PAID>CHARGED SO WE REPORT ACCUM CHARGES.
     C*
     C*** BEG CHG *** 003/21/02****************************************
     C******************** MOVE CACHRG    CHGAMT  92
     C                   EXSR      GETCHG
     C                   Z-ADD     XXCHRG        CHGAMT            9 2
     C*** END CHG *** 003/21/02****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C* PAYMENT AMOUNT
     C                   MOVE      DAPAID        PAYAMT            9 2
     C*** BEG ADD *** 01/06/99 ****************************************
     C*
     C* PAYMENT EXCEPTION CODE
     C* HARD CODE AN S=SUPPLEMENTAL PAYMENT IF IT'S A RECON AND > 0
     C     DARCN#        IFGT      *ZERO
     C     DAPAID        ANDGT     *ZERO
     C*
     C                   MOVE      'S'           XXPCOD
     C*
     C                   ELSE
     C*** BEG ADD *** 05/08/02 ****************************************
     C*
     C* TWCC REQUIRES P EOB CODE FOR NEGATIVE PAYMENTS
     C*
     C     DAPAID        IFLT      0                                            *IN60=OFF
     C                   MOVEL     'P'           XXPCOD
     C                   ELSE
     C*** END ADD *** 05/08/02 ****************************************
     C                   EXSR      EOMB
     C*
     C*** BEG ADD *** 05/08/02 ****************************************
     C                   ENDIF                                                  DAPAID LT 0
     C*** END ADD *** 05/08/02 ****************************************
     C*
     C                   ENDIF                                                  DARCN GT 0
     C*
     C                   MOVE      XXPCOD        PAYCDE
     C*** END ADD *** 01/06/99 ****************************************
     C*
     C* PLACE OF SERVICE
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   MOVE      DHPOS         PLACE
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   MOVE      DUPOS         PLACE
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C* FILLER
     C                   MOVE      *BLANKS       FILL13           13
     C*
     C* DIAGNOSIS ICD-9 CODES
     C* PRINCIPAL ICD-9
     C                   MOVE      'D'           XXLOST
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   MOVE      EHDIA1        XXDIA
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   MOVE      EUDIA1        XXDIA
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9              6
     C* OTHER DIAGNOSIS
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   MOVE      EHDIA2        XXDIA
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   MOVE      EUDIA2        XXDIA
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD91             6
     C* OTHER DIAGNOSIS
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   MOVE      EHDIA3        XXDIA
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   MOVE      EUDIA3        XXDIA
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD92             6
     C* OTHER DIAGNOSIS
     C*
     C     RECORD        IFEQ      'P'
     C*
     C                   MOVE      EHDIA4        XXDIA
     C*
     C                   ENDIF                                                  RECORD=P
     C*
     C     RECORD        IFEQ      'H'
     C*
     C                   MOVE      EUDIA4        XXDIA
     C*
     C                   ENDIF                                                  RECORD=H
     C*
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD93             6
     C*
     C     RECORD        IFEQ      'H'
     C*
     C* ICD-9-CM PROCEDURE CODES
     C* If Procedure Code Method = '9', then use 'O' type Diag Code
     C*** BEG CHG *** 04/05/01 ****************************************
     C**********           Z-ADD20        X
     C********** 1         SUBSTEUPCM:X   XXPCM   1    65
     C********** XXPCM     IFEQ '9'
     C     EUPCM         IFEQ      '9'
     C*** END CHG *** 04/05/01 ****************************************
     C*
     C                   MOVE      'O'           XXLOST
     C*
     C* PRIN ICD-9-CM PROC
     C                   MOVEL     EUPRC1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9CM
     C*
     C* OTHER ICD-9-CM PROC
     C                   MOVEL     EUPRC2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9C1
     C*
     C* OTHER ICD-9-CM PROC
     C                   MOVEL     EUPRC3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9C2
     C                   ELSE                                                   ELS EUPCM 9
     C*** BEG CHG *** 04/05/01 ****************************************
     C********** XXPCM     IFEQ '4'
     C     EUPCM         IFEQ      '4'
     C     EUPCM         OREQ      '5'
     C*** END CHG *** 04/05/01 ****************************************
     C                   MOVEL     EUPRC1        ICD9CM
     C                   MOVEL     EUPRC2        ICD9C1
     C                   MOVEL     EUPRC3        ICD9C2
     C                   END                                                    END EUPCM 4
     C                   END                                                    END EUPCM 9
     C                   ENDIF                                                  RECORD=H
     C*
     C* TAX ID #
     C                   MOVE      EOFEIN        TAXID#
     C* FILLER
     C                   MOVE      *BLANKS       FILL6             6
     C*
     C*
     C* Write DETAIL record to Work File
     C*
     C     RECORD        IFEQ      'P'
     C                   EXCEPT    DETAIL
     C                   ENDIF                                                  RECORD='P'
     C*
     C     RECORD        IFEQ      'H'
     C*
     C     DURATE        IFNE      'FSMRA'
     C*
     C   40              EXCEPT    DTL
     C   40              MOVE      '0'           *IN40
     C*
     C                   END
     C*
     C*
     C** IF HOSPITAL/OUT PATIENT THE SOME DETAIL SECTION CAN BE SKIPPED
     C     DURATE        IFEQ      'OUT  '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'OUT  '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C     DURATE        IFEQ      'MISC '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'MISC '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C     DURATE        IFEQ      'IR   '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'IR   '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C** IF HOSPITAL/FSMRA SKIP OUTPUT
     C     DURATE        IFEQ      'FSMRA'
     C                   GOTO      ENDMRA
     C                   END
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     'X1'          RECTP1            2
     C* FILLER
     C                   MOVE      *BLANKS       FILL2             2
     C* ITEMIZED CHARGE #1
     C     DURATE        IFEQ      'ROOMM'
     C     DURATE        OREQ      'ROOMS'
     C*** BEG ADD *** 09/06/02 ****************************************
     C*
     C* Add ROOM if this is an inpatient bill that was reconsidered
     C* because the recon line may not be the ROOM charges line
     C*
     C     DARCN#        ORGT      0
     C     EUICN         ANDEQ     'IS'
     C*
     C     DARCN#        ORGT      0
     C     EUICN         ANDEQ     'IM'
     C*** END ADD *** 09/06/02 ****************************************
     C*
     C                   MOVEL     'ROOM'        ITMIZ1            5
     C                   MOVE      '1'           *IN91
     C                   MOVE      '1'           *IN78
     C*
     C* UNITS
     C                   Z-ADD     DUUNTS        XXUNIT
     C*
     C                   MOVE      XXUNIT        UNITS1            3
     C* CHARGES
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** MOVE DACHRG    CHARG1  92
     C*** BEG CHG *** 04/05/01 ****************************************
     C******************** MOVE CACHRG    CHARG1  92
     C                   MOVE      EUROOM        CHARG1            9 2
     C*** END CHG *** 04/05/01 ****************************************
     C*** END CHG *** 01/06/99 ****************************************
     C   77              ADD       XXMRAC        CHARG1
     C   76              Z-ADD     XXMRAC        CHARG1
     C* AMOUNT PAID
     C*** BEG DEL *** 04/05/01 ****************************************
     C******************** MOVE DAPAID    AMTPD1  92
     C*** END DEL *** 04/05/01 ****************************************
     C*** BEG ADD *** 04/05/01 ****************************************
     C*
     C     DAPAID        IFLE      EUROOM
     C                   MOVE      DAPAID        AMTPD1            9 2
     C*
     C                   ELSE
     C                   MOVE      EUROOM        AMTPD1
     C*
     C                   ENDIF                                                  DAPAID < EUROOM
     C*
     C*** END DEL *** 04/05/01 ****************************************
     C   77              ADD       XXMRAP        AMTPD1
     C   76              Z-ADD     XXMRAP        AMTPD1
     C   77              MOVE      '0'           *IN77
     C* PAYMENT EXCEPTION CODE
     C*
     C*** BEG ADD *** 04/05/01 ****************************************
     C     AMTPD1        IFLT      CHARG1
     C*** END ADD *** 04/05/01 ****************************************
     C                   MOVE      XXPCOD        EXC1              1
     C*** BEG ADD *** 04/05/01 ****************************************
     C                   ENDIF                                                  AMTPD1
     C*** END ADD *** 04/05/01 ****************************************
     C*
     C                   END                                                    ROOMM OR ROOMS
     C* FILLER
     C**********           MOVE *BLANKS   FILL24 24        DEL 10/06/97
     C*
     C* ITEMIZED CHARGE #2
     C*
     C     DURATE        IFEQ      '00001'
     C                   MOVE      '00001'       ITMIZ2            5
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN78
     C* UNITS
     C*
     C                   Z-ADD     DUUNTS        XXUNIT
     C*
     C                   MOVE      XXUNIT        UNITS2            3
     C* CHARGES
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** MOVE DACHRG    CHARG2  92
     C                   MOVE      CACHRG        CHARG2            9 2
     C*** END CHG *** 01/06/99 ****************************************
     C   77              ADD       XXMRAC        CHARG2
     C   76              Z-ADD     XXMRAC        CHARG2
     C* AMOUNT PAID
     C                   MOVE      DAPAID        AMTPD2            9 2
     C   77              ADD       XXMRAP        AMTPD2
     C   76              Z-ADD     XXMRAP        AMTPD2
     C   77              MOVE      '0'           *IN77
     C* PAYMENT EXCEPTION CODE
     C*
     C                   MOVE      XXPCOD        EXC2              1
     C*
     C                   END                                                    00001
     C* FILLER
     C                   MOVE      *BLANKS       FILL4             4
     C* FILLER
     C*** BEG DEL *** 09/10/99 ****************************************
     C******************** MOVE *BLANKS   FILL28 28
     C*** END DEL *** 09/10/99 ****************************************
     C*
     C* Write Detail record to REPORT
     C*
     C  N90
     CANN89
     CAN 78              EXCEPT    DETL
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     'X2'          RECTP2            2
     C                   MOVEL     'X3'          RECTP3            2
     C                   MOVEL     'X4'          RECTP4            2
     C* ITEMIZED CHARGE #3
     C     DURATE        IFEQ      '00002'
     C                   MOVEL     '00002'       ITMIZ3            5
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN78
     C*
     C* UNITS
     C                   Z-ADD     DUUNTS        XXUNIT
     C*
     C                   MOVE      XXUNIT        UNITS3            3
     C* CHARGES
     C*** BEG CHG *** 01/06/99 ****************************************
     C******************** MOVE DACHRG    CHARG3  92
     C                   MOVE      CACHRG        CHARG3            9 2
     C*** END CHG *** 01/06/99 ****************************************
     C* AMOUNT PAID
     C                   MOVE      DAPAID        AMTPD3            9 2
     C* PAYMENT EXCEPTION CODE
     C*
     C                   MOVE      XXPCOD        EXC3              1
     C*
     C                   END                                                    00002
     C*
     C                   ENDIF                                                  RECORD = 'H'
     C* FILLER
     C**********           MOVE *BLANKS   FILL24 24        DEL 10/06/97
     C*
     C* CAT Scan and MRI's are entered on UB92 for acute inpatient
     C* ITEMIZED CHARGE #4, #5, #6 & #7
     C*** BEG ADD *** 04/26/01 ****************************************
     C     EUICN         IFEQ      'IS'
     C     EUICN         OREQ      'IM'
     C*** END ADD *** 04/26/01 ****************************************
     C*
     C     DURATE        IFGE      '70450'
     C     DURATE        ANDLE     '70492'
     C     DURATE        ORGE      '71250'
     C     DURATE        ANDLE     '71270'
     C     DURATE        ORGE      '72125'
     C     DURATE        ANDLE     '72133'
     C     DURATE        ORGE      '72192'
     C     DURATE        ANDLE     '72194'
     C     DURATE        ORGE      '73200'
     C     DURATE        ANDLE     '73202'
     C     DURATE        ORGE      '73700'
     C     DURATE        ANDLE     '73701'
     C     DURATE        ORGE      '74150'
     C     DURATE        ANDLE     '74170'
     C     DURATE        OREQ      '70336'
     C     DURATE        ORGE      '70540'
     C     DURATE        ANDLE     '70552'
     C     DURATE        OREQ      '71550'
     C     DURATE        ORGE      '72141'
     C     DURATE        ANDLE     '72149'
     C     DURATE        OREQ      '72196'
     C     DURATE        ORGE      '73220'
     C     DURATE        ANDLE     '73221'
     C     DURATE        ORGE      '73720'
     C     DURATE        ANDLE     '73721'
     C     DURATE        OREQ      '74181'
     C     DURATE        OREQ      '75552'
     C     DURATE        OREQ      '76400'
     C  N94              MOVE      DURATE        ITMIZ4            5
     C   94
     CANN95              MOVE      DURATE        ITMIZ5            5
     C   94
     CAN 95
     CANN96              MOVE      DURATE        ITMIZ6            5
     C   94
     CAN 95
     CAN 96              MOVE      DURATE        ITMIZ7            5
     C* CPT-4 MODIFIER
     C     DHMOD1        IFNE      *BLANKS
     C                   MOVE      DHMOD1        XXPMOD
     C*
     C                   ELSE
     C*
     C     DHMOD2        IFNE      *BLANKS
     C                   MOVE      DHMOD2        XXPMOD
     C*
     C                   ELSE
     C*
     C     DHMOD3        IFNE      *BLANKS
     C                   MOVE      DHMOD3        XXPMOD
     C*
     C                   ELSE
     C*
     C     DHMOD4        IFNE      *BLANKS
     C                   MOVE      DHMOD4        XXPMOD
     C*
     C                   ENDIF                                                  DHMOD4 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD3 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD2 NE BLANK
     C*
     C                   ENDIF                                                  DHMOD1 NE BLANK
     C*
     C  N94              MOVE      XXPMOD        CPT44             2
     C   94
     CANN95              MOVE      XXPMOD        CPT45             2
     C   94
     CAN 95
     CANN96              MOVE      XXPMOD        CPT46             2
     C   94
     CAN 95
     CAN 96              MOVE      XXPMOD        CPT47             2
     C* UNITS
     C*
     C                   Z-ADD     DUUNTS        XXUNIT
     C*
     C  N94              MOVE      XXUNIT        UNITS4            3
     C   94
     CANN95              MOVE      XXUNIT        UNITS5            3
     C   94
     CAN 95
     CANN96              MOVE      XXUNIT        UNITS6            3
     C   94
     CAN 95
     CAN 96              MOVE      XXUNIT        UNITS7            3
     C* CHARGES
     C*** BEG CHG *** 01/06/99 ****************************************
     C**N94*************** MOVE DACHRG    CHARG4  92
     C***94N95************ MOVE DACHRG    CHARG5  92
     C***94*95N96********* MOVE DACHRG    CHARG6  92
     C***94*95*96********* MOVE DACHRG    CHARG7  92
     C  N94              MOVE      CACHRG        CHARG4            9 2
     C   94
     CANN95              MOVE      CACHRG        CHARG5            9 2
     C   94
     CAN 95
     CANN96              MOVE      CACHRG        CHARG6            9 2
     C   94
     CAN 95
     CAN 96              MOVE      CACHRG        CHARG7            9 2
     C*** END CHG *** 01/06/99 ****************************************
     C* AMOUNT PAID
     C  N94              MOVE      DAPAID        AMTPD4            9 2
     C   94
     CANN95              MOVE      DAPAID        AMTPD5            9 2
     C   94
     CAN 95
     CANN96              MOVE      DAPAID        AMTPD6            9 2
     C   94
     CAN 95
     CAN 96              MOVE      DAPAID        AMTPD7            9 2
     C* PAYMENT EXCEPTION CODE
     C  N94              MOVE      XXPCOD        EXC4              1
     C   94
     CANN95              MOVE      XXPCOD        EXC5              1
     C   94
     CAN 95
     CANN96              MOVE      XXPCOD        EXC6              1
     C   94
     CAN 95
     CAN 96              MOVE      XXPCOD        EXC7              1
     C*
     C   94
     CAN 95
     CAN 96              MOVE      '1'           *IN97
     C   94
     CAN 95
     CANN96              MOVE      '1'           *IN96
     C   94
     CANN95              MOVE      '1'           *IN95
     C  N94              MOVE      '1'           *IN94
     C*
     C                   MOVE      '1'           *IN78
     C*
     C                   ENDIF                                                  DHPRCD GE 70450
     C*
     C*** BEG ADD *** 04/26/01 ****************************************
     C                   ENDIF                                                  EUICN =
     C*** END ADD *** 04/26/01 ****************************************
     C*
     C* FILLER
     C                   MOVE      *BLANKS       FILL4             4
     C* FILLER
     C*** BEG DEL *** 09/10/99 ****************************************
     C******************** MOVE *BLANKS   FILL28 28
     C*** END DEL *** 09/10/99 ****************************************
     C* FILLER
     C                   MOVE      *BLANKS       FILL33           33
     C*
     C* Write Detail record to REPORT
     C*
     C     *INOA         IFEQ      '1'
     C   90
     CAN 89              EXCEPT    HEADER
     C  N90
     CANN89              EXCEPT    HEADR1
     C                   MOVE      '0'           *INOA
     C                   END
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89
     CAN 78              EXCEPT    DETL
     C*
     C* Continue to read  detail charge file
     C*** BEG ADD *** 05/05/99 ****************************************
     C* If there are no auxiliary records write an error message
     C                   ELSE
     C                   EXCEPT    ERRMS3
     C*
     C*** END ADD *** 05/05/99 ****************************************
     C                   END                                                    *IN63=OFF
     C*
     C     ENDMRA        TAG
     C*
     C                   MOVE      '0'           *IN78
     C*
     C                   ENDIF                                                  XXEOB NE D
     C*
     C     DAKY          READE     PDA01                                  64
     C                   END                                                    *in64 end
     C*
     C* Write Detail record to WORK FILE
     C*
     C  N89              EXCEPT    DTLX1
     C  N89              EXCEPT    DTLX2
     C  N89              EXCEPT    DTLX3
     C  N89              EXCEPT    DTLX4
     C*
     C* Populate the data submission date EAESDT
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     QQRSUB        IFNE      'Y'
     C     EAESDT        ANDEQ     *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAESDT
     C                   ENDIF
     C*
     C                   MOVEL(P)  EAEOB#        EACNTL
     C*
     C*** BEG CHG *** 09/18/03 ****************************************
     C********** EAEDI     IFEQ 'R'
     C     QQPROC        IFEQ      EOSYS
     C     EAEDI         ANDEQ     'R'
     C*** END CHG *** 09/18/03 ****************************************
     C                   MOVEL     'Y'           EAEDI
     C                   ENDIF
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C*** BEGIN DEL *** 02/08/02 **************************************
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EAESDT
     C*** END   DEL *** 02/08/02 **************************************
     C*
     C                   Z-ADD     EAESDT        EADT
     C                   Z-ADD     DSTIME        EATIME
     C*
     C*** BEG DEL *** 06/12/01 ****************************************
     C******************** MOVE Q1USER    EAUSER
     C*** END DEL *** 06/12/01 ****************************************
     C                   MOVE      Q1PGM         EAPGM
     C*
     C                   UPDATE    PEA01
     C*
     C*** BEG ADD *** 12/11/00 ****************************************
     C* Populate EOCNTL and update maintenence fields
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     EOEOB#        EOCNTL
     C*** BEGIN ADD *** 02/08/02 **************************************
     C*
     C*** BEG CHG *** 09/18/03 ****************************************
     C********** EOTAPE    IFEQ 'R'
     C     QQPROC        IFEQ      EOSYS
     C     EOTAPE        ANDEQ     'R'
     C*** END CHG *** 09/18/03 ****************************************
     C                   MOVEL     'Y'           EOTAPE
     C                   ENDIF
     C*
     C                   MOVE      DSDATE        EOSBDT
     C*** END   ADD *** 02/08/02 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C*** BEG DEL *** 06/12/01 ****************************************
     C******************** MOVE Q1USER    EOUSER
     C*** END DEL *** 06/12/01 ****************************************
     C                   MOVE      Q1PGM         EOPGM
     C*
     C                   UPDATE    PEO01
     C*** END ADD *** 12/11/00 ****************************************
     C*
     C                   ELSE
     C*
     C                   EXCEPT    REA01
     C*** BEG ADD *** 12/11/00 ****************************************
     C                   EXCEPT    REO01
     C*** END ADD *** 12/11/00 ****************************************
     C*
     C                   ENDIF                                                  EAEDI = Y
     C*
     C*** BEG ADD *** 03/10/00 ****************************************
     C                   ELSE
     C*
     C                   EXCEPT    REA01
     C*** BEG ADD *** 12/11/00 ****************************************
     C                   EXCEPT    REO01
     C*** END ADD *** 12/11/00 ****************************************
     C*
     C                   ENDIF                                                  *IN59=OFF
     C*** END ADD *** 03/10/00 ****************************************
     C*
     C     NXTRCD        TAG
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C     EAEOB#        IFNE      ZZEOB#
     C                   Z-ADD     EAEOB#        ZZEOB#
     C                   ENDIF
     C*** END ADD *** 03/21/02 ****************************************
     C*
     C                   READ      PEA01                                  62
     C*
     C                   END                                                    *IN62 end
     C*
     C     *IN99         IFNE      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END
     C*
     C* UPDATE TAPE SUBMISSION SEQ# FILE WITH LAST SEQ# USED BY CO/SUB
     C                   MOVE      XXCO#         TSCO#
     C                   MOVE      CNTR          TSSEQ#                         LAST SEQ# USED
     C     *IN80         IFEQ      '1'
     C                   WRITE     PTS01
     C                   ELSE                                                   ELSE 1 *IN80
     C                   UPDATE    PTS01
     C                   END                                                    END 1 *IN80
     C*
     C*** BEGIN ADD *** 02/18/97 ****************************************
     C*  WRITES OUT TOTAL NUMBER OF EOMBS PER SUBSCRIBER
     C                   EXCEPT    CNT
     C*** END ADD ***** 02/18/97 ****************************************
     C* END OF PROGRAM
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************************
     C* CLRH - Subroutine to Clear Batch Control & Header Record Fields
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* BATCH CONTROL RECORD:                                         *
     C* RECTYP   RECORD TYPE = 0                                      *
     C* CARCDE   CARRIER CODE                                         *
     C* LOCCDE   LOCATION CODE                                        *
     C* YRSUB    YEAR OF SUBMISSION (W/CENTURY)                       *
     C* JULDT    JULIAN DATE (DATE TAPE WAS CREATED)                  *
     C* TYPE1A   ACCUMULATION OF TYPE 1A RECORDS                      *
     C* --PHYSICIAN--                                                 *
     C* FORM#    FORM NUMBER = 01                                     *
     C* --HOSPITAL--                                                  *
     C* FORM#    FORM NUMBER = 02                                     *
     C*                                                               *
     C* TYPE 1A - DOCUMENT HEADER RECORD:                             *
     C* RECTYP   RECORD TYPE = 1A                                     *
     C* LSNAME   LAST NAME                                            *
     C* FSNAME   FIRST NAME                                           *
     C* MID      MIDDLE INITIAL                                       *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* SSN      SOCIAL SECURITY NUMBER                               *
     C* INJDT    DATE OF INJURY                                       *
     C* CONDT    DATE FIRST CONSULTED                                 *
     C* CCIDH    CLAIM CARRIER ID                                     *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* ADMDT    ADMISSION DATE                                       *
     C* SUBDT    SUBMISSION DATE                                      *
     C* RECDT    RECIEVED DATE                                        *
     C* CCIDU    CLAIM CARRIER ID                                     *
     C*                                                               *
     C* TYPE 1B - DOCUMENT HEADER RECORD:                             *
     C* RECTYP   RECORD TYPE = 1B                                     *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* RECDT    DATE BILL RECIEVED                                   *
     C* CARPMT   DATE OF CARRIER PAYMENT                              *
     C* ICD9     PRIMARY ICD-9                                        *
     C* ICD91    SECONDARY ICD-9 1                                    *
     C* ICD92    SECONDARY ICD-9 2                                    *
     C* ICD93    SECONDARY ICD-9 3                                    *
     C* PRLIC#   PROVIDER LISCENCE NUMBER                             *
     C* TYPE2    ACCUMULATION OF TYPE 2 RECORDS                       *
     C* TAXID#   TAX ID NUMBER                                        *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* SRVDTF   SERVICE FROM DATE                                    *
     C* SRVDTT   SERVICE TO DATE                                      *
     C* TTLCHR   TOTAL CHARGES (LESS LINES W/DEEOB='D')               *
     C* ESTDUE   ESTIMATED AMOUNT DUE (LESS LINES W/DEEOB='D')        *
     C* EXCPT1   PAYMENT EXCEPTION CODE                               *
     C* SSN      CLAIMANT SS#                                         *
     C* INJDT    INJURY DATE                                          *
     C* MEDPRO   MEDICARE PROVIDER #                                  *
     C* TYPSER   TYPE OF SERVICE                                      *
     C****************************************************************
     C     CLRH          BEGSR
     C*
     C                   MOVE      '0'           RECTYP            2
     C   90              MOVE      '01'          FORM#             2
     C  N90              MOVE      '02'          FORM#             2
     C                   MOVEL     '00000'       CARCDE            9
     C                   MOVE      '0000'        CARCDE
     C                   MOVE      '00'          LOCCDE            2
     C                   MOVEL     '0000'        YRSUB             4 0
     C                   MOVE      '000'         JULDT             3 0
     C                   MOVE      *ZEROS        TYPE1A            5 0
     C                   MOVE      *BLANKS       LSNAME           20
     C                   MOVE      *BLANKS       FSNAME           12
     C                   MOVE      *BLANKS       MID               9
     C                   MOVE      *BLANKS       SSN
     C                   MOVE      *ZEROS        INJDT
     C*** BEG DEL *** 06/29/00 ****************************************
     C***********          MOVE *ZEROS    CONDT
     C*** END DEL *** 06/29/00 ****************************************
     C                   MOVE      *ZEROS        RECDT
     C*** BEG ADD *** 04/05/01 ****************************************
     C                   MOVE      *ZEROS        CCIDH
     C                   MOVE      *ZEROS        CCIDU
     C*** END ADD *** 04/05/01 ****************************************
     C*************        MOVE *ZEROS    CARPAY
     C                   MOVE      '000000'      ICD9
     C                   MOVE      *BLANKS       ICD91
     C                   MOVE      *BLANKS       ICD92
     C                   MOVE      *BLANKS       ICD93
     C                   MOVE      *BLANKS       PRLIC#
     C                   MOVE      *ZEROS        TYPE2             3 0
     C                   MOVE      *BLANKS       TAXID#
     C                   MOVE      *ZEROS        ADMDT
     C                   MOVE      *ZEROS        SUBDT
     C                   MOVE      *ZEROS        SRVDTF
     C                   MOVE      *ZEROS        SRVDTT
     C                   MOVE      *ZEROS        TTLCHR            9 2
     C                   MOVE      *ZEROS        ESTDUE            9 2
     C                   MOVE      *BLANKS       EXCPT1            1
     C                   MOVE      *BLANKS       MEDPRO           10
     C                   MOVE      *BLANKS       TYPSRV            3
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRD - Subroutine to Clear Detail Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* DETAIL RECORD:                                                *
     C* RECTYP   RECORD TYPE = 2                                      *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* SRVDTF   SERVICE FROM DATE                                    *
     C* SRVDTT   SERVICE TO DATE                                      *
     C* FEECDE   FEE GUIDELINE CODE                                   *
     C* MOD1     FIRST MODIFIER                                       *
     C* MOD2     SECOND MODIFIER                                      *
     C* DIAG1    FIRST DIAGNOSTIC CODE XREF#                          *
     C* DIAG2    SECOND DIAGNOSTIC CODE (NOT USED AT THIS TIME)       *
     C* DIAG3    THIRD DIAGNOSTIC CODE (NOT USED AT THIS TIME)        *
     C* DIAG4    FOURTH DIAGNOSTIC CODE (NOT USED AT THIS TIME)       *
     C* UNITS    UNITS                                                *
     C* CHGAMT   CHARGE AMOUNT                                        *
     C* PAYAMT   PAYMENT AMOUNT                                       *
     C* PAYCDE   PAYMENT EXCEPTION CODE                               *
     C* POS      PLACE OF SERVICE                                     *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* ICD9     PRINCIPAL ICD-9                                      *
     C* ICD91    OTHER DIAGNOSIS                                      *
     C* ICD92    OTHER DIAGNOSIS                                      *
     C* ICD93    OTHER DIAGNOSIS                                      *
     C* OTHDIA   OTHER DIAGNOSIS (NOT CAPTURING THIS DATA)            *
     C* ICD9CM   PRIN ICD-9-CM PROC                                   *
     C* ICD9C1   OTHER ICD-9-CM PROC                                  *
     C* ICD9C2   OTHER ICD-9-CM PROC                                  *
     C* TAXID#   TAX ID #                                             *
     C****************************************************************
     C     CLRD          BEGSR
     C*
     C                   MOVE      '2'           RCFLG2            1
     C                   MOVE      '09'          FORMID            2
     C                   MOVE      *ZEROS        SRVDTF
     C                   MOVE      *ZEROS        SRVDTT
     C                   MOVE      '00000'       FEECDE            5
     C                   MOVE      *BLANKS       MOD1              2
     C*** BEG ADD *** 06/20/03 ****************************************
     C                   MOVE      *BLANKS       MOD2              2
     C*** END ADD *** 06/20/03 ****************************************
     C                   MOVE      '0'           DIAG1             1
     C                   MOVE      '0'           DIAG2             1
     C                   MOVE      '0'           DIAG3             1
     C                   MOVE      '0'           DIAG4             1
     C                   MOVE      '000'         UNITS             3
     C                   MOVEL     '0000000'     CRGAMT            9
     C                   MOVEL     '00'          CRGAMT
     C                   MOVEL     '000000'      PAYAMT
     C                   MOVE      '000'         PAYAMT
     C                   MOVE      '0'           PAYCDE            1
     C                   MOVEL     '00'          PLACE             2
     C                   MOVE      *BLANKS       ICD9              6
     C                   MOVE      *BLANKS       ICD91             6
     C                   MOVE      *BLANKS       ICD92             6
     C                   MOVE      *BLANKS       ICD93             6
     C                   MOVE      *BLANKS       OTHDIA            6
     C                   MOVE      *BLANKS       ICD9CM            6
     C                   MOVE      *BLANKS       ICD9CM            6
     C                   MOVE      *BLANKS       ICD9C1            6
     C                   MOVE      *BLANKS       ICD9C2            6
     C                   MOVE      *ZEROS        TAXID#
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRX - Subroutine to Clear X(1,2,3,4) Fields                 *
     C****************************************************************
     C*
     C     CLRX          BEGSR
     C*
     C*** BEG DEL *** 04/05/01 ****************************************
     C******************** MOVE *BLANKS   ITMIZ1
     C******************** MOVE '00000'   ITMIZ2
     C******************** MOVE *BLANKS   ITMIZ3
     C******************** MOVE '00000'   ITMIZ4
     C******************** MOVE *BLANKS   ITMIZ5
     C******************** MOVE '00000'   ITMIZ6
     C******************** MOVE *BLANKS   ITMIZ7
     C*** END DEL *** 04/05/01 ****************************************
     C*** BEG ADD *** 04/05/01 ****************************************
     C                   MOVE      *BLANKS       ITMIZ1
     C                   MOVE      *BLANKS       ITMIZ2
     C                   MOVE      *BLANKS       ITMIZ3
     C                   MOVE      *BLANKS       ITMIZ4
     C                   MOVE      *BLANKS       ITMIZ5
     C                   MOVE      *BLANKS       ITMIZ6
     C                   MOVE      *BLANKS       ITMIZ7
     C*** END ADD *** 04/05/01 ****************************************
     C                   MOVE      '000'         UNITS1
     C                   MOVE      '000'         UNITS2
     C                   MOVE      '000'         UNITS3
     C                   MOVE      '000'         UNITS4
     C                   MOVE      '000'         UNITS5
     C                   MOVE      '000'         UNITS6
     C                   MOVE      '000'         UNITS7
     C                   MOVE      *ZEROS        CHARG1
     C                   MOVE      *ZEROS        CHARG2
     C                   MOVE      *ZEROS        CHARG3
     C                   MOVE      *ZEROS        CHARG4
     C                   MOVE      *ZEROS        CHARG5
     C                   MOVE      *ZEROS        CHARG6
     C                   MOVE      *ZEROS        CHARG7
     C                   MOVE      *ZEROS        AMTPD1
     C                   MOVE      *ZEROS        AMTPD2
     C                   MOVE      *ZEROS        AMTPD3
     C                   MOVE      *ZEROS        AMTPD4
     C                   MOVE      *ZEROS        AMTPD5
     C                   MOVE      *ZEROS        AMTPD6
     C                   MOVE      *ZEROS        AMTPD7
     C                   MOVE      *BLANKS       EXC1
     C                   MOVE      *BLANKS       EXC2
     C                   MOVE      *BLANKS       EXC3
     C                   MOVE      *BLANKS       EXC4
     C                   MOVE      *BLANKS       EXC5
     C                   MOVE      *BLANKS       EXC6
     C                   MOVE      *BLANKS       EXC7
     C                   MOVE      *BLANKS       CPT44
     C                   MOVE      *BLANKS       CPT45
     C                   MOVE      *BLANKS       CPT46
     C                   MOVE      *BLANKS       CPT47
     C*
     C                   ENDSR
     C****************************************************************
     C* EOMB - Subroutine to Load the EOMB Code from FDDEP           *
     C****************************************************************
     C*
     C     EOMB          BEGSR
     C*
     C                   MOVE      *BLANK        XXPCOD            1
     C*** BEG ADD *** 07/09/99 ****************************************
     C                   MOVE      *BLANK        KKEOB
     C*** END ADD *** 07/09/99 ****************************************
     C*
     C*   Adjudication Reduction DEESRC='A'
     C                   MOVE      'A'           XXESRC
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 07/09/99 ****************************************
     C******************** MOVELDEEOB     XXPCOD
     C     DEEOB         ANDNE     *BLANKS
     C*
     C                   MOVEL     DEEOB         KKEOB
     C*** END CHG *** 07/09/99 ****************************************
     C*
     C                   ELSE
     C*** BEGIN ADD *** 04/15/04 **************************************
     C*
     C*   Correct Coding DEESRC='C'
     C                   MOVE      'C'           XXESRC
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     DEEOB         ANDNE     *BLANKS
     C*
     C                   MOVEL     DEEOB         KKEOB
     C*
     C                   ELSE
     C*** END   ADD *** 04/15/04 **************************************
     C*
     C*   Network Reduction DEESRC='N'
     C                   MOVE      'N'           XXESRC
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 07/09/99 ****************************************
     C******************** MOVELDEEOB     XXPCOD
     C     DEEOB         ANDNE     *BLANKS
     C*
     C                   MOVEL     DEEOB         KKEOB
     C*** END CHG *** 07/09/99 ****************************************
     C*
     C                   ELSE
     C*
     C*   User Selected DEESRC='U'
     C                   MOVE      'U'           XXESRC
     C*
     C*** BEG ADD *** 03/21/02 ****************************************
     C*
     C     DEKY          CHAIN     PDE01                              60
     C*
     C* Cycle through so you don't pick up just a note.
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     DEEOB         IFNE      *BLANKS
     C*** BEGIN ADD *** 05/13/04 **************************************
     C     DESTAT        ANDNE     'I'
     C     DEPRNT        ANDNE     'N'
     C*** END   ADD *** 05/13/04 **************************************
     C*
     C                   MOVEL     DEEOB         KKEOB
     C                   LEAVE
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     DEKY          READE     PDE01                                  60
     C*
     C                   ENDDO                                                  *IN60=OFF
     C*** END ADD *** 03/21/02 ****************************************
     C*** BEG DEL *** 03/21/02 ****************************************
     C**********
     C********** DEKY      CHAINPDE01                60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C****BEG*CHG *** 07/09/99 ****************************************
     C******************** MOVELDEEOB     XXPCOD
     C********** DEEOB     ANDNE*BLANKS
     C**********
     C**********           MOVELDEEOB     KKEOB
     C****END*CHG *** 07/09/99 ****************************************
     C**********
     C**********           ENDIF                           *IN60=OFF
     C*** END DEL *** 03/21/02 ****************************************
     C*
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*** BEGIN ADD *** 04/15/04 **************************************
     C                   ENDIF                                                  *IN60=OFF
     C*** END   ADD *** 04/15/04 **************************************
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C* Use the cross referenced EOMB code if it is not blank
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EBXREF        IFNE      *BLANKS
     C                   MOVEL     EBXREF        XXPCOD
     C*** BEG ADD 01/06/99 ********************************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXPCOD
     C*** END ADD 01/06/99 ********************************************
     C                   ENDIF                                                  EBXREF NE BLANK
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDSR
     C****************************************************************
     C* DIAG - Subroutine to Load the Diagnosis codes                *
     C****************************************************************
     C*
     C     DIAG          BEGSR
     C*
     C* Select Diagnosis Cross Reference #1
     C                   SELECT
     C*
     C     DHDXR1        WHENEQ    'X'
     C*
     C                   MOVE      '1'           DIAG1             1
     C*
     C     DHDXR2        WHENEQ    'X'
     C*
     C                   MOVE      '2'           DIAG1
     C*
     C     DHDXR3        WHENEQ    'X'
     C*
     C                   MOVE      '3'           DIAG1
     C*
     C     DHDXR4        WHENEQ    'X'
     C*
     C                   MOVE      '4'           DIAG1
     C*
     C                   ENDSL
     C*
     C* Select Diagnosis Cross Reference #2
     C                   SELECT
     C*
     C     DHDXR1        WHENEQ    'X'
     C     DHDXR2        ANDEQ     'X'
     C*
     C                   MOVE      '2'           DIAG2             1
     C*
     C     DHDXR1        WHENEQ    'X'
     C     DHDXR3        ANDEQ     'X'
     C*
     C                   MOVE      '3'           DIAG2
     C*
     C     DHDXR1        WHENEQ    'X'
     C     DHDXR4        ANDEQ     'X'
     C*
     C                   MOVE      '4'           DIAG2
     C*
     C                   ENDSL                                                  DHDXR4=X
     C*
     C* Select Diagnosis Cross Reference #3
     C                   SELECT
     C*
     C     DHDXR1        WHENEQ    'X'
     C     DHDXR2        ANDEQ     'X'
     C     DHDXR3        ANDEQ     'X'
     C*
     C                   MOVE      '3'           DIAG3             1
     C*
     C     DHDXR1        WHENEQ    'X'
     C     DHDXR2        ANDEQ     'X'
     C     DHDXR4        ANDEQ     'X'
     C*
     C                   MOVE      '4'           DIAG3
     C*
     C                   ENDSL                                                  DHDXR4=X
     C*
     C* Select Diagnosis Cross Reference #4
     C     DHDXR1        IFEQ      'X'
     C     DHDXR2        ANDEQ     'X'
     C     DHDXR3        ANDEQ     'X'
     C     DHDXR4        ANDEQ     'X'
     C*
     C                   MOVE      '4'           DIAG4             1
     C*
     C                   ENDIF                                                  DHDXR1=X
     C*
     C                   ENDSR
     C****************************************************************
     C* GETCHG - Subroutine to accumulate charges for the line item  *
     C****************************************************************
     C*
     C     GETCHG        BEGSR
     C*
     C                   Z-ADD     DADSEQ        X2DSEQ
     C                   Z-ADD     0             XXCHRG
     C*
     C     DAKY2         CHAIN     PDA02                              62
     C*
     C     *IN62         DOWEQ     *OFF
     C     D2RCN#        ANDLE     EARCN
     C*
     C                   ADD       D2CHRG        XXCHRG
     C*
     C     DAKY2         READE     PDA02                                  62
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*** BEG ADD *** 09/17/03 ****************************************
     C****************************************************************
     C* GETMDT - Subroutine to retrieve the check mailing date       *
     C****************************************************************
     C*
     C     GETMDT        BEGSR
     C*
     C* USE THE CHECK MAILING DATE IF NOT BLANK
     C*
     C     @@WCIF        IFEQ      'Y'
     C*
     C                   MOVEL     EASUB#        ##SUB#            5
     C                   MOVEL     EAFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      EACO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C*
     C                   MOVE      PAYDT1        #1TRDT
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM                    #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
     C*
     C     #1CMDT        IFNE      *BLANKS
     C                   MOVE(P)   #1CMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DT3MDY
     C                   MOVEL     SRDTO         PAYDT             6 0
     C                   MOVEL     #1CMDT        PAYDT1
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##OUTS=N
     C*
     C                   ENDIF                                                  @@WCIF
     C*
     C                   ENDSR
     C****************************************************************
     C* PGMSTR - Retrieve install flags, etc.
     C****************************************************************
     C*
     C     PGMSTR        BEGSR
     C*
     C*  Check to see if WCMS is installed
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C*
     C                   ENDSR
     C*** END ADD *** 09/17/03 ****************************************
     C****************************************************************
     OFATPPTX   EADD         HEAD0
     O                       RECTYP               2
     O                       FORM#                3
     O                       CARCDE              12
     O                       LOCCDE              14
     O                       YRSUB               18
     O                       JULDT               21
     O                       TYPE1A              26
     O                       FILL38              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         HEAD1A
     O                       RECTYP               2
     O                       LSNAME              22
     O                       FSNAME              34
     O                       MID                 43
     O                       SSN                 52
     O                       INJDT               58
     O*** BEG CHG *** 06/29/00 ****************************************
     O***********              CONDT     64
     O*** BEG CHG *** 04/05/01 ****************************************
     O************************ CCID      80
     O                       CCIDH               80
     O*** END CHG *** 04/05/01 ****************************************
     O*** END CHG *** 06/29/00 ****************************************
     O          EADD         HEAD1B
     O                       RECTYP               2
     O                       RECDT                8
     O                       PAYDT               14
     O                       FILL                15
     O                       ICD9                21
     O                       ICD91               27
     O                       ICD92               33
     O                       ICD93               39
     O*** BEG CHG *** 06/29/00 ****************************************
     O***********              PRLIC#    47
     O***********              TYPE2     50
     O***********              TAXID#    59
     O                       PREFIX              41
     O*** BEG CHG *** 09/29/04 ****************************************
     O************************ PRLIC#    49
     O                       PRNUM               49
     O*** END CHG *** 09/29/04 ****************************************
     O                       TYPE2               52
     O                       TAXID#              61
     O                       PRLSNM              80
     O*** END CHG *** 06/29/00 ****************************************
     O          EADD         DETAIL
     O                       RECTYP               2
     O                       SRVDTF               7
     O                       SRVDTT              13
     O                       FILL                14
     O                       FEECDE              19
     O                       MOD1                21
     O                       MOD2                23
     O                       DIAG1               24
     O                       DIAG2               25
     O                       DIAG3               26
     O                       DIAG4               27
     O                       UNITS               30
     O                       CHGAMT              39
     O                       PAYAMT              48
     O                       PAYCDE              49
     O                       PLACE               51
     O                       FILL13              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** BEG ADD *** 06/29/00 ****************************************
     OFATHPTX   EADD         HED0
     O                       RECTYP               2
     O                       FORM#                3
     O                       CARCDE              12
     O                       LOCCDE              14
     O                       YRSUB               18
     O                       JULDT               21
     O                       TYPE1A              26
     O                       FILL38              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         HED1A
     O                       RECTYP               2
     O                       LSNAME              22
     O                       FSNAME              34
     O                       MID                 43
     O                       PAYDT               46
     O                       ADMDT               52
     O                       SUBDT               58
     O                       RECDT               64
     O*** BEG ADD *** 06/29/00 ****************************************
     O*** BEG CHG *** 04/05/01 ****************************************
     O************************ CCID      80
     O                       CCIDU               80
     O*** END CHG *** 04/05/01 ****************************************
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         HED1B
     O                       RECTYP               2
     O                       SRVDTF               8
     O                       SRVDTT              14
     O                       TTLCHR              23
     O                       ESTDUE              32
     O                       EXCPT1              33
     O                       SSN                 42
     O                       INJDT               48
     O                       MEDPRO              58
     O                       TYPSRV              61
     O                       FILL3               64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         DTL
     O                       RECTYP               2
     O                       ICD9                 7
     O                       ICD91               13
     O                       ICD92               19
     O                       ICD93               25
     O                       OTHDIA              31
     O                       ICD9CM              37
     O                       ICD9C1              43
     O                       ICD9C2              49
     O                       TAXID#              58
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL22              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         DTLX1
     O                       RECTP1               2
     O                       ITMIZ1               7
     O                  91   FILL2                9
     O                  91   UNITS1              12
     O                  91   CHARG1              21
     O                  91   AMTPD1              30
     O                  91   EXC1                31
     O                 N91   FILL24              31
     O                       ITMIZ2              36
     O                  92   FILL2               38
     O                  92   UNITS2              41
     O                  92   CHARG2              50
     O                  92   AMTPD2              59
     O                  92   EXC2                60
     O                  92   FILL4               64
     O                 N92   FILL28              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         DTLX2
     O                       RECTP2               2
     O                       ITMIZ3               7
     O                  93   FILL2                9
     O                  93   UNITS3              12
     O                  93   CHARG3              21
     O                  93   AMTPD3              30
     O                  93   EXC3                31
     O                 N93   FILL24              31
     O                       ITMIZ4              36
     O                  94   CPT44               38
     O                  94   UNITS4              41
     O                  94   CHARG4              50
     O                  94   AMTPD4              59
     O                  94   EXC4                60
     O                  94   FILL4               64
     O                 N94   FILL28              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         DTLX3
     O                       RECTP3               2
     O                       ITMIZ5               7
     O                  95   CPT45                9
     O                  95   UNITS5              12
     O                  95   CHARG5              21
     O                  95   AMTPD5              30
     O                  95   EXC5                31
     O                 N95   FILL24              31
     O                       ITMIZ6              36
     O                  96   CPT46               38
     O                  96   UNITS6              41
     O                  96   CHARG6              50
     O                  96   AMTPD6              59
     O                  96   EXC6                60
     O                  96   FILL4               64
     O                 N96   FILL28              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     O          EADD         DTLX4
     O                       RECTP4               2
     O                       ITMIZ7               7
     O                  97   CPT47                9
     O                  97   UNITS7              12
     O                  97   CHARG7              21
     O                  97   AMTPD7              30
     O                  97   EXC7                31
     O                 N97   FILL24              31
     O                       FILL33              64
     O*** BEG ADD *** 06/29/00 ****************************************
     O                       FILL16              80
     O*** END ADD *** 06/29/00 ****************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             XXFNNM              60
     O**********E            ROUTE          1
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       XXFNNM              60
     O          E            ROUTE          1
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
     O****************
     O*  PROMPT SELECTION
     O****************
     O          E            ROUTE          1 34
     O                                           18 'COMPANY  :'
     O                       XXCO#         Z     25
     O          E            ROUTE          1
     O                                           18 'SUBSCRIBER:'
     O                       XXSUB#        Z     27
     O          E            ROUTE          1
     O                                           18 'GROUP    :'
     O                       XXFND         Z     25
     O          E            ROUTE          1
     O                                           18 'FROM DATE:'
     O*** BEG CHG *** 12/13/99 ****************************************
     O************************ XXFROM    30 '  /  /  '
     O                       XXFROM              32 '  /  /    '
     O*** END CHG *** 12/13/99 ****************************************
     O          E            ROUTE          1
     O                                           18 'TO DATE  :'
     O*** BEG CHG *** 12/13/99 ****************************************
     O************************ XXTO      30 '  /  /  '
     O                       XXTO                32 '  /  /    '
     O*** END CHG *** 12/13/99 ****************************************
     O****************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O***                                   '(C) COPYRIGHT PROGRESSIV'
     O***                              +  0 'E DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEADER         1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEADER         1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEADER         1
     O                                           88 'FROM DATE:'
     O*** BEG CHG *** 12/13/99 ****************************************
     O************************ XXFROM    97 '  /  /  '
     O                       XXFROM              99 '  /  /    '
     O*** END CHG *** 12/13/99 ****************************************
     O                                          108 'TO DATE:'
     O*** BEG CHG *** 12/13/99 ****************************************
     O************************ XXTO     117 '  /  /  '
     O                       XXTO               119 '  /  /    '
     O*** END CHG *** 12/13/99 ****************************************
     O          E            HEADER         1
     O                       XXPROC             102
     O          E            HEADER         1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                       XXSBNM              60
     O          E            HEADER         2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                                           12 '-'
     O                       XXFNNM              51
     O          E            HEADER         1
     O               90                          10 'CARRIER'
     O               90                          22 'LOCATION'
     O               90                          29 'YEAR'
     O               90                          35 'DAY'
     O               90                          45 'TYPE 1A'
     O               90                          61 'CLAIMANT NAME'
     O               90                          95 'SOC SEC #'
     O*** BEG DEL *** 12/13/99 ****************************************
     O**************** 90               109 'INJURY DATE'
     O**************** 90               121 'CONSULTED'
     O**************** 90               132 'REC DATE'
     O**************** 90               143 'PAY DATE'
     O*** END DEL *** 12/13/99 ****************************************
     O*** BEG ADD *** 12/13/99 ****************************************
     O               90                         111 'INJURY DATE'
     O               90                         123 'CARR ID'
     O               90                         134 'REC DATE'
     O               90                         145 'PAY DATE'
     O*** END ADD *** 12/13/99 ****************************************
     O               90                         165 'ICD-9 CODES'
     O               90                         185 'LICENCE #'
     O               90                         193 'TYPE 2'
     O              N90                          12 'CARRIER'
     O              N90                          23 'LOCATION'
     O              N90                          29 'YEAR'
     O              N90                          34 'DAY'
     O              N90                          43 'TYPE 1A'
     O              N90                          57 'CLAIMANT NAME'
     O              N90                          87 'CLAIM ID'
     O*** BEG DEL *** 12/13/99 ****************************************
     O*************** N90                89 'PAY DATE'
     O*************** N90                99 'SUB DATE'
     O*************** N90               109 'REC DATE'
     O*************** N90               120 'FROM DATE'
     O*************** N90               129 'TO DATE'
     O*************** N90               145 'TOTAL CHARGES'
     O*************** N90               160 'ESTIMATED DUE'
     O*************** N90               170 'PAY CODE'
     O*************** N90               181 'SOC SEC #'
     O*************** N90               194 'INJURY DATE'
     O*** END DEL *** 12/13/99 ****************************************
     O*** BEG ADD *** 12/13/99 ****************************************
     O              N90                         106 'PAY DATE'
     O              N90                         117 'SUB DATE'
     O              N90                         128 'REC DATE'
     O              N90                         139 'FROM DATE'
     O              N90                         150 'TO DATE'
     O              N90                         164 'TOTAL CHRG'
     O              N90                         174 'EST DUE'
     O              N90                         183 'PAY CODE'
     O              N90                         187 'SS#'
     O              N90                         198 'INJURY DT'
     O*** END ADD *** 12/13/99 ****************************************
     O          E            HEADER         2
     O*** BEG CHG *** 12/13/99 ****************************************
     O**************** 90                51 'FROM DATE'
     O**************** 90                61 'TO DATE'
     O               90                          50 'FROM DATE'
     O               90                          64 'TO DATE'
     O*** END CHG *** 12/13/99 ****************************************
     O               90                          73 'FEE CODE'
     O               90                          84 'MODIFIER'
     O               90                          92 'DIAG1'
     O               90                         100 'UNITS'
     O               90                         116 'CHARGE AMOUNT'
     O               90                         133 'PAYMENT AMOUNT'
     O               90                         144 'PAY CODE'
     O               90                         154 'SERVICE'
     O              N90                          38 'PROVIDER #'
     O              N90                          47 'SERVICE'
     O              N90                          70 'DIAGNOSIS CODES'
     O              N90                          95 'ICD-9-CM CODES'
     O              N90                         108 'TAX ID#'
     O              N90                         118 'ITEMIZE'
     O              N90                         124 'CPT4'
     O              N90                         131 'UNITS'
     O              N90                         141 'CHARGE'
     O              N90                         156 'AMOUNT PAID'
     O              N90                         167 'EXCEPT'
     O          E            HEADR1         1
     O               90      CARCDE              11
     O               90      LOCCDE              19
     O               90      YRSUB               29
     O               90      JULDT               35
     O               90      TYPE1A              44
     O               90      FSNAME              60
     O               90      MID2                62
     O               90      LSNAME              83
     O               90      SSN                 95
     O*** BEG DEL *** 12/13/99 ****************************************
     O**************** 90      INJDT    107 '  /  /  '
     O**************** 90      CONDT    120 '  /  /  '
     O**************** 90      RECDT    132 '  /  /  '
     O**************** 90      PAYDT    143 '  /  /  '
     O*** END DEL *** 12/13/99 ****************************************
     O*** BEG ADD *** 12/13/99 ****************************************
     O               90      INJDT1             112 '  /  /    '
     O*** BEG CHG *** 06/29/00 ****************************************
     O***********      90      CONDT1   123 '  /  /    '
     O               90      CCID1              123
     O*** END CHG *** 06/29/00 ****************************************
     O               90      RECDT1             134 '  /  /    '
     O               90      PAYDT1             145 '  /  /    '
     O*** END ADD *** 12/13/99 ****************************************
     O               90      ICD9               152
     O               90      ICD91              159
     O               90      ICD92              166
     O               90      ICD93              173
     O               90      PRLIC#             184
     O               90      TYPE2              192
     O              N90      CARCDE              13
     O              N90      LOCCDE              20
     O              N90      YRSUB               29
     O              N90      JULDT               34
     O              N90      TYPE1A              42
     O              N90      FSNAME              57
     O              N90      MID2                58
     O              N90      LSNAME              79
     O*** BEG ADD *** 06/29/00 ****************************************
     O              N90      CCID1               89
     O*** END ADD *** 06/29/00 ****************************************
     O*** BEG DEL *** 12/13/99 ****************************************
     O*************** N90      PAYDT     89 '  /  /  '
     O*************** N90      SUBDT     99 '  /  /  '
     O*************** N90      RECDT    109 '  /  /  '
     O*************** N90      SRVDTF   119 '  /  /  '
     O*************** N90      SRVDTT   130 '  /  /  '
     O*************** N90      TTLCHRJ  143
     O*************** N90      ESTDUEJ  158
     O*************** N90      PAYCDE   166
     O*************** N90      SSN      181
     O*************** N90      INJDT    192 '  /  /  '
     O*** END DEL *** 12/13/99 ****************************************
     O*** BEG ADD *** 12/13/99 ****************************************
     O              N90      PAYDT1             106 '  /  /    '
     O              N90      SUBDT1             117 '  /  /    '
     O              N90      RECDT1             128 '  /  /    '
     O              N90      SVDTF1             139 '  /  /    '
     O              N90      SVDTT1             150 '  /  /    '
     O              N90      TTLCHR        J    163
     O              N90      ESTDUE        J    175
     O              N90      PAYCDE             177
     O              N90      SSN                187
     O              N90      INJDT1             198 '  /  /    '
     O*** END ADD *** 12/13/99 ****************************************
     O          E            DETL           1
     O*** BEG CHG *** 12/13/99 ****************************************
     O**************** 90      SRVDTF    50 '  /  /  '
     O**************** 90      SRVDTT    62 '  /  /  '
     O               90      SVDTF1              50 '  /  /    '
     O               90      SVDTT1              64 '  /  /    '
     O*** END CHG *** 12/13/99 ****************************************
     O               90      FEECDE              72
     O               90      MOD1                81
     O               90      DIAG1               90
     O               90      UNITS               99
     O               90      CHGAMT        J    114
     O               90      PAYAMT        J    131
     O               90      PAYCDE             140
     O               90      PLACE              151
     O              N90      MEDPRO              38
     O              N90      TYPE1               45
     O              N90      ICD9                55
     O              N90      ICD91               62
     O              N90      ICD92               69
     O              N90      ICD93               76
     O              N90      ICD9CM              84
     O              N90      ICD9C1              91
     O              N90      ICD9C2              98
     O              N90      TAXID#             109
     O              N90 91   ITMIZ1             117
     O              N90 92   ITMIZ2             117
     O              N90 93   ITMIZ3             117
     O              N90 94   ITMIZ4             117
     O              N90 95   ITMIZ5             117
     O              N90 96   ITMIZ6             117
     O              N90 97   ITMIZ7             117
     O              N90 98   ITEM               117
     O              N90 94   CPT44              123
     O              N90 95   CPT45              123
     O              N90 96   CPT46              123
     O              N90 97   CPT47              123
     O              N90 91   UNITS1             130
     O              N90 92   UNITS2             130
     O              N90 93   UNITS3             130
     O              N90 94   UNITS4             130
     O              N90 95   UNITS5             130
     O              N90 96   UNITS6             130
     O              N90 97   UNITS7             130
     O              N90 91   CHARG1        J    143
     O              N90 92   CHARG2        J    143
     O              N90 93   CHARG3        J    143
     O              N90 94   CHARG4        J    143
     O              N90 95   CHARG5        J    143
     O              N90 96   CHARG6        J    143
     O              N90 97   CHARG7        J    143
     O              N90 91   AMTPD1        J    156
     O              N90 92   AMTPD2        J    156
     O              N90 93   AMTPD3        J    156
     O              N90 94   AMTPD4        J    156
     O              N90 95   AMTPD5        J    156
     O              N90 96   AMTPD6        J    156
     O              N90 97   AMTPD7        J    156
     O              N90 91   EXC1               163
     O              N90 92   EXC2               163
     O              N90 93   EXC3               163
     O              N90 94   EXC4               163
     O              N90 95   EXC5               163
     O              N90 96   EXC6               163
     O              N90 97   EXC7               163
     O*** BEGIN ADD *** 02/18/97 ***************************************
     O          E            CNT         2
     O                                           25 'TOTAL NUMBER OF RECORDS'
     O                                           35 'PER FUND:'
     O                       COUNT               49
     O*** BEGIN ADD *** 02/18/97 ***************************************
     O          E            ERRMS1      3  1
     O                                           33 '*** THERE IS NO COMPANY'
     O                                           54 'AUXILIARY RECORD FOR'
     O                                           67 'THIS COMPANY'
     O          E            ERRMS1         1
     O                                           29 'ENTER ONE, THEN'
     O                                           50 'RE-RUN THIS JOB. ***'
     O          E            ERRMS2      3  1
     O                                           33 '*** THERE ARE NO EOMB R'
     O                                           53 'ECORDS FOR THIS FUND'
     O                                           75 ' WITHIN SELECTED DATES'
     O          E            ERRMS3      1  1
     O                                           33 '*** THERE ARE NO DETAIL'
     O                                           53 ' RECORDS FOR THIS EO'
     O                                           55 'B:'
     O                       EAEOB#        3     65
     OPEA01     E            REA01
     OPEO01     E            REO01
