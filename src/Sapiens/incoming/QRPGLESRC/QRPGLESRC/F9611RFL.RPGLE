     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H*    PROGRAM: F9611RFL                                          *
     H*    DESCRIPTION: CREATE WORKFILE AND REPORT-Handles P and H    *
     H*                 types either original submission i.e. EOCNTL  *
     H*                 EQ blank or resubmissions/reconsiderations    *
     H*                 if EOCNTL NE blanks and EOSBDT EQ blanks.     *
     H*                 Recons are submitted with resubmissions and   *
     H*                 will have the RECON flag EQ 'R'.              *
     H*****************************************************************
     H*    PROGRAMMER NOTES: THE CONTROL# CNTL IS MADE UP OF          *
     H*                      CCCYYJJJNNNNN                            *
     H*                      CCC=Submitter#, YY=Year, JJJ=Julian Date *
     H*                      NNNNN=The calculated sequence number     *
     H*****************************************************************
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      F9611CFL - CREATE WORKFILE                               *
     H*    CALLS TO:                                                  *
     H*      F9616CFL - CONVERT NAMES TO ALL UPPERCASE                *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/17/20 C:295 Project: SY420                                 *
     H*                -Mask SSN Recompile only                       *
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
     H*  11/19/03 C:079 Support: MC010 39855
     H*                 Use expanded lic# field when database is used
     H*
     H*  04/28/03 C:079 Project: FL043
     H*                 Add new expanded license number field 13A
     H*                 Add new indicator field. Expand record to 400
     H*                 for phys/dental and 500 to hospital
     H*
     H*  01/16/03 C:079 Project: 48MFD
     H*                 Per state do not use FEIN to determine out of
     H*                 state license numbers
     H*
     H*  01/08/03 C:062 Project: FL027
     H*                 Dental Processing.
     H*
     H*  08/08/02 C:079 Support: MC044 33604
     H*                 1. Change indicator for No records found to 66
     H*                    so it won't conflict with 99(chain ind)
     H*
     H*  06/07/02 C:079 Support: MC048 31577
     H*                 1. If lic# not in database and override flag
     H*                    <> Y then report as ZZ9999999(out of state)
     H*
     H*  04/30/02 C:079 Support: FL041
     H*                 1. Load other 3 ICD9 code using correct key
     H*                 1. Load princ proc and other based on PCM
     H*
     H*  04/23/02 C:079 Support: MC048 32199
     H*                 1. Report billed code fields if not blank(UB)
     H*
     H*  04/08/02 C:079 Support: MC048 31577
     H*                 1. Move left EUPRCx into key field so chain
     H*                    succeeds.
     H*
     H*  06/22/01 C:079 Support: MC048 27861
     H*                 1. Change indicator from 99 to 98 on chain to
     H*                    mod file. It caused CNTL# file not to upd.
     H*
     H*  06/07/01 C:079 Support: MC048 27094
     H*                 1. On recons dont submit if batch has not
     H*                    updated EOWCCK with EAWCPD.
     H*
     H*  05/22/01 C:079 Support: MC048 27094
     H*                 1. On recons get the recon received date using
     H*                    bill status = 'R' to get correct date.
     H*
     H*  04/18/01 C:079 Support: MC048 27094
     H*                 1. If out of state provider then default lic#
     H*                    to ZZ9999999
     H*                 2. Validate the modifier and default to blanks
     H*                    if invalid.
     H*
     H*  09/21/00 C:079 Project: FL034
     H*                 Change how recon is set to = 'R'
     H*
     H*  06/21/00 C:117 Project: FL030
     H*                 Chgs concerning Resubmissions due to
     H*                 additional Data Submission requirements
     H*
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION (CONDITIONED DATA FOR DATERC)
     H*
     H*  04/02/00 C:117 PROJECT: MC010  SEQ#: 21497
     H*                 GOT FUND NAME TO MATCH FUND ON REPRT WHEN FUND
     H*                 CHANGES
     H*
     H*  02/17/00 C:117 PROJECT: MC033  SEQ#: 21890
     H*                 PROCCH FIELD PRINTING ALL ZEROS
     H*
     H*  01/21/00 C:117 Project: MC010  Seq#: 21136
     H*                 Populated "To Date of Service" rather than
     H*                 filling w/ zeros.
     H*
     H*  01/05/00 C:117 Project: FL007
     H*                 Enable Hospital Data Submission
     H*
     H*  12/29/99 C:117 Project: MC010  Seq#: 21136
     H*                 Problems with the Billed Code; going to check
     H*                 for blanks in the field.
     H*
     H*  12/03/99 C:117 Project: Y2K
     H*                 Had to change some date formats to show
     H*                 all 8 characters rather than 6.
     H*
     H*  10/21/99 C:117 Project: FLMNT
     H*                 Sup Seq: 20175
     H*                 Changed where I got FLIC (FL Lic#) from; used
     H*                 to be EOFLIC, now it is EHLIC#.
     H*
     H*  09/21/99 C:117 Project: FL021 FL Tape Submission
     H*                 Changed whole program to use FAEOP instead of
     H*                 FAEAP.
     H*
     H*  08/24/99 C:117 Project: FLMNT
     H*                 Sup Seq: 19357
     H*                 Chained out to FDCAP to check for detail recs
     H*                 before doing anything with the control number.
     H*                 If there are no details, then it doesn't get
     H*                 a CNTL# & that record is skipped.
     H*
     H*  08/30/99 C:79  Support: 19312
     H*                 Select only Florida bills
     H*
     H*  06/12/99 C:117 Project: FL021 FL Tape Submission
     H*                 Added update of FAEAP; update fields added to
     H*                 FAEAP: EACNTL & EAESDT; mapped "Billed Code"
     H*
     H*  05/05/99  C:56   MC152
     H*                   Change FAEAL to FAEAL1, duplicate logicals
     H*
     H*  04/06/99 C:05  Project: FL109
     H*                 NOTE: Pgm names chgd from F951x to F961x
     H*                 1. Changeover to 'Provider Bills' version
     H*                 2. Use of FAEAP rather than FAEOP
     H*                 3. Delete use of FAE1P
     H*                 4. Delete use of FDHCP
     H*                 5. Update for Y2K usage
     H*                 6. Add usage of FAEHP, FAEUP, FDDHP
     H*                                 FDDUP, FDDAP, FDDEP
     H*                    NOTE: For ease of reading the pgm, I didn't
     H*                          comment out old code - but deleted it
     H*                          If necessary, refer back to B70 vers.
     H*
     H*  07/10/98 C:31  Support: 12441
     H*                 From/to service date issue
     H*                 1. Changed service date logic.
     H*
     H*  03/27/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Development
     H*
     H*  12/11/97 C:53 REQ/PROJ: FTMNT # 9612
     H*                CHG XPLAN LENGTH IN DS SO IT DOES NOT GET
     H*                OVERLAYED WITH BLANKS CAUSING AN ERROR ON TAPE
     H*
     H*  04/02/96 C:20 REQ/PROJ: FL006
     H*                1. MODIFICATION TO USE THE CORRECT KEY IN
     H*                   CHAINING TO THE DETAIL CHARGE AUXILIARY
     H*                   FILE.
     H*
     H*  02/19/96 C:20 REQ/PROJ: FL006
     H*                1. IF THIS IS A STANDALONE WCMC FUIND, E1ENDT
     H*                   WOULD NOT HAVE THE PROVIDER CHECK DATE.  WE
     H*                   HAVE MODIFIED PROGRAM TO USE THE DATE THE
     H*                   EOMB WAS CREATED AND THE PROVIDER PAYMENT
     H*                   DATE.
     H*
     H*  10/13/95 C:32 REQ/PROJ: FL006
     H*                MOVED CHAIN OF FDHCL TO BEGINNING OF MAIN LOOP
     H*
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  65   -   NO HIT ON FDHCP                                     *
     H* N65   -   HIT ON FDHCP                                        *
     H*  66   -   NO EOMB FOR FUND WITHIN DATES                       *
     H*  70   -   ACCDT IS ZEROS DO NOT PRINT                         *
     H*  71   -   DTERC IS ZEROS DO NOT PRINT                         *
     H*  72   -   DTEPD IS ZEROS DO NOT PRINT                         *
     H*  73   -   ADMDT IS ZEROS DO NOT PRINT                         *
     H*  74   -   CVFRM IS ZEROS DO NOT PRINT                         *
     H*  75   -   CVTO  IS ZEROS DO NOT PRINT                         *
     H*  76   -   FDOS2 IS ZEROS DO NOT PRINT                         *
     H***77   -   TDOS2 IS ZEROS DO NOT PRINT (NO LONGER USED)        *
     H*  80   -   NO HIT ON FMTSP - ADD NEW SEQ# RECORD BY CO         *
     H* N80   -   HIT ON FMTSP - UPDATE SEQ# RECORD BY CO             *
     H*  87   -   PHYSICIAN                                           *
     H*  87   -   HOSPITAL                                            *
     H*  89   -   DENTAL                                              *
     H*  90   -   PHYSICIAN (CALLED FOR BY USER)                      *
     H*  91   -   HOSPITAL (CALLED FOR BY USER)                       *
     H*  92   -   DENTAL (CALLED FOR BY USER)                         *
     H*  98   -   GENERAL CHAIN                                       *
     H*  99   -   GENERAL CHAIN                                       *
     H*******************************************************************
     F*** BEG DEL *** 04/28/03 ****************************************
     F**** FATPPP  O   F     170            DISK                      A
     F**** FATPPH  O   F     208            DISK                      A
     F*** BEGIN ADD *** 01/08/03 **************************************
     F**** FATPPD  O   F     140            DISK                      A
     F*** END   ADD *** 01/08/03 **************************************
     F*** END DEL *** 04/28/03 ****************************************
     F*** BEG ADD *** 04/28/03 ****************************************
     FFATPPP    O  A F  400        DISK
     FFATPPD    O  A F  400        DISK
     FFATPPH    O  A F  500        DISK
     F*** END ADD *** 04/28/03 ****************************************
     FSMCOL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** BEG CHG *** 09/21/99 ****************************************
     F******FAEOL4  IF  E           K        DISK
     FFAEOL     UF   E             DISK
     F*** END CHG *** 09/21/99 ****************************************
     FFMFNL     IF   E           K DISK
     FFDCALH    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   IF  E           K        DISK
     F*** BEG CHG *** 06/12/99 ****************************************
     F***********FAEAL1  IF  E           K        DISK
     F*** BEG DEL *** 09/21/99 ****************************************
     F*******FAEAL1  UF  E           K        DISK
     F*** END DEL *** 09/21/99 ****************************************
     F*** END CHG *** 06/12/99 ****************************************
     F*** END CHG *** 05/05/99 ****************************************
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     F*** BEGIN ADD *** 01/08/03 **************************************
     FFAEDL     IF   E           K DISK
     F*** END   ADD *** 01/08/03 **************************************
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     F*** BEGIN ADD *** 01/08/03 **************************************
     FFDDDL     IF   E           K DISK
     F*** END   ADD *** 01/08/03 **************************************
     F*** BEG DEL *** 09/21/99 ****************************************
     F*******FDDAL   IF  E           K        DISK
     F*** END DEL *** 09/21/99 ****************************************
     FFDDEL4    IF   E           K DISK
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FFMTSP     UF A E           K DISK
     F*** BEG ADD *** 04/11/00 ****************************************
     FFAESL3    IF   E           K DISK
     F*** END ADD *** 04/11/00 ****************************************
     F*** BEG ADD *** 06/21/00 ****************************************
     FFAEAL1    IF   E           K DISK
     F*** BEG ADD *** 06/21/00 ****************************************
     F*
     F*** BEG ADD *** 04/18/01 ****************************************
     FFTPMLFL   IF   E           K DISK
     F*** END ADD *** 04/18/01 ****************************************
     F*** BEG ADD *** 06/07/02 ****************************************
     FFMPLLFL   IF   E           K DISK
     FSTSTL     IF   E           K DISK
     F*** END ADD *** 06/07/02 ****************************************
     F*

     D EOMB            S              8    DIM(18) CTDATA PERRCD(1)
     D*
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
     D                 DS
     D  XXCNT                  1      8  0
     D  XXMMDD                 1      4  0
     D  XXYR                   5      8  0
     D                 DS
     D  XXPLAN                 1     10
     D  XPLAN                  5      6
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
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*** BEG ADD *** 06/21/00 ****************************************
     C                   PARM                    QQRSUB            1
     C*** END ADD *** 06/21/00 ****************************************
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
     C                   PARM      '*MDYY'       SRTFMT
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C                   KFLD                    EOFND
     C*
     C* Key List for the Output Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
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
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
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
     C*
     C*** BEG DEL *** 09/21/99 ****************************************
     C* Key List for the EOMB TRANSACTION FILE
     C***********EOKY1     KLIST
     C***********          KFLD           XXCO#
     C***********          KFLD           XXSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           XXEOB#
     C*** END DEL *** 09/21/99 ****************************************
     C*
     C* Key List for the EOMB HEADER HFCA FILE
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* Key List for the EOMB DETAIL HCFA FILE
     C     DHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*
     C* Key List for the EOMB HEADER UB92 FILE
     C     EUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* Key List for the EOMB DETAIL UB92 FILE
     C     DUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*** BEGIN ADD *** 01/08/03 **************************************
     C*
     C* Key List for the EOMB HEADER DENTAL FILE
     C     EDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* Key List for the EOMB DETAIL DENTAL FILE
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C*** BEG DEL *** 09/21/99 ****************************************
     C* Key List for the EOMB DETAIL AMOUNT FILE
     C********** DAKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           XXEOB#
     C**********           KFLD           XXRCN
     C*
     C********** DAKY1     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           XXEOB#
     C*** END DEL *** 09/21/99 ****************************************
     C*
     C* Key List for the EOMB DETAIL EOB CODE FILE
     C     DEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXESRC
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 06/21/00 ****************************************
     C*
     C*  EOMB Header Amount file
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    X1RCN
     C*** END ADD *** 06/21/00 ****************************************
     C*
     C*** BEG ADD *** 04/18/01 ****************************************
     C*
     C*  Procedure code modifier table file
     C*
     C     PMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** END ADD *** 04/18/01 ****************************************
     C*** BEG ADD *** 06/07/02 ****************************************
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C** Key List Florida License# Database file
     C*
     C     PLKY          KLIST
     C                   KFLD                    KKLIC#
     C*** END ADD *** 06/07/02 ****************************************
     C*
     C* Set-up and Initialize
     C*
     C*** BEG ADD *** 06/07/02 ****************************************
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    PLLIC#        KKLIC#
     C*** END ADD *** 06/07/02 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C********** *LIKE     DEFN DADSEQ    XXDSEQ
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C*** END CHG *** 09/21/99 ****************************************
     C     *LIKE         DEFINE    DEESRC        XXESRC
     C*** BEG CHG *** 09/21/99 ****************************************
     C********** *LIKE     DEFN EARCN     XXRCN
     C     *LIKE         DEFINE    EORCN         XXRCN
     C********** *LIKE     DEFN EAFYR     XXFYR
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C*** END CHG *** 09/21/99 ****************************************
     C*** BEG ADD *** 04/18/01 ****************************************
     C     *LIKE         DEFINE    PMKEY         XXKEY
     C*** END ADD *** 04/18/01 ****************************************
     C*** BEG ADD *** 06/21/00 ****************************************
     C     *LIKE         DEFINE    EARCN         X1RCN
     C*** END ADD *** 06/21/00 ****************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFRDT        XQFRDT            8 0
     C                   MOVE      QQTODT        XQTODT            8 0
     C                   Z-ADD     XQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     XQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     XXFRDT        SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*******              MOVELSRDTO     XXFROM  60
     C                   MOVEL     SRDTO         XXFROM            8 0
     C                   Z-ADD     XXTODT        SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*******              MOVELSRDTO     XXTO    60
     C                   MOVEL     SRDTO         XXTO              8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   Z-ADD     0             CNTR              5 0
     C                   TIME                    DSDTTM
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** QQPROC    IFEQ 'P'
     C**********           MOVEL'PHYSI'   XXPROC  9
     C**********           MOVE 'CIAN'    XXPROC
     C**********           MOVE '1'       *IN90
     C**********           ELSE
     C**********           MOVEL' HOSPI'  XXPROC
     C**********           MOVE 'TAL'     XXPROC
     C**********           MOVE '0'       *IN90
     C**********           END
     C                   SETOFF                                       909192
     C                   SELECT
     C     QQPROC        WHENEQ    'P'
     C                   MOVEL     'PHYSI'       XXPROC            9
     C                   MOVE      'CIAN'        XXPROC
     C                   SETON                                        90
     C     QQPROC        WHENEQ    'H'
     C                   MOVEL     'HOSPI'       XXPROC            9
     C                   MOVE      'TAL '        XXPROC
     C                   SETON                                        91
     C     QQPROC        WHENEQ    'D'
     C                   MOVEL     'DENTA'       XXPROC            9
     C                   MOVE      'L   '        XXPROC
     C                   SETON                                        92
     C                   ENDSL
     C*** END   CHG *** 01/08/03 **************************************
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
     C* Call sub-program SRDDIF8 to Calc Julian Date
     C*
     C                   MOVE      ZZDT          SRFRDC            8 0
     C                   MOVE      XXDT          SRTODC            8 0
     C*
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
     C* Get the Routing Information and Print Routing Page
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              61
     C*********     *IN61         IFEQ      '0'
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   MOVE      '0'           *IN99
     C*
     C**  BEGIN READING BATCH
     C*** BEG CHG *** 09/21/99 ****************************************
     C************         READ PEA01                    62
     C                   READ      PEO01                                  62
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C     *IN62         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEG ADD *** 08/24/99 ****************************************
     C     CAKY          CHAIN     PCA01                              59
     C     *IN59         IFEQ      *OFF
     C*** END ADD *** 08/24/99 ****************************************
     C                   MOVE      *BLANK        RECORD            1
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          Z-ADDEAFND     XXFND
     C***********          Z-ADDEAFYR     XXFYR
     C***********          Z-ADDEACASE    XXCASE
     C***********          Z-ADDEAEOB#    XXEOB#
     C***********          Z-ADDEARCN     XXRCN
     C                   Z-ADD     EOFND         XXFND
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE
     C                   Z-ADD     EOEOB#        XXEOB#
     C                   Z-ADD     EORCN         XXRCN
     C*** BEG ADD *** 06/21/00 ****************************************
     C                   Z-ADD     0             X1RCN
     C*** END ADD *** 06/21/00 ****************************************
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C*** BEG ADD *** 09/21/00 ****************************************
     C*  The following sets the recon flag to 'F' or 'R' and will be
     C*  applied to all detail line items.
     C                   MOVE      'F'           RECON             1
     C*
     C     EORCN         IFGT      0
     C     EOCNTL        ANDNE     *BLANKS
     C*
     C                   MOVE      'R'           RECON
     C*
     C                   ENDIF                                                  EORCN GT 0
     C*** END ADD *** 09/21/00 ****************************************
     C*
     C*** BEG ADD *** 06/07/01 ****************************************
     C     EORCN         IFGT      0
     C*
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C* ONLY SELECT ESRRDT WHERE STATUS IS 'R'
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C                   LEAVE
     C                   ENDIF
     C*
     C     ESKY2         READPE    PES01                                  60
     C*
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C* IF CHECK DATE IS < RECON DATE THEN BATCH HAS NOT UPDATED CHECK
     C* DATE SO DON'T INCLUDE THIS EOB
     C*
     C     EOWCCK        IFLT      ESRRDT
     C                   GOTO      NODT
     C                   ENDIF
     C*
     C                   ENDIF                                                  EORCN IFGT
     C*** END ADD *** 06/07/01 ****************************************
     C*
     C*** BEG ADD *** 06/21/00 ****************************************
     C*
     C*  The following grabs the records that match the date range
     C*  passed in for a resubmission.  (At the recon level)
     C*
     C     QQRSUB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              58
     C*
     C     *IN58         DOWEQ     *OFF
     C*
     C     EAESDT        IFGT      XXTODT
     C     EAESDT        ORLT      XXFRDT
     C*
     C                   ADD       1             X1RCN
     C     EAKY          READE     PEA01                                  58
     C*
     C                   ELSE
     C*
     C                   GOTO      PROCES
     C                   ENDIF                                                  EAESDT>TODT
     C                   ENDDO                                                  *IN58=*OFF
     C*
     C                   ELSE
     C*
     C                   GOTO      PROCES
     C                   ENDIF                                                  QQRSUB='Y'
     C*
     C*  If it never goes to PROCES TAG, then it needs to automatically
     C*  go to NODT TAG.
     C                   GOTO      NODT
     C*
     C*** END ADD *** 06/21/00 ****************************************
     C*
     C*** BEG DEL *** 09/21/99 ****************************************
     C***********EOKY1     CHAINPEO01                64
     C*** END DEL *** 09/21/99 ****************************************
     C*** BEG ADD *** 06/21/00 ****************************************
     C     PROCES        TAG
     C*** END ADD *** 06/21/00 ****************************************
     C*
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********EAEDI     IFEQ 'Y'
     C     EOBTYP        IFEQ      'N'
     C***********EOAST     ANDEQ'FL'
     C*** END CHG *** 09/21/99 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********EAEDI     OREQ 'Y'
     C     EOBTYP        OREQ      'H'
     C***********EOAST     ANDEQ'FL'
     C*** END CHG *** 09/21/99 ****************************************
     C*** BEGIN ADD *** 01/08/03 **************************************
     C     EOBTYP        OREQ      'D'
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********EASYS     IFEQ 'P'
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE 'P'       RECORD
     C**********           MOVE '1'       *IN89
     C**********           ELSE
     C***********EASYS     IFEQ 'H'
     C********** EOBTYP    IFEQ 'H'
     C*** END CHG *** 09/21/99 ****************************************
     C**********           MOVE 'H'       RECORD
     C**********           MOVE '0'       *IN89
     C**********           END
     C**********           END
     C                   SETOFF                                       878889
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVE      'P'           RECORD
     C                   SETON                                        87
     C     EOBTYP        WHENEQ    'H'
     C                   MOVE      'H'           RECORD
     C                   SETON                                        88
     C     EOBTYP        WHENEQ    'D'
     C                   MOVE      'D'           RECORD
     C                   SETON                                        89
     C                   ENDSL
     C*** END   CHG *** 01/08/03 **************************************
     C                   ENDIF                                                  END EAEDI
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** *IN89     IFEQ *IN90
     C*** BEG CHG *** 08/08/02 ****************************************
     C******************** MOVE '1'       *IN99
     C**********           MOVE '1'       *IN66
     C*** END CHG *** 08/08/02 ****************************************
     C**********           END
     C     *IN90         IFEQ      *ON
     C     *IN87         ANDEQ     *ON
     C     *IN91         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     *IN92         OREQ      *ON
     C     *IN89         ANDEQ     *ON
     C                   SETON                                        66
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
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
     C*
     C* Get the PHYSICIAN BILL HEADER RECORD
     C     RECORD        IFEQ      'P'
     C     EHKY          CHAIN     PEH01                              63
     C                   ENDIF                                                  RECORD=P
     C*
     C* Get the HOSPITAL BILL HEADER RECORD
     C     RECORD        IFEQ      'H'
     C     EUKY          CHAIN     PEU01                              63
     C                   ENDIF                                                  RECORD=H
     C*** BEGIN ADD *** 01/08/03 **************************************
     C*
     C* Get the DENTAL BILL HEADER RECORD
     C     RECORD        IFEQ      'D'
     C     EDKY          CHAIN     PED01                              63
     C                   ENDIF                                                  RECORD=D
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C********************  HEADER SECTION  **********************************
     C* Clear Header record Fields
     C*
     C                   EXSR      CLRH
     C*
     C* Move information to Header fields
     C*** BEG CHG *** 06/12/99 ****************************************
     C* CNTL
     C**************       MOVE C1SUBM    CCC              submitter cd
     C**************       MOVE XXYY      YY               year
     C**************       MOVE SRDAYS    JJJ              Julian date
     C**************       ADD  1         CNTR             calc seq #
     C**************       MOVE CNTR      SSSSS            seq #
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********EARCN     IFEQ *ZEROS
     C***********EASYS     ANDEQ'P'
     C     EOBTYP        IFEQ      'N'
     C     EOCNTL        ANDEQ     *BLANKS
     C     *IN59         ANDEQ     *OFF
     C*** END CHG *** 09/21/99 ****************************************
     C                   MOVE      C1SUBM        CCC                            submitter cd
     C                   MOVE      XXYY          YY                             year
     C                   MOVE      SRDAYS        JJJ                            Julian date
     C                   ADD       1             CNTR                           calc seq #
     C                   MOVE      CNTR          SSSSS                          seq #
     C                   ENDIF
     C*** END CHG *** 06/12/99 ****************************************
     C*** BEG ADD *** 01/05/00 ****************************************
     C     EOBTYP        IFEQ      'H'
     C     EOCNTL        ANDEQ     *BLANKS
     C     *IN59         ANDEQ     *OFF
     C                   MOVE      C1SUBM        CCC                            submitter cd
     C                   MOVE      XXYY          YY                             year
     C                   MOVE      SRDAYS        JJJ                            Julian date
     C                   ADD       1             CNTR                           calc seq #
     C                   MOVE      CNTR          SSSSS                          seq #
     C                   ENDIF
     C*** END ADD *** 01/05/00 ****************************************
     C*** BEGIN ADD *** 01/10/03 **************************************
     C     EOBTYP        IFEQ      'D'
     C     EOCNTL        ANDEQ     *BLANKS
     C     *IN59         ANDEQ     *OFF
     C                   MOVE      C1SUBM        CCC                            submitter cd
     C                   MOVE      XXYY          YY                             year
     C                   MOVE      SRDAYS        JJJ                            Julian date
     C                   ADD       1             CNTR                           calc seq #
     C                   MOVE      CNTR          SSSSS                          seq #
     C                   ENDIF
     C*** END   ADD *** 01/10/03 **************************************
     C*** BEG ADD *** 06/12/99 ****************************************
     C*** BEG DEL *** 09/21/99 ****************************************
     C***********EARCN     IFEQ *ZEROS
     C*** END DEL *** 09/21/99 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********EACNTL    ANDEQ*BLANKS
     C     EOCNTL        IFEQ      *BLANKS
     C***********EASYS     ANDEQ'P'
     C     EOBTYP        ANDEQ     'N'
     C*** END CHG *** 09/21/99 ****************************************
     C*** BEG ADD *** 01/05/00 ****************************************
     C     EOCNTL        OREQ      *BLANKS
     C     EOBTYP        ANDEQ     'H'
     C*** BEGIN ADD *** 01/10/03 **************************************
     C     EOCNTL        OREQ      *BLANKS
     C     EOBTYP        ANDEQ     'D'
     C*** END   ADD *** 01/10/03 **************************************
     C     *IN59         ANDEQ     *OFF
     C*** END ADD *** 01/05/00 ****************************************
     C*
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          MOVELCNTL      EACNTL
     C                   MOVEL     CNTL          EOCNTL
     C                   MOVEL     DSDATE        EOSBDT
     C***********          MOVELDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     EAESDT
     C***********          MOVELSRDTO     EOWCCK
     C*** END CHG *** 09/21/99 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          UPDATPEA01
     C                   UPDATE    PEO01
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C                   ELSE
     C*** BEG CHG *** 09/21/99 ****************************************
     C*
     C*This is a resubmission if EOCNTL<>Blanks & EOSBDT=Blanks
     C*
     C***********EAESDT    IFEQ *ZEROS
     C***********EACNTL    ANDNE*BLANKS
     C     EOSBDT        IFEQ      *BLANKS
     C     EOCNTL        ANDNE     *BLANKS
     C                   MOVEL     DSDATE        EOSBDT
     C***********          MOVELDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     EOWCCK
     C***********          MOVELEACNTL    CNTL
     C                   MOVEL     EOCNTL        CNTL
     C*
     C***********          UPDATPEA01
     C                   UPDATE    PEO01
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*****           MOVE *OFF      *IN89
     C**N90*****           MOVE *ON       *IN89
     C                   SETOFF                                       878889
     C*** END   CHG *** 01/08/03 **************************************
     C***********          EXCPTREA01
     C                   EXCEPT    REO01
     C*** BEG CHG *** 09/21/99 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 06/12/99 ****************************************
     C* CARR
     C                   MOVE      F25N01        CARR
     C*** BEGIN ADD *** 01/08/03 **************************************
     C                   MOVE      F25N01        CARRD
     C*** END   ADD *** 01/08/03 **************************************
     C* CARFIN
     C                   MOVE      F29N01        CARFIN                         CARFEIN
     C* CARZIP
     C                   MOVEL     F29N02        CARZIP
     C*** BEGIN ADD *** 01/08/03 **************************************
     C                   MOVEL     F29N02        CARZID
     C*** END   ADD *** 01/08/03 **************************************
     C* AGNT
     C     F25N02        IFGT      *ZEROS
     C                   MOVE      F25N02        AGNT
     C* AGTFIN
     C                   MOVE      F29N03        AGTFIN                         AGENTFEIN
     C* AGTZIP
     C                   MOVEL     F29N04        AGTZIP                         AGENTZIP
     C                   END
     C* SSN
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         SSN
     C                   END
     C* DOA
     C     CMACDT        IFEQ      0
     C                   MOVEL     '19'          TEMP              4 0
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          DOA
     C                   MOVE      '1'           *IN70
     C                   ELSE
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DOA               8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CMACDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     ACCDT   60
     C                   MOVEL     SRDTO         ACCDT             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* PHYS
     C*** BEGIN CHG *** 01/08/03 **************************************
     C*** BEG ADD *** 01/05/00 ****************************************
     C********   RECORD    IFEQ 'P'
     C*** END ADD *** 01/05/00 ****************************************
     C*** BEG CHG *** 10/21/99 ****************************************
     C***********EOFLIC    IFEQ *BLANKS
     C********   EHLIC#    IFEQ *BLANKS
     C*** END CHG *** 10/21/99 ****************************************
     C********   PDFLIC    IFNE *BLANKS
     C********             MOVE PDFLIC    PHYS
     C********             ELSE                            ELSE NE *BLANKS
     C********             MOVEL'ZZ999'   PHYS
     C********             MOVE '9999'    PHYS
     C********             END                             END NE *BLANKS
     C********             ELSE                            ELSE EQ *BLANKS
     C*** BEG CHG *** 10/21/99 ****************************************
     C***********          MOVE EOFLIC    PHYS
     C********             MOVE EHLIC#    PHYS
     C****END*CHG *** 10/21/99 ****************************************
     C********             END                             END EQ *BLANKS
     C********
     C****BEG*ADD *** 01/05/00 ****************************************
     C********             END                             END EQ 'P'
     C********
     C********   RECORD    IFEQ 'H'
     C********   EULIC#    IFNE *BLANKS
     C********             MOVE EULIC#    PHYS
     C********             ELSE                            ELSE NE *BLANKS
     C********             MOVEL'ZZ999'   PHYS
     C********             MOVE '9999'    PHYS
     C********             END                             END NE *BLANKS
     C********             END                             END EQ 'H'
     C*** END ADD *** 01/05/00 ****************************************
     C                   SELECT
     C     RECORD        WHENEQ    'P'
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL     EHFLIC        FLIC
     C                   MOVEL     'A'           REVIND
     C                   MOVE      *BLANKS       PHYS
     C                   ELSE
     C*** END ADD *** 04/28/03 ****************************************
     C     EHLIC#        IFEQ      *BLANKS
     C     PDFLIC        IFNE      *BLANKS
     C                   MOVE      PDFLIC        PHYS
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     'ZZ999'       PHYS
     C                   MOVE      '9999'        PHYS
     C                   ENDIF                                                  END NE *BLANKS
     C                   ELSE                                                   ELSE EQ *BLANKS
     C                   MOVE      EHLIC#        PHYS
     C                   ENDIF                                                  END EQ *BLANKS
     C*** BEG ADD *** 04/28/03 ****************************************
     C                   ENDIF                                                  EHFLIC NE BLANK
     C*** END ADD *** 04/28/03 ****************************************
     C     RECORD        WHENEQ    'H'
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL     EUFLIC        FLIC
     C                   MOVEL     'A'           REVIND
     C                   MOVE      *BLANKS       PHYS
     C                   ELSE
     C*** END ADD *** 04/28/03 ****************************************
     C     EULIC#        IFNE      *BLANKS
     C                   MOVE      EULIC#        PHYS
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     'ZZ999'       PHYS
     C                   MOVE      '9999'        PHYS
     C                   ENDIF                                                  END NE *BLANKS
     C*** BEG ADD *** 04/28/03 ****************************************
     C                   ENDIF                                                  EUFLIC NE BLANK
     C*** END ADD *** 04/28/03 ****************************************
     C     RECORD        WHENEQ    'D'
     C*** BEG ADD *** 04/28/03 ****************************************
     C     EDFLIC        IFNE      *BLANKS
     C                   MOVEL     EDFLIC        FLIC
     C                   MOVEL     'A'           REVIND
     C                   MOVE      *BLANKS       PHYS
     C                   ELSE
     C*** END ADD *** 04/28/03 ****************************************
     C     EDLIC#        IFNE      *BLANKS
     C                   MOVE      EDLIC#        PHYS
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     'ZZ999'       PHYS
     C                   MOVE      '9999'        PHYS
     C                   ENDIF                                                  END NE *BLANKS
     C*** BEG ADD *** 04/28/03 ****************************************
     C                   ENDIF                                                  EDFLIC NE BLANK
     C*** END ADD *** 04/28/03 ****************************************
     C                   ENDSL
     C*** END   CHG *** 01/08/03 **************************************
     C*
     C*** BEG DEL *** 01/16/03 ****************************************
     C* Per the state this is no longer a valid process because the are
     C* other in state prefixes now besides 59 and 65
     C*** BEG ADD *** 04/18/01 ****************************************
     C* Per the state "If provider license number  = ZZ9999999,
     C* 1st 2 digits of Fed Tax Id must not = '59' or '65'
     C**********
     C**********           MOVELEOFEIN    XXPREF  2
     C**********
     C********** XXPREF    IFNE '59'
     C********** XXPREF    ANDNE'65'
     C********** XXPREF    OREQ '  '
     C**********
     C**********           MOVEL'ZZ999'   PHYS
     C**********           MOVE '9999'    PHYS
     C**********
     C**********           ENDIF                           XXPREF NE
     C*** END ADD *** 04/18/01 ****************************************
     C*** END DEL *** 01/16/03 ****************************************
     C*** BEG ADD *** 06/07/02 ****************************************
     C*
     C* If client uses FL Lic# DB then edit against that
     C*
     C                   MOVEL(P)  'FL'          KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STBS01        XXDBFG            1
     C*
     C* Edit by Lic# DB if client flag is set to Y and override <> Y
     C*
     C     XXDBFG        IFEQ      'Y'
     C     EOFLG7        ANDNE     'Y'
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** RECORD    IFEQ 'P'
     C**********           MOVELEHLIC#    KKLIC#    P
     C**********
     C**********           ELSE
     C**********           MOVELEULIC#    KKLIC#    P
     C**********
     C**********           ENDIF                           RECORD IFEQ P
     C*** BEG DEL *** 04/28/03 ****************************************
     C**********           SELEC
     C********** RECORD    WHEQ 'P'
     C**********           MOVELEHLIC#    PHYS      P
     C********** RECORD    WHEQ 'H'
     C**********           MOVELEULIC#    PHYS      P
     C********** RECORD    WHEQ 'D'
     C**********           MOVELEDLIC#    PHYS      P
     C**********           ENDSL                           RECORD WHEQ
     C*** END DEL *** 04/28/03 ****************************************
     C*** END   CHG *** 01/08/03 **************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C                   SELECT
     C*
     C     RECORD        WHENEQ    'P'
     C*
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL     EHFLIC        KKLIC#
     C*
     C                   ELSE
     C                   MOVEL     EHLIC#        KKLIC#
     C*
     C                   ENDIF
     C*
     C     RECORD        WHENEQ    'H'
     C*
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL     EUFLIC        KKLIC#
     C*
     C                   ELSE
     C                   MOVEL     EULIC#        KKLIC#
     C*
     C                   ENDIF
     C*
     C     RECORD        WHENEQ    'D'
     C*
     C     EDFLIC        IFNE      *BLANKS
     C                   MOVEL     EDFLIC        KKLIC#
     C*
     C                   ELSE
     C                   MOVEL     EDLIC#        KKLIC#
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*** END ADD *** 04/28/03 ****************************************
     C*
     C     PLKY          CHAIN     PPL01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*** BEG CHG *** 11/19/03 ****************************************
     C******************** MOVEL'ZZ999'   PHYS
     C******************** MOVE '9999'    PHYS
     C                   MOVEL(P)  'ZZ999999'    FLIC
     C                   MOVE      '99999'       FLIC
     C                   MOVE      *BLANKS       PHYS
     C                   MOVEL(P)  'A'           REVIND
     C*** END CHG *** 11/19/03 ****************************************
     C*
     C                   ELSE
     C*
     C* Report license number entered by the user
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** RECORD    IFEQ 'P'
     C**********           MOVELEHLIC#    PHYS      P
     C*
     C**********           ELSE
     C**********           MOVELEULIC#    PHYS      P
     C*
     C**********           ENDIF                           RECORD IFEQ P
     C*** BEG DEL *** 04/28/03 ****************************************
     C**********
     C**********           SELEC
     C********** RECORD    WHEQ 'P'
     C**********           MOVELEHLIC#    PHYS      P
     C********** RECORD    WHEQ 'H'
     C**********           MOVELEULIC#    PHYS      P
     C********** RECORD    WHEQ 'D'
     C**********           MOVELEDLIC#    PHYS      P
     C**********           ENDSL                           RECORD WHEQ
     C*** END DEL *** 04/28/03 ****************************************
     C*** END   CHG *** 01/08/03 **************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C*
     C                   SELECT
     C*
     C     RECORD        WHENEQ    'P'
     C*
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EHFLIC        FLIC
     C                   MOVEL(P)  'A'           REVIND
     C*** BEG ADD *** 11/19/03 ****************************************
     C                   MOVE      *BLANKS       PHYS
     C*** END ADD *** 11/19/03 ****************************************
     C*
     C                   ELSE
     C                   MOVEL(P)  EHLIC#        PHYS
     C*
     C                   ENDIF
     C*
     C     RECORD        WHENEQ    'H'
     C*
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        FLIC
     C                   MOVEL(P)  'A'           REVIND
     C*** BEG ADD *** 11/19/03 ****************************************
     C                   MOVE      *BLANKS       PHYS
     C*** END ADD *** 11/19/03 ****************************************
     C*
     C                   ELSE
     C                   MOVEL(P)  EULIC#        PHYS
     C*
     C                   ENDIF
     C*
     C     RECORD        WHENEQ    'D'
     C*
     C     EDFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EDFLIC        FLIC
     C                   MOVEL(P)  'A'           REVIND
     C*** BEG ADD *** 11/19/03 ****************************************
     C                   MOVE      *BLANKS       PHYS
     C*** END ADD *** 11/19/03 ****************************************
     C*
     C                   ELSE
     C                   MOVEL(P)  EDLIC#        PHYS
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*** END ADD *** 04/28/03 ****************************************
     C*
     C                   ENDIF                                                  *IN99=*ON
     C*
     C                   ENDIF                                                  XXDBFG = Y
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 06/07/02 ****************************************
     C*** BEGIN ADD *** 01/08/03 **************************************
     C*
     C* PLACE OF TREATMENT
     C*
     C     RECORD        IFEQ      'D'
     C                   MOVEL(P)  EDPOTX        POT
     C                   ENDIF
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C* FEIN
     C     EOFEIN        IFNE      0
     C                   MOVE      EOFEIN        FEIN                           PROVFEIN
     C                   END
     C* LNAME
     C                   MOVE      *BLANKS       FLAG
     C                   MOVE      *BLANKS       STRING
     C                   MOVEL     CDLSN         STRING
     C                   MOVEL     CDLSN         PATT1
     C                   CALL      'F9616CFL'
     C                   PARM                    STRING           40
     C                   PARM                    PATT1            15
     C                   PARM                    FLAG              1
     C                   MOVEL(P)  STRING        LNAME
     C* FNAME
     C                   MOVE      *BLANKS       FLAG
     C                   MOVE      *BLANKS       STRING
     C                   MOVEL     CDFSN         STRING
     C                   MOVEL     CDFSN         PATT1
     C                   CALL      'F9616CFL'
     C                   PARM                    STRING           40
     C                   PARM                    PATT1            15
     C                   PARM                    FLAG              1
     C                   MOVEL(P)  STRING        FNAME
     C* MI
     C                   MOVE      CDMID         MI
     C* DATERC
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C* Condition what data goes in field based on RCN
     C*
     C     EORCN         IFEQ      *ZERO
     C*** END ADD *** 04/11/00 ****************************************
     C     EOCRDT        IFEQ      *ZERO
     C                   Z-ADD     EOIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EOIDT         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTERC   60
     C                   MOVEL     SRDTO         DTERC             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   ELSE
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             CALL 'SRCVTDAT'DTMDY
     C********             MOVELSRDTO     DTERC   60
     C                   MOVEL     SRDTO         DTERC             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C*** BEG ADD *** 04/11/00 ****************************************
     C                   ELSE                                                   EORCN=0
     C*
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C*** BEG ADD *** 05/22/01 ****************************************
     C* ONLY SELECT ESRRDT WHERE STATUS IS 'R'
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C                   LEAVE
     C                   ENDIF
     C*
     C     ESKY2         READPE    PES01                                  60
     C*
     C                   ENDDO                                                  *IN60 = *OFF
     C*** END ADD *** 05/22/01 ****************************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ESRRDT        IFEQ      *ZERO
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DTERC             8 0
     C*
     C                   ELSE                                                   ESRRDT=0
     C*
     C                   Z-ADD     ESRRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C                   Z-ADD     ESRRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DTERC             8 0
     C                   ENDIF                                                  ESRRDT=0
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C     EOCRDT        IFEQ      *ZERO
     C                   Z-ADD     EOIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C                   Z-ADD     EOIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DTERC             8 0
     C*
     C                   ELSE                                                   EOCRDT=0
     C*
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DTERC             8 0
     C*
     C                   ENDIF                                                  EOCRDT=0
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN=0
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C* DATEPD
     C***********EAWCPD    IFEQ *ZERO
     C     EOWCCK        IFEQ      *ZERO
     C***********          Z-ADDEATRDT    SRDTI
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATEPD            8 0
     C***********          Z-ADDEATRDT    SRDTI
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EOTRDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTEPD   60
     C                   MOVEL     SRDTO         DTEPD             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   ELSE
     C***********          Z-ADDEAWCPD    SRDTI
     C                   Z-ADD     EOWCCK        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATEPD            8 0
     C***********          Z-ADDEAWCPD    SRDTI
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EOWCCK        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTEPD   60
     C                   MOVEL     SRDTO         DTEPD             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   END
     C* TTLPD
     C***********          Z-ADDEAPAID    TTLPD  112
     C                   Z-ADD     EOPAID        TTLPD            11 2
     C*** BEGIN ADD *** 01/08/03 **************************************
     C                   Z-ADD     EOCHRG        TTLCH            11 2
     C*** END   ADD *** 01/08/03 **************************************
     C*** END CHG *** 09/21/99 ****************************************
     C* TYPE
     C                   MOVEL     EUBLTY        TYPE
     C* DSCGHR
     C                   MOVE      EUDCHR        DSCGHR                         DISCGHR
     C* ADMDTE
     C     EUADDT        IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          ADMDTE
     C                   MOVE      '1'           *IN73
     C                   ELSE
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         ADMDTE            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EUADDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     ADMDT   60
     C                   MOVEL     SRDTO         ADMDT             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   END
     C* COVFRM
     C     EUSCPF        IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          COVFRM
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         COVFRM            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EUSCPF        SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*******              MOVELSRDTO     CVFRM   60
     C                   MOVEL     SRDTO         CVFRM             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   END
     C* COVTO
     C     EUSCPT        IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          COVTO
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         COVTO             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EUSCPT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     CVTO    60
     C                   MOVEL     SRDTO         CVTO              8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* FACFIN
     C                   MOVE      EOFEIN        FACFIN                         FACFEIN
     C* FACZIP
     C                   MOVE      EOZIP         FACZIP
     C*
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE            7 0
     C                   Z-ADD     EOEOB#        XXEOB#            9 0
     C*
     C* Write the Header Record to the Work File and to report
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C**N90N89**           EXCPTHEADR
     C***90*89**           EXCPTHEADER
     C   90
     CAN 87              EXCEPT    HEADP
     C   91
     CAN 88              EXCEPT    HEADH
     C   92
     CAN 89              EXCEPT    HEADD
     C*** END   CHG *** 01/08/03 **************************************
     C     *INOA         IFEQ      '1'
     C     XXFND         ORNE      HLDFND
     C*** BEG ADD *** 04/02/00 ****************************************
     C     FNKEY         CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XXFNNM
     C*** END ADD *** 04/02/00 ****************************************
     C                   EXCEPT    HEAD
     C                   Z-ADD     XXFND         HLDFND            3 0          TO DETERMINE LVL BRK
     C                   END
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*89**           EXCPTHEAD1
     C**N90N89**           EXCPTHEAD1
     C     *IN90         IFEQ      *ON
     C     *IN87         ANDEQ     *ON
     C     *IN91         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     *IN92         OREQ      *ON
     C     *IN89         ANDEQ     *ON
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
     C*
     C********************  DETAIL SECTION  **********************************
     C* Get all Detail charge Records that are associated with the
     C* EOB Master record
     C*
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********DAKY      SETLLPDA01
     C***********DAKY1     READEPDA01                    63
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  63
     C     *IN63         DOWEQ     '0'
     C*
     C***********          Z-ADDDADSEQ    XXDSEQ
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C* Get all Detail Charge Records that are associated with the
     C* EOB Master record
     C*** BEG DEL *** 09/21/99 ****************************************
     C***********CAKY      CHAINPCA01                64
     C*** END DEL *** 09/21/99 ****************************************
     C*
     C* Get all Detail HCFA Records that are associated with the
     C* EOB Master record
     C     RECORD        IFEQ      'P'
     C     DHKY          CHAIN     PDH01                              60
     C                   ENDIF
     C*
     C* Get all Detail UB Records that are associated with the
     C* EOB Master record
     C     RECORD        IFEQ      'H'
     C     DUKY          CHAIN     PDU01                              60
     C                   ENDIF
     C*** BEGIN ADD *** 01/08/03 **************************************
     C*
     C* Get all Detail Dental Records that are associated with the
     C* EOB Master record
     C     RECORD        IFEQ      'D'
     C     DDKY          CHAIN     PDD01                              60
     C                   ENDIF
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C* Clear Detail record Fields
     C*
     C                   EXSR      CLRD
     C*
     C* Move information to Detail fields
     C*
     C* LOCATE
     C     RECORD        IFEQ      'P'
     C                   MOVE      DHPOS         LOCATE
     C                   ENDIF
     C     RECORD        IFEQ      'H'
     C                   MOVE      DUPOS         LOCATE
     C                   ENDIF
     C*
     C* DIAG1
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA1        XXDIA
     C                   ENDIF
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA1        XXDIA
     C                   ENDIF
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG1
     C*
     C* DIAG2
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA2        XXDIA
     C                   ENDIF
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA2        XXDIA
     C                   ENDIF
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG2
     C*
     C* DIAG3
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA3        XXDIA
     C                   ENDIF
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA3        XXDIA
     C                   ENDIF
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG3
     C*
     C* DIAG4
     C     RECORD        IFEQ      'P'
     C                   MOVE      EHDIA4        XXDIA
     C                   ENDIF
     C     RECORD        IFEQ      'H'
     C                   MOVE      EUDIA4        XXDIA
     C                   ENDIF
     C*** BEG DEL *** 04/30/02 ****************************************
     C******************** MOVE EODIA4    XXDIA
     C*** END DEL *** 04/30/02 ****************************************
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG4
     C*
     C     RECORD        IFEQ      'P'
     C* PROCCH
     C*** BEG ADD *** 06/12/99 ****************************************
     C     DHBPRC        IFNE      *BLANKS
     C*** BEG ADD *** 12/29/99 ****************************************
     C     ' '           SCAN      DHBPRC                                 60
     C   60              MOVEL     DHPRCD        PROCCH
     C  N60              MOVEL     DHBPRC        PROCCH
     C*** END ADD *** 12/29/99 ****************************************
     C*** BEG DEL *** 12/29/99 ****************************************
     C***************      MOVELDHBPRC    PROCCH
     C*** END DEL *** 12/29/99 ****************************************
     C*** BEG ADD *** 01/17/00 ****************************************
     C                   ELSE
     C                   MOVEL     DHPRCD        PROCCH
     C*** END ADD *** 01/17/00 ****************************************
     C*** BEG ADD *** 06/12/99 ****************************************
     C                   ENDIF
     C*** END ADD *** 06/12/99 ****************************************
     C*** BEG DEL *** 04/18/01 ****************************************
     C*********  DHMOD1    IFNE *BLANKS
     C*********            MOVE DHMOD1    PROCCH
     C**PROCPD*
     C*********            MOVE DHMOD1    PROCPD
     C*********            END                             END DHMOD1
     C*** END DEL *** 04/18/01 ****************************************
     C*
     C*** BEG ADD *** 04/18/01 ****************************************
     C* VALIDATE AND LOAD MODIFIER
     C*
     C                   MOVEL(P)  DHMOD1        XXKEY
     C*
     C*** BEG CHG *** 06/22/01 ****************************************
     C********** PMKY      CHAINPPM01                99
     C********** *IN99     IFEQ *OFF
     C     PMKY          CHAIN     PPM01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C*** END CHG *** 06/22/01 ****************************************
     C                   MOVE      DHMOD1        PROCCH
     C                   MOVE      DHMOD1        PROCPD
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** END ADD *** 04/18/01 ****************************************
     C*
     C                   END                                                    END EQ 'P'
     C*** BEGIN ADD *** 01/08/03 **************************************
     C     RECORD        IFEQ      'D'
     C* PROCCH
     C*
     C     DDBPRC        IFNE      *BLANKS
     C     ' '           SCAN      DDBPRC                                 60
     C   60              MOVEL     DDPRCD        PROCDN
     C  N60              MOVEL     DDBPRC        PROCDN
     C                   ELSE
     C                   MOVEL     DDPRCD        PROCDN
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/08/03 **************************************
     C*
     C*
      * Get EOMB code
     C                   MOVE      'A'           XXESRC
     C                   MOVE      *BLANKS       XXEOB             1
     C     DEKY          CHAIN     PDE01                              64
     C*
     C* If there was a hit then move DEEOB to XXEOB
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     DEEOB         XXEOB
     C                   ENDIF
     C*
     C     RECORD        IFEQ      'P'
     C                   MOVE      '0'           *IN60
     C                   Z-ADD     1             X                 2 0
     C     *IN60         DOWEQ     '0'
     C     X             IFEQ      18
     C                   MOVE      '1'           *IN60
     C                   END
     C                   MOVE      EOMB(X)       EOBMED
     C                   ADD       1             X
     C     XXEOB         IFEQ      EOB
     C                   MOVEL     MEDPD         PROCPD
     C                   MOVE      '1'           *IN60
     C                   END
     C                   ENDDO
     C   60              MOVEL     DHPRCD        PROCPD
     C                   END                                                    END EQ 'P'
     C*
     C* CHRG
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          MOVE DACHRG    CHRG   112
     C                   MOVE      CACHRG        CHRG             11 2
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C* SRVS
     C                   Z-ADD     *ZEROS        XXUNTS            2 0
     C     RECORD        IFEQ      'P'
     C*** For Anesthesia Codes:
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '01999'
     C*** If Service Date is after 06/01/91
     C     CAFDAT        IFGE      910601
     C*** Determine Payee Specialty
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** If "RNA" Specialty, use 15    else use 10   as divisor
     C     PDSPCD        IFEQ      'RNA'
     C     DHUNTS        DIV       15            XXUNTS
     C                   ELSE                                                   ELS EQ 'RNA'
     C     DHUNTS        DIV       10            XXUNTS
     C                   END                                                    END EQ 'RNA'
     C                   END                                                    END 0 *IN60
     C                   ELSE                                                   ELS GE 910601
     C*** If Service Date is before 06/01/91, then just use DHUNTS
     C                   Z-ADD     DHUNTS        XXUNTS
     C                   END                                                    END GE 910601
     C                   ELSE                                                   ELS EQ 00100/01999
     C*** If not an Anesthesia Code, then just use DHUNTS
     C                   Z-ADD     DHUNTS        XXUNTS
     C                   END                                                    END GE 910601
     C*
     C     XXUNTS        IFEQ      0
     C                   MOVE      '01'          SRVS
     C                   ELSE
     C                   MOVE      XXUNTS        SRVS
     C                   END
     C                   END                                                    END EQ 'P'
     C*
     C* PAID
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          MOVE DAPAID    PAID
     C                   MOVE      CAPAID        PAID
     C                   MOVE      CAPAID        PAID2            11 2
     C*** END CHG *** 09/21/99 ****************************************
     C* FDOS
     C     CAFDAT        IFEQ      0
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          FDOS              8 0
     C                   MOVE      '1'           *IN76
     C                   ELSE
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FDOS              8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CAFDAT        SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**********           MOVELSRDTO     FDOS2   60
     C                   MOVEL     SRDTO         FDOS2             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* TDOS
     C     CATDAT        IFEQ      0
     C*** BEG CHG *** 01/21/00 ****************************************
     C***********          Z-ADD0         TDOS    80
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TDOS              8 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TDOS2             8 0
     C***********          MOVE '1'       *IN77
     C*** END CHG *** 01/21/00 ****************************************
     C                   ELSE
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TDOS              8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CATDAT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     TDOS2   60
     C                   MOVEL     SRDTO         TDOS2             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* RECON
     C*** BEG DEL *** 09/21/00 ****************************************
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********DARCN#    IFGE 1
     C********** CARCN     IFGE 1
     C****END*CHG *** 09/21/99 ****************************************
     C**********           MOVE 'R'       RECON
     C**********           ELSE
     C**********           MOVE 'F'       RECON
     C**********           END
     C*** END DEL *** 09/21/00 ****************************************
     C* PLAN
     C*** BEGIN CHG *** 10/13/95 **************************************
     C     RECORD        IFEQ      'P'
     C                   MOVEL     DHPPLN        PLAN
     C                   ENDIF                                                  RECORD=P
     C     RECORD        IFEQ      'H'
     C                   MOVEL     '00'          PLAN
     C                   ENDIF                                                  RECORD=H
     C*** END   CHG *** 10/13/95 **************************************
     C*** BEG ADD *** 04/23/02 ****************************************
     C*
     C* USE BILLED CODE FIELDS IF THEY ARE POPULATED
     C*
     C     DUBPRC        IFNE      *ZEROS
     C     DUBRAT        ORNE      *BLANKS
     C                   MOVE      DUBPRC        REVCDE
     C                   MOVE      DUBRAT        HCPCS
     C*
     C                   ELSE
     C*
     C* USE PAID CODE FIELDS
     C*
     C                   MOVE      DURVCD        REVCDE                         REVCODE
     C                   MOVE      DURATE        HCPCS
     C*
     C                   END                                                    DUBPRC NE 0
     C*
     C* DEFAULT HCPCS TO ZEROS IF BLANK
     C*
     C     HCPCS         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        HCPCS
     C                   END                                                    HCPCS =BLANK
     C*** END ADD *** 04/23/02 ****************************************
     C*** BEG DEL *** 04/23/02 ****************************************
     C* REVCDE
     C**********
     C**********           MOVE DURVCD    REVCDE           REVCODE
     C**HCPCS***
     C********** DURATE    IFNE *BLANKS
     C**********           MOVE DURATE    HCPCS
     C**********           ELSE
     C**********           MOVE *ZEROS    HCPCS
     C**********           END
     C*** END DEL *** 04/23/02 ****************************************
     C* UNITS
     C     RECORD        IFEQ      'P'
     C                   Z-ADD     DHUNTS        XXUNIT            7 0
     C                   MOVE      XXUNTS        UNITS
     C                   ENDIF                                                  RECORD=P
     C     RECORD        IFEQ      'H'
     C                   Z-ADD     DUUNTS        XXUNIT
     C                   MOVE      XXUNIT        UNITS
     C                   ENDIF                                                  RECORD=H
     C*
     C* PC
     C                   MOVE      EUPCM         PC
     C*
     C* PRINPR
     C*
     C*** BEG ADD *** 04/30/02 ****************************************
     C                   SELECT
     C*
     C* USE ICD-9 IN PROCEDURE CODES
     C*
     C     EUPCM         WHENEQ    '9'
     C*** END ADD *** 04/30/02 ****************************************
     C*
     C* PRINCIPAL
     C*
     C*** BEG CHG *** 04/08/02 ****************************************
     C******************** MOVE EUPRC1    XXDIA
     C                   MOVEL(P)  EUPRC1        XXDIA
     C*** END CHG *** 04/08/02 ****************************************
     C  N50DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         PRINPR                         PRINPROC
     C   60              MOVE      *ZEROS        PRINPR
     C                   MOVE      '0'           *IN50
     C*
     C* OTHPR2
     C*** BEG CHG *** 04/08/02 ****************************************
     C******************** MOVE EUPRC2    XXDIA
     C                   MOVEL(P)  EUPRC2        XXDIA
     C*** END CHG *** 04/08/02 ****************************************
     C  N50DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         OTHPR2                         OTHPR2
     C   60              MOVE      *ZEROS        OTHPR2
     C                   MOVE      '0'           *IN50
     C*** BEG ADD *** 04/30/02 ****************************************
     C*
     C* USE CPT/HCPCS IN PROCEDURE CODES
     C*
     C     EUPCM         WHENEQ    '4'
     C     EUPCM         OREQ      '5'
     C*
     C* PRINCIPAL
     C*
     C                   MOVEL(P)  EUPRC1        PRINPR
     C                   MOVE      '0'           *IN50
     C*
     C* OTHPR2
     C*
     C                   MOVEL(P)  EUPRC2        OTHPR2
     C                   MOVE      '0'           *IN50
     C*
     C* LEAVE PROCEDURE CODES BLANK
     C*
     C     EUPCM         WHENEQ    '0'
     C*
     C* PRINCIPAL
     C*
     C                   MOVEL(P)  *ZEROS        PRINPR
     C*
     C* OTHPR2
     C*
     C                   MOVEL(P)  *ZEROS        OTHPR2
     C                   MOVE      '0'           *IN50
     C*
     C                   ENDSL
     C*** END ADD *** 04/30/02 ****************************************
     C*
     C* CHARGE
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          MOVE DACHRG    CHARGE           CHRG/REV
     C                   MOVE      CACHRG        CHARGE                         CHRG/REV
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C* Write Detail record to Work File and report
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*89**           EXCPTDTL
     C**N90N89**           EXCPTDETAIL
     C   87
     CAN 90              EXCEPT    DETP
     C   88
     CAN 91              EXCEPT    DETH
     C   89
     CAN 92              EXCEPT    DETD
     C*** END   CHG *** 01/08/03 **************************************
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*89**           EXCPTHEAD1
     C**N90N89**           EXCPTHEAD1
     C     *IN90         IFEQ      *ON
     C     *IN87         ANDEQ     *ON
     C     *IN91         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     *IN92         OREQ      *ON
     C     *IN89         ANDEQ     *ON
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
     C                   MOVE      '0'           *INOA
     C                   END
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*89**           EXCPTDETL
     C**N90N89**           EXCPTDETL
     C     *IN90         IFEQ      *ON
     C     *IN87         ANDEQ     *ON
     C     *IN91         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     *IN92         OREQ      *ON
     C     *IN89         ANDEQ     *ON
     C                   EXCEPT    DETL
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
     C*
     C* Continue to read  detail charge file
     C*
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********DAKY      READEPDA01                    63
     C     CAKY          READE     PCA01                                  63
     C*** END CHG *** 09/21/99 ****************************************
     C                   END                                                    *in63 end
     C*
     C********************  TOTAL  SECTION  **********************************
     C* Clear TOTAL record Fields
     C*
     C                   EXSR      CLRT
     C* TOTPD
     C*** BEG CHG *** 09/21/99 ****************************************
     C***********          MOVE EAPAID    TOTPD
     C                   MOVE      EOPAID        TOTPD
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C* Write TOTAL record to Report
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C*** BEGIN CHG *** 01/08/03 **************************************
     C***90*89**           EXCPTHEAD1
     C**N90N89**           EXCPTHEAD1
     C     *IN90         IFEQ      *ON
     C     *IN87         ANDEQ     *ON
     C     *IN91         OREQ      *ON
     C     *IN88         ANDEQ     *ON
     C     *IN92         OREQ      *ON
     C     *IN89         ANDEQ     *ON
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
     C                   MOVE      '0'           *INOA
     C                   END
     C*** BEG ADD *** 08/24/99 ****************************************
     C                   ENDIF                                                  *IN59=OFF
     C*** END ADD *** 08/24/99 ****************************************
     C*** BEG ADD *** 06/21/00 ****************************************
     C     NODT          TAG
     C*** END ADD *** 06/21/00 ****************************************
     C*** END CHG *** 09/21/99 ****************************************
     C***********          READ PEA01                    62
     C                   READ      PEO01                                  62
     C*** END CHG *** 09/21/99 ****************************************
     C                   END                                                    *in62 end
     C*
     C*** BEG CHG *** 08/08/02 ****************************************
     C********** *IN99     IFNE '1'
     C     *IN66         IFNE      '1'
     C*** END CHG *** 08/08/02 ****************************************
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
     C* END OF PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C****************************************************************
     C* CLRH - Subroutine to Clear Header Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* HEADER RECORD:(PHYSICIAN & HOSPITAL)                          *
     C* CNTL     CLAIM CONTROL NUMBER - FORMAT CCCYJJJSSS             *
     C*          CCC  = SUBMTTER/CARRIER CODE                         *
     C*          YY   = LAST DIGIT OF THE YEAR OF SUBMISSION          *
     C*          JJJ  = JULIAN DATE REPRESENTING THE DAY OF SUBMISSION*
     C*          SSSSS= SEQUENCE NUMBER (0001-9999) DESIGNATING THE   *
     C*                 FIRST, SECOND, SUBSEQUENT DOCUMENTS ON THE    *
     C*                 TAPE                                          *
     C* RCFLG1   RECORD FLAG = 1                                      *
     C* FORMID   FORM ID = 09                                         *
     C* CARR     CARRIER INTERNAL AUDIT #                             *
     C* CARRD    CARRIER INTERNAL AUDIT # - DENTAL                    *
     C* CARFIN   CARRIER FEDERAL ID #                                 *
     C* CARZIP   CARRIER LOCATION ZIP CODE                            *
     C* CARZID   CARRIER LOCATION ZIP CODE - DENTAL                   *
     C* AGNT     SERVICING AGENT INTERNAL AUDIT #                     *
     C* AGTFIN   SERVICING AGENT FEDERAL ID #                         *
     C* AGTZIP   SERVICING AGENT ZIP CODE                             *
     C* SSN      CLAIMANT SOC SEC #                                   *
     C* DOA      DATE OF ACCIDENT (CCYYMMDD)                          *
     C* PHYS     PROVIDER FLORIDA LICENSE #                           *
     C* FEIN     PROVIDER FEIN                                        *
     C* LNAME    CLAIMANT'S LAST  NAME                                *
     C* FNAME    CLAIMANT'S FIRST NAME                                *
     C* MI       CLAIMANT'S MIDDLE INITIAL                            *
     C* DATERC   DATE CARRIER RECEIVED BILL FROM PROVIDER             *
     C* DATEPD   DATE CARRIER PAID PROVIDER                           *
     C* TTLPD    TOTAL PAID                                           *
     C* FLIC     PHYSICIAN LICENSE# 13A                               *
     C* REVIND   REVISION INDICATOR FOR NEW LICENSE NUMBER FIELD      *
     C*                                                               *
     C* HEADER RECORD:(HOSPITAL)                                      *
     C* FORMID   FORM ID = 90                                         *
     C* TYPE     TYPE OF BILL                                         *
     C* ADMDATE  ADMISSION DATE                                       *
     C* DISCGHR  DISCHARGE HOUR                                       *
     C* COVFROM  DATE STATEMENT COVERS FROM                           *
     C* COVTO    DATE STATEMENT COVERS TO                             *
     C* FACFEIN  FACILTIY'S FEDERAL ID NUMBER                         *
     C* FACZIP   FACILITY'S ZIP CODE                                  *
     C*                                                               *
     C* HEADER RECORD:(DENTAL)                                        *
     C* FORMID   FORM ID = 11                                         *
     C* POT      PLACE OF TREATMENT                                   *
     C****************************************************************
     C     CLRH          BEGSR
     C*
     C                   MOVE      '00000000'    *IN(70)
     C                   MOVE      *BLANKS       CNTL             13
     C                   MOVE      '1'           RCFLG1            1
     C   90              MOVE      '09'          FORMID            2
     C                   MOVE      '00000'       CARR              5
     C                   MOVEL     '00000'       CARFIN            9
     C                   MOVE      '0000'        CARFIN
     C                   MOVEL     '00000'       CARZIP            9
     C                   MOVE      '0000'        CARZIP
     C                   MOVE      '00000'       AGNT              5
     C                   MOVEL     '00000'       AGTFIN            9
     C                   MOVE      '0000'        AGTFIN
     C                   MOVEL     '00000'       AGTZIP            9
     C                   MOVE      '0000'        AGTZIP
     C                   MOVE      *BLANKS       SSN               9
     C                   MOVE      '00000000'    DOA               8 0
     C                   MOVEL     'ZZ99'        PHYS              9
     C                   MOVE      '99999'       PHYS
     C                   MOVEL     '00000'       FEIN              9
     C                   MOVE      '0000'        FEIN
     C                   MOVE      *BLANKS       LNAME            30
     C                   MOVE      *BLANKS       FNAME            15
     C                   MOVE      *BLANKS       MI                1
     C                   MOVE      '00000000'    DATERC
     C                   MOVE      '00000000'    DATEPD
     C                   MOVEL     '000000'      TTLPD
     C                   MOVE      '00000'       TTLPD
     C*** BEGIN CHG *** 01/08/03 **************************************
     C**N90*****           MOVE '90'      FORMID
     C   91              MOVE      '90'          FORMID
     C*** END   CHG *** 01/08/03 **************************************
     C                   MOVE      *BLANKS       TYPE              3
     C                   MOVE      '00000000'    ADMDTE            8 0
     C                   MOVE      '00'          DSCGHR            2
     C                   MOVE      '00000000'    COVFRM            8 0
     C                   MOVE      '00000000'    COVTO             8 0
     C                   MOVE      '00000'       FACFIN            9
     C                   MOVE      '0000'        FACFIN
     C                   MOVE      '00000'       FACZIP            9 0
     C                   MOVE      '0000'        FACZIP
     C*** BEGIN ADD *** 01/08/03 **************************************
     C                   MOVE      '0000'        CARRD             4
     C                   MOVEL     '00000'       CARZID            5
     C   92              MOVE      '11'          FORMID
     C                   MOVEL     *ALL'0'       TTLCH
     C                   MOVEL     *BLANKS       POT               2
     C                   MOVE      *BLANKS       FILL10           10
     C*** END   ADD *** 01/08/03 **************************************
     C*** BEG ADD *** 04/28/03 ****************************************
     C                   MOVE      *BLANKS       REVIND            2
     C                   MOVE      *BLANKS       FLIC             13
     C                   MOVE      *BLANKS       FIL255          255
     C                   MOVE      *BLANKS       FIL215          215
     C                   MOVE      *BLANKS       FILL22           22
     C*** END ADD *** 04/28/03 ****************************************
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRD - Subroutine to Clear Detail Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* DETAIL RECORD: (PHYSICIAN, HOSPITAL & DENTAL)                 *
     C* CNTL     CLAIM CONTROL NUMBER - DUPLICATE OF HEADER           *
     C* RCFLG2   RECORD FLAG = 2                                      *
     C* FORMID   FORM ID = 09                                         *
     C* LOCATE   PLACE OF SERVICE                                     *
     C* DIAG1    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG2    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG3    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG4    ICD-9-CM DIAGNOSTIC CODE                             *
     C* PROCCH   MEDICAL PROCEDURE CODE - AS BILLED                   *
     C* PROCDN   MEDICAL PROCEDURE CODE - AS BILLED - DENTAL          *
     C* PROCPA   MEDICAL PROCEDURE CODE - AS PAID                     *
     C* CHRG     PROVIDER CHARGE                                      *
     C* SRVS     NUMBER OF SERVICES (UNITS)                           *
     C* PAID     CARRIER PAYMENT                                      *
     C* FDOS     FROM DATE OF SERVICE                                 *
     C* TDOS     TO DATE OF SERVICE                                   *
     C* RECON    RECONSIDERATION FLAG                                 *
     C* PLAN     PAYMENT PLAN TYPE                                    *
     C* FILL67   BLANK OUT TO 170 POSITIONS                           *
     C*                                                               *
     C* DETAIL RECORD: (HOSPITAL)                                     *
     C* FORMID   FORM ID = 90                                         *
     C* PRINDIAG ICD-9-CM DIAGNOSTIC CODE (DIAG1 FROM PHYSICIAN)      *
     C* REVCODE  REVENUE CODE                                         *
     C* HCPCS    HCPCS PROCEDURE CODE                                 *
     C* UNITS    UNITS OF SERVICE                                     *
     C* PC       PROCEDURE CODING METHOD IN 9B/10B                    *
     C* PRINPROC PRINCIPLE PROCEDURE                                  *
     C* OTHERPROC OTHER PROCEDURE                                     *
     C* CHRG/REV CHARGE PER REVENUE CODE                              *
     C* FILL116  BLANK OUT TO 208 POSITIONS                           *
     C****************************************************************
     C     CLRD          BEGSR
     C*
     C                   MOVE      '2'           RCFLG2            1
     C   90              MOVE      '09'          FORMID            2
     C                   MOVE      *BLANKS       LOCATE            2
     C                   MOVE      *ZEROS        DIAG1             6
     C                   MOVE      *ZEROS        DIAG2             6
     C                   MOVE      *ZEROS        DIAG3             6
     C                   MOVE      *ZEROS        DIAG4             6
     C                   MOVE      '0000000'     PROCCH            7
     C                   MOVE      '0000000'     PROCPD            7
     C                   MOVEL     '000000'      CHRG
     C                   MOVE      '00000'       CHRG
     C                   MOVE      '000'         SRVS              3
     C                   MOVEL     '000000'      PAID             11
     C                   MOVE      '00000'       PAID
     C                   MOVE      '00000000'    FDOS
     C                   MOVE      '00000000'    TDOS
     C*** BEG DEL *** 09/21/00 ****************************************
     C******************** MOVE *BLANKS   RECON   1
     C*** END DEL *** 09/21/00 ****************************************
     C                   MOVE      *ZEROS        PLAN              5
     C                   MOVE      *BLANKS       FILL67           67
     C*** BEGIN CHG *** 01/08/03 **************************************
     C**N90*****           MOVE '90'      FORMID
     C   91              MOVE      '90'          FORMID
     C*** END   CHG *** 01/08/03 **************************************
     C                   MOVE      '0000'        REVCDE            4 0
     C                   MOVE      *ZEROS        HCPCS             5
     C                   MOVE      *ZEROS        UNITS             7
     C                   MOVE      *BLANKS       PC                1
     C                   MOVE      *BLANKS       PRINPR            7
     C                   MOVE      *BLANKS       OTHPR2            7
     C                   MOVE      '000000'      CHARGE           11 2
     C                   MOVE      '00000'       CHARGE
     C                   MOVE      *BLANKS       FIL116          116
     C*** BEGIN ADD *** 01/08/03 **************************************
     C   92              MOVE      '11'          FORMID
     C                   MOVE      '00000'       PROCDN            5
     C                   MOVE      *BLANKS       FILL89           89
     C*** BEGIN ADD *** 01/08/03 **************************************
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRT - Subroutine to Clear TOTAL  Record's Fields            *
     C****************************************************************
     H* FIELD DEFINITION: (PER MARCH 31, 1990 STATE DATA DICTIONARY)  *
     H* TOTAL  RECORD:                                                *
     H* CNTL     CLAIM CONTROL NUMBER - DUPLICATE OF HEADER           *
     H* RCFLG3   RECORD FLAG = 3                                      *
     H* FILL18   BLANK OUT                                            *
     H* TOTPD    TOTAL PAID ON EOMB (MUST EQUAL TOTAL OF PAID FIELDS) *
     H* FILL65   BLANK OUT TO 102 POSTIONS                            *
     C****************************************************************
     C*
     C     CLRT          BEGSR
     C*
     C                   MOVE      '2'           RCFLG3            1
     C                   MOVE      *BLANKS       FILL18           18
     C                   MOVE      '000000'      TOTPD            11
     C                   MOVE      '00000'       TOTPD
     C                   MOVE      *BLANKS       FILL65           65
     C*
     C                   ENDSR
     C****************************************************************
     O*** BEG ADD *** 06/12/99 ***************************************
     O*** BEG CHG *** 09/21/99 ****************************************
     O**********PEA01   E                REA01
     OPEO01     E            REO01
     O*** END CHG *** 09/21/99 ****************************************
     O*** END ADD *** 06/12/99 ***************************************
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********FATPPP  EADD             HEADER
     OFATPPP    EADD         HEADP
     O*** END   CHG *** 01/08/03 **************************************
     O                       CNTL                13
     O                       RCFLG1              14
     O                       FORMID              16
     O                       CARR                21
     O                       CARFIN              30
     O                       CARZIP              39
     O                       AGNT                44
     O                       AGTFIN              53
     O                       AGTZIP              62
     O                       SSN                 71
     O                       DOA                 79
     O                       PHYS                88
     O                       FEIN                97
     O                       LNAME              127
     O                       FNAME              142
     O                       MI                 143
     O                       DATERC             151
     O                       DATEPD             159
     O                       TTLPD              170
     O*** BEG ADD *** 04/28/03 ****************************************
     O                       REVIND             172
     O                       FLIC               185
     O                       FIL215             400
     O*** END ADD *** 04/28/03 ****************************************
     O*** BEGIN CHG *** 01/08/03 **************************************
     O********EADD             DETAIL
     O          EADD         DETP
     O*** END   CHG *** 01/08/03 **************************************
     O                       CNTL                13
     O                       RCFLG2              14
     O                       FORMID              16
     O                       LOCATE              18
     O                       DIAG1               24
     O                       DIAG2               30
     O                       DIAG3               36
     O                       DIAG4               42
     O                       PROCCH              49
     O                       PROCPD              56
     O                       CHRG                67
     O                       SRVS                70
     O                       PAID                81
     O                       FDOS                89
     O                       TDOS                97
     O                       RECON               98
     O                       PLAN               103
     O                       FILL67             170
     O*
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********FATPPH  EADD             HEADR
     OFATPPH    EADD         HEADH
     O*** END   CHG *** 01/08/03 **************************************
     O                       CNTL                13
     O                       RCFLG1              14
     O                       FORMID              16
     O                       CARR                21
     O                       CARFIN              30
     O                       CARZIP              39
     O                       AGNT                44
     O                       AGTFIN              53
     O                       AGTZIP              62
     O                       TYPE                65
     O                       SSN                 74
     O                       DOA                 82
     O                       PHYS                91
     O                       LNAME              121
     O                       FNAME              136
     O                       MI                 137
     O                       ADMDTE             145
     O                       DSCGHR             147
     O                       COVFRM             155
     O                       COVTO              163
     O                       FACFIN             172
     O                       FACZIP             181
     O                       DATERC             189
     O                       DATEPD             197
     O                       TTLPD              208
     O*** BEG ADD *** 04/28/03 ****************************************
     O                       REVIND             210
     O                       FLIC               223
     O                       FIL255             479
     O                       FILL22             500
     O*** END ADD *** 04/28/03 ****************************************
     O*** BEGIN CHG *** 01/08/03 **************************************
     O********EADD             DTL
     O          EADD         DETH
     O*** END   CHG *** 01/08/03 **************************************
     O                       CNTL                13
     O                       RCFLG2              14
     O                       FORMID              16
     O                       DIAG1               22
     O                       DIAG2               28
     O                       DIAG3               34
     O                       DIAG4               40
     O                       REVCDE              44
     O                       HCPCS               49
     O                       UNITS               56
     O                       PC                  57
     O                       PRINPR              64
     O                       OTHPR2              71
     O                       CHARGE              82
     O                       PLAN                87
     O                       FIL116             203
     O*** BEGIN ADD *** 01/08/03 **************************************
     OFATPPD    EADD         HEADD
     O                       CNTL                13
     O                       RCFLG1              14
     O                       FORMID              16
     O                       CARRD               20
     O                       CARZID              25
     O                       FNAME               40
     O                       MI                  41
     O                       LNAME               71
     O                       SSN                 80
     O                       FEIN                89
     O                       PHYS                98
     O                       POT                100
     O                       DOA                108
     O                       TTLCH              119
     O                       TTLPD              130
     O*** BEG ADD *** 04/28/03 ****************************************
     O                       REVIND             132
     O                       FLIC               145
     O                       FIL255             400
     O*** END ADD *** 04/28/03 ****************************************
     O*** BEG DEL *** 04/28/03 ****************************************
     O************************ FILL10   140
     O*** END DEL *** 04/28/03 ****************************************
     O          EADD         DETD
     O                       CNTL                13
     O                       RCFLG2              14
     O                       FORMID              16
     O                       FDOS                24
     O                       PROCDN              29
     O                       CHRG                40
     O                       PAID                51
     O                       FILL89             140
     O*** END   ADD *** 01/08/03 **************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
     O                       XXFROM              30 '  /  /    '
     O          E            ROUTE          1
     O                                           18 'TO DATE  :'
     O                       XXTO                30 '  /  /    '
     O****************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                   '(C) COPYRIGHT PROGRESSIV'
     O***                              +  0 'E DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                           88 'FROM DATE:'
     O                       XXFROM              98 '  /  /    '
     O                                          108 'TO DATE:'
     O                       XXTO               118 '  /  /    '
     O          E            HEAD           1
     O                       XXPROC             102
     O          E            HEAD           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                       XXSBNM              60
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                                           12 '-'
     O                       XXFNNM              51
     O                                           59 'CARR'
     O                       CARR                65
     O                                           74 'AGNT'
     O                       AGNT                80
     O          E            HEAD           1
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********       90                12 'CONTROL #'
     O**********       90                28 'SOC SEC #'
     O**********       90                41 'ACC DATE'
     O**********       90                56 'YR   CASE'
     O**********       90                69 'EOMB#'
     O**********       90                85 'PHYS LIC#'
     O**********       90                99 'PROV FEIN'
     O**********       90               116 'CLAIMANT NAME'
     O**********       90               164 'DATE REC'
     O**********       90               179 'DATE PAID'
     O**********       90               196 'TOTAL PAID'
     O**********      N90                13 'CONTROL #'
     O**********      N90                26 'BILL TYPE'
     O**********      N90                37 'SOC SEC #'
     O**********      N90                47 'ACC DATE'
     O**********      N90                58 'PHYS LIC#'
     O**********      N90                73 'CLAIMANT NAME'
     O**********      N90               124 'ADMISSION DATE'
     O**********      N90               135 'DISCHARGE'
     O**********      N90               146 'FROM DATE'
     O**********      N90               157 'TO DATE'
     O**********      N90               172 'FACILITY ID#'
     O**********      N90               187 'FACILITY ZIP'
     O               90                          12 'CONTROL #'
     O               90                          28 'SOC SEC #'
     O               90                          41 'ACC DATE'
     O               90                          56 'YR   CASE'
     O               90                          69 'EOMB#'
     O               90                          85 'PHYS LIC#'
     O               90                          99 'PROV FEIN'
     O               90                         116 'CLAIMANT NAME'
     O               90                         164 'DATE REC'
     O               90                         179 'DATE PAID'
     O               90                         196 'TOTAL PAID'
     O               91                          13 'CONTROL #'
     O*** BEG DEL *** 04/28/03 ****************************************
     O**************** 91                26 'BILL TYPE'
     O**************** 91                37 'SOC SEC #'
     O**************** 91                47 'ACC DATE'
     O**************** 91                58 'PHYS LIC#'
     O*** END DEL *** 04/28/03 ****************************************
     O*** BEG ADD *** 04/28/03 ****************************************
     O               91                          22 'BILL TYPE'
     O               91                          33 'SOC SEC #'
     O               91                          43 'ACC DATE'
     O               91                          54 'PHYS LIC#'
     O*** END ADD *** 04/28/03 ****************************************
     O               91                          73 'CLAIMANT NAME'
     O               91                         124 'ADMISSION DATE'
     O               91                         135 'DISCHARGE'
     O               91                         146 'FROM DATE'
     O               91                         157 'TO DATE'
     O               91                         172 'FACILITY ID#'
     O               91                         187 'FACILITY ZIP'
     O               92                          14 'CONTROL #'
     O               92                          24 'LAST NAME'
     O               92                          63 'FIRST NAME    MID'
     O               92                          73 'SOC SEC #'
     O               92                          84 'ACC DATE'
     O               92                          88 'YR'
     O               92                          96 'CASE'
     O               92                         106 'EOMB#'
     O               92                         109 'PT'
     O               92                         119 'PHYS LIC#'
     O*** BEG DEL *** 04/28/03 ****************************************
     O**************** 92               129 'PROV FEIN'
     O**************** 92               144 'TOTAL CHARGE'
     O**************** 92               160 'TOTAL PAID'
     O*** END DEL *** 04/28/03 ****************************************
     O*** BEG DEL *** 04/28/03 ****************************************
     O               92                         133 'PROV FEIN'
     O               92                         148 'TOTAL CHARGE'
     O               92                         164 'TOTAL PAID'
     O*** END DEL *** 04/28/03 ****************************************
     O*** END   CHG *** 01/08/03 **************************************
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********       90                24 'PLACE OF SERV'
     O**********       90                51 'ICD-9-CM DIAG CODES'
     O**********       90                72 'MED PROC-BILL'
     O**********       90                89 'MED PROC-PAID'
     O**********       90               103 'PROV CHRG'
     O**********       90               123 'NO OF SERVICES'
     O**********       90               138 'PAID AMOUNT'
     O**********       90               153 'FR DT SERV'
     O**********       90               167 'TO DT SERV'
     O**********       90               176 'RECON'
     O**********       90               188 'PAY PLAN'
     O**********      N90                31 'DATE REC'
     O**********      N90                43 'DATE PAID'
     O**********      N90                58 'TOTAL PAID'
     O**********      N90                81 'ICD-9-CM DIAGNOSTIC'
     O**********      N90                87 'CODES'
     O**********      N90                97 'REVENUE'
     O**********      N90               108 'PROC CODE'
     O**********      N90               116 'UNITS'
     O**********      N90               121 'PC'
     O**********      N90               132 'PRIN PROC'
     O**********      N90               144 'OTHER PROC'
     O               90                          24 'PLACE OF SERV'
     O               90                          51 'ICD-9-CM DIAG CODES'
     O               90                          72 'MED PROC-BILL'
     O               90                          89 'MED PROC-PAID'
     O               90                         103 'PROV CHRG'
     O               90                         123 'NO OF SERVICES'
     O               90                         138 'PAID AMOUNT'
     O               90                         153 'FR DT SERV'
     O               90                         167 'TO DT SERV'
     O               90                         176 'RECON'
     O               90                         188 'PAY PLAN'
     O               91                          31 'DATE REC'
     O               91                          43 'DATE PAID'
     O               91                          58 'TOTAL PAID'
     O               91                          81 'ICD-9-CM DIAGNOSTIC'
     O               91                          87 'CODES'
     O               91                          97 'REVENUE'
     O               91                         108 'PROC CODE'
     O               91                         116 'UNITS'
     O               91                         121 'PC'
     O               91                         132 'PRIN PROC'
     O               91                         144 'OTHER PROC'
     O               92                          31 'DATE OF SERVICE'
     O               92                          37 'CODE'
     O               92                          52 'CHARGE'
     O               92                          68 'PAID'
     O*** END   CHG *** 01/08/03 **************************************
     O          E            HEAD1          1
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********       90      CNTL      14
     O**********       90      SSN       28
     O**********       90      ACCDT     41 '  /  /    '
     O**********       90      XXFYR Z   49
     O**********       90                50 '-'
     O**********       90      XXCASE    57
     O**********       90      XXEOB#Z   71
     O**********       90      PHYS      85
     O**********       90      FEIN      99
     O**********       90      FNAME    118
     O**********       90      MI       120
     O**********       90      LNAME    151
     O**********       90      DTERC    165 '  /  /    '
     O**********       90      DTEPD    178 '  /  /    '
     O**********       90      TTLPD J  198
     O**********      N90      CNTL      15
     O**********      N90      TYPE      23
     O**********      N90      SSN       37
     O**********      N90N70   ACCDT     47 '  /  /    '
     O**********      N90      PHYS      58
     O**********      N90      FNAME     75
     O**********      N90      MI        77
     O**********      N90      LNAME    108
     O**********      N90N73   ADMDT    121 '  /  /    '
     O**********      N90      DSCGHR   131
     O**********      N90N74   CVFRM    146 '  /  /    '
     O**********      N90N75   CVTO     158 '  /  /    '
     O**********      N90      FACFIN   171
     O**********      N90      FACZIP   186 '     -    '
     O               90      CNTL                14
     O               90      SSN                 28
     O               90      ACCDT               41 '  /  /    '
     O               90      XXFYR         Z     49
     O               90                          50 '-'
     O               90      XXCASE              57
     O               90      XXEOB#        Z     71
     O*** BEG CHG *** 04/28/03 ****************************************
     O**************** 90      PHYS      85
     O               90      FLIC                89
     O*** END CHG *** 04/28/03 ****************************************
     O               90      FEIN                99
     O               90      FNAME              118
     O               90      MI                 120
     O               90      LNAME              151
     O               90      DTERC              165 '  /  /    '
     O               90      DTEPD              178 '  /  /    '
     O               90      TTLPD         J    198
     O               91      CNTL                15
     O*** BEG DEL *** 04/28/03 ****************************************
     O**************** 91      TYPE      23
     O**************** 91      SSN       37
     O**************** 91N70   ACCDT     47 '  /  /    '
     O**************** 91      PHYS      58
     O*** END DEL *** 04/28/03 ****************************************
     O*** BEG ADD *** 04/28/03 ****************************************
     O               91      TYPE                20
     O               91      SSN                 31
     O               91N70   ACCDT               42 '  /  /    '
     O               91      FLIC                56
     O*** END ADD *** 04/28/03 ****************************************
     O               91      FNAME               75
     O               91      MI                  77
     O               91      LNAME              108
     O               91N73   ADMDT              121 '  /  /    '
     O               91      DSCGHR             131
     O               91N74   CVFRM              146 '  /  /    '
     O               91N75   CVTO               158 '  /  /    '
     O               91      FACFIN             171
     O               91      FACZIP             186 '     -    '
     O               92      CNTL                14
     O               92      LNAME             +001
     O               92      FNAME             +001
     O               92      MI                +001
     O               92      SSN               +001
     O               92      ACCDT             +001 '  /  /    '
     O               92      XXFYR         Z   +001
     O               92                        +000 '-'
     O               92      XXCASE            +000
     O               92      XXEOB#        Z   +001
     O               92      POT               +001
     O*** BEG CHG *** 04/28/03 ****************************************
     O**************** 92      PHYS    +001
     O               92      FLIC              +001
     O*** END CHG *** 04/28/03 ****************************************
     O               92      FEIN              +001
     O               92      TTLCH         J   +001
     O               92      TTLPD         J   +001
     O*** END   CHG *** 01/08/03 **************************************
     O          E            DETL           1
     O*** BEGIN CHG *** 01/08/03 **************************************
     O**********       90      LOCATE    18
     O**********       90      DIAG1     34
     O**********       90      DIAG2     41
     O**********       90      DIAG3     48
     O**********       90      DIAG4     55
     O**********       90      PROCCH    69
     O**********       90      PROCPD    86
     O**********       90      CHRG  J  105
     O**********       90      SRVS     117
     O**********       90      PAID2 J  139
     O**********       90N76   FDOS2    152 '  /  /    '
     O**********       90      TDOS2    166 '  /  /    '
     O**********       90      RECON    174
     O**********       90      PLAN     186
     O**********      N90      DTERC     31 '  /  /    '
     O**********      N90      DTEPD     43 '  /  /    '
     O**********      N90      TTLPD J   59
     O**********      N90      DIAG1     67
     O**********      N90      DIAG2     74
     O**********      N90      DIAG3     81
     O**********      N90      DIAG4     88
     O**********      N90      REVCDE    95
     O**********      N90      HCPCS    106
     O**********      N90      UNITS    117
     O**********      N90      PC       121
     O**********      N90      PRINPR   132
     O**********      N90      OTHPR2   142
     O               90      LOCATE              18
     O               90      DIAG1               34
     O               90      DIAG2               41
     O               90      DIAG3               48
     O               90      DIAG4               55
     O               90      PROCCH              69
     O               90      PROCPD              86
     O               90      CHRG          J    105
     O               90      SRVS               117
     O               90      PAID2         J    139
     O               90N76   FDOS2              152 '  /  /    '
     O               90      TDOS2              166 '  /  /    '
     O               90      RECON              174
     O               90      PLAN               186
     O               91      DTERC               31 '  /  /    '
     O               91      DTEPD               43 '  /  /    '
     O               91      TTLPD         J     59
     O               91      DIAG1               67
     O               91      DIAG2               74
     O               91      DIAG3               81
     O               91      DIAG4               88
     O               91      REVCDE              95
     O               91      HCPCS              106
     O               91      UNITS              117
     O               91      PC                 121
     O               91      PRINPR             132
     O               91      OTHPR2             142
     O               92N76   FDOS2               31 '  /  /    '
     O               92      PROCDN            +001
     O               92      CHARGE        J   +001
     O               92      PAID2         J   +001
     O*** END   CHG *** 01/08/03 **************************************
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
**  TABLE EOMB
22390010
22490020
22590040
22690060
22790620
23597530
24072240
24172255
24272265
24372270
24472285
24572295
24673040
24773085
24873115
24973525
25073580
25173615
