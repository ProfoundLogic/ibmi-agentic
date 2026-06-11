     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H****************************************************************
     H/TITLE W0122R - SINGLE/MULTIPLE ADJUSTMENT ENTRY
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL ALLOW THE USER TO ENTER A SINGLE ADJUST-  *
     H*   MENT OR OFFSET AN ADJUSTMENT INTO 4 SEPARATE TRANSACTIONS.  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:15 (ORIGINAL)               C:33 (V3 REWRITE)   *
     H* CREATED: 04/22/92  PROJECT: WC133         09/17/97    WC659   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 09/23/11 C:180 SUPPORT 78040
     H*                Fix incorrect reserve update.
     H*
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 05/04/09 C:062  Project: WC1807                               *
     H*                 IGNORE E TYPE WORK HISTORY ENTRIES            *
     H*                                                               *
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Handle new edit for negative total paid on    *
     H*                 the reserve. Change in DSPF only              *
     H*                                                               *
     H* 08/09/07 C:191 PROJECT: WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 02/23/07 C:185 Project: 114WMF                                *
     H*                Back To Base:114WBY Dated 06/11/01 & 05/29/01. *
     H*                                                               *
     H* 01/25/07 C:183 Project: 114WLY
     H*                Added PARM ##RTCD to WA019R call.              *
     H*                                                               *
     H* 02/06/06 C:168 Project: 132WBU                                *
     H*                Changed Error message id in display file from  *
     H*                WC30041 -> WC30068                             *
     H*                                                               *
     H* 01/11/06 C:53  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to load the reserve history buckets *
     H*                according to the order in the claim file.      *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 08/12/03 C:53 Support: WC102/38307                            *
     H*               Write check detail on the adjustment rcd if the *
     H*               FEIN or MOD changes.                            *
     H*                                                               *
     H* 08/05/03 C:15 Support: WC128/38332                            *
     H*               Added code to retrieve the correct rsv bucket   *
     H*               prior to the Claim File being updated to prevent*
     H*               the wrong rsv/paid amounts from being updated.  *
     H*                                                               *
     H* 05/15/03 C:15 Project: WC577                                  *
     H*               Added code to load the View Check File seq#.    *
     H*                                                               *
     H* 10/10/02 C:62  Project: WC1078                                *
     H*                - Populate PAXFYR/PAXCAS                       *
     H*                                                               *
     H* 07/18/02 C:15  Support: WC065/33461                           *
     H*                Added code to clear the scr02 entry fields     *
     H*                before they are loaded.                        *
     H*                                                               *
     H* 06/26/02 C:15  Support: WC065/33192                           *
     H*                Added code to carry over certain fields when   *
     H*                an adjustment is made.                         *
     H*                                                               *
     H* 03/19/02 C:15  Project: WC577                                 *
     H*                Found issue while researching CMPREVBAL        *
     H*                discrepancies.  When a recovery was being      *
     H*                adjusted, the rsv hist record on the correcting*
     H*                adjustments for closed claims was written as a *
     H*                positive amt instead of a negative amt.        *
     H*                                                               *
     H* 11/27/01 C:15  Support: WC094/30017                           *
     H*                Changed pgm to retrieve the claim rsv index on *
     H*                the screens which update the claims.           *
     H*                                                               *
     H* 08/16/01 C:62  Project: WC089 28640                           *
     H*                MAKE SURE WDRPP RECORD IS WRITTEN ON SINGLE ADJ*
     H*                                                               *
     H* 05/16/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code when      *
     H*                editing for a valid bank acct and when the     *
     H*                bank acct search is processed.                 *
     H*                                                               *
     H* 09/07/00 C:15  Project: WC577                                 *
     H*               -Found a problem with the offsetting rsv history*
     H*                record while researching CMPRSVBAL report for  *
     H*                clt 65.  The pgm added the offsetting amount   *
     H*                instead of subtracting the amount.  I resolved *
     H*                this issue.                                    *
     H*               -While resolving the above issue, I found a     *
     H*                problem with the offsetting and correcting     *
     H*                adjustment reserve edits.  I also resolved     *
     H*                this issue.                                    *
     H*                                                               *
     H* 08/15/00 C:15  Support: WC065/23851                           *
     H*                Added call to WA013R to process reserve edits  *
     H*                when entering a single adjustment.             *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add Daily batch lockout process                *
     H*                                                               *
     H* 04/25/00 C:15  Support: WC025/22607                           *
     H*                Added code to use the provider DBA flag to     *
     H*                deteremine which field is loaded into RPPNAM.  *
     H*                                                               *
     H* 05/19/99 C:33 SEQ#: WC033/17369                               *
     H*              -WHEN FIELD EXIT IS PRESSED FROM ANY OF THE 'TO' *
     H*               DATES ON SCR04, THE RESERVE TYPE IS 'C' AND THE *
     H*               'FROM' AND 'TO' DATE HAVE CHANGED, LOAD THE     *
     H*               WEEKS AND DAYS (BUT ALLOW THEM TO BE CHANGED TO *
     H*               ZERO).                                          *
     H*              -CLEAR THE CLAIMANT NAME BEFORE DISPLAY SCR01.   *
     H*                                                               *
     H* 03/02/99 C:15 Support: WC048/16846                            *
     H*               Fixed the claim reserve edit.                   *
     H*                                                               *
     H* 10/23/98 C:112 Support: WC100 Seq #13041                      *
     H*                Modified Multiple Adjustment Entry (SCR04)     *
     H*                to bypass week/day calculations on exit of the *
     H*                screen. Calc call commented out.  Manual       *
     H*                entry now required.                            *
     H*                                                               *
     H* 09/01/98 C:90 PROJECT: WC659                                  *
     H*               ALLOW BYPASS OF BLANK DATE OF DISABILTY FIELD.  *
     H*               WILL DISPLAY MESSAGE, THEN PASS TO NEXT SCREEN. *
     H*                                                               *
     H* 07/16/98 C:33 PROJECT: WC833                                  *
     H*               ALLOW FOR F4=ADDITIONAL ADDRESS SELECTION ON    *
     H*               SCR02 FOR OTHER THAN COMPENSATION PAYMENTS      *
     H*                                                               *
     H* 06/26/98 C:15 Support: WC051/12393                            *
     H*               Changed the Check# and Acct# on SCR02 from      *
     H*               required to optional.                           *
     H*                                                               *
     H* 06/24/98 C:33 SEQ#: WC999/11839                               *
     H*               LOAD GROUP # PRIOR TO CHAIN FOR CLAIM.          *
     H*                                                               *
     H* 06/02/98 C:15 Support: WC033/                                 *
     H*               Fixed the condition in ASGNNM which determines  *
     H*               whether or not the entered payee matches the    *
     H*               claimant's SS#.                                 *
     H*                                                              *
     H*  04/27/98  C:15  Project:                                    *
     H*                  Per JJ, removed the edit on check# on SCR02.*
     H*                                                              *
     H*  04/23/98  C:33  SEQ#: WC082/11032                           *
     H*                  LOAD KEY FIELDS FOR VALIDATING CLAIMS PRIOR *
     H*                  TO F KEY USAGE.                             *
     H*                                                              *
     H*  09/18/92 LPK  PROJECT: WC176                                *
     H*                CHANGED EDIT ON ADJUSTMENT FROM AND TO DATES. *
     H*                                                              *
     H*  09/18/92 LPK  PROJECT: WC175                                *
     H*                CHANGED EDIT ON REMAINING RESERVES.           *
     H*                                                              *
     H*  06/02/93 C:11 PROJECT WC241                                 *
     H*                DON'T REQUIRE CHECK# ON SCR02 WHEN ENTERING A *
     H*                SINGLE ADJUSTMENT.                            *
     H*                                                              *
     H*  09/06/94 C:15 PROJECT WC361                                 *
     H*                ADDED CODE TO INITIALIZE PABUL1, PAEOB# AND   *
     H*                PAPGTP.                                       *
     H*                                                              *
     H*  03/01/95 C:24 PROJECT WC311                                 *
     H*                ALLOW ZERO FEIN # ON DEDUCTIBLE RECOVERIES.   *
     H*                                                              *
     H* 04/15/97  C:30  Project: WC607                               *
     H*                 Change PAYEE SRCH program from W0531R to     *
     H*                 W0030R on SCR02 (F5) and SCR04 (F5).         *
     H*                                                              *
     H* 05/13/97 C:62 PROJECT WC674                                  *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N *
     H*                                                              *
     H* 05/15/97 C:62 PROJECT WC640                                  *
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;         *
     H*               "ADJUSTER" TO CXADJ.                           *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H*   GLOBAL:                                                    *
     H*   04 *ON  - Claim # input to pgm                             *
     H*   09 *ON  - @@MODE is 'D'                                    *
     H*   13 *ON  - Reserve Type is 'C'                              *
     H*   21 *ON  - RI 'EXCESS' in Claim headings                    *
     H*   22 *ON  - RI 'CONTROVERT' in Claim headings                *
     H*   23 *ON  - RI 'DENIED' in Claim headings                    *
     H*   24 *ON  - RI 'GROUP' provider entered in Claim headings    *
     H*   25 *ON  - RI 'ADDITIONAL ADDRESS' in Claim headings        *
     H*   60      - General File Access                              *
     H*                                                              *
     H*   SCR01:                                                     *
     H*   62 *OFF - Claim record found                               *
     H*   71 <-> 94 - Screen Edit Indicators                         *
     H*                                                              *
     H*   SCR02:                                                     *
     H*   26 *ON  - F KEY INVALID FOR NON-COMP TRANSACTION           *
     H*                                                              *
     H*   SCR03:                                                     *
     H*   50 *ON  - INVALID TABLE VALUE                              *
     H*   51 *ON  - CODE MUST BE ADJUSTMENT TYPE                     *
     H*   53 *ON  - INVALID RESERVE TYPE FOR TRANSACTION CODE        *
     H*   54 *ON  - DESCRIPTION MUST NOT BE BLANK                    *
     H*                                                              *
     H*   SCR04:                                                     *
     H*   20 *ON  - INVALID TABLE VALUE                              *
     H*   26 *ON  - FIELD CANNOT BE BLANK                            *
     H*   27 *ON  - INVALID TABLE VALUE                              *
     H*   28 *ON  - INVALID TABLE VALUE                              *
     H*   29 *ON  - BR7 MUST BE A LOST TIME TYPE                     *
     H*   30 *ON  - INVALID DATE                                     *
     H*   31 *ON  - INVALID DATE                                     *
     H*   32 *ON  - INVALID DATE                                     *
     H*   33 *ON  - DATES MUST BE IN ASCENDING ORDER                 *
     H*   34 *ON  - INVALID NUMBER OF WEEKS                          *
     H*   35 *ON  - INVALID NUMBER OF DAYS                           *
     H*   36 *ON  - INVALID AMOUNT                                   *
     H*   37 *ON  - AMOUNT EXCEEDS REMAINING RESERVES                *
     H*   38 *ON  - INVALID RESERVE TYPE FOR ADJUSTMENT              *
     H*   39 *ON  - INVALID DATE                                     *
     H*   50 *ON  - CURSOR INVALID FOR F15                           *
     H*   51 *ON  - DISPLAY SCMSG4                                   *
     H*   52 *ON  - NUMBER OF WEEKS/DAYS NOT EQUAL TO OFFSETTING ADJ *
     H*   53 *ON  - TOTAL AMOUNT NOT EQUAL TO OFFSETTING ADJ         *
     H*   91 *ON  - CODE MUST BE ADJUSTMENT TYPE                     *
     H*   93 *ON  - RECORD NOT FOUND                                 *
     H*   94 *ON  - CLAIM IS CLOSED                                  *
     H*   95 *ON  - INVALID PAYEE                                    *
     H*                                                              *
     H*   41-47   - POSITION CURSOR                                  *
     H*   71 *ON  - FIRST CORRECTING ENTRY                           *
     H*   72 *ON  - SECOND CORRECTING ENTRY                          *
     H*   73 *ON  - THIRD CORRECTING ENTRY                           *
     H*   74 *ON  - FOURTH CORRECTING ENTRY                          *
     H*                                                              *
     H****************************************************************
     FW0122F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCML1    UF   E           K DISK
     F                                     INFDS(DBFDSC)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWDPAL     UF A E           K DISK
     FWDPAP     IF   E             DISK
     F                                     RENAME(PPA01:PPA03)
     F*** BEGIN ADD *** 08/16/01 **************************************
     F*
     F*  Request Payment Detail file      KEY = CO#/FND/FYR/CASE/CHK#/TRDT
     F*** BEG CHG *** 08/12/03 ****************************
     F*WDRPL***O   E           K        DISK
     FWDRPL     UF A E           K DISK
     F*** END CHG *** 08/12/03 ****************************
     F                                     RENAME(PRP01:PRP02)
     F*** BEG ADD *** 08/12/03 ****************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     F*** END ADD *** 08/12/03 ****************************
     F*** END   ADD *** 08/16/01 **************************************
     FWDF1L     IF   E           K DISK
     F*** Begin add *** 05/16/01 **************************************
     FWDF2L     IF   E           K DISK
     F*** End add ***** 05/16/01 **************************************
     FWARPL     IF   E           K DISK
     FWDACL     IF   E           K DISK
     FWDACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     FWMNRL     IF   E           K DISK
     F*** Begin chg *** 05/16/01 **************************************
     F******WMBIL4  IF  E           K        DISK
     FWMBIL6    IF   E           K DISK
     F*** End chg ***** 05/16/01 **************************************
     FWMCXL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWMRHP     O    E             DISK
     F*** BEGIN ADD *** 08/15/00 **************************************
     FWMWHL     IF   E           K DISK
     F*** END ADD ***** 08/15/00 **************************************
     F*** BEG ADD *** 10/20/03 **********************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 **********************************

     D**** BEGIN CHG **** 09/01/98 ************************************
     D******************* MSG     1   4 79
     D MSG             S             79    DIM(5) CTDATA PERRCD(1)
     D**** END   CHG **** 09/01/98 ************************************
     D I6              S              6    DIM(63)                              Indicator Values
     D SVF             S              8  0 DIM(4)
     D SVT             S              8  0 DIM(4)
     D RTW             S              1    DIM(4)
     D SVW             S              3  0 DIM(4)
     D SVD             S              1  0 DIM(4)
     D RCD             S              2    DIM(4)
     D RTP             S              1    DIM(4)
     D FK1             S             79    DIM(8) CTDATA PERRCD(1)              SCR01 function Key
     D FK2             S             79    DIM(4) CTDATA PERRCD(1)              SCR02 function Key
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D fk1#            S              3  0                                      Indicator Values
     D fk2#            S              3  0                                      Indicator Values
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D*** Begin add *** 03/23/04 **************************************
     D*** End add ***** 03/23/04 **************************************
     D*
     D* CLAIM EXAMINER HEADING
     D*
     D OPTNM1          C                   CONST('Single/Multiple Adju-
     D                                     stment Entry')
     D OPTNM2          C                   CONST('Single Adjustment En-
     D                                     try')
     D OPTNM3          C                   CONST('Multiple Adjustment -
     D                                     Entry')
     D AFLAG           C                   CONST('INCREASE RESERVE FOR-
     D                                      SINGLE ADJUSTMENT T-
     D                                     O CLOSED CLAIM')
     D SFLAG           C                   CONST('DECREASE RESERVE FOR-
     D                                      SINGLE ADJUSTMENT T-
     D                                     O CLOSED CLAIM')
     D INCADJ          C                   CONST('INCREASE RESERVE FOR-
     D                                      ADJUSTMENT TO CLOS-
     D                                     ED CLAIM')
     D DECADJ          C                   CONST('DECREASE RESERVE FOR-
     D                                      ADJUSTMENT TO CLOS-
     D                                     ED CLAIM')
     D XXADJ           C                   CONST('EXAMINER:')
     D*** BEG ADD *** 10/20/03 *******************************************  ****
     D NF              C                   CONST('NOT FOUND')
     D*** END ADD *** 10/20/03 *******************************************  ****
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 08/16/01 **************************************
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D*** END   ADD *** 08/16/01 **************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*  SCCS02 - Field Definitions for Claim Heading Line 2
     D*
     D                 DS
     D  SCCS02                 1     79
     D  S2SS1                 26     28
     D  S2SS2                 30     31
     D  S2SS3                 33     36
     D  S2NAME                39     60
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CD                     1      8
     D                                     DIM(4)
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D                 DS
     D  YR                     1     12  0
     D                                     DIM(4)
     D  SCYR01                 1      3  0
     D  SCYR02                 4      6  0
     D  SCYR03                 7      9  0
     D  SCYR04                10     12  0
     D                 DS
     D  CA                     1     28  0
     D                                     DIM(4)
     D  SCCA01                 1      7  0
     D  SCCA02                 8     14  0
     D  SCCA03                15     21  0
     D  SCCA04                22     28  0
     D                 DS
     D  FE                     1     36  0
     D                                     DIM(4)
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D  SCFE04                28     36  0
     D                 DS
     D  MO                     1     12  0
     D                                     DIM(4)
     D  SCMO01                 1      3  0
     D  SCMO02                 4      6  0
     D  SCMO03                 7      9  0
     D  SCMO04                10     12  0
     D                 DS
     D  DS                     1    120
     D                                     DIM(4)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D                 DS
     D  B2                     1      8
     D                                     DIM(4)
     D  SCB201                 1      2
     D  SCB202                 3      4
     D  SCB203                 5      6
     D  SCB204                 7      8
     D                 DS
     D  B7                     1      8
     D                                     DIM(4)
     D  SCB701                 1      2
     D  SCB702                 3      4
     D  SCB703                 5      6
     D  SCB704                 7      8
     D                 DS
     D  PO                     1     32  0
     D                                     DIM(4)
     D  SCPO01                 1      8  0
     D  SCPO02                 9     16  0
     D  SCPO03                17     24  0
     D  SCPO04                25     32  0
     D                 DS
     D  FR                     1     32  0
     D                                     DIM(4)
     D  SCFR01                 1      8  0
     D  SCFR02                 9     16  0
     D  SCFR03                17     24  0
     D  SCFR04                25     32  0
     D                 DS
     D  TO                     1     32  0
     D                                     DIM(4)
     D  SCTO01                 1     08  0
     D  SCTO02                 9     16  0
     D  SCTO03                17     24  0
     D  SCTO04                25     32  0
     D                 DS
     D  WK                     1     12  0
     D                                     DIM(4)
     D  SCWK01                 1      3  0
     D  SCWK02                 4      6  0
     D  SCWK03                 7      9  0
     D  SCWK04                10     12  0
     D                 DS
     D  DY                     1      4  0
     D                                     DIM(4)
     D  SCDY01                 1      1  0
     D  SCDY02                 2      2  0
     D  SCDY03                 3      3  0
     D  SCDY04                 4      4  0
     D                 DS
     D  AM                     1     36  2
     D                                     DIM(4)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D                 DS
     D  DR                     1      8
     D                                     DIM(4)
     D  FNMETR                 1      2
     D  FNCOTR                 3      4
     D  FNEXTR                 5      6
     D  FNLETR                 7      8
     D*
     D*** Begin add *** 05/16/01 **************************************
     D                 DS
     D*** BEGIN CHG *** 08/09/07 **************************************
     D*************                           1  50 DATA
     D  DATA                   1     75
     D*** END  CHG  *** 08/09/07 **************************************
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*** End add ***** 05/16/01 **************************************
     D*** Begin add *** 02/23/07 **************************************
     D  QXSEQ#                30     34  0
     D*** End add ***** 02/23/07 **************************************
     D*** BEGIN ADD *** 08/09/07 **************************************
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*** END ADD ***** 08/09/07 **************************************
     D*
     D*** Begin add *** 03/23/04 **************************************
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 03/23/04 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     FNKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AUXILIARY COMPENSATION PAYEE FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C     ACKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PAYMENT DETAIL FILE KEY LIST
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  FUND YEAR KEY LIST
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  BANK INFORMATION
     C*
     C     BIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL
     C*** Begin dlt *** 05/16/01 **************************************
     C******************** KFLD           SCACCT
     C******************** KFLD           XXFND
     C*** End dlt ***** 05/16/01 **************************************
     C*** Begin add *** 05/16/01 **************************************
     C                   KFLD                    XXFND
     C                   KFLD                    XXCKTP
     C                   KFLD                    SCACCT
     C*** End add ***** 05/16/01 **************************************
     C*
     C*  KEY LIST FOR THE REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*  Key list for the Work History file
     C*
     C     WHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    XXWKDT
     C*
     C     WHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END ADD ***** 08/15/00 **************************************
     C*** BEG ADD *** 08/12/03 ****************************
     C*
     C     R3KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXSEQ#
     C*
     C     RPDKY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C                   KFLD                    XXTRDT            7 0
     C*** END ADD *** 08/12/03 ****************************
     C*** BEG ADD *** 10/20/03 **********************************
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C*** END ADD *** 10/20/03 **********************************
     C*
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C     *LIKE         DEFINE    SCFRDT        SVFDT
     C     *LIKE         DEFINE    SCTODT        SVTDT
     C     *LIKE         DEFINE    SCFEIN        SVFEIN
     C     *LIKE         DEFINE    SCMOD         SVMOD
     C     *LIKE         DEFINE    SCWKS         SVWKS
     C     *LIKE         DEFINE    SCDYWK        SVDYWK
     C     *LIKE         DEFINE    SCFEIN        ZZSS#
     C     *LIKE         DEFINE    SCAMT         XXAMT
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    CMRR01        SVRR
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C     *LIKE         DEFINE    RPAMT         XXREM
     C     *LIKE         DEFINE    PASEQ#        XXSEQ
     C     *LIKE         DEFINE    PASEQ#        SCSEQ
     C     *LIKE         DEFINE    PATYPE        SVTYPE
     C     *LIKE         DEFINE    RTTYPE        SVRTTP
     C     *LIKE         DEFINE    PAPODT        XXRNDT
     C     *LIKE         DEFINE    PAPODT        XXDT
     C     *LIKE         DEFINE    PAPODT        XXTEST
     C     *LIKE         DEFINE    PAPODT        XXDAT1
     C     *LIKE         DEFINE    PAPODT        XXDAT2
     C     *LIKE         DEFINE    PATRCD        SVTRCD
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PASEQ#        KKSEQ
     C     *LIKE         DEFINE    PACKTP        SVACCD
     C     *LIKE         DEFINE    PAACCT        SVACCT
     C     *LIKE         DEFINE    PALTCD        SVLTCD
     C     *LIKE         DEFINE    PAFNAL        SVFNAL
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    ACSEQ#        XXSEQ#
     C     *LIKE         DEFINE    ELOUTS        SVOUTS
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    RHEXPL        XXEXPL
     C**** BEGIN ADD **** 09/01/98 ************************************
     C     *LIKE         DEFINE    SCTRCD        SVTRC
     C**** END   ADD **** 09/01/98 ************************************
     C*** BEGIN ADD *** 08/15/00 **************************************
     C     *LIKE         DEFINE    PAAMT         SVAMT
     C     *LIKE         DEFINE    PAFEIN        SVPFEI
     C     *LIKE         DEFINE    SCFRDT        SVFRDT
     C     *LIKE         DEFINE    SCTODT        SVTODT
     C     *LIKE         DEFINE    SCFRDT        XXFROM
     C     *LIKE         DEFINE    SCTODT        XXTO
     C     *LIKE         DEFINE    WHDATE        XXFRDT
     C     *LIKE         DEFINE    WHDATE        XXWKDT
     C     *LIKE         DEFINE    CMRTW         XXRTW
     C*** END ADD ***** 08/15/00 **************************************
     C*** Begin add *** 06/26/02 ***********************************************
     C     *LIKE         DEFINE    PAPGTP        SVPGTP
     C     *LIKE         DEFINE    PAEOB#        SVEOB#
     C     *LIKE         DEFINE    PAEOBS        SVEOBS
     C     *LIKE         DEFINE    PADPCD        SVDPCD
     C     *LIKE         DEFINE    PADPCU        SVDPCU
     C*** End add ***** 06/26/02 ***********************************************
     C*** BEG ADD *** 10/20/03 **********************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 10/20/03 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     A011PL        PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM      *BLANK        ##RC              2
     C                   PARM                    ##03              1
     C                   PARM                    ##05              1
     C                   PARM                    ##09              1
     C*
     C     A012PL        PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCTRCD
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    ##RC
     C                   PARM                    ##RECD            2            Reserve Code
     C                   PARM                    ##RETP            1            Reserve Type
     C                   PARM                    ##TYPE            1            Transaction Type
     C*
     C*** BEGIN ADD *** 08/15/00 **************************************
     C     A013PL        PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    ##EOB#
     C                   PARM                    ##SEQ#            5 0
     C                   PARM                    SCAMT
     C                   PARM                    ##RECD
     C                   PARM                    ##RETP
     C                   PARM                    ##RC
     C                   PARM                    SCAMT
     C                   PARM                    ##RCN             3 0
     C                   PARM                    SCTRCD
     C*** END ADD ***** 08/15/00 **************************************
     C*
     C     A014PL        PLIST
     C                   PARM                    XXCO#                          Company
     C                   PARM                    SCFND                          Group
     C                   PARM                    SCFYR                          Year
     C                   PARM                    SCCASE                         Claim
     C                   PARM      'N'           SCGRP             1            Group Type
     C                   PARM                    SCPODT                         Posting Date
     C                   PARM                    SCRCVD            8 0          Received Date
     C                   PARM                    SCFRDT                         From Date
     C                   PARM                    SCTODT                         To Date
     C                   PARM                    RTTYPE                         Reserve Type
     C                   PARM                    PCRECD                         Reserve Code
     C                   PARM                    SCBR2                          Payment Value 1
     C                   PARM                    SCBR7                          Payment Value 2
     C                   PARM      'PY'          SCSTS             2            Payment Status
     C                   PARM                    SCLTCD            2            Paid on Time
     C                   PARM                    SCDYWK                         Number of Days
     C                   PARM                    ##RC                           Return Code
     C                   PARM      *ZEROS        ##EOB#            9 0          EOMB#
     C                   PARM                    SCAMT                          Payment Amount
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C/COPY SINDIC01
     C*
     C***  Begin Add  ***  02/23/07  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  02/23/07  **********************************
     C     TOP           TAG
     C*
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   MOVE      'WC'          XXAPPL
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFYR         XXFYR
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     SCFYR         IFEQ      *ZERO
     C                   Z-ADD     XXFYR         SCFYR
     C                   ENDIF                                                  SCFYR=*ZERO
     C*
     C***  BEGIN ADD  ***  05/19/99  *********************************
     C                   CLEAR                   SCCSNM
     C***  END   ADD  ***  05/19/99  *********************************
     C*
     C*  IF CALLED WITH A CLAIM GET THE CLAIM INFO
     C*
     C                   MOVE      *OFF          *IN96
     C*
     C     Q1PRMS        IFGE      4
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *ON           *IN04
     C                   MOVE      *ON           *IN96
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C*
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
     C*
     C                   MOVEL     S2NAME        SCCSNM
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  Q1PRMS>=4
     C*
     C*  Assign 2 lines of Function Key legend for SCR01
     C*
     C     *IN04         IFEQ      *ON
     C                   Z-ADD     1             CK                3 0
     C                   ELSE
     C                   Z-ADD     5             CK
     C                   ENDIF
     C*
     C                   MOVE(P)   FK1(CK)       SCFK1A
     C                   ADD       1             CK
     C                   MOVE(P)   FK1(CK)       SCFK1B
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE
     C*
     C                   MOVE      *OFF          *IN53
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
     C                   MOVEL     CXADJ         SCADJ
     C                   ELSE
     C                   MOVEL     XXADJ         SCADJ
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C**** BEGIN ADD **** 08/09/07 *********************************************
     C                   MOVE      *BLANKS       XXDSQ#            5 0
     C**** END   ADD **** 08/09/07 *********************************************
      *
      /Free
       // Load the function keys for web display;
       Exsr LoadFKeys;
      /End-free
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVEL(P)  OPTNM1        SRDFLD
     C                   EXSR      SCRNNM
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR01
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL(P)  OPTNM2        SRDFLD
     C                   EXSR      SCRNNM
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR02
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL(P)  OPTNM3        SRDFLD
     C                   EXSR      SCRNNM
     C                   EXFMT     SCR03
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVEL(P)  OPTNM3        SRDFLD
     C                   EXSR      SCRNNM
     C                   EXFMT     SCR04
     C*
     C                   ENDSL
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*==============================================================
     C*   SCR01 - If F4, F7, F8, F9, F11, F15, or F16, then
     C*           use screen fields and attempt claim master access
     C*==============================================================
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     *INKG         IFEQ      *ON
     C     *INKH         OREQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKK         OREQ      *ON
     C     *INKP         OREQ      *ON
     C     *INKQ         OREQ      *ON
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C***  BEGIN ADD  ***  04/23/98  *********************************
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C***  END   ADD  ***  04/23/98  *********************************
     C*
     C     CMKY          CHAIN(N)  PCM01                              62
     C*
     C     *IN62         IFEQ      *ON
     C                   MOVEA     I6(6)         CH6                            Error #6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN62=*ON
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVEA     I6(6)         CH6                            Error #6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   ENDIF                                                  F4/7/8/9/11/15/16
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*==============================================================
     C*   SCR04 - If F15, then determine cursor position and
     C*           use screen fields to attempt claim master access
     C*==============================================================
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     *INKP         ANDEQ     *ON
     C*
     C     Q2CPOS        DIV       256           LIN               3 0
     C     LIN           SUB       7             I1
     C     I1            DIV       3             I1
     C                   MVR                     REM               3 0
     C*
     C     LIN           IFLE      10
     C     REM           OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN50
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     I1            ADD       70            Y
     C                   MOVEA     '0000'        *IN(71)
     C                   MOVE      *ON           *IN(Y)
     C*
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CA(I1)        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *OFF
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN93
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C                   MOVE      *ON           *IN47
     C                   ELSE
     C                   MOVE      *ON           *IN93
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  LIN<=10/REM=*ZERO
     C*
     C                   ENDIF                                                  Q2RECF=SCR04
     C*
     C*==============================================================
     C*   F3 = EXIT
     C*==============================================================
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*==============================================================
     C*  F4=ADDITIONAL NAME/ADDRESS SEARCH
     C*==============================================================
     C*
     C     *INKD         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C***  BEGIN DEL  ***  07/16/98  *********************************
     C********** ##RETP    IFEQ 'C'
     C***  END   DEL  ***  07/16/98  *********************************
     C*
     C     ACKY1         CHAIN     PAC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0573R'
     C                   PARM      '2'           XCALL             1
     C                   PARM      'Y'           XXPMTF            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRNC        XXRRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     XXRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD=B/XXSEQ#=*ZER
     C*
     C     XXRRN         IFNE      *ZEROS
     C     XXRRN         CHAIN     PAC02                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ACPNAM        SCPNAM
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACADR2        SCADR2
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACST          SCST
     C                   MOVEL     ACZIP         SCZP1
     C                   MOVE      ACZIP         SCZIP2
     C*** BEGIN ADD *** 08/09/07 *****************************************  ****
     C                   MOVE      ACSEQ#        XXDSQ#
     C*** END ADD *** 08/09/07 *******************************************  ****
     C*** Begin add *** 01/11/06 **************************************
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 **************************************
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     ACCTRY        IFNE      *BLANK
     C     ACCTRY        ANDNE     'USA'
     C                   MOVEL(P)  ACADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  ACCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  ACCTRY        SCCTRY
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVE      '1'           *IN75
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *ON           *IN76
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C***  BEGIN DEL  ***  07/16/98  *********************************
     C**********           ELSE
     C**********
     C**********           MOVE *ON       *IN26
     C**********
     C**********           ENDIF                           ##RETP=C
     C***  END   DEL  ***  07/16/98  *********************************
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKD=*ON
     C*
     C*==============================================================
     C*  F7 = Financial summary
     C*==============================================================
     C*
     C     *INKG         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   CALL      'WA009R'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      *BLANK        ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C*
     C* If F3=Exit is hit, Return to Menu.
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKG=*ON
     C*
     C*==============================================================
     C*  F8 = Accident info
     C*==============================================================
     C*
     C     *INKH         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   CALL      'W0070R08'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C*
     C* If F3=Exit is hit, Return to Menu.
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKH=*ON
     C*
     C*==============================================================
     C*  F9 = Management info
     C*==============================================================
     C*
     C     *INKI         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   CALL      'W0070R09'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C*
     C* If F3=Exit is hit, Return to Menu.
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKI=*ON
     C*
     C*==============================================================
     C*  F10 - SEARCH                          KJDO
     C*==============================================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   MOVEA     '00000000'    *IN(41)
     C                   MOVEA     '00000'       *IN(71)
     C*
     C                   SELECT
     C*__________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCCASE'
     C     HFLD          OREQ      'SCFYR'
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C     *IN04         IFEQ      *ON
     C                   MOVEA     I6(63)        CH6               6            Error #63
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN04=*ON
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'CA'          ##SORT            2
     C                   PARM      DBRRN         ##RRN1            9 0
     C                   PARM      SCFYR         ##YEAR            3 0
     C*
     C* If F3=Exit is hit, Return to Menu.
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C     ##RRN         IFGT      *ZERO
     C                   EXSR      ASGNCS
     C                   MOVE      *ON           *IN26
     C                   ELSE
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  ##RRN>*ZERO
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*__________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C     HFLD          OREQ      'SCFE01'
     C     HFLD          OREQ      'SCMO01'
     C     HFLD          OREQ      'SCFE02'
     C     HFLD          OREQ      'SCMO02'
     C     HFLD          OREQ      'SCFE03'
     C     HFLD          OREQ      'SCMO03'
     C     HFLD          OREQ      'SCFE04'
     C     HFLD          OREQ      'SCMO04'
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C     HFLD          WHENEQ    'SCFE01'
     C     HFLD          OREQ      'SCMO01'
     C                   MOVE      SCFE01        QQFEIN
     C                   MOVE      SCMO01        QQMOD
     C     HFLD          WHENEQ    'SCFE02'
     C     HFLD          OREQ      'SCMO02'
     C                   MOVE      SCFE02        QQFEIN
     C                   MOVE      SCMO02        QQMOD
     C     HFLD          WHENEQ    'SCFE03'
     C     HFLD          OREQ      'SCMO03'
     C                   MOVE      SCFE03        QQFEIN
     C                   MOVE      SCMO03        QQMOD
     C     HFLD          WHENEQ    'SCFE04'
     C     HFLD          OREQ      'SCMO04'
     C                   MOVE      SCFE04        QQFEIN
     C                   MOVE      SCMO04        QQMOD
     C                   ENDSL
     C*
     C                   MOVE      '2'           QQCALL
     C*
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM                     LR
     C*
     C                   SELECT
     C*
     C     QQRTCD        WHENEQ    '*'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   EXSR      ASGNNM
     C                   MOVEA     '1101'        *IN(82)                        SCCHK# (PC)
     C     HFLD          WHENEQ    'SCFE01'
     C     HFLD          OREQ      'SCMO01'
     C                   MOVE      QQFEIN        SCFE01
     C                   MOVE      QQMOD         SCMO01
     C                   SETON                                        7143
     C     HFLD          WHENEQ    'SCFE02'
     C     HFLD          OREQ      'SCMO02'
     C                   MOVE      QQFEIN        SCFE02
     C                   MOVE      QQMOD         SCMO02
     C                   SETON                                        7243
     C     HFLD          WHENEQ    'SCFE03'
     C     HFLD          OREQ      'SCMO03'
     C                   MOVE      QQFEIN        SCFE03
     C                   MOVE      QQMOD         SCMO03
     C                   SETON                                        7343
     C     HFLD          WHENEQ    'SCFE04'
     C     HFLD          OREQ      'SCMO04'
     C                   MOVE      QQFEIN        SCFE04
     C                   MOVE      QQMOD         SCMO04
     C                   SETON                                        7443
     C                   ENDSL
     C*
     C**
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFE01'
     C     HFLD          OREQ      'SCMO01'
     C                   SETON                                        7142
     C     HFLD          WHENEQ    'SCFE02'
     C     HFLD          OREQ      'SCMO02'
     C                   SETON                                        7242
     C     HFLD          WHENEQ    'SCFE03'
     C     HFLD          OREQ      'SCMO03'
     C                   SETON                                        7342
     C     HFLD          WHENEQ    'SCFE04'
     C     HFLD          OREQ      'SCMO04'
     C                   SETON                                        7442
     C                   ENDSL
     C*
     C                   ENDSL
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*__________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCTRCD'
     C     HFLD          OREQ      'SCOFCD'
     C     HFLD          OREQ      'SCCD01'
     C     HFLD          OREQ      'SCCD02'
     C     HFLD          OREQ      'SCCD03'
     C     HFLD          OREQ      'SCCD04'
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   CALL      'W2141R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       ##KEY
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C     ##KEY         IFNE      *BLANKS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCTRCD'
     C                   MOVEL(P)  ##KEY         SCTRCD
     C                   MOVE      *ON           *IN42
     C     HFLD          WHENEQ    'SCOFCD'
     C                   MOVEL(P)  ##KEY         SCOFCD
     C                   MOVEL(P)  ##KEY         XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C  N60              MOVEL(P)  PCDESC        SCOFDS
     C                   MOVE      *ON           *IN55
     C     HFLD          WHENEQ    'SCCD01'
     C                   MOVEL(P)  ##KEY         SCCD01
     C                   MOVEL(P)  ##KEY         XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C  N60              MOVEL(P)  PCDESC        SCDS01
     C                   SETON                                        41  71
     C     HFLD          WHENEQ    'SCCD02'
     C                   MOVEL(P)  ##KEY         SCCD02
     C                   MOVEL(P)  ##KEY         XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C  N60              MOVEL(P)  PCDESC        SCDS02
     C                   SETON                                        41  72
     C     HFLD          WHENEQ    'SCCD03'
     C                   MOVEL(P)  ##KEY         SCCD03
     C                   MOVEL(P)  ##KEY         XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C  N60              MOVEL(P)  PCDESC        SCDS03
     C                   SETON                                        41  73
     C     HFLD          WHENEQ    'SCCD04'
     C                   MOVEL(P)  ##KEY         SCCD04
     C                   MOVEL(P)  ##KEY         XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C  N60              MOVEL(P)  PCDESC        SCDS04
     C                   SETON                                        41  74
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCTRCD'
     C                   MOVE      *ON           *IN26
     C     HFLD          WHENEQ    'SCCD02'
     C                   SETON                                        47  72
     C     HFLD          WHENEQ    'SCCD03'
     C                   SETON                                        47  73
     C     HFLD          WHENEQ    'SCCD04'
     C                   SETON                                        47  74
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCKEY<>*BLANKS
     C*
     C                   WRITE     SCR99
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
     C*__________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCACCT'
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C*
     C*** Begin add *** 05/16/01 **************************************
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   Z-ADD     SCPODT        QXTRDT
     C                   Z-ADD     SCFRDT        QXFRDT
     C                   Z-ADD     SCTODT        QXTODT
     C                   MOVEL     SCTRCD        QXTRCD
     C                   MOVEL     PCRECD        QXRECD
     C                   MOVEL     SCBR2         QXPV1
     C                   MOVEL     SCBR7         QXPV2
     C*** Begin add *** 02/23/07 **************************************
     C                   MOVE      *ZEROS        QXSEQ#
     C*** End add ***** 02/23/07 **************************************
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SCFEIN        QXFEIN
     C                   MOVE      SCMOD         QXMOD
     C                   MOVE      XXDSQ#        QXDSQ#
     C*** END ADD ***** 08/09/07 **************************************
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE                         Mode
     C                   PARM                    ##CO#                          Company
     C                   PARM                    ##FND                          Group #
     C                   PARM                    ##FYR                          Year
     C                   PARM                    ##CASE                         Case
     C                   PARM      'WC'          XXAPPL                         Applic Code
     C*** BEGIN CHG *** 08/09/07 **************************************
     C*******              PARM DATA      ##DATA 50        Selection criteria
     C                   PARM      DATA          ##DATA           75            Selection criteria
     C*** END   CHG *** 08/09/07 ************************************
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD                         Return Code
     C*** End add ***** 05/16/01 **************************************
     C*
     C                   CALL      'W2420R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##ACCT           20
     C                   PARM      *BLANKS       ##EFDT            8
     C                   PARM                    XXAPPL
     C*** Begin chg *** 05/16/01 **************************************
     C*******************  PARM *BLANKS   ##ACCD 10
     C                   PARM      ##CKTP        ##ACCD           10
     C*** End chg ***** 05/16/01 **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C     ##ACCT        IFNE      *BLANKS
     C                   MOVEL     ##ACCT        SCACCT
     C*** Begin add *** 05/16/01 **************************************
     C                   MOVEL     ##ACCD        XXCKTP           10
     C*** End add ***** 05/16/01 **************************************
     C                   ELSE
     C                   MOVE      *ON           *IN38
     C                   ENDIF                                                  SCKEY<>*BLANKS
     C*
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCBR2'
     C     HFLD          OREQ      'SCB201'
     C     HFLD          OREQ      'SCB202'
     C     HFLD          OREQ      'SCB203'
     C     HFLD          OREQ      'SCB204'
     C*
     C*** BEG ADD *** 02/23/07 **************************************
     C                   MOVE      SCFND         ##FND
     C*** END ADD *** 02/23/07 **************************************
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*** BEG ADD *** 02/23/07 **************************************
     C                   PARM      *BLANKS       ##RECD
     C                   PARM                    ##FND
     C*** END ADD *** 02/23/07 *************************************
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C     ##KEY         IFNE      *BLANK
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBR2'
     C                   MOVEL     ##KEY         SCBR2
     C                   MOVE      *ON           *IN41
     C                   MOVEL(P)  SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C  N60              MOVEL     B2DESC        SCDBR2
     C*
     C     HFLD          WHENEQ    'SCB201'
     C                   MOVEL     ##KEY         SCB201
     C                   SETON                                        7145
     C*
     C     HFLD          WHENEQ    'SCB202'
     C                   MOVEL     ##KEY         SCB202
     C                   SETON                                        7245
     C*
     C     HFLD          WHENEQ    'SCB203'
     C                   MOVEL     ##KEY         SCB203
     C                   SETON                                        7345
     C*
     C     HFLD          WHENEQ    'SCB204'
     C                   MOVEL     ##KEY         SCB204
     C                   SETON                                        7445
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCBR2'
     C                   MOVE      *ON           *IN40
     C     HFLD          WHENEQ    'SCB201'
     C                   SETON                                        7144
     C     HFLD          WHENEQ    'SCB202'
     C                   SETON                                        7244
     C     HFLD          WHENEQ    'SCB203'
     C                   SETON                                        7344
     C     HFLD          WHENEQ    'SCB204'
     C                   SETON                                        7444
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##KEY<>*BLANK
     C*
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCBR7'
     C     HFLD          OREQ      'SCB701'
     C     HFLD          OREQ      'SCB702'
     C     HFLD          OREQ      'SCB703'
     C     HFLD          OREQ      'SCB704'
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C*** BEG ADD *** 02/23/07 **************************************
     C                   MOVE      SCFND         ##FND
     C*** END ADD *** 02/23/07 **************************************
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*** BEG ADD *** 02/23/07 **************************************
     C                   PARM                    ##FND
     C*** END ADD *** 02/23/07 *************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##KEY         IFNE      *BLANK
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBR7'
     C*
     C                   MOVEL(P)  ##KEY         SCBR7
     C                   MOVE      *ON           *IN51
     C                   MOVEL(P)  SCBR7         XXKEY
     C                   MOVE      *BLANK        SCDBR7
     C*
     C     TBKY          CHAIN     PB701                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     B7DESC        SCDBR7
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     HFLD          WHENEQ    'SCB701'
     C                   MOVEL     ##KEY         SCB701
     C                   SETON                                        7146
     C*
     C     HFLD          WHENEQ    'SCB702'
     C                   MOVEL     ##KEY         SCB702
     C                   SETON                                        7246
     C*
     C     HFLD          WHENEQ    'SCB703'
     C                   MOVEL     ##KEY         SCB703
     C                   SETON                                        7346
     C*
     C     HFLD          WHENEQ    'SCB704'
     C                   MOVEL     ##KEY         SCB704
     C                   SETON                                        7446
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCBR2'
     C                   MOVE      *ON           *IN41
     C     HFLD          WHENEQ    'SCB701'
     C                   SETON                                        7145
     C     HFLD          WHENEQ    'SCB702'
     C                   SETON                                        7245
     C     HFLD          WHENEQ    'SCB703'
     C                   SETON                                        7345
     C     HFLD          WHENEQ    'SCB704'
     C                   SETON                                        7445
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##KEY<>*BLANK
     C*
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCPODT'
     C                   MOVEA     '0001'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCAMT'
     C                   MOVEA     '0010'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVEA     '0011'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVEA     '0100'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCWKS'
     C                   MOVEA     '0101'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCDYWK'
     C                   MOVEA     '0110'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCRCVD'
     C                   MOVEA     '0111'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCTRDS'
     C                   MOVEA     '1000'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFNAL'
     C                   MOVEA     '1001'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCRMDS'
     C                   MOVEA     '1010'        *IN(82)
     C                   SETON                                        9047
     C                   GOTO      STRT
     C*________________________________________________________________
     C*
     C                   OTHER
     C*
     C*  No Search available for field in cursor selection
     C*
     C                   SETON                                        4990
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C*===============================================================
     C*  F11 = Benefit info
     C*===============================================================
     C*
     C     *INKK         IFEQ      *ON
     C*
     C                   MOVEL     Q2RECF        SVRECF
     C*
     C                   CALL      'W0070R11'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKK=*ON
     C*
     C*==============================================================
     C*  F12 = CANCEL
     C*==============================================================
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C     Q2RECF        OREQ      'SCR03'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*===============================================================
     C*  F13 = PROCESS CHANGE OF USER PARAMETERS
     C*===============================================================
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  *INKM=*ON
     C*
     C*===============================================================
     C*  F15 = Remaining Reserve
     C*===============================================================
     C*
     C     *INKP         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        SVRECF
     C*
     C                   CALL      'W0070R12'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVE(P)   SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKP=*ON
     C*
     C*==============================================================
     C*  F16 = Payment Transaction Search (subset USRPRF ID)
     C*==============================================================
     C*
     C     *INKQ         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        SVRECF
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C*
     C                   CALL      'W0581R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      *ZERO         ##FYR             3
     C                   PARM      *ZERO         ##CASE            7
     C                   PARM      *BLANK        ##SPS            10            Payment Status
     C                   PARM      *ZERO         ##SFDT            8
     C                   PARM      *ZERO         ##STDT            8
     C                   PARM      *BLANK        ##SPC            10            Payment Codes
     C                   PARM      *BLANK        ##STT             5            Transaction Type
     C                   PARM      *BLANK        ##SPV1           10            Payment Value 1
     C                   PARM      *BLANK        ##SRC            10            Reserve Codes
     C                   PARM      *ZERO         ##SFEI            9            Federal ID #
     C                   PARM      *ZERO         ##SCHK            7            Check #
     C                   PARM      *ZERO         ##SEOB            9            EOMB #
     C                   PARM      UPUSR         ##SIRQ           10            Initial Requestor
     C                   PARM      *BLANK        ##SGRP            1            Group vs. Non/Group
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKQ=*ON
     C*
     C*===============================================================
     C*  F18 = CALL TO DIARY DATE MAINTENANCE
     C*===============================================================
     C*
     C     *INKS         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCSE             7
     C                   MOVE      'C'           QQCD
     C*
     C                   CALL      'W0190R'
     C                   PARM      *ZEROS        QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      *BLANKS       QQCO#             3
     C                   PARM      *BLANKS       QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCSE             7
     C                   PARM      *BLANKS       QQACDT            6
     C                   PARM      'C'           QQCD              1
     C*
     C     QQRTCD        CABEQ     'A'           TERM                     LR
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR03
     C*
     C                   ENDIF                                                  *INKS=*ON
     C*
     C*===============================================================
     C*   F21 - Change Provider Status (Group vs. Non-Group)
     C*===============================================================
     C*
     C     *INKV         IFEQ      *ON
     C*
     C     *IN24         IFEQ      *ON
     C*
     C                   SETOFF                                       24
     C*
     C                   ELSE
     C*
     C*  IF THE PAYEE EXISTS IN THE GROUP ID FILE, DISPLAY THE GROUP
     C*  TYPE FLAG.
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C*
     C                   EXSR      ASGNGP
     C*
     C                   ENDIF                                                  *IN24=*ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKV=*ON
     C*
     C*===============================================================
     C*  F24 = Change Function Key Display
     C*===============================================================
     C*
     C     *INKY         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C*
     C                   ADD       1             CK                3 0
     C*
     C     *IN04         IFEQ      *ON
     C*
     C     CK            IFGT      4
     C                   Z-ADD     1             CK
     C                   ENDIF                                                  CK>4
     C*
     C                   ELSE
     C*
     C     CK            IFGT      8
     C                   Z-ADD     5             CK
     C                   ENDIF                                                  CK>8
     C*
     C                   ENDIF                                                  *IN04=*ON
     C*
     C                   MOVEL(P)  FK1(CK)       SCFK1A
     C                   ADD       1             CK
     C                   MOVEL(P)  FK1(CK)       SCFK1B
     C*
     C**
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C                   ADD       1             CK2
     C*
     C     CK2           IFGT      3
     C                   Z-ADD     1             CK2
     C                   ENDIF                                                  CK2>3
     C*
     C                   MOVEL(P)  FK2(CK2)      SCFK2A
     C** ***************** ADD  1         CK2
     C** ***************** MOVELFK2,CK2   SCFK2B    P
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKY=*ON
     C*
     C*===============================================================
     C*  PROCESS HELP REQUESTS
     C*===============================================================
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*__________________________________________________________________
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*_________________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** BEG ADD *** 05/31/00 **************************************  *********
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C     ##RTCD        CABEQ     'F'           TERM
     C*** END ADD *** 05/31/00 **************************************  *********
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C                   Z-ADD     SCFND         XXFND
     C***  END   ADD  ***  06/24/98  *********************************
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C*
     C*  Edit SCR01 Fields
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVEA     I6(6)         CH6                            Error #6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   MOVE      'Y'           ##03                           SKIP ERROR #03
     C                   MOVE      'Y'           ##05                           SKIP ERROR #05
     C                   MOVE      'Y'           ##09                           SKIP ERROR #09
     C*
     C                   CALL      'WA011R'      A011PL
     C*
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA012R'      A012PL
     C                   ENDIF
     C*
     C     ##RC          IFNE      *BLANK
     C     ##RC          ANDNE     '15'                                         Skip FEIN Edit
     C     ##RC          ANDNE     '16'                                         Skip FEIN Edit
     C     ##RC          ANDNE     '17'                                         Skip FEIN Edit
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RC<>*BLANK
     C*
     C*  THE TYPE MUST BE AN ADJUSTMENT
     C*
     C     ##TYPE        CABNE     'A'           STRT                 2929
     C*
     C*  ENTRY TYPE MUST BE 'S' OR 'O'
     C*
     C     SCENTP        IFNE      'S'
     C     SCENTP        ANDNE     'O'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF                                                  SCENTP<>S/O
     C*
     C*  Assign *IN13 - *ON  = NOT Compensation
     C*                 *OFF = Compensation
     C*
     C                   MOVE      *OFF          *IN13
     C     ##RETP        IFNE      'C'
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C*
     C*  Assign *IN14 - *ON  = NOT Medical/Expense
     C*                 *OFF = Medical/Expense
     C*
     C                   MOVE      *ON           *IN14
     C     ##RETP        IFEQ      'M'
     C     ##RETP        OREQ      'E'
     C                   MOVE      *OFF          *IN14
     C                   ENDIF
     C*
     C                   MOVE      ##RECD        SCRECD
     C                   MOVE      ##RETP        SCRETP
     C*
     C*  Access Claim Master
     C*
     C     CMKY          CHAIN(N)  PCM01                              62
     C*
     C*  Date of Disability must be in Claim Master if Comp payment
     C*
     C**** BEGIN ADD **** 09/01/98 ************************************
     C     SVTRC         IFNE      SCTRCD
     C**** END   ADD **** 09/01/98 ************************************
     C     ##RETP        IFEQ      'C'
     C     CMDBDT        ANDEQ     *ZERO
     C**** BEGIN CHG **** 09/01/98 ************************************
     C******************** MOVEAI6,60     CH6     6        Error #60
     C******************** MOVEACH6       *IN,71
     C******************** SETON                     90
     C                   MOVEL(P)  MSG(5)        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG1
     C******************** MOVEL'SCR01'   Q2RECF    P
     C*
     C                   MOVE      SCTRCD        SVTRC
     C**** END   CHG **** 09/01/98 ************************************
     C                   GOTO      STRT
     C                   ENDIF                                                  ##RETP=C
     C**** BEGIN ADD **** 09/01/98 ************************************
     C                   ENDIF                                                  SVTRCD NE SCTRC
     C                   CLEAR                   SCMSG1
     C**** END   ADD **** 09/01/98 ************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   CLEAR                   SCMSG1
     C                   CLEAR                   SCMSG3
     C                   CLEAR                   SCMSG4
     C*
     C     ##RETP        IFEQ      'C'
     C                   MOVEA     '1101'        *IN(82)
     C                   ENDIF                                                  ##RETP=C
     C*
     C*  Initialize SVFDT(x) & SVTDT(x) for SCR02/03/04 Redisplay.
     C*
     C                   Z-ADD     *ZERO         SVFDT
     C                   Z-ADD     *ZERO         SVTDT
     C                   MOVEA     'NNNN'        RTW
     C                   CLEAR                   SVF
     C                   CLEAR                   SVT
     C*
     C*  Initialize SVFEIN & SVMOD for SCR02 Redisplay.
     C*
     C                   MOVE      *ZEROS        SVFEIN
     C                   MOVE      *ZEROS        SVMOD
     C*
     C*  SAVE THE TRANSACTION CODE TYPE FOR LATER USE
     C*
     C                   MOVE      ##TYPE        SVTRTP            1
     C*
     C*  Initialize Indicators 21 thru 25 - Screen Heading Line 3
     C*
     C                   MOVEA     '00000'       *IN(21)
     C*
     C*  Access Claim record occurred earlier - *IN62 is file status
     C*
     C     *IN62         IFEQ      *OFF
     C*
     C*  Assign Claim Heading
     C*
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
     C*
     C*  Execute Re-Insurance logic for Claim
     C*
     C                   MOVEL     S2NAME        SCCSNM
     C*
     C                   CALL      'WA010R'
     C                   PARM      DBRRNC        ##RRN                          FOR WMCMP
     C                   PARM      *ZERO         XXFNDR           13 2
     C                   PARM      *BLANK        XXEXRE            1
     C*
     C*  Excess
     C*
     C     XXEXRE        COMP      'Y'                                    21
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C                   MOVE      CMJRST        SCJRST
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVE      CMLTFL        SCLTFL
     C*
     C*  Controvert
     C*
     C     CMCVFL        COMP      'Y'                                    22
     C*
     C*  Denied
     C*
     C     CMDNY         COMP      'Y'                                    23
     C*
     C*  Additional Address
     C*
     C     CMKY          CHAIN     PAC01                              60
     C*
     C     *IN60         COMP      *OFF                                   25
     C*
     C                   ENDIF                                                  *IN62=*OFF
     C*
     C*  Assign Payment Transaction Description
     C*
     C                   MOVEL(P)  SCTRCD        XXKEY
     C*
     C     TBKY          CHAIN     PPC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PCDESC        SCTRDS
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  SAVE THE TRANSACTION CODE TYPE FOR LATER USE
     C*
     C                   MOVE      ##TYPE        SVTYPE
     C                   MOVE      PCTRTP        SVTRTP            1
     C*
     C                   Z-ADD     1             X                 3 0
     C     PCRECD        LOOKUP    RT(X)                                  60
     C                   Z-ADD     X             SVCMX             3 0
     C*
     C*  Assign Payment Reserve Type
     C*
     C                   MOVEL(P)  PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   Z-ADD     *ZERO         SCRCVD                         Date Received
     C                   MOVE      *BLANK        SCSTS                          Payment Status
     C                   MOVEL(P)  RTTYPE        SVRTTP
     C*
     C                   MOVEL(P)  SCTRCD        XXKEY
     C*
     C*  IF A SINGLE ADJUSTMENT WAS SELECTED, INITIALIZE FIELDS AND
     C*  GOTO SCREEN 2.  IF AN OFFSETTING ADJUSTMENT WAS SELECTED,
     C*  GOTO SEARCH TO SELECT A PAYMENT THEN GOTO SCREEN 3.
     C*
     C                   SELECT
     C*
     C     SCENTP        WHENEQ    'S'
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   CLEAR                   SCCHK#
     C                   CLEAR                   SCFRDT
     C                   CLEAR                   SCTODT
     C                   CLEAR                   SVFDT
     C                   CLEAR                   SVTDT
     C                   CLEAR                   SCFEIN
     C                   CLEAR                   SCMOD
     C                   CLEAR                   SVFEIN
     C                   CLEAR                   SVMOD
     C                   CLEAR                   SCBR2
     C                   CLEAR                   SCBR7
     C                   CLEAR                   SCAMT
     C                   CLEAR                   SCWKS
     C*** Begin add *** 05/16/01 **************************************
     C                   CLEAR                   SCACCT
     C                   CLEAR                   XXCKTP
     C*** End add ***** 05/16/01 **************************************
     C*** Begin add *** 07/18/02 **************************************
     C                   CLEAR                   SCPNAM
     C                   CLEAR                   SCADR1
     C                   CLEAR                   SCADR2
     C                   CLEAR                   SCCTY
     C                   CLEAR                   SCST
     C                   CLEAR                   SCZP1
     C                   CLEAR                   SCZIP2
     C                   CLEAR                   SCDBR2
     C                   CLEAR                   SCDBR7
     C*** End add ***** 07/18/02 **************************************
     C*
     C                   MOVEL     CMNCCI        SCNCCI
     C                   MOVEL     CMLTFL        SCLTFL
     C                   MOVEL(P)  PCDESC        SCTRDS
     C                   CLEAR                   SCDYWK
     C                   MOVE      'A'           SCADSB
     C*
     C*  LOAD CLAIMANT NAME AND ADDRESS FOR COMP TYPES
     C*
     C     RTTYPE        IFEQ      'C'
     C                   EXSR      ASGNNM
     C                   ENDIF                                                  RTTYPE=C
     C*
     C*  Assign 1 line of Function Key legend for SCR02
     C*
     C                   Z-ADD     1             CK2               3 0
     C                   MOVE(P)   FK2(CK2)      SCFK2A
     C** ***************** ADD  1         CK2
     C** ***************** MOVE FK2,CK2   SCFK2B    P
     C*
     C                   MOVEL(P)  'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C*
     C**
     C     SCENTP        WHENEQ    'O'
     C*
     C                   CALL      'W0620R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNC        ##RRN             9 0
     C                   PARM                    ##TYPE            1
     C                   PARM      SCTRCD        ##TRCD            2
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   WRITE     SCR99
     C*
     C                   Z-ADD     *ZEROS        SCSEQ
     C     ##RRN         CHAIN     WDPAP                              60
     C  N60              Z-ADD     PASEQ#        SCSEQ
     C*
     C     *INKL         IFEQ      *ON
     C     SCSEQ         OREQ      *ZERO
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=*ON/SCSEQ=*ZER
     C*
     C*  VERIFY THE RESERVE CODE OF THE SELECTED TRANSACTION IS VALID
     C*  FOR THE CLAIM.
     C*
     C     PARECD        LOOKUP    RT                                     60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  MSG(4)        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      79            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG1
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     PAFYR         XXFYR
     C                   Z-ADD     PACASE        XXCASE
     C                   Z-ADD     PASEQ#        XXSEQ
     C*
     C     PCRECD        IFNE      PARECD
     C                   MOVE      *BLANKS       SCOFCD
     C                   ELSE
     C                   MOVE      SCTRCD        SCOFCD
     C                   ENDIF                                                   PCRECD<>PARECD
     C*
     C                   MOVEL     PATRCD        SVTRCD
     C                   Z-ADD     PAFYR         SCOFYR
     C                   Z-ADD     PACASE        SCOFCA
     C                   Z-ADD     PACHK#        SCOFCH
     C                   Z-ADD     PAFEIN        SCOFFE
     C                   Z-ADD     PAMOD         SCOFMO
     C                   MOVE      'A'           SCOFTP
     C                   MOVE      *BLANKS       SCOFDS
     C                   MOVEL     PADESC        SCOFDS
     C                   MOVEL     PABR2         SCOFB2
     C                   MOVEL     PABR7         SCOFB7
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCOFPO
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOFFR
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOFTO
     C*
     C                   Z-ADD     PAWKS         SCOFWK
     C                   Z-ADD     PADAYS        SCOFDY
     C                   Z-SUB     PAAMT         SCOFAM
     C                   MOVE      PATYPE        SVTYPE
     C*
     C*  INITIALIZE FIELDS FOR SCREEN 4
     C*
     C                   CLEAR                   CD
     C                   CLEAR                   YR
     C                   CLEAR                   CA
     C                   CLEAR                   FE
     C                   CLEAR                   MO
     C                   CLEAR                   DS
     C                   CLEAR                   B2
     C                   CLEAR                   B7
     C                   CLEAR                   PO
     C                   CLEAR                   FR
     C                   CLEAR                   TO
     C                   CLEAR                   WK
     C                   CLEAR                   DY
     C                   CLEAR                   AM
     C*
     C                   MOVE      SCTRCD        CD(1)
     C                   Z-ADD     PAFYR         YR(1)
     C                   Z-ADD     PACASE        CA(1)
     C                   Z-ADD     PAFEIN        FE(1)
     C                   Z-ADD     PAMOD         MO(1)
     C                   MOVEL     PADESC        DS(1)
     C                   MOVE      PABR2         B2(1)
     C                   MOVE      PABR7         B7(1)
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         PO(1)
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FR(1)
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TO(1)
     C*
     C                   Z-ADD     PAWKS         WK(1)
     C                   Z-ADD     PADAYS        DY(1)
     C                   Z-ADD     PAAMT         AM(1)
     C*
     C                   MOVEA     FR            SVF
     C                   MOVEA     TO            SVT
     C                   MOVEA     WK            SVW
     C                   MOVEA     DY            SVD
     C*
     C                   MOVEL(P)  'SCR03 '      Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C* PROCESS SCREEN 2 - SINGLE ENTRY
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      'N'           REDSP2            1
     C*
     C*  Calculate Number of Days between "From" & "To" Service Dates
     C*  for Comp Payments Only
     C*
     C     RTTYPE        IFEQ      'C'
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   MOVE      SRERR         SRERR1            1
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   MOVE      SRERR         SRERR2            1
     C*
     C     SRERR1        IFNE      *ON
     C     SRERR2        ANDNE     *ON
     C*
     C     SVFDT         IFNE      SCFRDT
     C     SVTDT         ORNE      SCTODT
     C*
     C                   Z-ADD     SCFRDT        SVFDT
     C                   Z-ADD     SCTODT        SVTDT
     C                   Z-ADD     SCFRDT        QQBEG
     C                   Z-ADD     SCTODT        QQEND
     C                   EXSR      SRWKDY
     C*
     C     QQERR         IFEQ      *OFF
     C                   Z-ADD     XXWK          SCWKS
     C                   Z-ADD     XXDY          SCDYWK
     C                   Z-ADD     XXWK          SVWKS
     C                   Z-ADD     XXDY          SVDYWK
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF                                                  QQERR=*OFF
     C*
     C                   ENDIF                                                  SVFDT<>SCFRDT
     C*
     C                   ENDIF                                                  SRERR1<>*ON<>SRERR2
     C*
     C                   ENDIF                                                  RTTYPE=C
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C                   CALL      'WA012R'      A012PL
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      'Y'           ERROR2            1
     C                   ELSE
     C                   MOVE      'N'           ERROR2
     C                   ENDIF                                                  ##RC<>*BLANK
     C*
     C     ##RC          IFEQ      '15'
     C                   MOVE      'N'           ERROR2
     C                   ENDIF                                                  ##RC=15
     C*
     C     ##RC          IFEQ      '16'
     C     ##RC          OREQ      '17'
     C*
     C     SCTRCD        LOOKUP    DR                                     60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCFEIN        ORNE      *ZERO
     C                   MOVE      'Y'           ERROR2
     C                   ELSE
     C                   MOVE      'N'           ERROR2
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C*  If "Pay To" field is blank, call sub to assign Name/Address
     C*
     C     SCPNAM        IFEQ      *BLANK
     C     SVFEIN        ORNE      SCFEIN
     C     SVMOD         ORNE      SCMOD
     C                   Z-ADD     SCFEIN        SVFEIN
     C                   Z-ADD     SCMOD         SVMOD
     C                   EXSR      ASGNNM
     C                   WRITE     SCR02
     C                   MOVE      'Y'           REDSP2
     C                   ENDIF                                                  SCPNAM=*BLANK
     C*
     C                   ENDIF                                                  ##RC=16/17
     C*
     C     ERROR2        IFEQ      'Y'
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                  ERROR2=Y
     C*
     C*** BEGIN DLT *** 04/27/98 ***********************************************
     C*  BE SURE THE CHECK NUMBER IS ENTERED
     C**********
     C********** SCCHK#    IFLE *ZEROS
     C**********           MOVEAI6,60     CH6              Error #60
     C**********           MOVEACH6       *IN,71
     C**********           GOTO STRT
     C**********           ENDIF                           SCCHK#<=*ZERO
     C*** END DLT ***** 04/27/98 ***********************************************
     C*
     C*  GET A VALID PAID ON TIME CODE FOR THE API TO USE SO THAT A
     C*  FALSE ERROR WILL NOT OCCUR (ERROR #38)
     C*
     C     TBKY1         CHAIN     PLR01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     LRSTAT        ANDEQ     'I'
     C     TBKY1         READE     PLR01                                  60
     C                   ENDDO                                                  *IN60=*OFF/LRSTAT=I
     C*
     C                   MOVEL(P)  LRKEY         SCLTCD
     C*
     C*  LOAD A VALID RECIEVED DATE FOR THE API TO USE SO THAT A
     C*  FALSE ERROR WILL NOT OCCUR (ERROR #34)
     C*
     C                   Z-ADD     12312099      SCRCVD
     C*
     C                   CALL      'WA014R'      A014PL
     C*
     C*** Begin add *** 02/23/07 **************************************
     C     CLTID         IFEQ      '114'
     C     SCFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/23/07 **************************************
     C*
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** Begin add *** 02/23/07 **************************************
     C     SKIP          TAG
     C*** End add ***** 02/23/07 **************************************
     C*
     C*  CLEAR FALSE ERROR FIELDS
     C*
     C                   CLEAR                   SCLTCD
     C                   CLEAR                   SCRCVD
     C*
     C*  AMOUNT CANNOT BE > THE USER'S AUTHORITY LIMIT
     C*
     C     SCAMT         CABGT     UPPAL         STRT                 35
     C*
     C*  Payment Description is required
     C*
     C     SCTRDS        IFEQ      *BLANK
     C                   MOVEA     I6(62)        CH6                            Error #62
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTRDS=*BLANKD
     C*
     C*  BANK ACCOUNT MUST BE USED (AT ANY POINT IN TIME) FOR FUND
     C*
     C*** Begin add *** 05/16/01 **************************************
     C*  Call API to determine the Bank "Code".
     C*
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   Z-ADD     SCPODT        QXTRDT
     C                   Z-ADD     SCFRDT        QXFRDT
     C                   Z-ADD     SCTODT        QXTODT
     C                   MOVEL     SCTRCD        QXTRCD
     C                   MOVEL     PCRECD        QXRECD
     C                   MOVEL     SCBR2         QXPV1
     C                   MOVEL     SCBR7         QXPV2
     C*** Begin add *** 02/23/07 **************************************
     C                   MOVE      *ZEROS        QXSEQ#
     C*** End add ***** 02/23/07 **************************************
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SCFEIN        QXFEIN
     C                   MOVE      SCMOD         QXMOD
     C                   MOVE      XXDSQ#        QXDSQ#
     C*** END ADD ***** 08/09/07 **************************************
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE                         Mode
     C                   PARM                    ##CO#                          Company
     C                   PARM                    ##FND                          Group #
     C                   PARM                    ##FYR                          Year
     C                   PARM                    ##CASE                         Case
     C                   PARM      'WC'          XXAPPL                         Applic Code
     C*** BEGIN CHG *** 08/09/07 **************************************
     C*******              PARM DATA      ##DATA 50        Selection criteria
     C                   PARM      DATA          ##DATA           75            Selection criteria
     C*** END   CHG *** 08/09/07 ************************************
     C     XXCKTP        PARM                    ##CKTP                         Bank Acct Code
     C                   PARM                    ##RTCD                         Return Code
     C*** End add ***** 05/16/01 **************************************
     C*
     C*** BEGIN ADD *** 06/26/98 **************************************
     C     SCACCT        IFNE      *BLANKS
     C*** END ADD ***** 06/26/98 **************************************
     C*
     C     BIKY          CHAIN     PBI01                              60
     C     *IN60         CABEQ     *ON           STRT                     37
     C*
     C*** Begin add *** 05/16/01 **************************************
     C                   ELSE
     C*
     C*  Retrieve the check/voucher flag from the Group Aux file.
     C*
     C     FNKY          CHAIN     PF201                              60
      *** BEG ADD ***** 07/27/09 **************************************
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C   60              MOVE      *BLANK        ##CALL
     C  N60F2CTYP        IFNE      *BLANK
     C                   MOVE      F2CTYP        ##CALL
     C                   ELSE
     C                   MOVE      'C'           ##CALL
     C                   ENDIF
     C                   OTHER
      *** END ADD ***** 07/27/09 **************************************
     C   60              MOVE      *BLANK        ##CALL
     C  N60F2CTYP        IFEQ      'V'
     C                   MOVE      'V'           ##CALL
     C                   ELSE
     C                   MOVE      'C'           ##CALL
     C                   ENDIF
      *** BEG ADD ***** 07/27/09 **************************************
     C                   ENDSL
      *** END ADD ***** 07/27/09 **************************************
     C*
     C*
     C*  Call API to access "Current" Bank Account based on #2CKTP
     C*
     C                   MOVE      SCPODT        ##SRDT
     C*
     C                   CALL      'WA023R'
     C                   PARM                    ##CO#                          Company
     C                   PARM                    ##FND                          Group #
     C                   PARM                    XXAPPL                         Applic Code
     C                   PARM                    ##SRDT            8            MMDDCCYY
     C     SCACCT        PARM      *BLANKS       ##ACCT                         Bank Acct #
     C                   PARM                    ##CALL                         (V)oucher/(C)heck
     C                   PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C                   PARM      *BLANKS       ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM      *BLANKS       ##FTYP           10            Form Type
     C                   PARM      *BLANK        ##RTCD            1            Return Code
     C                   PARM      *ZERO         #2EFDT            7
     C                   PARM      *BLANKS       ##CPYQ           10
     C                   PARM                    ##CKTP
     C*** End add ***** 05/16/01 **************************************
     C*
     C*** BEGIN ADD *** 06/26/98 **************************************
     C                   ENDIF
     C*** END ADD ***** 06/26/98 **************************************
     C*
     C*  EDIT ADD/SUBTRACT FIELD
     C*
     C     SCADSB        IFNE      'A'
     C     SCADSB        ANDNE     'S'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   ENDIF                                                  SCADSB<>A/S
     C*
     C*  VALIDATE THE RESERVE AMOUNT REMAINING VS THE CHECK AMOUNT
     C*
     C                   Z-ADD     SCAMT         XXREM
     C*** Begin dlt *** 11/27/01 **************************************
     C*******************  Z-ADDSVCMX     X
     C*** End dlt ***** 11/27/01 **************************************
     C*** Begin add *** 11/27/01 **************************************
     C                   Z-ADD     1             X
     C     PCRECD        LOOKUP    RT(X)                                  60
     C*** End add ***** 11/27/01 **************************************
     C*
     C*** BEGIN DLT *** 08/15/00 **************************************
     C********** RPKY      CHAINPRP01                60
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** RPSTAT    IFEQ *BLANK
     C********** RPRECD    ANDEQPCRECD
     C**********
     C********** SCADSB    IFEQ 'A'
     C**********           ADD  RPAMT     XXREM
     C**********           ENDIF                           SCADSB=A
     C**********
     C**********           ENDIF                           RPSTAT=*BLANK
     C**********
     C********** RPKY      READEPRP01                    60
     C**********           ENDDO                           *IN60=*OFF
     C*** END DLT ***** 08/15/00 **************************************
     C*
     C     SCADSB        IFEQ      'A'
     C*
     C*** BEGIN DLT *** 08/15/00 **************************************
     C********** XXREM     IFGT RR,X
     C********** CMSTAT    ANDNE'C'
     C*** BEGIN DLT *** 03/02/99 **************************************
     C******************** MOVE *ON       *IN37
     C*** END DLT ***** 03/02/99 **************************************
     C*** BEGIN ADD *** 03/02/99 **************************************
     C**********           MOVEA'010100'  *IN,71
     C*** END ADD ***** 03/02/99 **************************************
     C**********           GOTO STRT
     C**********           ENDIF                           XXREM>RR,X
     C*** END DLT ***** 08/15/00 **************************************
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*  Execute Claim/Payment/Reserve Edits
     C*
     C                   MOVE      *ZEROS        ##EOB#
     C                   MOVE      *ZEROS        ##SEQ#
     C                   MOVE      *ZEROS        ##RCN
     C                   CALL      'WA013R'      A013PL
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD ***** 08/15/00 **************************************
     C*
     C                   ELSE
     C*
     C*** BEGIN DLT *** 03/02/99 **************************************
     C********** XXREM     CABGTAR,X      STRT       37
     C*** END DLT ***** 03/02/99 **************************************
     C*** BEGIN ADD *** 03/02/99 **************************************
     C     XXREM         IFGT      AR(X)
     C                   MOVEA     '010100'      *IN(71)
     C                   GOTO      STRT
     C                   ENDIF                                                  XXREM>RR,X
     C*** END ADD ***** 03/02/99 **************************************
     C*
     C                   ENDIF                                                  SCADSB=A
     C*
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*  Check for Duplicate or Possible Duplicate Payment
     C*
     C     XFIRST        IFEQ      *ON
     C     SVAMT         ORNE      SCAMT
     C     SVFRDT        ORNE      SCFRDT
     C     SVTODT        ORNE      SCTODT
     C     SVPFEI        ORNE      SCFEIN
     C*
     C                   MOVE      *OFF          XFIRST            1
     C                   MOVE      SCAMT         SVAMT
     C                   MOVE      SCFRDT        SVFRDT
     C                   MOVE      SCTODT        SVTODT
     C                   MOVE      SCFEIN        SVPFEI
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXFROM
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXTO
      *
     C     51            SUBST     SCCS01:12     QQDFND                         Fund Name
     C                   MOVEL     S2NAME        QQDCNM                         Claimant Name
     C                   MOVEL     SCPNAM        QQDPNM                         Provider Name
      *
     C                   CALL      'WA019R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKSEQ
     C                   PARM                    XXFROM
     C                   PARM                    XXTO
     C                   PARM                    SCAMT
     C                   PARM                    ##RECD
     C                   PARM                    SVPFEI
     C                   PARM                    QQDFND           51            Fund Name
     C                   PARM                    QQDCNM           36            Claimant Name
     C                   PARM                    QQDPNM           36            Provider Name
     C     *IN90         PARM      *ZERO         I90               1
     C                   PARM                    ##RETP
     C*** BEGIN ADD *** 09/05/00 **************************************
     C                   PARM                    SCMOD
     C*** END   ADD *** 09/05/00 **************************************
     C***  BEGIN ADD  ***  01/25/07  *********************************
     C                   PARM                    ##RTCD            1
     C***  END ADD ******  01/25/07  *********************************
      *
      *  If *IN90 is *ON, a Duplicate record was found, re-display SCR02
      *
     C     *IN90         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *  Compare Claimant Auxiliary Return to Work DTs against Service To DT
      *
     C     RTTYPE        IFEQ      'C'
     C*
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
      *
     C     CMERTW        IFGT      *ZERO
     C     CMERTW        ANDLT     XXTO
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C*     Check RTW date in work history.
     C                   Z-ADD     CMRTW         XXRTW                           use master
     C                   Z-ADD     XXDAT1        XXFRDT
     C                   EXSR      CKRTW
     C*
     C*     Use work history to edit service date.
     C*
     C     XXRTW         IFGT      *ZERO
     C     XXRTW         ANDLT     XXTO
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  XXRTW>*ZERO
     C*
     C     *IN43         IFEQ      *ON
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   ENDIF                                                  . IN43 = ON
     C*
     C     *IN42         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C     *IN57         OREQ      *ON
     C*
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWE
     C*
     C                   Z-ADD     XXRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
      *
     C                   Z-ADD     SCTODT        SCTDT1
     C                   Z-ADD     SCTODT        SCTDT2
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C                   EXFMT     SCR05
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C*  Check for user response F12 = Cancel
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 08/15/00 **************************************
     C*
     C     REDSP2        IFEQ      'Y'
     C                   MOVE      'N'           REDSP2
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  REDSP2=Y
     C*
     C*  ALL EDITS HAVE BEEN PASSED, WRITE ADJUSTMENT
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY2         READPE(N) PPA01                                  60
     C*
     C   60              Z-ADD     1             PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   Z-ADD     SCWKS         PAWKS
     C                   Z-ADD     SCDYWK        PADAYS
     C*
     C     SCADSB        IFEQ      'A'
     C                   Z-ADD     SCAMT         PAAMT
     C*** BEGIN DLT *** 09/07/00 **************************************
     C*******************  MOVE *BLANK    PAADST
     C*** END DLT ***** 09/07/00 **************************************
     C                   ELSE
     C                   Z-SUB     SCAMT         PAAMT
     C*** BEGIN DLT *** 09/07/00 **************************************
     C*******************  MOVE 'A'       PAADST
     C*** END DLT ***** 09/07/00 **************************************
     C                   ENDIF                                                  SCADSB=A
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     SCFND         PAFND
     C                   Z-ADD     SCFYR         PAFYR
     C                   Z-ADD     SCCASE        PACASE
     C                   Z-ADD     SCCHK#        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   MOVE      *BLANK        PAADST
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     SCFEIN        PAFEIN
     C                   Z-ADD     SCMOD         PAMOD
     C                   MOVE      SCBR2         PABR2
     C                   MOVE      SCBR7         PABR7
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   MOVE      SCTRCD        PATRCD
     C                   MOVE      'A'           PATYPE
     C                   MOVE      PCRECD        PARECD
     C                   MOVE      RTTYPE        PARETP
     C                   MOVE      *BLANKS       PADESC
     C                   MOVEL     SCTRDS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   MOVEL     CMOFF#        PAOFF#
     C*
     C                   MOVEL     SCACCT        PAACCT
     C*** Begin dlt *** 05/16/01 **************************************
     C********** BIKY      CHAINPBI01                60
     C**********           MOVELBICKTP    PACKTP
     C*** End dlt ***** 05/16/01 **************************************
     C*** Begin add *** 05/16/01 **************************************
     C                   MOVEL     XXCKTP        PACKTP
     C*** End add ***** 05/16/01 **************************************
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      XXDSQ#        PADSQ#
     C*** END   ADD *** 08/09/07 **************************************
     C*
     C                   CLEAR                   PABUL1
     C                   CLEAR                   PAEOB#
     C                   CLEAR                   PAPGTP
     C                   CLEAR                   PARCVD
     C                   CLEAR                   PADUE
     C                   CLEAR                   PALTCD
     C                   CLEAR                   PADPCD
     C                   CLEAR                   PADPCU
     C                   CLEAR                   PARCDT
     C                   CLEAR                   PARADJ
     C*
      *** BEG ADD ***** 07/27/09 **************************************
     C     F2CTYP        IFEQ      'X'
     C                   MOVE      'X'           PASTAT
     C                   ENDIF
      *** END ADD ***** 07/27/09 **************************************
     C*
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     PPA01
     C*** BEGIN ADD *** 08/16/01 **************************************
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   MOVE      SCFND         RPFND
     C                   MOVE      SCFYR         RPFYR
     C                   MOVE      SCCASE        RPCASE
     C                   MOVE      SCCHK#        RPCHK#
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
     C                   MOVEL     *BLANKS       RPRMDS
     C                   MOVEL     *BLANKS       RPRMK1
     C                   MOVEL     *BLANKS       RPRMK2
     C                   MOVEL     *BLANKS       RPRMK3
     C                   MOVEL     SCPNAM        RPPNAM
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVEL     SCST          RPST
     C                   MOVE      SCZP1         XXZIP1
     C                   MOVE      SCZIP2        XXZIP2
     C                   MOVE      XXZIP         RPZIP
     C                   MOVE      *BLANK        RPSTAT
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     *ZEROS        RPBUL1
     C                   MOVEL     SCLTCD        RPLTCD
     C*** Begin add *** 05/15/03 **************************************
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*** End add ***** 05/15/03 **************************************
     C*
     C                   WRITE     PRP02
     C*** END   ADD *** 08/16/01 **************************************
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*** Begin add *** 11/27/01 **************************************
     C                   Z-ADD     1             X
     C     PCRECD        LOOKUP    RT(X)                                  60
     C*** End add ***** 11/27/01 **************************************
     C                   Z-ADD     RR(X)         SVRR
     C*
     C                   SELECT
     C     SCADSB        WHENEQ    'A'
     C                   SUB       SCAMT         RR(X)
     C                   ADD       SCAMT         AR(X)
     C     SCADSB        WHENEQ    'S'
     C                   ADD       SCAMT         RR(X)
     C                   SUB       SCAMT         AR(X)
     C                   ENDSL
     C*
     C*  DO NOT UPDATE REMAINING RESEVES IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVRR          RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C                   SELECT
     C     SCADSB        WHENEQ    'A'
     C                   SUB       SCAMT         ELOUTS
     C                   ADD       SCAMT         ELTCLP
     C     SCADSB        WHENEQ    'S'
     C                   ADD       SCAMT         ELOUTS
     C                   SUB       SCAMT         ELTCLP
     C                   ENDSL
     C*
     C*  DO NOT UPDATE ELOUTS IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVOUTS        ELOUTS
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   ADJUSTMENT TO KEEP CLAIM MSTR (WMCMP) AND RSRV HIST (WMRHP)
     C*   IN BALANCE.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   SELECT
     C     SCADSB        WHENEQ    'S'
     C                   Z-SUB     SCAMT         XXAMT
     C                   MOVEL     SFLAG         XXEXPL
     C     SCADSB        WHENEQ    'A'
     C                   Z-ADD     SCAMT         XXAMT
     C                   MOVEL     AFLAG         XXEXPL
     C                   ENDSL
     C*
     C                   EXSR      RSVHST
     C*
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   CLEAR                   SCCASE
     C                   CLEAR                   SCTRCD
     C                   CLEAR                   SCADSB
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*_______________________________________________________________
     C*
     C* PROCESS SCREEN 3 - OFFSETTING ADJUSTMENT
     C*
     C     SCR03T        TAG
     C*
     C*  VALIDATE THE RESERVE CODE VS THE ORIGINAL RESERVE CODE.
     C*
     C                   MOVEL(P)  SCOFCD        XXKEY
     C*
     C     TBKY          CHAIN     PPC01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     50
     C     PCSTAT        CABEQ     'I'           STRT                     50
     C     PCRECD        CABNE     PARECD        STRT                 5353
     C*
     C**
     C                   MOVEL(P)  PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   MOVE      RTTYPE        SV1RTP            1
     C                   MOVE      PCRECD        SV1RCD            2
     C*
     C*  THE TYPE MUST BE AN ADJUSTMENT
     C*
     C     PCTRTP        CABNE     'A'           STRT                 5151
     C*
     C*  THE DESCRIPTION MUST NOT BE BLANK
     C*
     C     SCOFDS        CABEQ     *BLANKS       STRT                     54
     C*
     C*  ALL EDITS HAVE BEEN PASSED, GOTO SCREEN 4
     C*
     C     SCOFAM        MULT      -1            SCOAMT            9 2
     C*
     C                   MOVE      *OFF          *IN75
     C*
     C                   MOVE      'N'           OVWKDY            1
     C                   Z-ADD     *ZEROS        SVWKS
     C                   Z-ADD     *ZEROS        SVDYWK
     C                   CLEAR                   SCMSG3
     C                   CLEAR                   SCMSG4
     C*
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 4 - CORRECTING ADJUSTMENTS
     C*
     C     SCR04T        TAG
     C*
     C                   CLEAR                   RCD
     C                   CLEAR                   RTP
     C*
     C                   Z-ADD     *ZERO         XXWK
     C                   Z-ADD     *ZERO         XXDY
     C                   Z-ADD     *ZERO         XXAM
     C                   MOVEA     '0000'        *IN(51)
     C                   CLEAR                   SCMSG4
     C*
     C*  CHECK ALL SERVICE DATES TO DEFAULT WEEKS/DAYS IF COMP TRANS
     C*
     C                   MOVE      'N'           REDSP4            1
     C*
     C     SVRTTP        IFEQ      'C'
     C**** BEGIN DELETE *** 10/23/98 *********************************
     C*****************    EXSR WKSDYS
     C**** END DELETE ***** 10/23/98 *********************************
     C***  BEGIN ADD  ***  05/19/99  *********************************
     C                   EXSR      WKSDYS
     C***  END   ADD  ***  05/19/99  *********************************
     C                   ENDIF                                                  SVRTTP=C
     C*
     C                   Z-ADD     *ZERO         Y                 3 0
     C*
     C                   DO        4             I1                3 0
     C*
     C                   MOVEA     '0000'        *IN(71)
     C*
     C     I1            ADD       70            Y
     C*
     C     CD(I1)        IFNE      *BLANKS
     C     YR(I1)        ORNE      *ZEROS
     C     CA(I1)        ORNE      *ZEROS
     C     FE(I1)        ORNE      *ZEROS
     C     MO(I1)        ORNE      *ZEROS
     C     DS(I1)        ORNE      *BLANKS
     C     B2(I1)        ORNE      *BLANKS
     C     B7(I1)        ORNE      *BLANKS
     C     PO(I1)        ORNE      *ZEROS
     C     FR(I1)        ORNE      *ZEROS
     C     TO(I1)        ORNE      *ZEROS
     C     WK(I1)        ORNE      *ZEROS
     C     DY(I1)        ORNE      *ZEROS
     C     AM(I1)        ORNE      *ZEROS
     C*
     C     TO(I1)        IFNE      SVT(I1)
     C                   MOVE      'N'           RTW(I1)
     C                   ENDIF                                                  TO,I1<>SVT,I1
     C*
     C                   MOVE      *ON           *IN(Y)
     C                   MOVE      *ON           *IN55
     C*
     C*  VALIDATE ADJUSTMENT CODE
     C*
     C                   MOVEL(P)  CD(I1)        XXKEY
     C*
     C     TBKY          CHAIN     PPC01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     20
     C     PCSTAT        CABEQ     'I'           STRT                     20
     C*
     C                   MOVEL(P)  PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   MOVE      RTTYPE        RTP(I1)
     C                   MOVE      PCRECD        RCD(I1)
     C*
     C*  THE TYPE MUST BE AN ADJUSTMENT
     C*
     C     PCTRTP        CABNE     'A'           STRT                 9191
     C*
     C*  IF RESERVE TYPE IS COMPENSATION SETON INDICATOR
     C*
     C     RTTYPE        COMP      'C'                                    57
     C*
     C*  VALIDATE CASE#
     C*
     C                   Z-ADD     CA(I1)        XXCASE
     C                   Z-ADD     YR(I1)        XXFYR
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     93
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER
     C*
     C                   Z-ADD     1             X
     C*
     C     PCRECD        LOOKUP    RT(X)                                  60
     C*
     C     *IN60         CABEQ     *OFF          STRT                     38
     C*
     C*  AMOUNT MUST BE GREATER THAN ZERO
     C*
     C     AM(I1)        CABLE     *ZERO         STRT                   3636
     C*
     C*** BEGIN DLT *** 09/07/00 **************************************
     C*  IF CLAIM IS CLOSED CHECK THE RESERVE CUSHION
     C**********
     C********** CMSTAT    IFEQ 'C'
     C**********
     C**********           CALL 'SRCUSH'
     C**********           PARM           CMCO#
     C**********           PARM           CMFND
     C**********           PARM           CMFYR
     C**********           PARM           CMCASE
     C**********           PARM           PCRECD
     C**********           PARM           CMLTFL
     C**********           PARM           ##CUSH  92
     C**********
     C********** AM,I1     CABGT##CUSH    STRT       94
     C**********
     C**********           ENDIF                           CMSTAT=C
     C*** END DLT ***** 09/07/00 **************************************
     C*
     C*  VALIDATE THE RESERVE AMOUNT REMAINING VS THE CORRECTING AMOUNT
     C*
     C                   Z-ADD     AM(I1)        XXREM
     C*
     C                   EXSR      SRAM1
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C     CMSTAT        IFEQ      'O'
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RPSTAT        IFEQ      *BLANK
     C     RPRECD        ANDEQ     RCD(I1)
     C*
     C     SVTYPE        IFNE      'R'
     C                   ADD       RPAMT         XXREM
     C                   ENDIF                                                  SVTYPE<>R
     C*
     C                   ENDIF                                                  RPSTAT=*BLANK
     C*
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     SVTYPE        IFNE      'R'
     C     XXREM         CABGT     RR(X)         STRT                 37
     C                   ELSE
     C     XXREM         CABGT     AR(X)         STRT                 37
     C                   ENDIF                                                  SVTYPE<>R
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   ELSE
     C*
     C*  IF CLAIM IS CLOSED CHECK THE RESERVE CUSHION
     C*
     C                   CALL      'SRCUSH'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    PCRECD
     C                   PARM                    CMLTFL
     C                   PARM                    ##CUSH            9 2
     C*
     C     XXREM         CABGT     ##CUSH        STRT                 94
     C*
     C                   ENDIF                                                  CMSTAT=C
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C*  DESCRIPTION MUST NOT BE BLANK
     C*
     C     DS(I1)        IFEQ      *BLANKS
     C                   MOVEL     PCDESC        DS(I1)
     C                   ENDIF                                                  DS,I1=*BLANKS
     C*
     C*  THE PAYEE & MOD MUST BE VALID
     C*
     C*  ALLOW ZERO FEIN# FOR DEDUCTIBLE RESERVES
     C*
     C     SVTRCD        LOOKUP    DR                                     60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCFEIN        ORNE      *ZERO
     C*
     C                   MOVE      CMSS#         ZZSS#
     C*
     C     *IN57         IFEQ      *ON
     C     FE(I1)        ANDNE     ZZSS#
     C     FE(I1)        ANDNE     *ZERO
     C     *IN57         OREQ      *OFF
     C     FE(I1)        ANDNE     ZZSS#
     C*
     C                   Z-ADD     FE(I1)        XXFEIN
     C                   Z-ADD     MO(I1)        XXMOD
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     95
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    CABEQ'I'       STRT           95
     C     PDSTAT        IFNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   GOTO      TERM
     C                   ENDIF
     C*** END CHG *** 11/02/09 **************************************
     C*
     C                   ENDIF                                                  *IN57=*ON
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  VALIDATE PAYMENT VALUE 1 CODE
     C*
     C                   MOVEL(P)  B2(I1)        XXKEY
     C*
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     27
     C     B2STAT        CABEQ     'I'           STRT                     27
     C*
     C*  VALIDATE PAYMENT VALUE 2 CODE
     C*
     C                   MOVEL(P)  B7(I1)        XXKEY
     C*
     C     TBKY          CHAIN     PB701                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     28
     C     B7STAT        CABEQ     'I'           STRT                     28
     C*
     C     CMLTFL        IFEQ      'I'
     C     B7LTLF        CABNE     'Y'           STRT                 2929
     C                   ENDIF                                                  CMLTFL=I
     C*
     C*  EDIT POSTING DATE
     C*
     C                   Z-ADD     PO(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PO(I1)
     C*
     C     SRERR         CABEQ     *ON           STRT                     30
     C*
     C*  EDIT FROM DATE
     C*
     C                   Z-ADD     FR(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         FR(I1)
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
     C*
     C     SRERR         CABEQ     *ON           STRT                     31
     C*
     C     XXTEST        CABLT     CMACDT        STRT                   39
     C*
     C*  EDIT TO DATE
     C*
     C                   Z-ADD     TO(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         TO(I1)
     C*
     C     SRERR         CABEQ     *ON           STRT                     32
     C*
     C*  THE DATES MUST BE IN LOGICAL ORDER
     C*
     C                   Z-ADD     FR(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     TO(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C     XXDAT2        CABLT     XXDAT1        STRT                   33
     C*
     C*  Compare Claimant Aux Return to Work DTs against Service To DT
     C*
     C     RTTYPE        IFEQ      'C'
     C*
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
     C*
     C     CMERTW        IFGT      *ZERO
     C     CMERTW        ANDLT     XXDAT2
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  CMERTW>*ZERO
     C*
     C     CMRTW         IFGT      *ZERO
     C     CMRTW         ANDLT     XXDAT2
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  CMERT>*ZERO
     C*
     C     *IN42         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C*
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWE
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
     C*
     C                   Z-ADD     TO(I1)        SCTDT1
     C                   Z-ADD     TO(I1)        SCTDT2
     C*
     C     RTW(I1)       IFNE      'Y'
     C                   EXFMT     WDWMSG
     C                   WRITE     SCR99
     C                   WRITE     SCR04
     C  NKL              MOVE      'Y'           RTW(I1)
     C                   ENDIF                                                  RTW,I1<>Y
     C*
     C*  Check for user response F12 = Cancel
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN97
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   ENDIF                                                  *IN42=*ON=*IN43
     C*
     C*  COMPARE THE FULL SALARY PAID THROUGH DATE TO THE FROM DATE
     C*
     C     CMFSDT        IFNE      *ZERO
     C     CMFSDT        ANDGE     XXDAT2
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   ENDIF                                                  *ZERO<>CMFSDT>=XXDAT
     C*
     C                   ENDIF                                                  RTTYPE=C
     C*
     C* THE NUMBER OF WEEKS CAN NOT BE LESS THAN ZERO
     C*
     C     WK(I1)        CABLT     *ZERO         STRT                   34
     C*
     C*        DAYS MUST BE 0-7
     C**********************
     C     DY(I1)        IFLT      0
     C     DY(I1)        ORGT      7
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF                                                  0>DY,I1>7
     C*
     C*
     C****  ACCUMULATE WEEKS, DAYS, AND AMOUNT
     C*
     C                   ADD       WK(I1)        XXWK              5 0
     C                   ADD       DY(I1)        XXDY              3 0
     C                   ADD       AM(I1)        XXAM             11 2
     C                   ENDIF                                                  CD,I1<>*BLANKS
     C*
     C                   ENDDO                                                  I1->4
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN55=*OFF
     C*
     C*  VALIDATE THE RESERVE AMOUNT REMAINING VS THE OFFSETTING AMOUNT
     C*
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C*
     C                   Z-ADD     SCOAMT        XXREM
     C*** Begin dlt *** 11/27/01 **************************************
     C******************** Z-ADDSVCMX     X
     C*** End dlt ***** 11/27/01 **************************************
     C*** Begin add *** 11/27/01 **************************************
     C                   Z-ADD     1             X
     C     SV1RCD        LOOKUP    RT(X)                                  60
     C*** End add ***** 11/27/01 **************************************
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C     CMSTAT        IFEQ      'O'
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RPSTAT        IFEQ      *BLANK
     C     RPRECD        ANDEQ     SV1RCD
     C*
     C     SVTYPE        IFEQ      'R'
     C                   ADD       RPAMT         XXREM
     C                   ENDIF                                                  SVTYPE=R
     C*
     C                   ENDIF                                                  RPSTAT=*BLANK
     C*
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C                   Z-ADD     *ZERO         B                 3 0
     C*
     C                   DO        4             B
     C*
     C     YR(B)         IFEQ      SCOFYR
     C     CA(B)         ANDEQ     SCOFCA
     C*
     C     SV1RCD        IFEQ      RCD(B)
     C                   SUB       AM(B)         XXREM
     C                   ENDIF                                                  SV1RCD=RCD,B
     C*
     C                   ENDIF                                                  YR,B=SCOFYR
     C*
     C                   ENDDO                                                  B->4
     C*
     C                   CLEAR                   SRDFLD
     C*
     C     SVTYPE        IFEQ      'R'
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C     CMSTAT        IFEQ      'O'
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C     XXREM         IFGT      RR(X)
     C*** BEGIN CHG *** 09/07/00 **************************************
     C******************** MOVE *ON       *IN51
     C                   MOVE      *ON           *IN75
     C*** END CHG ***** 09/07/00 **************************************
     C                   MOVEL(P)  MSG(3)        SRDFLD
     C                   ENDIF                                                  XXREM>RR,X
     C*
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   ELSE
     C*
     C*  IF CLAIM IS CLOSED CHECK THE RESERVE CUSHION
     C*
     C                   CALL      'SRCUSH'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    SV1RCD
     C                   PARM                    CMLTFL
     C                   PARM                    ##CUSH
     C*
     C     XXREM         CABGT     ##CUSH        STRT                 75
     C*
     C                   ENDIF
     C*** END ADD ***** 09/07/00 **************************************
     C*
     C                   ELSE
     C*
     C     XXREM         IFGT      AR(X)
     C*** BEGIN CHG *** 09/07/00 **************************************
     C******************** MOVE *ON       *IN51
     C                   MOVE      *ON           *IN75
     C*** END CHG ***** 09/07/00 **************************************
     C                   MOVEL(P)  MSG(3)        SRDFLD
     C                   ENDIF                                                  XXREM>AR,X
     C*
     C                   ENDIF                                                  SVTYPE=R
     C*
     C     SRDFLD        IFNE      *BLANKS
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      79            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG3
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SRDFLD<>*BLANKS
     C*  NOTIFY USER IF TOTAL WEEKS, DAYS, OR AMOUNT OF CORRECTING
     C*  ADJUSTMENTS AREN'T EQUAL TO OFFSETTING ADJUSTMENT
     C*
     C**** BEGIN ADD *** 10/23/98 *************************************
     C     SCDY01        IFEQ      *ZEROS
     C     SCWK01        ANDEQ     *ZEROS
     C     SCDY02        ANDEQ     *ZEROS
     C     SCWK02        ANDEQ     *ZEROS
     C     SCDY03        ANDEQ     *ZEROS
     C     SCWK03        ANDEQ     *ZEROS
     C     SCDY04        ANDEQ     *ZEROS
     C     SCWK04        ANDEQ     *ZEROS
     C                   MOVEL     *ZEROS        SVDYWK
     C                   MOVEL     *ZEROS        SVWKS
     C                   ENDIF                                                  SCOFDY>0' '
     C**** END ADD ***** 10/23/98 *************************************
     C**** BEGIN DELETE *** 10/23/98 ******************************
     C********** SCOFDY    DIV  7         XXWK2
     C**********           MVR            SCOFDY
     C**********           ADD  XXWK2     SCOFWK
     C**********
     C********** XXDY      DIV  7         XXWK2   30
     C**********           MVR            XXDY
     C**********           ADD  XXWK2     XXWK
     C**********
     C**********           CLEARSRDFLD
     C**********
     C********** OVWKDY    IFEQ 'N'
     C********** XXWK      ORNE SVWKS
     C********** XXDY      ORNE SVDYWK
     C**********
     C********** XXWK      IFNE SCOFWK
     C********** XXDY      ORNE SCOFDY
     C**********           MOVE *ON       *IN51
     C**********           MOVE *ON       *IN52
     C**********           MOVE 'Y'       OVWKDY
     C**********           Z-ADDXXWK      SVWKS
     C**********           Z-ADDXXDY      SVDYWK
     C**********           MOVELMSG,1     SRDFLD    P
     C**********           ENDIF                           XXWK<>SCOFWK
     C**********
     C**********           ENDIF                           OVWKDY=N
     C**** END DELETE ***** 10/23/98 ******************************
     C     XXAM          IFNE      SCOAMT
     C     SRDFLD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN53
     C                   MOVEL(P)  MSG(2)        SRDFLD
     C                   ENDIF                                                  XXAM<>SCOAMT
     C*
     C     SRDFLD        IFNE      *BLANKS
     C*
     C                   MOVEA     '0000'        *IN(71)
     C*
     C                   DO        4             X
     C*
     C     AM(X)         IFNE      *ZEROS
     C     X             ADD       70            I1
     C                   MOVE      *ON           *IN(I1)
     C                   ENDIF                                                  AM,X<>*ZEROS
     C*
     C                   ENDDO                                                  4->X
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      79            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG4
     C*
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SRDFLD<>*BLANKS
     C*
     C     REDSP4        IFEQ      'Y'
     C                   MOVE      'N'           REDSP4
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  REDSP4=Y
     C*
     C                   MOVE      'N'           OVWKDY
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*_______________________________________________________________
     C*
     C*  UPDATE THE SELECTED TRANSACTION
     C*
     C                   Z-ADD     SCOFCA        XXCASE
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCSEQ         XXSEQ
     C                   CLEAR                   SVACCD
     C                   CLEAR                   SVACCT
     C*** Begin add *** 06/26/02 ***********************************************
     C                   CLEAR                   SVPGTP
     C                   CLEAR                   SVEOB#
     C                   CLEAR                   SVEOBS
     C                   CLEAR                   SVDPCD
     C                   CLEAR                   SVDPCU
     C*** End add ***** 06/26/02 ***********************************************
     C*
     C     PAKY1         CHAIN     PPA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      PADSQ#        SVDSQ#            5 0
     C*** END   ADD *** 08/09/07 **************************************
     C                   MOVEL(P)  PACKTP        SVACCD
     C                   MOVEL(P)  PAACCT        SVACCT
     C                   MOVEL     PALTCD        SVLTCD
     C                   MOVEL     PAFNAL        SVFNAL
     C*** Begin add *** 06/26/02 ***********************************************
     C                   MOVEL     PAPGTP        SVPGTP
     C                   Z-ADD     PAEOB#        SVEOB#
     C                   Z-ADD     PAEOBS        SVEOBS
     C                   Z-ADD     PADPCD        SVDPCD
     C                   MOVEL     PADPCU        SVDPCU
     C*** End add ***** 06/26/02 ***********************************************
     C*
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCYR01        PAXFYR
     C                   Z-ADD     SCCA01        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C                   MOVE      'A'           PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   UPDATE    PPA01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*_______________________________________________________________
     C*
     C*  WRITE THE OFFSETTING ADJUSTMENT RECORD
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY2         READPE(N) PPA01                                  60
     C*
     C   60              Z-ADD     1             PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   Z-ADD     SCOFCH        PACHK#
     C                   Z-ADD     SCSEQ         PABASQ
     C*
     C     SVTYPE        IFEQ      'R'
     C                   Z-ADD     SCOAMT        PAAMT
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      'Y'           PARADJ
     C                   ELSE
     C                   Z-SUB     SCOAMT        PAAMT
     C                   MOVE      'A'           PAADST
     C                   MOVE      *BLANKS       PARADJ
     C                   ENDIF                                                  SVTYPE=R
     C*
     C                   MOVEL(P)  SVACCD        PACKTP
     C                   MOVEL(P)  SVACCT        PAACCT
     C*
     C                   CLEAR                   PAOFF#
     C     CMKY          CHAIN(N)  PCM01                              60
     C  N60              MOVEL     CMOFF#        PAOFF#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     SCOFPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     SCOFFE        PAFEIN
     C                   Z-ADD     SCOFMO        PAMOD
     C*
     C                   MOVE      SCOFB2        PABR2
     C                   MOVE      SCOFB7        PABR7
     C*
     C                   Z-ADD     SCOFFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCOFTO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   Z-ADD     SCOFWK        PAWKS
     C                   Z-ADD     SCOFDY        PADAYS
     C                   MOVE      SCOFCD        PATRCD
     C                   MOVE      'A'           PATYPE
     C                   MOVE      SV1RCD        PARECD
     C                   MOVE      SV1RTP        PARETP
     C                   MOVEL(P)  SCOFDS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     SVLTCD        PALTCD
     C                   MOVEL     SVFNAL        PAFNAL
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCYR01        PAXFYR
     C                   Z-ADD     SCCA01        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C*
     C                   MOVE      Q1USER        PARQST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   CLEAR                   PABUL1
     C                   CLEAR                   PAEOB#
     C                   CLEAR                   PAPGTP
     C                   CLEAR                   PARCVD
     C                   CLEAR                   PADUE
     C                   CLEAR                   PADPCD
     C                   CLEAR                   PADPCU
     C                   CLEAR                   PARCDT
     C*** Begin add *** 06/26/02 ***********************************************
     C                   MOVEL     SVPGTP        PAPGTP
     C                   Z-ADD     SVEOB#        PAEOB#
     C                   Z-ADD     SVEOBS        PAEOBS
     C                   Z-ADD     SVDPCD        PADPCD
     C                   MOVEL     SVDPCU        PADPCU
     C*** End add ***** 06/26/02 ***********************************************
     C*
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SVDSQ#        PADSQ#            5 0
     C*** END   ADD *** 08/09/07 **************************************
      *** BEG ADD ***** 07/27/09 **************************************
     C     F2CTYP        IFEQ      'X'
     C                   MOVE      'X'           PASTAT
     C                   ENDIF
      *** END ADD ***** 07/27/09 **************************************
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     PPA01
     C*_______________________________________________________________
     C*
     C*  PROCESS THE OFFSETTING CLAIM MASTER RECORD
     C*
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEGIN ADD **** 09/23/11 *********************************
     C                   Z-ADD     1             X
     C*** END   ADD **** 09/23/11 *********************************
     C     SV1RCD        LOOKUP    RT(X)                                  60
     C*
     C                   Z-ADD     RR(X)         SVRR
     C*
     C     SVTYPE        IFEQ      'R'
     C                   SUB       SCOAMT        RR(X)
     C                   ADD       SCOAMT        AR(X)
     C                   ELSE
     C                   ADD       SCOAMT        RR(X)
     C                   SUB       SCOAMT        AR(X)
     C                   SUB       1             CMPCTR
     C                   ENDIF                                                  SVTYPE=R
     C*
     C*  DO NOT UPDATE REMAINING RESEVES IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVRR          RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C     SVTYPE        IFEQ      'R'
     C                   SUB       SCOAMT        ELOUTS
     C                   ADD       SCOAMT        ELTCLP
     C                   ELSE
     C                   ADD       SCOAMT        ELOUTS
     C                   SUB       SCOAMT        ELTCLP
     C                   ENDIF                                                  SVTYPE=R
     C*
     C*  DO NOT UPADTE ELOUTS IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVOUTS        ELOUTS
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   OFFSET ADJ TO KEEP CLAIM MSTR (WMCMP) AND RSV HIST (WMRHP)
     C*   IN BALANCE.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C     SVTYPE        IFEQ      'R'
     C*** BEGIN DLT *** 09/07/00 **************************************
     C*******************  Z-SUBSCOAMT    XXAMT
     C*******************  MOVELDECADJ    XXEXPL
     C*******************  ELSE
     C*******************  Z-ADDSCOAMT    XXAMT
     C*******************  MOVELINCADJ    XXEXPL
     C*** END DLT ***** 09/07/00 **************************************
     C*** BEGIN ADD *** 09/07/00 **************************************
     C                   Z-ADD     SCOAMT        XXAMT
     C                   MOVEL     INCADJ        XXEXPL
     C                   ELSE
     C                   Z-SUB     SCOAMT        XXAMT
     C                   MOVEL     DECADJ        XXEXPL
     C*** END ADD ***** 09/07/00 **************************************
     C                   ENDIF                                                  SVTYPE=R
     C*
     C                   EXSR      RSVHST
     C*
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  WRITE THE CORRECTING ADJUSTMENT RECORD(S)
     C*
     C                   Z-ADD     *ZERO         I1
     C*
     C                   DO        4             I1
     C*
     C     CD(I1)        IFNE      *BLANKS
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CA(I1)        XXCASE
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY2         READPE(N) PPA01                                  60
     C*
     C   60              Z-ADD     1             PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   MOVE      XXCO#         PACO#
     C                   MOVE      SCFND         PAFND
     C                   Z-ADD     YR(I1)        PAFYR
     C                   Z-ADD     CA(I1)        PACASE
     C                   Z-ADD     SCOFCH        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCOFYR        PAXFYR
     C                   Z-ADD     SCOFCA        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C*
     C     YR(I1)        IFEQ      SCOFYR
     C     CA(I1)        ANDEQ     SCOFCA
     C                   Z-ADD     SCSEQ         PABASQ
     C                   ENDIF                                                  YR,I1=SCOFYR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     PO(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     FE(I1)        PAFEIN
     C                   Z-ADD     MO(I1)        PAMOD
     C                   Z-ADD     AM(I1)        PAAMT
     C*
     C     SVTYPE        IFNE      'R'
     C                   Z-ADD     AM(I1)        PAAMT
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      *BLANKS       PARADJ
     C                   ELSE
     C                   Z-SUB     AM(I1)        PAAMT
     C                   MOVE      'A'           PAADST
     C                   MOVE      'Y'           PARADJ
     C                   ENDIF                                                  SVTYPE<>R
     C*
     C                   CLEAR                   PAOFF#
     C     CMKY          CHAIN(N)  PCM01                              60
     C  N60              MOVEL     CMOFF#        PAOFF#
     C*
     C                   MOVE      B2(I1)        PABR2
     C                   MOVE      B7(I1)        PABR7
     C*
     C                   Z-ADD     FR(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     TO(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   Z-ADD     WK(I1)        PAWKS
     C                   Z-ADD     DY(I1)        PADAYS
     C                   MOVE      CD(I1)        PATRCD
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RCD(I1)       PARECD
     C                   MOVE      RTP(I1)       PARETP
     C                   MOVE      *BLANKS       PADESC
     C                   MOVEL     DS(I1)        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL(P)  SVACCD        PACKTP
     C                   MOVEL(P)  SVACCT        PAACCT
     C                   MOVEL     SVLTCD        PALTCD
     C                   MOVEL     SVFNAL        PAFNAL
     C*
     C                   MOVE      Q1USER        PARQST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   CLEAR                   PABUL1
     C                   CLEAR                   PAEOB#
     C                   CLEAR                   PAPGTP
     C                   CLEAR                   PARCVD
     C                   CLEAR                   PADUE
     C                   CLEAR                   PADPCD
     C                   CLEAR                   PADPCU
     C                   CLEAR                   PARCDT
     C*** Begin add *** 06/26/02 ***********************************************
     C                   MOVEL     SVPGTP        PAPGTP
     C                   Z-ADD     SVEOB#        PAEOB#
     C                   Z-ADD     SVEOBS        PAEOBS
     C                   Z-ADD     SVDPCD        PADPCD
     C                   MOVEL     SVDPCU        PADPCU
     C*** End add ***** 06/26/02 ***********************************************
     C*
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SVDSQ#        PADSQ#
     C*** END   ADD *** 08/09/07 **************************************
      *** BEG ADD ***** 07/27/09 **************************************
     C     F2CTYP        IFEQ      'X'
     C                   MOVE      'X'           PASTAT
     C                   ENDIF
      *** END ADD ***** 07/27/09 **************************************
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     PPA01
     C*
     C* PROCESS THE CLAIM MASTER RECORD(S) EFFECTED
     C*
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CA(I1)        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C***** BEGIN ADD ******* 09/23/11 ********************************
     C                   Z-ADD     1             X
     C***** END ADD   ******* 09/23/11 ********************************
     C     RCD(I1)       LOOKUP    RT(X)                                  60
     C*
     C                   Z-ADD     RR(X)         SVRR
     C*
     C     SVTYPE        IFEQ      'R'
     C                   ADD       AM(I1)        RR(X)
     C                   SUB       AM(I1)        AR(X)
     C                   ELSE
     C                   SUB       AM(I1)        RR(X)
     C                   ADD       AM(I1)        AR(X)
     C                   ADD       1             CMPCTR
     C                   ENDIF                                                  SVTYPE=R
     C*
     C*  DO NOT UPDATE REMAINING RESEVES IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVRR          RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C     SVTYPE        IFEQ      'R'
     C                   ADD       AM(I1)        ELOUTS
     C                   SUB       AM(I1)        ELTCLP
     C                   ELSE
     C                   SUB       AM(I1)        ELOUTS
     C                   ADD       AM(I1)        ELTCLP
     C                   ENDIF                                                  SVTYPE=R
     C*
     C*  DO NOT UPDATE ELOUTS IF CLAIM IS CLOSED
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVOUTS        ELOUTS
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*** BEG ADD *** 08/12/03 ****************************
      *
     C     PAFEIN        IFNE      SCOFFE
     C     PAMOD         ORNE      SCOFMO
     C*
     C*  Access the View Check record.
     C*
     C                   Z-ADD     PABASQ        XXSEQ#
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     SCOFPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C******               Z-ADDPAPODT    XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C                   ENDIF
      *
      *  Access Payee Demographic record
      *
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
      *
     C     PDKY          CHAIN     PPD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
      *
     C                   MOVEL     XXLAST        RPPNAM
     C*
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVEL     PDZIP         RPZIP
     C                   MOVE      PDZIP         RPZIP
      *
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C*** Begin add *** 03/23/05 **************************************
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE(P)   PDADR3        RPADR3
     C                   END
     C*** End add ***** 03/23/05 **************************************
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP02
     C                   ENDIF
     C*
     C*** END ADD *** 08/12/03 ****************************
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   CORRECT ADJ TO KEEP CLAIM MSTR (WMCMP) AND RSV HIST (WMRHP)
     C*   IN BALANCE.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C*** Begin dlt *** 03/19/02 **************************************
     C******************** Z-ADDAM,I1     XXAMT
     C******************** MOVELINCADJ    XXEXPL
     C*** End dlt ***** 03/19/02 **************************************
     C*** Begin add *** 03/19/02 **************************************
     C     SVTYPE        IFEQ      'R'
     C                   Z-SUB     AM(I1)        XXAMT
     C                   MOVEL     DECADJ        XXEXPL
     C                   ELSE
     C                   Z-ADD     AM(I1)        XXAMT
     C                   MOVEL     INCADJ        XXEXPL
     C                   ENDIF                                                  SVTYPE=R
     C*** End add ***** 03/19/02 **************************************
     C*
     C                   EXSR      RSVHST
     C*
     C                   ENDIF                                                  CMSTAT=C
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CD,I1<>*BLANK
     C*
     C                   ENDDO                                                  I1->4
     C*
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVE      *BLANKS       SCCLEX
     C                   MOVE      *BLANKS       SCTRDS
     C                   Z-ADD     *ZERO         SCOAMT
     C*
     C                   CLEAR                   SCFYR
     C                   CLEAR                   SCCASE
     C                   CLEAR                   SCENTP
     C                   CLEAR                   SCTRCD
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
     C*===============================================================
     C*  ASGNCS - Subroutine to Assign Screen Group, Year, and Case
     C*===============================================================
     C     ASGNCS        BEGSR
     C*
     C     ##RRN         CHAIN     WMCMP                              62
     C*
     C     *IN62         IFEQ      *ON
     C*
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C*
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
     C*
     C                   MOVEL     S2NAME        SCCSNM
     C*
     C                   CLEAR                   SCNAME
     C     FNKY2         CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C*
     C                   ENDIF                                                  *IN62=*ON
     C*
     C                   ENDSR                                                  ASGNCS
     C*===============================================================
     C*  ASGNNM - Subroutine to Assign "Pay To" Name/Address
     C*===============================================================
     C     ASGNNM        BEGSR
     C*
     C*  Determine if "Pay To" is Claimant or Payee
     C*
     C                   MOVE      SCFEIN        CCFEIN            9
     C*
     C     SCFEIN        IFEQ      *ZERO
     C*** BEGIN CHG *** 06/02/98 ***************************************
     C********** CCFEIN    OREQ XXSS#
     C     CCFEIN        OREQ      CMSS#
     C*** END CHG ***** 06/02/98 ***************************************
     C*
     C*  Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      *ON           XXTERM            1
     C*
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   MOVEL     XXLAST        SCCNAM           40
     C                   MOVEL     SCCNAM        SCPNAM
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C*** Begin add *** 01/11/06 **************************************
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 **************************************
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  CDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C*  Access Payee Demographic record
     C*
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      ASGNGP
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C*
     C     PDFSN         IFNE      *BLANK
     C*
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      *ON           XXTERM            1
     C*
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   ENDIF                                                  PDFSN<>*BLANK
     C*
     C                   MOVEL     XXLAST        SCPNAM
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     PDADR2        SCADR2
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C                   MOVEL     PDZIP         SCZP1
     C                   MOVE      PDZIP         SCZIP2
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        SCPNAM
     C                   MOVE      PDADR2        SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   ENDIF
     C*** END ADD ***** 04/25/00 **************************************
     C*** Begin add *** 01/11/06 **************************************
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 **************************************
     C*** Begin add *** 03/23/05 **************************************
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  PDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  PDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  PDCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C                   END
     C*** End add ***** 03/23/05 **************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SCFEIN=*ZERO
     C*
     C                   Z-ADD     SCFEIN        SVFEIN
     C                   Z-ADD     SCMOD         SVMOD
     C*
     C                   ENDSR                                                  ASGNNM
     C*===============================================================
     C*  ASGNGP - Subroutine to Assign "Group" Provider Indicator
     C*===============================================================
     C     ASGNGP        BEGSR
     C*
     C     NRKY          CHAIN     PNR01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     NRSTAT        ANDEQ     'A'
     C                   SETON                                        24
     C                   ENDIF                                                  *IN60=*ON/NRSTAT=A
     C*
     C                   ENDSR                                                  ASGNGP
     C*===============================================================*
     C*  RSVHST - Write a reserve history record
     C*===============================================================*
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C*** Begin add *** 03/23/04 **************************************
     C                   Z-ADD     1             Y
     C     PARECD        LOOKUP    RT(Y)                                  99
     C                   MOVEL     PARECD        RRT(Y)
     C                   Z-ADD     XXAMT         RSU(Y)
     C*** End add ***** 03/23/04 **************************************
     C*** Begin DLT *** 03/23/04 **************************************
     C*** BEGIN CHG *** 09/07/00 **************************************
     C*******************  MOVELPCRECD    RHRT01
     C**********           MOVELPARECD    RHRT01
     C*** End DLT ***** 03/23/04 **************************************
     C*** END CHG ***** 09/07/00 **************************************
     C**********           Z-ADDXXAMT     RHSU01
     C*** End DLT ***** 03/23/04 **************************************
     C                   MOVEL     XXEXPL        RHEXPL
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHEFDT
     C*
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
     C*** END ADD ***** 03/03/10 **************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR                                                  RSVHST
     C*===============================================================*
     C*  SCRNNM - Centering screen names
     C*===============================================================*
     C     SCRNNM        BEGSR
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   ENDSR                                                  SCRNNM
     C*===============================================================
     C*  SRAM1 - CALCULATE AMOUNT TO COMPARE AGAINST RESERVES
     C*===============================================================
     C     SRAM1         BEGSR
     C*
     C                   Z-ADD     0             B
     C*
     C                   DO        4             B
     C*
     C     I1            IFNE      B
     C*
     C     YR(I1)        IFEQ      YR(B)
     C     CA(I1)        ANDEQ     CA(B)
     C*
     C     RCD(I1)       IFEQ      RCD(B)
     C                   ADD       AM(B)         XXREM
     C                   ENDIF                                                  RCD,I1=RCD,B
     C*
     C                   ENDIF                                                  YR,I1=YR,B
     C*
     C                   ENDIF                                                  I1<>B
     C*
     C                   ENDDO                                                  B->4
     C*
     C     YR(I1)        IFEQ      SCOFYR
     C     CA(I1)        ANDEQ     SCOFCA
     C*
     C     SV1RCD        IFEQ      RCD(I1)
     C                   SUB       SCOAMT        XXREM
     C                   ENDIF                                                  SV1RCD=RCD,I1
     C*
     C                   ENDIF                                                  YR,I1=SCOFYR
     C*
     C                   ENDSR                                                  SRAM1
     C*===============================================================
     C*  SRWKDY - Subroutine to calculate weeks and days
     C*===============================================================
     C     SRWKDY        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXDYS
     C                   MOVE      *OFF          QQERR
     C                   Z-ADD     *ZERO         XXWK
     C                   Z-ADD     *ZERO         XXDY
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQBEG             8 0
     C                   PARM                    QQEND             8 0
     C                   PARM                    XXDYS             5 0
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFEQ      *OFF
     C                   ADD       1             XXDYS
     C     XXDYS         DIV       7             XXWK
     C                   MVR                     XXDY
     C                   ENDIF                                                  QQERR=*OFF
     C*
     C                   ENDSR                                                  SRWKDY
     C*===============================================================
     C*  WKSDYS - Subroutine to calculate Correcting weeks/days
     C*===============================================================
     C     WKSDYS        BEGSR
     C*
     C                   DO        4             X
     C*
     C                   MOVE      *OFF          *IN98
     C*
     C                   Z-ADD     PO(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         PO(X)
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     FR(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVEL     SRDTO         FR(X)
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   MOVE      SRERR         SRERR1            1
     C*
     C                   Z-ADD     TO(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVEL     SRDTO         TO(X)
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   MOVE      SRERR         SRERR2            1
     C*
     C     SRERR1        IFNE      *ON
     C     SRERR2        ANDNE     *ON
     C*
     C     TO(X)         IFNE      SVT(X)
     C                   MOVE      'N'           RTW(X)
     C                   ENDIF                                                  TO,X<>SVT,X
     C*
     C     SVF(X)        IFNE      FR(X)
     C     SVT(X)        ORNE      TO(X)
     C***  BEGIN DEL  ***  05/19/99  *********************************
     C********** WK,X      OREQ *ZERO
     C********** DY,X      ANDEQ*ZERO
     C***  END   DEL  ***  05/19/99  *********************************
     C*
     C     SVT(X)        COMP      TO(X)                              9898
     C*
     C                   Z-ADD     FR(X)         SVF(X)
     C                   Z-ADD     TO(X)         SVT(X)
     C*
     C     SVW(X)        IFEQ      WK(X)
     C     SVD(X)        ANDEQ     DY(X)
     C     WK(X)         OREQ      *ZERO
     C     DY(X)         ANDEQ     *ZERO
     C*
     C                   Z-ADD     FR(X)         QQBEG
     C                   Z-ADD     TO(X)         QQEND
     C*
     C                   EXSR      SRWKDY
     C*
     C     QQERR         IFEQ      *OFF
     C*
     C                   Z-ADD     XXWK          WK(X)
     C                   Z-ADD     XXDY          DY(X)
     C                   Z-ADD     XXWK          SVW(X)
     C                   Z-ADD     XXDY          SVD(X)
     C*
     C                   MOVE      'Y'           REDSP4
     C*
     C                   ENDIF                                                  QQERR=*OFF
     C*
     C                   ENDIF                                                  SVW=WK
     C*
     C     *IN98         IFEQ      *ON                                          TO,X CHANGED
     C                   ADD       70            X
     C                   MOVE      *ON           *IN(X)
     C                   LEAVE
     C                   ELSE                                                   FR,X CHANGED
     C                   WRITE     SCR04
     C                   ENDIF                                                  *IN98=*ON
     C*
     C                   ENDIF                                                  SVF<>FR,X
     C*
     C                   ELSE
     C*
     C     FR(X)         IFEQ      *ZERO
     C     TO(X)         ANDEQ     *ZERO
     C*
     C     CD(X)         IFEQ      *BLANKS
     C     YR(X)         ANDEQ     *ZEROS
     C     CA(X)         ANDEQ     *ZEROS
     C     FE(X)         ANDEQ     *ZEROS
     C     MO(X)         ANDEQ     *ZEROS
     C     DS(X)         ANDEQ     *BLANKS
     C     B2(X)         ANDEQ     *BLANKS
     C     B7(X)         ANDEQ     *BLANKS
     C     PO(X)         ANDEQ     *ZEROS
     C     SVF(X)        ANDNE     FR(X)
     C     SVT(X)        ANDNE     TO(X)
     C                   MOVE      'N'           RTW(X)
     C                   Z-ADD     FR(X)         SVF(X)
     C                   Z-ADD     TO(X)         SVT(X)
     C                   ADD       70            X
     C                   MOVE      *ON           *IN(X)
     C                   MOVE      *ON           *IN98
     C                   SETOFF                                       3132
     C                   LEAVE
     C                   ENDIF                                                  CD,X=*BLANKS
     C*
     C                   ELSE
     C*
     C  N31              Z-ADD     FR(X)         SVF(X)
     C  N32              Z-ADD     TO(X)         SVT(X)
     C     X             ADD       70            I1
     C                   MOVE      *ON           *IN(I1)
     C                   MOVE      *ON           *IN98
     C                   LEAVE
     C*
     C                   ENDIF                                                  FR,X=*ZERO=TO,X
     C*
     C                   ENDIF                                                  SRERR1<>*ON<>SRERR2
     C*
     C                   ENDDO                                                  X->4
     C*
     C     *IN98         CABEQ     *ON           STRT
     C*
     C                   ENDSR                                                  WKSDYS
     C*
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*===============================================================*
     C*  This routine accesses the work history file - WMWHL - to
     C*  edit payment service dates against Left Work and Returned
     C*  to Work history records.
     C*
     C*  The two possible errors are:
     C*          o prior record is Return to Work
     C*          o prior record is Left Work, but date is greater
     C*            than the FROM service date of the payment
     C*
     C*  If no work history record is found prior to the TO service
     C*  date, the claim master Return to Work date is used to edit
     C*  only the TO service date.
     C*===============================================================*
     C     CKRTW         BEGSR
     C*
     C*   Position work history file at TO service date; read prior.
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C     WHKY          SETGT     PWH01
     C     WHKY2         READPE    PWH01                                  17
     C*
     C     *IN17         IFEQ      *ON
     C                   Z-ADD     CMRTW         XXRTW
     C                   ELSE                                                   . IN17 = OFF
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     *IN17         DOWEQ     *OFF
     C*
     C* SKIP E TYPE ENTRIES
     C*
     C     WHWKST        IFNE      'E'
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*   Set error indicators.
     C     WHWKST        IFEQ      'R'
     C                   Z-ADD     WHDATE        XXRTW
     C                   MOVE      *ON           *IN43
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   LEAVE
     C*** END   ADD *** 05/04/09 **************************************
     C                   ELSE
     C*
     C     WHWKST        IFEQ      'L'
     C                   Z-ADD     *ZERO         XXRTW
     C*
     C     WHDATE        IFGE      XXFRDT
     C                   Z-ADD     SCFRDT        SCFDT1
     C                   MOVE      *ON           *IN57
     C                   ENDIF                                                  . DATE>= FR
     C                   ENDIF                                                  . WKST = L
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   LEAVE
     C*** END   ADD *** 05/04/09 **************************************
     C                   ENDIF                                                  . WKST = R
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   ENDIF                                                  . WKST = E
     C     WHKY2         READPE    PWH01                                  17
     C                   ENDDO
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C                   ENDIF                                                  . IN17 = OFF
     C*
     C                   ENDSR
     C*** END ADD ***** 08/15/00 **************************************
      /Free
       //******************************************************************;
       // Clear function key lines for web display;
       //******************************************************************;
       Begsr ClearFKeys;
      /If Defined (WEB)
       Clear scFK1a;
       Clear scFK1b;
       Clear scFK2a;
       Clear scFK2b;
      /Endif
       Endsr;

       //******************************************************************;
       // Load function key lines for web display;
       //******************************************************************;
       Begsr LoadFKeys;
       fk1# = 0;
       fk2# = 0;
       Select;
       When not *in04;
          fk1# = 4;
       Endsl;
       Select;
       Other;
          fl1(1) = fk1(fk1# + 1);
          fl1(2) = fk1(fk1# + 2);
          fl1(3) = fk1(fk1# + 3);
          fl1(4) = fk1(fk1# + 4);
          fl2(1) = fk2(fk2# + 1);
          fl2(2) = fk2(fk2# + 2);
          fl2(3) = fk2(fk2# + 3);
          fl2(4) = fk2(fk2# + 4);
       Endsl;
       Endsr;
      /End-free
     C*
** MSG TABLE
TOTAL WEEKS/DAYS ARE NOT EQUAL TO OFFSETING WEEKS/DAYS
TOTAL AMOUNT IS NOT EQUAL TO OFFSETTING AMOUNT
OFFSETTING AMOUNT EXCEEDS RESERVE AMOUNT
RESERVE CODE ON SELECTED TRANSACTION IS NOT VALID FOR CLAIM.
MESSAGE: COMPENSATION PAYMENT REQUESTED & CLAIM DATE OF DISABILTY NOT ENTERED
**  FK1  1         2         3         4         5         6         7         8
F3=Exit      F7=Financial summary    F8=Accident info   F9=Management info
F10=Search   F11=Benefit info        F12=Cancel         F24=More keys
F15=Remaining reserve   F16=User request search   F24=More keys

F3=Exit      F7=Financial summary    F8=Accident info     F9=Management info
F10=Search   F11=Benefit info        F13=Chg user parms   F24=More keys
F15=Remaining reserve   F16=User request search   F24=More keys

**  FK2 - Function Keys  SCR02
F3=Exit   F8=Accident info   F9=Management info   F12=Cancel   F24=More keys
F4=Additional addresses   F7=Financial summary   F10=Search   F24=More keys
F11=Benefit info   F15=Remaining reserve    F24=More keys

