     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*
     H/TITLE W8080R - REOCCURRING COMPENSATION PAYMENT REQUEST
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER RE-       *
     H*               OCCURRING COMP PAYMENTS.                        *
     H*                                                               *
     H* CALLED FROM:  MENU                                            *
     H*                                                               *
     H* CALLS TO:     NONE                                            *
     H*                                                               *
     H* PROGRAMMER:   C:15   09/30/93        PROJECT: WC258           *
     H* V3 REWRITE:   C:30   12/22/97        PROJECT: WC659           *
     H*****************************************************************
     H* NOTE: IF INSTALLING THIS PROGRAM FROM V10, INSTALL WA023R3 AND*
     H*       WMEFP/L*. IF INSTALLING AT EFT CLIENT, ENTIRE WC1806    *
     H*       MAY BE REQUIRED!                                        *
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/14/22 C:291 Project: 129WXH                                *
     H*                Call GetBankCode with 'R' in ##Rtcd to         *
     H*                determine it is called from this program.      *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* 12/07/18 C:079 Support: 90/75255                              *
     H*                Use first name if present in the file          *
     H*                                                               *
     H* 01/12/16 C:306 Support: 39204                                 *
     H*                Added skip weekends logic in dairy date cal-   *
     H*                culation.                                      *
     H*                                                               *
     H* 07/23/15 C:136 Support: 36153                                 *
     H*                Added bank code to GetEftStatus procedure      *
     H*                                                               *
     H* 05/20/15 C:180 SUPPORT 34774                                  *
     H*                Check for PV1s that do not require             *
     H*                weeks/days to be calculated.                   *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Add call to apportionment window for           *
     H*                cltid 129 only.                                *
     H*                                                               *
     H* 09/20/13 C:279 Support: WC045 83217                           *
     H*                Modified to display PV1 code table based on    *
     H*                the reserve code.                              *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Use GetBankcode instead of WA023R1          *
     H*                2) Use GetEFTStatus instead of WA023R3         *
     H*                3) Use New added field RPEFTC instead of       *
     H*                                                               *
     H* 04/06/11 C:235 Project: 133WKT                                *
     H*                Reoccurring payments; Set default for AR,LA    *
     H*                & MS entry                                     *
     H*                                                               *
     H* 04/26/11 C:136 SUPPORT: WA025 76707                           *
     H*                CORRECT EFT EDIT FOR ADDITIONAL ADDRESSES      *
     H*                                                               *
     H* 12/06/10 C:136 PROJECT: WC1806 PRISS 78                       *
     H*                CHECK EFT STATUS OF PAYEE IF PAYMENT IS        *
     H*                FLAGGED FOR EFT.                               *
     H*                                                               *
     H* 12/04/10 C:183 SUPPORT 74375                                  *
     H*                Full salary date edit override to be applicable*
     H*                only when payment type = 'CO'                  *
     H*                                                               *
     H* 07/13/10 C:235 Project: 114WWK                                *
     H*                Add Recovery Option to Subro Drop Down         *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 02/22/10 C:232 Project: PRISS#9/WC1761                        *
     H*                Fixed the issue of project WC1761.             *
     H*                                                               *
     H* 12/29/09 C:234 SUPPORT WC071/71016                            *
     H*                RE-INTRODUCE EDIT 15                           *
     H*                                                               *
     H* 12/24/09 C:232 SUPPORT WC025/71123                            *
     H*                FIX THE ISSUE OF PROJECT WC1761. REMOVED THE   *
     H*                STATE 'TX' CONDITIONS FROM THE EDITS.          *
     H*                                                               *
     H* 11/05/09 C:220  Project: PRISS#9/WC1761                       *
     H*                 Pop up msg screen for compensation pmt only   *
     H*                 with full salary continued flag.              *
     H*                                                               *
     H* 06/29/09 C:191  Project: WC1806                               *
     H*                 UPDATE CALL TO WA023R3                        *
     H*                                                               *
     H* 06/08/09 C:136 SUPPORT: WA025 68607                           *
     H*               -WRITE A PRENOTE TRANSACTION ONLY FOR THE FIRST *
     H*                PAYMENT OF A RECURRING SERIES                  *
     H*                                                               *
     H* 12/25/08 C:197  Project: WC1761                               *
     H*                 CONTINUED SALARY FLAG EDIT FOR TX             *
     H*                                                               *
     H* 06/24/08 C:191  Project: WC1453                               *
     H*                 Check and send Pre-note transaction           *
     H*                                                               *
     H* 12/05/07 C:193  Project: 135WKJ                               *
     H*                 Change Denied Claim Payment Security Edit     *
     H*                 Message added to W8080F (compile only).       *
     H*                                                               *
     H* 08/09/07 C:191 Project: WC1453                                *
     H*                Add Changes to load dependent sequence # into  *
     H*                WARPP when an Additional Address record is     *
     H*                selected from WDACP. Also, the RPFEIN, RPMOD   *
     H*                and RPDSQ# will be passed to WA023R1 in the    *
     H*                "Data" field.                                  *
     H*
     H* 04/26/07 C:132 Project 133WEB                                 *
     H*                Add a Security Leve for EXACT Dups and a       *
     H*                Separate one for POSSIBLE dups.                *
     H*                                                               *
     H* 02/23/07 C:185 Project: 114WMF                                *
     H*                Back to Base:114WBY dated 06/11/01 & 05/29/01. *
     H*                                                               *
     H* 01/25/07 C:183 Project: 114WLY
     H*                Added Exact Duplicate Indicator Parm to the    *
     H*                WA019R call and to update the payment as       *
     H*                pending if this parm returns as '1'            *
     H*                                                               *
     H* 10/26/06 C:15  Support: WC129/55292                           *
     H*                Added code to load the group check flag with   *
     H*                the appropriate value, based on Grp Prvd file. *
     H*                                                               *
     H* 09/05/06 C:15  Project: 139WES                                *
     H*                Added code to remove the "remaining reserves"  *
     H*                function from all screens.                     *
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
     H* 09/21/05 C:53  PROJECT: WC1309                                *
     H*                Auto calculate the comp rate if amt 0
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*
     H*  3/22/04 C:58  Project: WC1147
     H*                Added SS# to SCR01.
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 02/12/03 C:53  Project: WC1112                                *
     H*                Added edit for maximum # of checks             *
     H*                                                               *
     H* 07/30/01 C:62  Support:  WC105/27933                          *
     H*                Fixed issue which caused F keys not to display *
     H*                in GUI.                                        *
     H*                                                               *
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add Daily batch lockout process                *
     H*                                                               *
     H* 05/30/00 C:53  Project: WC991                                 *
     H*                Add parms for WA013R to facilitate edit # 12   *
     H*                                                               *
     H*  04/26/00 C:61  Project: DR097                                *
     H*                 Chg ALL PDS pgms                              *
     H*                 1. Replace write to SDCMP with API DA700R     *
     H*                                                               *
     H* 04/25/00 C:15 Support: WC025/22607                            *
     H*               Added code to use the provider DBA flag to      *
     H*               deteremine which field is loaded into RPPNAM.   *
     H*                                                               *
     H* 02/25/00 C:15 Project: WC577                                  *
     H*               Added code to load new maint fields added to    *
     H*               WARPP.                                          *
     H*                                                               *
     H* 01/19/00 C:15 Support: WC048/20018                            *
     H*               Changed code which retrieves the next available *
     H*               DARTS seq# to be processed immediately before   *
     H*               the WRITE statement.                            *
     H*                                                               *
     H* 10/05/99 C:15  Support: WC091/19933                           *
     H*                Added code to load RPOFF#, RPRCVD, and RPDUE.  *
     H*                These fields were not being loaded.            *
     H*                                                               *
     H* 08/16/99 C:61  PROJECT WC577                                  *
     H*                Display case number when cycle for second      *
     H*                record entry.                                  *
     H*                                                               *
     H*  07/28/99 C:33 SEQ#: WC090/19045                              *
     H*                REMOVE THE DSPATR(PR) FROM SCMOD.              *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  03/02/99 C:33 SEQ#: WC095/13271
     H*                PASS THE RESERVE TYPE TO WA019R.
     H*                                                               *
     H* 02/19/99 C:15  Support: WC090/16648                           *
     H*                Added code to clear various screen fields      *
     H*                before SCR02 is accessed.                      *
     H*                                                               *
     H* 02/10/99 C:113 SUPPORT: WC025 #16528                          *
     H*                Added call to SRCVTDAT to correct SCSTDT after *
     H*                an F10 SEARCH.                                 *
     H*                                                               *
     H* 12/23/98 C:15 Support: WC072/15523                            *
     H*               Fixed pgm to display "Pay to" name the same as  *
     H*               the other payment pgms ('first middle last'     *
     H*               instead of 'last, middle first')                *
     H*                                                               *
     H* 09/06/98 C:02 PROJECT: 48WLU                                  *
     H*               MOVED CALL TO W0070RFL TO PRIOR TO DSIPLAYING   *
     H*               SCR02.  CALL WAS AFTER PAYMENT REQUEST WAS      *
     H*               ENTERED.                                        *
     H*                                                               *
     H* 07/16/98 C:33 PROJECT: WC833                                  *
     H*               ALLOW FOR F4=ADDITIONAL ADDRESS SELECTION ON    *
     H*               SCR02 FOR OTHER THAN COMPENSATION PAYMENTS      *
     H*                                                               *
     H*  03/31/98 C:33 SEQ#: WC095/10663                              *
     H*                DO NOT SHOW F4 IF NOT COMP PAYMENT.            *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*  01/29/98 C:15 Project: WC659                                 *
     H*                Per JJ, removed the edit which requires the    *
     H*                date of disability to exist for a comp pmt     *
     H*                to be processed.                               *
     H*                                                               *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*                                                               *
     H*  "Before" Version 3                                           *
     H*                                                               *
     H*  03/01/94 C:15 PROJECT: WC298                                 *
     H*                ADDED FEIN/MOD ENTRY TO SCR01 IN CASE THE      *
     H*                CHECKS DON'T GO TO THE CLAIMANT.  ADDED CODE   *
     H*                THAT REQUIRES THE TRANS CODE TO BE A 'C' TYPE. *
     H*                ADDED CODE TO CALCULATE THE # OF DAYS IN       *
     H*                ADVANCE TO PRINT THE CHECKS.                   *
     H*                                                               *
     H*  08/02/94 C:30 PROJECT: WC340                                 *
     H*                ON SCR02, IF EITHER DARTS FLAG IS 'Y', VERIFY  *
     H*                THAT THE EXAMINER IS ASSIGNED.  IF THERE IS NO *
     H*                EXAMINER ASSIGNED, SEND ERROR MSG WCR0499 AND  *
     H*                DO NOT ALLOW FUNCTION TO COMPLETE.             *
     H*                                                               *
     H*  09/01/95 C:33 PROJECT: WC340                                 *
     H*                IF DARTS RMK CODE IS ESTABLISHED IN THE RECUR- *
     H*                RING COMP PMT CTRL FILE, DEFAULT TO 'Y'        *
     H*                                                               *
     H*  12/14/95 C:30 Project:  WC457                                *
     H*           If the Payment Request Entry entered on SCR02 is a  *
     H*           Possible Duplicate in either the Payment Request    *
     H*           file (WARPP) or the Payment Detail file (WDPAP),    *
     H*           display a window with the duplicate record          *
     H*           information.  To continue, the user presses Enter   *
     H*           to return to prior screen.                          *
     H*           On the second pass of the Dup Error, the transaction*
     H*           will not display the window.                        *
     H*           Instead, the data will be processed.                *
     H*                                                               *
     H*  02/16/96 C:33 PROJECT: SUPT                                  *
     H*                CLEAR GROUP TYPE FIELD SO AS NEXT RECORD WILL  *
     H*                NOT PICK UP GROUP TYPE IF IT IS NOT A GROUP    *
     H*                TYPE.                                          *
     H*                                                               *
     H*  08/21/96 C:56 PROJECT: WC371                                 *
     H*                1. Ensure correct NCCI code is on SCR02 if     *
     H*                   changed in Claims Master Maint with CMD-15  *
     H*                                                               *
     H*                2. Add edit to Pmt Val 2 on SCR02 to allow     *
     H*                   only no lost time types                     *
     H*                                                               *
     H*                3. Add Accident Date and Date of Last Comp Pmt *
     H*                   to SCR02                                    *
     H*                                                               *
     H*  09/30/96 C:30 Project: WC583                                 *
     H*           When 'Duplicate' or 'Possible' Duplicate Payment is *
     H*           detected in the Payment Detail, logic added to check*
     H*           if payment is 'VOID'.  If so, then the payment is   *
     H*           not a duplicate.  Continue Payment Detail scan.     *
     H*                                                               *
     H*  11/27/96 C:15 PROJECT: WC616                                 *
     H*                ADDED CODE TO CLEAR THE COMP DARTS CONTROL     *
     H*                FIELDS.                                        *
     H*                                                               *
     H*  01/10/97 C:46 PROJECT: WC636                                 *
     H*                ADDED EDIT THAT START DATE CANNOT BE LESS THAN *
     H*                ACCIDENT DATE.                                 *
     H*                                                               *
     H* 05/13/97 C:62 PROJECT WC674                                   *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N  *
     H*                                                               *
     H* 05/15/97 C:62 PROJECT WC640                                   *
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;          *
     H*               "ADJUSTER" TO CXADJ.                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*   *IN60   -   GENERAL USAGE                                   *
     H*  SCR01                                                        *
     H*  SCR02                                                        *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF

      *  Display file
     FW8080F    CF   E             WORKSTN
     F                                     INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *_________________________________________________________________
      *  Company Master
     FSMCOL     IF   E           K DISK
      *_________________________________________________________________
      *  Fund Master
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
      *_________________________________________________________________
      *  User Parameters
     FWMUPL     IF   E           K DISK
      *_________________________________________________________________
      *  Fund Auxiliary
      *_________________________________________________________________
      *  Request for Payment
     FWARPL     UF A E           K DISK
     F                                     INFDS(DBFDS2)
      *_________________________________________________________________
      *  Claim Master
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *_________________________________________________________________
      *  Claim Master
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
      *_________________________________________________________________
      *  Claimant Demographics
     FWMCDL     IF   E           K DISK
      *_________________________________________________________________
      *  Comp Payment Control file
     FWMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
      *_________________________________________________________________
      *  Table file
     FWTABL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      *_________________________________________________________________
      *  Claim/DARTS Control
     FWMCXL     IF   E           K DISK
      *________________________________________________________________
      *  DARTS Remarks
      *________________________________________________________________
      *  DARTS Remark Code
     FSTCTL1    IF   E           K DISK
      *_________________________________________________________________
      *  User Parameters
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *_________________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
      *_________________________________________________________________
     F*  Payment Detail file -            KEY = CO#/FND/FYR/CASE/SEQ#
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
      *_________________________________________________________________
      *  Additional Address
     FWDACL     IF   E           K DISK
     FWDACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F                                     INFDS(DBFDSA)
      *_________________________________________________________________
      *  Payment Status Table
     FWTPSL     IF   E           K DISK
      *_________________________________________________________________
      *  Group Provider
     FWMNRL     IF   E           K DISK
      *_________________________________________________________________
      *  EDI Control File
      *_________________________________________________________________
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D FK1             S             79    DIM(8) CTDATA PERRCD(1)              SCR01 function Key
     D FK2             S             79    DIM(4) CTDATA PERRCD(1)              SCR02 function Key
     D AK2             S             79    DIM(3) CTDATA PERRCD(1)              Alt SCR02 function K
     D FK1X            S             79    DIM(8) CTDATA PERRCD(1)              SCR01 function Key
     D FK2X            S             79    DIM(4) CTDATA PERRCD(1)              SCR02 function Key
     D I6              S              6    DIM(63)                              Indicator Values
     D fk1#            S              3  0                                      Indicator Values
     D fk2#            S              3  0                                      Indicator Values
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D* CLAIM EXAMINER HEADING
     D*
     D XXADJ           C                   CONST('EXAMINER:')
     D OPTNM           C                   CONST('Reoccurring Comp Pay-
     D                                     ments')
     D NF              C                   CONST('NOT FOUND')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  QCCOM                  1     83
     D  QCCOM1                 1     40
     D  QCCOM2                44     83
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
      *  DDS Field Name
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6
      *
      *  SCCS02 - Field Definitions for Claim Heading Line 2
      *
     D                 DS
     D  SCCS02                 1     79
     D  S2SS1                 26     28
     D  S2SS2                 30     31
     D  S2SS3                 33     36
     D  S2NAME                39     60
     D*
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
     d charSS#         s              9
      ***  End   Add  ***  06/09/20  ****************************
     d ##Appl          s              2                                         Applic Code
     d ##Data          s             75                                         Selection criteria
     d ##cktp          s             10                                         Check/Voucher OUTQ
     d ##ptyp          s              1                                         Check/Voucher OUTQ
     d #1fnd           s              3                                         Check/Voucher OUTQ
     d #ekey           s             30                                         Check/Voucher OUTQ
     d ##date          s              7                                         Check/Voucher OUTQ
     d ##pre           s              1                                         Check/Voucher OUTQ
     d ##eft           s              1                                         Check/Voucher OUTQ
     d ##eftc          s              1                                         Check/Voucher OUTQ
      *******End Add********04/17/12**********************************
      *** beg add ***** 07/23/15 *************************************
     d ##outRRN        s              9                                          07
     d ##EobType       s              9                                          07
      *** end add ***** 07/23/15 *************************************
      *** Beg Add ***  11/25/13  ***********************************************
     d intCO#          s              3  0
     d intFND          s              3  0
     d intFYR          s              3  0
     d intCASE         s              7  0
     d intAMT          s              9  2
     d charBR2         s              2
     d intFRDT         s              8  0
     d intAAMT         s              9  2
     d charRTCD        s              1
     d flgPSIG         s              1
      *
     d ApprActPgm      pr                  ExtPgm('WA901R')
     d intCO#                         3  0
     d intFND                         3  0
     d intFYR                         3  0
     d intCASE                        7  0
     d intAMT                         9  2
     d charBR2                        2
     d intFRDT                        8  0
     d intAAMT                        9  2
     d charRTCD                       1
      *** End Add ***  11/25/13  ***********************************************
     C*
     C*   COMPANY FILE KEY LIST
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FUND FILE KEY LIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   USER PARAMETER FILE KEY LIST
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   FUND DETAIL FILE KEY LIST
     C*
     C*   REQUEST PAYMENT FILE KEY LIST
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C*   CLAIM MASTER FILE KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   CLAIMANT DEMOGRAPHICS FILE KEY LIST
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   PAYMENT CONTROL FILE KEY LIST
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   TABLE FILE KEY LIST
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   REMARK CODE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C*   KEY LIST FOR THE DARTS REMARK FILE
     C*
     C     C2KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  AUXILIARY COMPENSATION PAYEE FILE
     C*
     C     ACKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
      *  Key list for the EDI Control file
      *
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
      *
      *  PAYMENT DETAIL FILE
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C*
      *
     C     *LIKE         DEFINE    RPFEIN        SVFEIN
     C     *LIKE         DEFINE    RPRECD        SVRECD
     C     *LIKE         DEFINE    RPRETP        SVRETP
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    RPFEIN        XXFEIN
     C     *LIKE         DEFINE    RPMOD         XXMOD
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    RPFYR         XXFYR
     C     *LIKE         DEFINE    RPCASE        XXCASE
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C     *LIKE         DEFINE    PCRCLP        XXRMCD
     C     *LIKE         DEFINE    CMUSER        XXUSER
     C     *LIKE         DEFINE    CMDT          XXTEST
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    SCAMT         SVAMT
     C     *LIKE         DEFINE    SCSTDT        SVSTDT
     C     *LIKE         DEFINE    SCWKS         SVWKS
     C     *LIKE         DEFINE    SCDYWK        SVDYWK
     C     *LIKE         DEFINE    SCCHKS        SVCHKS
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@KC              1
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
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
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
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST            2
     C                   PARM                    XXTRID            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C     QQPRDS        IFNE      *BLANKS
     C                   MOVEL     QQPRDS        SRDFLD
     C                   ELSE
     C                   MOVEL     OPTNM         SRDFLD
     C                   ENDIF                                                  QQPRDS<>*BLANKS
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      *
     C* Retrieve client ID.
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
      *
      /Copy SINDIC01
      *
     C                   Z-ADD     5             CK                             Initialize
      *
     C     Q1PRMS        IFGE      3
     C     @@FYR         IFNE      *ZERO
     C     @@CASE        ANDNE     *ZERO
     C                   SETON                                        04
     C                   Z-ADD     1             CK                3 0
     C                   ELSE
     C                   SETOFF                                       04
     C                   Z-ADD     5             CK
     C                   ENDIF
     C                   ENDIF
      *
      *  Assign 2 lines of Function Key legend for SCR01
      *
     C                   MOVE      FK1(CK)       SCFK1A
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1A
     C                   ENDIF
     C                   ADD       1             CK
     C                   MOVE      FK1(CK)       SCFK1B
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1B
     C                   ENDIF
      /Free
       // Load the function keys for web display;
       Exsr LoadFKeys;
      /End-free
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         XXCO#
     C  N60              Z-ADD     UPFND         XXFND
     C*
     C     *IN04         IFEQ      *OFF
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPFYR         SCFYR
     C                   Z-ADD     0             SCCASE
     C                   ELSE
     C                   MOVE      @@FND         SCFND
     C                   Z-ADD     @@FND         XXFND
     C                   MOVE      @@FYR         SCFYR
     C                   MOVE      @@CASE        SCCASE
     C                   ENDIF
      *
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
     C   60              MOVE(P)   *BLANKS       SCNAME
     C*
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C                   MOVE      *OFF          *IN53
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
     C                   MOVEL     CXADJ         SCADJ            20
     C                   ELSE
     C                   MOVEL     XXADJ         SCADJ
     C                   ENDIF
      *
      *  Assign Claim Heading
      *
     C     *IN04         IFEQ      *ON
      *
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
     C                   ENDIF
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     S2SS1         IFNE      *BLANK
     C     S2SS2         ORNE      *BLANK
     C     S2SS3         ORNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  S2SS1         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       S2SS2:0       SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       S2SS3:0       SCSS#
      /Free
       charSS# = %trim(S2SS1) + %trim(S2SS2) + %trim(S2SS3);
       scSS# = DisplaySS#(MaskData('SSN':'charSS#':charSS#));
      /End-Free
      ******  End Chg  ***  06/09/20  **********************
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       XXDSQ#            5 0
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR01
     C                   END
     C*
     C*
     C     Q2RECF        IFEQ      'SCR02'
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR02
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
      *
      * =======================================================
      *   SCR01 - If F7, F8, F9, F11, F15, or F16, then
      *           use screen fields and attempt claim master access
      * =======================================================
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     *INKG         IFEQ      *ON
     C     *INKH         OREQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKK         OREQ      *ON
     C     *INKP         OREQ      *ON
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
      *
     C     CMKY          CHAIN     PCM01                              62
      *
     C     *IN62         IFEQ      *ON
     C                   MOVEA     I6(6)         CH6                            Error #6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
      * =======================================================
     C*  F3=EXIT
      * =======================================================
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
      * =======================================================
     C*  F12=CANCEL
      * =======================================================
     C*
     C     *INKL         IFEQ      *ON
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   OTHER
     C                   GOTO      TERM
     C                   ENDSL
     C                   ENDIF                                                  *INKL=*ON
     C*
      * =======================================================
     C*  F1=RESTART
      * =======================================================
     C*
     C     *INKA         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCCASE
     C                   MOVE      *BLANKS       SCTRCD
     C                   MOVE      *BLANKS       SCBR2
     C                   GOTO      TOP
     C                   END
      *
      * =======================================================
      *  F4 - Additional Address
      * =======================================================
     C*
     C     *INKD         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     ACKY1         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   CALL      'W0573R'
     C                   PARM      '2'           XCALL             1
     C                   PARM      'Y'           XXPMTF            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRN         XXRRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     XXRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     XXRRN         CHAIN     PAC02                              60
     C     *IN60         IFEQ      '0'
       //*** Beg Chg *** 12/07/18 ****************************************
     C****************   MOVEL     ACPNAM        SCPNAM
      /Free
                         If ACfNam = *blanks;
                            scPnam = ACpNam;
                         Else;
                            scPnam = %Trim(ACfNam) + ' ' + %Trim(ACpNam);
                         Endif;
      /End-free
       //*** End Chg *** 12/07/18 ****************************************
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACADR2        SCADR2
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACST          SCST
     C                   MOVEL     ACZIP         SCZP1
     C                   MOVE      ACZIP         SCZIP2
     C                   MOVE      ACSEQ#        XXDSQ#
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
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
     C*
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      '1'           *IN86
     C                   GOTO      STRT
     C                   ENDIF                                                     IN60
     C*
     C                   ENDIF                                                     Q2RECF
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                     INKD
      *
      * =======================================================
      *  F7 - Financial summary
      * =======================================================
      *
     C     *INKG         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF           10
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      *BLANK        ##MODE            1
     C                   PARM                    ##RRN             9 0
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F8 - Accident info
      * =======================================================
      *
     C     *INKH         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R08'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F9 - Management info
      * =======================================================
      *
     C     *INKI         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R09'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM                    ##RRN             9 0
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F16  - Payment Transaction Search (subset USRPRF ID)
      * =======================================================
      *
     C     *INKQ         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        SVRECF
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
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
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C* =======================================================
     C*  F10 - SEARCH
     C* =======================================================
     C*
     C     *INKJ         IFEQ      '1'
      *
     C                   MOVE      HFLD          FIELD
     C*
     C*  Run the date through DATCHK for the editing on the display
     C*
     C     SCSTDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVEA     I6(60)        CH6                            Error # 60
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCSTDT<>*ZER
      *
     C                   SELECT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     Q2RECF        WHENEQ    'SCR01'
      *
     C                   SELECT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCCASE'
     C     HFLD          OREQ      'SCFYR'
      *
     C     *IN04         IFEQ      *ON
     C                   MOVEA     I6(63)        CH6               6            Error #63
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      DBRRN1        ##RRN1            9 0
     C                   PARM      SCFYR         ##YEAR            3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##RRN         IFGT      *ZERO
     C                   EXSR      ASGNCS
     C                   MOVE      '1'           *IN26
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
      *
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   ENDIF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCTRCD'
      *
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2141R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCTRCD
     C                   MOVE      '1'           *IN27
     C     *INKC         CABEQ     *ON           TERM
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     Q2RECF        WHENEQ    'SCR02'
      *
     C                   SELECT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR2'
     C                   MOVE      SCFND         ##FND
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C******* Beg Chg ******* 09/20/13 **********************************
     C*******            PARM      *BLANKS       ##RECD
     C                   PARM                    ##RECD
     C******* End Chg ******* 09/20/13 **********************************
     C                   PARM                    ##FND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCBR2
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C                   MOVE      *ON           *IN41
     C                   MOVE      *BLANK        SCDBR2
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2DESC        SCDBR2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN40
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR7'
     C                   MOVE      SCFND         ##FND
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C                   PARM                    ##FND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCBR7
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7         XXKEY
     C                   MOVE      *ON           *IN51
     C                   MOVE      *BLANK        SCDBR7
     C     TBKY          CHAIN     PB701                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B7DESC        SCDBR7
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMK1'
     C     HFLD          OREQ      'SCRMK2'
     C     HFLD          OREQ      'SCRMK3'
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ##KEY         XXKEY
     C                   SETOFF                                       444547
      *
     C                   SELECT                                                 Cursor Next Field
     C     FLDNUM        WHENEQ    '1'
     C                   MOVEL     ##KEY         SCRMK1
     C                   SETON                                        44
     C     FLDNUM        WHENEQ    '2'
     C                   MOVEL     ##KEY         SCRMK2
     C                   SETON                                        45
     C     FLDNUM        WHENEQ    '3'
     C                   MOVEL     ##KEY         SCRMK3
     C                   SETON                                        47
     C                   ENDSL
      *
     C     TBKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *OFF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   SELECT                                                 Default Cursor
     C     FLDNUM        WHENEQ    '1'
     C                   SETON                                        46
     C     FLDNUM        WHENEQ    '2'
     C                   SETON                                        44
     C     FLDNUM        WHENEQ    '3'
     C                   SETON                                        45
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCSTS'
     C                   MOVE      *BLANK        ##KEY
      *
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCSTS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C                   MOVE      *ON           *IN40
     C                   MOVE      *BLANK        SCSDES
     C     TBKY          CHAIN     PPS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCSDES
     C                   ENDIF
     C                   ELSE
     C                   MOVEA     '1101'        *IN(82)
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCAMT'
     C                   MOVEA     '0010'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCWKS'
     C                   MOVEA     '0101'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCDYWK'
     C                   MOVEA     '0110'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRCVD'
     C                   MOVEA     '0111'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTRDS'
     C                   MOVEA     '1000'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMDS'
     C                   MOVEA     '1010'        *IN(82)
     C                   SETON                                        9047
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C                   OTHER
      *
      *  No Search available for field in cursor selection
     C                   SETON                                        4990
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL
      *
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* =======================================================
     C*  F11 - Benefit info
     C* =======================================================
     C     *INKK         IFEQ      '1'
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R11'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
     C*                                                    JJJ 9-29-89
     C* =======================================================
     C*  F15 - Remaining Reserve
     C* =======================================================
     C*                                                    |
     C     *INKP         IFEQ      '1'                                          |
      *
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R12'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   END                                                    |
     C*
      * =======================================================
     C*   PROCESS HELP REQUESTS
      * =======================================================
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* =======================================================
     C*   PROCESS CHANGE OF USER PARAMETERS
     C* =======================================================
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*
     C* =======================================================
     C*   F21 - Change Provider Status (Group vs. Non-Group)
     C* =======================================================
      *
     C     *INKV         IFEQ      '1'
      *
     C     *IN24         IFEQ      *ON
     C                   SETOFF                                       24
     C                   ELSE
      *  IF THE PAYEE EXISTS IN THE GROUP ID FILE, DISPLAY THE GROUP
      *  TYPE FLAG.
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
      *
     C     NRKY          CHAIN     PNR01                              60
     C     *IN60         IFEQ      '0'
     C     NRSTAT        ANDEQ     'A'
     C                   SETON                                        24
     C                   ENDIF
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* =======================================================
     C*   F24 - Change Function Key Display
     C* =======================================================
      *
     C     *INKY         IFEQ      *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C                   ADD       1             CK
     C     *IN04         IFEQ      *ON
     C     CK            IFGT      4
     C                   Z-ADD     1             CK
     C                   ENDIF
     C                   ELSE
     C     CK            IFGT      8
     C                   Z-ADD     5             CK
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      FK1(CK)       SCFK1A
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1A
     C                   ENDIF
     C                   ADD       1             CK
     C                   MOVE      FK1(CK)       SCFK1B
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1B
     C                   ENDIF
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C                   ADD       1             CK2
     C     CK2           IFGT      4
     C                   Z-ADD     1             CK2
     C                   ENDIF
      *
     C  N12
     CAN 12              MOVEL(P)  AK2(CK2)      SCFK2
     C                   MOVE      FK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK2X(CK2)     SCFK2
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   END                                                    *IN11 EQ '1'
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C**********************************
     C*   PROCESS REQUEST SCREEN 1
     C**********************************
     C     SCR01T        TAG
      *
     C                   MOVE      '0'           SECLVL            1
     C*
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C     ##RTCD        CABEQ     'F'           TERM
      *
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
      *
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C                   MOVE      '0'           @@RTCD
      *
      *  Edit SCR01 Fields
      *
     C                   CALL      'WA011R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM      *BLANK        ##RC              2
      *
     C     ##RC          IFEQ      *BLANK
     C                   MOVEL     Q1PGM         ##PGM
      *
     C                   CALL      'WA012R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCTRCD
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    ##RC
     C                   PARM                    ##RECD            2            Reserve Code
     C                   PARM                    ##RETP            1            Reserve Type
     C                   PARM                    ##TYPE            1            Transaction Type
     C                   PARM                    ##PGM            10            Program Name
     C                   ENDIF
      *
     C     ##RC          IFEQ      '15'
     C                   EXFMT     SCR04
     C     SCEOVR        IFEQ      'Y'
     C                   CLEAR                   ##RC
     C                   ELSE
     C                   MOVE      ##RC          Y                 3 0
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y                 3 0
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Transaction Type Must be Payment
      *
     C     ##TYPE        IFNE      'P'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Assign *IN12 - *ON  = NOT Compensation
      *                 *OFF = Compensation
      *
     C                   MOVE      *OFF          *IN12
     C     ##RETP        IFNE      'C'
     C                   MOVE      *ON           *IN12
     C                   ENDIF
      *
     C                   MOVE      ##RECD        SCRECD
     C                   MOVE      ##RETP        SCRETP
      *
      *  Access Claim Master
      *
     C     CMKY          CHAIN     PCM01                              62
      *
     C*  Date of Disability must be in Claim Master if Comp payment
     C*
      *
      *  MOVE DEMOGRAPHIC DATA IF NO PAYEE ENTERED
      *
     C                   MOVE      CMSS#         XXSS#             9
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '17'          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   END
      *
      *
     C                   MOVE      SCFEIN        CCFEIN            9
     C                   MOVE      *BLANKS       SCPNAM
      *
     C     SCFEIN        IFEQ      *ZERO
     C     CCFEIN        OREQ      XXSS#
     C     CDFSN         CAT       CDMID:1       SCPNAM
     C                   CAT       CDLSN:1       SCPNAM
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
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
     C*
     C                   ELSE
      *
      *  Access Payee Demographic record
      *
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         SCPNAM
     C                   CAT       PDFSN:1       SCPNAM
     C                   END
      *
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     PDADR2        SCADR2
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C                   MOVEL     PDZIP         SCZP1
     C                   MOVE      PDZIP         SCZIP2
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        SCPNAM
     C                   MOVE      PDADR2        SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   ENDIF
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
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
     C                   END
     C*
     C                   ENDIF
     C                   ENDIF
      *
      *  Only perform the following if EDI A49s are being processed
      *  for the claim's jurisdiction state.
      *
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     'A49'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      '0'
      *
      *  Call the FL EDI State Maint screen if this is the first comp
      *  pmt and the claim's jurisdiction state is FL.
      *
     C     *IN12         IFEQ      *OFF
     C     CMJRST        ANDEQ     'FL'
      *
      *  Determine if this was the first comp payment.
      *
     C                   MOVE      'Y'           XXFCPT            1
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPRETP        IFEQ      'C'
     C     RPSTAT        ANDNE     'D'
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C     PAKY          CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C     PARETP        IFEQ      'C'
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO
      *
     C     XXFCPT        IFEQ      'Y'
     C                   MOVE      'C'           QQCALL
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W0070RFL'                           60
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END
      *
     C                   END                                                    RPRETP = C
     C                   ENDIF                                                  *IN60 CHAINPIX01
     C*
     C*  ALL EDITS HAVE BEEN PASSED.  LOAD AND INITIALIZE SCR02 FIELDS
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     PCDESC        SCTRDS
     C                   MOVE      *BLANK        SCSDES
     C                   MOVE      *BLANK        SCDBR2
     C                   MOVE      *BLANK        SCDBR7
     C                   MOVE      *BLANKS       SCWKS
     C                   MOVE      *BLANKS       SCDYWK
     C                   MOVEL     *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCBR7
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVEL     *BLANKS       SCAMT
     C                   MOVE      *BLANKS       SCRMK1
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCRMDS
     C                   Z-ADD     *ZEROS        SCSTDT
     C                   Z-ADD     *ZEROS        SCCHKS
     C*
     C*  ACCUMULATE REMAINING RESERVE AMOUNT
     C*
     C                   MOVE      '0'           *IN97
     C                   MOVE      *ZEROS        SVAMT
     C                   MOVE      *ON           FIRST             1
      *
      *  Initialize Indicators 21 thru 25
      *
     C                   SETOFF                                       212223
     C                   SETOFF                                       242526
     C*
     C*  IF THE PAYEE EXISTS IN THE GROUP ID FILE, DISPLAY THE GROUP
     C*  TYPE FLAG USING *IN70.
     C*
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C                   MOVE      'N'           SCGRP             1
     C*
     C     NRKY          CHAIN     PNR01                              60
     C     *IN60         IFEQ      '0'
     C     NRSTAT        ANDEQ     'A'
     C                   SETON                                        24
     C                   MOVE      'Y'           SCGRP
     C                   ENDIF
      *
      *  Access Claim Master record occurred earlier - *IN62 is file status
      *
     C     *IN62         IFEQ      *OFF
      *
      *  Assign Claim Heading
      *
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
      *  Execute Re-Insurance logic for Claim
      *
     C                   MOVEL     S2NAME        SCCSNM
     C                   Z-ADD     DBRRN         ##RRN
     C                   CALL      'WA010R'
     C                   PARM                    ##RRN                          FOR WMCMP
     C                   PARM      *ZERO         XXFNDR           13 2
     C                   PARM      *BLANK        XXEXRE            1
      *  Excess
     C     XXEXRE        IFEQ      'Y'
     C                   SETON                                        21
     C                   ENDIF
      *
      *
     C                   MOVE      CMJRST        SCJRST
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVE      CMLTFL        SCLTFL
      *
      *  Controvert
     C     CMCVFL        IFEQ      'Y'
     C                   SETON                                        22
     C                   ENDIF
      *  Denied
     C     CMDNY         IFEQ      'Y'
     C                   SETON                                        23
     C                   ENDIF
      *
      *  Subrogated
     C     CMSUBR        IFEQ      'Y'
     C     CMSUBR        OREQ      'R'
     C                   SETON                                        26
     C                   ENDIF
      *
      *  Additional Address
     C     CMKY          CHAIN     PAC01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        25
     C                   ENDIF
     C                   ENDIF
      *
      *  Assign 1 line Function Key legend for SCR02
      *
     C                   Z-ADD     1             CK2               3 0
     C*
     C                   MOVE      FK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK2X(CK2)     SCFK2
     C                   ENDIF
      *
      *  Assign Payment Transaction Description
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PCDESC        SCTRDS
     C                   ENDIF
      *
      *  Assign Payment Reserve Type
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   Z-ADD     *ZERO         SCRCVD                         Date Received
     C                   MOVE      *BLANK        SCSTS                          Payment Status
      *
     C                   SETON                                        50
     C*
     C*  ONLY DISPLAY THE DARTS FLAGS IF THE CORRESPONDING REMARKS WERE
     C*  ENTERED IN THE PAYMENT CONTROL FILE.
     C*
     C                   MOVE      *BLANKS       PCRCEP
     C                   MOVE      *BLANKS       PCRCLP
     C     PCKY          CHAIN     PPC02                              60
      *
     C     PCRCEP        COMP      *BLANKS                                38
     C     PCRCLP        COMP      *BLANKS                                39
      *
      *  If DARTS remark code for 'Each Payment' and 'Last Payment' entered,
      *  default flag to 'Y'.
      *
     C                   MOVE      'N'           SCDRT1                         Each Payment
     C     PCRCEP        IFNE      *BLANKS
     C                   MOVE      'Y'           SCDRT1                         Each Payment
     C                   ENDIF
      *
     C                   MOVE      'N'           SCDRT2                         Last Payment
     C     PCRCLP        IFNE      *BLANKS
     C                   MOVE      'Y'           SCDRT2                         Last Payment
     C                   ENDIF
      *
      *  Default Payment Status value based on Auto Release flag and Claim/
      *  DARTS Control file field values.
      *
     C     PCARFL        IFEQ      'Y'
     C                   MOVE      CXPSC2        SCSTS                          Released Status
     C                   ELSE
     C                   MOVE      CXPMTS        SCSTS                          Pended Status
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
      *
     C                   MOVEL(P)  'RCDF'        KKTBID
     C                   MOVEL(P)  XXJRST        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   GTN501        SCWKS
     C                   MOVE(P)   GTN502        SCDYWK
     C                   MOVE(P)   GTN503        SCCHKS
     C                   ENDIF
      *
     C                   GOTO      STRT
      *
     C******************************************
     C* PROCESS SCREEN 2 - PAYMENT INFORMATION
     C******************************************
     C     SCR02T        TAG
      *
     C                   MOVE      '0'           SECLVL            1
      *
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '00000'       *IN(91)
      *
      *  PAYEE NAME CANNOT BE BLANK
      *
     C     SCPNAM        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN56
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  ADDRESS CANNOT BE BLANK
      *
     C     *IN12         IFEQ      '0'
     C     SCADR1        IFEQ      *BLANK
     C                   MOVEA     '001'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF                                                  SCADR1 = blank
     C     SCCTRY        IFEQ      *BLANKS
     C     SCCTRY        OREQ      'USA'
      *
      *  THE CITY CANNOT BE BLANK
      *
     C     SCCTY         IFEQ      *BLANK
     C                   MOVEA     '010'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCTY = blank
      *
      *  THE STATE MUST BE VALID
      *
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVEA     '011'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF                                                  SCST = '0'
      *
      *  THE ZIP CODE CANNOT BE ZERO
      *
     C     SCZP1         IFLE      *ZERO
     C                   MOVEA     '100'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF                                                  SCZP1 LE 0
     C                   ENDIF                                                  SCZP1 LE 0
     C                   ENDIF                                                  *IN12 = '0'
      *
      *  VALIDATE THE START DATE
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVEA     I6(60)        CH6                            Error # 60
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR = '1'
      *
      *  THE START DATE CANNOT BE LESS THAN THE ACCIDENT DATE
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
      *
     C     XXTEST        IFLT      CMACDT
     C                   MOVEA     I6(59)        CH6                            Error # 59
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  XXTEST < CMACDT
      *
      *  THE WEEKS AND DAYS CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C     SCWKS         IFLE      0
     C     SCDYWK        ANDLE     0
     C                   MOVEA     I6(58)        CH6                            Error # 58
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWKS & SCDYWK
      *
      *  THE # OF FUTURE CHECKS CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C     SCCHKS        IFLE      0
     C                   MOVEA     I6(57)        CH6                            Error # 57
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCHKS LE 0
      *
      *  THE # OF FUTURE CHECKS CANNOT BE GREATER THAN CONTROL MAXIMUM
      *
     C     PCKY          CHAIN     PPC02                              60
      *
     C     *IN60         IFEQ      *OFF
     C     PCMAX#        ANDNE     0
     C     SCCHKS        ANDGT     PCMAX#
     C                   MOVE      *ON           *IN95
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  AMOUNT CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C**         SCAMT     IFLE *ZERO
     C**                   MOVEAI6,39     CH6              Error # 39
     C**                   MOVEACH6       *IN,71
     C**                   SETON                     90
     C**                   GOTO STRT
     C**                   ENDIF                           SCAMT LE 0
      *
      *  If the Auto Release flag in the Payment Control file is 'Y',
      *  the Reserves must be adequate for ALL Future Payments.
      *
     C     PCARFL        IFEQ      'Y'
     C     SCAMT         MULT      SCCHKS        XXAMT
     C                   ELSE
     C                   Z-ADD     SCAMT         XXAMT
     C                   ENDIF
      *
      *  Access Payment Status file
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
      *
     C     TBKY          CHAIN     PPS01                              60
      *
     C     *IN60         IFEQ      *ON
     C     PSSTAT        OREQ      'I'
     C                   MOVEA     I6(36)        CH6                            Error # 36
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 / PSSTAT
      *
     C     PCARFL        IFEQ      'N'
     C     PSTYPE        IFEQ      'R'
     C     PSTYPE        OREQ      *BLANK
      *  Cannot Release Payment if Auto Release flag is 'N'.
     C                   MOVEA     I6(56)        CH6                            Error # 56
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF                                                  PSTYPE
     C                   ENDIF                                                  PCARFL = 'N'
      *
      *  Assign Start Date to "other" date fields not requiring edits.
      *
     C                   Z-ADD     SCSTDT        SCPODT            8 0          Posting Date
     C                   Z-ADD     SCSTDT        SCFRDT            8 0          From Date
     C                   Z-ADD     SCSTDT        SCTODT            8 0          To Date
      *
      * BUILD ##EKEY AND PASS IT TO WA014 TO CHECK TO ENSURE THAT PAYEE
      * EFT IS SET UP FOR EFT PAYMENT. ##EKEY = PAYEE TYPE (C,D,P) CAT
      * KEY INFO FOR WMEFL7
      *
     C                   MOVE      *BLANKS       ##EKEY
     C     SCFEIN        IFEQ      0
     C                   MOVEL(P)  SCFYR         XXWK10           10
     C                   MOVE      SCCASE        XXWK10
     C     XXDSQ#        IFEQ      0
     C                   MOVEL     'C'           ##EKEY
     C                   CAT(P)    XXWK10:0      ##EKEY
     C                   ELSE
     C                   MOVEL     'D'           ##EKEY
     C                   MOVEL     XXWK10        XXWK15           15
     C                   MOVE      XXDSQ#        XXWK15
     C                   CAT(P)    XXWK15:0      ##EKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'P'           ##EKEY
     C                   MOVEL(P)  SCFEIN        XXWK12           12
     C                   MOVE      SCMOD         XXWK12
     C                   CAT(P)    XXWK12:0      ##EKEY
     C                   ENDIF                                                  SCFEIN=0
      *
      *  Call API to Edit remaining screen fields
      *
     C                   CALL      'WA014R'
     C                   PARM                    XXCO#                          Company
     C                   PARM                    SCFND                          Group
     C                   PARM                    SCFYR                          Fund Year
     C                   PARM                    SCCASE                         Claim
     C                   PARM                    SCGRP             1            Group Type
     C                   PARM                    SCPODT                         Posting Date
     C                   PARM                    SCRCVD                         Received Date
     C                   PARM                    SCFRDT                         From Date
     C                   PARM                    SCTODT                         To Date
     C                   PARM                    RTTYPE                         Reserve Type
     C                   PARM                    PCRECD                         Reserve Code
     C                   PARM                    SCBR2                          Payment Value 1
     C                   PARM                    SCBR7                          Payment Value 2
     C                   PARM                    SCSTS                          Payment Status
     C                   PARM      'Y '          SCLTCD            2            Paid on Time
     C                   PARM                    SCDYWK                         Number of Days
     C                   PARM                    ##RC                           Return Code
     C                   PARM      *ZERO         SCEOB#            9 0          EOMB#
     C                   PARM                    SCAMT                          Payment Amount
     C                   PARM                    ##RCN             3 0
     C                   PARM                    ##EKEY           31
     C*
     C     CLTID         IFEQ      '144'
     C     SCFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** IF COMP PAYMENT
     C*
     C     SCRETP        IFEQ      'C'
     C*
     C     SCWKS         IFGT      0
     C     SCDYWK        ORGT      0
     C*
     C*
     C     ##RC          IFEQ      '39'
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      SCWKS         ##WKS             3
     C                   MOVE      SCDYWK        ##DYWK            1
     C*
     C                   CALL      'WA058R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##WKS
     C                   PARM                    ##DYWK
     C                   PARM                    SCBR2
     C                   PARM      SCAMT         ##AMT             9 2
     C*
     C     ##AMT         IFGT      *ZEROS
     C                   Z-ADD     ##AMT         SCAMT
     C                   MOVE      *BLANKS       ##RC
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
      *
     C     CMJRST        IFEQ      'TX'
     C     CMFSAL        ANDEQ     'Y'
     C     ##RC          ANDEQ     '42'
     C                   MOVE      *BLANKS       ##RC
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SKIP          TAG
      *
      *  Payment Description is required
      *
     C     SCTRDS        IFEQ      *BLANK
     C                   MOVEA     I6(62)        CH6                            Error #62
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  If the Payment Status "Status" is blank ("Released"), & the Return Code
      *  from previous edit check is blank, execute Claim/Payment/Reserve Edits
      *
     C     PSTYPE        IFEQ      *BLANK
     C     PSTYPE        OREQ      'R'
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA013R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCEOB#
     C                   PARM      *ZERO         RPSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    PCRECD
     C                   PARM                    RTTYPE
     C                   PARM                    ##RC
     C                   PARM                    XXAMT
     C                   PARM      *ZEROS        XXEOBS            3 0
     C                   PARM                    SCTRCD
     C                   ENDIF
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  EDIT REMARK CODE 1
      *
     C     SCRMK1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60RCSTAT        COMP      'I'                                    60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     '101'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *  EDIT REMARK CODE 2
      *
     C     SCRMK2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60RCSTAT        COMP      'I'                                    60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     '110'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *  EDIT REMARK CODE 3
      *
     C     SCRMK3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60RCSTAT        COMP      'I'                                    60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     '111'         *IN(33)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *  THE FIRST DARTS FLAG MUST BE Y OR N
      *
     C     SCDRT1        IFNE      'Y'
     C     SCDRT1        ANDNE     'N'
     C                   MOVEA     I6(55)        CH6                            Error # 55
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE SECOND DARTS FLAG MUST BE Y OR N
     C*
     C     SCDRT2        IFNE      'Y'
     C     SCDRT2        ANDNE     'N'
     C                   MOVEA     I6(54)        CH6                            Error # 54
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CMCLEX        IFLE      *BLANK
     C     SCDRT1        IFEQ      'Y'
     C                   MOVEA     I6(53)        CH6                            Error # 53
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ELSE
     C     SCDRT2        IFEQ      'Y'
     C                   MOVEA     I6(52)        CH6                            Error # 52
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  Check for Duplicate or Possible Duplicate Payment
     C*
     C     *IN97         IFEQ      *OFF
     C     SVAMT         ORNE      SCAMT
     C     SVSTDT        ORNE      SCSTDT
     C     SVWKS         ORNE      SCWKS
     C     SVDYWK        ORNE      SCDYWK
     C     SVCHKS        ORNE      SCCHKS
     C*
     C                   MOVE      SCAMT         SVAMT
     C                   MOVE      SCSTDT        SVSTDT
     C                   MOVE      SCWKS         SVWKS
     C                   MOVE      SCDYWK        SVDYWK
     C                   MOVE      SCCHKS        SVCHKS
     C*
     C                   MOVE      *OFF          DUPFND            1
     C                   Z-ADD     1             X                 3 0
     C*
     C                   DO        SCCHKS        X
     C                   EXSR      DATES
     C                   EXSR      DUPCHK
     C*
      * If User does not have authority, drop out and go back to screen one
     C     SECLVL        IFNE      '0'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDDO
     C*
     C     DUPFND        IFEQ      *ON
     C     SECLVL        ANDEQ     '0'
     C                   MOVE      *ON           *IN97
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##AMT         IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        ##AMT
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCFRD1            7 0
     C*
     C     CMJRST        IFEQ      'TX'
     C*
     C*  EDIT TO CHECK RESRVE CODE TYPE OR PAYMENT TYPE IS COMP TYPE
     C*
     C*
     C     CMFSAL        IFEQ      'Y'
     C     SCFRD1        ANDLE     CMFSDT
     C     SCRECD        ANDEQ     'CO'
     C     SCRETP        ANDEQ     'C'
     C*
     C                   SETOFF                                       58
     C*
     C     *IN58         DOUEQ     *OFF
     C                   EXFMT     SCR03
     C*
     C     SCFSOV        IFNE      'Y'
     C     SCFSOV        ANDNE     'N'
     C                   SETON                                        5890
     C*
     C                   ELSE
     C                   SETOFF                                       58
     C     SCFSOV        IFEQ      'N'
     C                   SETON                                        6590
     C                   GOTO      STRT
     C                   ELSE
     C     SCFSOV        IFEQ      'Y'
     C* Create check request
     C                   SETON                                            59
     C                   ENDIF                                                                     S
     C*
     C                   ENDIF                                                  SCFSOV<>'N'
     C                   ENDIF                                                  SCFSOV <> 'Y','N'
     C*
     C     *INKL         IFEQ      *ON
     C                   SETOFF                                         59KL
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF                                                  CMFSAL = 'Y'
     C*
     C                   ELSE
     C*
     C     CMFSAL        IFEQ      'Y'
     C     SCFRD1        ANDLE     CMFSDT
     C                   SETON                                        6590
     C*
     C                   ELSE
     C*
     C* Create check request
     C*
     C                   SETON                                          59
     C*
     C                   ENDIF                                                  CMFSAL =  'Y'
     C                   ENDIF                                                  CMJRST    IFEQ 'TX'
     C*
     C*  ALL EDITS HAVE BEEN PASSED.
     C*
     C*  GET THE LAST PAYMENT SEQUENCE NUMBER
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  60
     C   60              Z-ADD     0             RPSEQ#
     C*
     C*  WRITE ALL PAYMENT REQUESTS AND DARTS REMARKS, IF ANY.
     C*
     C                   Z-ADD     1             X                 3 0
     C                   DO        SCCHKS        X                              NEST-A
     C*
     C                   MOVE      XXCO#         RPCO#
     C                   MOVE      XXFND         RPFND
     C                   Z-ADD     SCFYR         RPFYR
     C                   Z-ADD     SCCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   EXSR      DATES
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPFRDT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTODT
     C*
     C                   Z-ADD     SCFEIN        RPFEIN
     C                   Z-ADD     SCMOD         RPMOD
      ******* BEG ADD *** 11/25/13 **************************************
     C*****              MOVE      SCAMT         RPAMT
      /Free
       xxkey = *Blanks;
       xxKey = scbr2;
       flgPSIG = *blanks;
       Chain TBky pb201;
       If %found(wtabl);
          flgPSIG = B2psig;
       EndIf;

       If CltId = '129' And flgPSIG = 'Y';

          intCO#   = xxCO#;
          intFND   = scFND;
          intFYR   = scFYR;
          intCASE  = scCASE;
          intAMT   = scAMT;
          charBR2  = scBR2;
          intFRDT  = scFRDT;
          intAAMT  = *Zeros;
          charRTCD = *Blank;

          ApprActPgm(intCo#:intFND:intFyr:intCase:
                     intAmt:charBr2:intFrdt:intAAmt:charRtcd);
          If charRtcd = 'Y';
             RPamt = intAAmt;
          Else;
             RPamt = intAmt;
         EndIf;
       Else;
          RPamt = scAmt;
       EndIf;
      /End-Free
      ******* END ADD *** 11/25/13 **************************************
     C                   MOVE      SCBR2         RPBR2
     C                   MOVE      SCBR7         RPBR7
     C                   MOVE      SCNCCI        RPNCCI
      *** Begin change *** 05/20/15 ****************************************
     C***                   Z-ADD     SCWKS         RPWKS
     C***                   Z-ADD     SCDYWK        RPDAYS
      /Free
       // Check if Weeks and Days calculation should be excluded for this PV1
        XXKEY = *Blanks;
        XXKEY = SCBR2;
        chain (XXCO#:XXKEY) PB201;
        If %found and B2EXWDC = 'Y';
           RPWKS  = *Zeros;
           RPDAYS = *Zeros;
        Else;
          RPWKS  = SCWKS;
          RPDAYS = SCDYWK;
        Endif;
      /End-Free
      *** End   change *** 05/20/15 ****************************************
     C                   MOVE      SCTRCD        RPTRCD
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      RTTYPE        RPRETP
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     SCTRDS        RPDESC
     C                   MOVE      SCRMDS        RPRMDS
     C                   MOVE      SCRMK1        RPRMK1
     C                   MOVE      SCRMK2        RPRMK2
     C                   MOVE      SCRMK3        RPRMK3
     C                   MOVE      SCPNAM        RPPNAM
     C                   MOVE      SCADR1        RPADR1
     C                   MOVE      SCADR2        RPADR2
     C                   MOVE      SCCTY         RPCTY
     C                   MOVE      SCST          RPST
     C                   MOVEL     SCZP1         RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
      *
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   Z-ADD     0             RPEOB#
     C                   MOVEL     *BLANKS       RPPGTP
     C                   MOVE      XXDSQ#        RPDSQ#
     C     SCGRP         IFEQ      'Y'
     C                   MOVE      'G'           RPPGTP
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   Z-ADD     DSTIME        RPTIME
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      *ZEROS        QXSEQ#
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      RPDSQ#        QXDSQ#
      *
      *******Beg Chg********04/17/12**********************************
     C**********         CALL      'WA023R1'
     C**********         PARM      '2'           ##MODE                         Mode
     C**********         PARM                    ##CO#                          Company
     C**********         PARM                    ##FND                          Group #
     C**********         PARM                    ##FYR                          Year
     C**********         PARM                    ##CASE                         Case
     C**********         PARM      'WC'          ##APPL            2            Applic Code
     C**********         PARM      DATA          ##DATA           75            Selection criteria
     C**********         PARM                    ##CKTP           10            Bank Acct Code
     C**********         PARM                    ##RTCD                         Return Code
      *
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       //*** Beg Add ***** 09/14/22 ************************
       ##rtcd = 'R';
       //*** End Add ***** 09/14/22 ************************
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: ##rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C* If EFT payee, then determine if pre-notification record is required
     C                   MOVE      *OFF          ##EFT                          EFT flag
     C                   MOVE      *OFF          ##PRE                          PRE note flag
      *******Beg Chg********04/17/12**********************************
     C*****RPCKTP        IFEQ      'EFT'
      /Free
       If RPeftc = 'E';
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C     RPFEIN        IFEQ      0
     C* Claimant/dependent
     C* Build ##KEY to access API for EFT record
     C                   MOVEL(P)  RPFYR         XXWK10           10
     C                   MOVE      RPCASE        XXWK10
     C     XXDSQ#        IFEQ      0
     C* Claimant
     C                   MOVE      'C'           ##PTYP
     C                   MOVEL(P)  XXWK10        #EKEY
     C                   ELSE
     C* dependent
     C                   MOVE      'D'           ##PTYP
     C                   MOVEL(P)  XXWK10        XXWK15           15
     C                   MOVE      XXDSQ#        XXWK15
     C                   MOVEL(P)  XXWK15        #EKEY
     C                   ENDIF
     C                   MOVE      ##FND         #1FND
     C                   ELSE
     C* Provider
     C                   MOVEL(P)  RPFEIN        XXWK12           12
     C                   MOVE      RPMOD         XXWK12
     C                   MOVEL(P)  XXWK12        #EKEY
     C*
     C                   MOVE      'P'           ##PTYP
     C                   MOVE      ##FND         #1FND
     C                   ENDIF                                                  RPFEIN=0
     C                   MOVE      RPDT          ##DATE
     C*
      *******Beg Chg********04/17/12**********************************
     C******             CALL      'WA023R3'
     C******             PARM                    ##CO#                          Company
     C******             PARM                    ##PTYP            1            Payee Type
     C******             PARM                    #1FND             3            Group #
     C******             PARM                    #EKEY            30            Key
     C******             PARM                    ##DATE            7            Date
     C******             PARM                    ##EFT             1            EFT flag
     C******             PARM                    ##PRE             1            PRE note flag
     C*
     C*****##EFT         IFEQ      *OFF
     C*****              CLEAR                   RPCKTP
     C*****              ENDIF                                                  ##EFT = *OFF
     C*
      /Free
       //Call the Procedure to retrieve EFT status.

       //*** beg chg ***** 07/23/15 *********************************
       //***If Not GetEFTStatus(##co#: ##ptyp: #1fnd: #ekey: ##date:
       //***##pre);
       If Not GetEFTStatus(##co#: ##ptyp: #1fnd: #ekey: ##date:
        ##pre:##OutRRN:##EobType:##cktp);
       //*** end chg ***** 07/23/15 *********************************
          RPeftc = *Blanks;
       EndIf;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   ENDIF                                                  RPCKTP='EFT'
     C                   MOVE      *BLANK        RPACCT                         Bank Acct #
      *
     C                   MOVE      SCSTS         RPSTS                          Status Code
     C                   MOVE      PSSRT         RPSORT                         Sort Sequence
      *
     C     PSTYPE        IFEQ      'R'
     C                   MOVE      *BLANK        RPSTAT
     C                   ELSE
     C                   MOVE      PSTYPE        RPSTAT
     C                   ENDIF
      *
     C                   MOVE      'N'           RPSENT                         Diary Sent Flag
     C                   Z-ADD     *ZERO         RPDSEQ                         Diary Sequence
     C                   MOVEL     Q1USER        RPRQST                         Initial Requestor
     C                   MOVE      'Y '          RPLTCD                         Pmt Late Code
     C                   MOVE      *BLANK        RPNTWK                         IN House Network Ser
     C                   Z-ADD     *ZERO         RPEOBS                         EOMB Seq#
     C                   MOVE      'N'           RPFNAL                         NOT Final Comp Paymn
     C                   MOVE      *BLANKS       RPOFF#                         office #
     C                   Z-ADD     *ZERO         RPDUE                          due date
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPRCVD                         date received
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     Q1USER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     Q1USER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C     CXDPPC        IFNE      *BLANK
     C     @@RTCD        ANDEQ     '1'
     C                   MOVE      CXDPPC        RPSTS
     C                   MOVE      'P'           RPSTAT
     C                   ENDIF
     C                   MOVE      XXDSQ#        RPDSQ#
      *
     C                   SELECT
     C     ##EFT         WHENEQ    *OFF
     C     ##EFT         OREQ      *ON
     C     ##PRE         ANDEQ     *OFF
     C* Not EFT payee OR
     C* EFT payee but pre-notification is not required, write transaction
     C                   WRITE     PRP01
     C     ##EFT         WHENEQ    *ON
     C     ##PRE         ANDEQ     *ON
     C* EFT payee and Pre-notification is required
     C                   Z-ADD     RPAMT         SVAMT
     C* Create Pre-notification with RPAMT = 0
     C     X             IFEQ      1
     C                   CLEAR                   RPAMT
     C                   WRITE     PRP01
     C                   ENDIF
     C     *IN59         IFEQ      *OFF
     C     CMJRST        ANDEQ     'TX'
     C                   GOTO      TXEDT
     C                   ENDIF
     C* Create check request
     C                   Z-ADD     SVAMT         RPAMT
      *******Beg Chg********04/17/12**********************************
     C*****              CLEAR                   RPCKTP
      /Free
       RPeftc = *Blanks;
      /End-Free
      *******End Chg********04/17/12**********************************
     C                   WRITE     PRP01
     C                   ENDSL
     C     TXEDT         TAG
     C                   SETOFF                                       59
      *
      *  IF THE CLAIM IS CLOSED, CALL SRPRCC TO ALLOCATE THE RESERVES
      *  FOR THIS RELEASED PAYMENT.
      *
     C     RPSTAT        IFEQ      *BLANK
     C     CMSTAT        ANDEQ     'C'
     C                   MOVE      'R'           QQTYPE
     C                   CALL      'SRPRCC'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    XXAMT                          Total Amount
     C                   ENDIF
      *
      *  Only perform the following if EDI A49s are being processed
      *  for the claim's jurisdiction state.
      *
      *
      *  Call the FL EDI State Maint screen if this is the first comp
      *  pmt and the claim's jurisdiction state is FL.
      *
      *
      *  Determine if this was the first comp payment.
      *
      *
      *
      *
     C*
     C*   WRITE THE DARTS REMARK IF SPECIFIED
     C*
     C     SCDRT1        IFEQ      'Y'                                          NEST-B
     C     SCDRT2        OREQ      'Y'
     C     X             ANDEQ     SCCHKS
     C*
     C*  INITIALIZE HOLD FIELDS
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVE      *BLANKS       XXCDAT            8 0
     C                   MOVE      *BLANKS       QCCOM
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      *BLANKS       XXSTAF
     C*
     C*  GET REMARK CODE FROM COMP PMT CONTROL FILE
     C*
     C     SCDRT2        IFEQ      'Y'
     C     X             ANDEQ     SCCHKS
     C                   MOVEL     PCRCLP        XXRMCD
     C                   ELSE
     C                   MOVEL     PCRCEP        XXRMCD
     C                   END
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         CABEQ     '1'           NODRT
     C*
     C*  THE REMARK DATE EQUALS THE POSTING DATE MINUS THE # OF DAYS
     C*  SPECIFIED IN THE COMP PMT CONTROL FILE.
     C*
     C                   MOVE      XXPODT        SRDTE             8 0
     C                   Z-ADD     PCDIDY        SRDAYS
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'                            98
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM            1
     C                   MOVE      SRDTE         XXCDAT
     C*
      *** Beg change *** 01/12/16 ****************************************
     C***If Calculated Darts remark date falls on Weekend SAT or SUN. In that case,
     C*** program will make that date fall on Week days.
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDTE             8 0          M/D/YY
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR                          Error Flag
     C
     C                   IF        SRSDAY = 'SAT.' Or SRSDAY = 'SUN.'           SatSunIf
     C
     C                   IF        SRSDAY = 'SAT.'
     C                   Z-ADD     1             SRDAYS
     C                   ElSEIF    SRSDAY = 'SUN.'
     C                   Z-ADD     2             SRDAYS
     C                   ENDIF
     C**** Recalculate the Darts dairy date again.
     C
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'                            98
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM            1
     C                   MOVE      SRDTE         XXCDAT
     C
     C                   END                                                    SatSunE
      *** End change *** 01/12/16 ****************************************
     C*  GET DIARY USER
     C*
     C                   MOVE      CMCLEX        XXSTAF
     C     XXSTAF        CHAIN     PUP02                              60
     C  N60              MOVEL     UPUSR         XXUSER
     C     *IN60         CABEQ     '1'           NODRT
     C*
     C** CREATE DARTS DIARY REMARK
     C*
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     RPFND         QCFND
     C                   Z-ADD     RPFYR         QCFYR
     C                   Z-ADD     RPCASE        QCCASE
     C*
     C*  GET NEXT SEQUENCE NUMBER FOR DARTS RECORD
      *
      *
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  XXUSER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZERO         Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVE      *BLANKS       Q#FOLD           80
     C                   MOVE      'Y'           Q#DIFL            1
     C                   MOVE      *BLANKS       Q#FL01            1
     C                   MOVE      *BLANKS       Q#FL02            1
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVE(P)   XXCDAT        Q#CDAT            8
     C                   MOVE(P)   *BLANKS       Q#COM2           19
      *
     C                   MOVEL(P)  CTDESC        QCCOM1
     C                   MOVEL(P)  SCPNAM        QCCOM2
     C                   MOVEL(P)  QCCOM         Q#COMM          256
      *
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD
     C                   ELSE
     C                   MOVE(P)   *BLANKS       Q#FOLD
     C                   END
      *
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL01
     C                   END
      *
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           Q#FL02
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL02
     C                   END
      *
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
      *
     C*
     C*  Load the WARPP diary sent flag and diary seq#.
     C*
     C                   Z-ADD     RPSEQ#        XXSEQ#
     C     RPKY1         CHAIN     PRP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'Y'           RPSENT
     C                   MOVE      Q#SEQ#        RPDSEQ
     C                   UPDATE    PRP01
     C                   END
     C*
     C     NODRT         TAG
     C*
     C                   END                                                    NEST-B
     C*
     C                   END                                                    NEST-A
     C     CXDPPC        IFNE      *BLANK
     C     @@RTCD        ANDEQ     '1'
     C                   CALL      'WA061R'
     C                   PARM      DBRRN2        @@RRN             9 0
     C                   PARM      *BLANKS       ##RTCD
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCTRCD
     C                   MOVE      *BLANK        SCBR2
     C                   Z-ADD     0             SCFEIN
     C                   Z-ADD     0             SCMOD
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C  N99              GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*   THIS SUBROUTINE CALCULATES RPFRDT, RPTODT & RPPODT
     C*****************************************************************
     C     DATES         BEGSR
     C*
     C*  CALCULATE THE 'FROM DATE' - IF THIS IS THE FIRST TIME THRU THE
     C*  LOOP, THE FROM DATE EQUALS THE START DATE. OTHERWISE ADD 1 TO
     C*  THE LAST 'TO DATE' TO GET THE FROM DATE.
     C*
     C     X             IFEQ      1
     C                   Z-ADD     SCSTDT        XXFRDT            8 0
     C                   ELSE
     C                   Z-ADD     XXTODT        SRDATE
     C                   Z-ADD     1             SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM                    SRERR             1
     C                   Z-ADD     SRDATE        XXFRDT
     C                   END
     C*
     C*  CALCULATE THE 'TO DATE' - ADD THE WEEKS AND DAYS TO THE FROM
     C*  DATE TO GET THE 'TO DATE'.
     C*
     C                   Z-ADD     XXFRDT        SRDATE
     C     SCWKS         MULT      7             SRDAYS
     C                   ADD       SCDYWK        SRDAYS
     C                   SUB       1             SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   Z-ADD     SRDATE        XXTODT            8 0
     C*
     C*   THE POSTING DATE SHOULD EQUAL THE 'TO DATE', UNLESS IT FALLS
     C*   ON A SATURDAY OR SUNDAY. IN THAT CASE, THE POSTING DATE WILL
     C*   NEED TO BE THE FRIDAY BEFORE THE 'TO DATE'.
     C*
     C     XXTODT        IFNE      *ZERO
     C*
     C                   MOVE      XXTODT        SRDTE
     C*
     C*   SUBTRACT '# OF DAYS IN ADVANCE TO PRINT THE CHECKS' FROM
     C*   THE 'TO DATE'.
     C*
     C     PCPRTD        IFGT      0
     C                   Z-ADD     PCPRTD        SRDAYS
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'                            98
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C                   END
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDTE             8 0          M/D/YY
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR                          Error Flag
     C                   END
     C*
     C     SRSDAY        IFNE      'SAT.'
     C     SRSDAY        ANDNE     'SUN.'
     C                   MOVE      SRDTE         XXPODT            8 0
     C                   ELSE
     C     SRSDAY        IFEQ      'SUN.'
     C                   Z-ADD     2             SRDAYS
     C                   ELSE
     C                   Z-ADD     1             SRDAYS
     C                   END
     C                   MOVE      *BLANK        SRERR
     C                   MOVE      *BLANK        SRTERM
     C                   CALL      'SRPAST8'                            98
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM            1
     C                   MOVE      SRDTE         XXPODT
     C                   END
     C*
     C                   ENDSR
     C*                                                                *
     C******************************************************************
     C*  DUPCHK - Determine if a Dup Payment or Possible Dup Payment
     C******************************************************************
     C     DUPCHK        BEGSR
      *
      *  Check to see if Payment Request is a Possible Duplicate, if this
      *  is the First Time thru or Second Time thru and something changed
      *
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXFROM            8 0
      *
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7
     C                   Z-ADD     XX7           XXTO              8 0
      *
     C     51            SUBST     SCCS01:12     QQDFND                         Fund Name
     C                   MOVEL     S2NAME        QQDCNM                         Claimant Name
     C                   MOVEL     SCPNAM        QQDPNM                         Provider Name
      *
     C                   MOVEL     SCRECD        SVRECD
     C                   MOVE      SCRETP        SVRETP
     C                   Z-ADD     SCFEIN        SVFEIN
      *
     C                   MOVE      '0'           @@RTCD
      *
     C                   CALL      'WA019R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    XXSEQ
     C                   PARM                    XXFROM
     C                   PARM                    XXTO
     C                   PARM                    SCAMT
     C                   PARM                    SVRECD
     C                   PARM                    SVFEIN
     C                   PARM                    QQDFND           51            Fund Name
     C                   PARM                    QQDCNM           36            Claimant Name
     C                   PARM                    QQDPNM           36            Provider Name
     C     *IN90         PARM      *ZERO         I90               1
     C                   PARM                    SVRETP
     C                   PARM                    SCMOD
     C                   PARM                    @@RTCD            1
      *
      *  If *IN90 is *ON, a Duplicate record was found.
      *
     C     @@RTCD        IFEQ      '2'
     C     @@RTCD        OREQ      '3'
     C                   MOVE      '1'           SECLVL
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVE      *ON           DUPFND
     C                   ENDIF
      *
     C                   ENDSR
      *
      ******************************************************************
      *  ASGNCS - Subroutine to Assign Screen Group, Group Year, and Case
      ******************************************************************
     C     ASGNCS        BEGSR
      *
     C     ##RRN         CHAIN     WMCMP                              62
      *
     C     *IN62         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
      *
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE(P)   FNNAME        SCNAME
     C   60              MOVE(P)   *BLANKS       SCNAME
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      /Free
       //******************************************************************;
       // Clear function key lines for web display;
       //******************************************************************;
       Begsr ClearFKeys;
      /If Defined (WEB)
       Clear scFK1a;
       Clear scFK1b;
       Clear scFK2;
      /Endif
       Endsr;

       //******************************************************************;
       // Load function key lines for web display;
       //******************************************************************;
       Begsr LoadFKeys;
       fk1# = 0;
       fk2# = 0;
       Select;
       When *in04;
          fk1# = 4;
       Endsl;
       Select;
       When ##clt# = '139';
          fl1(1) = fk1x(fk1# + 1);
          fl1(2) = fk1x(fk1# + 2);
          fl1(3) = fk1x(fk1# + 3);
          fl1(4) = fk1x(fk1# + 4);
          fl2(1) = fk2x(fk2# + 1);
          fl2(2) = fk2x(fk2# + 2);
          fl2(3) = fk2x(fk2# + 3);
          fl2(4) = fk2x(fk2# + 4);
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
      *
     C*****************************************************************
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
**  Function Keys  SCR01
F3=Exit      F7=Financial summary   F8=Accident info    F9=Management info
F10=Search   F11=Benefit info       F13=Chg user parm   F24=More keys
F15=Remaining reserve   F16=User request search   F24=More keys

F3=Exit      F7=Financial summary   F8=Accident info   F9=Management info
F10=Search   F11=Benefit info       F12=Cancel         F24=More keys
F15=Remaining reserve   F16=User request search   F24=More keys

**  Function Keys  SCR02  *These may be replaced by AK2*
F3=Exit   F4=Additional Address   F7=Financial summary   F24=More keys
F8=Accident info   F9=Management info   F10=Search   F24=More keys
F11=Benefit info   F12=Cancel   F15=Remaining reserve   F24=More keys
F21=Change group provider   F24=More keys
**  Alternate Function Keys  SCR02  *These may replace FK2*
F3=Exit   F7=Financial summary   F8=Accident info   F24=More keys
F9=Management info   F10=Search   F11=Benefit info   F24=More keys
F12=Cancel   F15=Remaining reserve   F21=Change group provider   F24=More keys
**  Function Keys  SCR01 (Clt139)
F3=Exit      F7=Financial summary   F8=Accident info   F9=Management info
F10=Search   F11=Benefit info       F12=Cancel         F24=More keys
F16=User request search   F24=More keys

F3=Exit      F7=Financial summary   F8=Accident info    F9=Management info
F10=Search   F11=Benefit info       F13=Chg user parm   F24=More keys
F16=User request search   F24=More keys

**  Function Keys  SCR02  (Clt139)
F3=Exit   F4=Additional Address   F7=Financial summary   F24=More keys
F8=Accident info   F9=Management info   F10=Search   F24=More keys
F11=Benefit info   F12=Cancel   F24=More keys
F21=Change group provider   F24=More keys
