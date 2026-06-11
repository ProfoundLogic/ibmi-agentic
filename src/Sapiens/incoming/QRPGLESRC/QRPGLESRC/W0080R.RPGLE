     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W0080R - REQUEST PAYMENT TRANSACTION ENTRY
     H*****************************************************************
     H* NOTE: IF INSTALLING THIS PROGRAM FROM V10, INSTALL WA023R3 AND*
     H*       WMEFP/L*. IF INSTALLING AT EFT CLIENT, ENTIRE WC1806    *
     H*       MAY BE REQUIRED!                                        *
     H*                                                               *
     H* REVISIONS:                                                    *
     h*                                                               *
     h* 09/10/20 C:079 Project: WC2749                                *
     h*                Check to see if FAEAL1 open before %Open       *
     h*                                                               *
     h* 08/20/20 C:062 Project: WC2749                                *
     h*                Add EPay Processing                            *
     h*                                                               *
     h* 08/19/19 C:079 Project: WC2604                                *
     h*                Lump Sum indicator                             *
     h*                                                               *
     h* 04/10/19 C:062 Project: 152WAB                                *
     h*                Apply CSI mods for S1 Medical.                 *
     H*                                                               *
     H* 05/30/18 C:053 Project : WC2515                               *
     H*                Additional contact name change                 *
     H*                                                               *
     H*  07/12/18 C:079 Support: OIC 45978                            *
     H*                 Make the edit of EOB Amount to payment        *
     H*                 for all clients.                              *
     H*                                                               *
     H* 04/04/18 C:079 Support : OIC 70081                            *
     H*                Fix issue with opening FAEAL1                  *
     H*                                                               *
     H* 03/14/18 C:079 Support : OIC 45978                            *
     H*                Add edit to compare payment to EOB amount      *
     H*                                                               *
     H* 01/10/17 C:309 Support : 44263                                *
     H*                Adding error edit for user's security check    *
     H*                to use Payment type code.                      *
     H*                                                               *
     H* 07/27/15 C:212 Project : 114MTE                               *
     H*                Payment Authority Check for EOB & Payment      *
     H*                                                               *
     H* 07/23/15 C:136 Support: 36153                                 *
     H*                Added bank code to GetEftStatus procedure      *
     H*                                                               *
     H* 06/09/15 C:180 Support: 35247                                 *
     H*                Changed so that Override weeks/days windows    *
     H*                applies to Comp reserve type only.             *
     H*                                                               *
     H* 03/11/15 C:180 Support: 33610                                 *
     H*                Fix weeks/days calculated by Javascript        *
     H*                                                               *
     H* 08/04/14 C:300 Project: WC2247                                *
     H*                1) Added hidden field on SCR02 for company no. *
     H*                   scCO#, Job name, User name, Job number      *
     H*                2) Added onblur & onload property on Payment   *
     H*                   Value 1 field to call AJAX program W0080R01 *
     H*                3) Do not calculate Weeks/Days if exclude flag *
     H*                   for Payment value 1 is 'Y'                  *
     H*                                                               *
     H*  01/30/14 C:279 Redmine: 3877                                 *
     H*                 Inactivated function keys which will not be   *
     H*                 used from this screen.                        *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Add call to apportionment window for           *
     H*                cltid 129 only.                                *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Use GetBankcode instead of WA023R1          *
     H*                2) Use GetEFTStatus instead of WA023R3         *
     H*                3) Use New added field RPEFTC instead of       *
     H*                   RPCKTP.                                     *
     H*                                                               *
     H* 07/22/11 C:257 SUPPORT: MC133 77161                           *
     H*                PROGRAM GOES IN LOOP BECAUSE OF INVALID VALUE  *
     H*                STORED IN Q2RECF.                              *
     H*                                                               *
     H* 04/26/11 C:136 SUPPORT: WA025 76707                           *
     H*                CORRECT EFT EDIT FOR ADDITIONAL ADDRESSES      *
     H*                                                               *
     H* 02/16/11 C:220/079/198
     H*                PROJECT: 133MKH                                *
     H*                ADD PROCESSING FOR LUBA TO FORCE CHECK GEN ON  *
     H*                COMPELETING BILL.                              *
     H*                                                               *
     H* 01/05/11 C:257 PROJECT: 114WXM                                *
     H*                Process Invoice # Field on Claim Payment       *
     H*                request screen                                 *
     H*                                                               *
     H* 12/06/10 C:136 PROJECT: WC1806 PRISS 78                       *
     H*                CHECK EFT STATUS OF PAYEE IF PAYMENT IS        *
     H*                FLAGGED FOR EFT.                               *
     H*                                                               *
     H* 12/04/10 C:183 SUPPORT 74375                                  *
     H*                Full salary date edit override to be applicable*
     H*                only when payment type = 'CO'                  *
     H*                                                               *
     H* 11/09/10 C:227 PROJECT 133WJX                                 *
     H*                CMS edit:Users Chk/ORM and/or TPOC date        *
     H*                                                               *
     H* 09/14/10 C:180 SUPPORT 74191                                  *
     H*                Remove *IN38 error message in DSPF for state
     H*                since state is output only field. Use *IN38 for
     H*                SCLTCD error message to match pgm validation
     H*                of this field.
     H*
     H*                Do not stop expense payment even when
     H*                full salary is being paid.
     H*
     H* 07/13/10 C:235 Project: 114WWK                                *
     H*                Add Recovery Option to Subro Drop Down         *
     H*                                                               *
     H* 07/12/10 C:136 SUPPORT: WA025 73052                           *
     H*                PASS DEPENDENT SS# TO PRENOTE PROCESS IF       *
     H*                EFT IS BEING WRITTEN FOR ADDITIONAL ADDRESS    *
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
     H* 11/09/09 C:220 Project: 136MEP                                *
     H*                Negative fee adjust/ReConsiderations           *
     H*                - Just Compiled                                *
     H*                                                               *
     H* 11/05/09 C:220  Project: PRISS#9/WC1761                       *
     H*                 Pop up msg screen for compensation pmt only   *
     H*                 with full salary continued flag.              *
     H*                                                               *
     H* 06/29/09 C:191  Project: WC1806                               *
     H*                 UPDATE CALL TO WA023R3                        *
     H*                                                               *
     H* 05/04/09 C:062  Project: WC1807                               *
     H*                 IGNORE E TYPE WORK HISTORY ENTRIES            *
     H*                                                               *
     H* 12/25/08 C:197  Project: WC1761                               *
     H*                 Continued salary flag edit for TX             *
     H*                 Remove existing edit for TX.                  *
     H*                                                               *
     H* 06/24/08 C:191  Project: WC1453  PRISS #08                    *
     H*                 Check and send Pre-note transaction           *
     H*                                                               *
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Handle new edit for negative total paid on    *
     H*                 the reserve. Change in DSPF only              *
     H*                                                               *
     H* 12/05/07 C:193  Project: 135WKJ                               *
     H*                 Change Denied Claim Payment Security Edit     *
     H*                 Message added to W0080F (compile only).       *
     H*                                                               *
     H* 10/22/07 C:062 Project: MC129 58963                           *
     H*                Add tracking record.                           *
     H*                                                               *
     H* 08/09/07 C:191 Project: WC1453                                *
     H*                Add Changes to load dependent sequence # into  *
     H*                WARPP when an Additional Address record is     *
     H*                selected from WDACP. Also, the RPFEIN, RPMOD   *
     H*                and RPDSQ# will be passed to WA023R1 in the    *
     H*                "Data" field.                                  *
     H*
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Integrated changes from the original project   *
     H*                45MDF from client version's W0080R.            *
     H*                                                               *
     H* 06/04/07 C:33  Seq#: DR135/58583                              *
     H*                Do not use User Parm values after entry.       *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*
     H* 04/26/07 C:132 Project 133WEB                                 *
     H*                Add a Security Level for EXACT Dups and a      *
     H*                Separate one for POSSIBLE dups.                *
     H*
     H* 04/03/07 C:062 Project WC034 50703                            *
     H*                Correct how the cursor positions when coming   *
     H*                into screen.
     H*
     H* 02/23/07 C:185 Project: 114WMF                                *
     H*                Back to Base:114WBY dated 06/11/01 & 05/29/01, *
     H*                             114WER dated 07/02/03 & 06/12/03, *
     H*                             114WFO dated 06/20/03.
     H*
     H* 01/25/07 C:183 Project 114WLY                                 *
     H*                Added Exact Duplicate Indicator Parm to the    *
     H*                WA019R call and to update the payment as       *
     H*                pending if this parm returns as '1'            *
     H*
     H* 11/01/06 C:53  PROJECT: 114WMF   114WIH 04/12/05 **REMOVED**
     H*                Back to base - Block User Access to Claims
     H*                                                               *
     H* 09/05/06 C:15  Project: 139WES                                *
     H*                Added code to remove the "remaining reserves"  *
     H*                function from all screens.                     *
     H*                                                               *
     H* 06/14/06 C:58  WC1385: Correct cursor positions after FEIN    *
     H*                        and payment status searches.           *
     H*                                                               *
     H* 02/06/06 C:168 Project: 132WBU                                *
     H*                Changed Error message id in display file from  *
     H*                WC30041 -> WC30068                             *
     H*                                                               *
     H* 01/11/06 C:15  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 11/11/05 C:53  PROJECT: WC1309                                *
     H*                Allow date change to recalc days/wks
     H*                                                               *
     H* 09/21/05 C:53  PROJECT: WC1309                                *
     H*                Auto calculate the comp rate if amt 0
     H*                                                               *
     H* 04/07/05       PROJECT: 126WDK                                *
     H*          C:62  PROJECT: 126MAD                                *
     H*                -KEEP FROM USING SEARCH ON SCSTS IF PROTECTED. *
     H*                -DEFAULT FIELDS FROM WMCIP.                    *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*
     H*  2/03/05 C:53  Project: 132WBQ
     H*                Add warning for Method of Settlement.
     H*
     H*  3/22/04 C:58  Project: WC1147
     H*                Added SS# to SCR01.
     H*
     H* 10/20/03 C:53  Project: WC909
     H*                Added country info to processing.
     H*
     H* 08/29/02 C:132 SEQ#: WC967
     H*                Add edit for PV1 if is a "U", and FEIN is
     H*                not zero, then the WMPDP/PDTYPE value must
     H*                be a five, else display error
     H*
     H* 12/12/01 C: 53 PROJECT: WC1060
     H*                ADD EXCLUDE MEDICAL PROCESSING.
     H*
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*                                                               *
     H* 02/15/01 C:15  Project: WC577                                 *
     H*                Added code to subset the PV1 search to only    *
     H*                list codes which are valid for the reserve     *
     H*                code of the pmt.                               *
     H*                                                               *
     H* 11/02/00 C:79  Project: MC071 24471                           *
     H*                Change condition for returning to calling pgm  *
     H*                to be QQAMT LE 0.                              *
     H*                                                               *
     H* 10/25/00 C:15  Project: WC577                                 *
     H*                Added code to load 'MC' to RPIPGM when called  *
     H*                from Bill Entry.                               *
     H*                                                               *
     H* 09/05/00 C:62  Project: MC055 24204                           *
     H*                ADDED DUPLICATE MODIFIER PARM TO WA019R.       *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add Daily batch lockout process                *
     H*                                                               *
     H* 05/30/00 C:53  Project: WC991                                 *
     H*                Add parm to WA013R for edit # 12               *
     H*                                                               *
     H* 05/02/00 C:62  Support: WC021/22750                           *
     H*                ADD CONDITION TO EOMB FIELDS TO ONLY DISPLAY   *
     H*                IF MANAGED CARE IS INSTALLED.                  *
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                 2. Fix display of dates on screen 2 & 4
     H*                                                               *
     H* 04/25/00 C:15  Support: WC025/22607                           *
     H*                Added code to use the provider DBA flag to     *
     H*                deteremine which field is loaded into RPPNAM.  *
     H*                                                               *
     H* 04/12/00 C:15  Project: WC577                                 *
     H*                Added code to prevent negative reserves.       *
     H*                                                               *
     H* 03/24/00 C:62  PROJECT MC048 22221  ** IMPORTANT **           *
     H*                Add rcn to screen and add recon to parm list of*
     H*                WA013R AND WA014R.                             *
     H*                ** DO NOT INSTALL THIS PROGRAM WITHOUT WA013R, *
     H*                   WA014R, FA015R AND WARPLA **                *
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                Added code to load new maint fields added to   *
     H*                WARPP.                                         *
     H*                                                               *
     H* 11/09/99 C:58  Project:  SUPP WC090   Seq 20551
     H*                Standardize name arrangement all programs which
     H*                create payment request record.
     H*                                                               *
     H* 08/27/99 C:56  Project: MC152                                 *
     H*                Protect certain fields when calling from prov. *
     H*                bills.                                         *
     H*                                                               *
     H* 07/26/99 C:56  Project: MC208                                 *
     H*                Do not allow F4=Additional Address when called *
     H*                by Provider Bills (F0393R).                    *
     H*                                                               *
     H* 07/21/99 C:112 Support: 99-WC577                              *
     H*                Add display for AWW and Comp rate              *
     H*                Position Cursor to amount field and not the    *
     H*                Post Date field.                               *
     H*                Remove edit for Address fields since it is     *
     H*                display only.                                  *
     H*                Calculate weeks/days only once.  If field is   *
     H*                cleared do not recalculate it.                 *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*
     H*  03/18/99 C:56 PROJECT: MC152
     H*                 1. Add Recon number as a PARM
     H*
     H*  06/03/99 C:58 SUPP: WC051  12246
     H*                Use WMWHL Work history for RTW date.
     H*
     H*  03/02/99 C:33 SEQ#: WC095/13271
     H*                PASS THE RESERVE TYPE TO WA019R.
     H*
     H*  02/12/99 C:31  Project: 81MOZ
     H*                 Pass Dt Rcvd to Chk Rqst
     H*                 1. Add Date Received parm (passed from F0086R)
     H*
     H*  09/24/98 C:78 Project: 68-SUPP
     H*                FIX ACCES TO DARTS. ADD IT IN SCR02 TOO.
     H*
     H* 09/06/98 C:02 PROJECT: 48WLU
     H*               MOVED CALL TO W0070RFL TO PRIOR TO DSIPLAYING
     H*               SCR02.  CALL WAS AFTER PAYMENT REQUEST WAS
     H*               ENTERED.
     H*
     H* 08/27/98 C:15 Support: WC082/13469
     H*               Fixed W0080F to use the correct indicators for
     H*               errors on the remark codes (33,34,35).
     H*
     H* 07/16/98 C:33 PROJECT: WC833
     H*               ALLOW FOR F4=ADDITIONAL ADDRESS SELECTION ON
     H*               SCR02 FOR OTHER THAN COMPENSATION PAYMENTS
     H*
     H*  06/24/98 C:78 Project: 68-SUPP
     H*                ADD F14=DARTS TO SCR01
     H*
     H*  05/26/98 C:53 Project: MC158
     H*                EOMB CHECK MISMATCH COPY MODULE ADDED
     H*
     H*  05/19/98 C:15 Project: WC577
     H*                The EOB# on SCR02 was not being cleared and it
     H*                was not being loaded into RPEOB#.
     H*
     H*  04/24/98 C:15 SEQ#: WC078/11307
     H*                Changed usage of a parm received from SRDDIF8.
     H*
     H*  03/31/98 C:33 SEQ#: WC095/10663
     H*                DO NOT SHOW F4 IF NOT COMP PAYMENT.
     H*
     H*  03/30/98 C:85 Project: WC577
     H*                V3 STANDARDS.  ALSO SCREEN 2 CHANGES.
     H*
     H*  02/23/98 C:85 Project: WC577
     H*                CHANGED THE PROGRAM MOVED TO CMPNAM
     H*                FROM W0080R TO W0070R.
     H*                CHANGED THE FIELD MOVED TO CMSMCD
     H*                FROM *BLANKS TO CTSMCD.
     H*
     H*  01/15/98 C:15 Project: WC659
     H*                Per JJ, removed the edit which requires the
     H*                date of disability to exist for a comp pmt
     H*                to be processed.
     H*
     H*  01/02/98 C:15 Project: WC659
     H*                Chgd pgm to default the posting date to the
     H*                system date.
     H*
     H*  10/09/97 C:15 PROJECT: WC659
     H*                Fixed code to comply with specs.
     H*
     H*  06/12/97 C:56 PROJECT: WC468
     H*                ADDED A FLAG TO SCR02 TO ALLOW THE USER TO
     H*                INDICATE THAT A PAYMENT REQUEST IS THE LAST
     H*                COMP PAYMENT ON A CLAIM.  THIS WILL TRIGGER
     H*                THE EDI SYSTEM TO SEND AN A49 'FN' TRANSACTION.
     H*
     H*  05/15/97 C:62 PROJECT WC640
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;
     H*               "ADJUSTER" TO CXADJ.
     H*
     H*  05/13/97 C:62 PROJECT WC674
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N
     H*
     H*  04/15/97 C:30 Project: WC607
     H*                Change F7 PAYEE SRCH program from W0531R to
     H*                W0030R on SCR01.  Move Address Line 2 from
     H*                Provider to WARPP Address Line 2.
     H*
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.
     H*    -------------------------------------------------------
     H*****************************************************************
     H*                                                               *
     H*  "Before" Version 3                                           *
     H*                                                               *
     H*  09/18/92 LPK  PROJECT: WC175                                 *
     H*  03/30/93 C:11 PROJECT WC219                                  *
     H*  07/16/93 C:22 PROJECT 99WC1                                  *
     H*  09/08/93 C:10 PROJECT WC261                                  *
     H*  10/07/93 C:15 PROJECT: WC258                                 *
     H*  06/03/94 C:25 PROJECT: WC330                                 *
     H*  09/06/94 C:15 PROJECT: WC361                                 *
     H*  02/22/95 C:15 PROJECT: WC361                                 *
     H*  03/06/95 C:15 PROJECT: WC361                                 *
     H*  12/14/95 C:30 Project: WC457                                 *
     H*  01/09/96 C:46 PROJECT: WC411                                 *
     H*  08/21/96 C:56 PROJECT WC371                                  *
     H*  09/30/96 C:30 Project: WC583                                 *
     H*  10/25/96 C:62 PROJECT: WC295                                 *
     H*  11/01/96 C:62 PROJECT: WC450                                 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   04       "RETURN" ON COMMAND 01 - DISPLAY SCREEN            *
     H*            PROTECT FIELDS COMING FROM FSMS                    *
     H*  N04       "END OF JOB" ON COMMAND 01 - DISPLAY SCREEN        *
     H*   70       TO CONTROL PROTECT ATTRIBUTE OF SCSTS              *
     H*****************************************************************
      *  Display file
     FW0080F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
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
      *  Claim Master
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *_________________________________________________________________
      *  Claim Master
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCMRRN)
      *_________________________________________________________________
      *  Claimant Demographics
     FWMCDL     IF   E           K DISK
      *_________________________________________________________________
      *  Table file
     FWTABL     IF   E           K DISK
      *_________________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
      *_________________________________________________________________
      *  Fund Auxiliary
     FWDF1L     IF   E           K DISK
      *_________________________________________________________________
      *  Request for Payment
     FWARPL     UF A E           K DISK
     F                                     INFDS(DBFDS2)
      *_________________________________________________________________
      *  Claim/DARTS Control
     FWMCXL     IF   E           K DISK
      *_________________________________________________________________
      *  Payment Detail file              KEY = CO#/FND/FYR/CASE/seq#
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
      *_________________________________________________________________
      *  Group Provider
     FWMNRL     IF   E           K DISK
      *_________________________________________________________________
      *  Payment Status Table
     FWTPSL     IF   E           K DISK
      *_________________________________________________________________
      *  Additional Address
     FWDACL     IF   E           K DISK
     FWDACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F                                     INFDS(DBFDSA)
      *________________________________________________________________
      ***DARTS Remarks
      *________________________________________________________________
      ***DARTS Remarks (Obtain Seq#)
      *________________________________________________________________
      *  DARTS Remark Code
     FSTCTL1    IF   E           K DISK
      *________________________________________________________________
      *  EDI Control File
     F*_________________________________________________________________
     F*  Work History File
     FWMWHL     IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMLLL     IF   E           K DISK
     FWARDP     O    E             DISK
     Ffaeal1    if   e           k Disk    UsrOpn
     D FL              S              1    DIM(13)
     D KEYY            S              3  0 DIM(2)
     D KEYC            S              7  0 DIM(2)
     D KEYS            S              5  0 DIM(2)
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D MSG             S             46    DIM(4) CTDATA PERRCD(1)
     D RECD            S              2    DIM(999)
     D RETP            S              1    DIM(999)
     D REM             S             11  2 DIM(999)
     D AMT             S             11  2 DIM(999)
     D FK1             S             79    DIM(8) CTDATA PERRCD(1)              SCR01 function Key
     D FK2             S             79    DIM(4) CTDATA PERRCD(1)              SCR02 function Key
     D AK2             S             79    DIM(3) CTDATA PERRCD(1)              Alt SCR02 function K
     D I6              S              6    DIM(63)                              Indicator Values
     D FK1X            S             79    DIM(8) CTDATA PERRCD(1)              SCR01 function Key
     D FK2X            S             79    DIM(4) CTDATA PERRCD(1)              SCR02 function Key
     D AK2X            S             79    DIM(3) CTDATA PERRCD(1)              Alt SCR02 function K
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
     D FL6             S             78    DIM(14)                              Function lines
     D FL7             S             78    DIM(14)                              Function lines
     D FL8             S             78    DIM(14)                              Function lines
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D fk1#            S              3  0                                      Indicator Values
     D fk2#            S              3  0                                      Indicator Values
      *
     D ##OFD           DS          3500
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
     D                 DS
     D  QDKEY                  1     40
     D  QDCO#                  1      3  0
     D  QDFND                  4      6  0
     D  QDFYR                  7      9  0
     D  QDCASE                10     16  0
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
     D  XXCOMM                 1    158
     D  SCDS01                 1     79
     D  SCDS02                80    158
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
      *
     D CPRDS           C                   CONST('            Payment -
     D                                     Request')
     D NF              C                   CONST('NOT FOUND')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     d/copy sprModule
     D/copy WPRBANK
     d ##Appl          s              2
     d ##Data          s             75
     d ##Cktp          s             10
     d #rtcd           s              1
     d ##Eftc          s              1
     d ##Eft           s              1                                         06
     d ##Pre           s              1                                         07
     d ##ptyp          s              1                                         02
     d #ekey           s             30                                         04
     d ##Date          s              7                                         05
     d ##outRRN        s              9                                          07
     d ##EobType       s              9                                          07
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
     d jsWKS           S                   LIKE(PAWKS)
     d jsDYS           S                   LIKE(PADAYS)
      *
     IPCM01
     I              CMSTAT                      C2STAT
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
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
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#             9
     C*
     C     CIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*
     C*   KEY LIST FOR THE REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  AUXILIARY COMPENSATION PAYEE FILE
     C*
     C     ACKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  Key list for the EDI Control file
     C*
     C     IXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
     C*
     C*
     C*  Key list for the Work History file
     C*
     C     WHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    XXWKDT
     C*                                                                *
     C*
     C     WHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*  EOB Payment Detail
     C*
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
      *
     C*
     C     *LIKE         DEFINE    DUPLI         CRSRFL
     C     *LIKE         DEFINE    SCTODT        XXFROM
     C     *LIKE         DEFINE    SCTODT        XXTO
     C     *LIKE         DEFINE    RPSEQ#        SVSEQ#
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPSEQ#        KKSEQ
     C     *LIKE         DEFINE    RPAMT         SVAMT
     C     *LIKE         DEFINE    RPFRDT        SVFRDT
     C     *LIKE         DEFINE    RPTODT        SVTODT
     C     *LIKE         DEFINE    RPFEIN        SVFEIN
     C     *LIKE         DEFINE    RPRECD        SVRECD
     C     *LIKE         DEFINE    RPRETP        SVRETP
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    WHDATE        XXFRDT
     C     *LIKE         DEFINE    WHDATE        XXWKDT
     C     *LIKE         DEFINE    CMRTW         XXRTW
     C     *LIKE         DEFINE    CIPTYP        KKPTYP
     C     *LIKE         DEFINE    CISYS         KKSYS
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    Q2RECF        X1RECF
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQAMT             9
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQPNAM           40
     C                   PARM                    QQTRCD            2
     C                   PARM                    @@KC              1
     C                   PARM                    QQRCVD            7
     C                   PARM                    QQRCN             3 0
     C                   PARM                    QQSYS             1
     C                   PARM                    QQBTYP            1
      *
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
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD
     C     IXAPI         PLIST
     C                   PARM                    KKCO#
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
      *
      * _________________________________________________________________
      *
      /copy SINDIC01
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
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
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##MCIF        IFEQ      'Y'
       //*** Beg Chg *** 09/10/20 ******************************
     C***************    Open      faeal1
       If Not %Open(FAEAL1);
         Open FAEAL1;
       Endif;
       //*** End Chg *** 09/10/20 ******************************
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
      *
     C* Retrieve client ID.
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
      *
     C                   MOVE      QQAMT         XXAMT
     C     QQCALL        IFEQ      'I'
     C     XXAMT         ANDLE     *ZEROS
     C                   RETURN
     C                   ENDIF
      *
     C                   MOVE      QQCALL        XXCALL            1
     C                   MOVE      *OFF          *IN13
      *
     C     QQFYR         IFNE      *BLANK
     C     QQFYR         ANDNE     *ZERO
     C     QQCASE        ANDNE     *BLANK
     C     QQCASE        ANDNE     *ZERO
     C                   SETON                                        04
     C                   Z-ADD     1             CK                3 0
     C                   ELSE
     C                   SETOFF                                       04
     C                   Z-ADD     5             CK
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
      *
      *  GET THE USER PARAMETER FILE
      *
     C     TOP           TAG
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
      *
     C     *IN04         IFEQ      *OFF
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPFYR         SCFYR
     C                   ELSE
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   ENDIF
      *
     C                   Z-ADD     UPCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      /free
       // Populate company number on SCR02 screen
       scCo# = kkCo#;
      /End-Free
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
      *
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C                   MOVE      *OFF          *IN53
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
     C                   MOVEL     CXADJ         SCADJ            19
     C                   ELSE
     C                   MOVEL     XXADJ         SCADJ
     C                   ENDIF
      *
      *  Assign Claim Heading
      *
     C     *IN04         IFEQ      *ON
     C                   MOVE      QQFND         KKFND
     C                   MOVE      QQFYR         KKFYR
     C                   MOVE      QQCASE        KKCASE
      *
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
     C     *IN04         IFEQ      *ON
     C                   MOVEL     CPRDS         SCPRDS
     C                   ELSE
     C                   MOVE      QQPRDS        SCPRDS
     C                   ENDIF
      *
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVE      *ZERO         SCFEIN
     C                   MOVE      *ZERO         SCMOD
     C*
     C                   MOVE      QQEOB#        XXEOB#            9 0
     C                   MOVE      *BLANKS       XXDSQ#
      ** position cursor to Amount field
      **
     C                   MOVE      'Y'           AMTPOS            1
     C                   SETON                                        69
     C                   MOVE      'N'           POTPOS
     C*
     C     QQCALL        IFEQ      'I'
     C                   MOVE      *ON           *IN13
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQEOB#        SCEOB#
     C     Q1PRMS        IFGE      16
     C                   Z-ADD     QQRCN         SCRCN
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRCN
     C                   ENDIF                                                  Q1PRMS
     C     Q1PRMS        IFGE      17
     C*
     C                   MOVEL(P)  'P'           KKPTYP
     C                   MOVEL(P)  QQSYS         KKSYS
     C*
     C     CIKY          CHAIN     PCI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVEL(P)  CITRCV        SCTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCD        SCTRCD
     C                   ENDIF
     C*
     C                   MOVEL     CISTS         SCSTS
     C*
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              62
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        SCBR2
     C                   ELSE
     C     QQBTYP        IFEQ      'X'
     C                   MOVE      CIXBR2        SCBR2
     C                   ELSE
     C                   MOVE      CIBR2         SCBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        SCBR7
     C                   ELSE
     C                   MOVE      CIBR7N        SCBR7
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   MOVEL     QQPNAM        SCPNAM
     C                   SETOFF                                       69
     C                   MOVE      'N'           AMTPOS
     C                   MOVE      'Y'           POTPOS            1
     C                   GOTO      SCR01T
     C                   ENDIF                                                  END QQCALL=I
     C     CLTID         IFEQ      '045'
     C     SCSTS         ANDNE     *BLANKS
     C                   SETON                                        70
     C                   ENDIF                                                  CLTID = '045'
      /Free
                         If isModEnabled('IDC');
                            isIDC = *on;
                         Else;
                            isIDC = *off;
                         Endif;
                         erLumpSum = *off;
      /End-Free
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C                   MOVE      *BLANKS       XXFKEY            2
     C     STRT          TAG
     C*
     C*
     C*
     C     S2SS1         IFNE      *BLANK
     C     S2SS2         ORNE      *BLANK
     C     S2SS3         ORNE      *BLANK
     C                   MOVEL(P)  S2SS1         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       S2SS2:0       SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       S2SS3:0       SCSS#
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '133'
     C     Q2RECF        ANDEQ     'SCR08'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     '1'           FLSCR8            1
     C                   MOVEA     '000000'      *IN(71)
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR01'
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR01
     C                   CLEAR                   XXDSQ#
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C* New invoice field will display only for client 114, when payment
     C* reserve type is expence,legal or vocational rehab.
     C     CLTID         IFEQ      '114'
     C     CLTID         OREQ      '152'

     C     RTTYPE        IFEQ      'E'
     C     SCFEIN        ANDNE     *ZEROS
     C*
     C     RTTYPE        OREQ      'L'
     C     SCFEIN        ANDNE     *ZEROS
     C*
     C     RTTYPE        OREQ      'M'
     C     PCRECD        ANDEQ     'RE'
     C     SCFEIN        ANDNE     *ZEROS
     C*
     C                   SETON                                        89
     C     CRSRFL        IFNE      '1'                                          CURSOR FLAG
     C                   SETON                                        64
     C                   MOVE      '1'           CRSRFL
     C                   ELSE
     C                   SETOFF                                       64
     C                   ENDIF                                                  CRSRFL
     C                   MOVE      '1'           DUPLI             1
     C                   ENDIF                                                  CLTID = '114'
     C                   ENDIF                                                  CLTID = '114'
      ** turn off position indicator
     C**
     C     AMTPOS        IFEQ      'Y'
     C                   SETON                                        87
     C                   MOVE      'N'           AMTPOS
     C                   ELSE
     C                   SETOFF                                       87
     C                   ENDIF
     C     POTPOS        IFEQ      'Y'
     C*
     C* Set Position based upon what fields are blank
     C*
     C                   SELECT
     C     SCSTS         WHENEQ    *BLANKS
     C                   SETON                                        54
     C     SCBR2         WHENEQ    *BLANKS
     C                   SETON                                        40
     C     SCBR7         WHENEQ    *BLANKS
     C                   SETON                                        41
     C     SCTRDS        WHENEQ    *BLANKS
     C                   SETON                                        51
     C     SCRMK1        WHENEQ    *BLANKS
     C     *IN53         ANDEQ     *OFF
     C                   SETON                                        46
     C     SCRMK2        WHENEQ    *BLANKS
     C     *IN53         ANDEQ     *OFF
     C                   SETON                                        44
     C     SCRMK3        WHENEQ    *BLANKS
     C     *IN53         ANDEQ     *OFF
     C                   SETON                                        45
     C     SCRMDS        WHENEQ    *BLANKS
     C                   SETON                                        47
     C                   ENDSL
     C                   MOVE      'N'           POTPOS
     C                   ELSE
     C                   SETOFF                                       40
     C                   ENDIF
     C     INVBR2        IFEQ      'Y'
     C                   MOVE      *BLANKS       INVBR2
     C                   MOVE      *ON           *IN31
     C                   ENDIF
      *
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR02
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF                                                  SRERR <> '1'
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF                                                  SRERR <> '1'
     C                   ENDIF
     C*
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
      *
      * =======================================================
      *   SCR01 - If F7, F8, F9, F11, F15, or F16, then
      *           use screen fields and attempt claim master access
      * =======================================================
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
      *
     C     *INKG         IFEQ      *ON
     C     *INKH         OREQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKK         OREQ      *ON
     C     *INKP         OREQ      *ON
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
      *
     C     CMKY          CHAIN     PCM01                              62
      *
     C     *IN62         IFEQ      *ON
     C                   MOVEA     I6(6)         CH6                            Error #6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
      *
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
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      Q2RECF        X1RECF
     C*
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
      *
     C     CMKY          CHAIN     PCM01                              62
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM      'F0393R'      #PNAME           10
     C                   PARM      '0'           #YES              1
     C*
     C     #YES          IFEQ      '1'
     C     CMLTFL        ANDNE     'R'
     C     QQCALL        ANDEQ     'I'
     C     CLTID         ANDEQ     '133'
     C*
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C     FLSCR8        IFEQ      ' '
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl8
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR08
     C                   ELSE
     C                   MOVEL     ' '           FLSCR8
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C     X1RECF        ANDEQ     'SCR02'
     C                   MOVE      *INKL         @@KC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C     X1RECF        ANDEQ     'SCR01'
     C                   MOVE      *INKL         @@KC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C                   ENDIF
      *
     C* =======================================================
     C*   F3 = EXIT
     C* =======================================================
     C*
     C     *INKC         IFEQ      '1'
     C     QQCALL        IFEQ      'I'
     C                   EXSR      WRTRDP
     C                   ENDIF
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* =======================================================
     C*   F12 = CANCEL
     C* =======================================================
     C*
     C     *INKL         IFEQ      '1'
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     QQCALL        IFEQ      'I'
     C                   EXSR      WRTRDP
     C                   ENDIF
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *OFF          *IN80
     C                   MOVE      *OFF          *IN81
     C                   MOVE      *OFF          *IN84
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   END
      *
      * =======================================================
      *  F4 - Additional Address
      * =======================================================
      *
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
     C                   MOVEL     ACPNAM        SCPNAM
     C     ACFNAM        IFNE      *BLANK
     C     ACFNAM        CAT(P)    ACPNAM:1      SCPNAM
     C                   ENDIF
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACADR2        SCADR2
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACST          SCST
     C                   MOVEL     ACZIP         SCZP1
     C                   MOVE      ACZIP         SCZIP2
     C                   MOVE      ACSEQ#        XXDSQ#            5 0
     C                   MOVE      *BLANKS       SCADR3
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
     C*
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   END
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
     C                   MOVE      KKCO#         ##CO#             3
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
     C                   MOVEA     '0000'        *IN(82)
     C*
     C     *INKJ         IFEQ      '1'
      *
     C                   MOVE      HFLD          FIELD
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
     C                   ELSE
     C                   MOVE      *ON           *IN79
     C                   ENDIF
      *
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
     C                   PARM                    ##RECD
     C                   PARM                    ##FND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCBR2
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C                   MOVEA     '01'          *IN(40)
     C                   MOVE      *BLANK        SCDBR2
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2DESC        SCDBR2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN40
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C     HFLD          WHENEQ    'SCEOB#'
     C     HFLD          OREQ      'SCRCN'
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKFND         #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C                   MOVE      #1SUB#        #2SUB#
     C                   MOVE      #1FND         #2FND
     C                   MOVE      SCFEIN        #2FEIN
     C                   MOVE      SCMOD         #2MOD
     C*
     C                   CALL      'F0350R'                             99
     C                   PARM      '2'           #2CALL            1
     C                   PARM                    #2SUB#            5 0
     C                   PARM                    #2FND             3 0
     C                   PARM      SCFYR         #2FYR             3 0
     C                   PARM      SCCASE        #2CASE            7 0
     C                   PARM      *ZEROS        #2EOB#            9 0
     C                   PARM      *BLANKS       #2CERT           15
     C                   PARM      *BLANKS       #2SFDT            8
     C                   PARM      *BLANKS       #2STDT            8
     C                   PARM                    #2FEIN            9
     C                   PARM                    #2MOD             3
     C                   PARM                    #2RCN             3 0
     C*
     C     #2EOB#        IFEQ      *ZEROS
     C*
     C                   SETON                                        32
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   SETON                                        54
     C                   Z-ADD     #2EOB#        SCEOB#
     C                   Z-ADD     #2RCN         SCRCN
     C                   GOTO      STRT
     C*
     C                   ENDIF
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
     C                   MOVE      *OFF          *IN41
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
     C     *IN69         ANDEQ     *ON
     C                   MOVE      *BLANK        ##KEY
     C                   MOVEA     '0'           *IN(96)                        ADD 12/12/01
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
     C                   MOVE      *ON           *IN88
     C                   MOVE      *BLANK        SCSDES
     C     TBKY          CHAIN     PPS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCSDES
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN54
     C                   MOVEA     '1101'        *IN(82)
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCLTCD'
     C                   MOVE      *BLANK        ##KEY
      *
     C                   CALL      'W2221R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCLTCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCLTCD        XXKEY
     C                   MOVEA     '1011'        *IN(82)                        Position Cursor
     C                   MOVE      *BLANK        SCDLTC
     C     TBKY          CHAIN     PLR01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     LRDESC        SCDLTC           30
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN29
     C                   MOVEA     '1100'        *IN(82)
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCPODT'
     C                   MOVEA     '0001'        *IN(82)
     C                   SETON                                        90
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
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVEA     '0011'        *IN(82)
     C                   SETON                                        90
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVEA     '0100'        *IN(82)
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
     C     HFLD          WHENEQ    'SCFNAL'
     C                   MOVEA     '1001'        *IN(82)
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
     C*  F14 - Darts
     C* =======================================================
     C*                                                    |
     C     *INKN         IFEQ      '1'
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QDKEY
     C                   Z-ADD     KKCO#         QDCO#
     C                   Z-ADD     SCFND         QDFND
     C                   Z-ADD     SCFYR         QDFYR
     C                   Z-ADD     SCCASE        QDCASE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QDKEY
     C                   PARM      'W0070R'      XXPGM
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKN='1'
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
      *   PROCESS HELP REQUESTS
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
     C*
     C                   CALL      'W0006R'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
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
     C     *IN69         IFEQ      *OFF
     C     CK2           IFGT      3
     C                   Z-ADD     1             CK2
     C                   ENDIF                                                  CK2>3/*IN12=*ON
     C                   MOVE      AK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   AK2X(CK2)     SCFK2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      FK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK2X(CK2)     SCFK2
     C                   ENDIF
     C                   ENDIF                                                  *IN69 = *OFF
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *__________________________________________________________________
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *
     C*=================================================================
     C*   SCR01T - PROCESS REQUEST SCREEN
     C*=================================================================
      *
     C     SCR01T        TAG
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD
     C     ##RTCD        CABEQ     'F'           TERM
      *
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C                   Z-ADD     SCCASE        XXCASE            7 0
      *
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C                   MOVEA     '0'           *IN(96)                        ADD 12/12/01
     C                   MOVE      '0'           @@RTCD
     C                   MOVE      '0'           @@RTCD
      *
      *
      *  Edit SCR01 Fields
      *
     C                   CALL      'WA011R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM      *BLANK        ##RC              2
      *
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA012R'
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
     C                   ENDIF
      *
      /Free
        erSecLvl = *Off;
        If ##RC = '48';
           erSecLvl = *On;
        EndIf;
      /End-Free
     C     ##RC          IFEQ      '15'
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl7
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR07
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
     C                   END
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
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      *BLANKS       XXMDBN
     C     CDKY          CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDMDBN        XXMDBN            1
     C                   ENDIF
     C     CLTID         IFEQ      '133'
     C     XXMDBN        ANDEQ     'Y'
     C     C2STAT        ANDEQ     'C'
     C                   CALL      'WA117R'
     C                   PARM      'C'           @@CALL            1
     C                   PARM                    @@KL              1
     C                   PARM                    @@YES             1
     C     @@KL          IFEQ      '1'
     C     @@YES         OREQ      ' '
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
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
      *
      *
     C     SVCASE        IFNE      SCCASE
     C*
     C                   Z-ADD     SCCASE        SVCASE            7 0
     C*
     C     *IN62         IFEQ      '0'
     C     C2STAT        ANDEQ     'C'
     C*
     C     CMKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C     CMJRST        CAT(P)    'MS':0        KKTBID
     C                   MOVEL(P)  LLMTST        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     '  MS'        KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     GTFG01        ANDEQ     'Y'
     C                   CALL      'WA720R'
     C                   PARM      '1'           KKMGID            1
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM      *BLANK        ##KL
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  Date of Disability must be in Claim Master if Comp payment
      *
      *  Initialize SVFDT & SVTDT for SCR02 Redisplay.
     C                   Z-ADD     *ZERO         SVFDT             8 0
     C                   Z-ADD     *ZERO         SVTDT             8 0
     C     XXCALL        IFEQ      'I'
     C                   SETOFF                                       69
     C                   ELSE
     C                   SETON                                        69
     C                   ENDIF                                                  XXCALL = 'I'
      *
      *  MOVE DEMOGRAPHIC DATA IF NO PAYEE ENTERED
      *
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '17'          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   END
      *
     C                   MOVEL     *BLANKS       SCCNAM           40
     C     CDFSN         CAT       CDMID:1       SCCNAM
     C                   CAT       CDLSN:1       SCCNAM
      *
     C                   MOVE      SCFEIN        CCFEIN            9
      *
     C     SCFEIN        IFEQ      *ZERO
     C     CCFEIN        OREQ      XXSS#
     C     XXCALL        IFNE      'I'
     C                   MOVEL     SCCNAM        SCPNAM
     C                   END
     C*
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C                   MOVE      *BLANKS       SCADR3
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
     C*
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   END
      *
     C                   ELSE
      *
      *  Access Payee Demographic record
      *
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
      *
     C     PDKY          CHAIN     PPD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C     XXCALL        IFNE      'I'
     C                   MOVEL     XXLAST        SCPNAM
     C                   END
     C**                   MOVELPDTITL    SCADR1
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
     C*
     C*  CLEAR FIELDS FOR SCREEN 2
      *
     C                   Z-ADD     SCFYR         KKFYR                          Fund Year
     C                   Z-ADD     SCCASE        KKCASE                         Claim
     C                   Eval      scLmpS = 'N'
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     PCDESC        SCTRDS
     C                   MOVE      *BLANK        SCSDES
     C                   MOVE      *BLANK        SCDBR2
     C                   MOVE      *BLANK        SCDBR7
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   MOVEL     *BLANKS       SCFRDT
     C                   MOVE      *BLANKS       SCTODT
     C                   MOVE      *BLANKS       SCWKS
     C                   MOVE      *BLANKS       SCDYWK
     C     XXCALL        IFNE      'I'
     C                   MOVEL     *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCBR7
     C                   ENDIF
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVEL     *BLANKS       SCAMT
     C                   MOVE      *BLANKS       SCRMK1
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCRMDS
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCLTCD
     C                   MOVE      *BLANKS       SCEOB#
     C                   MOVE      CMAWW         SCAWW
     C                   MOVE      CMCRT         SCRATE
     C                   MOVE      *OFF          DUPFL
     C     XXCALL        IFEQ      'I'
     C                   MOVE      QQFRDT        SCFRDT
     C                   MOVE      QQTODT        SCTODT
     C                   MOVE      QQAMT         SCAMT
     C                   MOVE      QQEOB#        SCEOB#
     C                   END                                                    END XXCALL=I
     C*
     C*          If received date passed: convert and move to display
     C*          ------------------------------------------------------
     C     Q1PRMS        IFGE      15
     C     QQRCVD        ANDNE     *BLANKS
     C                   MOVE      QQRCVD        XXRCVD            7 0
     C                   Z-ADD     XXRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         SCRCVD
     C                   ENDIF                                                  PRMS >= 15
     C*
     C*  ACCUMULATE REMAINING RESERVE AMOUNT
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C*
     C                   MOVE      '0'           *IN80
     C                   MOVE      *ZEROS        SVFRDT
     C                   MOVE      *ZEROS        SVTODT
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
     C                   MOVE      'N'           SCGRP
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
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
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
     C     F1KY          CHAIN     PF101                              60
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
     C     *IN69         IFEQ      *OFF
     C     CK2           IFGT      3
     C                   Z-ADD     1             CK2
     C                   ENDIF                                                  CK2>3/*IN12=*ON
     C                   MOVE      AK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   AK2X(CK2)     SCFK2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      FK2(CK2)      SCFK2
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK2X(CK2)     SCFK2
     C                   ENDIF
     C                   ENDIF                                                  *IN69 - *OFF
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
      *
     C*  Set indicators for use with SCR02 fields.
     C*
     C     RTTYPE        COMP      'M'                                    27
     C     RTTYPE        COMP      'E'                                    28
      *
     C     Q1PRMS        IFGE      15
     C     QQRCVD        IFEQ      *BLANKS
     C                   Z-ADD     0             SCRCVD                                     d
     C                   ENDIF                                                  RCVD = blanks
     C                   ELSE                                                   PRMS >= 15
     C                   Z-ADD     0             SCRCVD                                     d
     C                   ENDIF                                                  PRMS >= 15
     C     XXCALL        IFNE      'I'
     C                   MOVE      *BLANK        SCSTS                          Payment Status
     C                   ENDIF
     C                   MOVE      'N'           SCFNAL                         Final Comp Paymnt

     C                   If        cltid = '114'

     C*** call API to verify payment limit
     C                   CALL      'WA013R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCEOB#
     C                   PARM      *ZERO         RPSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    PCRECD
     C                   PARM                    RTTYPE
     C                   PARM                    ##RC
     C                   PARM                    SCAMT
     C                   PARM                    SCRCN
     C                   PARM                    SCTRCD                         ADD 05/30/00

      /Free
       // inititalize screen indicators
       erPayLimit = *off;
       msSts = *off;
       dspStsInd = *off;

       // verify return code value and set status accordingly
       If ##RC = '43';
          SCsts = 'PE';
          erPayLimit = *on;
          dspStsInd = *on;
          msSts = 'Payment amount exceeds the Claims Adjusters ' +
                  'authority limit';
       EndIf;
      /End-free

     C                   EndIf
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
     C     RPKY          CHAIN(N)  PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPRETP        IFEQ      'C'
     C     RPSTAT        ANDNE     'D'
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     RPKY          READE(N)  PRP01                                  60
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
     C                   MOVE      KKCO#         QQCO#
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
      *
     C                   SETON                                        50
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
      *********************************************************************
      * PROCESS SCREEN 2 - PAYMENT INFORMATION
      *********************************************************************
     C*
     C     SCR02T        TAG
      *
      *
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C                   MOVEA     '0'           *IN(96)                        ADD 12/12/01
     C                   SETOFF                                       50
     C                   Eval      erLumpSum = *off
     C*
     C*  Calculate Number of Days between "From" & "To" Service Dates
     C*  for Comp Payments Only.
     C*
     C                   MOVE      SCWKS         jsWKS
     C                   MOVE      SCDYWK        jsDYS
     C
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C                   IF        *IN60
     C                   MOVEA     '1111'        *IN(72)
     C                   GOTO      STRT
     C                   ENDIF
     C
     C                   IF        B2EXWDC = 'Y'
     C                   MOVE      *ZEROS        SCWKS
     C                   MOVE      *ZEROS        SCDYWK
     C                   ELSE
     C     RTTYPE        IFEQ      'C'

     C                   MOVE      *ON           *IN30
      *
     C                   Z-ADD     SCFRDT        QQBEG             8 0
     C                   Z-ADD     SCTODT        QQEND             8 0
      *
     C                   Z-ADD     *ZERO         XXDYS             5 0
     C                   MOVE      *BLANK        QQORES            1
     C                   MOVE      *BLANK        QQIRES            1
      *
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQBEG
     C                   PARM                    QQEND
     C                   PARM                    XXDYS
     C                   PARM                    QQORES
     C*
     C     DUPFL         IFEQ      *OFF
     C                   Z-ADD     *ZERO         SCWKS
     C                   Z-ADD     *ZERO         SCDYWK
     C                   ENDIF
     C*
     C     QQORES        IFEQ      *OFF
     C                   ADD       1             XXDYS
     C     XXDYS         DIV       7             SCWKS
     C                   MVR                     SCDYWK
     C                   ENDIF
      /Free

       // Display override window

       If jsWKS <> SCWKS or jsDYS <> SCDYWK;
          Dou SCWDOV = 'Y' or SCWDOV ='N';
             Exfmt WDW01;
             Select;
                When *inkl;
                     leave;
                When SCWDOV = 'Y';
                     SCWKS  = jsWKS;
                     SCDYWK = jsDYS;
                When SCWDOV = 'N';
                     *in55 = *off;
                Other;
                     *in55 = *on;
                     *in90 = *on;
             Endsl;
          Enddo;
       Endif;
       Q2RECF = *blanks;
       Q2RECF = %triml(Q2RECF) + 'SCR02';
      /End-free
     C   KL              GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      * If the PV1 from the table is a "U" and the FEIN is not zero
      * then it is an error, go write scr02 with the error msg.
     C                   MOVE      *BLANK        INVBR2            1
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C  N60B2PLCD        IFEQ      'U'
     C     SCFEIN        ANDGT     *ZEROS
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     PDTYPE        IFNE      '5'
     C                   MOVE      'Y'           INVBR2
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit Screen 2 fields
      *
     C*
     C*  PAYEE NAME CANNOT BE BLANK
     C*
     C*
     C*  ADDRESS CANNOT BE BLANK
     C*
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C*
     C*  THE STATE MUST BE VALID
     C*
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C*
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD
     C     ##RTCD        CABEQ     'F'           TERM
      *
      * BUILD ##EKEY AND PASS IT TO WA014 TO CHECK TO ENSURE THAT PAYEE
      * EFT IS SET UP FOR EFT PAYMENT. ##EKEY = PAYEE TYPE (C,D,P) CAT
      * KEY INFO FOR WMEL17
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
     C*
     C                   CALL      'WA014R'
     C                   PARM                    KKCO#                          Company
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
     C                   PARM                    SCLTCD                         Paid on Time
     C                   PARM                    SCDYWK                         Number of Days
     C                   PARM                    ##RC                           Return Code
     C                   PARM                    SCEOB#                         EOMB#
     C                   PARM                    SCAMT                          Payment Amount
     C                   PARM                    SCRCN
     C                   PARM                    ##EKEY           31
      *
     C     CLTID         IFEQ      '114'
     C     SCFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*
     C*** IF COMP PAYMENT
     C*
     C     SCRETP        IFEQ      'C'
     C*
     C     SCWKS         IFGT      0
     C     SCDYWK        ORGT      0
     C*
     C*** IF ERROR CODE = 39 - SCAMT <= 0 CHECK FOR AUTO COMP
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
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##AMT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMJRST        IFEQ      'TX'
     C     CMFSAL        ANDEQ     'Y'
     C     ##RC          ANDEQ     '42'
     C                   MOVE      *BLANKS       ##RC
     C                   ENDIF
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      /Free
                         if scLmpS = 'Y' and isIDC = *on and (scTrCd = 'CM'
                               or scTrCd = 'CP');
                            xxKey = scBr2;
                            Chain tbKy pb201;
                            If %Found(wtabl) and B2exwdc <> 'Y';
                               *in90 = *On;
                               erLumpSum = *On;
                               MsgLumpSum = 'Payment Value 1 code being -
                               used does not have the Exclude from weeks/-
                               days checked';
                            Endif;
                         Endif;
      /End-Free
     C     erLumpSum     Ifeq      *on
     C                   Goto      Strt
     C                   Endif
       // Compare payment amount with EOB amount
     C                   If        *in16 = *on
     C*
     C                   Move      kkCO#         #1co#
     C                   Move      kkfnd         #1wcf#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C                   MOVE      #1SUB#        #2SUB#
     C                   MOVE      #1FND         #2FND
     C*
     C                   Eval      kkSUB# = #2sub#
     C                   Eval      kkEOB# = scEOB#
     C                   Eval      kkRcn  = scRcn
     C*
     C     eaky1         Chain     pea01                              99
     C*
     C                   If        *in99 = *off and EApaid <> scAmt
     C*
     C                   Eval      msAmt = 'Payment does not equal EOB +
     C                                      paid amount.  Contact +
     C                                      software vendor'
     C                   eval      erAmt = *on
     C                   goto      strt
     C*
     C                   Endif
     C
     C                   Endif
     C     DUPLI         IFEQ      *ON
     C*
     C*  CALL WA081R TO CHECK FOR DUPLICATE PAYMENT
     C*
     C                   CALL      'WA081R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPSEQ#
     C                   PARM                    SCFEIN
     C                   PARM                    SCINV#
     C                   PARM                    XXFKEY
     C     *IN90         PARM      *ZERO         I90               1
     C*
     C* SCREEN SCR02 WILL BE RE-DISPLAYED *IN90 WILL BE ON IF DUPICATE PAYMENT
     C* FOUND
     C*
     C     *IN90         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      ' '           CRSRFL
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN90
     C*
     C                   ENDIF                                                  DUPLI
     C*
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
      *  Final Comp Payment flag
      *
     C     *IN12         IFEQ      *OFF
     C     SCFNAL        ANDNE     'Y'
     C     SCFNAL        ANDNE     'N'
     C                   MOVEA     I6(61)        CH6                            Error #61
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Access Payment Status table to obtain record status
      *
     C                   MOVEL     SCSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              61
     C     SCRETP        IFEQ      'M'
     C*  CHAIN TO GET THE EXCLUDE MEDICAL FLAG.
     C     ELKY          CHAIN     PEL01                              60
     C     ELEXMF        IFEQ      'Y'
     C     PSTYPE        ANDEQ     'R'
     C                   SETON                                        90  96
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *  If the Payment Status "Status" is blank ("Released"), & the Return Code
      *  from previous edit check is blank, execute Claim/Payment/Reserve Edits
      *
     C     PSTYPE        IFEQ      *BLANK
     C     PSTYPE        OREQ      'R'
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA013R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCEOB#
     C                   PARM      *ZERO         RPSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    PCRECD
     C                   PARM                    RTTYPE
     C                   PARM                    ##RC
     C                   PARM                    SCAMT
     C                   PARM                    SCRCN
     C                   PARM                    SCTRCD                         ADD 05/30/00
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
     C*  EDIT REMARK CODE 1
     C*
     C     SCRMK1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              33
     C  N33RCSTAT        COMP      'I'                                    33
     C     *IN33         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT REMARK CODE 2
     C*
     C     SCRMK2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              34
     C  N34RCSTAT        COMP      'I'                                    34
     C     *IN34         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT REMARK CODE 3
     C*
     C     SCRMK3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              35
     C  N35RCSTAT        COMP      'I'                                    35
     C     *IN35         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  Check to see if Payment Request is a Possible Duplicate. If this
     C*  is the First Time Thru or Second Time Thru and Something Changed
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
     C     FIRST         IFEQ      *ON
     C     XXFROM        ORNE      SVFRDT
     C     XXTO          ORNE      SVTODT
     C     SCAMT         ORNE      SVAMT
     C     PCRECD        ORNE      SVRECD
     C     SCFEIN        ORNE      SVFEIN
     C*
     C                   MOVE      *OFF          FIRST
     C                   MOVE      XXFROM        SVFRDT
     C                   MOVE      XXTO          SVTODT
     C                   MOVE      SCAMT         SVAMT
     C                   MOVE      PCRECD        SVRECD
     C                   MOVE      RTTYPE        SVRETP
     C                   Z-ADD     SCFEIN        SVFEIN
      *
     C     51            SUBST     SCCS01:12     QQDFND                         Fund Name
     C                   MOVEL     S2NAME        QQDCNM                         Claimant Name
     C                   MOVEL     SCPNAM        QQDPNM                         Provider Name
      *
     C                   MOVE      '0'           @@RTCD
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
      *  If *IN90 is *ON, a Duplicate record was found, re-display SCR02
      *
     C     @@RTCD        IFEQ      '2'
     C     @@RTCD        OREQ      '3'
     C                   MOVE      *ON           FIRST
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *ON           DUPFL             1
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *OFF          DUPFL             1
     C*
     C     ##AMT         IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        ##AMT
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
      *
      *  Compare Claimant Auxiliary Return to Work DTs against Service To DT
      *
     C     SCRETP        IFEQ      'C'
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
     C                   Z-ADD     XXFROM        XXFRDT
     C                   EXSR      CKRTW
     C*
     C*     Edit service date.
     C     XXRTW         IFGT      *ZERO
     C     XXRTW         ANDLT     XXTO
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  XXRTW>*ZERO
     C*
     C     *IN57         IFEQ      *ON
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         SCDATE
     C                   ENDIF                                                  . IN43 = ON
      *
     C     *IN42         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C     *IN57         OREQ      *ON
      *
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         SCRTWE
      *
     C                   Z-ADD     XXRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         SCRTW
      *
     C                   Z-ADD     SCTODT        SCTDT1
     C                   Z-ADD     SCTODT        SCTDT2
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl5
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR05
      *  Check for user response F12 = Cancel
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Determine if "Paid on Time" is not 'Y' and Detailed Reason
      *  required is 'Y'
      *
     C     SCLTCD        IFGT      *BLANKS
     C                   MOVEL(P)  SCLTCD        XXKEY
     C     TBKY          CHAIN     PLR01                              38
     C  N38LRSTAT        COMP      'I'                                    38
      *
     C     *IN38         IFEQ      *OFF
     C     LRREAS        ANDEQ     'Y'
     C                   EXSR      SCR04T
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     SCFRDT        SRDTI
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
     C*
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl6
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR06
     C*
     C     SCFSOV        IFNE      'Y'
     C     SCFSOV        ANDNE     'N'
     C                   SETON                                        5890
     C*
     C                   ELSE
     C                   SETOFF                                       58
     C*
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
     C                   SETON                                          59
     C*
     C                   ENDIF                                                  CMFSAL =  'Y'
     C*
     C                   ENDIF                                                  CMJRST    IFEQ 'TX'
     C*
     C*  Determine Next Sequence Number for Payment Request
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             XXSEQ             5 0
     C                   ELSE
     C     RPSEQ#        ADD       1             XXSEQ
     C                   END
      *
     C                   MOVE      KKCO#         RPCO#
     C                   MOVE      SCFND         RPFND
     C                   Z-ADD     SCFYR         RPFYR
     C                   MOVE      SCCASE        RPCASE
     C                   MOVE      XXSEQ         RPSEQ#
      *
      *  Assign Payment Request fields
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
     C*
     C                   MOVE      SCFEIN        RPFEIN
     C                   MOVE      SCMOD         RPMOD
      /Free
       xxkey = *Blanks;
       xxKey = scbr2;
       flgPSIG = *blanks;
       Chain TBky pb201;
       If %found(wtabl);
          flgPSIG = B2psig;
       EndIf;

       If CltId = '129' And flgPSIG = 'Y';

          intCO#   = kkCO#;
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
     C                   MOVE      SCBR2         RPBR2
     C                   MOVE      SCBR7         RPBR7
     C                   MOVE      SCNCCI        RPNCCI
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTODT
     C*
     C                   MOVE      SCWKS         RPWKS
     C                   MOVE      SCDYWK        RPDAYS
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
     C                   MOVE      *BLANKS       RPBUL1
     C                   MOVE      *BLANKS       RPCHK#
     C     QQEOB#        IFNE      *ZEROS
     C     QQEOB#        ANDNE     *BLANKS
     C                   MOVE      QQEOB#        RPEOB#
     C                   ELSE
     C                   Z-ADD     SCEOB#        RPEOB#
     C                   END
      *
     C     *IN24         IFEQ      *ON
     C                   MOVE      'G'           RPPGTP
     C                   ELSE
     C                   MOVE      *BLANK        RPPGTP
     C                   END
     C*
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
      *
     C                   MOVE      *BLANK        RPOFF#                         OFFICE#
      *
     C                   Z-ADD     *ZERO         RPRCVD                         Date Received
     C                   Z-ADD     *ZERO         RPDUE                          Due Date
      *
     C     SCRCVD        IFGT      *ZERO
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPRCVD                         Date Received
      *
      *  Access Payment Value 1 table to obtain # days to due date
      *
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
      *                                                    defaults
     C                   Z-ADD     RPRCVD        RPDUE                          Due Date
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     B2NDDT        ##NDDT            5 0          #Days to Due Date
     C                   Z-ADD     SCRCVD        ##DUE             8 0          Due Date
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DUE                          Due Date
     C                   PARM                    ##NDDT                         #Days to Due Date
     C                   PARM                    ##RTCD            1
      *
     C     ##RTCD        IFEQ      *OFF
     C                   Z-ADD     ##DUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDUE                          Due Date
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      KKCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      *ZEROS        QXSEQ#
     C                   MOVE      RPEOB#        QXEOB#
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      XXDSQ#        QXDSQ#
     C     XXDSQ#        IFNE      0
     C                   Z-ADD     *ZEROS        QXFEIN
     C                   Z-ADD     *ZEROS        QXMOD
     C                   ENDIF
      *
     C*
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = ' ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C* If EFT payee, then determine if pre-notification record is required
     C                   MOVE      *OFF          ##EFT                          EFT flag
     C                   MOVE      *OFF          ##PRE                          PRE note flag
      /Free
       If RPeftc = 'E';
      /End-Free
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
      /Free
       //Call the Procedure to retrieve EFT status.
       ##EFT = GetEFTStatus(##co#:##ptyp:#1fnd:#ekey:
        ##date:##pre:##OutRRN:##EobType:##cktp);

       If ##EFT = *Off;
          RPeftc = *Blanks;
       EndIf;
      /End-Free
     C*
     C                   ENDIF                                                  RPCKTP='EFT'
     C     CLTID         IFEQ      '114'
     C     RPCKTP        IFEQ      'CPARX'
     C                   MOVEL     'CPAMED'      RPCKTP
     C                   ENDIF
     C                   ENDIF
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
     C*
     C* IF REQUEST AMOUNT IS >= 5000 PUT IN PEND STATUS.
     C*
     C     RPFND         IFGE      100
     C     RPFND         ANDLE     115
     C*
     C     RPAMT         IFGE      5000
     C                   MOVE      'PE'          RPSTS
     C                   MOVE      'P'           RPSTAT
     C                   ENDIF
     C                   ENDIF
     C*
      *
     C                   MOVE      'N'           RPSENT                         Diary Sent Flag
     C                   Z-ADD     *ZERO         RPDSEQ                         Diary Sequence
     C                   MOVEL     Q1USER        RPRQST                         Initial Requestor
     C                   MOVE      SCLTCD        RPLTCD                         Pmt Late Code
     C                   MOVE      *BLANK        RPNTWK                         IN House Network Ser
     C     Q1PRMS        IFGE      16
     C                   Z-ADD     QQRCN         RPEOBS
     C                   ELSE
     C                   Z-ADD     SCRCN         RPEOBS
     C                   ENDIF                                                  Q1PRMS
     C                   MOVE      SCFNAL        RPFNAL                         Final Comp Paymnt fl
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
     C     XXCALL        IFEQ      'I'
     C                   MOVE      'MC'          RPIPGM
     C                   ENDIF
     C     CXDPPC        IFNE      *BLANK
     C     @@RTCD        ANDEQ     '1'
     C                   MOVE      CXDPPC        RPSTS
     C                   MOVE      'P'           RPSTAT
     C                   ENDIF
     C                   MOVE      XXDSQ#        RPDSQ#
     C                   MOVEL     SCINV#        RPINV#
     C                   Eval      RPlmpS = scLmpS
      *
     C                   SELECT
     C     ##EFT         WHENEQ    *OFF
     C     ##EFT         OREQ      *ON
     C     ##PRE         ANDEQ     *OFF
     C*** Beg Add *** 08/20/20 *********************************************
     C     RPeftC        OREQ      'V'
     C*** End Add *** 08/20/20 *********************************************
     C* Not EFT payee OR
     C* EFT payee but pre-notification is not required, write transaction Or
     C* E-Pay
     C                   WRITE     PRP01
     C     ##EFT         WHENEQ    *ON
     C     ##PRE         ANDEQ     *ON
     C* EFT payee and Pre-notification is required
     C                   Z-ADD     RPAMT         SVAMT
     C* Create Pre-notification with RPAMT = 0
     C                   CLEAR                   RPAMT
     C                   WRITE     PRP01
     C     *IN59         IFEQ      *OFF
     C     CMJRST        ANDEQ     'TX'
     C                   GOTO      TXEDT
     C                   ENDIF
     C* Create check request
     C                   Z-ADD     SVAMT         RPAMT
      /Free
       RPeftc = *Blanks;
      /End-Free
     C                   WRITE     PRP01
     C                   ENDSL
     C     TXEDT         TAG
     C                   SETOFF                                       59
     C*** EOMB CHECK MISMATCH COPY STMT
     C/COPY SCPYCMW80P
     C*
     C*  IF THE CLAIM IS CLOSED, CALL SRPRCC TO ALLOCATE THE RESERVES
     C*  FOR THIS RELEASED PAYMENT.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     RPSTAT        IFEQ      *BLANK
     C     C2STAT        ANDEQ     'C'
     C                   MOVE      'R'           QQTYPE
     C                   CALL      'SRPRCC'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF
     C*
     C*  Only perform the following if EDI A49s are being processed
     C*  for the claim's jurisdiction state.
     C*
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     'A49'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      '0'
     C*
     C*  Call the FL EDI State Maint screen if this is the first comp
     C*  pmt and the claim's jurisdiction state is FL.
     C*
     C*
     C*  Determine if this was the first comp payment.
     C*
     C*
     C*
     C*
     C*
     C*  If the LAST PMT FLAG = 'Y', call the EDI Generic Log API to
     C*    create an A49 'FN' lgo record.
     C*
     C     SCFNAL        IFEQ      'Y'
     C*
     C                   MOVE      KKCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      'S1'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C*
     C                   ENDIF
     C                   END                                                    *IN60 CHAINPIX01
     C     CXDPPC        IFNE      *BLANK
     C     @@RTCD        ANDEQ     '1'
     C                   CALL      'WA061R'
     C                   PARM      DBRRN2        @@RRN             9 0
     C                   PARM      *BLANKS       @@RTCD            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCTRCD
     C                   MOVE      *BLANK        SCFEIN
     C                   MOVE      *BLANK        SCMOD
     C*
     C     XXCALL        IFEQ      'I'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END XXCALL=I
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      * ________________________________________________________________
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      /Free
                         if %open(faeal1);
                            Close faeal1;
                         endif;
      /End-free
     C*
     C     Q1PRMS        IFGE      14
     C     @@KC          ANDEQ     ' '
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS>=14
     C*
     C                   MOVE      '1'           *INLR
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
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
      *
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
      *
     C     FNKY2         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
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
     C     *IN17         DOWEQ     *OFF
     C*
     C* SKIP E TYPE ENTRIES
     C*
     C     WHWKST        IFNE      'E'
     C*
     C*   Set error indicators.
     C     WHWKST        IFEQ      'R'
     C                   Z-ADD     WHDATE        XXRTW
     C                   MOVE      *ON           *IN43
     C                   LEAVE
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
     C                   LEAVE
     C                   ENDIF                                                  . WKST = R
     C                   ENDIF                                                  . WKST = E
     C     WHKY2         READPE    PWH01                                  17
     C                   ENDDO
     C*
     C                   ENDIF                                                  . IN17 = OFF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C     DSPMSG        BEGSR
     C*
     C*  GET THE RESERVE TYPE TO CONDITION WHICH MESSAGE WILL APPEAR.
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   MOVEA     '0000'        *IN(90)
     C     RETP(Z)       COMP      'M'                                    90
     C     RETP(Z)       COMP      'C'                                    91
     C     RETP(Z)       COMP      'L'                                    92
     C     RETP(Z)       COMP      'E'                                    93
     C*
     C*  DISPLAY THE AMOUNT BY WHICH THE RESERVES MUST BE INCREASED.
     C*
     C   90              MOVEL     MSG(1)        SCMSG            46
     C   91              MOVEL     MSG(2)        SCMSG
     C   92              MOVEL     MSG(3)        SCMSG
     C   93              MOVEL     MSG(4)        SCMSG
     C     AMT(Z)        SUB       REM(Z)        SCRR              9 2
     C                   MOVE      '1'           *IN94
     C                   MOVE      '1'           *IN95
     C*
     C                   ENDSR
      *
      ******************************************************************
      *  SCR04T - LATE REASON DETAIL EXPLANATION - REQUIRED   **  S U B
      ******************************************************************
     C     SCR04T        BEGSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDRDT
     C                   MOVEL     LRDART        SCRMCD
     C                   MOVE      *BLANKS       XXCOMM
      *
     C     STR4          TAG
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl4
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR04
      *
      *   F12 = Cancel
      *
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C     SCDS01        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN78
     C                   GOTO      STR4
     C                   ELSE
     C                   MOVE      '0'           *IN78
     C                   END
      *
     C     LRDART        IFNE      *BLANK
      *
      *  Late Reason code has DARTS Remark Code.
      *
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     KKCO#         QCCO#
     C                   Z-ADD     KKFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     XXCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C     XXCOMM        IFNE      *BLANK
      *  Late Reason comment entered by User
     C                   MOVEL(P)  XXCOMM        Q#COMM          256
     C                   ELSE                                                   COMM <> blanks
      *  Assign DARTS Remark Code Description
     C     LRDART        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  COMM <> blanks
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FL01            1
     C                   MOVEL(P)  *BLANKS       Q#FL02            1
     C                   MOVEL(P)  LRDART        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
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
     C                   ENDIF
      *
     C                   ENDSR
     C*  WRTRDP - Write tracking record.                               se
     C     WRTRDP        BEGSR
     C*
     C                   CLEAR                   PRD01
     C*
     C                   Z-ADD     UPCO#         RDCO#
     C                   MOVE      QQFND         RDFND
     C                   MOVE      QQFYR         RDFYR
     C                   MOVE      QQCASE        RDCASE
     C                   MOVE      QQEOB#        RDEOB#
     C                   Z-ADD     QQRCN         RDEOBS
     C                   MOVE      QQFEIN        RDFEIN
     C                   MOVE      QQMOD         RDMOD
     C                   MOVE      QQPNAM        RDPNAM
     C                   MOVE      QQAMT         RDAMT
     C                   MOVE      QQFRDT        RDFRDT
     C                   MOVE      QQTODT        RDTODT
     C                   MOVE      QQTRCD        RDTRCD
     C                   MOVE      QQRCVD        RPRCVD                                     d
     C*
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   Z-ADD     DSTIME        RDTIME
     C*
     C                   WRITE     PRD01
     C*
     C                   ENDSR
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
       When *in69;
          fk2  = ak2;
          fk2x = ak2x;
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
     C**************************************************************************
**  TABLE UA
-- ADD --
- UPDATE -
**  MSG
     THE MEDICAL RESERVES MUST BE INCREASED BY
THE COMPENSATION RESERVES MUST BE INCREASED BY
       THE LEGAL RESERVES MUST BE INCREASED BY
     THE EXPENSE RESERVES MUST BE INCREASED BY
**  Function Keys  SCR01
F3=Exit
F10=Search                      F13=Chg user parm   F14=Darts   F24=More keys
                        F16=User request search   F24=More keys

F3=Exit
F10=Search                      F12=Cancel   F14=Darts   F24=More keys
                        F16=User request search   F24=More keys

**  Function Keys  SCR02  *These may be replaced by AK2*
F3=Exit   F4=Additional Address                          F24=More keys
                                        F10=Search   F24=More keys
                   F12=Cancel   F14=Darts   F24=More keys
                        F21=Change group provider   F24=More keys
**  Alternate Function Keys  SCR02  *These may replace FK2*
F3=Exit                                             F24=More keys
                     F10=Search                      F12=Cancel   F24=More keys
F14=Darts                           F21=Change group provider   F24=More keys
**  Function Keys  SCR01 (Clt139)
F3=Exit
F10=Search                      F12=Cancel   F14=Darts   F24=More keys
F16=User request search   F24=More keys

F3=Exit
F10=Search                      F13=Chg user parm   F14=Darts   F24=More keys
F16=User request search   F24=More keys

**  Function Keys  SCR02  (Clt139)
F3=Exit   F4=Additional Address                          F24=More keys
                                        F10=Search   F24=More keys
                   F12=Cancel   F14=Darts   F24=More keys
F21=Change group provider   F24=More keys
**  Alternate Function Keys  SCR02  (Clt139)
F3=Exit                                             F24=More keys
                     F10=Search                      F12=Cancel   F24=More keys
F14=Darts   F21=Change group provider   F24=More keys
