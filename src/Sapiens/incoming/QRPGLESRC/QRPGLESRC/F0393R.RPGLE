     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H Bnddir('STBNDDIR')
     H Bnddir('FTBNDDIR')
     H*****************************************************************
     H/TITLE F0393R - Complete/Pend selection
     H*****************************************************************
     H*                                                               *
     H* ***NOTE TO PGMR: DO NOT INSTALL WITHOUT INSTALLING F7304R     *
     H*                                                               *
     H*  CALLED BY: F0390R                                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  06/23/98 C:61 PROJECT MC152                                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
    h*                                                               *
    h*  07/18/25 C:342 Project: MC451                                *
    h*                 NCCI Med Data Call Enhancements               *
     h*                                                               *
    h*  05/02/24 C:344 Project: PHW115526                            *
     h*                 Revised the onclick property for the          *
     h*                 'Complete' and or 'Cancel' radio buttons to   *
     h*                 clear button selections and Pend Type Desc.   *
     h*                 within the display file F0393F.               *
    h*                                                               *
    h*  07/18/23 C:342 Project: NY060                                *
    h*                 NY Compliance: CARC/RARC Update               *
     h*                                                               *
    h*  01/03/23 C:079 Project: WC2430                               *
    h*                 Call new Payment window if set to do so       *
    h*                 instead of W0080R                             *
    h*                                                               *
    h*  04/19/21 C:079 Support: 90/95537                             *
    h*                 Add Bank Code to API                          *
     h*                                                               *
     h*  07/20/20 C:079 Project: WC2749                               *
     h*                 Set EAeftC = V if EOB qualifies (Zero EOB's)  *
     h*                                                               *
     h*  06/10/19 C:079 Support: 099/80080                            *
     h*                 Make the panel fit the screen better          *
     h*                                                               *
     h*  04/30/19 C:079 Support: 090/75330                            *
     h*                 Remove paid amount edit from 45978            *
     h*                 Modernize the screen                          *
     h*                                                               *
     h*  04/10/19 C:062 Project: 152WAB                               *
     h*                 Apply CSI mods for S1 Medical.                *
     H*                                                               *
     H*  12/03/18 C:079 Project: 146MIO                               *
     H*                 Remove hard coded Procura pend for client 146 *
     H*                                                               *
     H*  07/12/18 C:079 Support: OIC 45978                            *
     H*                 Make the edit of EAPAID to amount passed to   *
     H*                 W0080R for all clients.                       *
     H*                                                               *
     H*  04/18/18 C:079 Support: 99 70418                             *
     H*                 Expand buttons to fit verbiage                *
     H*                                                               *
     H*  04/02/18 C:079 Project 114MJB                                *
     H*                 Update EABTYP                                 *
     0*                                                               *
     H*  03/14/18 C:079 Support: OIC 45978                            *
     H*                 Add edit to compare EAPAID to Paid amount     *
     H*                 Passed to W0080R                              *
     H*                                                               *
     H*  10/25/17 C:079 Support 114/50116                             *
     H*                 For client 114 only allow 1 pend to PRO       *
     H*                                                               *
     H*  11/22/16 C:309 Project 25MUJ                                 *
     H*                 if there are errors in the log file then      *
     H*                 prevent the bill from being Completed         *
     H*                                                               *
     H*  06/23/16 C:212 Project NY038                                 *
     H*                 change NY surcharge edits                     *
     H*                                                               *
     H*  04/18/16 C:310 Support 39897                                 *
     H*                 Modified code to get correct record in FAEAP  *
     H*                 file.                                         *
     H*                                                               *
     H*  02/24/15 C:296 Project 114MRD                                *
     H*                 For pended procura bills appear line item in  *
     H*                 History, this is for client 114.              *
     H*                                                               *
     H*  08/11/14 C:280 Support 86403                                 *
     H*                 add missing code per richie from c129pnsrc    *
     H*                                                               *
     H*  06/16/14 C:079 Support 85871                                 *
     H*                 Do not pend to PRO for client 114             *
     H*                                                               *
     H*  05/14/14 C:212 Project MC379                                 *
     H*                 Exclude Duplicate Bills : set EDI flag = 'N'  *
     H*                 for EOB code 61                               *
     H*                                                               *
     H*  03/18/14 C:280 Support 84888                                 *
     H*                 populate EOWCCK when EOAST = 'FL'             *
     H*                 and EOTAPE = 'E' and DFPAID = 0               *
     H*                                                               *
     H*  01/08/14 C:079 Support MC114 83494                           *
     H*                 Correct the error message indicator           *
     H*                                                               *
     H*  04/24/13 C:277 Redmine: 2214                                 *
     H*                 Correct the error message                     *
     H*                                                               *
     H*  04/17/13 C: 79 Redmine: CSI #2214                            *
     H*                 Correct indicator for ClaimCheck Error        *
     H*                                                               *
     H*  04/17/13 C:284 PROJECT: 114MID                               *
     H*                 Change default Pend = 'Y' and pend code='PRO' *
     H*                                                               *
     H*  08/16/12 C:280 PROJECT: 114MER                               *
     H*                 Add code for claimcheck                       *
     H*                                                               *
     H*  12/13/11 C:191 PROJECT: LA025                                *
     H*                 ACCUMULATE PHARMACY TAX FROM DETAIL           *
     H*                                                               *
     H*  12/05/11 C:190 PROJECT: 114ICM                               *
     H*                 CLEAR THE IMAGE ID, SO THAT BILLS CAN BE      *
     H*                 RE-ENTERED FROM WORK WITH INCOMING DOCS, IF   *
     H*                 CLTID = 114.                                  *
     H*                                                               *
     H*  10/10/11 C:277 SUPPORT 78078                                 *
     H*                 Produce an EOMB with the correct amount       *
     H*                 XREF (78186)                                  *
     H*                                                               *
     H*  02/16/11 C:220 PROJECT: 133MKH                               *
     H*                 ADD LOGIC FOR FORCE TO GENERATE CHECK ON      *
     H*                 COMPLETING THE BILL (FOR LUBA).               *
     H*                                                               *
     H*  12/18/10 C:227 PROJECT: 146MCV                               *
     H*                 EDIT FOR RECON LOGIC FOR PROCURA FOR CLT146   *
     H*                 SETUP EOMBUP = 'P' WHEN PENDED TO 'PRO'       *
     H*                                                               *
     H*  12/08/10 C:257 PROJECT: 133MJN(PRISS#22)                     *
     H*                 CHANGED LOGIC TO GET TOTAL PAYMENT AMOUNT     *
     H*                 IN WARP                                       *
     H*                                                               *
     H*  10/15/10 C:227 PROJECT: 133MJN                               *
     H*                 CHANGED LOGIC TO GET TAX RATE.                *
     H*                                                               *
     H*  10/13/10 C:079 PROJECT: 133MJN                               *
     H*                 ADD LOGIC TO POPULATE EATAX                   *
     H*                                                               *
     H*  09/29/10 C:257 PROJECT: 133MJN                               *
     H*                 ADD PHARMACY TAX FIELD AND TAX INCLUDE FLAG   *
     H*                                                               *
     H*  09/13/10 C:062 PROJECT: 133MJL                               *
     H*                 Require Invoice number for LUBA               *
     H*                                                               *
     H*  10/24/07 C:193 PROJECT: 135MJS                               *
     H*                 REMOVE EDIT FROM TX BILLS                     *
     H*                                                               *
     H*  10/04/07 C:33  Project: 45WFO  Back to Base                  *
     H*     01/02/02 C:062 Project: 45MDF                             *
     H*                    DON'T CALL F7303R, CALL W0080R INSTEAD.    *
     H*                                                               *
     H*  09/04/07 C:180 SUPPORT 59884
     H*                 Update EOCRPE when bill is pended.
     H*
     H*  07/23/07 C:079 PROJECT FL061
     H*                 Dont call W0080R if in Capture Policy State   *
     H*                 mode                                          *
     H*                                                               *
     H*  05/16/07 C:062 PROJECT MC294
     H*                 Update EABNPI.                                *
     H*                                                               *
     H*  04/11/07 C:079 SUPPORT 46223
     H*                 Remove ROOMM/ROOMS edit                       *
     H*                                                               *
     H*  01/18/07 C:188 CA002: 837 CA Medical EDI                     *
     H*                 Implement CA 837 Medical EDI Requirements     *
     H*                                                               *
     H*  04/19/06 C:163 PROJECT 114WMF
     H*                 Back to Base - EOMB Processing (114MEO)
     H*
     H*  11/14/06 C:61  SUPPORT MC144 55844                           *
     H*                 Do not call WA040R if not Workers Comp.       *
     H*                                                               *
     H*  11/11/05 C:163 Project FL052                                 *
     H*                 FL EDI Changes 2005                           *
     H*                                                               *
     H*  04/07/05 C:156 PROJECT: 126WDK                               *
     H*          126MAD ADD PARMS TO CALL OF W0080R.                  *
     H*                                                               *
     H*  03/09/05 C:15  Project: WC1269                               *
     H*                 Added code to load the payee out of country   *
     H*                 address field into FAEOP.                     *
     H*                                                               *
     H*  02/16/05 C:79   PROJECT MC264                                *
     H*                  Use CATODT to calculate FAEOP (EOTODT)       *
     H*                                                               *
     H*  11/28/04 C:79  Project: NY007                                *
     H*                 Populate Surcharge fields for NY bills        *
     H*                                                               *
     H*  07/30/03 C:79  Project: MA013                                *
     H*                 Update EOWCCK for all bills (Standalone)      *
     H*                                                               *
     H*  07/08/03 C:79  Support: MC072 37876                          *
     H*                 Don't overwrite first name of provider        *
     H*                                                               *
     H*  05/02/03 C:79  Project: FS253                                *
     H*                 Store current provider name/address in FAEOP  *
     H*                 Keep historical name/address in FAEAP         *
     H*                                                               *
     H*  04/16/03 C:62  Project: MCMNT                                *
     H*                 For Auto-chk does check get issued in batch?  *
     H*                                                               *
     H*  11/25/02 C:62  Project: 114MDC                               *
     H*                 Don't set CodeReview pend flag to blank,      *
     H*                 if pended again.                              *
     H*                                                               *
     H*  09/19/02 C:79  Support: MC025 34034                          *
     H*                 Require Header EOB code for IR and IP         *
     H*                                                               *
     H*  06/04/02 C:62  Support: 114MDC                               *
     H*                 UPDATE EOCRPE WITH BLANKS.                    *
     H*                                                               *
     H*  05/08/02 C:79  Support: MC025 31715                          *
     H*                 Add edit for negative payments (Recoupment)   *
     H*                                                               *
     H*  04/29/02 C:079   PROJECT: FSMNT                              *
     H*                   1.POPULATE PARMS ON CALL TO OUTS ACCT API   *
     H*                                                               *
     H*  07/13/01 C:079   PROJECT: 114MCG                             *
     H*                   1.USE CATDAT TO UPDATE EOTODT FOR PA        *
     H*                     HOSP BILLS                                *
     H*                                                               *
     H*  04/10/01 C:62  PROJECT: FS224  BASE ONLY                     *
     H*                 ADD PROCESSING FOR INVOICE FIELDS.            *
     H*                                                               *
     H*  04/05/01 C:79  SUPPORT: MC025 25456                          *
     H*                 1. REQUIRE ROOMS OR ROOMM WHEN TYPE OF SERVICE*
     H*                    IS "IM" OR "IS" FOR TEXAS HOSP BILLS       *
     H*                 2. REQUIRE HEADER EOMB CODE FOR ALL ACUTE INP *
     H*                    AND OUTPATIENT TEXAS HOSP BILLS            *
     H*                                                               *
     H*  11/30/00 C:62  PROJECT: MCMNT                                *
     H*                 -CORRECT LOAD PROCESS OF DEFAULT PEND CODE.   *
     H*                 -EDIT AGAINST INVALID PEND CODES.             *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  08/03/00 C:117 Project: FL032                                *
     H*                 Depending on what is installed, update EAESDT *
     H*                 and EOWCCK.  May need to be updated in        *
     H*                 in F7304R instead.                            *
     H*                                                               *
     H*  08/02/00 C:079 Project: LA015                                *
     H*                 Write/update EOWCCK for LA bills              *
     H*                                                               *
     H*  07/27/00 C:062 Project: MCMNT                                *
     H*                 KEEP FROM GOING TO LETTERS IF COMPLETE AND    *
     H*                 PEND FLAGS ARE SET TO 'Y'                     *
     H*                                                               *
     H*  07/11/00 C:079 Project: MCMNT                                *
     H*                 Increase total fields to 11.2.  At 9.2 some   *
     H*                 times the fields overflowed and gave invalid  *
     H*                 totals.                                       *
     H*                                                               *
     H*  06/21/00 C:117 Project: FL030                                *
     H*                 Chgs concerning Resubmissions due to          *
     H*                 additional Data Submission requirements       *
     H*                                                               *
     H*  06/07/00 C:117 PROJECT: MCMNT                                *
     H*                 Changing F11=EOMB/Notes to                    *
     H*                          F8=Header EOB/Notes
     H*
     H*  05/24/00 C:079 SUPPORT: MC044/#22902                         *
     H*                 Call WA040R to delete chk req for same day    *
     H*                 recon on a Pend.                              *
     H*
     H*  05/23/00 C:117 SUPPORT: MC044/#22993                         *
     H*                 Included logic for Dt rcvd fix                *
     H*                                                               *
     H*  05/11/00 C:079 Support: MC048/#22867                         *
     H*                 1.Upd DATRDT when updating E2TRDT at the recon*
     H*                   level.                                      *
     H*                                                               *
     H*  04/11/00 C:117 Project: FS211                                *
     H*                 Deleted the revisions for 09/03/99 proj.      *
     H*                                                               *
     H*  03/10/00  C:79   Support#: MC025/#22159                      *
     H*                 1. Check to see if F0390R updated the wrong   *
     H*                    recon for FDDAP.                           *
     H*                                                               *
     H*  03/06/00 C:79  Project: MCMNT                                *
     H*                 1. Clear CACHK# on Pend for current RCN       *
     H*                                                               *
     H*  02/17/00 C:79  Project: FS144                                *
     H*                 1. Include FSMRA in cross footing             *
     H*                 2. Dont include FSMRA in EOMB code check      *
     H*                                                               *
     H*  02/02/00 C:062 Project: MC221                                *
     H*                 SAVE PGM ID AND THEN RESTORE AFTER CLEARSCR01 *
     H*                                                               *
     H*  11/17/99  C:117 PROJECT: MC044  SUPPORT SEQ# 20185           *
     H*                  RECALCULATE THE TO & FROM DATES OF SERVICE   *
     H*                  FOR FILE FAEOP.                              *
     H*                                                               *
     H*  10/08/99  C:062 PROJECT: MC201                               *
     H*                  CHANGE CONDITION FROM EQ 'N' TO NE 'Y'.      *
     H*                                                               *
     H*  09/21/99  C:117 PROJECT: FL021                               *
     H*                  MOVE SYSTEM DATE TO EOWCCK AND BLANK OUT     *
     H*                  EOSBDT WHEN EOBLST='C'.                      *
     H*                                                               *
     H*  09/16/99 C:79 Support: 19005                                 *
     H*                1) Add Texas Header EOMB edit                  *
     H*                                                               *
     H*  09/08/99 C:56 Project: MC072 18869                           *
     H*                COMPILE ONLY                                   *
     H*                                                               *
     H*  09/03/99 C:56 Project: MC152                                 *
     H*                Use last recon number when looking up recon    *
     H*                received date for W0080R.                      *
     H*                                                               *
     H*  07/26/99 C:56 Project: MC208                                 *
     H*                Add call to WA040R to delete check requests    *
     H*                when a reconsideration is done on the same day *
     H*                a bill is completed.                           *
     H*                                                               *
     H*  07/16/99 C:62 Project: MC208                                 *
     H*                - ADD F11=EOMB/NOTES.                          *
     H*                                                               *
     H*  07/06/99 C:56 Project: KY010                                 *
     H*           1.   Send SCTPMT to W0080R instead of EOPAID.       *
     H*           2.   Call FA700R to get WCMS fund number            *
     H*                                                               *
     H*  07/01/99 C:56 Project: MC202                                 *
     H*             1. If pending the bill, call Request Letter       *
     H*                program (F0750R) if Bill Status Type Code      *
     H*                entered has 'Y' in Pend Letter flag.           *
     H*                                                               *
     H*             2. Send request letter to Bill Status History     *
     H*                program (F0395R01).                            *
     H*                                                               *
     H*  06/01/99 C:56 Project: MC152                                 *
     H*                Add EATRDT when updating existing FAEAP.       *
     H*                                                               *
     H*  05/05/99 C:56 Project: MC152                                 *
     H*                   Change FAEAL to FAEAL1, duplicate logicals  *
     H*                                                               *
     H*  04/21/99 C:56 Support: MC025 #17737                          *
     H*                1. Load 'P' into CAFLAG when cancelling a bill *
     H*                   This will prevent adjudication programs     *
     H*                   from recognizing as part of history         *
     H*                                                               *
     H*                2. Clear CAFLAG when completing bill.  This    *
     H*                   will allow adjudication to see charges as   *
     H*                   history                                     *
     H*                                                               *
     H*  03/18/99 C:56 Project: MC152                                 *
     H*                1. Pass Recon number to call of W0080R.        *
     H*                                                               *
     H*  01/18/99 C:79 PROJECT 25MKO                                  *
     H*                1. If Vendor Fee bill then do not allow tape   *
     H*                   submission or invoicing                     *
     H*                                                               *
     H*  01/15/99 C:94 PROJECT 25MKL                                  *
     H*                CHANGE THE PROGRAM TO CALL F7303R INSTEAD OF   *
     H*                INSTEAD OF W0080R IF AUTO CHECK ISSUANCE IS    *
     H*                INSTALLED.                                     *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       'Complete' must be 'Y' or *blank                    *
     H*  72       'Pend' must be 'Y' or *blank                        *
     H*  73       'Complete' or 'Pend' must be 'Y'                    *
     H*  74       'Pend type' required if 'Pend' equal 'Y'            *
     H*  75 - 76  Position cursor                                     *
     H*  77       'Pend' required if 'Pend type' not blank            *
     H*  86       'ROOMM' or 'ROOMS required for TX acute inpat bills *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     F*
     FF0393F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOP     UF A E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     F*
     F*  Bill Status Table File
     FFTBSL     IF   E           K DISK
     F*
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*
     F* Group Auxiliary FIle
     FFMF1L     IF   E           K DISK
     F*
     F* System Control File
     F*
     F* WCMC Detail Charge File
     FFDCAL12   UF   E           K DISK
     FFDCAL13   IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*
     F* Bill Header Amount File
     FFAEAP     O    E             DISK
     FFAEAL1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F*
     F* Physician Header File
     FFAEHL     IF   E           K DISK
     F*
     F* Hospital Header File
     FFAEUL     IF   E           K DISK
     F*
     F* Hospital Header File
     FFAEEL     IF   E           K DISK
     F*
     F* Pharmacy Header File
     FFAEPL     IF   E           K DISK
     F*
     F* WCMC Case Lockout File
     FFXLOP     UF   E           K DISK
     F*
     F* WCMC Control File
     FFMCXL     IF   E           K DISK
     F* EOMB Detail Amount Transaction File
     F* CLAIM MASTER FILE
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     FFAESL3    IF   E           K DISK
     FFDDAL     UF   E           K DISK
     FFDDUL     IF   E           K DISK
     F**** Beg Chg *** 07/18/23 **************************************
     F**** FDDEL1    IF   E           K DISK
     FFDDEL6    IF   E           K DISK
     F**** End Chg *** 07/18/23 **************************************
     F*  Provider Master FIle
     FFMPDL     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     F*  NY Hospital Rates file
     FFAHRLNY   IF   E           K DISK
     FFAPGLNY   IF   E           K DISK
     FFMFML3NY  if   e           k disk
     ffaesl7    if   e           k disk
     f                                     rename(pes01:pes02)
     f**** Beg Add *** 01/03/23 **********************************************
     fwmcml1    if   e           k disk    UsrOpn Rename(pcm01:pcm02)
     f                                     Prefix(C2:2) InfDs(DbfDs2)
     f**** End Add *** 01/03/23 **********************************************
     D FA740R          pr                  Extpgm('FA740R')
     D ##rrn                          9  0
     D ##rtcd                         1
     d*** Beg Add 07/20/20 *************************************************
     d GetBankAcc      pr                  ExtPgm('WA023R')
     d inCo#                          3
     d inFnd                          3
     d inAppl                         2
     d inSrDt                         8
     d outAcct                       20
     d inCall                         1
     d outHdCy                        1
     d outChkQ                       10
     d outFtyp                       10
     d outRtCd                        1
     d outEfDt                        7
     d outCpyQ                       10
     d outCkTp                       10
     d outCoid                       20
     d inEftC                         1

     d cvtDate         pr                  ExtPgm('SRCVTDAT')
     d  inDate                        8  0
     d  inFfmt                        8
     d  inTfmt                        8
     d  outDate                      10
     d  outErr                        1
     d  inDate         s              8  0
     d  inFfmt         s              8
     d  inTfmt         s              8
     d  outDate        s             10
     d  outErr         s              1
     d*** End Add 07/20/20 *************************************************
     d*** Beg Add 01/03/23 *************************************************
     d NewPaymentPgm   pr                  Extpgm('WTCLMPMT')
     d  inMode                        1
     d  inClmRrn                      9  0
     d  inPmtRrn                      9  0
     d  inTaxId                       9  0
     d  inModifier                    3  0
     d  inFromDt                      7  0
     d  inToDt                        7  0
     d  inEOB#                        9  0
     d  inRecon#                      3  0
     d  inSysType                     1
     d  inBillType                    1
     d  inPayment                     9  2
     d  inTransCode                   2
     d  inPayStatCode                 2
     d  inPayValue1                   2
     d  inPayValue2                   2
     d  inRecvDt                      7  0
     d  outReturnCode                 1
     d*** End Add 01/03/23 *************************************************
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     f**** Beg Add *** 01/03/23 **********************************************
     D DbfDs2          ds
     D  DbRrn2               397    400b 0
     f**** End Add *** 01/03/23 **********************************************
     d errProcSnt      s              1n
     D*================================================================
     D*  Stand Alone Fields - Top
     D*================================================================
     D XXNTWK          S              1A
     D kkmed#          s                   like(FMMED#) inz
     D inPtypeChg      s              1n   Inz(*on)
      *** Beg Add ***** 07/20/20 ******************************
     d inCo#           s              3
     d inFnd           s              3
     d inAppl          s              2
     d inSrDt          s              8
     d outAcct         s             20
     d inCall          s              1
     d outHdCy         s              1
     d outChkQ         s             10
     d outFtyp         s             10
     d outEfDt         s              7
     d outCpyQ         s             10
     d outCkTp         s             10
     d inEftC          s              1
     d outCoid         s             20
     d outRtCd         s              1
     d inEfDt          s              7  0
     d intCo#          s              3  0
     d intFnd          s              3  0
     d inAcct          s             20
     d strEPayVn       s              5
      *** Beg Add ***** 04/19/21 ******************************
     d inCkTp          s             10
      *** End Add ***** 04/19/21 ******************************
     d*** Beg Add 01/03/23 *************************************************
     d  inMode         s              1
     d  inClmRrn       s              9  0
     d  inPmtRrn       s              9  0
     d  inTaxId        s              9  0
     d  inModifier     s              3  0
     d  inFromDt       s              7  0
     d  inToDt         s              7  0
     d  inEOB#         s              9  0
     d  inRecon#       s              3  0
     d  inSysType      s              1
     d  inBillType     s              1
     d  inPayment      s              9  2
     d  inTransCode    s              2
     d  inPayStatCode  s              2
     d  inPayValue1    s              2
     d  inPayValue2    s              2
     d  inRecvDt       s              7  0
     d  outReturnCode  s              1
     d  clmRrn         s              9  0
     d*** End Add 01/03/23 *************************************************
     D*================================================================
     D*  Stand Alone Fields - Bottom
     D*================================================================
     D*** End Add *** 04/17/13 ***************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Complete/Pend select-
     D                                     ion')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     d/Copy sPrModule
     d/copy sPrGenPop
    D*** Beg Add ****** 07/20/20 **************************
     d/copy fprEpay
    D*** End Add ****** 07/20/20 **************************

     d isBErr          s               n   Inz(*off)
     dConMsg           c                   const('Line Item Error Exists/-
     d                                     Bill Cannot Be Completed')
     dConTitl          c                   const('          *****   -
     d                                     Error  *****          ')
     dConBtnTxt        c                   const('OK')

     I*
     IPEA02
     I              EACO#                       E2CO#
     I              EASUB#                      E2SUB#
     I              EAFND                       E2FND
     I              EAFYR                       E2FYR
     I              EACASE                      E2CASE
     I              EASS#                       E2SS#
     I              EAACDT                      E2ACDT
     I              EAEOB#                      E2EOB#
     I              EARCN                       E2RCN
     I              EATRDT                      E2TRDT
     I              EACHRG                      E2CHRG
     I              EAMRA                       E2MRA
     I              EAOTHR                      E2OTHR
     I              EAOTH2                      E2OTH2
     I              EANDIS                      E2NDIS
     I              EAPAID                      E2PAID
     I              EASTAT                      E2STAT
     I              EAUSER                      E2USER
     I              EAPGM                       E2PGM
     I              EATIME                      E2TIME
     I              EADT                        E2DT
     I              EAIUSR                      E2IUSR
     I              EAIPGM                      E2IPGM
     I              EAITIM                      E2ITIM
     I              EAIDT                       E2IDT
     I              EAEPFL                      E2EPFL
     I              EAESDT                      E2ESDT
     I              EAPNAM                      E2PNAM
     I              EAADR1                      E2ADR1
     I              EAADR2                      E2ADR2
     I              EACTY                       E2CTY
     I              EAST                        E2ST
     I              EAZIP                       E2ZIP
     I              EAINPE                      E2INPE
     I              EAOUTE                      E2OUTE
     I              EAASCE                      E2ASCE
     I              EAGRPE                      E2GRPE
     I              EAOCA1                      E2OCA1
     I              EABNPI                      E2BNPI
     I              EATAX                       E2TAX
     I              EANSNT                      E2NSNT
     I              EANPDT                      E2NPDT
     I*** Beg Add *** 07/20/20 ****************************************
     I              EAEFTC                      E2EFTC
     I*** End Add *** 07/20/20 ****************************************

     C*
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CPDF            1
     C                   PARM                    @@CPPN            3
     C                   PARM                    @@CANC            1
     C                   PARM                    @@CHRG           11 2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY '       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FMKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*
     C     FMKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C* EOMB Status Transaction
     C*
     C*
     C* Bill Status Table
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* Key list for group auxiliary file
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* Key list for system control file (SFBPF)
     C*
     C*
     C* key list for detail charge file
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR BILL HEADER AMOUNT FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR BILL DETAIL AMOUNT FILE
     C*
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C* KEY LIST FOR HOSPITAL HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOB HEADER EOMB FILE
     C*
     C     EEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR PHYSICIAN HEADER FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR PHARMACY HEADER FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Key list for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRRDT
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKBLST
     C*
     C* KEY LIST FOR HOSPITAL DETAIL FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     DUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     HRKY          KLIST
     C                   KFLD                    KKOPCT
     C*
     C     PGKY          KLIST
     C                   KFLD                    KKOPCT
     C*
     C*
     C     DEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKEOB
     C*
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     NWKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EONTWK
     C     esKy3         Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkSub#
     C                   Kfld                    kkFnd
     C                   Kfld                    kkFyr
     C                   Kfld                    kkCase
     C                   Kfld                    kkEob#
     C                   Kfld                    k2BlSt
     C                   Kfld                    k2StCd
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    HROPCT        KKOPCT
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    CADFEE        TTLDFE
     C     *LIKE         DEFINE    EOTAX         SMTAX
     C     *LIKE         DEFINE    EOTAX         DFTAX
     C     *LIKE         DEFINE    EOFRDT        KKEFDT
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         KKEOB
     C     *LIKE         DEFINE    DEEOBCD       KKEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    EOBLST        HLSTAT
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C     *LIKE         DEFINE    ESRRDT        KKRRDT
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EORCN         K1RCN
     C     *LIKE         DEFINE    EAIPGM        XXIPGM
     C     *LIKE         DEFINE    EOMRA         DFMRA
     C     *LIKE         DEFINE    EONDIS        DFNDIS
     C     *LIKE         DEFINE    EOOTHR        DFOTHR
     C     *LIKE         DEFINE    EOOTH2        DFOTH2
     C     *LIKE         DEFINE    EOCHRG        DFCHRG
     C     *LIKE         DEFINE    EOPAID        DFPAID
     C     *LIKE         DEFINE    EOPNAM        XXPNAM
     C     *LIKE         DEFINE    EOADR1        XXADR1
     C     *LIKE         DEFINE    EOADR2        XXADR2
     C     *LIKE         DEFINE    EOCTY         XXCTY
     C     *LIKE         DEFINE    EOST          XXST
     C     *LIKE         DEFINE    EOZIP         XXZIP
     C*
     C     *like         define    ESBlSt        k2BlSt
     C     *like         define    ESStCd        k2StCd
     C*
     C/COPY SCOPYMT01C
     C*================================================================
     C* CHKBAL - Check FDDAP/FAEAP for balancing at recon level
     C*================================================================
     C     CHKBAL        BEGSR
     C*
     C* Initialize total fields
     C                   Z-ADD     0             TLCHRG           11 2
     C                   Z-ADD     0             TLPAID           11 2
     C                   Z-ADD     0             TLMRA            11 2
     C                   Z-ADD     0             TLNDIS           11 2
     C                   Z-ADD     0             TLOTHR           11 2
     C                   Z-ADD     0             TLOTH2           11 2
     C                   Z-ADD     0             TLTAX            11 2
     C*
     C                   Z-ADD     0             SMCHRG           11 2
     C                   Z-ADD     0             SMPAID           11 2
     C                   Z-ADD     0             SMMRA            11 2
     C                   Z-ADD     0             SMNDIS           11 2
     C                   Z-ADD     0             SMOTHR           11 2
     C                   Z-ADD     0             SMOTH2           11 2
     C*
     C* NY Surcharge fields
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C                   Z-ADD     0             SMINPE           11 2
     C                   Z-ADD     0             SMOUTE           11 2
     C                   Z-ADD     0             SMASCE           11 2
     C                   Z-ADD     0             TLINPE           11 2
     C                   Z-ADD     0             TLOUTE           11 2
     C                   Z-ADD     0             TLASCE           11 2
     C                   Z-ADD     0             T1INPE           11 2
     C                   Z-ADD     0             T1OUTE           11 2
     C                   Z-ADD     0             T1ASCE           11 2
     C*
     C     EUKY          CHAIN     PEU01                              99
     C*
     C                   ENDIF
     C                   Z-ADD     0             SMTAX
     C*
     C* Calculate totals for the EOB for all recons
     C     CAKY          CHAIN(N)  PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        TLCHRG
     C                   ADD       CAPAID        TLPAID
     C                   ADD       CAMRA         TLMRA
     C                   ADD       CANDIS        TLNDIS
     C                   ADD       CAOTHR        TLOTHR
     C                   ADD       CAOTH2        TLOTH2
     C*
      /Free
           If IsModEnabled('Line Validation');
              If CAberr = 'Y';
                 isBErr = *On;
              EndIf;
           EndIf;
      /End-Free
     C* NY Surcharge calcultion
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   EXSR      NYSCHG
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ADD       CATAX         TLTAX
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C* Calculate amounts for the original bill and previous recons
     C*
     C     EAKY1         CHAIN     PEA02                              97
     C     *IN97         DOWEQ     *OFF
     C     E2RCN         IFNE      KKRCN
     C                   ADD       E2CHRG        SMCHRG
     C                   ADD       E2PAID        SMPAID
     C                   ADD       E2MRA         SMMRA
     C                   ADD       E2NDIS        SMNDIS
     C                   ADD       E2OTHR        SMOTHR
     C                   ADD       E2OTH2        SMOTH2
     C*
     C* NY Surcharge fields
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C                   ADD       E2INPE        SMINPE
     C                   ADD       E2OUTE        SMOUTE
     C                   ADD       E2ASCE        SMASCE
     C*
     C                   ENDIF
     C                   ADD       E2TAX         SMTAX
     C                   ENDIF                                                  E2RCN NE KKRCN
     C     EAKY1         READE     PEA02                                  97
     C                   ENDDO
     C*
     C* Calculate difference between total for bill and previous totals
     C     TLCHRG        SUB       SMCHRG        DFCHRG
     C     TLPAID        SUB       SMPAID        DFPAID
     C     TLMRA         SUB       SMMRA         DFMRA
     C     TLNDIS        SUB       SMNDIS        DFNDIS
     C     TLOTHR        SUB       SMOTHR        DFOTHR
     C     TLOTH2        SUB       SMOTH2        DFOTH2
     C     TLTAX         SUB       SMTAX         DFTAX
     C*
     C                   Z-ADD     0             SMCHRG
     C                   Z-ADD     0             SMPAID
     C                   Z-ADD     0             SMMRA
     C                   Z-ADD     0             SMNDIS
     C                   Z-ADD     0             SMOTHR
     C                   Z-ADD     0             SMOTH2
     C                   Z-ADD     0             SMTAX
     C                   MOVEL(P)  'P'           KKEOB
     C*
     C* Calculate detail amount totals for the current recon
     C     DAKY          CHAIN(N)  PDA01                              98
     C     *IN98         DOWEQ     *OFF
     C                   ADD       DACHRG        SMCHRG
     C                   ADD       DAMRA         SMMRA
     C                   ADD       DANDIS        SMNDIS
     C                   ADD       DAOTHR        SMOTHR
     C                   ADD       DAOTH2        SMOTH2
     C                   ADD       DAPAID        SMPAID
     C                   ADD       DATAX         SMTAX
     C*
     C     EOAST         IFEQ      'TX'
     C     EOTAPE        ANDEQ     'Y'
     C     DAPAID        ANDLT     0
     C                   EXSR      CHKREC
     C                   ENDIF
     C*
     C     DAKY          READE(N)  PDA01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C* Differences should be the same or give message to contact PDS
     C* and pend the bill.
     C     SMCHRG        IFNE      DFCHRG
     C     SMMRA         ORNE      DFMRA
     C     SMOTHR        ORNE      DFOTHR
     C     SMOTH2        ORNE      DFOTH2
     C     SMNDIS        ORNE      DFNDIS
     C     SMPAID        ORNE      DFPAID
     C     SMTAX         ORNE      DFTAX
     C*
     C                   SETON                                        9085
     C*
     C                   ENDIF                                                  SMCHRG IFNE
     C*
      /Free
         If isBErr = *On;
            *IN90  = *On;
            PopOneBtn(ConMsg:ConTitl:ConBtnTxt);
         EndIf;
      /End-Free
     C                   ENDSR
     C*
     C*================================================================
     C* CHKREC - Check for negative payment(Recoupment) EOB code P
     C*================================================================
     C     CHKREC        BEGSR
     C*
     C                   MOVE      DADSEQ        KKDSEQ
     C     DEKY          CHAIN     PDE01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*** BEG ADD *** 09/16/99 ****************************************
     C*
     C*================================================================
     C* EDITTX - Texas state specific edits
     C*================================================================
     C     EDITTX        BEGSR
     C*
     C* FOR ADJ STATE OF TEXAS AND FOR CLIENTS WHO REQUIRE THIS EDIT:
     C* LINE ITEMS THAT HAVE PAYMENT AMOUNTS NOT EQUALING CHARGE
     C* AMOUNT MUST HAVE AN EOB CODE ATTACHED. EXCLUDE FSMRA.
     C*
     C  N90SCCOMP        IFEQ      'Y'
     C*
     C     ##CLT#        IFNE      '135'
     C     EOMBUP        ORNE      'C'
     C     EOMBUP        ANDNE     'D'
     C                   MOVE      *BLANKS       ##RTN             1
     C                   MOVEL(P)  'FA205R'      ##OBJ            10
     C                   MOVEL(P)  '*PGM'        ##TYP             7
     C                   CALL      'FA722C'
     C                   PARM                    ##RTN
     C                   PARM                    ##OBJ
     C                   PARM                    ##TYP
     C     ##RTN         IFNE      'F'
     C                   Z-ADD     @@RRN         ##RRN1            9 0
     C                   CALL      'FA205R'
     C                   PARM      ' '           ##RTN1            1
     C                   PARM                    ##RRN1
     C     ##RTN1        IFEQ      'F'
     C                   SETON                                        9082
     C                   ENDIF                                                  ##RTN1 = 'F'
     C                   ENDIF                                                  ##RTN NE 'F'
     C                   ENDIF
     C*
     C* For Adjudication state of Texas and if Bill type = "H" and
     C* type of service = (AO,AS,HO,HS,IP,IR) and total paid is less
     C* than total charged require an EOMB header code
     C*
     C  N90EOBTYP        IFEQ      'H'
     C                   Z-ADD     *ZERO         SMCHRG
     C                   Z-ADD     *ZERO         SMPAID
     C*
     C     DAKY          CHAIN     PDA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   ADD       DACHRG        SMCHRG
     C                   ADD       DAPAID        SMPAID
     C*
     C     DAKY          READE     PDA01                                  99
     C*
     C                   ENDDO
     C*
     C*
     C     SMPAID        IFLT      SMCHRG
     C*
     C* Read header EOMB code file looking for a value in EEEOB
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                MOVE      *BLANKS       EEEOB
     C                   MOVE      *BLANKS       EEEOBCD
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C     EEKY          CHAIN     PEE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C                   LEAVE
     C*
     C                   ENDIF                                                  EEEOB NE BLANK
     C*
     C     EEKY          READE     PEE01                                  99
     C*
     C                   ENDDO
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C     EUKY          CHAIN     PEU01                              99
     C*
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IR'
     C*
     C                   CALL      'F0355R01'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      'A'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2
     C*
     C                   SETON                                        90
     C* Read header EOMB code file looking for a value in EEEOB
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                MOVE      *BLANKS       EEEOB
     C                   MOVE      *BLANKS       EEEOBCD
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C     ##CLT#        IFNE      '135'
     C     EOMBUP        ORNE      'C'
     C     EOMBUP        ANDNE     'D'
     C     EEKY          CHAIN     PEE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C                   LEAVE
     C*
     C                   ENDIF                                                  EEEOB NE BLANK
     C*
     C     EEKY          READE     PEE01                                  99
     C*
     C                   ENDDO
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C                   SETON                                        8490
     C                   ENDIF                                                              NK
     C*
     C                   ENDIF                                                  CLTID NE 135
     C                   ENDIF                                                  EUICN = AO
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*
     C                   ENDIF                                                  TLPAID<TLCHRG
     C*
     C                   ENDIF                                                  EOBTYP = 'H'
     C*
     C*
     C                   ENDIF                                                  SCCOMP = 'Y'
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C* EDITCA - California state specific edits
     C*================================================================
     C     EDITCA        BEGSR
     C*
     C** 'Complete this bill = Y', get charges
     C*
     C                   SETOFF                                       50
     C*
     C  N90SCCOMP        IFEQ      'Y'
     C*
     C                   Z-ADD     *ZERO         SMCHRG
     C                   Z-ADD     *ZERO         SMPAID
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       DACHRG        SMCHRG
     C                   ADD       DAPAID        SMPAID
     C     DAKY          READE     PDA01                                  99
     C                   ENDDO
     C*
     C** If paid is less than charges, EOB header code must exist for CA.
     C**   Pop-up message window to alert user, then entry program.
     C*
     C     SMPAID        IFLT      SMCHRG
     C*
     C** Check for an ACTIVE EOB header code.
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                MOVE      *BLANKS       EEEOB
     C                   MOVE      *BLANKS       EEEOBCD
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C     EESTAT        ANDNE     'I'
     C                   LEAVE
     C                   ENDIF                                                  EEEOB NE BLANK
     C     EEKY          READE     PEE01                                  99
     C                   ENDDO
     C*
     C** Valid EOB header not found...
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C     EESTAT        OREQ      'I'
     C*
     C                   EXFMT     ER1WDW
     C                   SETON                                        50
     C*
     C                   CALL      'F0355R01'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      'A'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2
     C*
     C                   SETON                                        90
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                MOVE      *BLANKS       EEEOB
     C                   MOVE      *BLANKS       EEEOBCD
     C*** End Chg *** 07/18/25 ***************************************
     C*
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C                   LEAVE
     C                   ENDIF                                                  EEEOB NE BLANK
     C     EEKY          READE     PEE01                                  99
     C                   ENDDO
     C*
     C*** Beg Chg *** 07/18/25 ***************************************
     C***  EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C*** End Chg *** 07/18/25 ***************************************
     C                   SETON                                        8490
     C                   ENDIF                                                              NK
     C*
     C                   ENDIF                                                  TLPAID<TLCHRG
     C                   ENDIF                                                  EOB CHECK
     C                   ENDIF                                                  SCCOMP = 'Y'
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   SETOFF                                       33
     C                   SETOFF                                       34
     C                   SETOFF                                       92
     C                   SETOFF                                       69
     C*
     C* SOURCE INVOICE DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSIDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCSIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSIDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        8990
     C                   ENDIF
     C                   ENDIF
     C*
     C* Cancel  must be 'Y' or *blank
     C*
     C  N90
     CANN30SCCANC        IFNE      'Y'
     C     SCCANC        ANDNE     *BLANK
     C                   SETON                                        9079
     C                   ENDIF                                                  SCCANC
     C*
     C* Complete   must be 'Y' or *blank
     C*
     C  N90SCCOMP        IFNE      'Y'
     C     SCCOMP        ANDNE     *BLANK
     C                   SETON                                        9071
     C                   ENDIF                                                  SCCOMP
     C*
     C* Pend must be 'Y' or *blank
     C*
     C  N90SCPEND        IFNE      'Y'
     C     SCPEND        ANDNE     *BLANK
     C                   SETON                                        9072
     C                   ENDIF                                                  SCPEND
     C*
     C     @@CANC        IFNE      'Y'
     C*
     C* Complete   OR Pend must be 'Y'
     C*
     C  N90SCCOMP        IFNE      'Y'
     C     SCPEND        ANDNE     'Y'
     C                   SETON                                        9073
     C                   ENDIF                                                  COMP  PEND
     C  N90SCCOMP        IFEQ      'Y'
     C     SCPEND        ANDEQ     'Y'
     C                   SETON                                        9073
     C                   ENDIF                                                  COMP  PEND
     C                   ENDIF                                                  @@CANC NE 'Y'
     C*
     C     @@CANC        IFEQ      'Y'
     C*
     C*  Complete OR Cancel OR Pend must be 'Y'
     C*
     C  N90SCCANC        IFNE      'Y'
     C     SCPEND        ANDNE     'Y'
     C     SCCOMP        ANDNE     'Y'
     C                   SETON                                        9080
     C                   ENDIF                                                  SCCANC NE 'Y'
     C*
     C     SCCANC        IFEQ      'Y'
     C     SCPEND        ANDEQ     'Y'
     C     SCCANC        OREQ      'Y'
     C     SCCOMP        ANDEQ     'Y'
     C     SCPEND        OREQ      'Y'
     C     SCCOMP        ANDEQ     'Y'
     C                   SETON                                        9081
     C                   ENDIF                                                  SCCANC = 'Y'
     C*
     C                   ENDIF                                                  @@CANC = 'Y'
     C*
     C* Invoice Number is required if LUBA and the bill is being comp.
     C*
     C     *IN90         IFEQ      *OFF
     C     ##CLT#        ANDEQ     '133'
     C     SCCOMP        ANDEQ     'Y'
     C     SCSINV        ANDEQ     *BLANKS
     C                   SETON                                        8890
     C                   ENDIF
     C*
     C* Pend type required if Pend = 'Y'
     C*
     C  N90SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     *BLANKS
     C                   SETON                                        9074
     C                   ENDIF                                                  SCPEND SCPTYP
     C*
     C* 'Pend' must equal 'Y' if 'Pend Type' entered
     C*
     C  N90SCPTYP        IFNE      *BLANKS
     C     SCPEND        ANDEQ     *BLANKS
     C                   SETON                                        9077
     C                   ENDIF                                                  SCPTYP SCPEND
     C*
     C* Pend type must be valid if entered
     C*
     C  N90SCPTYP        IFNE      *BLANKS
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  SCPTYP        KKCODE
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *ON
     C     BSSTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   ENDIF
     C                   ENDIF                                                  SCPTYP VALIDP
     C*
     C* IF THIS IS A RECON, APPORT OR A RECORD-ONLY CLAIM, DO NOT SEND
     C*   THIS TO PROCURA.
     C*
     C     ##CLT#        IFEQ      '146'
     C*
     C  N90SCPTYP        IFEQ      'PRO'
     C     KKRCN         IFNE      *ZERO
     C                   SETON                                        9087
     C                   ENDIF
     C                   EXSR      GETNTWK
     C*
     C  N90##CLT#        IFEQ      '114'
     C     XXNTWK        ANDNE     'Y'
     C     SCPTYP        ANDEQ     'PRO'
     C                   SETON                                        9070
     C                   ENDIF
     C**************************RMN
     C     CAKY1         CHAIN     PCA02                              98
     C     *IN98         DOWEQ     *OFF
     C     CACHRG        IFLT      CAPAID
     C     CACHRG        OREQ      0
     C                   SETON                                        9038
     C                   LEAVE
     C                   ENDIF                                                  CAPAID NE XXTOTL
     C     CAKY1         READE     PCA02                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C**************************RMN
     C                   ENDIF
     C                   ENDIF                                                  ##CLT# = 146
       // Do not send to Procura more than once for client 114
      /Free
        k2BlSt = 'P';
        k2StCd = 'SPR';
        Chain(n) esKy3 pes02;
        If *in90 = *off and ##clt# = '114' and %Found(faesl7) and
          scPtyp = 'PRO';
           *in90 = *on;
           errProcSnt = *on;
        Endif;
      /End-Free
     C*
     C* If reason = *blank and Pend = 'Y' then load reason with
     C*  Pend Type Description
     C*
     C  N90SCREAS        IFEQ      *BLANKS
     C     SCPEND        ANDEQ     'Y'
     C                   MOVEL(P)  'P'           KKTYPE                         PEND
     C                   MOVEL(P)  SCPTYP        KKCODE
     C     BSKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCREAS
     C   60              MOVEL(P)  *BLANKS       SCREAS
     C                   ENDIF                                                  SCREAS SCPEND
     C*
     C* Employee payment must be 'Y' or *blank
     C*
     C  N90*IN91         IFEQ      *ON                                          EOAST=FL
     C     SCEPMT        IFNE      'Y'
     C     SCEPMT        ANDNE     *BLANK
     C                   SETON                                        9092
     C                   ENDIF                                                  SCEPMT
     C                   ENDIF                                                  *IN91=*ON
     C*
     C* FOR ADJ STATE OF TEXAS AND FOR CLIENTS WHO REQUIRE THIS EDIT:
     C* LINE ITEMS THAT HAVE PAYMENT AMOUNTS NOT EQUALING CHARGE
     C* AMOUNT MUST HAVE AN EOB CODE ATTACHED.
     C*
     C                   SELECT
     C*
     C     EOAST         WHENEQ    'TX'
     C*
     C                   EXSR      EDITTX
     C*
     C*
     C     EOAST         WHENEQ    'CA'
     C                   EXSR      EDITCA
     C*
     C*
     C                   ENDSL
     C*
     C* VERIFY THE AMOUNT CALC FOR ALL LINE ITEMS.  IF ANY LINE ITEM'S
     C* CALC IS NOT CORRECT GIVE AN ERROR
     C*
     C     SCCOMP        IFEQ      'Y'
     C     CAKY1         CHAIN     PCA02                              98
     C     *IN98         DOWEQ     *OFF
     C                   Z-ADD     0             XXTOTL
     C     CACHRG        SUB       CAMRA         XXTOTL
     C                   SUB       CAOTHR        XXTOTL
     C                   SUB       CAOTH2        XXTOTL
     C                   SUB       CANDIS        XXTOTL
     C     CAPAID        IFNE      XXTOTL
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF                                                  CAPAID NE XXTOTL
     C     CAKY1         READE     PCA02                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C                   ENDIF                                                  SCCOMP = 'Y'
     C*
     C*
     C* CALL REQUEST LETTER PROGRAM IF PENDING BILL AND PEND LETTER
     C* FLAG IS 'Y'.
     C*
     C  N90SCPEND        IFEQ      'Y'
     C     BSLTR         ANDEQ     'Y'
     C     BSLTTY        IFEQ      'T'
     C*
     C                   CALL      'F0757R'
     C                   PARM      'A'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0          FAEOP
     C                   PARM      *ZEROS        ##RRN2            9 0          FALTP
     C     *IN90         PARM      *BLANKS       ##KL              1
     C*
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##STR
     C                   CALL      'SRCENTER'
     C                   PARM                    ##STR            80
     C                   PARM      40            ##LEN             3 0
     C                   MOVEL(P)  ##STR         ##PRDS
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         ##FYR
     C                   MOVE      EOCASE        ##CASE
     C                   MOVE      EOFEIN        ##FEIN
     C                   MOVE      EOMOD         ##MOD
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##FSDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##TSDT
     C                   MOVE      EOCHRG        ##TTLC
     C                   MOVE      *ZEROS        ##LTR#
     C*
     C     EOBTYP        IFEQ      'H'
     C                   MOVE      '4'           ##PRTY
     C                   ELSE
     C                   MOVE      '1'           ##PRTY
     C                   ENDIF                                                  EOBTYP = 'H'
     C*
     C     EOCSCD        IFEQ      '320'
     C                   MOVE      'N'           ##PRNT
     C                   ELSE
     C                   MOVE      *BLANKS       ##PRNT                         EOCSCD = '320'
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       ##RTCD
     C                   Z-ADD     EOEOB#        #1EOB#
     C                   Z-ADD     EORCN         ##RCN
     C                   CALL      'F0750R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND             3
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##FEIN
     C                   PARM                    ##MOD
     C                   PARM                    ##FSDT            8
     C                   PARM                    ##TSDT            8
     C                   PARM                    ##TTLC            9
     C                   PARM                    ##LTR#            9
     C                   PARM                    ##PRTY            1
     C                   PARM                    ##PRNT            1
     C                   PARM                    ##RTCD
     C                   PARM                    #1EOB#            9 0
     C                   PARM                    ##RCN             3 0
     C*
     C     ##RTCD        IFEQ      'N'
     C                   SETON                                        90
     C                   ENDIF                                                  ##RTCD = 'N'
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCPEND = 'Y'
     C*
     C*
     C* Check to see if FAEAP and FDDAP are in synch at the recon level
     C*
     C  N90SCCOMP        IFEQ      'Y'
     C                   Eval      isBErr = *Off
     C*
     C                   EXSR      CHKBAL
     C*
     C*
     C                   ENDIF                                                  SCCOMP=Y
     C*
     C*
     C                   move      *off          Found             1
     C*
     C     ##CLT#        IFEQ      '114'
     C     SCCOMP        ANDEQ     'Y'
     C*
     C                   MOVE      DADSEQ        KKDSEQ
     C     DEKY2         CHAIN     PDE01                              99
     C*
b2   C     *IN99         DOWNE     *ON
     C*
     C                   IF        destat <> 'I'
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL     DEEOB         TEST              1
     C                   MOVEL     DEEOBCD       TEST              1
     C**** End Chg *** 07/18/23 **************************************
     C*
b3   C     TEST          IFEQ      'P'
     C                   move      *on           Found
     C                   leave
e3   C                   endif
     C*
     C                   endif
     C*
     C     DEKY2         reade     PDE01                                  99
     C*
e2   C                   enddo
     C*
b2   C                   if        found = *on
     C                   eval      *in90 = *on
     C                   eval      *in69 = *on
e2   C                   endif
     C*
e1   C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
       //*** Beg Add *** 07/20/20 *************************************
       //==============================================================
       // GetEobEPayMC - Set EOB EFTC flag to V if E-Pay
       //==============================================================
      /Free
       Begsr GetEpayStat;
        strEPayVn =  *Blanks;
        If isModEnabled('WorkComp') and isModEnabled('E-Pay');
           // Get the bank account
           inCo#  = %EditC(EOco#:'X');
           inFnd  = %EditC(EOfnd:'X');
           inAppl = 'WC';
           inDate = DSdate;
           inFfmt = *blanks;
           inTfmt = '*MDYY';
           Callp cvtDate(Indate:inFfmt:inTfmt:outDate:outErr);
           inSrDt = outDate;
           inCall = ' ';
           inEfTc = 'V';

           // Get the Bank Account Number
           getBankAcc(inCo#:inFnd:inAppl:inSrDt:outAcct:inCall:outHdCy:
            outChkQ:outFtyp:outRtCd:outEfDt:outCpyQ:outCkTp:
            outCoid:inEftC);
           If outAcct <> *blanks;
              Monitor;
                 inEfDt = %Dec(outEfDt:7:0);
              On-error;
              Endmon;
              intCo# = EOco#;
              intFnd = EOfnd;
              inAcct = outAcct;
              inCkTp = outCkTp;
       //*** Beg Chg *** 04/19/21 ******************************************
              strEPayVn =  GetEobEPayMC(intCo#:inAppl:intFnd:inAcct:inEfDt:
               inCkTp);
       //*** End Chg *** 04/19/21 ******************************************
           Endif;
        Endif;
       EndSr;
      /End-Free
       //*** End Add *** 07/20/20 *************************************
     C*================================================================
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVE      '1'           *IN90
     C*
     C                   SELECT
     C*
     C*  Search Pend types
     C*
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*
     C*
     C*================================================================
     C* KHDO - F8=EOB/NOTES
     C*================================================================
     C     KHDO          BEGSR
     C*
     C*                    MOVE ##SUB#    KKSUB#
     C*                    MOVE ##FND     KKFND
     C*                    MOVE ##FYR     KKFYR
     C*                    MOVE ##CASE    KKCASE
     C*                    MOVE ##EOB#    KKEOB#
     C*
     C*          ESKY      CHAINPES01                99
     C*
     C*          *IN99     IFEQ *OFF
     C*
     C*                    Z-ADDDBRRNA    ##RRN
     C*
     C*                    ENDIF
     C*
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C*
     C*  CLEAR 'SEARCHING' MSG
     C*
     C                   CALL      'SRRMVMSG'                           99
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Default Complete and Pend Flags
     C*
     C                   SELECT
     C*
     C     @@CPDF        WHENEQ    'C'
     C                   MOVEL(P)  'Y'           SCCOMP
     C*
     C     @@CPDF        WHENEQ    'P'
     C                   MOVEL(P)  'Y'           SCPEND
     C*
     C                   ENDSL
     C                   MOVEL(P)  EOSINV        SCSINV
     C                   Z-ADD     EOSIDT        SCSIDT
     C*
     C     EOSINV        IFEQ      *BLANKS
     C     EOSIDT        ANDEQ     *ZEROS
     C                   SETOFF                                       16
     C                   ENDIF
       // Load the choices for document filter.
      /Free

       Clear scBSChoic;
       Clear scBSChVal;

       Setll (EOco#:'P':' ') pbs01;
       Reade (EOco#:'P') pbs01;
       Dow not %eof(ftbsl);
          If BSstat = 'A';
             If scBSChoic = *blanks;
                scBSChoic = ' ,' + %trim(BScode) + ' - ' + %trim(BSdesc);
                scBSChVal = ' ,' + %trim(BScode);
             Else;
                scBSChoic = %trim(scBSChoic) + ',' + %trim(BScode) +
                             ' - ' + %Trim(BSdesc);
                scBSChVal = %trim(scBSChVal) + ',' + %trim(BScode);
             Endif;
          Endif;
          Reade (EOco#:'P') pbs01;
       Enddo;
      /End-Free
     C*
     C*  Default Pend Type
     C*
     C     @@CPDF        IFEQ      'P'
     C     SCPTYP        IFEQ      *BLANKS
     C                   MOVE      @@CPPN        SCPTYP
     C                   ENDIF
     C                   ENDIF                                                  @@CPDF
     C*
     C                   MOVE      EOEPMT        SCEPMT
     C* IF THIS IS THE ORIGINAL BILL AND
     C*  PROTECT SCREEN FIELDS, PEND BILL WITH 'PRO'.
     C*
     C*
     C* Get reductions heading from WCMC Control File
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
      /Free
                         Evalr  scHmr1 = %Trim(CXhMra);
                         Evalr  scHnet = %Trim(CXhNet);
                         Evalr  scHot1 = %Trim(CXhOt1);
                         Evalr  scHot2 = %Trim(CXhOt2);
      /End-free
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C* CALCULATE PREVIOUS PAID AMOUNT AND THIS PAYMENT
     C*
     C                   Z-ADD     0             SCCHG1
     C                   Z-ADD     0             SCMRA1
     C                   Z-ADD     0             SCNDIS
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCPPMT
     C                   Z-ADD     0             SCTPMT
     C                   Z-ADD     0             SCPAID
     C*
     C     EORCN         IFEQ      *ZEROS
     C     CAKY1         CHAIN     PCA02                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        SCCHG1
     C                   ADD       CAMRA         SCMRA1
     C   31              ADD       CANDIS        SCNDIS
     C                   ADD       CAOTHR        SCOTHR
     C                   ADD       CAOTH2        SCOTH2
     C                   ADD       CAPAID        SCPAID
     C     CARCN         IFLT      EORCN
     C                   ADD       CAPAID        SCPPMT
     C                   ELSE
     C                   ADD       CAPAID        SCTPMT
     C                   ENDIF                                                  CARCN NE EORCN
     C     CAKY1         READE     PCA02                                  99
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C     CAKY1         CHAIN     PCA02                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        SCCHG1
     C                   ADD       CAMRA         SCMRA1
     C   31              ADD       CANDIS        SCNDIS
     C                   ADD       CAOTHR        SCOTHR
     C                   ADD       CAOTH2        SCOTH2
     C                   ADD       CAPAID        SCPAID
     C     CAKY1         READE     PCA02                                  99
     C                   ENDDO
     C*
     C     EAKY1         CHAIN(N)  PEA02                              99
     C     *IN99         DOWEQ     *OFF
     C*                    ADD  E2OTHR    SCOTHR
     C*                    ADD  E2OTH2    SCOTH2
     C     E2RCN         IFLT      EORCN
     C                   ADD       E2PAID        SCPPMT
     C                   ENDIF                                                  CARCN NE EORCN
     C     EAKY1         READE(N)  PEA02                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C     SCPAID        SUB       SCPPMT        SCTPMT
     C                   ENDIF                                                  EORCN = *ZEROS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     SCPEND        IFEQ      'Y'
     C                   MOVE      'P'           EOBLST
     C                   ENDIF
     C*
     C     SCCOMP        IFEQ      'Y'
     C                   MOVE      'C'           EOBLST
     C                   ENDIF
     C*
     C     SCCANC        IFEQ      'Y'
     C                   MOVE      'X'           EOBLST
     C     ##CLT#        IFEQ      '114'
     C     ##CLT#        OREQ      '152'
     C                   CLEAR                   EOIDOC
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  SCSINV        EOSINV
     C                   Z-ADD     SCSIDT        EOSIDT
     C*
     C                   MOVE      SCEPMT        EOEPMT
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     DSTIME        EOTIME
     C                   MOVE      EOCO#         @@CO#
     C                   MOVE      EOSUB#        @@SUB#
     C                   MOVE      EOFND         @@FND
     C*  If it is an outside account, update EOWCCK & EAESDT
     C                   CALL      'FA701R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@OUTS            1
     C*
     C*  Check to see if WCMS is installed, if not, update EOWCCK
     C*  and EAESDT
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
14   C                   PARM      ' '           @@NPWC            1
15   C                   PARM      ' '           @@OS              1
16   C                   PARM      ' '           @@ARLS            1
17   C                   PARM      ' '           @@CCOD            1
18   C                   PARM      ' '           @@CRVW            1
19   C                   PARM      ' '           @@PROM            1
20   C                   PARM      ' '           @@MCRV            1
21   C                   PARM      ' '           @@FREQ            1
22   C                   PARM      ' '           @@CRAC            1
23   C                   PARM      ' '           @@PREO            1
24   C                   PARM      ' '           @@PARN            1
25   C                   PARM      ' '           @@MGRI            1
26   C                   PARM      ' '           @@KCIF            1
27   C                   PARM      ' '           @@CURS            1
28   C                   PARM      ' '           @@PCIB            1
     C*
     C*  Also, check to see if the payment is 0
     C*
     C     @@OUTS        IFEQ      'Y'
     C     DFPAID        ANDGT     0
     C     @@WCIF        ORNE      'Y'
     C     DFPAID        ANDGT     0
     C*
     C*
     C     EOAST         OREQ      'FL'
     C     EOTAPE        ANDEQ     'E'
     C     DFPAID        ANDEQ     0
     C*
     C*
     C     EOBLST        IFEQ      'C'
     C                   MOVEL     SRDTO         EOWCCK
     C                   MOVE      *BLANKS       EOSBDT
     C                   ENDIF
     C                   ENDIF                                                  @@OUTS=Y
     C*
     C                   MOVEL(P)  SCPTYP        ##STCD            3
     C                   MOVEL(P)  SCREAS        ##REAS           60
     C*
     C*
     C*
     C*
     C* GET CURRENT PROVIDER NAME AND ADDRESS
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     c*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXPNAM
     C                   MOVEL(P)  PDADR2        XXADR1
     C                   MOVEL(P)  *BLANKS       XXADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        XXPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      XXPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        XXADR1
     C                   MOVEL(P)  PDADR2        XXADR2
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*
     C                   MOVEL(P)  PDCTY         XXCTY
     C                   MOVEL(P)  PDST          XXST
     C                   Z-ADD     PDZIP         XXZIP
     C                   MOVEL(P)  PDADR3        EOADR3
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C     ##CLT#        IFEQ      '146'
     C     SCPTYP        ANDEQ     'PRO'
     C     ##CLT#        OREQ      '114'
     C     SCPTYP        ANDEQ     'PRO'
     C                   MOVEL     'P'           EOMBUP
     C                   Eval      EOnSnt = 'P'
     C                   ENDIF
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C* INITIALIZE INDICATORS FOR ERROR CHECKING: THESE ARE INDICATORS
     C* BEYOND 71 - 89 RESERVED BY COPYBOOK. THESE SHOULD BE SET OFF
     C* IN THE 'PGMEND' SUBROUTINE ALSO.
     C*
      /Free
        errProcSnt = *off;
      /End-Free
     C                   SETOFF                                       33
     C                   SETOFF                                       34
     C*
     C*  Process F10 search
     C*
     C     *INKJ         CASEQ     '1'           KJDO
     C     *INKH         CASEQ     *ON           KHDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      '1'           OUTFLG
     C*
     C* REMOVE RECORD FROM LOCKOUT FILE
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@MODE = 'A'
      /free
       If eoblst = 'C' and eoast = 'FL' and
          eorcn = 0 and eopaid = 0;
          Exsr Set_EDI_FL;
       Endif;
      /end-free
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C* RETRIEVE CLIENT ID
     C                   CALL      'RTVCLTID'
     C                   PARM                    ##CLT#            3
     C*
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   MOVEL(P)  Q1PGM         XXIPGM
     C                   CLEAR                   SCR01
     C                   MOVEL(P)  XXIPGM        Q1PGM
     C* Set Change Mode
     C*
     C                   MOVEL(P)  'C'           @@MODE
     C*
     C* Get FAEOP record
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C* Load key fields
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C* PROTECTS SOURCE INVOICE FIELDS IF THE RECONSIDERATION <> 000
     C*
     C     EORCN         IFNE      0
     C                   SETON                                        16
     C                   ENDIF
     C*
     C* POSITIONS CURSOR ON "COMPLETE THIS BILL"
     C*
     C     ##CLT#        IFNE      '133'
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       34
     C                   ENDIF
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C     @@CANC        IFEQ      'Y'                                          allow cancel
     C                   SETOFF                                       30
     C                   ELSE
     C                   SETON                                        30        disallow cancel
     C                   ENDIF
     C*
     C* DETEMINE IF NETWORK REDUCTION FILED SHOULD BE DISPLAYED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        31
     C                   ELSE
     C                   SETOFF                                       32
     C                   ENDIF
     C*
     C                   Z-ADD     EOEOB#        SCEOMB
     C     EOAST         IFEQ      'FL'
     C                   SETON                                        91
     C                   ENDIF                                                  EOAST='FL'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         YYMD              8 0
      /Free
      /End-free
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* NYSCHG - NY surcharge calculation
     C*================================================================
     C     NYSCHG        BEGSR
     C*
     c/free
        kkmed# = %subst(EUmcar:1:7);
        Chain(n) kkmed# pfm01;
        If %found (fmfml3ny);
      /end-free

     C* Inpatient Elector
     C*
     C     EUICN         IFGE      'IA'
     C     EUICN         ANDLE     'IZ'
     C*
     C*
     C*
     C*
     C     DURVCD        IFEQ      115
     C     DURVCD        OREQ      125
     C     DURVCD        OREQ      135
     C     DURVCD        OREQ      145
     C     DURVCD        OREQ      155
     C     DURVCD        OREQ      235
     C     DURVCD        OREQ      650
     C     DURVCD        OREQ      657
     C     DURVCD        OREQ      659
     C*
     C                   ELSE
     C*
     C*
     C*
     C*
     C                   SELECT
     C     CAFDAT        WHENGE    1090401
     C     CAPAID        MULT(H)   .0963         T1INPE
     C     CAFDAT        WHENGE    1060101
     C     CAPAID        MULT(H)   .0895         T1INPE
     C     CAFDAT        WHENGE    1040701
     C     CAPAID        MULT(H)   .0885         T1INPE
     C                   OTHER
     C     CAPAID        MULT(H)   .0818         T1INPE
     C                   ENDSL
     C*
     C                   ADD       T1INPE        TLINPE
     C*
     C                   ENDIF                                                  DURVCD IFEQ 115
     C*
     C*
     C                   ENDIF                                                  EUICN GE
     C*
     C* Outpatient and Freestanding ASC Elector
     C*
     C     EUICN         IFGE      'AO'
     C     EUICN         ANDLE     'HS'
     C*
     C                   MOVEL(P)  EUMCAR        KKOPCT
     C*
     C* ASC
     C*
     C     EUBLTY        IFGE      '840'
     C     EUBLTY        ANDLE     '849'
     C     EUICN         ORGE      'AO'
     C     EUICN         ANDLE     'AS'
     C     DUPSGP        ORGT      0
     C*
     C* Freestanding ASC or Outpatient Hospital ASC
     C*
     C*
     C*
     C*
     C     FMTYPE        IFNE      'A'
     C*
     C*
     C*
     C*
     C                   SELECT
     C     CAFDAT        WHENGE    1090401
     C     CAPAID        MULT(H)   .0963         T1OUTE
     C     CAFDAT        WHENGE    1060101
     C     CAPAID        MULT(H)   .0895         T1OUTE
     C     CAFDAT        WHENGE    1040701
     C     CAPAID        MULT(H)   .0885         T1OUTE
     C                   OTHER
     C     CAPAID        MULT(H)   .0818         T1OUTE
     C                   ENDSL
     C*
     C                   ADD       T1OUTE        TLOUTE
     C*
     C                   ENDIF
     C*
     C*
     C     FMTYPE        IFEQ      'A'
     C*
     C*
     C*
     C*
     C                   SELECT
     C     CAFDAT        WHENGE    1090401
     C     CAPAID        MULT(H)   .0963         T1ASCE
     C     CAFDAT        WHENGE    1060101
     C     CAPAID        MULT(H)   .0895         T1ASCE
     C     CAFDAT        WHENGE    1040701
     C     CAPAID        MULT(H)   .0885         T1ASCE
     C                   OTHER
     C     CAPAID        MULT(H)   .0818         T1ASCE
     C                   ENDSL
     C*
     C                   ADD       T1ASCE        TLASCE
     C*
     C                   ENDIF
     C*
     C*
     C                   ELSE
     C*
     C     EUICN         IFGE      'HO'
     C     EUICN         ANDLE     'HS'
     C*
     C*
     C*
     C     DURVCD        IFEQ      115
     C     DURVCD        OREQ      125
     C     DURVCD        OREQ      135
     C     DURVCD        OREQ      145
     C     DURVCD        OREQ      155
     C     DURVCD        OREQ      235
     C     DURVCD        OREQ      650
     C     DURVCD        OREQ      651
     C     DURVCD        OREQ      652
     C     DURVCD        OREQ      657
     C     DURVCD        OREQ      659
     C*
E1   C                   ELSE
     C*
     C*
     C*
     C*
     C                   SELECT
     C     CAFDAT        WHENGE    1090401
     C     CAPAID        MULT(H)   .0963         T1OUTE
     C     CAFDAT        WHENGE    1060101
     C     CAPAID        MULT(H)   .0895         T1OUTE
     C     CAFDAT        WHENGE    1040701
     C     CAPAID        MULT(H)   .0885         T1OUTE
     C                   OTHER
     C     CAPAID        MULT(H)   .0818         T1OUTE
     C                   ENDSL
     C*
     C                   ADD       T1OUTE        TLOUTE
     C*
     C                   ENDIF                                                  DURVCD IFEQ
     C*
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
E1   C                   ENDIF                                                  EUBLTY IFGE
     C*
     C                   ENDIF                                                  EUICN GE
     C*
     C/free
        EndIf;
      /end-free
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C* CALCULATE TOTAL AMOUNTS FOR THE BILL
     C*
     C                   Z-ADD     0             TLPAID
     C                   Z-ADD     0             TLMRA
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLOTH2
     C                   Z-ADD     0             TLTAX
     C                   Z-ADD     0             EOFRDT
     C                   Z-ADD     0             EOTODT
     C*
     C                   CLEAR                   TTLDFE
     C     CAKY          CHAIN(N)  PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CAPAID        TLPAID
     C                   ADD       CAMRA         TLMRA
     C                   ADD       CANDIS        TLNDIS
     C                   ADD       CAOTHR        TLOTHR
     C                   ADD       CAOTH2        TLOTH2
     C     EPKY          CHAIN     PEP01                              99
     C     EPTAX         IFEQ      'Y'
     C                   ADD       CADFEE        TTLDFE
     C                   ADD       CATAX         TLTAX
     C                   ELSE
     C                   ADD       0             TLTAX
     C                   ENDIF
     C*
     C*  Calculates TO and FROM service dates
     C*
     C     EOFRDT        IFEQ      *ZEROS
     C     EOTODT        ANDEQ     *ZEROS
     C                   Z-ADD     CAFDAT        EOFRDT
     C                   Z-ADD     CAFDAT        EOTODT
     C                   ENDIF
     C*
     C     CAFDAT        IFLT      EOFRDT
     C                   Z-ADD     CAFDAT        EOFRDT
     C                   ENDIF
     C*
     C     CAFDAT        IFGT      EOTODT
     C                   Z-ADD     CAFDAT        EOTODT
     C                   ENDIF
     C*
     C     CATDAT        IFGT      EOTODT
     C                   Z-ADD     CATDAT        EOTODT
     C                   ENDIF
     C*
     C     CAKY          READE(N)  PCA01                                  99
     C                   ENDDO
     C*
     C     SCPTYP        IFNE      'CRP'
     C                   MOVEL(P)  *BLANKS       EOCRPE
     C                   ENDIF
     C     BSLTR         IFEQ      'Y'
     C     BSLTTY        ANDEQ     'T'
     C     SCPEND        ANDEQ     'Y'
     C                   MOVEL(P)  'T'           EOCRPE
     C                   ENDIF
     C                   Z-ADD     TLPAID        EOPAID
     C                   Z-ADD     TLTAX         EOTAX
     C                   Z-ADD     TLMRA         EOMRA
     C                   Z-ADD     TLNDIS        EONDIS
     C                   Z-ADD     TLOTHR        EOOTHR
     C                   Z-ADD     TLOTH2        EOOTH2
     C*
     C                   Z-ADD     @@CHRG        EOCHRG
     C*
     C* Use current prov name/address unless the EOB was imported via
     C* an import program which begins F8***
     C*
     C     2             SUBST     EOIPGM:1      ZZIPGM            2
     C*
     C     ZZIPGM        IFNE      'F8'
     C                   MOVEL     XXPNAM        EOPNAM
     C                   MOVEL     XXADR1        EOADR1
     C                   MOVEL     XXADR2        EOADR2
     C                   MOVEL     XXCTY         EOCTY
     C                   MOVEL     XXST          EOST
     C                   MOVEL     XXZIP         EOZIP
     C                   ENDIF
     C*
     C* NY Surcharge fields
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C                   Z-ADD     TLINPE        EOINPE
     C                   Z-ADD     TLOUTE        EOOUTE
     C                   Z-ADD     TLASCE        EOASCE
     C*
     C     EOINPE        ADD       EOOUTE        EOGRPE
     C                   ADD       EOASCE        EOGRPE
     C*
     C                   ENDIF
      /Free
        If isBErr = *Off ;
           EOberr = ' '  ;
        EndIf;
      /End-Free
     C*
     C                   UPDATE    DBRCDF
     C*
     C* WRITE/UPDATE BILL HEADER AMOUNT RECORD FOR ORIGINAL BILL
     C*
     C     KKRCN         IFEQ      *ZEROS
     C     EAKY          CHAIN     PEA02                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EOCHRG        E2CHRG
     C                   Z-ADD     EOMRA         E2MRA
     C                   Z-ADD     EONDIS        E2NDIS
     C                   Z-ADD     EOOTHR        E2OTHR
     C                   Z-ADD     EOOTH2        E2OTH2
     C                   Z-ADD     EOPAID        E2PAID
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         E2PGM
     C                   MOVEL     Q1USER        E2USER
     C                   Z-ADD     DSTIME        E2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     SRDTO         E2TRDT
     C*
     C                   SELECT
     C     SCCANC        WHENEQ    'Y'
     C                   MOVE      'X'           E2EPFL
     C     SCPEND        WHENEQ    'Y'
     C                   MOVE      'E'           E2EPFL
     C     SCCOMP        WHENEQ    'Y'
     C                   MOVE      *BLANKS       E2EPFL
     C                   ENDSL
     C                   MOVEL     EOBNPI        E2BNPI
     C                   Z-ADD     EOWCCK        E2ESDT
     C*
     C* Use current prov name/address unless the EOB was imported via
     C* an import program which begins F8***
     C*
     C     2             SUBST     EOIPGM:1      ZZIPGM            2
     C*
     C     ZZIPGM        IFNE      'F8'
     C                   MOVEL     XXPNAM        E2PNAM
     C                   MOVEL     XXADR1        E2ADR1
     C                   MOVEL     XXADR2        E2ADR2
     C                   MOVEL     XXCTY         E2CTY
     C                   MOVEL     XXST          E2ST
     C                   MOVEL     XXZIP         E2ZIP
     C                   ENDIF
     C*
     C* NY Surcharge fields
     C*
     C                   Z-ADD     EOINPE        E2INPE
     C                   Z-ADD     EOOUTE        E2OUTE
     C                   Z-ADD     EOASCE        E2ASCE
     C*
     C                   Z-ADD     EOGRPE        E2GRPE
     C*
     C                   Z-ADD     EOGRPE        E2OCA1
     C                   Z-ADD     EOTAX         E2TAX
     C* IF HOSP OR PHYS BILL, LOAD NETWORK FIELDS.
     C*
     C     ##CLT#        IFEQ      '146'
     C     ##CLT#        OREQ      '114'
     C     SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     'PRO'
     C                   MOVE      'P'           E2NSNT
     C                   Z-ADD     YYMD          E2NPDT
     C                   ELSE
     C                   MOVE      ' '           E2NSNT
     C                   Z-ADD     0             E2NPDT
     C                   ENDIF
     C                   ENDIF                                                  ##CLT# = 146
       //*** Beg Add *** 07/20/20 *******************************************
      /Free
       Chain f1Ky pf101;
       If %Found(FMF1L);
          If F1outA = 'Y';
             EApLnk = '1';
          Else;
             If EOpaid <= 0;
                EApLnk = '2';
             Else;
                EApLnk = '3';
             Endif;
          Endif;
       Endif;

       If EApLnk = '2' or EApLnk = '3';
          If E2eftC = 'V';
             E2eftC = ' ';
          Endif;
          Exsr GetEpayStat;
          If strEPayVn <> *Blanks;
             E2eftC = 'V';
          Endif;
       Endif;
      /End-free
     C*
       //*** End Add *** 07/20/20 *******************************************
     C                   UPDATE    PEA02
     C                   ELSE
     C                   Z-ADD     EOCHRG        EACHRG
     C                   Z-ADD     EOMRA         EAMRA
     C                   Z-ADD     EONDIS        EANDIS
     C                   Z-ADD     EOOTHR        EAOTHR
     C                   Z-ADD     EOOTH2        EAOTH2
     C                   Z-ADD     EOPAID        EAPAID
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   MOVE      'A'           EASTAT
     C                   MOVE      EOSYS         EASYS
     C                   MOVE      EOBTYP        EABTYP
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C     EOSYS         IFEQ      'H'                                          hospital
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EUEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'                                          physician
     C     EOBTYP        ANDEQ     'N'
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EHEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'V'                                          vendor fee
     C     EOBTYP        ANDEQ     'V'
     C*
     C                   MOVE      'P'           EABBLF
     C                   MOVE      'N'           EAEDI
     C*
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'P'                                          pharmacy
     C     EOBTYP        ANDEQ     'X'
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EPEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF                                                  F1OUTA = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1USER        EAIUSR
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   SELECT
     C     SCCANC        WHENEQ    'Y'
     C                   MOVE      'X'           EAEPFL
     C     SCPEND        WHENEQ    'Y'
     C                   MOVE      'E'           EAEPFL
     C     SCCOMP        WHENEQ    'Y'
     C                   MOVE      *BLANKS       EAEPFL
     C                   ENDSL
     C                   MOVEL     EOBNPI        EABNPI
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C*
     C* Store historical name/address unless the EOB was imported via
     C* an import program which begins F8***
     C*
     C     2             SUBST     EOIPGM:1      ZZIPGM            2
     C*
     C     ZZIPGM        IFNE      'F8'
     C                   MOVEL     XXPNAM        EAPNAM
     C                   MOVEL     XXADR1        EAADR1
     C                   MOVEL     XXADR2        EAADR2
     C                   MOVEL     XXCTY         EACTY
     C                   MOVEL     XXST          EAST
     C                   MOVEL     XXZIP         EAZIP
     C                   ELSE
     C                   MOVEL     EOPNAM        EAPNAM
     C                   MOVEL     EOADR1        EAADR1
     C                   MOVEL     EOADR2        EAADR2
     C                   MOVEL     EOCTY         EACTY
     C                   MOVEL     EOST          EAST
     C                   MOVEL     EOZIP         EAZIP
     C                   ENDIF
     C*
     C* NY Surcharge fields
     C*
     C                   Z-ADD     EOINPE        EAINPE
     C                   Z-ADD     EOOUTE        EAOUTE
     C                   Z-ADD     EOASCE        EAASCE
     C*
     C                   Z-ADD     EOGRPE        EAGRPE
     C                   Z-ADD     EAGRPE        EAOCA1
     C                   Z-ADD     EOTAX         EATAX
     C* LOAD NETWORK FIELDS.
     C*
     C     SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     'PRO'
     C                   MOVE      'P'           EANSNT
     C                   Z-ADD     YYMD          EANPDT
     C                   ELSE
     C                   MOVE      ' '           EANSNT
     C                   Z-ADD     0             EANPDT
     C                   ENDIF
       //*** Beg Add *** 07/20/20 ************************************
      /Free
                         If EApLnk = '2' or EApLnk = '3';
                            Exsr GetEpayStat;
                            If strEPayVn <> *Blanks;
                               EAeftC = 'V';
                            Endif;
                         Endif;
      /End-free
       //*** End Add *** 07/20/20 ************************************
     C                   WRITE     PEA01
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  KKRCN = *ZEROS
     C*
     C* WRITE/UPDATE BILL HEADER AMOUNT RECORD FOR RECONS
     C*
     C     KKRCN         IFNE      *ZEROS
     C                   Z-ADD     0             SMCHRG
     C                   Z-ADD     0             SMPAID
     C                   Z-ADD     0             SMMRA
     C                   Z-ADD     0             SMNDIS
     C                   Z-ADD     0             SMOTHR
     C                   Z-ADD     0             SMOTH2
     C                   Z-ADD     0             SMTAX
     C*
     C* CALCULATE AMOUNTS FOR THE ORIGINAL BILL AND PREVIOUS RECONS
     C*
     C     EAKY1         CHAIN     PEA02                              97
     C     *IN97         DOWEQ     *OFF
     C     E2RCN         IFNE      KKRCN
     C                   ADD       E2CHRG        SMCHRG
     C                   ADD       E2PAID        SMPAID
     C                   ADD       E2MRA         SMMRA
     C                   ADD       E2NDIS        SMNDIS
     C                   ADD       E2OTHR        SMOTHR
     C                   ADD       E2OTH2        SMOTH2
     C     EPTAX         IFEQ      'Y'
     C                   ADD       E2TAX         SMTAX
     C                   ENDIF
     C                   ENDIF                                                  E2RCN NE KKRCN
     C     EAKY1         READE     PEA02                                  97
     C                   ENDDO
     C     EAKY          CHAIN     PEA02                              98
     C     *IN98         IFEQ      *OFF
     C     EOCHRG        SUB       SMCHRG        E2CHRG
     C     EOPAID        SUB       SMPAID        E2PAID
     C     EOMRA         SUB       SMMRA         E2MRA
     C     EONDIS        SUB       SMNDIS        E2NDIS
     C     EOOTHR        SUB       SMOTHR        E2OTHR
     C     EOOTH2        SUB       SMOTH2        E2OTH2
     C     EPTAX         IFEQ      'Y'
     C     EOTAX         SUB       SMTAX         E2TAX
     C                   ENDIF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        E2USER
     C                   MOVEL     Q1PGM         E2PGM
     C                   Z-ADD     DSTIME        E2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E2DT
     C                   MOVEL     SRDTO         E2TRDT
     C*
     C                   SELECT
     C     SCCANC        WHENEQ    'Y'
     C                   MOVE      'X'           E2EPFL
     C     SCPEND        WHENEQ    'Y'
     C                   MOVE      'E'           E2EPFL
     C     SCCOMP        WHENEQ    'Y'
     C                   MOVE      *BLANKS       E2EPFL
     C                   ENDSL
     C                   MOVEL     EOBNPI        E2BNPI
     C                   Z-ADD     EOWCCK        E2ESDT
     C*
     C* Store historical name/address unless the EOB was imported via
     C* an import program which begins F8***
     C*
     C     2             SUBST     EOIPGM:1      ZZIPGM            2
     C*
     C     ZZIPGM        IFNE      'F8'
     C                   MOVEL     XXPNAM        E2PNAM
     C                   MOVEL     XXADR1        E2ADR1
     C                   MOVEL     XXADR2        E2ADR2
     C                   MOVEL     XXCTY         E2CTY
     C                   MOVEL     XXST          E2ST
     C                   MOVEL     XXZIP         E2ZIP
     C                   ELSE
     C                   MOVEL     EOPNAM        E2PNAM
     C                   MOVEL     EOADR1        E2ADR1
     C                   MOVEL     EOADR2        E2ADR2
     C                   MOVEL     EOCTY         E2CTY
     C                   MOVEL     EOST          E2ST
     C                   MOVEL     EOZIP         E2ZIP
     C                   ENDIF
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C     EOINPE        SUB       SMINPE        E2INPE
     C     EOOUTE        SUB       SMOUTE        E2OUTE
     C     EOASCE        SUB       SMASCE        E2ASCE
     C*
     C     E2INPE        ADD       E2OUTE        E2GRPE
     C                   ADD       E2ASCE        E2GRPE
     C*
     C                   Z-ADD     E2GRPE        E2OCA1
     C*
     C                   ENDIF
     C*
     C* LOAD NETWORK FIELDS.
     C*
     C     SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     'PRO'
     C                   MOVE      'P'           E2NSNT
     C                   Z-ADD     YYMD          E2NPDT
     C                   ELSE
     C                   MOVE      ' '           E2NSNT
     C                   Z-ADD     0             E2NPDT
     C                   ENDIF
       //*** Beg Add *** 07/20/20 ******************************************
      /Free
                         Chain f1Ky pf101;
                         If %Found(FMF1L);
                            If F1outA = 'Y';
                               EApLnk = '1';
                            Else;
                               If EOpaid <= 0;
                                  EApLnk = '2';
                               Else;
                                  EApLnk = '3';
                               Endif;
                            Endif;
                         Endif;

                         If EApLnk = '2' or EApLnk = '3';
                            If E2eftC = 'V';
                               E2eftC = ' ';
                            Endif;
                            Exsr GetEpayStat;
                            If strEPayVn <> *Blanks;
                               E2eftC = 'V';
                            Endif;
                         Endif;
      /End-free
       //*** End Add *** 07/20/20 ******************************************
     C                   UPDATE    PEA02
     C                   ELSE
     C                   CLEAR                   PEA01
     C     EOCHRG        SUB       SMCHRG        EACHRG
     C     EOPAID        SUB       SMPAID        EAPAID
     C     EOMRA         SUB       SMMRA         EAMRA
     C     EONDIS        SUB       SMNDIS        EANDIS
     C     EOOTHR        SUB       SMOTHR        EAOTHR
     C     EOOTH2        SUB       SMOTH2        EAOTH2
     C     EOTAX         SUB       SMTAX         EATAX
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     EOSYS         EASYS
     C                   MOVE      EOBTYP        EABTYP
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EUEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EHEDI         EAEDI
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     EPKY          CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EPEDI         EAEDI
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'V'                                          vendor fee
     C     EOBTYP        ANDEQ     'V'
     C*
     C                   MOVE      'P'           EABBLF
     C                   MOVE      'N'           EAEDI
     C*
     C                   ENDIF                                                  EOSYS = 'N'
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF                                                  EOPAID LE 0
     C                   ENDIF                                                  F1OUTA = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1USER        EAIUSR
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1PGM         EAIPGM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSTIME        EAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   SELECT
     C     SCCANC        WHENEQ    'Y'
     C                   MOVE      'X'           EAEPFL
     C     SCPEND        WHENEQ    'Y'
     C                   MOVE      'E'           EAEPFL
     C     SCCOMP        WHENEQ    'Y'
     C                   MOVE      *BLANKS       EAEPFL
     C                   ENDSL
     C                   MOVEL     EOBNPI        EABNPI
     C                   Z-ADD     EOWCCK        EAESDT
     C*
     C* Store historical name/address unless the EOB was imported via
     C* an import program which begins F8***
     C*
     C     2             SUBST     EOIPGM:1      ZZIPGM            2
     C*
     C     ZZIPGM        IFNE      'F8'
     C                   MOVEL     XXPNAM        EAPNAM
     C                   MOVEL     XXADR1        EAADR1
     C                   MOVEL     XXADR2        EAADR2
     C                   MOVEL     XXCTY         EACTY
     C                   MOVEL     XXST          EAST
     C                   MOVEL     XXZIP         EAZIP
     C                   ELSE
     C                   MOVEL     EOPNAM        EAPNAM
     C                   MOVEL     EOADR1        EAADR1
     C                   MOVEL     EOADR2        EAADR2
     C                   MOVEL     EOCTY         EACTY
     C                   MOVEL     EOST          EAST
     C                   MOVEL     EOZIP         EAZIP
     C                   ENDIF
     C*
     C* NY Surcharge fields
     C*
     C     EOAST         IFEQ      'NY'
     C     EOBTYP        ANDEQ     'H'
     C*
     C     EOINPE        SUB       SMINPE        EAINPE
     C     EOOUTE        SUB       SMOUTE        EAOUTE
     C     EOASCE        SUB       SMASCE        EAASCE
     C*
     C     EAINPE        ADD       EAOUTE        EAGRPE
     C                   ADD       EAASCE        EAGRPE
     C*
     C                   Z-ADD     EAGRPE        EAOCA1
     C*
     C                   ENDIF
     C*
     C*
     C*
     C* LOAD NETWORK FIELDS.
     C*
     C     SCPEND        IFEQ      'Y'
     C     SCPTYP        ANDEQ     'PRO'
     C                   MOVE      'P'           EANSNT
     C                   Z-ADD     YYMD          EANPDT
     C                   ELSE
     C                   MOVE      ' '           EANSNT
     C                   Z-ADD     0             EANPDT
     C                   ENDIF
       //*** Beg Add *** 07/20/20 ************************************
      /Free
                         If EApLnk = '2' or EApLnk = '3';
                            Exsr GetEpayStat;
                            If strEPayVn <> *Blanks;
                               EAeftC = 'V';
                            Endif;
                         Endif;
      /End-free
       //*** End Add *** 07/20/20 ************************************
     C                   WRITE     PEA01
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDIF                                                  KKRCN NE *ZEROS
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C* Update transaction date in detail amount trans file with date
     C* in header amount file for the current recon.
     C     EAKY          CHAIN(N)  PEA02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DAKY          CHAIN     PDA01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C                   Z-ADD     E2TRDT        DATRDT
     C                   UPDATE    PDA01
     C*
     C     DAKY          READE     PDA01                                  98
     C*
     C                   ENDDO                                                  *IN98=OFF
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*
     C* IF BILL STATUS IS PENDED, WRITE 'P' TO CAFLAG FIELD IN FDCAP
     C* FOR RECORDS WRITTEN DURING THE CURRENT RECON
     C*
     C     EOBLST        IFEQ      'P'
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EORCN         IFEQ      CARCN
     C*
     C                   Z-ADD     *ZEROS        CACHK#
     C                   MOVE      'P'           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                  EORCN=CARCN
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'P'
     C*
     C* IF BILL STATUS IS 'C', WRITE EOB# TO CHK# FIELD IN FDCAP
     C*   THIS WILL ALLOW LINE ITEMS TO APPEAR IN HISTORY
     C*
     C* For bill status 'C' or pended procura bills write eob# to
     C* ckh# field in FDCAP this will allow line item to appear in history.
      /Free
        If EOblst = 'C' or (##clt# = '114' and scPend = 'Y' and scPtyp = 'PRO');
      /End-Free
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EOEOB#        CACHK#
     C                   MOVE      ' '           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'C'
     C*
     C*
     C* WHEN COMPLETING BILL, LOAD CAFLAG WITH 'P'.  THIS WILL PREVENT
     C* ADJUDICATION PROGRAMS FROM SEEING THESE LINE ITEMS AS
     C* HISTORY
     C*
     C     EOBLST        IFEQ      'X'
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   MOVE      'P'           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C                   ENDIF                                                  EOBLST = 'C'
     C*
     C* CALL F0395R01 TO WRITE BILL STATUS HISTORY RECORD
     C*
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN         ##RRN2            9 0          faeop
     C                   MOVE      EOBLST        ##BLST            1
     C                   MOVE      *BLANKS       ##PGM             8
     C                   MOVE      ##LTR#        #1LTR#            9 0
     C                   CALL      'F0395R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'H'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##BLST
     C                   PARM                    ##STCD
     C                   PARM                    ##REAS
     C                   PARM                    ##PGM
     C                   PARM                    #1LTR#
     C* CALL WCMC CHECK REQUEST IF PENDING BILL AND THERE IS AN
     C* AMOUNT TO PAY FROM THE SAME DAY RECON
     C*
     C     ##BLST        IFEQ      'P'
     C*
     C                   MOVEL     EOSUB#        ##SUB#            5
     C                   MOVEL     EOFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      ##CO#         #1CO#             3
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
     C*
     C* CALL WA040R TO DELETE ANY CHECK REQUESTS FOR THIS EOMB/RCN
     C                   Z-ADD     EORCN         KKEOBS            7 0
     C*
     C                   MOVE      ##WCF#        #1WCF#            3 0
     C*
     C                   CALL      'WA040R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    #1WCF#
     C                   PARM                    EOFYR
     C                   PARM                    EOCASE
     C                   PARM                    EOEOB#
     C                   PARM                    KKEOBS
     C*
     C                   ENDIF                                                  ##OUTS=N
      *
     C                   ENDIF                                                  ##BLST=P
     C*
     C* CALL WCMC CHECK REQUEST IF COMPLETING BILL AND THERE IS AN
     C* AMOUNT TO PAY
     C*
     C     ##BLST        IFEQ      'C'
     C                   MOVEL     EOSUB#        ##SUB#            5
     C                   MOVEL     EOFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      ##CO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFNE      'Y'
     C*
     C* DETERMINE IF A CHECK SHOULD BE ISSUED
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1FL04        ANDEQ     'Y'
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
     C* CALL WA040R TO DELETE ANY CHECK REQUESTS FOR THIS EOMB/RCN
     C                   Z-ADD     EORCN         KKEOBS            7 0
     C*
     C                   MOVE      ##WCF#        #1WCF#            3 0
     C*
     C* DONT CALL CHECK REQUEST IF WE WERE CAPTURING POLICY STATE DATA
     C* ONLY.  DONT DELETE CHECK REQUEST
     C*
     C     EOCPST        IFNE      'Y'
     C                   CALL      'WA040R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##CO#
     C                   PARM                    #1WCF#
     C                   PARM                    EOFYR
     C                   PARM                    EOCASE
     C                   PARM                    EOEOB#
     C                   PARM                    KKEOBS
     C*
     C* DETERMINE IS AUTO CHECK ISSUANCE IS IN EFFECT
     C*
     C     ##ACIS        IFNE      'Y'
     C     ##ACIS        OREQ      'Y'
     C     @@PCIB        ANDNE     'Y'
     C     CLTID         OREQ      '045'
     C                   MOVEL(P)  Q1PGM         ##PRDS           40
     C                   MOVEL     EOFYR         ##FYR             3
     C                   MOVEL(P)  EOCASE        ##CASE            7
     C                   MOVEL(P)  EOFEIN        ##FEIN            9
     C                   MOVEL     EOMOD         ##MOD             3
     C                   MOVEL     EOEOB#        ##EOB#            9
     C                   MOVEL     EOPNAM        ##PNAM           40
     C                   MOVEL     EOTRCD        ##TRCD            2
     C     EPTAX         IFEQ      'Y'
     C                   MOVE      EOPAID        ##AMT             9
     C                   ELSE
     C                   MOVE      SCTPMT        ##AMT             9
     C                   ENDIF
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##FRDT            8
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##TODT            8
     C                   Z-ADD     EORCN         ##RCN
     C                   SELECT
     C*
     C*** ORIGINAL BILL
     C*
     C     EORCN         WHENEQ    *ZEROS
     C                   MOVEL     EOCRDT        ##RCVD
     C*
     C*** RECONSIDERATION
     C*
     C     EORCN         WHENNE    *ZEROS
     C                   Z-ADD     EORCN         KKRCN
     C*
     C                   MOVE      *HIVAL        KKRRDT
     C                   MOVE      'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     ESRRDT        ##RCVD
     C                   ELSE
     C     EORCN         SUB       1             KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     ESRRDT        ##RCVD
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDSL
       //*** Beg Add *** 01/03/23 *********************************************
       // Call new Claim Payment program if installed
       If isModEnabled('New Payments');
          Open WMCML1;
          Chain (EOco#:#1WCF#:EOfyr:EOcase) pcm02;
          If %Found(WMCML1);
             clmRrn = DbRrn2;
          Endif;
          inMode        = 'I';
          inClmRrn      = ClmRrn;
          inPmtRrn      = 0;
          inTaxId       = EOfein;
          inModifier    = EOmod;
          inFromDt      = EOfrDt;
          inToDt        = EOtoDt;
          inEob#        = EOeob#;
          inRecon#      = EOrcn;
          inSysType     = EOsys;
          inBillType    = EObTyp;
          If EPtax = 'Y';
             inPayment = EOpaid;
          Else;
             inPayment = scTpmt;
          Endif;
          inTransCode   = EOtrCd;
          inPayStatCode = *blanks;
          inPayValue1   = *blanks;
          inPayvalue2   = *blanks;
          inRecvDt      = %Dec(##RcvD:7:0);
          outReturnCode = *blanks;

          NewPaymentPgm(inMode:inClmRrn:inPmtRrn:inTaxId:inModifier:inFromDt:
           inToDt:inEob#:inRecon#:inSysType:inBillType:inPayment:inTransCode:
           inPayStatCode:inPayValue1:inPayValue2:inRecvDt:outReturnCode);
          If %Open(WMCML1);
             Close WMCML1;
          Endif;
       Else;
       //*** End Add *** 01/03/23 *********************************************
       // Determine if paid amount matches amount being passed to the
       // payment request program.
     C*
     C*
     C                   CALL      'W0080R'
     C                   PARM                    ##PRDS
     C                   PARM      'I'           ##CALL
     C                   PARM                    ##WCF#
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##FEIN
     C                   PARM                    ##MOD
     C                   PARM                    ##FRDT
     C                   PARM                    ##TODT
     C                   PARM                    ##AMT
     C                   PARM                    ##EOB#
     C                   PARM                    ##PNAM
     C                   PARM                    ##TRCD
     C                   PARM      ' '           ##KC              1
     C                   PARM                    ##RCVD            7
     C                   PARM                    ##RCN             3 0
     C                   PARM      EOSYS         ##SYS             1
     C                   PARM      EOBTYP        ##BTYP            1
      /Free
      /End-free
     C     ##KC          IFEQ      '1'
     C     ##CLT#        ANDEQ     '133'
     C                   MOVE      'C'           @@MODE
     C*
     C                   SETON                                        90
     C*
     C                   SELECT
     C     @@CPDF        WHENEQ    'C'
     C                   MOVEL(P)  'Y'           SCCOMP
     C                   MOVEL(P)  ' '           SCPEND
     C*
     C     @@CPDF        WHENEQ    'P'
     C                   MOVEL(P)  'Y'           SCPEND
     C                   MOVEL(P)  ' '           SCCOMP
     C*
     C                   ENDSL
     C*
     C                   EXSR      PRECHG
     C*
     C                   ENDIF
       //*** Beg Add *** 01/03/23 *********************************************
       Endif;
       //*** End Add *** 01/03/23 *********************************************
     C*
     C                   ELSE                                                   XXACHK = Y
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         QZRRN
     C                   ENDIF                                                  *IN99
     C*
     C                   MOVEL(P)  EOFEIN        QZFEIN
     C                   MOVEL(P)  EOMOD         QZMOD
     C                   MOVEL(P)  EOEOB#        QZEOB#
     C                   CALL      'F7303R'
     C                   PARM      *INKC         QZINKC            1
     C                   PARM      *INKL         QZINKL            1
     C                   PARM      'C'           QZCALL            1
     C                   PARM                    QZRRN             9 0
     C                   PARM                    QZEOB#            9
     C                   PARM                    QZFEIN            9
     C                   PARM                    QZMOD             3
     C                   PARM      EOSYS         QZSYS             1
     C                   PARM      EOBTYP        QZPRTY            1            N/H
     C*
     C                   ENDIF                                                  XXACHK NE 'Y'
     C                   ENDIF                                                  EOCPST NE 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##OUTS = 'N'
     C                   ENDIF                                                  ##BLST = 'C'
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEOP
     C                   ENDSR
     C*================================================================
     C* GETNTWK - Get Network
     C*================================================================
     C     GETNTWK       BEGSR
     C                   EVAL      XXNTWK = 'N'
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      '0'
     C     NWSTAT        ANDEQ     'A'
     C                   EVAL      XXNTWK = 'N'
     C                   ELSE
     C                   EVAL      XXNTWK = 'Y'
     C                   ENDIF
     C*
     C                   ENDSR
      /free
       //===============================================================
       // SET_EDI_FL : Set EDI Flag
       //===============================================================
       Begsr Set_EDI_Fl;

         //*** Beg Chg *** 07/18/23 **************************************
         //*** Setll deky2 FDDEL1;
         //*** Reade deky2 FDDEL1;
         //*** Dow not %Eof(FDDEL1);
            //*** If %trim(DEeob) = '61';
         Setll deky2 FDDEL6;
         Reade deky2 FDDEL6;
         Dow not %Eof(FDDEL6);
            If %trim(DEeobCd) = '61';
         //*** End Chg *** 07/18/23 **************************************
               Chain @@rrn FAEOP;
               If %Found(FAEOP);
                  eotape = 'N';
                  eopgm = q1pgm;
                  eotime = dstime;
                  eodt = %dec(%date():*cymd);
                  Update DBRCDF;
               Endif;
               ##rtcd = *blanks;
               Callp FA740R (@@rrn:##rtcd);
               leave;
            Endif;
            //*** Beg Chg *** 07/18/23 **************************************
            //*** Reade deky2 fddel1;
            Reade deky2 fddel6;
            //*** End Chg *** 07/18/23 **************************************
         Enddo;

       Endsr;
       //===============================================================
      /end-free
