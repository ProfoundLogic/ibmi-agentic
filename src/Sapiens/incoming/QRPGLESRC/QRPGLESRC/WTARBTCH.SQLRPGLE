     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTARBTCH - Work with A/R Batches
     h dftname(WTARBTCH)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 07/20/22                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2675 - Redesign of Work with A/R Batches       *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/05/25 C:300 Support: 119231 (CoveRisk)                    *
      //                 Bad A/R batch record having 0 posting amount  *
      //                 and Blank A/R code got created when user click*
      //                 Cancel button on 'Auto Cash Dissolve screen'  *
      //                 While adding new Cash Dissolve Master Batch   *
      //                                                               *
      //  04/11/25 C:300 Support: 118839 (SUNZ)                        *
      //                 1) DSPF Changes:                              *
      //                 Change Group field from type-ahead to drop-down
      //                 2) Problem with batches over $9,999,999- Error*
      //                 in details grid, receiver too small to hold result
      //                                                               *
      //  04/10/25 C:300 Support: 118851                               *
      //                 Initialize Employer Name in Batch Details Grid*
      //                 When Employer number is 0, it showing Employer*
      //                 name from previously loaded Batch record      *
      //                                                               *
      //                 Scenario -- These EFT batches are created from*
      //                 'Create EFT Batch' screen (W1256R) and do not *
      //                 have any data values in detail batch record   *
      //                                                               *
      //  04/08/25 C:300 Support: 118724 (CoveRisk)                    *
      //                 DSPF CHanges:                                 *
      //                 1) On Master Batch Entry, Default Batch Amount*
      //                    field from .00 to Blank                    *
      //                 2) On Master Batch Edit, highlight Batch Amount*
      //                    field using onfocus/onblur property        *
      //                 3) On Batch Detail Entry, set focus on Group field
      //                 4) On Batch Detail Entry, set focus on Pending*
      //                    Posting Amount on clicking Save Button     *
      //                    and highlight the field using onfocus/onblur
      //                 5) On Batch Detail Entry, set focus on Employer
      //                    Number on clicking Save/Add Next Button    *
      //                                                               *
      //  03/21/25 C:342 Support: 118705                               *
      //                 - Fix AR Auto Cash Dissolve Distribution Error*
      //                   when manually updating auto cash dissolve   *
      //                   subfile.                                    *
      //                                                               *
      //  03/19/25 C:333 Project: WC2990                               *
      //                 Noticed below existing issues.                *
      //                 - When the screen first loads, the cursor     *
      //                   selects the first row, but scBat# holds the *
      //                   the batch number of the bottom row, causing *
      //                   issues with the locking process.            *
      //                 - When the screen reloads, it loses the       *
      //                   currently selected row.                     *
      //                 - If the last batch detail record is deleted, *
      //                   the batch is also removed from the master   *
      //                   grid. However, Bat_sav# still retains the   *
      //                   deleted batch number, causing issues with   *
      //                   the locking process.                        *
      //                                                               *
      //  03/18/25 C:300 Support: 118661 (LUBA)                        *
      //                 A/R Batch Entry not accepting entries         *
      //                 Receiver value too small to hold result       *
      //                                                               *
      //  02/21/25 C:300 Support: 118558 (CoveRisk)                    *
      //                 A/R Batch Entry screen extremely Slow to Load *
      //                 even after applying the Filter.               *
      //                 Changing strSqlCursor to build it over Filter *
      //                 and Replacing 2 SQL queries with loop         *
      //                                                               *
      //  02/20/25 C:333 Project: WC2990  (#118555)                    *
      //                 -  verify the Approval and Transfer flags,    *
      //                  before subgrid action.                       *
      //                                                               *
      //  02/11/25 C:300 Support: 118444 (CoveRisk)                    *
      //                 1) Showing incorrect 'Insufficient Authority' *
      //                 error message on A/R code field while editing *
      //                 A/R Batch Detail record                       *
      //                 2) Fix error with User Group Restriction on   *
      //                 A/R Master Batch Delete                       *
      //                 3) fix Group Restriction error on cash dissolve batch
      //                                                               *
      //  02/03/25 C:300 Support: 118386 (CoveRisk)                    *
      //                 DSPF: Add a column for A/R Code in the A/R    *
      //                       Batch Details Grid                      *
      //                                                               *
      //01/31/25 C:333 Support: 118268                                 *
      //               A/R Batch Entry - Not Processing for Credit     *
      //               Amounts to Wash Until All Used Appropriately    *
      //               -A/R batch detail screen is not working properly*
      //                for negative check amount.                     *
      //                                                               *
      //01/07/25 C:342 Support: 118051                                 *
      //               1.removed edit on the add of A/R Batch to allow *
      //                 Cash Dissolve on EFT                          *
      //                                                               *
      //  12/16/24 C:333 Project: WC2990                               *
      //                 A/R Batch: Update Locking Process.            *
      //                 -Replacing current locking process with       *
      //                  SPILOCK API.                                 *
      *                                                               *                            *
      * 12/06/24 C:062 Ticket: 115609(133)                            *
      * 08/27/24 C:062 Check to see if a payment is being reversed    *
      *                and create window to confrm if they want to    *                            *
      *                resend the invoice to IVC.                     *                            *
      //                                                               *
      //  11/06/24 C:342 Support: 117962                               *
      //                 Check batch lock when using below options.    *
      //                  1. Flag for approval.                        *
      //                  2. Flag for transfer.                        *
      //                  3. Edit/delete on master grid                *
      //                  4. Edit/delete on detail grid                *
      //                                                               *
      //  10/24/24 C:300 Support: 117899 (Prescient)                   *
      //                 A/R Batch Detail Entry screen is unable to select
      //                 'NS' A/R Transaction Code from the selection menu
      //                 DSPF: Changes in rcdBatDtl to set 'max choice'*
      //                       to 25 for AR Code selection field       *
      //                       (which is by default 10 for auto complete)
      //                                                               *
      //  10/11/24 C:342 Support: 117058 (Cove Risk)                   *
      //                 1.Changes done to properly validate group/emp *
      //                   restriction for batch#                      *
      //                 2.Changes done to properly validate linked    *
      //                   batch# (which is created for 146WKR)        *
      //                                                               *
      //  09/24/24 C:300 Project: WC2675, Support: 117038 (SUNZ)       *
      //                 DSPF: Change visibility indicator of Edit and *
      //                       Delete links in 1st grid to inquiry_1   *
      //                       with Indicator format (Visible/Hidden)  *
      //              1) Do not include zero checks in check Crossfoot *
      //                 errors window (08/11/98 changes in W1210R)    *
      //              2) Do not show Edit,Delete links in A/R Batches  *
      //                 Master Grid, if any of the Transfer Approved  *
      //                 or Transfer Flagged is Y.                     *
      //              3) To indicate Batch Transfer in progress --     *
      //                 'Amount to be Posted' should keep reducing as *
      //                 records are transferring, if user Refresh Grid*
      //              4) Reload A/R Batch Details Grid when filter is  *
      //                 applied in A/R Batch Master Grid              *
      //              5) Check lock on Trnsfr data area before reading.*
      //                 Batch Transfer job create Exclusive lock on   *
      //                 Trnsfr data area, Which casue program error   *
      //                 for 'Flag for Approval' and 'Flag for Transfer'
      //              6) Do not load A/R Batch Details Grid when there *
      //                 are over 9999 records. Always ask user to use *
      //                 Filter button on those batches.               *
      //                                                               *
      //  09/17/24 C:300 Project: WC2675, Support: 117537 (MHCA)       *
      //              1) A/R Batch Error - Data Area ACHBAT not found. *
      //                 ACHBAT Data Area not available on each client,*
      //                 First (In *dtaara) command reads both data    *
      //                 areas (trnsfr and achbat) and get above error *
      //              2) Show 'Create EFT File' options only when      *
      //                 WCFL24 or WCFL36 flag is 'Y' (like in W1200R) *
      //                                                               *
      //  08/07/24 C:300 Project: WC2675, Support: 117254              *
      //                 Adding User defined description for A/R Code  *
      //                 on Add/Edit A/R Batch details pop-up screen   *
      //                 DSPF:1) Added User defined description for A/R*
      //                         code on rcdBatDtl record format       *
      //                      2) Corrected Posting Amount field, it was*
      //                         a type-ahead                          *
      //                                                               *
      //  08/02/24 C:300 Project: WC2675, Support: 117182              *
      //               1)A/R Batch Details Entry - Default Group After *
      //                 1st Detail Entry but Allow to Change          *
      //               2)2nd Detail Entry not showing the batch as Cash*
      //                 Dissolve                                      *
      //               3)set Focus on Add/Edit A/R Batch details pop-up*
      //                 DSPF: Added set focus field in rcdARBtch and  *
      //                 rcdBatDtl record formats                      *
      //                                                               *
      //  08/01/24 C:300 Project: WC2675, Support: 117038              *
      //                 DSPF: Apply Filter to A/R Batch Details Grid  *
      //                 Check for 9999 records in A/R Batch Details Grid*
      //                                                               *
      //  07/31/24 C:300 Project: WC2675, Support: 117038              *
      //                 Using SQL Cursor to load AR Batch Grid faster *
      //                 At SUNZ, WDEAP file has Millions of records   *
      //                 and thosands of records for each batch        *
      //                                                               *
      //  07/26/24 C:300 Project: WC2675, Support: 117038              *
      //                 DSPF: Apply Filter to Main A/R Batch Grid     *
      //                 DSPF: Added new Column for 'Transfer Date'    *
      //                 Show Transfer Status 'Pending' if its Blank   *
      //                 Check for 9999 records in Main A/R Batch Grid *
      //                                                               *
      //  06/24/24 C:300 Project: 146WKR (Prescient)                   *
      //                 Collateral Processing                         *
      //                                                               *
      //  05/06/24 C:300 Project: WC2675, Support: 115549              *
      //                 Auto-cash dissolve AR Batch Creating Detail   *
      //                 Record with 0 Employer #                      *
      //                                                               *
      //  04/26/24 C:342 Project: WC2675,                              *
      //                 Check Crossfoot error when check amount       *
      //                 entered does not match total posted amount    *
      //                 DSPF changes:                                 *
      //                 Added record format rcdCrossFt.               *
      //                                                               *
      //  04/22/24 C:300 Project: WC2675, Support: 115470              *
      //                 Allow to Edit auto-cash dissolve Information  *
      //                 Like old program, do not delete Master batch (all 9)
      //                 record for auto-cash dissolve batch (EAcash=Y)*
      //                                                               *
      //  04/19/24 C:300 Project: WC2675, Support: 115469              *
      //                 Delete all records of the selected batch when *
      //                 deleting from master grid                     *
      //                 Also Show batch# in the Confirmation message  *
      //                 While deleting A/R Batch record               *
      //                                                               *
      //  04/18/24 C:300 Project: WC2675, Support: 115452              *
      //                 Program Error on Auto Cash Dissolve Grid      *
      //                 Increasing the size of arrays from 12 to 99   *
      //                                                               *
      //  04/15/24 C:300 Project: WC2675, Support: 115391              *
      //                 On Add Batch Details info screen, If no values*
      //                 then 'Activty Status' field should be Blank.  *
      //                 It should not look like typeahead input field *
      //                 Changes in DSPF and WTARBTCH02 ajax program.  *
      //                                                               *
      //  01/17/24 C:300 Project: WC2675                               *
      //                 Handle Employer Number Search Error when user *
      //                 selects incomplete Policy on A/R Batch Details*
      //                 2) Fix reload issue on Refresh                *
      //                                                               *
      //  01/12/24 C:300 Project: WC2675                               *
      //                 DSPF: Make Search iocn out of tabbing sequence*
      //                 on Add/Edit A/R Batch Details record rcdBatDtl*
      //                                                               *
      //  12/18/23 C:300 Project: WC2675                               *
      //                 DSPF: Remove 'Work With' from Page Title      *
      //                 Heading and Tab Name                          *
      //                                                               *
      //  12/11/23 C:300 Project: WC2675, Support: 114247              *
      //                 Fix Unexpected screen error enountered during *
      //                 Edit/Delete of A/R Batch Master record        *
      //                                                               *
      //  11/07/23 C:300 Support: 98110                                *
      //                 DSPF: Add Invoice # to A/R Batch Detail box   *
      //                                                               *
      //  10/23/23 C:300 Project: WC2675, Support: 114389              *
      //                 1) Show 'Cash Dissolve Batch' flag correctly  *
      //                 in the detail box                             *
      //                 2) Read wdeal9 using key to Correct few Edits *
      //                 of 'Flag for Approval' and 'Flag for Transfer'*
      //                                                              *
      //  10/20/23 C:65  SUPPORT:114398 (InHouse)                      *
      //                 Remove the edit on preventing billing trans-  *
      //                 actions from being posted to a prior month.   *
      //                 This will prevent posting possibly needed     *
      //                 adjustments for IVC.  The estimated, revised, *
      //                 and audit bills already allow for back posting.*
      //                                                               *
      //  10/18/23 C:300 Project: WC2675                               *
      //                 Reload A/R Batch Master Grid to show 'Batch   *
      //                 Transmitted' info in detail box after runing  *
      //                 'Create EFT File' Action                      *
      //                                                               *
      //  10/16/23 C:300 Project: WC2675                               *
      //                 Check for correct swc01 file                  *
      //                                                               *
      //  10/09/23 C:300 Project: WC2675, Support: 114172              *
      //                 Delete Batch record with Group 0, Emp-Div 0-0 *
      //                 as soon as a batch detail record is created   *
      //                 And also correct space in an error message    *
      //                                                               *
      //  10/05/23 C:300 Project: WC2675, Support: 114172              *
      //                 1)With 'Create EFT File' Action, getting error*
      //                 'Invalid EFT setup for Group 0, Emp-Div 0-0'  *
      //                 2)Update co# in ValidateAct()                 *
      //                                                               *
      //  09/22/23 C:300 Project: WC2675, Support: 113641              *
      //                 DSPF - on 'Inquiry by Employer Number' window *
      //                 Keep Division and Year search icons out of    *
      //                 tabbing flow                                  *
      //                                                               *
      //  09/13/23 C:300 Project: WC2675, Support: 113576              *
      //                 Do not reposition selected AR Batch record to *
      //                 the top row of grid When user selects any     *
      //                 A/R Batch record from the middle of the list  *
      //                 OR on using any of the action items found in  *
      //                 "--- Select List Action ---".                 *
      //                 'Position At Top' property removed for sflARBtch*
      //                 AR Batch Master grid in DSPF                  *
      //                                                               *
      //  09/12/23 C:300 Project: WC2675, Support: 113851              *
      //                 'Persist State' property missing for sflARBtch*
      //                 AR Batch Master grid in DSPF                  *
      //                                                               *
      //  09/11/23 C:342 Project: WC2675, Support: 113577              *
      //                 1.Fixed visibility issue for few fields on Add*
      //                 Batch Information screen using "Save/Add Next"*
      //                 button.                                       *
      //                 2.Validate "Agent not effective as of the     *
      //                 transaction date " error message only when    *
      //                 valid year is entered.                        *
      //                 3.Initialize error indicator for rcdAutoDss   *
      //                 record format when user cancels transaction   *
      //                 and edit again.                               *
      //                 4.Evaluate EAtrdt when "Transaction Date for  *
      //                 Retro Batches" field is hidden.               *
      //                 5.Initialize error indicator for when user    *
      //                 search for "Employer" or "AR Transaction"     *
      //                 using search icon.                            *
      //                 6.Validate 'EFT setup is not valid for this   *
      //                 employer/division" error only when valid      *
      //                 employer information is entered.              *
      //                                                               *
      //  09/08/23 C:300 Project: WC2675, Support: 113613              *
      //                 Initialize Transfer Date in 'AR Batch Inquiry *
      //                 by Employer Number' grid                      *
      //                 Load 'AR Batch Inquiry' grid in Batch # order *
      //                 instead of Posting Date oder                  *
      //                                                               *
      //  09/05/23 C:300 Project: WC2675, Support: 113584              *
      //                 1.Do not allow EDIT or DELETE if either the flag
      //                 for approved or flag for transfer is set to "Y"
      //                 2.Leave out 'Mass Update' functionality from  *
      //                 "--- Select List Action ---".                 *
      //                                                               *
      //  09/04/23 C:300 Project: WC2675, Support: 113611              *
      //                 Include Purge Date in the Purge Record Deleted*
      //                 Confirmation Message Window                   *
      //                                                               *
      //  09/04/23 C:300 Project: WC2675, Support: 113605              *
      //                 Program Call Error with Create EFT File option*
      //                                                               *
      //  09/04/23 C:300 Project: WC2675, Support: 113591              *
      //                 Include-Exclude All Buttons Not working       *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTARBTCH  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflARBtch:scSflRRN)
     f                                     Sfile(sflBtchDtl:rrnBtchDtl)
     f                                     Sfile(sflAutoDss:rrnCashDtl)
     f                                     Sfile(sflPurHist:rrnPurHist)
     f                                     Sfile(sflBatInq:rrnBatInq)
     f*** Beg Add ***** 04/26/24 *****************************
     f                                     Sfile(sflCrossFt:scCFSflRRN)
     f*** End Add ***** 04/26/24 *****************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
      * A/R Batch file
     fwdeal9    if a e           k disk    rename(pea01:pea09)
     f                                     infds(dbfds)
     f                                     prefix(L9_)
     fwdeal4    uf   e           k disk    rename(pea01:pea04)
     f                                     prefix(L4_)
      *** Begin Add ***** 06/24/24 *****************************
     f                                     infds(dbfds5)
      *** End   Add ***** 06/24/24 *****************************
     fwdeap     uf a e             disk    rename(pea01:pea0p)
     F                                     INFDS(FIDS02)
     fwdeal     if   e           k disk    rename(pea01:pea00)
     f                                     infds(dbfds2)
     fwdeal2    if   e           k disk    rename(pea01:pea02)
     f                                     prefix(L2_)
     f                                     infds(dbfds4)
     fwdeal8    if   e           k disk    rename(pea01:pea08)
     f                                     prefix(L8_)
      * Batch Purge file
     fwmbpp     uf   e             disk    rename(pbp01:pbp0p)
     fwmbpl     if a e           k disk    rename(pbp01:pbp00)
     f                                     infds(dbfds3)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
      * Agent History Master File
     fwmahl     if   e           k disk
      * Employer Details File
     fwdelp     if   e             disk
     fwdell     if   e           k disk    rename(pel01:pel02)
     f                                     infds(dbwdell)
     fwdell2    if   e           k disk    rename(pel01:pel0l2)
      * Employer Cross Reference File
     fwzexl     if   e           k disk
      * Fund Detail File
     fwdf1l     if   e           k disk
      * Group Aux File
     fwdf2l     if   e           k disk
      * Employer Accounts Receivable File
     fwmeal8    if   e           k disk    rename(PEA01:peaM8) prefix(M8)
      * Cash Dissolve Hierarchy File
     fwdcdl     if   e           k disk    rename(pcd01:pcd00)
      * A/R Premium Category Info
     fwtc3l     if   e           k disk    rename(pc301:pc300)
      *
     Fwtabl     if   e           k disk
     F                                     INCLUDE(PTT01)
     F                                     INCLUDE(PSY01)
      // for ARCashDiss
     f* Auto Cash Dissolve Entries
     fwzeap     uf a e             disk    usropn
     f                                     rename(pea01:peazp)
     f                                     prefix(D)
     fwzeal     if   e           k disk    usropn
     f                                     rename(pea01:peaz0)
     f                                     prefix(D0)
     f                                     infds(dbfdsz)
     f*** Beg Add ***** 04/26/24 *****************************
     f* Fund master File
     fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     f*** End Add ***** 04/26/24 *****************************

       //***** Beg Del ***** 12/16/24 ***************************
       //*** Dcl-F WXRIL disk usage(*update) keyed;
       //*** Beg Chg *** 11/06/24 *******************************************
       //*** Dcl-F WXRIL3 disk usage(*update) keyed rename(PRI01:PRI03) prefix(L3);
       //*** Dcl-F WXRIL3 disk usage(*update:*output:*delete) keyed
       //*** rename(PRI01:PRI03) prefix(L3);
       //*** End Chg *** 11/06/24 *******************************************
       //*** Dcl-Ds KeyWXRIL  likerec(PRI01:*KEY);
       //*** Dcl-Ds KeyWXRIL3 likerec(PRI03:*KEY);
       //***** End Del ***** 12/16/24 ***************************
       Dcl-Ds FIDS02;
          dbrrn02 int(10) pos(397);
       End-Ds;

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
      *** Begin Add ***** 06/24/24 *****************************
     d dbfds5          ds
     d  dbrrn5               397    400b 0
      *** End   Add ***** 06/24/24 *****************************
     d dbfdsz          ds
     d  dbrrnz               397    400b 0
     d dbwdell         ds
     d  dbwdell1             397    400b 0

     d ARCode          s             10A   inz(*blanks)
     d cashDissolveFl  s               n
     d currYear        s                   like(ELfyr)
     d rnewYear        s                   like(ELfyr)
     d svcash          s                   like(EAcash)
     d svidte          s                   LIKE(EAidt)
     d svipgm          s                   LIKE(EAipgm)
     d sviusr          s                   LIKE(EAiusr)
     d svidt           s                   LIKE(EAidt)
     d kkhier          s                   like(CDHIER)

      // work variables
     d saveDT          s                   like(EAdt)
     d saveTM          s                   like(EAtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d @@mode          s              1
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d strErrMsg       s            256    inz
     d isDelResp       s               n   inz
     d*
      // Flag for Multi Group Restrictions
     d multig          s              1n   inz(*off)
     d kkstat          s              1    inz(' ')
     d relRcd#         s              9  0 inz(0)
     d @@bhdr          s              9  0
     d ARDtlRrn        s              9  0
      *** Begin Add ***** 10/09/23 *****************************
     d savEAPrrn       s              9  0                                      save RRN of WDEAP
      *** End   Add ***** 10/09/23 *****************************
      *** Begin Add ***** 08/05/25 *****************************
     d svBtnARCan      s               n   inz(*off)
      *** End   Add ***** 08/05/25 *****************************
     d*
      *** Begin Add ***** 07/26/24 *****************************
     d str9999Msg      s            256
      // Filter screen variable
     d meetFltr        s               n
     d dsFilter        ds                  likerec(rcdARBFltr) Inz
      *** End   Add ***** 07/26/24 *****************************
      *** Begin Add ***** 08/01/24 *****************************
     d meetFltr2       s               n
     d dsFilter2       ds                  likerec(rcdARFltr2) Inz
      *** End   Add ***** 08/01/24 *****************************
      *** Begin Add ***** 09/24/24 *****************************
     d str9999Msg2     s            256    Inz
     d BatDtlCount     s              7  0 Inz
      *** End   Add ***** 09/24/24 *****************************

      // for Add process processaddbtch
     d isInvoiceCloud  s              1n   Inz
     d isICgroup       s              1n   Inz
     d strArKey        s             10
     d intnInv         s                   Like(EAinv#)
     d Add2fl          s              1A   inz(*blanks)

      // for getdts
     D svdte           s                   LIKE(EAidt)
     D DTflg           S              1n
     D DtChgd          S              1n
     D IDTflg          S              1n
     D Lesflg          S              1n
     d*

      // new varibale for search
     d pocym           s              5  0 inz(*zeros)
     d dtcym           s              5  0 inz(*zeros)
     d rbcym           s              5  0 inz(*zeros)
     d*
      // variable for call
     d chrChkType      s             10    inz
     d chrEftType      s              1    inz('E')
     d chrEftKey       s             30    inz
     d chrEfDt         s              7    inz
     d chrEobType      s              3    inz
     d chrCo#          s              3    inz
     d chrFnd          s              3    inz
     d chrFyr          s              3    inz
     d chrPre          s              1    inz
     d chrRRN          s              9    inz
     d erEftSetUp      s            200    inz
     d*
     d outCo#          s              3    inz
     d outGroup        s              3    inz
     d outEmp#         s              9    inz
     d outDiv          s              5    inz
     d outYear         s              3    inz
     d outEffDt        s              8    inz
     d outMode         s              1    inz
     d inError         s              1    inz
     d ioData          s            100    inz
     d outBat#         s              9    inz
     d outDate6        s              6    inz
     d outOutQ         s             10    inz
     d outdate         s              8
     d outDt8          s              8  0 inz
     d outFfmt         s              8    inz
     d outTfmt         s              8    inz
     d inDt10          s             10    inz
     d*
     d svsflrrn        s              5  0 inz
     d inInclude       s             10
     d inIncludeP      s             10
     d xxgrst          s              1    inz
     d mtxt            s             22    inz
     d jusr            s             10    inz
     d trnsfr          s              1    dtaara
     d istrnsfr        s              1    inz
     d achbat          s             30    dtaara
     d kkkey           s             10
      // for hival
     d HiChkNo         s              7  0 inz(*hival)
     d HiChkAmt        s              9  2 inz(*hival)

      // SYtype and cash not dissolve loop
     D xxDT            s                   like(EApodt)
     D billTrans       s              1n

      // SYtype and cash not dissolve loop
     D xxRbDt          s                   like(EApodt)

      // save details of 1st record
     d co#_sav         s                   like(EAco#)
     d stat_sav        s                   like(EAstat)
     d bat#_sav        s                   like(EAbat#)
     d cash_sav        s                   like(EAcash)
     d reload          s               n   inz(*off)
     d svrrnPrd        s              9  0 inz
      *** Begin Add ***** 06/24/24 *****************************
     d btyp_sav        s                   like(EAachf)
     d xxlnkbat#       s                   like(EAbat#)
      *** End   Add ***** 06/24/24 *****************************

      // reload screen field
     d WtPolARflg      s               n   inz(*off)
     d isError1        s               n   inz(*off)

     d xxbal           s                   like(EAbata)
     d xxfnd           s                   like(EAfnd)
     d xxdiv           s                   like(EAdiv)
     d xxemp#          s                   like(EAemp#)

     D cashDisolve     s              1n
     D svcashDissolve  s              1n
     D xxPoDt          s                   like(EApodt)
      *** Beg Add ***** 01/31/25 ************************
     d xxChkInd        s               n   inz(*off)
      *** End Add ***** 01/31/25 ************************

      // calling parameter for WtPolARInq
     d @@rrn           s              9
     d @@next          s             10
     d @@tabind        s              5
     d @@Qte           s              1
     d @@PostDt        s              8  0
     d @@PostAmt       s             11  2

     d @@mode1         s              1

      // calling parameter for autority
     D $$appl          s              1
     D $$auth          s              1
     D $$co#           s              3  0
     D $$emp#          s              9  0
     D $$fnd           s              3  0
     D $$rtcd          s              1
     D $$usr           s             10
     d failed          s              1n

      // calling parameter for autority
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@emp#          s              9  0
     D @@div           s              5  0
     D @@fyr           s              3  0
     D @@agt#          s              5  0
     D @@anam          s             60
     D @@prod          s              5  0
     D @@pnam          s             60
     D @@area          s              3  0
     D @@phon          s              7  0
     D @@plan          s              1
     D @@date          s              8  0
     d ckplan          s              1n

      // calling parameter for SRCCSC
     D #2utyp          s              1
     D #2ulvl          s              3
     D #2ctyp          s              1
     D #2clvl          s              3
     D #2exec          s              1
     D #2res           s              1
     D #2ctp2          s              1
     D #2ctp3          s              1
     D #2ctp4          s              1
     D #2ctp5          s              1
     D #2ctp6          s              1

     d ##co#           s              3  0
     D ##call          s              1
     D earrn           s              9  0
      *** Beg Add ***** 12/16/24 ******************************
     d infndVar        s              3  0 inz(0)
      *** End Add ***** 12/16/24 ******************************

      // calling parameter saved for javascript program
     d svcash1         s              1
     d svtrdt1         s              7  0
     d svachf1         s              1

     d achbatds        DS
     d  xxjast                 1      1    inz
     d  xxjusr                 2     11    inz
     d  xxjbat                12     20  0 inz
     d  xxfill                21     30    inz

      // new varibale for search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      // Proto-types

      // Process AR Cash Dissolve payment screen
     d ARCashDiss      pr
     d earrn                          9  0

      *** Beg Add ***** 12/16/24 ***************************
      // Check Lock
     d CheckLock       pr              n
     d inco#                          3  0
     d infnd                          3  0
     d inemp#                         9  0
     d intype                        10    value
      *** End Add ***** 12/16/24 ***************************

      // Check Authority
     dChkAuthority     pr                  ExtPgm('SRCCSC')
     d inUTYP                         1    const
     d inULVL                         3    const
     d inSTYP                         1    const
     d inSLVL                         3    const
     d inEXEC                         1    const
     d outRes                         1

     d*** Beg Add ***** 04/26/24 *****************************
      // Check and load crossfoot error
     dChkCrossFt       pr
     d*** End Add ***** 04/26/24 *****************************

      // Check SROnline
     dChkSROnline      pr                  ExtPgm('SRONLI')
     d outCo#                         3    const
     d outGroup                       3    const
     d outEmp#                        9    const
     d outDiv                         5    const
     d outYear                        3    const
     d outEffDt                       8    const
     d outMode                        1    const
     d inError                        1
     d ioData                       100

      // Create EFT file
     dCrtEFTf          pr                  ExtPgm('W1260C')
     d outCo#                         3    const
     d outBat#                        9    const
     d outDate6                       6    const
 
      // Dissolves eligible Payments across A/R balances
     d DissolvePay     pr

      // Convert CYMD (7,0) date to YMD (6,0)
      //*** Begin Chg ***** 09/04/23 *****************************
      // dDtYMD            pr                  ExtPgm('W1260C')
     dDtYMD            pr                  ExtPgm('SRCVTDAT')
      //*** End   Chg ***** 09/04/23 *****************************
     d outDt8                         8  0 const
     d outFfmt                        8    const
     d outTfmt                        8    const
     d inDt10                        10    const
     d inError                        1    const

      // Edit Open Batches (More Action Item Toolbar)
      // and  Edit Report (A/R Batch Edit)
     dEdtReport        pr                  ExtPgm('W9200C')
     d outCo#                         3    const
     d outBat#                        9    const
     d outOutQ                       10    const
 
      // Get Employer Info and Balance
     d EmpInf          pr
 
      // search employer number
     d EmployerSrch    pr
 
      // Look for invoice number in WMEAP and WDEAP
     d FindInv#        pr

      // Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      // Get Value
     d getValue        pr                  like(EAamt)
     d  ArCode                             like(EAarcd) const
     d  ArAmt                              like(EAamt)  const

      // Initialize batch Detail Record error indicators
     d InzBatDtErr     pr

      // Load A/R Batch Detail Grid
     d LoadARBatch     pr

      // Load A/R Batch Inquiry Grid
     d LoadARBatInq    pr

      // Load A/R Batch Master Grid
     d LoadARBGrid     pr
     d inIncludeBat                  10    const

      // Load A/R Batch Purge History Grid
     d LoadPurHist     pr
     d inIncludeBatP                 10    const

      * Main Processing
     d Main            pr

      *** Begin Add ***** 07/26/24 *****************************
      // Meet Filter for AR Batch Master Grid
     d MeetFlt         pr
      *** End   Add ***** 07/26/24 *****************************

      *** Begin Add ***** 08/01/24 *****************************
      // Meet Filter for AR Batch Details Grid
     d MeetFlt2        pr
      *** End   Add ***** 08/01/24 *****************************

      // Employer search
     d outSelect       ds                  likeds(dsSrchCrit)
     d isEmpNbr        s              1
     d intEmpRrn       s              9  0
     d outCall         s              1
     d outRrn#         s              9

      // Process Pgmstr
     d PgmStr          pr

      * Process Addition of New A/R Batch
     d ProcessAdd      pr

      * Process Addition of A/R Batch Details
     d ProcessAddBtch  pr

      * Process Mass Update
     d ProcMasUpd      pr

      * Process More Action items from A/R Batch Toolbar
     d ProcMorActTB    pr

      * Process other Options for A/R batch master
     d ProcOther       pr

      * Process other Options for A/R batch detail
     d ProcOtherBD     pr

      * Process Performed Action from the action dropdown
     d ProcPerAct      pr

      * Process the Row Click in Producers Grid
     d ProcRoWClk      pr

      * Process to Update all Batch records
     d ProcUpdBat      pr
     d inrrn                          9  0
     d inmod                          1
     d inbata                        11  2
     d intrdt                         8  0

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      // Check Group Authority
     d SA008R          pr                  ExtPgm('SA008R')
     d $$co#                          3  0
     d $$usr                         10
     d $$appl                         1    Const
     d $$rtcd                         1
     d $$auth                         1    options(*nopass)
     d $$fnd                          3  0 options(*nopass)
     d $$emp#                         9  0 options(*nopass)
     d*** Begin Add *** 08/27/24 *******************************************
     d chrJson         s          32000    Varying
     d chrMessage      s            256
     d chrPanelTitle   s             50
     d*** End   Add *** 08/27/24 *******************************************

      // Call SRCCSC
     d SRCCSC          pr                  ExtPgm('SRCCSC')
     d #2utyp                         1
     d #2ulvl                         3
     d #2ctyp                         1
     d #2clvl                         3
     d #2exec                         1
     d #2res                          1
     d #2ctp2                         1
     d #2ctp3                         1
     d #2ctp4                         1
     d #2ctp5                         1
     d #2ctp6                         1

      // Transfer Batches (More Action Item Toolbar)
     d TransBat        pr                  extpgm('W9205C')
     d outCo#                         3
     d outdate                        8
     d outOutQ                       10

     * Validate Action procedure.
     d ValidateAct     pr              n
     d strmode                        1

      *** Beg Add ***** 02/20/25 *******************************************
      * Validate Action procedure for Batch detail grid
     d ValidateAct2    pr              n
     d strmode                        1
      *** End Add ***** 02/20/25 *******************************************

     * Validate A/R Batch Master Record procedure.
     d ValidateBat     pr              n
     d strmode                        1

     * Validate A/R Batch Detail Record procedure.
     d ValidateBatDt   pr              n
     d strmode                        1

      *** Begin Add ***** 07/26/24 *****************************
     * Validate value entered on filter screen for AR Batch Master
     d ValidateFlt     pr              n
      *** End   Add ***** 07/26/24 *****************************

      *** Begin Add ***** 08/01/24 *****************************
     * Validate value entered on filter screen for AR Batch Details
     d ValidateFlt2    pr              n
      *** End   Add ***** 08/01/24 *****************************

     * Validate A/R Batch Inquiry
     d ValidateInq     pr              n

     * Validate Mass Batch Update Window
     d ValidateMas     pr              n

     * Validate Purge Batch
     d ValidatePur     pr              n

      // Check Policy current account
     d WA002R          pr                  ExtPgm('WA002R')
     d @@co#                          3  0
     d @@fnd                          3  0
     d @@emp#                         9  0
     d @@div                          5  0
     d @@fyr                          3  0
     d @@agt#                         5  0
     d @@anam                        60
     d @@prod                         5  0
     d @@pnam                        60
     d @@area                         3  0
     d @@phon                         7  0
     d @@plan                         1
     d @@date                         8  0

      // call WA055C
     d WA055C          pr                  ExtPgm('WA055C')
     d ##call                         1
     d earrn                          9  0

      * Employer Search
     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d @@rrn                          9
     d @@mode                         1
     d @@next                        10
     d @@tabind                       5
     d @@Qte                          1
     d @@PostDt                       8  0
     d @@PostAmt                     11  2

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
     d/copy sPrGenErr
     d/copy wPrBank
     d/copy wPrAR
     d/copy WTSearchDs
     d/copy wPrEmplr
     d/copy wPrPmtSvc
     d/copy sPrSecure
     d*** Begin Add *** 08/27/24 *******************************************
     D/copy sPrRunCmd
     d*** End   Add *** 08/27/24 *******************************************
     d*** Beg Add ***** 12/16/24 *******************************************
     D/copy sPrLock
     d*** End Add ***** 12/16/24 *******************************************

      //
      //   Key List for the Documents Table File
      //
     C     keyWDEAL      klist
     C                   kfld                    co#_sav
     C                   kfld                    stat_sav
     C                   kfld                    bat#_sav

     C     elky2         klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE
     C                   kfld                    sysdt7

     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE

     C     eaky2         klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupI
     C                   kfld                    scYearI
     C                   kfld                    scEmpNoI
     C                   kfld                    scDivI

     C     eaky8         klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scYearE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE
      *
     c     eaky9         klist
     c                   kfld                    ##co#
     c                   kfld                    kkstat
     c                   kfld                    scbat#
     c                   kfld                    scChkNoE
     c                   kfld                    ScChkAmtE
      *
      *** Begin Add ***** 10/09/23 *****************************
     c     eaky99        klist
     c                   kfld                    EAco#
     c                   kfld                    EAstat
     c                   kfld                    EAbat#
     c                   kfld                    HiChkNo
     c                   kfld                    HiChkAmt
      *** End   Add ***** 10/09/23 *****************************
      *
      *** Begin Add ***** 10/23/23 *****************************
     c     eaky999       klist
     c                   kfld                    ##co#
     c                   kfld                    kkstat
     c                   kfld                    scbat#
      *** End   Add ***** 10/23/23 *****************************
      *
     C     eaky          klist
     C                   kfld                    ##co#
     C                   kfld                    scbat#
      *
     C     cdky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    kkhier
      *
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
      *
     C* for SYtype and cash dissolve loop
     C     ahky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE
     C                   kfld                    scYearE

     C     ahky1         klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE
     C                   kfld                    scYearE
     C                   kfld                    kktrdt

     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey


     C     eaky4         klist
     C                   kfld                    ##co#
     C                   kfld                    scbat#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE


     C     exky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE

      *================================================================
      *  *like statements
      *================================================================
     C     *like         define    EAbat#        kkbat#
     C     *like         define    EAtrdt        kktrdt
     C     *like         define    EAachf        svachf
     C     *like         define    EAfnd         svfnd
      *** Beg Add ***** 08/02/24 *****************************
     C     *like         define    EAfnd         svfnd2
      *** End Add ***** 08/02/24 *****************************
     C     *like         define    EAsycd        svsycd
     C     *like         define    EAfyr         svfyr
     C     *like         define    EAemp#        svemp#
     C     *like         define    EAdiv         svdiv
     C     *like         define    EAchk#        svchk#
     C     *like         define    EAckam        svckam
      *** Beg Chg ***** 04/11/25 *****************************
     C*****     *like         define    EAamt         svamt
     C     *like         define    EAbata        svamt
      *** End Chg ***** 04/11/25 *****************************
     C     *like         define    EAamt         svamt3
     C     *like         define    cashDisolve   sameck
     C     *like         define    EApodt        sysdt7
     C     *like         define    scPostAmtE    svamt2
     C     *like         define    EAbata        svbata
     C     *like         define    EAtfap        svtfap
     C     *like         define    EAtffl        svtffl
     C     *like         define    EAstat        svstat
     C     *like         define    EAtrdt        svtrdt
      *================================================================

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index


      /free
       Exec Sql Set Option Commit = *None;

       GetClientID(clientID);

       ProgramStart(process);

       //*** Begin Add ***** 07/26/24 *****************************
       str9999msg = 'Your request returned too many records. ' +
        'Click Continue to see the first 9999 records. ' +
        'Click Filter on the display screen to narrow your results.';

       scBstsF = 'P';                    // Initialize Filter with Pending Status
       dsFilter.scBstsF = 'P';           // Initialize Filter with Pending Status
       //*** End   Add ***** 07/26/24 *****************************

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // ARCashDiss - AR Cash Dissolve payment screen
       //===================================================================
     P ARCashDiss      B
     D ARCashDiss      PI
     d earrn                          9  0

     d isexit          s               n   Inz(*off)
     D xxDT            s                   like(EApodt)
     d isError         s               n   inz(*off)
     d I               s              4  0 inz(*zeros)

     d kkco#           s                   like(EAco#)
     d kkfnd           s                   like(EAfnd)
     d kkemp#          s                   like(EAemp#)
     d kkdiv           s                   like(EAdiv)
     d kkfyr           s                   like(EAfyr)
     d kkpodt          s                   like(EApodt)
     d kkarcd          s                   like(EAarcd)
     d elrrn           s              9  0
     d totAmt          s                   like(EAamt)  inz(0)
     d tfrAmt          s                   like(EAamt)  inz(0)
     d arType          s              1
     d svtrdt          s                   like(EAdt)
     d achFlg          s              1

      // define Arrays for A/R Codes, Headings and Selection Criteria
      // Grid 1 -- Specific A/R Transaction Codes
      //***  Beg Chg  **** 04/18/24 *******************************
     d***** strARCD1        s              2    dim(12)
     d***** strFYR1         s              3  0 dim(12)
     d***** strPodt1        s              8  0 dim(12)
     d***** strAmt1         s              9  2 dim(12)
     d***** strdbrr1        s              9  0 dim(12)
     d strARCD1        s              2    dim(99)
     d strFYR1         s              3  0 dim(99)
     d strPodt1        s              8  0 dim(99)
     d strAmt1         s              9  2 dim(99)
     d strdbrr1        s              9  0 dim(99)
      //***  End Chg  **** 04/18/24 *******************************

     c     readKy        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     ahky          klist
     c                   kfld                    ##co#
     c                   kfld                    xxFND
     c                   kfld                    xxEMP#
     c                   kfld                    xxDIV
     c                   kfld                    scFYR

     c     ahky1         klist
     c                   kfld                    ##co#
     c                   kfld                    xxFND
     c                   kfld                    xxEMP#
     c                   kfld                    xxDIV
     c                   kfld                    scFYR
     c                   kfld                    kktrdt            7 0


      /free

       // Process until exit button is pressed.
       Clear rcdAutoDss;
       BtnARCan = *off;
       isexit = *off;
       isError = *off;
       @@next = *blanks;
       //***  Beg Add  **** 09/11/23 *******************************
       exsr inzError;
       //***  End Add  **** 09/11/23 *******************************

       DoW BtnARCan = *off and isexit = *off;
          if isError = *off and @@next = *blanks;
             exsr Keyinz;
             exsr pgmstr;
             exsr read;
             scPnlHdg2 = 'Work with A/R Cash Dissovle Payments';
          endif;
          exfmt rcdAutoDss;
          //***  Beg Add  **** 09/11/23 *******************************
          If isError = *on;
             ReadC sflAutoDss;
             Dow not %eof;
                exsr inzError;
                sflChange = *on;
                update sflAutoDss;
                ReadC sflAutoDss;
             Enddo;
          Endif;
          //***  End Add  **** 09/11/23 *******************************

        // Check if year search is press
          exsr loadYear;

          Select;

          // Save Button
          When BtnARSave  = *on;
             BtnARSave = *off;
          // Read -- A/R Transaction Codes
             i = 1;
             totAmt = 0;
             tfrAmt = 0;
             ReadC sflAutoDss;
             Dow not %eof;
                strARCD1(i) = scARCd;
                strFYR1(i)  = scFYR;
                strPodt1(i) = scPodt;
                strAmt1(i)  = scAmt;
                i += 1;
                isError = *off;
                //*** Beg Chg *** 03/21/25 ***********************************
                //*** exsr ValidARSfl;
                if scARCd <> *blanks or scFYR <> *zeros or
                 scPodt <> *zeros or scAmt <> *zeros;
                   exsr ValidARSfl;
                Endif;
                //*** End Add *** 03/21/25 ***********************************
                sflChange = *on;
                update sflAutoDss;
                ReadC sflAutoDss;
             Enddo;
          // valiate total amount
                exsr errcka;

             If isError = *off;
                Exsr WrtARCode;
                isexit = *on;
                exsr pgmend;
             else;
                isError = *on;
             EndIf;

          // Cacel Button
          When BtnARCan = *on;
             //*** Begin Add ***** 08/05/25 *****************************
             svBtnARCan = *on;
             //*** End   Add ***** 08/05/25 *****************************
             BtnARCan = *off;
             isexit = *on;

          EndSl;
       enddo;

     c*================================================================
     c* WrtARCode  - Write or update
     c*================================================================
       Begsr WrtARCode;

        Readc sflAutoDss;
        Dow not %eof;
           if scdbrr = *zeros;
              exsr writer;
           else;
              exsr updatr;
           endif;
           ReadC sflAutoDss;
        Enddo;

       Endsr;
     c*================================================================
     c* ValidARSfl - Validate data (entire screen)
     c*================================================================
       Begsr ValidARSfl;

        exsr inzError;
        // Validate A/R code
        if (scARCd <> *blank and
         (scFYR = *zero or scPodt = *zero or scAmt = *zero))
         OR (scARCd = *blank and
         (scFYR <> *zero or scPodt <> *zero or scAmt <> *zero));
            erARCd = *on;
            msARCd ='WCR1083';
            erFYR = *on;
            msFYR ='WCR1083';
            erPodt = *on;
            msPodt ='WCR1083';
            erAmt = *on;
            msAmt ='WCR1083';
            isError = *on;
        endif;

        if scARCd <> *blank and erARCd = *off;
           arType = GetARType(scARCd);
           if ValidCode(scARCd) = '0' or
              (arType <> 'P' and arType <> 'I' and
               arType <> 'Q');
              erARCd = *on;
              msARCd ='WC31505';
              isError = *on;
           endif;

           // Validate year
           if ValidEmpYr(scFYR:xxemp#:xxdiv:elrrn:xxfnd) = '0'
            and erFYR = *off;
              erFYR = *on;
              msFYR = 'WCR0947';
              isError = *on;
           endif;

           // Validation Check for Posting date
           if scPodt <> *zeros and erPodt = *off;
              erPodt = ValidDate(scPodt:charMsg);
              if erPodt = *on;
                 msPodt = charMsg;
                 isError = *on;
              endif;
           endif;

           // Validate Amount
           if scAmt = 0 and erAmt = *off;
              erAmt = *on;
              msAmt = 'WCR0556';
              isError = *on;
           endif;

           // Total screen amounts to display possible error in Errcka.
           if scAmt > 0;
              totAmt = totAmt + scAmt;
           endif;
        endif;

        // Employer must have assigned Agent effective for the fund
        // year effective as of the Transaction Date
        //***  Beg Chg  **** 09/11/23 *******************************
        //*** if erARCd = *off;
        if erARCd = *off and erFYR = *off;
        //***  End Chg  **** 09/11/23 *******************************
           if svtrdt = *zero;
              kktrdt = xxdt;
           else;
              kktrdt = svtrdt;
           endif;

           setgt ahky1 pah01;
           readpe ahky  pah01;
           dow not %eof(wmahl) and
              AHstat = 'I';
              readpe ahky pah01;
           enddo;

           if %eof(wmahl);
              // When no active Agent found in wmahl file

              if (scARCd <> *blank or scPodt <> *zero or
               scAmt <> *zero) and erARCd = *off;

                 if svtrdt = *zero;
                    erARCd2 = *on;
                    msARCd2 = 'Agent not effective as of ' +
                              'the transaction date';
                    isError  = *on;
                 else;
                    erARCd = *on;
                    msARCd = 'WCR0637';
                    isError = *on;
                 endif;

              endif;
           endif;
        endif;

       endsr;

     c*================================================================
     c* Read - Read from Wzeal file
     c*================================================================
       Begsr Read;

        exsr setsfl;
        exsr readWzeal;
        // Grid 1 -- A/R Transaction Codes
        i=1;
        //***  Beg Chg  **** 04/18/24 *******************************
        //*** Dow i <= 12;
        Dow i <= 99;
        //***  End Chg  **** 04/18/24 *******************************
           sflChange = *off;
           rrnCashDtl += 1;
           scARCd     = strARCD1(i);
           scFYR      = strFYR1(i);
           scPodt     = DtMDYY(strPodt1(i));
           scAmt      = strAmt1(i);
           scdbrr     = strdbrr1(i);
           selPoYear  = *off;
           if strARCD1(i) <> *blanks or
              strFYR1(i)  <> *zeros  or
              strPodt1(i) <> *zeros  or
              strAmt1(i)  <> *zeros;
              sflChange = *on;
           endif;
           Write sflAutoDss;
           i +=1;
        Enddo;
        rrnCashDtl = 0;

       Endsr;

     c*================================================================
     c* readWzeal - read from wzeal file
     c*================================================================
       begsr readWzeal;

        setll readKy peaz0;
        reade readKy peaz0;
        i=1;
        dow not %Eof(wzeal);
           strARCD1(i) = D0EAarcd;
           strFYR1(i)  = D0EAfyr;
           strPodt1(i) = D0EApodt;
           strAmt1(i)  = D0EAamt;
           strdbrr1(i) = dbrrnz;
           i +=1;
           reade readKy peaz0;
        enddo;
       endsr;

      *================================================================
      * loadYear - read year button
      *================================================================
       Begsr loadYear;
        i = 1;
        ReadC sflAutoDss;
        Dow not %eof;
           if selPoYear = *on;
              selPoYear = *off;

              @@rrn = *blanks;
              if scEmpNoE <> *zeros;
                 @@next = 'PostYear';
                 Chain(n) (##co#:scGroupE:scEmpNoE:scDivE) PEL02;
                 If %found(wdell);
                    @@rrn  = %char(dbwdell1);
                 Endif;
              Endif;

              // Call WtPolARInq for Policy year
              If @@rrn <> *blanks;
                 @@mode = 'D';
                 @@TabInd = *blanks;
                 @@PostDt = *zeros;
                 @@PostAmt = *zeros;
                 WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                            @@PostDt:@@PostAmt);
                 If @@TabInd <> *blanks;
                    scFYR = %int(@@TabInd);
                 Endif;
              Endif;
              i += 1;
           endif;
           //*** Beg Chg *** 03/21/25 ***********************************
           //*** if scARCd <> *blanks or scFYR <> *zeros or
           //*** scPodt <> *zeros or scAmt <> *zeros;
           //***   sflChange = *on;
           //***   update sflAutoDss;
           //*** endif;
           sflChange = *on;
           update sflAutoDss;
           //*** End Chg *** 03/21/25 ***********************************
           ReadC sflAutoDss;
        Enddo;

       Endsr;
      *================================================================
      * setsfl - Error check all subfile records (entire screen)
      *================================================================
       Begsr setsfl;
        // Clear the subfile
        rrnCashDtl = 0;
        scSflADsp = *off;
        scSflAclr2 = *on;
        Write rcdAutoDss;
        scSflADsp = *on;
        scSflAclr2 = *off;
       Endsr;
      *================================================================
      * Errcka - Error check all subfile records (entire screen)
      *================================================================
       Begsr errcka;

        // Total amount should equal zero.  The total of the payment buckets should equal the
        // transfer amount.
        if totAmt <>  EAckam;
           ReadC sflAutoDss;
           Dow not %eof;
              erAmt = *on;
              msAmt = 'WC31506';
              sflChange = *on;
              update sflAutoDss;
              ReadC sflAutoDss;
           Enddo;
           isError = *on;
        endif;

       endsr;

      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
       Begsr keyinz;
        chain(n) earrn pea0p;
        if %found(wdeap);
           kkco#  =  ##co#;
           kkfnd  =   EAfnd;
           kkemp# =   EAemp#;
           kkdiv  =   EAdiv;
           kkfyr  =  *loval;
           kkpodt =  *loval;
           kkarcd =  *loval;
           achFlg =  EAachf;
           svtrdt =  EAtrdt;
        endif;
       endsr;

      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
       Begsr mvdbsc;
        scARCd = DEAarcd;
        scPodt = DtMDYY(DEApodt);
        scFYR  = DEAfyr;
        scAmt  = DEAamt;
       Endsr;

      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
       Begsr mvscdb;
        DEAarcd =  scARCd;
        DEAdesc = GetARDesc(scARCd);
        DEApodt =  DtCYMD(scPodt);
        DEAfyr  =  scFYR;
        DEAamt  =  scAmt;
       Endsr;

      *================================================================
      * PGMEND - End of program processing
      *================================================================
       Begsr pgmend;

        setll 1 peazp;
        read peazp;
        dow not %eof(wzeap);
           EAarcd = DEAarcd;
           EAdesc = DEAdesc;
           if achFlg  = 'L';
              EAtrdt = svtrdt;
           endif;
           EAfyr  = DEAfyr;
           EApodt = DEApodt;
           EAamt  = DEAamt;
           EAcash = 'D';
           EActyp = GetARCat(DEAarcd);
           EAidt  = Dateto7(SystemDate());
           EAiusr = Q1USER;
           EAipgm = Q1PGM;
           EAdt   = Dateto7(SystemDate());
           EAtime = Timeto6(SystemTime());
           EAuser  = Q1USER;
           EApgm  = Q1PGM;
           if DEAfyr <> *zero and DEApodt <> *zero and
            DEAdesc <> *blanks and DEAarcd <> *blanks;
              write pea0p;
              //***  Beg Add  **** 08/02/24 *******************************
              savEAPrrn = dbrrn02;          // Save WDEAP file RRN of new batch detail record
              //***  End Add  **** 08/02/24 *******************************
           endif;
           read peazp;
        enddo;

        chain earrn pea0p;
        delete pea0p;

       endsr;

      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
       begsr pgmstr;
        xxfnd  = EAfnd;
        xxemp# = EAemp#;
        xxdiv  = EAdiv;

        xxdt   = DateTo7(SystemDate);
       endsr;

      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
       begsr updatr;
        chain scdbrr peazp;
        if %found(wzeap);
           //*** Beg Add *** 03/21/25 ***********************************
           if scARCd = *blanks or scFYR = *zeros or
           scPodt = *zeros or scAmt = *zeros;
              delete peazp;
              leavesr;
           endif;
           //*** End Add *** 03/21/25 ***********************************
           exsr mvscdb;
           update peazp;
        endif;
       endsr;

      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
       begsr writer;
        clear peazp;
        exsr mvscdb;
        write peazp;
       endsr;

      *================================================================
      * INZERROR initialize error variables
      *================================================================
       begsr inzError;
        erARCd = *off;
        msARCd = *blanks;
        erARCd2 = *off;
        msARCd2 = *blanks;
        erFYR = *off;
        msFYR = *blanks;
        erPodt = *off;
        msPodt = *blanks;
        erAmt = *off;
        msAmt = *blanks;
        charMsg = *blanks;
       endsr;

      /end-free

     P ARCashDiss      E

       //*** Beg Add *** 04/26/24 *******************************************
       //==============================================================
       // ChkCrossFt  - Check and Load Crossfoot error
       //==============================================================
     p ChkCrossFt      b
     d ChkCrossFt      pi

     d xxPEOF          s              1    Inz
     d isexit          s               n   Inz(*off)
     d strSqlCursor    s           1000
     ddsWDEAP          ds                  inz qualified
     d co#                            3  0
     d bat#                           9  0
     d fnd                            3  0
     d emp#                           9  0
     d chk#                           7  0
     d chkamt                         9  2
     d totPstAmt                      9  2
     d diff                           9  2

      /Free

        Clear WCfl30;
        Chain 'WC' swc01;                                  // Chain SWCPF file
        Clear rcdCrossFt;
        // Clear the subfile
        scCFSflRRN = 0;
        scCFSflDsp = *off;
        scCFSflClr = *on;
        Write rcdCrossFt;
        scCFSflClr = *off;
        scCFSflDsp = *on;
        scErrMsg   = 'The Posted amounts entered do not match to the original +
                      check amount for the following checks:';

        //*** Beg Add *** 09/24/24 *******************************************
        // Including (EAchk# <> 0) in below sql
        //*** End Add *** 09/24/24 *******************************************

        strSqlCursor = 'Select EAco#, EAbat#, EAfnd, EAemp#, EAchk#, +
           EAckam , SUM(EAamt) As TotPostAmt, (EAckam - Sum(EAamt)) +
           As Difference From Wdeap Where EAco# = ' + %editc(##co#:'X') +
           ' and EAbat# = '+ %editc(bat#_sav:'X')+ ' and EAckam <> 9999999.99 +
           and EAchk# <> 0 +
           Group by EAco#, EAbat#, EAfnd, EAemp#, EAchk#,  +
           EAckam Having (EAckam - Sum(EAamt)) <> 0 Order by EAchk#';

        Exec Sql Prepare sqlDB2 From :strSqlCursor;
        Exec Sql Declare Crossfoot Cursor For sqlDB2;
        Exec Sql Open Crossfoot;

        Dow sqlStt = '00000';
           Exec Sql Fetch Next From Crossfoot Into :dsWDEAP;
           If sqlStt = '00000';
              Exsr ChkPEO;
              Exsr LoadCrossFt;
           Endif;
           If sqlStt <> '00000';
              Leave;
           Endif;
        Enddo;

        Exec Sql Close Crossfoot;

        If scCFSflRRN > 0;
           scCFSflRRN = 1;
           Exfmt rcdCrossFt;
        Endif;


        //===============================================================
        // ChkPEO - Check if policy is PEO
        //===============================================================
        Begsr ChkPEO;
         If WCfl30  = 'Y';
            xxPEOF = *blanks;
            Chain (##co#:dsWDEAP.fnd) pfn00;
            If %found(wmfnl);
               If FNpeof = 'M' or FNpeof = 'Y' or FNpeof = 'C';
                  xxPEOF = 'M';
               Endif;
            Endif;
         Endif;
        Endsr;

        //===============================================================
        // LoadCrossFt - Load Crossfoot error grid.
        //===============================================================
        Begsr LoadCrossFt;
         If xxPEOF <> 'M';
            scCFSflRRN += 1;
            scCFSflSel = *off;
            If scCFSflRRN = 1;
               scCFSflSel = *on;
            Endif;
            scCFGrp    = dsWDEAP.fnd;
            scCFEmp    = dsWDEAP.emp#;
            scCFChkNo  = dsWDEAP.chk#;
            scCFChkAmt = dsWDEAP.chkamt;
            scCFPstAmt = dsWDEAP.totPstAmt;
            scCFDifAmt = dsWDEAP.diff;
            write sflCrossFt;
         Endif;
        Endsr;

      /end-free

     P ChkCrossFt      E

       //*** End Add *** 04/26/24 *******************************************
       //==============================================================
       // DissolvePay - Dissolves eligible Payments across A/R balances
       //==============================================================
     p DissolvePay     b
     d DissolvePay     pi

      /Free
        chain fnky pf201;
        if %found(wdf2l);
           if (F2ccyf <> *blank or F2rcyf <> *blank or F2acyf <> *blank
            or F2lcyf <> *blank) and EAcash = 'Y';

              if scRenCashE = 'Y';
                 ##call = 'D';
              else;
                 ##call = 'C';
              endif;

              earrn = dbrrn02;

              if %open(wzeap);
                 close wzeap;
              endif;
              if %open(wzeal);
                 close wzeal;
              endif;

              WA055C(##call:earrn);

              if not %open(wzeap);
                 open wzeap;
              endif;
              if not %open(wzeal);
                 open wzeal;
              endif;

              ARCashDiss(earrn);
           endif;
        endif;

      /End-Free
     p DissolvePay     e

       //==============================================================
       // EmpInf - Get Employer Info and Balance
       //==============================================================
     p EmpInf          b
     d EmpInf          pi

      /Free
        sysdt7 = DateTo7(SystemDate);
        xxbal = *zeros;
        // Get balance from live A/R
        setll eaky8 peaM8;
        reade eaky8 peaM8;
        dow not %eof(wmeal8);
           if M8EApodt <= sysdt7;
              xxbal = xxbal + getValue(M8EAarcd : M8EAamt);
           endif;
           reade eaky8 peaM8;
        enddo;

        // Get balance from A/R Batch (not transferred)
        setll eaky8 pea02;
        reade eaky8 pea02;
        dow not %eof(wdeal2);
            if L2_EApodt <= sysdt7 and L2_EAstat <> 'T';
               xxbal = xxbal + getValue(L2_EAarcd : L2_EAamt);
            endif;
            reade eaky8 pea02;
        enddo;

        @@co# = ##co#;
        @@fnd = scGroupE;
        @@emp# = scEmpNoE;
        @@div = scDivE;
        @@fyr = scYearE;
        @@agt#= *zeros;
        @@anam = *blanks;
        @@plan = '*';
        @@date = *zeros;

        WA002R(@@co#:@@fnd:@@emp#:@@div:@@fyr:@@agt#:@@anam:
               @@prod:@@pnam:@@area:@@phon:@@plan:@@date);
        ckplan = *on;

      /End-Free
     p EmpInf          e

       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       isEmpNbr = '0';
       outMode  = 'E';
       outCall  = '2';
       clear dsSrchCrit;
       clear outSelect;
       dsEmpGrp  = scGroupE;
       outSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(outMode:outCall:outRrn#:outSelect);

       //***  Beg Chg  **** 01/17/24 *******************************
       //*** If outRrn# <> '000000000';
       If outRrn# <> '000000000'
        and outRrn# <> '999999999';
       //***  End Chg  **** 01/17/24 *******************************
          IntEmpRrn = %Dec(outRrn#:9:0);
          Chain  intEmpRrn pel01;
          If %Found(Wdelp);
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
             If %Found(Wmeml);
                scEmpNoE   = EMemp#;
                scEmpNameE = EMname;
                scDivE     = EMdiv;
                scGroupE   = EMfnd;
                isEmpNbr = '1';
                Chain(n) (EMCO#:EMFND:EMEMP#:EMDIV) WZEXL;
                   If %found(WZEXL);
                      scPriGroup = EXofnd;
                      scPriEmp = EXoemp;
                      scPriDiv = EXodiv;
                   Endif;
             EndIf;
          EndIf;
       EndIf;

       //***  Beg Add  **** 01/17/24 *******************************
       // Employer Detail Record does not exist in WDELP file for selected
       // Fund, Employer, division, then show error --
       // 'Policy Years information has not been established for this employer/'
       If outRrn# = '999999999';
          scEmpNoE   = *zeros;
          scEmpNameE = *Blanks;
          scDivE     = *zeros;
          scYearE    = *zeros;
          scGroupE   = *zeros;
          scPriGroup = *zeros;
          scPriEmp   = *zeros;
          scPriDiv   = *zeros;

          PopOneBtn('Policy Years Information has not been established for the-
           selected Employer/Division. Please select a different-
           Employer/Division.':'Employer Search Error');

       EndIf;
       //***  End Add  **** 01/17/24 *******************************

      /End-Free
     p EmployerSrch    e

       //==============================================================
       // FindInv# - Look for invoice number in WMEAP and WDEAP
       //==============================================================
     p FindInv#        b
     d FindInv#        pi

      /Free
        intNinv = 0;
        strArKey = EAarcd;
        Chain (EAco#:strArKey) ptt01;
        If %Found(wtabl);
           // Check WMEAP
           Setll (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt) peaM8;
           Reade (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt) peaM8;

           Dow not %Eof(WMEAL8);
              Chain (M8EAco#:M8EAarcd) ptt01;
              If %found(wtabl) and M8EAnInv > 0;
                 intNinv = M8EAnInv;
                 scInvTypID = M8EAinvtypid;
                 Leave;
              Endif;
              Reade (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt) peaM8;
           Enddo;

           // Check WDEAP
           If intNinv = 0;
              Setll (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt) pea02;
              Reade (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt)
               pea02;
              Dow not %Eof(WDEAL2);
                 Chain (L2_EAco#:L2_EAarcd) ptt01;
                 If %found(wtabl) and L2_EAinv# > 0;
                    intNinv = L2_EAinv#;
                    scInvTypID = L2_EAinvtypid;
                    Leave;
                 Endif;
                 Reade (EAco#:EAfnd:EAfyr:EAemp#:EAdiv:EApodt) pea02;
              Enddo;
           Endif;

        Endif;

      /End-Free
     p FindInv#        e

      *=================================================================
      * getValue - returns positive or negative amount based on A/R Type
      *=================================================================
     p getValue        b
     d getValue        pi                  like(EAamt)
     d  arCode                             like(EAarcd) const
     d  arAmt                              like(EAamt)  const

     d  arType         s                   like(TTtype)
     d  arCtgy         s                   like(TTCTYP)

       kkkey = arCode;
       chain tbky ptt01;
       if %found(wtabl);
          arType = TTtype;
          arCtgy = TTCTYP;
       endif;

       select;
       when arCtgy = 'P';

          select;
          when arType = 'P' or arType = 'T';
             return    arAmt * -1;
          when arType = 'A' or arType = 'B' or
               arType = 'D' or arType = 'N' or
               arType = 'R' or arType = 'V';
             return arAmt;
          other;
             return *zero;
          endsl;

       other;
          return *zero;
       endsl;

     p getValue        e

      //===================================================================
      // Initialize batch Detail Record error indicators
      //===================================================================
     p InzBatDtErr     b
     d InzBatDtErr     pi
      /Free
       erSysTypE = *off;
       msSysTypE = *blanks;
       erGroupE = *off;
       msGroupE = *blanks;
       erGroupE2 = *off;
       erEmpNoE = *off;
       msEmpNoE = *blanks;
       erEmpNoE2 = *off;
       erDivE = *off;
       msDivE = *blanks;
       erDivE2 = *off;
       erDivE3 = *off;
       msDivE3 = *blanks;
       erYearE = *off;
       msYearE = *blanks;
       erTrnDateE = *off;
       msTrnDateE = *blanks;
       erChkNoE = *off;
       msChkNoE = *blanks;
       erChkAmtE = *off;
       msChkAmtE = *blanks;
       erPostAmtE = *off;
       msPostAmtE = *blanks;
       erPostAmt2 = *off;
       erPostDtE = *off;
       msPostDtE = *blanks;
       erPostDtE2 = *off;
       msPostDtE2 = *blanks;
       erInvTypID = *off;
       msInvTypID = *blanks;
       erARCodeE = *off;
       msARCodeE = *blanks;

      /End-Free
     p InzBatDtErr     e

       //===================================================================
       // Load Batch Detail Grid
       //===================================================================
     p LoadARBatch     b
     d LoadARBatch     pi

      /free
        // Clear the subfile
        rrnBtchDtl= 0;
        scSflBtch= *off;
        scSflDsp = *off;
        scSflClr1= *on;
        Write rcdARBtch;
        scSflClr1=*off;
        scSflBtch= *on;
        scSflDsp = *on;
        scRowSel2 = 1;

        inquiry = *off;
        If dspMode;
           inquiry = *on;
        Endif;

        //*** Begin Add ***** 09/24/24 *****************************
        str9999msg2 = 'Batch# ' +  %trim(%editc(bat#_sav:'3')) +
         ' has over 9999 records. ' +
         'Click Filter on the display screen to narrow your results.';

        // Get Batch Details Records Count
        BatDtlCount = 0;
        Exec SQL
         select Count(*) into :BatDtlCount from WDEAP
         where EAco# = :co#_sav and EAstat = :stat_sav and EAbat# = :bat#_sav
         and (EAchk# <> 9999999 or EAckam <> 9999999.99);

        // Do not load this grid, if count over 9999 and no filter selected
        If BatDtlCount > 9999;
           If scFGrp# = *zeros and scTGrp# = *zeros
            and scFPoDt = *zeros and scTPoDt = *zeros;
              PopOneBtn(str9999msg2:'A/R Batch Details':'Continue');
              Return;
           Endif;
       Endif;
       //*** End   Add ***** 09/24/24 *****************************

        // Populate the subfile
        Setll keyWDEAL pea00;
        Reade keyWDEAL pea00;
        Dow not %eof(wdeal);
        If EAchk#<> HiChkNo or EAckam<> HiChkAmt;
           //*** Begin Add ***** 08/01/24 *****************************
           // Validate for filter for AR Batch Details Grid
           meetFltr2 = *on;
           If scFGrp# <> *zeros or scTGrp# <> *zeros
            or scFPoDt <> *zeros or scTPoDt <> *zeros;
              MeetFlt2();
           Endif;

           If meetFltr2 = *on;
           //*** End   Add ***** 08/01/24 *****************************

           If EAtfap ='Y' or EAtffl = 'Y';
              inquiry = *on;
           Endif;

           rrnBtchDtl+= 1;
           scSflSel1 = *off;

           // set the selection on 1st record for first load
           If svrrnPrd = *zeros;
              If rrnBtchDtl = 1;
                 scSflSel1 = *on;
              Endif;
           Else;
              If svrrnPrd = rrnBtchDtl;
                 scSflSel1 = *on;
              Endif;
           Endif;
           scChkNo = EAchk#;
           scChkAmt = EAckam;
           scPostAmt = EAamt;
           scPostDate = dtMDYY(EApodt);
           //*** Begin Add ***** 02/03/25 *****************************
           scARCode = EAarcd;
           //*** End   Add ***** 02/03/25 *****************************
           scARDesc = EAdesc;
           scGroup = EAfnd;
           scEmployer = EAemp#;
           scDivision = EAdiv;
           scYear = EAfyr;
           //*** Begin Add ***** 04/10/25 *****************************
           scEmpName = *Blanks;
           //*** End   Add ***** 04/10/25 *****************************
           if EAemp# <> *zeros;
              Chain(n) (EAco#:scGroup:scEmployer:scDivision) WMEML;
              If %found(WMEML);
                  scEmpName = EMname;
              Endif;
           Endif;

           scrrnBtch = dbrrn2;

             //*** Begin Add ***** 08/01/24 *****************************
             If rrnBtchDtl > 9999;
                PopOneBtn(str9999msg:'A/R Batch Details':'Continue');
                Leave;
             Endif;
             //*** End   Add ***** 08/01/24 *****************************

           Write sflBtchDtl;
           //*** Begin Add ***** 08/01/24 *****************************
           EndIf;
           //*** End   Add ***** 08/01/24 *****************************
        Endif;
        Reade keyWDEAL pea00;

        Enddo;

        // Set visibility indicators for Detail box fields
        visCashDis = *on;
        visBtchHdr = *off;
        If rrnBtchdtl >= 1;
           if cash_sav = 'Y';
              visCashDis = *off;
           endif;

           // Transaction Date for Retro Batches
           if EAachf = 'R';
              visBtchHdr = *on;
           endif;
        Endif;

        rrnBtchdtl = 1;
        If svrrnPrd  <> 0;
           rrnBtchdtl = svrrnPrd;
           scRowSel2 = svrrnPrd;
           svrrnPrd = 0;
        Endif;

      /end-free
     p LoadARBatch     e

       //===================================================================
       // Load Batch Inquiry Grid
       //===================================================================
     p LoadARBatInq    b
     d LoadARBatInq    pi

     d strSqlCursor    s           1000

     ddsWDEAP          ds
     D dsbat#                         9  0
     D dschk#                         7  0
     D dsckam                         9  2
     D dsamt                          9  2
     D dspodt                         7  0
     D dsarcd                         2
     D dsdesc                        30
     D dsctyp                         1
     D dssycd                         2
     D dstfdt                         7  0
     D dstfam                        11  2
     D dsrrn                          9  0

      /free
       // Clear the subfile
       rrnBatInq = 0;
       dspBatInq = *off;
       clrBatInq = *on;
       Write rcdBatInq;
       clrBatInq = *off;
       dspBatInq = *on;

       //*** Begin Add ***** 09/08/23 *****************************
       // Replace wdeal2 file with WDEAP file in SQL CURSOR code
       //*** End   Add ***** 09/08/23 *****************************

       // Build Custom SQL to load AR Batch Inqury Grid
       strSqlCursor = 'Select EAbat#, EAchk#, EAckam, EAamt, EApodt, EAarcd, +
        EAdesc, EActyp, EAsycd, EAtfdt, EAtfam, rrn(WDEAP) +
        from WDEAP where EAco# = ' + %EditC(##co#:'X') +
        ' and EAfnd = ' + %EditC(scGroupI:'X') + ' and EAemp# = ' +
        %EditC(scEmpNoI:'X') + ' and EAdiv = ' + %EditC(scDivI:'X') +
        ' and EAfyr = ' + %EditC(scYearI:'X') + ' order by EAbat#';

        Exec SQL Prepare sqlDB From :strSqlCursor;
        Exec SQL Declare cursorDB Cursor For sqlDB;
        Exec SQL Open cursorDB;
        Dou SQLSTT <> '00000';
           Exec SQL Fetch next From cursorDB into :dsWDEAP;
           Select;
           When SQLSTT <> '00000';
              Iter;
           Endsl;

       // Populate the subfile
          rrnBatInq += 1;
          scBatISel = *off;
 
          If rrnBatInq = 1;
             scBatISel = *on;
          Endif;
 
          scBat#I = dsbat#;
          scChkNoI = dschk#;
          scChkAmtI = dsckam;
          scPostAmtI = dsamt;
          scPostDtI = CvtToDate(dspodt);
          scARCdI = dsarcd;
          If dsdesc <> *blanks;
             scARCdI = dsarcd + ' - ' + %trim(dsdesc);
          Endif;
          scARTypI = dsctyp;
          scSYTypI = dssycd;
          kkkey = dssycd;
          Chain(n) tbky psy01;
          If %found and SYstat <> 'I';
             scSYTypI = %trim(scSYTypI) + ' - '+ %trim(SYdesc);
          Endif;

          //*** Begin Add ***** 09/08/23 *****************************
          scTrfDtI = CvtToDate(0);
          //*** End   Add ***** 09/08/23 *****************************

          If dstfam <> 0;
             scTrfDtI = CvtToDate(dstfdt);
          Endif;

          scBatIRRN = dsrrn;

          Write sflBatInq;
       Enddo;

       Exec SQL Close cursorDB;

       If rrnBatInq = 0;
          PopOneBtn('No Records Found!':'A/R Batch List':'Ok');
       Endif;
       rrnBatInq = 1;

      /end-free
     p LoadARBatInq    e

       //===================================================================
       // Load A/R Batch Master Grid
       //===================================================================
     p LoadARBGrid     b
     d LoadARBGrid     pi
     d inIncludeBat                  10    const
     d svbat#          s                   like(EAbat#)
     d xxtotamt        s                   like(EAtfam)
     d xxtotamtp       s                   like(EAtfam)
      *** Begin Add ***** 07/31/24 *****************************
     d strSqlCursor    s           1000
     DdsBatKey         ds                  qualified inz
     D co#                            3  0
     D stat                           1
     D bat#                           9  0
      *** End   Add ***** 07/31/24 *****************************

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdARBtch;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;
       svbat#   = 0;

       //*** Begin Add ***** 09/24/24 *****************************
       inquiry_1 = *on;
       //*** End   Add ***** 09/24/24 *****************************

       // Populate the subfile
       //*** Begin Chg ***** 07/31/24 *****************************
       //*** Setll *loval wdeal9;
       //*** Read wdeal9;
       //*** Dow not %eof(wdeal9);

       //*** Begin Chg ***** 02/21/25 *****************************
       //*** strSqlCursor = 'Select DISTINCT eaco#,eastat,eabat# +
       //***  from WDEAP order by eabat# desc';

       // To speed up loading of A/R master grid, build strSqlCursor on the Filter values
       strSqlCursor = 'Select DISTINCT eaco#,eastat,eabat# from WDEAP +
        Where eastat <> ''T''';

       If dsFilter.scBstsF = 'T';
          strSqlCursor = 'Select DISTINCT eaco#,eastat,eabat# from WDEAP +
           Where eastat = ''T''';
       Endif;

       If dsFilter.scBstsF = 'B';
          strSqlCursor = 'Select DISTINCT eaco#,eastat,eabat# from WDEAP +
           Where eastat in ('' '' , ''T'')';
       Endif;

       If dsFilter.scTDCrt <> 0;
          strSqlCursor = %trim(strSqlCursor) +
           ' and eaidt >= ' + %editc(dtCYMD(dsFilter.scFDCrt):'X') +
           ' and eaidt <= ' + %editc(dtCYMD(dsFilter.scTDCrt):'X');
       Endif;

       If dsFilter.scTBat# <> 0;
          strSqlCursor = %trim(strSqlCursor) +
           ' and eabat# >= ' + %editc(dsFilter.scFBat#:'X') +
           ' and eabat# <= ' + %editc(dsFilter.scTBat#:'X');
       Endif;

       strSqlCursor = %trim(strSqlCursor) + ' order by eabat# desc';
       //*** End   Chg ***** 02/21/25 *****************************

       Exec Sql Prepare sqlDB3 From :strSqlCursor;
       Exec Sql Declare curBatKey Cursor For sqlDB3;
       Exec sql open curBatKey;

       Dow sqlStt = '00000';
          Exec sql fetch next from curBatKey into :dsBatKey;
          If sqlStt <> '00000';
             Iter;
          Endif;
          If sqlStt = '00000';
             Chain (dsBatKey.co#:dsBatKey.stat:dsBatKey.bat#) pea09;
             If %found(wdeal9);

       //*** End   Chg ***** 07/31/24 *****************************
          If L9_EAbat# <> svbat#;
             //*** Begin Add ***** 07/26/24 *****************************
             // Validate for filter for AR Batch Master Grid
             //*** Begin Del ***** 02/21/25 *****************************
             //*** meetFltr = *on;
             //*** If scBstsF <> *blanks or scFDCrt <> *zeros or scTDCrt <> *zeros
             //***  or scFBat# <> *zeros or scTBat# <> *zeros;
             //***    MeetFlt();
             //*** Endif;

             //*** If meetFltr = *on;
             //*** End   Del ***** 02/21/25 *****************************
             //*** End   Add ***** 07/26/24 *****************************
             svbat# = L9_EAbat#;

             scSflRRN += 1;
             scSflSel = *off;
 
             If svsflrrn = 0;
                If scSflRRN = 1;
                   scSflSel = *on;
                Endif;
             Else;
                If scSflRRN = svsflrrn;
                   scSflSel = *on;
                Endif;
             Endif;
 
             //*** Begin Add ***** 09/24/24 *****************************
             If L9_EAtfap = 'Y' or L9_EAtffl = 'Y';
                inquiry_1 = *off;
             Endif;
             If L9_EAtfap <> 'Y' and L9_EAtffl <> 'Y' and not dspMode;
                inquiry_1 = *on;
             Endif;
             //*** End   Add ***** 09/24/24 *****************************

             scBat# = L9_EAbat#;
             scBatA = L9_EAbata;
             scAmtEnt = 0;
             scAmtP = 0;
             xxtotamt = 0;
             xxtotamtp = 0;
             scTfdt = CvtToDate(L9_EAtfdt);
             //*** Begin Chg ***** 07/26/24 *****************************
             //*** If L9_EAtfdt = 0;
             //***    scTfdt = CvtToDate(L9_EAidt);
             //*** Endif;
             // Date Created
             scCrdt = CvtToDate(L9_EAidt);
             //*** End   Chg ***** 07/26/24 *****************************
             scTfap = L9_EAtfap;
             scTfap2 = L9_EAtfap;
             If scTfap2 = ' ';
                scTfap2 = 'N';
             Endif;
             scTffl = L9_EAtffl;
             scTffl2 = L9_EAtffl;
             If scTffl2 = ' ';
                scTffl2 = 'N';
             Endif;
             scOnli = L9_EAonli;
             scOnli2 = L9_EAonli;
             If scOnli2 = ' ';
                scOnli2 = 'N';
             Endif;
             scStat = *Blanks;
             If L9_EAstat = 'T';
                scStat = 'Transferred';
             //*** Begin Add ***** 07/26/24 *****************************
             Else;
                scStat = 'Pending';
             //*** End   Add ***** 07/26/24 *****************************
             Endif;

             //*** Begin Chg ***** 02/21/25 *****************************
             // Get Total of Amount Entered
             //*** Exec SQL
             //***  select sum(EAamt) into :xxtotamt
             //***   from WDEAP
             //***   where EAbat# = :scBat# and EAckam <> 9999999.99;

             //*** scAmtEnt = xxtotamt;

             // Get Total of Amount to be posted

             //*** Beg Add *** 09/24/24 *******************************************
             // Reduce Amount to be poasted for tranferred batches, if user Refresh Grid
             // Including (EAstat = :L9_EAstat) in below sql
             //*** End Add *** 09/24/24 *******************************************

             //*** Exec SQL
             //***  select sum(EAamt) into :xxtotamtp
             //***  from WDEAP, WTSYP
             //***  where EAbat# = :scBat# and EAckam <> 9999999.99
             //***  and SYkey = EAsycd and SYstat <> 'I' and SYtype = 'Y';

             //*** scAmtP   = xxtotamtp;


             Setll (L9_EAco#:L9_EAbat#) pea04;
             Reade (L9_EAco#:L9_EAbat#) pea04;
             Dow not %eof(wdeal4);
                If L4_EAckam <> 9999999.99;
                   xxtotamt += L4_EAamt;

                   Chain (L4_EAco#:L4_EAsycd) psy01;
                   If %found and SYstat <> 'I' and SYtype = 'Y';
                      xxtotamtp += L4_EAamt;
                   Endif;
                Endif;
                Reade (L9_EAco#:L9_EAbat#) pea04;
             Enddo;

             scAmtEnt = xxtotamt;
             scAmtP   = xxtotamtp;
             //*** End   Chg ***** 02/21/25 *****************************

             scBtyp = *Blanks;
             Select;
             When L9_EAachf = 'A';
                scBtyp = 'EFT';
             When L9_EAachf = 'F';
                scBtyp = 'FEE';
             When L9_EAachf = 'S';
                scBtyp = 'SR ';
             When L9_EAachf = 'L';
                scBtyp = 'LB ';
             When L9_EAachf = 'D';
             scBtyp = 'DED';
             When L9_EAachf = 'R';
                scBtyp = 'RDP';
             When L9_EAachf = 'T';
                scBtyp = '3PY';
             When L9_EAachf = 'U';
                scBtyp = '3BL';
             When (L9_EAachf = 'B' or L9_EAachf = ' ') and L9_EAonli = 'Y';
                scBtyp = 'BIL';
             Endsl;


             // Select Checkbox
             If inIncludeBat = 'Include';
                scSelect = 'Y';
                scnxtchg = *on;
             Else;
                scSelect = 'N';
                scnxtchg = *off;
             Endif;


             // Detail Box Hidden fields
             scCash = 'N';
             //*** Begin Chg ***** 10/23/23 *****************************
             // If L9_EAcash = 'Y';
             If L9_EAcash = 'Y' or L9_EAcash = 'D';
             //*** End   Chg ***** 10/23/23 *****************************
                scCash = 'Y';
             Endif;
             scTrdt = CvtToDate(L9_EAtrdt);

             // Batch Created
             scCDate = CvtToDate(L9_EAidt);
             scCTime = L9_EAtime;
             scCUser = L9_EAuser;

             // Batch Transmitted
             scTDate = CvtToDate(L9_EAachd);
             scTTime = L9_EAacht;
             scTUser = L9_EAachu;


             scRRN  = dbrrn;

             //*** Begin Add ***** 07/26/24 *****************************
             If scSflRRN > 9999;
                PopOneBtn(str9999msg:'A/R Batches':'Continue');
                Leave;
             Endif;
             //*** End   Add ***** 07/26/24 *****************************

             Write sflARBtch;
             // Save 1st record information to load in A/R Transaction details Grid
             If bat#_sav = 0;
                co#_sav  = L9_EAco#;
                stat_sav = L9_EAstat;
                bat#_sav = L9_EAbat#;
                cash_sav = L9_EAcash;
                ARDtlRrn = scRRN;
                //*** Begin Add ***** 06/24/24 *****************************
                btyp_sav = L9_EAachf;           // Save Batch Type
                //*** End   Add ***** 06/24/24 *****************************
             Endif;
          //*** Begin Add ***** 07/26/24 *****************************
          //*** Begin Del ***** 02/21/25 *****************************
          //*** Endif;                                // If meetFltr = *on
          //*** End   Del ***** 02/21/25 *****************************
          //*** End   Add ***** 07/26/24 *****************************
          Endif;
       //*** Begin Chg ***** 07/31/24 *****************************
       //***    Read wdeal9;
       //*** Enddo;
       Endif;

          Endif;
          // Clear SQL error due to execution of 2 SQL statements in this loop
          If sqlStt <> '00000';
             sqlStt = '00000';
          Endif;
       Enddo;
       //*** Beg Add ***** 03/19/25 *******************************
       scBat# = bat#_sav;
       //*** End Add ***** 03/19/25 *******************************
       Exec sql close curBatKey;
       //*** End   Chg ***** 07/31/24 *****************************

       Clear inInclude;
       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          //*** Beg Del ***** 03/19/25 ****************************
          //***svSflRRN = 0;
          //*** End Del ***** 03/19/25 ****************************
       Endif;

      /end-free
     p LoadARBGrid     e

       //===================================================================
       // Load A/R Batch Purge History grid
       //===================================================================
     p LoadPurHist     b
     d LoadPurHist     pi
     d inIncludeBatP                 10    const

      /free
       // Clear the subfile
       rrnPurHist = 0;
       dspPurHist = *off;
       clrPurHist = *on;
       Write rcdPurBat;
       clrPurHist = *off;
       dspPurHist = *on;

       // Populate the subfile
       Setll *loval pbp00;
       Read pbp00;
       Dow not %eof(wmbpl);
          rrnPurHist += 1;
          scPurHSel = *off;
 
          If rrnPurHist = 1;
             scPurHSel = *on;
          Endif;
 
          scPgDtH = CvtToDate(BPpgdt);
          scCmDtH = CvtToDate(BPcmdt);
          scNameH = GetUserName(BPuser);

          // Set visibilty of Select Box
          visSelectP = *off;
          If BPcmdt = 0;
             visSelectP = *on;
          Endif;

          // Select Checkbox
          If inIncludeBatP = 'Include';
             scSelectP = 'Y';
             scnxtchgP = *on;
          Else;
             scSelectP = 'N';
             scnxtchgP = *off;
          Endif;

          scPurHRRN = dbrrn3;

          Write sflPurHist;
          Read pbp00;
       Enddo;

       Clear inIncludeP;
       rrnPurHist = 1;

      /end-free
     p LoadPurHist     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
       //*** Begin Add ***** 07/26/24 *****************************
     d isexit          s               n   Inz(*off)
       //*** End   Add ***** 07/26/24 *****************************
       //*** Begin Add ***** 08/01/24 *****************************
     d isexit2         s               n   Inz(*off)
       //*** End   Add ***** 08/01/24 *****************************
      /free

       LoadActionBar();
       BtnCancel = *off;
       reload = *on;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load A/R Batch Master Grid
          If reload = *on;
             LoadARBGrid(inInclude);
             reload = *off;
          endif;

          // Load A/R Batch Detail Grid
          LoadARBatch();

          // display screen
          Exsr ExfmtScreens;
          //***  Beg Add  **** 08/02/24 *******************************
          scFocus = '0';
          //***  End Add  **** 08/02/24 *******************************

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             inInclude = ' ';
             bat#_sav = 0;
             //***  Beg Add  **** 01/17/24 *******************************
             svsflrrn = 0;
             //***  End Add  **** 01/17/24 *******************************
             reload = *on;

          // Catch Row click event
          When RowClick = *on;
             RowClick = *off;
             ProcRoWClk();           // Process the Row Click in AR master Grid
             reload = *off;

          // Include All
          When btnBatIncl = *on;
             btnBatIncl = *off;
             inInclude = 'Include';
             //*** Begin Add ***** 09/04/23 *****************************
             reload = *on;
             //*** End   Add ***** 09/04/23 *****************************

          // Exclude All
          When btnBatExcl = *on;
             btnBatExcl = *off;
             inInclude = ' ';
             //*** Begin Add ***** 09/04/23 *****************************
             reload = *on;
             //*** End   Add ***** 09/04/23 *****************************

          // Process More Action items from A/R Batch Toolbar
          When btnShowWin = *on;
             btnShowWin = *off;
             // Check Security
             If ((txtShowWin = 'Edit Open Batches' and
              CheckSecLevel('ARBat_EBat':UPtype:UPlvl))
              OR (txtShowWin = 'Transfer Batches' and
              CheckSecLevel('ARBat_TBat':UPtype:UPlvl))
              OR (txtShowWin = 'Purge' and
              CheckSecLevel('ARBat_Purg':UPtype:UPlvl))
              OR (txtShowWin = 'List by Employer' and
              CheckSecLevel('ARBat_List':UPtype:UPlvl)));
                ProcMorActTB();
             Else;
                PopOneBtn('User ' + q1user + ' not Authorized to Process ' +
                 txtShowWin + ' Action.':'A/R Batch Toolbar':'Ok');
             Endif;

          // Perform Action
          When btnPerAct = *on;
             btnPerAct = *off;
             ProcPerAct();
             //*** Begin Add ***** 09/13/23 *****************************
             If reload = *on;
                ProcRoWClk();           // Process the Row Click in AR master Grid
             Endif;
             //*** End   Add ***** 09/13/23 *****************************

          // Add A/R Batch Master
          When BtnARBAdd = *On;
             BtnARBAdd = *Off;
             ProcessAdd();

          // Add AR Batch detail information
          When BtnBtchAdd = *on;
             @@mode1 = 'A';
             BtnBtchAdd = *off;
             //*** Beg Add ***** 02/20/25 *****************************************
             Chain(n) scRrnBtch wdeap;
             If not ValidateAct2(@@mode1);
                Iter;
             Endif;
             //*** End Add ***** 02/20/25 *****************************************
             //*** Begin Add ***** 06/24/24 *****************************
             If btyp_sav <> 'D';           // Save Batch Type
             //*** End   Add ***** 06/24/24 *****************************
                //*** Beg Chg ***** 12/16/24 ****************************
                //***ProcessAddBtch();
                If not (CheckLock(intCo#:infndVar:scBat#:'ARB_DTLADD'));
                   ProcessAddBtch();
                   ClearPolicyLock(intCo#:infndVar:scBat#);
                Endif;
                //*** End Chg ***** 12/16/24 ****************************
             //*** Begin Add ***** 06/24/24 *****************************
             Endif;
            // Not allowed to Add a deductible type of Batch
             If btyp_sav = 'D';            // Save Batch Type
                strErrMsg = *Blanks;
                strErrMsg = 'Deductible batch records cannot be manually +
                 modified, use Deductible Invoice Process';
                PopOneBtn(strErrMsg:'Error':'Ok');
             Endif;
             //*** End   Add ***** 06/24/24 *****************************

          //*** Begin Add ***** 07/26/24 *****************************
          // Filter Processing for AR Batch Master Grid
          When BtnFilter = *on;
             BtnFilter = *off;
             isExit = *off;
             Dow isExit = *off;
                // Set Current Date and Current Date less 1 year date
                scDate2 = DateTo8(SystemDate());
                scDate1 = DateTo8(CvtToDate(scDate2) - %years(1));
                ExFmt rcdARBFltr;
                Select;
                When BtnFltrApp = *on;
                   BtnFltrApp = *off;
                   If ValidateFlt();
                      dsFilter.scBstsF = scBstsF;
                      dsFilter.scFDCrt = scFDCrt;
                      dsFilter.scTDCrt = scTDCrt;
                      dsFilter.scFBat# = scFBat#;
                      dsFilter.scTBat# = scTBat#;
                      isExit = *on;
                      dspClrFltr = *on;
                      reload = *on;      // re-Load A/R Batch Master Grid
                      //***  Beg Add  **** 09/24/24 *******************************
                      bat#_sav = 0;      // re-Load A/R Batch Details Grid
                      svsflrrn = 0;
                      //***  End Add  **** 09/24/24 *******************************
                   Endif;

                When BtnFltrCan = *on;
                   BtnFltrCan = *off;
                   scBstsF = dsFilter.scBstsF;
                   scFDCrt = dsFilter.scFDCrt;
                   scTDCrt = dsFilter.scTDCrt;
                   scFBat# = dsFilter.scFBat#;
                   scTBat# = dsFilter.scTBat#;
                   isExit = *on;

                   msFDCrt = *blanks;
                   erFDCrt = *off;
                   msTDCrt = *blanks;
                   erTDCrt = *off;
                   msFBat# = *blanks;
                   erFBat# = *off;
                   msTBat# = *blanks;
                   erTBat# = *off;
                Endsl;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             BtnClrFltr = *off;
             Clear rcdARBFltr;
             scBstsF = 'P';                    // Initialize Filter with Pending Status
             dspClrFltr = *off;
             Clear dsFilter;
             dsFilter.scBstsF = 'P';           // Initialize Filter with Pending Status
             reload = *on;      // re-Load A/R Batch Master Grid
             //***  Beg Add  **** 09/24/24 *******************************
             bat#_sav = 0;      // re-Load A/R Batch Details Grid
             svsflrrn = 0;
             //***  End Add  **** 09/24/24 *******************************
          //*** End   Add ***** 07/26/24 *****************************

          //*** Begin Add ***** 08/01/24 *****************************
          // Filter Processing for AR Batch Details Grid
          When BtnFilter2 = *on;
             BtnFilter2 = *off;
             isExit2 = *off;
             Dow isExit2 = *off;
                ExFmt rcdARFltr2;
                Select;
                When BtnFltApp2 = *on;
                   BtnFltApp2 = *off;
                   If ValidateFlt2();
                      dsFilter2.scFGrp# = scFGrp#;
                      dsFilter2.scTGrp# = scTGrp#;
                      dsFilter2.scFPoDt = scFPoDt;
                      dsFilter2.scTPoDt = scTPoDt;
                      isExit2 = *on;
                      dspClrFlt2 = *on;
                   Endif;

                When BtnFltCan2 = *on;
                   BtnFltCan2 = *off;
                   scFGrp# = dsFilter2.scFGrp#;
                   scTGrp# = dsFilter2.scTGrp#;
                   scFPoDt = dsFilter2.scFPoDt;
                   scTPoDt = dsFilter2.scTPoDt;
                   isExit2 = *on;

                   msFGrp# = *blanks;
                   erFGrp# = *off;
                   msTGrp# = *blanks;
                   erTGrp# = *off;
                   msFPoDt = *blanks;
                   erFPoDt = *off;
                   msTPoDt = *blanks;
                   erTPoDt = *off;
                Endsl;
             Enddo;

          // Filter clear processing
          When btnClrFlt2 = *on;
             btnClrFlt2 = *off;
             Clear rcdARFltr2;
             dspClrFlt2 = *off;
             Clear dsFilter2;
          //*** End   Add ***** 08/01/24 *****************************

          Other;
             ProcOther();             // Process other Options for AR batch Master
             @@mode1 = 'C';
             ProcOtherBD();           // Process other Options for AR batch detail

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       // Initialize Screen fields
       scSelAct = '--- Select List Action ---';

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 740;
          APnlHeight = pnlHeight + 60;

          Write rcdComPnl;
          Write rcdPanel;
          Exfmt rcdARBtch;
          Read rcdComPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

      *** Begin Add ***** 07/26/24 *****************************
       //==================================================================
       // MeetFlt -  Determine if the records meet the Filter criteria for AR Batch Master Grid
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
      /free

       // Check for Batch transferred Status
       If scBstsF = 'P' and L9_EAstat <> *Blanks;
          meetFltr = *off;
       Endif;
       If scBstsF = 'T' and L9_EAstat <> 'T';
          meetFltr = *off;
       Endif;

       // Check for Date Created Range
       If scFDCrt <> *zeros and L9_EAidt < dtCYMD(scFDCrt);
          meetFltr = *off;
       Endif;
       If scTDCrt <> *zeros and L9_EAidt > dtCYMD(scTDCrt);
          meetFltr = *off;
       Endif;

       // Check for Batch# Range
       If scFBat# <> *zeros and L9_EAbat# < scFBat#;
          meetFltr = *off;
       Endif;
       If scTBat# <> *zeros and L9_EAbat# > scTBat#;
          meetFltr = *off;
       Endif;

      /end-free
     p MeetFlt         e
      *** End   Add ***** 07/26/24 *****************************

      *** Begin Add ***** 08/01/24 *****************************
       //==================================================================
       // MeetFlt2 -  Determine if the records meet the Filter criteria for AR Batch Details Grid
       //==================================================================
     p MeetFlt2        b
     d MeetFlt2        pi
      /free

       // Check for Group# Range
       If scFGrp# <> *zeros and EAfnd < scFGrp#;
          meetFltr2 = *off;
       Endif;
       If scTGrp# <> *zeros and EAfnd > scTGrp#;
          meetFltr2 = *off;
       Endif;

       // Check for Posting Date Range
       If scFPoDt <> *zeros and EApodt < dtCYMD(scFPoDt);
          meetFltr2 = *off;
       Endif;
       If scTPoDt <> *zeros and EApodt > dtCYMD(scTPoDt);
          meetFltr2 = *off;
       Endif;

      /end-free
     p MeetFlt2        e
      *** End   Add ***** 08/01/24 *****************************

       //===================================================================
       // PgmStr - Process records
       //===================================================================
     p PgmStr          b
     d PgmStr          pi

      /Free
       svsycd = *blanks;
       svfnd  = *zeros;
       //*** Begin Add ***** 04/22/24 *****************************
       svcash = *blanks;
       svcashDissolve = *off;
       visCashDis = *on;
       //*** End   Add ***** 04/22/24 *****************************

       // Refresh renewal cash flag in case this program instance is
       // not the original instance to exist in the QILE activation
       // group.

       scRenCashE = *blanks;
       @@bhdr  = ARDtlRrn;

       chain(n) @@bhdr WDEAP;
       //*** Begin Add ***** 04/22/24 *****************************
       if not %found(wdeap);
          chain(n) scRrnBtch wdeap;
       endif;
       //*** End   Add ***** 04/22/24 *****************************
       if %found(wdeap);
          svcash = EAcash;
          ##co#  = EAco#;
          scCO#1 = EAco#;

          // EAcash will equal 'D' if the batch header record has been dissolved.
          if @@mode1 = 'A' and svcash = 'D';
             svcash = 'Y';
          endif;

          //*** Begin Del ***** 04/22/24 *****************************
          //*** if @@mode1 <> 'A';
          //***    chain(n) scRrnBtch wdeap;
          //***    svcash = EAcash;
          //*** endif;
          //*** End   Del ***** 04/22/24 *****************************

          svcashDissolve = *off;
          visCashDis = *on;
          if svCash = 'Y';
             svcashDissolve = *on;
             visCashDis = *off;
          endif;
          cashDisolve = svcashDissolve;

          scTrDtsav = 0;
          If EAtrdt > *Zero;
             scTrDtsav = DtMDYY(EAtrdt);
          Endif;

          svtrdt = EAtrdt;
          svachf = EAachf;
          svbata = EAbata;
          svtfap = EAtfap;
          svtffl = EAtffl;
          svstat = EAstat;
          scbat# = EAbat#;
          scbata = EAbata;
          kkbat# = EAbat#;
          svidt = EAidt;
          svipgm = EAipgm;
          sviusr = EAiusr;

          // save value for AJAX program
          scsvcash = svcash;
          scsvtrdt = EAtrdt;
          scsvachf = EAachf;

          svcash1 = svcash;
          svtrdt1 = EAtrdt;
          svachf1 = EAachf;
       endif;

       dspMode = @@mode1= 'D';

       isInvoiceCloud = isModEnabled('Invoice Cloud');
       // isICgroup = IcGroupOpt(EAco#:EAfnd);

       // Set Visibility of Invoice Type ID
       isInvType = IsModEnabled('S/R Online') or
                   IsModEnabled('Third Party Payment');

      /End-Free
     p PgmStr          e

       //===================================================================
       // ProcessAdd - Process Addition of New A/R Batch
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  xxbat#         s                   like(EAbat#) INZ
      /free

       Clear RcdBatAdd;
       mode = 'A';
       isexit = *off;
       BtnBatCan = *off;

       Dow BtnBatCan = *off and isexit = *off;

          scBatHdg = 'Add A/R Batch';
          Exfmt RcdBatAdd;
          Select;
          // Save Button
          When BtnSaveBat = *on;
             BtnSaveBat = *off;
             If ValidateBat(mode);
                Exsr WrtBat;
                isexit = *on;
                reload = *on;
                bat#_sav = 0;             // Reload the master grid from top
                svsflrrn = 0;             // Reload the master grid from top
             EndIf;

          // Cacel Button
          When BtnBatCan = *on;
               BtnBatCan = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        // Get latest Batch Number
        Exec SQL
         Select max(EAbat#) into :xxbat# from WDEAP
          where EAco# = :intCo#;

        EAco#   = intCo#;                // Company Number
        EAbat#  = xxbat# + 1;            // Batch Number
        EAbata  = scBatAad;              // Batch Amount
        EAachf  = scBtypad;              // Batch Type
        EAtrdt  = dtCYMD(scTrdtad);      // Transaction Date
        EAcash  = scCashad;              // Cash Dissolve Batch
        EAchk#  = *all'9';               // Check Number
        EAckam  = *all'9';               // Check Amount
        EAtfap  = 'N';                   // Transfer Approved Flag
        EAtffl  = 'N';                   // Transfer Flag

        EAuser  = q1user;                // User
        EApgm   = q1pgm;                 // Program
        EAtime  = Timeto6(SystemTime()); // Time
        EAdt    = Dateto7(SystemDate()); // Date Mintained
        EAiusr  = q1user;                // Initial User
        EAipgm  = q1pgm;                 // Initial Program
        EAidt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new A/R Batch subroutine
       BegSr WrtBat;
        Clear pea0p;
        Exsr MoveScDb;
        Write pea0p;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcessAddBtch -Add Batch
       //===================================================================
     P ProcessAddBtch  B
     D ProcessAddBtch  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdBatDtl;
       PgmStr();
       mode = 'A';
       isexit = *off;
       BtnBtchCan = *off;
       visBtcSvAd = *on;
       //***  Beg Add  **** 04/08/25 *******************************
       scGrpFocus = '1';
       scEmpFocus = '0';
       scAmtFocus = '0';
       //***  End Add  **** 04/08/25 *******************************

       Dow BtnBtchCan = *off and isexit = *off;
          //***  Beg Add  **** 08/02/24 *******************************
          scFocus = '1';
          //***  End Add  **** 08/02/24 *******************************

          // Loading initial values

          scBtchHdg = 'Add Batch Information';
          // save company number of header batch record to pass Javascript program
          scCO#1 = EAco#;
          scsvCash = svCash1;
          scsvtrdt = svtrdt1;
          scsvachf = svachf1;

          visPostSch = *on;
          if svcashDissolve = *on;
             visPostSch = *off;
          Endif;
          visBtcSvAd = *on;

          if WtPolARflg = *on or isError1 = *on;
             Exsr SaveAR;
          elseif selEmpSrch <> *on;
             Exsr LoadInit;
          endif;
          selEmpSrch = *off;
          Exfmt rcdBatDtl;

          //*** Beg Add ***** 01/31/25 ***********************************
          If scChkAmtE < 0;
             xxChkInd = *On;
          Else;
             xxChkInd = *Off;
          Endif;
          //*** End Add ***** 01/31/25 ***********************************
          Select;
          When selEmpSrch = *on;
             InzBatDtErr();
             EmployerSrch();
          When selPoAmtDt = *on;
             //***  Beg Add  **** 09/11/23 *******************************
             InzBatDtErr();
             //***  End Add  **** 09/11/23 *******************************
             @@rrn = *blanks;
             If scEmpNoE <> *zeros and scYearE <> *zeros;
                @@next = 'PostAmtDt';
                Chain(n) (##co#:scGroupE:scEmpNoE:scDivE:scYearE) PEL02;
                If %found(wdell);
                   @@rrn  = %char(dbwdell1);
                Endif;
             Endif;
             If scEmpNoE <> *zeros and scYearE = *zeros;
                @@next = 'PostYear';
                Chain(n) (##co#:scGroupE:scEmpNoE:scDivE) PEL02;
                If %found(wdell);
                   @@rrn  = %char(dbwdell1);
                Endif;
             Endif;

             @@mode = 'D';
             @@TabInd = *blanks;
             @@PostDt = *zeros;
             @@PostAmt = *zeros;

             // Call WtPolARInq for Post Amt and Date
             If @@rrn <> *blanks;
                WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                           @@PostDt:@@PostAmt);
                If @@PostDt <> *zeros and @@PostAmt <> *zeros;
                   scPostDtE = @@PostDt;
                   scPostAmtE = @@PostAmt;
                Endif;
                If @@TabInd <> *blanks and @@next = 'PostYear';
                   scYearE = %int(@@TabInd);
                   @@next = 'PostAmtDt';
                   @@rrn = *blanks;
                   Chain(n) (##co#:scGroupE:scEmpNoE:
                             scDivE:scYearE) PEL02;
                   If %found(wdell);
                      @@rrn = %char(dbwdell1);
                   Endif;
                   If @@rrn <> *blanks;
                      WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                                 @@PostDt:@@PostAmt);
                   endif;
                   If @@PostDt<> *zeros and @@PostAmt <> *zeros;
                      scPostDtE  = @@PostDt;
                      scPostAmtE = @@PostAmt;
                   Endif;
                Endif;
             Endif;
             WtPolARflg = *on;

          // Save/Add Next Button
          When BtnBtcSvAd = *on;
             BtnBtcSvAd = *off;
             If ValidateBatDt(mode);
                Exsr Writer;
                //***  Beg Add  **** 10/09/23 *******************************
                reload = *on;
                //***  End Add  **** 10/09/23 *******************************

                // Initialize Screen fields for re-load
                if sameck <> *on;
                   //*** Beg Add ***** 08/02/24 *****************************
                   svfnd2 = scGroupE;
                   //*** End Add ***** 08/02/24 *****************************
                   Clear rcdBatDtl;
                   //*** Beg Add ***** 09/11/23 *****************************
                   PgmStr();
                   //*** End Add ***** 09/11/23 *****************************
                   // set visibility of Transaction Date for Retro Batches
                   if EAachf = 'R';
                      visBtchHdr = *on;
                   endif;
                   //*** Beg Chg ***** 08/02/24 *****************************
                   //*** scGroupE = EAfnd;
                   svfnd = svfnd2;
                   scGroupE = svfnd2;
                   //*** End Chg ***** 08/02/24 *****************************
                   scSysTypE = EAsycd;
                   //***  Beg Add  **** 04/08/25 *******************************
                   scGrpFocus = '0';
                   scEmpFocus = '1';
                   scAmtFocus = '0';
                   //***  End Add  **** 04/08/25 *******************************
                endif;
                //***  Beg Add  **** 04/08/25 *******************************
                if sameck = *on;
                   scGrpFocus = '0';
                   scEmpFocus = '0';
                   scAmtFocus = '1';
                EndIf;
                //***  End Add  **** 04/08/25 *******************************
                //*** Beg Add ***** 04/26/24 *****************************
                // Check crossfoot error if check amount and post amount not matches
                //*** Beg Chg ***** 01/31/25 *****************************
                //***If svAmt <= 0;
                If (xxChkInd = *Off and svAmt <= 0) or
                 (xxChkInd = *On and svAmt >= 0);
                //*** End Chg ***** 01/31/25 *****************************
                   ChkCrossFt();
                Endif;
                //*** End Add ***** 04/26/24 *****************************
                BtnBtchCan = *off;
             else;
                isError1 = *on;
             endif;

          // Save Button
          When BtnBtchSav = *on;
             BtnBtchSav = *off;
             If ValidateBatDt(mode);
                Exsr Writer;
                //***  Beg Add  **** 10/09/23 *******************************
                reload = *on;
                //***  End Add  **** 10/09/23 *******************************
                //***  Beg Add  **** 04/08/25 *******************************
                scGrpFocus = '0';
                scEmpFocus = '0';
                scAmtFocus = '1';
                //***  End Add  **** 04/08/25 *******************************
                if sameck <> *on;
                   isexit = *on;
                EndIf;
                //*** Beg Add ***** 04/26/24 *****************************
                // Check crossfoot error if check amount and post amount not matches
                //*** Beg Chg ***** 01/31/25 *****************************
                //***If svAmt <= 0;
                If (xxChkInd = *Off and svAmt <= 0) or
                 (xxChkInd = *On and svAmt >= 0);
                //*** End Chg ***** 01/31/25 *****************************
                   ChkCrossFt();
                Endif;
                //*** End Add ***** 04/26/24 *****************************
             Else;
                isError1 = *on;
             EndIf;

          // Cacel Button
          When BtnBtchCan = *on;
             BtnBtchCan = *off;
             isexit = *on;
             //*** Beg Add ***** 04/26/24 *****************************
             // Check crossfoot error if check amount and post amount not matches
             //*** Beg Chg ***** 01/31/25 *****************************
             //***If svAmt > 0;
             If (xxChkInd = *Off and svAmt > 0) or
              (xxChkInd = *On and svAmt < 0);
             //*** End Chg ***** 01/31/25 *****************************
                ChkCrossFt();
             Endif;
             //*** End Add ***** 04/26/24 *****************************

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        EAsycd = ScSysTypE;
        EAfnd  = scGroupE;
        EAfyr  = scYearE;
        EAemp# = scEmpNoE;
        EAdiv  = scDivE;
        EAarcd = scARCodeE;
        Chain (##co#:EAarcd) PTT01;
        If %found(wtabl);
           EAdesc = TTdesc;
           EActyp = TTctyp;
        Endif;
        //***  Beg Add  **** 08/07/24 *******************************
        // Move User Defined A/R Code Description.
        If scARDescE <> *Blanks;
           EAdesc = scARDescE;
        Endif;
        //***  End Add  **** 08/07/24 *******************************
        EAcash = svcash;
        EAchk# = scChkNoE;
        EAckam = ScChkAmtE;
        EAamt  = scPostAmtE;
        EApodt = dtCYMD(scPostDtE);
        EAlate = scLatePyE;
        EAinvtypid = scInvTypID;
        EAtrdt = DtCYMD(scTrnDateE);

        //***  Beg Add  **** 09/11/23 *******************************
        If visBtchHdr <> *on;
           EAtrdt = svtrdt;
        Endif;
        //***  End Add  **** 09/11/23 *******************************
        isICgroup = IcGroupOpt(EACo#:EAfnd);

        If EAinv# = 0 and isInvoiceCloud and isICgroup;
           FindInv#();
           EAinv# = intnInv;
        Endif;

       //*** Begin Add *** 08/27/24 **************************************
       // Check to see if a payment reversal happenned to a paid
       // Invoice.  If there was a reversal then launch popup to ask
       // if they want to resend invoice to IVC
       If CheckPaymentReversal(EAco#:EAfnd:EAemp#:EAdiv:EApoDt:EAinv#);
          chrMessage = 'You are reversing a paid IVC invoice, do you ' +
                       'want to resend that invoice to IVC?';
          chrPanelTitle   = 'Resend Invoice to IVC';
          If PopTwoBtn(chrMessage:chrPanelTitle) = *On;
             chrJson = '{' +
                 '"type":"billing",' +
                 '"invoice":"'  + %EditC(EAinv#:'X')    + '",' +
                 '"company":"'  + %EditC(EACo#:'X')     + '",'  +
                 '"group":"'    + %EditC(EAfnd:'X')     + '",'  +
                 '"employer":"' + %EditC(EAemp#  :'X')  + '",'  +
                 '"division":"' + %EditC(EAdiv:'X')     + '",'  +
                 '"active":"true"' +
                 '}';
             ThirdPartyInvoicing(chrJson);
             FinalizeThirdPartyInvoicing(EAco#:EAfnd);
          EndIf;
       EndIf;
       //*** End   Add *** 08/27/24 **************************************

       EndSr;

       // Writing AR batch details subroutine
       BegSr Writer;
        Clear pea0p;
        EAco# = ##co#;
        EAbat# = kkbat#;
        Exsr MoveScDb;
        if visBtchHdr = *off;
           EAtrdt = svtrdt;
        endif;
        EAachf = svachf;
        EAbata = svbata;
        EAtfap = svtfap;
        EAtffl = svtffl;
        EAstat = svstat;
        Exsr Zupfld;
        write pea0p;
        //***  Beg Add  **** 10/09/23 *******************************
        savEAPrrn = dbrrn02;          // Save WDEAP file RRN of new batch detail record
        //***  End Add  **** 10/09/23 *******************************
        //*** Begin Add ***** 08/05/25 *****************************
        svBtnARCan = *off;
        //*** End   Add ***** 08/05/25 *****************************
        if EAcash = 'Y';
           DissolvePay();

           //*** Begin Add ***** 08/05/25 *****************************
           // Delete just written A/R batch record, If user Click Cancel on Auto Cash screen
           if svBtnARCan = *on;
              chain savEAPrrn pea0p;
              delete pea0p;
           Endif;
           //*** End   Add ***** 08/05/25 *****************************
        endif;
        FEOD WDEAP;
        svsycd = EAsycd;
        svfnd  = EAfnd;
        svemp# = EAemp#;
        svdiv  = EAdiv;
        svfyr  = EAfyr;
        svchk# = EAchk#;
        svckam = EAckam;
        svamt  = EAckam;
        if svCash <> 'Y';
           exsr ttlChk;
        endif;
        //***  Beg Add  **** 10/09/23 *******************************
        // Find master batch record with all 9's check number and check amount
        //*** Begin Add ***** 04/22/24 *****************************
        // Do not delete this record for Auto cash dissovle batch
        //*** Begin Del ***** 05/06/24 *****************************
        //*** if EAcash <> 'Y';
        //*** End   Del ***** 05/06/24 *****************************
        //*** End   Add ***** 04/22/24 *****************************
        //*** Begin Add ***** 08/05/25 *****************************
        // Do not delete this record for Auto cash dissovle batch
        // If user Click Cancel on Auto Cash dissolve screen
        if EAcash = 'Y' and svBtnARCan = *on;
        Else;
        //*** End   Add ***** 08/05/25 *****************************
        Chain eaky99 pea00;
        If %found(wdeal);
           chain dbrrn2 pea0p;
           If %found(wdeap);
              delete pea0p;                // Delete record with Group 0, Emp-Div 0-0
              ARDtlRrn = savEAPrrn;        // Replace with RRN of new batch detail record
              //*** Begin Add ***** 04/22/24 *****************************
              if EAcash = 'Y';
                 cash_sav = 'D';
              Endif;
              //*** End   Add ***** 04/22/24 *****************************
           Endif;
        Endif;
        //*** Begin Add ***** 04/22/24 *****************************
        //*** Begin Del ***** 05/06/24 *****************************
        //*** Endif;
        //*** End   Del ***** 05/06/24 *****************************
        //*** End   Add ***** 04/22/24 *****************************
        //*** Begin Add ***** 08/05/25 *****************************
        Endif;
        //*** End   Add ***** 08/05/25 *****************************
        //***  End Add  **** 10/09/23 *******************************
       EndSr;

      *================================================================
      * ttlChk - Determine if the total check has been accounted for
      *================================================================
       begsr ttlChk;
        sameck = *off;
        setll eaky4 PEA04;
        reade eaky4 pea04;
        dow not %eof(wdeal4);
           If L4_EAchk# = svchk#;
              svamt = svamt - L4_EAamt;
           Endif;
           reade eaky4 pea04;
        enddo;
        //*** Beg Chg ***** 01/31/25 *******************************
        //***if svamt > *zeros;
        If (xxChkInd = *Off and svamt > *zeros) or
         (xxChkInd = *On and svamt < *zeros);
        //*** End Chg ***** 01/31/25 *******************************
           sameck = *on;
        endif;
       endsr;

      *================================================================
      * Reload - Re-load the infomation from the last check
      *================================================================
       //***  Beg Chg  **** 10/09/23 *******************************
       // Begsr Reload;
       Begsr ReloadSR;
       //***  Eng Chg  **** 10/09/23 *******************************
        scYearE    = svfyr;
        scEmpNoE   = svemp#;
        scDivE     = svdiv;
        scChkNoE   = svchk#;
        scChkAmtE  = svckam;
        scPostAmtE = svamt;
        sameck = *off;
       endsr;
      *================================================================
      * Zupfld - Update program, user, date/time fields
      *================================================================
       Begsr Zupfld;
        EApgm = Q1PGM;
        EAuser = Q1USER;
        EAdt = Dateto7(SystemDate());
        EAtime = Timeto6(SystemTime());
        if @@mode1= 'A' or EAidt = *zeros;
           Exsr Getdts;
           if svidte > *zeros;
              EAidt = svidte;
           else;
              EAidt = EAdt;
           Endif;
           EAipgm = Q1PGM;
           EAiusr = Q1USER;
        endif;
       endsr;

      * =========================================================== *
      *    getdts - check for different EAidts or Zero Dates
      * =========================================================== *
       Begsr getdts;

        // reset the save fields, then read WDEAL8 logical to get lowest dates.
        svidte = *zeros;
        svdte = *zeros;
        idtflg = *off;
        dtflg = *off;
        lesflg = *off;

        If EAdt <> *zeros and svdte = *zeros;
           svdte  = EAdt;
           If EAidt = *zeros;
              svidte = EAdt;
              dtflg = *on;
           Endif;
        Endif;

        If EAidt <> *zeros and svidte = *zeros;
           svidte = EAidt;
        Endif;

        If svidte = *zeros and svdte = *zeros;
           svidte = DateTo7(SystemDate);
           svdte  = svidte;
           dtflg  = *on;
        Endif;

        Setll eaky pea08;
        reade eaky pea08;
        // If no hit on file, then zero out save fields, there are no other
        // A/R batch records for this batch
        if %eof(wdeal8);
           svidte = *zeros;
           svdte  = *zeros;
        endif;

        Dow not %eof(wdeal8);
           select;
           When L8_EAidt <> *zeros;
              if L8_EAidt < svidte;
                 svidte = L8_EAidt;
                 idtflg = *on;
                 lesflg = *on;
              endif;
           When L8_EAidt = *zeros and L8_EAdt <> *zeros;
              if L8_EAdt < svidte;
                 svdte = L8_EAdt;
                 dtflg = *on;
                 lesflg = *on;
              endif;
           Endsl;
           reade eaky pea08;
        enddo;

        // After pass loop, get the lower date if either Flag is on
        // Else zero out the svdt field
        if idtflg OR dtflg;
           if svdte < svidte;
              svidte = svdte;
           endif;
        else;
           svidte = *zeros;
        endif;

       endsr;

      *================================================================
      * LoadInit - Move data base fields to screen fields
      *================================================================
       Begsr LoadInit;

        Setll ##co# psy01;
        reade ##co# psy01;
        dow not %eof(wtabl);
           If SYtype = 'Y' and SYstat ='A';
              ScSysTypE = sykey;
              leave;
           endif;
           reade ##co# psy01;
        enddo;
        scGroupE = svfnd;
        scPremFinE = *blanks;

        if Add2fl = *blanks;
           Add2fl = 'Y';
        elseif sameck = *on;
           //***  Beg Chg  **** 10/09/23 *******************************
           // exsr reload;
           exsr reloadSR;
           //***  End Chg  **** 10/09/23 *******************************
        endif;

        scEmpNameE = *blanks;
        scActStsE = *blanks;
        scCanDateE = *zeros;

        svfyr = scYearE;
        svemp# = scEmpNoE;
        svdiv = scDivE;
        svamt3 = scPostAmtE;

        // Get Employer Cross Reference info for header
        chain exky pex01;
        if %found(wzexl);
           scPriGroup = EXofnd;
           scPriEmp = EXoemp;
           scPriDiv = EXodiv;
        endif;

        scARCodeE = *blanks;
        //***  Beg Add  **** 08/07/24 *******************************
        // User Defined A/R Code Description.
        scARDescE = *blanks;
        //***  End Add  **** 08/07/24 *******************************
        scLatePyE = *blanks;
        svamt2 = scPostAmtE;

        scInvTypID = *zeros;

        if ClientID = '133';
           scPostDtE = DateTo8(SystemDate);
        endif;
        scTrnDateE = *zeros;
        scVIP = *blanks;

        if scEmpNoE <> *zeros;
           chain emky wmeml;
           if %found(wmeml);
              EmpInf();
           endif;
        endif;

        if svcash = 'Y';
           svcashDissolve = *on;
        else;
           svcashDissolve = *off;
        endif;
       Endsr;

      * =========================================================== *
      * SaveAR - save AR details for processing
      * =========================================================== *
       Begsr SaveAR;
        if WtPolARflg = *on;
           WtPolARflg = *off;
        endif;
        if isError1 = *on;
           isError1 = *off;
        endif;
       endsr;

      /end-free
     P ProcessAddBtch  E

       //===================================================================
       // ProcOther - Process other Options for AR batch master
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBatAdd;
       isexit = *off;

       ReadC sflARBtch;
       Dow not %eof;

          Select;
          When scARBEdt = *On; // Processing for Edit A/R Batch Link
             scARBEdt = *Off;
             mode = 'C';

             //*** Beg Add ***** 12/16/24 *****************************************
             If CheckLock(intCo#:infndVar:scBat#:'ARB_EDIT');
                Return;
             Endif;
             //*** End Add ***** 12/16/24 *****************************************

             Chain(n) scRRN WDEAP;
             //*** Beg Add *** 11/06/24 *******************************************
             //*** Beg Del *** 12/16/24 *******************************************
             //***If Validatelck();
             //***   PopOneBtn('Batch ID selected is already in use':
             //***             'A/R Batch Lock':'Ok');
             //***   Return;
             //***Endif;
             //*** End Del *** 12/16/24 *******************************************
             //*** End Add *** 11/06/24 *******************************************
             //*** Begin Add ***** 09/05/23 *****************************
             Update sflARBtch;
             Chain q1user pup00;
             If ValidateAct(mode);
             //*** End   Add ***** 09/05/23 *****************************

             Exsr MovDbSc;

             scBatHdg = 'Edit A/R Batch';
             BatAddEdt = *on;   //disable non-editable field
             svsflrrn = scSflrrn;  //save rrn value

             Dow isexit = *off;
                Exfmt rcdBatAdd;

                Select;
                // Save Button
                When BtnSaveBat = *on;
                   BtnSaveBat = *off;
                   // Check if WDEAP updated since screen displayed (A/R Batch File)
                   flgRcdUpdated=RcdUpdatedErr('WDEAP':dbrrn02:'EAdt':'EAtime':
                    'EAuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN WDEAP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateBat(mode);
                         ProcUpdBat(scRRN:mode:scBatAad:scTrDtad);
                //       Chain scRRN WDEAP;
                //       Exsr MoveScDb;
                //       Update pea0p;
                         isexit = *on;
                         reload = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnBatCan = *on;
                   BtnBatCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;
             //*** Beg Add ***** 12/16/24 *******************************
             ClearPolicyLock(intCo#:infndVar:scBat#);
             //*** End Add ***** 12/16/24 *******************************
             //*** Begin Add ***** 09/05/23 *****************************
             EndIf;
             //*** End   Add ***** 09/05/23 *****************************

          When scARBDlt = *On; // Processing for Delete A/R Batch Link
             scARBDlt = *Off;

             //*** Beg Add ***** 12/16/24 *****************************************
             If CheckLock(intCo#:infndVar:scBat#:'ARB_DELETE');
                Return;
             Endif;
             //*** End Add ***** 12/16/24 *****************************************

             //*** Beg Add *** 11/06/24 *******************************************
             //*** Beg Del *** 12/16/24 *******************************************
             //***Chain(n) scRRN WDEAP;
             //***If Validatelck();
             //***   PopOneBtn('Batch ID selected is already in use':
             //***             'A/R Batch Lock':'Ok');
             //***   Return;
             //***Endif;
             //*** End Del *** 12/16/24 *******************************************
             //*** End Add *** 11/06/24 *******************************************
             Chain(e) scRRN WDEAP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             //*** Begin Chg ***** 04/19/24 *****************************
             //*** Chain scRRN WDEAP;
             Chain(n) scRRN WDEAP;
             //*** End   Chg ***** 04/19/24 *****************************
             //*** Begin Add ***** 09/05/23 *****************************
             Update sflARBtch;
             Chain q1user pup00;
             mode = 'D';
             If ValidateAct(mode);
             //*** End   Add ***** 09/05/23 *****************************
             Exsr MovDbSc;

             scBatHdg = 'Delete A/R Batch';

             Dow isexit = *off;

                Write rcdBatAdd;
                //*** Begin Chg ***** 04/19/24 *****************************
                //*** isDelresp = PopTwoBtn('Do you want to delete this A/R Batch+
                //*** ?':'Confirm Delete':'Yes':'No');
                strErrMsg = 'Do you want to Delete this A/R Batch+
                 # ' +  %trim(%editc(EAbat#:'3'));
                isDelresp = PopTwoBtn(strErrMsg:'Confirm Delete':'Yes':'No');
                //*** End   Chg ***** 04/19/24 *****************************
                Select;
                When isDelresp = *On;
                   //*** Begin Add ***** 06/24/24 *****************************
                   xxlnkbat# = 0;
                   //*** End   Add ***** 06/24/24 *****************************
                   //*** Begin Chg ***** 04/19/24 *****************************
                   //*** Delete pea0p;
                   // Delete all remaining detail records of the selected batch
                   setll (EAco#:EAbat#) PEA04;
                   reade (EAco#:EAbat#) PEA04;
                   dow not %eof(wdeal4);
                      //*** Begin Add ***** 06/24/24 *****************************
                      If L4_EAbat2 <> 0;
                         xxlnkbat# = L4_EAbat2;
                      EndIf;
                      //*** End   Add ***** 06/24/24 *****************************
                      Delete pea04;
                      reade (EAco#:EAbat#) PEA04;
                   enddo;
                   //*** End   Chg ***** 04/19/24 *****************************
                   //*** Begin Add ***** 06/24/24 *****************************
                   // Check for Linked A/R batches and Delete if found any record
                   If xxlnkbat# <> 0;
                      Setll (EAco#:xxlnkbat#) PEA04;
                      Reade (EAco#:xxlnkbat#) PEA04;
                      Dow not %Eof(WDEAL4);
                         Delete pea04;
                         Reade (EAco#:xxlnkbat#) PEA04;
                      Enddo;
                   Endif;
                   //*** End   Add ***** 06/24/24 *****************************
                   PopOneBtn('A/R Batch Deleted!':'Delete':'Ok');
                   isexit = *on;
                   reload = *on;
                   bat#_sav = 0;         // Reload the master grid from top
                   svsflrrn = 0;         // Reload the master grid from top
                When isDelresp = *Off;
                   svsflrrn = scSflrrn;  //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
             //*** Beg Add ***** 12/16/24 *******************************
             ClearPolicyLock(intCo#:infndVar:scBat#);
             //*** End Add ***** 12/16/24 *******************************
             //*** Begin Add ***** 09/05/23 *****************************
             EndIf;
             //*** End   Add ***** 09/05/23 *****************************

          Other;
             scSflRRN = svSflRRN;    // Restore the saved row selection number
             scRowSel = svSflRRN;    // Restore the saved row selection number

          Endsl;

          Unlock WDEAP;

          ReadC sflARBtch;
       Enddo;

       If scSflRRN = 0;
          scSflRRN = scRowSel;    // Restore the saved row selection number
       Endif;

       //*** Beg Add *** 11/06/24 *******************************************
       //*** Beg Del *** 12/16/24 *******************************************
       // Delete processed record from wxrip
       //***If isexit = *on;
       //***   Monitor;
       //***      delete pri03;
       //***   On-Error;
       //***   EndMon;
       //***Endif;
       //*** End Del *** 12/16/24 *******************************************
       //*** End Add *** 11/06/24 *******************************************

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scBatAad = EAbata;              // Batch Amount
        scBtypad = EAachf;              // Batch Type
        scTrdtad = dtMDYY(EAtrdt);      // Transaction Date
        scCashad = EAcash;              // Cash Dissolve Batch
        saveDT   = EAdt;
        saveTM   = EAtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        EAbata = scBatAad;              // Batch Amount
        EAtrdt = dtCYMD(scTrdtad);      // Transaction Date
        EAuser = q1user;                // User
        EApgm  = q1pgm;                 // Program
        EAtime = Timeto6(SystemTime()); // Time
        EAdt   = Dateto7(SystemDate()); // Date Mintained
        saveDT = EAdt;
        saveTM = EAtime;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProcOtherBD - Process other Options for AR batch detail
       //===================================================================
     P ProcOtherBD     B
     D ProcOtherBD     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBatDtl;
       isexit = *off;
       //visBtchHdr = *off;

       ReadC sflBtchDtl;
       Dow not %eof;
          //*** Beg Add ***** 12/16/24 *****************************************
          Chain(n) scRrnBtch wdeap;
          //*** Beg Add ***** 02/20/25 *****************************************
          mode = 'C';
          If not ValidateAct2(mode);  //To verify the Approval and Transfer flags,
                                      //there is a possibility that User 2 may have performed those
                                      //actions, and user 1 didn't know until screen is refreshed.
             Leave;
          Endif;
          //*** End Add ***** 02/20/25 *****************************************
          //*** End Add ***** 12/16/24 *****************************************
          //***  Beg Add  **** 08/02/24 *******************************
          scFocus = '1';
          //***  End Add  **** 08/02/24 *******************************

          Select;
          When scBtchEdt = *on;         // Processing for Edit
             //*** Beg Add *** 11/06/24 *******************************************
             scBtchEdt = *off;
             //*** Beg Add ***** 12/16/24 *****************************************
             If CheckLock(intCo#:infndVar:EAbat#:'ARB_EDIT');
                Return;
             Endif;
             //*** End Add ***** 12/16/24 *****************************************
             //*** Beg Del *** 12/16/24 *******************************************
             //***Chain(n) scRrnBtch pea0p;
             //***If ValidateLck();
             //***   PopOneBtn('Batch ID selected is already in use':
             //***             'A/R Batch Lock':'Ok');
             //***   Return;
             //***Endif;
             //*** End Del *** 12/16/24 *******************************************
             //*** End Add *** 11/06/24 *******************************************
             // call for basic info of batch header and processing record.
             PgmStr();

             //*** Beg Del *** 11/06/24 *******************************************
             //*** scBtchEdt = *off;
             //*** End Del *** 11/06/24 *******************************************
             mode = 'C';
             svrrnPrd = rrnBtchDtl;    //Save Rrn value for edited record
             svsflrrn = scSflrrn;      //save rrn value for master grid

             Chain(n) scRrnBtch pea0p;
             Exsr LoadInit;

             scBtchHdg = 'Edit A/R Batch Detail Information';

             Dow isexit = *off;
                //*** Begin Add ***** 06/24/24 *****************************
               // Not allowed to Edit a deductible type of Batch
                If EAachf = 'D';
                   strErrMsg = *Blanks;
                   strErrMsg = 'Deductible batch records cannot be manually +
                    modified, use Deductible Invoice Deletion';
                   PopOneBtn(strErrMsg:'Error':'Ok');
                   isexit = *on;
                   leave;
                Endif;
                //*** End   Add ***** 06/24/24 *****************************
                visPostSch = *off;
                If scEmpNoE <> *zeros and visCashDis = *on;
                   visPostSch = *on;
                Endif;
                Exfmt rcdBatDtl;

                Select;
                When selEmpSrch = *on;
                   EmployerSrch();
                   //***  Beg Add  **** 09/11/23 *******************************
                   InzBatDtErr();
                   //***  End Add  **** 09/11/23 *******************************

                When selPoAmtDt = *on;
                   //***  Beg Add  **** 09/11/23 *******************************
                   InzBatDtErr();
                   //***  End Add  **** 09/11/23 *******************************
                   @@rrn = *blanks;
                   If scEmpNoE <> *zeros and scYearE <> *zeros;
                      @@next = 'PostAmtDt';
                      Chain(n) (##co#:scGroupE:scEmpNoE:scDivE:scYearE) Pel02;
                      If %found(wdell);
                         @@rrn  = %char(dbwdell1);
                      Endif;
                   Endif;
                   If scEmpNoE <> *zeros and scYearE = *zeros;
                      @@next = 'PostYear';
                      Chain(n) (##co#:scGroupE:scEmpNoE:scDivE) Pel02;
                      If %found(wdell);
                         @@rrn  = %char(dbwdell1);
                      Endif;
                   Endif;
                   @@mode = 'D';
                   @@TabInd = *blanks;
                   @@PostDt = *zeros;
                   @@PostAmt = *zeros;

                   // Call WtPolARInq for Post Amount and Date
                   If @@rrn <> *blanks;
                      WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                                @@PostDt:@@PostAmt);
                      If @@PostDt <> *zeros and @@PostAmt <> *zeros;
                         scPostDtE = @@PostDt;
                         scPostAmtE = @@PostAmt;
                      Endif;
                      If @@TabInd <> *blanks and @@next = 'PostYear';
                         scYearE  = %int(@@TabInd);
                         @@next = 'PostAmtDt';
                         @@rrn = *blanks;
                         Chain(n) (##co#:scGroupE:scEmpNoE:
                                   scDivE:scYearE) Pel02;
                         If %found(wdell);
                            @@rrn  = %char(dbwdell1);
                         Endif;
                         If @@rrn <> *blanks;
                            WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                                       @@PostDt:@@PostAmt);
                         endif;
                         If @@PostDt <> *zeros and @@PostAmt <> *zeros;
                            scPostDtE  = @@PostDt;
                            scPostAmtE = @@PostAmt;
                         Endif;
                      Endif;
                   Endif;

                // Save Button
                When BtnBtchSav = *on;
                   BtnBtchSav = *off;
                      If ValidateBatDt(mode);
                         Chain scRrnBtch Wdeap;
                         Exsr UpdatR;
                         isexit = *on;
                         reload = *on;
                         //*** Beg Add ***** 04/26/24 *****************************
                         ChkCrossFt();
                         //*** End Add ***** 04/26/24 *****************************
                      EndIf;

                // Cancel Button
                When BtnBtchCan = *on;
                   BtnBtchCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;
             //*** Beg Add ***** 12/16/24 *****************************************
             ClearPolicyLock(intCo#:infndVar:scBat#);
             //*** End Add ***** 12/16/24 *****************************************


          When scBtchDlt = *on;     // processing for Delete link
             //*** Beg Add *** 11/06/24 *******************************************
             scBtchDlt = *off;
             //*** Beg Add ***** 12/16/24 *****************************************
             If CheckLock(intCo#:infndVar:EAbat#:'ARB_DELETE');
                Return;
             Endif;
             //*** End Add ***** 12/16/24 *****************************************
             //*** Beg Del *** 12/16/24 *******************************************
             //***Chain(n) scRrnBtch pea0p;
             //***If Validatelck();
             //***   PopOneBtn('Batch ID selected is already in use':
             //***             'A/R Batch Lock':'Ok');
             //***   Return;
             //***Endif;
             //*** End Del *** 12/16/24 *******************************************
             //*** End Add *** 11/06/24 *******************************************
             // call for basic info of batch header and processing record.
             PgmStr();

             //*** Beg Del *** 11/06/24 *******************************************
             //*** scBtchDlt = *off;
             //*** End Del *** 11/06/24 *******************************************
             Chain(e) scRrnBtch pea0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user.  Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrnBtch pea0p;
             Exsr LoadInit;

             scBtchHdg = 'Delete A/R Batch detail information';

             Dow isexit = *off;
                //*** Begin Add ***** 06/24/24 *****************************
               // Not allowed to Delete a deductible type of Batch
                If EAachf = 'D';
                   strErrMsg = *Blanks;
                   strErrMsg = 'Deductible batch records cannot be manually +
                    modified, use Deductible Invoice Deletion';
                   PopOneBtn(strErrMsg:'Error':'Ok');
                   isexit = *on;
                   leave;
                Endif;
                //*** End   Add ***** 06/24/24 *****************************

                Write rcdBatDtl;
                //***  Beg Chg  **** 04/19/24 *******************************
                //*** isDelresp = PopTwoBtn('Do you want to delete this A/R Batch +
                //*** information?':'Confirm Delete':'Yes':'No');
                strErrMsg = 'Do you want to Delete this A/R Batch Details +
                 Information for Batch# ' +  %trim(%editc(EAbat#:'3'));
                isDelresp = PopTwoBtn(strErrMsg:'Confirm Delete':'Yes':'No');
                //***  End Chg  **** 04/19/24 *******************************
                Select;
                When isDelResp = *on;
                   Delete pea0p;
                   PopOneBtn('A/R Batch information is +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                   reload = *on;
                   //*** Beg Add ***** 03/19/25 *****************************
                   Chain (EAco#:EAbat#) pea04;
                   If not %Found(wdeal4);
                      bat#_sav = 0;         // Reload the master grid from top
                      svsflrrn = 0;         // Reload the master grid from top
                   Endif;
                   //*** End Add ***** 03/19/25 *****************************
                When isDelResp = *off;
                   isexit = *on;
                   svrrnPrd = rrnBtchDtl;
                   svsflrrn = scSflrrn;      //save rrn value for master grid
                EndSl;
             EndDo;
             //*** Beg Add ***** 12/16/24 *****************************************
             ClearPolicyLock(intCo#:infndVar:scBat#);
             //*** End Add ***** 12/16/24 *****************************************
          Endsl;
        //Unlock pea0p;
          ReadC sflBtchDtl;
       Enddo;

       //*** Beg Add *** 11/06/24 *******************************************
       // Delete processed record from wxrip
       //*** Beg Del *** 12/16/24 *******************************************
       //***If isexit = *on;
       //***   Monitor;
       //***      delete pri03;
       //***   On-Error;
       //***   EndMon;
       //***Endif;
       //*** End Del *** 12/16/24 *******************************************
       //*** End Add *** 11/06/24 *******************************************

       // End of SR to move database fields into screen fields

       // Move data base fields to screen fields
       Begsr LoadInit;
        scSysTypE = EAsycd;
        scGroupE = EAfnd;
        scEmpNameE = *blanks;
        scActStsE = *blanks;
        scCanDateE =*zeros;
        scYearE = EAfyr;
        scEmpNoE = EAemp#;
        scDivE = EAdiv;
        svemp# = scEmpNoE;
        svdiv = scDivE;
        svfyr = scYearE;
        svamt3 = EAamt;

        // Get Employer Cross Reference info for header
        chain exky pex01;
        if %found(wzexl);
           scPriGroup = EXofnd;
           scPriEmp = EXoemp;
           scPriDiv = EXodiv;
        endif;

        scARCodeE = EAarcd;
        //***  Beg Add  **** 08/07/24 *******************************
        // Move into User Defined A/R Code Description.
        scARDescE = EAdesc;
        //***  End Add  **** 08/07/24 *******************************
        scChkNoE = EAchk#;
        ScChkAmtE = EAckam;
        scPostAmtE = EAamt;
        svamt2 = scPostAmtE;
        scLatePyE = EAlate;

        if scLatePyE = 'N';
           scLatePyE = *blanks;
        endif;
        scInvTypID = EAinvtypid;

        scPostDtE = DtMDYY(EApodt);
        scTrnDateE = DtMDYY(EAtrdt);
        scVIP = *blanks;

        if EAemp# <> *zeros;
           chain emky wmeml;
           if %found(wmeml);
              EmpInf();
           endif;
        endif;

        if EAcash = 'Y' or svcash = 'Y';
           svcashDissolve = *on;
           svcash = 'Y';
        else;
           svcashDissolve = *off;
        endif;
       Endsr;

      *================================================================
      * UpdatR - Update data base record and after processing
      *================================================================
       Begsr UpdatR;
        chain scRrnBtch pea0p;
        if %found(wdeap);
        exsr MoveScDb;
           EApgm  = Q1PGM;
           EAuser = Q1USER;
           EAdt   = DateTo7(SystemDate);
           EAtime = TimeTo6(SystemTime);

           // Inital Date zeros, load the initial fields
           if EAidt = *zeros;
              EAidt = EAdt;
              EAipgm = Q1PGM;
              EAiusr = Q1USER;
           endif;
           update pea0p;

           //*** Begin Add ***** 08/05/25 *****************************
           svBtnARCan = *off;
           //*** End   Add ***** 08/05/25 *****************************
           if EAcash = 'Y';
              DissolvePay();
           endif;
        endif;
       endsr;

       BegSr MoveScDb;
        EAsycd = ScSysTypE;
        EAfnd = scGroupE;
        EAfyr = scYearE;
        EAemp# = scEmpNoE;
        EAdiv = scDivE;
        EAarcd = scARCodeE;
        Chain (##co#:EAarcd) PTT01;
        If %found(wtabl);
           EAdesc = TTdesc;
           EActyp = TTctyp;
        Endif;
        //***  Beg Add  **** 08/07/24 *******************************
        // Move User Defined A/R Code Description.
        If scARDescE <> *Blanks;
           EAdesc = scARDescE;
        Endif;
        //***  End Add  **** 08/07/24 *******************************
        //*** Begin Del ***** 04/22/24 *****************************
        //*** EAcash = svcash;
        //*** End   Del ***** 04/22/24 *****************************
        EAchk# = scChkNoE;
        EAckam = ScChkAmtE;
        EAamt = scPostAmtE;
        EApodt = dtCYMD(scPostDtE);
        EAlate = scLatePyE;
        EAinvtypid = scInvTypID;
        EAtrdt = DtCYMD(scTrnDateE);

        isICgroup = IcGroupOpt(EACo#:EAfnd);

        If EAinv# = 0 and isInvoiceCloud and isICgroup;
           FindInv#();
           EAinv# = intnInv;
        Endif;

       EndSr;

      /end-free

     P ProcOtherBD     E

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
     d  inmod          s              1
     d  @@bata         s             11  2 inz(0)
     d  @@trdt         s              8  0 inz(0)

      /Free

       //*** Beg Del ***** 12/16/24 *************************************
       //*** Unlock WXRIL3;
       //*** End Del ***** 12/16/24 *************************************

       ReadC sflARBtch;
       Dow not %eof;
          If scSelect = 'Y';
             //*** Begin Del ***** 09/13/23 *****************************
             // svsflrrn = scSflrrn;  //save rrn value
             //*** End   Del ***** 09/13/23 *****************************
             Chain(n) scRRN pea0p;
             Chain q1user pup00;

             Select;
             When scSelAct = 'Flag for Approval';
                inmod = 'W';
                If ValidateAct(inmod);
                   ProcUpdBat(scRRN:inmod:@@bata:@@trdt);
                   //*** Beg Add ***** 10/11/24 *****************************
                   Exsr ChkLnkBat;
                   //*** End Add ***** 10/11/24 *****************************
                   reload = *on;
                //*** Beg Add ***** 12/16/24 *****************************
                   ClearPolicyLock(intCo#:infndVar:scBat#);
                //*** End Add ***** 12/16/24 *****************************
                EndIf;
                //*** Begin Add ***** 06/24/24 *****************************
                // Check for Linked A/R batches and update if found any
                //*** Beg Del ***** 10/11/24 *****************************
                //*** Exsr ChkLnkBat;
                //*** End Del ***** 10/11/24 *****************************
                //*** End   Add ***** 06/24/24 *****************************
             When scSelAct = 'Flag for Transfer';
                inmod = 'X';
                If ValidateAct(inmod);
                   ProcUpdBat(scRRN:inmod:@@bata:@@trdt);
                   //*** Beg Add ***** 10/11/24 *****************************
                   Exsr ChkLnkBat;
                   //*** End Add ***** 10/11/24 *****************************
                   reload = *on;
                //*** Beg Add ***** 12/16/24 *****************************
                   ClearPolicyLock(intCo#:infndVar:scBat#);
                //*** End Add ***** 12/16/24 *****************************
                EndIf;
                //*** Begin Add ***** 06/24/24 *****************************
                // Check for Linked A/R batches and update if found any
                //*** Beg Del ***** 10/11/24 *****************************
                //*** Exsr ChkLnkBat;
                //*** End Del ***** 10/11/24 *****************************
                //*** End   Add ***** 06/24/24 *****************************
             When scSelAct = 'Edit Report';
                //*** Beg Chg ***** 12/16/24 *******************************
                //*** outCo# = %editc(EAco#:'X');
                //*** evalr outBat# = %char(EAbat#);
                //*** outOutQ = UPoutq;
                //*** EdtReport(outCo#:outBat#:outOutQ);
                If not(CheckLock(intCo#:infndVar:scBat#:'ARB_EDTRPT'));
                   outCo# = %editc(EAco#:'X');
                   outBat# = %EditC(EAbat#:'X');
                   outOutQ = UPoutq;
                   EdtReport(outCo#:outBat#:outOutQ);
                Endif;
                //*** End Chg ***** 12/16/24 *******************************
             When scSelAct = 'Create EFT File';
                inmod = 'E';
                If ValidateAct(inmod);
                   outCo# = %editc(EAco#:'X');
                   //*** Beg Chg ***** 12/16/24 ****************************
                   //***evalr outBat# = %char(EAbat#);
                   outBat# = %EditC(EAbat#:'X');
                   //*** End Chg ***** 12/16/24 ****************************
                   // Convert CYMD (7,0) date to YMD (6,0)
                   outDt8 = %dec(EAidt:8:0);
                   outFfmt = '*CYMD';
                   outTfmt = '*YMD';
                   inDt10 = *blanks;
                   inError = *blanks;
                   DTYMD(outDt8:outFfmt:outTfmt:inDt10:inError);
                   //*** Begin Add ***** 09/04/23 *****************************
                   If inError = *off;
                   //*** End   Add ***** 09/04/23 *****************************
                      outDate6 = %subst(inDt10:1:6);
                      CrtEFTf(outCo#:outBat#:outDate6);
                      //*** Begin Add ***** 10/18/23 *****************************
                      reload = *on;
                      //*** End   Add ***** 10/18/23 *****************************
                   //*** Begin Add ***** 09/04/23 *****************************
                   Endif;
                   //*** End   Add ***** 09/04/23 *****************************
                EndIf;
             When scSelAct = 'Mass Update';
                inmod = 'M';
                If ValidateAct(inmod);
                   ProcMasUpd();
                EndIf;
             Endsl;
             //*** Beg Add *** 11/06/24 ******************************
             // Delete processed record from wxrip
             //*** Beg Del *** 12/16/24 ******************************
             //***If scSelAct = 'Flag for Approval' or
             //***  scSelAct = 'Flag for Transfer';
             //***   Monitor;
             //***      delete pri03;
             //***   On-Error;
             //***   EndMon;
             //***Endif;
             //*** End Del *** 12/16/24 ******************************
             //*** End Add *** 11/06/24 ******************************
          Endif;

          ReadC sflARBtch;
       Enddo;

       //*** Begin Add ***** 06/24/24 *****************************
       // Check for Linked A/R batches and update if found any record
       Begsr ChkLnkBat;
        xxlnkbat# = 0;
        Setll (EAco#:EAbat#) pea04;
        Reade(N) (EAco#:EAbat#) pea04;
        Dow not %Eof(WDEAL4);
           If L4_EAbat2 <> 0;
              xxlnkbat# = L4_EAbat2;
              leave;
           EndIf;
           Reade(N) (EAco#:EAbat#) pea04;
        Enddo;
        If xxlnkbat# <> 0;
           Chain(n) (EAco#:xxlnkbat#) pea04;
              IntEmpRrn = %Dec(dbrrn5:9:0);
              Chain(n) IntEmpRrn pea0p;          // Validate Linked Batch record
              //*** Beg Add ***** 10/11/24 *****************************
              scbat# = xxlnkbat#;                // Pass link batch# for validation
              //*** End Add ***** 10/11/24 *****************************
              If ValidateAct(inmod);
                 ProcUpdBat(IntEmpRrn:inmod:@@bata:@@trdt);
              EndIf;
              Chain(n) scRRN pea0p;              // Reset to original Batch record
        EndIf;
       Endsr;
       //*** End   Add ***** 06/24/24 *****************************

      /End-Free
     p ProcPerAct      e

       //===================================================================
       // ProcMasUpd - Process to Mass Update of Batch
       //===================================================================
     p ProcMasUpd      b
     d ProcMasUpd      pi
     d  isexit         s               n   Inz(*off)

      /Free

       Clear rcdMasUpd;
       isexit = *off;

       Dow isexit = *off;

          Exfmt rcdMasUpd;

          Select;
          // Submit Button
          When BtnMasSbm = *on;
             BtnMasSbm = *off;
             If ValidateMas();
                Exsr MasUpd;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnMasCan = *on;
             BtnMasCan = *off;
             isexit = *on;
          EndSl;
       EndDo;

       Begsr MasUpd;
        // Update Check# and Posting Date of Batch
        Setll (EAco#:EAbat#) pea04;
        Reade (EAco#:EAbat#) pea04;
        Dow not %eof(wdeal4);
           L4_EAchk# = scNchk;                 // Check#
           L4_EApodt = dtCYMD(scPodt);         // Posting Date

           L4_EAuser  = q1user;                // User
           L4_EApgm   = q1pgm;                 // Program
           L4_EAtime  = Timeto6(SystemTime()); // Time
           L4_EAdt    = Dateto7(SystemDate()); // Date Mintained
           L4_EAiusr  = q1user;                // Initial User
           L4_EAipgm  = q1pgm;                 // Initial Program
           L4_EAidt   = Dateto7(SystemDate()); // Initial Date

           Update pea04;
        Reade (EAco#:EAbat#) pea04;
        Enddo;
       Endsr;
      /End-Free
     p ProcMasUpd      e

       //===================================================================
       // ProcMorActTB - Process More Action items from A/R Batch Toolbar
       //                1) Edit Open Batches
       //                2) Transfer Batches
       //                3) Purge
       //                4) List by Employer
       //===================================================================
     p ProcMorActTB    b
     d ProcMorActTB    pi
     d  isexit         s               n   Inz(*off)

      /Free
       ##Co# = intCo#;
       Select;
       When txtShowWin = 'Edit Open Batches';
          // Edit Open Batches from A/R Batch toolbar (More Actions)
          Chain q1user pup00;
          outCo# = %char(intCo#);
          outBat# = ' ';
          outOutQ = UPoutq;
          EdtReport(outCo#:outBat#:outOutQ);

       When txtShowWin = 'Transfer Batches';
          // Transfer Batches from A/R Batch toolbar (More Actions)
          Chain q1user pup00;
          outCo# = %char(intCo#);
          outdate = ' ';
          outOutQ = UPoutq;
          TransBat(outCo#:outdate:outOutQ);

       When txtShowWin = 'Purge';
          // Purge from A/R Batch toolbar (More Actions)
          Clear rcdPurBat;
          isexit = *off;
          Dow isexit = *off;
             // Load A/R Batch Purge History Grid
             LoadPurHist(inIncludeP);
             scSelActP = '--- Select List Action ---';
             ChoSelActP = '--- Select List Action ---,Delete';
             Exfmt rcdPurBat;

             Select;
             // Include All
             When btnPurIncl = *on;
                btnPurIncl = *off;
                inIncludeP = 'Include';

             // Exclude All
             When btnPurExcl = *on;
                btnPurExcl = *off;
                inIncludeP = ' ';

             // Perform Action
             When btnPerActP = *on;
                btnPerActP = *off;

                ReadC sflPurHist;
                Dow not %eof;
                   If scSelectP = 'Y';
                      Chain scPurHRRN pbp0p;
                      If %found(wmbpp);
                         If BPcmdt = 0;
                            Delete pbp0p;
                   //*** Begin Chg ***** 09/04/23 *****************************
                   //       PopOneBtn('A/R Batch Purge Request Deleted!':
                   //        'Delete':'Ok');
                            PopOneBtn('A/R Batch Purge Request for Date - ' +
                             %char(DTMDYY(BPpgdt))+', Deleted!':'Delete':'Ok');
                   //*** End   Chg ***** 09/04/23 *****************************
                         Endif;
                      Endif;
                   Endif;
                   ReadC sflPurHist;
                Enddo;

             // Submit Button
             When BtnPurSbm = *on;
                BtnPurSbm = *off;
                If ValidatePur();
                   Exsr WrtPur;
                   clear scPgDt;
                EndIf;

             // Cancel Button
             When BtnPurCan = *on;
                BtnPurCan = *off;
                isexit = *on;
             EndSl;
          EndDo;

       When txtShowWin = 'List by Employer';
          // List by Employer from A/R Batch toolbar (More Actions)
          Clear rcdBatInq;
          isexit = *off;

          // Load A/R Batch Inquiry Grid
          rrnBatInq = 0;
          dspBatInq = *off;
          clrBatInq = *on;
          Write rcdBatInq;
          clrBatInq = *off;
          dspBatInq = *on;

          Dow isexit = *off;
             Exfmt rcdBatInq;

             Select;
             When selEmpSrch = *on;       //Employer Search
                isEmpNbr = '0';
                outMode  = 'E';
                outCall  = '2';
                clear dsSrchCrit;
                clear outSelect;
                dsEmpGrp  = scGroupI;
                outSelect  = dsSrchCrit;
                outRrn#   = '000000000';
                EmpSearch(outMode:outCall:outRrn#:outSelect);

                If outRrn# <> '000000000';
                   IntEmpRrn = %Dec(outRrn#:9:0);
                   Chain intEmpRrn pel01;
                   If %Found(Wdelp);
                      scGroupI = ELfnd;
                      scEmpNoI = ELemp#;
                      scDivI   = ELdiv;
                   EndIf;
                EndIf;

             When selYearSrh = *on;     // Year Search
                @@rrn = *blanks;
                If scGroupI <> *zeros and scEmpNoI <> *zeros;
                   @@next = 'PostYear';
                   Chain(n) (##co#:scGroupI:scEmpNoI:scDivI) PEL02;
                   If %found(wdell);
                      @@rrn  = %char(dbwdell1);
                   Endif;
                Endif;

                @@mode = 'D';
                @@TabInd = *blanks;
                @@PostDt = *zeros;
                @@PostAmt = *zeros;

                // Call WtPolARInq for Policy Year Search
                If @@rrn <> *blanks;
                   WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                              @@PostDt:@@PostAmt);
                   If @@TabInd <> *blanks and @@next = 'PostYear';
                      scYearI = %int(@@TabInd);
                   Endif;
                Endif;

             // Submit Button
             When BtnInqSbm = *on;
                BtnInqSbm = *off;
                If ValidateInq();
                   // Load A/R Batch Inquiry Grid
                   LoadARBatInq();
                EndIf;

             // Cancel Button
             When BtnInqCan = *on;
                BtnInqCan = *off;
                isexit = *on;
             EndSl;
          EndDo;

       Endsl;

       Begsr WrtPur;
        // Write Batch Purge request
        BPco# = ##Co#;                  // Company
        BPpgdt = dtCYMD(scPgdt);        // Purge Date
        BPcmdt = 0;                     // Complete Date
        BPuser = q1user;                // User
        BPpgm  = q1pgm;                 // Program
        BPtime = Timeto6(SystemTime()); // Time
        BPdt   = Dateto7(SystemDate()); // Date Mintained
        Write pbp00;
       Endsr;



      /End-Free
     p ProcMorActTB    e

       //===================================================================
       // ProcRoWClk - Process the Row Click in AR master Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load AR details grid for selected row.
       Chain scsflRRN sflARBtch;
       Chain(n) scRRN wdeap;

       ARDtlRrn = scRRN;
       svsflRRN = scsflRRN;
       scrowsel = scsflRRN;

       co#_sav  = EAco#;
       stat_sav = EAstat;
       bat#_sav  = EAbat#;
       cash_sav  = EAcash;
       //*** Begin Add ***** 06/24/24 *****************************
       btyp_sav = EAachf;              // Save Batch Type
       //*** End   Add ***** 06/24/24 *****************************

      /end-free

     P ProcRoWClk      E

       //===================================================================
       // ProcUpdBat - Process to Update all Batch records
       //===================================================================
     p ProcUpdBat      b
     d ProcUpdBat      pi
     d inrrn                          9  0
     d inmod                          1
     d inbata                        11  2
     d intrdt                         8  0

     d svbata          s                   like(EAbata)
     d svtfap          s                   like(EAtfap)
     d svtffl          s                   like(EAtffl)
     d svtrdt          s                   like(EAtrdt)
     d svstat          s                   like(EAstat)

      /Free
       // Update Main Batch record
       Chain inrrn pea0p;
       If %found(wdeap);
          Select;
          When inmod = 'W';
             If EAtfap = 'Y';
                EAtfap = 'N';
             Else;
                EAtfap = 'Y';
             Endif;
          When inmod = 'X';
             If EAtffl = 'Y';
                EAtffl = 'N';
             Else;
                EAtffl = 'Y';
             Endif;
          When inmod = 'C';
             EAbata = scBatAad;              // Batch Amount
             EAtrdt = dtCYMD(scTrdtad);      // Transaction Date
             saveDT = Dateto7(SystemDate());
             saveTM = Timeto6(SystemTime());
          Endsl;

          EAuser  = q1user;                // User
          EApgm   = q1pgm;                 // Program
          EAtime  = Timeto6(SystemTime()); // Time
          EAdt    = Dateto7(SystemDate()); // Date Mintained

          Update pea0p;

          svbata = EAbata;
          svtfap = EAtfap;
          svtffl = EAtffl;
          svtrdt = EAtrdt;
          svstat = EAstat;

          // Update All Batch records
          Setll (EAco#:EAbat#) pea04;
          Reade (EAco#:EAbat#) pea04;
          Dow not %eof(wdeal4);
             L4_EAbata = svbata;
             L4_EAtfap = svtfap;
             L4_EAtffl = svtffl;
             L4_EAstat = svstat;
             // Prevent EATRDT from being updated for retro batches
             If L4_EAachf <> 'R';
                L4_EAtrdt = svtrdt;
             Endif;

             Update pea04;
          Reade (EAco#:EAbat#) pea04;
          Enddo;

       Endif;

      /End-Free
     p ProcUpdBat      e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;

          scActMode = @@mode;
       Endif;

       pnlHeight = 120;
       apnlHeight = pnlHeight + 60;

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;
       //*** Begin Add ***** 02/11/25 *****************************
       ##Co# = intCo#;
       //*** End   Add ***** 02/11/25 *****************************

       // Retrieve user parameter
       Chain q1user pup00;

       // Choices for Action Dropdown
       If @@Mode = 'D';
          ChoSelAct = ' ';
       Else;
            // ChoSelAct = '--- Select List Action ---,Flag for Approval,+
            //  Flag for Transfer,Edit Report,Create EFT File,Mass Update';
          ChoSelAct = '--- Select List Action ---';

          // Check Flag for Approval Security
          If CheckSecLevel('ARBat_FlgA':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Flag for Approval';
          EndIf;

          // Check Flag for Transfer Security
          If CheckSecLevel('ARBat_FlgT':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Flag for Transfer';
          EndIf;

          // Check Edit Report Security
          If CheckSecLevel('ARBat_ERpt':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Edit Report';
          EndIf;

          //*** Begin Add ***** 09/17/24 *****************************
          // Get Flags value from Workers Comp control file SWCPF
          Chain 'WC' swc01;
          If %found() and (WCfl24 = 'Y' or WCfl38 = 'Y');
          //*** End   Add ***** 09/17/24 *****************************
          // Check Create EFT File Security
          If CheckSecLevel('ARBat_CrtF':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Create EFT File';
          EndIf;
          //*** Begin Add ***** 09/17/24 *****************************
          EndIf;
          //*** End   Add ***** 09/17/24 *****************************

          //*** Begin Del ***** 09/05/23 *****************************
          // Check Mass Update Security
          // If CheckSecLevel('ARBat_MUpd':UPtype:UPlvl);
          //    ChoSelAct = %trim(ChoSelAct) + ',Mass Update';
          // EndIf;
          //*** End   Del ***** 09/05/23 *****************************
       Endif;
       scSelAct = '--- Select List Action ---';

       // Set Visibility of Invoice Type ID
       isInvType = IsModEnabled('S/R Online') or
                   IsModEnabled('Third Party Payment');

       // Check for Multi Group Restrictions
       $$co#  = intCo#;
       $$usr  = q1user;
       $$appl = 'W';
       $$rtcd = *off;
    
       SA008R($$co#:$$usr:$$appl:$$rtcd);
       multig = $$rtcd;

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Action.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAct     b
     d ValidateAct     pi              n
     d strmode                        1
     d isError         s               n   inz
     d strmsg          s            100    inz
     d strmsgd         s            500    inz
     d strmsgid        s             10    inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.

       // Modes
       // 'W' -- Flag for Approval  -- opt 8
       // 'X' -- Flag for Transfer  -- opt 9
       // 'E' -- Create EFT File    -- opt 12
       // 'M' -- Mass Update        -- opt 13
       // 'C' -- Change             -- opt 2
       // 'D' -- Delete             -- opt 4

        //*** Beg Add *** 11/06/24 *******************************************
        scnxtchg = *off;
        //*** Beg Chg *** 12/16/24 *******************************************
        //***If strmode = 'W' or strmode = 'X';
        //***   If ValidateLck();
        //***      PopOneBtn('Batch ID selected is already in use':
        //***                'A/R Batch Lock':'Ok');
        //***      scnxtchg = *on;
        //***      Update sflARBtch;
        //***      Return *off;
        //***   Endif;
        //***Endif;
        If strmode = 'W' or strmode = 'X' or strmode = 'E';
           Select;
           When strmode = 'W';     //Flag for Approval
              If CheckLock(intCo#:infndVar:scBat#:'ARB_APPRVL');
                 scnxtchg = *on;
                 Update sflARBtch;
                 Return *off;
              Endif;
           When strmode = 'X';     //Flag for Transfer
              If CheckLock(intCo#:infndVar:scBat#:'ARB_XFER');
                 scnxtchg = *on;
                 Update sflARBtch;
                 Return *off;
              Endif;
           When strmode = 'E';     //Create EFT File
              If CheckLock(intCo#:infndVar:scBat#:'ARB_CRTEFT');
                 scnxtchg = *on;
                 Update sflARBtch;
                 Return *off;
              Endif;
           EndSl;
        Endif;

        //*** End Chg *** 12/16/24 *******************************************
        //*** End Add *** 11/06/24 *******************************************
        //Chain scRRN pea0p;
        //*** Begin Add ***** 10/05/23 *****************************
        scCo# = intCo#;
        ##Co# = intCo#;
        //*** End   Add ***** 10/05/23 *****************************

       // Don't allow batch to unapprove if it has already been submitted for EFT
        If strmode = 'W' and EAachf = 'A' and EAachd <> 0;
           If clientID <> '146';
              strmsg = 'Batch can not be edited after EFT has been submitted.';
              strmsgid = *Blanks;
              isError = *on;
           Endif;
        Endif;

       // Check if this batch has any restrictions
        //*** Beg Add ***** 10/11/24 *****************************
        clear xxgrst;
        if strmode <> 'C';           // not required for Batch Edit link
        //*** End Add ***** 10/11/24 *****************************
        if multig = *on;
           $$co#  = ##co#;
           $$usr  = q1user;
           $$appl = 'W';
           $$rtcd = *off;
           $$auth = ' ';
           //*** Beg Chg ***** 10/11/24 *****************************
           //*** $$fnd  = scGroupE;
           //*** $$emp# = scEmpNoE;

           //*** SA008R($$co#:$$usr:$$appl:$$rtcd:$$auth:$$fnd:$$emp#);
           // Check group restriction for each of the detailed batch records
           Setll eaky999 wdeal9;
           Reade eaky999 wdeal9;
           Dow not %eof(wdeal9);
              //*** Begin Add ***** 02/11/25 *****************************
              // When there is no detail record in the batch, Group and Employer
              // will be 0 and nothing to check
              If L9_EAfnd = 0 or L9_EAemp# = 0;
                 Leave;
              Endif;
              //*** End   Add ***** 02/11/25 *****************************

              $$fnd  = L9_EAfnd;
              $$emp# = L9_EAemp#;
              SA008R($$co#:$$usr:$$appl:$$rtcd:$$auth:$$fnd:$$emp#);
              If $$rtcd = *on or $$auth = 'I';
                 Leave;
              Endif;
              Reade eaky999 wdeal9;
           Enddo;
           //*** End Chg ***** 10/11/24 *****************************
           failed = $$rtcd;
           if failed = *on or $$auth = 'I';
              xxgrst = 'Y';
           endif;
        else;
           if UPxfnd = 'Y' and UPfnd <> EAfnd and EAfnd <> 0;
              xxgrst = 'Y';
           endif;
        endif;
        //*** Beg Add ***** 10/11/24 *****************************
        endif;
        //*** End Add ***** 10/11/24 *****************************

        If (strmode = 'C' or strmode = 'D' or strmode = 'M' or
           strmode = 'W' or strmode = 'X') and isError = *off;
           If xxgrst = 'Y';
              strmsg = *Blanks;
              strmsgid = 'WC32375';
              isError = *on;
           Endif;
        Endif;

       // Not allowed to Change, Delete or Mass Update if either the flag for approved
       // or the flag for transfer is set to "Y"
        If (strmode = 'C' or strmode = 'D' or strmode = 'M') and isError = *off;
           If EAtffl = 'Y' or EAtfap = 'Y';
              strmsg = *Blanks;
              strmsgid = 'WC31534';
              isError = *on;
           Endif;
        Endif;

       // Allow 'Create EFT File' only if A/R Batch is Approved
        If strmode = 'E' and EAachf = 'A' and EAtfap <> 'Y' and isError = *off;
           strmsg = *Blanks;
           strmsgid = 'WC40122';
           isError = *on;
        Endif;

       // Allow 'Flag for Transfer' only if 'Create EFT File' option has run
        If strmode = 'X' and EAachf = 'A' and EAachd = 0  and isError = *off;
           strmsg = *Blanks;
           strmsgid = 'WC31535';
           isError = *on;
        Endif;

       // Check if this batch got transferred
        If (strmode = 'C' or strmode = 'W' or strmode = 'X') and isError = *off;
           If EAstat = 'T';
              strmsg = *Blanks;
              strmsgid = 'WC30660';
              isError = *on;
           Endif;
        Endif;

       // Check if batch amount equals total amount entered
        If (strmode = 'E' or strmode = 'W' or strmode = 'X') and isError = *off;
           If scBatA <> scAmtEnt;
              strmsg = *Blanks;
              strmsgid = 'WC30664';
              isError = *on;
           Endif;
        Endif;

       // Don't allow batch to Approve and Transfer as employer is deleted
        If (strmode = 'W' or strmode = 'X') and isError = *off;
           //*** Begin Chg ***** 10/23/23 *****************************
           // Setll *loval wdeal9;
           // Read wdeal9;
           Setll eaky999 wdeal9;
           Reade eaky999 wdeal9;
           //*** End   Chg ***** 10/23/23 *****************************
           Dow not %eof(wdeal9);
              If L9_EAco# = scCo# and L9_EAbat# = scBat#;
                 chain (L9_Eaco#:L9_Eafnd:L9_Eaemp#:L9_Eadiv) pem00;
                 If Not %Found(wmeml);
                    //*** Begin Chg ***** 10/09/23 *****************************
                    // strmsg = 'This option not allowed for this batch as+
                    strmsg = 'This option not allowed for this batch as +
                    //*** End   Chg ***** 10/09/23 *****************************
                    Employer/Div is deleted.';
                    strmsgid = *Blanks;
                    isError = *on;
                    Leave;
                 Endif;
              Endif;
              //*** Begin Chg ***** 10/23/23 *****************************
              // Read wdeal9;
              Reade eaky999 wdeal9;
              //*** End   Chg ***** 10/23/23 *****************************
           Enddo;
        Endif;

       // Check if batch transfer is in progress
        If (strmode = 'W' or strmode = 'X') and isError = *off;
           //*** Begin Chg ***** 09/17/24 *****************************
           //*** In *dtaara;
           //*** Begin Chg ***** 09/24/24 *****************************
           //*** In trnsfr;
           //*** End   Chg ***** 09/17/24 *****************************
           //*** istrnsfr = '0';
           //*** istrnsfr = trnsfr;
           //*** If istrnsfr = '1';
           //***    strmsg = *Blanks;
           //***    strmsgid = 'WC30667';
           //***    isError = *on;
           //*** Endif;
           Monitor;
              In trnsfr;
           On-Error;
              strmsg = *Blanks;
              strmsgid = 'WC30667';
              isError = *on;
           EndMon;
           //*** End   Chg ***** 09/24/24 *****************************
        Endif;


       // Validate all conditions of ACH Batch for 'Create EFT File'
        If strmode = 'E';
          // Check for non-ACH or transferred batches
           If (EAachf = ' ' or EAstat = 'T') and isError = *off;
              strmsg = *Blanks;
              strmsgid = 'WC30275';
              isError = *on;
           Endif;

          // Ensure that ACH batch is not transmitted already
           If EAachd <> 0 and isError = *off;
              // Set message variable date
              mtxt = %char(DTMDYY(EAachd)) + ' by ' + EAachu;
              strmsg = *Blanks;
              strmsgid = 'WCR3323';
              isError = *on;
           Endif;

          // Ensure that ACH batch is not in progress
           //*** Begin Chg ***** 09/17/24 *****************************
           //*** In *dtaara;
           In achbat;
           //*** End   Chg ***** 09/17/24 *****************************
           achbatds = *blanks;
           achbatds = achbat;
           If xxjast <> ' ' and isError = *off;
              jusr = xxjusr;                    // Set message variable user
              strmsg = *Blanks;
              strmsgid = 'WCR3322';
              isError = *on;
           Endif;

          // Ensure that all records of ACH batch have ACH processing
           // Get Flags value from Workers Comp control file
           Chain 'WC' swc01;
           //*** Begin Chg ***** 10/16/23 *****************************
           // If %found(ssylf) and isError = *off;
           If %found() and isError = *off;
           //*** End   Chg ***** 10/16/23 *****************************
              if WCfl24 <> 'Y';
              //*** Begin Chg ***** 10/23/23 *****************************
              // Setll *loval wdeal9;
              // Read wdeal9;
              Setll eaky999 wdeal9;
              Reade eaky999 wdeal9;
              //*** End   Chg ***** 10/23/23 *****************************
                 Dow not %eof(wdeal9);
                    If L9_EAco# = scCo# and L9_EAbat# = scBat#;
                       outCo# = %editc(L9_EAco#:'X');
                       outGroup = %editc(L9_EAfnd:'X');
                       outEmp# = %editc(L9_EAemp#:'X');
                       outDiv = %editc(L9_EAdiv:'X');
                       outYear = %editc(L9_EAfyr:'X');
                       outEffDt = %char(DTMDYY(L9_EApodt));
                       outMode = '1';
                       inError = ' ';
                       ioData = *Blanks;
                       ChkSROnline(outCo#:outGroup:outEmp#:outDiv:outYear:
                        outEffDt:outMode:inError:ioData);

                       If inError <> '0';
                          strmsg = *Blanks;
                          strmsgid = 'WC30392';
                          isError = *on;
                          Leave;
                       Endif;
                    Endif;
                    //*** Begin Chg ***** 10/23/23 *****************************
                    // Read wdeal9;
                    Reade eaky999 wdeal9;
                    //*** End   Chg ***** 10/23/23 *****************************
                 Enddo;
              Else;
             // Ensure that all records of ACH batch have ACH setup
                 //*** Begin Chg ***** 10/23/23 *****************************
                 // Setll *loval wdeal9;
                 // Read wdeal9;
                 Setll eaky999 wdeal9;
                 Reade eaky999 wdeal9;
                 //*** End   Chg ***** 10/23/23 *****************************
                 Dow not %eof(wdeal9);
                    //*** Begin Chg ***** 10/05/23 *****************************
                    // If L9_EAco# = scCo# and L9_EAbat# = scBat#;
                    If L9_EAco# = scCo# and L9_EAbat# = scBat#
                     and L9_EAemp# <> 0;
                    //*** End   Chg ***** 10/05/23 *****************************
                       chrEfDt = %Editc(L9_EAtrdt:'X');
                       chrEftKey = %Editc(L9_EAemp#:'X') + %Editc(L9_EAdiv:'X');
                       chrCo# = %Editc(L9_EAco#:'X');
                       chrFnd = %Editc(L9_EAfnd:'X');
                       chrFyr = %Editc(L9_EAfyr:'X');
                       If GetEftStatus(chrCo#:chrEftType:chrFnd:chrEftKey:
                        chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr)='0';
                          erEftSetUp = 'Invalid EFT set up for ' +
                           'Group ' +  %trim(%editc(L9_EAfnd:'3'))  + ' ' +
                           'Employer-Div ' + %trim(%editc(L9_EAemp#:'3')) +
                           '-' +  %trim(%editc(L9_EAdiv:'3'));
                          PopOneBtn(erEftSetUp:'Error');
                          isError = *on;
                          return *off;
                       Endif;
                    Endif;
                    //*** Begin Chg ***** 10/23/23 *****************************
                    // Read wdeal9;
                    Reade eaky999 wdeal9;
                    //*** End   Chg ***** 10/23/23 *****************************
                 Enddo;
              Endif;
           Endif;

        Endif;

       // Ensure all records in Cash Dissolve Batch have been dissolved
        If strmode = 'W' and EAcash <> ' ' and isError = *off;
           //*** Begin Chg ***** 10/23/23 *****************************
           // Setll *loval wdeal9;
           // Read wdeal9;
           Setll eaky999 wdeal9;
           Reade eaky999 wdeal9;
           //*** End   Chg ***** 10/23/23 *****************************
           Dow not %eof(wdeal9);
              If L9_EAcash <> 'D';
                 strmsg = *Blanks;
                 strmsgid = 'WC30368';
                 isError = *on;
              Endif;
              //*** Begin Chg ***** 10/23/23 *****************************
              // Read wdeal9;
              Reade eaky999 wdeal9;
              //*** End   Chg ***** 10/23/23 *****************************
           Enddo;
        Endif;

        //*** Beg Del *** 11/06/24 *******************************************
       // Check if Batch ID selected is already in use
        //*** If (strmode = 'C' or strmode = 'D') and isError = *off;
        //***   KeyWXRIL3.L3RInam = 'WDEAP';
        //***   KeyWXRIL3.L3RIco# = EAco#;
        //***   KeyWXRIL3.L3RIbat# = EAbat#;
        //***   Monitor;
        //***      Select;
        //***      When strmode = 'C';
        //***         chain %kds(KeyWXRIL3) PRI03;
        //***         If %found(WXRIL3);
        //***            update PRI03;
        //***         Endif;

        //***      When strmode = 'D';
        //***         setll %kds(KeyWXRIL3) PRI03;
        //***         If %equal(WXRIL3);
        //***            reade %kds(KeyWXRIL3) PRI03;
        //***            Dow not %eof(WXRIL3);
        //***               update PRI03;
        //***               reade %kds(KeyWXRIL3) PRI03;
        //***            Enddo;
        //***         Else;
        //***            KeyWXRIL.RInam = 'WDEAP';
        //***            KeyWXRIL.RIrrn = dbrrn02;
        //***            Chain %kds(KeyWXRIL) PRI01;
        //***         Endif;
        //***         If isError = *off;
        //***            chain %kds(KeyWXRIL3) PRI03;    //lock it
        //***         Endif;
        //***      Endsl;
        //***   On-Error;
        //***      strmsg = *Blanks;
        //***      strmsgid = 'WCR0903';
        //***      isError = *on;
        //***   EndMon;
        //*** Endif;
        //*** End Del *** 11/06/24 *******************************************

       //*** Begin Add ***** 06/24/24 *****************************
      // Not allowed to Delete a deductible type of Batch
       If strmode = 'D' and isError = *off;
          If EAachf = 'D';
             strmsg = *Blanks;
             strmsg = 'Deductible batch records cannot be manually modified, +
             use Deductible Invoice Deletion';
             strmsgid = *Blanks;
             isError = *on;
          Endif;
       Endif;
       //*** End   Add ***** 06/24/24 *****************************








      // Return *on indicator when No error exists.
       strErrMsg = *blanks;
       If isError = *on;
          If strmsgid <> ' ';
             strMsgD = RtvMsgD('WMESGG':strmsgid);
             strErrMsg = 'Error on Batch# ' +  %trim(%editc(scBat#:'3')) +
              ' -- ' + %trim(strMsgD);
             If strmsgid = 'WCR3323';
                strErrMsg = %trim(strErrMsg) + ' ' + %trim(mtxt);
             Endif;
          Endif;
          If strmsg <> ' ';
          strErrMsg = 'Error on Batch# ' +  %trim(%editc(scBat#:'3')) +
           ' -- ' + %trim(strMsg);
          Endif;
          PopOneBtn(strErrMsg:'Error');
          //***  Beg Add  **** 10/09/23 *******************************
          // As sfl grid is not reloaded, keep the row selection (scselect) active
          //***  Beg Chg  **** 12/11/23 *******************************
          // Do not update sfl grid for Edit/Delete
          //*** scnxtchg = *on;
          //*** Update sflARBtch;
          If strmode = 'W' or strmode = 'X' or strmode = 'E' or strmode = 'M';
             scnxtchg = *on;
             Update sflARBtch;
          EndIf;
          //***  End Chg  **** 12/11/23 *******************************
          //***  End Add  **** 10/09/23 *******************************
          //*** Beg Add ***** 12/16/24 ********************************
          ClearPolicyLock(intCo#:infndVar:scBat#);
          //*** End Add ***** 12/16/24 ********************************
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAct     e

      *** Beg Add ***** 02/20/25 *******************************************
       //===================================================================
      // Validate the Action for Batch Detail grid.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAct2    b
     d ValidateAct2    pi              n
     d strmode                        1
     d isError         s               n   inz
     d strmsg          s            100    inz
     d strmsgd         s            500    inz
     d strmsgid        s             10    inz
      /Free

        isError = *off;
        // Not allowed to Change, Delete or Mass Update if either the flag for approved
        // or the flag for transfer is set to "Y"
        If (strmode = 'C' or strmode = 'A') and isError = *off;
           If EAtffl = 'Y' or EAtfap = 'Y';
              strmsg = *Blanks;
              strmsgid = 'WC31534';
              isError = *on;
           Endif;
        Endif;
        // Return *on indicator when No error exists.
        strErrMsg = *blanks;
        If isError = *on;
           If strmsgid <> ' ';
              strMsgD = RtvMsgD('WMESGG':strmsgid);
              strErrMsg = 'Error on Batch# ' +  %trim(%editc(EABAT#:'3')) +
               ' -- ' + %trim(strMsgD);
           Endif;
           PopOneBtn(strErrMsg:'Error');
           return *off;
        Else;
           return *on;
        Endif;
      /End-Free
     p ValidateAct2    e
      *** End Add ***** 02/20/25 *******************************************

      //===================================================================
      // Validate the A/R Batch Master Record.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBat     b
     d ValidateBat     pi              n
     d strmode                        1
     d isError         s               n   inz
     d strUserLvl      S              3
     d strBatchLvl     S              3
     d isRestricted    S               n
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erBatAad = *off;
        msBatAad = *blanks;
        erTrDtad = *off;
        msTrDtad = *blanks;
        erBtypad = *off;
        msBtypad = *blanks;
        erCashad = *off;
        msCashad = *blanks;

       // Batch Amount can't be Zero
      //If scBatAad = *zeros;
      //   erBatAad = *on;
      //   msBatAad = 'WCR0502';
      //   isError = *on;
      //Endif;

       // Validate Transaction Date
        If scTrDtad <> 0;
           erTrDtad = ValidDate(scTrDtad:charMsg);
           If erTrDtad = *on;
              msTrDtad = charMsg;
              isError = *on;
           Endif;
        Endif;

        If strmode = 'A';      //Add Mode
          // Transaction Date required for EFT or LockBox batch types
           If scBtypad = 'A' or scBtypad = 'L';
              If scTrDtad = *zeros;
                 erTrDtad = *on;
                 msTrDtad = 'WCR0503';
                 isError = *on;
              Endif;
           Endif;

          // Transaction Date must be Blank for Blank/Regular batch types
           If scBtypad = ' ' and scTrDtad <> *zeros;
              erTrDtad = *on;
              msTrDtad = 'WC30272';
              isError = *on;
           Endif;

          // Transaction cannot be more than a year in the past
           If scTrDtad <> 0 and erTrDtad = *off;
              If %Diff(SystemDate():cvtToDate(scTrDtad):*Y) >= 1;
                 erTrDtad = *on;
                 msTrDtad = 'WC30361';
                 isError = *on;
              Endif;
           Endif;

           //*** Beg Del **** 01/07/25 *******************************
          // EFT (ACH) Batch can't be Cash Dissolve
           //*** If scBtypad = 'A' and scCashad = 'Y';
           //***    erBtypad = *on;
           //***    msBtypad = 'WC30354';
           //***    erCashad = *on;
           //***    msCashad = 'WC30354';
           //***    isError = *on;
           //*** Endif;
           //*** End Del **** 01/07/25 *******************************

          // Check user Authority for EFT (ACH) Batch
           If scBtypad = 'A';
              // Get ACH batch security level and type
              Chain intCo# pco00;
              If %found(smcol);
                 strBatchLvl = %editc(COachl:'X');
              Endif;
              // Get User security level and type
              Chain q1user pup00;
              If %found(smupl);
                 strUserLvl = %editc(UPlvl:'X');
              Endif;

              // Compare security level and type
              ChkAuthority(UPtype:strUserLvl:COacht:strBatchLvl:'1'
                :isRestricted);
              If isRestricted = *on;
                 erBtypad = *on;
                 msBtypad = 'EFT0003';
                 isError = *on;
              Endif;
           Endif;
        Endif;





      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateBat     e

      //===================================================================
      // Validate AR batch Detail Record
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBatDt   b
     d ValidateBatDt   pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free
       // Initialize all the messageId and error indicators.
       InzBatDtErr();

       If (svcash ='Y' and @@mode1 = 'A') or
             (svcash ='Y' and @@mode1 = 'C');
          sysdt7 = DateTo7(SystemDate);
          if svachf = 'L' and scRenCashE = ' ';
             sysdt7 = svtrdt;
          endif;
          currYear = 0;
          rnewYear = 0;
             // Get current year
          Setgt elky2 pel0l2;
          Readpe emky pel0l2;
          If not %eof(wdell2);
             currYear = ELfyr;
          Endif;
          // Get renewal year
          Setll elky2 pel0l2;
          Reade emky pel0l2;
          If not %eof(wdell2);
             rnewYear = ELfyr;
          Endif;
             //Use the renewal year as the current year if current year doesn't exist.
          If currYear = 0;
             currYear = rnewYear;
          Endif;
             //Use the current year as the renewal year if renewal year doesn't exist.
          If rnewYear = 0;
             rnewYEAR = currYear;
          Endif;
             //Set the year.
          If scRenCashE = 'Y';
             scYearE = rnewYear;
          Else;
             scYearE = currYear;
          Endif;
       Endif;

       // System Type cannot be Zero
       If scSysTypE = *Blanks;
          erSysTypE = *on;
          msSysTypE = 'WCR0503';
          isError = *on;
       Endif;

       // Group cannot be Zero
       If scGroupE = *zeros;
          erGroupE = *on;
          msGroupE = 'WCR0502';
          isError = *on;
       Endif;

       // Employer Nummber cannot be Zero
       If scEmpNoE = *zeros;
          erEmpNoE = *on;
          msEmpNoE = 'WCR0502';
          isError = *on;
       Endif;

       // Year cannot be Zero
       If scYearE = *zeros and visCashDis = *on;
          erYearE = *on;
          msYearE = 'WCR0502';
          isError = *on;
       Endif;

       // check policy year valid for group
       If erYearE = *off and erGroupE = *off and scYearE <> *zero;
           Chain (##co#:scGroupE:scYearE) wdf1l;
           If not %Found(wdf1l);
              erYearE = *on;
              msYearE = 'WCR0517';
              isError = *on;
           EndIf;
       Endif;

       // Validate Employer Number
       If (scEmpNoE <> *zero or scYearE <> *zero) and (erEmpNoE = *off
          and erYearE = *off);
           Chain (##co#:scGroupE:scEmpNoE:scDivE) wmeml;
           If not %Found(wmeml);
              erEmpNoE = *on;
              msEmpNoE = 'WCR0538';
              erDivE = *on;
              msDivE = 'WCR0538';
              erYearE = *on;
              msYearE = 'WCR0538';
              isError = *on;
           EndIf;
       Endif;

       // Check for Restricted Group/Employer
       If erGroupE = *off and erEmpNoE = *off;
          if multig = *on;
             $$co#  = ##co#;
             $$usr  = q1user;
             $$appl = 'W';
             $$rtcd = *off;
             $$auth = ' ';
             $$fnd  = scGroupE;
             $$emp# = scEmpNoE;

             SA008R($$co#:$$usr:$$appl:$$rtcd:$$auth:$$fnd:$$emp#);
             failed = $$rtcd;
             if failed = *on or $$auth = 'I';
                erGroupE2 = *on;
                erEmpNoE2 = *on;
                erDivE2 = *on;
                isError = *on;
             endif;
          else;
          if (UPxfnd = 'Y' and UPfnd <> scGroupE)
           or (UPxemp = 'Y' and UPfnd <> scGroupE)
           or (UPxemp = 'Y' and UPemp# <> scEmpNoE)
           or (UPxemp = 'Y' and UPdiv <> scDivE);
             erGroupE2 = *on;
             erEmpNoE2 = *on;
             erDivE2 = *on;
             isError = *on;
          endif;
          endif;
       endif;

       // check for 'Employer does not exist for policy year'
       If erEmpNoE = *off and erEmpNoE2 = *off and
          erDivE = *off and erDivE2 = *off and erYearE = *off;
           Chain (##co#:scGroupE:scEmpNoE:scDivE:scYearE) pel02;
           If not %Found(wdell);
              erEmpNoE = *on;
              msEmpNoE = 'WCR0694';
              erDivE = *on;
              msDivE = 'WCR0694';
              erYearE = *on;
              msYearE = 'WCR0694';
              isError = *on;
           EndIf;
       Endif;

       // check for EFT setup
       //***  Beg Chg  **** 09/11/23 *******************************
       //*** If svachf = 'A' And @@Mode1= 'A' and erDivE = *off and erDivE2 = *off;
       If svachf = 'A' And @@Mode1= 'A' and erDivE = *off and erDivE2 = *off
          and erGroupE = *off and erEmpNoE = *off and erYearE =*off;
       //***  End Chg  **** 09/11/23 *******************************
          chrEfDt = %Editc(svtrdt:'X');
          chrEftKey = %Editc(scEmpNoE:'X') + %Editc(scDivE:'X');
          chrCo# = %Editc(##co#:'X');
          chrFnd = %Editc(scGroupE:'X');
          chrFyr = %Editc(scYearE:'X');
          If GetEftStatus(chrCo#:chrEftType:chrFnd:chrEftKey:
           chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr) = *off;
             erDivE3 = *on;
             msDivE3 = 'EFT setup is not valid for this ' +
              'employer/division.';
             isError = *on;
          Endif;
       Endif;

       // Do not allow same check number and check amount to be dissolved twice to the same policy.
       if svCash = 'Y';
          setll eaky9 pea09;
          reade eaky9 pea09;
          dow not %eof(wdeal9);
             relRcd# = dbrrn;
             if relRcd# <> scRrnBtch and
              l9_EAfnd  = scGroupE  and
              l9_EAemp# = scEmpNoE  and
              l9_EAdiv  = scDivE    and
              l9_EAchk# = scChkNoE  and
              l9_EAckam = ScChkAmtE and erCHKNoE = *off;
                erCHKNoE = *on;
                msCHKNoE = 'WC30383';
                isError = *on;
                leave;
             endif;
          reade eaky9 pea09;
          enddo;
       endif;

       // Check for error 'This transaction isn't eligible for cash dissolve.'
       if svCash = 'Y';
          chain fnky pf201;
          kkhier = 10000;
          setll cdky pcd00;
          readpe fnky pcd00;
          if %eof(wdcdl);
             cashDissolveFl = *off;
          else;
             cashDissolveFl = *on;
          endif;
          chain fnky pc300;
          if not %found(wtc3l)
           or C3cncd = *blank or C3cpcd = *blank
           or C3spcd = *blank or C3apcd = *blank;
             cashDissolveFl  = *off;
          endif;

          If (F2ccyf = *blank and F2rcyf = *blank and F2acyf = *blank
           and F2lcyf = *blank) or cashDissolveFl = *off;
             If erGroupE = *off and erGroupE2 = *off
              and erEmpNoE = *off and erEmpNoE2 = *off
              and erDivE = *off and erDivE2 = *off and erDivE3 = *off;
                erGroupE = *on;
                msGroupE = 'WC30355';
                erEmpNoE = *on;
                msEmpNoE = 'WC30355';
                erDivE = *on;
                msDivE = 'WC30355';
                isError = *on;
             endif;
          endif;
       endif;

       // AR code can't be blank
       If scARCodeE = *Blanks and visCashDis = *on;
          erARCodeE = *on;
          msARCodeE = 'WCR0503';
          isError = *on;
       Endif;

       // Validation Check for Posting date
       If visCashDis = *on;
          erPostDtE = ValidDate(scPostDtE:charMsg);
          If erPostDtE = *on;
             msPostDtE = charMsg;
             isError = *on;
          Endif;
       Endif;

       // Posting Amount can't be zero
       If scPostAmtE = *zeros and visCashDis = *on;
          erPostAmtE = *on;
          msPostAmtE = 'WCR0556';
          isError = *on;
       Endif;

       // check for xxbal

       // Position cursor to check # if screen values change. If payment type is
       // a refund, check if amount will cause a positive balance in A/R and
       // display message.

       // Redisplay for Employer Information when emp info changes and
       //  position cursor at check number field
       If isError = *off and
        (scEmpNoE <> svemp# or
         scDivE <> svdiv  or
         scYearE <> svfyr);
          EmpInf();
          svemp# = scEmpNoE;
          svdiv  = scDivE;
          svfyr  = scYearE;
       Endif;
       // Inform user that the refund is too large and that it will
       //  result in a net positive balance (excluded from cash dissolve)
       Select;
       When isError;
       When svCash = 'Y';
       When scPostAmtE <> svamt2;
          svamt2 = scPostAmtE;
          kkkey  = scARCodeE;
          Chain tbky ptt01;
          Select;
          When not %found();
          When TTstat = 'I';
          When TTtype = 'R';
             EmpInf();           // recalculate balance
             If @@mode1 = 'C';
                xxBal += (scPostAmtE - svamt3);
             Endif;
             If (xxBal + scPostAmtE) > 0 and erPostAmtE = *off;
                erPostAmtE = *on;
                msPostAmtE = 'WC30193';
                isError = *on;
             Endif;
          Endsl;
       Endsl;

       // check greater than all 9
       If scChkNoE >= 9999999 and erChkNoE = *off;
          erChkNoE = *on;
          msChkNoE = 'WC31442';
          isError = *on;
       Endif;

       If ScChkAmtE >= 9999999.99 and erChkAmtE = *off;
          erChkAmtE = *on;
          msChkAmtE = 'WC31442';
          isError = *on;
       Endif;

       // Do not allow post amount of zero (if not cash dissolve or LUBA)
       Select;
       When isError;
       When svCash = 'Y';
       When clientID = '133' and @@Mode1 = 'A' and scPostAmtE = 0;
          scPostAmtE = ScChkAmtE;
       When scPostAmtE = 0 and erPostAmtE = *off;
          erPostAmt2 = *on;
          isError= *on;
       Endsl;

       // Do not allow a check amount of zero for cash dissolve
       Select;
       When isError;
       When svCash <> 'Y';
       When ScChkAmtE = 0 and erChkAmtE = *off;
          erChkAmtE = *on;
          msChkAmtE ='WC30367';
          isError = *on;
       Endsl;

       // Check number must be enter when a deductible payment is entered
       //*** Begin Chg ***** 02/11/25 *****************************
       // Do not check following validation for cash dissolve batch when
       // A/R Transaction Code field is hidden
       //*** If iserror = *off;
       If iserror = *off and scARCodeE <> *Blanks and visCashDis = *on;
          kkkey = scARCodeE;
          Chain tbky ptt01;
       //*** End   Chg ***** 02/11/25 *****************************

          If TTctyp = 'R' and TTtype ='Q' and scChkNoE = *zeros
           and erChkNoE = *off;
             erChkNoE = *on;
             msChkNoE = 'WCR1169';
             isError = *on;
          Endif;

          If ClientID ='129';
          // Check user security level/type for posting A/R code
             #2utyp = UPtype;
             #2ulvl = %editc(UPlvl:'X');
             #2clvl = %char(TTmins);
             #2ctyp = TTty01;
             #2exec = '1';
             #2res  = *off;
             #2ctp2 = TTty02;
             #2ctp3 = TTty03;
             #2ctp4 = TTty04;
             #2ctp5 = TTty05;
             #2ctp6 = TTty06;
             SRCCSC(#2utyp:#2ulvl:#2ctyp:#2clvl:#2exec:
                    #2res:#2ctp2:#2ctp3:#2ctp4:#2ctp5:#2ctp6);
             If #2RES = *on and erARCodeE = *off;
                erARCodeE = *on;
                msARCodeE = 'WC31458';
                isError = *on;
             Endif;
          Endif;
       Endif;

       // check policy current account
       If @@plan = 'A' and ckplan = *on;
          If (TTtype = 'P' or TTtype = 'I' or TTtype = 'Q')
           and (erEmpNoE = *off and erEmpNoE2 = *off and erDivE = *off
           and erDivE2 = *off and erDivE3 = *off and erYearE = *off);
             ckplan = *off;
             erEmpNoE = *on;
             msEmpNoE = 'WC30225';
             erDivE = *on;
             msDivE = 'WC30225';
             erYearE = *on;
             msYearE= 'WC30225';
             isError = *on;
          Endif;
       Endif;

       // Check error when SYtype is Y
       If SYtype = 'Y' and cashDisolve = *off;
       //  no cash dissolve
          xxDt = DateTo7(SystemDate);
          xxPoDt = DtCYMD(scPostDtE);
          //***  Beg Del  **** 10/20/23 *******************************
          // billTrans = *off;
          // If TTtype <> 'P' and TTtype <> 'I' and TTtype <> 'Q' and
          //  TTtype <> 'R' and TTtype <> 'N' and TTtype <> 'T';
          //    billTrans = *on;
          // endif;

          // check if Posting date less than current Month
          // if billTrans = *on and scARCodeE <> F1srbb and erPostDtE = *off;
          //    if xxpodt < xxdt;
          //       pocym = %int(%Subst(%char(xxpodt):1:5));
          //       dtcym = %int(%Subst(%char(xxdt):1:5));
          //       if pocym <> dtcym;
          //          erPostDtE = *on;
          //          msPostDtE = 'WCR0924';
          //          isError = *on;
          //       endif;
          //    endif;
          // endif;
          //***  End Del  **** 10/20/23 *******************************

          // Employer must have assigned Agent effective for the fund
          // year effective as of the Posting Date
          //***  Beg Chg  **** 09/11/23 *******************************
          //***  If erPostDtE = *off and erPostDtE2 = *off;
          If erPostDtE = *off and erPostDtE2 = *off and erYearE = *off;
          //***  End Chg  **** 09/11/23 *******************************
             if svtrdt <> 0;
                kktrdt = svtrdt;
             else;
                kktrdt = xxdt;
             endif;

             setgt ahky1 pah01;
             readpe ahky pah01;
             dow not %eof(wmahl) and AHstat = 'I';
                readpe ahky  pah01;
             enddo;
             if %eof(wmahl);
                if svtrdt <> 0;
                   erPostDtE2 = *on;
                   msPostDtE2 = 'Agent not effective as of ' +
                    'the transaction date';
                   isError = *on;
                else;
                   erPostDtE = *on;
                   msPostDtE = 'WCR0637';
                   isError = *on;
                endif;
             endif;
          endif;
       endif;

       // Check error for transaction date for Retro Batches
       If visBtchHdr = *on;
       // should be a valid date
          erTrnDateE = ValidDate(scTrnDateE:charMsg);
          If erTrnDateE = *on;
              msTrnDateE = charMsg;
              isError = *on;
          endif;
          // should not be entered for prior month
          xxDt   = DateTo7(SystemDate);
          xxRbDt = DtCYMD(scTrnDateE);
          rbcym = xxRbDt;
          dtcym = xxdt;
          if rbcym < dtcym and erTrnDateE = *off;
             erTrnDateE = *on;
             msTrnDateE = 'WC30388';
             isError = *on;
          endif;
       // only retro payments can be added
          if scARCodeE <> f1rccd and cashDisolve = *off and erARCodeE = *off;
             erARCodeE = *on;
             msARCodeE = 'WCR3372';
             isError = *on;
          endif;
       endif;

       if EMeacd = 'Y' and svcash = 'Y';
          PopOneBtn('The employer entered for this A/R Batch Detail +
                     record is flagged to exclude from Auto-cash-dissolve. +
                     Please go back to the detail entry screen to proceed +
                     with other check entries. +
                     for Auto Cash Dissolve.':'Error');
          isError = *on;
       endif;


       // return error code
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateBatDt   e

      *** Begin Add ***** 07/26/24 *****************************
      //===================================================================
      // Validate Filter criteria for AR Batch Master Grid
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFlt     b
     d ValidateFlt     pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msFDCrt = *blanks;
       erFDCrt = *off;
       msTDCrt = *blanks;
       erTDCrt = *off;
       msFBat# = *blanks;
       erFBat# = *off;
       msTBat# = *blanks;
       erTBat# = *off;

      // Validate Date Created Range
       If scFDCrt <> *zeros;
          erFDCrt= ValidDate(scFDCrt:charMsg);
          if erFDCrt = *on;
             msFDCrt = charMsg;
             isError = *on;
          Endif;
       Endif;
       If scTDCrt <> *zeros;
          erTDCrt= ValidDate(scTDCrt:charMsg);
          if erTDCrt = *on;
             msTDCrt = charMsg;
             isError = *on;
          Endif;
       Endif;
      // Dates must be in Ascending order
       If scFDCrt <> 0 or scTDCrt <> 0;
          If erTDCrt = *off and erFDCrt = *off;
             erTDCrt = AscendDates(scFDCrt:scTDCrt:charMsg);
             If erTDCrt = *on;
                erFDCrt = *on;
                msFDCrt = charMsg;
                msTDCrt = charMsg;
                isError = *on;
             Endif;
          Endif;
       Endif;

      // Validate Batch# Range
       If scFBat# > scTBat#;
          erFBat# = *on;
          msFBat# = 'WCR0750';
          erTBat# = *on;
          msTBat# = 'WCR0750';
          isError = *on;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateFlt     e
      *** End   Add ***** 07/26/24 *****************************

      *** Begin Add ***** 08/01/24 *****************************
      //===================================================================
      // Validate Filter criteria for AR Batch Details Grid
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFlt2    b
     d ValidateFlt2    pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msFGrp# = *blanks;
       erFGrp# = *off;
       msTGrp# = *blanks;
       erTGrp# = *off;
       msFPoDt = *blanks;
       erFPoDt = *off;
       msTPoDt = *blanks;
       erTPoDt = *off;

      // Validate Group# Range
       If scFGrp# > scTGrp#;
          erFGrp# = *on;
          msFGrp# = 'WCR0750';
          erTGrp# = *on;
          msTGrp# = 'WCR0750';
          isError = *on;
       Endif;

      // Validate Posting Date Range
       If scFPoDt <> *zeros;
          erFPoDt= ValidDate(scFPoDt:charMsg);
          if erFPoDt = *on;
             msFPoDt = charMsg;
             isError = *on;
          Endif;
       Endif;
       If scTPoDt <> *zeros;
          erTPoDt= ValidDate(scTPoDt:charMsg);
          if erTPoDt = *on;
             msTPoDt = charMsg;
             isError = *on;
          Endif;
       Endif;
      // Dates must be in Ascending order
       If scFPoDt <> 0 or scTPoDt <> 0;
          If erTPoDt = *off and erFPoDt = *off;
             erTPoDt = AscendDates(scFPoDt:scTPoDt:charMsg);
             If erTPoDt = *on;
                erFPoDt = *on;
                msFPoDt = charMsg;
                msTPoDt = charMsg;
                isError = *on;
             Endif;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateFlt2    e
      *** End   Add ***** 08/01/24 *****************************

      //===================================================================
      // Validate A/R Batch Inquiry
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateInq     b
     d ValidateInq     pi              n
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       erGroupI = *off;
       msGroupI = *blanks;
       erEmpNoI = *off;
       msEmpNoI = *blanks;
       erDivI = *off;
       msDivI = *blanks;
       erYearI = *off;
       msYearI = *blanks;

      // Group cannot be Zero
       If scGroupI = *zeros;
          erGroupI = *on;
          msGroupI = 'WCR0502';
          isError = *on;
       Endif;

      // Employer Nummber cannot be Zero
       If scEmpNoI = *zeros;
          erEmpNoI = *on;
          msEmpNoI = 'WCR0502';
          isError = *on;
       Endif;

      // Policy Year cannot be Zero
       If scYearI = *zeros;
          erYearI = *on;
          msYearI = 'WCR0502';
          isError = *on;
       Endif;

      // Check policy year valid for group
       If erYearI = *off and erGroupI = *off and scYearI <> *zero;
           Chain (##co#:scGroupI:scYearI) wdf1l;
           If not %Found(wdf1l);
              erYearI = *on;
              msYearI = 'WCR0517';
              isError = *on;
           EndIf;
       Endif;

      // Validate Employer Number
       If (scEmpNoI <> *zero or scYearI <> *zero) and (erEmpNoI = *off
          and erYearI = *off);
           Chain (##co#:scGroupI:scEmpNoI:scDivI) wmeml;
           If not %Found(wmeml);
              erEmpNoI = *on;
              msEmpNoI = 'WCR0538';
              erDivI = *on;
              msDivI = 'WCR0538';
              erYearI = *on;
              msYearI = 'WCR0538';
              isError = *on;
           EndIf;
       Endif;


      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateInq     e

       //*** Beg Add *** 11/06/24 *******************************************
       //*** Beg Del *** 12/16/24 *******************************************
      //===================================================================
      // Validate Batch lock.
      // Returns *on  ----> Any errors
      // Returns *off ----> No errors
      //===================================================================
     p*ValidateLck     b
     d*ValidateLck     pi              n
      /Free

       //***KeyWXRIL3.L3RInam = 'WDEAP';
       //***KeyWXRIL3.L3RIco# = EAco#;
       //***KeyWXRIL3.L3RIbat# = EAbat#;

       //***Monitor;
       //***   Chain %kds(KeyWXRIL3) PRI03;    //Lock it
       //***   If not %found(WXRIL3);
       //***      Clear PRI03;
       //***      L3RIstat = 'A';
       //***      L3RInam = 'WDEAP';
       //***      L3RIrrn = dbrrn02;
       //***      L3RIco# = EAco#;
       //***      L3RIbat# = EAbat#;
       //***      L3RIjobn = Q1jobn;
       //***      L3RIJOB =  Q1job;
       //***      L3RIdate = Q1dt;
       //***      L3RITIME = Q1time;
       //***      L3RIipgm = Q1pgm;
       //***      L3RIpgm  = Q1pgm;
       //***      L3RIuser = Q1user;
       //***      write PRI03;
       //***      Chain %kds(KeyWXRIL3) PRI03; //Lock it
       //***   Endif;
       //***   return *off;
       //***On-Error;
       //***   return *on;
       //***EndMon;

      /End-Free
     p*ValidateLck     e
       //*** End Del *** 12/16/24 *******************************************
       //*** End Add *** 11/06/24 *******************************************

      //===================================================================
      // Validate Mass Batch Update Window
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateMas     b
     d ValidateMas     pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erNchk = *off;
        msNchk = *blanks;
        erPoDt = *off;
        msPoDt = *blanks;

       // Check Number can't be Zero
        If scNchk = *zeros;
           erNchk = *on;
           msNchk = 'WCR0502';
           isError = *on;
        Endif;

       // Validate Posting Date
        erPoDt = ValidDate(scPoDt:charMsg);
        If erPoDt = *on;
           msPoDt = charMsg;
           isError = *on;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateMas     e

      //===================================================================
      // Validate Purge Batch
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidatePur     b
     d ValidatePur     pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erPgDt = *off;
        msPgDt = *blanks;
        erPgDt2 = *off;
        mgPgDt2 = *blanks;

       // Validate Purge Date
        erPgDt = ValidDate(scPgDt:charMsg);
        If erPgDt = *on;
           msPgDt = charMsg;
           isError = *on;
        Endif;

       // Check for Duplicate Purge Date
        If erPgDt = *off;
           Chain(N) (##Co#:dtCYMD(scPgdt)) pbp00;
           If %found(wmbpl);
              erPgDt2 = *on;
              mgPgDt2 = 'Record already exists for this purge date.';
              isError = *on;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidatePur     e
     p*** Begin Add *** 08/27/24 *******************************************
      *=====================================================================
      * CheckPaymentReversal - Check to see if an invoice payment was
      *                        reversed.
      *=====================================================================
     p CheckPaymentReversal...
     p                 b
     d CheckPaymentReversal...
     d                 pi              n
     d  inCompany#                         like(EAco#)   const
     d  inGroup#                           like(EAfnd)   const
     d  inEmployer#                        like(EAemp#)  const
     d  inDivision                         like(EAdiv)   const
      *** Beg Chg *** 03/18/25 *************************************
     d*****  inPostDate                         like(EAPoDt)  const
     d  inPostDate                    8  0 const
      *** End Chg *** 03/18/25 *************************************
     d  inInvoice#                         like(EAinv#)  const

     d  reversedPayment...
     d                 s               n   Inz(*Off)
     d  arType         s              1    Inz

        // Initialize Return Variable
        reversedPayment = *Off;

        // Is this an IVC Group
        isICgroup = IcGroupOpt(inCompany#:inGroup#);

        // Invoice Cloud must enabled and the group must be an IVC group
        If isInvoiceCloud And isICgroup;

           // Get the A/R Type
           arType = GetARType(EAarCd);

           // Check for a reversal
           If ((arType = 'P' or arType = 'I' or arType = 'Q' or
                arType = 'T') And EAamt < 0) Or
              ((arType = 'N' or arType = 'R') And EAamt > 0);

              // Now check to see if there was a paid invoice
              If MatchPaidInvoice(inCompany#:inGroup#:inEmployer#:
                    inDivision:inInvoice#);
                 reversedPayment = *On;
              EndIf;
           EndIf;

        EndIf;

        Return reversedPayment;

     p CheckPaymentReversal...
     p                 e

      *=====================================================================
      * MatchPaidInvoice - Look for a paid invoice record that matches
      *                    the reversal.
      *=====================================================================
     p MatchPaidInvoice...
     p                 b
     d MatchPaidInvoice...
     d                 pi              n
     d  inCompany#                         like(EAco#)   const
     d  inGroup#                           like(EAfnd)   const
     d  inEmployer#                        like(EAemp#)  const
     d  inDivision                         like(EAdiv)   const
     d  inInvoice#                         like(EAinv#)  const

     d  paymentMatched...
     d                 s               n   Inz(*Off)
     d  decCount       s              5  0 Inz(0)

        // Initialize Return Variable
        paymentMatched = *Off;

        // Look to see if a hold record exists.
        Exec Sql Select count(*)
                 Into :decCount
                 From wdeap_hld
                 Where EAco#=:inCompany# And EAfnd=:inGroup# And
                       EAemp#=:inEmployer# And EAdiv=:inDivision And
                       EAinv#=:inInvoice#;

        // If hold record doesn't exist then it may have been paid
        If decCount = 0;
           // Let's look for the payment for that invoice
           // In the A/R batch file
           Exec Sql Select count(*)
                    Into :decCount
                    From wdeap join wtttp on EAco#=TTco# And
                                             EAarCd=TTkey
                    Where EAco#=:inCompany# And EAfnd=:inGroup# And
                          EAemp#=:inEmployer# And EAdiv=:inDivision And
                          EAinv#=:inInvoice# And
                          TTtype in ('P', 'I', 'Q', 'T') And
                          EAamt>0;

           // Payment Found for the Invoice if the count <> 0
           If decCount <> 0;
              paymentMatched = *On;
           EndIf;
        EndIf;

        Return paymentMatched;

     p MatchPaidInvoice...
     p                 e
     p*** End   Add *** 08/27/24 *******************************************

      *** Beg Add ***** 12/16/24 ****************************
      //===================================================================
      // Check Lock
      // Returns *on  ----> Lock found
      // Returns *off ----> No Lock
      //===================================================================
     p CheckLock       b
     d CheckLock       pi              n
     d inco#                          3  0
     d infnd                          3  0
     d inbat#                         9  0
     d intype                        10    value

     d strJobInfo      s             28
     d strProcess      s             10
     d strProcess25    s             25
     d strUser         s             50
     d xxMsg           s            256    inz
     d xxTtl           s             50    inz
     d xxRsp           s               n   inz

      /Free
       If GetPolicyLockStatus(intCo#:infndVar:inbat#:strJobInfo:
        strProcess:strUser) = *On;
          Select;
          When strProcess = 'ARB_EDIT';
             strProcess25 = 'A/R Batch Edit';
          When strProcess = 'ARB_DELETE';
             strProcess25 = 'A/R Batch Delete';
          When strProcess = 'ARB_DTLADD';
             strProcess25 = 'A/R Batch Add Detail';
          When strProcess = 'ARB_APPRVL';
             strProcess25 = 'A/R Batch Approval';
          When strProcess = 'ARB_XFER';
             strProcess25 = 'A/R Batch Transfer';
          When strProcess = 'ARB_CRTEFT';
             strProcess25 = 'A/R Batch Create EFT File';
          When strProcess = 'ARB_EDTRPT';
             strProcess25 = 'A/R Batch Edit Report';
          Endsl;
          xxMsg = 'This operation is available when ' + %Trim(strProcess25) +
           ' Job(' + %Trim(strJobInfo) + ') has completed for ' +
           %Trim(strUser) + '.';
          xxTtl = '*** Batch Locked! ***';
          xxRsp = PopOneBtn(xxMsg:xxTtl);
          Return *On;
       Else;
          SetPolicyLock(intCo#:infndVar:inbat#:inType);
          Return *Off;
       Endif;

      /End-Free
     p CheckLock       e
      *** End Add ***** 12/16/24 ****************************

      // utility procedure definitions
      /COPY SCOPYCOMPI

