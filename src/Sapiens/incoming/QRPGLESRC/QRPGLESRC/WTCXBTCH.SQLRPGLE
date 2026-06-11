     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTCXBTCH - Work with Cancellation Batches
     h dftname(WTCXBTCH)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 05/15/23                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2906 - Redesign of Work with Cancellation Batches
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/20/25 C:300 Support: 119605                               *
      //                 Deletion of Single Div from Cancellation Batch*
      //                 Details Grid -- Not Removing Entire Employer (all divisions)
      //                 records from WXCNP workfile (like old screen) *
      //                                                               *
      //  08/14/25 C:300 Support: 119564                               *
      //                 Correct Edit on Estimated Issue Date          *
      //                                                               *
      //  07/28/25 C:062 Project: 153WBJ                               *
      //                 Add late fee processing.                      *
      //                                                               *
      //  06/04/25 C:342 Support: 119061                               *
      //                 Changes to load added columns in DSPF         *
      //                 DSPF Changes:                                 *
      //                 - Added Ann Pay,EFT,Credit Adj,VIP,#Cx column *
      //                 - Removed CX Hist column and                  *
      //                 - Added CX Hist column response to # of CX    *
      //                 - Remove A/R inquiry link and assigne its     *
      //                   functionality of "Amount Due" column.       *
      //                 - Added Finance column                        *
      //                 - Changed name of ACH flag to EFT             *
      //                 - Changed name of Negative Adjustment to      *
      //                   Credit Adjustment.                          *
      //                                                               *
      //  05/13/25 C:333 Support: 118669 (FCC)                         *
      //                 Return Funds Screen Request.                  *
      //                 DSPF Changes:                                 *
      //                 - Renamed rcdPanel to rcdPanelCx.             *
      //                 - Added visibility for TabCXBatch.            *
      //                 - Changed overlay format for rcdCXBtch.       *
      //                 - Added visibility for HdgCxBatch.            *
      //                                                               *
      //  07/25/24 C:300 Project: WC2906, Support: 117053 (SUNZ)       *
      //                 Delete all records from Cancellation Batch Details
      //                 grid when we delete a CX batch from main grid *
      //                                                               *
      //  05/31/24 C:291 Support: PHW115694                            *
      //                 Fixed the error with "Issue" Link. It was     *
      //                 keep spinning when clicking on "Issue" link   *
      //                 to issue cancellation.                        *
      //                 FIxed Batch# passed to W0163C.                *
      //                                                               *
      //  04/08/24 C:300 Project: WC2906, Support: 115309              *
      //                 Cancellation Batch Details screen NOT showing *
      //                 any infomration on clicking Delete link       *
      //                 DSPF: Move all fields into a container in     *
      //                 rcdBatEdit record format                      *
      //                                                               *
      //  04/02/24 C:300 Project: WC2906, Support: 115310              *
      //                 Cancellation Batch Details grid NOT showing   *
      //                 link when Premium Finance Compnay exists on   *
      //                 the Employer                                  *
      //                                                               *
      //  04/02/24 C:300 Project: WC2906, Support: 115309              *
      //                 Show batch# in the Confirmation message while *
      //                 deleting Cancellation Batch Details record    *
      //                                                               *
      //  01/17/24 C:300 Project: WC2906                               *
      //                 Fix issue with screen reload on Refresh       *
      //                                                               *
      //  01/12/24 C:300 Project: WC2906                               *
      //                 Fix Date Comparison issues                    *
      //                                                               *
      //  12/18/23 C:300 Project: WC2906                               *
      //                 DSPF: Remove 'Work With' from Page Title      *
      //                 Heading and Tab Name                          *
      //                                                               *
      //  11/08/23 C:300 Project: WC2906                               *
      //                 DSPF: Allow Amount Due field to be Editable on*
      //                 Edit 'Cancellation Batch Details' window      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTCXBTCH  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflCXBtch:scSflRRN)
     f                                     Sfile(sflCXBtchD:scSflRRND)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Cancellation Batch File
     fwxccl     if a e           k disk    rename(pcc01:pcc09)
     f                                     infds(dbfds)
     fwxccp     uf a e             disk    rename(pcc01:pcc0p)
     f                                     infds(dbfds2)
      * Cancellation Batch Detail file
     fwxcnl3    if a e           k disk    rename(pcn01:pcn03)
     f                                     infds(dbfds3)
     fwxcnp     uf a e             disk    rename(pcn01:pcn0p)
     f                                     infds(dbfds4)
      ***  Beg Chg  **** 07/25/24 *******************************
     f***** wxcnl4    if   e           k disk    rename(pcn01:pcn04)
     fwxcnl4    uf   e           k disk    rename(pcn01:pcn04)
      ***  End Chg  **** 07/25/24 *******************************
      * Cancel Reason file
     fwtcrl     if   e           k disk    rename(pcr01:pcr00)
      * Rate Set Code Master file
     fwmrsl     if   e           k disk    rename(prs01:prs00)
      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds5)
      * Premium Finance Co.
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
     f                                     infds(dbfds6)
      * Fund Master file
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     infds(dbfds7)
      * Employer Detail file
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds8)
      * Company file
     fsmcol     uf   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
      * Group Aux File
     fwdf2l     if   e           k disk
      * State Code File
     fststl5    if   e           k disk    usropn
      * Admin/Darts Control File
     fwmexl     if   e           k disk    rename(pex01:pex00)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     d dbfds6          ds
     d  dbrrn6               397    400b 0
     d dbfds7          ds
     d  dbrrn7               397    400b 0
     d dbfds8          ds
     d  dbrrn8               397    400b 0

      // work variables
     d saveDT          s                   like(CCdt)
     d saveTM          s                   like(CCtime)
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
     d IntPos          s              5  0
     D ieSTcodes       s            200A   inz
     d*
     d svsflrrn        s              5  0 inz
     d inInclude       s             10
     d CXDtlRrn        s              9  0
     d svsflrrnd       s              5  0 inz
     d svrrnCXD        s              9  0 inz
     d kkRtcd          s                   like(ELrtcd)
     d kkPfdt          s                   like(ELpfdt)
     d kkjrst          s                   like(RSjrst)

      // calling parameter for W0050R
     d ##kc            s              1
     d ##kl            s              1
     d ##mode          s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     d ##qte           s              1
     d ##view          s              1  0
     d ##rrn3          s              9  0

      // calling parameter for WtPolARInq
     d #1rrn           s              9
     d #1mode          s              1
     d #1next          s             10
     d #1tabind        s              5
     d #1Qte           s              1
     d*#1PostDt        s              8  0
     d*#1PostAmt       s             11  2

      // calling parameter for WtPolCXRn
     d #2rrn           s              9
     d #2mode          s              1
     d #2next          s             10
     d #2tabind        s              5
     d #2Qte           s              1

      // parameter for W0163c
     d qqco#           s              3
     d qqfnd           s              3
     d qqdate          s              8
     d qqcdat          s              8
     d qqbat#          s              9
     d qqccrt          s              1
     d qqtype          s              1
     d qqform          s              1
     d qqoutq          s             10
     d qqmode          s              1
     d qqsrt1          s              2
     d qqsrt2          s              2

     d outco#          s              3    inz                                  Company
     d outfnd          s              3    inz                                  Fund
     d outfrdt         s              8    inz                                  Policy From Date
     d outtodt         s              8    inz                                  Policy To Date
     d outbldt         s              8    inz                                  Billing Thru Date
     d outpydt         s              8    inz                                  Payment Thru Date
     d outetyp         s              1    inz                                  Type of account
     d outmin          s              7    inz                                  Minimum Balance
     d outrpmy         s              6    inz                                  Reporting Month/Year
     d outday          s              2    inz                                  Excl New acc fr Day
     d outbat#         s              9    inz                                  Batch#
     d outrsn          s              2    inz                                  Cancel Resaon Code
     d outcom1         s             60    inz                                  Comment 1
     d outcom2         s             60    inz                                  Comment 2
     d outoutq         s             10    inz                                  OutQ
     d outmode         s              1    inz                                  Mode
     d outrndt         s              8    inz                                  Run Date
     d outisdt         s              8    inz                                  Estimated Issue Date
     d outeafl         s              1    inz                                  Estimate/Audit Flag
     d outwedt         s              8    inz                                  Week Ending Date
     d outxadt         s              8    inz                                  Excl New Acc Date
     d outfreq         s              1    inz                                  Reporting Frequency
     d outsrpy         s              1    inz                                  Incl Cur acc pr year
     d outindp         s              1    inz                                  Incl deposit in Calc
     d outabil         s              1    inz                                  Incl Agency Bill Emp
     d outstieflg      s              1    inz                                  States Inc/Excl
     d outstcodes      s            150    inz                                  States
      ***  Beg Add  **** 08/20/25 *******************************
     d q1co#           s                   like(CNco#)
     d q1fnd           s                   like(CNfnd)
     d q1bat#          s                   like(CNbat#)
     d q1emp#          s                   like(CNemp#)
      ***  End Add  **** 08/20/25 *******************************


      // save details of 1st record
     d co#_sav         s                   like(CCco#)
     d fund_sav        s                   like(CCfnd)
     d bat#_sav        s                   like(CCbat#)
     d reload          s               n   inz(*off)
     d ##key           s              2    inz
     d OutCltID        s              3    inz
     d*** Begin Add *** 07/28/25 *******************************************
     d hld153          s                   Like(is153)
     d*** End   Add *** 07/28/25 *******************************************

     d isUseScript     s              1n   inz
     d processType     c                   const('Cancellations')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d
     d days_diff       s              3  0 inz
     d isWarning       s               n   inz
     d date1           s               d   datfmt(*usa)
     d date2           s               d   datfmt(*usa)

     d                 DS
     d  xxdate                 1      8  0
     d  XXdtmm                 1      2  0
     d  XXdtdd                 3      4  0
     d  XXdtcy                 5      8  0
      *
     d                 DS
     d  xxRPmy                 1      6  0
     d  xxrpmm                 1      2  0
     d  xxrpcy                 3      6  0

      // Build Cancellation Notice Batch
     d BldCXBtch       pr                  ExtPgm('W0160C')
     d   outco#                       3                                         Company
     d   outfnd                       3                                         Fund
     d   outfrdt                      8                                         Policy From Date
     d   outtodt                      8                                         Policy To Date
     d   outbldt                      8                                         Billing Thru Date
     d   outpydt                      8                                         Payment Thru Date
     d   outetyp                      1                                         Type of account
     d   outmin                       7                                         Minimum Balance
     d   outrpmy                      6                                         Reporting Month/Year
     d   outday                       2                                         Excl New acc fr Day
     d   outbat#                      9                                         Batch#
     d   outrsn                       2                                         Cancel Resaon Code
     d   outcom1                     60                                         Comment 1
     d   outcom2                     60                                         Comment 2
     d   outoutq                     10                                         OutQ
     d   outmode                      1                                         Mode
     d   outrndt                      8                                         Run Date
     d   outisdt                      8                                         Estimated Issue Date
     d   outeafl                      1                                         Estimate/Audit Flag
     d   outwedt                      8                                         Week Ending Date
     d   outxadt                      8                                         Excl New Acc Date
     d   outfreq                      1                                         Reporting Frequency
     d   outsrpy                      1                                         Incl Cur acc pr year
     d   outindp                      1                                         Incl deposit in Calc
     d   outabil                      1                                         Incl Agency Bill Emp
     d   outstieflg                   1                                         States Inc/Excl
     d   outstcodes                 150                                         States

      * Emp info
     d W0050R          pr                  ExtPgm('W0050R')
     d ##kc                           1
     d ##kl                           1
     d ##mode                         1
     d ##rrn                          9  0
     d ##rrn2                         9  0
     d ##qte                          1
     d ##view                         1  0
     d ##rrn3                         9  0

      * Submit Cancellation Update & Print Notice
     d SbmMassCX       pr                  ExtPgm('W0163C')
     d qqco#                          3
     d qqfnd                          3
     d qqdate                         8
     d qqcdat                         8
     d qqbat#                         9
     d qqccrt                         1
     d qqtype                         1
     d qqform                         1
     d qqoutq                        10
     d qqmode                         1
     d qqsrt1                         2
     d qqsrt2                         2

     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d #1rrn                          9
     d #1mode                         1
     d #1next                        10
     d #1tabind                       5
     d #1Qte                          1

     d WtPolCXRn       pr                  ExtPgm('WTPOLCXRN')
     d #2rrn                          9
     d #2mode                         1
     d #2next                        10
     d #2tabind                       5
     d #2Qte                          1

      // Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      * Issue Mass Cancellation
     d IssMassCX       pr

      // Load CX Batch Master Grid
     d LoadCXBGrid     pr

      // Load Cancellation Batch Detail Grid
     d LoadCXBDGrid    pr
     d inIncludeBat                  10    const

      * Main Processing
     d Main            pr

      // Process Pgmstr
     d PgmStr          pr

      * Process Addition of New CX Batch
     d ProcessAdd      pr

      * Process other Options for CX batch master
     d ProcOther       pr

      * Process other option for Cancellation Batch Detail Grid
     d ProcOtherBD     pr

      * Process Performed Action from the action dropdown
     d ProcPerAct      pr

      * Process the Row Click in Producers Grid
     d ProcRoWClk      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate CX Batch Master Record procedure.
     d ValidateBat     pr              n

      * Validate CX Batch Detail Record procedure.
     d ValidateBatDt   pr              n
     d strmode                        1

      * Validtate Issue Mass Cancellation Batch procedure
     d ValidIssMCx     pr              n

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
     d/copy sPrXML
     d/copy sPrLetter

      //
      //   Key List for the Documents Table File
      //
     C     keyWXCCL      klist
     C                   kfld                    co#_sav
     C                   kfld                    fund_sav
     C                   kfld                    bat#_sav

     C     keyWXCNL3     klist
     C                   kfld                    co#_sav
     C                   kfld                    fund_sav
     C                   kfld                    bat#_sav

      ***  Beg Add  **** 08/20/25 *******************************
     C     keyWXCNL4     klist
     C                   kfld                    q1co#
     C                   kfld                    q1fnd
     C                   kfld                    q1bat#
     C                   kfld                    q1emp#
      ***  End Add  **** 08/20/25 *******************************
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
       //*** Begin Add *** 07/28/25 ****************************************
       Select;
       When clientID = '153';
          is153  = *On;
          hld153 = *On;
       Other;
          is153  = *Off;
          hld153 = *Off;
       EndSl;
       //*** End   Add *** 07/28/25 ****************************************

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // Issue Mass Cancellation Batch
       //===================================================================
     p IssMassCX       b
     d IssMassCX       pi
     d  isexit         s               n   Inz(*off)
      /Free

       clear rcdMCxBtch;
       //*** Beg Chg ***** 05/31/24 **********************
       //***Chain(n) ##rrn pcc0p;
       Chain(n) scRrn pcc0p;
       //*** End Chg ***** 05/31/24 **********************

       ##key = 'SY';
       SYofif = *blanks;
       visGCHL = *off;
       Chain ##key ssy01;
       If %found(ssylf);
          If SYofif = 'Y';
             visGCHL = *on;
          Endif;
       Endif;
       If outCltID  = '129';
          scSRT1 ='AN';
          scSRT2 ='EN';
       Endif;
       scAEB = *blanks;
       If scGCHL = ' ';
          scGCHL = 'N';
       Endif;
       isUseScript = UseScriptura(processType);

       scCXNDate = DateTo8(systemDate());
       Dow isexit = *off;
          Exfmt rcdMCxBtch;

          // Perform actions on screen.
          Select;

          // Submit button
          When btnSubmit = *On;
             btnSubmit = *Off;
             If ValidIssMCx();
                //*** Beg Chg ***** 05/31/24 **********************
                //***Chain ##rrn pcc0p;
                Chain scRrn pcc0p;
                //*** End Chg ***** 05/31/24 **********************
                exsr Mvscdb;
                update pcc0p;
                // call W0163C to submit cancellation update & print notice
                exsr subCxNotice;
                isexit = *on;
             Endif;
          // Cancel button
          When btnCancel = *On;
             btnCancel = *Off;
             isexit = *on;
          Endsl;
       Enddo;

       Begsr Mvscdb;
        CCbpf  = *blank;
        CCuser = q1user;
        CCpgm  = q1pgm;
        CCdt   = Dateto7(SystemDate()); // Date
        CCtime = Timeto6(SystemTime()); // Time
       Endsr;

       // call W0163C to submit cancellation update & print notice
       Begsr subCxNotice;
        If visGCHL <> *on;
           scGCHL = 'N';
        Endif;
        qqco# = %char(CCco#);
        //*** Beg Chg ***** 05/31/24 **************************
        //***qqfnd = %char(CCfnd);
        //***qqdate = %char(scCXNDate);
        //***qqcdat = %char(scCXEfDate);
        //***qqbat# = %char(CCbat#);
        qqfnd = %Editc(CCfnd:'X');
        qqdate = %EditC(scCXNDate:'X');
        qqcdat = %EditC(scCXEfDate:'X');
        qqbat# = %EditC(CCbat#:'X');
        //*** Beg Chg ***** 05/31/24 **************************

        Select;
        When scType ='R';
           scType = scAEB;
        When scType ='L' and scAEB='A';
           scType = 'F';
        When scType ='L' and scAEB='E';
           scType = 'G';
        When scType ='L' and scAEB='B';
           scType = 'H';
        Other;
        Endsl;

        qqccrt = scGCHL;
        qqtype = scType;
        qqform = scForm;
        qqoutq = UPoutq;
        qqmode = 'D';
        qqsrt1 = scSrt1;
        qqsrt2 = scSrt2;
        // call W0163C
        SbmMassCX(qqco#:qqfnd:qqdate:qqcdat:qqbat#:qqccrt:
               qqtype:qqform:qqoutq:qqmode:qqsrt1:qqsrt2);
       Endsr;

      /End-Free
     p IssMassCX       e

       //===================================================================
       // Load CX Batch Master Grid
       //===================================================================
     p LoadCXBGrid     b
     d LoadCXBGrid     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdCXBtch;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll *loval wxccl;
       Read wxccl;
       Dow not %eof(wxccl);
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
          scFnd  = CCfnd;
          scBat# = CCbat#;
          scBlDt = CvtToDate(CCbldt);
          scPtDt = CvtToDate(CCptdt);
          scRpDt = CvtToDate(0);
          scRpmy = CvtToDate(0);
          If CCwedt <> 0 and WCfl23 = 'Y';
             scRpDt = CvtToDate(CCwedt);
             visbrpdt = *on;
          Endif;
          If CCwedt = 0 and CCrpmy <> 0 and WCfl23 <> 'Y';
             scRpmy = CvtToDate(dt6to8(dt6MDYY(CCrpmy)));
             visbrpdt = *off;
          Endif;
          scMin  = CCmin;
          scRsn  = CCrsn;
          scOff# = CCoff#;

          scEtypD = *Blanks;
          Select;
          When CCetyp = 'S';
             scEtypD = 'Self Reporting';
          When CCetyp = 'E';
             scEtypD = 'Estimating';
          When CCetyp = 'B';
             scEtypD = 'Both';
          Endsl;

          scEaflD = *Blanks;
          Select;
          When CCeafl = 'A';
             scEaflD = 'Audit';
          When CCeafl = 'E';
             scEaflD = 'Estimate';
          When CCeafl = 'B';
             scEaflD = 'Both';
          Endsl;

          scAdppD = *Blanks;
          Select;
          When CCadpp = 'A';
             scAdppD = 'Amount';
          When CCadpp = 'P';
             scAdppD = 'Percentage';
          Endsl;

         // Get NCCI Carrier # from Group Auxiliary File
          Chain (CCCo#:CCFnd) pf201;
          If %found(wdf2l);
             scNcar# = F2car#;
          Endif;

          scRRN  = dbrrn;

          Write sflCXBtch;
          // Save 1st record information to load in CX Transaction details Grid
          If bat#_sav = 0;
             co#_sav  = CCco#;
             fund_sav = CCfnd;
             bat#_sav = CCbat#;
             CXDtlRrn = scRRN;
          Endif;

          Read wxccl;
       Enddo;

       Clear inInclude;
       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadCXBGrid     e

       //===================================================================
       // Load CX Batch Detail Grid
       //===================================================================
     p LoadCXBDGrid    b
     d LoadCXBDGrid    pi
     d inIncludeBat                  10    const

      /free
       // Clear the subfile
       scSflRRND = 0;
       scSflDsp  = *off;
       scSflDspD = *off;
       scSflClrD = *on;
       Write rcdCXBtch;
       scSflDsp  = *on;
       scSflDspD = *on;
       scSflClrD = *off;
       scRowSel2 = 1;

       // Populate the subfile
       Setll keyWXCNL3 wxcnl3;
       Reade keyWXCNL3 wxcnl3;
       Dow not %eof(wxcnl3);
          scSflRRND += 1;
          scSflSelD = *off;
 
          If svsflrrnd = 0;
             If scSflRRND = 1;
                scSflSelD = *on;
             Endif;
          Else;
             If scSflRRND = svsflrrnd;
                scSflSelD = *on;
             Endif;
          Endif;
 
          scSelCX = CNselc;
          scGroup = CNFnd;
          scEmployer = CNemp#;
          scDivision = CNdiv;
          scYear = CNfyr;
          scAmtDue = CNamtd;
          scCXRsn = CNrecd;
          //*** Beg Add **** 06/04/25 *******************************
          scAnnPay = CNaip;
          scEFT = CNach;
          scCredAdj = CNadj;
          scVIP = CNvip;
          sc#Cx = CNcnc#;
          //*** End Add **** 06/04/25 *******************************

          // Set Visibility for Premium finance company
          visPremFin = *off;
          //*** Beg Add **** 06/04/25 *******************************
          scFinance  = *blanks;
          //*** End Add **** 06/04/25 *******************************
          //***  Beg Chg  **** 04/02/24 *******************************
          //***  If CNPFIN = 'Y';
          Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
          If %found(wdell) and ELpfin <> *blanks;
          //***  Beg Chg  **** 04/02/24 *******************************
             visPremFin = *on;
             //*** Beg Add **** 06/04/25 *******************************
             scFinance  = 'Y';
             //*** End Add **** 06/04/25 *******************************
          Endif;
          scRrnCXD = dbrrn3;

          // Select Checkbox
          If inIncludeBat = 'Include';
             scSelectCX = 'Y';
             scnxtchgD = *on;
          Else;
             scSelectCX = 'N';
             scnxtchgD = *off;
          Endif;

          Write sflCXBtchD;

          Reade keyWXCNL3 wxcnl3;
       Enddo;

       Clear inInclude;
       scSflRRND = 1;
       If svSflRRND <> 0;
          scSflRRND = svSflRRND;
          scRowSel2 = svSflRRND;
          svSflRRND = 0;
       Endif;

      /end-free
     p LoadCXBDGrid    e

      *//===================================================================
      *// LoadSTCodes - Load State codes dropdown
      *//===================================================================
     p LoadSTCodes     b
     d LoadSTCodes     pi
     d strSTdesc       s             30
     D i               s              3  0 inz
     D kkSt            s             10    inz
     D StateListDS     DS           150
     D   xxStList                   150
     D   AryStList                    2    DIM(75) OVERLAY(xxStList)
      /Free

       If Not %Open(STSTL5);
          Open STSTL5;
       Endif;

       xxStList = CCStList;
       scSTsel = *Blanks;
       scSTval = *Blanks;
       strSTdesc = *Blanks;

       If ccStInEx <> *blanks;
          For i = 1 to 75;
             kkst = AryStList(i);
             If kkst <> *blanks;
                chain kkSt PST01;
                If %found(STSTL5);
                   strSTdesc = TitleCase(STname);
                   If scSTsel = *Blanks;
                      scSTsel = %trim(STkey)+'-'+%trim(strSTdesc);
                      scSTval = %trim(STkey);
                   else;
                      scSTsel=%trim(scSTsel)+','+%trim(STkey)+'-'+
                              %trim(strSTdesc);
                      scSTval = %trim(scSTval)+','+%trim(STkey);
                   endif;
                endif;
             endif;
          Endfor;

       Else;

          // Read WTTTP file from starting
          read STSTL5;
          Dow not %Eof(STSTL5);
             strSTdesc = TitleCase(STname);
             If scSTsel = *Blanks;
                scSTsel = %trim(STkey)+'-'+%trim(strSTdesc);
                scSTval = %trim(STkey);
             else;
                scSTsel=%trim(scSTsel)+','+%trim(STkey)+'-'+%trim(strSTdesc);
                scSTval = %trim(scSTval)+','+%trim(STkey);
             endif;
             read STSTL5;
          enddo;
       Endif;

       If %Open(STSTL5);
          Close STSTL5;
       Endif;
      /End-Free

     p LoadSTCodes     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       LoadActionBar();
       BtnCancel = *off;
       reload = *on;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load CX Batch Master Grid
          If reload = *on;
             LoadCXBGrid();
             reload = *off;
          endif;

          // Load CX Batch Detail Grid
          LoadCXBDGrid(inInclude);

          // display screen
          Exsr ExfmtScreens;
          scFocus = '0';

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
             scFocus = '1';

          // Exclude All
          When btnBatExcl = *on;
             btnBatExcl = *off;
             inInclude = ' ';
             scFocus = '1';

          // Perform Action
          When btnPerAct = *on;
             btnPerAct = *off;
             ProcPerAct();
             scFocus = '1';

          // Add CX Batch Master
          When BtnCXBAdd = *On;
             BtnCXBAdd = *Off;
             ProcessAdd();

          Other;
             ProcOther();             // Process other Options for CX batch Master
             ProcOtherBD();           // Process other Options for CX batch detail

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
           pnlHeight = 1040;
           APnlHeight = pnlHeight + 60;

           Write rcdComPnl;
          //*** Beg Chg ***** 05/13/25 ************************************
          // Write rcdPanel;
          // Exfmt rcdRFBtch;
          // Read rcdComPnl;
          // Read rcdPanel;
          tabVisInd = *On;
          Write rcdPanelCx;
          Exfmt rcdCXBtch;
          Read rcdComPnl;
          Read rcdPanelCx;
          //*** End Chg ***** 05/13/25 ************************************

        EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of New CX Batch
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  xxbat#         s                   like(CCbat#) INZ
      /free

       Clear RcdBatAdd;
       Clear pcc0p;
       mode = 'A';
       isexit = *off;
       BtnBatCan = *off;
       LoadSTcodes();

      // Set selection criteria for Office Number
        scOff#CH = 'OFco# = ' + %EditC(intCo#:'X') + ' and ' +
         'OFstat<>' + '''I''';

       scFl23 = WCfl23;

       Dow BtnBatCan = *off and isexit = *off;

          scBatHdg = 'Add Cancellation Batch';
          Exfmt RcdBatAdd;
          Select;
          // Save Button
          When BtnSaveBat = *on;
             BtnSaveBat = *off;
             If ValidateBat();
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
        // Get Next Available CX Batch Number from SMCOP
        Exec SQL
         Select COnacxbat#  into :xxbat# from SMCOP
          where COco# = :intCo#;

        If xxbat# = 0;
           // Get latest CX Batch Number
           Exec SQL
            Select max(CCbat#) into :xxbat# from WXCCP
            where CCco# = :intCo#;
        Endif;
        xxbat# = xxbat# + 1;

        // Update Next Available CX Batch Number in SMCOP
        Exec SQL
         Update SMCOP set COnacxbat# = :xxbat#
         where COco# = :intCo#;

        CCco#   = intCo#;                // Company Number
        CCfnd   = scGroupE;              // Group Number
        CCbat#  = xxbat#;                // Batch Number
        CCetyp  = scEtypa;               // Account Type
        CCrsn   = scRsna;                // Cancellation Reason Code
        CCfrdt  = dtCYMD(scFrdta);       // Policy Effective From Date
        CCtodt  = dtCYMD(scTodta);       // Policy Effective To Date
        CCbldt  = dtCYMD(scBldta);       // Billing Through Date
        If scWedta = 0 and scRpmya = 0;
           If scBldta = 0;
              CCbldt  = Dateto7(SystemDate()); // Move Today's Date
           Endif;
           CCptdt  = Dateto7(SystemDate()); // Payment Through Date
        Endif;
        CCmin   = 0;
        CCadpp  = ' ';
        If scMina <> 0 and scPprma = 0;
           CCmin   = scMina;                // Minimum Balance
           CCadpp  = 'A';                   // Batch based on Amount due or % Premium
        Endif;
        If scMina = 0 and scPprma <> 0;
           CCmin   = scPprma;               // Percentage of Premium
           CCadpp  = 'P';                   // Batch based on Amount due or % Premium
        Endif;
        CCeafl  = scEafla;               // Balance Type
        CCindp  = scIndpa;               // Include Deposit in Balance Calculation
        CCwedt  = dtCYMD(scWedta);       // Reporting Date
        CCrpmy  = dt5CYMD(scRpmya);      // Reporting MM/CCYY
        // Move Week Ending Date into Reporting Month/Year
        If CCwedt <> 0;
           CCrpmy = %dec(%subst(%editc(CCwedt:'X'):1:5):5:0);
        EndIf;
        CCfreq  = scFreqa;               // Reporting Frequency
        CCday   = scDaya;                // Exclude New Accounts from Day
        CCxadt  = dtCYMD(scXadta);       // Exclude After Date
        CCoff#  = scOff#a;               // Office Number
        CCagt#  = 0;                     // Agent#
        CCcom1  = scCom1a;               // Comment1
        CCcom2  = scCom2a;               // Comment2
        CCstinex= scStCodIE;             // States Inclusion/Exclusion
        CCstlist= *Blanks;               // States List
        //Remove commas separating states
        If scSTcodes <> *blanks;
           ieSTcodes = ' ';
           ieSTcodes = %trim(%xlate(',':' ':scSTcodes));
           intpos = 0;
           Dou intpos = 0;
              intpos = %scan(' ':%trim(ieSTcodes));
              If intpos <> 0;
                 ieSTcodes = %replace('':ieSTcodes:intpos:1);
              Endif;
           Enddo;
           CCstlist = %subst(ieSTcodes:1:150);
        Endif;

        CCstat  = 'A';                   // Status
        CCbpf   = 'N';                   // Batch Processed

        CCuser  = q1user;                // User
        CCpgm   = q1pgm;                 // Program
        CCtime  = Timeto6(SystemTime()); // Time
        CCdt    = Dateto7(SystemDate()); // Date Mintained
       EndSr;

       // Submit Job to Build Cancellation Notice Batch
       BegSr SbmCxBtch;
        outCo# = %EditC(intCo#:'X');
        outfnd = %EditC(CCfnd:'X');
        outfrdt = %EditC(scFrdta:'X');
        outtodt = %EditC(scTodta:'X');
        outbldt = %EditC(scBldta:'X');
        outpydt = %EditC(Dateto8(SystemDate()):'X');
        outetyp = CCetyp;
        outmin = %EditC(CCmin:'X');
        outrpmy = %EditC(scRpmya:'X');
        outday = %EditC(CCday:'X');
        outbat# = %EditC(CCbat#:'X');
        outrsn = CCrsn;
        outcom1 = CCcom1;

        If %subst(outcom1:60:1) = ' ';
           %subst(outcom1:60:1) = '¢';
        Endif;

        outcom2 = CCcom2;

        If %subst(outcom2:60:1) = ' ';
           %subst(outcom2:60:1) = '¢';
        Endif;

        outoutq = UPoutq;
        outmode = 'D';
        outrndt = %EditC(Dateto8(SystemDate()):'X');

        If WCfl23 = 'Y';
           outisdt = %EditC(scIsdta:'X');           // Estimated Issue Date
        EndIf;

        outeafl = CCeafl;
        outwedt = %EditC(scWedta:'X');
        outxadt = %EditC(scXadta:'X');
        outfreq = CCfreq;
        outsrpy = scSrpya;                          // Include Current Accounts Prior Year Missing
        outindp = CCindp;
        outabil = scAbila;                          // Include Agency Bill Employers
        outstieflg = CCstinex;
        outstcodes = CCstlist;

        // Set From date, To date, Run date based on WCfl23 flag (Reporting Date / MMYYYY)
        If WCfl23 <> 'Y' And scRpmya = 0;
           outFrDt = %EditC(SCfrDta:'X');
           outToDt = %EditC(SCtoDta:'X');
           outRnDt = %EditC(DateTo8(systemDate()):'X');
        Else;
           xxdate = 0;
           xxRPmy = 0;
           xxRPmy = scRpmya;
           xxdtmm = xxrpmm;
           xxdtcy = xxrpcy;
           xxdtdd = 01;

           outfrdt = %EditC(xxdate:'X');                                    // First date of Month
           outtodt = %EditC(Dateto8(GetLastDay(CvtToDate(xxdate))):'X');    // Last date of Month
           outrndt = *zeros;
        Endif;

        If WCfl23 = 'Y' And scWeDta = 0;
           outFrDt = %EditC(scFrDta:'X');
           outToDt = %EditC(scToDta:'X');
           outRnDt = %EditC(DateTo8(systemDate()):'X');
        Else;
           // From date, To date, must be first and last of Month If frequency = M or B
           If (scFreqa = 'M' or scFreqa = 'B');
              outfrdt = %EditC(Dateto8(GetFirstDay(CvtToDate(scWedta))):'X');
              outtodt = %EditC(scWedta:'X');
              outrndt = *zeros;
           Endif;

           // To date must be Reporting Date and From Date must be 7 days    less
           // If frequency = W (Weekly)
           If scFreqa = 'W';
              outfrdt = %EditC(dtPast(scWedta:7):'X');
              outtodt = %EditC(scWedta:'X');
              outrndt = *zeros;
           Endif;
        Endif;

        BldCXBtch(outco#:outfnd:outfrdt:outtodt:outbldt:outpydt:outetyp:
         outmin:outrpmy:outday:outbat#:outrsn:outcom1:outcom2:outoutq:
         outmode:outrndt:outisdt:outeafl:outwedt:outxadt:outfreq:
         outsrpy:outindp:outabil:outstieflg:outstcodes);
       EndSr;

       // Writing new CX Batch subroutine
       BegSr WrtBat;
        Clear pcc0p;
        Exsr MoveScDb;
        Write pcc0p;

        // Submit Job to Build Cancellation Notice Batch
        Exsr SbmCxBtch;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options for CX batch master
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  isIssErr       s               n   Inz(*off)
      /free

       Clear rcdBatAdd;
       isexit = *off;

       ReadC sflCXBtch;
       Dow not %eof;

          Select;
          When scCXIssu = *On; // Processing for Issue Mass CX Link
             scCXIssu = *Off;
             update sflCXBtch;

             Chain(n) scRRN WXCCP;
             Exsr MovDbSc;
             scBatHdg = 'Issue Mass Cancellation';

             If CCbpf <> 'Y';            // Batch Processed Flag
                Write rcdBatAdd;
                PopOneBtn('This Cancel Batch is being Processed. No Deletion +
                 or Viewing Allowed.':'Issue Mass Cancellation Error':'Ok');

             Else;

                Exsr ChkRec;         // Check Employer Records, selected for Mass CX
                If isIssErr = *off;
                   IssMassCX();
                Else;
                   Write rcdBatAdd;
                   PopOneBtn('Blank Cancellation Reason Codes exist for the +
                    selected Employers. Before Issuing Mass Cancellations, +
                    Enter a Reason Code for the Detail Records that do not +
                    Currently have one.':'Issue Mass Cancellation Error':'Ok');
                Endif;
             Endif;

          When scCXDlt = *On; // Processing for Delete CX Batch Link
             scCXDlt = *Off;
             update sflCXBtch;

             Chain(e) scRRN WXCCP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRRN WXCCP;
             Exsr MovDbSc;

             scBatHdg = 'Delete Cancellation Batch';

             If CCbpf <> 'Y';            // Batch Processed Flag
                Write rcdBatAdd;
                PopOneBtn('This Cancel Batch is being Processed. No Deletion +
                 or Viewing Allowed.':'Delete Error':'Ok');
             Else;

             Dow isexit = *off;

                Write rcdBatAdd;
                isDelresp = PopTwoBtn('Do you want to Delete this Cancellation +
                 Batch?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   //***  Beg Add  **** 07/25/24 *******************************
                   // Delete all records from CX batch details file WXCNP
                   Setll (CCco#:CCfnd:CCbat#) wxcnl4;
                   Reade (CCco#:CCfnd:CCbat#) wxcnl4;
                   Dow not %eof(wxcnl4);
                      Delete pcn04;
                      Reade (CCco#:CCfnd:CCbat#) wxcnl4;
                   Enddo;
                   //***  End Add  **** 07/25/24 *******************************
                   Delete pcc0p;
                   PopOneBtn('Cancellation Batch Deleted!':'Delete':'Ok');
                   isexit = *on;
                   reload = *on;
                   bat#_sav = 0;         // Reload the master grid from top
                   svsflrrn = 0;         // Reload the master grid from top
                When isDelresp = *Off;
                   svsflrrn = scSflrrn;  //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;
             Endif;

          Other;
             scSflRRN = svSflRRN;    // Restore the saved row selection number
             scRowSel = svSflRRN;    // Restore the saved row selection number

          Endsl;

          Unlock WXCCP;

          ReadC sflCXBtch;
       Enddo;

       If scSflRRN = 0;
          scSflRRN = scRowSel;    // Restore the saved row selection number
       Endif;

       // Subrotine to Check Employer Records, selected for Mass CX
       // When option Issue Mass CX is chosen read through
       // detail recs for selected batch. If selected for canx,
       // verify that all detail recs for that employer have non
       // blank reason codes. Display error msg if blank.
       BegSr ChkRec;
        isIssErr = *off;
        //*** Beg Chg ***** 05/31/24 **********************
        //***Chain (CCco#:scFnd:scBat#) pcn04;
        //***Dow %found(wxcnl4);
        Setll (CCco#:scFnd:scBat#) pcn04;
        Reade (CCco#:scFnd:scBat#) pcn04;
        //*** Beg Chg ***** 05/31/24 **********************
        Dow Not %Eof(wxcnl4);
           If CNselc = 'Y' and CNrecd = *blanks;
              isIssErr = *on;
              leave;
           Endif;
           Reade (CCco#:scFnd:scBat#) pcn04;
        Enddo;
       EndSr;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scGroupE = CCfnd;
        scEtypa  = CCetyp;
        scRsna   = CCrsn;
        scFrdta  = dtMDYY(CCfrdt);
        scTodta  = dtMDYY(CCtodt);
        scBldta  = dtMDYY(CCbldt);
        scMina   = 0;
        scPprma  = 0;
        If CCadpp = 'A';
           scMina   = CCmin;
        Endif;
        If CCadpp = 'P';
           scPprma  = CCmin;
        Endif;
        scEafla  = CCeafl;
        scIndpa  = CCindp;
        scWedta  = dtMDYY(CCwedt);
        scXadta  = dtMDYY(CCxadt);
        scFreqa  = CCfreq;
        scRpmya  = dtMDYY(CCrpmy);
        scDaya   = CCday;
        scOff#a  = CCoff#;
        scCom1a  = CCcom1;
        scCom2a  = CCcom2;
        scStCodIE= CCstinex;

        LoadSTcodes();

       // Set selection criteria for Office Number
        scOff#CH = 'OFco# = ' + %EditC(intCo#:'X') + ' and ' +
         'OFstat<>' + '''I''';

        scFl23 = WCfl23;
       EndSr;

      /end-free
     P ProcOther       E

       //===================================================================
       // ProcOtherBD - Process Edit option for Cancellation Batch Detail
       //===================================================================
     p ProcOtherBD     b
     d ProcOtherBD     pi
     d* inmod          s              1
     d* @@bata         s             11  2 inz(0)
     d* @@trdt         s              8  0 inz(0)
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /Free


       Clear rcdBatEdit;
       //*** Begin Add *** 07/28/25 ****************************************
       is153 = hld153;
       //*** End   Add *** 07/28/25 ****************************************
       isexit = *off;
       ReadC sflCXBtchD;
       Dow not %eof;
          scFocus = '1';
          Select;
          When scCXBDEdit = *on;
             scCXBDEdit = *off;
             Chain(e) scRRNCXD WXCNP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             svsflrrnd = scSflrrnd;  //save rrn value
             Chain(n) scRRNCXD pcn0p;
             Exsr Mvdbsc;

             // Load only selected Cancellation Reason code for state.
             Clear chCXst;
             Chain (CNco#:CNfnd:CNemp#:CNdiv) pem00;
             If EMmst <> *blanks;
                chCXst = 'CRco# = ' + %char(CNCo#) +
                ' and CRst = ' +''''+EMmst+''''+
                 ' and CRstat <> ''I''';
             Else;
                chCXst = 'CRco# = ' + %char(CNCo#) +
                ' and CRst = ' +''''+EMpst+''''+
                 ' and CRstat <> ''I''';
             Endif;

             scBatDHdg = 'Edit Cancellation Batch Detail Information';
             scBatDTop = 800;
             //***  Beg Add  **** 04/08/24 *******************************
             scConTop = 825;
             //***  End Add  **** 04/08/24 *******************************
             Dow isexit = *off;
                Exfmt rcdBatEdit;
                Select;
                When BtnSavBatD = *on;
                   BtnSavBatD = *off;
                   // Check if WXCNP updated since screen displayed (CX Batch Detail)
                   flgRcdUpdated=RcdUpdatedErr('WXCNP':dbrrn4:'CNdt':'CNtime':
                    'CNuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNCXD pcn0p;
                      Exsr Mvdbsc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateBatDt(mode);
                         Chain scRRNCXD pcn0p;
                         Exsr Mvscdb;
                         Update pcn0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                When BtnCanBatD = *on;
                   BtnCanBatD = *off;
                   isexit = *on;
                Endsl;
             Enddo;

          // Process Delete option for Cancellation Batch Detail.
          When scCXBDDlt = *on;
             scCXBDDlt = *off;
             Chain(e) scRRNCXD WXCNP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             //***  Beg Chg  **** 08/20/25 *******************************
             //*** Chain scRRNCXD pcn0p;
             Chain(n) scRRNCXD pcn0p;
             //***  End Chg  **** 08/20/25 *******************************
             Exsr Mvdbsc;
             scBatDHdg = 'Delete Cancellation Batch detail information';
             scBatDTop = 135;
             //***  Beg Add  **** 04/08/24 *******************************
             scConTop = 160;
             //***  End Add  **** 04/08/24 *******************************
             Dow isexit = *off;
                Write rcdBatEdit;
                //***  Beg Chg  **** 04/02/24 *******************************
                //***  isDelresp = PopTwoBtn('Do you want to delete this +
                //***  Cancellation Batch information?':'Confirm Delete':'Yes':'No');
                strErrMsg = 'Do you want to Delete Cancellation Batch Details +
                 Information for Batch# ' +  %trim(%editc(CNbat#:'3'));
                isDelresp = PopTwoBtn(strErrMsg:'Confirm Delete':'Yes':'No');
                //***  End Chg  **** 04/02/24 *******************************
                Select;
                When isDelResp = *on;
                   //***  Beg Chg  **** 08/20/25 *******************************
                   //*** Delete pcn0p;
                   // Delete all records from CX batch details for selected Employer (all divisions)
                   q1co#  = CNco#;
                   q1fnd  = CNfnd;
                   q1bat# = CNbat#;
                   q1emp# = CNemp#;
                   Setll keyWXCNL4 wxcnl4;
                   Reade keyWXCNL4 wxcnl4;
                   Dow not %eof(wxcnl4);
                      Delete pcn04;
                      Reade keyWXCNL4 wxcnl4;
                   Enddo;
                   //***  End Chg  **** 08/20/25 *******************************
                   //***  Beg Chg  **** 04/02/24 *******************************
                   //***  PopOneBtn('Cancellation Batch information is +
                   //***   Deleted!':'Delete':'Ok');
                   strErrMsg = 'Cancellation Batch Details Information is +
                    Deleted for Batch# ' +  %trim(%editc(CNbat#:'3'));
                   PopOneBtn(strErrMsg:'Delete':'Ok');
                   //***  End Chg  **** 04/02/24 *******************************
                   isexit = *on;
                When isDelResp = *off;
                   isexit = *on;
                   svrrnCXD = scRrnCXD;
                   svsflrrnd = scSflrrnd;     //save rrn value for detail grid
                EndSl;
             Enddo;

          // Process Emp Info option for Cancellation Batch Detail.
          When scCXBDEInf = *on;
             scCXBDEInf = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNCXD pcn0p;
             Chain (CNco#:CNfnd:CNemp#:CNdiv) pem00;
             If %found(wmeml);
                ##kc = *inkc;
                ##kl = *inkl;
                ##mode ='Z';
                ##rrn  = 0;
                ##rrn2 = 0;
                ##qte  = 'W';
                ##view = 0;
                ##rrn3 = dbrrn5;
                W0050R(##kc:##kl:##mode:##rrn:##rrn2:##qte:##view:##rrn3);
             Endif;

          // Process Emp Finance information.
          When scCXBDFInf = *on;
             scCXBDFInf = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNCXD pcn0p;
             Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
             If %found(wdell) and ELpfin <> *blanks;
                Clear rcdPrFnDtl;
                isexit = *off;

                scPrmFnHdg = 'Premium Finance Company Information';
                PrFnCoEdit = *on;       //disable non-editable field
                setgt (CNco#:CNfnd:ELpfin:ELpfdt) ppf00;
                readpe (CNco#:CNfnd:ELpfin) ppf00;
                Exsr MovDbSc;

                Dow isexit = *off;
                   Exfmt rcdPrFnDtl;
                   // Cancel Button
                   If BtnPrFnCan = *on;
                      BtnPrFnCan = *off;
                      isexit = *on;
                   Endif;
                Enddo;
             Endif;



          // Process Amount Due hyperlink to show Employer A/R Inquiry-"Summary - All Years"
          //*** Beg Del **** 06/04/25 *******************************
          //***When scAmtARInq = *on;
          //***   scAmtARInq = *off;
          //***   svsflrrnd = scSflrrnd;  //save rrn value
          //***   Chain(n) scRRNCXD pcn0p;
          //***   Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
          //***   #1rrn = %char(dbrrn8);
          //***   #1mode = 'D';
          //***   #1next = 'AmountDue';
          //***   #1TabInd = *blanks;
          //***   WtPolARInq(#1rrn:#1mode:#1next:#1TabInd:#1Qte);
          //*** End Del **** 06/04/25 *******************************

          // Process A/R inquiry information.
          When scCXBDARIq = *on;
             //*** Beg Add ***** 05/13/25 ************************
             tabVisInd  = *Off;
             Write rcdPanelCx;
             //*** End Add ***** 05/13/25 ************************
             scCXBDARIq = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain(n) scRRNCXD pcn0p;
             Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
             #1rrn = %char(dbrrn8);
             #1mode = 'D';
             #1next = 'ARInq';
             #1TabInd = *blanks;
             WtPolARInq(#1rrn:#1mode:#1next:#1TabInd:#1Qte);

          // Process Cancellation History information.
          When scCXBDHist = *on;
             scCXBDHist = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain(n) scRRNCXD pcn0p;
             Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
             #2rrn = %char(dbrrn8);
             #2mode = 'D';
             #2next = 'CXHist';
             #2TabInd = *blanks;
             WtPolCXRn(#2rrn:#2mode:#2next:#2TabInd:#1Qte);

          Endsl;

          Unlock WXCNP;

          ReadC sflCXBtchD;
       Enddo;

       // Move data base fields to screen fields
       // for CX Batch Detail info
       Begsr Mvdbsc;
        scGroupE = CNfnd;
        scEmpE = CNemp#;
        scDivE = CNdiv;
        scYearE = CNfyr;
        scCXNoE = CNcnc#;
        scAIPE = CNaip;
        scADJE = CNadj;
        scVipE = CNvip;
        scAmtDueE = CNamtd;
        scSelCXE = CNselc;
        scCXRsnE = CNrecd;
        scRFeeE = CNrfee;
        scCom1E = CNcom1;
        scCom2E = CNcom2;
        saveDT = CNdt;
        saveTM = CNtime;
       Endsr;

       // Move screen fields to data base file.
       Begsr Mvscdb;
        //*** Begin Add ***** 11/08/23 *****************************
        CNamtd = scAmtDueE;
        //*** End   Add ***** 11/08/23 *****************************
        //*** Begin Add *** 07/28/25 ***************************************
        If CNlFee <> CRlFee;
           CNamtd = (CNamtD - CNlFee) + CRlFee;
        EndIf;
        CNlFee = CRlFee;
        //*** End   Add *** 07/28/25 ***************************************
        CNselc = scSelCXE;
        CNrecd = scCXRsnE;
        CNrfee = scRFeeE;
        CNcom1 = scCom1E;
        CNcom2 = scCom2E;
        CNuser = q1user;                // User
        CNpgm  = q1pgm;                 // Program
        CNtime = Timeto6(SystemTime()); // Time
        CNdt   = Dateto7(SystemDate()); // Date Mintained
        saveDT = CNdt;
        saveTM = CNtime;
       Endsr;

       // Subrotine to move database fields into screen fields
       // for Premium Finance co. info
       BegSr MovDbSc;
        scFcodE   =   PFfcod;
        scEfDtE   =   dtMDYY(PFefdt);
        scExDtE   =   dtMDYY(PFexdt);
        scNameE   =   PFname;
        scAdr1E   =   PFadr1;
        scAdr2E   =   PFadr2;
        scCtyE    =   PFcty;
        scStE     =   PFst;
        scContE   =   PFcont;
        If PFarea <> *zeros or PFphn# <> *zeros;
           scAreaPhnE = %int(%editc(PFarea:'X') + %editc(PFphn#:'X'));
        Endif;
        scTypeE     =   PFtype;
       EndSr;





      /End-Free
     p ProcOtherBD     e

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
     d* inmod          s              1
     d* @@bata         s             11  2 inz(0)
     d* @@trdt         s              8  0 inz(0)

      /Free

        // Unlock WXRIL3;

       ReadC sflCXBtchD;
       Dow not %eof;
          If scSelectCX = 'Y';
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNCXD pcn0p;

             Select;
             When scSelAct = 'Change Cancellation Status';
                If CNselc = 'Y';
                   CNselc = 'N';
                Else;
                   CNselc = 'Y';
                EndIf;
                CNuser = q1user;                // User
                CNpgm  = q1pgm;                 // Program
                CNtime = Timeto6(SystemTime()); // Time
                CNdt   = Dateto7(SystemDate()); // Date Mintained
                update pcn0p;
             Endsl;
          Endif;

          ReadC sflCXBtchD;
       Enddo;

      /End-Free
     p ProcPerAct      e

       //===================================================================
       // ProcRoWClk - Process the Row Click in CX master Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load AR details grid for selected row.
       Chain scsflRRN sflCXBtch;
       Chain(n) scRRN wxccp;

       CXDtlRrn = scRRN;
       svsflRRN = scsflRRN;
       scrowsel = scsflRRN;

       co#_sav  = CCco#;
       fund_sav = CCfnd;
       bat#_sav  = CCbat#;

      /end-free

     P ProcRoWClk      E

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
       Chain(N) intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       processMe = *on;

       // Choices for Action Dropdown
       If @@Mode = 'D';
          ChoSelAct = ' ';
       Else;
          // ChoSelAct = '--- Select List Action ---,Change Cancellation Status,+
          ChoSelAct = '--- Select List Action ---';
          ChoSelAct = %trim(ChoSelAct) + ',Change Cancellation Status';
       Endif;
       scSelAct = '--- Select List Action ---';

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the A/R Batch Master Record.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBat     b
     d ValidateBat     pi              n

     d isError         s               n   inz
     d isSRinfo        s              1    inz
     d xxMEdate        s              8  0 inz
     d xxMMYYYY        s              6  0 inz
     d xxRPdate        s              8  0 inz
      /Free

        isError = *off;
        isSRinfo = ' ';
        xxMEdate = 0;
        xxMMYYYY = 0;
        xxRPdate = 0;
        xxdate = 0;
        xxRPmy = 0;
       // Initialize all the messageId and error indicators.
        erText   = *off;
        msText   = *blanks;
        erGroupE = *off;
        msGroupE = *blanks;
        erEtypa  = *off;
        msEtypa  = *blanks;
        erRsna   = *off;
        msRsna   = *blanks;
        erFrDta  = *off;
        msFrDta  = *blanks;
        erToDta  = *off;
        msToDta  = *blanks;
        erBlDta  = *off;
        msBlDta  = *blanks;
        erMina   = *off;
        msMina   = *blanks;
        erPprma  = *off;
        msPprma  = *blanks;
        erEafla  = *off;
        msEafla  = *blanks;
        erWeDta  = *off;
        msWeDta  = *blanks;
        erRpmya  = *off;
        msRpmya  = *blanks;
        erFreqa  = *off;
        msFreqa  = *blanks;
        erXaDta  = *off;
        msXaDta  = *blanks;
        erIsDta  = *off;
        msIsDta  = *blanks;
        erStCodIE = *off;
        msStCodIE = *blanks;


       // Group number can't be Zero
        If scGroupE <= *zero;
           erGroupE = *on;
           msGroupE = 'WCR0502';
           isError = *on;
        Endif;

       // Type of Account can't be blank.
        If scEtypa = *blanks;
           erEtypa = *on;
           msEtypa = 'WCR0503';
           isError = *on;
        Endif;

       // Cancellation Reason Code can't be blank.
        If scRsna = *blanks;
           erRsna = *on;
           msRsna = 'WCR0503';
           isError = *on;
        Endif;

       // Check if user has entered Missing S/R Report Criteria
        If (scWeDta <> 0 or scXaDta <> 0 or scRpmya <> 0 or scDaya <> 0
         or scFreqa <> ' ' or scSrpya <> ' ' or scAbila <> ' ');
           isSRinfo = 'Y';
        Endif;

       // When Type of Account = 'S',
       // Validate A/R Balance Analysis and Missing S/R Report Criteria
        If scEtypa = 'S';
          // 1) Cannot Enter both Criteria
           If (scFrDta <> 0 or scToDta <> 0 or scBlDta <> 0 or scMina <> 0
               or scPprma <> 0 or scEafla <> ' ' or scIndpa <> ' ')
            And (scWeDta <> 0 or scXaDta <> 0 or scRpmya <> 0 or scDaya <> 0
               or scFreqa <> ' ' or scSrpya <> ' ' or scAbila <> ' ');
              erText  = *on;
              msText  = 'WC30050';
              isError = *on;
           Endif;

          // 2) Must Enter any 1 Criteria
           If (scFrDta = 0 and scToDta = 0 and scBlDta = 0 and scMina = 0
               and scPprma = 0 and scEafla = ' ' and scIndpa = ' ')
            And (scWeDta = 0 and scXaDta = 0 and scRpmya = 0 and scDaya = 0
               and scFreqa = ' ' and scSrpya = ' ' and scAbila = ' ');
              erText  = *on;
              msText  = 'WC30051';
              isError = *on;
           Endif;
        Endif;



        If erText = *off;

          // Validate - For A/R Balance Analysis - Section
           If isSRinfo <> 'Y';
             // Validation Check for Policy From Date
              erFrDta = ValidDate(scFrDta:charMsg);
              If erFrDta = *on;
                 msFrDta = charMsg;
                 isError = *on;
              Endif;

             // Validation Check for Policy To Date
              erToDta = ValidDate(scToDta:charMsg);
              If erToDta = *on;
                 msToDta = charMsg;
                 isError = *on;
              Endif;
             // Policy To Date must be less than or equal to today's date
              //***  Beg Chg  **** 12/01/24 *******************************
              //*** If erToDta = *off and scToDta > Dateto8(SystemDate());
              If erToDta = *off and dtCYMD(scToDta) > Dateto7(SystemDate());
              //***  End Chg  **** 12/01/24 *******************************
                 erToDta = *on;
                 msToDta = 'WCR0606';
                 isError = *on;
              Endif;

             // Dates must be in Ascending order
              If scFrDta <> 0 and scToDta <> 0;
                 If erToDta = *off and erFrDta = *off;
                    erToDta = AscendDates(scFrDta:scToDta:charMsg);
                    If erToDta = *on;
                       erFrDta = *on;
                       msToDta = charMsg;
                       msFrDta = charMsg;
                       isError = *on;
                    Endif;
                 Endif;
              Endif;

             // Validation Check for Billings Through Date
              If scBlDta <> 0;
                 erBlDta = ValidDate(scBlDta:charMsg);
                 If erBlDta = *on;
                    msBlDta = charMsg;
                    isError = *on;
                 Endif;
              Endif;

             // Validate "Minimum balance" and "Percentage of Premium"
              If scMina <= *zero and scPprma <= *zero;
                 erMina = *on;
                 msMina = 'WC31465';
                 erPprma = *on;
                 msPprma = 'WC31465';
                 isError = *on;
              Endif;
              If scPprma >= 100;
                 erPprma = *on;
                 msPprma = 'WC31464';
                 isError = *on;
              Endif;
              If scMina <> *zero and scPprma <> *zero;
                 erMina = *on;
                 msMina = 'WCR0590';
                 erPprma = *on;
                 msPprma = 'WCR0590';
                 isError = *on;
              Endif;

             // Balance Type can't be blank.
              If scEafla = *blanks;
                 erEafla = *on;
                 msEafla = 'WCR0503';
                 isError = *on;
              Endif;
           Endif;


          // Validate - For Missing Self Reporting Reports - Section
           If WCfl23 = 'Y' and scEtypa = 'S' and isSRinfo = 'Y';
             // Validate Reporting Date
              erWeDta = ValidDate(scWeDta:charMsg);
              If erWeDta = *on;
                 msWeDta = charMsg;
                 isError = *on;
              Endif;
             // Reporting Date must be a Month End date if Frequency is 'M' or 'B'
              If erWeDta = *off and (scFreqa = 'M' or scFreqa = 'B');
                // Get Month End date (last date of Month)
                 xxMEdate = Dateto8(GetLastDay(CvtToDate(scWeDta)));
                 If scWeDta <> xxMEdate;
                    erWeDta = *on;
                    msWeDta = 'WC30254';
                    isError = *on;
                 Endif;
              Endif;

             // Reporting Frequency can't be blank.
              If scFreqa = *blanks;
                 erFreqa = *on;
                 msFreqa = 'WCR0503';
                 isError = *on;
              Endif;

             // Validate Exclude New Accounts Starting on/after Day - Date
              If scXaDta <> 0;
                 erXaDta = ValidDate(scXaDta:charMsg);
                 If erXaDta = *on;
                    msXaDta = charMsg;
                    isError = *on;
                 Endif;
              Endif;
           Endif;

          // Validate - For Missing Self Reporting Monthly Reports - Section
           If WCfl23 <> 'Y' and scEtypa = 'S' and isSRinfo = 'Y';
             // Reporting MM/CCYY can't be Zero
              xxMMYYYY = dt6Check(scRpmya);
              If xxMMYYYY = 0;
                 erRpmya = *on;
                 msRpmya = 'WCR0500';
                 isError = *on;
              Endif;

             // Validate Exclude New Accounts Starting on/after Day - Date
              If scDaya <> 0 and erRpmya = *off and scRpmya <> 0;
                 xxRPmy = scRpmya;
                 xxdtmm = xxrpmm;
                 xxdtcy = xxrpcy;
                 xxdtdd = scDaya;
                 xxRPdate = xxdate;

                 erDaya = ValidDate(xxRPdate:charMsg);
                 If erDaya = *on;
                    msDaya = 'WCR2488';
                    isError = *on;
                 Endif;
              Endif;
           Endif;

        Endif;

       // Validate Estimated Issue Date
        If scIsDta <> 0;
           erIsDta = ValidDate(scIsDta:charMsg);
           If erIsDta = *on;
              msIsDta = charMsg;
              isError = *on;
           Endif;
        Endif;
       // Issue Date must be greater than or equal to today's date
        //***  Beg Chg  **** 12/01/24 *******************************
        //*** If erIsDta = *off and scIsDta > Dateto8(SystemDate());
        //***  Beg Chg  **** 08/14/25 *******************************
        //*** If erIsDta = *off and dtCYMD(scIsDta) > Dateto7(SystemDate());
        If erIsDta = *off and dtCYMD(scIsDta) < Dateto7(SystemDate());
        //***  End Chg  **** 08/14/25 *******************************
        //***  End Chg  **** 12/01/24 *******************************
           erIsDta = *on;
           msIsDta = 'WCR0592';
           isError = *on;
        Endif;

       // Validation Check for State Codes Inclusion/Exclusion.
        If scSTcodes = *Blanks and scSTCodIE <> ' ';
           erStCodIE = *on;
           msStCodIE = 'WCR3178';
        Endif;






      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateBat     e

       //===================================================================
       // Validate Cancellation batch Detail Record
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateBatDt   b
     d ValidateBatDt   pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

       erSelCXE = *off;
       msSelCXE = *blanks;
       erCXRsnE = *off;
       msCXRsnE = *blanks;

       // Select for Cancellation cannot be blank
       If scSelCXE = *blanks;
          erSelCXE = *on;
          msSelCXE = 'WCR0503';
          isError = *on;
       Endif;

       // Cancellation Reason should be valid

       If scCXRsnE = *blanks;
          erCXRsnE = *on;
          msCXRsnE = 'WCR0503';
          isError = *on;
       Endif;
       If scCXRsnE <> *blanks;
          Chain (CNco#:CNfnd:CNemp#:CNdiv:CNfyr) pel00;
          If %found(wdell);
             kkRtcd = ELrtcd;
             kkPfDT = ELpfdt;
             Chain (CNco#:kkRtcd) prs00;
             If %found(wmrsl);
                kkjrst = RSjrst;
                setgt (CNco#:kkjrst:scCXRsnE:kkPfDT) pcr00;
                readpe (CNco#:kkjrst:scCXRsnE) pcr00;
                If %found(wtcrl) and CRstat = 'I';
                   erCXRsnE = *on;
                   msCXRsnE = 'WCR0513';
                   isError = *on;
                Elseif not %found(wtcrl);
                   erCXRsnE = *on;
                   msCXRsnE = 'WCR0513';
                   isError = *on;
                Endif;
             Endif;
          Endif;
       Endif;

       // return error code
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateBatDt   e


       //===================================================================
       // Validtate Issue Mass Cancellation Batch
       //===================================================================
     p ValidIssMCx     b
     d ValidIssMCx     pi              n
     d isError         s               n   inz
     d isCancel        s               n   inz
      /Free

       isError = *off;
       isCancel = *off;

       erCXNDate = *off;
       msCXNDate = *blanks;
       erCXEfDate = *off;
       msCXEfDate = *blanks;

       erType = *off;
       msType = *blanks;
       erForm = *off;
       msForm = *blanks;
       erSrt1 = *off;
       msSrt1 = *blanks;
       erSrt2 = *off;
       msgSrt2 = *blanks;
       erAEB1 = *off;
       erAEB2 = *off;
       erAEB3 = *off;
       msAEB = *blanks;
       msgAEB1 = *blanks;
       msgAEB2 = *blanks;

       // Check Cancellation notice date
       scCXNDate = DtCheck(scCXNDate);
       If scCXNDate = *zero;
          msCXNDate = 'WCR0500';
          erCXNDate = *on;
          isError = *on;
       Endif;

       // Check Cancellation Effective date
       If scCXEfDate <> *zero;
          scCXEfDate = DtCheck(scCXEfDate);
          If scCXEfDate = *zero;
             msCXEfDate = 'WCR0500';
             erCXEfDate = *on;
             isError = *on;
          Endif;
       Endif;

       // "Address label type" and "Print labels ..." are mutually inclusive
       If (scAEB <> *blanks and scType = *blanks) or
          (scAEB = *blanks and scType <> *blanks);
          msAEB = 'WCR1083';
          msType = 'WCR1083';
          erAEB3 = *on;
          erType = *on;
          isError = *on;
       Endif;

       // Validate form selection.LASER not allowed with logo forms
       If scForm = 'L' and scType = 'L';
          msForm = 'WC30120';
          erForm = *on;
          isError = *on;
       Endif;

       // Check Primary sort
       If scSrt1 = *blanks;
          msSrt1 = 'WCR0503';
          erSrt1 = *on;
          isError = *on;
       Endif;
       // Not checking other values for pri sort as we are using dropdown

       // Check Secondary sort with primary sort
       If scSrt2 <> *blanks and scSrt1 <> 'AN' and scSrt1 <> 'AA';
          msgSrt2 = 'This choice only valid if "Primary sort" ' +
                     'is "Agent #" or "Agent Alpha"';
          erSrt2 = *on;
          isError = *on;
       Endif;

       // Check print label with primary sort
       If erAEB3 <> *on and (scAEB = 'A' or scAEB = 'B');
          If scSrt1 <> 'AA' and scSrt1 <> 'AN';
             msgAEB1 = '"Agents" or "Both" only allowed if '+
                     '"Primary Sort Order" is by "Agent #" or "Agent Alpha"';
             erAEB1 = *on;
             isError = *on;
          Endif;
       Endif;

       // Check print label with Address label form
       If (erAEB1 <> *on and erAEB3 <> *on)
          and (scAEB = 'E' or scAEB = 'B');
          If scForm = *blanks;
             msgAEB2 = '"Employers" or "Both" only allowed if "Employer ' +
                     'Address Label Form" entered.';
             erAEB2 = *on;
             isError = *on;
          Endif;
       Endif;

       //  Display Warning if CX Notice Date differs from current date
       //  by more than one week.
       If isError <> *on;
          date1 = %date(scCXNDate: *usa);
          date2 = %date();
          days_diff = *zeros;
          days_diff = %diff(date1: date2: *days);
          If days_diff > 7;
             isWarning = PopTwoBtn('The Cancellation Notice Date Entered +
              Differs from the Current Date by more than 7 Days. +
              Do you want to Process?':
              'Warning':'Yes':'No');
             Select;
             When isWarning = *On;
                //Do nothing
             When isWarning = *Off;
                isError = *on;
                isCancel = *on;
             EndSl;
          Endif;
       Endif;

       If isUseScript = *on and iscancel <> *on and isError <> *on;
          xxProcType = processType;
          xxMass = 'Y';

          In *Lock *dtaara;
          Clear svLDA;
          Chain (CCCo#:CCFnd) pex00;

          Select;
          When %found and EXCXCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXCXCD;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

          If xxSubmit = *off;
             isError = *on;
          Endif;
       Endif;

       // return error code
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;
      /End-Free
     p ValidIssMCx     e






      // utility procedure definitions
      /COPY SCOPYCOMPI
