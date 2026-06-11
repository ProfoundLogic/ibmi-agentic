     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTRFBTCH - Work with Return Funds Batches
     h dftname(WTRFBTCH)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 10/13/23                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2676 - Redesign of Work with Return Funds Batches*
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  05/13/25 C:333 Support: 118669 (FCC)                         *
      //                 Return Funds Screen Request.                  *
      //                 DSPF Changes:                                 *
      //                 - Added Columns EmpName,Adj,Ctst Aud,Agency#, *
      //                 Cust ID# (sflRFBtchD)                         *
      //                 - Renamed Column headings (sflRFBtchD).       *
      //                 - Renamed rcdPanel to rcdPanelRF.             *
      //                 - Added visibility for TabRFBatch.            *
      //                 - Changed overlay format for rcdRFBtch.       *
      //                 - Added visibility for HdgRFBatch.            *
      //                                                               *
      //  04/17/25 C:349 Support: 118885                               *
      //                 Clear svAmtE in case user cancels ADD batch   *
      //                 detail screen after receiving the error msg.  *
      //                 Initialize the ##co# variable with current    *
      //                 company value for proper file klist retrieval *
      //                                                               *
      //  04/15/24 C:300 Project: WC2676, Support: 115390              *
      //                 Return Funds Batch window should specify the  *
      //                 batch# being Deleted from the main grid       *
      //                 DSPF: Move top of rcdBatAdd record format     *
      //                 from 135 to 285                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fWTRFBTCH  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRFBtch:scSflRRN)
     f                                     Sfile(sflRFBtchD:scSflRRND)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
      * Return Funds Batch Control file
     fwxrnl     if a e           k disk    rename(prn01:prn00)
     f                                     infds(dbfds)
     fwxrnp     uf a e             disk    rename(prn01:prn0p)
      * Return Funds Batch Details file
     fwxrtl     uf   e           k disk    rename(prt01:prt00)
     fwxrtl1    if   e           k disk    rename(prt01:prt01)
     f                                     infds(dbfds2)
     fwxrtp     uf a e             disk    rename(prt01:prt0p)
     f                                     infds(dbfds5)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Employer Master file
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds4)
      * Employer Details File
     fwdelp     if   e             disk
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds3)
      * Premium Finance Co.
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
      * A/R file
     Fwmeal     if   e           k disk
      * Agent Master
     Fwmagl     if   e           k disk    rename(pag01:pag00)
      * Agency Billing File
     Fwdabl11   if   e           k disk    rename(pab01:pab11)
      * Return Premium Batch
     Fwxrtl5    if   e           k disk    prefix(l5_) rename(prt01:prt05)
     Fwxrtl6    if   e           k disk    prefix(l6_) rename(prt01:prt06)
      * Return Premium Check Request
     Fwar4l4    if   e           k disk
     Fwar4l10   if   e           k disk    rename(pr401:pr410)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

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

      // work variables
     d saveDT          s                   like(RTdate)
     d saveTM          s                   like(RTtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d ##co#           s              3  0
     d @@mode          s              1
     D indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d strmsgD         s            500    inz
     d strmsgid        s             10    inz
     d strErrMsg       s            256    inz
     d isDelResp       s               n   inz
     d outCo#          s              3    inz
     d outBat#         s              9    inz
     d outOutQ         s             10    inz
     d outMode         s              1    inz
     d*
     d svsflrrn        s              5  0 inz
     d svsflrrnd       s              5  0 inz
     d svPtflE         s              1    inz
     d svAmtE          s             11  2 inz
     d inInclude       s             10
     d inIncludeD      s             10
     d*
     d PremChk         s             80    dtaara(PREMCHK)
     d*

      // calling parameter for W0050R
     d ##kc            s              1
     d ##kl            s              1
     d ##mode          s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     d ##qte           s              1
     d ##view          s              1  0
     d ##rrn3          s              9  0

      // calling parameter for WA002R
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

      // save details of 1st record
     d co#_sav         s                   like(RNco#)
     d bat#_sav        s                   like(RNbat#)
     d btyp_sav        s                   like(RNbtyp)
     d ptyp_sav        s                   like(RNptyp)
     d com1_sav        s                   like(RNcom1)
     d com2_sav        s                   like(RNcom2)
     d com3_sav        s                   like(RNcom3)
     d RFDtlRrn        s              9  0
     d reload          s               n   inz(*off)

      // calling parameter for WtPolARInq
     d @@rrn           s              9
     d @@next          s             10
     d @@tabind        s              5
     d @@Qte           s              1
     d @@PostDt        s              8  0
     d @@PostAmt       s             11  2

      // calling parameter for WtAgtCfg
     d @@rrn1          s              9
     d @@mode1         s              1
     d @@next1         s             10
     d @@tabind1       s              5

      // Employer search DS
     d outSelect       ds                  likeds(dsSrchCrit)
     d intEmpRrn       s              9  0
     d outCall         s              1
     d outRrn#         s              9

      // Build Return Premium Batch (Add Auto Batch)
     dBuildBatch       pr                  ExtPgm('W0500C')
     d outCo#                         3    const
     d outBat#                        9    const
     d outOutQ                       10    const

      // Batch Edit Report (Option 18)
     dEdtReport        pr                  ExtPgm('W0500C18')
     d outCo#                         3    const
     d outBat#                        9    const
     d outOutQ                       10    const
 
      // search employer number
     d EmployerSrch    pr

      // Employer search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d outMode                        1
     d outCall                        1
     d outRrn#                        9
     d outSelect                           likeds(dsSrchCrit)

      // Generate Check Request (OPtion 25)
     dGenChkReq        pr                  ExtPgm('W0500C25')
     d outCo#                         3    const
     d outBat#                        9    const
 
      // Calculate Agency Billing balance (pending return premium checks)
     D GetBalAB        pr              n
 
      // Calculate A/R balance (pending return premium checks)
     D GetBalance      pr              n
 
      // Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      // Load Return Funds Batch Detail Grid
     d LoadRFBatch     pr
     d inIncludeBatD                 10    const

      // Load Return Funds Batch Master Grid
     d LoadRFBGrid     pr
     d inIncludeBat                  10    const

      * Main Processing
     d Main            pr

      * Process Addition of New Return Funds Batch
     d ProcessAdd      pr

      * Process Addition of Return Funds Batch Details
     d ProcessAddBDtl  pr

      * Process other Options for Return Funds batch master
     d ProcOther       pr

      * Process other Options for Return Funds batch detail
     d ProcOtherBD     pr

      * Process Performed Action from the action dropdown for Master Grid
     d ProcPerAct      pr

      * Process Performed Action from the action dropdown for Batch Details Grid
     d ProcPerActD     pr

      * Process the Row Click in Producers Grid
     d ProcRoWClk      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Action procedure.
     d ValidateAct     pr              n
     d strmode                        1

     * Validate Return Funds Batch Master Record procedure.
     d ValidateBat     pr              n
     d strmode                        1

     * Validate Return Funds Batch Detail Record procedure.
     d ValidateBatDtl  pr              n
     d strmode                        1

      // Check Policy current account Plan
     d WA002R          pr                  extpgm('WA002R')
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

      * Policy A/R Inquiry
     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d @@rrn                          9
     d @@mode                         1
     d @@next                        10
     d @@tabind                       5
     d @@Qte                          1
     d @@PostDt                       8  0
     d @@PostAmt                     11  2

      * Agent Inquiry
     d WtAgtCfg        pr                  ExtPgm('WTAGTCFG')
     d @@rrn1                         9
     d @@mode1                        1
     d @@next1                       10
     d @@tabind1                      5

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
     d/copy wprAgent

      //
      //   Key List for the Documents Table File
      //
     C     keyWXRTL1     klist
     C                   kfld                    co#_sav
     C                   kfld                    bat#_sav
      *
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

       If process;
          Main();
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       outMode  = 'E';
       outCall  = '2';
       clear dsSrchCrit;
       clear outSelect;
       dsEmpGrp  = scGroupE;
       outSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(outMode:outCall:outRrn#:outSelect);

       If outRrn# <> '000000000'
        and outRrn# <> '999999999';
          IntEmpRrn = %Dec(outRrn#:9:0);
          Chain intEmpRrn pel01;
          If %Found(Wdelp);
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
             If %Found(Wmeml);
                scEmpNoE   = EMemp#;
                scEmpNameE = EMname;
                scDivE     = EMdiv;
                scGroupE   = EMfnd;
                scCustE    = EMcust;
             EndIf;
          EndIf;
       EndIf;

       // Employer Detail Record does not exist in WDELP file for selected
       // Fund, Employer, division, then show error --
       // 'Policy Years information has not been established for this employer/'
       If outRrn# = '999999999';
          scEmpNoE   = *zeros;
          scEmpNameE = *Blanks;
          scDivE     = *zeros;
          scYearE    = *zeros;
          scGroupE   = *zeros;
          scCustE    = *Blanks;

          PopOneBtn('Policy Years Information has not been established for the-
           selected Employer/Division. Please select a different-
           Employer/Division.':'Employer Search Error');

       EndIf;

      /End-Free
     p EmployerSrch    e

       //===================================================================
       // GetBalAB - Calculate Agency Billing balance (pending return premium checks)
       //===================================================================
     P GetBalAB        b
     D GetBalAB        pi              n
     D balance         s             11  2

     C     keyWDABL11    klist
     C                   kfld                    ##co#
     C                   kfld                    scFeinE
     C                   kfld                    scModE
      /Free

       // Get balance from Agency Billing
       balance = 0;

       Setll keyWDABL11 pab11;
       Reade keyWDABL11 pab11;
       Dow not %eof(wdabl11);
          Select;
          When ABarty = 'A' or ABarty = 'B' or ABarty = 'D'
           or ABarty = 'N' or ABarty = 'R' or ABarty = 'V';
             balance = balance + ABamt;
          When ABarty = 'P' or ABarty = 'T';
             balance = balance - ABamt;
          Endsl;
          Reade keyWDABL11 pab11;
       Enddo;

       // Add other batch records to balance
       Setll keyWDABL11 prt06;
       Reade keyWDABL11 prt06;
       Dow not %eof(wxrtl6);
          If l6_RTbat# <> RTbat#;
             balance = balance + l6_RTamt;
          Endif;
          Reade keyWDABL11 prt06;
       Enddo;

       // Add unissued check request records from balance.
       Setll keyWDABL11 pr410;
       Reade keyWDABL11 pr410;
       Dow not %eof(war4l10);
          If R4stat <> 'Y';
             balance = balance + R4amt;
          Endif;
          Reade keyWDABL11 pr410;
       Enddo;

       If (balance + scAmtE) > 0;
          Return *On;
       Endif;

       Return *Off;

      /End-Free
     P GetBalAB        e

       //===================================================================
       // GetBalance - Calculate A/R balance (A/R balance - pending return premium checks)
       //===================================================================
     P GetBalance      b
     D GetBalance      pi              n
     D balance         s             11  2

     C     eaky          klist
     C                   kfld                    ##co#
     C                   kfld                    scGroupE
     C                   kfld                    scEmpNoE
     C                   kfld                    scDivE
     C                   kfld                    scYearE
      /Free

       // Get balance from A/R
       balance = 0;

       Setll eaky pea01;
       Reade eaky pea01;
       Dow not %eof(wmeal);
          Select;
          When EAarty = 'A' or EAarty = 'B' or EAarty = 'D'
           or EAarty = 'N' or EAarty = 'R' or EAarty = 'V';
             balance = balance + EAamt;
          When EAarty = 'P' or EAarty = 'T';
             balance = balance - EAamt;
          Endsl;
          Reade eaky pea01;
       Enddo;

       // Add other batch records to balance
       Setll eaky prt05;
       Reade eaky prt05;
       Dow not %eof(wxrtl5);
          If l5_RTbat# <> RTbat#;
             balance = balance + l5_RTamt;
          Endif;
          Reade eaky prt05;
       Enddo;

       // Add unissued check request records from balance.
       Setll eaky pr401;
       Reade eaky pr401;
       Dow not %eof(war4l4);
          If R4stat <> 'Y';
             balance = balance + R4amt;
          Endif;
          Reade eaky pr401;
       Enddo;

       If (balance + scAmtE) > 0;
          Return *On;
       Endif;

       Return *Off;

      /End-Free
     P GetBalance      e

       //===================================================================
       // Load Return Funds Batch Detail Grid
       //===================================================================
     p LoadRFBatch     b
     d LoadRFBatch     pi
     d inIncludeBatD                 10    const

      /free
       // Clear the subfile
       scSflRRND = 0;
       scSflDsp = *off;
       scSflDspD = *off;
       scSflClrD = *on;
       Write rcdRFBtch;
       scSflDsp = *on;
       scSflDspD = *on;
       scSflClrD = *off;
       scRowSel2 = 1;

       // Populate the subfile
       Setll keyWXRTL1 wxrtl1;
       Reade keyWXRTL1 wxrtl1;
       Dow not %eof(wxrtl1);
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
          scSelrf = RTselc;
          scGroup = RTFnd;
          scEmp = RTemp#;
          scDiv = RTdiv;
          scFyr = RTfyr;
          scAmt = RTamt;
          //*** Beg Add ***** 05/13/25 ***************************
          If RTemp# <> 0;
             Chain (RTco#:RTfnd:RTemp#:RTdiv) pem00;
             If %Found(Wmeml);
                scEmpNam = EMname;                      //Employer Name
             Endif;
          Endif;

          SetGt (RTco#:RTfnd:RTemp#:RTdiv:RTfyr:RTidt:RTitim) pea01;
          ReadE (RTco#:RTfnd:RTemp#:RTdiv:RTfyr) pea01;
          If %eof(wmeal);
             scAdj = '';
          Else;
             scAdj = 'Y';
          Endif;

          scCtstAud = RTcafl;
          scAgency# = RTagt#;
          scCustID# = RTcust;

          //*** End Add ***** 05/13/25 ***************************

          scPaid = *Blanks;
          Select;
          When RTptfl = 'A';
             scPaid = 'Agency';
          When RTptfl = 'E';
             scPaid = 'Employer';
          When RTptfl = 'G';
             scPaid = 'Group';
          When RTptfl = 'P';
             scPaid = 'Premium Finance Company';
          Other;
             scPaid = RTptfl;
          Endsl;

          // Set Visibility of Paid To OR FEIN fields in the detail box
          visbEmp = *On;
          // Account Current Batch Type OR Agency Payee Type
          If btyp_sav = 'A' or (btyp_sav = 'I' and ptyp_sav = 'A');
             visbEmp = *Off;
          Endif;

          // Set Visibility for Premium finance company
          visPremFin = *off;
          If RTpfin <> ' ';
             visPremFin = *on;
          Endif;
          scRrnRFD = dbrrn2;

          // Select Checkbox
          If inIncludeBatD = 'Include';
             scSelectRF = 'Y';
             scnxtchgD = *on;
          Else;
             scSelectRF = 'N';
             scnxtchgD = *off;
          Endif;

          Write sflRFBtchD;

          Reade keyWXRTL1 wxrtl1;
       Enddo;

       Clear inIncludeD;
       scSflRRND = 1;
       If svSflRRND <> 0;
          scSflRRND = svSflRRND;
          scRowSel2 = svSflRRND;
          svSflRRND = 0;
       Endif;

      /end-free
     p LoadRFBatch     e

       //===================================================================
       // Load Return Funds Batch Master Grid
       //===================================================================
     p LoadRFBGrid     b
     d LoadRFBGrid     pi
     d inIncludeBat                  10    const
     d svbat#          s                   like(RNbat#)

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflDspD = *off;
       scSflClr = *on;
       Write rcdRFBtch;
       scSflDsp = *on;
       scSflDspD = *on;
       scSflClr = *off;
       scRowSel = 1;
       svbat#   = 0;

       // Populate the subfile
       Setll *loval wxrnl;
       Read wxrnl;
       Dow not %eof(wxrnl);
          If RNbat# <> svbat#;
             svbat# = RNbat#;

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
             scBat# = RNbat#;
             scFgrp = RNfgrp;
             scTgrp = RNtgrp;
             scFaud = CvtToDate(RNfaud);
             scTaud = CvtToDate(RNtaud);
             scMin  = RNmin;

             scMtyp = *Blanks;
             Select;
             When RNmtyp = 'A';
                scMtyp = 'Auto';
             When RNmtyp = 'M';
                scMtyp = 'Manual';
             Other;
                scMtyp = RNmtyp;
             Endsl;

             scBtyp = *Blanks;
             Select;
             When RNbtyp = 'A';
                scBtyp = 'Account Current';
             When RNbtyp = 'D';
                scBtyp = 'Deposit';
             When RNbtyp = 'P';
                scBtyp = 'Premium';
             When RNbtyp = 'I';
                scBtyp = 'Independent Cash Distribution';
             Other;
             scBtyp = RNbtyp;
             Endsl;

             // Select Checkbox
             If inIncludeBat = 'Include';
                scSelect = 'Y';
                scnxtchg = *on;
             Else;
                scSelect = 'N';
                scnxtchg = *off;
             Endif;

             scRRN  = dbrrn;

             Write sflRFBtch;
             // Save 1st record information to load in Return Funds Batch details Grid
             If bat#_sav = 0;
                co#_sav  = RNco#;
                bat#_sav = RNbat#;
                btyp_sav = RNbtyp;
                ptyp_sav = RNbtyp;
                com1_sav = RNcom1;
                com2_sav = RNcom2;
                com3_sav = RNcom3;
                RFDtlRrn = scRRN;
             Endif;
          Endif;
          Read wxrnl;
       Enddo;

       Clear inInclude;
       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadRFBGrid     e

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

          // Load Return Funds Batch Master Grid
          If reload = *on;
             LoadRFBGrid(inInclude);
             reload = *off;
          endif;

          // Load Return Funds Batch Detail Grid
          LoadRFBatch(inIncludeD);

          // display screen
          Exsr ExfmtScreens;
          scFocus = '0';

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;
             inInclude = ' ';
             inIncludeD = ' ';
             bat#_sav = 0;
             svSflRRN = 0;
             reload = *on;

          // Catch Row click event
          When RowClick = *on;
             RowClick = *off;
             ProcRoWClk();           // Process the Row Click in Return Funds master Grid
             reload = *off;

          // Include All Master Grid
          When btnBatIncl = *on;
             btnBatIncl = *off;
             inInclude = 'Include';
             reload = *on;

          // Exclude All Master Grid
          When btnBatExcl = *on;
             btnBatExcl = *off;
             inInclude = ' ';
             reload = *on;

          // Include All Batch Details Grid
          When btnBatDInc = *on;
             btnBatDInc = *off;
             inIncludeD = 'Include';
             scFocus = '1';

          // Exclude All Batch Details Grid
          When btnBatDExc = *on;
             btnBatDExc = *off;
             inIncludeD = ' ';
             scFocus = '1';

          // Perform Action for Master Grid
          When btnPerAct = *on;
             btnPerAct = *off;
             ProcPerAct();
             If reload = *on;
                ProcRoWClk();           // Process the Row Click in Return Funds master Grid
             Endif;

          // Perform Action for Batch Details Grid
          When btnPerActD = *on;
             btnPerActD = *off;
             ProcPerActD();
             scFocus = '1';

          // Add Return Funds Batch Master
          When BtnRFBAdd = *On;
             BtnRFBAdd = *Off;
             ProcessAdd();

          // Add Return Funds Batch detail information
          When BtnRFBDtlA = *on;
             BtnRFBDtlA = *off;
             ProcessAddBDtl();
             scFocus = '1';

          Other;
             ProcOther();             // Process other Options for Return Funds batch Master
             ProcOtherBD();           // Process other Options for Return Funds batch detail

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
          pnlHeight = 800;
          APnlHeight = pnlHeight + 60;

          Write rcdComPnl;
          //*** Beg Chg ***** 05/13/25 ************************************
          // Write rcdPanel;
          // Exfmt rcdRFBtch;
          // Read rcdComPnl;
          // Read rcdPanel;
          tabVisInd = *On;
          Write rcdPanelRF;
          Exfmt rcdRFBtch;
          Read rcdComPnl;
          Read rcdPanelRF;
          //*** End Chg ***** 05/13/25 ************************************

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of New Return Funds Batch
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  xxbat#         s                   like(RNbat#) INZ
      /free

       Clear RcdBatAdd;
       mode = 'A';
       isexit = *off;
       BtnBatCan = *off;

       Dow BtnBatCan = *off and isexit = *off;

          scBatHdg = 'Add Return Funds Batch';

          // Set Choices and Choices values for Batch Type
          scBtypCH = *Blanks;
          scBtypVal = *Blanks;
          scBtypCH = 'Account Current,Premium';
          scBtypVal = 'A,P';
          If WCfl36 = 'Y';
             scBtypCH = 'Account Current,Premium,Deposit,+
              Independent Cash Distribution';
             scBtypVal = 'A,P,D,I';
          Endif;


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
         Select max(RNbat#) into :xxbat# from wxrnp
          where RNco# = :intCo#;

        RNco#   = intCo#;                // Company Number
        RNbat#  = xxbat# + 1;            // Batch Number
        RNmtyp  = scMtypad;              // Create Method
        RNbtyp  = scBtypad;              // Batch Type
        RNcafl  = scCaflad;              // Exclude Contested Audits
        RNfgrp  = scFgrpad;              // From Group
        RNtgrp  = scTgrpad;              // To Group
        RNfaud  = dtCYMD(scFaudad);      // From Transaction Date
        RNtaud  = dtCYMD(scTaudad);      // To Transaction Date
        RNmin   = scMinad;               // Minimum Return Amount
        RNptyp  = scPtypad;              // Payee Type
        RNcom1  = scCom1ad;              // Comment 1
        RNcom2  = scCom2ad;              // Comment 2
        RNcom3  = scCom3ad;              // Comment 3

        If scMtypad = 'A';
           RNbpf = 'N';                  // Batch Processing Flag
        Else;
           RNbpf = 'Y';                  // Batch Processing Flag
        Endif;

        RNuser  = q1user;                // User
        RNpgm   = q1pgm;                 // Program
        RNtime  = Timeto6(SystemTime()); // Time
        RNdate  = Dateto7(SystemDate()); // Date Mintained
        RNiusr  = q1user;                // Initial User
        RNipgm  = q1pgm;                 // Initial Program
        RNitim  = Timeto6(SystemTime()); // Initial Time
        RNidt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Return Funds Batch subroutine
       BegSr WrtBat;
        Clear prn0p;
        Exsr MoveScDb;
        Write prn0p;

        // Build Return Premium Batch
        If scMtypad = 'A';
           outCo# = %editc(RNco#:'X');
           evalr outBat# = %char(RNbat#);
           outOutQ = UPoutq;
           BuildBatch(outCo#:outBat#:outOutQ);
        EndIf;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcessAddBDtl - Process Addition Return Funds Batch Details
       //===================================================================
     P ProcessAddBDtl  B
     D ProcessAddBDtl  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     d  xxbat#         s                   like(RNbat#) INZ
      /free

       Clear RcdBatDtl;
       mode = 'A';
       isexit = *off;
       BtnBDtlCan = *off;
       RFBtchEdt = *off;

       //*** Begin Add ***** 04/17/25 *****************************
       Clear svAmtE;
       //*** End   Add ***** 04/17/25 *****************************

       Dow BtnBDtlCan = *off and isexit = *off;

          scBDtlHdg = 'Add Return Funds Batch Details';
          visBtcSvAd = *on;                    // Show Save/Add Net Button
          scCO#1 = intco#;
          //*** Begin Add ***** 04/17/25 *****************************
          ##co# = intco#;
          //*** End   Add ***** 04/17/25 *****************************

          // Set Visibility of Employer layout Container
          visbEmp = *On;
          // Account Current Batch Type OR Agency Payee Type
          If btyp_sav = 'A' or (btyp_sav = 'I' and ptyp_sav = 'A');
             visbEmp = *Off;
             layouttop = 635;
          Endif;

          scSelcE = 'Y';
          scCom1E = com1_sav;
          scCom2E = com2_sav;
          scCom3E = com3_sav;

          Exfmt RcdBatDtl;

          Select;

          // Save/Add Next Button
          When BtnBtcSvAd = *on;
             BtnBtcSvAd = *off;
             If ValidateBatDtl(mode);
                Exsr WrtBatDtl;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear RcdBatDtl;
                BtnBDtlCan = *off;
                RFBtchEdt = *off;
                scCO#1 = intco#;
             EndIf;

          When selEmpSrch = *on;
             erGroupE = *off;
             msGroupE = *blanks;
             erEmpNoE = *off;
             msEmpNoE = *blanks;
             EmployerSrch();

          When selPolYr = *on;
             erYearE = *off;
             msYearE = *blanks;
             @@rrn = *blanks;
             @@next = 'PostYear';
             If intco# <> 0 and scGroupE <> 0 and scEmpNoE <> 0;
                Chain(n) (intco#:scGroupE:scEmpNoE:scDivE) pel00;
                If %found(wdell);
                   @@rrn = %char(dbrrn3);
                Endif;
             Endif;

             @@mode = 'D';
             @@TabInd = *blanks;
             @@PostDt = *zeros;
             @@PostAmt = *zeros;

             // Call WtPolARInq for Policy Year
             If @@rrn <> *blanks;
                WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                           @@PostDt:@@PostAmt);
                If @@TabInd <> *blanks and @@next = 'PostYear';
                   scYearE = %int(@@TabInd);
                Endif;
             Endif;

          When selAgtSrch = *on;
             erFeinE = *off;
             msFeinE = *blanks;
             selAgtSrch = *off;

             @@rrn1  = *blanks;
             @@mode1 = 'S';
             @@next1 = 'GetAgency';
             @@TabInd1 = *blanks;
             WtAgtCfg(@@rrn1:@@mode1:@@next1:@@TabInd1);
             Monitor;
                scFeinE = %int(@@next1);
             On-error;
             Endmon;
             Monitor;
                scModE = %int(@@TabInd1);
             On-error;
             Endmon;

             scAgtNmE = *Blanks;
             Chain (intco#:scFeinE:scModE) pag00;
             If %found(wmagl);
                scAgtNmE = AGname;
             Endif;

          // Save Button
          When BtnBDtlSav = *on;
             BtnBDtlSav = *off;
             If ValidateBatDtl(mode);
                Exsr WrtBatDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnBDtlCan = *on;
             BtnBDtlCan = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;

        RTco#   = intCo#;                // Company Number
        RTbat#  = bat#_sav;              // Batch Number
        RTfnd   = scGroupE;              // Group
        RTemp#  = scEmpNoE;              // Employer Number
        RTdiv   = scDivE;                // Division
        RTfyr   = scYearE;               // Year
        // Get Employer Name
        If RTemp# <> 0;
           Chain (RTco#:RTfnd:RTemp#:RTdiv) pem00;
           If %Found(Wmeml);
              RTempn = EMname;                // Employer Name
           Endif;
        Endif;
        RTagt#  = scAgt#E;               // Agent Number
        RTagtn  = scAgtNameE;            // Agency Name
        RTfein  = scFeinE;               // FEIN
        RTmod   = scModE;                // Mod
        If visbEmp = *Off;               // Agency Biiling
           // Get Agency Number
           RTagt# = GetAgent#(RTco#:RTfein:RTmod:RTfnd);               // Agency Name

           // Get Agency Name
           Chain (RTco#:RTFein:RTMod) pag00;
           If %found(wmagl);
              RTagtn = AGname;             // Agency Name
           Endif;
        Endif;
        RTcom1  = scCom1E;               // Comment 1
        RTcom2  = scCom2E;               // Comment 2
        RTcom3  = scCom3E;               // Comment 3
        RTamt   = scAmtE;                // Return Fund Amount
        RTselc  = scSelcE;               // Selected
        If visbEmp = *Off;
           RTacfl  = 'Y';                   // Account Current Flag
        Else;
           RTacfl  = 'N';                   // Account Current Flag
        Endif;
        RTptfl  = scPtflE;               // Paid To

        Chain (RTco#:RTfnd:RTemp#:RTdiv:RTfyr) pel00;
        If %found(wdell);
           RTpfin  = ELpfin;                // Premium Fin Co.
           RTcafl  = ELctst;                // Contested Audit Flag
        Endif;

        RTarcd  = scARCodeE;             // A/R Cdeo
        RTbtyp  = RNbtyp;                // Batch Type Flag
        RTcust  = scCustE;               // Cutomer ID
      //RTsdfl  = scPtypad;              // Surplus Dist Flag


        RTuser  = q1user;                // User
        RTpgm   = q1pgm;                 // Program
        RTtime  = Timeto6(SystemTime()); // Time
        RTdate  = Dateto7(SystemDate()); // Date Mintained
        RTiusr  = q1user;                // Initial User
        RTipgm  = q1pgm;                 // Initial Program
        RTitim  = Timeto6(SystemTime()); // Initial Time
        RTIdt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Return Funds Batch subroutine
       BegSr WrtBatDtl;
        Clear prt0p;
        Exsr MoveScDb;
        Write prt0p;
       EndSr;

      /end-free
     P ProcessAddBDtl  E

       //===================================================================
       // ProcOther - Process other Options for Return Funds Batch
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBatAdd;
       isexit = *off;

       // Set Choices and Choices values for Batch Type
       scBtypCH = *Blanks;
       scBtypVal = *Blanks;
       scBtypCH = 'Account Current,Premium';
       scBtypVal = 'A,P';

       ReadC sflRFBtch;
       Dow not %eof;

          Select;
          When scRFBDlt = *On; // Processing for Delete of Return Funds Batch
             scRFBDlt = *Off;

             Chain(e) scRRN WXRNP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRRN WXRNP;
             mode = 'D';
             If ValidateAct(mode);
                Exsr MovDbSc;
                scBatHdg = 'Delete Return Funds Batch';

                Dow isexit = *off;
                   Write rcdBatAdd;
                   //***  Beg Chg  **** 04/15/24 *******************************
                   // isDelresp = PopTwoBtn('Do you want to delete this Return Fu+
                   // nds Batch?':'Confirm Delete':'Yes':'No');
                   strErrMsg = 'Do you want to Delete this Return Funds +
                    Batch# ' +  %trim(%editc(RNbat#:'3'));
                   isDelresp = PopTwoBtn(strErrMsg:'Confirm Delete':'Yes':'N+
                    o':440:90);
                   //***  End Chg  **** 04/15/24 *******************************
                   Select;
                   When isDelresp = *On;
                      Exsr DltBatDtl;       // Delete Batch Detail records
                      Delete prn0p;         // Delete Batch Master record
                      PopOneBtn('Return Funds Batch Deleted!':'Delete':'Ok');
                      isexit = *on;
                      reload = *on;
                      bat#_sav = 0;         // Reload the master grid from top
                      svsflrrn = 0;         // Reload the master grid from top
                   When isDelresp = *Off;
                      svsflrrn = scSflrrn;  //save rrn value
                      // If Delete link clicked on different (unselected) row
                      If scRowSel <> scSflRRN;
                         ProcRoWClk();
                         reload = *on;
                      Endif;
                      isexit = *on;
                   EndSl;
                EndDo;
             EndIf;

          Other;
             scSflRRN = svSflRRN;    // Restore the saved row selection number
             scRowSel = svSflRRN;    // Restore the saved row selection number

          Endsl;

          Unlock WXRNP;

          ReadC sflRFBtch;
       Enddo;

       If scSflRRN = 0;
          scSflRRN = scRowSel;    // Restore the saved row selection number
       Endif;

       // Subrotine to Delete Return Fund Batch Detail recods in WXRTL file
       BegSr DltBatDtl;
        Setll (RNco#:RNbat#) prt00;
        Reade (RNco#:RNbat#) prt00;
        Dow not %eof(wxrtl);
           Delete prt00;
           Reade (RNco#:RNbat#) prt00;
        Enddo;
       EndSr;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scMtypad = RNmtyp;
        scBtypad = RNbtyp;
        scCaflad = RNcafl;
        scFgrpad = RNfgrp;
        scTgrpad = RNtgrp;
        scFaudad = dtMDYY(RNfaud);
        scTaudad = dtMDYY(RNtaud);
        scMinad  = RNmin;
        scPtypad = RNptyp;
        scCom1ad = RNcom1;
        scCom2ad = RNcom2;
        scCom3ad = RNcom3;
       EndSr;


      /end-free
     P ProcOther       E

       //===================================================================
       // ProcOtherBD - Process other Options for Return Funds Details Batch
       //===================================================================
     P ProcOtherBD     B
     D ProcOtherBD     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdBatDtl;
       isexit = *off;

       ReadC sflRFBtchD;
       Dow not %eof;
          scFocus = '1';

          Select;
          When scRFBDEdit = *on;  // Processing for Edit of Return Funds Batch Details
             scRFBDEdit = *off;
             Chain(e) scRRNRFD WXRTP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             svsflrrnd = scSflrrnd;  //save rrn value
             Chain(n) scRRNRFD prt0p;
             Exsr Movdbsc;
             mode = 'C';

             scBDtlHdg = 'Edit Return Funds Batch Detail Information';
             scCO#1 = intco#;
             RFBtchEdt = *On;           // Dissable fields

             // Set Visibility of Employer layout Container
             visbEmp = *On;
             // Account Current Batch Type OR Agency Payee Type
             If btyp_sav = 'A' or (btyp_sav = 'I' and ptyp_sav = 'A');
                visbEmp = *Off;
                layouttop = 635;
             Endif;

             Dow isexit = *off;
                Exfmt rcdBatDtl;
                Select;
                When BtnBDtlSav = *on;
                   BtnBDtlSav = *off;
                   // Check if WXRTP updated since screen displayed (RF Batch Detail)
                   flgRcdUpdated=RcdUpdatedErr('WXRTP':dbrrn5:'RTdate':'RTtime':
                    'RTuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNRFD prt0p;
                      Exsr Movdbsc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateBatDtl(mode);
                         Chain scRRNRFD prt0p;
                         Exsr Movscdb;
                         Update prt0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                When BtnBDtlCan = *on;
                   BtnBDtlCan = *off;
                   isexit = *on;
                Endsl;
             Enddo;

          When scRFBDDlt = *On; // Processing for Delete of Return Funds Batch Details
             scRFBDDlt = *Off;

             Chain(e) scRRNRFD WXRTP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRRNRFD WXRTP;

             // Set Visibility of Employer layout Container
             visbEmp = *On;
             // Account Current Batch Type OR Agency Payee Type
             If btyp_sav = 'A' or (btyp_sav = 'I' and ptyp_sav = 'A');
                visbEmp = *Off;
                layouttop = 635;
             Endif;

             mode = 'D';
             Exsr MovDbSc;
             scBDtlHdg = 'Delete Return Funds Batch';

             Dow isexit = *off;
                Write rcdBatDtl;
                isDelresp = PopTwoBtn('Do you want to delete this Return Fu+
                nds Batch Details?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Delete prt0p;         // Delete Batch Master record
                   PopOneBtn('Return Funds Batch Details Deleted!':'Delet-
                    e':'Ok');
                   isexit = *on;
                   svsflrrnd = 0;         // Reload the master grid from top
                When isDelresp = *Off;
                   svsflrrnd = scSflrrnd;  //save rrn value
                   isexit = *on;
                EndSl;
             EndDo;

          // Process Emp Info option
          When scRFBDEInf = *on;
             scRFBDEInf = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNRFD prt0p;
             Chain (RTco#:RTfnd:RTemp#:RTdiv) pem00;
             If %found(wmeml);
                ##kc = *inkc;
                ##kl = *inkl;
                ##mode ='Z';
                ##rrn  = 0;
                ##rrn2 = 0;
                ##qte  = 'W';
                ##view = 0;
                ##rrn3 = dbrrn4;
                W0050R(##kc:##kl:##mode:##rrn:##rrn2:##qte:##view:##rrn3);
             Endif;

          // Process Premium Finance information.
          When scRFBDFInf = *on;
             scRFBDFInf = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNRFD prt0p;
             Chain (RTco#:RTfnd:RTemp#:RTdiv:RTfyr) pel00;
             If %found(wdell) and ELpfin <> *blanks;
                Clear rcdPrFnDtl;
                isexit = *off;

                scPrmFnHdg = 'Premium Finance Company Information';
                PrFnCoEdit = *on;       //disable non-editable field
                setgt (ELco#:ELfnd:ELpfin:ELpfdt) ppf00;
                readpe (ELco#:ELfnd:ELpfin) ppf00;
                Exsr MovDbSc2;

                Dow isexit = *off;
                   Exfmt rcdPrFnDtl;
                   // Cancel Button
                   If BtnPrFnCan = *on;
                      BtnPrFnCan = *off;
                      isexit = *on;
                   Endif;
                Enddo;
             Endif;


          // Process A/R inquiry information.
          When scRFBDARIq = *on;
             //*** Beg Add ***** 05/13/25 ************************
             tabVisInd  = *Off;
             Write rcdPanelRF;
             //*** End Add ***** 05/13/25 ************************
             scRFBDARIq = *off;
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain(n) scRRNRFD prt0p;
             Chain (RTco#:RTfnd:RTemp#:RTdiv:RTfyr) pel00;
             @@rrn = %char(dbrrn3);
             @@mode = 'D';
             @@next = 'ARInq';
             @@TabInd = *blanks;
             @@PostDt = *zeros;
             @@PostAmt = *zeros;
             WtPolARInq(@@rrn:@@mode:@@next:@@TabInd:@@Qte:
                           @@PostDt:@@PostAmt);


          Other;
             scSflRRND = svSflRRND;    // Restore the saved row selection number
             scRowSel2 = svSflRRND;    // Restore the saved row selection number

          Endsl;

          Unlock WXRTP;

          ReadC sflRFBtchD;
       Enddo;

       If scSflRRND = 0;
          scSflRRND = scRowSel2;    // Restore the saved row selection number
       Endif;

       // Move screen fields to data base file.
       Begsr Movscdb;
        RTamt   = scAmtE;                // Return Fund Amount
        RTptfl  = scPtflE;               // Paid To
        RTcust  = scCustE;               // Cutomer ID
        RTcom1  = scCom1E;               // Comment 1
        RTcom2  = scCom2E;               // Comment 2
        RTcom3  = scCom3E;               // Comment 3

        RTuser  = q1user;                // User
        RTpgm   = q1pgm;                 // Program
        RTtime  = Timeto6(SystemTime()); // Time
        RTdate  = Dateto7(SystemDate()); // Date Mintained

        saveDT = RTdate;
        saveTM = RTtime;
       Endsr;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scGroupE = RTfnd;
        scEmpNoE = RTemp#;
        scDivE = RTdiv;
        scYearE = RTfyr;
        scEmpNameE = RTempn;
        //scAgt#E = RTagt#;
        //scAgtNameE = RTagtn;
        scFeinE = RTfein;
        scModE = RTmod;
        scAgtNmE = RTagtn;
        scCom1E = RTcom1;
        scCom2E = RTcom2;
        scCom3E = RTcom3;
        scAmtE = RTamt;
        svAmtE = RTamt;
        scSelcE = RTselc;
        scPtflE = RTptfl;
        svPtflE = RTptfl;
        scPtypad = RTpfin;
        scCaflE = RTcafl;
        scARCodeE = RTarcd;
        scPtypad = RTbtyp;
        scCustE = RTcust;
        scPtypad = RTsdfl;

        saveDT   = RTdate;
        saveTM   = RTtime;
       EndSr;

       // Subrotine to move database fields into screen fields
       // for Premium Finance co. info
       BegSr MovDbSc2;
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

      /end-free
     P ProcOtherBD     E

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
     d inmod           s              1
     d strmsg          s            500    inz

      /Free

       ReadC sflRFBtch;
       Dow not %eof;
          If scSelect = 'Y';
             Chain(n) scRRN prn0p;

             Select;
             When scSelAct = 'Batch Edit Report';
                inmod = 'E';
                If ValidateAct(inmod);
                   strmsg = 'For Batch# ' +  %trim(%editc(scBat#:'3')) +
                    ' -- ' + 'Batch Edit Report is ready to Submit.';
                   PopOneBtn(strmsg:'Batch Edit Report');

                   outCo# = %editc(RNco#:'X');
                   evalr outBat# = %char(RNbat#);
                   outOutQ = UPoutq;
                   EdtReport(outCo#:outBat#:outOutQ);
                   reload = *on;
                EndIf;
             When scSelAct = 'Generate Check Request';
                inmod = 'G';
                If ValidateAct(inmod);
                   strmsg = 'For Batch# ' +  %trim(%editc(scBat#:'3')) +
                    ' -- ' + 'Generate Check Request is ready to Submit.';
                   PopOneBtn(strmsg:'Generate Check Request');

                   // Change Batch Processing flag to 'N'
                   Chain scRRN prn0p;
                   If %found(wxrnp);
                      RNbpf = 'N';
                      RNuser  = q1user;                // User
                      RNpgm   = q1pgm;                 // Program
                      RNtime  = Timeto6(SystemTime()); // Time
                      RNdate  = Dateto7(SystemDate()); // Date Mintained
                      Update prn0p;
                   EndIf;

                   outCo# = %editc(RNco#:'X');
                   evalr outBat# = %char(RNbat#);
                   GenChkReq(outCo#:outBat#);
                   reload = *on;
                EndIf;
             Endsl;
          Endif;

          ReadC sflRFBtch;
       Enddo;

      /End-Free
     p ProcPerAct      e

       //===================================================================
       // ProcPerActD - Process Performed Action from the action dropdown for Batch Details Grid
       //===================================================================
     p ProcPerActD     b
     d ProcPerActD     pi

      /Free

       ReadC sflRFBtchD;
       Dow not %eof;
          If scSelectRF = 'Y';
             svsflrrnd = scSflrrnd;  //save rrn value
             Chain scRRNRFD prt0p;

             Select;
             When scSelActD = 'Change Batch Selected Status';
                If RTselc = 'Y';
                   RTselc = 'N';
                Else;
                   RTselc = 'Y';
                EndIf;
                RTuser = q1user;                // User
                RTpgm  = q1pgm;                 // Program
                RTtime = Timeto6(SystemTime()); // Time
                RTdate = Dateto7(SystemDate()); // Date Mintained
                update prt0p;
             Endsl;
          Endif;

          ReadC sflRFBtchD;
       Enddo;

      /End-Free
     p ProcPerActD     e

       //===================================================================
       // ProcRoWClk - Process the Row Click in Return Funds master Grid
       //===================================================================
     P ProcRoWClk      B
     D ProcRoWClk      PI

      /free

       // process to load Retrun Funds details grid for selected row.
       Chain scsflRRN sflRFBtch;
       Chain(n) scRRN wxrnp;

       RFDtlRrn = scRRN;
       svsflRRN = scsflRRN;
       scrowsel = scsflRRN;

       co#_sav  = RNco#;
       bat#_sav = RNbat#;
       btyp_sav = RNbtyp;
       ptyp_sav = RNbtyp;
       com1_sav = RNcom1;
       com2_sav = RNcom2;
       com3_sav = RNcom3;

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
       Chain intCo# pco00;
       If %found(smcol) and COname <> ' ';
          scCompName = COname;
       Endif;
       scCo# = intCo#;

       // Retrieve user parameter
       Chain q1user pup00;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       // Check if Return Fund Module is Installed or Not
       If WCfl25 <> 'Y';
          PopOneBtn('The Return Fund Module has not been Installed. Please -
           contact Sapiens for more Information.':'Return Fund Process');

          processMe = *off;
          Return;
       Endif;


       // Choices for Action Dropdown for Master Grid
       If @@Mode = 'D';
          ChoSelAct = ' ';
       Else;
            // ChoSelAct = %trim(ChoSelAct) + ',Batch Edit Report,Generate Check +
            //  Request';
          ChoSelAct = '--- Select List Action ---';

          // Check Batch Edit Report Security
          If CheckSecLevel('RFBat_BEdt':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Batch Edit Report';
          EndIf;

          // Check Generate Check Request Security
          If CheckSecLevel('RFBat_GenC':UPtype:UPlvl);
             ChoSelAct = %trim(ChoSelAct) + ',Generate Check Request';
          EndIf;
       Endif;
       scSelAct = '--- Select List Action ---';

       // Choices for Action Dropdown for Batch Detail Grid
       If @@Mode = 'D';
          ChoSelActD = ' ';
       Else;
          ChoSelActD = '--- Select List Action ---';
          ChoSelActD = %trim(ChoSelActD) + ',Change Batch Selected Status';
       Endif;
       scSelActD = '--- Select List Action ---';

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
     d strmsgd         s            500    inz
     d strmsgid        s             10    inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.

       // Modes
       // 'E' -- Batch Edit Report        -- opt 18
       // 'G' -- Generate Check Request   -- opt 25
       // 'D' -- Delete                   -- opt 4


       // Don't allow any option if batch is processing.
        If RNbpf = 'N';
           strmsgid = 'WC31470';
           isError = *on;
        EndIf;

       // Check for Return Premium Lock, using PREMCHK Data Area
        If strmode = 'G' and isError = *off;
           Monitor;
              In *Lock PremChk;
           On-Error 431;
              strmsgid = 'WC30299';
              isError = *on;
           EndMon;

           Monitor;
              Out PremChk;
           On-Error;
           EndMon;
        Endif;





      // Return *on indicator when No error exists.
       strErrMsg = *blanks;
       If isError = *on;
          If strmsgid <> ' ';
             strMsgD = RtvMsgD('WMESGG':strmsgid);
             strErrMsg = 'Error on Batch# ' +  %trim(%editc(scBat#:'3')) +
              ' -- ' + %trim(strMsgD);
          Endif;
          PopOneBtn(strErrMsg:'Error');
          // As sfl grid is not reloaded, keep the row selection (scselect) active
          scnxtchg = *on;
          Update sflRFBtch;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAct     e

      //===================================================================
      // Validate the Return Funds Batch Master Record.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBat     b
     d ValidateBat     pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erMtypad = *off;
        msMtypad = *blanks;
        erBtypad = *off;
        msBtypad = *blanks;
        erBtypad2= *off;
        mgBtypad2= *blanks;
        erFgrpad = *off;
        msFgrpad = *blanks;
        erTgrpad = *off;
        msTgrpad = *blanks;
        erFaudad = *off;
        msFaudad = *blanks;
        erTaudad = *off;
        msTaudad = *blanks;
        erMinad = *off;
        msMinad = *blanks;

        If strmode = 'A';      //Add Mode
          // Create Method can't be Blank
           If scMtypad = *blanks;
              erMtypad = *on;
              msMtypad = 'WCR0503';
              isError = *on;
           Endif;

          // Batch Type can't be Blank
           If scBtypad = *blanks;
              erBtypad = *on;
              msBtypad = 'WCR0503';
              isError = *on;
           Endif;

          // Validate Batch Type based on Create Method
           If erMtypad = *off and erBtypad = *off;
              If WCfl36 = 'Y';
                 If scMtypad = 'A' and (scBtypad <> 'P' and scBtypad <> 'D');
                    erBtypad2 = *on;
                    mgBtypad2='Auto-created Batch Types must be ''P'' or ''D''';
                    isError = *on;
                 Endif;
              Else;
                 If scMtypad = 'A' and scBtypad <> 'P';
                    erBtypad2 = *on;
                    mgBtypad2 = 'Auto-created Batch Types must be ''P''';
                    isError = *on;
                 Endif;
              Endif;
           Endif;

          // Validate Group Range
           If scFgrpad > scTgrpad;
              erFgrpad = *on;
              msFgrpad = 'WCR0702';
              erTgrpad = *on;
              msTgrpad = 'WCR0702';
              isError = *on;
           Endif;

          // Validate From Transaction Date
           If scFaudad <> 0;
              erFaudad = ValidDate(scFaudad:charMsg);
              If erFaudad = *on;
                 msFaudad = charMsg;
                 isError = *on;
              Endif;
           Endif;

          // Validate To Transaction Date
           If scTaudad <> 0;
              erTaudad = ValidDate(scTaudad:charMsg);
              If erTaudad = *on;
                 msTaudad = charMsg;
                 isError = *on;
              Endif;
           Endif;

          // Validate Transaction Date Range
          // Dates must be in Ascending order
           If scFaudad <> 0 or scTaudad <> 0;
              If erFaudad = *off and erTaudad = *off;
                 erTaudad = AscendDates(scFaudad:scTaudad:charMsg);
                 If erTaudad = *on;
                    erFaudad = *on;
                    msFaudad = charMsg;
                    erTaudad = *on;
                    msTaudad = charMsg;
                    isError = *on;
                 Endif;
              Endif;
           Endif;

          // For Auto Create Method,
          // Validate Transaction Date Range and Minimum Return Amount
           If scMtypad = 'A';

              erFaudad = ValidDate(scFaudad:charMsg);
              If erFaudad = *on;
                 msFaudad = charMsg;
                 isError = *on;
              Endif;

              erTaudad = ValidDate(scTaudad:charMsg);
              If erTaudad = *on;
                 msTaudad = charMsg;
                 isError = *on;
              Endif;

              If scMinad <= 0;
                 erMinad = *on;
                 msMinad = 'WCR3183';
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
      // Validate the Return Funds Batch Detail Record.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBatDtl  b
     d ValidateBatDtl  pi              n
     d strmode                        1
     d isError         s               n   inz
     d LO              c                   'abcdefghijklmnopqrs-
     d                                     tuvwxyz'
     d UP              c                   'ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ'
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erGroupE = *off;
        msGroupE = *blanks;
        erEmpNoE = *off;
        msEmpNoE = *blanks;
        erDivE = *off;
        msDivE = *blanks;
        erYearE = *off;
        msYearE = *blanks;
        erAmtE  = *off;
        msAmtE  = *blanks;
        erPtflE = *off;
        msPtflE = *blanks;
        erCustE = *off;
        msCustE = *blanks;
        erFeinE = *off;
        msFeinE = *blanks;
        erModE = *off;
        msModE = *blanks;
        erARCodeE = *off;
        msARCodeE = *blanks;

        If strmode = 'A';      //Add Mode
           // Group cannot be Zero
           If scGroupE = *zeros;
              erGroupE = *on;
              msGroupE = 'WCR0502';
              isError = *on;
           Endif;

           // Employer cannot be Zero
           If scEmpNoE = *zeros and visbEmp = *On;
              erEmpNoE = *on;
              msEmpNoE = 'WCR0502';
              isError = *on;
           Endif;
           // Validate Employer
           If scEmpNoE <> *zero;
               Chain (intco#:scGroupE:scEmpNoE:scDivE) wmeml;
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

           // Year cannot be Zero
           If scYearE = *zeros and visbEmp = *On;
              erYearE = *on;
              msYearE = 'WCR0502';
              isError = *on;
           Endif;
           // check policy year valid for this Employer
           If erYearE = *off and erGroupE = *off and erEmpNoE = *off;
              If scYearE <> *zero or scEmpNoE <> *zero or scDivE <> *zero;
                 Chain (intco#:scGroupE:scEmpNoE:scDivE:scYearE) wdell;
                 If not %Found(wdell);
                    erYearE = *on;
                    msYearE = 'WCR0639';
                    isError = *on;
                 EndIf;
              EndIf;
           Endif;
           // check policy Account Current Plan
           If erYearE = *off and erGroupE = *off and erEmpNoE = *off;
              If scYearE <> *zero or scEmpNoE <> *zero or scDivE <> *zero;
                 @@co# = intco#;
                 @@fnd = scGroupE;
                 @@emp#= scEmpNoE ;
                 @@div = scDivE;
                 @@fyr = scYearE;
                 @@agt#= *zeros ;
                 @@anam = *blanks;
                 @@plan = '*' ;
                 @@date = *zeros;

                 WA002R(@@co#:@@fnd:@@emp#:@@div:@@fyr:@@agt#:@@anam:
                  @@prod:@@pnam:@@area:@@phon:@@plan:@@date);
                 If (@@plan <> 'A' and visbEmp = *Off)
                  or                       // Account Current Batch Type OR Agency Payee Typeee
                  (@@plan = 'A' and visbEmp = *On);
                    erEmpNoE = *on;
                    msEmpNoE = 'WC30283';
                    erDivE = *on;
                    msDivE = 'WC30283';
                    erYearE = *on;
                    msYearE = 'WC30283';
                    isError = *on;
                 EndIf;
                 // Agent number not found, or effective
                 If @@agt# = 0 and scPtflE = 'A';
                    erPtflE = *on;
                    msPtflE = 'WCR0530';
                    isError = *on;
                 Endif;

                 scAgt#E = @@agt#;
                 scAgtNameE = @@anam;

              EndIf;
           Endif;
        Endif;
        // End of Add Mode Validations

        // Return Fund Amount can not be Zero
        If scAmtE = *zeros;
           erAmtE = *on;
           msAmtE = 'WCR3183';
           isError = *on;
        Endif;
        // Check Return Fund Amount for Debit Situation
        If scAmtE <> *zeros and scAmtE <> svAmtE;
           svAmtE = scAmtE;
           // Calculate A/R balance
           If visbEmp = *On and GetBalance() = *on;
              erAmtE = *on;
              msAmtE = 'WC30298';
              isError = *on;
           Endif;
           // Calculate Agency Billing balance
           If visbEmp = *Off and GetBalAB() = *on;
              erAmtE = *on;
              msAmtE = 'WC30298';
              isError = *on;
           Endif;
        Endif;

        If visbEmp = *On;
           // Paid To can not be Blank
           If scPtflE = *blanks;
              erPtflE = *on;
              msPtflE = 'WCR0503';
              isError = *on;
           Endif;

           // Validate Cutomer ID
           If scCustE <> *Blanks;
               Chain (intco#:scGroupE:scEmpNoE:scDivE) wmeml;
               If %Found(wmeml);
                  If %XLATE(LO:UP:scCustE) <> %XLATE(LO:UP:EMcust);
                     erCustE = *on;
                     msCustE = 'WCR3221';
                     isError = *on;
                  EndIf;
               EndIf;
           Else;
           // Auto Populate Customer ID
               Chain (intco#:scGroupE:scEmpNoE:scDivE) wmeml;
               If %Found(wmeml);
                  If EMcust <> *Blanks;
                     scCustE = EMcust;
                  EndIf;
               EndIf;
           Endif;
        Endif;

        If visbEmp = *Off;
           // Agency FEIN can not be Zero
           If scFeinE = *zeros;
              erFeinE = *on;
              msFeinE = 'WCR0502';
              isError = *on;
           Endif;
           // Validate Agency FEIN
           If scFeinE <> *zeros;
              If ValidFein(scFeinE:scModE) = *Off;
                 erFeinE = *on;
                 msFeinE = 'WCR0505';
                 erModE  = *on;
                 msModE  = 'WCR0505';
                 isError = *on;
              Endif;
           Endif;

           // A/R Code can not be Blank
           If scARCodeE = *blanks;
              erARCodeE = *on;
              msARCodeE = 'WCR0503';
              isError = *on;
           Endif;
           // Validate A/R Code
           If scARCodeE <> *blanks;
              If ValidCode(scARCodeE) = *Off;
                 erARCodeE = *on;
                 msARCodeE = 'WCR0505';
                 isError = *on;
              Endif;
           Endif;
        Endif;

        If strmode = 'C';      //Change Mode

           // Valid change of Paid To
           If visbEmp = *On;
              If scPtflE <> svPtflE and scPtflE = 'A';
                 @@co# = intco#;
                 @@fnd = scGroupE;
                 @@emp#= scEmpNoE ;
                 @@div = scDivE;
                 @@fyr = scYearE;
                 @@agt#= *zeros ;
                 @@anam = *blanks;
                 @@plan = '*' ;
                 @@date = *zeros;

                 WA002R(@@co#:@@fnd:@@emp#:@@div:@@fyr:@@agt#:@@anam:
                  @@prod:@@pnam:@@area:@@phon:@@plan:@@date);
                 If @@plan = 'A';
                    erEmpNoE = *on;
                    msEmpNoE = 'WC30283';
                    erDivE = *on;
                    msDivE = 'WC30283';
                    erYearE = *on;
                    msYearE = 'WC30283';
                    isError = *on;
                 Endif;
                 If @@agt# = 0;
                    erPtflE = *on;
                    msPtflE = 'WCR0530';
                    isError = *on;
                 Endif;
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
     p ValidateBatDtl  e




      // utility procedure definitions
      /COPY SCOPYCOMPI

