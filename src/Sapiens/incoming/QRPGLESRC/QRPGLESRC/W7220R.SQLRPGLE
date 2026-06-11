     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/TITLE WC2696 - Prompt for S/R Worksheet
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7220R - Prompt for S/R Worksheet Report
     H*****************************************************************
     H* Revisions                                                     *
     H*                                                               *
     H* 07/17/23 C:65  Project: 146WKN                                *
     H*                Correct BSCD check prior to submission.        *
     H*                                                               *
     H* 05/04/23 C:333 Support: 99540 (InHouse)                       *
     H*                Monthly Self-Report prompt screen edit         *
     H*                                                               *
     H* 05/01/23 C:062 Project: WC2838                                *
     H*                Adjust the lda to match the additonal changes  *
     H*                for insured and agency info.                   *
     H*                                                               *
     H* 01/27/22 C:300 Support: 91124                                 *
     H*                Do not run the process if policy is having     *
     H*                weekly plan.                                   *
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*
     h* 06/16/21 C:180 Support 94517                                  *
     h*                -Fix sort edit                                 *
     h*                -Do not display "Div Blank for All" prompt     *
     h*                 when doing multiple run (DSPF changes only)   *
     h*
     H* 06/02/21 C:180 Support 94517                                  *
     H*                Fix period (Remove changes dated 10/23/20 as   *
     H*                the use of format DD/MM/YYYY was causing the   *
     H*                day part to be incorrectly reported as the     *
     H*                as the period month.                           *
     H*                Distinguish between single run from menu and   *
     H*                single run from toobar.                        *
     H*                Merge selected code dated 01/14/21 from        *
     H*                higher base version (Applicable to single      *
     H*                employer run when policy year is unknown).     *
     H*                                                               *
     H* 02/26/21 C:291 Support: 94569(102)                            *
     H*                Resolve spool file output to iDarts if         *
     H*                Scriptura is NOT turned on. Added new checkbox *
     H*                "Resolve to iDarts" that will only be visible  *
     H*                when Scriptura is not Turned on.               *
     H*                Add Visibility indicator on "Div blank for all"*
     H*                output field and change css class to           *
     H*                output-field to diplay proper font.            *
     H*                                                               *
     H* 02/18/21 C:62  Support: 94569(102)                            *
     H*                Open up division.                              *
     H*                Make scdiv a character field and  add the      *
     H*                verbiage next to it                            *
     H*                Add edit "Division must be Numeric or blanks"  *
     H*                Add Visibility indicator on "Div blank for all"*
     H*                output field and change css class to           *
     H*                output-field to display proper fon.            *
     H*                                                               *
     H* 01/14/21 C:33  Support: 93874                                 *
     H*                Determine frequency for single employer.       *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 10/23/20 C:298 Support: 91227                                 *
     H*                Corrected date when Self Reporting plan        *
     H*                is Monthly.                                    *
     H*                                                               *
     H* 10/20/20 C:298 Support: 91225                                 *
     H*                Edit message should not displayed if           *
     H*                any other division is self-reporting.          *
     H*                                                               *
     h* 12/14/20 C:180 Support 93267                                  *
     h*                - Add missing 'Resolve Form to Darts' field.   *
     h*                - Organize parameters to W7220C.               *
     h*                - Fix single employer run not generating output*
     h*                     * Fix incorrect reporting period          *
     h*                     * Fix iDarts parm from local data ar      *
     h*                - Fix amout due - Remove blanks and increase   *
     h*                  size from 7,2 to 9,2 to match parameters.    *
     H*                                                               *
     h*****************************************************************

     fW7220F    cf   e             workstn extfile('W7220F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     f*
     fWtabl     if   e           k disk    Rename(PBC01:PBC00)
     fWmfnl     IF   E           K DISK    Rename(PFN01:PFN00)
     fsmcol     IF   E           k DISK    Rename(PCO01:PCO00)
     fWmupl     IF   E           K DISK    Rename(PUP01:PUP00)
     fStctl1    if   e           k disk    Rename(pct01:pct00)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fwdell1    if   e           k disk    Rename(pel01:pel01)
     fwmaap     IF   E           K disk    Rename(paa01:paa0p)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     f*                                    INFDS(DBFDSD)
     fWmemp     if   e           k disk    Rename(pem01:pem0p)
     FWmexl     IF   E           K DISK    Rename(PEX01:PEX00)
     FWdehl     IF   E           K DISK    Rename(Peh01:Peh00)
     FStspl     IF   E           K DISK    rename(psp01:psp00)

     d isUseScript     s              1n   inz
     d isEstPol        s              1n   inz
     d ##key#          s             10
     d processType     c                   const('Self-Reporting Worksheet')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     dpsds           esds                  extname(szq1p)

     D*DBFDSD          DS
     D* DBRRN                397    400B 0

     D DBFDS3          DS
     D  DBRRN3               397    400B 0

     d WtSRWrksh       pr                  Extpgm('W7220R')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A
     d
     d WtSRWrksh       pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A

     d outCltId        s              3
     d outFrdt         s              8
     d outTodt         s              8
     d xxFrDt          s              8
     d xxToDt          s              8
     d outrpmy         s              6
     d outqueue        s             10
     d outmode         s              1
     d outemp#         s              9
     d outdiv          s              5
     d outrndt         s              8
     d outdue          s              9
     d outsrt          s              1
     d outfagt         s              5
     d outtagt         s              5
     d inttagt         s              5  0
     d outstdt         s              8
     d outendt         s              8
     d outfemp         s              9
     d outtemp         s              9
     d inttemp         s              9  0
     d outprmm         s              1
      *// Begin Add **** 12/14/20 *******************************
     d outDarts        s              1
      *// End   Add **** 12/14/20 *******************************
     d outprtcmpf      s              1
     d outindp         s              1
     d outall          s              1
     d inMode          s              1
     d strrptl         s             23
     d outsufx         s              3
     d outMass         s              1
     d inkey           s             10
     d inerror         s              1
     d intrrn          s              9  0
     d*inRptTool       s              1

     d charMsg         s            100
     d outRrn#         s              9
     d empRrn          s              9  0
     d inemprrn        s              9  0
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELFyr)
     d*** begin add ***  07/17/23  **************************************
     d xxDiv           s                   Like(ELdiv)
     d*** end add *****  07/17/23  **************************************
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
     d strCmntDsc      s             30    inz(*Blanks)
      *
     d incall1         s              1
     d inErr           s              1
     d outCo#          s              3
     d outFnd          s              3
     d sVLDA           DS          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d*** Begin Chg *** 05/01/23 **************************************
     d*xxBody***                    350    overlay(svlda:673)
     d xxBody                       200    overlay(svlda:673)
     d*** End   Chg *** 05/01/23 **************************************
     d outDocPath      s            300
     d outMessage      s            256
     d outTitle        s             50
     d outCnlHide      s              1    inz(' ')
     d OutModeOut      s              1    inz(' ')
     d isScript        s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     d massFlag        s              1
     d ##Stdt          s              8A
     d ##Endt          s              8A
     d ##Rpmy          s              6A
     d @@Rpmy          s              6A
     D                 ds
     D  xxRPMY                 1      8  0
     D  xxRPMN                 1      2  0
     D  xxRPDD                 3      4  0
     D  xxRPYR                 5      8  0
      ***** Beg Add ***** 10/23/20 ************************************
      ***** Beg Delete *** 06/02/21 ***********************************
     D*                ds
     D* intRPMY                1      8  0
     D* @@RPDD                 1      2  0
     D* @@RPMN                 3      4  0
     D* @@RPYR                 5      8  0
      ***** End Delete *** 06/02/21 ***********************************
      ***** End Add ***** 10/23/20 ************************************
       //***  Begin Add  ***  01/14/21  ****************
     dsysDt            s               d
     dsysDt7main       s              7  0
       //***  End   Add  ***  01/14/21  ****************
      ***** Beg Add *** 06/02/21 ***********************************
     d strType         s              1A
     d DateUSA         s               D   DATFMT(*USA)
     d period8a        s              8A
     d Date6a          s              6A   inz
     d Date8a          s              8A   inz
     d intY4           s              4  0
     d intMM           s              2  0
     d intDD           s              2  0
      ***** End  Add *** 06/02/21 ***********************************
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d***  Begin Chg  ***  01/14/21  ****************
     d*/copy sCopyPolPr
     d/include sCopyPolPr
     d***  End   Chg  ***  01/14/21  ****************
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter
     d/copy sPrModule
     d***  Begin Add  ***  01/14/21  ****************
     d/copy sPrSql
     d***  End   Add  ***  01/14/21  ****************
     d/copy sPrXML
     d/copy sPrGenPop

      * Employer Search Proc.
     d EmployerSrch    pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1    Const
     d inCall1                        1    Const
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

     d GetSuffix       PR                  extpgm('SA030R')
     d outsufx                        3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * S/R Worksheet Report Proc.
     d SRWrkShProc     pr

      * S/R Worksheet Report Submit
     d SrWrkShSbm      pr                  ExtPgm('W7220C')
     d outCo#                         3
     d outFnd                         3
     d outrpmy                        6
     d outqueue                      10
     d outmode                        1
     d outemp#                        9
     d outdiv                         5
     d outrndt                        8
     d outdue                         9
     d outsrt                         1
     d outfagt                        5
     d outtagt                        5
     d outstdt                        8
     d outendt                        8
     d outFEMP                        9
     d outTEMP                        9
     d outprmm                        1
      *** Begin Change *** 12/14/20 *********************************
     d*outprtcmpf                     1
     d outDarts                       1
      *** End   Change *** 12/14/20 *********************************
     d outindp                        1
     d outall                         1
      *** Begin Add    *** 12/14/20 *********************************
     d outprtcmpf                     1
      *** End   Add    *** 12/14/20 *********************************

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       //***  Begin Add  ***  01/14/21  ****************
       sysDt = SystemDate();
       sysDt7 = DateTo7(sysDt);
       sysDt8 = DateTo8(sysDt);
       sysDt7main = sysDt7;
       //***  End   Add  ***  01/14/21  ****************

       // Intialize the Screen fields
       LoadInVal();

       isUseScript = UseScriptura(processType);
       isScEnb = *off;
       If isUseScript;
          isScEnb = *on;
       Endif;

       //*** Beg Add ***** 02/26/21 **********************
       isUseSpoolOut = UseSpoolOut(processType);
       isSplEnb = *off;
       If isUseSpoolOut and not isUseScript;
          isSplEnb = *on;
          posRsToDrt = 600;
          scRsToDart = 'Y';
       Endif;
       //*** End Add ***** 02/26/21 **********************

       If Q1prms > 2 And inRrn <> ' ' And inRrn <> '000000000';
          isFld = *off;
          intRrn   = %Dec(inRrn:9:0);
          isPopUp  = '1';
          //isPop    = '1';
          isPolDtl   = '1';
          //*** Beg Add ***** 02/18/21 **************************
          visDivAll  = '0';
          //*** End Add ***** 02/18/21 **************************
          isFld   = '0';
          tipEmpDiv = *blank;
          Chain intRRN pel0p;
          If %Found(wdelp);
             scFnd     = ELfnd;
             scEmp#    = ELemp#;
             //*** Begin Chg *** 02/18/21 **********************************
             //*** scDiv     = ELdiv;
             scDiv     = %EditW(ELdiv:'   0 ');
             //*** End   Chg *** 02/18/21 **********************************
             //*** begin add ***  07/17/23  **************************************
             xxDiv = ELDiv;
             //*** end add *****  07/17/23  **************************************
             intFyr    = ELfyr;
             ##key# = %Editc(ELbscd:'X');
             Chain (ELco#:##key#) pbs01;
             If %found and BSFREQ = 'M';
                scRptHdg = 'Monthly Reporting Period MMYYYY';
                visAll = *on;
             Endif;
             If %found and BSFREQ = 'W';
                scRptHdg = 'Weekly Reporting Period MMDDYYYY';
                visAll = *off;
             Endif;
          Endif;
       Else;
          isPopUp = '0';
          isFld   = '1';
          //isPop   = '1';
          isPolDtl   = '0';
          //*** Beg Add ***** 02/18/21 **************************
          visDivAll  = '1';
          //*** End Add ***** 02/18/21 **************************
          tipEmpDiv = 'Blank for all.';
          scSngMult = 'M';
          scRptHdg = 'Reporting Period';
          visAll = *on;
       Endif;

       scPfDt = dtMDYY(ELPFDT);
       scPtDt = dtMDYY(ELPTDT);
       scFdTh = dtMDYY(ELPFDT);
       scTdTh = dtMDYY(ELPTDT);

       isEstPol = *off;
       //***** Beg Chg ***** 10/20/20 **************************
       //*****Setll (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       //*****Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       //*****Dow not %eof(WDELL1);
       Chain(n) (ELco#:ELfnd:ELfyr:ELemp#:Eldiv) pel01;
       If %found(wdell1);
       //***** End Chg ***** 10/20/20 **************************
          ##key# = %Editc(ELbscd:'X');
          Chain(n) (ELco#:##key#) pbs01;
          If %found and BSsefl = 'E';
             PopOneBtn('This Option is only for policies that self-report.':
              'Option Not Available');
             isEstPol = *on;
             return;
          Endif;
       //***** Beg Add ***** 01/27/22 **************************
          If %found and BSfreq = 'W';
             PopOneBtn('This option is not available for weekly self-reportin+
             g.':'Option Not Available');
             isEstPol = *on;
             return;
          Endif;
       //***** End Add ***** 01/27/22 **************************
       //***** Beg Chg ***** 10/20/20 **************************
          //*****Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       //*****Enddo;
       Endif;
       //***** End Chg ***** 10/20/20 **************************


       Return;

      /End-Free

     p ProgramStart    e

       //===================================================================
       // SRWrkShProc - S/R Worksheet Report Process Submit
       //===================================================================
     p SRWrkShProc     b
     d SRWrkShProc     pi

      /Free

       outco# = %EditC(intCo#:'X');

       If scFnd <> 0;
          outFnd = %EditC(scFnd:'X');
       EndIf;

       xxFrdt = %editc(scFdt:'X');
       xxToDt = %editc(scTDt:'X');

       //*** Begin Add *** 06/02/21 ***********************
       period8a = %editc(scRpMy:'X');
       Select;
       when %subst(period8a:1:4) = '0000';   //Format MM/YY
          If %int(%subst(period8A:7:2)) <= 39;
             @@RpMy = %subst(period8A:5:2) + '20' +
              %subst(period8A:7:2);
          Else;
             @@RpMy = %subst(period8A:5:2) + '19' +
              %subst(period8A:7:2);
          Endif;
       when %subst(period8A:1:2) = '00' and strType = 'M';     //Format MM/YYYY

       //***** Beg Chg *** 05/04/23 *************************************
          //***@@RpMy = %subst(period8A:3);
          If (%subst(Period8a:5:2) = '19') or (%subst(Period8a:5:2) = '20');
             @@RpMy = %subst(period8A:3);

          Else;                                                 //Format MMDDYY
             If %int(%subst(Period8a:7:2)) <= 39;
                @@RpMy = %subst(Period8a:3:2) + '20' + %subst(Period8a:7:2);
             Else;
                @@RpMy = %subst(Period8a:3:2) + '19' +  %subst(Period8a:7:2);
             Endif;
          Endif;
       //***** End Chg *** 05/04/23 *************************************

       when %subst(period8A:1:2) = '00' and strType = 'W';     //Format MM/DD/YY
          If %int(%subst(period8A:7:2)) <= 39;
             @@RpMy = %subst(period8A:3:2) + '20' +
              %subst(period8A:7:2);
          Else;
             @@RpMy = %subst(period8A:3:2) + '19' +
              %subst(period8A:7:2);
          Endif;
       Other;
        //Extract period mm/yyyy from mm/dd/yyyy entered value.
          xxRpmy = scRpmy;
          @@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
       Endsl;
       ##Rpmy = @@Rpmy;
       //*** End   Add *** 06/02/21 ***********************

       //For Single Run or Run from Toolbar

       //*** Begin Change *** 06/02/21 ***********************
       //If scEmp# <> 0 or (scFEMP = scTEMP and scFEMP <> 0);
       Select;
       //Single run from toolbar
       When scEmp# <> 0 and intRrn > *Zero;
       //*** End   Change *** 06/02/21 ***********************
       //*** begin chg ***  07/17/23  *******************************************
       //*****   Chain (intco#:scFnd:intFyr:scEmp#) pel01;
          Chain (intco#:scFnd:intFyr:scEmp#:xxDiv) pel01;
       //*** end chg *****  07/17/23  *******************************************
          If %found(WDELL1);
             ##key# = %Editc(ELbscd:'X');
             Chain (ELco#:##key#) pbs01;
             //***** Beg Delete ***** 06/02/21 ***********************
             //If %found and BSFREQ = 'M';     //Monthly
                //***** Beg Chg ***** 10/23/20 ***********************
                //*****xxRpmy = scRpmy;
                //*****@@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
                //*****intRpmy = scRpmy;
                //*****@@Rpmy = %Editc(@@RpMn:'X') + %Editc(@@RpYr:'X');
                //***** End Chg ***** 10/23/20 ***********************
                //**##Rpmy = @@Rpmy;

                // Prepare Start Date
                //##stDt = @@Rpmy;
                //##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);
                // Prepare End Date
                //##Endt = %Editc(DateTo8(GetLastDay
                //          (cvttodate(%Dec(##stDt:8:0)))):'X');
             //Endif;
                //***** End Delete ***** 06/02/21 ***********************
             If %found and BSFREQ = 'W';     //Weekly
                //*** Begin Delete 06/02/21 *********************************
                //xxRpmy = scRpmy;
                //@@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
                //##Rpmy = @@Rpmy;
                //*** End    Delete 06/02/21 *********************************
                // Prepare Start Date
                ##stDt = %EditC(dtPast(scRpmy:7):'X')  ;
                // Prepare End Date
                period8a = %Editc(scRpMy:'X');
                If %subst(period8a:1:2) = '00';  // mmddyy format
                   Date6a = %subst(%editc(scRpMy:'X'):3);
                   DateUSA  = %date(Date6a:*mdy0);
                   intMM   = %subdt(DateUsa:*months);
                   intdd   = %subdt(DateUsa:*days);
                   inty4   = %subdt(DateUsa:*years);
                   ##Endt  = %editc(intMM:'X') + %editc(intDD:'X') +
                    %editc(intY4:'X');
                Else;
                   ##Endt = %EditC(scRpmy:'X');
                Endif;

             //*** Begin Add *** 06/02/21 *******************************************
             Else;   //M = Monthly, or neither M nor W or not found = Default monthly
                // Prepare Start Date
                ##stDt = @@Rpmy;
                ##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);
                // Prepare End Date
                ##Endt = %Editc(DateTo8(GetLastDay
                          (cvttodate(%Dec(##stDt:8:0)))):'X');
             //*** End   Add *** 06/02/21 *******************************************
             Endif;

          //*** Begin Add *** 06/02/21 **********************************************
          Endif;
          //*** End   Add *** 06/02/21 **********************************************
          //*** Begin Delete *** 06/02/21 *******************************************
          //*** Begin Add *** 12/14/20 **********************************************
          //Else;
          //   xxRpmy = scRpmy;
          //   @@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
          //   ##Rpmy = @@Rpmy;
          //   // Prepare Start Date
          //   ##stDt = @@Rpmy;
          //   ##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);
          //   // Prepare End Date
          //   ##Endt = %Editc(DateTo8(GetLastDay
          //             (cvttodate(%Dec(##stDt:8:0)))):'X');
          //*** End Add *** 12/14/20 **********************************************
          //Endif;
       //Else;
          //*** End   Delete *** 06/02/21 *******************************************

          //Mass Run
          //***** Beg Chg ***** 10/23/20 ***********************
          //*****xxRpmy = scRpmy;
          //*****@@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');

          //*** Beg Change  ***** 06/02/21 ****************************
          //intRpmy = scRpmy;
          //@@Rpmy = %Editc(@@RpMn:'X') + %Editc(@@RpYr:'X');
          //***** End Chg ***** 10/23/20 ***********************
          When intRrn = 0;
          //Single or Mass Run from Menu
             If scEmp# <> 0 or (scFEMP = scTEMP and scFEMP <> 0);
                If GetFrequency() = 'Weekly';
                   // Prepare Start Date
                   ##stDt = %EditC(dtPast(scRpmy:7):'X')  ;
                   // Prepare End Date
                   period8a = %Editc(scRpMy:'X');
                   If %subst(period8a:1:2) = '00';  // mmddyy format
                      Date6a = %subst(%editc(scRpMy:'X'):3);
                      DateUSA  = %date(Date6a:*mdy0);
                      intMM   = %subdt(DateUsa:*months);
                      intdd   = %subdt(DateUsa:*days);
                      inty4   = %subdt(DateUsa:*years);
                      ##Endt  = %editc(intMM:'X') + %editc(intDD:'X') +
                       %editc(intY4:'X');
                   Else;
                      ##Endt = %EditC(scRpmy:'X');
                   Endif;
                Else;
                   ##Rpmy = @@Rpmy;
                   // Prepare Start Date
                   ##stDt = @@Rpmy;
                   ##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);
                   // Prepare End Date
                   ##Endt = %Editc(DateTo8(GetLastDay
                             (cvttodate(%Dec(##stDt:8:0)))):'X');
                Endif;
             Else;
          //*** End Change *** 06/02/21 *******************************
                ##Rpmy = @@Rpmy;                 //Default to Monthly
                // Prepare Start Date
                ##stDt = @@Rpmy;
                ##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);
                // Prepare End Date
                ##Endt = %Editc(DateTo8(GetLastDay
                          (cvttodate(%Dec(##stDt:8:0)))):'X');
       //*** Beg Change  ***** 06/02/21 ****************************
            Endif;
       //Endif;
       Endsl;
       //*** End Change  ***** 06/02/21 ****************************

       If scAll = 'Y';
          outstdt = xxFrDt;
          outendt = xxTodt;
       Else;
          outstdt = ##StDt;
          outendt = ##Endt;
       Endif;

       outrpmy = ##RpMY;

       outrndt = %editc(scRndt:'X');

       If scEmp# <> *Zeros;
          outEmp# = %editc(scEmp#:'X');
       Else;
          outEmp# = '000000000';
       Endif;

       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scDiv <> *Zeros;
       //***    outDiv = %editc(scDiv:'X');
       //*** Else;
       //***    outDiv = '00000';
       //*** Endif;
       If scDiv = *Blanks;
          outDiv = scDiv;
       Else;
          Monitor;
             intDiv = %Dec(%Trim(scDiv):5:0);
             outDiv = %EditC(intDiv:'X');
          On-Error;
             outDiv = '00000';
          EndMon;
       Endif;
       //*** End   Chg *** 02/18/21 ****************************************

       If scFemp <> *Zeros;
          outFemp = %editc(scFemp:'X');
       Else;
          outFemp = '000000000';
       Endif;

       If scTemp <> *Zeros;
          outTemp = %editc(scTemp:'X');
       Else;
            intTemp = *Hival;
            outTemp = %editc(intTemp :'X');
       Endif;

       If scFagt <> *Zeros;
          outFagt = %editc(scFagt:'X');
       Else;
          outFagt = '00000';
       Endif;

       If scTagt <> *Zeros;
          outTagt = %editc(scTagt:'X');
       Else;
          intTagt  = *Hival;
          outTagt = %editc(intTagt :'X');
       Endif;

       outSrt  = scSrt;
       outprmm = scPrmm;
       outprtcmpf = scPrtcmpf;
       outindp    = scIndp;
       outall = scAll;
       //*** Beg Add ***** 02/26/21 ***** 12/14/20 *************
       outdarts = scRsToDart;        //scDarts renamed to scRsToDart 02/26
       //*** End Add ***** 02/26/21 ***** 12/14/20 *************

       If scDue <> *Zeros;
          outDue  = %Editc(scDue:'X');
       Else;
       //*** Begin Change *** 12/14/20 ************************************
       //   outDue = '0000000';
          outDue = '000000000';
       //*** End   Change *** 12/14/20 ************************************
       Endif;

       outqueue   = strOutQ;
       outMode    = 'D';

      //*** Begin change *** 12/14/20 **************************************
      // SRWrkshSbm(outCo#:outFnd:outrpmy:outqueue:outmode:outemp#:
      //            outdiv:outrndt:outdue:outsrt:outfagt:outtagt:
      //            outstdt:outendt:outfemp:outtemp:outprmm:
      //            outprtcmpf:outindp:outall);

       SRWrkshSbm(outCo#:outFnd:outrpmy:outqueue:outmode:outemp#:
                  outdiv:outrndt:outdue:outsrt:outfagt:outtagt:
                  outstdt:outendt:outfemp:outtemp:outprmm:
                  outdarts:outindp:outall:outprtcmpf);
      //*** End   change *** 12/14/20 **************************************



      /End-Free

     p SRWrkShProc     e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdSRWksht;

       // Load Action panel with the Company Name.
       intCo#     = UserCo#(Q1User);
       scCompName = GetCompNam(intCo#);
       scFnd      = UserGrp(Q1User);

       intFnd     = scFnd;
       isFld = *off;
       isScEnb =  *off;
       ispopup = *off;
       scSngMult = ' ';
       scPfDt = 0;
       scPtDt = 0;
       scDue = 0;
       scSrt = *blanks;
       scFagt = 0;
       scTagt = 0;
       scFemp = 0;
       scTemp = 0;
       scPrmm = 'Y';
       scRndt = DateTo8(SystemDate());
       scPrtCmpF = 'N';
       scAll = 'N';
       scIndp = 'N';
       //*** Beg Add ***** 02/26/21 *************************
       scRsToDart = 'N';
       //*** End Add ***** 02/26/21 *************************

       // Get the client id
       GetClientID(outCltId);

       If outCltId = '132';
          scIndp = 'Y';
       Else;
          scIndp = 'N';
       Endif;

       // Retrieve Output Queue of the User.
       Chain(n) Q1User pup00;
       If %Found(Wmupl);
          strOutQ = UPoutQ;
       EndIf;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(Q1User:GrpSelect:GrpFrom:GrpWhere:'A');

      /End-Free

     p SetCriteria     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       If isEstPol = *on;
          Return;
       Endif;

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          setCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When selEmpSrch = *On;
             EmployerSrch();

          When BtnSubmit = *on;
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                Exsr OutControl;
                If xxSubmit = *off;
                   Leave;
                Endif;
                SRWrkShProc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

             SetCriteria();
             ProgramStart();

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;

          If Q1Prms > 2;
             Exfmt RcdSRWksht;
          Else;
             Write rcdComPnl;
             Write RcdTabSRW;

             Exfmt RcdSRWksht;

             Read rcdComPnl;
             Read RcdTabSRW;
          Endif;

       EndSl;

       Endsr;

       //===================================================================
       // Write Output Control Screen
       //===================================================================
       Begsr OutControl;

           xxSubmit = *on;

           If isUseScript;

              xxProcType = processType;
              If scEmp# <> 0 or (scFEMP = scTEMP and scFEMP <> 0);
                 xxMass = ' ';
              Else;
                 xxMass = 'Y';
              Endif;

              In *Lock *dtaara;

              Clear svLDA;

              Chain(n) (intco#:intFnd) pex00;
              If %found and EXMICD <> ' ';
                 xxDart = 'Y';
                 xxRMCD = EXMICD;
              Else;
                 xxDart = 'N';
                 xxRMCD = ' ';
              Endif;

              Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
             //***** xxSubmit = OutputWindow(xxProcType:xxMass);
             If intRRN <> 0;
                xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':intRRN);
             Else;
                xxSubmit = OutputWindow(xxProcType:xxMass);
             Endif;
          //*** end chg *****  08/13/21  ***********************************
      //*** Begin Add *** 12/14/20 ***********************************************
              //* Pull in any settings from local data area.
              In *Lock *dtaara;
              Out *dtaara;

              scRsToDart   = xxDart;   //scDarts renamed to scRsToDart 02/26
      //*** End Add   *** 12/14/20 ***********************************************

           Endif;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // EmployerSrch  - Employer/Div/year Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = scFND;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch('E':'2':outRrn#:inSelect);

       If outRrn# <> '000000000';
          empRrn=%Dec(outRrn#:9:0);
          Chain(n) empRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELemp#;
             intDiv  = ELdiv;
             intFyr  = ELfyr;

             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
             If %Found(Wmeml);
                scEmp# = EMemp#;
                //*** Begin Chg *** 02/18/21 *******************************
                //*** scDiv  = EMdiv;
                scDiv  = %EditW(EMdiv:'   0 ');
                //*** End   Chg *** 02/18/21 *******************************
             EndIf;
          EndIf;
       EndIf;

      /End-Free

     p EmployerSrch    e

     P***  Begin Add  ***  01/14/21  ********************
      //===================================================================
      // Determine the frequency
      //===================================================================
     pGetFrequency     b
     DGetFrequency     pi            25    varying

     DdecEmp#          s              9  0 inz
     DdecFyr           s              3  0 inz
     DstrFreq          s              1    inz
     DdsEmp            ds                  qualified
     D co#                            3  0
     D fnd                            3  0
     D emp#                           9  0
     D fyr                            3  0
      /Free
       Select;
       When scEmp# <> 0;
          decEmp# = scEmp#;
       Other;
          decEmp# = scFEmp;
       Endsl;
       Exec sql
        with
        currentFyr as
        (select ELco#, ELfnd, ELemp#, max(ELfyr) as fyr
         from WDELP
         where ELco# = :intCo# and ELfnd = :scFnd and ELemp# = :decEmp#
          and ELpfDt <= :sysDt7main
         group by ELco#, ELfnd, ELemp#)
        select BSfreq into :strFreq
         from WTBSP
         where BSkey =
         (select b.ELbsCd
           from currentFyr a
           join WDELP b on a.ELco#=b.ELco# and a.ELfnd=b.ELfnd
            and a.ELemp#=b.ELemp# and a.fyr=b.ELfyr
           fetch first 1 rows only);
       If strFreq = *blank;
          strFreq = 'M';
       Endif;
       Select;
       When strFreq = 'M';
          Return 'Monthly';
       When strFreq = 'W';
          Return 'Weekly';
       Endsl;
      /End-free
     pGetFrequency     e
     P***  End   Add  ***  01/14/21  ********************
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // initialize the error indicator
       //===================================================================
       isValid = *Off;
       msFnd      = *blanks;
       erFnd      = *off;
       msemp1     = *blanks;
       msEmp2     = *blanks;
       msEmp3     = *blanks;
       msEmp4     = *blanks;
       eremp1     = *off;
       erEmp2     = *off;
       erEmp3     = *off;
       erEmp4     = *off;
       msDiv1     = *blanks;
       //*** Begin Add *** 02/18/21 ****************************************
       erDiv      = *off;
       //*** End   Add *** 02/18/21 ****************************************
       erDiv1     = *off;
       msSrt      = *blanks;
       erSrt      = *off;
       msFrmE1    = *blanks;
       msFrmE2    = *blanks;
       erFrmE1    = *off;
       erFrmE2    = *off;
       msToE1     = *blanks;
       mSToE2     = *blanks;
       erToE1     = *off;
       erToE2     = *off;
       msFrmA1    = *blanks;
       msFrmA2    = *blanks;
       erFrmA1    = *off;
       erFrmA2    = *off;
       msToA1     = *blanks;
       msToA2     = *blanks;
       erToA1     = *off;
       erToA2     = *off;
       msGenRep1  = *blanks;
       msGenRep2  = *blanks;
       erGenRep1  = *off;
       erGenRep2  = *off;
       msFdt      = *blanks;
       msDt2      = *blanks;
       erFdt      = *off;
       erDt2      = *off;
       msDt3      = *blanks;
       erDt3      = *off;
       msTDt      = *blanks;
       msToDt2    = *blanks;
       erTDt      = *off;
       erToDt2    = *off;
       msRepM1    = *blanks;
       erRepM1    = *off;
       msCurBDt   = *blanks;
       erCurBDt   = *off;
       msDue      = *blanks;
       erDue      = *off;
       msIndp     = *blanks;
       erIndp     = *off;
       msPrtM     = *blanks;
       erPrtM     = *off;

       // Validation Check for Fund
       If scFnd   = *Zeros;
          erFnd   = *On;
          msFnd   = 'WC30622';
          isValid  = *on;
       EndIf;

       //*** Begin Add *** 02/18/21 ****************************************
       // Validate division
       If scDiv <> *Blanks;
          Monitor;
             intDiv = %Dec(%Trim(scDiv):5:0);
          On-Error;
             erDiv = *On;
             isValid  = *on;
          EndMon;
       EndIf;
       //*** End   Add *** 02/18/21 ****************************************

       //Validate only one set of fields entered
       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scEmp# <> *Zeros or scDiv <> *Zeros;
       If scEmp# <> *Zeros or scDiv <> ' ';
       //*** End   Chg *** 02/18/21 ****************************************
          If scSrt <> *Blanks;
             erSrt  = *On;
             msSrt  = 'WC30653';
             erEmp1    = *On;
             msEmp1  = 'WC30653';
             erDiv1  =   *On;
             msDiv1  = 'WC30653';
             isValid  = *on;
          EndIf;
       EndIf;

       //Validate ranges must be zero if single Emp request
       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scEmp# <> *Zeros or scDiv <> *Zeros;
       If scEmp# <> *Zeros or scDiv <> *Blank;
       //*** End   Chg *** 02/18/21 ****************************************
          If scFemp <> *Zeros or scTemp <> *Zeros;
             erFrmE1 = *On;
             MSFRME1 = 'WCR0590';
             erToE1 =  *On;
             msToE1 =  'WCR0590';
             erEmp1    = *On;
             msEmp1  = 'WCR0590';
             erDiv1 =  *On;
             msDiv1 =  'WCR0590';
             isValid  = *on;
          EndIf;
       EndIf;

       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scEmp# <> *Zeros or scDiv <> *Zeros;
       If scEmp# <> *Zeros or scDiv <> *Blanks;
       //*** End   Chg *** 02/18/21 ****************************************
          If scFagt <> *Zeros or scTagt <> *Zeros;
             erFrmA1 = *on;
             msFrmA1 = 'WCR0590';
             erToA1  = *on;
             msToA1  = 'WCR0590';
             erEmp1  =   *on;
             msEmp1  = 'WCR0590';
             erDiv1 =  *On;
             msDiv1 =  'WCR0590';
             isValid  = *on;
          EndIf;
       EndIf;

       //Validate either individual employer entered or mass run
       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scEmp# = *Zeros and scDiv = *Zeros
       If scEmp# = *Zeros and scDiv = *Blanks
       //*** End   Chg *** 02/18/21 ****************************************
        and scSrt = *Blanks;
          erSrt   = *On;
          msSrt   = 'WC30654';
       //*** Begin Add *** 06/16/21 *************************************
          If scSngMult <> 'M';
       //*** End   Add *** 06/16/21 *************************************
           erEmp1  = *On;
           msEmp1  = 'WC30654';
           erDiv1  = *On;
           msDiv1  = 'WC30654';
       //*** Begin Add *** 06/16/21 *************************************
          Endif;
       //*** End   Add *** 06/16/21 *************************************
          isValid = *on;
       EndIf;

       //Validations for single employer run
       //*** Begin Chg *** 02/18/21 ****************************************
       //*** If scEmp# <> *Zeros Or scDiv <> *Zeros;
       If scEmp# <> *Zeros And scDiv <> *Blanks;
       //*** End   Chg *** 02/18/21 ****************************************
       //validate the Emp/Div entered
          intFnd = scFnd;
          intEmp# = scEmp#;
          Chain(n)(intCo#:intFnd:intEmp#) pem00;
          If not %found(wmeml);
             erEmp1 = *On;
             msEmp1 = 'WCR0538';
             isValid  = *on;
          Endif;
          //*** Begin Chg *** 02/18/21 *************************************
          //*** intDiv = scDiv;
          //*** Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
          //*** If not %found(wmeml);
          //***    erDiv1 = *On;
          //***    msDiv1 = 'WC30063';
          //***    isValid  = *on;
          //*** Endif;
          If erDiv = *Off;
             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
             If not %found(wmeml);
                erDiv1 = *On;
                msDiv1 = 'WC30063';
                isValid  = *on;
             Endif;
          EndIf;
          //*** End   Chg *** 02/18/21 *************************************

       //final bill must have been run before submitting for single emp
          //*** Begin Chg *** 02/18/21 *************************************
          //*** Chain(n)(intCo#:intFnd:intEmp#) peh00;
          //*** If not %found(wdehl);
          //***    erEmp1 = *On;
          //***    msEmp1 = 'WC30339';
          //***    isValid  = *on;
          //*** Endif;
          //*** intDiv = scDiv;
          //*** Chain(n)(intCo#:intFnd:intEmp#:intDiv) peh00;
          //*** If not %found(wdehl);
          //***    erDiv1 = *On;
          //***    msDiv1 = 'WC30339';
          //***    isValid  = *on;
          //*** Endif;
          If erDiv = *Off;
             Chain(n)(intCo#:intFnd:intEmp#:intDiv) peh00;
             If not %found(wdehl);
                erDiv1 = *On;
                msDiv1 = 'WC30339';
                isValid  = *on;
             Endif;
          EndIf;
          //*** End   Chg *** 02/18/21 *************************************
       Endif;

       // Validate Minimum balance due
       If scDue < *Zeros;
          erDue = *On;
          msDue = 'WCR0547';
          isValid  = *on;
       EndIf;

       // Validate Include Deposit
       If scInDp <> 'Y' and scInDp <> 'N';
          erInDp = *On;
          msInDp = 'WCR0501';
          isValid  = *on;
       EndIf;

       // Validate the low range is not greater than the high range
       // Employer number
       If scFemp > scTemp;
          erFrmE1 = *On;
          msFrmE1 = 'WCR0750';
          isValid = *on;
       EndIf;

       // Agent Number
       If scFagt > scTagt;
          erFrmA1 = *On;
          msFrmA1 = 'WCR0750';
          isValid = *on;
       EndIf;

       If scAll <> 'Y' and scAll <> 'N';
          erGenRep1 = *On;
          msGenRep1 = 'WCR0501';
          isValid = *On;
       Endif;

       // Check From and To date for valid values
       // Validate the Policy effective 'from' date
       If scAll = 'Y';
          erFDt = ValidDate(scFDt:charMsg);
          If erFDt = *on;
             msFDt = charMsg;
             isValid  = *on;
          Endif;

       // Validate the Policy effective 'To' date
          erTDt = ValidDate(scTDt:charMsg);
          If erTDt = *on;
             msTDt = charMsg;
             isValid  = *on;
          Endif;

          If erFdt = *off and erTdt = *off;
             //"from date" cannot be greater than "to date"
             erTDt = AscendDates(scFDt:scTDt:charMsg);
             If erFDt = *on;
                erTDt = *on;
                msFDt = charMsg;
                msTDt = charMsg;
                isValid  = *on;
             Endif;
          Endif;

       // Reporting month/year cannot be entered.
          If scRpmy <> *Zeros;
             erRepM1 = *On;
             msRepM1 = 'WCR0590';
             erGenRep1 = *On;
             msGenRep1 = 'WCR0590';
             isValid  = *on;
          Endif;

       Else;

          If scFdt <> *Zeros or scTdt <> *Zeros;
             erGenRep1 = *On;
             msGenRep1 = 'WCR0590';
             erFdt = *on;
             msFdt = 'WCR0590';
             isValid  = *on;
          Endif;

          // Validate Reporting Month year
          //For Single Run or Run from Toolbar
          //*** Begin change *** 06/02/21 **************************
          //If scEmp# <> 0 or (scFEMP = scTEMP and scFEMP <> 0);
          Select;
          When scEmp# <> 0 and intRrn > 0; //Single run from Toolbar
          //*** End   change *** 06/02/21 **************************
             Chain (intco#:scFnd:intFyr:scEmp#) pel01;
             If %found(WDELL1);
                ##key# = %Editc(ELbscd:'X');
                Chain (ELco#:##key#) pbs01;
                //*** Begin Delete *** 06/02/21 ********************************
                //If %found and BSFREQ = 'M';
                //   xxRpmy = scRpmy;
                //   @@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
                //   If dt6Check(%Dec(@@Rpmy:6:0)) = *Zeros;
                //      erRepM1  = *On;
                //      msRepM1  = 'WCR0500';
                //      isValid  = *on;
                //   Endif;
                //Endif;
                //If %found and BSFREQ = 'W';
                //   If dtCheck(%Dec(scRpmy:8:0)) = *Zeros;
                //      erRepM1  = *On;
                //      msRepM1  = 'WCR0500';
                //      isValid  = *on;
                //   Endif;
                //Endif;
                //*** End  Delete *** 06/02/21 **********************************
                //*** Begin Add *** 06/02/21 ****************************************

                   If %found and BSfreq = 'W';
                       strType = BSfreq;
                   Else;
                      strType = 'M';
                   Endif;

                    If period8Check(scRpMy:strType) = *on;
                       erRepM1  = *On;
                       msRepM1  = 'WCR0500';
                       isValid  = *on;
                    Endif;
                //*** End   Add *** 06/02/21 ****************************************
             Endif;
          //For Mass Run
          //*** Begin Change *** 06/02/21 *********************************************
          //Else; //Multiple run from Menu  (Single or Multiple employers)
          //    xxRpmy = scRpmy;                        //Default Validate Monthly
          //    @@Rpmy = %Editc(xxRpMn:'X') + %Editc(xxRpYr:'X');
          //    If dt6Check(%Dec(@@Rpmy:6:0)) = *Zeros;
          //       erRepM1  = *On;
          //       msRepM1  = 'WCR0500';
          //       isValid  = *on;
          //    Endif;
          //Endif;
          When intRrn = *Zero;
             If scEmp# <> 0 or (scFEMP = scTEMP and scFEMP <> 0);  //Single run
                If GetFrequency() = 'Weekly';             //Validate Weekly
                   strType = 'W';
                Else;
                   strType = 'M';
                Endif;
             Else;
                strType = 'M';                            //Multiple run
             Endif;                                      //Default monthly

             If period8Check(scRpMy:strType) = *on;
                erRepM1  = *On;
                msRepM1  = 'WCR0500';
                isValid  = *on;
             Endif;
          Endsl;
          //*** End   Change *** 06/02/21 **********************************************
       Endif;

        //Validate current balance run date
        erCurBdt = ValidDate(scRndt:charMsg);
        If erCurBdt = *on;
           msCurBdt = charMsg;
           isValid  = *on;
        Endif;

        //Sort Selection Must Be "E" or "A" or "N"
        If scEmp# = 0;
           If scSrt <> 'E' and scSrt <> 'A';
              erSrt = *on;
              //**** Begin Change *** 06/16/21 *******************************
              //msSrt = 'WCR0846';
              msSrt = 'WCR0724';
              //**** End   Change *** 06/16/21 *******************************
              isValid  = *on;
           Endif;
        Endif;

        //Validate print missing months checkbox
        If scPrmm <> 'Y' and scPrmm <> 'N';
           erPrtM = *on;
           msPrtM = 'WCR0501';
           isvalid = *on;
        Endif;

      /End-Free
     p ValidPnl        e
      //*** Begin Add **** 06/02/21 ***************************************
       //*----------------------------------------------------------------
       //* Validate Weekly/Monthly Period                                *
       //*----------------------------------------------------------------
       Dcl-Proc period8Check;
        Dcl-Pi *N ind;
          inPeriod packed(8:0);
          inType char(1);
        End-Pi;
        Dcl-s MdyDate date(*mdy);
        Period8a = %Editc(inPeriod:'X');
        Select;
        When inType = 'W' and %subst(Period8a:1:4) = '0000';
           return *on;
        When inType = 'W' and %subst(Period8a:1:2)= '00';   //Format MM/DD/YY
           Monitor;
              Date6a = %subst(Period8a:3);
              MdyDate = %date(Date6a:*mdy0);
              return *off;
           On-Error;
              return *on;
           EndMon;
        When inType = 'W';     //Format MM/DD/YYYY
           Date8a = Period8a;
        When inType = 'M' and %subst(Period8a:1:4) = '0000';    //Format MMYY
          If %int(%subst(Period8a:7:2)) <= 39;
             Date8a = %subst(Period8a:5:2) + '0120' + %subst(Period8a:7:2);
          Else;
             Date8a = %subst(Period8a:5:2) + '0119' +  %subst(Period8a:7:2);
          Endif;
        When inType = 'M' and %subst(Period8a:1:2) = '00';    //Format MMYYYY
       //***** Beg Chg *** 05/04/23 *************************************
             //***Date8a = %subst(Period8a:3:2) + '01' + %subst(Period8a:5:4);
          If (%subst(Period8a:5:2) = '19') or (%subst(Period8a:5:2) = '20');
             Date8a = %subst(Period8a:3:2) + '01' + %subst(Period8a:5:4);
          Else;                                               //Format MMDDYY
             If %int(%subst(Period8a:7:2)) <= 39;
                Date8a = %subst(Period8a:3:2) + '0120' + %subst(Period8a:7:2);
             Else;
                Date8a = %subst(Period8a:3:2) + '0119' + %subst(Period8a:7:2);
             Endif;
          Endif;
       //***** End Chg *** 05/04/23 *************************************

        When inType = 'M' ; //Format MM/DD/YYYY
           Date8a = Period8a;
        Endsl;
        Monitor;
           MdyDate = %date(Date8a:*USA0);
           return *off;
        On-Error;
           return *on;
        EndMon;
       End-Proc;
       //*** End   Add **** 06/02/21 ***************************************
