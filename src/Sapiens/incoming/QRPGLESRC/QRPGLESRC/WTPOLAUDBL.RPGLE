     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE WTPOLAUDBL - Prompt for Audit Bill
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H*   Rewrite W7710R      C:309 for Project: WC2434               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS                                                     *
     h*                                                               *
     h* 08/01/25 C:65  Support: 119398   (Sunz)                       *
     h*                Change the spread discount window to indicate  *
     h*                all summarized divisions will run if the state *
     h*                rpt div is being run.  An additional message   *
     H*                will print on prompt screen to indicate this   *
     H*                as well - if a summarized division is being    *
     H*                run, or the state rpt division is being run    *
     H*                then all summarized divisions will run as well.*
     H*                This is a change to help support WC2984.       *
     H*                Additionally correct the logic for saving the  *
     H*                expiration date of div being run compared to   *
     H*                other divisions.                               *
     H*                                                               *
     H* 03/06/25 C:65  Project: WC2670 (Phw#118641 - Internal)        *
     H*                Correct the condition to set the mass flag for *
     H*                the call to OutputWindow to only be blank if   *
     H*                begin called for a specific employer and       *
     H*                1) running for single division or 2) only one  *
     H*                division or 3) combination bill enabled. These *
     H*                are only times when one email will go out.     *
     H*                Only call OutputWindow with a RRN if not mass  *
     H*                per the above changes.                         *
     H*                                                               *
     H* 09/23/24 C:291 Support: 117680                                *
     H*                Fix heading on Audit Bill Popup window. It was *
     H*                showing expiration date on Audit Bill popup    *
     H*                heading instead of CX date on cancelled policy.*
     H*                Fixed the msg from "division" to "divisions".  *
     H*                                                               *
     H* 07/24/24 C:300 Support: 116983                                *
     H*                Email Body Not Pulling Tokens                  *
     H*                Add RRN to OutputWindow procedure to populate  *
     H*                email addresses and email tokens               *
     H*                                                               *
     H* 05/03/24 C:062 Project: WC2670                                *
     H*                Correct how process determines mass run.       *
     h*                                                               *
     H* 04/04/24 C:291 Project: WC2670                                *
     H*                Assign proper remark code for Audit Bill and   *
     h*                Revised Audit bill for Scriptura output window.*
     h*                                                               *
     H* 03/14/24 C:291 Project: WC2670                                *
     H*                Ensure active payroll exist on atleast one     *
     H*                Division.                                      *
     H*                Added 2nd error message on scEmp#.             *
     H*                                                               *
     h* 03/23/23 C:291 Project: C150WFU                               *
     h*                Add Submit/Next button to the screen to allow  *
     h*                multiple single employer audits to be submitted*
     h*                without reopening the screen from the menu.    *
     h*                                                               *
     h* 05/05/22 C:291 Project: WC2670                                *
     h*                Add processing to display output control window*
     h*                for Audit Bills when Scriptura is requested.   *
     h*                                                               *
     h* 12/17/19 C:180 Project: 150WEH                                *
     h*                Add SR payroll Audit logic                     *
     h*                                                               *
     h* 10/07/19 C:65  Support: 83795 (LUBA)                          *
     h*                Correct defaulting of remark code to only      *
     h*                happen in error subroutine when the code has   *
     h*                not been cleared out.                          *
     h*                                                               *
     h* 08/30/19 C:136 Support: 80196 (SCS)                           *
     h*                Allow audit on previous years of multi-division*
     h*                policy, when current year is cancelled         *
     h*                                                               *
     h* 08/22/19 C:65  Support: 82271                                 *
     h*                Increase division count field to 5.0 size in   *
     h*                screen file.                                   *
     h*                                                               *
     h* 05/10/19 C:295 Support: 75256                                 *
     h*                Corrected Cancel button functionality.         *
     h*                Added Bypass Validation to cancel button-As it *
     h*                was requiring value for Audit bill option field*
     h*                                                               *
     h* 05/07/19 C:298 Support: 78719                                 *
     h*                Implemented java script changes on field       *
     h*                ScMode and code fix to auto populate A/R       *
     h*                transaction code.                              *
     h*                                                               *
     h* 02/12/19 C:300 Support: 76055 (CoveRisk)                      *
     h*                Getting WC30219 error on Policy Year when      *
     h*                Division 0 is cancelled, Division 1 is active  *
     h*                and Audit Bill is run on just Division 0       *
     h*                                                               *
     h* 12/28/18 C:300 Support: 75256 (RMLA)                          *
     h*                Correct edit that ensures all payrolls must    *
     h*                 be audited so that single division Cx Audits  *
     h*                 can process.                                  *
     h*          C:136 Ensure A/R code is populated for Calc Update   *
     h*                 and Print.                                    *
     h*                Change billing option to default to blank      *
     h*                                                               *
     h* 12/21/18 C:65  Support: 75426 (SCS)                           *
     h*                Mode value is not getting set for submit CL    *
     h*                based on option chosen - just passing '0' in.  *
     h*                                                               *
     h* 11/09/18 C:180 Support: 74463                                 *
     h*                Fix screen RcdAudBil overlay when clicking on  *
     h*                process accross all divisions                  *
     h*                                                               *
     h* 09/25/18 C:62  Project: 150WCS(73447)                         *
     h*                Correct defaults and edits.                    *
     h*                                                               *
     h* 08/08/18 C:62  Project: 150WCS                                *
     h*                Add all division prcoessing.                   *
     h*                                                               *
     h* 04/13/18 C:65  Support: 70121 (SCS)                           *
     h*                Chain back to division after counting divs.    *
     h*                                                               *
     h* 02/19/18 C:65  Support: 64488 (Luba)                          *
     h*                Ensure A/R code is not populated/passed to     *
     h*                submit when not running a calculate, update,   *
     h*                and print request.                             *
     h*                                                               *
     h* 12/13/17 C:136 Support: 51669                                 *
     h*                Fix post date edit                             *
     h*                                                               *
     h* 10/13/17 C:65  Project: 150WBU                                *
     h*                For client 150, only allow audit bill to run   *
   h*                if entire policy is expired and has audited    *
     h*                payroll records.  Additionally - fix coding    *
     H*                for everyone else to check if division is      *
     H*                expired when multiple divisions exist.  This   *
     H*                check should not have been bypassed for multi  *
     H*                division policies.                             *
     H*                                                               *
     H*****************************************************************
     fwtpolaudblcf   e             workstn extfile('WTPOLAUDBL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     f                                     infds(FIDS01)
     *
     fsmcol     if   e           k disk
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwmupl1    if   e           k disk    rename(pup01:pup01)
     fwmaxl     if   e           k disk    rename(pax01:pax00)
     fwtabl     if   e           k disk
     f                                     ignore(psy01)

     fwmfnl     if   e           k disk
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwdf1l     if   e           k disk    Rename(pf101:pf100)
     fwmldl1    if   e           k disk    rename(pld01:pld01)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmepl1    if   e           k disk    rename(pep01:pep01)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fstctl1    if   e           k disk    rename(pct01:pct01)
     fssylf     if   e           k disk    rename(ssy01:ssy00)
     f*** Begin Add *** 09/25/18 ********************************
     f                                     rename(swc01:swc00)
     f*** End   Add *** 09/25/18 ********************************
     f*** Begin Del *** 09/25/18 ********************************
     f**********                           ignore(swc01)
     f*** End   Del *** 09/25/18 ********************************
     f                                     ignore(spc01)
     f                                     ignore(sfb01)
     f                                     ignore(src01)
     f                                     ignore(sdr01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     f                                     ignore(sgl01)
     f                                     ignore(sp201)
      //*** Beg Add ***** 05/05/22 ***********************************
     fwsbll     if   e           k disk    Rename(pbl01:pbl00)
      //*** End Add ***** 05/05/22 ***********************************
      *
     d  commentCdDs    DS
     d  ##Comm1                1      2
     d  ##Comm2                3      4
     d  ##Comm3                5      6

     d conMsg          c                   const('This policy is under a retros-
     d                                     pective rating plan and has already -
     d                                     run settlement calculation/adjustmen-
     d                                     t. Due to this policy will not adjus-
     d                                     t total premium billed in A/R, but r-
     d                                     ather will only adjust the premium b-
     d                                     illed vs. retrospective premium bill-
     d                                     ed.')
     d

     d conTitle        c                   const('***********  Information Mess-
     d                                     age  ************')
     d conBtn1         c                   const('Continue')
     d conBtn2         c                   const('Exit')
     d conBtn3         c                   const('Cancel')
      *
     d btnRsp          s              1a   inz
     d curDate8        s              8  0
     d curDate7        s                   Like(ELptdt)
     d empRrn          s              9  0
     d EM_rrn          s              9  0
     d EL_rrn          s              9  0
     d Flag            s              1
     d intRrn          s              9  0
     d inSelect        s           1024
     d indMulti        s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intDiv1         s              5  0
     d intDays         s              3  0
     d inMode          s              1
     d inCall          s              1
     d inStart         s              5  0 inz
     d outCltId        s              3A
     d outRrn#         s              9
     d strBsCd         s              3
     d strSYK          s              2A
     d strCmntDsc      s            100A
     d strAcct         S              1
     d svCanclDt       s                   Like(ELptdt)
     d svCanclDt1      s                   Like(ELptdt)
     d svCanclDt2      s                   Like(ELptdt)
     d svCanclDt3      s                   Like(ELptdt)
     d svPolToDt       s                   Like(ELptdt)
     d svPolToDt1      s                   Like(ELptdt)
     d svARSM          s                   like(ELARSM)
     d svDiarFAcb      s                   like(scDiarFAcb)
     d svRevAudCb      s                   like(scRevAudCb)
     d svRemarkCd      s                   like(scRemarkCd)
      *** Begin Add *** 08/30/19 ********************************
     d svStRptPolExDt  s                   Like(ELptdt)
      *** End   Add *** 08/30/19 ********************************
     d ##Co#           s              3  0
     d ##Fnd           s              3  0
     d ##Emp#          s              9  0
     d ##COc#          s              3
     d ##Fndc          s              3
     d ##EmpC#         s              9
     d ##DivC          s              5
     d ##Fyr           s              3  0
     d ##FyrC          s              3
     d ##LDAL          s              3
     d ##LDAT          s              1
     d ##LVL           s              3
     d ##Name          s             80
     d ##Phone         s              7  0
     d ##Plan          s              1
     d ##Prod#         s              5  0
     d ##Subl          s              3
     d ##Subt          s              1
     d ##Type          s              1
     d ##Mode          s              1
     d ##Mode1         s              1  0
     d ##Area          s              3  0
     d ##Date          s              8  0
     d ##Div           s              5  0
     d ##Edn           s             80A
     d ##Exec          s              1
     d ##OutQ          S                   Like(Upoutq)
     d ##Agent#        s              5  0
     d ##AgentNm       s             60
     d ##ProdNm        s             60
     d ##AudTrnCd      s              2
     d ##RevAudCb      s              1
     d ##DDrStfCd      s              3
     d ##Cont          s              1
     d ##DefDrCd       s              5
     d ##RemarkCd      s              5
     d ##Fcommnt1      s             30
     d ##Fcommnt2      s             30
     d ##Fcommnt3      s             30
     d ##IDedBlCb      s              1
     d*** Begin Add *** 08/08/18 **************************************
     d ##AllDvCb       s              1
     d*** End   Add *** 08/08/18 **************************************
     d ##PostDat       s              8
     D*** Begin Add *** 09/26/18 *******************************
     D message         s            256a
     D msgTitle        s             50a
     D*** End   Add *** 09/26/18 *******************************
      //*** Beg Add ***** 03/14/24 ******************************************
     d flgPyrlFnd      s              1
      //*** End Add ***** 03/14/24 ******************************************
      *
     d*** Begin Add *** 09/25/18 ***************************************
     D                 ds
     D  ef                     1    035  0
     D                                     Dim(5)                               EFF.DATE
     D  EMefD1               001    007  0
     D  EMefD2               008    014  0
     D  EMefD3               015    021  0
     D  EMefD4               022    028  0
     D  EMefD5               029    035  0

     D                 ds
     D  cx                     1     35  0
     D                                     Dim(5)
     D  EMcaD1                 1      7  0
     D  EMcaD2                 8     14  0
     D  EMcaD3                15     21  0
     D  EMcaD4                22     28  0
     D  EMcaD5                29     35  0
     d*** End   Add *** 09/25/18 ***************************************

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy sPrSecure
     d/copy wPrEmpLr
     d/copy sPrDate
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy wtSearchDs
     d/copy sPrString
     d/copy sPrGenPop
     d*** Begin Add *** 09/25/18 *****************************
     d/copy wprEft
     d/copy wprBank
     d*** End   Add *** 09/25/18 *****************************
     d*** Beg Add ***** 05/05/22 *******************************************
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Audit Bill')
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
     d*** End Add ***** 05/05/22 *******************************************
      *

     * WA002R Call
     d CallWA002R      pr                  Extpgm('WA002R')
     d  ##Co#                         3  0
     d  ##Fnd                         3  0
     d  ##Emp#                        9  0
     d  ##Div                         5  0
     d  ##Fyr                         3  0
     d  ##Agent#                      5  0
     d  ##AgentNm                    60
     d  ##Prod#                       5  0
     d  ##ProdNm                     60
     d  ##Area                        3  0
     d  ##Phone                       7  0
     d  ##Plan                        1    Const
     d  ##Date                        8  0

     * W7710C Call
     d CallW7710C      pr                  ExtPgm('W7710C')
     d ##CoC#                         3    Const
     d ##FndC                         3    Const
     d ##FyrC                         3
     d ##EmpC#                        9
     d ##DivC                         5
     d ##PostDat                      8
     d ##AudTrnCd                     2
     d ##Comm1                        2
     d ##Comm2                        2
     d ##Comm3                        2
     d ##RevAudCb                     1
     d ##OutQ                        10
     d ##RemarkCd                     5
     d ##fCommnt1                    30
     d ##fCommnt2                    30
     d ##fCommnt3                    30
     d ##Cont                         1
     d ##DefDrCd                      5
     d ##DDrStfCd                     3
     d ##IDedBlCb                     1
     d*** Begin Add *** 08/08/18 *****************************
     d ##AllDvCb                      1
     d*** End   Add *** 08/08/18 *****************************
      *
     d callSRCCSC      pr                  Extpgm('SRCCSC')
     d  ##Type                        1
     d  ##Lvl                         3
     d  ##LDat                        1
     d  ##LDal                        3
     d  ##Exec                        1    Const
     d  Flag                          1

     * Calcualte Posting Date
     d CalPostDat      pr

     * Clear Errors
     d ClearErrors     pr

     * Polpulate Comment Code multi-select Box.
     d CmntCodLod      pr

     * Employer/Div Search
     d Emp_DivSearch   pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

     * Display screen
     d ExfmtScreens    pr

    d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     * Initialize
     d Initialize      pr

     * Load Screen fields
     d LoadScreen      pr

     * Main Process
     d MainProcess     pr

     * Check if there is any error on screen
     d NoError         pr              n

     * Program Start
     d ProgramStart    pr

     * Set Criteria
     d SetCriteria     pr

     * Submit Driver cl program
     d SubmitDrvCL     pr

     * Entry parameter
     d WTPolAudBlp     pr                  Extpgm('WTPOLAUDBL')
     d  @@KC                          1
     d  @@KL                          1
     d  @@Mode                        1
     d  @@Rrn                         9
      *
     d WTPolAudBlp     pi
     d  @@KC                          1
     d  @@KL                          1
     d  @@Mode                        1
     d  @@Rrn                         9
      *

      /Free
         ProgramStart();
         //*** Begin Add *** 09/25/18 ************************
         // Check to see if allowed to Run Audits
         If q1prms = 4; // Only from tool bar
            message = AuditEdits();
            If message <> ' ';
               msgTitle = 'Audit Error';
               PopOneBtn(message:msgTitle);
               Return;
            EndIf;
         EndIf;
         //*** End   Add *** 09/25/18 ************************
         LoadScreen();
         MainProcess();
         *InLr = *on;
      /End-Free

      //===================================================================
      // Audt Pre-Edits
      //===================================================================
     p AuditEdits      b
     d AuditEdits      pi           200a

     d currentDate     s              7  0
     d cancelDate      s              7  0 Inz(0)
     d i1              s              2  0 Inz(1)
     d tableKey        s             10    Inz(' ')

     d outCo#          s              3
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate         s              7
     d outPreNote      s              1

      /Free

       // Let's access the policy
       //
       Chain intRRN pel0p;

       // Option not available for claims only systems.
       //
        Chain ('WC') swc00;
        If WCfl08 = 'C';
           Return 'Audit bill is not available, system is set for claims ' +
           'only.';
        EndIf;

       // Get the cancel date if there is one
       currentDate = DateTo7(SystemDate());

       Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;

       i1 = %LookUp(0:ef);
       If i1 <> 0;
          i1 = i1 - 1;
       Else;
          i1 = 5;
       EndIf;

       If i1 <> 0;
          cancelDate = cx(i1);
       EndIf;

       If cancelDate > ELpTdt;
          cancelDate = 0;
       EndIf;

       //Is this SUNZ?
       //*** beg chg ***** 12/28/18 *******************
       //*** If outCltId = '150';
       If scAllDvCb = 'Y';
       //*** end chg ***** 12/28/18 *******************

          // Check if all divisions are expired and have
          // audited payrolls.
          Chain (ELco#:ELfnd:ELfYr:ELemp#) pel01;
          If %Found(wdell1);
             DoU %Eof(wdell1);

                If currentDate < ELpTdt And ELpFdt <> ELpTdt And
                   cancelDate = 0;
                   Return    'An audit bill may not be processed ' +
                             'before all divisions have expired';
                Else;
                   Chain (ELco#:ELfnd:ELfYr:ELemp#:ELdiv) pep01;
                   If %Found(wmepl1);
                      DoU %Eof(wmepl1);
                         If EPstat = 'A' And EPsaFl <> 'A';
                            Return 'All active payroll records for this ' +
                                   'policy must be audited in order to ' +
                                   'perform an audit bill.';
                         EndIf;
                         ReadE (ELco#:ELfnd:ELfYr:ELemp#:ELdiv) pep01;
                      EndDo;
                   EndIf;
                EndIf;
                ReadE (ELco#:ELfnd:ELfYr:ELemp#) pel01;
             EndDo;
          EndIf;
       Else;

          // No audit before cancellation of the policy
          If cancelDate <> 0 And currentDate < cancelDate And
             ELpFdt <> ELpTdt;
             Return 'An audit bill may not be processed before ' +
                    'the cancellation date of the policy.';
          EndIf;

          // No audit before the policy expiration date
          If cancelDate = 0 And currentDate < ELpTdt And
             ELpFdt <> ELpTdt;
             Return 'An audit bill may not be processed before ' +
                    'the policy expiration date.';
          EndIf;

          // Active payrolls must be audited
          Chain (ELco#:ELfnd:ELfYr:ELemp#:ELdiv) pep01;
          If %Found(wmepl1);
             DoU %Eof(wmepl1);
                If EPstat = 'A' And EPsaFl <> 'A';
                   Return 'All active payroll records for this ' +
                          'policy must be audited in order to ' +
                          'perform an audit bill.';
                EndIf;
                ReadE (ELco#:ELfnd:ELfYr:ELemp#:ELdiv) pep01;
             EndDo;
          EndIf;
       EndIf;

       // Check the user authority for audit bill in case of
       // large deductible.
       If ELbsCd <> 0;
          Chain (ELco#:ELfnd) pex00;
          tableKey = %EditC(ELbsCd:'X');
          Chain (ELco#:tableKey) pbs01;
          If %Found(wtabl) And BSlPgm = 'L';
             ##LDal = %EditC(EXlDal:'X');
             ##LDat = EXlDat;
             Chain Q1User pup00;
             If %Found(wmupl);
                ##type = UPtype;
                ##lvl  = %EditC(UPlvl:'X');
             EndIf;

             callSRCCSC(##Type:##Lvl:##LDat:##LDal:'1':Flag);
             If Flag = '1';
                Return 'Insufficient authority to process an audit ' +
                       'bill for a large deductible policy.';
             EndIf;
          EndIf;
       EndIf;

       // Let's Check Eft
       Exsr ChkEFT;

       // Refresh policy data
       Chain intRRN pel0p;

       // No errors
       Return ' ';

       //============================================================
       // ChkEFT - Check EFT setup.
       //============================================================
       Begsr ChkEFT;

          outEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X') + '  ';
          outDate = %Editc(DateTo7(SystemDate):'X');
          outCo#  = %editc(ELco#:'X');
          outFnd# = %editc(ELfnd:'X');

          If not GetEftStatus(outCo#:OutEftType:outFnd#:outEftKey:outDate:
                              outPreNote);

             If ELbscd <> *zero;
                tableKey = %EditC(ELbscd:'X');
                Chain (ELco#:tableKey) pbs01;
                If %Found(wtabl) and BSeftf = 'Y';
                   Return 'Employer EFT setup is required to run Audited ' +
                          'bill for this bill plan.';
                Endif;
             Endif;
          Endif;

       Endsr;

      /End-Free
     p AuditEdits      e

      //===================================================================
      // ClearErrors- Clear Error/Message fields
      //===================================================================
     p ClearErrors     b
     d ClearErrors     pi
      /Free

        msMode     = *Blanks;
        msFnd      = *Blanks;
        erFnd      = *Off   ;
        erMode     = *Off;
        msEmp#     = *Blanks;
        erEmp#     = *Off;
        msDiv      = *Blanks;
        erDiv      = *Off;
        msPolyear  = *Blanks;
        erPolyear  = *Off;
        msPostDat  = *Blanks;
        erPostDat  = *Off;
        msIDedBlCb = *Blanks;
        erIDedBlCb = *Off;
        msRevAudCb = *Blanks;
        erRevAudCb = *Off;
        msSingDvCb = *Blanks;
        erSingDvCb = *Off;
        msDiarFacb = *Blanks;
        erDiarFacb = *Off;
        msCmCodes  = *Blanks;
        erCmCodes  = *Off;
        msFcommnt  = *Blanks;
        erFcommnt  = *Off;
        msArcd     = *Blanks;
        erArcd     = *Off;
        ermsgFile  = *Blanks;
        msRemarkCd = *Blanks;
        erRemarkCd = *Off;
        msDDrStfCd = *Blanks;
        erDDrStfCd = *Off;
        msDefDrCd  = *Blanks;
        erDefDrCd  = *Off;
        //*** Beg Add ***** 03/14/24 *******************************
        erEmp#1    = *Off;
        msEmp#1    = ' ';
        //*** End Add ***** 03/14/24 *******************************

      /End-Free
     p ClearErrors     e

      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     P ExfmtScreens    b
     D ExfmtScreens    pi
      /Free

         Select;
         When TabIndex = 0;
            TabHeight  = 770;
            APnlHeight = TabHeight + 60;

            If isPopUp= *On;
               If indMulti = *On and BtnOK = ' ';
                  BtnOK = *Off;
                  Exfmt RcdAudBil2;
               Else;
                  Exfmt RcdAudBil ;
               EndIf;
            Else;
               Write rcdComPnl;
               Write RcdTbAudBl;
               If indMulti = *On and BtnOK = ' ';
                  BtnOK = *Off;
                  Exfmt RcdAudBil2;
               Else;
                  Exfmt RcdAudBil ;
               EndIf;
               Read  rcdComPnl;
               Read  RcdTbAudBl;
            EndIf;
         EndSl;
      /End-Free
     P ExfmtScreens    e

      //===================================================================
      // Initialize - Initialize screen fields
      //===================================================================
     P Initialize      b
     D Initialize      pi
      /Free

        scMode      =  *Blank;
        scEmp#      =  *Zeros;
        scDiv       =  *Zeros;
        scEmpName   =  *Blank;
        scPolYear   =  *Zeros;
        scSingDvCb  =  *Blank;
        scRevAudCb  =  'N';
        scIDedBlCb  =  *Blank;
        scDiarFAcb  =  *Blank;
        scARcd      =  *Blanks;
        scRemarkCd  =  *Blanks;
        scDefDrCd   =  *Blanks;
        scDivCount  =  *Zeros;
        scCmCodes   =  *Blanks;
        scFcommnt1  =  *Blanks;
        scFcommnt2  =  *Blanks;
        scFcommnt3  =  *Blanks;
        isSingleDv  =  *Off;
        isRemarkCd  =  *Off;
        isEmp#      =  *Off;
        isARcd      =  *Off;
        isDiarfaCb  =  *Off;
        //*** Begin Add *** 08/08/18 *******************************
        notAllDiv   =  *Off;
        //*** End   Add *** 08/08/18 *******************************

      /End-Free
     p Initialize      e

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi
      /Free

       // Process until Exit button is pressed.
        Dou ExitNow = *On ;

           ExfmtScreens();

           Select;
           When selEmpSrch = *On;
              selEmpSrch = *Off;
              Emp_DivSearch();

           When BtnGrpChg = *On;
              BtnGrpChg = *Off;
             // If user changes group calculate Posting Date again
              CalPostDat();

           When BtnSubmit = *On;
              BtnSubmit = *Off;
              ClearErrors();
             // Validate screen Fields, if No Error then submit driver CL program
              If NoError();
                 SubmitDrvCL();
                 leave;
              EndIf;

           //*** Beg Add ***** 03/23/23 **************************************************
           When BtnSbmtNxt = *On;
              BtnSbmtNxt = *Off;
              ClearErrors();
             // Validate screen Fields, if No Error then submit driver CL program
              If NoError();
                 SubmitDrvCL();
                 ProgramStart();
                 Loadscreen();
              EndIf;
           //*** End Add ***** 03/23/23 **************************************************

           When BtnCancel = *On;
              BtnCancel = *Off;
              leave;

           When BtnRefresh= *On;
              BtnRefresh= *Off;
              ProgramStart();
              ClearErrors();
              Loadscreen();

           EndSl;
        EndDo;
      /End-Free
     P MainProcess     e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     P ProgramStart    b
     D ProgramStart    pi

      /Free

       // Intialize Screen fields and indicators
        Initialize();

       // Load Action panel with Company Name
        scCompName = GetCompNam(userCo#(q1user));
        intCo#     = userCo#(q1user);

        If q1prms    = 4;
           intRrn    = %Dec(@@Rrn:9:0);
           ##Mode    = @@Mode;
           scMode    = @@Mode;
           //*** Begin Add *** 09/25/18 *****************************
           If scMode = ' ';
              scMode = '1';
           EndIf;
           //*** End   Add *** 09/25/18 *****************************
           isDivcount= *On;
           Monitor;
              ##Mode1= %Dec(@@Mode:1:0);
           On-Error;
              ##Mode1= *Zeros;
           EndMon;
           isMenu    = *On;
           isEmp#    = *Off;
           isPopUp   = *On;
           isGrpDisb = *On;             // Disable Group, Empoyer and Division fields if
                                        // called from Toolbar

           //*** Beg Add ***** 03/23/23 ******************************************
           scMenuCall = 'N';
           //*** End Add ***** 03/23/23 ******************************************
        Else;
           intRrn    = *Zeros;
           isDivcount= *Off;
           isMenu    = *Off;
           isEmp#    = *On;
           isPopUp   = *Off;
           isGrpDisb = *Off;
           //*** Beg Add ***** 03/23/23 ******************************************
           scMenuCall = 'Y';
           //*** End Add ***** 03/23/23 ******************************************
        EndIf;

       // Retrieve Client Id
        GetClientID(outCltID);
        If outCltiD = '071' or outCltiD = '139';
           isSingleDv = *On;
        EndIf;

        If outCltiD   = '071' ;
           scSingDvCb = 'Y'   ;
           scRevAudCb = 'N'   ;
        EndIf;


        If ##Mode = '3' ;
           isARcd     = *On;
        Else;
           isARcd     = *Off;
        EndIf;

        If ##Mode = '2' ;
           isDiarfaCb = *On;
        Else;
           isDiarfaCb = *Off;
        EndIf;

       // Check if Office is installed
        Chain ('SY') ssy00;
        If Not %Found(ssylf) or SYofif <> 'Y';
           isRemarkCd = *Off;
        Else;
           isRemarkCd = *On;
        EndIf;

        //*** Beg Add ***** 05/05/22 ***********************************
        //If Scriptura is being used, hide the iDarts/resolve fields as
        // those fields will be displayed on output window.
        isUseScript = UseScriptura(processType);
        If isUseScript;
           isRemarkCd = *Off;
        Endif;
        //*** End Add ***** 05/05/22 ***********************************

       // Retrieve user parameter
        Chain Q1User pup00;
        If %Found(wmupl);
           scEmp#    =  UPemp#;
           scDiv     =  UPdiv;
           strAcct   =  *Blanks;
           ##OutQ    =  UPoutQ ;
           //*** Begin Add *** 08/08/18 ************************************
           // Check to see if user has authority to all divisions
           If CheckSecLevel('AuditAllDv':UPtype:UPlvl);
              notAllDiv  = *Off;
           Else;
              notAllDiv  = *On;
              scAllDivTT = 'You are not currently authorized to run '      +
                           'audit billings across all divisions '          +
                           'regardless of the expiration or cancellation ' +
                           'date of the divisions.  This bill will '       +
                           'generate for policies based on the '           +
                           'expiration date of the policy year only.  '    +
                           'Cancelled divisions different than the '       +
                           'expiration date will need to be processed '    +
                           'separately.';
           EndIf;
           //*** End   Add *** 08/08/18 ************************************
        EndIf;


        SetCriteria();

      /End-Free
     p ProgramStart    e

      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi
      /Free

         GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
         scFnd = UPfnd;

      /End-Free
     p SetCriteria     e

      //===================================================================
      // SubmitDrvCL - Submit W7280C Premium Due Submit Program
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi
      /Free

         //*** begin add ***  12/21/18  ************************************
         If scMode <> ' ';
            ##mode1 = %int(scMode);
         Else;
            ##mode1 = 1;
            scarcd = '  ';
         Endif;
         //*** end add *****  12/21/18  ************************************
         If scRevAudCb = 'Y';
            ##Mode1 = ##Mode1 + 3;
         EndIf;
         ##RevAudCb = %Char(##Mode1);

         If scSingDvCb = 'Y';
            Select;
            When ##Mode1= 1 ;
               ##RevAudCb = 'A';

            When ##Mode1= 2 ;
               ##RevAudCb = 'B';

            When ##Mode1= 3 ;
               ##RevAudCb = 'C';

            When ##Mode1= 4 ;
               ##RevAudCb = 'D';

            When ##Mode1= 5 ;
               ##RevAudCb = 'E';

            When ##Mode1= 6 ;
               ##RevAudCb = 'F';
            EndSl;
         EndIf;

         ##CoC#      =  %Editc(intCo#:'X');
         ##FndC      =  %Editc(scFnd:'X');
         ##FyrC      =  %Editc(scPolYear:'X');
         ##EmpC#     =  %Editc(scEmp#:'X');
         ##DivC      =  %Editc(scDiv:'X');
         ##PostDat   =  %Editc(scPostDat:'X');
         ##AudTrnCd  =  scArcd;
         ##RemarkCd  =  scRemarkCd;
         ##Fcommnt1  =  scFcommnt1;
         ##Fcommnt2  =  scFcommnt2;
         ##Fcommnt3  =  scFcommnt3;
         ##Cont      =  *Blanks;
         ##DefDrCd   =  scDefDrCd;
         ##DDrStfCd  =  scDDrStfCd;
         ##IDedBlCb  =  scIDedBlCb;
         //*** Begin Add *** 08/08/18 **************************************
         ##AllDvCb   =  scAllDvCb;
         //*** End   Add *** 08/08/18 **************************************

       //*** Begin Chg *** 08/08/18 ****************************************
       //CallW7710C(##CoC#:##FndC:##FyrC:##EmpC#:##DivC:##PostDat:##AudTrnCd:
       // ##Comm1:##Comm2:##Comm3:##RevAudCb:##OutQ:##RemarkCd:
       // ##Fcommnt1:##Fcommnt2:##Fcommnt3:##Cont:##DefDrCd:##DDrStfCd:
       // ##IDedBlCb);
         CallW7710C(##CoC#:##FndC:##FyrC:##EmpC#:##DivC:##PostDat:##AudTrnCd:
          ##Comm1:##Comm2:##Comm3:##RevAudCb:##OutQ:##RemarkCd:
          ##Fcommnt1:##Fcommnt2:##Fcommnt3:##Cont:##DefDrCd:##DDrStfCd:
          ##IDedBlCb:##AllDvCb);
       //*** End   Chg *** 08/08/18 ****************************************

      /End-Free
     P SubmitDrvCL     e

      //===================================================================
      // NoError - Validate screen fields
      //===================================================================
     p NoError         b
     d NoError         pi              n

      *** beg add ***** 12/13/17 *************
     d intPostYear     s              4  0
     d intSysYear      s              4  0
     d intUsaDate      s              8  0
      *** end add ***** 12/13/17 *************
     d isError         s               n   Inz(*off)
      /Free

       // Audit Bill Option must be choosen
        If scMode  = *Blanks and isMenu = *Off;
           erMode  = *On;
           msMode  = 'WCR0503';
           isError =  *On;
        EndIf;

       // Fund must be entered
        If scFnd   = *Zeros;
           erFnd   = *On;
           msFnd   = 'WCR0503';
           isError =  *On;
        EndIf;

       // Default Remark code and Staff code when Final audit flag
       // switched to Blank
        If scDiarFAcb = *Blanks and svDiarFAcb = 'Y';
           scDDrStfCd = *Blanks;
           scDefDrCd  = *Blanks;
        EndIf;

       // Check if scRevAudCb was changed
        //*** begin chg ***  10/07/19  ******************************
        //*****If scRevAudCb <> svRevAudCb and scRevAudCb = 'Y' ;
        If scRevAudCb <> svRevAudCb and scRevAudCb = 'Y' and
         scRemarkCd <> ' ';
        //*** end chg *****  10/07/19  ******************************
           If isRemarkCd = *On;
              scRemarkCd = EXrfaf;
           Else;
           //*** begin chg ***  10/07/19  ******************************
           //*****   scRemarkCd = EXaud ;
              scRemarkCd = *blanks;
           //*** end chg *****  10/07/19  ******************************
           EndIf;

           If EX3rdb = 'Y';
              scRemarkCd = *Blanks;
           EndIf;
        EndIf;

        //*** Begin Add *** 12/17/19 ***********************************
        //Use self reported Audit code if not a revised audit.
        If isRemarkCd = *On and scRemarkCd = *blanks and
           ELaudsr = 'Y' and SCemp# <> *Zeros;
           scRemarkCd = EXaudsr;
        Endif;
        //*** End   Add *** 12/17/19 ***********************************

       // Policy Year must be greater than or equal to Zero
        If (scPolyear <= *Zeros and scEmp# <> *Zeros) or
         (scPolyear <= *Zeros and scDiv <> *Zeros);
           erPolYear =  *On;
           msPolYear = 'WCR0502';
           isError   =  *On;
        Endif;

       // Validate Employer/Division if not equal to Zero
        If scDiv <> *Zeros or scEmp# <> *Zeros;
           If ValidEmp(scEmp#:scDiv:EM_rrn:scFnd:intCo#) = *Off;
              erEmp#    =  *On;
              msEmp#    = 'WCR0538';
              erDiv     =  *On;
              msDiv     = 'WCR0538';
              isError   =  *On;
           EndIf;
          // Validate Employer/Division/Policy Year
           If scPolYear <> *Zeros;
              If ValidEmpYr(scPolYear:scEmp#:scDiv:EL_rrn:scFnd:intCo#) = *Off;
                 erPolYear =  *On;
                 msPolYear = 'WCR0639';
                 isError   =  *On;
              EndIf;
           EndIf;
        EndIf;

       // Verify if Policy is Large Deductible, based on Billing Schedule code
        Chain (intCo#:scFnd:scEmp#:scDiv) pel00;
        strBSCD = %Char(ELbscd);

        Chain (intCo#:strBSCD) pbs01;
        If %Found(wtabl) and BSlpgm = 'L';
           Chain (intCo#:scFnd:scEmp#:scDiv:scPolYear) pld01;
           If %Found(wmldl1);
              Chain (q1User) pup00;
              Chain (intCo#:scFnd) wmexl;
              ##Type = UPtype;
              ##Lvl  = %Char(UPlvl);
              ##LDat = EXldat;
              ##LDal = %Char(EXldal);

              callSRCCSC(##Type:##Lvl:##LDat:##LDal:'1':Flag);

              If Flag = '1';
                 msEmp#='WC40074';
                 erEmp#= *On;
                 isError=*On;
              EndIf;
           EndIf;
        EndIf;

       // Validate Single Division mode Flag
        If isSingleDv = *On ;
           If scSingDvCb <> 'Y' and scSingDvCb <> *Blanks;
              erSingDvCb = *On;
              msSingDvCb = 'WCR0571';
              isError    =  *On;
           EndIf;

           If scSingDvCb = 'Y' and scEmp# = *Zeros;
              erSingDvCb = *On;
              msSingDvCb = 'WCR1083';
              erEmp#     = *On;
              msEmp#     = 'WCR1083';
              isError    =  *On;
           EndIf;
        EndIf;

       // Make sure All Audited Payrolls have been entered
       // for the specified Employer
        If scEmp# <> *Zeros;
           Chain (intCo#:scFnd:scEmp#:scDiv:scPolYear) pel00;
           svPolToDt = ELptdt;

           scDivCount = 0;
          // Get Division Count
           Setll (intCo#:scFnd:scPolYear:scEmp#) pel01;
           Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
           Dow Not %Eof(wdell1);
              If ELpfdt <> ELptdt;
                scDivCount += 1;
              EndIf;
              Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
           EndDo;

        //*** begin add ***  04/13/18  ************************************
           Chain (intCo#:scFnd:scEmp#:scDiv:scPolYear) pel00;
           //*** Beg Add ***** 03/23/23 ***********************************
           Chain (intCo#:scFnd:scEmp#:scDiv) pem00;
           //*** End Add ***** 03/23/23 ***********************************
        //*** end add *****  04/13/18  ************************************
           If scDivCount  > 1;
              indMulti = *On;
              svCanclDt = GetCxDate(intCo#:scFnd:scEmp#:scDiv);

             //Default Policy To Date For Flat Cancellations.
           //*** begin chg ***  08/01/25  ***************************************
           //*****   If svCanclDt = *Zeros;
           //*****      If EMcad1 < ELpfdt and EMcad1 <> *Zeros and EMefd1 <> *Zeros;
           //*****         svPolToDt = ELpfdt;
           //*****      EndIf;
           //*****   Else;
           //*****      svPolToDt  = svCanclDt;
           //*****   EndIf;
              If svCanclDt <> *Zeros and svCanclDt <= ELPtdt;
                 svPolToDt  = svCanclDt;
                 If svCanclDt < ELPfdt;
                    svPolToDt = ELPfdt;
                 EndIf;
              EndIf;
           //*** end chg *****  08/01/25  ***************************************

              svCanclDt1 = svCanclDt;
           //*** begin del ***  08/01/25  ***************************************
           //*****   svARSM = ELarsm;
           //*** end del *****  08/01/25  ***************************************
           EndIf;

           If scSingDvCb = 'Y';
              indMulti = *Off;
           EndIf;

           //*** Beg Add ***** 03/14/24 *****************************
           flgPyrlFnd = ' ';
           //*** End Add ***** 03/14/24 *****************************
           Setll (intCo#:scFnd:scPolYear:scEmp#) pel01;
           Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
           Dow Not %Eof(wdell1);
              If indMulti = *On;
                 intDiv1 = ELdiv;
              Else;
                 intDiv1 = scDiv;
              EndIf;
            //*** begin add ***  12/28/18  ************************************
            // The audit bill run across all divisions that have the same expiration date
            // So initialize cancel date to use correct expiration date of each division
              svCanclDt = *Zeros;
            //*** end add *****  12/28/18  ************************************

              Chain (intCo#:scFnd:scEmp#:intDiv1) pem00;
              If EMcad1 >= ELpfdt and EMcad1 <= ELptdt and EMefd1 <> *Zeros;
                 svCanclDt = EMcad1;
              EndIf;

              If svCanclDt <> *Zeros;
                 svPolToDt1  = svCanclDt;
              Else;
                 svPolToDt1  = ELptdt;
              EndIf;

              If EMstrp = 'Y';
                 svCanclDt2 = svCanclDt;
              //*** Begin Add ***  08/30/19  **************************************
                 svStRptPolExDt = ELptdt;
              //*** End   Add ***  08/30/19  **************************************
              EndIf;

             //Default Policy To Date For Flat Cancellations.
              If svCanclDt = *Zeros;
                 If EMcad1 < ELpfdt and EMcad1 <> *Zeros and EMefd1 <> *Zeros;
                    svPolToDt1  = ELpfdt;
                 EndIf;
              EndIf;

        //*** begin chg ***  10/13/17  **************************************
        //****If svPolToDt1  = svPolToDt;
        //*** beg chg ***** 12/28/18 **********************
        //***    If svPolToDt1 = svPolToDt or outCltid = '150';
        //*** begin chg ***  08/01/25  ********************************************
        //****If svPolToDt1 = svPolToDt or scAllDvCb = 'Y';
              If scAllDvCb = 'Y' or (isSumInc = *on and ELARSM = 'Y') or
               (svPolToDt1 = svPolToDt and ELARSM = ' ' and isNotSum = *on) or
               (svPolToDt1 = svPolToDt and isNotSum = *off);
        //*** end chg *****  08/01/25  ********************************************
        //*** end chg ***** 12/28/18 **********************
        //*** end chg *****  10/13/17  **************************************
                 Setll (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                 Reade (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                 Dow Not %Eof(wmepl1) ;
                    If EPstat = 'A' and EPsafl <> 'A';
                       erPostDat = *On;
                       msPostDat = 'WCR0641';
                       isError   =  *On;
                    EndIf;
                    Reade (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                 EndDo;
                 //*** Beg Add ***** 03/14/24 *****************************
                 If flgPyrlFnd = *Blank or flgPyrlFnd = 'N';
                    flgPyrlFnd = 'N';
                    Setll (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                    Reade (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                    Dow Not %Eof(wmepl1) ;
                       If EPstat = 'A';
                          flgPyrlFnd = 'Y';
                          Leave;
                       EndIf;
                       Reade (intCo#:scFnd:scPolyear:scEmp#:intDiv1) pep01;
                    EndDo;
                 Endif;
                 //*** End Add ***** 03/14/24 *****************************
              EndIf;


              If indMulti = *On ;
                 Reade (intCo#:scFnd:scPolyear:scEmp#) pel01;
              Else;
                 leave;
              EndIf;
           EndDo;

           //*** Beg Add ***** 03/14/24 *****************************
           // Check to see any payroll exist
           If flgPyrlFnd = 'N';
              erEmp#1 = *On;
              msEmp#1 = 'No active payroll records exist ' +
              'for this employer.';
              isError = *On;
           Endif;
           //*** End Add ***** 03/14/24 *****************************

           //*** Begin Add ***  08/30/19  **************************************
           //  Don't consider the policy cx'd if policy cx'd in future year.
           //  svCanclDt1 is not populated unless policy is multi-div.
           If svCanclDt1 <> 0 And svCanclDt1 > svStRptPolExDt;
              Clear svCanclDt1;
           Endif;
           //*** End   Add ***  08/30/19  **************************************

        //*** begin chg ***  10/13/17  **************************************
        //*****   If indMulti = *Off;
           Chain (intCo#:scFnd:scEmp#:scDiv:scPolYear) pel00;

        // Verify all divisions are fully expired for client 150.
          //*** beg chg ***** 12/28/18 ********************
          //*** If outCltId = '150';
           If scAllDvCb = 'Y';
          //*** end chg ***** 12/28/18 ********************

              curDate7 = dateto7(SystemDate());
              Setll (intCo#:scFnd:scPolYear:scEmp#) pel01;
              Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;

              Dow Not %Eof(wdell1);

                 Chain (intCo#:scFnd:scEmp#:ELDiv) pem00;
                 If EMcad1 >= ELpfdt and EMcad1 <= ELptdt and EMefd1 <> *Zeros;
                    svCanclDt = EMcad1;
                 EndIf;

                 If svCanclDt = *Zeros;
                    svCanclDt = ELptdt;
                 EndIf;

                 If curDate7 < svCanclDt and ELpfdt <> ELptdt;
                    erPolYear = *On;
                    msPolYear = 'WC30219';
                    isError   =  *On;
                    Leave;
                 EndIf;

                 Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
              Enddo;
           Else;
        // Verify division begin run is fully expired, as audit will only
        // run for divisions equal to this expiration date.
        //*** end chg *****  10/13/17  **************************************
        //*** Beg add *****  02/12/19  **************************************
        // When running Audit Bill for Single Division,
        // RESET WMEMP record for current division to get correct value of EMcad1
              Chain (intCo#:scFnd:scEmp#:scdiv) pem00;
        //*** End add *****  02/12/19  **************************************
              svCanclDt3 = *Zeros;
              If scEmp# > *Zeros;
                 curDate8 = dateto8(SystemDate());
                 curDate7 = dateto7(SystemDate());
              EndIf;
              svCanclDt3 = EMcad1;
              If svCanclDt3 = *Zeros and curDate7 < ELptdt
               and ELpfdt <> ELptdt;
                 erPolYear = *On;
                 msPolYear = 'WC30219';
                 isError   =  *On;
              EndIf;
           EndIf;

           //*** begin del ***  08/01/25  *************************************
           //* Removing this edit due to WC2984 allowing deleted divs to be summarized.
           //*****If svCanclDt1 <> *Zeros and svCanclDt1 <> svCanclDt2
           //***** and svARSM = 'Y';
           //*****   erPostDat  = *On;
           //*****   msPostDat  = 'WC30375';
           //*****   isError    =  *On;
           //*****EndIf;
           //*** end del *****  08/01/25  *************************************

        EndIf;                                                    // if intEmp# <>*Zeros

       // User must have proper Security for running a Mass Billing
        If scEmp# = *Zeros;
           Chain (intCo#:scFnd) pex00;
           If Not %Found(wmexl);
              Chain (intCo#) pex00;
           EndIf;
           If %Found(wmexl);
              Chain (q1User) pup00;
              ##Type = UPtype;
              ##Lvl  = %Char(UPlvl);
              ##Subt =  EXsubt ;
              ##Subl =  %Char(EXsubl) ;
              callSRCCSC(##Type:##Lvl:##Subt:##Subl:'1':Flag);
              If Flag = *On;
                 erEmp# = *On;
                 msEmp# = 'WCR1122';
                 isError=  *On;
              EndIf;
           EndIf;
        EndIf;

       // If Running Mass Audit, No FYR can be selected
        If scEmp# = *Zeros  and scDiv = *Zeros and scPolYear <> *Zeros;
           erPolYear = *On;
           msPolYear = 'WC30064';
           isError   =  *On;
        EndIf;

        //*** beg chg ***** 12/13/17 **************
        // Validate Posting Date
        //*** If scPostDat =  *Zeros;
        //***    erPostDat = *On;
        //***    msPostDat = 'WCR0500';
        //***    isError   =  *On;
        //*** EndIf;
        //*** If scPostDat <> *Zeros;
        //***    If dtCheck(scPostDat) = *Zeros ;
        //***       erPostDat = *On;
        //***       msPostDat = 'WCR0500';
        //***       isError   =  *On;
        //***    Else;
        //***      // Posting Date cannot be in prior month.
        //***       If scPostDat < dateto8(SystemDate());
        //***          erPostDat = *On;
        //***          msPostDat = 'WCR0924';
        //***          isError   =  *On;
        //***       EndIf;
        //***    EndIf;
        //*** EndIf;

       // Post date must be valid and not in prior month
        intUsaDate = dtCheck(scPostDat);
        intPostYear = %Subdt(%Date(intUsaDate:*Usa):*y);
        intSysYear = %Subdt(%Date():*y);
        Select;
        When intUsaDate = 0;
           erPostDat = *On;
           msPostDat = 'WCR0500';
           isError   =  *On;
        When intPostYear < intSysYear Or
         (intPostYear = intSysYear And
         %Subdt(%Date(intUsaDate:*Usa):*m) < %Subdt(%Date():*m));
           erPostDat = *On;
           msPostDat = 'WCR0924';
           isError   =  *On;
        Other;
           scPostDat = intUsaDate;
        Endsl;
        //*** end chg ***** 12/13/17 **************

       // Include Deductible Balance must be "Y" or Blank
        If scIDedBlCb <> 'Y' and scIDedBlCb <> *Blanks;
           erIDedBlCb = *On;
           msIDedBlCb = 'WCR1241';
           isError    =  *On;
        EndIf;

       // Revised Bill must be "Y" or "N"
        If scRevAudCb <> 'Y' and scRevAudCb <> 'N';
           erRevAudCb = *On;
           msRevAudCb = 'WCR0501';
           isError    =  *On;
        EndIf;

       // Validate Final Audit Flag check box
        If scDiarFAcb <> 'Y' and scDiarFAcb <> *Blanks;
           erDiarFAcb = *On;
           msDiarFAcb = 'WCR1241';
           isError    =  *On;
        EndIf;

       // Extract and validate Comment Codes
        If %scan(',':%trimR(scCMCodes)) = *Zeros;
           commentCdDs = scCMCodes;
        EndIf;
        Dow %scan(',':%trimR(scCMCodes)) > *Zeros;
           scCMCodes   = %Replace ('':scCMCodes:%scan(',':%trimR(scCMCodes)):1);
            commentCdDs = scCMCodes;
        Enddo;

        If (##Comm1 <> *Blanks or ##Comm2 <> *Blanks or ##Comm3 <> *Blanks)
         and (scFcommnt1 <> *Blanks or scFcommnt2 <> *Blanks
         or scFcommnt3 <> *Blanks);
            erCmCodes  = *On;
            msCmCodes  = 'WCR0590';
            erFcommnt  = *On;
            msFcommnt  = 'WCR0590';
            isError    =  *On;
        EndIf;

       // Maximum 3 comment codes can be selected
        If %len(%trim(scCmCodes)) > 6 ;
           erCmCodes= *On;
           msCmCodes= 'WCR4823';
           isError  =  *On;
        EndIf;

        If ##Comm1 <> *Blanks;
           Chain (intCo#:##Comm1) pbc01;
           If Not %Found(wtabl) or BCstat = 'I';
              erCmCodes = *On;
              msCmCodes =  'WCR0513';
              isError   =  *On;
           EndIf;
        EndIf;
        If ##Comm2 <> *Blanks;
           Chain (intCo#:##Comm2) pbc01;
           If Not %Found(wtabl) or BCstat = 'I';
              erCmCodes = *On;
              msCmCodes = 'WCR0513';
              isError   =  *On;
           EndIf;
        EndIf;
        If ##Comm3 <> *Blanks;
           Chain (intCo#:##Comm3) pbc01;
           If Not %Found(wtabl) or BCstat = 'I';
              erCmCodes  = *On;
              msCmCodes  = 'WCR0513';
              isError    =  *On;
           EndIf;
        EndIf;

       // Validate A/R Transaction Code if entered
        //*** beg chg ***** 12/28/18 ************
       // If isARcd = *On and scArcd <> *Blanks;
        If scMode = '3';
        //*** beg chg ***** 12/28/18 ************
           Chain (intCo#:scARcd) ptt01;
           If %Found(wtabl);
              If TTstat = 'I';
                 erArcd    = *On;
                 msArcd    = 'WCR0513';
                 isError   =  *On;
              EndIf;

              If TTtype <> 'D';
                 erArcd    = *On;
                 msArcd    = 'WCR0638';
                 isError   =  *On;
              EndIf;
           //*** beg add ***** 12/28/18 ************
           Else;
              erArcd    = *On;
              msArcd    = 'WCR0503';
              isError   =  *On;
           //*** end add ***** 12/28/18 ************
           EndIf;
        EndIf;

       // Validate Remark Code if Entered
        If isRemarkCd = *On and scRemarkCd <> *Blanks;
           Chain (scRemarkCd) pct01;
           If Not %Found(stctl1);
              ermsgFile  = 'WMESGG';
              erRemarkCd = *On;
              msRemarkCd = 'WCR0827';
              isError    =  *On;
           Else;
              If CTstat = 'I';
                 ermsgFile    = 'SCMESG';
                 erRemarkCd = *On;
                 msRemarkCd = 'SCM0614';
                 isError    =  *On;
              EndIf;
           EndIf;
        EndIf;

       // Auto populate Dart Remark code and Staff code if Final Audit
       // Flag is switch to 'Y' and such codes exist in Aduit Tracking
        If sVDiarFAcb = *Blanks and scDiarFAcb = 'Y';
           sVDiarFAcb = scDiarFAcb;
           Chain (intCo#:scFnd) pax00;
           If %Found(wmaxl) ;
              If scDDrStfCd = *Blanks;
                 scDDrStfCd = AXdasc;
              EndIf;
              If scDefDrCd  = *Blanks;
                 Select;
                 When scRevAudCb = 'Y';
                    scDefDrCd = AXradc;
                 When scRevAudCb = 'N';
                    scDefDrCd = AXiadc;
                 EndSl;
              EndIf;
           EndIf;
        EndIf;

       // Validate that generate Final Audit flag syncs with the Darts
       // Remark and Staff code
        If (scDiarFAcb = 'Y'  and scDefDrCd  =  *Blanks)  or
         (scDiarFAcb   = 'Y'  and scDDrStfCd =  *Blanks)  or
         (scDiarFAcb   = ' '  and scDefDrCd  <> *Blanks)  or
         (scDiarFAcb   = ' '  and scDDrStfCd <> *Blanks);

           erDiarFAcb = *On;
           msDiarFAcb = 'WCR1083';
           isError    =  *On;
        EndIf;

       // Validate Audit Default Darts Remark Code
        If scDefDrCd <> *Blanks;
           Chain (scDefDrCd) pct01;
           If Not %Found(stctl1);
              erDefDrCd  = *On;
              msDefDrCd  = 'WC30055';
              isError    =  *On;
           EndIf;
        EndIf;

       // Validate Audit Default Darts Staff Code
        If scDDrStfCd <> *Blanks;
           Chain (scDDrStfCd) pup01;
           If Not %Found(stctl1);
              erDefDrCd  = *On;
              msDefDrCd  = 'WCR0892';
              isError    =  *On;
           EndIf;
        EndIf;

       // Display Information Window if Retro Settlement has been run
        If isError = *Off;
           strBSCD = %Char(ELbscd);
           Chain (intCo#:strBSCD) pbs01;
           If %Found(wtabl) and (BSlpgm = 'P' or BSlpgm = 'I')
            and ELstam <>*Zeros;

              btnRsp = PopThreeBtn(conMsg:conTitle:conBtn1:conBtn2:conBtn3);
              Select;
              When btnRsp = *On;      // User choose to continue
                 isError  =  *Off;

              When btnRsp = *Off;     // User pressed Exit (F3)
                 isError  =  *On;
                 ExitNow  =  *On;

              When btnRsp = ' ';     // User pressed Cancel (F12)
                 isError  =  *On;

              EndSl;
           EndIf;
        EndIf;
           //*** Beg Add ***** 05/05/22 ***********************************
           If isError  = *off;
              xxSubmit = *on;
              If isUseScript;
                 xxProcType = processType;
                 Chain intCo# pbl00;
                 //*** Begin Chg *** 05/03/24 ******************************
                 //*** If scDivCount = 1 and BLPrmna = ' ';
                 //*** begin chg ***  03/06/25  ****************************
                 //*****If scDivCount = 1 Or scEmp# <> 0;
                 If intRRN <> 0 and (scDivCount = 1 or BLCombA = 'Y');
                 //*** end chg *****  03/06/25  ****************************
                 //*** End   Chg *** 05/03/24 ******************************
                    xxMass = ' ';
                 Else;
                    xxMass = 'Y';
                 Endif;

                 In *Lock *dtaara;
                 Clear svLDA;
                 xxDart = 'N';
                 xxRMCD = ' ';
                 Chain (intCo#:scFnd) pex00;
                 //*** Beg Chg ***** 04/04/24 ***********************
                 //***If %Found and EXPNRM <> ' ';
                 //***   xxDart = 'Y';
                 //***   xxRMCD = EXPNRM;
                 //***Endif;
                 If %Found and EX3rdb <> 'Y';
                    scRemarkCd = EXaud;
                    If scRevAudCb = 'Y' and EXrfaf <> ' ';
                       scRemarkCd = EXrfaf;
                    Endif;
                 Else;
                    scRemarkCd = *Blanks;
                 EndIf;
                 If scRemarkCd <> ' ';
                    xxDart = 'Y';
                    xxRMCD = scRemarkCd;
                 Endif;
                 //*** End Chg ***** 04/04/24 ***********************
                 Out *dtaara;
                 //*** begin chg ***  07/24/24  ***********************************
                 //***** xxSubmit = OutputWindow(xxProcType:xxMass);
                 //*** begin chg ***  03/06/25  ****************************
                 //*****If intRRN <> 0;
                 If intRRN <> 0 and xxMass = ' ';
                 //*** end chg *****  03/06/25  ****************************
                    xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':intRRN);
                 Else;
                    xxSubmit = OutputWindow(xxProcType:xxMass);
                 Endif;
                 //*** end chg *****  07/24/24  ***********************************
                 If xxSubmit = *off;
                    isError = *On;
                 Else;
                    In *Lock *dtaara;
                    Out *dtaara;
                    scRemarkCd = xxRMCD;
                 Endif;
              Endif;
           Endif;
           //*** End Add ***** 05/05/22 ***********************************

        If isError = *On;
           Return *Off;
        Else;
           Return *On;
        EndIf;

      /End-Free
     P NoError         e

      //===================================================================
      // Load Screen Fields
      //===================================================================
     P LoadScreen      b
     D LoadScreen      pi
      /Free

        ##Mode     = scMode;
        svRevAudCb = scRevAudCb;
        svDiarFAcb = scDiarFAcb;
        svRemarkCd = scRemarkCd;
        //***** Beg Add ***** 05/07/19 *******************************
        scAudTrCd= EXaucd;
        //***** End Add ***** 05/07/19 *******************************

       // Check if scRevAudCb was changed
        If scRevAudCb <> svRevAudCb and scRevAudCb = 'Y' ;
           If isRemarkCd = *On;
              scRemarkCd = EXrfaf;
           Else;
              scRemarkCd = EXaud ;
           EndIf;

           If EX3rdb = 'Y';
              scRemarkCd = *Blanks;
           EndIf;
        EndIf;

       // Default Remark code and Staff code when Final audit flag
       // switched to Blank
        If scDiarFAcb = *Blanks and svDiarFAcb = 'Y';
           scDDrStfCd = *Blanks;
           scDefDrCd  = *Blanks;
        EndIf;

        If q1prms > *Zeros and intRRN <> *Zeros;
           Chain intRRN pel0p;
           If %Found (wdelp);
              scFnd      = ELfnd ;
              scPolYear  = ELfyr ;
              scEmp#     = ELemp#;
              scDiv      = ELdiv ;

              svPolToDt  = ELptdt;
              ##Co#      = ELco# ;
              ##Fnd      = ELfnd ;
              ##Fyr      = ELfyr ;
              ##Emp#     = ELemp#;
              ##Div      = ELdiv ;
              ##Date     = dtMDYY(ELptdt);
              ##Plan     = '*';
           EndIf;

          // Get Agent and Producer information
           CallWA002R(##Co#:##Fnd:##Emp#:##Div:##Fyr:##Agent#:##AgentNm:
            ##Prod#:##ProdNm:##Area:##Phone:##Plan:##Date);

           If ##Plan = 'A';
              strAcct= 'Y';
           EndIf;

           scEmpName = GetEmpName(##Emp#:##Div:EM_rrn:##Fnd:##Co#:*Blanks);

           Chain EM_rrn pem0p;
           If %Found(wmemp);
              ##Emp#    = EMemp# ;
              ##Div     = EMdiv  ;
           EndIf;
           ##Edn     = *Blanks;
           ##Name    = scEmpName;
        EndIf;

       // Calculate Posting Date
        CalPostDat();

        If isRemarkCd = *On and EX3rdb <> 'Y';
           scRemarkCd = EXaud;
        //*** begin add ***  10/07/19  ******************************
           If scRevAudCb = 'Y' and EXrfaf <> ' ';
              scRemarkCd = EXrfaf;
           Endif;
        //*** end add *****  10/07/19  ******************************
        Else;
           scRemarkCd = *Blanks;
        EndIf;

        //*** begin chg ***  02/19/18  *************************************
        //*****scARcd = EXaucd;
        If isARcd = *on;
           scARcd = EXaucd;
        Endif;
        //*** end chg *****  02/19/18  *************************************

        scDivCount = 0;
        //*** begin add ***  08/01/25  ***************************************
        svStRptPolExDt = 0;
        svARSM = ' ';
        scAllDvCb = 'Y';
        //*** end add *****  08/01/25  ***************************************
       // Get Division count
        Setll (intCo#:scFnd:scPolYear:scEmp#) pel01;
        Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
        Dow Not %Eof (wdell1) ;
           If ELpfdt <> ELptdt;
              scDivCount+= 1;
           EndIf;
        //*** begin add ***  08/01/25  ***************************************
           Chain (intCo#:scfnd:scEmp#:ELDiv) pem00;
           If %found and EMStrp = 'Y' and ELARSM = 'Y';
              svARSM = 'Y';
              Select;
              When EMCad1 <= ELPfdt and EMCad1 >= ELPfdt and EMCad1 <> 0;
                 svPolToDt = EMCad1;
              When EMcad1 < ELpfdt and EMcad1 <> *Zeros;
                 svPolToDt = ELPfdt;
              Other;
                 svStRptPolExDt = ELptdt;
              Endsl;
           Endif;
        //*** end add *****  08/01/25  ***************************************
           Reade (intCo#:scFnd:scPolYear:scEmp#) pel01;
        EndDo;
        //*** begin add ***  08/01/25  ***************************************
        Chain (intCo#:scFnd:scEmp#:scDiv:scPolYear) pel00;
        Chain (intCo#:scfnd:scEmp#:scDiv) pem00;
        isSumInc = *off;
        isNotSum = *off;
        //*** end add *****  08/01/25  ***************************************

        //*** Beg Add ***** 09/23/24 *********************************
        scAudBill = 'Audit Bill';
        //*** End Add ***** 09/23/24 *********************************
        scEmpDiv# =  %Editc(scEmp#:'X') + '  ' + %Editc(scDiv:'X');
        If scDivCount > 1;
           indMulti = *On;
           svCanclDt = GetCxDate(intCo#:scFnd:scEmp#:scDiv);
           //*** begin add ***  08/01/25  ***************************************
           If svCanclDt <> *Zeros and svCanclDt <= ELPtdt;
              If svCanclDt < ELPfdt;
                 svCanclDt = ELPfdt;
              EndIf;
           Else;
              Clear svCanclDt;
           EndIf;
           //*** end add *****  08/01/25  ***************************************
           //*** Begin Add *** 08/08/18 *********************************
           //*** Beg Chg ***** 09/23/24 *********************************
           //***scCxExHdr = 'Audit Bill - This bill will be run over all ' +
           //***            ' division with';
           scCxExHdr = 'Audit Bill - This bill will be run over all ' +
                       ' divisions with';
           //***If svCanclDt = 0;
           If svCanclDt <> 0;
           //*** End Chg ***** 09/23/24 *********************************
              scCxExHdr = %Trim(scCxExHdr) + ' ' +
                alphaUSA(CvtToDate(svCanclDt));
           Else;
              scCxExHdr = %Trim(scCxExHdr) + ' ' +
                alphaUSA(CvtToDate(ELptdt));
           EndIf;
           scCxExHdr = %Trim(scCxExHdr) + ' expiration/cancellation date.';
           scAudBill = scCxExHdr;
           //*** End   Add *** 08/08/18 *********************************
           //*** Begin Add *** 11/09/18 *********************************
           scPrcAMsg = *blanks;
           scPrcAll = 'AUDIT BILL  -  THIS BILL WILL BE RUN OVER ALL DIVISIONS';
           //*** End   Add *** 11/09/18 *********************************

          //Default Policy To Date For Flat Cancellations.
           If svCanclDt  = *Zeros;
              If EMcad1 < ELpfdt and EMcad1 <> *Zeros and EMefd1 <> *Zeros;
                 svPolToDt = ELpfdt;
              EndIf;
           Else ;
              svPolToDt = svCanclDt;
           EndIf;
           //*** begin add ***  08/01/25  ***************************************
           scPrcAMsg = scPrcAll;
           //* Display message/edit based on all summarized divisions running or not.
           Select;
           When ELARSM = 'Y' or (svARSM = 'Y' and svPolToDt = svStRptPolExDt);
              isSumInc = *on;
           When ELARSM = ' ' and svARSM = 'Y' and svPolToDt <> svStRptPolExDt;
              isNotSum = *on;
           Endsl;
           If scCxExHdr <> ' ' and (isSumInc = *on or isNotSum = *on);
              scCxExHdr = %trim(scCxExHdr) + '*';
              scAudBill = scCxExHdr;
           Endif;
           //*** end add *****  08/01/25  ***************************************
        EndIf;

        Chain (intCo#:scFnd:scPolYear) pf100;
        If %Found(wdf1l);
           scIDedBlCb = F1idbd;
        EndIf;

       // Load Comment codes
        CmntcodLod();

      /End-Free
     p LoadScreen      e

       //===================================================================
       // Emp_DivSearch - Employer/Div Search
       //===================================================================
     p Emp_DivSearch   b
     d Emp_DivSearch   pi
      /Free

       inMode  = 'E';
       inCall  = '2';
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = scFnd;
       dsEmpName = scEmpName;
       empRrn    = *Zeros;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';
          empRrn = %Dec(outRrn#:9:0);
          Chain empRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;

             Chain (intCo#:ELfnd:ELemp#:ELdiv) pem00;
             If %Found(wmeml);
                scEmp#     = EMemp#;
                scEmpName  = EMname;
                scDiv      = EMdiv;
                scEmpDiv#  = %Editc(EMemp#:'X') + '  ' + %Editc(EMdiv:'X');
             EndIf;
          EndIf;
       EndIf;
      /End-Free
     p Emp_DivSearch   e

       //===================================================================
       // CmntCodLod - Load comment code multiselect box.
       //===================================================================
     p CmntcodLod      b
      /Free

       scCmntSel  = ' ';
       scCmntVal  = *Blanks;
       strCmntDsc = *Blanks;

       // Read WTBCP file for loading comment codes
       Setll intCo# pbc01;
       ReadE intCo# pbc01;
       Dow not %Eof(wtabl);
          If BCstat = 'A';
             strCmntDsc = TitleCase(BCdesc);
             scCmntSel  = %trim(scCmntSel)+','+%trim(BCkey)+'-'+
                          %trim(%xlate(',':' ': StrcmntDsc));
             scCmntVal = %trim(scCmntVal)+','+%trim(BCkey);
          Endif;
       ReadE intCo# pbc01;
       Enddo;

      /End-Free
     p CmntCodLod      e

       //===================================================================
       // CalPostDat - Calculate Posting Date
       //===================================================================
     p CalPostDat      b
      /Free

         Chain (intCo#:scFnd) pex00;
         If %Found(wmexl) and strAcct = 'Y';
            intDays = EXaaid;
         Else;
            intDays = EXauid;
         EndIf;

         // Posting Date (Audit Interim Days Plus Current Date)
         scPostDat = dtfuture(dateto8(SystemDate()):intDays);
         If scPostDat = *Zeros;
            scPostDat = dateto8(SystemDate());
         EndIf;

      /End-Free
     p CalPostDat      e
