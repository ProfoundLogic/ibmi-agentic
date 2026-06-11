     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title W7230R - Audit Request Promt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7230R - PROMPT FOR AUDIT REPORTS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS IS THE PROMPT PROGRAM FOR AUDIT REQUESTS.               *
     H*       THIS PROGRAM PROMPTS FOR 1A) POLICY EFFECT DT RANGE, OR *
     H*       1B) INDIVIDUAL EMPLOYER INFO (FYR/EMP#/DIV), 2) WHETHER *
     H*       OR NOT WMAUP AUDIT TRACKING FILE SHOULD BE UPDATED,     *
     H*       3) A DARTS COMMENT (BLANK= NO DARTS WILL BE WRITTEN)    *
     H*       4) AND WHETHER OR NOT TO USE S/R PAYROLL FOR S/R        *
     H*       EMPLOYERS                                               *
     H*****************************************************************
     h* Revisions:                                                    *
     H*                                                               *
     H* 06/13/25 C:344 Support: 118608                                *
     H*                Added unique Employer and Division work fields *
     H*                to exclusively relate to the field values that *
     H*                were extracted via the execution of the drop-  *
     H*                down selection from the Policy Toolbar.        *
     H*
     H* 05/16/23 C:62  Project: WC2838
     H*                Fix Mass flag
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     h*                                                               *
      * 06/09/21 C:65  Project: WC2671 - screen file change only      *
      *                Do not generate output should be available     *
      *                regardless of audit request to process value.  *
      *                                                               *
      * 10/28/20 C:079 Project: WC2671                                *
      *                If not called using group select end program   *
      *
      * 01/10/19 C:291 Project: WC2401                                *
      *                Add a drop-down for Region.                    *
      *                                                               *
      *  03/22/17 C:053 Support: 46241
      *                 Recompile over changed copy module
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      * 11/20/15 C:33  38370                                          *
      *                Pass correct mode to submitter.                *
      *                                                               *
      * 10/05/15 C:180 Support 36759                                  *
      *                Display file changes only -                    *
      *                Fixed the chkbox for "Update Audit Tracking.." *
      *                No code changes to this pgm -                  *
      *                                                               *
      * 09/17/15B  C:3??  regenerated WC2254                          *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
     h*                                                               *
     h* 09/17/15 C:136 Support: 37292                                 *
     h*                Ensure that default DARTS Remark code is       *
     h*                passed to submitter in mass run so that doc's  *
     h*                can be resolved to DARTS in W7231C1            *
     h*                                                               *
     H* 09/14/15 C:306 Support: 36722                                 *
     H*                Fixed the Update Audit Tracking checkbox       *
     H*                grayed out issue.
     H*                                                               *
     h* 07/30/15 C:33  35307                                          *
     h*                Load company name.                             *
     h*                                                               *
     h* 04/16/15 C:65  Project: SY251                                 *
     h*                Add API call to launch new Department          *
     h*                letters.                                       *
     h*                                                               *
     h* 05/07/15  C:295  Priss 16: 114WMZ                             *
     h*                 -Repositioned the Checkbox in the Audit       *
     h*                  Request Screen                               *
     h*                                                               *
     h* 04/13/15  C:295  Priss 15: 114WMZ                             *
     h*                 -Added golden asterisk to mandate             *
     h*                  the Darts Remark field.                      *
     h*                 -The Dart Remark should be defaulted as per   *
     h*                  set up Group Admin Dart Table.               *
     h*                                                               *
     H* 02/23/15  C:282  Project: 114WMZ                              *
     H*                  Re-design the complete program as per the    *
     h*                  compsuite standards.                         *
     h*                  Included two new fields in this program.     *
     h*                  i) Audit Request to process                  *
     h*                 ii) Previously Run Audit Requests             *
     h*                                                               *
     h*****************************************************************
     fW7230F    cf   e             workstn extfile('W7230F')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     fWdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     f                                     Infds(dbfds1)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     fWmupl     if   e           k disk    Rename(pup01:pup00)
     fWtabl     if   e           k disk
     fWmexl     if   e           k disk    Rename(pex01:pex00)
     fstctl1    if   e           k disk    Rename(pct01:pct01)
     f*** Beg Add *** 10/28/20 *************************************
     fwxgsl     if   e           k disk
     f*** End Add *** 10/28/20 *************************************

     dpsds           esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d @@Kc            s              1
     d @@Kl            s              1
     d @@Mode          s              1
     d @@Rrn           s              9  0
     d @@TabIndex      s              5
     d ##CltId         s              3
      // These can be deleted when WTACTPNL fields exist
     d btnShowWin      s              1    Inz('0')
     d txtShowWin      s             25    Inz
     d charRrn         s              9
     d charKey         s             10
     d charMode        s              1
     d charOutq        s                   Like(Upoutq)
     d charMsg         s            100
     d flgActPnl       s              1
     d outRrn#         s              9
     d empRrn          s              9  0
     d isAggregate     s               n   Inz(*off)
     d isMassRun       s               n   Inz(*off)
     d isMenuAvbl      s               n   Inz(*on)
     d*** Beg Chg *** 10/28/20 *********************************************
     d*** isSingleEmp     s               n   Inz(*off)
     d isSnglEmp       s               n   Inz(*off)
     d isSubmit        s               n
     d*** End Chg *** 10/28/20 *********************************************
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELfyr)
     d intPExpToDt     s              8  0
     d intPExpFrDt     s              8  0
     d charPolTyp      s              1
     d scDrtRmkCd      s              5
     d scemp#          s                   like(ELemp#)
     d scfyr           s                   like(ELfyr)
     d scdiv           s                   like(ELdiv)
     d xxRrn           s              9  0
     d outRmcd         s              5
     d outDesc         s             40
     d outRtcd         s              1
     d inSelect        s           1024
      *
     d inCall          s              1
     d inKc            s              1
     d inKl            s              1
     d inCo#           s              3
     d inFnd           s              3
     d inPExpToDt      s              8
     d inPExpFrDt      s              8
     d inFyr           s              3
     d inEmp#          s              9
     d inDiv           s              5
     d inUpdAudT       s              1
     d inDrtRmkCd      s              5
     d inOutq          s             10
     d inMode          s              1
     d inComCd1Ds      s             30
     d inComCd2Ds      s             30
     d inComCd3Ds      s             30
     d inComCd4Ds      s             30
     d inComCd5Ds      s             30
     d inComCd1        s              2
     d inComCd2        s              2
     d inComCd3        s              2
     d inComCd4        s              2
     d inComCd5        s              2
     d inSlfRptPy      s              1
     d inPolTyp        s              1
     d inAUTY          s              2
     d inATIE          s              1
     d inAR            s             30
     d inACTY          s              1
     d inACST          s              1
     d inG15C          s              1
     d inAFTY          s              1
     d inDDUE          s              8
     d inPRDT          s              8
     d inASSF          s              1
     d inPSlfRptP      s              1
     d inAudRProc      s              1
     d inUpdAudTR      s              1
     d inRegion        s              3
     d*** Beg Add *** 06/13/25 ****************************************
     d xxEmp#          s                   Like(ELemp#) Inz
     d xxDiv           s                   Like(ELdiv) Inz
     d*** End Add *** 06/13/25 ****************************************
     d*** Beg Add *** 10/28/20 ****************************************
     d strMessage      s            150
     d processType     s            100    Inz('Audit Request')
     d mass            s              1
     d intGrpCount     s              3s 0
     d intOneFnd       s              3s 0
     d inGenAllDiv     s              1
     d inNoOutput      s              1
     d inGen301En      s              1
     d inGen311En      s              1
     d inListClms      s              1
     d isPriorSlct     s              1n
     d outData         s           6500    Varying
     d exitNow         s              1n
     d scEmpDivNm      s             80a
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxMultiG                       1    overlay(svlda:127)
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
     d*** End Add *** 10/28/20 ****************************************
      *****************************************************************
      * Prototype Declaration
      *****************************************************************
      * Audit Requests - From Aggregate
     d AudReqAgg       pr

      * Audit Requests - Single/Mass
     d AudReqMnu       pr

      * Audit Requests -  From Policy Dropdown
     d AudtReqPolT     pr

      * Audit Report Proc.
     d AudRpProc       pr

      * Audit Report Submit
     d AudRpSbm        pr                  ExtPgm('W7230C')
     d inCo#                          3
     d inFnd                          3
     d inPExpToDt                     8
     d inPExpFrDt                     8
     d inFyr                          3
     d inEmp#                         9
     d inDiv                          5
     d inUpdAudT                      1
     d inDrtRmkCd                     5
     d inOutq                        10
     d inMode                         1
     d inComCd1Ds                    30
     d inComCd2Ds                    30
     d inComCd3Ds                    30
     d inComCd4Ds                    30
     d inComCd5Ds                    30
     d inComCd1                       2
     d inComCd2                       2
     d inComCd3                       2
     d inComCd4                       2
     d inComCd5                       2
     d inSlfRptPy                     1
     d inPolTyp                       1
     d inAUTY                         2
     d inATIE                         1
     d inAR                          30
     d inACTY                         1
     d inACST                         1
     d inG15C                         1
     d inAFTY                         1
     d inDDUE                         8
     d inPRDT                         8
     d inASSF                         1
     d inPSlfRptP                     1
     d inAudRProc                     1
     d inUpdAudTR                     1
     d inRegion                       3
     d inGenAllDiv                    1
     d inNoOutput                     1
     d inGen301En                     1
     d inGen311En                     1
     d inListClms                     1

      * Clear Error/Message fields
     d ClrErr          pr

      * Container 2 Widgets Layout
     d Con2WLayout     pr

      * DARTS Remark
     d DartRmkSr       pr                  ExtPgm('SC305R')
     d inCall                         1
     d inkc                           1
     d inkl                           1
     d outRmcd                        5
     d outDesc                       40
     d outRTCD                        1

      * Darts Remark Proc.
     d DrtRmkSrch      pr

      * Employer Search Proc.
     d EmployerSrch    pr

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Process other options
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel 1 Sreen Values
     d ValidPnl1       pr

      * Validate Panel 2 Sreen Values
     d ValidPnl2       pr

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
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
     d*** Beg Add *** 10/28/20 ********************************************
     d/copy sPrGenPop
     d/copy sPrJobInfo
     d/copy sPrXml
     d/copy sPrUser
     d*** End Add *** 10/28/20 ********************************************
    
      * Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@KC
     c                   Parm                    @@KL
     c                   Parm                    @@Mode
     c                   Parm                    @@RRN
      *
     c     keyWMEML      Klist
     c                   Kfld                    intCo#
     c                   Kfld                    intFnd
     c                   Kfld                    intEmp#
     c                   Kfld                    intDiv
      *
     c     keyWtabl      Klist
     c                   Kfld                    intCo#
     c                   Kfld                    charKey
      *
     c     keyWmupl      Klist
     c                   Kfld                    q1user
      * ====================================================================
      /Free

       // Start Of Program
       ProgramStart();

      *// Main Procesing
       //*** Beg Add *** 10/28/20 *****************************************
       If not exitNow;
       //*** End Add *** 10/28/20 *****************************************
          Main();
       //*** Beg Add *** 10/28/20 *****************************************
       Endif;
       //*** End Add *** 10/28/20 *****************************************

       *InLr = *on;

      /End-Free
       //===================================================================
       // AudReqAgg - Audit Requests - From Aggregate
       //===================================================================
     p AudReqAgg       b
     d AudReqAgg       pi

      /Free
       // Common Action Panel will get displayed.
       flgActPnl = 'N';

       // Container 1 will be visible.
       indCont1  = *On;

       // Group widget will be visible.
       //*** Beg Del *** 10/28/20 ************************************************
       //***indFnd = *On;
       //*** End Del *** 10/28/20 ************************************************

       // Policy Expiration Date Range and Policy Type Widget will be hidden.
       indMassReq = *Off;

       // Single Employer/Division and Year will be visible.
       indSigReq = *On;

       // Container 2 Layout.
       scCon2top  = 320;

       // Container 2 Widgets Layout.
       Con2WLayout();

       // Read the Employer Detail File
       Chain(n) xxRRN pel0p;
       If %Found(wdelp);
          intCo#  = ELco#;
          intFnd  = ELfnd;
          intEmp# = ELemp#;
          intDiv  = ELdiv;

          //*** Beg Del *** 10/28/20 ***********************************
          //***scFnd = ELfnd;
          //*** End Del *** 10/28/20 ***********************************

          Chain(n) keyWMEML pem00;
             If %Found(WMEML);
                scEmpDivNm = EMname;
                scEmpDiv   = %EditC(EMemp#:'X') + %EditC(EMdiv:'X') +
                          %EditC(EMfnd:'X');
             EndIf;

          scFyr = ELFyr;
          // Clear fyrChoices;
          //*** Beg Del *** 10/28/20 ************************************
          //***fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
          //***        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
          //***        + ' AND ELdiv = ' + %char(ELdiv)
          //***        + ' AND ELfyr = ' + %char(ELfyr);
          //*** End Del *** 10/28/20 ************************************
          // Clear divChoices;

       EndIf;

      /End-Free

     p AudReqAgg       e
       //===================================================================
       // AudReqMnu - Audit Requests - Single/Mass
       //===================================================================
     p AudReqMnu       b
     d AudReqMnu       pi

      /Free
       //*** Beg Add *** 10/28/20 ******************************************
       exitnow = *off;
       If not PgmInStack('W7944C');
          strMessage = 'This menu option is outdated and must be replaced +
           by new menu options present in Control Panel Navigation.';
          PopOneBtn(strMessage:'Outdated Menu Option');
          exitNow = *on;
          Return;
       Endif;

       // Determine if only on group in group select file
       intGrpCount = 0;
       isMultGrps = *on;
       Setll intCo# pgs01;
       Reade intCo# pgs01;
       dow not %Eof(WXGSL);
          If GSieFl = 'I';
             intGrpCount = intGrpCount + 1;
             IntOneFnd = GSfnd;
          Endif;
          Reade intCo# pgs01;
       Enddo;
       If intGrpCount = 1;
          isMultGrps = *off;
          scFnd = IntOneFnd;
          // Load the default DARTS remark for the group
          Chain (intCo#:intOneFnd) pex00;
          If %Found(wmexl) and EXauRq <> *blanks;
             Chain EXaurq pct01;
             If %Found (stctl1);
                scDrtRmk   = CTrmCd + ' - ' + %Trim(CTdesc);
                scDrtRmkCd = CTrmCd;
             Endif;
          Endif;
       Else;
          isMultGrps = *on;
       Endif;
       //*** End Add *** 10/28/20 ******************************************

       // Common Action Panel will get displayed.
       flgActPnl = 'N';

       // Container 1 will be Visible.
       indCont1  = *On;

       // Group widget will be visible.
       //*** Beg Del *** 10/28/20 ************************************************
       //***indFnd = *On;
       //*** End Del *** 10/28/20 ************************************************

       // Policy Expiration Date Range and Policy Type Widget will be visible.
       indMassReq = *On;

       // Single Employer/Division and Year will be visible.
       indSigReq = *On;

       // Container 2 Layout.
       scCon2Top  = 320;

       // Container 2 Widgets Layout.
       Con2WLayout();
          // Set empChoices
          //*** Beg Add *** 10/28/20 ***************************************
             empChoices = 'EMco# = ' + %char(intCo#) + ' AND EMfnd = '
              + %char(intOneFnd) + ' and EMstRp = ''Y''';
          //*** End Add *** 10/28/20 ***************************************

      /End-Free

     p AudReqMnu       e
       //===================================================================
       // AudtReqPolT - Audit Requests -  From Policy Dropdown
       //===================================================================
     p AudtReqPolT     b
     d AudtReqPolT     pi

      /Free

       // Set up Policy Type.
       scPolTyp = 'B';
       //*** Beg Add *** 10/28/20 ******************************************
       // Set up Aud Req Proc to All
       scAudRProc = 'A';
       //*** End Add *** 10/28/20 ******************************************

       // Policy Action Panel will get displayed.
       flgActPnl = 'Y';

       // Container 1 will be hidden.
       indCont1  = *Off;

       // Group widget will be hidden.
       //*** Beg Del *** 10/28/20 ************************************************
       //***indFnd = *Off;
       //*** End Del *** 10/28/20 ************************************************
       //*** Beg Add *** 10/28/20 ************************************************
       indNonMass = *On;
       isMultGrps = *off;
       scDivChr = %Editc(scDiv:'X');
       //*** End Add *** 10/28/20 ************************************************

       // Policy Expiration Date Range and Policy Type Widget will be hidden.
       indMassReq = *Off;

       // Single Employer/Division and Year will be hidden.
       indSigReq = *Off;

       // Container 2 Layout.
       scCon2top  = 95;

       // Container 2 Widgets Layout.
       Con2WLayout();

       // Read the Employer Detail File
       Chain(n) xxRRN pel0p;
       intCo#  = ELco#;
       intFnd  = ELfnd;
       intEmp# = ELemp#;
       intDiv  = ELdiv;
       intFyr  = ELFyr;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

      /End-Free

     p AudtReqPolT     e
       //===================================================================
       // AudRpProc - Audit Process Submit
       //===================================================================
     p AudRpProc       b
     d AudRpProc       pi
     d intPos          s              5  0
      /Free

       inCo# = %EditC(intCo#:'X');
       //*** Beg Chg *** 10/28/20 *****************************************
       //***If scfnd <> 0;
       If scfnd <> 0 and flgActPnl = 'Y';
       //*** End Chg *** 10/28/20 *****************************************
          inFnd   = %EditC(scFnd:'X');
       Else;
          inFnd   = %EditC(intFnd:'X');
       EndIf;

       If scPExpToDt <> 0;
          inPExpToDt = %EditC(scPExpToDt:'X');
       Else;
          inPExpToDt = *Blanks;
       EndIf;

       If scPExpToDt <> 0;
          inPExpFrDt = %EditC(scPExpFrDt:'X');
       Else;
          inPExpFrDt = *Blanks;
       EndIf;

       If scFyr <> 0;
          inFyr   = %EditC(scFyr:'X');
       Else;
          inFyr   = %EditC(intFyr:'X');
       EndIf;
       If scRegion <> 0;
          inRegion = %EditC(scRegion:'X');
       Else;
          inRegion = *Blanks;
       EndIf;


       //*** Beg Add *** 06/13/25 ******************************************
       // Set Employer and Division rpt parm fields with work fields from
       // policy dropdown.
       Select;
       // If called from Policy Dropdown, use the fields extracted via RRN.
       When xxRrn <> 0 And charMode = ' ';
          inEmp# = %Editc(xxEmp#:'X');
          inDiv  = %Editc(xxDiv:'X');
       Other;
       //*** End Add *** 06/13/25 ******************************************
          //*** Beg Chg *** 10/28/20 **********************************
          //***inEmp#     = %EditC(intEmp#:'X');
          //***inDiv      = %EditC(intDiv:'X');
          If scEmp# <> *zeros;
             inEmp# = %Editc(scEmp#:'X');
             inDiv  = scDivChr;
          Else;
             inEmp#  = *zeros;
             inDiv   = *blanks;
          Endif;
          //*** End Chg *** 10/28/20 **********************************
       //*** Beg Add *** 06/13/25 ******************************************
       Endsl;
       //*** End Add *** 06/13/25 ******************************************
       inUpdAudT  = scUpdAudT;
       // ScDrtRmkCd is not populated during mass run
       //  Remove '-' in case Darts Code has less than 4 chars
       inDrtRmkCd = scDrtRmk;
       intPos = %scan('-':inDrtRmkCd);
       If intPos > 0;
          inDrtRmkCd = %replace('    ':inDrtRmkCd:intPos);
       Endif;
       inOutq     = charOutq;
       inMode     = charMode;
       Select;
       When isAggregate;
          inMode = '*';
       Other;
          inMode = 'D';
       Endsl;
       inComCd1Ds = %SubSt(scComCd1:14);
       inComCd2Ds = %SubSt(scComCd2:14);
       inComCd3Ds = %SubSt(scComCd3:14);
       inComCd4Ds = %SubSt(scComCd4:14);
       inComCd5Ds = %SubSt(scComCd5:14);
       inComCd1   = %SubSt(scComCd1:1:2);
       inComCd2   = %SubSt(scComCd2:1:2);
       inComCd3   = %SubSt(scComCd3:1:2);
       inComCd4   = %SubSt(scComCd4:1:2);
       inComCd5   = %SubSt(scComCd5:1:2);
       inSlfRptPy = scSlfRptPy;

       If scPolTyp <> *Blank;
          inPolTyp   = scPolTyp;
       Else;
          inPolTyp   = *Blank;
       EndIf;

       inAUTY     = *Blanks;
       inATIE     = *Blanks;
       inAR       = *Blanks;
       inACTY     = *Blanks;
       inACST     = *Blanks;
       inG15C     = *Blanks;
       inAFTY     = *Blanks;
       inDDUE     = *Blanks;
       inPRDT     = *Blanks;
       inASSF     = *Blanks;
       inPSlfRptP = scPSlfRptP;
       inAudRProc = scAudRProc;
       inUpdAudTR = scUpdAudTR;
       //*** Beg Add *** 10/28/20 *****************************************
       inNoOutput = scNoOutput;
       If indNonMass = *on;
          inGenAllDiv = scGenAllDv;
       Else;
          inGenAllDiv = ' ';
       Endif;
       inGen301En = scGen301En;
       inGen311En = scGen311En;
       inListClms = scListClms;
       //*** End Add *** 10/28/20 *****************************************
       AudRpSbm(inCo#:inFnd:inPExpToDt:inPExpFrDt:inFyr:inEmp#:
          inDiv:inUpdAudT:inDrtRmkCd:inOutq:inMode:inComCd1Ds:
          inComCd2Ds:inComCd3Ds:inComCd4Ds:inComCd5Ds:inComCd1:
          inComCd2:inComCd3:inComCd4:inComCd5:inSlfRptPy:inPolTyp:
          inAUTY:inATIE:inAR:inACTY:inACST:inG15C:inAFTY:inDDUE:
       //*** Beg Chg *** 10/28/20 *****************************************
       //*** inPRDT:inASSF:inPSlfRptP:inAudRProc:inUpdAudTR:inRegion);
          inPRDT:inASSF:inPSlfRptP:inAudRProc:inUpdAudTR:inRegion:inGenAllDiv:
          inNoOutput:inGen301En:inGen311En:inListClms);
       //*** End Chg *** 10/28/20 *****************************************

      /End-Free

     p AudRpProc       e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid    = *off;
       erFnd      = *off;
       msFnd      = *blanks;
       msPExpToDt = *off;
       erPExpToDt = *blanks;
       msPExpFrDt = *off;
       erPExpFrDt = *blanks;
       msPolTyp   = *off;
       erPolTyp   = *blanks;
       erMutualEx = *off;
       msMutualEx = *blanks;
       erFyr      = *off;
       msFyr      = *blanks;
       erEmpDivNm = *off;
       msEmpDivNm = *blanks;
       erDrtRmk   = *off;
       msDrtRmk   = *blanks;
       //*** Beg Add *** 10/28/20 ******************************************
       erDiv      = *off;
       msDiv      = *blanks;
       //*** End Add *** 10/28/20 ******************************************

      /End-Free

     p ClrErr          e
       //===================================================================
       // Con2WLayout - Container 2 Widgets Layout
       //===================================================================
     p Con2WLayout     b
     d Con2WLayout     pi

      /Free

       // Container 2 Widgets Layout.
       scLSelCrtT = scCon2Top + 5;
       scLAudReqT = scCon2Top + 35;
       scIAudReqT = scCon2Top + 35;
       scLDrtRmkT = scCon2Top + 35;
       scDAudRPrT = scCon2Top + 50;
       scTDrtRmkT = scCon2Top + 50;
       scBDrtRmkT = scCon2Top + 50;
       //*** Beg Add *** 10/28/20 ******************************************
       If indNonMass = *off;
       //*** End Add *** 10/28/20 ******************************************
          scCUpdAuT  = scCon2Top + 75;
          scCUpdAuRT = scCon2Top + 95;
          scCNoOutpT = scCon2Top + 115;
       //*** Beg Add *** 10/28/20 ******************************************
       Else;
          scCGenDivT = scCon2Top + 75;
          scCUpdAuT  = scCon2Top + 95;
          scCUpdAuRT = scCon2Top + 115;
          scCNoOutpT = scCon2Top + 135;
       Endif;
       If isSnglEmp  = *on;
          scCGenDivT = scCon2Top + 50;
          scCUpdAuT  = scCon2Top + 75;
          scCUpdAuRT = scCon2Top + 95;
          scCNoOutpT = scCon2Top + 115;
       Endif;
       If isNotScrpt = *on;
          scCsRpayT  = scCon2Top + 75;
          scCPrtsRpT = scCon2Top + 95;
          scCGen301T = scCon2Top + 115;
          scCGen311T = scCon2Top + 135;
          scCclmAudT = scCon2Top + 155;
       Else;
          scCsRpayT  = scCon2Top + 50;
          scCPrtsRpT = scCon2Top + 70;
          scCGen301T = scCon2Top + 90;
          scCGen311T = scCon2Top + 110;
          scCclmAudT = scCon2Top + 130;
       Endif;
       //*** End Add *** 10/28/20 ******************************************
       scLComCdT  = scCon2Top + 160;
       scDComCd1T = scCon2Top + 175;
       scDComCd2T = scCon2Top + 195;
       scDComCd3T = scCon2Top + 215;
       scDComCd4T = scCon2Top + 235;
       scDComCd5T = scCon2Top + 255;
       scBCancelT = scCon2Top + 285;
       scBSubmitT = scCon2Top + 285;

      /End-Free

     p Con2WLayout     e
       //===================================================================
       // DrtRmkSrch  - Darts Remark Code Search
       //===================================================================
     p DrtRmkSrch      b
     d DrtRmkSrch      pi

      /Free

       inCall = '2';
       inKc   = '0';
       inKl   = '0';
       outRmcd = *Blanks;
       outDesc = *Blanks;
       outRtcd = *Blank;

       DartRmkSr(inCall:inKc:inKl:outRmcd:outDesc:outRtcd);

       scDrtRmk   = outRmcd + ' - ' + %Trim(outDesc);
       scDrtRmkCd = outRMCD;

      /End-Free

     p DrtRmkSrch      e
       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free

       inMode  = 'E';
       inCall  = '2';
       clear dsSrchCrit;
       clear inSelect;
       //*** Beg Chg *** 10/28/20 ********************************************
       //***dsEmpGrp  = scFnd;
       dsEmpGrp  = intOneFnd;
       //*** End Chg *** 10/28/20 ********************************************
       dsEmpName = scEmpDivNm;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';

          empRrn=%Dec(outRrn#:9:0);
          Chain(n) empRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain(n) keyWMEML pem00;
             If %Found(WMEML);
                scEmpDivNm = EMname;
                //*** Beg Chg *** 10/28/20 ***************************
                scEmp# = EMemp#;
                scDiv  = EMdiv;
                //scDivChr  = %Editc(EMdiv:'X');
                scFyr = ELfyr;
                //*** End Chg *** 10/28/20 ***************************
                scEmpDiv   = %EditC(EMemp#:'X') + %EditC(EMdiv:'X') +
                           %EditC(EMfnd:'X');
                scFnd = EMfnd;
                indpolyear = *off;
             EndIf;

          // Clear fyrChoices;
          //*** Beg Del *** 10/28/20 **************************************
          //***fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
          //*** + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
          //*** + ' AND ELdiv = ' + %char(ELdiv);
          //*** End Del *** 10/28/20 **************************************
          EndIf;
          // Clear divChoices;

       EndIf;

      /End-Free

     p EmployerSrch    e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       //*** Beg Del *** 10/28/20 ******************************************
       //***scFnd      = 0;
       //*** End Del *** 10/28/20 ******************************************
       scPExpToDt = 0;
       scPExpFrDt = 0;
       scPolTyp   = *Blanks;
       scEmpDivNm = *Blanks;
       scFyr      = 0;
       scAudRProc = 'N';
       scDrtRmk   = *Blanks;
       scDrtRmkCd = *Blanks;
       If (xxRrn <> *Zero And charMode = ' ');
          Chain xxRRN pel0p;
          If %Found(wdelp);
       //*** Beg Add *** 06/13/25 ******************************************
             xxEmp# = ELemp#;
             xxDiv = ELdiv;
       //*** End Add *** 06/13/25 ******************************************
       //*** Beg Add *** 10/28/20 ******************************************
             scDiv = ELdiv;
       //*** End Add *** 10/28/20 ******************************************
             Chain (ELco#:ELfnd) pex00;
             If %Found(wmexl) and EXauRq <> *blanks;
                Chain EXaurq pct01;
                If %Found (stctl1);
                   scDrtRmk   = CTrmCd + ' - ' + %Trim(CTdesc);
                   scDrtRmkCd = CTrmCd;
                Endif;
             Endif;
          Endif;
       Endif;
       scComCd1   = *Blanks;
       scComCd2   = *Blanks;
       scComCd3   = *Blanks;
       scComCd4   = *Blanks;
       scComCd5   = *Blanks;

       sccompany# = %Char(userCo#(q1user));
       indpolyear =*on;
       //*** Beg Add *** 10/28/20 ******************************************
       If Not UseScriptura(processType);
          isNotScrpt = *On;
       Endif;
       indNonMass =*off;
       If PriorSelection(IntCo#:Q1user:'W7230F':'G':outData);
          Monitor;
          scGenAllDv = %Subst(outData:1:1);
          scGen301En = %Subst(outData:2:1);
          scGen311En = %Subst(outData:3:1);
          scListClms = %Subst(outData:4:1);
          On-error;
          Endmon;
       Endif;
       //scDivChr = *blanks;
       //*** End Add *** 10/28/20 ******************************************
      /End-Free

     p LoadInVal       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou exitNow = *On ;

          SetCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          If selEmpSrch=*On;
             EmployerSrch();
          EndIf;

          If selDrtRmk = *On;
             DrtRmkSrch();
          EndIf;

          Select;
          When BtnSubmit = *on;

          // Clear Audit Request Fields
          ClrErr();

          If flgActPnl = 'N';
             ValidPnl1();
          EndIf;

          ValidPnl2();

          If isValid = *Off;
          //*** Beg Add *** 10/28/20 *******************************************
             mass = ' ';
             isSubmit = *on;
             If UseScriptura(processType) and scNoOutput <> 'Y';

                //If scEmp# <> 0 and scDiv <> *blanks;
                //   Mass = ' ';
                //Else;
                //   Mass = 'Y';
                //Endif;

                In *Lock *dtaara;

                Clear svLDA;

                xxDart = 'N';
                xxRMCD = ' ';

                // Get default DARTS remark code from the group
                Select;
                   When flgActPnl = 'Y';
                      Chain(n) (intCo#:intFnd) pex00;
                      If %Found(WMEXL) and EXauRq <> ' ';
                         xxDart = 'Y';
                         xxRmCd = EXauRq;
                      Endif;
                   Other;
                      Setll intCo# pgs01;
                      Reade intCo# pgs01;
                      dow not %Eof(WXGSL);
                         If GSieFl = 'I';
                            Chain(n) (intCo#:GSFnd) pex00;
                            If %Found(WMEXL) and EXauRq <> ' ';
                               xxDart = 'Y';
                               xxRmCd = EXauRq;
                              Leave;
                            Endif;
                         Endif;
                         Reade intCo# pgs01;
                      Enddo;
                Endsl;

                If isMultGrps = *on;
                   xxMultiG = 'Y';
                Else;
                   xxMultiG = ' ';
                Endif;

                Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
          //***** isSubmit = OutputWindow(processType:mass);
                If xxRRN <> 0 and scGenAllDv <> 'Y';
                   //*** Begin Add *** 05/16/23 *************************
                   Mass = ' ';
                   //*** End   Add *** 05/16/23 *************************
                   isSubmit = OutputWindow(processType:mass:' ':' ':xxRRN);
                Else;
                   //*** Begin Add *** 05/16/23 *************************
                   Mass = 'Y';
                   //*** End   Add *** 05/16/23 *************************
                   isSubmit = OutputWindow(processType:mass);
                Endif;
          //*** end chg *****  08/13/21  ***********************************
             Endif;

             // Save the screen selections
             If indNonMass = *on;
                %Subst(outData:1:1)= scGenAllDv;
             Endif;
             %Subst(outData:2:1) = scGen301En;
             %Subst(outData:3:1) = scGen311En;
             %Subst(outData:4:1) = scListClms;
             If indSlfRpt <> *on;
                %Subst(outData:5:1) = scSlfRptPy;
             Endif;
             If indPslfRpt <> *on;
                %Subst(outData:6:1) = scPslfRptP;
             Endif;

             PriorSelection(IntCo#:Q1user:'W7230F':'S':outData);
             If isSubmit = *off;
             Else;
          //*** End Add *** 10/28/20 *******************************************
             AudRpProc();
             Leave;
          //*** Beg Add *** 10/28/20 *******************************************
             Endif;
          //*** End Add *** 10/28/20 *******************************************
          EndIf;
          When BtnCancel = *on;
             Leave;
          EndSl;

          Select;
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          // Other Process
          Other;
             ProcOther();
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;

          If flgActPnl = 'Y';
             Write rcdPolPnl;
          Else;
             Write rcdComPnl;
          EndIf;
          Write rcdTabAuRq;

          Exfmt rcdAudRq;

          If flgActPnl = 'Y';
             Read rcdPolPnl;
          Else;
             Read rcdComPnl;
          EndIf;
          Read rcdTabAuRq;

       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProcOther - Process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

      /Free
      /End-Free

     p ProcOther       e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       If q1prms > 2;
          charMode = @@Mode;
       EndIf;

       If q1prms > 3;
          // charRRN  = @@RRN;
          // xxRrn    = %Dec(@@RRN:9:0);
          xxRrn = @@RRN;
       EndIf;

       //*** Beg Add *** 10/28/20 ******************************************
       // Do not Generate Output Checkbox will default to 'disabled'
       indNotOutp = *On;
       // Get the users company number
       intCo# = userCo#(q1user);
       //*** End Add *** 10/28/20 ******************************************

       // Intialize the Screen fields
       LoadInVal();

       // Previously Run Checkbox will default to 'disabled'
       indUpdAuTR = *On;

       // Load Action panel with the Company Name.
       //*** Beg Del *** 10/28/20 ******************************************
       //***intCo# = userCo#(q1user);
       //*** End Del *** 10/28/20 ******************************************
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) keyWmupl pup00;
       If %Found(Wmupl);
          charOutq = Upoutq;
       EndIf;

       // Get the client id
       GetClientID(##CltId);

       If (xxRrn <> *Zero And charMode = ' ');
       //*** Beg Chg *** 10/28/20 ******************************************
       //*** isSingleEmp = *on;
          isSnglEmp = *on;
       //*** End Chg *** 10/28/20 ******************************************
         // Audit Requests -  From Policy Dropdown Process
         AudtReqPolT();

       ElseIf (xxRrn = *Zero And charMode = '*');
          isAggregate = *on;
          // Audit Requests - From Aggregate Process
          AudReqAgg();

       Else;
          isMassRun = *on;
          // Audit Requests - Single/Mass Process
          AudReqMnu();
          //*** Beg Add *** 10/28/20 **************************************************************
          If exitNow;
             Return;
          Endif;
          //*** End Add *** 10/28/20 **************************************************************
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       indGrpRd = *Off;
       If scEmpDivNm <> *Blanks;
          indGrpRd = *On;
       EndIf;

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       If selEmpSrch=*On;
          fcEmpDivNm=*On;
       EndIf;
       selEmpSrch=*Off;

       If selDrtRmk =*On;
          fcDrtRmk  =*On;
       EndIf;
       selDrtRmk = *Off;

       indUpdAudT = *Off;

       charKey = %Char(ELbscd);
       Chain(n) keyWtabl pbs01;
       If %Found(Wtabl);
          If BSsefl = 'E';
             indSlfRpt  = *On;
             indPSlfRpt = *On;
          Else;
             indSlfRpt  = *Off;
             indPSlfRpt = *Off;
          EndIf;

       EndIf;
        //*** Beg Add *** 10/28/20 ***************************************
       If indSlfRpt <> *on;
          scSlfrptPy = %Subst(outData:5:1);
          scPslfRptP = %Subst(outData:6:1);
       Endif;
        // Clear divChoices;
       empChoices = 'EMco# = ' + %char(intCo#) + ' AND EMfnd = '
        + %char(intOneFnd) + ' and EMstRp = ''Y''';
       divChoices = 'EMco# = ' + %char(intCo#) + ' AND EMfnd = '
        + %char(intOneFnd) + ' and EMemp# = ' + %char(EMemp#);
        //scDivChr = *blanks;
        //*** End Add *** 10/28/20 ***************************************

      /End-Free

     p SetCriteria     e
       //===================================================================
       // ValidPnl1 - Validate Panel 1 Screen Fields
       //===================================================================
     p ValidPnl1       b
     d ValidPnl1       pi

      /Free

       //*** Beg Del *** 10/28/20 ******************************************
       //***If scfnd = *Zeros;
       //***  erFnd      = *On;
       //***  msFnd      = 'WCR0503';
       //***  isValid    = *On;
       //***EndIf;
       //*** Beg Del *** 10/28/20 ******************************************

       If indMassReq = *On;

          //*** Beg Chg *** 10/28/20 ***************************************
          //***If scEmpDivNm = *Blanks;
          If scEmp# = *zeros;
          //*** End Chg *** 10/28/20 ***************************************

          // Validation Check for Expiration To Date.
             erPExpToDt = ValidDate(scPExpToDt:charMsg);
             If erPExpToDt = *on;
                msPExpToDt = charMsg;
                isValid    = *on;
             Endif;

          // Validation Check for Expiration From Date.
             erPExpFrDt = ValidDate(scPExpFrDt:charMsg);
             If erPExpFrDt = *on;
                msPExpFrDt = charMsg;
                isValid    = *on;
             Endif;

             If erPExpToDt = *off and erPExpFrDt = *off;
                erPExpToDt = AscendDates(scPExpToDt:scPExpFrDt:charMsg);
                If erPExpToDt = *on;
                   erPExpFrDt = *on;
                   msPExpToDt = charMsg;
                   msPExpFrDt = charMsg;
                   isValid    = *on;
                Endif;
             Endif;

          Endif;

          // Validation Check for Mutual Exclusive.
          //*** Beg Chg *** 10/28/20 ***************************************
          //***If (scPExpToDt <> 0 And scPExpFrDt <> 0) And (scEmpDivNm <> *Blanks);
          If (scPExpToDt <> 0 And scPExpFrDt <> 0) And (scEmp# <> *Zeros);
          //*** End Chg *** 10/28/20 ***************************************
            erMutualEx = *On;
            msMutualEx = 'WCR0590';
            isValid    = *On;
          EndIf;

          // Validation Check for Policy Type.
          If (scPExpToDt <> 0 And scPExpToDt <> 0) And (scPolTyp = *Blank);
             erPolTyp   = *On;
             msPolTyp   = 'WCR0503';
             isValid    = *On;
          EndIf;

       Endif;

       // Validation Check for Employer/Division
       //*** Beg Chg *** 10/28/20 ******************************************
       //***If (scPExpToDt = 0 And scPExpFrDt = 0)  And (scEmpDivNm = *Blanks);
       If (scPExpToDt = 0 And scPExpFrDt = 0) And (scEmp# = *zeros);
       //*** End Chg *** 10/28/20 ******************************************
          erEmpDivNm = *On;
          msEmpDivNm = 'WCR0503';
          isValid    = *On;
       EndIf;

       // Validation Check for Year.
       //*** Beg Chg *** 10/28/20 **************************************
       //***If (scEmpDivNm <> *Blanks  And scFyr = *Zeros);
       If (scEmp# <> *Zeros  And scFyr = *Zeros);
       //*** End Chg *** 10/28/20 **************************************
         erFyr      = *On;
         msFyr      = 'WCR0503';
         isValid    = *On;
       EndIf;

       // Validation Check for Darts Remarks only is Script not installed
       //*** Beg Chg *** 10/28/20 ***************************************
       //***If (scDrtRmk = *Blanks);
       If (scDrtRmk = *Blanks and Not UseScriptura(processType));
       //*** End Chg *** 10/28/20 ***************************************
         erDrtRmk   = *On;
         msDrtRmk   = 'WCR0503';
         isValid    = *On;
       EndIf;

      /End-Free

     p ValidPnl1       e
       //===================================================================
       // ValidPnl2 - Validate Panel 2 Screen Fields
       //===================================================================
     p ValidPnl2       b
     d ValidPnl2       pi

      /Free

       // Validation Check for Darts Remarks.
       //*** Beg Chg *** 10/28/20 ***************************************
       //***If (scDrtRmk = *Blanks);
       If (scDrtRmk = *Blanks and Not UseScriptura(processType));
       //*** End Chg *** 10/28/20 ***************************************
         erDrtRmk   = *On;
         msDrtRmk   = 'WCR0503';
         isValid    = *On;
       EndIf;

      /End-Free

     p ValidPnl2       e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
