     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*NEW)
     H/define CONVERT
     H/TITLE WTPOLESTBL - Prompt for Estimated Bill
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H* Created on 08/08/2018 by c:62 for Project 150WCS              *
     H*****************************************************************
     H* Revisions:                                                    *
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
     H* 10/15/24 C:320 Project: WC2984                                *
     H*                Allow deleted summarized divisions to be picked*
     H*                for A/R reconciliation.                        *
     H*                                                               *
     H* 09/27/24 C:291 Support: 117660                                *
     H*                Added new message for Unbilled Divisions.      *
     H*                Show edit message "Division will be billed"    *
     H*                for divisions with no Error.                   *
     H*                                                               *
     H* 07/24/24 C:300 Support: 116983                                *
     H*                Email Body Not Pulling Tokens                  *
     H*                Add RRN to OutputWindow procedure to populate  *
     H*                email addresses and email tokens               *
     H*                                                               *
     H* 05/03/24 C:062 Project: WC2670                                *
     H*                Correct how process determines mass run.       *
     H*                                                               *
     H* 03/12/24 C:291 Project: WC2670                                *
     H*                Ensure at least one division has active payroll*
     H*                exist. Otherwise don't allow to submit the Bill*
     H*                Added 2nd error message on scEmp#.             *
     H*                Clear the Remark code if 3rd party billing is  *
     H*                set on.                                        *
     H*                                                               *
     H* 01/30/24 C:291 Project: WC2670                                *
     H*                Correct Scriptura Bill prompt to display after *
     H*                Multiple division message window.              *
     H*                                                               *
     H* 05/05/22 C:291 Project: WC2670                                *
     H*                Add processing to display output control window*
     H*                for Estimated Bills when Scriptura is requested*
     H*                                                               *
     H* 08/19/21 C:318 Support:97181                                  *
     H*                Changed to correct the value of @@call         *
     H*                                                               *
     H* 04/26/21 C:180 Project:WC1986                                 *
     H*                Add starting policy year and Quote Env Flag    *
     H*                parms to GetEftStatus                          *
     H*                                                               *
     H* 03/30/21 C:62  Project: WC2663                                *
     H*                Add IC processing.                             *
     H*                                                               *
     H* 06/10/20 C:65  Support: 89482 (Sunz)                          *
     H*                Change GetDivCount to use a 5.0 size field     *
     H*                instead of 3.0 size policies can have more     *
     H*                than 999 divisions.  Screen change as well     *
     H*                on scDivCnt.                                   *
     H*                                                               *
     H* 03/16/20 C:180 Project wc2700                                 *
     H*                Allow non-renewal cancellation override.       *
     H*          C:136 Reimplement edit to prevent est bill from      *
     H*                running on cx'd policies.                      *
     H*   !!!!!!!!!!!! Do not EBF this code without the rest !!!!!!!!!!!!!!!
     H*   !!!!!!!!!!!! of the project. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*                                                               *
     H* 03/02/20 C:318 Support: 87198                                 *
     H*                Updated display file WTPOLESTBL, corrected     *
     H*                the tab order on quote bill screen.            *
     H*                                                               *
     H* 02/20/20 C:318 Support: 87396                                 *
     H*                Corrected button positions when called from    *
     H*                the menu.                                      *
     H*                                                               *
     H* 12/09/19 C:298 Project WC2691                                 *
     H*                Add Multi-select box for Multiple Employers    *
     H*                for 'Quote Status'.                            *
     H*                                                               *
     H* 11/05/19 C:65  Support: 83772 (LUBA)                          *
     H*                Rechain to user file in case outqueue changed. *
     H*                                                               *
     H* 10/28/19 C:65  Support: 83700 (LUBA)                          *
     H*                Remove SetCriteria calls from the submit       *
     H*                section - it was resetting all the fields in   *
     H*                middle of processing.  Do however SetCriteria  *
     H*                when a group change occurs to reset defaults.  *
     H*                Fix major error when 08/21/19 was done - the   *
     H*                bill option drop down was defaulting value to  *
     H*                blank, which wiped out value for mass bills.   *
     H*                                                               *
     H* 10/08/19 C:62  Support: 83413 (150)                           *
     H*                Correct error check on unbilled.               *
     H*                                                               *
     H* 08/27/19 C:65  Support: 82395 (LUBA)                          *
     H*                There is no mode 4 in quote, so check on mode  *
     H*                2 for enabling Darts on prompt screen.         *
     H*                                                               *
     H* 08/21/19 C:212 Support: 81158                                 *
     H*                * Show blank as default in DSPF-Bill Options   *
     H*                * Re-order the options in DSPF-Bill Options    *
     h*                * Corrected Cancel button functionality.       *
     h*                Added Bypass Validation to cancel button-As it *
     h*                was requiring value for Audit bill option field*
     H*                                                               *
     H* 07/11/19 C:65  Support: 80878 (LUBA)                          *
     H*                Fix security check on mass bill.               *
     H*                                                               *
     H* 05/03/19 C:180 Project 133WRA                                 *
     H*                Remove edit on hard copy flag when Agent 2nd   *
     H*                copy is selected.                              *
     H*                                                               *
     H* 03/27/19 C:298 Support: 77929                                 *
     H*                Correct Population of Interim Days over        *
     H*                screen prompt.                                 *
     H*                                                               *
     H* 03/22/19 C:65  Support: 77782 (Synergy)                       *
     H*                Correct additional edit on ACH setup to only   *
     H*                happen on live bills and change error message. *
     H*                                                               *
     H* 12/27/18 C:136 Support: 75794                                 *
     H*                Correct edits that prevent original bill       *
     H*                from deleting prior month A/R or from          *
     H*                running on a EFT policy again.                 *
     H*****************************************************************
     fwtpolestblcf   e             workstn extfile('WTPOLESTBL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflBillEdt:rrnBillEdt)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     f                                     infds(FIDS01)
     *
     fsmcol     if   e           k disk
     fwmupl     if   e           k disk    Rename(pup01:pup00)
     fwmupl1    if   e           k disk    Rename(pup01:pup01)
     fwmaxl     if   e           k disk    Rename(pax01:pax00)
     fwtabl     if   e           k disk
     f                                     Ignore(psy01)

     fwabll     uf   e           k disk    Rename(pbl01:pbl00)
     fwabll3    uf   e           k disk    Rename(pbl01:pbl03)
     fwdeal4    if   e           k disk    Rename(pea01:pea04)
     fwdehl     if   e           k disk    Rename(peh01:peh00)
     fwdehl13   if   e           k disk    Rename(peh01:peh13)
     fwdelp     if   e             disk    Rename(pel01:pel0p)
     fwdell     if   e           k disk    Rename(pel01:pel00) InfDs(dbfds)
     fwdell1    uf   e           k disk
     fwdf1l     if   e           k disk    Rename(pf101:pf100)
     Fwdrrl     if   e           k disk    Rename(prr01:prr00)
     fwmdbl3    if   e           k disk    Rename(pdb01:pdb03)
     fwmeal6    if   e           k disk    Rename(pea01:pea06)
     fwmefl6    if   e           k disk    Rename(pef01:pef06)
     fwmeml     if   e           k disk    Rename(pem01:pem00)
     fwmemp     if   e             disk    Rename(pem01:pem0p)
     fwmepl1    if   e           k disk    Rename(pep01:pep01)
      //*** Beg Add ***** 03/12/24 ****************************************
     fwmepl7    if   e           k disk    Rename(pep01:pep07)
      //*** End Add ***** 03/12/24 ****************************************
     fwmexl     if   e           k disk    Rename(pex01:pex00)
     fwmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fwmgcl     if   e           k disk    Rename(pgc01:pgc00)
     fwmldl1    if   e           k disk    Rename(pld01:pld01)
     fwmrpl4    if   e           k disk    Rename(PRP01:PRP04)
     Fwmrsl     if   e           k disk    Rename(prs01:prs00)
     fwxdvp     uf a e           k disk    Rename(pdv01:pdv0p) UsrOpn
     fstctl1    if   e           k disk    Rename(pct01:pct01)
     fssylf     if   e           k disk    Rename(ssy01:ssy00)
     f                                     Ignore(spc01)
     f                                     Ignore(sfb01)
     f                                     Ignore(src01)
     f                                     Ignore(sdr01)
     f                                     Ignore(sbc01)
     f                                     Ignore(scs01)
     f                                     Ignore(sgl01)
     f                                     Ignore(sp201)
      //*** Beg Add ***** 05/05/22 ***********************************
     fwsbll     if   e           k disk    Rename(pbl01:pbl001)
      //*** End Add ***** 05/05/22 ***********************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     fWDCXL     IF   E           K Disk
      *** End Add ***  10/15/24  ***  WC2984  ************************
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0
      *
     d  commentCdDs    ds
     d  ##Comm1                1      2
     d  ##Comm2                3      4
     d  ##Comm3                5      6

     d                 ds
     d  ef                     1    035  0
     d                                     Dim(5)                               EFF.DATE
     d  EMefD1               001    007  0
     d  EMefD2               008    014  0
     d  EMefD3               015    021  0
     d  EMefD4               022    028  0
     d  EMefD5               029    035  0

     d                 ds
     d  cx                     1     35  0
     d                                     DIM(5)
     d  EMcaD1                 1      7  0
     d  EMcaD2                 8     14  0
     d  EMcaD3                15     21  0
     d  EMcaD4                22     28  0
     d  EMcaD5                29     35  0

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
     d strCMCodes      s                   Like(scCMCodes)
     d dtlRrn          s              9  0
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
     d svARSM          s                   Like(ELARSM)
     d svRemarkCd      s                   Like(scRemarkCd)
     d ##Co#           s              3  0
     d ##Fnd           s              3  0
     d ##Emp#          s              9  0
     d company#        s              3  0
     d massRun         s               n   Inz(*Off)
     d singleDiv       s               n   Inz(*Off)
     d saveDiv         s                   Like(ELdiv)
     d call            s              1  0
     d program         s             10    Inz('WA073')
     d objType         s              7    Inz('*PGM')
     d newACH          s              1    Inz(*Off)
     d eftErrMsg       s               n   Inz(*Off)
     D key             s                   Like(BSkey)
     d isSelf          s               n   Inz(*Off)
     d isAccount       s               n   Inz(*Off)
     d isArOnly        s              1    Inz(*Off)
     d divExists       s               n   Inz(*Off)
     d seen            s               n   Inz(*Off)
     d allUnbilled     s               n   Inz(*Off)

     d ##CoC#          s              3
     d ##FndC          s              3
     d ##FyrC          s              3
     d ##EmpC#         s              9
     d ##DivC          s              5
     d ##PostDat       s              8
     d ##EstTrnCd      s              2
     d ##DueDate       s              8
     d ##PrtBal        s              1
     d ##SlfEstFl      s              1
     d ##Call          s              1
     d ##Override      s              1
     d ##SrCode        s              2
     d ##SrAdjCode     s              2
     d ##Quote         s              1
     d ##OutQ          s             10
     d ##NewRenew      s              1
     d ##DartsFlg      s              1
     d ##HardCopy      s              1
     d ##AgentOrder    s              1
     d ##FinalQuote    s              1
     d ##Rmkcode       s              5
     d ##ArCode        s              2
     d ##EffDate       s              8
     d ##FreeComm1     s             30
     d ##FreeComm2     s             30
     d ##FreeComm3     s             30
     d ##ArCode2       s              2
     d ##BillSel       s              1
     d ##Library       s             10    Inz(' ')

     d ##Ctype         s              1
     d ##Clvl          s              3
     d ##Utype         s              1
     d ##Ulvl          s              3
     d ##Exec          s              1
     d ##Results       s              1

     d ##Fyr           s              3  0
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
     d ##EfDtC         s              8
     d ##Err           s              1
     d ##DataSR        s            100
      //*** Beg Add ***** 03/12/24 ******************************************
     d flgPyrlFnd      s              1
      //*** End Add ***** 03/12/24 ******************************************
      *

     d/copy scopypr01d
     d/copy sPrSecure
     d/copy sPrCompany
     d/copy wPrBank
     d/copy wPrEmpLr
     d/copy wprEft
     D/copy sPrAtrium
     d/copy sPrDate
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy wtSearchDs
     d/copy sPrString
     d/copy sPrGenPop
     d*** Begin Add *** 03/30/21 *******************************************
     d/copy sPrModule
     d/copy wPrPmtSvc
     d*** End   Add *** 03/30/21 *******************************************
     d*** Beg Add ***** 05/05/22 *******************************************
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     s            100
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
     * SRCHKOBJ Call
     d SrChkObj        pr                  Extpgm('SRCHKOBJ')
     d  program                      10
     d  objType                       7
     d  newACH                        1

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

     * WtPolEstCS
     d CallWtPolEstCS  pr                  ExtPgm('WTPOLESTCS')
     d ##CoC#                         3    Const
     d ##FndC                         3    Const
     d ##FyrC                         3
     d ##EmpC#                        9
     d ##DivC                         5
     d ##PostDat                      8
     d ##EstTrnCd                     2
     d ##Comm1                        2
     d ##Comm2                        2
     d ##Comm3                        2
     d ##DueDate                      8
     d ##PrtBal                       1
     d ##SlfEstFl                     1
     d ##Call                         1
     d ##Override                     1
     d ##SrCode                       2
     d ##SrAdjCode                    2
     d ##Quote                        1
     d ##OutQ                        10
     d ##NewRenew                     1
     d ##DartsFlg                     1
     d ##HardCopy                     1
     d ##AgentOrder                   1
     d ##FinalQuote                   1
     d ##Rmkcode                      5
     d ##ArCode                       2
     d ##EffDate                      8
     d ##FreeComm1                   30
     d ##FreeComm2                   30
     d ##FreeComm3                   30
     d ##ArCode2                      2
     d ##BillSel                      1
     d ##Library                     10

     * WtPolEstFC
     d CallWtPolEstFC  pr                  ExtPgm('WTPOLESTFC')
     d  ##Library                    10
     d  ##Action                      1    Const
      *
     d callSRCCSC      pr                  Extpgm('SRCCSC')
     d  ##Ctype                       1
     d  ##Clvl                        3
     d  ##Utype                       1
     d  ##Ulvl                        3
     d  ##Exec                        1
     d  ##Results                     1

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

    d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      * SR Online
     d SROnline        pr                  ExtPgm('SRONLI')
     d ##CoC#                         3
     d ##FndC                         3
     d ##EmpC#                        9
     d ##DivC                         5
     d ##FyrC                         3
     d ##EfDtC                        8
     d ##Mode                         1
     d ##Err                          1
     d ##DataSR                     100

     *Entry parameter
     d Main            pr                  Extpgm('WTPOLESTBL')
     d  @@Call                        1
     d  @@Qte                         1
     d  @@KC                          1
     d  @@KL                          1
     d  @@Rrn                         9
     d Main            pi
     d  @@Call                        1
     d  @@Qte                         1
     d  @@KC                          1
     d  @@KL                          1
     d  @@Rrn                         9

      /Free

         ProgramStart();
         LoadScreen();
         MainProcess();
         *InLr = *on;
      /End-Free

       //===================================================================
       // CalPostDat - Calculate Posting Date
       //===================================================================
     p CalPostDat      b
      /Free

        //***** Beg Add ***** 03/27/19 *******************************
        ##Agent#  = 0;
        ##AgentNm = ' ';
        ##Prod#   = 0;
        ##ProdNm  = ' ';
        ##Area    = 0;
        ##Phone   = 0;
        ##Plan    = '*';
        ##Date    = dtMDYY(ELptDt);
        CallWA002R(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:##Agent#:##AgentNm:
         ##Prod#:##ProdNm:##Area:##Phone:##Plan:##Date);

        If ##plan = 'A';
           strAcct = 'Y';
        Else;
           strAcct = ' ';
        Endif;
        //***** End Add ***** 03/27/19 *******************************
         Chain (intCo#:scFnd) pex00;
         If %Found(wmexl) and strAcct = 'Y';
        //***** Beg Chg ***** 03/27/19 *******************************
            //*****intDays = EXaaid;
            intDays = EXaoID;
         Else;
            //*****intDays = EXauid;
            intDays = EXrvID;
        //***** End Chg ***** 03/27/19 *******************************
         EndIf;

         // Posting Date (Audit Interim Days Plus Current Date)
         scPostDat = dtfuture(dateto8(systemDate()):intDays);
         If scPostDat = *Zeros;
            scPostDat = dateto8(systemDate());
         EndIf;

      /End-Free
     p CalPostDat      e

      //===================================================================
      // ChkArTrans - Check AR Transactions
      //===================================================================
     P ChkArTrans      b
     D ChkARTrans      pi              n

     D found           s              1    Inz(*Off)

     d                 DS
     d  currentDate            1      7  0
     d  sysYrMnth              1      5  0
     d  sysDays                6      7  0

     d                 DS
     d  postDate               1      7  0
     d  postYrMnth             1      5  0
     d  postDays               6      7  0

     d                 DS
     d  tranDate               1      7  0
     d  tranYrMnth             1      5  0
     d  tranDays               6      7  0
      /Free

       currentDate = DateTo7(systemDate());
       If Not singleDiv;
          Chain(n) (company#:scFnd:scPolYear:scEmp#) pel01;
          If %Found(wdell1);
             DoU %Eof(wdell1);
                //*** Begin Add *** 10/08/19 *******************************
                If processDiv(ELco#:ELfnd:ELemp#:ELdiv:scPolYear:
                                      scFromDiv:scToDiv:scBillSel);
                //*** End   Add *** 10/08/19 *******************************
                   Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pea06;
                   If %Found(wmeal6);
                      DoU %Eof(wmeal6);
                         If EAarTy = 'A' Or EAarTy = 'B' Or EAarTy = 'H';
                            postDate = EApoDt;
                            tranDate = EAtrDt;
                            If postYrMnth < sysYrMnth And
                               tranYrMnth < sysYrMnth;
                               Return *On;
                            EndIf;
                         EndIf;
                         ReadE (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pea06;
                      EndDo;
                   EndIf;
                //*** Begin Add *** 10/08/19 *******************************
                EndIf;
                //*** End   Add *** 10/08/19 *******************************
                ReadE(n) (company#:scFnd:scPolYear:scEmp#) pel01;
             EndDo;
          EndIf;
       Else;
          // Validate A/R on division only for single division call,
          // and/or A/R posted for state reporting division if A/R
          // summarized.
          Chain (company#:scFnd:scEmp#:scDiv:scPolYear) pea06;
          If %Found(wmeal6);
             DoU %Eof(wmeal6);
                If EAarTy = 'A' Or EAarTy = 'B' Or EAarTy = 'H';
                   postDate = EApoDt;
                   tranDate = EAtrDt;
                   If postYrMnth < sysYrMnth And
                      tranYrMnth < sysYrMnth;
                      Return *On;
                   EndIf;
                EndIf;
                ReadE (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pea06;
             EndDo;
          EndIf;

          // Check state reporting division for A/R summarized divisions.
          If ELarSm = 'Y';
             Chain (company#:scFnd:scEmp#) pem00;
             If %Found(wmeml);
                DoU %Eof(wmeml) Or EMstRp = 'Y';
                   ReadE (company#:scFnd:scEmp#) pem00;
                EndDo;
             EndIf;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:scPolYear) pel00;
             Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pea06;
             If %Found(wmeal6);
                DoU %Eof(wmeal6);
                   If EArDiv = saveDiv And
                      (EAarTy = 'A' Or EAarTy = 'B' Or EAarTy = 'H');
                      postDate = EApoDt;
                      tranDate = EAtrDt;
                      If postYrMnth < sysYrMnth And
                         tranYrMnth < sysYrMnth;
                         Chain (company#:scFnd:scEmp#:scDiv) pem00;
                         Chain (EMco#:EMfnd:EMemp#:EMdiv:scPolYear) pel00;
                         Return *On;
                      EndIf;
                   EndIf;
                   ReadE (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) pea06;
                EndDo;
             EndIf;
             Chain (company#:scFnd:scEmp#:scDiv) pem00;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:scPolYear) pel00;
          EndIf;
       EndIf;

       Return *Off;

      /End-Free
     P ChkARTrans      e

      //===================================================================
      // ChkNewACH - Check and see if we are using the new ACH process
      //===================================================================
     P ChkNewACH       b
     D ChkNewACH       pi              n

     D found           s              1    Inz(*Off)

      /Free

       SrChkObj(program:objType:newACH);

       Return newACH;

      /End-Free
     P ChkNewACH       e

      //===================================================================
      // ClearErrors- Clear Error/Message fields
      //===================================================================
     p ClearErrors     b
     d ClearErrors     pi
      /Free

       erAdjArCd  = *Off;
       msAdjArCd  = ' ';
       erAgtCopy  = *Off;
       msAgtCopy  = ' ';
       erBillSel  = *Off;
       mgBillSel  = ' ';
       erCmCodes  = *Off;
       msCmCodes  = ' ';
       erDiv      = *Off;
       msDiv      = ' ';
       erEmp#     = *Off;
       msEmp#     = ' ';
       erEstArCd  = *Off;
       msEstArCd  = ' ';
       erFCommnt  = *Off;
       msFCommnt  = ' ';
       erFnd      = *Off;
       msFnd      = ' ';
       erFnlQte   = *Off;
       msFnlQte   = ' ';
       erFromDiv  = *Off;
       mgFromDiv  = ' ';
       erGenHrdC  = *Off;
       msGenHrdC  = ' ';
       erIntDays  = *Off;
       msIntDays  = ' ';
       erMode     = *Off;
       msMode     = ' ';
       erPolYear  = *Off;
       msPolYear  = ' ';
       erPostDat  = *Off;
       msPostDat  = ' ';
       erPrtBal   = *Off;
       msPrtBal   = ' ';
       erRemarkCd = *Off;
       msRemarkCd = ' ';
       erRsvDrts  = *Off;
       msRsvDrts  = ' ';
       erMsgFile  = 'WMESGG';
       erSrArCd   = *Off;
       msSrArCd   = ' ';
       erToDiv    = *Off;
       mgToDiv    = ' ';
       erBatch#   = *Off;
       msBatch#   = ' ';
       //*** Beg Add ***** 03/12/24 *******************************
       erEmp#1    = *Off;
       msEmp#1    = ' ';
       //*** End Add ***** 03/12/24 *******************************

      /End-Free

     p ClearErrors     e

      //===================================================================
      // ClrDivFile - Clear the division work file (WXDVP)
      //===================================================================
     P ClrDivFile      b
     D ClrDivFile      pi

      /Free

       Chain (company#:scFnd:scEmp#) pdv0p;
       If %Found(wxdvp);
          DoU %Eof(wxdvp);
             Delete wxdvp;
             ReadE (company#:scFnd:scEmp#) pdv0p;
          EndDo;
       EndIf;

      /End-Free

     P ClrDivFile      e

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
       // DivErrorChk - Division Error Check
       //===================================================================
     p DivErrorChk     b
     d DivErrorChk     pi           100
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outEdtPgm                    10
     d  outEdtTab                     5s 0
     d  outEdtRrn                     9s 0

     d currentDate     s              7  0
     d cancelDate      s              7  0 Inz(0)
     d i1              s              2  0 Inz(1)
     d errorMessage    s            150    Inz
     d prvBill         s               n   Inz(*Off)
     d outCo#          s              3
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate         s              7
     d outPreNote      s              1
     D key             s                   Like(BSkey)
     d isEFT           s               n   Inz(*Off)
     d isError         s               n   Inz(*off)
      *** Begin Add *** 03/16/20 ****************************************
     d outMode         s              3    Inz('BIL')
      *** End   Add *** 03/16/20 ****************************************
       //*** Beg Add  ***** 04/26/21 ************************
       Dcl-S outRrn  char(9)  inz('000000000');
       Dcl-S outEobT char(3)  inz;
       Dcl-S outCkTp char(10) inz;
       Dcl-S outFyr  char(3)  inz;
       Dcl-S outEnv  char(1)  inz;
       //*** End  Add ***** 04/26/21 ************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     d  xxReq#         s                   Like(CXREQ#)
      *** End Add ***  10/15/24  ***  WC2984  ************************

     d                 DS
     d  postDate               1      7  0
     d  postYrMnth             1      5  0
     d  postDays               6      7  0

     d                 DS
     d  tranDate               1      7  0
     d  tranYrMnth             1      5  0
     d  tranDays               6      7  0
      /Free

       currentDate = DateTo7(SystemDate());

       i1 = %LookUp(0:ef);
       If i1 = 0;
          i1 = 5;
       Else;
          i1 = i1 - 1;
       EndIf;

       Chain (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pel00;
       If %Found(wdell);
          outEdtRrn = dbRRN;
       EndIf;

       outEdtPgm = ' ';
       outEdtTab = 0;

       If i1 <> 0 And cx(i1) <> 0;
          cancelDate = cx(i1);
       EndIf;

       //*** Begin Add *** 03/16/20 **************************************
       If cancelDate <> 0 And
        OvrRenewalCx(outMode:inCompany#:inGroup#:
        inEmployer#:inDivision:inYear:cancelDate) = *On;
          cancelDate = 0;
       Endif;
       //*** End   Add *** 03/16/20 **************************************
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
                xxReq# = *Hival;
                SetGt (EMco#:EMfnd:scPolYear:EMemp#:EMdiv:xxReq#) PCX01;
                If %Found(WDCXL);
                   ReadPE (EMco#:EMfnd:scPolYear:EMemp#:EMdiv) PCX01;
                   If Not %EOF(WDCXL) and CXEnFl = ' ';
                      cancelDate = 0;
                   EndIf;
                EndIf;
      *** End Add ***  10/15/24  ***  WC2984  ************************

       // Do not allow billing if employer cancelled.
       If cancelDate <> 0;
          errorMessage = 'Estimated bill is not allowed on a ' +
                         'cancelled policy.';
          outEdtPgm = 'WTPOLCXRN';
          outEdtTab = 0;
          Return errorMessage;
       EndIf;

       // Schedule Debit/Credit factor must not exceed entered
       // maximum values.
       Chain (ELco#:ELrtCd) prs00;
       If %Found(wmrsl);
          SetGT  (ELco#:ELfnd:RSjrSt:ELpfDt) prr00;
          ReadPE (ELco#:ELfnd:RSjrSt:ELpfDt) prr00;
          If Not %Eof(wdrrl) And ELdbCr <> 0;
             If ELdbCr > 1.0;
                If ELdbCr > RRdMax And RRdMax <> 0;
                   errorMessage = 'Schedule Debit Factor is greater ' +
                                  'than the maximum allowed in ' +
                                  'Statistical Reporting/Rating record.';
                   outEdtPgm = 'WTPOLDTL';
                   outEdtTab = 0;
                   Return errorMessage;
                EndIf;
             EndIf;

             If ELdbCr < 1.0;
                If ELdbCr < RRcMax And RRcMax <> 0;
                   errorMessage = 'Schedule Debit Factor is greater ' +
                                  'than the maximum allowed in ' +
                                  'Statistical Reporting/Rating record.';
                   outEdtPgm = 'WTPOLDTL';
                   outEdtTab = 0;
                   Return errorMessage;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

       // Turn flag on here if opt 30 would rerun an original bill
       // with a datesubsequent to the first run. The flag will be looked
       // at in the opt 30subroutine to prevent A/R from being changed.
       If @@Qte <> 'Q'  And GetAchFlg() = *On;
          SetLL  (ELco#:ELfnd:ELfYr:ELemp#:currentDate) peh13;
          ReadPE (ELco#:ELfnd:ELfYr:ELemp#:currentDate) peh13;
          DoW Not %Eof(wdehl13);

             // Check if new ACH module is installed ('WC1806')
             If ChkNewACH();
                Chain (ELco#:ELfnd:ELemp#:ELdiv) pef06;
                If %Found(wmefl6);
                   DoU %Eof(wmefl6);
                      If (currentDate < EFexDt And currentDate >= EFefdt) Or
                         (EFexDt = 0 And currentDate >= EFefDt);
                         prvBill = *On;
                         Leave;
                      EndIf;
                      ReadE (ELco#:ELfnd:ELemp#:ELdiv) pef06;
                   EndDo;
                EndIf;
                *in62 = *Off;
                If prvBill = *On;
                   Leave;
                EndIf;
             Else;
                SetGt  (ELco#:ELfnd:ELemp#:ELdiv:currentDate) pdb03;
                ReadPE (ELco#:ELfnd:ELemp#:ELdiv) pdb03;
                If (Not %Eof(wmdbl3) And DBexDt = 0) Or
                   (Not %Eof(wmdbl3) And DBexDt <> 0 And
                    currentDate < DBexDt);
                   prvBill = *On;
                   Leave;
                EndIf;
             EndIf;
             ReadPE (ELco#:ELfnd:ELfYr:ELemp#:currentDate) peh13;
          EndDo;
       EndIf;

       If prvBill = *On And (scMode = '3' Or scMode = '4');
          errorMessage = 'An estimated bill has already been run for this ' +
                         'ACH employer.  ACH employers cannot have ' +
                         'estimated bills run on different days.';
          outEdtPgm = ' ';
          outEdtTab = 0;
          Return errorMessage;
       EndIf;

       // Only allow bill to be run if a carrier policy # is entered
       // for all divisions
       ExSr Carrier#;
       If errorMessage <> ' ';
          outEdtPgm = 'WTPOLLMST';
          outEdtTab = 0;
          Return errorMessage;
       EndIf;

       // Employer EFT setup is required
       Exsr ChkEFT;
       If errorMessage <> ' ';
          outEdtPgm = 'WTPOLLMST';
          outEdtTab = 4;
          Return errorMessage;
       EndIf;

       //------------------------------------------------------------

       // Do not allow single division original bill if a billing has
       // already been run.
       If scEmp# <> 0 and scMode = '4' And singleDiv;
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfYr) peh00;
          If %Found(wdehl);
             outEdtPgm = ' ';
             outEdtTab = 0;
             errorMessage = 'Billing has already been run.';
             Return errorMessage;
          EndIf;
       EndIf;

       // Do not allow a non-print only bill outside of quote if no
       // valid ACH information for a policy with online S/R processing.
       //*** beg chg ***** 12/27/18 ***********************
       //***If isQuote = *On And scMode <> '1' And scEmp# <> 0;
       If isQuote = *Off And scMode <> 'b' And scEmp# <> 0;
       //*** end chg ***** 12/27/18 ***********************
          ##CoC#   = %Editc(ELco#:'X');
          ##FndC   = %Editc(ELfnd:'X');
          ##EmpC#  = %Editc(ELemp#:'X');
          ##DivC   = %Editc(ELdiv:'X');
          ##FyrC   = %Editc(ELfYr:'X');
          ##EfDtC  = %EditC(dtCheck(ELpfDt):'X');
          ##Mode   = '1';
          ##Err    = ' ';
          ##DataSR = ' ';
          SROnline(##CoC#:##FndC:##EmpC#:##DivC:##FyrC:##EfDtC:##Mode:
                   ##Err:##DataSR);

          If ##err = '1';
             outEdtPgm = 'WTPOLDTL';
             outEdtTab = 1;
             errorMessage = 'No valid ACH information for a policy ' +
                            'with online S/R processing.';
             Return errorMessage;
          EndIf;
       EndIf;

       // Do not allow premium or A/R update request if there are 'B'
       // or 'A' transactionS in A/R with a posting date less than
       // the current system month
       //*** beg chg ***** 12/27/18 ************************
       //***If isQuote = *On And scMode <> 'b' And ChkARTrans() = *On;
       If isQuote = *Off And scMode <> 'b' And ChkARTrans() = *On;
       //*** end chg ***** 12/27/18 ************************
          outEdtPgm = 'WTPOLARINQ';
          outEdtTab = 0;
          errorMessage = 'A/R update is not allowed because there ' +
                         'are A or B type transactions with a ' +
                         'posting date less than the current month.';
          Return errorMessage;
       EndIf;

       currentDate = DateTo7(systemDate());
       postDate    = dtCYMD(scPostDat);

       // Check if new ACH Module is Installed ('WC1806')
       If Not ChkNewACH();
          // Set EFT valid flag
          isEFT = *Off;
          Chain (ELco#:ELfnd:ELemp#:ELdiv) pef06;
          If %Found(wmefl6);
             DoU %Eof(wmefl6);
                If (currentDate < EFexDt And currentDate >= EFefDt And
                    EFexDt <> 0) Or (currentDate >= EFefDt And EFexDt = 0);
                        isEFT = *On;
                    EndIf;
                    ReadE (ELco#:ELfnd:ELemp#:ELdiv) pef06;
                 EndDo;
              EndIf;

              // Use EFT valid flag in further edit
              If isEFT and eftErrMsg = *Off and postDate < currentDate and
                  %Parms() > 2;
                 outEdtPgm = 'WTPOLLMST';
                 outEdtTab = 4;
                 errorMessage = 'The posting date for an ACH employer ' +
                                'should not be prior to the current '   +
                                'date less than the current date.';
                 Return errorMessage;
                 eftErrMsg = *On;
              Else;
                 eftErrMsg = *Off;
              EndIf;
           Else;
              SetGt  (ELco#:ELfnd:ELemp#:ELdiv:currentDate) pdb03;
              ReadPE (ELco#:ELfnd:ELemp#:ELdiv) pdb03;
              If (Not %Eof(wmdbl3) And DBexDt = 0 And postDate < currentDate
                  And eftErrMsg = *Off and %Parms() > 2) Or
                 (Not %Eof(wmdbl3) And DBexDt <> 0 And currentDate < DBexDt
                  And postDate < currentDate And eftErrMsg = *Off And
                 %Parms() > 2);
                 outEdtPgm = ' ';
                 outEdtTab = 0;
                 errorMessage = 'The posting date for an ACH employer ' +
                                'should not be prior to the current '   +
                                'date less than the current date.';
                 Return errorMessage;
                 eftErrMsg = *On;
              Else;
                 eftErrMsg = *Off;
              EndIf;
           EndIf;

       // No errors
       Return ' ';

       //===============================================================
       // Carrier # check - Carrier # check for running bills.
       //===============================================================
       BegSr Carrier#;

          // Only allow bill to be run if a carrier policy # is entered
          // for all divisions
          Chain (ELco#:ELfnd) pgc00;
          If %Found(wmgcl) And GCcrFl = 'Y';
             If @@Qte = 'Q';
                Chain (ELco#:ELfnd:ELfYr:ELemp#) pel01;
                If %Found(wdell1);
                   DoU %Eof(wdell1);
                      Chain (ELco#:ELfnd:ELfYr:ELemp#:ELdiv) prp04;
                      If (%Found(wmrpl4) And RPcap# = ' ') Or
                          Not %Found(wmrpl4);
                         errorMessage = 'A bill cannot be submitted unless ' +
                                        'the carrier policy # has been ' +
                                        'entered for all divisions.';
                         Leave;
                      EndIf;
                      ReadE (ELco#:ELfnd:ELfYr:ELemp#) pel01;
                   EndDo;
                EndIf;
             EndIf;
          EndIf;

       EndSr;
       //============================================================
       // ChkEFT - Check EFT setup.
       //============================================================
       Begsr ChkEFT;

         //*** Begin Delete *** 04/26/21 ***********************************************
         // This code will need to be executed from Quote as well.
         // If @@Qte <> 'Q';
         //*** End   Delete *** 04/26/21 ***********************************************
             outCo#    = %editc(ELco#:'X');
             outFnd#   = %editc(ELfnd:'X');
             outEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X') + '  ';
             outDate   = %Editc(DateTo7(SystemDate):'X');
             //*** Begin Change *** 04/26/21 *******************************************
             //If not GetEftStatus(outCo#:OutEftType:outfnd#:outEftKey:
             //                    outDate:outPreNote);
             outFyr = %Editc(inYear:'X');
             outEnv = @@Qte;
             If not GetEftStatus(outCo#:OutEftType:outFnd#:outEftKey:outDate:
                               outPreNote:outRrn:outEobT:outCkTp:outFyr:outEnv);
             //*** End   Change *** 04/26/21 *******************************************
                If ELbscd <> *zero;
                   key = %char(ELbscd);
                   Chain (ELco#:key) pbs01;
                   If %found and BSeftf = 'Y';
                      errorMessage = 'Employer EFT setup is required to ' +
                                  'run Estimated bill for this bill plan.';
                      LeaveSr;
                   Endif;
                Endif;
             Endif;
         //*** Begin Delete *** 04/26/21 ***********************************************
         // Endif;
         //*** End   Delete *** 04/26/21 ***********************************************

       Endsr;

      /End-Free

     p DivErrorChk     e

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
       dtlRrn    = *Zeros;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';
          dtlRrn = %Dec(outRrn#:9:0);
          Chain dtlRrn pel0p;
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

      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     P ExfmtScreens    b
     D ExfmtScreens    pi
      /Free

         If isArOnly = *On;
            If isTool = *On;
               lftNRB     = 555;
               topBalance = 230;
               topComment = 295;
               topArCodes = 265;
               topButtons = 310;
            Else;
               If isSingle = *On;
                  lftNRB     = 250;
                  topBalance = 230;
                  topComment = 295;
                  topArCodes = 180;
                  topButtons = 230;
               Else;
                  lftNRB     = 250;
                  topBalance = 230;
                  topComment = 295;
                  topArCodes = 265;
                  topButtons = 330;
               EndIf;
            EndIf;
         Else;
            If isTool = *On;
               lftNRB     = 555;
               topBalance = 230;
               topComment = 295;
               topArCodes = 515;
               topButtons = 565;
            Else;
               If isSingle = *On;
                  lftNRB     = 555;
                  topBalance = 180;
                  topComment = 245;
                  topArCodes = 465;
        //***** Beg Chg ***** 02/20/20 ***********************
        //******* topButtons = 515;
                  topButtons = 565;
        //***** End Chg ***** 02/20/20 ***********************
               Else;
                  lftNRB     = 555;
                  topBalance = 195;
                  topComment = 260;
                  topArCodes = 480;
        //***** Beg Chg ***** 02/20/20 ***********************
        //******* topButtons = 530;
                  topButtons = 565;
        //***** End Chg ***** 02/20/20 ***********************
               EndIf;
            EndIf;
         EndIf;

         Select;
         When TabIndex = 0;
            TabHeight  = 770;
            APnlHeight = TabHeight + 60;

            If isPopUp= *On;
               Exfmt RcdEstBil ;
            Else;
               Write rcdComPnl;
               Write RcdTbEstBl;
               Exfmt RcdEstBil;
               Read  rcdComPnl;
               Read  RcdTbEstBl;
            EndIf;
         EndSl;
      /End-Free
     P ExfmtScreens    e
      //===================================================================
      // GetAchFlg - Get the ACH flag
      //===================================================================
     P GetAchFlg       b
     D GetAchFlg       pi              n

     d achFlag         s               n   Inz(*Off)
     d key             s              2    Inz('WC')

      /Free

       Chain (key) swc01;
       If %Found(ssylf) And WCfl24 = 'Y';
          achFlag = *On;
       EndIf;

       Return achFlag;

      /End-Free
     P GetAchFlg       e
      //===================================================================
      // GetDivCount - Get the number of Divisions
      //===================================================================
     P GetDivCount     b
     D*** begin chg ***  06/10/20  *************************************
     D***** GetDivCount     pi             3  0
     D GetDivCount     pi             5  0
     D*** end chg *****  06/10/20  *************************************
     D inCompany#                     3  0
     D inGroup#                       3  0
     D inEmployer#                    9  0
     D inYear                         3  0

     D*** begin chg ***  06/10/20  *************************************
     d***** dvCount         s              3  0 Inz(0)
     d dvCount         s              5  0 Inz(0)
     D*** end chg *****  06/10/20  *************************************

      /Free

       Chain (inCompany#:inGroup#:inYear:inEmployer#) pel01;
       If %Found(wdell1);
          DoU %Eof(wdell1);
             dvCount = dvCount + 1;
             ReadE (inCompany#:inGroup#:inYear:inEmployer#) pel01;
          EndDo;
       EndIf;

       Return dvCount;

      /End-Free
     P GetDivCount     e
      //===================================================================
      // GetSlfEstFlg - Get the Self-Reporting/Estimated Flag
      //===================================================================
     P GetSlfEstFlg    b
     D GetSlfEstFlg    pi             1

     D selfEstFlag     s              1    Inz(' ')
     D key             s                   Like(BSkey)
      /Free

       If scEmp# <> 0;
          Chain (company#:scFnd:scEmp#:scDiv:scPolYear) pel00;
          If %Found(wdell);
             key = %EditC(ELbsCd:'X');
             Chain (company#:key) pbs01;
             If %Found(wtabl);
                selfEstFlag = BSsefl;
             EndIf;
          EndIf;
       EndIf;

       Return selfEstFlag;

      /End-Free
     P GetSlfEstFlg    e
      //===================================================================
      // Initialize - Initialize screen fields
      //===================================================================
     P Initialize      b
     D Initialize      pi
      /Free

       scAdjArCd  = ' ';
       scAgtCopy  = ' ';
       scBillSel  = 'A';
       scCmCodes  = ' ';
       scDiv      = 0;
       scEmp#     = 0;
       scEstArCd  = ' ';
       scFCommnt1 = ' ';
       scFCommnt2 = ' ';
       scFCommnt3 = ' ';
       scFnd      = 0;
       scFnlQte   = 'N';
       scFromDiv  = 0;
       scGenHrdC  = ' ';
       scIntDays  = 0;
       scMode     = @@Call;
       //*** begin chg ***  10/28/19  ********************************
       //*****If scMode = ' ';
       If scMode = ' ' and intRRN <> 0;
       //*** end chg *****  10/28/19  ********************************
          scMode = 'b';
       EndIf;
       scNRB      = ' ';
       scPolYear  = 0;
       scPostDat  = 0;
       scPrtBal   = 'Y';
       scRemarkCd = ' ';
       scRsvDrts  = ' ';
       scSrArCd   = ' ';
       scToDiv    = 0;
       isEmp#     =  *Off;
       isARcd     =  *Off;
       isRsvDarts =  *Off;
       // isAll      =  *Off;
       isSingle   =  *Off;
       isRange    =  *Off;
       isToolSing =  *Off;
       shwName    =  *On;
       scArPos3   =  '265px';
       scArPos4   =  '515px';

      /End-Free
     p Initialize      e

      //===================================================================
      // List Division Edits
      //===================================================================
     P ListDivEdits    b
     D ListDivEdits    pi

     d* Program to be called
     d CallProgram     pr                  ExtPgm('STDRVPGM')
     d   inRRN                        9
     d   inMode                       1
     d   outCallPgm                  10
     d   outTab                       5    Options(*NoPass)
     d   inQuote                      1    Options(*NoPass)

     d inRRN           s              9    inz
     d inMode          s              1    inz
     d outCallPgm      s             10    inz
     d outTab          s              5    inz('00000')
     d inQuote         s              1    inz

      /Free

       If scBillSel = 'R' or scBillSel = 'U';
          DoU btnSubmit = *On or btnCancel = *On;
             btnSubmit  = *Off;
             btnCancel  = *Off;
             btnRefresh = *Off;
             rrnBillEdt = 0;
             clrBillEdt = *On;
             dspBillEdt = *Off;
             write RcdEstEdt;
             dspbillEdt = *On;
             clrBillEdt = *Off;

             Setll *LoVal pdv0p;
             Read pdv0p;
             DoW Not %Eof(wxdvp);
                scBillDiv   = DVdiv;
                scDbaName   = DVdba;
                ttDbaName   = DVdba;
                scFedName   = DVfdNm;
                ttFedName   = DVfdNm;
                //*** Beg Chg ***** 05/27/24 ******************************
                //***scEditMsg   = %Subst(DVerrMsg:1:40);
                If DVerrMsg = ' ';
                   scEditMsg = 'Division will be billed';
                Else;
                   scEditMsg = DVerrMsg;
                Endif;
                //*** Beg Chg ***** 05/27/24 ******************************
                ttEditMsg   = DVerrMsg;
                scEditPgm   = DVedtPgm;
                scEditTab   = DVedtTab;
                scEditRrn   = DVedtRrn;

                scDivUrl  = ' ';
                divErrors = *Off;
                If scEditPgm <> ' ';
                   divErrors  = *On;
                   inRRN      = %EditC(scEditRrn:'X');
                   inMode     = 'C';
                   outCallPgm = scEditPgm;
                   outTab     = %EditC(scEditTab:'X');
                   inQuote    = @@Qte;
                   scDivUrl   = CreateURL('STDRVPGM':scDbaName:inRrn:inMode:
                                           scEditPgm:outTab:inQuote);
                EndIf;

                rrnBillEdt  = rrnBillEdt + 1;
                write sflBillEdt;
                Read pdv0p;
             EndDo;

             ExFmt RcdEstEdt;
             If btnRefresh = *On;
                PolDivEdits();
             Endif;
          EndDo;
       Else;
          BtnSubmit  = *On;
       EndIf;

      /End-Free
     p ListDivEdits    e

      //===================================================================
      // Load Screen Fields
      //===================================================================
     P LoadScreen      b
     D LoadScreen      pi
      /Free

        //***** Beg Add ***** 12/09/19 ***********************
          If isQuote = *On;
             scQteType = 'E' ;
             scQteTypes = 'L,D,l,d' ;
          Endif;
        //***** End Add ***** 12/09/19 ***********************
       // Load Comment codes
        CmntcodLod();

      /End-Free
     p LoadScreen      e

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi
      /Free

       // Process until Exit button is pressed.
        Dou ExitNow = *On ;

           // Let's show them screens
           ExfmtScreens();

           Select;
           // Employer Search
           When selEmpSrch = *On;
              selEmpSrch = *Off;
              Emp_DivSearch();

           // Group Change
           When BtnGrpChg = *On;
              BtnGrpChg = *Off;
             // If user changes group calculate Posting Date again
           //*** begin chg ***  10/28/19  ********************************
           //*****   CalPostDat();
              SetCriteria();
           //*** end chg *****  10/28/19  ********************************

           // Try to submit the bill
           When BtnSubmit = *On;
              BtnSubmit = *Off;
              // Reset Error Fields
              ClearErrors();
             // Validate screen Fields, if No Error then submit driver CL program
              If NoError();
                 // If Range Or Unbilled, list divisions with errors/no errs
                 ListDivEdits();
                 If BtnSubmit = *On;
                    // Submit the job
                    SubmitDrvCL();
                    Leave;
                 Else;
                    // Reapply te criteria
                 //*** begin del ***  10/28/19  ********************************
                 //*****   SetCriteria();
                 //*** end del *****  10/28/19  ********************************
                 EndIf;
              Else;
                 // Reapply the criteria
                 //*** begin del ***  10/28/19  ********************************
                 //*****SetCriteria();
                 //*** end del *****  10/28/19  ********************************
              EndIf;

           // Cancel
           When BtnCancel = *On;

              // Close and Remove Work file
              If %Open(wxdvp);
                 Close wxdvp;
                 CallWtPolEstFC(##Library:'D');
              EndIf;
              BtnCancel = *Off;
              leave;

           // Refresh
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
      // NoError - Validate screen fields
      //===================================================================
     p NoError         b
     d NoError         pi              n

     d cancelDate      s              7  0 Inz(0)
     d currentDate     s              7  0
     d found           s               n
     d i1              s              2  0 Inz(1)
     d intPostYear     s              4  0
     d intSysYear      s              4  0
     d intUsaDate      s              8  0
     d isEFT           s               n   Inz(*Off)
     d isError         s               n   Inz(*off)
     d postDate        s              7  0
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate         s              7
     d outPreNote      s              1
     d prvBill         s               n   Inz(*Off)
     d outCo#          s              3
     d outEdtPgm       s             10
     d outEdtTab       s              5s 0
     d outEdtRrn       s              9s 0
      *** beg add ***** 03/16/20 *************************
     d intDivCxDate    s              7  0
     d outMode         s              3    Inz('BIL')
      *** end add ***** 03/16/20 *************************
      //*** Beg Add ***** 09/27/24 ******************************************
     d flgUnbillDiv    s               n
      //*** End Add ***** 09/27/24 ******************************************

      /Free
       // Basic Screen Edits
       ExSr ScreenEdits;

       //*** Beg Add ***** 09/27/24 ******************************************
       //Check if there is any Unbilled division on this policy year
       If scBillSel = 'U';
          flgUnbillDiv = *Off;
          Setll    (company#:scFnd:scPolYear:scEmp#) pel01;
          Reade(n) (company#:scFnd:scPolYear:scEmp#) pel01;
          Dow Not %Eof(wdell1);
             If ELfaf = ' ';
                flgUnbillDiv = *On;
                Leave;
             Endif;
             Reade(n) (company#:scFnd:scPolYear:scEmp#) pel01;
          Enddo;
          If flgUnbillDiv = *Off;
             isError = *On;
             erEmp#1 = *On;
             msEmp#1 = 'There are no unbilled divisions for this employer.';
          Endif;
       Endif;
       //*** End Add ***** 09/27/24 ******************************************

       If isError = *Off and scEmp# <> 0;
          flgPyrlFnd = 'N';
          Select;
          When scBillSel = 'S';
             Chain (company#:scFnd:scPolYear:scEmp#:scDiv) pep07;
             If %Found(wmepl7);
                flgPyrlFnd = 'Y';
             Endif;
          Other;
             Chain (company#:scFnd:scEmp#) pem00;
             If %Found(wmeml);
                DoU %Eof(wmeml) or flgPyrlFnd = 'Y';
                   If processDiv(EMco#:EMfnd:EMemp#:EMdiv:scPolYear:
                                         scFromDiv:scToDiv:scBillSel);
                      Chain (EMco#:EMfnd:scPolYear:EMemp#:EMdiv) pep07;
                      If %Found(wmepl7);
                         flgPyrlFnd = 'Y';
                         Leave;
                      Endif;
                   Endif;
                   ReadE (company#:scFnd:scEmp#) pem00;
                EndDo;
             Endif;
          ENDSL;
       Endif;
       If flgPyrlFnd = 'N';
          isError = *On;
          erEmp#1 = *On;
          msEmp#1 = 'No active payroll records exist for this employer.';
       Endif;
       //*** End Add ***** 03/12/24 ******************************************

       // Check Policy Edits if Single division or All Division
       If scBIllSel = 'S' Or scBillSel = 'A' Or scBillSel = ' ';
          // Policy edits applied to the screen fields
          ExSr PolScrnEdits;
       Else;
          // Don't process if basic screen edits encountered.
          If Not isError;
             // Policy Division edits applied to Division Work file (WXDVP)
             isError = PolDivEdits();
          EndIf;
       EndIf;

        If isError = *On;
           Return *Off;
        Else;
           If Not ShowAllDivWdw();  // If OK pressed
              //*** Beg Chg ***** 05/05/22 ***********************************
              //*** Return *On;
              xxSubmit = *on;
              If isUseScript;
                 xxProcType = processType;
                 Chain company# pbl001;
                 //*** Begin Chg *** 05/03/24 ******************************
                 //**If (scBillSel = 'S' or scDivCnt = 1) and BLPrmne = ' ';
                 //*** begin chg ***  03/06/25  ****************************
                 //*****If scBillSel = 'S' or scDivCnt = 1 Or scEmp# <> 0;
                 If intRRN <> 0 and (scBillSel = 'S' or scDivCnt = 1 or
                  BLCombE = 'Y');
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
                 If scRemarkCd <> ' ';
                    xxDart = 'Y';
                    xxRMCD = scRemarkCd;
                 Endif;
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
                    Return *Off;
                 Else;
                    In *Lock *dtaara;
                    Out *dtaara;
                    scRsvDrts = xxDart;
                    scRemarkCd = xxRMCD;
                    Return *On;
                 Endif;
              Else;
                 Return *On;
              Endif;
              //*** End Chg ***** 05/05/22 ***********************************
           Else;
              Return *Off;
           EndIf;
        EndIf;

        //===============================================================
        // PolScrnEdits - Policy Edits applied to screen fields
        //===============================================================
        Begsr PolScrnEdits;

          //*** beg add ***** 03/16/20 *************************
          // Allow bill to process on cx'd policy if the cx
          //  is over-ridden
           If scEmp# <> 0;
              intDivCxDate = GetCxDate(company#:scFnd:scEmp#:scDiv);
      *** Beg Chg ***  10/15/24  ***  WC2984  ************************
      ***     If intDivCxDate <> 0 And
      ***      OvrRenewalCx(outMode:company#:scFnd:scEmp#:scDiv:scPolYear:
      ***      intDivCxDate) = *Off;
      ***        erEmp#  = *On;
      ***        msEmp#  ='WC31363';
      ***        isError = *On;
      ***     Endif;

              If intDivCxDate <> 0 And
               OvrRenewalCx(outMode:company#:scFnd:scEmp#:scDiv:scPolYear:
               intDivCxDate) = *Off;
                 If (scBillSel <> 'S' and scBillSel <> 'U') Or
                    (scBillSel =  'S' and ELFAF     <> ' ') ;
                    erEmp#  = *On;
                    msEmp#  ='WC31363';
                    isError = *On;
                 Endif;
              Endif;
      *** End Chg ***  10/15/24  ***  WC2984  ************************
           EndIf;
          //*** end add ***** 03/16/20 *************************

           // Do not allow single division original bill if a billing has
           // already been run.
           If scEmp# <> 0 and scMode = '4' And singleDiv;
              Chain (company#:scFnd:scEmp#:scDiv:scPolYear) peh00;
              If %Found(wdehl);
                 erEmp#  = *On;
                 msEmp#  ='WC30365';
                 isError = *On;
              EndIf;
           EndIf;

           // Do not allow a non-print only bill outside of quote if no
           // valid ACH information for a policy with online S/R processing.
           //*** beg chg ***** 03/22/19 ***********************
           //***If isQuote = *On And scMode <> '1' And scEmp# <> 0;
           If isQuote = *Off And scMode <> 'b' And scEmp# <> 0;
           //*** end chg ***** 03/22/19 ***********************
              ##CoC#   = %Editc(intCo#:'X');
              ##FndC   = %Editc(scFnd:'X');
              ##EmpC#  = %Editc(scEmp#:'X');
              ##DivC   = %Editc(scDiv:'X');
              ##FyrC   = %Editc(scPolYear:'X');
              ##EfDtC  = %EditC(dtCheck(ELpfDt):'X');
              ##Mode   = '1';
              ##Err    = ' ';
              ##DataSR = ' ';
              SROnline(##CoC#:##FndC:##EmpC#:##DivC:##FyrC:##EfDtC:##Mode:
                       ##Err:##DataSR);

              If ##err = '1';
                 erPolYear = *On;
           //*** beg chg ***** 03/22/19 ***********************
           //****msPolYear ='WC30365';
                 msPolYear ='WC30391';
           //*** end chg ***** 03/22/19 ***********************
                 isError   = *On;
              EndIf;
           EndIf;

           // Do not allow premium or A/R update request if there are 'B'
           // or 'A' transactionS in A/R with a posting date less than
           // the current system month
           //*** beg chg ***** 12/27/18 ************************
           //***If isQuote = *On And scMode <> 'b' And ChkARTrans() = *On;
           If isQuote = *Off And scMode <> 'b' And ChkARTrans() = *On;
           //*** end chg ***** 12/27/18 ************************
              erPostDat = *On;
              msPostDat = 'WCR0856';
              isError   = *On;
           EndIf;

           currentDate = DateTo7(systemDate());
           postDate    = dtCYMD(scPostDat);

           // Check if new ACH Module is Installed ('WC1806')
           If Not ChkNewACH();
              // Set EFT valid flag
              isEFT = *Off;
              Chain (company#:scFnd:scEmp#:scDiv) pef06;
              If %Found(wmefl6);
                 DoU %Eof(wmefl6);
                    If (currentDate < EFexDt And currentDate >= EFefDt And
                        EFexDt <> 0) Or (currentDate >= EFefDt And
                        EFexDt = 0);
                        isEFT = *On;
                    EndIf;
                    ReadE (company#:scFnd:scEmp#:scDiv) pef06;
                 EndDo;
              EndIf;

              // Use EFT valid flag in further edit
              If isEFT and eftErrMsg = *Off and postDate < currentDate and
                  %Parms() > 2;
                 erPostDat = *On;
                 msPostDat = 'WC30322';
                 isError   = *On;
                 eftErrMsg = *On;
              Else;
                 eftErrMsg = *Off;
              EndIf;
           Else;
              SetGt  (company#:scFnd:scEmp#:scDiv:currentDate) pdb03;
              ReadPE (company#:scFnd:scEmp#:scDiv) pdb03;
              If (Not %Eof(wmdbl3) And DBexDt = 0 And postDate < currentDate
                  And eftErrMsg = *Off and %Parms() > 2) Or
                 (Not %Eof(wmdbl3) And DBexDt <> 0 And currentDate < DBexDt
                  And postDate < currentDate And eftErrMsg = *Off And
                 %Parms() > 2);
                 erPostDat = *On;
                 msPostDat = 'WC30322';
                 isError   = *On;
                 eftErrMsg = *On;
              Else;
                 eftErrMsg = *Off;
              EndIf;
           EndIf;

        EndSr;
        //===============================================================
        // ScreenEdits - Basic Screen Edits
        //===============================================================
        Begsr ScreenEdits;

           // Fund year must be greater than 0
           If scPolYear <= 0 And scNRB = ' ';
              erPolYear = *On;
              msPolYear ='WCR0502';
              isError   = *On;
           EndIf;

           // Employer# must be greater than zero
           If scEmp# <= 0 And scNRB = ' ';
              erEmp#  = *On;
              msEmp#  ='WCR0502';
              isError = *On;
           EndIf;

           // Validate Employer/Division if Entered
           If scEmp# <> 0;
              Chain (company#:scFnd:scEmp#:scDiv) pem00;
              If Not %Found(wmeml);
                 erEmp#  = *On;
                 msEmp#  ='WCR0538';
                 isError = *On;
              Else;
                 Chain (company#:scFnd:scEmp#:scDiv:scPolYear) pel00;
                 If Not %Found(wdell);
                    erPolYear = *On;
                    msPolYear ='WCR0694';
                    isError   = *On;
                 EndIf;
              EndIf;
           EndIf;

           // Division Range must be entered if bill selection of range used
           If scBillSel = 'R';

              // From division must be valid
              If ValidEmp(scEmp#:scFromDiv:empRrn:scFnd:company#) = *Off;
                 erFromDiv  = *On;
                 mgFromDiv  = 'Invalid division entered.';
                 isError    = *On;
              EndIf;

              // To division must be valid
              If ValidEmp(scEmp#:scToDiv:empRrn:scFnd:company#) = *Off;
                 erToDiv  = *On;
                 mgToDiv  = 'Invalid division entered.';
                 isError  = *On;
              EndIf;

              // From and To divisions must be in ascending order
              If scToDiv < scFromDiv;
                 erFromDiv  = *On;
                 mgFromDiv  = 'Divisions must be in ascending order.';
                 erToDiv    = *On;
                 mgToDiv    = 'Divisions must be in ascending order.';
                 isError    = *On;
              EndIf;
           EndIf;

           // FYR/EMP#/DIV and Renewal/New/Both Flag cannot both be set.
           If scNRB <> ' ' And (scPolYear <> 0 Or scEmp# <> 0 Or scDiv <> 0);
              erEmp#  = *On;
              msEmp#  ='WCR0918';
              isError = *On;
           EndIf;

           // User must have proper security for running a mass billing.
           If scNRB <> ' ';
              Chain (company#:scFnd) pex00;
              If Not %Found(wmexl);
                 Chain (company#) pex00;
              EndIf;
              If %Found(wmexl);
                 ##Ctype   = EXsubT;
              //*** begin chg ***  07/11/19  *******************************
              //*****   ##Ctype   = %EditC(EXsubL:'X');
                 ##Clvl    = %EditC(EXsubL:'X');
              //*** begin chg ***  07/11/19  *******************************
                 ##Utype   = UPtype;
                 ##Ulvl    = %EditC(UPlvl:'X');
                 ##Exec    = '1';
                 ##Results = '1';
                 CallSRCCSC(##Utype:##Ulvl:##Ctype:##Clvl:##Exec:##Results);
                 If ##Results = *On;
                    erNRB   = *On;
                    msNRB   = 'WCR1122';
                    isError = *On;
                 Else;
                    // Warn that about to run mass run
                    If massRun = *Off;
                       erNRB   = *On;
                       msNRB   ='WC30045';
                       isError = *On;
                       massRun = *On;
                    EndIf;
                 EndIf;
              Else;
                 // Warn that about to run mass run
                 If massRun = *Off;
                    erNRB   = *On;
                    msNRB   ='WC30045';
                    isError = *On;
                    massRun = *On;
                 EndIf;
              EndIf;
           EndIf;

           // Validate the Posting Date
           If dtCheck(scPostDat) = 0;
              erPostDat = *On;
              msPostDat = 'WCR0500';
              isError   = *On;
           Else;
              scPostDat = dtCheck(scPostDat);
           EndIf;

           // Validate the Interim Days
           If GetSlfEstFlg() = 'S' And scIntDays < 0;
              erIntDays = *On;
              msIntDays = 'WCR0547';
              isError   = *On;
           EndIf;

          // Extract and validate Comment Codes
           Clear commentCdDs;
           strCmCodes = scCMCodes;
           If %scan(',':%trimR(strCMCodes)) = *Zeros;
              commentCdDs = strCMCodes;
           EndIf;
           Dow %scan(',':%trimR(strCMCodes)) > *Zeros;
              strCMCodes = %Replace ('':strCMCodes:%scan(',':
                                      %trimR(strCMCodes)):1);
              commentCdDs = strCMCodes;
           Enddo;

           // You can only use comment codes or free-from comments
           If scCmCodes <> ' ' And
              (scFcommnt1 <> ' ' Or scFcommnt2 <> ' ' Or scFcommnt1 <> ' ');
              erCmCodes = *On;
              msCmCodes = 'WCR0590';
              erFcommnt = *On;
              msFcommnt = 'WCR0590';
              isError   = *On;
           EndIf;

           // Only three codes are allowed
           If %Len(%Trim(scCmCodes)) > 8;
              erCmCodes = *On;
              msCmCodes = 'WC40169';
              isError   = *On;
           EndIf;

           // Validate the resolve to Darts Flag.
           If scRsvDrts = 'Y';
              Chain (scRemarkCd) pct01;
              If %Found(stctl1);
                 If CTstat = 'I';
                    erRemarkCd = *On;
                    msRemarkCd = 'SCM0614';
                    erMsgFile  = 'SCMESG';
                    isError    = *On;
                 EndIf;
              Else;
                 erRemarkCd = *On;
                 msRemarkCd = 'WCR0827';
                 erMsgFile  = 'WMESGG';
                 isError    = *On;
              EndIf;
           EndIf;

           // If Hardcopy is 'Y' then 2nd Order flag must be 'Y'
             //*** Begin Change *** 05/03/19 *************************************
              //If scGenHrdC = 'N' And scAgtCopy = 'Y';
              //erAgtCopy = *On;
              //msAgtCopy = 'WC20025';
              //isError   = *On;
           If (scGenHrdC = 'N' or scGenHrdC = *blanks) And scAgtCopy = 'Y';
              *in63 = *On;
           Else;
              *in63 = *Off;
             //*** End   Change *** 05/03/19 *************************************
           EndIf;

           // Run ECM Specific Process
           If isECM = *On;
              Select;
              When scBatch# < 0;
                 erBatch# = *On;
                 msBatch# = 'WCR0547';
                 isError  = *On;
              When scBatch# > 0;
                 Chain (company#:scBatch#) pea04;
                 If Not %Found(wdeal4);
                    erBatch# = *On;
                    msBatch# = 'WCR0867';
                    isError  = *On;
                 EndIf;
              EndSl;

              If scBatch# <> 0 And Not isError;
                 ExSr srBatch;
              EndIf;
           EndIf;
           //*** Beg Del ***** 01/30/24 ***********************************
           //*** Beg Add ***** 05/05/22 ***********************************
           //***  If isError  = *off;
           //***     xxSubmit = *on;
           //***     If isUseScript;
           //***        xxProcType = processType;
           //***        Chain company# pbl001;
           //***        If (scBillSel = 'S' or scDivCnt = 1) and BLPrmne = ' ';
           //***           xxMass = ' ';
           //***        Else;
           //***          xxMass = 'Y';
           //***        Endif;
           //***
           //***        In *Lock *dtaara;
           //***        Clear svLDA;
           //***        xxDart = 'N';
           //***        xxRMCD = ' ';
           //***        Chain(n) (company#:scFnd) pex00;
           //***        If %found and EXPNRM <> ' ';
           //***           xxDart = 'Y';
           //***           xxRMCD = EXPNRM;
           //***        Endif;
           //***        Out *dtaara;
           //***        xxSubmit = OutputWindow(xxProcType:xxMass);
           //***        If xxSubmit = *off;
           //***          isError = *On;
           //***        Else;
           //***           In *Lock *dtaara;
           //***           Out *dtaara;
           //***           scRsvDrts = xxDart;
           //***           scRemarkCd = xxRMCD;
           //***        Endif;
           //***     Endif;
           //***  Endif;
           //*** End Add ***** 05/05/22 ***********************************
           //*** End Del ***** 01/30/24 ***********************************

        EndSr;
        //===============================================================
        // SrBatch - Check Batch
        //===============================================================
        Begsr SrBatch;

           If scEmp# = 0;
              If scNRB = 'B';
                 Chain (company#:scFnd) pbl00;
                 If %Found(wabll);
                    found = *On;
                 EndIf;
              Else;
                 Chain (company#:scFnd:scNRB) pbl03;
                 If %Found(wabll3);
                    found = *On;
                 EndIf;
              EndIf;

              DoW found;
                 // Update Retro Credit  A/R batch#
                 If BLflg1 <> ' ' Or BLflg3 <> ' ';
                    BLarq# = scBatch#;
                    If scNRB = 'B';
                       Update pbl00;
                    Else;
                       Update pbl03;
                    Endif;
                 EndIf;
                 // Read Next AR Transaction Record
                 If scNRB = 'B';
                    ReadE (company#:scFnd) pbl00;
                    If Not %Eof(wabll);
                       found = *On;
                    EndIf;
                 Else;
                    ReadE (company#:scFnd:scNRB) pbl03;
                    If Not %Eof(wabll3);
                       found = *On;
                    EndIf;
                 EndIf;
              EndDo;
           Else;
              If singleDiv;
                 Chain (company#:scFnd:scPolYear:scEmp#:scDiv) pel01;
                 If %Found(wdell1);
                    ELaMin = scBatch#;
                    Update pel01;
                 EndIf;
              Else;
                 Chain (company#:scFnd:scPolYear:scEmp#) pel01;
                 If %Found(wdell1);
                    DoU %Eof(wdell1);
                       ELaMin = scBatch#;
                       Update pel01;
                       ReadE (company#:scFnd:scPolYear:scEmp#) pel01;
                    EndDo;
                 EndIf;
              EndIf;
           EndIf;

        EndSr;
      /End-Free
     P NoError         e

       //===================================================================
       // PolDivEdits - Policy Division Edits applied to WXDVP
       //===================================================================
     p PolDivEdits     b
     d PolDivEdits     pi              n

     d  outEdtPgm      s             10
     d  outEdtTab      s              5s 0
     d  outEdtRrn      s              9s 0
      *** Beg Add ***  10/15/24  ***  WC2984  ************************
     d  xxReq#         s                   Like(CXREQ#)
      *** End Add ***  10/15/24  ***  WC2984  ************************
      /Free

       // Clear WXDVP
       ClrDivFile();

       divExists = *Off;  // Are any divisions Selected
       allDivErr = *On;   // Do all divisions have errors

       Chain (company#:scFnd:scEmp#) pem00;
       If %Found(wmeml);
          DoU %Eof(wmeml);
             If processDiv(EMco#:EMfnd:EMemp#:EMdiv:scPolYear:
                                   scFromDiv:scToDiv:scBillSel);
                Clear pdv0p;
                DVco#    = EMco#;
                DVfnd    = EMfnd;
                DVemp#   = EMemp#;
                DVdiv    = EMdiv;
                DVdba    = EMname;
                DVfdNm   = EMfdNm;
                DVuser   = Q1user;
                DVpgm    = Q1pgm;
                DVdt     = DateTo7(systemDate());
                DVtime   = TimeTo6(systemTime());
                DVerrMsg = DivErrorChk(EMco#:EMfnd:EMemp#:EMdiv:
                             scPolYear:outEdtPgm:outEdtTab:outEdtRrn);
                DVedtPgm = outEdtPgm;
                DVedtTab = outEdtTab;
                DVedtRrn = outEdtRrn;
                divExists= *On;
                If DVerrMsg = ' ';
                   allDivErr = *Off;
                EndIf;


                Write pdv0p;
             EndIf;
             ReadE (company#:scFnd:scEmp#) pem00;
          EndDo;
       EndIf;

       If divExists = *Off And scBillSel = 'U';
          msDiv   = 'WC40170';
          erDiv   = *On;
          Return *On;
       EndIf;

       Return *Off;

      /End-Free

     p PolDivEdits     e

       //===================================================================
       // ProcessDiv - Determine if Division is being processed
       //===================================================================
     p ProcessDiv      b
     d ProcessDiv      pi              n
     D inCompany#                     3  0
     D inGroup#                       3  0
     D inEmployer#                    9  0
     D inDivision                     5  0
     D inYear                         3  0
     D inFromDiv                      5  0
     D inToDiv                        5  0
     D inBillSel                      1

      /Free

       Select;
       When inBillSel = 'R';
          If EMdiv >= inFromDiv And EMdiv <= intoDiv;
             Return *On;
          EndIf;
       When inBillSel = 'U';
          Chain (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pel00;
          If %Found(wdell) And ELfaf = ' ';
             Return *On;
          EndIf;
       //*** Begin Add *** 10/08/19 **************************************
       Other;
          Return *On;
       //*** End   Add *** 10/08/19 **************************************
       EndSl;

       Return *Off;

      /End-Free

     p ProcessDiv      e
      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     P ProgramStart    b
     D ProgramStart    pi
      /Free

        //*** Beg Add ***** 05/05/22 ***********************************
        Select;
        //*** Beg Chg ***** 01/30/24 ***********************************
        //*** When isQuote = *On;
        When @@Qte = 'Q';
        //*** End Chg ***** 01/30/24 ***********************************
           processType = 'Quote Bill';
        Other;
           processType = 'Estimated Bill';
        Endsl;
        isUseScript = UseScriptura(processType);
        //*** End Add ***** 05/05/22 ***********************************
       // Initialize Fields
        Initialize();

       // Load Action panel with Company Name
        scCompName = GetCompNam(userCo#(q1user));
        intCo#     = userCo#(q1user);

        ##Call   = @@Call;

       // Retrieve Client Id
        GetClientID(outCltID);

       // Retrieve user parameter
        Chain Q1User pup00;
        If %Found(wmupl);
           company#  =  UPco#;
           scFnd     =  UPfnd;
           scPolYear =  0;
           strAcct   =  *Blanks;
        EndIf;


        isSelf = *On;
        // Single Employer called from toolbar
        If Q1prms > 2 And @@Rrn <> ' ' And @@Rrn <> '000000000';
           intRrn   = %Dec(@@Rrn:9:0);

           Monitor;
              ##Mode1= %Dec(@@Call:1:0);
           On-Error;
              ##Mode1= *Zeros;
           EndMon;

           isToolSing = *On;
           isTool     = *On;
           isEmp#     = *Off;
           isPopUp    = *On;
           isGrpDisb  = *On;            // Disable Group, Empoyer and Division fields if
                                        // called from Toolbar
           Chain intRRN pel0p;
           If %Found(wdelp);
              company#  = ELco#;
              scFnd     = ELfnd;
              scPolYear = ELfyr;
              scEmp#    = ELemp#;
              scDiv     = ELdiv;
              //*** Begin Add *** 03/30/21 *********************************
              scFQte     = ELfQte;
              isICSubmit = *Off;
              // Do we submit Quote Information to Invoice Cloud
              If (scFQte   = 'Y' And scICSubmit = 'Y') Or
                 (scFnlQte = 'Y' And scICSubmit = 'Y');
                 scSbmIC    = ' ';
                 isICSubmit = *Off;
              EndIf;
              //*** End   Add *** 03/30/21 *********************************

              // Get Employer Name
              dtlRrn    = 0;
              scEmpName = GetEmpName(scEmp#:scDiv:dtlRrn:scFnd:
                              company#:@@Qte);

              // Populate employer/Div
              scEmpDiv#  = %Editc(scEmp#:'X') + '-' + %Editc(scDiv:'X');

              // Set default Bill selection
              scBillSel = 'A';

              // Determine the number of divisions and display warning wdw
              scDivCnt  = GetDivCount(company#:scFnd:scEmp#:scPolYear);

              // Determine if any divisions are billed.  If all are unbilled
              // then only allow bill select of ALL
              allUnbilled = *Off;
              Chain (company#:scFnd:scPolYear:scEmp#) peh13;
              If Not %Found(wdehl13);
                 allUnbilled = *On;
              EndIf;

              // Is this a multi division policy?
              If scDivCnt > 1 And Not allUnbilled;
                 indMulti = *On;
                 isSingle = *Off;
                 // Populate the Billing selection drop down
                 ExSr PopBsChoices;
              Else;
                 scBsChoice = 'All Divisions';
                 scBsValue = 'A';
                 If allUnbilled;
                    toolBs = 'Only one selection is available since ' +
                             'none of the divisions have been billed.';
                 Else;
                    toolBs = 'Only one selection is available since ' +
                             'this employer only has one division.';
                 EndIf;
                 scBillSel = 'A';
                 indMulti  = *Off;
                 isSingle  = *On;
              EndIf;

              // Access Admin Control
              Chain (company#:scFnd) pex00;

              // Is this a self-reporting policy
              key       = %EditC(ELbsCd:'X');
              Chain (ELco#:key) pbs01;
              If %Found(wtabl) And BSseFl = 'S';
                 isSelf = *On;
              Else;
                 scIntDays = 0;
                 isSelf = *Off;
              EndIf;

              // Let's get the plan
              ##Agent#  = 0;
              ##AgentNm = ' ';
              ##Prod#   = 0;
              ##ProdNm  = ' ';
              ##Area    = 0;
              ##Phone   = 0;
              ##Plan    = '*';
              ##Date    = dtMDYY(ELptDt);
              CallWA002R(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:##Agent#:##AgentNm:
               ##Prod#:##ProdNm:##Area:##Phone:##Plan:##Date);

              isAccount = *Off;
              If ##Plan = 'A';
                 isAccount = *On;
              EndIf;
           EndIf;

        Else;
           isMenu    = *On;
           intRrn    = *Zeros;
           isTool    = *Off;
           isEmp#    = *On;
           isPopUp   = *Off;
           isGrpDisb = *Off;
        EndIf;
        //*** Begin Add *** 03/30/21 *********************************
        // Are we submitting for Invoice cloud?
        If @@Qte = 'Q' And IsModEnabled('Invoice Cloud') And
           ICGroupOpt(company#:scFnd) And ICQuoteOpt(company#:scFnd);
           scICSubmit = 'Y';
        Endif;
        // If payment is already recieved on single quote, can't submit
        If isGrpDisb = *on
         and QuotePaymentRecieved(company#:scFnd:scEmp#:scDiv);
           scICSubmit = ' ';
        Endif;

        // Get the Invoice cloud heading
        scICHdg  = 'Submit Quote to ' +
                   %Trim(GetModuleProperty('Invoice Cloud':'Name'));
        //*** End   Add *** 03/30/21 *********************************

        // Populate Bill Options
        If @@Qte = 'Q';
         //***** Beg Chg *** 08/21/19 ***********************************
         //***  scBoChoice = 'Print Only,Calculate and Print';
         //***  scBoValue  = 'b,2';

           scBoChoice = ',Calculate and Print,Print Only';
           scBoValue  = ',2,b';
         //***** End Chg *** 08/21/19 ***********************************
           scTitle    = 'Quote Bill';
        Else;
         //***** Beg Chg *** 08/21/19 ***********************************
         //***  scBoChoice = 'Print Only,Calculate and Print,Update A/R' +
         //***               ' Only,Calculate Update and Print';
         //***  scBoValue  = 'b,2,3,4';

           scBoChoice = ',Calculate and Print,Calculate Update and Print,' +
                        'Update A/R Only,Print Only';
           scBoValue  = ',2,4,3,b';
         //***** End Chg *** 08/21/19 ***********************************
           scTitle    = 'Estimated Bill';
        EndIf;

        SetCriteria();

        //===============================================================
        // PopBsChoises - Populate the Bill Selection Choices
        //===============================================================
        Begsr PopBsChoices;

           // All is the default choice
           scBsChoice = 'All Divisions';
           scBsValue  = 'A';

           // Check Single Division security
           If CheckSecLevel('BillSingle':UPtype:UPlvl);
              scBsChoice = %Trim(scBsChoice) + ',Single Division';
              scBsValue  = %Trim(scBsValue) + ',S';
           EndIf;

           // Check Division Range security
           If CheckSecLevel('BillDivRng':UPtype:UPlvl);
              scBsChoice = %Trim(scBsChoice) + ',Division Range';
              scBsValue  = %Trim(scBsValue) + ',R';
           EndIf;

           // Check Unbilled Division security
           If CheckSecLevel('BillUnbill':UPtype:UPlvl);
              scBsChoice = %Trim(scBsChoice) + ',Unbilled Division';
              scBsValue  = %Trim(scBsValue) + ',U';
           EndIf;

           // Secure and add tool tip if no autority to other options
           If scBsValue = 'A';
              isSingle  = *On;
              toolBs    = ' You are not currently authorized to run '     +
                          'billings using specific billing selections.  ' +
                          'This bill will generate for the entire '       +
                          'policy.  If you wish for only specific '       +
                          'divisions to be generated, please notify '     +
                          'your lead to determine the appropriate '       +
                          'authority level user that can assist you.';
           EndIf;

        EndSr;

      /End-Free
     p ProgramStart    e

      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi
      /Free

       // Calculate Post Date
       CalPostDat();

       // Access Admin Control
       Chain (company#:scFnd) pex00;

        // Is this a single division call
        singleDiv = *Off;
        If @@Call > '4';
           scBsChoice = 'Single Division';
           scBsValue  = 'S';
           toolBs     = 'Only single division selection is available' +
                        ' from the single division menus.';
           isMenu     = *On;
           singleDiv  = *On;
           // isAll      = *Off;
           isToolSing = *On;
           isSingle   = *On;
           isRange    = *Off;
           scBillSel  = 'S';
           call       = %Dec(@@Call:1:0);
           call       = call - 4;
        //***** Beg Chg ***** 08/19/21 *******************************
        //****** @@Call     = %EditC(call:'X');
        //****** scMode     = @@Call;
           scMode     = %EditC(call:'X');
        //***** End Chg ***** 08/19/21 *******************************
        EndIf;

        // Set off the Range indicator
        If scBillSel = 'R';
           isRange   = *On;
           shwName   = *Off;
        Else;
           isRange   = *Off;
           shwName   = *On;
        EndIf;

        // Check to see if Update A/R Only
        isArOnly = *Off;
        If scMode = '3';
           isArOnly  = *On;
           scGenHrdC = 'N';
           scAgtCopy = 'N';
        EndIf;

        // Load Interim Days
        If isSelf = *On;
           scIntDays = EXrvID;
        EndIf;

        // Check to see if showing Interum Days
        shwIntDays = *Off;
        If isSelf = *On And isArOnly = *Off;
           shwIntDays = *On;
        EndIf;

        isMass = *Off;
        If isTool = *Off And Not singleDiv;
           isMass = *On;
        EndIf;

        isECM = *Off;
        If outCltID = '071' ;
           isECM = *On;
        EndIf;

        // Load A/R Code holding fields
        scEstArCd2 = EXesCd;
        scSrArCd2  = EXsrCd;
        scAdjArCd2 = EXdaCd;

        // Check to see A/R is being updated and default field values
        If scMode = '3' Or scMode = '4';
           isARcd     = *On;
           scEstArCd  = EXesCd;
           scSrArCd   = EXsrCd;
           scAdjArCd  = EXdaCd;
        Else;
           isARcd     = *Off;
           scEstArCd  = ' ';
           scSrArCd   = ' ';
           scAdjArCd  = ' ';
        EndIf;

        // The one exception is if Quote and Setup says to update A/R.
        If (scMode = '2' And @@Qte = 'Q' And EX3rdB = 'Y');
           isARcd     = *On;
           scEstArCd  = EXesCd;
           scSrArCd   = EXsrCd;
           scAdjArCd  = EXdaCd;
        EndIf;

        // Is this from Quote
        isQuote = *Off;
        If @@Qte = 'Q';
           isQuote = *On;
        EndIf;

       // Check if Office is installed
        Chain ('SY') ssy00;
        If Not %Found(ssylf) or SYofif <> 'Y' Or
           (scMode <> '4' And scMode <> '2' And @@Qte <> 'Q') Or
        //*** begin chg ***  08/27/19  ***********************************************
        //*****   (scMode <> '4' And @@Qte = 'Q');
           (scMode <> '2' And @@Qte = 'Q');
        //*** end chg *****  08/27/19  ***********************************************
           isRsvDarts = *Off;
           scRemarkCd = EXorg;
           scGenHrdC  = 'Y';
           scAgtCopy  = 'N';
        Else;
           isRsvDarts = *On;
           scRemarkCd = EXorg;
           scGenHrdC  = 'Y';
           scAgtCopy  = 'N';
        EndIf;

        If @@Qte = 'Q';
           scRemarkCd = EXqte;
        EndIf;

        //*** Beg Add ***** 03/12/24 ***********************
        If EX3rdb = 'Y';
           Clear scRemarkCd;
        Endif;
        //*** End Add ***** 03/12/24 ***********************

        If scRemarkCd <> ' ';
           scRsvDrts  = 'Y';
        Else;
           scRsvDrts  = ' ';
        EndIf;

        If @@CAll = ' ';
           isRsvDarts = *On;
        EndIf;

        // Will there be hard copy?
        isHardCopy = *Off;
        If isRsvDarts = *On And isArOnly = *Off;
           isHardCopy = *On;
        EndIf;

        // Will there be Agent hard copy?
        isHardCpy2 = *Off;
        If isTool = *Off And isArOnly = *Off;
           isHardCpy2 = *On;
        EndIf;

        //*** End Add ***** 05/05/22 ***********************************
        If isUseScript;
           isRsvDarts = *Off;
        Endif;
        //*** End Add ***** 05/05/22 ***********************************

       // Set Group Restrictions
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       // Division work file only used if called from toolbar
       If isTool = *On And Not %Open(WXDVP);
          // Copy DivisionWork file to QTEMP
          CallWtPolEstFC(##Library:'C');

          // Open Division work file
          Open wxdvp;
       EndIf;

      /End-Free
     p SetCriteria     e

      //===================================================================
      // ShowAllDivWdw - If Multiple Divisions Message Window
      //===================================================================
     P ShowAllDivWdw   b
     D ShowAllDivWdw   pi              n
      /Free

          If isTool = *On And scMode <> 'b' And scBillSel = 'A' And
             scDivCnt > 1;

             BtnOK     = *Off;
             BtnCancel = *Off;
             Exfmt RcdEstBil2;

             Select;
             When btnOK = *On;
                Return *Off;
             When btnCancel = *On;
                Return *On;
             EndSl;
          Else;
             Return *Off;
          EndIf;

      /End-Free
     p ShowAllDivWdw   e

      //===================================================================
      // SubmitDrvCL - Submit WTPOLESTCS - Esimated Billing.
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi
      /Free

        //***** Beg Add ***** 12/09/19 ***********************
         CleanUpBilling();
        //***** End Add ***** 12/09/19 ***********************
         ##CoC#       =  %Editc(intCo#:'X');
         ##FndC       =  %Editc(scFnd:'X');
         ##FyrC       =  %Editc(scPolYear:'X');
         ##EmpC#      =  %Editc(scEmp#:'X');

         If scBillSel = 'S' Or scBillSel = 'A';
            ##DivC       =  %Editc(scDiv:'X');
         Else;
            ##DivC       =  ' ';
         EndIf;

         ##PostDat    =  %EditC(scPostDat:'X');
         ##EstTrnCd   =  scEstArCd;
         ##DueDate    =  '000000' + %EditC(scIntDays:'X');
         ##PrtBal     =  scPrtBal;
         ##SlfEstFl   = GetSlfEstFlg();

         If scMode = 'b';
            ##Call    = ' ';
         Else;
            ##Call    = scMode;
         EndIf;

         ##Override   = ' ';
         ##SrCode     = scSrArCd;
         ##SrAdjCode  = scAdjArCd;
         ##Quote      = @@Qte;
         //*** begin add ***  11/05/19  **********************************
         Chain Q1User pup00;
         //*** end add *****  11/05/19  **********************************
         ##OutQ       = UPoutQ;

         If scNRB <> ' ';
            ##NewRenew = scNRB;
         Else;
            If scBillSel = 'S' or scBillSel = 'R' Or scBillSel = 'U';
               ##NewRenew = '#';
            Else;
               ##NewRenew = ' ';
            EndIf;
         EndIf;

         ##DartsFlg   = scRsvDrts;
         ##HardCopy   = scGenHrdC;
         ##AgentOrder = scAgtCopy;
         //*** Begin Add *** 05/03/19 **************************************
         If *in63 = *On;
            ##HardCopy   = 'Y';
            ##AgentOrder = 'O';
         Endif;
         //*** End Add  *** 05/03/19 ***************************************
         //*** Begin Chg *** 03/30/21 **************************************
         //*** ##FinalQuote = scFnlQte;
         Select;
         When scFnlQte = 'N' And scSbmIC = 'N';
            ##FinalQuote = ' ';
         When scFnlQte = 'Y' And scSbmIC = 'N';
            ##FinalQuote = 'Y';
         When scFnlQte = 'N' And scSbmIC = 'Y';
            ##FinalQuote = 'I';
         When scFnlQte = 'Y' And scSbmIC = 'Y';
            ##FinalQuote = 'B';
         Endsl;
         //*** End   Chg *** 03/30/21 **************************************
         ##Rmkcode    = scRemarkCd;
         ##ArCode     = ' ';
         ##EffDate    = '00000000';
         ##FreeComm1  = scFcommnt1;
         ##FreeComm2  = scFcommnt2;
         ##FreeComm3  = scFcommnt3;
         ##ArCode2    = ' ';
         ##BillSel    = scBillSel;

         CallWtPolEstCS(##CoC#:##FndC:##FyrC:##EmpC#:##DivC:##PostDat:
            ##EstTrnCd:##Comm1:##Comm2:##Comm3:##DueDate:##PrtBal:##SlfEstFl:
            ##call:##Override:##SrCode:##SrAdjCode:##Quote:##OutQ:##NewRenew:
            ##DartsFlg:##HardCopy:##AgentOrder:##FinalQuote:##RmkCode:##ArCode:
            ##EffDate:##FreeComm1:##FreeComm2:##FreeComm3:##ArCode2:##BillSel:
            ##library);

         // Close and Remove Work file
         If %Open(wxdvp);
            Close wxdvp;
            CallWtPolEstFC(##Library:'D');
         EndIf;

      /End-Free
     P SubmitDrvCL     e
        //***** Beg Add ***** 12/09/19 ***********************
        //===================================================================
        // CleanUpBilling
        //===================================================================
     P CleanUpBilling  b
     D CleanUpBilling  pi
     D aryQteTypes     s              1    DIM(9999)
     D Indx            s              4  0 inz
     D StrQteSlection  s            100    inz(*blanks)
     D decStRptDv      s              3  0 inz
      /Free
       StrQteSlection = scQteTypes;
       indx = 1;
       Dow StrQteSlection <> ' ';
          aryQteTypes(indx) = ParseString(StrQteSlection:',':1:'1');
          indx = indx + 1;
       Enddo;
       indx = 0;
       If scEmp# <> 0 and scPolYear <> 0;
          Chain (intCo#:scFnd:scPolYear:scEmp#:scDiv) pbl00;
       Else;
          Chain (intCo#:scFnd) pbl00;
       Endif;
       If %Found(wabll);
          decStRptDv = GetStRptDv(BLCo#:BLFnd:BLEmp#);
          indx = %Lookup((GetQuoteSts(BLCo#:BLFnd:BLEmp#:BLFyr:decStRptDv))
                  :aryQteTypes);
          If scQteType = 'E' and indx <> 0 ;
             Setll (BLCo#:BLFnd:BLFyr:BLEmp#) pbl00;
             If %Found(wabll);
                DoU %Eof(wabll);
                   Delete(e) pbl00;
                   Exsr Throwerror;
                   Reade (BLCo#:BLFnd:BLFyr:BLEmp#) pbl00;
                Enddo;
             Endif;
          Endif;
          If scQteType = 'I' and indx = 0 ;
             Setll (BLCo#:BLFnd:BLFyr:BLEmp#) pbl00;
             If %Found(wabll);
                DoU %Eof(wabll);
                   Delete(e) pbl00;
                   Exsr Throwerror;
                   Reade (BLCo#:BLFnd:BLFyr:BLEmp#) pbl00;
                Enddo;
             Endif;
          Endif;
       Endif;

       //Error Subroutine
       Begsr Throwerror;

          If %Error and %Status(wabll) = 1218;

             PopOneBtn('Policy '+%char(scFnd)+'/'+%char(scEmp#)+'/'+
             %char(scPolYear)+' does not meet selected Quote Status'+
             ' criteria. The process tried to remove billing record(s)'+
             ' for this policy but is finding a lock.'+
             ' Some of the records may have already been deleted'+
             ' prior to finding a lock.':'Error');

          Endif;

       Endsr;

      /End-Free
     P CleanUpBilling  e
        //***** End Add ***** 12/09/19 ***********************

