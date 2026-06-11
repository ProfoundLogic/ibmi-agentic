     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/TITLE WC2401 - Prompt for Premium Notice Report
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTPNTC - Prompt for Premium Notices Report
     h*****************************************************************
     h* Revisions                                                     *
     H*
     H* 10/14/25 C:65  Project: Phw#119909   (UBIC)
     H*                Populate the mass flag when premium notices are
     H*                called in mass compared to single mode/requests.
     H*
     H* 05/31/24 C:62  Ticket: 115689(099)
     H*                Change logic for conditions where both
     H*                scriptura and output are set.
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*                                                               *
     H* 08/17/21  C:079 Project: WC2663                               *
     H*                 Re-populate intFyr for inv# validation        *
     H*                 Auto fill dates based on invoice number       *
     H*                                                               *
     H* 03/31/21  C:062 Project: WC2663                               *
     H*                 Add Invoice#                                  *
     H*                                                               *
     H* 08/08/17  C:079 Project: SY395                                *
     H*                 Add Scriptura Output Control Window           *
     H*                                                               *
     H* 04/25/17  C:309 Project: WC2435                               *
     H*                 Fixing screen field id and subroutine         *
     H*                 /procedure name as per standards.             *
     h*                                                               *
     h*****************************************************************

     fWTRPTPRNTCcf   e             workstn extfile('WTRPTPRNTC')
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
     fStctl1    IF   E           K DISK    Rename(PCT01:PCT00)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fwdell1    IF   E           K disk    Rename(pel01:pel00)
     f*** begin add ***  08/13/21  ***********************************
     f                                     INFDS(DBFDSD2)
     f*** end add *****  08/13/21  ***********************************
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     f*                                    INFDS(DBFDSD)
     FWmexl     IF   E           K DISK    Rename(PEX01:PEX00)
     FWmahl     IF   E           K DISK    Rename(PAH01:PAH00)
     FWmarl     IF   E           K DISK    Rename(PAR01:PAR00)

     D*LO              C                   CONST('abcdefghijklmnopqrs-
     D*                                    tuvwxyz')
     D*UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D*                                    TUVWXYZ')

     dpsds           esds                  extname(szq1p)

     D*DBFDSD          DS
     D* DBRRND               397    400B 0
     d*** begin add ***  08/13/21  ***********************************
     D DBFDSD2         DS
     D  DBRRND2              397    400B 0

     d xxRRN           s              9  0
     d*** end add *****  08/13/21  ***********************************

     D  dsCmntCode     DS
     D  Cmntcod1               1      2
     D  Cmntcod2               3      4
     D  Cmntcod3               5      6
      *
     d WtPremNtc       pr                  Extpgm('WTRPTPRNTC')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A
     d
     d WtPremNtc       pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inrrn                         9A

     d outCltId        s              3
     d outFrdt         s              8
     d outTodt         s              8
     d outDate         s              8
     d outDat2         s              8
     d xxFrDt          s              8
     d xxToDt          s              8
     d outAddu         s              3
     d outGrma         s              1
     d outDrmk         s              5
     d outDres         s              1
     d outIdys         s              3
     d outPriSort      s              2
     d outSecSort      s              2
     d outMinb         s              7
     d outEmpNmF       s              2
     d outEmpNmT       s              2
     d outEmpNoF       s              9
     d outEmpNoT       s              9
     d outAgtNmF       s              2
     d outAgtNmT       s              2
     d outAgtNoF       s              5
     d outAgtNoT       s              5
     d outExDf         s              1
     d outExp          s              1
     d outDep          s              1
     d outSupBalP      s              1
     d outCmCod1       s              2
     d outCmCod2       s              2
     d outCmCod3       s              2
     d outPFB          s              1
     d outOutq         s             10
     d outMode         s              1
     d inMode          s              1
     d strrptl         s             23
     d outsufx         s              3
     d outAll          s              1
     d outMass         s              1
     d inkey           s             10
     d inerror         s              1
     d EL_rrn          s              9  0
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
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
     d strCmntDsc      s             30    inz(*Blanks)
      *
     d incall1         s              1
     d inErr           s              1
     d outCo#          s              3
     d outFnd          s              3
     d outPyr          s              3
     d outEmp#         s              9
     d outDiv          s              5
     d*** Beg Add *** 08/08/17 ************************************
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
     d xxBody                       350    overlay(svlda:673)
     d outDocPath      s            300
     d outMessage      s            256
     d outTitle        s             50
     d outCnlHide      s              1    inz(' ')
     d OutModeOut      s              1    inz(' ')
     d isScript        s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     d massFlag        s              1
     d xxSubmit        s              1n
     d xxProcType      s            100
     d*** Begin Add *** 03/31/21 ****************************************
     d arCount         s              9  0
     d outInvoice#     s             10
     d arPostDate      s              7  0 Inz(0)
     d*** End   Add *** 03/31/21 ****************************************
     d processType     c                   const('Premium Notice')
     d*** End Add *** 08/08/17 ************************************
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d*** Begin Chg *** 03/31/21 *******************************************
     d***/copy sCopyPolPr
     d/include sCopyPolPr
     d*** End   Chg *** 03/31/21 *******************************************
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
     d*** Beg Add *** 08/08/17 ************************************
     d/copy sPrModule
     d/copy sPrXML
     d*** End Add *** 08/08/17 ************************************

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

      * Polpulate Comment Code multi-select Box.
     d CmntCodLod      pr

      * Premium Notice Report Proc.
     d PremNtcProc     pr

      * New/Renewal Business Report Submit
     d PremNtcSbm      pr                  ExtPgm('W7320C')
     d outCo#                         3
     d outFnd                         3
     d outDate                        8
     d outDat2                        8
     d outAddu                        3
     d outExDf                        1
     d outCmCod1                      2
     d outCmCod2                      2
     d outCmCod3                      2
     d outPyr                         3
     d outEmp#                        9
     d outDiv                         5
     d outFrDt                        8
     d outToDt                        8
     d outPriSort                     2
     d outSecSort                     2
     d outEmpNmF                      2
     d outEmpNmT                      2
     d outAgtNmF                      2
     d outAgtNmT                      2
     d outEmpNoF                      9
     d outEmpNoT                      9
     d outAgtNoF                      5
     d outAgtNoT                      5
     d outOutq                       10
     d outMode                        1
     d outDrmk                        5
     d outIdys                        3
     d outGrma                        1
     d outMinb                        7
     d outDres                        1
     d outDep                         1
     d outExp                         1
     d outPFB                         1
     d outMass                        1
     d outSupBalP                     1
     d outAll                         1
     d*** Begin Add *** 03/31/21 *******************************************
     d outInvoice#                   10
     d*** End   Add *** 03/31/21 *******************************************

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // PremNtcProc - Premium Notices Report Process Submit
       //===================================================================
     p PremNtcProc     b
     d PremNtcProc     pi

      /Free

       outco# = %EditC(intCo#:'X');

       If scFnd <> 0;
          outFnd = %EditC(scFnd:'X');
       EndIf;

       // Set form/to date to scFrmDt
       If scPolYear = *Zeros And scEmpNbr = *Zeros And scDivision = *Zeros;
          If scFRMDT<> *Zeros;
             xxFrdt = %editc(scFrmDT:'X');
             xxToDt = %editc(scToDate:'X');
          Else;
             xxFrdt = %editc(scFDt:'X');
             xxTodt = %editc(scTDt:'X');
          Endif;
       Endif;

       If scPolYear <> *Zeros Or scEmpNbr <> *Zeros Or scDivision <> *Zeros;
          xxFrdt = *Blanks;
          xxTodt = *Blanks;
       Endif;

       outFrDt  = xxFrdt;
       outToDt  = xxTodt;

       If scFrmDt <> *Zero;
          outDate = %EditC(scFrmDt:'X');
          outDat2 = %EditC(scToDate:'X');
          outAll = ' ';
       Else;
          outDate  = %EditC(scFDt:'X');
          outDat2  = %EditC(scTDT:'X');
          outAll = 'Y';
       Endif;

       If scAddDays <> 0;
          outAddu  = %EditC(scAddDays:'X');
       Else;
          outAddu  = *Zeros;
       EndIf;

       If scEmpNbr <> *Zeros;
          outEmp# = %editc(scEmpNbr:'X');
       Else;
          outEmp# = '000000000';
       Endif;

       If scDivision <> *Zeros;
          outDiv = %editc(scDivision:'X');
       Else;
          outDiv = '00000';
       Endif;

       If scPolYear <> *Zeros;
          outPyr = %editc(scPolYear:'X');
       Else;
          outPyr = '000';
       Endif;

       If scEmpNoF <> *Zeros;
          outEmpNoF  = %editc(scEmpNoF:'X');
       Else;
          outEmpNoF = '000000000';
       Endif;

       If scEmpNoT <> *Zeros;
          outEmpNoT  = %editc(scEmpNoT:'X');
       Else;
          outEmpNoT = '000000000';
       Endif;

       //*** Begin Add *** 03/31/21 ****************************************
       outInvoice# = %EditC(scInv#:'X');

       // If invoice # past change installmnt date to posting date of invoice
       If isSingle = *On And scInv# <> 0;
          // Need to get the positng date
          Exec Sql Select EApoDt Into :arPostDate From WMEAP
                   Where EAco#=:intCo# And EAfnd=:intFnd And
                         EAemp#=:intEmp# And EAdiv=:intDiv And
                         EAninv=:scInv#
                   Limit 1;

          If arPostDate <> 0;
             outDate = %EditC(dtMDYY(arPostDate):'X');
             outDat2 = outDate;
             outAll = ' ';
          EndIf;

       Endif;
       //*** End   Add *** 03/31/21 ****************************************

       If scAgtNoF <> *Zeros;
          outAgtNoF  = %editc(scAgtNoF:'X');
       Else;
          outAgtNoF = '00000';
       Endif;

       If scAgtNoF <> *Zeros;
          outAgtNoT  = %editc(scAgtNoT:'X');
       Else;
          outAgtNoT = '00000';
       Endif;

       outEmpNmF  = scEmpNmF;
       outEmpNmT  = scEmpNmT;
       outAgtNmF  = scAgtNmF;
       outAgtNmT  = scAgtNmT;
       outExdf    = scExcDedFe;
       outCmCod1  = CmntCod1;
       outCmCod2  = CmntCod2;
       outCmCod3  = CmntCod3;
       outPriSort = scPriSort;
       outSecSort = scSecSort;
       outDrmk    = scDartRmk;

       If scIntrMday <> *Zeros;
          outIdys    = %Editc(scIntrMday:'X');
       Else;
          outIdys = '000';
       Endif;

       outGrma    = scGenerate;

       If scMinBal <> *Zeros;
          outMinb    = %Editc(scMinBal:'X');
       Else;
          outMinb = '0000000';
       Endif;

       outPFB     = scOutType;
       outOutQ    = strOutQ;
       outMode    = 'D';
       outSupBalP = scSupBalP;
       outMass    = *Blanks;
       outDres    = scResDart;
       outDep     = scIncDepst;
       outExp     = scIncExpCo;

       //*** Begin Add *** 03/31/21 ****************************************
       //PremNtcSbm(outCo#:outFnd:outDate:outDat2:outAddu:outExdf:outCmCod1:
       //outCmCod2:outCmCod3:outPyr:outEmp#:outDiv:outFrDt:outToDt:outPriSort:
       //outSecSort:outEmpNmF:outEmpNmT:outAgtNmF:outAgtNmT:outEmpNoF:
       //outEmpNoT:outAgtNoF:outAgtNoT:outOutQ:outMode:outDrmk:outIdys:
       //outGrma:outMinb:outDres:outDep:outExp:outPFB:outMass:outSupBalP:
       //outAll);
       PremNtcSbm(outCo#:outFnd:outDate:outDat2:outAddu:outExdf:outCmCod1:
        outCmCod2:outCmCod3:outPyr:outEmp#:outDiv:outFrDt:outToDt:outPriSort:
        outSecSort:outEmpNmF:outEmpNmT:outAgtNmF:outAgtNmT:outEmpNoF:
        outEmpNoT:outAgtNoF:outAgtNoT:outOutQ:outMode:outDrmk:outIdys:
        outGrma:outMinb:outDres:outDep:outExp:outPFB:outMass:outSupBalP:
        outAll:outInvoice#);
       //*** End   Add *** 03/31/21 ****************************************

      /End-Free

     p PremNtcProc     e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdPrmNtc;

       // Load Action panel with the Company Name.
       intCo#     = UserCo#(Q1User);
       scCompName = GetCompNam(intCo#);
       scFnd      = UserGrp(Q1User);
       //*** Beg Add *** 08/17/21 **********************************************
       scCo#     = intCo#;
       //*** End Add *** 08/17/21 **********************************************

       intFnd     = scFnd;
       scExcDedFe = 'N';
       scIncExpCo = 'Y';
       scIncDepst = 'Y';

       //Get Default DARTS Remark Code From Admin/DARTS control maint.
       Chain(n)(intCo#:intFnd) Pex00;
       If %found(wmexl);
          scDartRmk = Expnrm;
          scSupBalp = Exspyb;
       Else;
          scDartRmk = *Blanks;
          scSupBalp = *Blanks;
       Endif;

       //Defaults the "Resolve to DARTS" flag
       If outCltid = '114';
          scRESDART = 'Y';
       Else;
          scRESDART = 'N';
       Endif;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          setCriteria();

          // To load Comment code values.
          CmntCodLod();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When selEmpSrch = *On;
             EmployerSrch();

          When BtnSubmit = *on;
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
          //*** Beg Add *** 08/08/17 ********************************
                Exsr OutControl;
                If        xxSubmit = *off;
                   Leave;
                Endif;
          //*** End Add *** 08/08/17 ********************************
                PremNtcProc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // To load Comment code values.
             SetCriteria();
             CmntCodLod();
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

          If Q1Prms >=3;
             Exfmt RcdPrmNtc;
          Else;
             Write rcdComPnl;
             Write RcdTabPrNt;

             Exfmt RcdPrmNtc;

             Read rcdComPnl;
             Read RcdTabPrNt;
          Endif;

       EndSl;

       Endsr;
         //*** Beg Add *** 08/08/17 **********************************

       //===================================================================
       // Write Output Control Screen
       //===================================================================
       Begsr OutControl;

           xxSubmit = *on;

           if isScript = 'Y';

             xxProcType = processType;
             In svLDA;
             Clear svLDA;


             if scDartRmk <> *blanks;
                xxDart = 'Y';
                xxRMCD = scDartRmk;
             Else;
                xxDart = 'N';
                xxRMCD = ' ';
             Endif;

             Out svLDA;

          //*** begin chg ***  08/13/21  ***********************************
          //*****   xxSubmit = OutputWindow(xxProcType:massFlag:outCnlHide);
             If xxRRN <> 0;
                xxSubmit = OutputWindow(xxProcType:massFlag:outCnlHide:' ':
                 xxRRN);
             Else;
          //*** begin add ***  10/14/25  ***********************************
                massFlag = 'Y';
          //*** end add *****  10/14/25  ***********************************
                xxSubmit = OutputWindow(xxProcType:massFlag:outCnlHide);
             Endif;
          //*** end chg *****  08/13/21  ***********************************

           endif;
         //*** End Add *** 08/08/17 **********************************
       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInVal();

       // When screen is called as pop up
       If Q1Prms >= 3;
          Eval isPopUp  = '1';
          Eval isPop    = '1';
          Eval isFld    = '0';
          inEmpRrn = %Dec(inRrn:9:0);
       Endif;

       If inEmprrn <> 0;
          Chain(n) inEmpRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;
             intFyr  = ELFyr;

             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
             If %Found(Wmeml);
                scEmpNbr   = EMemp#;
                scDivision = EMdiv;
                scEmpName  = EMname;
                scPolYear  = ELFyr;
                scFnd      = ELfnd;
                isPolDtl   = '1';
                //*** Beg Add *** 08/17/21 *********************************
                scFnd2     = scFnd;
                //*** End Add *** 08/17/21 *********************************
             EndIf;
          Endif;
          //*** Begin Add *** 03/31/21 *************************************
          isSingle = *On;
          //*** End   Add *** 03/31/21 *************************************
       Else;
          isPopUp = '0';
          isFld   = '1';
          isPop   = '1';
          isPolDtl= '0';
          //*** Begin Add *** 03/31/21 *************************************
          isSingle = *Off;
          //*** End   Add *** 03/31/21 *************************************
       EndIf;

       // Get the client id
       GetClientID(outCltId);

       isFMIC = *off;
       If outCltId = '102';
          isFMIC = *on;
       Endif;


       // Retrieve Output Queue of the User.
       Chain(n) Q1User pup00;
       If %Found(Wmupl);
          strOutQ = UPoutQ;
       EndIf;

       // Retrieve Output Suffix.
       GetSuffix(outSufx);
       strRptl = 'PDSRPT' + outSufx;
       scRptLib = 'This report will produce output file WFNBP in library '+
                  %trim(strrptl) + '.';

       tabClick = *Off;
       //*** Beg Add *** 08/08/17 ************************************
       // Determine if using spool output and/or Scriptura
       // Set field indicator to determine if fields are displayed
                         isUseSpoolOut = UseSpoolOut(processType);
                         isUseXML = UseScriptura(processType)
                                    or Use3rdXml(processType);
                         //*** Begin Chg *** 05/1/24 ***********************
                         //*** if isUseXML;
                         //***   isScript = 'Y';
                         //***   isScripFld = *off;
                         //*** else;
                         //***   isScript = 'N';
                         //***   isScripFld = *on;
                         //*** Endif;
                         Select;
                         When isUseXml And isUseSpoolOut;
                            isScript   = 'Y';
                            isScripFld = *On;
                         When isUseXml;
                            isScript   = 'Y';
                            isScripFld = *Off;
                         When isUseSpoolOut;
                            isScript   = 'N';
                            isScripFld = *On;
                         EndSl;
                         //*** End   Chg *** 05/1/24 ***********************
       //*** End Add *** 08/08/17 ************************************

       Return;

      /End-Free

     p ProgramStart    e

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
                scEmpNbr   = EMemp#;
                scDivision = EMdiv;
             EndIf;
          EndIf;
       EndIf;

      /End-Free

     p EmployerSrch    e

       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // initialize the error indicator
       isValid = *Off;
       msFnd      = *blanks;
       erFnd      = *off;
       msFDt      = *blanks;
       erFDt      = *off;
       msTDt      = *blanks;
       erTDt      = *off;
       msFrmDt    = *blanks;
       erFrmDt    = *off;
       msToDate   = *blanks;
       erToDate   = *off;
       msAddDays  = *blanks;
       erAddDays  = *off;
       msGenerate = *blanks;
       erGenerate = *off;
       msDartRmk  = *blanks;
       erDartRmk  = *off;
       msResDart  = *blanks;
       erResDart  = *off;
       msIntrmDay = *blanks;
       erIntrmDay = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msSecSort  = *blanks;
       erSecSort  = *off;
       msMinBal   = *blanks;
       erMinBal   = *off;
       msEmpNamF  = *blanks;
       erEmpNamF  = *off;
       msEmpNamT  = *blanks;
       erEmpNamT  = *off;
       msAgtNamF  = *blanks;
       erAgtNamF  = *off;
       msAgtNamT  = *blanks;
       erAgtNamT  = *off;
       msEmpNbr   = *blanks;
       erEmpNbr   = *off;
       msDivision = *blanks;
       erDivision = *off;
       msPolYear  = *blanks;
       erPolYear  = *off;
       msEmpNumF  = *blanks;
       erEmpNumF  = *off;
       msEmpNumT  = *blanks;
       erEmpNumT  = *off;
       msAgtNumF  = *blanks;
       erAgtNumF  = *off;
       msAgtNumT  = *blanks;
       msAgtNumT  = *off;
       msCmCodes  = *blanks;
       erCmCodes  = *off;
       msExcDedFe = *blanks;
       erExcDedFe = *off;
       msIncExpCo = *blanks;
       erIncExpCo = *off;
       msIncDepst = *blanks;
       erIncDepst = *off;
       msOutType  = *blanks;
       erOutType  = *off;
       //*** Begin Add *** 03/31/21 ****************************************
       mgInv#     = *blanks;
       erInv#     = *off;
       //*** End   Add *** 03/31/21 ****************************************
       //*** begin add ***  08/13/21  ***********************************
       xxRRN = 0;
       //*** end add *****  08/13/21  ***********************************

       // Validation Check for Fund
       If scFnd   = *Zeros;
          erFnd   = *On;
          msFnd   = 'WC30622';
          isValid  = *on;
       EndIf;

       // Validate Policy effective date
       // both installment and policy eff dates cannot be entered.
       If isFMIC = *On and scFrmDt<> *Zero or scToDate <> *zero;
          If scFDt <> *Zero or scTDt <> *Zero;
             erFrmDt  = *On;
             msFrmDt  = 'WCR0590';
             erToDate = *On;
             msToDate = 'WCR0590';
             erFDt    = *On;
             msFDt    = 'WCR0590';
             erTDt    = *On;
             msTDt    = 'WCR0590';
             isValid  = *on;
          Endif;
       Endif;

       If scFrmDt <> *Zero or isFMIC = *off;
       // Validation the Installment (Notice) date entered
          erFrmDt  = ValidDate(scFrmDt:charMsg);
          If erfrmDt  = *on;
             msFrmDt  = charMsg;
             isValid  = *on;
          Endif;

       // Validation the Installmet 'to' date entered
          erToDate = ValidDate(scToDate:charMsg);
          If erToDate = *on;
             msToDate = charMsg;
             isValid  = *on;
          Endif;

       //Installment "from date" cannot be greater than "to date"
          erToDate = AscendDates(scFrmDt:scToDate:charMsg);
          If erToDate = *on;
             erfrmDt  = *on;
             msToDate = charMsg;
             msFrmDT  = charMsg;
             isValid  = *on;
          Endif;

       // Validate installment "from" and "to" dates are in same month
          If %Subst(%Editc(scFrmDt:'X'):1:2)  <>
           %Subst(%Editc(scToDate:'X'):1:2)  or %Subst(%Editc(scFrmDt:'X'):5:4)
            <> %Subst(%Editc(scToDate:'X'):5:4);
             erFrmDt  = *On;
             msFrmDt  = 'WC30183';
             isValid  = *on;
          EndIf;

       Else;

       // Check From and To date for valid values
       // Validate the Policy effective 'from' date
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

       // "from date" cannot be greater than "to date"
          erTDt = AscendDates(scFDt:scTDt:charMsg);
          If erFDt = *on;
             erTDt = *on;
             msFDt = charMsg;
             msTDt = charMsg;
             isValid  = *on;
          Endif;
       Endif;

       // Validation Check for Generate
       If scGenerate= *Blanks;
          erGenerate = *On;
          msGenerate = 'WCR0503';
          isValid  = *on;
       EndIf;

       If scGenerate ='B';
          If scFrmDt<> scToDate;
             erFrmDt  = *On;
             msFrmDt  = 'WC30182';
             isValid  = *on;
          Endif;
       Endif;

       // Validate the Add'l Days From Installment Due
       If scAddDays < *Zeros ;
          erAddDays = *On;
          msAddDays = 'WCR0547';
          isValid  = *on;
       EndIf;

       // Validate Exclude Deductible Fees (must be "Y" OR "N")
       If scExcDedFe <> 'Y' and scExcDedFe <> 'N';
          erExcDedFe = *On;
          msExcDedFe = 'WCR0501';
          isValid  = *on;
       EndIf;

       // Validate Interim days
       If scIntrmDay < *Zeros;
          erIntrmDay= *On;
          msIntrmDay= 'WCR0547';
          isValid  = *on;
       EndIf;

       //Extract comment code in a DS.
       Dow %scan(',':%trimR(scCmCodes)) > *zeros;
          scCmCodes = %Replace ('':scCmCodes:%scan(',':%trimR(scCmCodes)):1);
          dsCmntcode  = scCMCodes;
       Enddo;

       // Validate that 3 commnet code are selected
       If %len(%trim(scCmCodes)) > 6 ;
          erCmCodes = *On;
          msCmCodes = 'WCR4823';
          isValid  = *on;
       EndIf;

       // Validate the Darts Remark code
       If scDartRmk <> *Blanks;
          Chain scDartRmk pct00;
          If not %found;
             erDartRmk  = *On;
             msDartRmk  = 'WCR0878';
             isValid  = *on;
          Endif;
       EndIf;

       // Validate the "Resolve to DARTS" flag
       If scResDart <> 'Y' and scResDart <> 'N';
          erResDart = *On;
          msResDart = 'WCR0501';
          isValid  = *on;
       EndIf;

       //If "Resolve to DARTS" flag = 'Y', then user must select a valid
       //DARTS remark.
       If scResDart  = 'Y';
          Chain scDartRmk Pct00;
          If not %found;
             erDartRmk  = *On;
             msDartRmk  = 'WCR0878';
             isValid  = *on;
          Endif;
       Endif;

       //Validate only one set of fields entered
       If scPolYear <> *Zeros or scEmpNbr <> *Zeros or scDivision <> *Zeros;
          If scPriSort <> *Blanks;
             erPriSort  = *On;
             msPriSort  = 'WC30653';
             erEmpNbr   = *On;
             msEmpNbr   = 'WC30653';
             isValid  = *on;
          EndIf;
       EndIf;

       //Validate either individual employer entered or large run
       If scPolYear = *Zeros and scEmpNbr = *Zeros and scDivision = *Zeros
        and scPriSort = *Blanks;
          erPriSort  = *On;
          msPriSort  = 'WC30654';
          erEmpNbr   = *On;
          msEmpNbr   = 'WC30654';
          isValid  = *on;
       EndIf;

       //Validations for single employer run
       If scPolYear <> *Zeros Or scEmpNbr <> *Zeros Or scDivision <> *Zeros;
       //validate the Emp/Div entered
          intEmp# = scEmpNbr;
          Chain(n)(intCo#:intFnd:intEmp#) pem00;
          If not %found(wmeml);
             erEmpNbr = *On;
             msEmpNbr = 'WCR0538';
             isValid  = *on;
          Endif;
          intDiv = scDivision;
          Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem00;
          If Not %Found(wmeml);
             erDivision = *On;
             msDivision = 'WC30063';
             isValid  = *on;
          Endif;
       //validate the Policy year entered
          intFyr = scPolYear;
          Chain(n)(intCo#:intFnd:intFyr:intEmp#:intDiv) pel00;
          If Not %Found (wdell1);
             erPolYear = *On;
             msPolYear = 'WCR0639';
             isValid  = *on;
       //*** begin add ***  08/13/21  ***********************************
          Else;
             xxRRN = dbrrnd2;
       //*** end add *****  08/13/21  ***********************************
          Endif;

       //Premium notice can't be run for single employer If agency Bill.
          intfyr = *Hival;
          Setgt (intCo#:intFnd:intEmp#:intDiv:intFyr) pah00;
          Readpe (intCo#:intFnd:intEmp#:intDiv) pah00;
          Dow  Not %Eof (Wmahl) and AHstat <> 'A';
             Readpe (intCo#:intFnd:intEmp#:intDiv) pah00;
          Enddo;
          If Not %Eof(wmahl);
             AHefdt = *Hival;
             Setgt (intCo#:intFnd:Ahrt#:AHefdt) par00;
             Readpe (intCo#:intFnd:AHrt#) par00;
             Dow  Not %Eof (Wmarl) and ARstat <> 'A';
                Readpe (intCo#:intFnd:AHrt#) par00;
             Enddo;
          Endif;
          If Not %Eof(wmarl) and ARplan ='A';
             erPolYear = *On;
             msPolYear = 'WC31370';
             msDivision = *On;
             msDivision = 'WC31370';
             erEmpNbr = *On;
             msEmpNbr = 'WC31370';
             isValid  = *on;
          Endif;

          //*** Begin Add *** 03/31/21 *************************************
          // If invoice# is entered then there must be A/R for that invoice#
          //*** Beg Add *** 08/17/21 ***************************************
          intFyr = scPolYear;
          //*** End Add *** 08/17/21 ***************************************
          If scInv# <> 0;
            Exec sql Select count(*) Into :arCount from wmeap
             Where EAco# = :intCo# And EAfnd = :intFnd And EAEmp# = :intEmp#
               And EAdiv = :intDiv And EAfyr = :intFyr And EAninv = :scInv#;

            If arCount = 0;
               mgInv# = 'There aren''t any A/R records for this invoice#.';
               erInv# = *On;
               isValid  = *On;
            EndIf;
          EndIf;
          //*** End   Add *** 03/31/21 *************************************

       Endif;


       // Validate the secondary sort field
       //Secondary sort must be Emplr#/Name if agent #/Name as Primary
       If scPriSort = 'AA' or scPriSort = 'AN';
          If scSecSort <> 'EA' and scSecSort <> 'EN';
             erSecSort  = *On;
             msSecSort  = 'WCR0146';
             isValid  = *on;
          EndIf;
       Endif;

       // Validate Minimum balance due
       If scMinBal < *Zeros;
          erMinBal = *On;
          msMinBal = 'WCR0547';
          isValid  = *on;
       EndIf;

       // Validate Include Expense Constant
       If scIncExpCo <> 'Y' and scIncExpCo <> 'N';
          erIncExpCo = *On;
          msIncExpCo = 'WCR0501';
          isValid  = *on;
       EndIf;

       // Validate Include Deposit
       If scIncDepst <> 'Y' and scIncDepst <> 'N';
          erIncDepst= *On;
          msIncDepst= 'WCR0501';
          isValid  = *on;
       EndIf;

       // Validate type of output(P, F & B')
       If scOutType = *Blanks;
          erOutType= *On;
          msOutType= 'WCR0503';
          isValid  = *on;
       EndIf;

       // Validate the Employer sort ranges
       If scEmpNmT <> *Blanks;
          // Convert to upper case
          scEmpNmF = Upper(scEmpNmF);
          scEmpNmT = Upper(scEmpNmT);
          If scEmpNoT <> *Zeros;
             erEmpNumT = *On;
             erEmpNamF = *On;
             erEmpNamT = *On;
             erEmpNumF = *On;
             msEmpNumT = 'WCR0918';
             msEmpNumF = 'WCR0918';
             msEmpNamF = 'WCR0918';
             msEmpNamT = 'WCR0918';
             isValid  = *on;
          Endif;
       EndIf;

       // Validate the Agent sort ranges
       If scAgtNmT <> *Blanks;
          // Convert to upper case
          scAgtNmF = Upper(scAgtNmF);
          scAgtNmT = Upper(scAgtNmT);
          If scAgtNoT <> *Zeros;
             erAgtNamF = *On;
             erAgtNamT = *On;
             erAgtNumF = *On;
             erAgtNumT = *On;
             msAgtNamF = 'WCR0918';
             msAgtNamT = 'WCR0918';
             msAgtNumF = 'WCR0918';
             msAgtNumT = 'WCR0918';
             isValid  = *on;
          Endif;
       EndIf;

       // Validate the low range is not greater than the high range
       // Employer Name
       If scEmpNmF > scEmpNmT;
          erEmpNamF = *On;
          msEmpNamF = 'WCR0750';
          isValid  = *on;
       EndIf;

       // Employer number
       If scEmpNoF > scEmpNoT;
          erEmpNumF = *On;
          msEmpNumF = 'WCR0750';
          isValid  = *on;
       EndIf;

       // Agent Name
       If scAgtNmF > scAgtNmT;
          erAgtNamF = *On;
          msAgtNamF = 'WCR0750';
          isValid  = *on;
       EndIf;

       // Agent Number
       If scAgtNoF > scAgtNoT;
          erAgtNumF = *On;
          msAgtNumF = 'WCR0750';
          isValid  = *on;
       EndIf;


      /End-Free
     p ValidPnl        e

       //===================================================================
       // CmntCodLod - Load comment code multiselect box.
       //===================================================================
     p CmntCodLod      b
      /Free

       scCmntSel  = ' ';
       scCmntVal  = *Blanks;
       strCmntDsc = *Blanks;

       // Read WTBCP file from starting
       Setll intCo# pbc00;
       ReadE intCo# pbc00;
       Dow Not %Eof(wtabl);
          If BCstat = 'A';
             strCmntDsc = TitleCase(BCdesc);
             scCmntSel  = %trim(scCmntSel)+','+%trim(BCkey)+'-'+
                          %trim(%xlate(',':' ': StrCmntDsc));
             scCmntVal = %trim(scCmntVal)+','+%trim(BCkey);
          Endif;
          ReadE intCo# pbc00;
       Enddo;

      /End-Free
     p CmntCodLod      e
