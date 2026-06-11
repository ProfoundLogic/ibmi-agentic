     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTLTNOT - Prompt for Late Notice Report - Estimate
     h*****************************************************************
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*    Menu                                                            *
     H*  CALLS TO:                                                         *
     H*    W7290C                                                          *
     H*                                                                    *
     H*  CREATED: 10/03/16  PROGRAMMER- C:300                              *
     H*  PROJECT: WC2456 - Prompt for Late Notice Report - Estimate        *
     H*                                                                    *
     H***********************************************************************
     h*****************************************************************
     h* Revisions                                                     *
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*                                                               *
     H* 03/30/21 C:65  Support: Phw#94661 (LUBA)                      *
     H*                Add visibility properties to the darts fields  *
     H*                to only display them on regular prompt screen  *
     H*                if scriptura is not being used.  If Scriptura  *
     H*                is used then darts fields is on output control *
     H*                window.                                        *
     H*                If running generate report only then clear out *
     H*                the resolve to iDarts and remark code fields.  *
     H*                                                               *
     H* 09/28/18 C:079 Support: 133/73227                             *
     H*                Increase Act Status Choices field to 2000      *
     H*                                                               *
     H* 04/07/17 C:306 Support: 46260                                 *
     H*                Fixed the program to generate the late notice  *
     H*                report for for single employer (standalone).   *
     H*                Also added a code Implement Scriptura output   *
     H*                control window. (SY371).                       *
     h*                                                               *

     fWTRPTLTNOTcf   e             workstn extfile('WTRPTLTNOT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     fWmeml     IF   E           k DISK    Rename(pem01:pem00)
     fWmfnl     IF   E           k DISK    Rename(pfn01:pfn00)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)
     fWtabl     IF   E           K DISK    include(pbc01)
     f                                     include(ptt01)
     fwtavl     if   e           k disk    Rename(pav01:pav00)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fStctl1    if   e           k disk
    F** Beg add ***  04/07/17  ***********************************
     Fwmexl     if   e           k disk    Rename(pex01:pex00)
     Fwxgsl     if   e           k disk    Rename(pgs01:pgs00)
    F** End add ***  04/07/17  ***********************************

     dpsds           esds                  extname(szq1p)

      *
     d WtLateNot       pr                  Extpgm('WTRPTLTNOT')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inRrn                         9A
     d
     d WtLateNot       pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inRrn                         9A

     d charMsg         s            100
     d intCo#          s                   Like(EMco#)
     d intEmp#         s                   Like(EMemp#)
     d intDiv          s                   Like(EMdiv)
     d intFnd          s                   Like(ELfnd)
     d strOutq         s                   Like(Upoutq)
     d intpos          s              3  0 inz(0)
     d inARcdsTmp      s            200    inz(*Blank)
     d inAScdsTmp      s            200    inz(*Blank)
     d inCmcdsTmp      s            200    inz(*Blank)
     d strARdesc       s             30
     d strAvtDsc       s             30
     d strCmntDsc      s             30
      *
     d outRrn#         s              9
     d empRrn          s              9  0
     d inSelect        s           1024
     d isPopup         s               n
     d isValid         s               n   Inz(*off)
     d intRrn          s              9  0 inz(0)
      *
     d outCo#          s              3
     d outFnd          s              3
     d inFrDt          s              8
     d inToDt          s              8
     d outDt           s             16
     d outBlDt         s              8
     d outAmt          s              9
     d outRptF         s              1
     d outCtst         s              1
     d outOutq         s             10
     d outMode         s              1
     d outFiDy         s              2
     d outEbDy         s              2
     d outAbDy         s              2
     d outRaDy         s              2
     d outSrt1         s              2
     d outSrt2         s              2
     d outEA           s              4
     d outEN           s             18
     d outAA           s              4
     d outAN           s             10
     d outAcst         s              1
     d outEssr         s              1
     d outPayD         s              8
     d outCom          s             10
     d outDrmk         s              5
     d outAust         s              1
     d outARCode       s             60
     d outArie         s              1
     d outIefl         s              1
     d outAstc         s             10
     d outTofnd        s              3
     d outDart         s              1
     d outEmp#         s              9
     d outDiv          s              5
     d outFrEF         s              8
     d outToEF         s              8
     d outEF           s             16
     d outIndp         s              1
     d outAddu         s              3
     d outJrSt         s              2
     d outGrpSlct      s              1
       // Activity Status code DS
     d dsActSts        DS
     d dsActSts1                      2
     d dsActSts2                      2
     d dsActSts3                      2
     d dsActSts4                      2
     d dsActSts5                      2
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy wPrEmpLr
     d/copy sPrDate
     //**** Beg Chg *** 04/07/17 ************************/
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Estimated Late Notice')
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



     //**** End Chg *** 04/07/17 ************************/

      * Clear Error/Message fields
     d ClrErr          pr

      * Late Notice Report - Estimate proc.
     d LatNotproc      pr

      * Late Notice Report - Estimate Submit
     d LatNotSbm       pr                  ExtPgm('W7290C')
     d outCo#                         3
     d outFnd                         3
     d outDt                         16
     d outBlDt                        8
     d outAmt                         9
     d outRptF                        1
     d outCtst                        1
     d outOutq                       10
     d outMode                        1
     d outFiDy                        2
     d outEbDy                        2
     d outAbDy                        2
     d outRaDy                        2
     d outSrt1                        2
     d outSrt2                        2
     d outEA                          4
     d outEN                         18
     d outAA                          4
     d outAN                         10
     d outAcst                        1
     d outEssr                        1
     d outPayD                        8
     d outCom                        10
     d outDrmk                        5
     d outAust                        1
     d outARCode                     60
     d outArie                        1
     d outIefl                        1
     d outAstc                       10
     d outTofnd                       3
     d outDart                        1
     d outEmp#                        9
     d outDiv                         5
     d outEF                         16
     d outIndp                        1
     d outAddu                        3
     d outJrSt                        2
     d outGrpSlct                     1

      * Load Activity Status Codes
     d LoadActStsCd    pr

      * Load AR Payment Codes select box
     d LoadARCodes     pr

      * Polpulate Comment Code multi-select Box.
     d LoadCmCodes     pr

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

      * Validate Panel Sreen Values
     d ValidPnlNoPop   pr              n

      * Validate Panel Sreen Values
     d ValidPnlPop     pr              n

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid   = *off;
       msAccTyp  = *blanks;
       erAccTyp  = *off;
       msAccSts  = *blanks;
       erAccSts  = *off;
       msAudSts  = *blanks;
       erAudSts  = *off;
       msInsFrDt = *blanks;
       erInsFrDt = *off;
       msInsToDt = *blanks;
       erInsToDt = *off;
       msMinAmt = *blanks;
       erMinAmt = *off;
       msAddDay  = *blanks;
       erAddDay  = *off;
       msJrSt    = *blanks;
       erJrSt    = *off;
       msFIDay   = *blanks;
       erFIDay   = *off;
       msEBDay   = *blanks;
       erEBDay   = *off;
       msABDay   = *blanks;
       erABDay   = *off;
       msRADay   = *blanks;
       erRADay   = *off;
       msEmpNbr  = *blanks;
       erEmpNbr  = *off;
       msDivNbr  = *blanks;
       erDivNbr  = *off;
       msPEffFrDt= *blanks;
       erPEffFrDt= *off;
       msPEffToDt= *blanks;
       erPEffToDt= *off;
       msEmpNmF  = *blanks;
       erEmpNmF  = *off;
       msEmpNmT  = *blanks;
       erEmpNmT  = *off;
       msEmpNoF  = *blanks;
       erEmpNoF  = *off;
       msEmpNoT  = *blanks;
       erEmpNoT  = *off;
       msAgtNmF  = *blanks;
       erAgtNmF  = *off;
       msAgtNmT  = *blanks;
       erAgtNmT  = *off;
       msAgtNoF  = *blanks;
       erAgtNoF  = *off;
       msAgtNoT  = *blanks;
       erAgtNoT  = *off;
       msPriSort = *blanks;
       erPriSort = *off;
       msSecSort = *blanks;
       erSecSort = *off;
       msRmCd    = *blanks;
       erRmCd    = *off;
       msCmCd    = *blanks;
       erCmCd    = *off;
       msArCodIE = *blanks;
       erArCodIE = *off;
       msARcd    = *blanks;
       erARcd    = *off;
       msEpActSts= *blanks;
       erEpActSts= *off;
       msASCd    = *blanks;
       erASCd    = *off;
      /End-Free

     p ClrErr          e
       //===================================================================
       // LatNotproc - Late Notice Report -Estimate Submit
       //===================================================================
     p LatNotproc      b
     d LatNotproc      pi

      /Free

       outCo#    = %EditC(intCo#:'X');
       If isPopup = *on;
          outFnd    = %Subst(scGrp:1:3);
       EndIf;

       If scInsFrDt <> 0;
          inFrDt = %EditC(scInsFrDt:'X');
       Else;
          inFrDt = *Blanks;
       EndIf;

       IF scInsToDt <> 0;
          inToDt = %EditC(scInsToDt:'X');
       Else;
          inToDt = *Blanks;
       EndIf;

       outDt     = inFrDt + inToDt;
       outBlDt   = inToDt;
       outAmt    = %EditC(scMinAmt:'X');
       outRptF   = scGenRpt;
       outCtst   = scConAud;
       outOutq   = strOutq;
       outMode   = 'D';
       outFiDy   = %EditC(scFIDay:'X');
       outEbDy   = %EditC(scEBDay:'X');
       outAbDy   = %EditC(scABDay:'X');
       outRaDy   = %EditC(scRADay:'X');
      //**** Beg Chg *** 04/07/17 ************************/
       // For single employer by default 'EN' should be passed as sorting.
       If isPopup = *On;
          outSrt1   = 'EN';
       Else;
      //**** End Chg *** 04/07/17 ************************/
       outSrt1   = scPriSort;
      //**** Beg Chg *** 04/07/17 ************************/
       EndIf;
      //**** End Chg *** 04/07/17 ************************/
       outSrt2   = scSecSort;
       outEA     = %trim(Upper(scEmpNmF)) + %trim(Upper(scEmpNmT));
       outEN     = %EditC(scEmpNoF:'X') + %EditC(scEmpNoT:'X');
       outAA     = %trim(Upper(scAgtNmF)) + %trim(Upper(scAgtNmT));
       outAN     = %EditC(scAgtNoF:'X') + %EditC(scAgtNoT:'X');
      //**** Beg Chg *** 04/07/17 ************************/
       // For single employer by default should be passed.
       If isPopup = *ON;
          outAcst   = 'B';
          outEssr   = 'B';
       Else;
      //**** End Chg *** 04/07/17 ************************/
       outAcst   = scAccSts;
       outEssr   = scAccTyp;
      //**** Beg Chg *** 04/07/17 ************************/
       EndIf;
       //***outPayD   = *zeros;
       outPayD   = *blanks;
      //**** End Chg *** 04/07/17 ************************/
       outCom    = %Trim(inCMcdsTmp);
       outDrmk   = scRmCd;
      //**** Beg Chg *** 04/07/17 ************************/
       // For single employer by default should be passed.
       If isPopup = *On;
          outAust   = 'B';
       Else;
      //**** End Chg *** 04/07/17 ************************/
       outAust   = scAudSts;
      //**** Beg Chg *** 04/07/17 ************************/
       EndIf;
      //**** End Chg *** 04/07/17 ************************/
       outARCode = %Trim(inARcdsTmp);
       outArie   = scArCodIE;
       outIefl   = scEpActSts;
       outAstc   = dsActSts;
       outTofnd  = *Blanks;
       If isPopup = *on;
          outTofnd = outFnd;
       EndIf;
       outDart   = scDart;
       outEmp#   = %EditC(scEmpNbr:'X');
      //**** Beg Chg *** 04/07/17 ************************/
       If isPopup = *On;
          outDiv = *blanks;
       Else;
      //**** End Chg *** 04/07/17 ************************/
       outDiv    = %EditC(scDivNbr:'X');
      //**** Beg Chg *** 04/07/17 ************************/
       EndIf;
      //**** End Chg *** 04/07/17 ************************/

       If scPEDtF <> 0;
          outFrEF= %EditC(scPEDtF:'X');
       Else;
          outFrEF= *zeros;
       EndIf;

       IF scPEDtT <> 0;
          outToEF= %EditC(scPEDtT:'X');
       Else;
          outToEF= *zeros;
       EndIf;

       outEF     = outFrEF+ outToEF;
       outIndp   = scIndp;
       outAddu   = %EditC(scAddDay:'X');
       outJrSt   = scJrSt;
       outGrpSlct = 'Y';
       If isPopup = *on;
          outGrpSlct = 'N';
       EndIf;

        LatNotSbm(outCo#:outFnd:outDt:outBlDt:outAmt:outRptF:outCtst:outOutq:
          outMode:outFiDy:outEbDy:outAbDy:outRaDy:outSrt1:outSrt2:outEA:outEN:
          outAA:outAN:outAcst:outEssr:outPayD:outCom:outDrmk:outAust:outARCode:
          outArie:outIefl:outAstc:outTofnd:outDart:outEmp#:outDiv:outEF:outIndp:
          outAddu:outJrSt:outGrpSlct);

      /End-Free

     p LatNotproc      e

       //===================================================================
       // LoadARCodes - Load AR Payment Codes Select Box
       //===================================================================
     p LoadARCodes     b
     d LoadARCodes     pi

      /Free
       scARSel = *Blanks;
       scARVal = *Blanks;
       strARdesc = *Blanks;

       // Read WTTTP file from starting
       Chain(n) intCo# ptt01;
       If %found(wtabl);
          Dow not %Eof(wtabl);
             If TTstat = 'A';
                strARdesc = TitleCase(TTdesc);
                If scARSel = *Blanks;
                   scARSel = %trim(TTkey)+'-'+%trim(strARdesc);
                   scARVal = %trim(TTkey);
                else;
                   scARSel=%trim(scARSel)+','+%trim(TTkey)+'-'+%trim(strARdesc);
                   scARVal = %trim(scARVal)+','+%trim(TTkey);
                endif;
             endif;
          ReadE intCo# ptt01;
          enddo;
       endif;

      /End-Free

     p LoadARCodes     e
       //===================================================================
       // LoadActStscd - Load Activity Status Codes
       //===================================================================
     p LoadActStscd    b
     d LoadActStscd    pi

      /Free
       scAvtsel  = *Blanks;
       scAvtVal  = *Blanks;
       strAvtDsc = *Blanks;

       // Read WTAVP file from starting
       Setll intCo# pav00;
       ReadE intCo# pav00;
       Dow not %Eof(wtavl);
          If AVstat = 'A';
             strAvtDsc = TitleCase(AVdesc);
             If scAvtSel = *Blanks;
                scAvtSel = %trim(AVkey)+'-'+
                           %trim(%xlate(',':' ': strAvtDsc));
                scAvtVal = %trim(AVkey);
             else;
                scAvtSel = %trim(scAvtSel)+','+%trim(AVkey)+'-'+
                           %trim(%xlate(',':' ': strAvtDsc));
                scAvtVal = %trim(scAvtVal)+','+%trim(AVkey);
             endif;
          endif;
       ReadE intCo# pav00;
       enddo;

      /End-Free

     p LoadActStscd    e
       //===================================================================
       // LoadCmCodes - Load comment code multiselect box.
       //===================================================================
     p LoadCmCodes     b
      /Free

       scCCSel = *Blanks;
       scCCVal = *Blanks;
       strCmntDsc = *Blanks;

       // Read WTBCP file from starting
       Setll intCo# pbc01;
       ReadE intCo# pbc01;
       Dow not %Eof(wtabl);
          If BCstat = 'A';
             strCmntDsc = TitleCase(BCdesc);
             scCCSel = %trim(scCCSel)+','+%trim(BCkey)+'-'+
                       %trim(%xlate(',':' ': StrcmntDsc));
             scCCVal = %trim(scCCVal)+','+%trim(BCkey);
          Endif;
       ReadE intCo# pbc01;
       Enddo;

      /End-Free
     p LoadCmCodes     e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi
      /Free
       Clear RcdLtNtPop;
      //**** Beg Chg *** 04/07/17 ************************/
       scIndp = 'N';
       scDart = 'N';
      //**** End Chg *** 04/07/17 ************************/
      /End-Free
     p LoadInVal       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // To load AR Payment code values.
          LoadARCodes();

          // To load Activity Status Codes
          LoadActStsCd();

          // To load Comment code values.
          LoadCmCodes();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
             When BtnSubmit = *on;
                // Clear Errors
                ClrErr();
                // Validate Edits
                If ValidPnl() = *On;
                   LatNotproc();
                   Leave;
                EndIf;
             When BtnCancel = *on;
                Leave;

             // do nothing - catch tab click event
             When tabClick = *on;
                tabClick = *off;
             When BtnRefresh = *On;
                BtnRefresh =*Off;
                // Intialize the Screen fields
                LoadInVal();
                ClrErr();
                // Load Action panel with the Company Name.
                intCo#   = userCo#(q1user);
                scCompName = GetCompNam(intCo#);
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

             If isPopup = *on;
                Exfmt RcdLtNtPop;
             Else;
                Write rcdComPnl;
                Write RcdLtNotR;
                Exfmt RcdLtNtRpt;
                Read rcdComPnl;
                Read RcdLtNotR;
             EndIf;
       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
      //*** begin add ***  04/07/17  ***********************************
       isUseScript = UseScriptura(processType);
       //*** begin chg ***  03/30/21  ***********************************
       //*****isScript = isUseScript;
       visSpool = *off;
       If not isUseScript;
          visSpool = *on;
       Endif;
       //*** end chg *****  03/30/21  ***********************************
      //*** end add *****  04/07/17  ***********************************

       // Intialize the Screen fields
       LoadInVal();

      // Determine Menu call or ToolBar call
       isPopup = *off;
       If Q1prms >= 3;
          isPopup = *on;
          intRrn  = %int(inRrn);
          // Get Employer and Division
          If intRrn <> 0;
             Chain(n) intRrn pel0p;
             scEmpNbr = ELemp#;
             scDivNbr = ELDiv;
             // Get Employer Name
             Chain(n)(ELCo#:ELFnd:ELEmp#:ELDiv) pem00;
             If %Found(Wmeml);
                scEmpName= EMName;
             Endif;
             // Get Group
             Chain(n) (ELco#:ELfnd) pfn00;
             If %Found(Wmfnl);
                scGrp = %EditC(FNfnd:'X') + '-' + FNname;
             Endif;
          Endif;
       Endif;

       // Load Action panel with the Company Name.
       intCo#     = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup00;
       If %Found(Wmupl);
          strOutq = UPoutq;
       EndIf;

       tabClick = *Off;
       Return;
      /End-Free

     p ProgramStart    e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n
      /Free

       Select;
          When isPopup;
             Return ValidPnlPop();
          Other;
             Return ValidPnlNoPop();
       EndSl;

      /End-Free
     p ValidPnl        e
       //===================================================================
       // ValidPnlNoPop - Validate Panel Screen Fields for Menu call
       //===================================================================
     p ValidPnlNoPop   b
     d ValidPnlNoPop   pi              n

      /Free

       // Validation Check for Account Type.
       If scAccTyp = *Blanks;
          erAccTyp = *On;
          msAccTyp = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for Account status.
       If scAccSts  = *Blanks;
          erAccSts  = *On;
          MsAccSts  = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for Audit status.
       If scAudSts  = *Blanks;
          erAudSts  = *On;
          MsAudSts  = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for Installment from Date
       If DtCheck(scInsFrDt) = 0;
          erInsFrDt = *on;
          msInsFrDt = 'WCR0500';
          isValid  = *On;
       Endif;

       // Validation Check for To Date
       If DtCheck(scInsToDt) = 0;
          erInsToDt = *on;
          msInsToDt = 'WCR0500';
          isValid  = *On;
       Endif;

       If erInsToDt = *off and erInsFrDt = *off;
          erInsToDt = AscendDates(scInsFrDt:scInsToDt:charMsg);
          If erInsToDt = *on;
             erInsFrDt = *on;
             msInsToDt = charMsg;
             msInsFrDt = charMsg;
             isValid  = *On;
          Endif;
       Endif;

       // Validation Check for Minimum Amount
       If scMinAmt = *Zeros;
          erMinAmt = *On;
          MsMinAmt = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for Effective To Date.
       If scPEDtT <> 0;
          If DtCheck(scPEDtT) = 0;
             erPEffToDt = *on;
             msPEffToDt = 'WCR0500';
             isValid  = *On;
          Endif;
       Endif;

       // Validation Check for Effective From Date.
       If scPEDtF <> 0;
          If DtCheck(scPEDtF) =0;
             erPEffFrDt = *on;
             msPEffFrDt = 'WCR0500';
             isValid  = *On;
          Endif;
       Endif;

       If erPEffToDt = *off and erPEffFrDt = *off;
          erPEffToDt = AscendDates(scPEDtF:scPEDtT:charMsg);
          If erPEffToDt = *on;
             erPEffFrDt = *on;
             msPEffToDt = charMsg;
             msPEffFrDt = charMsg;
             isValid  = *On;
          Endif;
       Endif;

       // Validation Check Employer Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scEmpNmF <> *Blanks or scEmpNmT <> *Blanks)
        and (scEmpNoF <> *Zeros or scEmpNoT <> *Zeros);
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0918';
          erEmpNmT  = *On;
          msEmpNmT  = 'WCR0918';
          erEmpNoF  = *On;
          msEmpNoF  = 'WCR0918';
          erEmpNoT  = *On;
          msEmpNoT  = 'WCR0918';
          isValid  = *On;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scEmpNmF = *Blanks and scEmpNmT <> *Blanks
        and erEmpNmF  = *Off and erEmpNmT  = *Off;
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0503';
          isValid  = *On;
       EndIf;
       If scEmpNmF <> *Blanks and scEmpNmT = *Blanks
        and erEmpNmF  = *Off and erEmpNmT  = *Off;
          erEmpNmT  = *On;
          msEmpNmT  = 'WCR0503';
          isValid  = *On;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scEmpNmF > scEmpNmT and erEmpNmF  = *Off;
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0750';
          isValid  = *On;
       EndIf;
       If scEmpNoF > scEmpNoT and erEmpNoF  = *Off;
          erEmpNoF  = *On;
          msEmpNoF  = 'WCR0750';
          isValid  = *On;
       EndIf;

       // Validation Check Agent Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scAgtNmF <> *Blanks or scAgtNmT <> *Blanks)
        and (scAgtNoF <> *Zeros or scAgtNoT <> *Zeros);
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0918';
          erAgtNmT  = *On;
          msAgtNmT  = 'WCR0918';
          erAgtNoF  = *On;
          msAgtNoF  = 'WCR0918';
          erAgtNoT  = *On;
          msAgtNoT  = 'WCR0918';
          isValid  = *On;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scAgtNmF = *Blanks and scAgtNmT <> *Blanks
        and erAgtNmF  = *Off and erAgtNmT  = *Off;
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0503';
          isValid  = *On;
       EndIf;
       If scAgtNmF <> *Blanks and scAgtNmT = *Blanks
        and erAgtNmF  = *Off and erAgtNmT  = *Off;
          erAgtNmT  = *On;
          msAgtNmT  = 'WCR0503';
          isValid  = *On;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scAgtNmF > scAgtNmT and erAgtNmF  = *Off;
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0750';
          isValid  = *On;
       EndIf;
       If scAgtNoF > scAgtNoT and erAgtNoF  = *Off;
          erAgtNoF  = *On;
          msAgtNoF  = 'WCR0750';
          isValid  = *On;
       EndIf;

       // Validation Check for Primary Sort.
       // Make it mandatory if running for Multiple Employers.
       If scPriSort = *Blanks;
          erPriSort  = *On;
          MSPriSort  = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for DARTS Remark Code
       If scRmCd <> *Blanks or scDart = 'Y';
          Chain scRmCd PCT01;
          If Not %Found();
             erRmCd  = *On;
             MsRmCd  = 'WCR0827';
             isValid  = *On;
          EndIf;
       EndIf;

       // Validation Check for Comment Codes
       If scCmCodes <> *Blanks;
          inCMcdsTmp = ' ';
          inCMcdsTmp =  %trim(%xlate(',':' ':scCmCodes));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inCMcdsTmp));
             If intpos <> 0;
                inCMcdsTmp = %replace('':inCMcdsTmp:intpos:1);
             EndiF;
          EndDo;
          If %len(%trim(inCMcdsTmp)) > 10;
             erCmCd = *On;
             msCmCd = 'WCR4819';
             isValid  = *On;
          EndIf;
       EndIf;

       // Validation Check for A/R Codes Inclusion/Exclusion.
       If scARcodes = *Blanks and scArCodIE <> ' ';
          erArCodIE = *on;
          msArCodIE = 'WC31098';
          isValid  = *On;
       EndIf;

       If scARcodes <> *Blanks and scArCodIE <> 'I' and scArCodIE <> 'E';
          erArCodIE  = *on;
          msArCodIE  = 'WCR4817';
          isValid  = *On;
       EndIf;

       // Validation Check for AR Codes.
       If scARcodes <> *Blanks;
          inarcdsTmp = ' ';
          inarcdsTmp =  %trim(%xlate(',':' ':scARcodes));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inarcdsTmp));
             If intpos <> 0;
                inarcdsTmp = %replace('':inarcdsTmp:intpos:1);
             EndiF;
          EndDo;
          If %len(%trim(inarcdsTmp)) > 60;
             erARcd  = *On;
             msARcd  = 'WCR4816';
             isValid  = *On;
          EndIf;
       EndIf;

       // Validation Check for Employer Activity Status Codes Inclusion/Exclusion
       If scActStsCd = *Blanks and scEpActSts <> ' ';
          erEpActSts = *on;
          msEpActSts = 'WCR1130';
          isValid  = *On;
       EndIf;

       // Validation Check for Activty Status Codes
       If scActStsCd <> *Blanks;
          If %len(%trim(scActStsCd)) >= 15;
             erASCd = *on;
             msASCd = 'WCR4819';
             isValid  = *On;
          Else;
             dsActSts1 = ParseString(scActStsCd:',':1);
             dsActSts2 = ParseString(scActStsCd:',':2);
             dsActSts3 = ParseString(scActStsCd:',':3);
             dsActSts4 = ParseString(scActStsCd:',':4);
             dsActSts5 = ParseString(scActStsCd:',':5);
             If ParseString(scActStsCd:',':6) <> *Blanks;
                erASCd = *on;
                msASCd = 'WCR4819';
                isValid  = *On;
             EndIf;
          EndIf;
       EndIf;

      //*** begin add ***  04/07/17  ***********************************
       If isValid  = *off;
          xxSubmit = *on;

          If isUseScript;

             xxProcType = processType;

             If isPopup = *On;
                xxMass = ' ';
             Else;
                xxMass = 'Y';
             Endif;

             In *Lock *dtaara;

             Clear svLDA;

             xxDart = 'N';
             xxRMCD = ' ';

             Setll *loval pgs00;
             Read pgs00;

             Dow not %eof(wxgsl);

             If GSieFl = 'I';

                Chain(n) (UPco#:GSfnd) pex00;
                If %found and EXLNRM <> ' ';
                   xxDart = 'Y';
                   xxRMCD = EXLNRM;
                   Leave;
                Endif;

             EndIf;

             Read pgs00;
             Enddo;
          //*** begin add ***  03/30/21  ***********************************
             If scGenRpt = 'Y';
                xxDart = 'N';
                xxRMCD = ' ';
                xxCopy = 'Y';
             Endif;
          //*** end add *****  03/30/21  ***********************************

             Out *dtaara;

             xxSubmit = OutputWindow(xxProcType:xxMass);

             If xxSubmit = *off;
                isValid = *On;
          //*** begin add ***  03/30/21  ***********************************
             Else;

                In *Lock *dtaara;
                Out *dtaara;

                scDart = xxDart;
                scRmcd = xxRMCD;
          //*** end add *****  03/30/21  ***********************************
             EndIf;
          Endif;
       Endif;
      //** End add ***  04/07/17  ***********************************

       If isValid = *Off;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     p ValidPnlNoPop   e
       //===================================================================
       // ValidPnlPop - Validate Panel Screen Fields for Popup Call
       //===================================================================
     p ValidPnlPop     b
     d ValidPnlPop     pi              n

      /Free

       // Validation Check for Installment from Date
       If DtCheck(scInsFrDt) = 0;
          erInsFrDt = *on;
          msInsFrDt = 'WCR0500';
          isValid  = *On;
       Endif;

       // Validation Check for To Date
       If DtCheck(scInsToDt) = 0;
          erInsToDt = *on;
          msInsToDt = 'WCR0500';
          isValid  = *On;
       Endif;

       If erInsToDt = *off and erInsFrDt = *off;
          erInsToDt = AscendDates(scInsFrDt:scInsToDt:charMsg);
          If erInsToDt = *on;
             erInsFrDt = *on;
             msInsToDt = charMsg;
             msInsFrDt = charMsg;
             isValid  = *On;
          Endif;
       Endif;

       // Validation Check for Minimum Amount
       If scMinAmt = *Zeros;
          erMinAmt = *On;
          MsMinAmt = 'WCR0503';
          isValid  = *On;
       EndIf;

       // Validation Check for DARTS Remark Code
       If scRmCd <> *Blanks or scDart = 'Y';
          Chain scRmCd PCT01;
          If Not %Found();
             erRmCd  = *On;
             MsRmCd  = 'WCR0827';
             isValid  = *On;
          EndIf;
       EndIf;

       // Validation Check for Comment Codes
       If scCmCodes <> *Blanks;
          inCMcdsTmp = ' ';
          inCMcdsTmp =  %trim(%xlate(',':' ':scCmCodes));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inCMcdsTmp));
             If intpos <> 0;
                inCMcdsTmp = %replace('':inCMcdsTmp:intpos:1);
             EndiF;
          EndDo;
          If %len(%trim(inCMcdsTmp)) > 10;
             erCmCd = *On;
             msCmCd = 'WCR4819';
             isValid  = *On;
          EndIf;
       EndIf;

      //*** begin add ***  04/07/17  ***********************************
       If isValid  = *off;    // when no error on screen the excute the below code.

         xxSubmit = *on;

         If isUseScript;

            xxProcType = processType;

            If isPopup = *On;
              xxMass = ' ';
            Else;
             xxMass = 'Y';
            Endif;

            In *Lock *dtaara;

            Clear svLDA;

            xxDart = 'N';


             Setll (UPCo#:%dec(%Subst(scGrp:1:3):3:0)) pfn00;
            Reade(n) (UPCo#) pfn00;

            Dow not %eof;
             //If FNFnd > scTFnd and scTFnd <> 0;
             //   Leave;
             //Endif;
               Chain(n) (UPCo#:FNFnd) pex00;
               If %found and EXLNRM <> ' ';
                xxDart = 'Y';
                xxRMCD = EXLNRM;
                Leave;
               Endif;
             Reade(n) (UPCo#) pfn00;
            Enddo;
         //*** begin add ***  03/30/21  ***********************************
            If scGenRpt = 'Y';
               xxDart = 'N';
               xxRMCD = ' ';
               xxCopy = 'Y';
            Endif;
          //*** end add *****  03/30/21  ***********************************

            Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
          //***** xxSubmit = OutputWindow(xxProcType:xxMass);
            If intRRN <> 0;
               xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':intRRN);
            Else;
               xxSubmit = OutputWindow(xxProcType:xxMass);
            Endif;
          //*** end chg *****  08/13/21  ***********************************

            If xxSubmit = *off;
               isValid = *On;
         //*** begin add ***  03/30/21  ***********************************
            Else;

               In *Lock *dtaara;
               Out *dtaara;

               scDart = xxDart;
               scRmcd = xxRMCD;
         //*** end add *****  03/30/21  ***********************************
            EndIf;
         Endif;

       Endif;
      //** End add ***  04/07/17  ***********************************

       If isValid = *Off;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     p ValidPnlPop     e
