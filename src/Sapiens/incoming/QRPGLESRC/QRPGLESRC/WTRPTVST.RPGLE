      * (c) 2021 Sapiens. All Rights Reserved.                             *
     h**********************************************************************
     h/title WTRPTVST - Visit Tracking Report
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Generate Visit Tracking Report
     h*                and/or write to outfile WFVIP.
     h*
     h*  Called from: WTRPTVSTO
     h*  Calls to:    None
     h*
     h**********************************************************************
     h*  Created on 01/25/2015 by C:062 for Project: 114WGU
     h**********************************************************************
     h*  Revisions:
     h*
     h*  06/21/21 C:298 Project: SY426                                     *
     h*                 Re-Brand and Config for Visit Tracking             *
     h*
      *  09/26/19 C:291 Project: 129WVN                                    *
      *                 Print Sequence # on the report.                    *
     h**********************************************************************
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwxgsl     if   e           k disk    rename(pgs01:pgs00)
     fwdelp     if   e             Disk    ReName(pel01:pel0p) usropn
     fwmeml     if   e           k Disk    ReName(pem01:pem00) usropn
     fwmall     if   e           k Disk    ReName(pal01:pal00) usropn
     fwmrpl     if   e           k Disk    ReName(prp01:prp00)
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwmcml1    if   e           k Disk
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     fwmaal     if   e           k Disk    ReName(paa01:paa00)
     fwmagp     if   e             Disk    ReName(pag01:pag0p)
     fwmagl     if   e           k Disk    ReName(pag01:pag00)
     fwtvil     if   e           k Disk    ReName(pvi01:pvi00)
     fwmvil1    if   e           k Disk
     fwmvil2    if   e           k Disk    ReName(pvi01:pvi02)
     fwmvil3    if   e           k Disk    ReName(pvi01:pvi03)
     fwfvip     o    e             Disk    Rename(pvi01:pvi0p)
     fwsvsl     uf   e           k Disk    Rename(pvs01:pvs00)

     Fwtrptvst  o    e             Printer OflInd(*IN01)

     d psds          esds                  extname(szq1p)

     d* Check to see if the report is overflowing
     d ChkOverFlow     pr

     d* Delete Employer Overrides and Close files
     d DelOverrideEmp  pr
     d  quoteType                     1

     d* Write Notes
     d WriteNotes      pr

     d* Formats the detail heading and detail value line
     d FormatDtl       pr
     d headingIn                    100    varying
     d valueIn                      100    varying
     d minLengthIn                    3  0
     d iterationIn                    3  0

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Get Agency Key Information
     d GetAgtKey       pr
     D  rrn                           9  0

     d* Get Agency Information
     d GetAgtInfo      pr
     d  company                       3  0
     d  group                         3  0
     d  fein                          9  0
     d  mod                           3  0

     d* Get Claim Key Information
     d GetClmKey       pr
     D  rrn                           9  0

     d* Get Claim Information
     d GetClmInfo      pr
     d  company                       3  0
     d  group                         3  0
     d  year                          3  0
     d  case                          7  0

     d* Get Employer Key Information
     d GetEmpKey       pr
     D  rrn                           9  0

     d* Get Policy Information
     d GetEmpInfo      pr
     d  company                       3  0
     d  group                         3  0
     d  employer                      9  0
     d  division                      5  0
     d  location                     10

     d* Get Group Information
     d GetGroupInfo    pr
     d  groupIn                       3  0

     d* Get Group Select
     d GetGrpSlct      pr            60
     d  sequence                      3  0 Const

     d* Main Processing
     d Main            pr

     d* No Visits Found
     d NoVisitsFnd     pr

     d* Setup Employer Overrides and Open files
     d OverrideEmp     pr
     d  quoteType                     1

     d* Process Visits
     d ProcessVisits   pr

     d* Program Start
     d ProgramStart    pr

     d QCmdExc         pr                  ExtPgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const

     d* Report Criteria
     d ReportCrit      pr

     d* Report Processing
     d ReportProc      pr

     d* Split Text
     d SplitText       pr
04320d  textIn                      100
04320d  textOut1                     93
04320d  textOut2                     93

     d* Subset Processing
     d Subset          pr              n

     d* Write Detail Record Format
     d WriteDetail     pr

     d* Write Visit Detail Informaion
     d WriteVstDtl     pr

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany
     d/copy wPrUsrPrms
     d/copy wPrEmplr
     d/copy wPrGroup

     D inCompany       s              3
     D inVstType       s             40
     D inVstCat        s             10
     D inVstAssgn      s             10
     D inVstStat       s              1
     D inVstFrDt       s              8
     D inVstToDt       s              8
     D inCmpFrDt       s              8
     D inCmpToDt       s              8
     D inCmpNoDt       s              1
     D inQteFlag       s              1
     D inOutType       s              1
     D inAgtRrn        s              9
     D inClmRrn        s              9
     D inEmpRrn        s              9
     D inLocation      s             10
     D inMember        s             10
     D inKey           s             20

     D intCo#          s                   Like(EMco#)
     D intAgtRrn       s              9  0
     D intClmRrn       s              9  0
     D intEmpRrn       s              9  0
     D intVsFrDt7      s              7  0
     D intVsToDt7      s              7  0
     D intCmFrDt7      s              7  0
     D intCmToDt7      s              7  0
     D intVsFrDt8      s              8  0
     D intVsToDt8      s              8  0
     D intCmFrDt8      s              8  0
     D intCmToDt8      s              8  0
     d clientID        s              3    Inz('000')
     d singleAgt       s               n
     d singleClm       s               n
     d singleEmp       s               n
     d multiProc       s               n
     d visitSelect     s               n
     d strCommand      s            255
     d ##CltID         s              3
     d position        s              3  0
     d lengthLeft      s              3  0
     d printed         s               n   Inz(*Off)
     d groupSelected   s               n   Inz(*Off)
     d arrGroupList    s             60    Dim(999) Inz

     d kkFein          s                   Like(AAfein)
     d kkMod           s                   Like(AAmod)
     d kkAgt#          s                   Like(AAagt#)

     d kkFnd           s                   Like(CMfnd)
     d kkFyr           s                   Like(CMfyr)
     d kkCase          s                   Like(CMcase)

     d kkEmp#          s                   Like(EMemp#)
     d kkDiv           s                   Like(EMdiv)
     d kkElID          s                   Like(ALelID)

     d hldFnd          s              3  0       Inz(0)
     d hldFein         s              9  0
     d hldMod          s              3  0
     d hldFyr          s              3  0
     d hldCase         s              7  0
     d hldEmp#         s              9  0
     d hldDiv          s              5  0
     d hldLocId        s             10

     d address1        s             40
     d address2        s             40
     d city            s             25
     d state           s              2
     d zipcode         s              9  0
     d AdL1            s             40
     d AdL2            s             40
     d AdL3            s             40
     d visitCount      s              9  0 Inz(0)

     D* Visit Tracking Database Fields
     D  visitFieldsDs  ds
     D  VIdtHd1
     D  VIdtHd2
     D  VIdtHd3
     D  VIdtHd4
     D  VIdtHd5
     D  VIdtHd6
     D  VIdtHd7
     D  VIdtHd8
     D  VIdtHd9
     D  VIdtHd10
     D  VIcbHd1
     D  VIcbHd2
     D  VIcbHd3
     D  VIcbHd4
     D  VIcbHd5
     D  VIcbHd6
     D  VIcbHd7
     D  VIcbHd8
     D  VIcbHd9
     D  VIcbHd10
     D  VIinHd1
     D  VIinHd2
     D  VIinHd3
     D  VIinHd4
     D  VIinHd5
     D  VIinHd6
     D  VIinHd7
     D  VIinHd8
     D  VIinHd9
     D  VIinHd10
     D  VIdcHd1
     D  VIdcHd2
     D  VIdcHd3
     D  VIdcHd4
     D  VIdcHd5
     D  VIdcHd6
     D  VIdcHd7
     D  VIdcHd8
     D  VIdcHd9
     D  VIdcHd10
     D  VItfHd1
     D  VItfHd2
     D  VItfHd3
     D  VItfHd4
     D  VItfHd5
     D  VItfHd6
     D  VItfHd7
     D  VItfHd8
     D  VItfHd9
     D  VItfHd10
     D  visitFields                  20    Overlay(visitFieldsDs) Dim(50)

     D* From Date Selection File
     D  vstDteFFds     ds
     D  vsFDt01
     D  vsFDt02
     D  vsFDt03
     D  vsFDt04
     D  vsFDt05
     D  vsFDt06
     D  vsFDt07
     D  vsFDt08
     D  vsFDt09
     D  vsFDt10
     D  vstDteFF                      7  0 Overlay(vstDteFFds) Dim(10)

     D* To Date Selection File
     D  vstDteTFds     ds
     D  vsTDt01
     D  vsTDt02
     D  vsTDt03
     D  vsTDt04
     D  vsTDt05
     D  vsTDt06
     D  vsTDt07
     D  vsTDt08
     D  vsTDt09
     D  vsTDt10
     D  vstDteTF                      7  0 Overlay(vstDteTFds) Dim(10)

     D* No Date Selection File
     D  vstDteNFds     ds
     D  vsNDt01
     D  vsNDt02
     D  vsNDt03
     D  vsNDt04
     D  vsNDt05
     D  vsNDt06
     D  vsNDt07
     D  vsNDt08
     D  vsNDt09
     D  vsNDt10
     D  vstDteNF                      1    Overlay(vstDteNFds) Dim(10)

     D* From Decimal Selection File
     D  vstDecFFds     ds
     D  vsFDe01
     D  vsFDe02
     D  vsFDe03
     D  vsFDe04
     D  vsFDe05
     D  vsFDe06
     D  vsFDe07
     D  vsFDe08
     D  vsFDe09
     D  vsFDe10
     D  vstDecFF                     20  6 Overlay(vstDecFFds) Dim(10)

     D* To Decimal Selection File
     D  vstDecTFds     ds
     D  vsTDe01
     D  vsTDe02
     D  vsTDe03
     D  vsTDe04
     D  vsTDe05
     D  vsTDe06
     D  vsTDe07
     D  vsTDe08
     D  vsTDe09
     D  vsTDe10
     D  vstDecTF                     20  6 Overlay(vstDecTFds) Dim(10)

     D* No Decimal Selection File
     D  vstDecNFds     ds
     D  vsNDe01
     D  vsNDe02
     D  vsNDe03
     D  vsNDe04
     D  vsNDe05
     D  vsNDe06
     D  vsNDe07
     D  vsNDe08
     D  vsNDe09
     D  vsNDe10
     D  vstDecNF                      1    Overlay(vstDecNFds) Dim(10)

     D* From Integer Selection File
     D  vstIntFFds     ds
     D  vsFIn01
     D  vsFIn02
     D  vsFIn03
     D  vsFIn04
     D  vsFIn05
     D  vsFIn06
     D  vsFIn07
     D  vsFIn08
     D  vsFIn09
     D  vsFIn10
     D  vstIntFF                     20  0 Overlay(vstIntFFds) Dim(10)

     D* To Integer Selection File
     D  vstIntTFds     ds
     D  vsTIn01
     D  vsTIn02
     D  vsTIn03
     D  vsTIn04
     D  vsTIn05
     D  vsTIn06
     D  vsTIn07
     D  vsTIn08
     D  vsTIn09
     D  vsTIn10
     D  vstIntTF                     20  0 Overlay(vstIntTFds) Dim(10)

     D* No Integer Selection File
     D  vstIntNFds     ds
     D  vsNIn01
     D  vsNIn02
     D  vsNIn03
     D  vsNIn04
     D  vsNIn05
     D  vsNIn06
     D  vsNIn07
     D  vsNIn08
     D  vsNIn09
     D  vsNIn10
     D  vstIntNF                      1    Overlay(vstIntNFds) Dim(10)

     D* To Check Box Selection File
     D  vstChkFds      ds
     D  vsCb01
     D  vsCb02
     D  vsCb03
     D  vsCb04
     D  vsCb05
     D  vsCb06
     D  vsCb07
     D  vsCb08
     D  vsCb09
     D  vsCb10
     D  vstChkF                       1    Overlay(vstChkFds) Dim(10)

     D* To Check Box No Value
     D  vstChkNFds     ds
     D  vsNCb01
     D  vsNCb02
     D  vsNCb03
     D  vsNCb04
     D  vsNCb05
     D  vsNCb06
     D  vsNCb07
     D  vsNCb08
     D  vsNCb09
     D  vsNCb10
     D  vstChkNF                      1    Overlay(vstChkNFds) Dim(10)

     D* To Text Box Selection File
     D  vstTxtFds      ds
     D  vsTf01
     D  vsTf02
     D  vsTf03
     D  vsTf04
     D  vsTf05
     D  vsTf06
     D  vsTf07
     D  vsTf08
     D  vsTf09
     D  vsTf10
     D  vstTxtF                     100    Overlay(vstTxtFds) Dim(10)

     D* Visit Tracking Dates
     D  vstDateds      ds
     D  viDt01
     D  viDt02
     D  viDt03
     D  viDt04
     D  viDt05
     D  viDt06
     D  viDt07
     D  viDt08
     D  viDt09
     D  viDt10
     D  vstDateA                      7  0 Overlay(vstDateds) Dim(10)

     D* Visit Tracking Decimal Fields
     D  vstDecds       ds
     D  viDec01
     D  viDec02
     D  viDec03
     D  viDec04
     D  viDec05
     D  viDec06
     D  viDec07
     D  viDec08
     D  viDec09
     D  viDec10
     D  vstDec                       19  6 Overlay(vstDecds) Dim(10)

     D* Visit Tracking Integer Fields
     D  vstIntds       ds
     D  viInt01
     D  viInt02
     D  viInt03
     D  viInt04
     D  viInt05
     D  viInt06
     D  viInt07
     D  viInt08
     D  viInt09
     D  viInt10
     D  vstInt                       19  0 Overlay(vstIntds) Dim(10)

     D* Visit Tracking Check Box Fields
     D  vstChkds       ds
     D  viCb01
     D  viCb02
     D  viCb03
     D  viCb04
     D  viCb05
     D  viCb06
     D  viCb07
     D  viCb08
     D  viCb09
     D  viCb10
     D  vstChk                        1    Overlay(vstChkds) Dim(10)

     D* Visit Tracking Text Fields
     D  vstTxtds       ds
     D  viTf01
     D  viTf02
     D  viTf03
     D  viTf04
     D  viTf05
     D  viTf06
     D  viTf07
     D  viTf08
     D  viTf09
     D  viTf10
     D  vstTxt                      100    Overlay(vstTxtds) Dim(10)
    C*
     C     *entry        plist
     C                   parm                    inCompany
     C                   parm                    inVstType
     C                   parm                    inVstCat
     C                   parm                    inVstAssgn
     C                   parm                    inVstStat
     C                   parm                    inVstFrDt
     C                   parm                    inVstToDt
     C                   parm                    inCmpFrDt
     C                   parm                    inCmpToDt
     C                   parm                    inCmpNoDt
     C                   parm                    inQteFlag
     C                   parm                    inOutType
     C                   parm                    inAgtRrn
     C                   parm                    inClmRrn
     C                   parm                    inEmpRrn
     C                   parm                    inLocation
     C                   parm                    inMember
     C                   parm                    inKey

      /free

       // Main Procesing
       Main();

       *inlr = *on;

      /end-free
       //===================================================================
       // ChkOverFlow - Check to see if there is overflow
       //===================================================================
     p ChkOverFlow     b
     d ChkOverFlow     pi
     d

      /free

       If *in01 = *On;
          Write NewPage;
          Write Heading;
          *in01 = *off;
       EndIf;

      /End-Free

     p ChkOverFlow     e
       //===================================================================
       // DelOverrideEmp - Delete Employer Overrdes and Close Files
       //===================================================================
     p DelOverrideEmp  b
     d DelOverrideEmp  pi
     d  quoteType                     1

     d mode            s              1

      /free

       // Close Employer Files
       If %open(WDELP);
          Close WDELP;
       EndIf;

       If %open(WMEML);
          Close WMEML;
       EndIf;

       If %open(WMALL);
          Close WMALL;
       EndIf;

       // Delete Employer File Overrides If Quote Employer
       If mode = 'Q';
          strCommand = 'DLTOVR FILE(WDELP)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'DLTOVR FILE(WMEML)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'DLTOVR FILE(WMALL)';
          Callp QCmdExc(strCommand:%len(strCommand));
       Endif;

      /end-free

     p DelOverrideEmp  e
       //===================================================================
       // FormatDtl - Format The Detail Lines
       //===================================================================
     p FormatDtl       b
     d FormatDtl       pi
     d headingIn                    100    Varying
     d valueIn                      100    Varying
     d minLengthIn                    3  0
     d iterationIn                    3  0
     d

     d heading         s            100    Varying
     d value           s            100    Varying
     d iteration       s              3  0
     d minLength       s              3  0
     d valueIn1        s            100
     d valueOut1       s             93
     d valueOut2       s             93

      /free

       heading    = headingIn;
       value      = valueIn;
       valueIn1   = valueIn;
       minLength  = minLengthIn;
       iteration  = iterationIn;

       Select;
       When detHeading = ' ';
          detHeading = %Trim(heading);
          detValues  = %Trim(value);
          position   = position   + minLength + 2;
          lengthLeft = lengthLeft - minLength - 2;
       When position > 93;
          WriteDetail();
          ExSr PrepareSplit;
          detHeading = %Trim(heading);
          detValues  = %Trim(value);
          position   = position   + minLength + 2;
          lengthLeft = lengthLeft - minLength - 2;
       When minLength <= lengthLeft;
          detHeading = %Subst(detHeading:1:position) + %Trim(heading);
          detValues  = %Subst(detValues:1:position) + %Trim(value);
          position   = position   + minLength + 2;
          lengthLeft = lengthLeft - minLength - 2;
       Other;
          WriteDetail();
          ExSr PrepareSplit;
          detHeading = %Trim(heading);
          detValues  = %Trim(value);
          position   = position   + minLength + 2;
          lengthLeft = lengthLeft - minLength - 2;
       EndSl;
       //==================================================================
       //  Check to see if text string is too ong and prepare second field
       //==================================================================
       Begsr PrepareSplit;

          If minLength > 93;
             SplitText(valueIn1:valueOut1:valueOut2);
             value = valueOut1;
             detValues2 = valueOut2;
          EndIf;

       Endsr;

      /end-free

     p FormatDtl       e
       //===================================================================
       // GetAgtInfo - Get Agency Information
       //===================================================================
     p GetAgtInfo      b
     d GetAgtInfo      pi
     d  company                       3  0
     d  group                         3  0
     d  fein                          9  0
     d  mod                           3  0

      /free

       // Get Agency Heading Information
       Chain (company:fein:mod) pag00;
       If %Found(WMAGL);
          agency# = %EditC(VIagt#:'4');
          agtName = AGname;

          address1 = AGmad1;
          address2 = ' ';
          city     = AGmCty;
          state    = AGmSt;
          zipCode  = AGmZip;
          FormatAddress(address1:address2:city:state:zipCode:AdL1:AdL2:AdL3);
          agtAdr1    = ADL1;
          agtCSZ     = ADL2;

       EndIf;

      /end-free

     p GetAgtInfo      e
       //===================================================================
       // GetAgtKey - Get Agency Key Information
       //===================================================================
     p GetAgtKey       b
     d GetAgtKey       pi
     d  agtRrn                        9  0

      /free

       Chain agtRrn pag0p;
       If %Found(WMAGP);
          kkFein = AGfein;
          kkMod  = AGmod;
          selSingleH = 'Agency:';
          selSingleV = %EditC(AGFein:'X') + ' ' + %EditC(AGmod:'X');
          Chain (AGco#:AGfein:AGmod) paa00;
          If %Found(WMAAL);
             kkAgt#=AAagt#;
          EndIf;
       EndIf;

      /end-free

     p GetAgtKey       e
       //===================================================================
       // GetClmInfo - get Claim Information
       //===================================================================
     p GetClmInfo      b
     d GetClmInfo      pi
     d  company                       3  0
     d  group                         3  0
     d  year                          3  0
     d  case                          7  0

      /free

       // Get Claims Heading Information
       Chain (company:group:year:case) pcm01;
       If %Found(WMCML1);
          claim# = %EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') + '-' +
                   %EditC(CMcase:'X');
          clmName = %Trim(CMfsN);
          If CMmid <> ' ';
             clmName = %Trim(clmName) + ' ' + CMmid + '.';
          EndIf;
          clmName = %Trim(clmName) + ' ' + CMlsN;
          Chain (CMss#) pcd00;
          If %Found(WMCDL);
             address1 = CDadr1;
             address2 = CDadr2;
             city     = CDcty;
             state    = CDSt;
             zipCode  = CDzip;
             FormatAddress(address1:address2:city:state:zipCode:AdL1:AdL2:AdL3);
             clmAdr1    = ADL1;
             clmAdr2    = ADL2;
             clmAdr3    = ADL3;
          EndIf;
       EndIf;

      /end-free

     p GetClmInfo      e
       //===================================================================
       // GetClmKey - get Claim Key Info
       //===================================================================
     p GetClmKey       b
     d GetClmKey       pi
     d  clmRrn                        9  0

      /free

       Chain clmRrn pcm0p;
       If %Found(WMCMP);
          selSingleH = 'Claim:';
          selSingleV = %EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') + '-' +
                        %EditC(CMcase:'X');
          kkFnd=CMfnd;
          kkFyr=CMfYr;
          kkCase=CMcase;
       EndIf;

      /end-free

     p GetClmKey       e
       //===================================================================
       // GetEmpInfo - Get Employer Information
       //===================================================================
     p GetEmpInfo      b
     d GetEmpInfo      pi
     d  company                       3  0
     d  group                         3  0
     d  employer                      9  0
     d  division                      5  0
     d  location                     10

      /free

       // Override and Open Policy files
       OverrideEmp(inQteFlag);

       // Get Policy Header Information
       Chain (company:group:employer:division) pem00;
       If %Found(WMEML);
          employer# = %EditC(EMemp#:'4');
          division# = %EditC(EMdiv:'4');
          location  = VIlocID;
          Chain (emCo#:emFnd:emEmp#:EMdiv) prp00;
          If %Found(WMRPL);
             policy# = RPcap#;
          EndIf;
          empName   = EMname;
       EndIf;

       If location <> '0000000000' Or location <> '          ';
          Chain (company:group:employer:division:location) pal00;
          If %Found(WMALL);
             locName = ALname;
          EndIf;
       EndIf;

       // Delete Overrides
       DelOverrideEmp(inQteFlag);

      /end-free

     p GetEmpInfo      e
       //===================================================================
       // GetEmpKey - Get Employer Key Information
       //===================================================================
     p GetEmpKey       b
     d GetEmpKey       pi
     d  empRrn                        9  0

      /free

        OverrideEmp(inQteFlag);

        Chain empRrn pel0p;
        If %Found(WDELP);
          selSingleH = 'Policy:';
          selSingleV = %EditC(ELfnd:'X') + '-' + %EditC(ELemp#:'X') + '-' +
                        %EditC(ELdiv:'X');
           kkFnd=ELfnd;
           kkEmp#=ELemp#;
           kkDiv=ELdiv;
           kkElId=inLocation;
        EndIf;

        DelOverrideEmp(inQteFlag);

      /end-free

     p GetEmpKey       e
       //===================================================================
       // GetGroupInfo - Get group Information
       //===================================================================
     p GetGroupInfo    b
     d GetGroupInfo    pi
     d  groupIn                       3  0

      /free

       group# = groupIn;
       groupNm = GetGrpName(groupIn);

      /end-free

     p GetGroupInfo    e
       //===================================================================
       // GetGrpSlct - Get Group Select Listing
       //===================================================================
     p GetGrpSlct      b
     d GetGrpSlct      pi            60
     d  sequence                      3  0 Const

     d  x              s              3  0 Inz(1)

      /free

       If sequence = 1;
          Clear arrGroupList;
          Chain (intCo#) pgs00;
          If %Found(WXGSL);
             DoU %Eof(WXGSL);
                If GSieFl = 'I';
                   If arrGroupList(x) = ' ';
                      arrGroupList(x) = %EditC(GSfnd:'X');
                   Else;
                      If %Len(%Trim(arrGroupList(x))) < 56;
                         arrGroupList(x) = %Trim(arrGroupList(x)) + ', ' +
                                     %EditC(GSfnd:'X');
                      Else;
                         x = x + 1;
                         arrGroupList(x) = %EditC(GSfnd:'X');
                      EndIf;
                   EndIf;
                EndIf;
                ReadE (intCo#) pgs00;
             EndDo;
          EndIf;
       EndIf;

       Return arrGroupList(sequence);

      /end-free

     p GetGrpSlct      e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)
     D company         s              3
     D vstFrDt         s              8
     D vstToDt         s              8
     D cmpFrDt         s              8
     D cmpToDt         s              8
     D agtRrn          s              9
     D clmRrn          s              9
     D empRrn          s              9

      /free

       // Start Of Program
       ProgramStart();

       // Process until exit button is pressed.
       Chain (intCo#:inVstCat:inVstType) pvi00;
       If %Found(WTVIL);
          // Only read thru group select if group select was used
          If inMember <> *blanks;
             // Loop thru selected groups
             Chain (intCo#) pgs00;
             If %Found(WXGSL);
                DoU %Eof(WXGSL);
                   If GSieFl = 'I';
                      ProcessVisits();
                   EndIf;
                   ReadE (intCo#) pgs00;
                EndDo;
             EndIf;
          Else;
             ProcessVisits();
          EndIf;
       EndIf;

       // Print nothing found if nothing found
       If (inOutType = 'P' Or inOutType = 'B') And visitCount = 0;
          NoVisitsFnd();
       EndIf;

       // Print summary when done
       If (inOutType = 'P' Or inOutType = 'B');
          ReportCrit();
       EndIf;

       // Delete the selection criteria record
       Delete (intCo#:q1user:inKey) pvs00;

      /end-free
     p Main            e
       //===================================================================
       // NoVisitsFnd - No Visits Found
       //===================================================================
     p NoVisitsFnd     b
     d NoVisitsFnd     pi

      /free

       Write Heading;
       Write NoVisits;
       Write NewPage;

      /end-free

     p NoVisitsFnd     e
       //===================================================================
       // OverrideEmp - Override Employer Files and Open Files
       //===================================================================
     p OverrideEmp     b
     d OverrideEmp     pi
     d  quoteType                     1

     d mode            s              1

      /free

       // Override Employer Files  If Quote Employer
       If mode = 'Q';
          strCommand = 'OVRDBF FILE(WDELP) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELP)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'OVRDBF FILE(WMEML) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEML)';
          Callp QCmdExc(strCommand:%len(strCommand));

          strCommand = 'OVRDBF FILE(WMALL) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMALL)';
          Callp QCmdExc(strCommand:%len(strCommand));
       Endif;

       // Open Employer Files
       If not %open(WDELP);
          Open WDELP;
       EndIf;

       If not %Open(WMEML);
          Open WMEML;
       EndIf;

       If not %Open(WMALL);
          Open WMALL;
       EndIf;

      /end-free

     p OverrideEmp     e
       //===================================================================
       // ProcessVisits - Process Vists
       //===================================================================
     p ProcessVisits   b
     d ProcessVisits   pi

     d eof             s               n
     d found           s               n

      /free

       // Start Reading Visit Tracking File
       Select;
       When singleAgt = *On;
          Chain (intCo#:inVstCat:inVstType:kkFein:kkMod:kkAgt#) pvi01;
          found = %Found(WMVIL1);
       When singleClm = *On;
          Chain (intCo#:inVstCat:inVstType:kkFnd:kkFyr:kkCase) pvi02;
          found = %Found(WMVIL2);
       When singleEmp = *On;
          If inLocation <> ' ';
             Chain (intCo#:inVstCat:inVstType:
                    kkFnd:kkEmp#:kkDiv:inLocation) pvi03;
          Else;
             Chain (intCo#:inVstCat:inVstType:kkFnd:kkEmp#:kkDiv) pvi03;
          EndIf;
          found = %Found(WMVIL3);
       When multiProc = *On;
          Select;
          When VIcat = 'Agency';
             Chain (intCo#:inVstCat:inVstType) pvi01;
             found = %Found(WMVIL1);
          When VIcat = 'Claims';
             Chain (intCo#:inVstCat:inVstType:gsFnd) pvi02;
             found = %Found(WMVIL2);
          When VIcat = 'Policy';
             Chain (intCo#:inVstCat:inVstType:gsFnd) pvi03;
             found = %Found(WMVIL3);
          EndSl;
       EndSl;

       If found = *On;
          DoU eof = *On;
             // Only select items that meet selection criteria
             If Subset() = *On;

                // Get Group Information
                GetGroupInfo(VIfnd);

                // Print to Report
                If inOutType = 'P' Or inOutType = 'B';
                   ReportProc();
                EndIf;

                // Write to Outfile
                If inOutType = 'F' Or inOutType = 'B';
                   VIvsDt8 = dtMDYY(VIvsDt);
                   VIcmDt8 = dtMDYY(VIcmDt);
                   VIdt01E = dtMDYY(VIdt01);
                   VIdt02E = dtMDYY(VIdt02);
                   VIdt03E = dtMDYY(VIdt03);
                   VIdt04E = dtMDYY(VIdt04);
                   VIdt05E = dtMDYY(VIdt05);
                   VIdt06E = dtMDYY(VIdt06);
                   VIdt07E = dtMDYY(VIdt07);
                   VIdt08E = dtMDYY(VIdt08);
                   VIdt09E = dtMDYY(VIdt09);
                   VIdt10E = dtMDYY(VIdt10);
                   VIiUsr = q1User;
                   VIiPgm = q1Pgm;
                   VIiDt  = DateTo7(systemDate());
                   VIiTim = TimeTo6(systemTime());
                   Write pvi0p;
                EndIf;
             EndIf;

             // Read Next Visit Tracking Record
             Select;
             When singleAgt = *On;
                ReadE (intCo#:inVstCat:inVstType:AGfein:
                       AGmod:AAagt#) pvi01;
                eof = %Eof(WMVIL1);
             When singleClm = *On;
                ReadE (intCo#:inVstCat:inVstType:CMfnd:CMfyr:CMcase) pvi02;
                eof = %Eof(WMVIL2);
             When singleEmp = *On;
                If inLocation <> ' ';
                   ReadE (intCo#:inVstCat:inVstType:
                          kkFnd:kkEmp#:kkDiv:inLocation) pvi03;
                Else;
                   ReadE (intCo#:inVstCat:inVstType:kkFnd:kkEmp#:kkDiv) pvi03;
                EndIf;
                eof = %Eof(WMVIL3);
             When multiProc = *On;
                Select;
                When VIcat = 'Agency';
                   ReadE (intCo#:inVstCat:inVstType) pvi01;
                   eof = %Eof(WMVIL1);
                When VIcat = 'Claims';
                   ReadE (intCo#:inVstCat:inVstType:GSfnd) pvi02;
                   eof = %Eof(WMVIL2);
                When VIcat = 'Policy';
                   ReadE (intCo#:inVstCat:inVstType:GSfnd) pvi03;
                   eof = %Eof(WMVIL3);
                EndSl;
             EndSl;
          EndDo;
       EndIf;

      /end-free
     p ProcessVisits   e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Load Action panel with the Company Name.
       intCo# = %Dec(inCompany:3:0);
       // Get company name
       // scActPnHdg = GetCompNam(intCo#);

       // Assign RRNs
       intAgtRrn = %Dec(inAgtRrn:9:0);
       intClmRrn = %Dec(inClmRrn:9:0);
       intEmpRrn = %Dec(inEmpRrn:9:0);

       //Assign Dates
       intVsFrDt8 = %Dec(inVstFrDt:8:0);
       intVsToDt8 = %Dec(inVstToDt:8:0);
       intCmFrDt8 = %Dec(inCmpFrDt:8:0);
       intCmToDt8 = %Dec(inCmpToDt:8:0);
       intVsFrDt7 = dtCYMD(intVsFrDt8);
       intVsToDt7 = dtCYMD(intVsToDt8);
       intCmFrDt7 = dtCYMD(intCmFrDt8);
       intCmToDt7 = dtCYMD(intCmToDt8);

       // Get the client id
       GetClientID(##CltId);

       // Determine if Running for specific claim/policy/agency
       singleAgt = *Off;
       singleClm = *Off;
       singleEmp = *Off;
       multiProc = *Off;

       // Check to see if this is a single item or multi
       Select;
       When intAgtRrn <> 0;
          singleAgt = *On;
          GetAgtKey(intAgtRrn);
          groupSelected = *Off;
       When intClmRrn <> 0;
          singleClm = *On;
          GetClmKey(intClmRrn);
          groupSelected = *On;
       When intEmpRrn <> 0;
          singleEmp = *On;
          GetEmpKey(intEmpRrn);
          groupSelected = *On;
       Other;
          multiProc = *On;
          If inVstCat <> 'Agency';
             groupSelected = *On;
          EndIf;
       EndSl;

       // Indicate if group selection in play
       *in23 = groupSelected;

       // Populate Report fields
       vstType = inVstType;

       Select;
       When inVstStat = 'A';
          vstStatS = 'Active';
       When inVstStat = 'I';
          vstStatS = 'Inactive';
       When inVstStat = 'C';
          vstStatS = 'Completed';
       Other;
          vstStatS = '*All';
       EndSl;

       If inVstAssgn <> ' ';
          vstAssignS =  GetUsrName(inVstAssgn);
       Else;
          vstAssignS = '*All';
       EndIf;

       Select;
       When %Dec(inVstFrDt:8:0) <> 0;
          vstVstDate = AlphaUSA(CvtToDate(%Dec(inVstFrDt:8:0))) + '-' +
                               AlphaUSA(CvtToDate(%Dec(inVstToDt:8:0)));
       Other;
          vstVstDate = '*All';
       EndSl;

       Select;
       When %Dec(inCmpFrDt:8:0) <> 0;
          vstCmpDate = AlphaUSA(CvtToDate(%Dec(inCmpFrDt:8:0))) + '-' +
                               AlphaUSA(CvtToDate(%Dec(inCmpToDt:8:0)));
       When inCmpNoDt = 'Y';
          vstCmpDate = 'No Date';
       Other;
          vstCmpDate = '*All';
       EndSl;

       Select;
       When inOutType = 'F';
          outType = 'File';
       When inOutType = 'P';
          outType = 'Report';
       When inOutType = 'B';
          outType = 'File and Report';
       EndSl;
       vstType  = inVstType;
       vstTypeH = CenterFld(%Trim(inVstType):40);

       // Get Additional Selection Criteria
       Chain(n) (intCo#:q1user:inKey) pvs00;
       If %Found(wsvsl);
          visitSelect = *On;
       Else;
          visitSelect = *Off;
       EndIf;

      /end-free
     p ProgramStart    e
       //===================================================================
       // ReportCrit - Report Criteria
       //===================================================================
     p ReportCrit      b
     d ReportCrit      pi

     d i               s              3  0

      /free

       Write RptCrit1;
       If *in23 = *on;
          Select;
          When singleClm = *On;
             grpList = %EditC(CMfnd:'X');
             Write RptCrit2;
          When singleEmp = *On;
             grpList = %EditC(ELfnd:'X');
             Write RptCrit2;
          Other;
             grpList = ' ';
             grpList = GetGrpSlct(1);
             Write RptCrit2;
             For i=1 to 998;
                grpList = ' ';
                grpList = GetGrpSlct(i+1);
                If grpList <> ' ';
                   Write RptCrit3;
                Else;
                   Leave;
                EndIf;
             EndFor;
          EndSl;
       EndIf;
       Write RptCrit4;

       If selSingleH <> ' ';
          Write RptCrit6;
       EndIf;

       // Load Additional Selection Criteria
       For i=1 To 50;
           selHeading = ' ';
           selValue   = ' ';
           If visitFields(i) <> ' ';
              selHeading = visitFields(i);
              Select;
              When i >= 1 and i<=10;
                 Select;
                 When vstDteFF(i) <> 0;
                    selValue = AlphaUSA(CvtToDate(vstDteFF(i))) + '-' +
                               AlphaUSA(CvtToDate(vstDteTF(i)));
                 When vstDteNF(i) = 'Y';
                    selValue = 'No Date';
                 EndSl;
              When i >= 11 and i<=20;
                 Select;
                 When vstChkF(i-10) = 'Y';
                    selValue = 'Yes';
                 When vstChkNF(i-10) = 'Y';
                    selValue = 'No';
                 EndSl;
              When i >= 21 and i<=30;
                 Select;
                 When vstIntTF(i-20) <> 0;
                    selValue = %EditC(vstIntFF(i-20):'4') + '-' +
                               %EditC(vstIntTF(i-20):'4');
                 When vstIntNF(i-20) = 'Y';
                    selValue = 'No Value';
                 EndSl;
              When i >= 31 and i<=40;
                 Select;
                 When vstDecTF(i-30) <> 0;
                    selValue = %EditC(vstDecFF(i-30):'4') + '-' +
                               %EditC(vstDecTF(i-30):'4');
                 When vstDecNF(i-30) = 'Y';
                    selValue = 'No Value';
                 EndSl;
              When i >= 41 and i<=50;
                 If vstTxtF(i-40) <> ' ';
                    selValue = vstTxtF(i-40);
                 EndIf;
              EndSl;

              If selValue<>' ';
                 Write RptCrit5;
              EndIf;
           EndIf;
       EndFor;

      /end-free

     p ReportCrit      e
       //===================================================================
       // ReportProc - Report Processing
       //===================================================================
     p ReportProc      b
     d ReportProc      pi

      /free

       // Print Heading
       Write Heading;
       printed = *On;
       visitCount = visitCount + 1;

       // Populate Visit Tracking Header Fields
       vstDate = alphaUSA(cvtToDate(VIvsDt));

       If VIcmDt <> 0;
          cmpDate = alphaUSA(cvtToDate(VIcmDt));
       Else;
          cmpDate = ' ';
       EndIf;

       Select;
       When VIstat = 'A';
          vstStat = 'Active';
       When VIstat = 'I';
          vstStat = 'Inactive';
       When VIstat = 'C';
          vstStat = 'Completed';
       When VIstat = ' ';
          vstStat = 'All';
       EndSl;

       // Populate Visit Assignment
       vstAssign =  GetUsrName(VIAssign);

       //*** Beg Add ***** 09/26/19 *****************
       // Populate Visit Sequence #
       vstSeq# =  %EditC(VIseq#:'4');
       //*** End Add ***** 09/26/19 *****************

       // Get Heading Information
       Select;
       When inVstCat = 'Agency';

          // Print Box
          Write AgencyBox;

          If hldFein <> VIfein Or hldMod <> VImod;
             GetAgtInfo(VIco#:VIfnd:VIfein:VImod);
             hldFein = VIfein;
             hldMod = VImod;
          EndIf;
          // Print the Agency Heading
          Write agtVstHead;
       When inVstCat = 'Claims';
          Write  grpHeading;

          // Print Box
          Write ClaimsBox;

          If hldFnd <> VIfnd Or hldFyr <> VIfyr Or hldCase <> VIcase;
             GetClmInfo(VIco#:VIfnd:VIfyr:VIcase);
             hldFnd = VIfnd;
             hldFyr = VIfyr;
             hldcase = VIcase;
          EndIf;
          // Print the Claims Heading
          Write clmVstHead;
       When inVstCat = 'Policy';
          Write  grpHeading;

          // Print Box
          Write PolicyBox;

          If hldFnd <> VIfnd Or hldEmp# <> VIemp# Or hldDiv <> VIdiv Or
             hldLocID <> VIlocID;
             GetEmpInfo(VIco#:VIfnd:VIemp#:VIdiv:VIlocID);
             hldFnd = VIfnd;
             hldEmp# = VIemp#;
             hldDiv = VIdiv;
             hldLocID = VIlocID;
          EndIf;
          // Print the Policy Heading
          Write polVstHead;
       EndSl;

       // Populate Notes
       WriteNotes();

       // Write Detail Information
       WriteVstDtl();

      /end-free

     p ReportProc      e
       //===================================================================
       // SlitText - Split text
       //===================================================================
     p SplitText       b
     d SplitText       pi
     d  textIn                      100
     d  textOut1                     93
     d  textOut2                     93

     d text            s             93    Varying
     d exit            s               n
     d endPos          s              5  0
     d endPosHold      s              5  0
     d strPos          s              5  0 Inz(1)
     d i               s              5  0
     d length          s              5  0

      /free

       For i=1 To 2;
          Exit = *Off;
          text = %Subst(textIn:1:93);
          length = %Len(%Trim(text));

          DoU Exit = *On;
             endPos = %Scan(' ':%Trim(text):strPos);
             Select;
             When endPos = 0 And strPos = 1;
                exit = *On;
                text = %Subst(textIn:strPos:93);
                strPos = 94;
                textIn = %Subst(textIn:strPos);
                strPos = 1;
             When (endPos = 0 And strPos > 1) Or strPos > 93;
                exit = *On;
                If length = 93;
                   text = %Subst(textIn:1:endPoshold);
                Else;
                   strPos = length + 1;
                EndIf;
                   textIn = %Subst(textIn:strPos);
                strPos = 1;
             Other;
                strPos = endPos + 1;
                endPosHold = endPos - 1;
             EndSl;
          EndDo;

          Select;
          When i = 1;
             textOut1 = text;
          When i = 2;
             textOut2 = text;
          EndSl;

       EndFor;

      /end-free

     p SplitText       e
       //===================================================================
       // Subset - Subset Processing
       //===================================================================
     p Subset          b
     d Subset          pi              n

     d fromDt7         s              7  0
     d toDate7         s              7  0
     d fromDt8         s              8  0
     d toDate8         s              8  0
     d i               s              3  0

      /free

       // Assignment must match if entered
       If inVstAssgn <> ' ' And inVstAssgn <> VIassign;
          Return *Off;
       EndIf;

       // Status must match if entered
       If inVstStat <> ' ' And inVstStat <> VIstat;
          Return *Off;
       EndIf;

       // Visit date must fall within date range
       fromDt8 = %Dec(inVstFrDt:8:0);
       toDate8 = %Dec(inVstToDt:8:0);
       If (fromDt8 <> 0 And toDate8 <> 0);
          fromDt7 = dtCYMD(fromDt8);
          toDate7 = dtCYMD(toDate8);
          If VIvsDt < fromDt7 Or VIvsDt > toDate7;
             Return *Off;
          EndIf;
       EndIf;

       // Completion date must fall within date range
       fromDt8 = %Dec(inCmpFrDt:8:0);
       toDate8 = %Dec(inCmpToDt:8:0);
       If (fromDt8 <> 0 And toDate8 <> 0);
          fromDt7 = dtCYMD(fromDt8);
          toDate7 = dtCYMD(toDate8);
          If VIcmDt < fromDt7 Or VIcmDt > toDate7;
             Return *Off;
          EndIf;
       EndIf;

       // If no date is checked then exclude items with a completion date
       If inCmpNoDt = 'Y'And VIcmDt <> 0;
          Return *Off;
       EndIf;

       If visitSelect = *On;
          For i=1 To 10;
             // Dates
             If vstDteNF(i) = 'Y';
               If vstDateA(i) <> 0;
                  Return *Off;
               EndIf;
             Else;
               If VstDteFF(i) <> 0;
                  If vstDateA(i) < vstDteFF(i) Or vstDateA(i) > vstDteTF(i);
                     Return *Off;
                  EndIf;
               EndIf;
             EndIf;

             // Decimal
             If vstDecNF(i) = 'Y';
               If vstDec(i) <> 0;
                  Return *Off;
               EndIf;
             Else;
               If VstDecFF(i) <> 0;
                  If vstDec(i) < vstDecFF(i) Or vstDec(i) > vstDecTF(i);
                     Return *Off;
                  EndIf;
               EndIf;
             EndIf;

             // Integers
             If vstIntNF(i) = 'Y';
               If vstInt(i) <> 0;
                  Return *Off;
               EndIf;
             Else;
               If VstIntFF(i) <> 0;
                  If vstInt(i) < vstIntFF(i) Or vstInt(i) > vstIntTF(i);
                     Return *Off;
                  EndIf;
               EndIf;
             EndIf;

             // Text
             If VstTxtF(i) <> ' ';
                If StringFound(vstTxt(i):vstTxtF(i):*On) = *Off;
                   Return *Off;
                EndIf;
             EndIf;

             // Check Box
             If vstChkNF(i) = 'Y' And VstChk(i) <> ' ';
                Return *Off;
             EndIf;
             If vstChkNF(i) <> 'Y' And VstChkF(i) = 'Y' And VstChk(i) = ' ';
                Return *Off;
             EndIf;

          EndFor;
       EndIf;

       Return *On;

      /end-free

     p Subset          e
       //===================================================================
       // WriteDetail - Writes the Built Header and Value Lines
       //===================================================================
     p WriteDetail     b
     d WriteDetail     pi
     d

      /free

       ChkOverFlow();
       Write detail;
       If detValues2 <> ' ';
          Write detail2;
       EndIf;

       Clear detHeading;
       Clear detValues;
       Clear detValues2;
       position = 0;
       lengthLeft = 93;

      /End-Free

     p WriteDetail     e
       //===================================================================
       // WriteNotes - Breakout and Write Notes
       //===================================================================
     p WriteNotes      b
     d WriteNotes      pi

     d note            s             93    Varying
     d exit            s               n
     d endPos          s              5  0
     d endPosHold      s              5  0
     d strPos          s              5  0 Inz(1)
     d i               s              5  0
     d length          s              5  0

      /free

       For i=1 To 3;
          Exit = *Off;
          note = %Subst(VInotes:1:93);
          length = %Len(%Trim(note));

          DoU Exit = *On;
             endPos = %Scan(' ':%Trim(note):strPos);
             Select;
             When endPos = 0 And strPos = 1;
                exit = *On;
                note = %Subst(VInotes:strPos:93);
                strPos = 94;
                VInotes = %Subst(VInotes:strPos);
                strPos = 1;
             When (endPos = 0 And strPos > 1) Or strPos > 93;
                exit = *On;
                note = %Subst(VInotes:1:endPoshold);
                VInotes = %Subst(VInotes:strPos);
                strPos = 1;
             Other;
                strPos = endPos + 1;
                endPosHold = endPos - 1;
             EndSl;
          EndDo;

          Select;
          When i = 1;
             notes1 = note;
          When i = 2;
             notes2 = note;
          When i = 3;
             notes3 = note;
          EndSl;

       EndFor;

       *In21 = *Off;
       *In22 = *Off;

       If Notes2 <> ' ';
          *In21 =*On;
       EndIf;

       If Notes3 <> ' ';
          *In22 =*On;
       EndIf;

       Write notes;

      /end-free

     p WriteNotes      e
       //===================================================================
       // WriteVstDtl - Write Visit Detail
       //===================================================================
     p WriteVstDtl     b
     d WriteVstDtl     pi

     d lnDtHd1         s              3  0
     d lnDtHd2         s              3  0
     d lnDtHd3         s              3  0
     d lnDtHd4         s              3  0
     d lnDtHd5         s              3  0
     d lnDtHd6         s              3  0
     d lnDtHd7         s              3  0
     d lnDtHd8         s              3  0
     d lnDtHd9         s              3  0
     d lnDtHd10        s              3  0

     d lnCbHd1         s              3  0
     d lnCbHd2         s              3  0
     d lnCbHd3         s              3  0
     d lnCbHd4         s              3  0
     d lnCbHd5         s              3  0
     d lnCbHd6         s              3  0
     d lnCbHd7         s              3  0
     d lnCbHd8         s              3  0
     d lnCbHd9         s              3  0
     d lnCbHd10        s              3  0

     d lnCb01          s              3  0
     d lnCb02          s              3  0
     d lnCb03          s              3  0
     d lnCb04          s              3  0
     d lnCb05          s              3  0
     d lnCb06          s              3  0
     d lnCb07          s              3  0
     d lnCb08          s              3  0
     d lnCb09          s              3  0
     d lnCb10          s              3  0

     d lnInHd1         s              3  0
     d lnInHd2         s              3  0
     d lnInHd3         s              3  0
     d lnInHd4         s              3  0
     d lnInHd5         s              3  0
     d lnInHd6         s              3  0
     d lnInHd7         s              3  0
     d lnInHd8         s              3  0
     d lnInHd9         s              3  0
     d lnInHd10        s              3  0

     d lnInt01         s              3  0
     d lnInt02         s              3  0
     d lnInt03         s              3  0
     d lnInt04         s              3  0
     d lnInt05         s              3  0
     d lnInt06         s              3  0
     d lnInt07         s              3  0
     d lnInt08         s              3  0
     d lnInt09         s              3  0
     d lnInt10         s              3  0

     d lnDcHd1         s              3  0
     d lnDcHd2         s              3  0
     d lnDcHd3         s              3  0
     d lnDcHd4         s              3  0
     d lnDcHd5         s              3  0
     d lnDcHd6         s              3  0
     d lnDcHd7         s              3  0
     d lnDcHd8         s              3  0
     d lnDcHd9         s              3  0
     d lnDcHd10        s              3  0

     d lnDec01         s              3  0
     d lnDec02         s              3  0
     d lnDec03         s              3  0
     d lnDec04         s              3  0
     d lnDec05         s              3  0
     d lnDec06         s              3  0
     d lnDec07         s              3  0
     d lnDec08         s              3  0
     d lnDec09         s              3  0
     d lnDec10         s              3  0

     d lnTfHd1         s              3  0
     d lnTfHd2         s              3  0
     d lnTfHd3         s              3  0
     d lnTfHd4         s              3  0
     d lnTfHd5         s              3  0
     d lnTfHd6         s              3  0
     d lnTfHd7         s              3  0
     d lnTfHd8         s              3  0
     d lnTfHd9         s              3  0
     d lnTfHd10        s              3  0

     d lnTf01          s              3  0
     d lnTf02          s              3  0
     d lnTf03          s              3  0
     d lnTf04          s              3  0
     d lnTf05          s              3  0
     d lnTf06          s              3  0
     d lnTf07          s              3  0
     d lnTf08          s              3  0
     d lnTf09          s              3  0
     d lnTf10          s              3  0

     D i               s              3  0
     D headingIn       s            100    Varying
     D valueIn         s            100    Varying
     D minLengthIn     s              3  0

      /free

       // Get Lengths of Values
       lnDtHd1 = %Len(%Trim(VIdtHd1));
       lnDtHd2 = %Len(%Trim(VIdtHd2));
       lnDtHd3 = %Len(%Trim(VIdtHd3));
       lnDtHd4 = %Len(%Trim(VIdtHd4));
       lnDtHd5 = %Len(%Trim(VIdtHd5));
       lnDtHd6 = %Len(%Trim(VIdtHd6));
       lnDtHd7 = %Len(%Trim(VIdtHd7));
       lnDtHd8 = %Len(%Trim(VIdtHd8));
       lnDtHd9 = %Len(%Trim(VIdtHd9));
       lnDtHd10 = %Len(%Trim(VIdtHd10));

       lnCbHd1 = %Len(%Trim(VIcbHd1));
       lnCbHd2 = %Len(%Trim(VIcbHd2));
       lnCbHd3 = %Len(%Trim(VIcbHd3));
       lnCbHd4 = %Len(%Trim(VIcbHd4));
       lnCbHd5 = %Len(%Trim(VIcbHd5));
       lnCbHd6 = %Len(%Trim(VIcbHd6));
       lnCbHd7 = %Len(%Trim(VIcbHd7));
       lnCbHd8 = %Len(%Trim(VIcbHd8));
       lnCbHd9 = %Len(%Trim(VIcbHd9));
       lnCbHd10 = %Len(%Trim(VIcbHd10));

       lnInHd1 = %Len(%Trim(VIinHd1));
       lnInHd2 = %Len(%Trim(VIinHd2));
       lnInHd3 = %Len(%Trim(VIinHd3));
       lnInHd4 = %Len(%Trim(VIinHd4));
       lnInHd5 = %Len(%Trim(VIinHd5));
       lnInHd6 = %Len(%Trim(VIinHd6));
       lnInHd7 = %Len(%Trim(VIinHd7));
       lnInHd8 = %Len(%Trim(VIinHd8));
       lnInHd9 = %Len(%Trim(VIinHd9));
       lnInHd10 = %Len(%Trim(VIinHd10));

       lnDcHd1 = %Len(%Trim(VIdcHd1));
       lnDcHd2 = %Len(%Trim(VIdcHd2));
       lnDcHd3 = %Len(%Trim(VIdcHd3));
       lnDcHd4 = %Len(%Trim(VIdcHd4));
       lnDcHd5 = %Len(%Trim(VIdcHd5));
       lnDcHd6 = %Len(%Trim(VIdcHd6));
       lnDcHd7 = %Len(%Trim(VIdcHd7));
       lnDcHd8 = %Len(%Trim(VIdcHd8));
       lnDcHd9 = %Len(%Trim(VIdcHd9));
       lnDcHd10 = %Len(%Trim(VIdcHd10));

       lnTfHd1 = %Len(%Trim(VItfHd1));
       lnTfHd2 = %Len(%Trim(VItfHd2));
       lnTfHd3 = %Len(%Trim(VItfHd3));
       lnTfHd4 = %Len(%Trim(VItfHd4));
       lnTfHd5 = %Len(%Trim(VItfHd5));
       lnTfHd6 = %Len(%Trim(VItfHd6));
       lnTfHd7 = %Len(%Trim(VItfHd7));
       lnTfHd8 = %Len(%Trim(VItfHd8));
       lnTfHd9 = %Len(%Trim(VItfHd9));
       lnTfHd10 = %Len(%Trim(VItfHd10));

       position = 0;
       lengthLeft = 93;

       For i=1 to 50;
          headingIn = ' ';

          Select;
          When i = 1;
             If lnDtHd1 <> 0;
                headingIn = VIdtHd1;
                valueIn   = alphaUSA(cvtToDate(VIdt01));
                If lnDtHd1 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd1;
                EndIf;
             EndIf;
          When i = 2;
             If lnDtHd2 <> 0;
                headingIn = VIdtHd2;
                valueIn   = alphaUSA(cvtToDate(VIdt02));
                If lnDtHd2 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd2;
                EndIf;
             EndIf;
          When i = 3;
             If lnDtHd3 <> 0;
                headingIn = VIdtHd3;
                valueIn   = alphaUSA(cvtToDate(VIdt03));
                If lnDtHd3 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd3;
                EndIf;
             EndIf;
          When i = 4;
             If lnDtHd4 <> 0;
                headingIn = VIdtHd4;
                valueIn   = alphaUSA(cvtToDate(VIdt04));
                If lnDtHd4 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd4;
                EndIf;
             EndIf;
          When i = 5;
             If lnDtHd5 <> 0;
                headingIn = VIdtHd5;
                valueIn   = alphaUSA(cvtToDate(VIdt05));
                If lnDtHd5 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd5;
                EndIf;
             EndIf;
          When i = 6;
             If lnDtHd6 <> 0;
                headingIn = VIdtHd6;
                valueIn   = alphaUSA(cvtToDate(VIdt06));
                If lnDtHd6 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd6;
                EndIf;
             EndIf;
          When i = 7;
             If lnDtHd7 <> 0;
                headingIn = VIdtHd7;
                valueIn   = alphaUSA(cvtToDate(VIdt07));
                If lnDtHd7 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd7;
                EndIf;
             EndIf;
          When i = 8;
             If lnDtHd8 <> 0;
                headingIn = VIdtHd8;
                valueIn   = alphaUSA(cvtToDate(VIdt08));
                If lnDtHd8 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd8;
                EndIf;
             EndIf;
          When i = 9;
             If lnDtHd9 <> 0;
                headingIn = VIdtHd9;
                valueIn   = alphaUSA(cvtToDate(VIdt09));
                If lnDtHd9 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd9;
                EndIf;
             EndIf;
          When i = 10;
             If lnDtHd10 <> 0;
                headingIn = VIdtHd10;
                valueIn   = alphaUSA(cvtToDate(VIdt10));
                If lnDtHd10 < 10;
                   minLengthIn = 10;
                Else;
                   minLengthIn = lnDtHd10;
                EndIf;
             EndIf;
          When i = 11;
             If lnCbHd1 <> 0;
                headingIn = VICbHd1;

                If VIcb01 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd1 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd1;
                EndIf;
             EndIf;
          When i = 12;
             If lnCbHd2 <> 0;
                headingIn = VICbHd2;

                If VIcb02 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd2 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd2;
                EndIf;
             EndIf;
          When i = 13;
             If lnCbHd3 <> 0;
                headingIn = VICbHd3;

                If VIcb03 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd3 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd3;
                EndIf;
             EndIf;
          When i = 14;
             If lnCbHd4 <> 0;
                headingIn = VICbHd4;

                If VIcb04 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd4 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd4;
                EndIf;
             EndIf;
          When i = 15;
             If lnCbHd5 <> 0;
                headingIn = VICbHd5;

                If VIcb05 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd5 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd5;
                EndIf;
             EndIf;
          When i = 16;
             If lnCbHd6 <> 0;
                headingIn = VICbHd6;

                If VIcb06 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd6 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd6;
                EndIf;
             EndIf;
          When i = 17;
             If lnCbHd7 <> 0;
                headingIn = VICbHd7;

                If VIcb07 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd7 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd7;
                EndIf;
             EndIf;
          When i = 18;
             If lnCbHd8 <> 0;
                headingIn = VICbHd8;

                If VIcb08 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd8 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd8;
                EndIf;
             EndIf;
          When i = 19;
             If lnCbHd9 <> 0;
                headingIn = VICbHd9;

                If VIcb09 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd9 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd9;
                EndIf;
             EndIf;
          When i = 20;
             If lnCbHd10 <> 0;
                headingIn = VICbHd10;

                If VIcb10 = 'Y';
                   valueIn   = 'Yes';
                Else;
                   valueIn   = 'No ';
                EndIf;

                If lnCbHd10 < 3;
                   minLengthIn = 3;
                Else;
                   minLengthIn = lnCbHd10;
                EndIf;
             EndIf;
          When i = 21;
             If lnInHd1 <> 0;
                headingIn = VIinHd1;
                valueIn   = %EditC(VIint01:'4');
                If lnInHd1 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd1;
                EndIf;
             EndIf;
          When i = 22;
             If lnInHd2 <> 0;
                headingIn = VIinHd2;
                valueIn   = %EditC(VIint02:'4');
                If lnInHd2 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd2;
                EndIf;
             EndIf;
          When i = 23;
             If lnInHd3 <> 0;
                headingIn = VIinHd3;
                valueIn   = %EditC(VIint03:'4');
                If lnInHd3 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd3;
                EndIf;
             EndIf;
          When i = 24;
             If lnInHd4 <> 0;
                headingIn = VIinHd4;
                valueIn   = %EditC(VIint04:'4');
                If lnInHd4 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd4;
                EndIf;
             EndIf;
          When i = 25;
             If lnInHd5 <> 0;
                headingIn = VIinHd5;
                valueIn   = %EditC(VIint05:'4');
                If lnInHd5 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd5;
                EndIf;
             EndIf;
          When i = 26;
             If lnInHd6 <> 0;
                headingIn = VIinHd6;
                valueIn   = %EditC(VIint06:'4');
                If lnInHd6 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd6;
                EndIf;
             EndIf;
          When i = 27;
             If lnInHd7 <> 0;
                headingIn = VIinHd7;
                valueIn   = %EditC(VIint07:'4');
                If lnInHd7 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd7;
                EndIf;
             EndIf;
          When i = 28;
             If lnInHd8 <> 0;
                headingIn = VIinHd8;
                valueIn   = %EditC(VIint08:'4');
                If lnInHd8 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd8;
                EndIf;
             EndIf;
          When i = 29;
             If lnInHd9 <> 0;
                headingIn = VIinHd9;
                valueIn   = %EditC(VIint09:'4');
                If lnInHd9 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd9;
                EndIf;
             EndIf;
          When i = 30;
             If lnInHd10 <> 0;
                headingIn = VIinHd10;
                valueIn   = %EditC(VIint10:'4');
                If lnInHd10 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnInHd10;
                EndIf;
             EndIf;
          When i = 31;
             If lnDcHd1 <> 0;
                headingIn = VIdcHd1;
                valueIn   = %EditC(VIdec01:'4');
                If lnDcHd1 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd1;
                EndIf;
             EndIf;
          When i = 32;
             If lnDcHd2 <> 0;
                headingIn = VIdcHd2;
                valueIn   = %EditC(VIdec02:'4');
                If lnDcHd2 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd2;
                EndIf;
             EndIf;
          When i = 33;
             If lnDcHd3 <> 0;
                headingIn = VIdcHd3;
                valueIn   = %EditC(VIdec03:'4');
                If lnDcHd3 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd3;
                EndIf;
             EndIf;
          When i = 34;
             If lnDcHd4 <> 0;
                headingIn = VIdcHd4;
                valueIn   = %EditC(VIdec04:'4');
                If lnDcHd4 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd4;
                EndIf;
             EndIf;
          When i = 35;
             If lnDcHd5 <> 0;
                headingIn = VIdcHd5;
                valueIn   = %EditC(VIdec05:'4');
                If lnDcHd5 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd5;
                EndIf;
             EndIf;
          When i = 36;
             If lnDcHd6 <> 0;
                headingIn = VIdcHd6;
                valueIn   = %EditC(VIdec06:'4');
                If lnDcHd6 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd6;
                EndIf;
             EndIf;
          When i = 37;
             If lnDcHd7 <> 0;
                headingIn = VIdcHd7;
                valueIn   = %EditC(VIdec07:'4');
                If lnDcHd7 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd7;
                EndIf;
             EndIf;
          When i = 38;
             If lnDcHd8 <> 0;
                headingIn = VIdcHd8;
                valueIn   = %EditC(VIdec08:'4');
                If lnDcHd8 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd8;
                EndIf;
             EndIf;
          When i = 39;
             If lnDcHd9 <> 0;
                headingIn = VIdcHd9;
                valueIn   = %EditC(VIdec09:'4');
                If lnDcHd9 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd9;
                EndIf;
             EndIf;
          When i = 40;
             If lnDcHd10 <> 0;
                headingIn = VIdcHd10;
                valueIn   = %EditC(VIdec10:'4');
                If lnDcHd10 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnDcHd10;
                EndIf;
             EndIf;
          When i = 41;
             If lnTfHd1 <> 0;
                headingIn = VItfHd1;
                valueIn   = VItf01;
                If lnTfHd1 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd1;
                EndIf;
             EndIf;
          When i = 42;
             If lnTfHd2 <> 0;
                headingIn = VItfHd2;
                valueIn   = VItf02;
                If lnTfHd2 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd2;
                EndIf;
             EndIf;
          When i = 43;
             If lnTfHd3 <> 0;
                headingIn = VItfHd3;
                valueIn   = VItf03;
                If lnTfHd3 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd3;
                EndIf;
             EndIf;
          When i = 44;
             If lnTfHd4 <> 0;
                headingIn = VItfHd4;
                valueIn   = VItf04;
                If lnTfHd4 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd4;
                EndIf;
             EndIf;
          When i = 45;
             If lnTfHd5 <> 0;
                headingIn = VItfHd5;
                valueIn   = VItf05;
                If lnTfHd5 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd5;
                EndIf;
             EndIf;
          When i = 46;
             If lnTfHd6 <> 0;
                headingIn = VItfHd6;
                valueIn   = VItf06;
                If lnTfHd6 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd6;
                EndIf;
             EndIf;
          When i = 47;
             If lnTfHd7 <> 0;
                headingIn = VItfHd7;
                valueIn   = VItf07;
                If lnTfHd7 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd7;
                EndIf;
             EndIf;
          When i = 48;
             If lnTfHd8 <> 0;
                headingIn = VItfHd8;
                valueIn   = VItf08;
                If lnTfHd8 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd8;
                EndIf;
             EndIf;
          When i = 49;
             If lnTfHd9 <> 0;
                headingIn = VItfHd9;
                valueIn   = VItf09;
                If lnTfHd9 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd9;
                EndIf;
             EndIf;
          When i = 50;
             If lnTfHd10 <> 0;
                headingIn = VItfHd10;
                valueIn   = VItf10;
                If lnTfHd10 < %Len(%Trim(valueIn));
                   minLengthIn = %Len(%Trim(valueIn));
                Else;
                   minLengthIn = lnTfHd10;
                EndIf;
             EndIf;
          EndSl;

          If headingIn <> ' ';
             FormatDtl(headingIn:valueIn:minLengthIn:i);
          EndIf;

          If i = 50 and detHeading <> ' ';
             WriteDetail();
          EndIf;

       EndFor;

      /end-free

     p WriteVstDtl     e
