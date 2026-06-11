     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h/title MTGRPPRI- Group Configuration(Profound)
     h dftname(MTGRPPRI)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('STAPPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 06/10/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : MC447 - Redesign of Work with Groups             *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fMTGRPCFG  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEOMB:RrnEOMB)
     f                                     Sfile(sflGrpCfg:scSflRRN)
     f                                     Sfile(sflModInfo:scSflRRN2)
     f                                     Sfile(sflNetInfo:scSflRRN3)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     ffmfnp     if   e             disk    rename(pfn01:pfn0p)
     f                                     infds(dbfds1)
     ffmfnl     uf   e           k disk    rename(pfn01:pfn00)
     ffmfnl1    if   e           k disk    rename(pfn01:pfn01)
      * Fund Auxiliary File
     ffmf1l     uf a e           k disk    rename(pf101:pf100)
     f                                     infds(dbfds2)
      * Group State Information File
     ffdf2l     uf a e           k disk    rename(pf201:pf200)
     f                                     infds(dbfds3)
      * WCMC Control File
     ffmcxl     uf a e           k disk    rename(pcx01:pcx00)
     f                                     infds(dbfds4)
      * Group EOMB Comments File
     ffdf3l     if   e           k disk    rename(pf301:pf300)
     f                                     infds(dbfds5)
     ffdf3p     uf a e             disk    rename(pf301:pf30p)
     f                                     infds(dbfds6)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application Security File
     fsdtbl1    if   e           k disk

     d psds          esds                  extname(szq1p)
     d dbfds1          ds
     d  dbrrn1               397    400b 0
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
       // variable for Tab Names
     d arrTabNames     s            100    varying inz dim(20)
     d n1              s             10i 0 inz(1)
     d strTabNames     s           2000    varying inz
      // work variables
     d i               s              3  0
     d j               s              3  0
     d clientID        s              3    inz('000')
     d kkst            s              2    inz
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inSub           s              5  0
     d inFnd           s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d isFirst         s               n   inz(*on)
     d isSaved         s               n   inz(*off)
     d hitSaved        s               n   inz(*off)
     d error           s               n   inz(*off)
     d error1          s               n   inz(*off)
     d error2          s               n   inz(*off)
     d error3          s               n   inz(*off)
     d UsrDftFnd       s                   like(UPfnd) inz
     d UsrDftSub       s                   like(UPsub#) inz
     d UsrDftFyr       s                   like(UPfyr) inz
     d isWcif          s               n   inz
     d isNtwk          s               n   inz
     d isCrvw          s               n   inz
     d isWcin          s               n   inz
     d isNpwc          s               n   inz
     d xxDate1         s              8  0
     d beginDate       s              7  0 inz(0)
     d endDate         s              7  0 inz(0)
     d*
      // work variables
     d saveDT1         s                   like(FNdt)
     d saveTM1         s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3a        s                   like(FNdt)
     d saveTM3a        s                   like(FNtime)
     d saveDT3b        s                   like(FNdt)
     d saveTM3b        s                   like(FNtime)
     d saveDT3c        s                   like(FNdt)
     d saveTM3c        s                   like(FNtime)
     d saveDT3d        s                   like(FNdt)
     d saveTM3d        s                   like(FNtime)
     d saveDT4         s                   like(FNdt)
     d saveTM4         s                   like(FNtime)
     d saveDT5         s                   like(FNdt)
     d saveTM5         s                   like(FNtime)
     d svdbrrn3a       s                   like(dbrrn3)
     d svdbrrn3b       s                   like(dbrrn3)
     d svdbrrn3c       s                   like(dbrrn3)
     d svdbrrn3d       s                   like(dbrrn3)
     d flgRcdUpdated   s              1    inz
     d
     d svSflRRN        s              9  0 inz
     d svSflRRN2       s              9  0 inz
     d
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
     d
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
      * Array to store Tab index numbers, which is clicked by user before SAVE button
     d ClickTab        s              5  0 dim(4)

       // Variable for WA038R
     d  ##co#          s              3  0
     d  ##fnd          s              3  0
     d  ##rtcd         s              1
     d
       // Variable for GroupType
     d  outRtCD        S              1
     d  outCo          S              3
     d  outSub#        S              5
     d  outFnd         S              3
     d  outSts         S              1
     d
       // Variable for EOMB comments
     d                 ds
     d dsLine1                 1    105
     d  PartA                  1     70
     d  PartB                 71    105
     d                 ds
     d dsLine2                 1    105
     d  PartC                  1     35
     d  PartD                 36    105

     d @@co#           s              3  0
     d @@fnd           s              3  0
     d svnewrrn        s              5  0 inz
     d svdftrrn        s              5  0 inz

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GetCoFnd        pr                  extpgm('WA038R')
     d ##co#                          3  0
     d ##fnd                          3  0
     d ##rtcd                         1

      * Initialize the Screen fields for Add Group
     d InzGrpInfScVal  pr

     d GroupType       pr                  extpgm('FA701R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                1
      * Load Control Info
     d LoadControlInf  pr

      * Load EOMB Comments
     d LoadEOMBGrid    pr

      * Load Master Info
     d LoadMstInfo     pr

      * Load State Info
     d LoadStInfo      pr

      * Main Processing
     d Main            pr

      * Process Add EOMB Comments
     d ProcessAddEOMB  pr

      * Process Other Option for EOMB grid
     d ProcotherEomb   pr

      * Process Save All Info
     d ProcessSave     pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Group Primary info. procedure.
     d ValidateGroup   pr              n

     * Validate Group State info. procedure.
     d ValidateState   pr              n

     * Validate Group Control info. procedure.
     dValidateControl  pr              n

     * Validate EOMB Comments
     d ValidateEOMB    pr              n
     d strmode                        1

     * Write Update Fdf3p file for Eomb Comments
     d WrtUpdfdf3p     pr              n
     d strmode                        1

     d/copy sprUsrPrms
     d/copy sPrDate
     d/copy sprCompany
     d/copy sPrAtrium
     d/copy sPrSecure
     d/copy sPrGenPop
     d/copy sPrModule

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyFdf3l      klist
     c                   kfld                    inCo#
     c                   kfld                    inSub
     c                   kfld                    inFnd

      /free

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inSub = FNsub#;
       inFnd = FNfnd;

       GetClientID(clientID);

       ProgramStart(process);

       If process;

          If TbNmRcdPnl <> *Blanks;
             Main();
          Else;
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;

       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // Add information to list
       //===================================================================
     p AddToList       b
     d AddToList       pi           101a   varying
     d inString                     100a   varying const
     d inArray                      100a   varying dim(20)
     d inIndex                       10i 0

     d outString       s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;

      /End-free
     p AddToList       e

       //===================================================================
       // Clear All Tabs Error
       //===================================================================
     p ClrErrAllTab    b
     d ClrErrAllTab    pi

      /Free

       ClrErrGroup();
       ClrErrState();
       ClrErrControl();

      /End-free
     p ClrErrAllTab    e

       //===================================================================
       // Clear Control Tab Error
       //===================================================================
     p ClrErrControl   b
     d ClrErrControl   pi

      /Free

       erNurseHdg = *off;
       msNurseHdg = *blanks;
       erCaseDel  = *off;
       msCaseDel  = *blanks;
       erCaseTrf  = *off;
       msCaseTrf  = *blanks;
       erCaseDen  = *off;
       msCaseDen  = *blanks;
       erSecTyp01 = *off;
       msSecTyp01 = *blanks;
       erSecTyp02 = *off;
       msSecTyp02 = *blanks;
       erSecTyp03 = *off;
       msSecTyp03 = *blanks;
       erRmkCd    = *off;
       msRmkCd    = *blanks;

       // Provider Bill Control Information
       erMedExHdg = *off;
       msMedExHdg = *blanks;
       erMEUsr    = *off;
       msMEUsr    = *blanks;
       erMRedHdg  = *off;
       msMRedHdg  = *blanks;
       erORed1Hdg = *off;
       msORed1Hdg = *blanks;
       erORed2Hdg = *off;
       msORed2Hdg = *blanks;
       erNtRedHdg = *off;
       msNtRedHdg = *blanks;
       erChkDel   = *off;
       msChkDel   = *blanks;
       erSecTyp04 = *off;
       msSecTyp04 = *blanks;
       erRecSecLv = *off;
       msRecSecLv = *blanks;
       erSecTyp05 = *off;
       msSecTyp05 = *blanks;
       erRmkCdEob = *off;
       msRmkCdEob = *blanks;
       erCmpPnd   = *off;
       msCmpPnd   = *blanks;

      /End-free
     p ClrErrControl   e

       //===================================================================
       // Clear Group Tab Error
       //===================================================================
     p ClrErrGroup     b
     d ClrErrGroup     pi

      /Free

       erFnd  = *off;
       msFnd  = *blanks;
       erName = *off;
       msName = *blanks;
       erShnm = *off;
       msShnm = *blanks;
       erMad1 = *off;
       msMad1 = *blanks;
       erMcty = *off;
       msMcty = *blanks;
       erMst  = *off;
       msMst  = *blanks;
       erMzip = *off;
       msMzip = *blanks;
       erPad1 = *off;
       msPad1 = *blanks;
       erPcty = *off;
       msPcty = *blanks;
       erPst  = *off;
       msPst  = *blanks;
       erPzip = *off;
       msPzip = *blanks;
       erEfDta = *off;
       msEfDta = *blanks;
       erExDta = *off;
       msExDta = *blanks;

       erAdjcSt = *off;
       msAdjcSt = *blanks;

       erAnam = *off;
       msAnam = *blanks;
       erAad1 = *off;
       msAad1 = *blanks;
       erActy = *off;
       msActy = *blanks;
       erAst  = *off;
       msAst  = *blanks;
       erAzip = *off;
       msAzip = *blanks;
       erAltPhone = *off;
       msAltPhone = *blanks;
       erAWCGrp   = *off;
       msAWCGrp   = *blanks;

      /End-free
     p ClrErrGroup     e

       //===================================================================
       // Clear State Tab Error
       //===================================================================
     p ClrErrState     b
     d ClrErrState     pi

      /Free

       erInsCdNum = *off;
       msInsCdNum = *blanks;
       erInsFedID = *off;
       msInsFedID = *blanks;
       erInszip   = *off;
       msInszip   = *blanks;
       erTPACdNum = *off;
       msTPACdNum = *blanks;
       erTPAFedID = *off;
       msTPAFedID = *blanks;
       erTPAzip   = *off;
       msTPAzip   = *blanks;

       erKYFEIN   = *off;
       msKYFEIN   = *blanks;

       erMstBzID  = *off;
       msMstBzID  = *blanks;

       erCAFEIN   = *off;
       msCAFEIN   = *blanks;

      /End-free
     p ClrErrState     e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       // Clear the subfile
       scSflRRN2 = 0;
       scSflRRN3 = 0;
       scSflDsp2 = *off;
       scSflDsp3 = *off;
       scSflClr3 = *on;
       Write rcdGrpAdd;
       scSflDsp2 = *on;
       scSflDsp3 = *on;
       scSflClr3 = *off;

      /End-Free
     p ClrSubfiles     e

       //===================================================================
       // InzGrpInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzGrpInfScVal  b
     d InzGrpInfScVal  pi
     d
      /Free

       visGrpCfg  = *off;
       visMstCont = *off;
       visARCont  = *off;
       visNetCont = *off;
       visStCont  = *off;
       visMedCont = *off;
       visBilCont = *off;
       visGrpHdg  = *off;
       GrpEdt  =  *on;
       dsbEdit = *on;

       Select ;
          When F1outa = 'Y';
             isgrpadd = *on;
          When F1outa = 'N';
             isgrpadd = *off;
          When F1outa = ' ';
             isgrpadd = *off;
       Endsl;

       visCSI = *off;
       visWCGrp = *off;
       visNtRedHd = *off;
       visCSIAud = *off;
       visClmChk = *off;
       VisCVT# = *off;

       Select;

       When arrTabNames(TabIndex+1) = 'Group Information';
          visMstCont = *on;
          visGrpHdg  = *on;
          if clientID = '114';
            visCSI  = *on;
            selEOBcd = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
               'GTstat<>' + '''I''' + 'and GTtbid=' + '''OEOB''';
          endif;

          // Non-Workers Comp group
          If isWcif = *on;
             visWCGrp = *on;
          Endif;

       When arrTabNames(TabIndex+1) = 'State Information';
          visStCont  = *on;

       When arrTabNames(TabIndex+1) = 'Control Information';
          visMedCont = *on;
          visBilCont = *on;
          // Set visibility for Network Reduction Heading
          If isNtwk = *on;
             visNtRedHd = *on;
          Endif;

          // Set visibility for Procura Audit flag
          if clientID = '114';
            visCSIAud = *on;
          endif;

          // Set visibility for Process Through Claim Check
          If isCrvw = *on;
             visClmChk = *on;
          Endif;

          // Set visibility for CVT Claim Number
          Select;
          When @@mode = 'A' and outSTS ='N';
             VisCVT# = *off;
             DisCVT# = *on;
          When outSTS = 'N';
             VisCVT# = *on;
             DisCVT# = *on;
          Other;
             VisCVT# = *on;
             DisCVT# = *off;
          Endsl;

          //Medical Case Control Information
          //Security Types Selection Criteria
          SelSecTyp = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
             'GTstat<>' + '''I''' + 'and GTtbid=' + '''SECT''';

          //Provider Bill Control Information
          //Medical Examiner User
          SelMEUsr = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
             'GTstat<>' + '''I''' + 'and GTtbid=' + '''DIUS''';

          //Default Pend Code
          SelPndCode = 'BSco# = ' + %EditC(intCo#:'X') + ' and ' +
             'BSstat<>' + '''I''' + 'and BStype=' + '''P''';

       Endsl;

       // Set visibility for ALLOW MC Case Closure

      /End-Free
     p InzGrpInfScVal  e

       //===================================================================
       // LoadControl- Load Control Information
       //===================================================================
     p LoadControlInf  B
     d LoadControlInf  PI

      /free

       scFocus4 = *on;
       chain(n) (inCo#:inSub:inFnd) pcx00;
       //Medical Case Control Information   // from Fmcxp file
       scNurseHdg   =  CXhdnu;
       scSupHdg     =  CXhdsu;
       scStf3Hdg    =  CXhds3;
       scClmSHdg    =  CXhdcs;
       scClmAdHdg   =  CXhdca;
       scFldAdHdg   =  CXhdfa;
       scCaseDel    =  CXcldl;
       scSecTyp01   =  CXcldt;
       scCaseTrf    =  CXcltl;
       scSecTyp02   =  CXcltt;
       scCaseDen    =  CXdnsl;
       scSecTyp03   =  CXdnst;
       scRmkCd      =  CXrvdt;

       //Provider Bill Control Information  // from Fmcxp file
       scMedExHdg   =  CXhex;
       scMEUsr      =  CXflex;
       scMRedHdg    =  CXhmra;
       scORed1Hdg   =  CXhot1;
       scORed2Hdg   =  CXhot2;
       scNtRedHdg   =  CXhnet;
       scChkDel     =  CXckdl;
       scSecTyp04   =  CXckdt;
       scROMBRec    =  CXrcal;
       scRecSecLv   =  CXrcsl;
       scSecTyp05   =  CXrcst;
       scRmkCdEob   =  CXrmeb;
       scCmpPnd     =  CXcpdf;
       scPendCode   =  CXcppn;

       scPrClmChk   =  CXcrus;
       If visCSI = *on;
         if CXpauf = 'Y';
           scAudFlg  = 'Y';
         else;
           scAudFlg  = *blanks;
         endif;
       Endif;
       scCVTClm#   =  CXcvt#;

       saveDT4 = CXdt;
       saveTM4 = CXtime;
      /end-free
     p LoadControlInf  e

       //===================================================================
       // Load EOMB Grid info
       //===================================================================
     p LoadEOMBGrid    b
     d LoadEOMBGrid    pi

      /free
       // Clear the subfile
       RrnEOMB = 0;
       dspEOMB = *off;
       clrEOMB = *on;
       Write rcdEOMB;
       clrEOMB = *off;
       dspEOMB = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll keyFdf3l fdf3l;
       Reade keyFdf3l fdf3l;
       Dow not %eof(fdf3l);

          RrnEOMB += 1;
          scEOMBSel = *off;
 
          If svsflrrn2 = 0;
             If RrnEOMB = 1;
                scEOMBSel = *on;
             Endif;
          Else;
             If RrnEOMB = svsflrrn2;
                scEOMBSel = *on;
             Endif;
          Endif;
          scEOMBEfDt = CvtToDate(F3efdt);
          scComm1    = F3ec01;
          If F3stat = 'I';
             scEombSts  = 'Inactive';
          Else;
             scEombSts  = 'Active';
          Endif;
          scEombRrn  = dbrrn5;

          Write sflEOMB;
          Reade keyFdf3l fdf3l;

       Enddo;

       RrnEOMB = 1;

       If svSflRRN2 <> 0;
          RrnEOMB  = svSflRRN2;
          scRowSel = svSflRRN2;
          svSflRRN2 = 0;
       Endif;

      /end-free
     p LoadEOMBGrid    e

       //===================================================================
       // LoadMstInfo - Load Group Master Information
       //===================================================================
     p LoadMstInfo     B
     d LoadMstInfo     PI
     d  isexit         s               n   Inz(*off)

      /free

       scFocus2 = *on;
       Chain xxRrn pfn0p;
       chain(n) (inCo#:inSub:inFnd) pf100;
       scFnd     =   FNfnd;        // Fund Number
       scName    =   FNname;       // Name
       scShnm    =   FNshnm;       // Short Name
       scMad1    =   FNmad1;       // Mailing Addres 1
       scMad2    =   FNmad2;       // Mailing Addres 2
       scMcty    =   FNmcty;       // Mailing City
       scMst     =   FNmst;        // Mailing State
       scMzip    =   FNmzip;       // Mailing Zipcode
       scPad1    =   FNpad1;       // Physical Addres 1
       scPad2    =   FNpad2;       // Physical Addres 2
       scPcty    =   FNpcty;       // Physical City
       scPst     =   FNpst;        // Physical State
       scPzip    =   FNpzip;       // Physical Zipcode
       scStsE    =   FNstat;       // Status
       scAdjcSt  =   FNast;        // Adjudication State

       If FNara1 <> *zeros or FNph1 <> *zeros;
          scPhone1 = %int(%editc(FNara1:'X') + %editc(FNph1:'X'));
       Endif;
       If FNarea <> *zeros or FNph2 <> *zeros;
          scPhone2 = %int(%editc(FNarea:'X') + %editc(FNph2:'X'));
       Endif;
       If FNfara <> *zeros or FNfax <> *zeros;
          scFax    = %int(%editc(FNfara:'X') + %editc(FNfax:'X'));
       Endif;

       scefDta = dtMDYY(FNEfdt);        // Effective date
       scexDta = dtMDYY(FNExdt);        // Expiration date

       scWCGrpE    =   FNwcf#;          // WCMS Group Number
       scTestGrp   =   FNtest;          // Test Group Flag
       scNonWCGrp  =   FNnwcg;          // Non-WCMS Group
       scPymtLmt   =   FNmpln;          // Medical Payment Limit
       scExGrpClm  =   FNclelig;        // Exclude From Base Claims El
       scEOBCd     =   FNoEob;          // Outbound EOB Code

       scAltFlg    =   F1flg1;          // Alternate Addres flag
       scAnam      =   F1anme;          // Alternate Company Name
       scAad1      =   F1aad1;          // Alternate Addres 1
       scAad2      =   F1aad2;          // Alternate Addres 2
       scActy      =   F1acty;          // Alternate City
       scAst       =   F1ast;           // Alternate State
       scAzip      =   F1azip;          // Alternate Zipcode

       If F1aara <> *zeros or F1aphn<> *zeros;
          scAltPhone  = %int(%editc(F1aara:'X') + %editc(F1aphn:'X'));
       Endif;

       scAFedID    =   F1afei;           // Alternate Fed ID
       scStdAlone  =   F1outa;           // StandAlone
       scChkIss    =   F1fl04;           // WCMC Check Issuance

       saveDT1 = FNdt;
       saveTM1 = FNtime;
       saveDT2 = F1dt;
       saveTM2 = F1time;
      /end-free
     p LoadMstInfo     e

       //===================================================================
       // LoadStInfo - Load Group State Information
       //===================================================================
     p LoadStInfo      B
     d LoadStInfo      PI

      /free

       scFocus3 = *on;
       kkst = 'FL';
       Chain(n) (inCo#:inSub:inFnd:kkst) pf200;
       If %found(fdf2l);
          scInsCdNum  =  F25n01;
          scTPACdNum  =  F25n02;
          scInsFedID  =  F29n01;
          scTPAFedID  =  F29n03;
          scInszip    =  F29n02;
          scTPAzip    =  F29n04;
          svdbrrn3a = dbrrn3;
          saveDT3a = F2date;
          saveTM3a = F2time;
       Endif;

       //CA state Information
       kkst = 'CA';
       Chain(n) (inCo#:inSub:inFnd:kkst) pf200;
       If %found(fdf2l);
          scCAFein  =  F29n01;
          svdbrrn3b = dbrrn3;
          saveDT3b = F2date;
          saveTM3b = F2time;
       Endif;

       //KY state Information
       kkst = 'KY';
       Chain(n) (inCo#:inSub:inFnd:kkst) pf200;
       If %found(fdf2l);
          scKYFein  =  F29n01;
          svdbrrn3c = dbrrn3;
          saveDT3c = F2date;
          saveTM3c = F2time;
       Endif;

       //TX state Information
       kkst = 'TX';
       Chain(n) (inCo#:inSub:inFnd:kkst) pf200;
       If %found(fdf2l);
          scMstBzID =  F29n01;
          svdbrrn3d = dbrrn3;
          saveDT3d = F2date;
          saveTM3d = F2time;
       Endif;

      /end-free
     p LoadStInfo      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;

          // Add EOMB Comments
          When BtnEOMBAdd = *On;
             BtnEOMBAdd = *Off;
             ProcessAddEOMB();

          // Enter key or Save button
          When btnSave = *on;
             btnSave = *off;
             isSaved = *off;
             hitsaved = *on;
             error = *off;
             If @@mode <> 'D';

                // Check if FMFNP updated since screen displayed (Group Master)
                flgRcdUpdated = RcdUpdatedErr('FMFNP':dbrrn1:'FNdt':'FNtime':
                 'FNuser':saveDT1:saveTM1);

                If flgRcdUpdated = '0' and saveDT2 <> 0; // No error
                   // Check if FMF1P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FMF1P':dbrrn2:'F1dt':
                    'F1time':'F1user':saveDT2:saveTM2);
                EndIf;
                If flgRcdUpdated = '0' and saveDT3a <> 0; // No error
                   // Check if FDF2P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FDF2P':svdbrrn3a:'F2date':
                    'F2time':'F2user':saveDT3a:saveTM3a);
                EndIf;
                If flgRcdUpdated = '0' and saveDT3b <> 0; // No error
                   // Check if FDF2P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FDF2P':svdbrrn3b:'F2date':
                    'F2time':'F2user':saveDT3b:saveTM3b);
                EndIf;
                If flgRcdUpdated = '0' and saveDT3c <> 0; // No error
                   // Check if FDF2P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FDF2P':svdbrrn3c:'F2date':
                    'F2time':'F2user':saveDT3c:saveTM3c);
                EndIf;
                If flgRcdUpdated = '0' and saveDT3d <> 0; // No error
                   // Check if FDF2P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FDF2P':svdbrrn3d:'F2date':
                    'F2time':'F2user':saveDT3d:saveTM3d);
                EndIf;
                If flgRcdUpdated = '0' and saveDT4 <> 0; // No error
                   // Check if FMCXP updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FMCXP':dbrrn4:'CXdt':
                    'CXtime':'CXuser':saveDT4:saveTM4);
                EndIf;

                If flgRcdUpdated = '0';
                   If %Lookup('Group Information':arrTabNames) <> 0;
                      error1 = ValidateGroup();
                   Endif;
                   If %Lookup('State Information':arrTabNames) <> 0;
                      error2 = ValidateState();
                   Endif;
                   If %Lookup('Control Information':arrTabNames) <> 0;
                      error3 = ValidateControl();
                   Endif;

                   Select;
                   When error1=*on;
                      tabIndex=%Lookup('Group Information':arrTabNames)-1;
                      error=*on;
                   When error2=*on;
                      tabIndex=%Lookup('State Information':arrTabNames)-1;
                      error=*on;
                   When error3=*on;
                      tabIndex=%Lookup('Control Information':arrTabNames)-1;
                      error=*on;
                   Endsl;

                   If error1 = *off and error2 = *off and error3 = *off;
                      ProcSavGroup();
                      ProcSavState();
                      ProcSavControl();
                      PopDataSaved();
                      isSaved = *on;
                   Endif;
                Endif;
             Endif;

          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'MTGRPPRI';
             inPgmName     = 'Primary Group Information';
             inTabName     =  arrTabNames(TabIndex + 1);
             inTabName     =  %Trim(inTabName) + ' - ' + 'User Access';
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          Other;
             ProcOtherEomb();                   // EOMB Comments Grid
          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        indexTab = TabIndex + 1;

        If isFirst = *on or btnRefresh = *on or isSaved = *on;
           isFirst = *off;
           btnRefresh = *off;
           isSaved = *off;
           hitSaved= *off;
           clear rcdGrpAdd;
           ClrSubfiles();
           LoadMstInfo();
           LoadStInfo();
           LoadControlInf();
        Endif;
        InzGrpInfScVal();
        inquiry = *off;
        If dspMode;
           inquiry = *on;
        Endif;

        Select;
        When arrTabNames(TabIndex+1) = 'Group Information';
           pnlHeight = 330;
           APnlHeight = pnlHeight + 60;
           TopMst  = 125;

           ClrErrAllTab();
           If not dspMode and hitSaved;
              ValidateGroup();
           Endif;
           Write rcdGrpPnl;
           Write rcdPanel;
           Exfmt rcdGrpAdd;
           Read rcdGrpPnl;
           Read rcdPanel;

        When arrTabNames(TabIndex+1) = 'State Information';
           pnlHeight = 210;
           APnlHeight = pnlHeight + 60;
           TopState = 85;

           ClrErrAllTab();
           If not dspMode and hitSaved;
              ValidateState();
           Endif;
           Write rcdGrpPnl;
           Write rcdPanel;
           Exfmt rcdGrpAdd;
           Read rcdGrpPnl;
           Read rcdPanel;

        When arrTabNames(TabIndex+1) = 'Control Information';
           pnlHeight = 610;
           APnlHeight = pnlHeight + 60;
           TopMed = 90;
           TopBill = 355;

           ClrErrAllTab();
           If not dspMode and hitSaved;
              ValidateControl();
           Endif;
           Write rcdGrpPnl;
           Write rcdPanel;
           Exfmt rcdGrpAdd;
           Read rcdGrpPnl;
           Read rcdPanel;

        When arrTabNames(TabIndex+1) = 'EOMB Comments';
           pnlHeight = 280;
           APnlHeight = pnlHeight + 60;
           LoadEOMBGrid();

           Write rcdGrpPnl;
           Write rcdPanel;
           Exfmt rcdEOMB;
           Read rcdGrpPnl;
           Read rcdPanel;
        EndSl;
       Endsr;

      /end-free
     p Main            e

      //===================================================================
      // ProcessAddEOMB - Add EOMB Comments
      //===================================================================
     p ProcessAddEOMB  b
     d ProcessAddEOMB  pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /Free

       Clear rcdEombAdd;
       isexit = *off;
       mode   = 'A';
       Dow isexit = *off;
          scEombHdg = 'Add EOMB Comments';
          visSavEomb = *on;
          scEombStsE  = 'A';

          Exfmt rcdEombAdd;
          Select;
          // Save Button
          When BtnSavEomb = *on;
             BtnSavEomb = *off;
             If ValidateEomb(mode);
                WrtUpdFdf3p(mode);
                Write pf30p;
                isexit = *on;
             Endif;

          // Cancel Button
          When BtnCanEomb = *on;
             BtnCanEomb = *off;
             isexit = *on;
          Endsl;
       Enddo;

      /End-Free
     p ProcessAddEOMB  e

      //===================================================================
      // ProcotherEomb  - Process Other option for EOMB grid
      //===================================================================
     p ProcotherEomb   b
     d ProcotherEomb   pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /Free

       Clear rcdEombAdd;
       isexit = *off;
       ReadC sflEomb;
       Dow not %eof;

          Select;
          // Edit Link
          When scEombEdt  = *on;
             scEombEdt  = *off;
             mode = 'C';
             scEombHdg = 'Edit EOMB Comments';
             Chain(e) scEombRrn pf30p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             svsflrrn2 = RrnEomb;  //save rrn value
             Chain(n) scEombRrn pf30p;
             Exsr Movedbsc;
             dsbEombDt = *on;
             visSavEomb = *on;
             Dow isexit = *off;
                Exfmt rcdEombAdd;
                Select;

                When BtnSavEomb = *on;
                   BtnSavEomb = *off;

                   // Check if fdf3p updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('FDF3P':dbrrn6:'F3dt':'F3time':
                    'F3user':saveDT5:saveTM5:'1');
                   If flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scEombRrn pf30p;
                      Exsr Movedbsc;
                   EndIf;

                   If flgRcdUpdated = '0'; // No error
                      // Validate the Eomb Comments
                      If ValidateEomb(mode);
                         Chain scEombRrn pf30p;
                         WrtUpdFdf3p(mode);
                         update pf30p;
                         isExit = *on;
                      Endif;
                   Endif;

                When BtnCanEomb = *on;
                   BtnCanEomb = *off;
                   isExit = *on;
                Endsl;
             Enddo;

          //Display Eomb Comments
          When scEombDsp  = *on;
             scEombDsp  = *off;
             svsflrrn2 = RrnEomb;
             mode = 'D';
             scEombHdg = 'Display EOMB Comments';
             Chain(n) scEombRrn pf30p;
             Exsr Movedbsc;
             inquiry = *on;
             visSavEomb = *off;
             Exfmt RcdEombAdd;
             If BtnCanEomb = *on;
                BtnCanEomb = *off;
             Endif;
          Endsl;
          ReadC sflEomb;

       Enddo;

       Begsr Movedbsc;
        scEombStsE =  F3stat;
        scEombDate = dtMDYY(F3Efdt);        // Effective date
        scComm1  =  F3ec01 + %Subst(F3ec02:1:35);
        scComm2  =  %Subst(F3ec02:36:35) + F3ec03;

        scComm3  =  F3ec04 + %Subst(F3ec05:1:35);
        scComm4  =  %Subst(F3ec05:36:35) + F3ec06;

        scComm5  =  F3ec07 + %Subst(F3ec08:1:35);
        scComm6  =  %Subst(F3ec08:36:35) + F3ec09;

        scComm7  =  F3ec10 + %Subst(F3ec11:1:35);
        scComm8  =  %Subst(F3ec11:36:35) + F3ec12;

        scComm9  =  F3ec13 + %Subst(F3ec14:1:35);
        scComm10 =  %Subst(F3ec14:36:35) + F3ec15;

        scComm11 =  F3ec16 + %Subst(F3ec17:1:35);
        scComm12 =  %Subst(F3ec17:36:35) + F3ec18;

        scComm13 =  F3ec19 + %Subst(F3ec20:1:35);
        scComm14 =  %Subst(F3ec20:36:35) + F3ec21;

        scComm15 =  F3ec22 + %Subst(F3ec23:1:35);
        scComm16 =  %Subst(F3ec23:36:35) + F3ec24;

        scComm17 =  F3ec25 + %Subst(F3ec26:1:35);
        scComm18 =  %Subst(F3ec26:36:35) + F3ec27;

        scComm19 =  F3ec28 + %Subst(F3ec29:1:35);
        scComm20 =  %Subst(F3ec29:36:35) + F3ec30;

        saveDT5  = F3dt;
        saveTM5  = F3time;
       Endsr;

      /End-Free
     p ProcotherEomb   e

       //===================================================================
       // ProcSavControl - Sate Control Information
       //===================================================================
     P ProcSavControl  B
     D ProcSavControl  PI

      /free

        //Write Medical Case Control Information   // update/write Fmcxp file
        chain (inCo#:inSub:inFnd) pcx00;
        If %found(fmcxl);
           Exsr Movescdb3;
           update pcx00;
        Else;
           clear pcx00;
           Exsr Movescdb3;
           CXiusr  =  q1user;                // Initial User
           CXipgm  =  q1pgm;                 // Initial Program
           CXitim  =  Timeto6(SystemTime()); // Initial Time
           CXidt   =  Dateto7(SystemDate()); // Initial Date
           write pcx00;
        Endif;

        Begsr Movescdb3;
         CXco#   =  inCo#;
         CXsub#  =  inSub;
         CXfnd   =  inFnd;
         CXhdnu  =  scNurseHdg;
         CXhdsu  =  scSupHdg;
         CXhds3  =  scStf3Hdg;
         CXhdcs  =  scClmSHdg;
         CXhdca  =  scClmAdHdg;
         CXhdfa  =  scFldAdHdg;
         CXcldl  =  scCaseDel;
         CXcldt  =  scSecTyp01;
         CXcltl  =  scCaseTrf;
         CXcltt  =  scSecTyp02;
         CXdnsl  =  scCaseDen;
         CXdnst  =  scSecTyp03;
         CXrvdt  =  scRmkCd;

         CXhex   =  scMedExHdg;
         CXflex  =  scMEUsr;
         CXhmra  =  scMRedHdg;
         CXhot1  =  scORed1Hdg;
         CXhot2  =  scORed2Hdg;
         CXhnet  =  scNtRedHdg;
         CXckdl  =  scChkDel;
         CXckdt  =  scSecTyp04;
         CXrcal  =  scROMBRec;
         CXrcsl  =  scRecSecLv;
         CXrcst  =  scSecTyp05;
         CXrmeb  = scRmkCdEob;
         CXcpdf  =  scCmpPnd;
         CXcppn  =  scPendCode;

         CXcrus  =  scPrClmChk;
         If visCSI = *on;
            if scAudFlg = 'Y';
               CXpauf = 'Y';
            else;
               CXpauf = *blanks;
           endif;
         Endif;
         CXcvt# = scCVTClm#;
         CXstat  =  'A';
         CXuser  =  q1user;                // User
         CXpgm   =  q1pgm;                 // Program
         CXtime  =  Timeto6(SystemTime()); // Time
         CXdt    =  Dateto7(SystemDate()); // Date Mintained

         saveDT4 = CXdt;
         saveTM4 = CXtime;
        Endsr;

      /end-free
     p ProcSavControl  e

       //===================================================================
       // ProcSavGroup - Save Group Information
       //===================================================================
     P ProcSavGroup    B
     D ProcSavGroup    PI

      /free

       chain (inCo#:inSub:inFnd) pfn00;
       chain (inCo#:inSub:inFnd) pf100;
       If %found(fmf1l);
          Exsr Movescdb1;
          update pf100;
       Else;
          clear pf100;
          Exsr Movescdb1;
          F1iusr   = q1user;                // User
          F1ipgm   = q1pgm;                 // Program
          F1itim   = Timeto6(SystemTime()); // Time
          F1idt    = Dateto7(SystemDate()); // Date Mintained
          write pf100;
       Endif;
       update pfn00;

       Begsr Movescdb1;
        FNname  = scName;                // Name
        FNshnm  = scShnm;                // Short Name
        FNast   = scAdjcSt;              // Adjudication State
        FNmad1  = scMad1;                // Mailing Addres 1
        FNmad2  = scMad2;                // Mailing Addres 2
        FNmcty  = scMcty;                // Mailing City
        FNmst   = scMst;                 // Mailing State
        FNmzip  = scMzip;                // Mailing Zipcode
        FNpad1  = scPad1;                // Physical Addres 1
        FNpad2  = scPad2;                // Physical Addres 2
        FNpcty  = scPcty;                // Physical City
        FNpst   = scPst;                 // Physical State
        FNpzip  = scPzip;                // Physical Zipcode

        If scPhone1 > *zeros;
           FNara1  = %dec(%subst(%editc(scPhone1:'X'):1:3):3:0); // Phone 1 Area Code
           FNph1   = %dec(%subst(%editc(scPhone1:'X'):4):7:0);   // Phone 1 Number
        EndIf;
        If scPhone1 = *zeros;
           FNara1  = *zeros;
           FNph1   = *zeros;
        EndIf;

        If scPhone2 > *zeros;
           FNarea  = %dec(%subst(%editc(scPhone2:'X'):1:3):3:0); // Phone 2 Area Code
           FNph2   = %dec(%subst(%editc(scPhone2:'X'):4):7:0);   // Phone 2 Number
        EndIf;
        If scPhone2 = *zeros;
           FNarea  = *zeros;
           FNph2   = *zeros;
        EndIf;

        If scFax > *zeros;
           FNfara  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax Area Code
           FNfax   = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax Number
        EndIf;
        If scFax = *zeros;
           FNfara  = *zeros;
           FNfax   = *zeros;
        EndIf;

        FNefdt  = dtCYMD(scEfDta);       // Effective date
        FNexdt  = dtCYMD(scExDta);       // Effective date

        FNwcf#  = scWCGrpE;              // WCMS Group Number
        FNtest  = scTestGrp;             // Test Group Flag
        FNnwcg  = scNonWCGrp;            // Non-WCMS Group
        FNmpln  = scPymtLmt;             // Medical Payment Limit
        FNclelig= scExGrpClm;            // Exclude From Base Claims El
        FNoEob  = scEOBCd;               // Outbound EOB Code

        // Move data to fmf1p file
        F1co#   = inCo#;                 // Compnay Number
        F1sub#  = inSub;                 // Subcriber
        F1fnd   = inFnd;                 // fund
        F1flg1  = scAltFlg;              // Alternate Addres flag
        F1anme  = scAnam;                // Alternate Company Name
        F1aad1  = scAad1;                // Alternate Addres 1
        F1aad2  = scAad2;                // Alternate Addres 2
        F1acty  = scActy;                // Alternate City
        F1ast   = scAst;                 // Alternate State
        F1azip  = scAzip;                // Alternate Zipcode
        If scAltPhone > *zeros;
           F1aara  = %dec(%subst(%editc(scAltPhone:'X'):1:3):3:0); // Phone Area Code
           F1aphn  = %dec(%subst(%editc(scAltPhone:'X'):4):7:0);   // Phone Number
        EndIf;
        If scAltPhone = *zeros;
           F1aara  = *zeros;
           F1aphn  = *zeros;
        EndIf;
        F1afei  = scAFedID;              // Alternate Fed ID
        F1outa  = scStdAlone;            // StandAlone
        F1fl04  = scChkIss;              // WCMC Check Issuance
        //End of moving screen field to fmfnp and fmf1p file

        FNstat  = scStsE;                // Status
        FNuser  = q1user;                // User
        FNpgm   = q1pgm;                 // Program
        FNtime  = Timeto6(SystemTime()); // Time
        FNdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT1 = FNdt;
        saveTM1 = FNtime;

        F1stat  = scStsE;                // Status
        F1user  = q1user;                // User
        F1pgm   = q1pgm;                 // Program
        F1time  = Timeto6(SystemTime()); // Time
        F1dt    = Dateto7(SystemDate()); // Date Mintained
        saveDT2 = F1dt;
        saveTM2 = F1time;
       Endsr;

      /end-free
     P ProcSavGroup    e

       //===================================================================
       // ProcSavState - Save State Information
       //===================================================================
     P ProcSavState    B
     D ProcSavState    PI

      /free

        //Write/Update WCMC Group State Information
        //Write/Update FL state Information
        If scInsCdNum <> *zeros;
           kkst = 'FL';
           Chain (inCo#:inSub:inFnd:kkst) pf200;
           If %found(fdf2l);
              F25n01  = scInsCdNum;
              F25n02  = scTPACdNum;
              F29n01  = scInsFedID;
              F29n03  = scTPAFedID;
              F29n02  = scInszip;
              F29n04  = scTPAzip;
              Exsr UpdFdf2p;
           Else;
              clear pf200;
              F2st    =  'FL';
              F25n01  = scInsCdNum;
              F25n02  = scTPACdNum;
              F29n01  = scInsFedID;
              F29n03  = scTPAFedID;
              F29n02  = scInszip;
              F29n04  = scTPAzip;
              Exsr WrtFdf2p;
           Endif;
           saveDT3a = F2date;
           saveTM3a = F2time;
        Endif;

        //Write/Update CA State Information
        If scCAFein <> *zeros;
           kkst = 'CA';
           Chain (inCo#:inSub:inFnd:kkst) pf200;
           If %found(fdf2l);
              F29n01  =  scCAFein;
              Exsr UpdFdf2p;
           Else;
              clear pf200;
              F2st    =  'CA';
              F29n01  =  scCAFein;
              Exsr WrtFdf2p;
           Endif;
           saveDT3b = F2date;
           saveTM3b = F2time;
        Endif;

        //Write/Update KY state Information
        If scKYFein <> *zeros;
           kkst = 'KY';
           Chain (inCo#:inSub:inFnd:kkst) pf200;
           If %found(fdf2l);
              F29n01  =  scKYFein;
              Exsr UpdFdf2p;
           Else;
              clear pf200;
              F2st    =  'KY';
              F29n01  =  scKYFein;
              Exsr WrtFdf2p;
           Endif;
           saveDT3c = F2date;
           saveTM3c = F2time;
        Endif;

        //Write TX state Information
        If scMstBzID<> *zeros;
           kkst = 'TX';
           Chain (inCo#:inSub:inFnd:kkst) pf200;
           If %found(fdf2l);
              F29n01  =  scMstBzID;
              Exsr UpdFdf2p;
           Else;
              clear pf200;
              F2st    =  'TX';
              F29n01  =  scMstBzID;
              Exsr WrtFdf2p;
           Endif;
           saveDT3d = F2date;
           saveTM3d = F2time;
        Endif;

       Begsr WrtFdf2p;
        F2co#   = inCo#;
        F2sub#  = inSub;
        F2fnd   = inFnd;
        F2stat  = 'A';
        F2user  = q1user;                // User
        F2pgm   = q1pgm;                 // Program
        F2time  = Timeto6(SystemTime()); // Time
        F2date  = Dateto7(SystemDate()); // Date Mintained
        F2iusr  = q1user;                // Initial User
        F2ipgm  = q1pgm;                 // Initial Program
        F2itim  = Timeto6(SystemTime()); // Initial Time
        F2idt   = Dateto7(SystemDate()); // Initial Date
        Write pf200;
       Endsr;

       Begsr UpdFdf2p;
        F2stat  =  'A';
        F2user  = q1user;                // User
        F2pgm   = q1pgm;                 // Program
        F2time  = Timeto6(SystemTime()); // Time
        F2date  = Dateto7(SystemDate()); // Date Mintained
        Update pf200;
       Endsr;

      /end-free
     p ProcSavState    e

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
       Endif;

       apShowMenu = *on;

       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'MTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Employer Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPEMP';

       scMenuChoi = %trim(scMenuChoi) + ',Accounting Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPACT';

       // Set Screen Heading
       scHeading = 'Primary Group Information';

       aMenHeight = 80;

       apFootHght = aMenHeight + 125;

       // get the system values

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
       scCo# = intCo#;
       //Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

      // Get User Default Group
       UsrDftFnd = 0;
       Chain q1user pup00;
       If %found(smupl);
          UsrDftFnd = UPfnd;
          UsrDftSub = UPsub#;
          UsrDftFyr = UPfyr;
       EndIf;

       // Retrieve System Values
       isWcif   = *off;
       isWcif   = IsModEnabled('WorkComp');

       isWcin   = *off;
       isWcin   = IsModEnabled('Work Comp Interface');

       isNpwc   = *off;
       isNpwc   = IsModEnabled('NonPDSWC');

       isNtwk   = *off;
       isNtwk   = IsModEnabled('Network/PPO');

       isCrvw   = *off;
       isCrvw   = IsModEnabled('Claim Check Installed');

       processMe = *on;

       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup00;
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Group Information tab
       Chain (intCo#:'MTGRPPRI':'Primary Group Information':
        'Group Information - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Group Information':arrTabNames:n1);
       Endif;

       // State Information tab
       Chain (intCo#:'MTGRPPRI':'Primary Group Information':
        'State Information - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('State Information':arrTabNames:n1);
       Endif;

       // Control Information tab
       Chain (intCo#:'MTGRPPRI':'Primary Group Information':
        'Control Information - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Control Information':arrTabNames:n1);
       Endif;

       // EOMB Comments tab
       Chain (intCo#:'MTGRPPRI':'Primary Group Information':
        'EOMB Comments - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('EOMB Comments':arrTabNames:n1);
       Endif;
       TbNmRcdPnl = strTabNames;

       outRTCD = *blanks;
       outSTS  = *blanks;
       outCO   = %editc(intco#:'X');
       outSUB# = %editc(UsrDftSub:'X');
       outFND  = %editc(FNfnd:'X');
       GroupType(outRTCD:outCO:outSUB#:outFND:outSTS);

       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Group Control information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateControl  b
     dValidateControl  pi              n
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       ClrErrControl();

       //*** Medical Case Control Information ***//
       //CM Nurse Heading
       If scNurseHdg = *blanks;
          erNurseHdg = *on;
          msNurseHdg = 'MCR0503';
          isError = *on;
       Endif;

       //Case Deletion
       If scCaseDel  = *zeros;
          erCaseDel  = *on;
          msCaseDel  = 'MCR0556';
          isError = *on;
       Endif;

       //Case Transfer
       If scCaseTrf  = *zeros;
          erCaseTrf  = *on;
          msCaseTrf  = 'MCR0556';
          isError = *on;
       Endif;

       //Override Case Denied
       If scCaseDen  = *zeros;
          erCaseDen  = *on;
          msCaseDen  = 'MCR0556';
          isError = *on;
       Endif;

       //Security Type 1
       If scSecTyp01 = *blanks;
          erSecTyp01 = *on;
          msSecTyp01 = 'MCR0503';
          isError = *on;
       Endif;

       //Security Type 2
       If scSecTyp02 = *blanks;
          erSecTyp02 = *on;
          msSecTyp02 = 'MCR0503';
          isError = *on;
       Endif;

       //Security Type 3
       If scSecTyp03 = *blanks;
          erSecTyp03 = *on;
          msSecTyp03 = 'MCR0503';
          isError = *on;
       Endif;

       //Remark Code for Review Date Diary
       If scRmkCd  = *blanks;
          erRmkCd  = *on;
          msRmkCd  = 'MCR0503';
          isError = *on;
       Endif;

       //Provider Bill Control Information
       //Medical examiner heading
       If scMedExHdg = *blanks;
          erMedExHdg = *on;
          msMedExHdg = 'MCR0503';
          isError = *on;
       Endif;

       //Medical Examiner User
       If scMEUsr = *blanks;
          erMEUsr = *on;
          msMEUsr = 'MCR0503';
          isError = *on;
       Endif;

       //MRA Reductions Heading
       If scMRedHdg = *blanks;
          erMRedHdg = *on;
          msMRedHdg = 'MCR0503';
          isError = *on;
       Endif;

       //Other Reductions 1 Heading
       If scORed1Hdg= *blanks;
          erORed1Hdg= *on;
          msORed1Hdg= 'MCR0503';
          isError = *on;
       Endif;

       //Other Reductions 2 Heading
       If scORed2Hdg= *blanks;
          erORed2Hdg= *on;
          msORed2Hdg= 'MCR0503';
          isError = *on;
       Endif;

       //Network Reductions Heading
       If scNtRedHdg= *blanks;
          erNtRedHdg= *on;
          msNtRedHdg= 'MCR0503';
          isError = *on;
       Endif;

       //Delete EOMB with Check
       If scChkDel  = *zeros;
          erChkDel  = *on;
          msChkDel  = 'MCR0556';
          isError = *on;
       Endif;

       //Secuirty Level 04
       If scSecTyp04 = *blanks;
          erSecTyp04 = *on;
          msSecTyp04 = 'MCR0503';
          isError = *on;
       Endif;

       //EOMB Recon Security
       If scRecSecLv = *zeros;
          erRecSecLv = *on;
          msRecSecLv = 'MCR0556';
          isError = *on;
       Endif;

       //Secuirty Level 05
       If scSecTyp05 = *blanks;
          erSecTyp05 = *on;
          msSecTyp05 = 'MCR0503';
          isError = *on;
       Endif;

       //Remark Code for EOBs
       If scRmkCdEob = *blanks;
          erRmkCdEob = *on;
          msRmkCdEob = 'MCR0503';
          isError = *on;
       Endif;

       //Complete or Pend
       If scCmpPnd  = *blanks;
          erCmpPnd  = *on;
          msCmpPnd  = 'MCR0503';
          isError = *on;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *on;
       Else;
          return *off;
       Endif;

      /End-Free
     pValidateControl  e

       //===================================================================
       // Validate Eomb Information
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateEomb    b
     d ValidateEomb    pi              n
     d strmode                        1
     d isError         s               n   inz

      /Free

        // Initialize all the messageId and error indicators.
        isError = *off;
        erEombDate = *off;
        msEombDate = *blanks;
        erComm1    = *off;
        msComm1    = *blanks;

        //Comment1 cannot be blank
        If scComm1 = *blanks;
           erComm1 = *on;
           msComm1 = 'MCR0503';
           isError = *on;
        Endif;

        If strmode = 'A';
           xxDate1 = dtCheck(scEombDate);
           If xxDate1 = 0;
              isError = *on;
              erEombDate = *on;
              msEombDate = 'MCR0500';
           Endif;
        Endif;

        // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateEomb    e

      //===================================================================
      // Validate the Group Primary information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateGroup   b
     d ValidateGroup   pi              n
     d isError         s               n   inz
     d isError1        s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       ClrErrGroup();

      // Group name cannot be blank.
       If scName = *blanks;
          erName = *on;
          msName = 'MCR0503';
          isError = *on;
       Endif;

      // Short name cannot be blank.
       If scShnm = *blanks;
          erShnm = *on;
          msShnm = 'MCR0503';
          isError = *on;
       Endif;

      // Enter complete info for Mailing address as this is mandatory
       If scMad1 = *Blanks;
          erMad1 = *on;
          msMad1 = 'MCR0503';
          isError = *on;
       Endif;
       If scMcty = *blanks;
          erMcty = *on;
          msMcty = 'MCR0503';
          isError = *on;
       Endif;
       If scMst  = *blanks;
          erMst  = *on;
          msMst  = 'MCR0503';
          isError = *on;
       Endif;
       If scMzip = *zeros;
          erMzip = *on;
          msMzip = 'MCR0556';
          isError = *on;
       Endif;

      // Enter complete info for Physical address, if any of the field is entered
       If scPad1 <> *blanks or scPad2 <> *blanks or
       scPcty <> *blanks or scPst <> *blanks or scPzip <> 0;
          If scPad1 = *Blanks;
             erPad1 = *on;
             msPad1 = 'MCR0503';
             isError = *on;
          Endif;
          If scPcty = *blanks;
             erPcty = *on;
             msPcty = 'MCR0503';
             isError = *on;
          Endif;
          If scPst  = *blanks;
             erPst  = *on;
             msPst  = 'MCR0503';
             isError = *on;
          Endif;
          If scPzip = *zeros;
             erPzip = *on;
             msPzip = 'MCR0556';
             isError = *on;
          Endif;
       Endif;

      // Validation Check for Effective Date
       xxDate1 = dtCheck(scEfDta);
       If xxDate1 = 0;
          isError = *on;
          erEfDta = *on;
          erEfDta = 'MCR0500';
       Endif;

      // Validation Check for Expiration Date
       If scExDta <> 0;
          xxDate1 = dtCheck(scExDta);
          If xxDate1 = 0;
             isError = *on;
             erExDta = *on;
             msExDta = 'MCR0500';
          Endif;
       Endif;

       // Dates must be in Ascending order
       If scEfDta <> 0 and scExDta <> 0;
          If erExDta = *off and erEfDta = *off;
             beginDate =  DtCYMD(scEfDta);
             endDate   =  DtCYMD(scExDta);
             If endDate < beginDate;
                erEfDta = *on;
                msEfDta = 'MCR0529';
                erExDta = *on;
                msExDta = 'MCR0529';
             Endif;
          Endif;
       Endif;

      // Adjudication State must be entered
       If scAdjcSt = *blanks;
          erAdjcst  = *on;
          msAdjcst  = 'MCR0503';
          isError = *on;
       Endif;

      // if Alternate Address flag is Y then name and address must be entered
      // Enter complete info for Alternate Name and address,
      // if any of the field is entered
       If scAnam <> *blanks or scAad1 <> *blanks or scAad2 <> *blanks or
        scActy <> *blanks or scAst <> *blanks or scAzip <> 0 or scAltFlg = 'Y';
          If scAnam = *Blanks;
             erAnam = *on;
             msAnam = 'MCR0503';
             isError = *on;
          Endif;
          If scAad1 = *Blanks;
             erAad1 = *on;
             msAad1 = 'MCR0503';
             isError = *on;
          Endif;
          If scActy = *blanks;
             erActy = *on;
             msActy = 'MCR0503';
             isError = *on;
          Endif;
          If scAst  = *blanks;
             erAst  = *on;
             msAst  = 'MCR0503';
             isError = *on;
          Endif;
          If scAzip = *zeros;
             erAzip = *on;
             msAzip = 'MCR0556';
             isError = *on;
          Endif;
          If scAltFlg = 'Y';
             If scAltPhone = *zeros;
                erAltPhone = *on;
                msAltPhone = 'MCR0556';
                isError = *on;
             Endif;
          Endif;
       Endif;

      // If Not Standalone then WCMS Group Number must be validate
       If scStdAlone <> 'Y' and visWCGrp = *on;
          If scWCGrpE = *zeros and isNpwc <> *on;
             erAWCGrp = *on;
             msAWCGrp = 'MCR0556';
             isError = *on;
          Endif;
          If scWCGrpE <> *zeros;
             If isWcif = *on and isWcin = *on and isNpwc <> *on;
                ##co# = intco#;
                ##fnd = scWCGrpE;
                ##rtcd= '0';
                GetCoFnd(##co#:scWCGrpE:##rtcd);
                If ##rtcd = '1';
                   erAWCGrp = *on;
                   msAWCGrp = 'MCR0508';  //Invalid Group Number entered.
                   isError = *on;
                Endif;
             Endif;
          Endif;
          If scWCGrpE <> *zeros;
             Chain (intco#:scWCGrpE) pfn01;
             If %found(fmfnl1) and Fnfnd <> scFnd;
                erAWCGrp = *on;
                msAWCGrp = 'MCR0320';  //X-REF group number must be unique.
                isError = *on;
             Endif;
          Endif;
       Else;
          If scWCGrpE <> *zeros and isNpwc <> *on and visWCGrp = *on;
             erAWCGrp = *on;
             msAWCGrp = 'MCR0801';     // field must be left blank
             isError = *on;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *on;
       Else;
          return *off;
       Endif;

      /End-Free
     p ValidateGroup   e

      //===================================================================
      // Validate the Group State information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateState   b
     d ValidateState   pi              n
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       ClrErrState();

       //WCMC Group State Information
       //FL state Information
       If scInsCdNum <> *zeros or scInsFedID <> *zeros or
          scInszip <> *zeros  or scTPACdNum <> *zeros or
          scTPAFedID<> *zeros  or scTPAZip <> *zeros;
          //Insurer code number
          If scInsCdNum = *zeros;
             erInsCdNum = *on;
             msInsCdNum = 'MCR0556';
             isError = *on;
          Endif;

         //Insurer Federal Tax ID
          If scInsFedID = *zeros;
             erInsFedID = *on;
             msInsFedID = 'MCR0556';
             isError = *on;
          Endif;

         //Insurer Location Zip
          If scInszip  = *zeros;
             erInszip  = *on;
             msInszip  = 'MCR0556';
             isError = *on;
          Endif;

         //Service co/TPA code#
          If scTPACdNum = *zeros;
             erTPACdNum = *on;
             msTPACdNum = 'MCR0556';
             isError = *on;
          Endif;

         //Service co/TPA fed Tax ID
          If scTPAFedID = *zeros;
             erTPAFedID = *on;
             msTPAFedID = 'MCR0556';
             isError = *on;
          Endif;

         //Service co/TPA Loc zip
          If scTPAzip  = *zeros;
             erTPAzip  = *on;
             msTPAzip  = 'MCR0556';
             isError = *on;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *on;
       Else;
          return *off;
       Endif;

      /End-Free
     p ValidateState   e

      //===================================================================
      // WrtUpdFdf3p    -  Move screen fields to Fdf3p file
      //===================================================================
     p WrtUpdFdf3p     b
     d WrtUpdFdf3p     pi              n
     d strmode                        1
      /Free

       F3efdt  =  dtCYMD(scEombDate);       // Effective date
       F3stat  =  scEombStsE;               // Status
       dsLine1 =  scComm1;
       dsLine2 =  scComm2;
       F3ec01  =  PartA;
       F3ec02  =  PartB + PartC;
       F3ec03  =  PartD;

       dsLine1 =  scComm3;
       dsLine2 =  scComm4;
       F3ec04  =  PartA;
       F3ec05  =  PartB + PartC;
       F3ec06  =  PartD;

       dsLine1 =  scComm5;
       dsLine2 =  scComm6;
       F3ec07  =  PartA;
       F3ec08  =  PartB + PartC;
       F3ec09  =  PartD;

       dsLine1 =  scComm7;
       dsLine2 =  scComm8;
       F3ec10  =  PartA;
       F3ec11  =  PartB + PartC;
       F3ec12  =  PartD;

       dsLine1 =  scComm9;
       dsLine2 =  scComm10;
       F3ec13  =  PartA;
       F3ec14  =  PartB + PartC;
       F3ec15  =  PartD;

       dsLine1 =  scComm11;
       dsLine2 =  scComm12;
       F3ec16  =  PartA;
       F3ec17  =  PartB + PartC;
       F3ec18  =  PartD;

       dsLine1 =  scComm13;
       dsLine2 =  scComm14;
       F3ec19  =  PartA;
       F3ec20  =  PartB + PartC;
       F3ec21  =  PartD;

       dsLine1 =  scComm15;
       dsLine2 =  scComm16;
       F3ec22  =  PartA;
       F3ec23  =  PartB + PartC;
       F3ec24  =  PartD;

       dsLine1 =  scComm17;
       dsLine2 =  scComm18;
       F3ec25  =  PartA;
       F3ec26  =  PartB + PartC;
       F3ec27  =  PartD;

       dsLine1 =  scComm19;
       dsLine2 =  scComm20;
       F3ec28  =  PartA;
       F3ec29  =  PartB + PartC;
       F3ec30  =  PartD;

       F3user  = q1user;                // User
       F3pgm   = q1pgm;                 // Program
       F3time  = Timeto6(SystemTime()); // Time
       F3dt    = Dateto7(SystemDate()); // Date Mintained
       If strmode ='A';
          F3co#   =  inCo#;
          F3sub#  =  inSub;
          F3fnd   =  inFnd;
          F3iusr  = q1user;                // Initial User
          F3ipgm  = q1pgm;                 // Initial Program
          F3itim  = Timeto6(SystemTime()); // Initial Time
          F3idt   = Dateto7(SystemDate()); // Initial Date
       Endif;
       Return strmode;

      /End-Free
     p WrtUpdFdf3p     e
