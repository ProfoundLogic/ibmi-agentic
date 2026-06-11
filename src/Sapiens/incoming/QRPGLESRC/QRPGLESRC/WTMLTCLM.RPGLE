       CTL-OPT COPYRIGHT('(c) 2022 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  WTMLTCLM  - Multi-claim Maintenance.                           *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program is used to maintain multi claims per accident     *
       //                                                                 *
       //                                                                 *
       //  Created: 02/03/21 C:180  Project:  WC2732                      *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //  06/10/22 C:291 Project: WC2852                                 *
       //                 Add an edit to not allow Claim to be added to   *
       //                 Multiple Claim if Cat# exist in CMcata.         *
       //                                                                 *
       //  03/29/22 C:062 Ticket: 101248(146)                             *
       //                 Add call to W0320R01 to update Cat#.            *
       //                                                                 *
       //  03/24/22 C:333 Project: SY434                                  *
       //                 - Update Grids for Sorting Filtering Upgrade    *
       //                                                                 *
       //******************************************************************
       Dcl-Pi *N;
        @@PRDS char(40);
        @@MODE char(1);
        @@RRNA packed (9:0) options(*nopass);
       End-Pi;

       Dcl-F wtmltclm  workstn handler('PROFOUNDUI(HANDLER)')
        sfile(sflMltClm:SflRrnMlt)
        sfile(sflMltCase:SflRRnCase);

       Dcl-F wtActPnl  workStn Handler('PROFOUNDUI(HANDLER)')
        include(RcdComPnl);

       Dcl-F smcol  disk usage(*input) keyed;
       Dcl-F wmfnl  disk usage(*input) keyed;
       Dcl-F wdf1l  disk usage(*input) keyed;
       Dcl-F wmupl  disk usage(*input) keyed;
       Dcl-F wmcdl  disk usage(*input) keyed;
       Dcl-F wmcmp  disk usage(*input) rename(pcm01:pcm04);
       Dcl-F wmcml  disk usage(*input:*update) keyed rename(pcm01:pcm03);
       Dcl-F wmcml1 disk usage(*input) keyed infds(DBFDSL1);
       Dcl-F wamcl1 disk usage(*input) keyed infds(DBFDS1);
       Dcl-F wamcp  disk usage(*input:*update:*output:*delete)
                    rename(pmc01:pmc02);
       Dcl-F wamcl2 disk usage(*input) keyed rename(pmc01:pmc03) prefix(Nx);
       Dcl-F sdacl  disk usage(*input) keyed;
       Dcl-F wamcl  disk usage(*input) keyed rename(pmc01:pmc04) infds(DBFDS4);

       //External Prototypes
       Dcl-Pr WTSEARCH  EXTPGM;
        @@SrchMode char(1);
        @@SrchCall char(1);
        @@SrchRrn  char(9);
        @@SrchCriteria likeds(dsSrchCrit);
       End-Pr;

       Dcl-Pr CheckSecurity  EXTPGM('SRCCSC');
        inUTYP char(1) const;
        inULVL char(3) const;
        inCTP1 char(1) const;
        inCLV1 char(3) const;
        inEXEC char(1) const;
        outRes char(1) const;
        inCTP2 char(1) const;
        inCTP3 char(1) const;
        inCTP4 char(1) const;
        inCTP5 char(1) const;
        inCTP6 char(1) const;
        inCLV2 char(3) const;
        inCLV3 char(3) const;
        inCLV4 char(3) const;
        inCLV5 char(3) const;
        inCLV6 char(3) const;
        inCTP7 char(1) const;
        inCTP8 char(1) const;
        inCTP9 char(1) const;
        inCTP0 char(1) const;
        inCLV7 char(3) const;
        inCLV8 char(3) const;
        inCLV9 char(3) const;
        inCLV0 char(3) const;
       End-Pr;

       Dcl-Pr ClaimSecurity  EXTPGM('WA067R');
        inCo#    char(3) const;
        inGroup  char(3) const;
        inYear   char(3) const;
        inCase   char(7) const;
        inUser   char(10) const;
        outRstr  char(1) const;
       End-Pr;

       //*** Begin Add *** 03/29/22 ****************************************
       Dcl-Pr AssignCat# EXTPGM('W0320R01');
        inCo#Dec  Like(MCco#)  const;
        inGrpDec  Like(MCfnd)  const;
        inYearDec Like(MCfyr)  const;
        inEmp#Dec Like(MCemp#) const;
        inDivDec  Like(MCdiv)  const;
        inClm#    Like(MCclm#) const;
       End-Pr;
       //*** End   Add *** 03/29/22 ****************************************

       //Copy Books
      /Free
      /Copy sPrDate
      /Copy sPrCompany
      /Copy wPrEmplr
      /Copy wPrGroup
      /Copy wPrUsrPrms
      /Copy wtSearchDs
      /Copy sPrAtrium
      /End-Free

       //Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;

       //File Information Data Structure
       Dcl-Ds DBFDS1 ;
         DBrrn1 int(10) pos(397);
       End-Ds;

       Dcl-Ds DBFDS4 ;
         DBrrn4 int(10) pos(397);
       End-Ds;

       Dcl-Ds DBFDSL1 ;
         DBrrnL1 int(10) pos(397);
       End-Ds;

       //Other data structures
       Dcl-Ds KeyWmcml1 likerec(pcm01:*key);
       Dcl-Ds KeyMltClm likerec(pmc01:*key);
       Dcl-Ds KeyWdf1l  likerec(pf101:*key);
       Dcl-Ds KeyWmcml  likerec(pcm03:*key);

       //Stand Alone
       Dcl-S intCo# packed(3:0);
       Dcl-S intFnd packed(3:0);
       Dcl-S intFyr packed(3:0) inz;
       Dcl-S intMultRrn packed(9:0) inz;
       Dcl-S DtZeroIso  date(*ISO);
       Dcl-S SysDate packed(7:0);
       Dcl-S SydTime packed(6:0);
       Dcl-S SavMcDesc like(MCdesc) inz;
       Dcl-S intCaseRrn packed(9:0) inz;
       Dcl-S intCMcase like(CMcase) inz;
       //*** Begin Add *** 03/29/22 ****************************************
       Dcl-S intCMemp# like(CMemp#) inz;
       Dcl-S intCMdiv  like(CMdiv)  inz;
       //*** End   Add *** 03/29/22 ****************************************
       Dcl-S PrvFnd packed(3:0)  inz;
       Dcl-S outRstr  char(1) inz;
       Dcl-S strCMcase like(MCcase) inz;

       //Indicator
       Dcl-S isValid ind inz(*Off);
       Dcl-S isRowSel ind inz(*Off);

       // Start Of Program
       Exsr ProgramStart;

       //Main Loop
       Dou ExitNow= *On;
          //Clear the Subfile
          ClearSubfiles();

          //Load Multiclaim subfile/grid
          LoadMltClm();

          //Load Cases subfile/grid
          LoadCases(intMultRrn);

          //Display Screens
          FormatScreens();

          //Process Screens
          Select;
          When TabClick = *on;
             tabClick = *off;
          When BtnAddMlt = *On;
             EditAddMltClm('A');
             BtnAddMlt = *Off;
          When BtnAddCase = *on;
             AddCase();
          When BtnSflRowC = *On;
             If CsrRrnMlt = 0;
                CsrRrnMlt = 1;
             Endif;
             Chain CsrRrnMlt sflMltClm;
             intMultRrn = scMultRrn;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
          Other;
             ProcSfl();
          Endsl;
          scFirst = '0';
          scRmvFlt = '0';
       Enddo;

       *InLr = *on;

       //*----------------------------------------------------------------
       //* Program Start                                                 *
       //*----------------------------------------------------------------
       Begsr ProgramStart;

       //Clear screens
       Clear RcdMltClm;
       Clear SflMltClm;
       Clear SflMltCase;
       TabIndex = *Zero;

       //Company
       intCo# = UserCo#(q1User);

       Chain Q1user PUP01;

       //Populate Action Panel with Company Name
       Clear scCompName;
       scCompName = GetCompNam(intCo#);

       If @@Mode = '1';
          scColOut    = '4';
          disAddMlt   = *On;
          disLnkEdit  = *On;
          disAddCase  = *On;
          disLnkDlt   = *On;
       Else;
          scColOut    = ' ';
          disAddMlt   = *Off;
          disLnkEdit  = *Off;
          disAddCase  = *Off;
          disLnkDlt   = *Off;
       Endif;


       //Check if called from tool bar or Action Panel
       If %parms() > 2;
          chain @@rrna pcm04;
          If %found(wmcmp);
             intCo# = CMco#;
             intFnd = CMfnd;
             intFyr  = CMfyr;
             intCMcase = CMcase;
             //*** Begin Add *** 03/29/22 *******************************
             intCMemp# = CMemp#;
             intCMdiv  = CMdiv;
             //*** End   Add *** 03/29/22 *******************************
             strCMcase = '0000000' + %Editc(CMcase:'X');
          Endif;

          //Action Panel
          If @@Mode = 'F';
             Chain (intCo#:intFnd:intFyr:strCMcase) pmc04;
             If %found(wamcl);
                scFirst = '1';
                scFndFlt = %char(MCfnd);
                scFyrFlt = %char(MCfyr);
                scClm#Flt = %char(MCclm#);
                intMultRrn = DBrrn4;
              Endif;
          Else;
              scFirst = '0';
              scRmvFlt = '1';
              FocusClm# = *On;
          Endif;
       Else;
          FocusClm# = *Off;
          scFirst = '0';
          scRmvFlt = '1';
       Endif;

       //Position ADD button
       If @@Mode = '1';
          leftBtnAdd = 725;
          leftAddCas = 725;
       Else;
          leftBtnAdd = 820;
          leftAddCas = 820;
       Endif;

       //Check Authority
       If @@Mode<> '1';        //Update modes overriden by user security
          disAddMlt  = CheckAuth('MultAddEdt');
          disLnkEdit = disAddMlt;
          disAddCase = CheckAuth('MultAddClm');
          disLnkDlt  = CheckAuth('MultDelClm');
          Select;
          When disLnkEdit = *On and disLnkDlt = *On;
             scColOut = '4';
             leftBtnAdd = 725;
             leftAddCas = 725;
          When disLnkEdit = *On;
             scColOut = '1';
             leftBtnAdd = 725;
          When disLnkDlt = *On;
             scColOut = '2';
             leftAddCas = 725;
          Endsl;
       Endif;


        TabHeight = 960;
        APnlHeight = TabHeight + 120;

       Endsr;
       //*----------------------------------------------------------------
       //* Format Screens                                                *
       //*----------------------------------------------------------------
       Dcl-Proc FormatScreens;
        Dcl-Pi *N;
        End-Pi;

        Select;
        When BtnAddMlt = *On or LnkEditMlt = *On;
           Exfmt RcdAddMlt;
        When BtnAddCase = *On;
           Exfmt RcdAddCase;
        When LnkDltCase = *On;
           Exfmt RcdDltCase;
        Other;
           Write RcdComPnl;
           Write RcdTabMlt;
           Exfmt RcdMltClm;
           Read RcdComPnl;
           Read RcdTabMlt;
        Endsl;
       End-Proc;
       //*----------------------------------------------------------------
       //* Check Authority                                               *
       //*----------------------------------------------------------------
       Dcl-Proc CheckAuth;
       Dcl-Pi *N char(1);
         inReqAuth char(10) const;
       End-Pi;

       Dcl-S strAuth char(1);
       Dcl-S inUTYP char(1);
       Dcl-S inULVL char(3);

       //Retrieve authority
       clear pac01;
       chain inReqAuth pac01;
       // Get user sec level/type
       inULVL = %Editc(GetUsrLvl(Q1user:inUTYP):'X');
       strAuth = '0';

       //Check Security
       CheckSecurity(inUTYP:inULVL:ACstp1:%Editc(ACslv1:'X'):'1':strAuth
        :ACstp2:ACstp3:ACstp4:ACstp5:ACstp6:%Editc(ACslv2:'X')
        :%Editc(ACslv3:'X'):%Editc(ACslv4:'X'):%Editc(ACslv5:'X')
        :%Editc(ACslv6:'X'):ACstp7:ACstp8:ACstp9:ACstp0:%Editc(ACslv7:'X')
        :%Editc(ACslv8:'X'):%Editc(ACslv9:'X'):%Editc(ACslv0:'X'));

        Return strAuth;
        Return strAuth;
       End-Proc;

       //*----------------------------------------------------------------
       //* Clear Subfiles                                                *
       //*----------------------------------------------------------------
       Dcl-Proc ClearSubfiles;
       Dcl-Pi *N;
       End-Pi;

       SflRrnMlt = 0;
       ClrSflMlt  = *on;
       DspSflMlt = *off;
       DspCtlMlt = *off;

       SflRrnCase = 0;
       ClrSflCase  = *on;
       DspSflCase = *off;
       DspCtlCase = *off;
       Write RcdMltClm;

       If scFirst = '1';
          isRowSel = *On;
       Else;
          isRowSel = *Off;
       Endif;
       End-Proc;
       //*----------------------------------------------------------------
       //* Load Multi claims subfile                                     *
       //*----------------------------------------------------------------
       Dcl-Proc LoadMltClm;
        Dcl-Pi *N;
        End-Pi;

        Dcl-S intSvCo# like(MCco#);
        Dcl-S intSvFnd like(MCfnd);
        Dcl-S intSvFyr like(MCfyr);
        Dcl-S intSvClm# like(MCclm#);


        ClrSflMlt  = *off;
        DspCtlMlt = *on;
        SflRrnMlt = 0;
        intSvCo# = *Zeros;
        intSvFnd = *Zeros;
        intSvFyr = *Zeros;
        intSvClm# = *Zeros;

        If intFyr = *Zero or @@Mode = '0' or @@Mode = 'F';
           Setll (intCo#) pmc01;
           Reade (intCo#) wamcl1;

           If not %eof(wamcl1);
              Exsr MovDbSc;
           Endif;

           Dow not %eof(wamcl1);
              If MCfnd <> intSvFnd or MCfyr <> intSvFyr or MCclm# <> intSvClm#;
                 Exsr MovDbSc;
              Endif;
              Reade (intCo#) wamcl1;
           Enddo;

        Else;
           Setll (intCo#:intFnd:intFyr:strCMcase) pmc04;
           Reade (intCo#:intFnd:intFyr:strCMcase) wamcl;
           If not %eof(wamcl);
              Exsr MovDbSc;
           Endif;

           Dow not %eof(wamcl);
              If MCfnd <> intSvFnd or MCfyr <> intSvFyr or MCclm# <> intSvClm#;
                 Exsr MovDbSc;
              Endif;
              Reade (intCo#:intFnd:intFyr:strCMcase) wamcl;
           Enddo;
        Endif;


        //If SflRrnMlt > *Zero;
           DspSflMlt = *On;
        //Else;
        //   DspSflMlt = *Off;
        //Endif;
        SflRrnMlt = 1;
       //*----------------------------------------------------------------
       //* Move fields from Database to Screen                           *
       //*----------------------------------------------------------------
       Begsr MovDbSc;
       scFndName=%EditC(MCfnd:'X') + '-' +
        %subst(GetGrpName(MCfnd):1:36);
       scFyr = MCfyr;
       scClm# = MCclm#;
       scDesc = MCdesc;
       Clear scRowSel;
       If intFyr = *Zero or @@Mode = '0' or @@Mode = 'F';
           scMultRrn = DBrrn1;
       Else;
           scMultRrn = DBrrn4;
           intMultRrn = DBrrn4;
       Endif;
       SflRrnMlt = SflRrnMlt + 1;
       If SflRrnMlt = 1 and intMultRrn = *Zero;
          intMultRrn = scMultRrn;
       Endif;

       If isRowSel = *Off;
          Select;
          When SflRrnMlt = CsrRrnMlt;
             scRowSel = 'A';
             isRowSel = *On;
          When scMultRrn = intMultRrn;
             scRowSel = 'A';
             isRowSel = *On;
          When SflRrnMlt = 1 and intMultRrn = 0;
             scRowSel = 'A';
             isRowSel = *On;
          Endsl;
       Endif;

       Write SflMltClm;
       intSvCo# = MCco#;
       intSvFnd = MCfnd;
       intSvFyr = MCfyr;
       intSvClm# = MCclm#;
       Endsr;
       End-Proc;
       //*----------------------------------------------------------------
       //* Load Cases Subfile                                            *
       //*----------------------------------------------------------------
       Dcl-Proc LoadCases;
        Dcl-Pi *N;
          @@RrnMc packed(9:0);
        End-Pi;

        Dcl-S intEmpRrn packed(9:0);
        Dcl-S intMcRrn packed(9:0);


        intMcRrn = @@RrnMc;
        intEmpRrn = *Zeros;
        ClrSflCase = *Off;

        Chain(n) intMcRRn Wamcp;
        If %found(Wamcp);
           KeyMltClm.MCco#  = MCco#;
           KeyMltClm.MCfnd  = MCfnd;
           KeyMltClm.MCfyr  = MCfyr;
           KeyMltClm.MCclm# = MCclm#;
           KeyWmcml1.CMco#  = MCco#;
           KeyWmcml1.CMfnd  = MCfnd;
           KeyWmcml1.CMfyr  = MCfyr;
           SavMcDesc        = MCdesc;
        Else;
           Clear KeyMltClm;
        Endif;

        Setll %kds(KeyMltClm:4) pmc01;
        If %equal(Wamcl1);
           Reade %kds(KeyMltClm:4) Pmc01;
           Dow not %eof(Wamcl1);
              KeyWmcml1.CMcase = %int(%subst(MCcase:8:7));
              chain %kds(KeyWmcml1) pcm01;
              If %found(Wmcml1);
                 scCase =  %Editc(CMfnd:'X') + '-' + %Editc(CMfyr:'X') +
                           '-' + %Editc(CMcase:'X');

                 ClaimSecurity(%editc(CMco#:'X'):%editc(CMfnd:'X'):
                               %editc(CMfyr:'X'):%editc(CMcase:'X'):
                                Q1User:outRstr);

                 scCaseUrl = *blanks;
                 scClmRrn = DBrrnL1;

                 If outRstr = '2';
                    scClaimant = '**Restricted**';
                 Else;
                    Chain CMss# pcd01;
                    If %Found(wmcdl);
                       scClaimant = %trim(CDlsn) + ', ' + %trim(CDfsn);
                       If CDmid <> *blank;
                          scClaimant = %trim(scClaimant) + ' ' + CDmid + '.';
                       Endif;
                     scCaseUrl = CreateURL('STDRVPGM':%subst(scClaimant:1:40):
                                 %Editc(scClmRrn:'X'):'D':'WTCLMSUM');
                    Else;
                       scClaimant = *blanks;
                    Endif;
                 Endif;

                 If CMacdt > *Zero;
                    scAcdt =  %date(CMacdt:*cymd);
                 Else;
                    scAcdt = DtZeroIso;
                 Endif;
                 intEmpRrn = GetEmpRrn(CMemp#:CMdiv:CMfnd:CMco#);
                 scEmpName = GetEmpName(CMemp#:CMdiv:intEmpRRn:CMfnd:CMco#);
                 scCaseRrn = DBrrn1;
                 SflRrnCase = SflRrnCase + 1;
                 Write SflMltCase;
              Endif;
              Reade %kds(KeyMltClm:4) Pmc01;
           Enddo;
        Endif;

        DspCtlCase = *On;
        DspSflCase =  *On;

        If SflRrnCase = *Zero;
           SflRrnCase = 1;
        Endif;

        If BtnSflRowC = *On;
           SflRrnMlt = CsrRrnMlt;
        Endif;

        BtnSflRowC = *Off;

       End-Proc;
       //*----------------------------------------------------------------
       //* Edit/Add Multi Claims                                         *
       //*----------------------------------------------------------------
       Dcl-Proc EditAddMltClm;
        Dcl-Pi *N;
         inProcMode char(1) const;
         inRrn packed(9:0) options(*nopass);
        End-Pi;

        Dcl-s intRrn packed(9:0);

        If %parms() > 1;
          intRrn = inRrn;
        Else;
           intRrn = *zeros;
        Endif;

        Clear RcdAddMlt;

        Select;
        When inProcMode = 'A';
           scHdgMlt = 'Add Multiple Claim';
           scFyr = UPfyr;
           scFnd = LoadGrpSel();
           If scFnd<>0;
              disFnd = *On;
           Else;
              disFnd = *Off;
           EndIf;
           visCaseN = *On;
           disFyr = *Off;
           disClm# = *Off;
           disCaseN = *Off;

           //Defaults if called from toolbar
           If intFyr > *Zero;
              scFnd = intFnd;
              scFyr = intFyr;
              scCaseN = intCMcase;
              If scClm# = *zero;
                 scClm# = GetNxtMclm#();
              Endif;
              disFnd = *On;
              disFyr = *On;
              disCaseN = *On;
           Endif;
        When inProcMode = 'C';
           scHdgMlt = 'Edit Multiple Claim';
           chain(n) intRrn pmc02;
           If %found(Wamcp);
              disFnd = *On;
              disFyr = *On;
              disClm# = *On;
              disCaseN = *On;
              visCaseN = *Off;
              scFnd = MCfnd;
              scFyr = MCfyr;
              scClm# = MCclm#;
              scDesc = MCdesc;
           Endif;

        Endsl;
        BtnCanMlt = *off;
        BtnSaveMlt= *off;


        //Capture data
        Dou BtnCanMlt = *on;
           FormatScreens();
           Select;
           When BtnNxtMclm = *On;
              If scClm# = *Zero or PrvFnd <> scFnd;
                 scClm# = GetNxtMclm#();
                 PrvFnd = scFnd;
                 Exsr ResetErrors;
              Endif;
           When BtnMltSrch = *On;
              Exsr ValMltClaim;
              If isValid = *On;
                 scCaseN = SearchClaim(scFnd:scFyr);
              Endif;
              BtnMltSrch = *Off;
           When BtnSaveMlt= *on;
              Exsr ValMltClaim;
              BtnSaveMlt = *off;
              If isValid = *on;
                 Exsr SavMltClaim;
                 Leave;
              Endif;
           When BtnCanMlt = *on;
              BtnCanMlt = *off;
              Clear RcdAddMlt;
              Leave;
           Other;
              Exsr ValMltClaim;
           Endsl;
        Enddo;
        BtnAddMlt = *Off;
       //*---------------------------------------------------------------
       //* Reset Errors                                                 *
       //*---------------------------------------------------------------
       Begsr ResetErrors;
       erFnd = *Off;
       erFyr = *Off;
       erClm# = *Off;
       erDesc = *Off;
       erCaseN = *Off;
       isValid = *On;
       Endsr;

       //*---------------------------------------------------------------
       //* Validate Multi Claim Data                                    *
       //*---------------------------------------------------------------
       Begsr ValMltClaim;
       Exsr ResetErrors;

       If scFnd = *Zero;
          mgFnd = 'WCR0498';        //Invalid group
          erFnd = *On;
          isValid = *Off;
       Endif;

       If scFyr = *Zero;
          mgFyr = 'WCR1392';        //Policy year required
          erFyr = *On;
          isValid = *Off;
       Endif;

       KeyWdf1l.F1co# = intCo#;
       KeyWdf1l.F1fnd = scFnd;
       KeyWdf1l.F1fyr = scFyr;     //Policy year invalid for group
       chain %kds(KeyWdf1l) PF101;
       If not %found(Wdf1l);
          mgFyr = 'WCR0517';
          erFyr = *On;
          isValid = *Off;
       Endif;

       If BtnMltSrch = *Off;
          If scClm# <= *Zero;
             mgClm#  = 'WCR0502';      //Multi claim cannot be zero
             erClm# = *On;
             isValid = *Off;
          Endif;

          If inProcMode = 'A';
             KeyMltClm.MCco#  = intCo#;
             KeyMltClm.MCFnd  = scFnd;
             KeyMltClm.MCFyr  = scFyr;
             KeyMltClm.MCclm# = scClm#;
             Setll %kds(KeyMltClm:4) pmc01;
             If %equal(Wamcl1);
               mgClm# = 'WCR0845';       //Mutli claim already exists
               erClm# = *on;
               isValid = *Off;
             Endif;
          Endif;

          If scDesc = *blank;
             mgDesc = 'WCR0520';      //Desc required
             erDesc = *On;
             isValid = *Off;
          Endif;

          Select;
          When inProcMode = 'A' and scCaseN <= *Zero;     //Case number cannot be zero
             mgCaseN = 'WCR0502';
             erCaseN = *On;
             isValid = *Off;

          When inProcMode = 'A' and scCaseN > *Zero;
             Chain (intCo#:scFnd:scFyr:scCaseN) pcm01;
             If not %found(wmcml1);
                mgCaseN = 'WCR0681';         //Case not found
                erCaseN = *On;
                isValid = *Off;
             Endif;
             //*** Beg Add ***** 06/10/22 ******************
             If %Found(wmcml1) and CMcata <> 0;
                mgCaseN = 'WC40177';         //Cat# exist in CMcata
                erCaseN = *On;
                isValid = *Off;
             Endif;
             //*** End Add ***** 06/10/22 ******************
          Endsl;

       Endif;

       Endsr;
       //*---------------------------------------------------------------
       //* Save Multi Claim Data                                        *
       //*---------------------------------------------------------------
       Begsr SavMltClaim;
       If inProcMode = 'A';
          Clear pmc02;
          MovScDb('A');
          write pmc02;
          Feod Wamcp;
          //*** Begin Add *** 03/29/22 **********************************
          AssignCat#(MCco#:MCfnd:MCfyr:MCemp#:MCdiv:MCclm#);
          //*** End   Add *** 03/29/22 **********************************
       Else;
          Chain(n) intRRn Wamcp;
          If %found(Wamcp);                 //Set update key
              KeyMltClm.MCco#  = MCco#;
              KeyMltClm.MCfnd  = MCfnd;
              KeyMltClm.MCfyr  = MCfyr;
              KeyMltClm.MCclm# = MCclm#;
             Exsr UpdMltCases;
          Else;
              LeaveSr;
          Endif;
       Endif;
       Endsr;
       //*---------------------------------------------------------------
       //* Update Multi Claim Cases                                     *
       //*---------------------------------------------------------------
       Begsr UpdMltCases;
       Setll %kds(KeyMltClm:4) pmc01;
       If %equal(Wamcl1);
          Reade %kds(KeyMltClm:4) Pmc01;
          Dow not %eof(Wamcl1);
             intRrn = Dbrrn1;
             Chain intRRn Wamcp;
             If %found(Wamcp);
                MovScDb('U');
                Update pmc02;
             Endif;
             Reade %kds(KeyMltClm:4) Pmc01;
          Enddo;
       Endif;
       Endsr;
       End-Proc;
       //*----------------------------------------------------------------
       //* Add Case to Multi claim                                       *
       //*----------------------------------------------------------------
       Dcl-Proc AddCase;
        Dcl-Pi *N;
        End-Pi;

        Dcl-S intRrn packed(9:0);


        Clear RcdAddCase;

        BtnCanCase = *Off;
        BtnSvCase  = *Off;
        BtnSvNcase = *Off;

        HdgAddCase = 'Add Claim to Multi Claim Number ' +
                      %Editc(KeyMltClm.MCclm#:'X');

        //Fund must match Multi Claim fund
        scCaseFnd = KeyMltClm.MCfnd;
        scCaseFyr = KeyMltClm.MCfyr;

        //Defaults if called from toolbar
        If intFyr > *Zero;
           scCaseCase = intCMcase;
           disCaseFnd = *On;
           disCaseFyr = *On;
           disCaseCas = *On;
           disSvNcase = *On;
           visSvNcase = *Off;
        Else;
           disSvNcase = *Off;
           visSvNcase = *On;
        Endif;


        disCaseFnd = *On;
        disCaseFyr = *On;

        //Capture data
        Dou BtnCanCase = *on;
           FormatScreens();
           Select;
           When BtnClmSrch = *On;
              Exsr ValCase;
              If isValid = *on;
                 scCaseCase = SearchClaim(scCaseFnd:scCaseFyr);
              Endif;
              BtnClmSrch = *Off;
           When BtnSvCase = *on;
              Exsr ValCase;
              BtnSvCase = *off;
              If isValid = *on;
                 Exsr SavCase;
                 Leave;
              Endif;
           When BtnSvNcase = *On;
              Exsr ValCase;
              BtnSvNcase = *off;
              If isValid = *on;
                 Exsr SavCase;
                 scCaseFnd = *Zeros;           //Clear prompt values for next add
                 scCaseFyr = *Zeros;
                 scCaseCase = *Zeros;
              Endif;
           When BtnCanCase = *on;
              BtnCanCase = *off;
              Clear RcdAddCase;
              Leave;
           Other;
              Exsr ValCase;
           Endsl;
        Enddo;
        BtnAddCase = *Off;
       //*---------------------------------------------------------------
       //* Validate Case                                                *
       //*---------------------------------------------------------------
       Begsr ValCase;
       erCaseFnd = *Off;
       erCaseFyr = *Off;
       erCase = *Off;
       isValid = *On;

       If scCaseFnd = *Zero;
          mgCaseFnd = 'WCR0498';        //Invalid group
          erCaseFnd = *On;
          isValid = *Off;
       Endif;

       If scCaseFyr = *Zero;
          mgCaseFyr = 'WCR1392';        //Policy year required
          erCaseFyr = *On;
          isValid = *Off;
       Endif;

       KeyWdf1l.F1co# = intCo#;
       KeyWdf1l.F1fnd = scCaseFnd;
       KeyWdf1l.F1fyr = scCaseFyr;     //Policy year invalid for group
       chain %kds(KeyWdf1l) PF101;
       If not %found(Wdf1l);
          mgCaseFyr = 'WCR0517';
          erCaseFyr = *On;
          isValid = *Off;
       Endif;

       If BtnClmSrch = *Off;
          If scCaseCase <= *Zero;
             mgCase  = 'WCR0502';        //Case number cannot be zero
             erCase = *On;
             isValid = *Off;
          Endif;

          KeyWmcml1.CMco#  = KeyMltClm.MCco#;
          KeyWmcml1.CMfnd  = scCaseFnd;
          KeyWmcml1.CMfyr  = scCaseFyr;
          KeyWmcml1.CMcase = scCaseCase;
          chain %kds(KeyWmcml1) PCM01;
          If not %found(Wmcml1);
             mgCase = 'WCR0681';         //Case not found
             erCase = *On;
             isValid = *Off;
          Endif;
          //*** Beg Add ***** 06/10/22 ******************
          If %Found(wmcml1) and CMcata <> 0;
             mgCase = 'WC40177';
             erCase = *On;
             isValid = *Off;
          Endif;
          //*** End Add ***** 06/10/22 ******************

          KeyMltClm.MCcase = '0000000' + %Editc(scCaseCase:'X');
          Setll %kds(KeyMltClm) pmc01;
          If %equal(Wamcl1);
            mgCase = 'WCR0845';            //Case already exists
            erCase = *on;
            isValid = *Off;
          Endif;
       Endif;
       Endsr;
       //*---------------------------------------------------------------
       //* Save Case                                                    *
       //*---------------------------------------------------------------
       Begsr SavCase;
          Clear pmc02;
          MovScDb('C');
          write pmc02;
          Feod Wamcp;
          //*** Begin Add *** 03/29/22 **********************************
          AssignCat#(MCco#:MCfnd:MCfyr:MCemp#:MCdiv:MCclm#);
          //*** End   Add *** 03/29/22 **********************************
          intMultRrn = *Zero;              //Reset subfile position
       Endsr;

       End-Proc;
       //*----------------------------------------------------------------
       //*  Move Screen fields to Database                               *
       //*----------------------------------------------------------------
       Dcl-Proc MovScDb;
        Dcl-Pi *N;
          inUpdType char(1) const;
        End-Pi;

       Select;
       When inUpdType ='A';                  //Add Multi claim
         MCco# = intCo#;
         MCfnd = scFnd;
         MCfyr = scFyr;
         MCclm# = scClm#;
         MCcase = '0000000' + %editc(scCaseN:'X');
         //*** Begin Add *** 03/29/22 *******************************
         KeyWmcml1.CMco#  = MCco#;
         KeyWmcml1.CMfnd  = MCfnd;
         KeyWmcml1.CMfyr  = MCfyr;
         KeyWmcml1.CMcase = scCaseN;
         chain %kds(KeyWmcml1) pcm01;
         MCemp# = CMemp#;
         MCdiv  = CMdiv;
         //*** End   Add *** 03/29/22 *******************************
         MCDesc = scDesc;
       When inUpdType ='U';                  //Update Multi claim
         MCDesc = scDesc;
       When inUpdType ='C';                  //Add case to Multi claim
         MCco# = intCo#;
         MCfnd = scCaseFnd;
         MCfyr = scCaseFyr;
         MCclm# = KeyMltClm.MCclm#;
         MCcase = KeyMltClm.MCcase;
         //*** Begin Add *** 03/29/22 *******************************
         KeyWmcml1.CMco#  = MCco#;
         KeyWmcml1.CMfnd  = MCfnd;
         KeyWmcml1.CMfyr  = MCfyr;
         KeyWmcml1.CMcase = scCaseCase;
         chain %kds(KeyWmcml1) pcm01;
         MCemp# = CMemp#;
         MCdiv  = CMdiv;
         //*** End   Add *** 03/29/22 *******************************
         MCDesc = SavMcDesc;
       Endsl;

       MCpgm  = Q1pgm;
       MCuser = Q1user;
       MCdt   = Dateto7(SystemDate());
       MCtime = Timeto6(SystemTime());

       End-Proc;
       //*----------------------------------------------------------------
       //* Delete Case                                                   *
       //*----------------------------------------------------------------
       Dcl-Proc DelCase;
        Dcl-Pi *N;
         inCaseRrn packed(9:0);
        End-Pi;

        chain(n) inCaseRrn pmc02;
        If %found(Wamcp);
           scMltClm# = KeyMltClm.MCclm#;
           scCaseDlt = scCase;
           scClaimntD = scClaimant;
           scInjDate = scAcDt;
           Exfmt RcdDltCase;
           If BtnDelete = *On;
              chain inCaseRrn pmc02;
              If %found(Wamcp);
                 Delete pmc02;
                 intMultRrn = *Zero;
                 BtnDelete = *Off;
                 KeyWmcml.CMco#  = MCco#;
                 KeyWmcml.CMfnd  = MCfnd;
                 KeyWmcml.CMemp# = MCemp#;
                 KeyWmcml.CMdiv  = MCdiv;
                 KeyWmcml.CMfyr  = MCfyr;
                 KeyWmcml.CMcase = %int(%subst(MCcase:8:7));
                 chain %kds(KeyWmcml) pcm03;
                 If %found(Wmcml);
                    CMcata = *zeros;
                    Update PCM03;
                 Endif;
              Endif;
           Endif;
        Endif;
       End-Proc;
       //*----------------------------------------------------------------
       //* Process Multi Claim Subfile                                   *
       //*----------------------------------------------------------------
       Dcl-Proc ProcSfl;
        Dcl-Pi *N;
        End-Pi;
        Readc SflMltClm;
        Dow not %eof;
           If LnkEditMlt = *On;
               intMultRrn = scMultRrn;
               EditAddMltClm('C':intMultRrn);
               LnkEditMlt = *Off;
               Update SflMltClm;
           Endif;
           Readc SflMltClm;
        Enddo;

        Readc SflMltCase;
        Dow not %eof;
           If LnkDltCase = *On;
               intCaseRrn = scCaseRrn;
               DelCase(intCaseRrn);
               LnkDltCase = *Off;
               Update SflMltCase;
           Endif;
           Readc SflMltCase;
        Enddo;
       End-Proc;

       //*----------------------------------------------------------------
       //* Load Group Selection                                          *
       //*----------------------------------------------------------------
       Dcl-Proc LoadGrpSel;
        Dcl-Pi *N packed(3:0);
        End-Pi;

        Dcl-s intFnd packed(3:0) inz;

        intFnd=GetGrpAuthSql(q1User:scFndField:scFndFile:scFndSel:'A');
        Return intFnd;
       End-Proc;
       //*----------------------------------------------------------------
       //* Search Claim                                                  *
       //*----------------------------------------------------------------
       Dcl-Proc SearchClaim;
        Dcl-Pi *N packed (7:0);
         inSrchFnd  packed(3:0);
         inSrchFyr  packed(3:0);
        End-Pi;
        Dcl-S strMode char(1) inz('C');
        Dcl-S strCall char(1) inz('2');
        Dcl-s strRrn char(9) inz('000000000');
        Dcl-S strCriteria char(1024) inz;
        Dcl-S intClmRrn packed(9:0) inz;
        Dcl-S intCaseFound packed(7:0) inz;

        Clear dsSrchCrit;
        Clear strCriteria;
        dsClmGrp = inSrchFnd;
        dsClmYr =  inSrchFyr;
        strCriteria = dsSrchCrit;
        strMode = 'C';
        strCall = '2';
        strRrn = '000000000';
        WTSEARCH(strMode:strCall:strRrn:strCriteria);
        If strRrn <> '000000000';
           intClmRrn = %Dec(strRrn:9:0);
           chain intClmRrn pcm04;
           If %found(Wmcmp);
              intCaseFound = CMcase;
           Endif;
        Endif;
        Return intCaseFound;
       End-Proc;
       //*----------------------------------------------------------------
       //* Get Next MultiClaim Number                                    *
       //*----------------------------------------------------------------
       Dcl-Proc GetNxtMclm#;
        Dcl-Pi *N packed(7:0);
        End-Pi;

        Dcl-S NxtMclm# packed(7:0);

        NxtMclm# = 1;

        Setgt (intCo#:scFnd) pmc03;
        Readpe (intCo#:scFnd) pmc03;
        If not %eof(wamcl2);
           NxtMclm# = NxMCclm# + 1;
        Endif;

        BtnNxtMclm = *Off;
        Return NxtMclm#;
       End-Proc;
