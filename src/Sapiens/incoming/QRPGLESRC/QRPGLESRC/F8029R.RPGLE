     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  F8029R - Upload Part 1 Corvel Translator
     H*
     H*  Description: This pgm will upload bills for Corvel
     H*
     H*  Created    : 10/21/2016  Programmer: 212  Project: 89MBT
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  07/19/17 C:079 Support : 89/48388                            *
     H*                 Use Pay to provider on the check              *
     H*                                                               *
     H*  12/21/16 C:212 Support : 44779                               *
     H*                  Load U1DFC as 8A                             *
     H*                                                               *
     H*****************************************************************
    F*
    f* CorVel flat file
     ffad0p     if   e           k disk
    f*
    f* CorVel flat file
     ffad1p     uf a e           k disk
    f*
    f* CorVel header file
     ffad2l     uf a e           k disk    rename(pU101:pD200)
    f*
    f* CorVel detail file
     ffad3l     uf a e           k disk    rename(pU201:pD300)
    f*
    f* CorVel history file
     ffad9l     uf a e           k disk    rename(pD901:pD900)
    f*
    f* CorVel extract files
     ffad00p    uf a e           k disk    rename(pD0001:pD000p)
     ffad10p    uf a e           k disk    rename(pD1001:pD100p)
     ffad11p    uf a e           k disk    rename(pD1101:pD110p)
     ffad37p    uf a e           k disk    rename(pD3701:pD370p)
     ffad12p    uf a e           k disk    rename(pD1201:pD120p)
     ffad13p    uf a e           k disk    rename(pD1301:pD130p)
     ffad36p    uf a e           k disk    rename(pD3601:pD360p)
     ffad15p    uf a e           k disk    rename(pD1501:pD150p)
     ffad16p    uf a e           k disk    rename(pD1601:pD160p)
     ffad18p    uf a e           k disk    rename(pD1801:pD180p)
     ffad25p    uf a e           k disk    rename(pD2501:pD250p)
     ffad30p    uf a e           k disk    rename(pD3001:pD300p)
     ffad50p    uf a e           k disk    rename(pD5001:pD500p)
     ffad51p    uf a e           k disk    rename(pD5101:pD510p)
     ffad52p    uf a e           k disk    rename(pD5201:pD520p)
     ffad90p    uf a e           k disk    rename(pD9001:pD900p)
     ffad99p    uf a e           k disk    rename(pD9901:pD990p)
    f*
     fststl     if   e           k disk
    D*
     d** Procedure definitions
     d*
     d BillHeader      pr
     dBillingProvider  pr
     d BillLevelAdj    pr
     d BillLine        pr
     d BillTrailer     pr
     d Comments        pr
     d ExtractHeader   pr
     d ExtractTrailer  pr
     d GetBilTyp       pr             1
     d  inTypBil                      3
     d ICDDiagnosis    pr
     d ICDDiagnosis2   pr
     d ICDProcedure    pr
     d LineComment     pr
     d LineReasMod     pr
     d NPI             pr
     d Patient         pr
     d UBHeader        pr
     d UBHeader2       pr

     d WrtD1           pr
     d WrtD2           pr
     d WrtD3           pr
     d WrtD9           pr

     d Initialize      pr

     d** Data Structures
     d*
     d D2DSin        e ds                  extname(fad2p:*input) inz
     d D3DSin        e ds                  extname(fad3p:*input) inz

     d D2DS            ds                  likerec(pD200:*output) inz
     d D3DS            ds                  likerec(pD300:*output) inz
     d D9DS            ds                  likerec(pD900:*output) inz
     d*
     d D00DS           ds                  likerec(pD000p:*output) inz
     d D10DS           ds                  likerec(pD100p:*output) inz
     d D11DS           ds                  likerec(pD110p:*output) inz
     d D37DS           ds                  likerec(pD370p:*output) inz
     d D12DS           ds                  likerec(pD120p:*output) inz
     d D13DS           ds                  likerec(pD130p:*output) inz
     d D36DS           ds                  likerec(pD360p:*output) inz
     d D15DS           ds                  likerec(pD150p:*output) inz
     d D16DS           ds                  likerec(pD160p:*output) inz
     d D18DS           ds                  likerec(pD180p:*output) inz
     d D25DS           ds                  likerec(pD250p:*output) inz
     d D30DS           ds                  likerec(pD300p:*output) inz
     d D50DS           ds                  likerec(pD500p:*output) inz
     d D51DS           ds                  likerec(pD510p:*output) inz
     d D52DS           ds                  likerec(pD520p:*output) inz
     d D90DS           ds                  likerec(pD900p:*output) inz
     d D99DS           ds                  likerec(pD990p:*output) inz
     d*
     d                 ds
     d D0data                      1000
     d ExtId                          8    overlay(D0data:1)
     d TrID                          12    overlay(D0data:*next)
     d SiteCd                         3    overlay(TrID:1)
     d BilNo                          8    overlay(TrID:4)
     d BilSeq                         1    overlay(TrID:12)
     d LineNo                         3    overlay(D0data:*next)
     d RecTyp                         2    overlay(D0data:*next)
     d Filler                       975    overlay(D0data:*next)
     d*
     d                 ds
     d DTime                         20  0
     d CDateIso                       8  0 overlay(DTime)
     d CTimeHms                       6  0 overlay(DTime:*next)
     d*
     D** Standalone variables
     D*
     d indWrtD2        s               n   inz(*off)
     d indWrtD3        s               n   inz(*off)
     d indWrtD9        s               n   inz(*off)
     d TrIdKy          s                   like(U1TrId) inz
     d xxcnt00         s              5s 0 inz
     d xxcnt10         s              5s 0 inz
     d xxcnt11         s              5s 0 inz
     d xxcnt37         s              5s 0 inz
     d xxcnt12         s              5s 0 inz
     d xxcnt13         s              5s 0 inz
     d xxcnt36         s              5s 0 inz
     d xxcnt15         s              5s 0 inz
     d xxcnt16         s              5s 0 inz
     d xxcnt18         s              5s 0 inz
     d xxcnt25         s              5s 0 inz
     d xxcnt30         s              5s 0 inz
     d xxcnt50         s              5s 0 inz
     d xxcnt51         s              5s 0 inz
     d xxcnt52         s              5s 0 inz
     d xxcnt90         s              5s 0 inz
     d xxcnt99         s              5s 0 inz

     d kklin#          s                   like(U2Lin#) inz
     d kkkey           s                   like(STkey) inz
     d*
    C*
      /free
        //================================================================
        //mainline
        //================================================================

        // initialisation routine
        initialize();

        // read input data
        Setll *start fad0p;
        Read pD001;
        DoW not %eof(fad0p);

           // initialize values based on record read
           TrIdky = TrId;

           Select;

            When RecTyp = '00';

               // call proc to extract header info
               ExtractHeader();

            When RecTyp = '10';

               // call proc to extract bill header info
               BillHeader();

            When RecTyp = '11';

               // call proc to extract ICD diagnosis info
               ICDDiagnosis();

            When RecTyp = '37';

               // call proc to extract ICD diagnosis 2 info
               ICDDiagnosis2();

            When RecTyp = '12';

               // call proc to extract ICD procedure file
               ICDProcedure();

            When RecTyp = '13';

               // call proc to extract UB header info
               UBHeader();

            When RecTyp = '36';

               // call proc to extract UB header 2 info
               UBHeader2();

            When RecTyp = '15';

               // call proc to extract comments info
               Comments();

            When RecTyp = '16';

               // call proc to extract billing provider info
               BillingProvider();

            When RecTyp = '18';

               // call proc to extract patient info
               Patient();

            When RecTyp = '25';

               // call proc to extract NPI info
               NPI();

            When RecTyp = '30';

               // call proc to extract bill level adjustment info
               BillLevelAdj();

            When RecTyp = '50';

               // call proc to extract bill line info
               BillLine();

            When RecTyp = '51';

               // call proc to extract line reason/modifier info
               LineReasMod();

            When RecTyp = '52';

               // call proc to extract line comment info
               LineComment();

            When RecTyp = '90';

               // call proc to bill trailer info
               BillTrailer();

            When RecTyp = '99';

               // call proc to extract trailer info
               ExtractTrailer();

            Other;

           EndSl;

           // call proc to extract header info
           // ClearVal();

           // write data to files
           WrtD2();
           WrtD3();
           WrtD9();

           // read next record
           Read pD001;
        EndDo;

        // write data into D1 file
        WrtD1();

        *inlr = *on;

        //==================================================================
        // end of mainline
        //==================================================================
      /End-Free

        //================================================================
        // BillHeader - extract bill header information
        //================================================================
     P BillHeader      B
     D BillHeader      PI
      /Free

        // extract data for D10
        D10DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D10DS.D10SiteCd;
        %subst(U1TrId:4:8)  =  D10DS.D10BilNo;
        %subst(U1TrId:12:1) =  D10DS.D10BilSeq;
        U1Clm#  = D10DS.D10ClmId;
        // prefix group number
        U1Clm#  = '900' + '-' + %trim(U1Clm#);
        // get and validate state
        U1ASt = D10DS.D10ReprSt;
        kkkey = U1ASt;
        Chain(n) kkkey pst01;
        If %found(ststl);
        Else;
           // if state not found in file, default it
           U1ASt = 'UC';
        EndIf;
        Monitor;
        U1CrDt  = %dec(%char(%date(D10DS.D10BURcvDt:*iso0):*iso0):8:0);
        On-Error;
        U1CrDt  = 00010101;
        EndMon;
        Monitor;
        U1DOI   = %dec(%char(%date(D10DS.D10DOI:*iso0):*iso0):8:0);
        On-Error;
        U1DOI   = 00010101;
        EndMon;
        U1BTyp  = GetBilTyp (D10DS.D10TypBil);
        U1Acct  = D10DS.D10PtCtlNo;
        Monitor;
        U1FrDt  = %dec(%char(%date(D10DS.D10FSrvDt:*iso0):*iso0):8:0);
        On-Error;
        U1FrDt  = 00010101;
        EndMon;
        Monitor;
        U1ToDt  = %dec(%char(%date(D10DS.D10TSrvDt:*iso0):*iso0):8:0);
        On-Error;
        U1ToDt  = 00010101;
        EndMon;
        Monitor;
        U1CsDt  = %dec(%char(%date(D10DS.D10BilDt:*iso0):*iso0):8:0);
        On-Error;
        U1CsDt  = 00010101;
        EndMon;
        U1BAdj  = D10DS.D10BilRsn;
        U1IDoc  = D10DS.D10DCN;

        // default network
        If U1ntwk = *blanks;
           U1Ntwk = 'CORVL';
        EndIf;

        // write into extract files
        Write pD100p D10DS;

        xxcnt10 += 1;
        indWrtD2 = *on;

      /End-Free
     P BillHeader      E

        //================================================================
        // BillingProvider - extract billing provider information
        //================================================================
     PBillingProvider  B
     DBillingProvider  PI
      /Free

        // extract data for D16
        D16DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D16DS.D16SiteCd;
        %subst(U1TrId:4:8)  =  D16DS.D16BilNo;
        %subst(U1TrId:12:1) =  D16DS.D16BilSeq;
        Monitor;
        U1Fein = %dec(D16DS.D16PrTxID:9:0);
        On-Error;
        U1Fein  = *zeros;
        EndMon;
        //*** Beg Del *** 07/19/17 ***************************************
        //***If D16DS.D16PrFNam <> *blanks;
        //***   U1PNam = D16DS.D16PrLNam;
        //***   U1PFsN = D16DS.D16PrFNam;
        //***Else;
        //***   U1PNam = D16DS.D16PayToNm;
        //***EndIf;
        //*** End Del *** 07/19/17 ***************************************
        //*** Beg Add *** 07/19/17 ***************************************
        If D16DS.D16PayToNm <> *blanks;
           U1PNam = D16DS.D16PayToNm;
           U1PFsN = *blanks;
        Else;
           U1PNam = D16DS.D16PrLNam;
           U1PFsN = D16DS.D16PrFNam;
        EndIf;
        //*** End Add *** 07/19/17 ***************************************
        U1Adr1 = D16DS.D16PrBlAd1;
        U1Cty  = D16DS.D16PrBlCty;
        U1St   = D16DS.D16PrBlSt;
        U1Zip  = D16DS.D16PrBlZip;
        U1SpCd = D16DS.D16PrSplCd;

        // write into extract files
        Write pD160p D16DS;

        xxcnt16 += 1;
        indWrtD2 = *on;

      /End-Free
     PBillingProvider  E

        //================================================================
        // BillLevelAdj - extract bill level adjustment information
        //================================================================
     P BillLevelAdj    B
     D BillLevelAdj    PI
      /Free

        // extract data for D30
        D30DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D30DS.D30SiteCd;
        %subst(U1TrId:4:8)  =  D30DS.D30BilNo;
        %subst(U1TrId:12:1) =  D30DS.D30BilSeq;
        U1BAdj = D30DS.D30RsnCd;
        U1BACD = D30DS.D30RsnCdDs;
        U1BAGP = D30DS.D30AdjGrp;
        U1BACD = D30DS.D30RsnCdDs;
        If U1BACD = *blanks;
           U1BACD = D30DS.D30LinCom;
        EndIf;

        // write into extract files
        Write pD300p D30DS;

        xxcnt30 += 1;
        indWrtD2 = *on;

      /End-Free
     P BillLevelAdj    E

        //================================================================
        // BillLine - extract bill Line information
        //================================================================
     P BillLine        B
     D BillLine        PI
      /Free

        // extract data for D50
        D50DS = D0data;

        // populate data files fields

        // get bill type
        U1BTyp = GetBilTyp (D10DS.D10TypBil);

        // defualt to M if still blanks
        If U1BTyp = *blanks;
           U1BTyp = 'M';
        EndIf;
        If D10DS.D10TypBil = 'OTH' or
           D10DS.D10TypBil = 'RO';
           If D50DS.D50Type = 'NDC';
              U1BTyp = 'P';
           EndIf;
           If D50DS.D50Type = 'REV';
              U1BTyp = 'H';
           EndIf;
           If D50DS.D50Type = 'PRO';
              If %Subst(D50DS.D50ProcCd:1:1) = 'D';
                 U1BTyp = 'D';
              Else;
                 U1BTyp = 'M';
              EndIf;
           EndIf;
        EndIf;

        U2Type = '2';
        %subst(U2TrId:1:3)  =  D50DS.D50SiteCd;
        %subst(U2TrId:4:8)  =  D50DS.D50BilNo;
        %subst(U2TrId:12:1) =  D50DS.D50BilSeq;
        Monitor;
        U2Lin# = %dec(D50DS.D50LineNo:8:0);
        On-Error;
        U2Lin#  = *zeros;
        EndMon;
        Monitor;
        U2SvDt = %dec(%char(%date(D50DS.D50SrvDt:*iso0):*iso0):8:0);
        On-Error;
        U2SvDt  = 00010101;
        EndMon;

        // proc code population
        U2Code = D50DS.D50ProcCd;

        // rev code population
        If U1BTyp = 'H';
           U2RvCd = D50DS.D50ProcCd;
        EndIf;

        // NDC code population
        If U1BTyp = 'P';
           U2NDC = D50DS.D50ProcCd;
        EndIf;

        U2CDsc = D50DS.D50ProcDs;
        // Units are coming in as 9 char with 7,2 data so picking first 7
        Monitor;
        U2Unts = %dec(%subst(D50DS.D50Units:1:7):7:0);
        On-Error;
        U2Unts  = *zeros;
        EndMon;
        U2PoS  = D50DS.D50PlcSrv;
        U2Chrg = D50DS.D50ActChg;
        U2Mra  = D50DS.D50StdRed;
        U2NDis = D50DS.D50PPORed;
        U2Paid = D50DS.D50AlwFee;
        U2DXR  = D50DS.D50DiXRef;
        U2RxMQ = D50DS.D50QtyDis;
        U2RX#  = D50DS.D50RX#;
        U2NWRF = D50DS.D50Refill;
        U2Cert = D50DS.D50Cert;
        U2RNPI = D50DS.D50PhNPI;
        U2BPrc = D50DS.D50BlLinCd;

        // write into extract files
        Write pD500p D50DS;

        xxcnt50 += 1;
        indWrtD2 = *on;
        indWrtD3 = *on;

      /End-Free
     P BillLine        E

        //================================================================
        // BillTrailer - extract bill trailer information
        //================================================================
     P BillTrailer     B
     D BillTrailer     PI
      /Free

        // extract data for D90
        D90DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D90DS.D90SiteCd;
        %subst(U1TrId:4:8)  =  D90DS.D90BilNo;
        %subst(U1TrId:12:1) =  D90DS.D90BilSeq;
        Monitor;
        U1Cost = %int(D90DS.D90CostBl)/100;
        On-Error;
        U1Cost  = *zeros;
        EndMon;
        // Since amounts below are coming in 13 char and to be moved in
        // a 11 char field, dropping off first two digits assuming values
        // are not coming in that big
        If %subst(D90DS.D90ActChr:1:1) = '-';
           U1Chrg = '-' + %subst(D90DS.D90ActChr:4:10);
        Else;
           U1Chrg = %subst(D90DS.D90ActChr:3:11);
        EndIf;
        If %subst(D90DS.D90StdRed:1:1) = '-';
           U1MRA  = '-' + %subst(D90DS.D90StdRed:4:10);
        Else;
           U1MRA  = %subst(D90DS.D90StdRed:3:11);
        EndIf;
        If %subst(D90DS.D90PPORed:1:1) = '-';
           U1NDis = '-' + %subst(D90DS.D90PPORed:4:10);
        Else;
           U1NDis = %subst(D90DS.D90PPORed:3:11);
        EndIf;
        If %subst(D90DS.D90AlwFee:1:1) = '-';
           U1Paid = '-' + %subst(D90DS.D90AlwFee:4:10);
        Else;
           U1Paid = %subst(D90DS.D90AlwFee:3:11);
        EndIf;

        // write into extract files
        Write pD900p D90DS;

        xxcnt90 += 1;
        indWrtD2 = *on;

      /End-Free
     P BillTrailer     E

        //================================================================
        // Comments - extract comments information
        //================================================================
     P Comments        B
     D Comments        PI
      /Free

        // extract data for D15
        D15DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D15DS.D15SiteCd;
        %subst(U1TrId:4:8)  =  D15DS.D15BilNo;
        %subst(U1TrId:12:1) =  D15DS.D15BilSeq;
        U1EDs1 = D15DS.D15ComTxt;

        // write into extract files
        Write pD150p D15DS;

        xxcnt15 += 1;
        indWrtD2 = *on;

      /End-Free
     P Comments        E

        //================================================================
        // ExtractHeader - extract header information
        //================================================================
     P ExtractHeader   B
     D ExtractHeader   PI
      /Free

        // extract data for D00
        D00DS = D0data;

        // populate data files fields

        D9DS.D9ExtId    =  D00DS.D00ExtId;
        D9DS.D9SiteCd   =  D00DS.D00SiteCd;
        D9DS.D9BilNo    =  D00DS.D00BilNo;
        D9DS.D9BilSeq   =  D00DS.D00BilSeq;
        D9DS.D9LineNo   =  D00DS.D00LineNo;
        D9DS.D9RecTyp   =  D00DS.D00RecTyp;
        D9DS.D9ExtDt    =  D00DS.D00ExtDt;
        D9DS.D9ExtTm    =  D00DS.D00ExtTm;
        D9DS.D9ExtVer   =  D00DS.D00ExtVer;
        D9DS.D9ExtFNm   =  D00DS.D00ExtFNm;
        D9DS.D9ExtRun   =  D00DS.D00ExtRun;
        D9DS.D9Filler   =  D00DS.D00Filler;

        // write into extract files
        Write pD000p D00DS;

        xxcnt00 += 1;
        indWrtD9 = *on;

      /End-Free
     P ExtractHeader   E

        //================================================================
        // ExtractTrailer - extract trailer information
        //================================================================
     P ExtractTrailer  B
     D ExtractTrailer  PI
      /Free

        // extract data for D99
        D99DS = D0data;

        // populate data files fields

        D9DS.D9ExtId   = D99DS.D99ExtId;
        D9DS.D9SiteCd  = D99DS.D99SiteCd;
        D9DS.D9BilNo   = D99DS.D99BilNo;
        D9DS.D9BilSeq  = D99DS.D99BilSeq;
        D9DS.D9LineNo  = D99DS.D99LineNo;
        D9DS.D9RecTyp  = D99DS.D99RecTyp;
        D9DS.D9RcTyp10 = D99DS.D99RcTyp10;
        D9DS.D9RcTyp11 = D99DS.D99RcTyp11;
        D9DS.D9RcTyp12 = D99DS.D99RcTyp12;
        D9DS.D9RcTyp13 = D99DS.D99RcTyp13;
        D9DS.D9RcTyp14 = D99DS.D99RcTyp14;
        D9DS.D9RcTyp15 = D99DS.D99RcTyp15;
        D9DS.D9RcTyp16 = D99DS.D99RcTyp16;
        D9DS.D9RcTyp17 = D99DS.D99RcTyp17;
        D9DS.D9RcTyp18 = D99DS.D99RcTyp18;
        D9DS.D9RcTyp19 = D99DS.D99RcTyp19;
        D9DS.D9RcTyp50 = D99DS.D99RcTyp50;
        D9DS.D9RcTyp51 = D99DS.D99RcTyp51;
        D9DS.D9RcTyp52 = D99DS.D99RcTyp52;
        D9DS.D9RcTyp90 = D99DS.D99RcTyp90;
        D9DS.D9TCostBl = D99DS.D99TCostBl;
        D9DS.D9TActChr = D99DS.D99TActChr;
        D9DS.D9TStdRed = D99DS.D99TStdRed;
        D9DS.D9TPPORed = D99DS.D99TPPORed;
        D9DS.D9TAlwFee = D99DS.D99TAlwFee;
        D9DS.D9TAddChr = D99DS.D99TAddChr;
        D9DS.D9TPPDisc = D99DS.D99TPPDisc;
        D9DS.D9TInvAmt = D99DS.D99TInvAmt;
        D9DS.D9RcTyp23 = D99DS.D99RcTyp23;
        D9DS.D9RcTyp24 = D99DS.D99RcTyp24;
        D9DS.D9RcTyp20 = D99DS.D99RcTyp20;
        D9DS.D9RcTyp22 = D99DS.D99RcTyp22;
        D9DS.D9Filler1 = D99DS.D99Filler1;
        D9DS.D9Filler2 = D99DS.D99Filler2;
        D9DS.D9Filler3 = D99DS.D99Filler3;
        D9DS.D9RsvMny  = D99DS.D99RsvMny;
        D9DS.D9RcTyp25 = D99DS.D99RcTyp25;
        D9DS.D9RcTyp21 = D99DS.D99RcTyp21;
        D9DS.D9RcTyp30 = D99DS.D99RcTyp30;
        D9DS.D9RcTyp31 = D99DS.D99RcTyp31;
        D9DS.D9RcTyp32 = D99DS.D99RcTyp32;
        D9DS.D9RcTyp33 = D99DS.D99RcTyp33;
        D9DS.D9RcTyp34 = D99DS.D99RcTyp34;
        D9DS.D9RcTyp35 = D99DS.D99RcTyp35;
        D9DS.D9RcTyp36 = D99DS.D99RcTyp36;
        D9DS.D9RcTyp37 = D99DS.D99RcTyp37;
        D9DS.D9RcTyp80 = D99DS.D99RcTyp80;
        D9DS.D9Filler  = D99DS.D99FILLER4;

        // write into extract files
        Write pD990p D99DS;

        xxcnt99 += 1;
        indWrtD9 = *on;

      /End-Free
     P ExtractTrailer  E

        //================================================================
        // GetBilTyp - get type of bill
        //================================================================
     P GetBilTyp       B
     D GetBilTyp       PI             1
     D  inTypBil                      3

     D outBilTyp       s              1    inz
      /Free

        // get type of bill derived from the code received

        outBilTyp = *blanks;
        Select;

         When inTypBil = 'AMB';
          outBilTyp = 'M';

         When inTypBil = 'ASC';
          outBilTyp = 'H';

         When inTypBil = 'CHI';
          outBilTyp = 'M';

         When inTypBil = 'CIQ';
          outBilTyp = 'M';

         When inTypBil = 'DEN';
          outBilTyp = 'D';

         When inTypBil = 'DME';
          outBilTyp = 'P';

         When inTypBil = 'EXP';
          outBilTyp = *blanks;

         When inTypBil = 'HI';
          outBilTyp = 'H';

         When inTypBil = 'HO';
          outBilTyp = 'H';

         When inTypBil = 'LAB';
          outBilTyp = 'M';

         When inTypBil = 'LIR';
          outBilTyp = *blanks;

         When inTypBil = 'MCB';
          outBilTyp = *blanks;

         When inTypBil = 'MHP';
          outBilTyp = 'M';

         When inTypBil = 'ML';
          outBilTyp = *blanks;

         When inTypBil = 'ORT';
          outBilTyp = 'M';

         When inTypBil = 'OTH';
          outBilTyp = *blanks;

         When inTypBil = 'OUT';
          outBilTyp = 'M';

         When inTypBil = 'PHA';
          outBilTyp = 'P';

         When inTypBil = 'PT';
          outBilTyp = 'M';

         When inTypBil = 'RO';
          outBilTyp = *blanks;

         When inTypBil = 'SNF';
          outBilTyp = 'H';

         When inTypBil = 'SUR';
          outBilTyp = 'M';

         When inTypBil = 'VIS';
          outBilTyp = 'M';

        Other;
          outBilTyp = *blanks;

        EndSl;

        return outBilTyp;

      /End-Free
     P GetBilTyp       E

        //================================================================
        // ICDDiagnosis - extract ICD diagnosis information
        //================================================================
     P ICDDiagnosis    B
     D ICDDiagnosis    PI
      /Free

        // extract data for D11
        D11DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D11DS.D11SiteCd;
        %subst(U1TrId:4:8)  =  D11DS.D11BilNo;
        %subst(U1TrId:12:1) =  D11DS.D11BilSeq;
        If D11DS.D11ICDInd <> *blanks;
           If D11DS.D11ICDInd = 'Y';
             Monitor;
              U1ICD1001 = %replace('':D11DS.D11Dia1:%scan('.':D11DS.D11Dia1):1);
             On-error;
              U1ICD1001 = D11DS.D11Dia1;
             EndMon;
             Monitor;
              U1ICD1002 = %replace('':D11DS.D11Dia2:%scan('.':D11DS.D11Dia2):1);
             On-error;
              U1ICD1002 = D11DS.D11Dia2;
             EndMon;
             Monitor;
              U1ICD1003 = %replace('':D11DS.D11Dia3:%scan('.':D11DS.D11Dia3):1);
             On-error;
              U1ICD1003 = D11DS.D11Dia3;
             EndMon;
             Monitor;
              U1ICD1004 = %replace('':D11DS.D11Dia4:%scan('.':D11DS.D11Dia4):1);
             On-error;
              U1ICD1004 = D11DS.D11Dia4;
             EndMon;
             Monitor;
              U1ICD1005 = %replace('':D11DS.D11Dia5:%scan('.':D11DS.D11Dia5):1);
             On-error;
              U1ICD1005 = D11DS.D11Dia5;
             EndMon;
             Monitor;
              U1ICD1006 = %replace('':D11DS.D11Dia6:%scan('.':D11DS.D11Dia6):1);
             On-error;
              U1ICD1006 = D11DS.D11Dia6;
             EndMon;
             Monitor;
              U1ICD1007 = %replace('':D11DS.D11Dia7:%scan('.':D11DS.D11Dia7):1);
             On-error;
              U1ICD1007 = D11DS.D11Dia7;
             EndMon;
             Monitor;
              U1ICD1008 = %replace('':D11DS.D11Dia8:%scan('.':D11DS.D11Dia8):1);
             On-error;
              U1ICD1008 = D11DS.D11Dia8;
             EndMon;
           Else;
             Monitor;
              U1ICD1 = %replace('':D11DS.D11Dia1:%scan('.':D11DS.D11Dia1):1);
             On-error;
              U1ICD1 = D11DS.D11Dia1;
             EndMon;
             Monitor;
              U1ICD2 = %replace('':D11DS.D11Dia2:%scan('.':D11DS.D11Dia2):1);
             On-error;
              U1ICD2 = D11DS.D11Dia2;
             EndMon;
             Monitor;
              U1ICD3 = %replace('':D11DS.D11Dia3:%scan('.':D11DS.D11Dia3):1);
             On-error;
              U1ICD3 = D11DS.D11Dia3;
             EndMon;
             Monitor;
              U1ICD4 = %replace('':D11DS.D11Dia4:%scan('.':D11DS.D11Dia4):1);
             On-error;
              U1ICD4 = D11DS.D11Dia4;
             EndMon;
             Monitor;
              U1ICD5 = %replace('':D11DS.D11Dia5:%scan('.':D11DS.D11Dia5):1);
             On-error;
              U1ICD5 = D11DS.D11Dia5;
             EndMon;
             Monitor;
              U1ICD6 = %replace('':D11DS.D11Dia6:%scan('.':D11DS.D11Dia6):1);
             On-error;
              U1ICD6 = D11DS.D11Dia6;
             EndMon;
             Monitor;
              U1ICD7 = %replace('':D11DS.D11Dia7:%scan('.':D11DS.D11Dia7):1);
             On-error;
              U1ICD7 = D11DS.D11Dia7;
             EndMon;
             Monitor;
              U1ICD8 = %replace('':D11DS.D11Dia8:%scan('.':D11DS.D11Dia8):1);
             On-error;
              U1ICD8 = D11DS.D11Dia8;
             EndMon;
           EndIf;
        EndIf;

        U1ICD10Ind =  D11DS.D11ICDInd;

        // write into extract files
        Write pD110p D11DS;

        xxcnt11 += 1;
        indWrtD2 = *on;

      /End-Free
     P ICDDiagnosis    E

        //================================================================
        // ICDDiagnosis2 - extract ICD diagnosis 2 information
        //================================================================
     P ICDDiagnosis2   B
     D ICDDiagnosis2   PI
      /Free

        // extract data for D37
        D37DS = D0data;

        // write into extract files
        Write pD370p D37DS;

        xxcnt37 += 1;
        indWrtD2 = *off;

      /End-Free
     P ICDDiagnosis2   E

        //================================================================
        // ICDProcedure  - extract ICD procedure information
        //================================================================
     P ICDProcedure    B
     D ICDProcedure    PI
      /Free

        // extract data for D11
        D12DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D12DS.D12SiteCd;
        %subst(U1TrId:4:8)  =  D12DS.D12BilNo;
        %subst(U1TrId:12:1) =  D12DS.D12BilSeq;
        If D12DS.D12ICDInd = 'Y';
           U1PRC1001 = D12DS.D12PRC1;
           U1PRC1002 = D12DS.D12PRC2;
           U1PRC1003 = D12DS.D12PRC3;
           U1PRC1004 = D12DS.D12PRC4;
           U1PRC1005 = D12DS.D12PRC5;
           U1PRC1006 = D12DS.D12PRC6;
        Else;
           U1PRC1 = D12DS.D12PRC1;
           U1PRC2 = D12DS.D12PRC2;
           U1PRC3 = D12DS.D12PRC3;
           U1PRC4 = D12DS.D12PRC4;
           U1PRC5 = D12DS.D12PRC5;
        EndIf;

        Monitor;
        U1PCDt  = %dec(%char(%date(D12DS.D12PRC1Dt:*iso0):*iso0):8:0);
        On-Error;
        U1PCDt  = 00010101;
        EndMon;

        // write into extract files
        Write pD120p D12DS;

        xxcnt12 += 1;
        indWrtD2 = *on;

      /End-Free
     P ICDProcedure    E

        //================================================================
        // LineComment- extract line comment information
        //================================================================
     P LineComment     B
     D LineComment     PI
      /Free

        // extract data for D52
        D52DS = D0data;

        // populate data files fields

        U2Type = '2';
        %subst(U2TrId:1:3)  =  D52DS.D52SiteCd;
        %subst(U2TrId:4:8)  =  D52DS.D52BilNo;
        %subst(U2TrId:12:1) =  D52DS.D52BilSeq;
        Monitor;
        U2Lin# = %dec(D52DS.D52LineNo:8:0);
        On-Error;
        U2Lin#  = *zeros;
        EndMon;
        If U2EoD1 = *blanks;
           U2EoD1 = D52DS.D52LinCom;
        Else;
           If U2EoD2 = *blanks;
              U2EoD2 = D52DS.D52LinCom;
           Else;
              If U2EoD3 = *blanks;
                 U2EoD3 = D52DS.D52LinCom;
              EndIf;
           EndIf;
        EndIf;

        // write into extract files
        Write pD520p D52DS;

        xxcnt52 += 1;
        indWrtD3 = *on;

      /End-Free
     P LineComment     E

        //================================================================
        // LineReasMod - extract line reason/modifier information
        //================================================================
     P LineReasMod     B
     D LineReasMod     PI
      /Free

        // extract data for D51
        D51DS = D0data;

        // populate data files fields

        U2Type = '2';
        %subst(U2TrId:1:3)  =  D51DS.D51SiteCd;
        %subst(U2TrId:4:8)  =  D51DS.D51BilNo;
        %subst(U2TrId:12:1) =  D51DS.D51BilSeq;
        Monitor;
        U2Lin# = %dec(D51DS.D51LineNo:8:0);
        On-Error;
        U2Lin#  = *zeros;
        EndMon;
        U2EOB1 = D51DS.D51RsnCd1;
        U2EOD1 = D51DS.D51RsnCd1D;
        U2EOB2 = D51DS.D51RsnCd2;
        U2EOD2 = D51DS.D51RsnCd2D;
        U2EOB3 = D51DS.D51RsnCd3;
        U2EOD3 = D51DS.D51RsnCd3D;
        U2Mod  = D51DS.D51ModCd1;
        U2Mod2 = D51DS.D51ModCd2;
        U2Mod3 = D51DS.D51ModCd3;
        U2Mod4 = D51DS.D51ModCd4;
        U2SAG1 = D51DS.D51AdjCd1;
        U2SAG2 = D51DS.D51AdjCd3;
        U2SAG3 = D51DS.D51AdjCd3;

        // write into extract files
        Write pD510p D51DS;

        xxcnt51 += 1;
        indWrtD3 = *on;

      /End-Free
     P LineReasMod     E

        //================================================================
        // NPI - extract NPI information
        //================================================================
     P NPI             B
     D NPI             PI
      /Free

        // extract data for D25
        D25DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D25DS.D25SiteCd;
        %subst(U1TrId:4:8)  =  D25DS.D25BilNo;
        %subst(U1TrId:12:1) =  D25DS.D25BilSeq;
        U1ANPI = D25DS.D25AtNPI;
        U1TNPI = D25DS.D25OpNPI;
        U1RFNP = D25DS.D25RfNPI;

        // write into extract files
        Write pD250p D25DS;

        xxcnt25 += 1;
        indWrtD2 = *on;

      /End-Free
     P NPI             E

        //================================================================
        // Patient - extract patient information
        //================================================================
     P Patient         B
     D Patient         PI
      /Free

        // extract data for D18
        D18DS = D0data;

        // populate data files fields

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D18DS.D18SiteCd;
        %subst(U1TrId:4:8)  =  D18DS.D18BilNo;
        %subst(U1TrId:12:1) =  D18DS.D18BilSeq;
        U1SS#  = D18DS.D18PatSSN;
        U1CLsN = D18DS.D18PatLNm;
        U1CFsN = D18DS.D18PatFNm;

        // write into extract files
        Write pD180p D18DS;

        xxcnt18 += 1;
        indWrtD2 = *on;

      /End-Free
     P Patient         E

        //================================================================
        // UBHeader - extract UB header information
        //================================================================
     P UBHeader        B
     D UBHeader        PI
      /Free

        // extract data for D13

        D13DS = D0data;

        // populate data files fields

        // get bill type
        U1BTyp = GetBilTyp (D10DS.D10TypBil);

        // defualt to M if still blanks
        If U1BTyp = *blanks;
           U1BTyp = 'M';
        EndIf;
        If D10DS.D10TypBil = 'OTH' or
           D10DS.D10TypBil = 'RO';
           If D13DS.D13SrvTyp <> *blanks;
              U1BTyp = 'H';
           EndIf;
        EndIf;

        U1Type = '1';
        %subst(U1TrId:1:3)  =  D13DS.D13SiteCd;
        %subst(U1TrId:4:8)  =  D13DS.D13BilNo;
        %subst(U1TrId:12:1) =  D13DS.D13BilSeq;
        U1DcSt = D13DS.D13DschCd;
        Monitor;
        U1AdDt = %dec(%char(%date(D13DS.D13UBAdmDt:*iso0):*iso0):8:0);
        On-Error;
        U1AdDt  = 00010101;
        EndMon;
        U1Drg# = D13DS.D13DrgCd;
        //***** Beg Chg *** 12/21/16 *********************************
        Monitor;
        U1DFC = %char(%date(D13DS.D13FCnslDt:*iso0):*iso0);
        On-Error;
        U1DFC = '00010101';
        EndMon;

        //**** Monitor;
        //|* U1DFC  = %dec(%char(%date(D13DS.D13FCnslDt:*iso0):*iso0):8:0);
        //|* On-Error;
        //|* U1DFC   = 00010101;
        //**** EndMon;
        //***** End Chg *** 12/21/16 *********************************
        Monitor;
        U1DcDt = %dec(%char(%date(D13DS.D13DschDt:*iso0):*iso0):8:0);
        On-Error;
        U1DcDt  = 00010101;
        EndMon;
        Monitor;
        U1DcHr = %dec(D13DS.D13DschHr:2:0);
        On-Error;
        U1DcHr  = *zeros;
        EndMon;
        U1Mcar = D13DS.D13MediNo;
        U1BlTy = D13DS.D13SrvTyp;
        U1SvTy = D13DS.D13TypSrv;
        Monitor;
        U1AdHr = %dec(D13DS.D13AdmHr:2:0);
        On-Error;
        U1AdHr  = *zeros;
        EndMon;

        If D11DS.D11IcdInd = 'Y';
         Monitor;
         U1ADia1001 = %replace('':D13DS.D13AdmICD:%scan('.':D13DS.D13AdmICD):1);
         On-error;
         U1ADia1001 = D13DS.D13AdmICD;
         EndMon;
        Else;
         Monitor;
         U1ADDg = %replace('':D13DS.D13AdmICD:%scan('.':D13DS.D13AdmICD):1);
         On-error;
         U1ADDg = D13DS.D13AdmICD;
         EndMon;
        EndIf;

        U1AdSc = D13DS.D13AdmSrc;

        // write into extract files
        Write pD130p D13DS;

        xxcnt13 += 1;
        If U1BTyp = 'H';
           indWrtD2 = *on;
        Else;
           indWrtD2 = *off;
        EndIf;

      /End-Free
     P UBHeader        E

        //================================================================
        // UBHeader2 - extract UB header 2 information
        //================================================================
     P UBHeader2       B
     D UBHeader2       PI
      /Free

        // extract data for D36
        D36DS = D0data;

        // write into extract files
        Write pD360p D36DS;

        xxcnt36 += 1;
        indWrtD2 = *off;

      /End-Free
     P UBHeader2       E

        //================================================================
        // WrtD1 - write/update to file FAD1P
        //================================================================
     P WrtD1           B
     D WrtD1           PI
      /Free

        // write info into file

        // write the header info
        Setll *start fad2l;
        Read pD200;
        DoW not %Eof (fad2l);
           D1data = D2DSin;
           Write pD101;

           Read pD200;
        EndDo;

        // write the detail info
        Setll *start fad3l;
        Read pD300;
        DoW not %Eof (fad3l);
           D1data = D3DSin;
           Write pD101;

           Read pD300;
        EndDo;

      /End-Free
     P WrtD1           E

        //================================================================
        // WrtD2 - write/update to file FAD2P
        //================================================================
     P WrtD2           B
     D WrtD2           PI
      /Free

        // verify if entry exists then update else write info

        If indWrtD2 = *on;

           // default values
           if U1TrDt = *zeros;
              U1TrDt = %dec(%char(%date():*iso0):8:0);
           endif;

           // copy values in output DS to write into file
           D2DS = D2DSin;

           Chain (TrIDky) pD200;
           If %found (fad2l);
              Update pD200 D2DS;
           Else;
              Write pD200 D2DS;
           EndIf;

           // copy back current values back in input DS
           D2DSin = D2DS;

           indWrtD2 = *off;
        EndIf;

      /End-Free
     P WrtD2           E

        //================================================================
        // WrtD3 - write/update to file FAD3P
        //================================================================
     P WrtD3           B
     D WrtD3           PI
      /Free

        // verify if entry exists then update else write info

        If indWrtD3 = *on;

           // copy values in output DS to write into file
           D3DS = D3DSin;

           kklin# = U2Lin#;
           Chain (TrIDky:kklin#) pD300;
           If %found (fad3l);
              Update pD300 D3DS;
           Else;
              Write pD300 D3DS;
           EndIf;

           // copy back current values back in input DS
           D3DSin = D3DS;

           indWrtD3 = *off;
        EndIf;

      /End-Free
     P WrtD3           E

        //================================================================
        // WrtD9 - write/update to file FAD9P
        //================================================================
     P WrtD9           B
     D WrtD9           PI
      /Free

        // verify if entry exists then update else write info

        If indWrtD9 = *on;

           Chain (ExtId:SiteCd:BilNo:BilSeq:LineNo:RecTyp) pD900;
           If %found (fad9l);
              Update pD900 D9DS;
           Else;
              Write pD900 D9DS;
           EndIf;

           indWrtD9 = *off;
        EndIf;

      /End-Free
     P WrtD9           E

        //================================================================
        // Initialize - perform intialization activities
        //================================================================
     P Initialize      B
     D Initialize      PI
      /Free

        // initialize values etc.

        dtime = %dec(%timestamp());

      /End-Free
     P Initialize      E

