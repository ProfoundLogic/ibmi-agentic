     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H
     h DftActGrp(*No) ActGrp('CONNECT')
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     h/Title WIPOLICY - Portal Policy Requests
      *==========================================================
      * Description: Stores procedures to process various
      *              portal requests for policy JSON returns
      *==========================================================
      * Revisions:
     H*
     H*  11/04/25  C:65  Support: 120001 (Vital)
     H*                  Allow safety fee amount to be imported into a
     H*                  quote when called from web services.
     H*
     H*  09/09/25  C:342 Project : CN036
     H*                  Multi-Group Processing
     H*                  GetExcludeCompRatingFlag procedure - change to
     H*                  always exclude, regardless of flag value.
     H*
     H*  03/28/25  C:333 Support: 118734 (Vital)
     H*                  Experience Rating not being Set in GO on Quotes
     H*                  from Connect
     H*
     H*  10/21/24  C:344 Project: WC2952
     H*                  Increase work rate field to 8.3
     H*
     H*  09/04/24  C:79  Project: 102WJS
     H*                  Default Region, County and Rep from WMCYP
     H*
     H*  03/01/24  C:62  Ticket: 115168(140)
     H*                  Correct policy end date for leap year.
     H*
     H*  08/02/23  C:79  Support: 109162 (FMIC)
     H*                  Report current period correctly
     H*
     H*  09/14/22  C:62  Project: 133WSX
     H*                  Valen updates.
     H*
     H*  08/26/22  C:65  Support: 103396 (UBIC)
     H*                  Always write/update payroll as active.           *
     H*                                                                   *
     H*  07/11/22  C:33  Support: 102826
     H*                  SaveApp - Only process auto locations once per group
     H*
     H*  06/24/22  C:33  Support: 96456
     H*                  ListApp - Use different dates for app date range *
     H*                                                                   *
     H*  04/22/22  C:33  Support: 102038
     H*                  SaveApp - Class code must be numeric             *
     H*                                                                   *
     H*  04/21/22  C:33  Support: 102020
     H*                  SaveApp - Limit error messages returned          *
    H*                                                                   *
    H*  02/23/22  C:33  Support: 101183 {no comments}                    *
    H*                  Explicity close the ListPolicies cursor b4 open  *
    H*                                                                   *
    H*  02/08/22  C:33  Support: 100882 {no comments}                    *
    H*                  SaveApp - Load ELnwRw                            *
     H*
     H* 01/19/22 C:33  Project: WC2837
     H*                Extend net rate conditional
    H*                                                                   *
    H*  12/13/21  C:33  Support: 99718 {no comments}                     *
    H*                  ListApp - add UPstaf <> *blank in selection sql  *
    H*                                                                   *
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  Increase throughput                              *
      *
      * 11/15/21 C:62  Project: 146WJV
      *                -Default group marked as exclude, only process that
      *                 group.
      *                -If default group not excluded then only allow other
      *                 groups that are not exluded form being picked up.
      *
      * 10/05/21 C:65  Project: WC2819
      *                Return payroll net rates for net rate groups.
      *
      * 09/11/21 C:33  Account for user not authorized to employer
      *
      * 08/05/21 C:180 Project: WC2820
      *          C:33  Add Valen analytics.
      *
      * 04/12/21 C:136 Project: WC1986
      *                Adjust for EFT and 3rd Party in Quote and
      *                Default from St Rpt Div
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      * 02/03/20 C:136 Project: WC2187
      *                Populate maintenance date in Scriptura work files
      *
      *==========================================================
     fwdxsl     uf a e           k disk    rename(pxs01:pxs00)
     fwiacindi  o    e           k disk    Rename(pci01:pOwners) UsrOpn
     fwiaclocs  o    e           k disk    Rename(plo01:pLocs) UsrOpn
     fwiacmain  o    e           k disk    Rename(pac01:pMain) UsrOpn
     fwiacprcar o    e           k disk    Rename(ppc01:pPrLoss) UsrOpn
     fwiacqst   o    e           k disk    Rename(pqs01:pQuest) UsrOpn

     fwiacstprm o    e           k disk    Rename(psp01:pStPrem) UsrOpn
     fwiacstrat o    e           k disk    Rename(psr01:pStClas) UsrOpn
     fwiacsts   o    e           k disk    Rename(Pst01:pStList) UsrOpn
     f
     fwieml     uf   e           k disk    rename(pem01:pl100) prefix(L1:2)
     f                                     usropn
     fwieml1    if   e           k disk    rename(pem01:pl101) prefix(L1:2)
     f                                     usropn
     fwmagp     if   e             disk    Rename(pag01:pag0p) UsrOpn
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     f*** begin add ***  10/05/21  ****************************************
     fwdf1l     if   e           k disk    rename(pf101:pf100)
     f*** end add *****  10/05/21  ****************************************
     FXMLPREL1  UF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     infds(dbfds)
     F***  Begin Add  ***  01/19/22  *********************************
     FWTABL     if   e           k disk    ignore(pvs01)
     F***  End   Add  ***  01/19/22  *********************************
     F
     FWAQTP     uf a e             disk    rename(pqt01:pqt0p)    usropn
     FWDELP     uf a e             disk    rename(pel01:pel0p)    usropn
     FWDXSP     uf a e             disk    rename(pxs01:pxs0p)    usropn
     FWIAPP     uf a e             disk    rename(pap01:papip)    usropn
     FWIAPPANS  uf a e             disk    rename(pap01:papipans) usropn
     FWIAPPLOS  uf a e             disk    rename(pap01:papiplos) usropn
     FWIEMP     uf a e             disk    rename(pem01:pemip)    usropn
     FWIGSP     uf a e             disk    rename(pgs01:pgsip)    usropn
     FWIGSPDRV  uf a e             disk    rename(pgs01:pgsipdrv) usropn
     F***  Begin Add  ***  08/05/21  *************
     FWIVSP     uf a e             disk    rename(pvs01:pvsip)    usropn
     FWIVSPEXP  uf a e             disk    rename(pvs01:pvsipexp) usropn
     F***  End   Add  ***  08/05/21  *************
     FWMAHP     uf a e             disk    rename(pah01:pah0p)    usropn
     FWMALP     uf a e             disk    rename(pal01:pal0p)    usropn
     FWMECP     uf a e             disk    rename(pec01:pec0p)    usropn
     FWMEMP     uf a e             disk    rename(pem01:pem0p)    usropn
     FWMEPP     uf a e             disk    rename(pep01:pep0p)    usropn
     FWMLDP     uf a e             disk    rename(pld01:pld0p)    usropn
     FWMNBP     uf a e             disk    rename(pnb01:pnb0p)    usropn
     FWMOOP     uf a e             disk    rename(poo01:poo0p)    usropn

      *Entry
     dRequestPgm       pr                  ExtPgm('WIPOLICY')
     d                              256
     d                               10
     d                                 a   Len(8500000) Varying
     d                                 a   Len(8500000) Varying
     d                               10
     dRequestPgm       pi
     d inRequest                    256
     d inUser                        10
     d inParms                         a   Len(8500000) Varying
     d outJSON                         a   Len(8500000) Varying
     d ioRtnCode                     10

     dAgencyInfo       pr                  extpgm('WIAGENCY')
     d                              256    const
     d                               10    const
     d                                 a   len(8500000) varying
     d                                     options(*varsize) const
     d                                 a   len(8500000) varying
     d                               10    const

     dListExternal     pr                  extpgm(strProgram)
     d                              256    const
     d                               10    const
     d                                 a   len(5000) varying
     d                                 a   len(8500000) varying
     d                               10    const

     dListQuestions    pr                  extpgm('WIQUEST')
     d                              256    const
     d                               10    const
     d                                 a   len(5000) varying
     d                                 a   len(8500000) varying
     d                               10    const

     dListRatings      pr                  extpgm('WICARRAT')
     d                              256    const
     d                               10    const
     d                                 a   len(8500000) varying
     d                                     options(*varsize) const
     d                                 a   len(8500000) varying
     d                               10    const

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     D***  Begin Add  ***  12/09/21 ****************
     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D***  End   Add  ***  12/09/21 ****************

     d* File/field definitions
     ddsWAQTPdef       ds                  likerec(pqt0p:*all)
     ddsWDELPdef       ds                  likerec(pel0p:*all)
     ddsWDXSPdef       ds                  likerec(pxs0p:*all)
     ddsWIAPPdef       ds                  likerec(papip:*all)
     ddsWIAPPANSdef    ds                  likerec(papipans:*all)
     ddsWIAPPLOSdef    ds                  likerec(papiplos:*all)
     ddsWIEMPdef       ds                  likerec(pemip:*all)
     ddsWIGSPdef       ds                  likerec(pgsip:*all)
     ddsWIGSPDRVdef    ds                  likerec(pgsipdrv:*all)
     d***  Begin Add  ***  08/05/21  **********************
     ddsWIVSPdef       ds                  likerec(pvsip:*all)
     ddsWIVSPEXPdef    ds                  likerec(pvsipexp:*all)
     d***  End   Add  ***  08/05/21  **********************
     ddsWMAHPdef       ds                  likerec(pah0p:*all)
     ddsWMALPdef       ds                  likerec(pal0p:*all)
     ddsWMECPdef       ds                  likerec(pec0p:*all)
     ddsWMEMPdef       ds                  likerec(pem0p:*all)
     ddsWMEPPdef       ds                  likerec(pep0p:*all)
     ddsWMLDPdef       ds                  likerec(pld0p:*all)
     ddsWMNBPdef       ds                  likerec(pnb0p:*all)
     ddsWMOOPdef       ds                  likerec(poo0p:*all)

     ddsPrem           ds                  qualified inz
     d written                       13  2
     d earned                        13  2
     ddsLoss           ds                  qualified inz
     d incurred                      13  2
     d ratio                          9  2
     ddsInfo           ds                  qualified inz
     d premium                             likeds(dsPrem)
     d loss                                likeds(dsLoss)
     ddsInfoSingle     ds                  likeds(dsInfo) inz dim(100000)
     ddsInfoSum        ds                  likeds(dsInfo) inz

     d psds          esds                  ExtName(szq1p)

     dconSqlFileMismatch...
     d                 c                   '42802'
     d maxSize         c                   999
     d***  Begin Chg  ***  04/21/22  ****************
     d*array****       s               a   Len(32767)
     d array           s               a   Len(32767) varying
     d***  End   Chg  ***  04/21/22  ****************
     d clientID        s              3a
     d comma           s              1a
     d docNode         s                   Like(yajl_val)
     d errMsgArray     s            500    dim(maxSize) varying
     D i0              s             10i 0 inz
     D i1              s             10i 0 inz
     D i2              s             10i 0 inz
     D i3              s             10i 0 inz
     d inJson          s               a   Len(2000000) varying
     d intCo#          s              3  0
     d intUserCo#      s              3  0
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d isLossCalculated...
     d                 s              1n   inz('0')
     d isShowEmptyRate...
     d                 s              1n   inz('1')
     d isShowSql       s              1n
     d json            s               a   len(8500000) varying
     D key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d rrn9            s              9  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strCodeDesc     s            100a   varying
     d strCodeDescOth  s            100a   varying
     d strCodeExtract  s             25a   varying
     d strEntryPointGlobal...
     d                 s             10a   varying inz
     d strErrorMsg     s            500a   Varying
     d strEnv          s              4a
     d strJsonExtract600...
     d                 s            600a   varying
     d strProgram      s             10    inz
     d strSqlCursor    s               a   Len(32000) Varying
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d val             s                   Like(yajl_val)

     d decCount        s             10i 0 inz(0)
     d maxCount        s             10i 0 inz(500)
     d maxLen          s             10i 0 inz(32000)

      *** Begin Add *** 08/05/21 *******************************
     d intVndEfDt      s              7P 0 inz
      *** End   Add *** 08/05/21 *******************************

     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const

     d SRPRT1          pr                  extpgm('SRPRT1')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##fyr                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##date                         8    const
     d ##amount                      11
     d ##amount2                     11
     d ##term                         1    const

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)

     ddsSMCIP        e ds                  extname(SMCIP)
     ddsWMFIP        e ds                  extname(WMFIP)

     D dbfds           ds
     D  dbrrn                397    400b 0

     d dsDate          ds                  qualified
     d  dt                             d
     d  dt7                           7  0
     d  dt8                           8  0
     d  dt7Alpha                      7
     d  dt8Alpha                      8

     d dsDateRange     ds                  qualified
     d  from                               likeds(dsDate)
     d  to                                 likeds(dsDate)

     ddsEmployerIn     ds                  qualified inz
     d name                         100a   varying
     d dba                          100a   varying
     d fein                           9a
     d number                         9a
     d div                            5a
     d year                           3a

     ddsWDRRP        e ds                  extname(WDRRP) qualified inz

     d x#RRN           s              9  0 inz(0)

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

      /Copy sPrAtrium
      /Copy sPrCompany
      /Copy sPrDate
      /Copy sPrFiles
      /Copy sPrJson
      /copy sPrLetter
      /Copy sPrSecure
      /Copy sPrSql
      /Copy sPrString
      *Copy sPrUsrPrms
      /copy sPrXML
      /Copy wPrAgent
      /Copy wPrAR
       //*** beg add ***** 04/1221 *****************
      /Copy wPrBank
       //*** end add ***** 04/12/21 *****************
      /Copy wPrClaims
      /Copy wPrEmplr
      /Copy wPrGroup
      /Copy wPrProduce
      /Copy wPrUsrPrms
      /copy wPrRateSet
      /copy wPrSecure
      /copy wPrTable
      /Include yajl_h

      *==========================================================
      * Main
      *==========================================================
      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;
       Select;
       When inRequest = 'CheckFEIN';
          DupFein();
       When inRequest = 'Duplicate FEIN Check';
          DupFein();
       When inRequest = 'GetApplication';
          GetApplication();
       When inRequest = 'GetDivisionDetail';
          GetDivisionDetail();
       When inRequest = 'GetPolicyDetail';
          GetPolicyDetail();
       When inRequest = 'ListApplications';
          ListApps();
       When inRequest = 'ListEmployers';
          ListEmployers();
       When inRequest = 'ListLocations';
          ListLocations();
       When inRequest = 'ListQuoteStatusHistory';
          ListQuoteStatusHistory();
       When inRequest = 'ListPolicies';
          PolicySearch();
       When inRequest = 'ListPolicyDivisions';
          ListPolicyInfo();
       When inRequest = 'ListPolicyYears';
          ListPolicyInfo();
       When inRequest = 'ModifyPolicyEffectiveDates';
          ModifyPolicyEffectiveDates();
       When inRequest = 'PrintAcordForm';
          PrintAcord();
       When inRequest = 'SaveApplicationData';
          SaveAppData();
       Other;
          Exsr BuildStub;
       EndSl;
       If %open(WMAGP);
          Close WMAGP;
       Endif;
       *Inlr = *On;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(outJSON:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtnCode;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        intUserCo# = intCo#;
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
        RtvEnv(strEnv);
        isEnvQuote = strEnv = '*QTE';
        isEnvLive = strEnv = '*LIV';
       Endsr;
      /End-free
     p*===================================================================
     p* Build JSON for Gradient Driver List
     p*  (Called from BuildPolicyMasterInfo)
     p*===================================================================
     pBuildGradientDrivers...
     p                 b
     dBuildGradientDrivers...
     d                 pi              a   Len(500000) varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const

     DintCtr           s              3  0 Inz
     DstrJsonWIGSPDRV  s          64000a   varying
     DstrSqlWIGSPDRV   s            500
     ddsWIGSPDRV     e ds                  extname(WIGSPDRV) qualified
     doutJson          s               a   Len(500000) varying

      /Free

       Exsr BuildSqlWIGSPDRV;
       Exsr ProcessWIGSPDRV;

       Return outJson;

       //=================================================================
       // Build json nodes for gradient score drivers
       //=================================================================
       Begsr BuildJsonWIGSPDRV;

        //***  Begin Chg  ***  12/09/21  **************
        //strJsonWIGSPDRV = %Trim(strJsonWIGSPDRV) +
        // '{' +
        // %Trim(BuildJsonNode('heading':dsWIGSPDRV.GSdrvName)) + ',        ' +
        // %Trim(BuildJsonNode('value':%Char(dsWIGSPDRV.GSdrvVal))) +
        // '}';
          strJsonWIGSPDRV +=
           '{' +
           BuildJsonNode('heading':%trim(dsWIGSPDRV.GSdrvName)) + ',        ' +
           BuildJsonNode('value':%Char(dsWIGSPDRV.GSdrvVal)) +
           '}';
        //***  End   Chg  ***  12/09/21  **************

       Endsr;
       //=================================================================
       // Build SQL for WIGSPDRV (Gradient Drivers)
       //=================================================================
       Begsr BuildSqlWIGSPDRV;

        strSqlWIGSPDRV = 'Select * From WIGSPDRV' +
         ' Where GSco# = ' + %Char(inCo#) +
         ' And GSfnd = ' + %Char(inFnd) +
         ' And GSemp# = ' + %Char(inEmp#) +
         ' And GSfYr = ' + %Char(inFyr);

        Exec Sql Prepare sqlWIGSPDRV From :strSqlWIGSPDRV;
        Exec Sql Declare cursorWIGSPDRV Cursor For sqlWIGSPDRV;
        Exec Sql Open cursorWIGSPDRV;

       Endsr;
       //=================================================================
       // Process SQL to access WIGSPDRV
       //=================================================================
       Begsr ProcessWIGSPDRV;

        Reset intCtr;
        strJsonWIGSPDRV = '"driverList":[';
        Dou Not IsValidFetch(sqlStt);
           Exec SQL fetch next from cursorWIGSPDRV into :dsWIGSPDRV;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           intCtr += 1;
           If intCtr > 1;
            //***  Begin Chg  ***  12/09/21  **************
            //strJsonWIGSPDRV = %Trim(strJsonWIGSPDRV) + ', ';
              strJsonWIGSPDRV += ', ';
            //***  End   Chg  ***  12/09/21  **************
           Endif;
           Exsr BuildJsonWIGSPDRV;
        Enddo;

       //***  Begin Chg  ***  12/09/21  **************
       //strJsonWIGSPDRV = %trim(strJsonWIGSPDRV) + ']';
        strJsonWIGSPDRV += ']';
       //***  End   Chg  ***  12/09/21  **************
        Exec SQL close cursorWIGSPDRV;
        outJson         = strJsonWIGSPDRV;

       Endsr;
      /End-Free
     pBuildGradientDrivers...
     p                 e

     p*===================================================================
     p* Build JSON for policy detail information
     p*  (Called from GetApplication)
     p*===================================================================
     pBuildPolicyDetailInfo...
     p                 b
     dBuildPolicyDetailInfo...
     d                 pi              a   Len(500000) varying
     d inCo#                          3    const
     d inFnd                          3    const
     d inEmp#                         9    const
     d inDiv                          5    const
     d inFyr                          3    const options(*nopass)
     d* If no year passed, array of all years will be returned, latest first

     DdecCount         s             10i 0 inz
     DdecRrnWDELP      s              9  0 inz
     DflgQte           s              1a   inz('Q')
     dinJson           s               a   len(8500000) varying inz static
     dinName40         s             40    inz
     DisEofWDELP       s              1n   inz('0')
     DisEofWMEPP       s              1n   inz('0')
     DisEofWMLDP       s              1n   inz('0')
     DisEofWMWVP       s              1n   inz('0')
     D*** begin add ***  10/05/21  ***************************************
     DisNetRate        s              1n   inz('0')
     D*** end add *****  10/05/21  ***************************************
     DoutEnv           s              1a   inz
     DoutParms         s               a   len(10000) varying inz
     DrrnAA            s              9  0
     DrrnAG            s              9  0
     DrrnAH            s              9  0
     DstrJsonWMEPP     s          64000    inz
     DstrJsonWMLDP     s          64000    inz
     DstrJsonWMWVP     s          64000    varying inz
     DstrSqlWDELP      s            500
     DstrSqlWMEPP      s            500
     DstrSqlWMLDP      s            500
     DstrSqlWMWVP      s            500    varying inz
     DstrSt            s              2

     ddsWDELP        e ds                  extname(WDELP) qualified inz
     ddsWMAGP          ds                  likerec(pag0p:*input) inz
     ddsWMEPP        e ds                  extname(WMEPP) qualified inz
     ddsWMLDP        e ds                  extname(WMLDP) qualified inz
     ddsWMWVP        e ds                  extname(WMWVP) qualified inz

     d premPct         s             12  9
     ddsDeductible     ds                  qualified inz
     d type                          15    varying
     d premPct                       12p 9
     d amount                         9p 0
     d aggregate                      9p 0
     d lossSub                        2p 0
     d basis                          2p 0
     d netGross                       1
     d verb                           1
     d class                          5p 0
     d pct                            2p 0

     dclassDesc        s             30a
     dclassEffDt       s              7s 0
     dcontClass        s              1a
     dclassCode        s              5s 0
     dhazardGroup      s              5a
     dratingState      s              2a
     drateSetCode      s              3a

     doutJson          s               a   Len(500000) varying
      /Free

       Exsr BuildSqlWDELP;
       Exsr ProcessWDELP;
       Exec SQL close cursorWDELP;
       Exec SQL close cursorWMEPP;
       Exec SQL close cursorWMLDP;
       Exec SQL close cursorWMWVP;
       Return outJson;

       //=================================================================
       // Build json nodes for agency elements
       //=================================================================
       Begsr BuildJsonAgency;
        If strEnv = '*QTE';
           outEnv = 'Q';
        Endif;
        GetEmpAgt(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELemp#:dsWDELP.ELdiv
         :dsWDELP.ELfyr:*hival:inName40:rrnAH:rrnAA:rrnAG:outEnv);
        If not %open(WMAGP);
           Open WMAGP;
        Endif;
        Reset dsWMAGP;
        Chain rrnAG WMAGP dsWMAGP;
        Clear outParms;
        outParms = '{' +
         BuildJsonNode('group':%editc(dsWDELP.ELfnd:'X')) + ',' +
         BuildJsonNode('fein':%char(dsWMAGP.AGfein)) + ',' +
         BuildJsonNode('mod':%char(dsWMAGP.AGmod)) + ',' +
         BuildJsonNode('includeRequest':*off) +
         '}';
        AgencyInfo('GetAgency':inUser:outParms:inJson:*blanks);
        outJson += ',' + %subst(inJson:1);
       Endsr;

       //=================================================================
       // Build json nodes for rating elements
       //=================================================================
       Begsr BuildJsonRatings;
        Clear outParms;
        outParms  = '{' +
         BuildJsonNode('group#':%editc(dsWDELP.ELfnd:'X')) + ',' +
         BuildJsonNode('ratingState':
          GetRatingState(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELemp#:
           dsWDELP.ELdiv:dsWDELP.ELfyr)) + ',' +
         BuildJsonNode('effDate':%char(dsWDELP.ELpfDt):'date') + ',' +
         BuildJsonNode('showEmptyRate':isShowEmptyRate) + ',' +
         BuildJsonNode('employer':%editc(dsWDELP.ELemp#:'X')) + ',' +
         BuildJsonNode('division':%editc(dsWDELP.ELdiv:'X')) + ',' +
         BuildJsonNode('year':%editc(dsWDELP.ELfyr:'X')) +
         '}';
        ListRatings('GetCarrierRating':inUser:outParms:inJson:*blanks);
        outJson += %subst(inJson:2);
       Endsr;

       //=================================================================
       // Build json nodes for policy detail information
       //=================================================================
       Begsr BuildJsonWDELP;
       //*** begin add ***  10/05/21  ****************************************
       //***  Begin Chg  ***  01/19/22  *****************
       //isNetRate = *off;
       //Chain(n) (dsWDELP.ELCO#:dsWDELP.ELFND:dsWDELP.ELFYR) pf100;
       //If %found and F1NetR = 'Y' and dsWDELP.ELPFDT >= F1NetD;
       //   isNetRate = *on;
       // Endif;
        isNetRate = IsUseNetRate(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELemp#
         :dsWDELP.ELdiv:dsWDELP.ELfyr);
       //***  End   Chg  ***  01/19/22  *****************
       //*** end add *****  10/05/21  ****************************************
        strSt = RatingSt(dsWDELP.ELCO#:dsWDELP.ELRTCD);
        GetWDRRP(dsWDELP.ELco#:dsWDELP.ELfnd:strSt:dsWDELP.ELpfDt);
        decRrnWDELP = GetEmpYrRR(dsWDELP.ELfyr:dsWDELP.ELemp#:dsWDELP.ELdiv
         :dsWDELP.ELfnd:dsWDELP.ELco#);
        dsDeductible.type = RtvDeductible(dsWDELP.ELco#:dsWDELP.ELfnd
         :dsWDELP.ELemp#:dsWDELP.ELdiv:dsWDELP.ELfyr
         :dsDeductible.premPct:dsDeductible.amount:dsDeductible.Aggregate
         :dsDeductible.lossSub:dsDeductible.basis:dsDeductible.netGross
         :dsDeductible.verb:dsDeductible.class:dsDeductible.pct);
        Select;
        When dsDeductible.type = 'L';
           dsDeductible.type = 'Large';
        When dsDeductible.type = 'S';
           dsDeductible.type = 'Small';
        Endsl;
        outJson = %trim(outJson) + '{ ' +
         PolicyJSONData(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELemp#
          :dsWDELP.ELdiv:dsWDELP.ELfyr) + ',' +
         BuildJsonNode('year':%char(dsWDELP.ELfyr)) + ', ' +
         BuildJsonNode('quoteStatus':GetQuoteStsDesc(dsWDELP.ELco#:
          dsWDELP.ELfnd:dsWDELP.ELemp#:dsWDELP.ELdiv:
          dsWDELP.ELfyr:flgQte)) + ', ' +
         BuildJsonCode(%editc(dsWDELP.ELbsCd:'X')
          :BillSchdDs(dsWDELP.ELco#:%editc(dsWDELP.ELbsCd:'X'))
          :'billingPlan') + ', ' +
         BuildJsonNode('billingStatus':%trim(GetBillSts(decRrnWDELP))) + ', ' +
         BuildJsonNode('#OfEmployees':%editc(dsWDELP.ELempy:'X')) + ', ' +
         BuildJsonCode(%editc(dsWDELP.ELgovC:'X')
          :GetPayrollDesc(dsWDELP.ELco#:dsWDELP.ELpfDt:strSt:dsWDELP.ELrtCd
           :dsWDELP.ELgovC:*blanks)
          :'governingClass') + ', ' +
         BuildJsonNode('ratingSt':
          GetRatingState(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELemp#:
          dsWDELP.ELdiv:dsWDELP.ELfyr)) + ', ' +
         BuildJsonNode('rateSetCode':dsWDELP.ELrtcd) + ', ' +
         '"policyDates": {' +
          %trim(BuildJsonNode('effective':%char(dsWDELP.ELpfDt):'date')) +
           ', ' +
          %trim(BuildJsonNode('expiration':%char(dsWDELP.ELptDt):'date')) +
           '}, ' +

         '"deductibleInfo":{' +
          BuildJsonNode('type':dsDeductible.type) + ',' +
          BuildJsonNode('premiumDiscountPercent'
           :%editc(dsDeductible.premPct:'3')) + ',' +
          BuildJsonNode('amnoutPerOccurrance'
           :%editc(dsDeductible.amount:'3')) + ',' +
          BuildJsonNode('aggregate':%editc(dsDeductible.aggregate:'3')) + ',' +
          BuildJsonNode('lossSubjectCode'
           :%editc(dsDeductible.lossSub:'3')) + ',' +
          BuildJsonNode('basisCode':%editc(dsDeductible.basis:'3')) + ',' +
          BuildJsonNode('netGross':dsDeductible.netGross) + ',' +
          BuildJsonNode('verbiage':dsDeductible.verb) + ',' +
          BuildJsonNode('classCode':%editc(dsDeductible.class:'3')) + ',' +
          BuildJsonNode('percentate':%editc(dsDeductible.pct:'3')) + '},' +

         '"ratingScore": {' +
          BuildJsonNode('score':%editc(dsWDELP.ELscore:'3')) + ', ' +
          BuildJsonNode('received':%char(dsWDELP.ELscoreDt):'date') + ', ' +
          BuildJsonNode('bypass':dsWDELP.ELscoreBP) +
           '}, ';

        Exsr BuildJsonRatings;
        Exsr BuildJsonAgency;

        Exsr BuildSqlWMEPP;
        Exsr ProcessWMEPP;
        outJson = %trim(outJson) + ',' + %trim(strJsonWMEPP);

        outJson = %trim(outJson) + '}';
       Endsr;

       //=================================================================
       // Build json nodes for payroll information
       //=================================================================
       Begsr BuildJsonWMEPP;

        // Additional Payroll Information
        ratingState = GetRatingState(dsWMEPP.EPco#:dsWMEPP.EPfnd:
                      dsWMEPP.EPemp#:dsWMEPP.EPdiv:dsWMEPP.EPfyr);
        classEffDt  = dsWMEPP.EPefDt;
        rateSetCode = dsWDELP.ELrtCd;
        classCode   = dsWMEPP.EPclas;
        classDesc   = GetPayrollDesc(dsWMEPP.EPco#:classEffDt:
            ratingState:rateSetCode:classCode:dsWMEPP.EPext:
            hazardGroup:contClass);

        strJsonWMEPP = %trim(strJsonWMEPP) + '{' +
         '"dates":{' +
          %trim(BuildJsonNode('effective':%char(dsWMEPP.EPefDt):'date')) +
           ', ' +
          %trim(BuildJsonNode('cancel':%char(dsWMEPP.EPcaDt):'date'))+
           '}, ' +
         %trim(BuildJsonNode('code':%editc(dsWMEPP.EPclas:'X'))) + ', ' +
         %trim(BuildJsonNode('extension':dsWMEPP.EPext)) + ', ' +
         %trim(BuildJsonNode('description':classDesc)) + ', ' +
         %trim(BuildJsonNode('#ofEmployees':%char(dsWMEPP.EPeCnt))) + ', ' +
         %trim(BuildJsonNode('contractingClass':contClass)) + ', ' +
         %trim(BuildJsonNode('hazardGroup':hazardGroup)) + ', ' +
       //*** begin chg ***  10/05/21  ****************************************
       //***  %trim(BuildJsonNode('amount':%editc(dsWMEPP.EPeAPr:'3'))) + ', ' +
       //***  %trim(BuildJsonNode('rate':%editc(dsWMEPP.EPaRat:'3'))) + ', ' +
       //***  %trim(BuildJsonNode('premium':%editc(dsWMEPP.EPeMPr:'3'))) + '} ';
         %trim(BuildJsonNode('amount':%editc(dsWMEPP.EPeAPr:'3'))) + ', ';
        If isNetRate;
           strJsonWMEPP = %trim(strJsonWMEPP) + ' ' +
            %trim(BuildJsonNode('rate':%editc(dsWMEPP.EPEntr:'3'))) + ', ' +
            %trim(BuildJsonNode('premium':%editc(dsWMEPP.EPEntp:'3'))) + '} ';
        Else;
           strJsonWMEPP = %trim(strJsonWMEPP) + ' ' +
            %trim(BuildJsonNode('rate':%editc(dsWMEPP.EPaRat:'3'))) + ', ' +
            %trim(BuildJsonNode('premium':%editc(dsWMEPP.EPeMPr:'3'))) + '} ';
        Endif;
       //*** end chg *****  10/05/21  ****************************************

       Endsr;
       //=================================================================
       // Build json nodes for deductible inormation
       //=================================================================
       Begsr BuildJsonWMLDP;
        strJsonWMLDP = %trim(strJsonWMLDP) +
         '"small":{' +
          %trim(BuildJsonNode('billIfCancelled':dsWDELP.ELcxBl)) + ', ' +
          '"codes":[{' +
           %trim(BuildJsonNode('code':dsWDELP.ELdCde)) + ', ' +
            '"dates":{' +
             %trim(BuildJsonNode('effective':%char(dsWDELP.ELdCEf):'date')) +
              ', ' +
             %trim(BuildJsonNode('expiration':%char(dsWDELP.ELdCEx):'date'))+
              '}}, {' +
           %trim(BuildJsonNode('code':dsWDELP.ELdCd2)) + ', ' +
            '"dates":{' +
             %trim(BuildJsonNode('effective':%char(dsWDELP.EL2DEf):'date')) +
               ', ' +
             %trim(BuildJsonNode('expiration':%char(dsWDELP.EL2DEx):'date'))+
               '}}]}, ' +
         '"large":{' +
          %trim(BuildJsonNode('autoRenew':dsWMLDP.LDrnw1)) + ', ' +
          '"plans":[' +
           '{"dates":{' +
            %trim(BuildJsonNode('effective':%char(dsWMLDP.LDefD1):'date')) +
             ', ' +
            %trim(BuildJsonNode('expiration':%char(dsWMLDP.LDexp1):'date'))+
             '}, ' +
           %trim(BuildJsonNode('description':dsWMLDP.LDdes1)) + ', ' +
           '"discount":{' +
            %trim(BuildJsonNode('percentage':%editc(dsWMLDP.LDpct1:'3'))) +
             ', ' +
            %trim(BuildJsonNode('type':dsWMLDP.LDtyp1)) + '}, ' +
           '"amounts":{' +
            %trim(BuildJsonNode('deductible':%editc(dsWMLDP.LDamp1:'3'))) +
             ', ' +
            %trim(BuildJsonNode('aggregate':%editc(dsWMLDP.LDamg1:'3'))) +
             '}, ' +
           %trim(BuildJsonNode('type':dsWMLDP.LDpln1)) + ', ' +
           %trim(BuildJsonNode('accidentBasis':dsWMLDP.LDvrb1)) + ', ' +
           %trim(BuildJsonNode('occupationalDiseaseBasis':dsWMLDP.LDoCV1)) +
            ', ' +
           %trim(BuildJsonNode('statisticalCode':
            %editc(dsWMLDP.LDcls1:'X'))) + ', ' +
           %trim(BuildJsonNode('useTotalIncurred':dsWMLDP.LDtot1)) + ', ' +
           %trim(BuildJsonNode('billingLevel':dsWMLDP.LDpLv1)) + ', ' +
           '"reserveHierachy": [{' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr101)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr102)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr103)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr104)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr105)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr106)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr107)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr108)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr109)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr110)) + '}]}, ' +
           '{"dates":{' +
            %trim(BuildJsonNode('effective':%char(dsWMLDP.LDefD2):'date')) +
             ', ' +
            %trim(BuildJsonNode('expiration':%char(dsWMLDP.LDexp2):'date'))+
             '}, ' +
           %trim(BuildJsonNode('description':dsWMLDP.LDdes2)) + ', ' +
           '"discount":{' +
            %trim(BuildJsonNode('percentage':%editc(dsWMLDP.LDpct2:'3'))) +
             ', ' +
            %trim(BuildJsonNode('type':dsWMLDP.LDtyp2)) + '}, ' +
           '"amounts":{' +
            %trim(BuildJsonNode('deductible':%editc(dsWMLDP.LDamp2:'3'))) +
             ', ' +
            %trim(BuildJsonNode('aggregate':%editc(dsWMLDP.LDamg2:'3'))) +
             '}, ' +
           %trim(BuildJsonNode('type':dsWMLDP.LDpln2)) + ', ' +
           %trim(BuildJsonNode('accidentBasis':dsWMLDP.LDvrb2)) + ', ' +
           %trim(BuildJsonNode('occupationalDiseaseBasis':dsWMLDP.LDoCV2)) +
            ', ' +
           %trim(BuildJsonNode('statisticalCode':
            %editc(dsWMLDP.LDcls2:'X'))) + ', ' +
           %trim(BuildJsonNode('useTotalIncurred':dsWMLDP.LDtot2)) + ', ' +
           %trim(BuildJsonNode('billingLevel':dsWMLDP.LDpLv2)) + ', ' +
           '"reserveHierachy": [{' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr201)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr202)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr203)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr204)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr205)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr206)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr207)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr208)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr209)) + '},{ ' +
            %trim(BuildJsonNode('code':dsWMLDP.LDr210)) + '}]} ' +
        ']}';
       Endsr;

       //=================================================================
       // Build json nodes for specific waiver inormation
       //=================================================================
       Begsr BuildJsonWMWVP;
        strJsonWMWVP +=
         '"small":{';
       Endsr;

       //=================================================================
       // Build SQL to access WDELP
       //=================================================================
       Begsr BuildSqlWDELP;
        strSqlWDELP = 'select * ' +
         'from WDELP ' +
         'where ELCO# = ' + inCo# + ' and ELFND = ' + inFnd +
         ' and ELEMP# = ' + inEmp# + ' and ELDIV = ' + inDiv;
        If %parms >= 5 and inFyr <> *blanks;
           strSqlWDELP = %trim(strSqlWDELP) + ' ' +
            'and ELFYR = ' + inFyr;
        Endif;
        strSqlWDELP = %trim(strSqlWDELP) + ' ' +
         'order by ELCO#, ELFND, ELEMP#, ELDIV, ELFYR desc';
        Exec SQL prepare sqlWDELP from :strSqlWDELP;
        Exec SQL declare cursorWDELP cursor for sqlWDELP;
        Exec SQL open cursorWDELP;
       Endsr;

       //=================================================================
       // Build SQL to access WMEPP
       //=================================================================
       Begsr BuildSqlWMEPP;
        strSqlWMEPP = 'select * ' +
         'from WMEPP ' +
         'where EPCO#  = ' + %char(dsWDELP.ELco#)  +
          ' and EPFND  = ' + %char(dsWDELP.ELfnd)  +
          ' and EPEMP# = ' + %char(dsWDELP.ELemp#) +
          ' and EPDIV  = ' + %char(dsWDELP.ELdiv)  +
          ' and EPFYR  = ' + %char(dsWDELP.ELfyr);
        Exec SQL prepare sqlWMEPP from :strSqlWMEPP;
        Exec SQL declare cursorWMEPP cursor for sqlWMEPP;
        Exec SQL open cursorWMEPP;
       Endsr;

       //=================================================================
       // Build SQL to access WMLDP
       //=================================================================
       Begsr BuildSqlWMLDP;
        strSqlWMLDP = 'select * ' +
         'from WMLDP ' +
         'where LDCO#  = ' + %char(dsWDELP.ELco#)  +
          ' and LDFND  = ' + %char(dsWDELP.ELfnd)  +
          ' and LDEMP# = ' + %char(dsWDELP.ELemp#) +
          ' and LDDIV  = ' + %char(dsWDELP.ELdiv)  +
          ' and LDFYR  = ' + %char(dsWDELP.ELfyr);
        Exec SQL prepare sqlWMLDP from :strSqlWMLDP;
        Exec SQL declare cursorWMLDP cursor for sqlWMLDP;
        Exec SQL open cursorWMLDP;
       Endsr;

       //=================================================================
       // Build SQL to access WMWVP
       //=================================================================
       Begsr BuildSqlWMWVP;
        strSqlWMWVP = 'select * ' +
         'from WMWVP ' +
         'where WVCO#  = ' + %char(dsWDELP.ELco#)  +
          ' and WVFND  = ' + %char(dsWDELP.ELfnd)  +
          ' and WVEMP# = ' + %char(dsWDELP.ELemp#) +
          ' and WVDIV  = ' + %char(dsWDELP.ELdiv)  +
          ' and WVFYR  = ' + %char(dsWDELP.ELfyr);
        Exec SQL prepare sqlWMWVP from :strSqlWMWVP;
        Exec SQL declare cursorWMWVP cursor for sqlWMWVP;
        Exec SQL open cursorWMWVP;
       Endsr;

       //=================================================================
       // Process SQL to WDELP
       //=================================================================
       Begsr ProcessWDELP;
        outJson = '"detailInformation": [';
        Dou isEofWDELP;
           Exec SQL fetch next from cursorWDELP into :dsWDELP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWDELP = *on;
              Iter;
           When %len(%trim(outJson)) > 22;
              outJson = %trim(outJson) + ',';
           Endsl;
           // Build json with detail information
           Exsr BuildJsonWDELP;
        Enddo;
        outJson = %trim(outJson) + ']';
        Exec SQL close cursorWDELP;

        //*** Begin Add *** 08/05/21 **************************
        intVndEfDt = *Zeros;
        If %parms >= 5 and inFyr <> *blanks;
           intVndEfDt = dsWDELP.ELpfdt;
        Endif;
        //*** End   Add *** 08/05/21 **************************

       Endsr;

       //=================================================================
       // Process SQL to WMEPP
       //=================================================================
       Begsr ProcessWMEPP;
        strJsonWMEPP = '"payroll":[';
        Dou isEofWMEPP;
           Exec SQL fetch next from cursorWMEPP into :dsWMEPP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMEPP = *on;
              Iter;
           When %len(%trim(strJsonWMEPP)) > 11;
              strJsonWMEPP = %trim(strJsonWMEPP) + ',';
           Endsl;
           // Build json with payroll information
           Exsr BuildJsonWMEPP;
        Enddo;
        strJsonWMEPP = %trim(strJsonWMEPP) + ']';
        Exec SQL close cursorWMEPP;
       Endsr;

       //=================================================================
       // Process SQL to WMLDP
       //=================================================================
       Begsr ProcessWMLDP;
        strJsonWMLDP = '"deductibles":{';
        Dou isEofWMLDP;
           Exec SQL fetch next from cursorWMLDP into :dsWMLDP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMLDP = *on;
              Iter;
           Endsl;
           // Build json with deductible information
           Exsr BuildJsonWMLDP;
        Enddo;
        strJsonWMLDP = %trim(strJsonWMLDP) + '}';
        Exec SQL close cursorWMLDP;
       Endsr;

       //=================================================================
       // Process SQL to WMWVP
       //=================================================================
       Begsr ProcessWMWVP;
        strJsonWMLDP = '"specificWaivers":{list:[';
        Dou isEofWMWVP;
           Exec SQL fetch next from cursorWMWVP into :dsWMWVP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMWVP = *on;
              Iter;
           Endsl;
           // Build json with specific waiver information
           Exsr BuildJsonWMWVP;
        Enddo;
        strJsonWMWVP += '],' +
         BuildJsonNode('count':%editc(decCount:'X'));
        strJsonWMWVP += strJsonWMWVP + '],' + '}';
        Exec SQL close cursorWMWVP;
       Endsr;
      /End-free
     pBuildPolicyDetailInfo...
     p                 e

     p*===================================================================
     p* Build JSON for policy master information
     p*  (Called from GetApplication)
     p*===================================================================
     pBuildPolicyMasterInfo...
     p                 b
     dBuildPolicyMasterInfo...
     d                 pi              a   Len(1500000) varying
     d inApp#                         9  0 const
     d inCurrent                      1    const
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5    const options(*nopass)
     d* If no division passed, array of all divisions will be returned
     d inFyr                          3    const options(*nopass)
     d* If no detail information is needed/wanted, pass '***'
     d inFyrWIEMP                     3  0 const options(*nopass)

     dinJson           s               a   len(8500000) varying inz static

     ddt7Cx            s              7  0 inz
     DintCtr           s              3  0 Inz
      //*** Begin Add *** 08/05/21 **************************************
     DintEfdt          s              7P 0 inz
      //*** End   Add *** 08/05/21 **************************************
     DisEofWDLDP       s              1n   inz('0')
     DisEofWMALP       s              1n   inz('0')
     DisEofWMECP       s              1n   inz('0')
     DisEofWMEMP       s              1n   inz('0')
     DisEofWMOOP       s              1n   inz('0')
     DisGetScoreInfo   s              1n   inz('0')
      //*** Begin Add *** 08/05/21 **************************************
     disGradiant       s               N   inz(*Off)
     disValen          s               N   inz(*Off)
     disValidWIVSP     s               N   inz(*Off)
      //*** End   Add *** 08/05/21 **************************************
     DoutParms         s               a   len(5000) varying inz static
     DstrAuth          s              1
     DstrAuthorized    s              5    inz('true')
     DstrAuthType      s             10    inz('all')
      //*** Begin Add *** 08/05/21 **************************************
     dstrChar          s              1a
      //*** End   Add *** 08/05/21 **************************************
     dstrConDesc1      s             15a   inz
     dstrConDesc2      s             15a   inz
     dstrConDesc3      s             15a   inz
     dstrConDesc4      s             15a   inz
     dstrCxReason      s             65a   inz
      //*** Begin Add *** 08/05/21 **************************************
     dstrExpCat        s            256a   inz
     DstrJsonScoring   s          64000a   varying static
      //*** End   Add *** 08/05/21 **************************************
     DstrJsonWDLDP     s          64000a   varying static
     DstrJsonWIAPPANS  s          64000a   varying static
     DstrJsonWIGSP     s          64000a   varying static
      //*** Begin Add *** 08/05/21 **************************************
     DstrJsonWIVSP     s          64000a   varying static
     DstrJsonWIVSPEXP  s          64000a   varying static inz('  ')
      //*** End   Add *** 08/05/21 **************************************
     DstrJsonWMALP     s          64000a   varying static
     DstrJsonWMECP     s          64000a   varying static
     D***  Begin Chg  ***  09/11/21  *************************
     D**********strJsonWMEMP     s      a   len(1500000) varying inz static
     DstrJsonWMEMP     s               a   len(1500000) varying inz(',') static
     D***  End   Chg  ***  09/11/21  *************************
     DstrJsonWMOOP     s          64000a   varying static
      //*** Begin Add *** 08/05/21 **************************************
     d***  Begin Chg  ***  12/09/21  **************
     d*****strScrVnd        s             25a
     dstrScrVnd        s             25a   varying
     d***  End   Chg  ***  12/09/21  **************
      //*** End   Add *** 08/05/21 **************************************
     DstrSqlWDLDP      s            500
     DstrSqlWIAPPANS   s            500
     DstrSqlWIGSP      s            500
      //*** Begin Add *** 08/05/21 **************************************
     dstrSqlWIVSP      s            500a
     dstrSqlWIVSPEXP   s            500a
      //*** End   Add *** 08/05/21 **************************************
     DstrSqlWMALP      s            500
     DstrSqlWMECP      s            500
     DstrSqlWMEMP      s            500
     DstrSqlWMOOP      s            500
     DstrRvwTrgs       s              5a   varying inz
     DstrJsonSummary   s          64000a   varying static
     DstrSqlSummary    s            500

     DdesignationDesc  s             40    varying
     d***  Begin Chg  ***  12/09/21  **************
     D***sicDesc          s             40
     D***naicDesc         s             40
     D***legalStsDesc     s             40
     DsicDesc          s             40    varying
     DnaicDesc         s             40    varying
     DlegalStsDesc     s             40    varying
     d***  End   Chg  ***  12/09/21  **************

     ddsWDLDP        e ds                  extname(WDLDP) qualified
     ddsWIAPPANS     e ds                  extname(WIAPPANS) qualified
      //*** Begin Add *** 08/05/21 **************************************
     ddsWIVSP        e ds                  extname(WIVSP) qualified
     ddsWIVSPEXP     e ds                  extname(WIVSPEXP) qualified
      //*** End   Add *** 08/05/21 **************************************
     ddsWIGSP        e ds                  extname(WIGSP) qualified
     ddsWIQUP        e ds                  extname(WIQUP) qualified inz
     ddsWMALP        e ds                  extname(WMALP) qualified
     ddsWMECP        e ds                  extname(WMECP) qualified
     ddsWMEMP        e ds                  extname(WMEMP) qualified
     ddsWMOOP        e ds                  extname(WMOOP) qualified
     DdsSummary        ds                  inz qualified
     D year                           3  0
     D state                          2
     D annPrem                       11  2

     doutJson          s               a   len(2000000) varying inz static

      /Free
       isGetScoreInfo = %parms >= 8;
       Clear strJsonWDLDP;
       Clear strJsonWIAPPANS;
       Clear strJsonWIGSP;
       Clear strJsonWMALP;
       Clear strJsonWMECP;
       //***  Begin Chg  ***  09/11/21  ********************
       //**********Clear strJsonWMEMP;
       Reset strJsonWMEMP;
       //***  End   Chg  ***  09/11/21  ********************
       Clear strJsonWMOOP;
       Clear strJsonSummary;
       Clear outJson;
       strAuth = InqUpdAccessEmp(inCo#:inFnd:inEmp#:0:inUser);
       Select;
       When strAuth = '*';
          strAuthorized = 'false';
          strAuthType   = 'none';
       When strAuthority = '*View';
          strAuthType   = 'inquiry';
       When strAuth = 'D';
          strAuthType   = 'inquiry';
       Endsl;
       Select;
       When strAuthorized <> 'false';
          Exsr BuildSqlWMEMP;
          Exsr ProcessWMEMP;
        //Exsr BuildSqlWIAPPANS;
          Exsr ProcessWIAPPANS;
          Exsr BuildSqlWIGSP;
        //*** Begin Change *** 08/05/21 ***********************************************************
          //Exsr ProcessWIGSP;
          Exsr BuildSqlWIVSP;
          Exsr ProcessScoring;
        //*** End   Change *** 08/05/21 ***********************************************************
        //***  Begin Add  ***  09/11/21  ****************
          //* State summary of policy/quote
          Exsr StateSummary;
        //***  End   Add  ***  09/11/21  ****************
       Endsl;
       outJson = '{' +
        '"company":{' +
         BuildJsonNode('number':%editc(inCo#:'X')) + ', ' +
         BuildJsonNode('name':GetCompNam(inCo#)) + '}, ' +
        '"group":{' +
         BuildJsonNode('number':%editc(inFnd:'X')) + ', ' +
         BuildJsonNode('name':GetGrpName(inFnd:rrn9:inCo#)) + '}, ' +
        BuildJsonNode('currentlySelected':inCurrent)          + ', ' +
        BuildJsonNode('employer'         :%editc(inEmp#:'X')) + ', ' +
        %Trim(strJsonWIAPPANS) + ' ' +
        '"authorization":{' +
         BuildJsonNode('authorized':strAuthorized:'boolean') + ', ' +
         BuildJsonNode('type':strAuthType) + '} ,';

       //*** Beging change *** 08/05/21 *****************************************
       //If strJsonWIGSP <> *blanks;
       //   strJsonWIGSP = %scanrpl(',':'':strJsonWIGSP:1:1);
       //   outJson += %trim(strJsonWIGSP) + ',';
       //Endif;
       If strJsonScoring <> *blanks;
          strJsonScoring = %scanrpl(',':'':strJsonScoring:1:1);
          outJson += %trim(strJsonScoring) + ',';
       Endif;
       //*** End  change *** 08/05/21 *****************************************

       //***  Begin Chg  ***  09/11/21  ****************
       //* State summary of policy/quote
       //**********Exsr StateSummary;
       //**********If strJsonSummary <> *blanks;
       //**********   outJson += %trim(strJsonSummary) + ',';
       //**********Endif;
       outJson += %trim(strJsonSummary);
       //***  End   Chg  ***  09/11/21  ****************

       strJsonWMEMP = %scanrpl(',':'':strJsonWMEMP:1:1);
       outJson += %trim(strJsonWMEMP);
       //***  Begin Add  ***  09/11/21  *************************
       outJson = %scanrpl(',':'':outJson:%len(outJson)-1);
       //***  End   Add  ***  09/11/21  *************************
       outJson += '}';
       Return outJson;

       //=================================================================
       // Build json nodes for location year information
       //=================================================================
       Begsr BuildJsonWDLDP;
        strJsonWDLDP = %trim(strJsonWDLDP) + '{' +
         %trim(BuildJsonNode('year':%char(dsWDLDP.LDfyr))) + ', ' +
         %trim(BuildJsonNode('#ofEmployees':%char(dsWDLDP.LDempY))) + ', ' +
         %trim(BuildJsonNode('payroll':%char(dsWDLDP.LDtPYr))) +
         '}';
       Endsr;
       //=================================================================
       // Build json nodes for application responses
       //=================================================================
       Begsr BuildJsonWIAPPANS;

        Reset dsWIQUP;
        Exec SQL
         select * into :dsWIQUP
          from WIQUP
          where QUco# = :inCo# and QUid# = :dsWIAPPANS.APid#;
        strJsonWIAPPANS += '{' +
         BuildJsonNode('questionType':dsWIQUP.QUtype) + ',' +
         BuildJsonNode('code':dsWIQUP.QUcode) + ',' +
         BuildJsonNode('id':%Char(dsWIAPPANS.APid#)) + ', ' +
         BuildJsonNode('status':dsWIQUP.QUstat) + ',' +
         BuildJsonNode('question':dsWIQUP.QUquestion) + ',' +
         BuildJsonNode('answerType':dsWIQUP.QUanswer) + ',' +
         BuildJsonNode('response':dsWIAPPANS.APresp) + ', ' +
         BuildJsonNode('rateSetCode':dsWIAPPANS.APrtcd) + ', ' +
         BuildJsonNode('payrollClass':%Char(dsWIAPPANS.APclas)) +
         '}';
       Endsr;
       //=================================================================
       // Build json nodes for Gradient Score Inforamtion
       //=================================================================
       Begsr BuildJsonWIGSP;
        Select;
        When dsWIGSP.GSrvwTrgs = 'Y';
           strRvwTrgs = 'true';
        When dsWIGSP.GSrvwTrgs = 't';
           strRvwTrgs = 'true';
        Other;
           strRvwTrgs = 'false';
        Endsl;
         //Begin Change *** 08/05/21 *******************************************
         // strJsonWIGSP += '{' +
         //  BuildJsonNode('year':%Char(dsWIGSP.GSfyr)) +
         //  ', ' +
         //  BuildJsonNode('predictedLoss':%Char(dsWIGSP.GSprdLos)) +
        strJsonWIGSP += BuildJsonNode('predictedLoss':%Char(dsWIGSP.GSprdLos)) +
         ', ' +
         BuildJsonNode('quotedPremium':%Char(dsWIGSP.GSqtePrm)) +
         ', ' +
         BuildJsonNode('predictedLossRatio':%Char(dsWIGSP.GSpLosRat)) +
         ', ' +
         BuildJsonNode('percentile':%Char(dsWIGSP.GSperc)) +
         ', ' +
         BuildJsonNode('reviewTargets':strRvwTrgs:'boolean') +
         ', ' +
         BuildJsonNode('targetPLR':%Char(dsWIGSP.GStrgPlr)) +
         ', ' +
         BuildJsonNode('targetPremium':%Char(dsWIGSP.GStrgPrm)) +
         ', ' +
         BuildJsonNode('highRangePremium':%Char(dsWIGSP.GShRngPrm)) +
         ', ' +
         BuildJsonNode('lowRangePremium':%Char(dsWIGSP.GSlRngPrm)) +
         ', ' +
         BuildJsonNode('targetFactor':%Char(dsWIGSP.GStrgFct)) +
         ', ' +
         BuildJsonNode('highRangeFactor':%Char(dsWIGSP.GShRngFct)) +
         ', ' +
         BuildJsonNode('lowRangeFactor':%Char(dsWIGSP.GSlRngFct)) +
         ', ' +
         BuildGradientDrivers(dsWIGSP.GSco#:dsWIGSP.GSfnd:
           dsWIGSP.GSemp#:dsWIGSP.GSfYr)  +
         '},';

       Endsr;
       //*** Begin Add *** 08/05/21 **************************
       //=================================================================
       // Build json nodes for Valen Score Information
       //=================================================================
       Begsr BuildJsonWIVSP;
        strJsonWIVSP += '"info":{' +
         BuildJsonNode('score_id':dsWIVSP.VSscrId) +
         ',' +
         BuildJsonNode('score_key':dsWIVSP.VSscrKey) +
         '}, ' +
         '"insured":{ ' +
         BuildJsonNode('exposure_shift_prediction':%Char(dsWIVSP.VSexpShft)) +
         ', ' +
         BuildJsonNode('final_misclass_bin':%Char(dsWIVSP.VSmisclsBf)) +
         ', ' +
         BuildJsonNode('final_misclass_score':%Char(dsWIVSP.VSmisclsSf)) +
         ', ' +
         BuildJsonNode('final_premium_impact_bin':%Char(dsWIVSP.VSprmImpBf)) +
         ', ' +
         BuildJsonNode('final_premium_impact_score':%Char(dsWIVSP.VSprmImpSf)) +
         ', ' +
         BuildJsonNode('final_price_score':%Char(dsWIVSP.VSpriceSf)) +
         ', ' +
         BuildJsonNode('final_recommended_company':dsWIVSP.VSrecComf) +
         ', ' +
         BuildJsonNode('final_recommended_tier':dsWIVSP.VSrecTierf) +
         ', ' +
         BuildJsonNode('final_risk_bin':%Char(dsWIVSP.VSriskBf)) +
         ', ' +
         BuildJsonNode('final_risk_bin_lower_bound':%Char(dsWIVSP.VSriskBlF)) +
         ', ' +
         BuildJsonNode('final_risk_bin_upper_bound':%Char(dsWIVSP.VSriskBuF)) +
         ', ' +
         BuildJsonNode('final_risk_interpretation':dsWIVSP.VSriskIntF) +
         ', ' +
         BuildJsonNode('final_risk_score':%Char(dsWIVSP.VSriskSf)) +
         ', ' +
         BuildJsonNode('final_technical_premium':%Char(dsWIVSP.VStechPrmF)) +
         ', ' +
         BuildJsonNode('loss_ratio_relativity_prediction':%Char(+
          dsWIVSP.VSlossRat)) +
         ', ' +
         BuildJsonNode('loss_ratio_relativity_prediction_rounded':strChar)  +
         ', ' +
         BuildJsonNode('misclass_bin':%Char(dsWIVSP.VSmisclsB)) +
         ', ' +
         BuildJsonNode('misclass_interpretation':dsWIVSP.VSmisclsI) +
         ', ' +
         BuildJsonNode('misclass_lower_bound':%Char(dsWIVSP.VSmisclsL)) +
         ', ' +
         BuildJsonNode('misclass_upper_bound':%Char(dsWIVSP.VSmisclsU)) +
         ', ' +
         BuildJsonNode('mod_price_score_factor':%Char(dsWIVSP.VSmodPrcS)) +
         ', ' +
         BuildJsonNode('original_policy_term_number':dsWIVSP.VSorgPol) +
         ', ' +
         BuildJsonNode('pi_bin':%Char(dsWIVSP.VSpiB)) +
         ', ' +
         BuildJsonNode('pi_interpretation':dsWIVSP.VSpiI) +
         ', ' +
         BuildJsonNode('pi_lower_bound':%Char(dsWIVSP.VSpiL)) +
         ', ' +
         BuildJsonNode('pi_prediction':strChar) +
         ', ' +
         BuildJsonNode('pi_upper_bound':%Char(dsWIVSP.VSpiU)) +
         ', ' +
         BuildJsonNode('plrr_adjusted':%Char(dsWIVSP.VSplrrAdj1)) +
         ', ' +
         BuildJsonNode('plrr_adjusted_floored':%Char(dsWIVSP.VSplrrAdj2)) +
         ', ' +
         BuildJsonNode('premium_adjusted':%char(dsWIVSP.VSprmAdj)) +
         ', ' +
         BuildJsonNode('price_bin':%char(dsWIVSP.VSpriceB)) +
         ', ' +
         BuildJsonNode('price_bin_lower_bound':%Char(dsWIVSP.VSpriceBl)) +
         ', ' +
         BuildJsonNode('price_bin_upper_bound':%Char(dsWIVSP.VSpriceBu)) +
         ', ' +
         BuildJsonNode('price_score':strChar) +
         ', ' +
         BuildJsonNode('risk_bin':%Char(dsWIVSP.VSpriceBu)) +
         ', ' +
         BuildJsonNode('risk_interpretation':strChar) +
         ', ' +
         BuildJsonNode('risk_lower_bound':strChar) +
         ', ' +
         BuildJsonNode('risk_upper_bound':strChar) +
         ', ' +
         //*** Begin Chg *** 09/14/22 **************************************
         //*** BuildJsonNode('term_effective_date':%Char(dsWIVSP.VSEffDt));
         BuildJsonNode('term_effective_date':%Char(dsWIVSP.VSEffDt)) +
         ', ' +
         BuildJsonNode('years_w_carrier':%Char(dsWIVSP.VSyrsCar)) +
         ', ' +
         BuildJsonNode('original_premium_manual_initial':
            %Char(dsWIVSP.VSorgMan));
         //*** End   Chg *** 09/14/22 **************************************


         strJsonWIVSP+= '},' + strJsonWIVSPEXP;
       Endsr;
       //=================================================================
       // Build json nodes for Valen Score Information - WIVSPEXP
       //=================================================================
       Begsr BuildJsonWIVSPEXP;
        Select;
        When strExpCat <> dsWIVSPEXP.VScategory;
           strJsonWIVSPEXP=
            %scanrpl(',':'],':strJsonWIVSPEXP:%len(strJsonWIVSPEXP)-1);
           strJsonWIVSPEXP +=
            '"' + %trim(dsWIVSPEXP.VScategory) + '":' + ElementOpen('[');
           strExpCat= dsWIVSPEXP.VScategory;
        Endsl;
        strJsonWIVSPEXP += '{' +
         BuildJsonNode('name':%trim(dsWIVSPEXP.VSname)) + ',' +
         BuildJsonNode('description':%trim(dsWIVSPEXP.VSdesc)) + ',' +
         BuildJsonNode('rank':%Char(dsWIVSPEXP.VSseq#)) + ',' +
         BuildJsonNode('importance':%Char(dsWIVSPEXP.VSimprtnc)) +
         '}, ';
       Endsr;
       //*** End   Add *** 08/05/21 **************************
       //=================================================================
       // Build json nodes for location information
       //=================================================================
       Begsr BuildJsonWMALP;

        // If values is not Y then set to N
        If dsWMALP.ALnamP <> 'Y';
           dsWMALP.ALnamP = 'N';
        EndIf;
        If dsWMALP.ALfdnP <> 'Y';
           dsWMALP.ALfdnP = 'N';
        EndIf;

        // Get the SIC description
      //***  Begin Chg  ***  12/09/21  **************
      //sicDesc  = GetGenCodeDs(dsWMALP.ALco#:'SIC':dsWMALP.ALsic)          ;
        sicDesc = %trim(GetGenCodeDs(dsWMALP.ALco#:'SIC':dsWMALP.ALsic));
      //***  End   Chg  ***  12/09/21  **************

        // Get the NAIC description
      //***  Begin Chg  ***  12/09/21  **************
      //naicDesc = GetGenCodeDs(dsWMALP.ALco#:'NAIC':dsWMALP.ALnaic)        ;
        naicDesc = %trim(GetGenCodeDs(dsWMALP.ALco#:'NAIC':dsWMALP.ALnaic));
      //***  End   Chg  ***  12/09/21  **************

        strJsonWMALP = %trim(strJsonWMALP) + '{' +
         %trim(BuildJsonNode('type':dsWMALP.ALeLTp)) + ', ' +
         %trim(BuildJsonNode('id':dsWMALP.ALeLId)) + ', ' +
         %trim(BuildJsonNode('fein':%char(dsWMALP.ALfein))) + ', ' +
         %trim(BuildJsonNode('stateRiskID':dsWMALP.ALsemp)) + ', ' +
         %trim(BuildJsonNode('stateUnemploymentID':dsWMALP.ALue#)) + ', ' +
         %trim(BuildJsonNode('typeOfBusiness':dsWMALP.ALtpob)) + ', ' +

         '"SIC":{' +
          %trim(BuildJsonNode('code':dsWMALP.ALsic)) + ', ' +
          %trim(BuildJsonNode('description':sicDesc)) +
         '}, ' +

         '"NAIC":{' +
          %trim(BuildJsonNode('code':dsWMALP.ALnaic)) + ', ' +
          %trim(BuildJsonNode('description':naicDesc)) +
         '}, ' +

         '"name":{' +
        //***  Begin Chg  ***  12/09/21  ************** +
        //%trim(BuildJsonNode('dbaPersonalName':dsWMALP.ALnamP)) + ', ' +
        //%trim(BuildJsonNode('dba':dsWMALP.ALname + dsWMALP.ALnam2)) + ', ' +
        //%trim(BuildJsonNode('federalPersonalName':dsWMALP.ALfdnP)) + ', ' +
        //%trim(BuildJsonNode('federal':dsWMALP.ALfdNm + dsWMALP.ALfdn2)) +
          BuildJsonNode('dbaPersonalName':%trim(dsWMALP.ALnamP)) + ', ' +
          BuildJsonNode('dba':%trim(dsWMALP.ALname + dsWMALP.ALnam2)) + ', ' +
          BuildJsonNode('federalPersonalName':%trim(dsWMALP.ALfdnP)) + ', ' +
          BuildJsonNode('federal':%trim(dsWMALP.ALfdNm + dsWMALP.ALfdn2)) +
        //***  End   Chg  ***  12/09/21  ************** +
         '}, ' +

         '"dates":{' +
          %trim(BuildJsonNode('effective':%char(dsWMALP.ALefDt):'date')) +
         '}, ' +

         '"address":{' +
          %trim(BuildJsonAddress(dsWMALP.ALmAd1:dsWMALP.ALmAd2:
           dsWMALP.ALmCty:dsWMALP.ALmSt:%char(dsWMALP.ALmZip):'mailing')) +
           ', ' +
          %trim(BuildJsonAddress(dsWMALP.ALpAd1:dsWMALP.ALpAd2:
           dsWMALP.ALpCty:dsWMALP.ALpSt:%char(dsWMALP.ALpZip):'physical')) +
         '}, ' +

         '"phone":[' +
          '{'+%trim(BuildJsonPhone(%char(dsWMALP.ALarea):%char(dsWMALP.ALphn#):
           '':'phone1')) + '}, ' +
          '{'+%trim(BuildJsonPhone(%char(dsWMALP.ALfAra):%char(dsWMALP.ALfax#):
           'fax':'fax')) + '} ' +
         '], ' +

         '"lastNodeWMALP":"last json Node prior to file reads (ignore)" ';

        Exsr BuildSqlWMOOP;
        Exsr ProcessWMOOP;
        strJsonWMALP = %trim(strJsonWMALP) + ',' + %trim(strJsonWMOOP);

        If %parms >= 5 and inFyr <> '***';
           Exsr BuildSqlWDLDP;
           Exsr ProcessWDLDP;
           strJsonWMALP = %trim(strJsonWMALP) + ',' + %trim(strJsonWDLDP);
        Endif;

        strJsonWMALP = %trim(strJsonWMALP) + '}';
       Endsr;

       //=================================================================
       // Build json nodes for contact information
       //=================================================================
       Begsr BuildJsonWMECP;
        // Get the contact designation description
        //designationDesc = GetGenCodeDs(dsWMECP.ECco#:'EMCT':dsWMECP.ECpCnt);
        Exec Sql
         select EXcnt1, EXcnt2, EXcnt3, EXcnt4
          into :strConDesc1, :strConDesc2, :strConDesc3, :strConDesc4
          from WMEXP
          where EXco# = :dsWMECP.ECco# And EXfnd = :dsWMECP.ECfnd;
        Clear designationDesc;
        Select;
        When dsWMECP.ECpCnt = '1';
          designationDesc = strConDesc1;
        When dsWMECP.ECpcnt = '2';
           designationDesc = strConDesc2;
        When dsWMECP.ECpcnt = '3';
           designationDesc = strConDesc3;
        When dsWMECP.ECpcnt = '4';
           designationDesc = strConDesc4;
        Endsl;
        strJsonWMECP = %trim(strJsonWMECP) + '{' +
         '"name":{' +
          %trim(BuildJsonNode('first':dsWMECP.ECfsN)) + ', ' +
          %trim(BuildJsonNode('last':dsWMECP.EClsN)) +
       //   %trim(BuildJsonNode('first':dsWMECP.ECfsN)) + ', ' +
          '},' +
         %trim(BuildJsonNode('position':dsWMECP.ECposn)) + ', ' +
         %trim(BuildJsonAddress(dsWMECP.ECadr1:dsWMECP.ECadr2:dsWMECP.ECcty:
          dsWMECP.ECst:%char(dsWMECP.ECzip):'address')) + ', ' +
         '"designation" : {' +
         %trim(BuildJsonNode('code':dsWMECP.ECpCnt)) + ', ' +
         %trim(BuildJsonNode('header':designationDesc)) + '}, ' +
         %trim(BuildJsonNode('email':dsWMECP.ECeMal)) + ', ' +
         '"phone":[' +
          '{'+%trim(BuildJsonPhone(%char(dsWMECP.ECarea):%char(dsWMECP.ECphn#):
           '':'phone1')) + '}, ' +
          '{'+%trim(BuildJsonPhone(%char(dsWMECP.ECfAra):%char(dsWMECP.ECfPhn):
           'fax':'fax')) + '}, ' +
          '{'+%trim(BuildJsonPhone(%char(dsWMECP.ECcAra):%char(dsWMECP.ECcPhn):
           'cell':'phone2')) + '} ' +
          ']' +
         '}';
       Endsr;

       //=================================================================
       // Build json nodes for master information
       //=================================================================
       Begsr BuildJsonWMEMP;

        // If values is not Y then set to N
        If dsWMEMP.EMnamP <> 'Y';
           dsWMEMP.EMnamP = 'N';
        EndIf;
        If dsWMEMP.EMfdnP <> 'Y';
           dsWMEMP.EMfdnP = 'N';
        EndIf;

        dt7Cx = GetCxDate(dsWMEMP.EMco#:dsWMEMP.EMfnd:dsWMEMP.EMemp#
         :dsWMEMP.EMdiv:strCxReason);

        // Get the Legal Status description
      //***  Begin Chg  ***  12/09/21  **************
      //legalStsDesc = GetGenCodeDs(dsWMEMP.EMco#:'BUSI':dsWMEMP.EMtpob)    ;
        legalStsDesc = %trim(GetGenCodeDs(dsWMEMP.EMco#:'BUSI':dsWMEMP.EMtpob));
      //***  End   Chg  ***  12/09/21  **************

        // Get the SIC description
      //***  Begin Chg  ***  12/09/21  **************
      //sicDesc  = GetGenCodeDs(dsWMEMP.EMco#:'SIC':dsWMEMP.EMsic)          ;
        sicDesc  = %trim(GetGenCodeDs(dsWMEMP.EMco#:'SIC':dsWMEMP.EMsic));
      //***  End   Chg  ***  12/09/21  **************

        // Get the NAIC description
      //***  Begin Chg  ***  12/09/21  **************
      //naicDesc = GetGenCodeDs(dsWMEMP.EMco#:'NAIC':dsWMEMP.EMnaic)        ;
        naicDesc = %trim(GetGenCodeDs(dsWMEMP.EMco#:'NAIC':dsWMEMP.EMnaic));
      //***  End   Chg  ***  12/09/21  **************

        strJsonWMEMP = %trim(strJsonWMEMP) + '{' +
         %trim(BuildJsonNode('division':%char(dsWMEMP.EMdiv))) + ', ' +

         '"masterInformation": {' +
          %trim(BuildJsonNode('fein':%char(dsWMEMP.EMfein))) + ', ' +
          '"name":{' +
         //***  Begin Chg  ***  12/09/21  ************** +
         //%trim(BuildJsonNode('dbaPersonalName':dsWMEMP.EMnamP)) + ', ' +
         //%trim(BuildJsonNode('dba':dsWMEMP.EMname + dsWMEMP.EMnam2)) + ', ' +
         //%trim(BuildJsonNode('federalPersonalName':dsWMEMP.EMfdnP)) + ', '+
         //%trim(BuildJsonNode('federal':dsWMEMP.EMfdNm  + dsWMEMP.EMfdn2)) +
           BuildJsonNode('dbaPersonalName':%trim(dsWMEMP.EMnamP)) + ', ' +
           BuildJsonNode('dba':%trim(dsWMEMP.EMname + dsWMEMP.EMnam2)) + ', ' +
           BuildJsonNode('federalPersonalName':%trim(dsWMEMP.EMfdnP)) + ', '+
           BuildJsonNode('federal':%trim(dsWMEMP.EMfdNm  + dsWMEMP.EMfdn2)) +
         //***  End   Chg  ***  12/09/21  ************** +
          '},' +
          BuildJsonNode('stateRptFlg':dsWMEMP.EMstRp) + ', ' +
         '"dates":{' +
          BuildJsonNode('effective':%char(dsWMEMP.EMefD1):'date') + '}, '+

         CxJSONData(dsWMEMP.EMco#:dsWMEMP.EMfnd:dsWMEMP.EMemp#:dsWMEMP.EMdiv) +
          ', ' +

          '"address":{' +
           %trim(BuildJsonAddress(dsWMEMP.EMmAd1:dsWMEMP.EMmAd2:dsWMEMP.EMmCty:
            dsWMEMP.EMmSt:%char(dsWMEMP.EMmZip):'mailing')) + ', ' +
           %trim(BuildJsonAddress(dsWMEMP.EMpAd1:dsWMEMP.EMpAd2:dsWMEMP.EMpCty:
            dsWMEMP.EMpSt:%char(dsWMEMP.EMpZip):'physical')) +
           '}, ' +

          '"phone":[' +
           '{'+%trim(BuildJsonPhone(%char(dsWMEMP.EMarea):%char(dsWMEMP.EMphn#):
            '':'phone1')) + '}, ' +
           '{'+%trim(BuildJsonPhone(%char(dsWMEMP.EMfaxA):%char(dsWMEMP.EMfax#):
            'fax':'fax')) + '} ' +
          '], ' +

         '"underwriter":{' +
          BuildJsonNode('staffCode':dsWMEMP.EMuwtr) + ',' +
          BuildJsonNode('name'
           :%trim(GetUsrName(GetUserId(dsWMEMP.EMuwtr)))) + ',' +
          BuildJsonNode('email':%trim(GetUsrEmail(GetUserId(dsWMEMP.EMuwtr)))) +
           '},' +

         '"legalStatus":{' +
          %trim(BuildJsonNode('code':dsWMEMP.EMtpob)) + ', ' +
          %trim(BuildJsonNode('description':legalStsDesc)) +
         '}, ' +

         '"SIC":{' +
          %trim(BuildJsonNode('code':dsWMEMP.EMsic)) + ', ' +
          %trim(BuildJsonNode('description':sicDesc)) +
         '}, ' +

         '"NAIC":{' +
          %trim(BuildJsonNode('code':dsWMEMP.EMnaic)) + ', ' +
          %trim(BuildJsonNode('description':naicDesc)) +
         '}, ' +

          %trim(BuildJsonNode('natureOfBusiness':%Trim(dsWMEMP.EMnaob))) +
          ', ' +

          %trim(BuildJsonNode('otherRatingId':'')) + ', ' +
          %trim(BuildJsonNode('additionalCoverages':'')) + ', ';

        If isLossCalculated;
           dsInfo = dsInfoSingle(dsWMEMP.EMdiv+1);
           strJsonWMEMP += BuildJsonNode('grossWritten'
            :%editc(dsInfo.premium.written:'3')) + ',';
           strJsonWMEMP += BuildJsonNode('earnedPremium'
            :%editc(dsInfo.premium.earned:'3')) + ',';
           strJsonWMEMP += BuildJsonNode('incurredLosses'
            :%editc(dsInfo.loss.incurred:'3')) + ',';
           strJsonWMEMP += BuildJsonNode('lossRatio'
            :%editc(dsInfo.loss.ratio:'3')) + ',';
        Endif;

        strJsonWMEMP +=
         '"lastNodeWMEMP":"last json Node prior to file reads (ignore)" ';

        Reset dsWMALP;
        Exsr BuildSqlWMOOP;
        Exsr ProcessWMOOP;
        strJsonWMEMP = %trim(strJsonWMEMP) + ',' + %trim(strJsonWMOOP);

        Exsr BuildSqlWMECP;
        Exsr ProcessWMECP;
        strJsonWMEMP = %trim(strJsonWMEMP) + ',' + %trim(strJsonWMECP);

        Exsr BuildSqlWMALP;
        Exsr ProcessWMALP;
        strJsonWMEMP = %trim(strJsonWMEMP) + ',' + %trim(strJsonWMALP);

        If %parms >= 7 and inFyr <> '***';
           strJsonWMEMP = %scanrpl(',':'':strJsonWMEMP:%len(strJsonWMEMP)-1);
           strJsonWMEMP = %trim(strJsonWMEMP) + ', ' +
            %trim(BuildPolicyDetailInfo(%editc(inCo#:'X'):%editc(inFnd:'X'):
            %editc(inEmp#:'X'):%editc(dsWMEMP.EMdiv:'X'):inFyr));
        Endif;

        strJsonWMEMP = %trim(strJsonWMEMP) + '}}';

       Endsr;

       //=================================================================
       // Build json nodes for owner/officer information
       //=================================================================
       Begsr BuildJsonWMOOP;
        strJsonWMOOP = %trim(strJsonWMOOP) + '{' +
         %trim(BuildJsonNode('id':%EditC(dsWMOOP.OOseq#:'X'))) + ', ' +
         '"name":{' +
          %trim(BuildJsonNode('first':dsWMOOP.OOoFsN)) + ', ' +
          %trim(BuildJsonNode('last':dsWMOOP.OOoLsN)) + ', ' +
          %trim(BuildJsonNode('middle':'')) +
          '},' +
         %trim(BuildJsonNode('title':dsWMOOP.OOtitl)) + ', ' +
         %trim(BuildJsonNode('dob':%char(dsWMOOP.OOdOB):'date')) + ', ' +
         %trim(BuildJsonNode('ssn':dsWMOOP.OOoSS#)) + ', ' +
         %trim(BuildJsonNode('ownership':%editc(dsWMOOP.OOofPc:'3'))) + ', ' +
         %trim(BuildJsonNode('coverage':dsWMOOP.OOexFl)) + ', ' +
         '"dates":{' +
          %trim(BuildJsonNode('effective':%char(dsWMOOP.OOefDt):'date'))+', '+
          %trim(BuildJsonNode('expiration':%char(dsWMOOP.OOexDt):'date'))+'},'+
         %trim(BuildJsonNode('classCode':%editc(dsWMOOP.OOclas:'X'))) +
         '}';
       Endsr;

       //=================================================================
       // Build SQL to access WDLDP
       //=================================================================
       Begsr BuildSqlWDLDP;
        strSqlWDLDP = 'select * ' +
         'from WDLDP ' +
         'where LDCO#  = ' + %char(dsWMALP.ALco#)  +
          ' and LDFND  = ' + %char(dsWMALP.ALfnd)  +
          ' and LDEMP# = ' + %char(dsWMALP.ALemp#) +
          ' and LDDIV  = ' + %char(dsWMALP.ALdiv)  +
          ' and LDELID = ''' + dsWMALP.ALeLId + '''';
        If %parms >= 5 and inFyr <> *blanks;
           strSqlWDLDP = %trim(strSqlWDLDP) + ' ' +
            'and LDFYR = ' + inFyr;
        Endif;
        strSqlWDLDP = %trim(strSqlWDLDP) + ' ' +
         'order by LDCO#, LDFND, LDEMP#, LDDIV, LDELID, LDFYR desc';
        Exec SQL prepare sqlWDLDP from :strSqlWDLDP;
        Exec SQL declare cursorWDLDP cursor for sqlWDLDP;
        Exec SQL open cursorWDLDP;
       Endsr;

       //=================================================================
       // Build SQL for WIAPPANS (Application Responses)
       //=================================================================
       Begsr BuildSqlWIAPPANS;
        strSqlWIAPPANS = 'Select * From WIAPPANS' +
         ' Where APco# = ' + %Char(inCo#) +
         ' And APapp# = ' + %Char(inApp#);
        Exec Sql Prepare sqlWIAPPANS From :strSqlWIAPPANS;
        Exec SQL Declare cursorWIAPPANS Cursor For sqlWIAPPANS;
        Exec SQL Open cursorWIAPPANS;
       Endsr;
       //=================================================================
       // Build SQL for WIGSP (Gradient Score)
       //=================================================================
       Begsr BuildSqlWIGSP;

        If not isGetScoreInfo;
           Leavesr;
        Endif;

        strSqlWIGSP = 'Select * From WIGSP' +
         ' Where GSco# = ' + %Char(inCo#) +
         ' And GSfnd = ' + %Char(inFnd) +
         ' And GSemp# = ' + %Char(inEmp#) +
         ' And GSfYr = ' + %Char(inFyrWIEMP);

        Exec Sql Prepare sqlWIGSP From :strSqlWIGSP;
        Exec SQL Declare cursorWIGSP Cursor For sqlWIGSP;
        Exec SQL Open cursorWIGSP;

       Endsr;
       //*** Begin Add *** 08/05/21 **************************************
       //=================================================================
       // Build SQL for WIVSP (Valen score)
       //=================================================================
       Begsr BuildSqlWIVSP;

        If not isGetScoreInfo;
           Leavesr;
        Endif;

        strSqlWIVSP = 'Select * From WIVSP' +
         ' Where VSco# = ' + %Char(inCo#) +
         ' And VSfnd = ' + %Char(inFnd) +
         ' And VSemp# = ' + %Char(inEmp#) +
         ' And VSfYr = ' + %Char(inFyrWIEMP);

        Exec Sql Prepare sqlWIVSP From :strSqlWIVSP;
        Exec SQL Declare cursorWIVSP Cursor For sqlWIVSP;
        Exec SQL Open cursorWIVSP;

        strSqlWIVSPEXP = 'Select * From WIVSPEXP' +
         ' Where VSco# = ' + %Char(inCo#) +
         ' And VSfnd = ' + %Char(inFnd) +
         ' And VSemp# = ' + %Char(inEmp#) +
         ' And VSfYr = ' + %Char(inFyrWIEMP);

        Exec Sql Prepare sqlWIVSPEXP From :strSqlWIVSPEXP;
        Exec SQL Declare cursorWIVSPEXP Cursor For sqlWIVSPEXP;
        Exec SQL Open cursorWIVSPEXP;
       Endsr;
       //*** End   Add *** 08/05/21 **************************************
       //=================================================================
       // Build SQL to access WMALP
       //=================================================================
       Begsr BuildSqlWMALP;
        strSqlWMALP = 'select * ' +
         'from WMALP ' +
         'where ALCO#  = ' + %char(dsWMEMP.EMco#)  +
          ' and ALFND  = ' + %char(dsWMEMP.EMfnd)  +
          ' and ALEMP# = ' + %char(dsWMEMP.EMemp#) +
          ' and ALDIV  = ' + %char(dsWMEMP.EMdiv);
        Exec SQL prepare sqlWMALP from :strSqlWMALP;
        Exec SQL declare cursorWMALP cursor for sqlWMALP;
        Exec SQL open cursorWMALP;
       Endsr;

       //=================================================================
       // Build SQL to access WMECP
       //=================================================================
       Begsr BuildSqlWMECP;
        strSqlWMECP = 'select * ' +
         'from WMECP ' +
         'where ECCO#  = ' + %char(dsWMEMP.EMco#)  +
          ' and ECFND  = ' + %char(dsWMEMP.EMfnd)  +
          ' and ECEMP# = ' + %char(dsWMEMP.EMemp#) +
          ' and ECDIV  = ' + %char(dsWMEMP.EMdiv);
        Exec SQL prepare sqlWMECP from :strSqlWMECP;
        Exec SQL declare cursorWMECP cursor for sqlWMECP;
        Exec SQL open cursorWMECP;
       Endsr;

       //=================================================================
       // Build SQL to access WMEMP
       //=================================================================
       Begsr BuildSqlWMEMP;
        strSqlWMEMP = 'select * ' +
         'from WMEMP ' +
         'where EMCO# = ' + %editc(inCo#:'X') + ' and EMFND = ' +
          %editc(inFnd:'X') + ' and EMEMP# = ' + %editc(inEmp#:'X');
        If %parms >= 4 and inDiv <> *blanks;
           strSqlWMEMP = %trim(strSqlWMEMP) + ' ' +
            'and EMDIV = ' + inDiv;
        Endif;
        strSqlWMEMP = %trim(strSqlWMEMP) + ' ' +
         'order by EMCO#, EMFND, EMEMP#, EMDIV';
        Exec SQL prepare sqlWMEMP from :strSqlWMEMP;
        Exec SQL declare cursorWMEMP cursor for sqlWMEMP;
        Exec SQL open cursorWMEMP;
       Endsr;

       //=================================================================
       // Build SQL to access WMOOP
       //=================================================================
       Begsr BuildSqlWMOOP;
        strSqlWMOOP = 'select * ' +
         'from WMOOP ' +
         'where OOCO#  = ' + %char(dsWMEMP.EMco#)  +
          ' and OOFND  = ' + %char(dsWMEMP.EMfnd)  +
          ' and OOEMP# = ' + %char(dsWMEMP.EMemp#) +
          ' and OODIV  = ' + %char(dsWMEMP.EMdiv)  +
          ' and OOELID = ''' + dsWMALP.ALeLId + '''';
        Exec SQL prepare sqlWMOOP from :strSqlWMOOP;
        Exec SQL declare cursorWMOOP cursor for sqlWMOOP;
        Exec SQL open cursorWMOOP;
       Endsr;

       //*** Begin Add *** 08/05/21 ***************************************
       //=================================================================
       // Process Scoring node
       //=================================================================
       Begsr ProcessScoring;
       If not isGetScoreInfo;
          Leavesr;
       Endif;
       isValidWIVSP = *Off;
       isGradiant = *Off;
       isValen = *Off;
       strScrVnd = *blank;
       strJsonScoring = '"scoring":[';
       strJsonScoring +='{' +
       BuildJsonNode('year':%Char(inFyrWIEMP)) +
        ', ' ;
       Exec SQL fetch next from cursorWIGSP into :dsWIGSP;
       If isValidFetch(sqlStt);
          Exsr ProcessWIGSP;
       Endif;
       Exec SQL fetch next from cursorWIVSP into :dsWIVSP;
       If isValidFetch(sqlStt);
          Exsr ProcessWIVSP;
       Endif;

       Select;
       When isGradiant = *On and isValen = *On;
          strJsonScoring += BuildJsonNode('vendor':strScrVnd) + ', ' +
           strJsonWIGSP + strJsonWIVSP;
       When isGradiant = *On;
          strJsonScoring += BuildJsonNode('vendor':strScrVnd) + ', ' +
           strJsonWIGSP;
       When isValen = *On;
          strJsonScoring += BuildJsonNode('vendor':strScrVnd) + ', ' +
           strJsonWIVSP;
       Other;
          If intVndEfDt = *Zero;
             intVndEfDt = dsWMEMP.EMefd1;
          Endif;
          strScrVnd = GetGrScoringVnd(inCo#:inFnd:intVndEfdt);
          strJsonScoring += BuildJsonNode('vendor':strScrVnd);
       Endsl;

       strJsonScoring+= '}]';
       Endsr;
       //*** End   Add *** 08/05/21 ***************************************
       //=================================================================
       // Process SQL to access WDLDP
       //=================================================================
       Begsr ProcessWDLDP;
        strJsonWDLDP = '"years":[';
        Dou isEofWDLDP;
           Exec SQL fetch next from cursorWDLDP into :dsWDLDP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWDLDP = *on;
              Iter;
           When %len(%trim(strJsonWDLDP)) > 9;
              strJsonWDLDP = %trim(strJsonWDLDP) + ',';
           Endsl;
           // Build json with master information
           Exsr BuildJsonWDLDP;
        Enddo;
        strJsonWDLDP = %trim(strJsonWDLDP) + ']';
        Exec SQL close cursorWDLDP;
       Endsr;
       //=================================================================
       // Process SQL to access WIAPPANS
       //=================================================================
       Begsr ProcessWIAPPANS;
        // ListQuestions will be used directly from Portal
        Leavesr;
        outParms  = '  {' + BuildJsonNode('template':'*RESPONSES') +
         ',' + BuildJsonNode('company':%char(inCo#)) +
         ',' + BuildJsonNode('app#':%char(inApp#)) +
         '}';
        strProgram = 'WIQUEST';
        ListQuestions('ListQuestions':inUser:outParms:inJson:*blanks);
        strJsonWIAPPANS = %subst(inJson:2) + ',';
        Leavesr;
        Reset intCtr;
        strJsonWIAPPANS = '"questionList":[';
        Dou Not IsValidFetch(sqlStt);
           Exec SQL fetch next from cursorWIAPPANS into :dsWIAPPANS;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           intCtr += 1;
           If intCtr > 1;
              strJsonWIAPPANS = %Trim(strJsonWIAPPANS) + ', ';
           Endif;
           Exsr BuildJsonWIAPPANS;
        Enddo;
        strJsonWIAPPANS = %trim(strJsonWIAPPANS) + ']' + ',';
        Exec SQL close cursorWIAPPANS;
       Endsr;
       //=================================================================
       // Process SQL to access WIGSP
       //=================================================================
       Begsr ProcessWIGSP;
       //*** Begin Change *** 08/05/21 **************************
       // If not isGetScoreInfo;
       //    Leavesr;
       // Endif;

       // strJsonWIGSP = '"gradientInfo":[';
       // Dou not IsValidFetch(sqlstt);
       //    Exec SQL fetch next from cursorWIGSP into :dsWIGSP;
       //    Select;
       //    When not IsValidFetch(sqlStt);
       //       Iter;
       //    Endsl;
       //    Exsr BuildJsonWIGSP;
       // Enddo;
        isGradiant = *On;
        strScrVnd = 'Gradient';
        strJsonWIGSP = '"gradient":{';
        Dow IsValidFetch(sqlstt);
           Exsr BuildJsonWIGSP;
           Exec SQL fetch next from cursorWIGSP into :dsWIGSP;
        Enddo;
       //*** End   Change *** 08/05/21 **************************

        strJsonWIGSP = %scanrpl(',':'':strJsonWIGSP:%len(strJsonWIGSP)-1);
        //Begin Delete ** 08/05/21 ********************************
        //strJsonWIGSP += ']';
        //End Delete ** 08/05/21 *********************************
        Exec SQL close cursorWIGSP;

       Endsr;
       //*** Begin Add *** 08/05/21 ***************************************
       //=================================================================
       // Process SQL to access WIVSP
       //=================================================================
       Begsr ProcessWIVSP;
        If isGradiant = *On;
           strJsonWIVSP = ',"valen":{';
        Else;
           strJsonWIVSP = '"valen":{';
        Endif;

        isValen = *On;
        isValidWIVSP = *On;
        strScrVnd = 'Valen';
        Exsr ProcessWIVSPEXP;

        Dow IsValidWIVSP = *On;
           Exsr BuildJsonWIVSP;
           Exec SQL fetch next from cursorWIVSP into :dsWIVSP;
           If isValidFetch(sqlstt);
              isValidWIVSP = *On;
           Else;
              isValidWIVSP = *Off;
           Endif;
        Enddo;

        strJsonWIVSP = %scanrpl(',':'':strJsonWIVSP:%len(strJsonWIVSP)-1);
        strJsonWIVSP += '}';
        Exec SQL close cursorWIVSP;
       Endsr;
       //=================================================================
       // Process SQL to access WIVSPEXP
       //=================================================================
       Begsr ProcessWIVSPEXP;
        Reset strJsonWIVSPEXP;
        Exec SQL fetch next from cursorWIVSPEXP into :dsWIVSPEXP;
        If IsValidFetch(sqlstt);
           Dow IsValidFetch(sqlstt) ;
              Exsr BuildJsonWIVSPEXP;
              Exec SQL fetch next from cursorWIVSPEXP into :dsWIVSPEXP;
           Enddo;
           strJsonWIVSPEXP=
            %scanrpl(',':']':strJsonWIVSPEXP:%len(strJsonWIVSPEXP)-1);
           Exec SQL close cursorWIVSPEXP;
        Endif;
        strJsonWIVSPEXP = ' "explanations":{' + strJsonWIVSPEXP + '}';
       Endsr;
       //*** End   Add *** 08/05/21 **************************************
       //=================================================================
       // Process SQL to access WMALP
       //=================================================================
       Begsr ProcessWMALP;
        strJsonWMALP = '"locations":[';
        Dou isEofWMALP;
           Exec SQL fetch next from cursorWMALP into :dsWMALP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMALP = *on;
              Iter;
           When %len(%trim(strJsonWMALP)) > 13;
              strJsonWMALP = %trim(strJsonWMALP) + ',';
           Endsl;
           // Build json with master information
           Exsr BuildJsonWMALP;
        Enddo;
        strJsonWMALP = %trim(strJsonWMALP) + ']';
        Exec SQL close cursorWMALP;
       Endsr;

       //=================================================================
       // Process SQL to access WMECP
       //=================================================================
       Begsr ProcessWMECP;
        strJsonWMECP = '"contacts":[';
        Dou isEofWMECP;
           Exec SQL fetch next from cursorWMECP into :dsWMECP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMECP = *on;
              Iter;
           When %len(%trim(strJsonWMECP)) > 12;
              strJsonWMECP = %trim(strJsonWMECP) + ',';
           Endsl;
           // Build json with master information
           Exsr BuildJsonWMECP;
        Enddo;
        strJsonWMECP = %trim(strJsonWMECP) + ']';
        Exec SQL close cursorWMECP;
       Endsr;

       //=================================================================
       // Process SQL to access WMEMP
       //=================================================================
       Begsr ProcessWMEMP;

       //***  Begin Chg  ***  09/11/21  ********************
       //**********strJsonWMEMP = ', "divisions":[';
        strJsonWMEMP += ' "divisions":[';
       //***  End   Chg  ***  09/11/21  ********************
        Dou isEofWMEMP;
           Exec SQL fetch next from cursorWMEMP into :dsWMEMP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMEMP = *on;
              Iter;
           When %len(%trim(strJsonWMEMP)) > 15;
              strJsonWMEMP = %trim(strJsonWMEMP) + ',';
           Endsl;
           // Build json with master information
           Exsr BuildJsonWMEMP;
        Enddo;
        strJsonWMEMP = %trim(strJsonWMEMP) + ']';
        Exec SQL close cursorWMEMP;
       Endsr;

       //=================================================================
       // Process SQL to access WMOOP
       //=================================================================
       Begsr ProcessWMOOP;
        strJsonWMOOP = '"ownersOfficers":[';
        Dou isEofWMOOP;
           Exec SQL fetch next from cursorWMOOP into :dsWMOOP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWMOOP = *on;
              Iter;
           When %len(%trim(strJsonWMOOP)) > 18;
              strJsonWMOOP = %trim(strJsonWMOOP) + ',';
           Endsl;
           // Build json with master information
           Exsr BuildJsonWMOOP;
        Enddo;
        strJsonWMOOP = %trim(strJsonWMOOP) + ']';
        Exec SQL close cursorWMOOP;
       Endsr;

       //=================================================================
       // Build json node for state summary information
       //=================================================================
       Begsr StateSummary;

        strJsonSummary = '"stateSummary":[';

        strSqlSummary = 'Select ELFYR, RSJRST, sum(ELANPR) From wdelp' +
         ' join wmrsp on rsco# = elco# and rsrtcd = elrtcd ' +
         ' Where elco# = ' + %Char(inCo#) +
         ' And elfnd = ' + %Char(inFnd) +
         ' And elemp# = ' + %Char(inEmp#);

        If %parms >= 7 and inFyr <> '***' and inFyr <> *blanks;
           strSqlSummary = %trim(strSqlSummary) + ' And elfyr = ' + inFyr;
        Endif;

        strSqlSummary = %trim(strSqlSummary) +
         ' Group by elfyr, rsjrst ' +
         ' Order by elfyr, rsjrst ';

        Exec Sql Prepare sqlSummary From :strSqlSummary;
        Exec Sql Declare cursorSummary Cursor For sqlSummary;
        Exec Sql Open cursorSummary;

        Reset intCtr;
        Dou Not IsValidFetch(sqlStt);
           Exec SQL fetch next from cursorSummary into :dsSummary;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           intCtr += 1;
           If intCtr > 1;
              strJsonSummary = %Trim(strJsonSummary) + ', ';
           Endif;

           strJsonSummary = %Trim(strJsonSummary) +
            '{' +
            %Trim(BuildJsonNode('year':%char(dsSummary.year))) + ',   ' +
            %Trim(BuildJsonNode('state':dsSummary.state)) + ',   ' +
            %Trim(BuildJsonNode('premium':%Char(dsSummary.annPrem))) +
            '}';
        Enddo;

       //***  Begin Chg  ***  09/11/21  ****************
       //**********strJsonSummary = %trim(strJsonSummary) + ']';
        strJsonSummary += '], ';
       //***  End   Chg  ***  09/11/21  ****************

       Endsr;
      /End-Free
     pBuildPolicyMasterInfo...
     p                 e

      *==========================================================
      * Description: This routine will use the global variable, strSqlCursor,
      *  to process for calculating premium and losses for the policies.
      *  The SQL must be able to be loaded into dsPolDtl.
      *==========================================================
     pCalculatePremiumLosses...
     p                 b

     disEofLosses      s              1n   inz
     d##amount         s             11
     d##amount2        s             11

     ddsPolDtl         ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     d stRp                           1
     d faf                            1
     d stam                          11  2
     d anPr                          11  2
     d aaPr                          11  2

      /Free
       Exec sql close cursorLosses;
       Exec sql prepare sqlLosses from :strSqlCursor;
       Exec sql declare cursorLosses cursor for sqlLosses;
       Exec sql open cursorLosses;

       Exsr Calculate;

       //=================================================================
       // Calculate premium/losses
       //=================================================================
       Begsr Calculate;
        Dou isEofLosses;
           Reset dsInfo;
           Exec sql fetch next from cursorLosses into :dsPolDtl;
           Select;
           When not IsValidFetch(sqlstt);
              isEofLosses = *on;
              Iter;
           Endsl;
           // Get the written premium
           Select;
           When dsPolDtl.stam = 0 and dsPolDtl.faf = 'A';
              dsInfo.premium.written = dsPolDtl.aaPr;
              dsInfoSum.premium.written += dsPolDtl.aaPr;
           When dsPolDtl.stam = 0;
              dsInfo.premium.written = dsPolDtl.anPr;
              dsInfoSum.premium.written += dsPolDtl.anPr;
           Other;
              dsInfo.premium.written = dsPolDtl.stam;
              dsInfoSum.premium.written += dsPolDtl.stam;
           Endsl;
           // Get the earned premium
           ##amount = ' ';
           ##amount2 = ' ';
           SrPrt1(%editc(dsPolDtl.co#:'X'):%editc(dsPolDtl.fnd:'X')
            :%editc(dsPolDtl.fyr:'X'):%editc(dsPolDtl.emp#:'X')
            :%editc(dsPolDtl.div:'X'):%editc(DateTo8(SystemDate):'X')
            :##amount:##amount2:'1');
           Monitor;
              dsInfoSum.premium.earned += %Dec(##amount:11:2)/100;
              dsInfo.premium.earned = %dec(##amount:11:2)/100;
           On-Error;
              dsInfoSum.premium.earned += 0;
           Endmon;
           // Get the total incurred
           dsInfo.loss.incurred = GetIncLoss(dsPolDtl.co#:dsPolDtl.fnd
            :dsPolDtl.emp#:dsPolDtl.div:dsPolDtl.fyr);
           dsInfoSum.loss.incurred += dsInfo.loss.incurred;
           // Calculate loss ratio
           If dsInfo.premium.earned <> 0;
             Monitor;
                dsInfo.loss.ratio =
                 %dech(dsInfo.loss.incurred / dsInfo.premium.earned:5:4) * 100;
             On-Error;
                dsInfo.loss.ratio = 9999999.99;
             EndMon;
           Else;
             dsInfo.loss.ratio = 0;
           EndIf;
           dsInfoSingle(dsPolDtl.div+1) = dsInfo;
        Enddo;

        // Calculate loss ratio
        If dsInfoSum.premium.earned <> 0;
          Monitor;
             dsInfoSum.loss.ratio =
              %dech(dsInfoSum.loss.incurred/dsInfoSum.premium.earned:5:4)*100;
          On-Error;
             dsInfoSum.loss.ratio = 9999999.99;
          EndMon;
        Else;
          dsInfoSum.loss.ratio = 0;
        EndIf;
        isLossCalculated = *on;
       Endsr;

      /End-free
     pCalculatePremiumLosses...
     p                 e
      *==========================================================
      * Delete quote from associated files
      *==========================================================
     pDeleteQuote...
     p                 b
     dDeleteQuote...
     d                 pi
     dinCo#                           3  0 const
     dinFnd                           3  0 const
     dinFyr                           3  0 const
     dinEmp#                          9  0 const
     dinDiv                           5  0 const options(*nopass)
      /Free
       Exec sql
        delete from WAQTP
         where QTco#=:inCo# and QTfnd=:inFnd and QTemp#=:inEmp#;
       Exec sql
        delete from WDELP
         where ELco#=:inCo# and ELfnd=:inFnd and ELemp#=:inEmp#;
       Exec sql
        delete from WDXSP
         where XSco#=:inCo# and XSfnd=:inFnd and XSemp#=:inEmp#;
       Exec sql
        delete from WIAPP
         where APco#=:inCo# and APfnd=:inFnd and APemp#=:inEmp#;
       Exec sql
        delete from WIAPPANS
         where APco#=:inCo# and APfnd=:inFnd and APemp#=:inEmp#;
       Exec sql
        delete from WIAPPLOS
         where APco#=:inCo# and APfnd=:inFnd and APemp#=:inEmp#;
       Exec sql
        delete from WIEMP
         where EMco#=:inCo# and EMfnd=:inFnd and EMemp#=:inEmp#;
       Exec sql
        delete from WIGSP
         where GSco#=:inCo# and GSfnd=:inFnd and GSemp#=:inEmp#;
       Exec sql
        delete from WIGSPDRV
         where GSco#=:inCo# and GSfnd=:inFnd and GSemp#=:inEmp#;
       Exec sql
        delete from WMAHP
         where AHco#=:inCo# and AHfnd=:inFnd and AHemp#=:inEmp#;
       Exec sql
        delete from WMALP
         where ALco#=:inCo# and ALfnd=:inFnd and ALemp#=:inEmp#;
       Exec sql
        delete from WMECP
         where ECco#=:inCo# and ECfnd=:inFnd and ECemp#=:inEmp#;
       Exec sql
        delete from WMEMP
         where EMco#=:inCo# and EMfnd=:inFnd and EMemp#=:inEmp#;
       Exec sql
        delete from WMEPP
         where EPco#=:inCo# and EPfnd=:inFnd and EPemp#=:inEmp#;
       Exec sql
        delete from WMLDP
         where LDco#=:inCo# and LDfnd=:inFnd and LDemp#=:inEmp#;
       Exec sql
        delete from WMNBP
         where NBco#=:inCo# and NBfnd=:inFnd and NBemp#=:inEmp#;
       Exec sql
        delete from WMOOP
         where OOco#=:inCo# and OOfnd=:inFnd and OOemp#=:inEmp#;
      /End-free
     pDeleteQuote...
     p                 e
      *==========================================================
      * Request:  Duplicate FEIN Check
      * Description: Returns true/false in dupFeinExists tag if
      *  FEIN exists in Quote within days allowed or if
      *  FEIN exists in a live active policy
      * inParm: feinToCheck    :char (required - must convert to integer)
      * inParm: #OfDaysAllowed :char (optional - must convert to integer)
      * outJSON: rtnCode       : ""/"Error"
      * outJSON: requestParms  :
      * outJSON: errorMsg      : 500 char varying
      * outJSON: results       : dupFeinExists: true/false  boolean
      *==========================================================
     p DupFein         b

     d compDate        s               d
     d hasDupFein      s              5a
     d idx             s             10i 0
     d int#OfDays      s              3  0
     d intCompDate     s              7  0
     d intFein         s              9  0
     d intMinLivEfdt   s              7  0
     d intMinPrtEfdt   s              7  0
     d intMinSbmEfdt   s              7  0
     d isLocked        s               n
     d rtnCode         s              5a
     d str#Days        s              4a
     d strFein         s             10a   varying
     d strLivLib       s             10
     d strStatus       s             10

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr BuildSql;
       Endif;
       Exsr BuildReturnJSON;
       Return;

       //-------------------------------------------
       Begsr BuildReturnJSON;
       //-------------------------------------------
        If ioRtnCode <> 'Error';
           Select;
           When intMinSbmEfdt = 0 And intMinLivEfDt = 0
            And intMinPrtEfdt = 0;
              hasDupFein = 'false';
              strStatus = 'Available';
           When isLocked = *On;
              hasDupFein = 'true';
              strStatus = 'Locked';
           When intMinPrtEfdt <> 0;
              hasDupFein = 'false';
              strStatus = 'Exists';
           EndSl;
           json = json + ' "results": ' +
            '{' +
            %Trim(BuildJsonNode('dupFeinExists':hasDupFein:'boolean')) + ',' +
            %Trim(BuildJsonNode('status':strStatus)) + ',' +
            %Trim(BuildJsonNode('type':'FEIN')) +
            '}';
        Else;
           comma = *Blank;
           For idx = 1 to %Elem(errMsgArray);
              If errMsgArray(idx) = *Blanks;
                 Leave;
              EndIf;
              If comma <> *Blank;
                 array = %Trim(array) + comma;
              Else;
                 comma = ',';
              Endif;
              array = %Trim(array) + errMsgArray(idx);
           EndFor;
           idx = %CheckR(' ':json);
           Select;
           When idx = 0;
           Other;
              If %Subst(json:idx:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ] ';
        EndIf;
        outJson = json;
       Endsr;
       //-------------------------------------------
       // Build SQLs
       //-------------------------------------------
       Begsr BuildSQL;
       // Find FEIN on submitted quote within date range
        strSqlCursor = 'Select Min(EMefd1) From ' +
         %Trim(RtvQuoteLib('*QTE')) + '/WMEMP ' +
         'Where EMco# = ' + strCo# +
         ' And EMfein = ' + strFein +
         ' And EMefd1 >= ' + %EditC(intCompDate:'X') +
         ' And EMfqlk = '' ''';
        Exec SQL Prepare qteString From :strSqlCursor;
        Exec Sql Declare q1 Cursor For qteString;
        Exec Sql Open q1;
        If SqlStt <> '00000' And SqlStt <> '22002';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Open Q1 SQL statement failed. ' +
            'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X') + '"';
           ioRtnCode = 'Error';
        Else;
           Exec Sql Fetch q1 into :intMinSbmEfDt;
           Select;
           When SqlStt = '00000';   // got a hit
           When SqlStt = '22002';   // null result
           Other;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Fetch Q1 SQL statement failed. ' +
               'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X')  + '"';
              ioRtnCode = 'Error';
           Endsl;
        Endif;
        Exec Sql Close q1;
        If intMinSbmEfDt <> 0;
           isLocked = *On;
           LeaveSr;
        EndIf;
       // Find FEIN on active live policy
        strLivLib = %Trim(RtvQuoteLib('*LIV'));
        strSqlCursor = 'Select Min(EMefd1) From ' +
         %Trim(strLivLib) + '/WMEMP Join ' +
         %Trim(strLivLib) + '/WDELP' +
         ' On EMco# = ELco# And EMfnd = ELfnd And' +
         ' EMemp# = ELemp# And EMdiv = ELdiv' +
         ' Where EMco# = ' + strCo# +
         ' And EMfein = ' + strFein +
         ' And ELpfdt <= ' + %Editc(sysDate7:'X') +
         ' And ELptdt >  ' + %Editc(sysDate7:'X');
        Exec SQL Prepare livString From :strSqlCursor;
        Exec Sql Declare l1 Cursor For livString;
        Exec Sql Open l1;
        If SqlStt <> '00000' And SqlStt <> '22002';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Open L1 SQL statement failed. ' +
            'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X') + '"';
           ioRtnCode = 'Error';
        Else;
           Exec Sql Fetch l1 into :intMinLivEfdt;
           Select;
           When SqlStt = '00000';   // got a hit
           When SqlStt = '22002';   // null result
           Other;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Fetch L1 SQL statement failed. ' +
              'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X')  + '"';
              ioRtnCode = 'Error';
           Endsl;
        Endif;
        Exec Sql Close l1;
        If intMinLivEfDt <> 0;
           isLocked = *On;
           LeaveSr;
        EndIf;
       // Find FEIN on Portal Quote
        strSqlCursor = 'Select Min(EMefd1) From ' +
         %Trim(RtvQuoteLib('*QTE')) + '/WMEMP ' +
         'Where EMco# = ' + strCo# +
         ' And EMfein = ' + strFein +
         ' And EMefd1 >= ' + %EditC(intCompDate:'X') +
         ' And EMfqlk = ''P''';
        Exec SQL Prepare qtePortalString From :strSqlCursor;
        Exec Sql Declare q2 Cursor For qtePortalString;
        Exec Sql Open q2;
        If SqlStt <> '00000' And SqlStt <> '22002';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Open Q2 SQL statement failed. ' +
            'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X') + '"';
           ioRtnCode = 'Error';
        Else;
           Exec Sql Fetch q2 into :intMinPrtEfDt;
           Select;
           When SqlStt = '00000';   // got a hit
           When SqlStt = '22002';   // null result
           Other;
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Fetch Q1 SQL statement failed. ' +
               'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X')  + '"';
              ioRtnCode = 'Error';
           Endsl;
        Endif;
        Exec Sql Close q2;
        If intMinPrtEfDt <> 0;
           isLocked = *Off;
           LeaveSr;
        EndIf;
       Endsr;
       //-------------------------------------------
       // Break parms out of JSON
       //-------------------------------------------
       Begsr GetParms;
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';
           node = YAJL_object_find(docNode:'feinToCheck');
           strFein = YAJL_get_string(node);
           BuildGiantJSON(json :'feintoCheck' :strFein);
           Select;
           When %len(strFein) < 8 or %len(strFein) > 9;
              errMsgArray(1) = '"Invalid FEIN."';
              ioRtnCode = 'Error';
           When strFein <> *Blanks;
              Monitor;
                 intFein  = %Int(strFein);
              On-Error;
                 errMsgArray(1) = '"FEIN has character data."';
                 ioRtnCode = 'Error';
              EndMon;
           Other;
              errMsgArray(1) = '"No FEIN was passed in."';
              ioRtnCode = 'Error';
           Endsl;
           node = YAJL_object_find(docNode:'#OfDaysAllowed');
           str#Days = YAJL_get_string(node);
           BuildGiantJSON(json:'#OfDaysAllowed':str#Days);
           json = json + '},';
              If str#Days <> *Blanks;
                 Monitor;
                    int#OfDays = %Int(str#Days);
                 On-Error;
                    idx = %Lookup(*Blanks:errMsgArray);
                    errMsgArray(idx) = '"#OfDaysAllowed has character data."';
                    ioRtnCode = 'Error';
                 EndMon;
              Else;
                 int#OfDays = 0;
              Endif;
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtnCode = 'Error';
           LeaveSr;
        Endif;
        compDate = sysDate + %Days(int#OfDays);
        intCompDate = %Dec(compDate:*CYMD);
       Endsr;
      /End-Free

     p DupFein         e
     p*===================================================================
     p* Get application information
     p*===================================================================
     pGetApplication...
     p                 b
     dGetApplication...
     d                 pi

     DdecWIEMPcnt      s             10i 0 inz
     dflgQte           s              1    inz('Q')
     Di1               s             10i 0 inz
     Dint2             s              2  0 inz
     DinCo#            s              3  0 inz
     DinDiv            s              5  0 inz
     DinEmp#           s              9  0 inz
     DinFnd            s              3  0 inz
     DinFyr            s              3  0 inz
     DinApp#           s              9  0 inz
     DisEofWIAPP       s              1n   inz
     DisEofWIAPPLOS    s              1n   inz
     DisEofWIEMP       s              1n   inz
     DisWIEMPfirst     s              1n   inz(*On)
     dptrResults       s               *
     d***  Begin Chg  ***  12/09/21  **************
     d***strEntryPoint    s             10a   inz
     dstrEntryPoint    s             10a   inz varying
     d***  End   Chg  ***  12/09/21  **************
     DstrJsonAgency    s           5000    inz varying
     DstrJsonProducer  s           5000    inz varying
     DstrJsonRequest   s               a   len(5000) varying
     DstrJsonResults   s               a   len(8500000) varying
     D                                     based(ptrResults)
     DstrJsonWIAPPLOS  s           5000    inz varying
     DstrSqlWIAPPLOS   s            500    inz varying
     DstrSqlWIEMP      s            500    inz varying
     DstrSqlWIEMPwh    s            500    inz varying
     DstrSqlWIEMPyr    s             50    inz varying

     DdsInParms        ds                  inz qualified
     D company                        3
     D group                          3
     D employer                       9
     D division                       5
     D year                           3
     D app#                           9

     DdsWIAPP        e ds                  extname(WIAPP) inz qualified
     DdsWIAPPlos     e ds                  extname(WIAPPLOS) inz qualified
     DdsWIEMP          ds                  inz qualified
     D app#                           9  0
     D co#                            3  0
     D fnd                            3  0
     D emp#                           9  0
     D fyr                            3  0
     D current                        1
      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr BuildOutput;
       Endif;
       //Exsr BuildStub;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJson = json;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetApplicationInfo}'));
       Endsr;

       //=================================================================
       // Build json for agency information
       //=================================================================
       Begsr BuildJsonAgency;
        ptrResults = %alloc(8500000);
        strJsonAgency =
         BuildJsonNode('agentFEIN':%editc(dsWIAPP.APagtFein:'X')) + ',' +
         BuildJsonNode('agentMod':%editc(dsWIAPP.APagtMod:'X')) + ',';
        strJsonRequest = '  {' +
         BuildJsonNode('fein':%editc(dsWIAPP.APagtFein:'X')) + ',' +
         BuildJsonNode('mod':%editc(dsWIAPP.APagtMod:'X')) + ',' +
         BuildJsonNode('includeRequest':'0') + '}';
        strProgram = 'WIAGENCY';
        ListExternal('GetAgency':inUser:strJsonRequest:strJsonResults:*blanks);
        strJsonAgency = strJsonResults + ',';
        Dealloc(n) ptrResults;
       Endsr;

       //=================================================================
       // Build json for producer information
       //=================================================================
       Begsr BuildJsonProducer;
        ptrResults = %alloc(8500000);
        strJsonProducer =
         BuildJsonNode('producer':%editc(dsWIAPP.APprod:'X')) + ',';
        strJsonRequest = '  {' +
         BuildJsonNode('co#':%editc(dsWIAPP.APco#:'X')) + ',' +
         BuildJsonNode('prod#':%editc(dsWIAPP.APprod:'X')) + ',' +
         BuildJsonNode('fein':%editc(dsWIAPP.APagtFein:'X')) + ',' +
         BuildJsonNode('mod':%editc(dsWIAPP.APagtMod:'X')) + ',' +
         BuildJsonNode('includeRequest':'0') + '}';
        strProgram = 'WIPROD';
        ListExternal('GetProducer':inUser:strJsonRequest:strJsonResults:*blank);
        strJsonProducer = strJsonResults + ',';
        Dealloc(n) ptrResults;
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;

        //* Set application # from incoming data.
        If dsInParms.app# <> ' ';
           inApp# = %int(dsInParms.app#);
        Else;
           inApp# = 0;
        Endif;

        inCo# = intCo#;
        If dsInParms.company <> *blank;
           inCo# = %int(dsInParms.company);
        Endif;


        //* Pull group, employer #, and fund year from incoming data.
        If inApp# <> 0;
           Open wieml;
           Setll (inCo#:inApp#) pl100;
           Reade(n) (inCo#:inApp#) pl100;
           Dow not %eof;
              If L1Current = 'Y';
                 inFnd  = L1Fnd;
                 inEmp# = L1Emp#;
                 inFyr  = L1Fyr;
                 If dsInParms.year <> '***';
                    dsInParms.year = %editc(inFyr:'X');
                 Endif;
                 Leave;
              Endif;
              Reade(n) (%int(inCo#):inApp#) pl100;
           Enddo;
           Close wieml;
        Else;
           inFnd = 0;
           inEmp# = 0;
           If dsInParms.group <> *blank;
              inFnd  = %int(dsInParms.group);
           Endif;
           If dsInParms.employer <> *blank;
              inEmp# = %int(dsInParms.employer);
           Endif;
        Endif;

        Select;
        When isEnvQuote and not UseGroupInPortal(inCo#:inFnd:'Quote');
           AddErrorMessageJson('Group ' + %char(inFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        When isEnvLive and not UseGroupInPortal(inCo#:inFnd:'Policy');
           AddErrorMessageJson('Group ' + %char(inFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endsl;

        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

        json += ',"applicationInfo":{';

        Exsr BuildSqlWIAPPLOS;
        Exsr ProcessWIAPPLOS;
        Reset dsWIAPP;
        Exec SQL
         select * into :dsWIAPP
          from WIAPP
          where APAPP# = :inApp#;

        // Get entry point
        Select;
        When dsWIAPP.APiUsr = 'WCEXPRESS';
           strEntryPoint = 'FQ';
        When dsWIAPP.APiUsr = 'CSPORTAL';
           strEntryPoint = 'PORTAL';
        Other;
           strEntryPoint = 'WCMS';
        Endsl;

        Exsr BuildSqlWIEMP;

        Exsr BuildJsonAgency;
        Exsr BuildJsonProducer;

        json +=  '"application":{' +
         BuildJsonNode('number':%editc(dsWIAPP.APapp#:'X')) + ',' +
         BuildJsonNode('policyNumber':%EditC(dsWIEMP.fnd:'X') +
          %EditC(dsWIEMP.emp#:'X') + %EditC(dsWIEMP.fyr:'X')) + ',' +
         strJsonAgency +
         strJsonProducer +
         BuildJsonNode('yearsInBusiness':%editc(dsWIAPP.APyib:'X')) + ',' +
         BuildJsonNode('auditFrequency':dsWIAPP.APaudFrq) + ',' +
         BuildJsonNode('entryPoint':strEntryPoint) + ', ' +
         BuildJsonNode('quoteStatus':GetQuoteStsDesc(dsWIEMP.co#:dsWIEMP.fnd:
          dsWIEMP.emp#:GetStRptDv(dsWIEMP.co#:dsWIEMP.fnd:dsWIEMP.emp#):
          dsWIEMP.fyr:flgQte)) + ', ' +
         '"losses":' + %trim(strJsonWIAPPLOS) + '}';

        Exsr ProcessWIEMP;

        json += '}';

       Endsr;

       //=================================================================
       // Build json nodes for application losses
       //=================================================================
       Begsr BuildSqlWIAPPLOS;
        strSqlWIAPPLOS = 'select * ' +
         'from WIAPPLOS ' +
         'where APAPP# = ' + %editc(inApp#:'X');
        Exec SQL close cursorWIAPPLOS;
        Exec SQL prepare sqlWIAPPLOS from :strSqlWIAPPLOS;
        Exec SQL declare cursorWIAPPLOS cursor for sqlWIAPPLOS;
        Exec SQL open cursorWIAPPLOS;
       Endsr;

       //=================================================================
       // Build json nodes for application cross reference
       //=================================================================
       Begsr BuildSqlWIEMP;
        strSqlWIEMPwh = 'where ';
        If dsInParms.app# <> *blanks;
           strSqlWIEMPwh += 'EMAPP# = ' + dsInParms.app# + ' and ';
        Endif;
        If dsInParms.employer <> *blanks;
           strSqlWIEMPwh += 'EMCO# = ' + dsInParms.company + ' and ' +
            'EMFND = ' + dsInParms.group + ' and ' +
            'EMEMP# = ' + dsInParms.employer + ' and ';
        Endif;
        If dsInParms.year <> *blanks and dsInParms.year <> '***';
           strSqlWIEMPyr += 'and EMFYR = ' + dsInParms.year;
        Endif;
        strSqlWIEMPwh = %subst(strSqlWIEMPwh:1:%len(strSqlWIEMPwh)-4);
        Reset strSqlWIEMP;
        Reset isEofWIEMP;
        Reset isWIEMPfirst;
        Dou isEofWIEMP;
           Exec SQL close cursorWIEMP;
           Select;
           When strSqlWIEMP = *blanks;
              strSqlWIEMP = 'select EMAPP#, EMCO#, EMFND, EMEMP#, EMFYR, ' +
               'EMCURRENT ' +
               'from WIEMP ' + strSqlWIEMPwh + strSqlWIEMPyr;
           When decWIEMPcnt = 0 and dsInParms.app# = *blanks and isWIEMPfirst;
              // If no records exist in x-ref check employer master
              strSqlWIEMP = 'select 0, EMCO#, EMFND, EMEMP#, 0, '''' ' +
               'from WMEMP ' + strSqlWIEMPwh + ' ' +
               'group by EMCO#, EMFND, EMEMP#';
              isWIEMPfirst = *Off;
           Other;
              isEofWIEMP = *on;
           Endsl;
           Reset decWIEMPcnt;
           Exec SQL prepare sqlWIEMP from :strSqlWIEMP;
           Exec SQL declare cursorWIEMP cursor for sqlWIEMP;
           Exec SQL open cursorWIEMP;
           If not isEofWIEMP;
              Exec SQL fetch next from cursorWIEMP into :dsWIEMP;
              If IsValidFetch(sqlStt);
                 decWIEMPcnt = 1;
              Endif;
           Endif;
        Enddo;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        // Load the json tree
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson : strErrorMsg);
        If strErrorMsg = ' ';
           json += ' "requestParms":{';
           // Load the data structure with the parameter values from the tree
           Reset i0;
           Dow YAJL_object_loop(docNode:i0:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'company';
                 dsInParms.company = YAJL_get_string(val);
              When key = 'group';
                 dsInParms.group = YAJL_get_string(val);
              When key = 'employer';
                 dsInParms.employer = YAJL_get_string(val);
              When key = 'division';
                 dsInParms.division = YAJL_get_string(val);
              When key = 'year';
                 dsInParms.year = YAJL_get_string(val);
              When key = 'app#';
                 dsInParms.app# = YAJL_get_string(val);
              When key = 'filters';
                 // Load the filter tree
                 node1 = YAJL_object_find(docNode: 'filters');
                 Reset i1;
                 Dow YAJL_object_loop(node1:i1:key:val);
                 Enddo;
              Endsl;
           Enddo;
           json += '}';
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
       Endsr;

       //=================================================================
       // Process SQL to access WIAPPLOS
       //=================================================================
       Begsr ProcessWIAPPLOS;
        strJsonWIAPPLOS = '[';
        Reset isEofWIAPPLOS;
        Reset int2;
        Dou isEofWIAPPLOS;
           Exec SQL fetch next from cursorWIAPPLOS into :dsWIAPPLOS;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWIAPPLOS = *on;
              Iter;
           Other;
              int2 += 1;
           Endsl;
         //***  Begin Chg  ***  08/05/21  *****************************
         //strJsonWIAPPLOS += '{' +
         // BuildJsonNode('year':%editc(dsWIAPPLOS.APyear:'X')) + ',' +
         // BuildJsonNode('annualPremium':%editc(dsWIAPPLOS.APannPrem:'J'))+','+
         // BuildJsonNode('#OfLosses':%editc(dsWIAPPLOS.AP#losses:'X')) + ',' +
         // BuildJsonNode('amountPaid':%editc(dsWIAPPLOS.APpaid:'J')) + ',' +
         // BuildJsonNode('reserve':%editc(dsWIAPPLOS.APreserve:'J')) + '},';
           strJsonWIAPPLOS += '{';
           strJsonWIAPPLOS +=
            BuildJsonNode('year':%editc(dsWIAPPLOS.APyear:'X')) + ',';
           Select;
           When dsWIAPPLOS.APannPrem = 0 and dsWIAPPLOS.APannPremZ <> 'Y';
              strJsonWIAPPLOS += BuildJsonNode('annualPremium':*blanks);
           Other;
              strJsonWIAPPLOS +=
               BuildJsonNode('annualPremium':%editc(dsWIAPPLOS.APannPrem:'J'));
           Endsl;
           strJsonWIAPPLOS += ',';
           Select;
           When dsWIAPPLOS.AP#Losses = 0 and dsWIAPPLOS.AP#LossesZ <> 'Y';
              strJsonWIAPPLOS += BuildJsonNode('#OfLosses':*blanks);
           Other;
              strJsonWIAPPLOS +=
               BuildJsonNode('#OfLosses':%editc(dsWIAPPLOS.AP#Losses:'X'));
           Endsl;
           strJsonWIAPPLOS += ',';
           Select;
           When dsWIAPPLOS.APpaid = 0 and dsWIAPPLOS.APpaidZ <> 'Y';
              strJsonWIAPPLOS += BuildJsonNode('amountPaid':*blanks);
           Other;
              strJsonWIAPPLOS +=
               BuildJsonNode('amountPaid':%editc(dsWIAPPLOS.APpaid:'J'));
           Endsl;
           strJsonWIAPPLOS += ',';
           Select;
           When dsWIAPPLOS.APreserve = 0 and dsWIAPPLOS.APreserveZ <> 'Y';
              strJsonWIAPPLOS += BuildJsonNode('reserve':*blanks);
           Other;
              strJsonWIAPPLOS +=
               BuildJsonNode('reserve':%editc(dsWIAPPLOS.APreserve:'J'));
           Endsl;
           strJsonWIAPPLOS += '},';
         //***  End   Chg  ***  08/05/21  *****************************
        Enddo;
        If int2 > 0;
           strJsonWIAPPLOS = %subst(strJsonWIAPPLOS:1:%len(strJsonWIAPPLOS)-1);
        Endif;
        strJsonWIAPPLOS += ']';
       Endsr;

       //=================================================================
       // Process SQL to access WIEMP
       //=================================================================
       Begsr ProcessWIEMP;
        Reset isEofWIEMP;
        json += ', "policies":[ ';
        Dou isEofWIEMP;
           Exec SQL fetch next from cursorWIEMP into :dsWIEMP;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofWIEMP = *on;
              Iter;
           Endsl;
           // Load Policy Master
           json += BuildPolicyMasterInfo(dsWIEMP.app#:dsWIEMP.current:
            dsWIEMP.co#:dsWIEMP.fnd:dsWIEMP.emp#:dsInParms.division:
            dsInParms.year:dsWIEMP.fYr);
           json += ',';
        Enddo;
        // Remove comma
        json = %subst(json:1:%len(json)-1);
        json += ']';
        Exec SQL close cursorWIEMP;
       Endsr;
      /End-free
     pGetApplication...
     p                 e

       //===========================================================
       // Get division detail
       //===========================================================
     pGetDivisionDetail...
     p                 b

     dintTest          s             25  0 inz

     ddsIn             ds                  qualified
     d co#                            3
     d group                          3
     d emp#                           9
     d div                            5
      /Free

       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build output json
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        json += '"divisionDetail": ';
        json += BuildPolicyMasterInfo(*zeros:*blank:%int(dsIn.co#)
         :%int(dsIn.group):%int(dsIn.emp#):dsIn.div:'***');
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetDivisionDetail}'));
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'employer';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              dsIn.div = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '},';

        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr Validate;
        If dsIn.co# = *blanks;
           dsIn.co# = %editc(intCo#:'X');
        Endif;

        Monitor;
           intTest = %int(dsIn.co#);
           intTest = %int(dsIn.group);
           intTest = %int(dsIn.emp#);
           intTest = %int(dsIn.div);
        On-error;
           AddErrorMessageJson('Invalid request parms.':ioRtnCode);
        Endmon;

        Monitor;
           If dsIn.group <> *zero and dsIn.group <> *blank;
              Select;
              When isEnvQuote and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Quote');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              When isEnvLive and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Policy');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              Endsl;
           Endif;
        On-error;
        Endmon;
       Endsr;
      /End-Free
     pGetDivisionDetail...
     p                 e

      //*** Begin Add *** 11/15/21 *****************************************
       //===========================================================
       // Gets Exclude From Comparative Rating Flag (Called From SaveAppData)
       //===========================================================
     p GetExcludeCompRatingFlag...
     p                 b
     d GetExcludeCompRatingFlag...
     d                 pi              n
     d  inCo#                         3  0
     d  inFnd                         3  0

     d chrExcludeCompRating...
     d                 s              1    Inz(' ')

      /Free
       Exec sql
          Select FIexcCmpRt Into :chrExcludeCompRating
          From WMFIP
          Where FIco# = :inCo# And
                FIfnd = :inFnd;

       //*** Beg add ***** 09/09/25 *************************************
       Return *On;
       //*** End add ***** 09/09/25 *************************************
       If chrExcludeCompRating = 'Y';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     p GetExcludeCompRatingFlag...
     p                 e
      //*** End   Add *** 11/15/21 *****************************************

       //===========================================================
       // Gets factors for writing state premium record
       //  for PrintACORD
       //===========================================================
     p GetFactor       b
     d GetFactor       pi            12s 9
     d  inType                        2a   Const
     d  inCo#                         3  0
     d  inFnd                         3s 0
     d  inState                       2a
     d  inCode                        3a
     d  inEfDt                        7s 0

     d outFactor       s             12s 9
      /Free
       Select;
       When inType = 'IL';
          Exec SQL
           Select LMpct
           Into :outFactor
           From WTLMP
           Where LMco# = :inCo#
            And LMfnd = :inFnd
            And LMst = :inState
            And LMkey = :inCode
            And LMefdt <= :inEfDt
            And LMstat = 'A'
           Order By LMefdt desc
           Limit 1;
       When inType = 'DD' and inCode <> *Blanks;
          Exec SQL
           Select DDpct
           Into :outFactor
           From WTDDP
           Where DDco# = :inCo#
            And DDfnd = :inFnd
            And DDst = :inState
            And DDcode = :inCode
            And DDefdt <= :inEfDt
            And DDstat = 'A'
           Order By DDefdt Desc
           Limit 1;
       When inType = 'DD' and inCode = *Blanks;

       EndSl;
       Return outFactor;
      /End-Free
     p GetFactor       e

       //===========================================================
       // Gets fund year (Called from SaveAppData)
       //===========================================================
     p GetFyr          b
     d GetFyr          pi             3  0
     d  prCo#                         3  0
     d  prFnd                         3  0
     d  prEfDt                        7  0
     d  prFyr          s              3  0
      /Free
       Exec sql
        Select F1fyr Into :prFyr
          From WDF1P
         Where F1co#  = :prCo#
          And F1fnd   = :prFnd
          And F1stpo <= :prEfDt
          And F1enpo >  :prEfDt;
       Return prFyr;
      /End-Free
     p GetFyr          e

      //------------------------------------------------------------------------*
      // GetGroupDartCode - returns misc Darts Code
      //------------------------------------------------------------------------*
     pGetGroupDartCode...
     p                 b
     dGetGroupDartCode...
     d                 pi             5a
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inType                        25    const varying options(*nopass)

     dstrType          s             25    varying inz('MISC')

      /Free
       If %parms >= 3 and inType <> *blanks;
          strType = inType;
       Endif;
       strType = Upper(strType);
       Clear DXmisc;
       Clear EXacord;
       Clear EXdocUplod;
       Chain (inCo#:inFnd) pdx00;
       Chain (inCo#:inFnd) pex00;
       Select;
       When strType = 'ACORD';
          Return EXacord;
       When strType = 'DOC UPLOAD';
          Return EXdocUplod;
       When strType = 'MISC';
          Return DXmisc;
       Endsl;
      /End-Free

     pGetGroupDartCode...
     p                 e

       //===========================================================
       // Get the default group for initial entry
       //===========================================================
     pGetGroupDefault  b
     dGetGroupDefault  pi             3  0
     d inCo#                          3  0 const
     d inFein                         9  0 const
     d inMod                          3  0 const
     d inGroup                        3  0 const

     d outGroup        s              3  0 inz
      /Free
       // Use the group passed in
       outGroup = inGroup;
       If outGroup = *zero;
          // Check Agent Assignment for default group
          Exec sql
           select AAfnd into :outGroup
            from WMAAP
            where AAco# = :inCo#
             and AAfein = :inFein
             and AAmod = :inMod
             and AAportDft = 'Y';
       Endif;
       If outGroup = *zero;
          // Check Group Master for default group
          Exec sql
           select FNfnd into :outGroup
            from WMFNP
            where FNco# = :inCo#
             and FNportDft = 'Y';
       Endif;
       If outGroup = *zero;
          // Use the first group in Agent Assignment
          Exec sql
           select min(AAfnd) into :outGroup
            from WMAAP
            where AAco# = :inCo#
             and AAfein = :inFein
             and AAmod = :inMod;
       Endif;
       Return outGroup;
      /End-Free
     pGetGroupDefault  e

       //===========================================================
       // GetMinMaxAmts
       //===========================================================
     p GetMinMaxAmts   b
     d GetMinMaxAmts   pi            11  2
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inState                       2
     d  inKey                        10    Varying const
     d  inEntryType                  25    Varying const
     d  inEntry                      25    Varying const
     d  inWvType                     25    Varying const
     d  inEfDt                        7  0
     d  outMaxAmt                    11  2
     d  outPrmLvl                     1
     d  outMinAmt      s             11  2
      /Free
       Exec Sql
        Select DTmin, DTmax, DTprmLvl
        Into :outMinAmt, :outMaxAmt, :outPrmLvl
        From WIRATDDT
        Where DTco# = :inCo# And DTfnd = :inFnd
         And DTst = :inState And DTkey = :inKey
         And DTetyp = :inEntryType And Digits(DTentry) = Digits(:inEntry)
         And DTwvType = :inWvType And DTefdt <= :inEfDt
        Order By DTefdt Desc
        Limit 1;
       Return outMinAmt;
      /End-Free
     p GetMinMaxAmts   e
       //===========================================================
       // Gets next app# for initial save (Called from SaveAppData)
       //===========================================================
     p GetNextApp#     b
     d GetNextApp#     pi             9  0
     d prNextApp#      s              9  0 inz(1)
      /Free
       Exec Sql
        Select Max(APapp#) + 1 Into :prNextApp# from WIAPP;
        If prNextApp# < 1;
           prNextApp# = 1;
        Endif;
        Return prNextApp#;
      /End-Free
     p GetNextApp#     e
       //===========================================================
       // Gets next emp# (Called from SaveAppData)
       //===========================================================
     p GetNextQte#     b
     d GetNextQte#     pi             9  0
     d  inCo#                         3  0
     d  inFnd                         3  0
     d prNextEmp#      s              9  0
      /Free
       Exec Sql
        Select FNnaqn Into :prNextEmp# from WMFNP
         Where FNco# = :inCo# And FNfnd = :inFnd;
       Exec Sql
        Update WMFNP Set FNnaqn = :prNextEmp#+1
         Where FNco# = :inCo# And FNfnd = :inFnd;
       Return prNextEmp#;
      /End-Free
     p GetNextQte#     e
      //============================================================
      // Returns policy expiration date for SaveApp based on
      //  group commercial flag (common renewal)
      //============================================================
     p GetPolExpDate   b
     d GetPolExpDate   pi             7  0
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inEffDate                     7  0
     d comRnwFlag      s              1a
     d fndYrExpDate    s              7  0
     d
      /Free
       Exec Sql
        Select FNcgfl
        Into :comRnwFlag
        From WMFNP
        Where FNco# = :inCo# And FNfnd = :inFnd;
       If comRnwFlag <> 'Y';
          Exec Sql
           Select F1enpo
           Into :fndYrExpDate
           From WDF1P
           Where F1co# = :inCo# And F1fnd = :inFnd
            And F1stpo <= :inEffDate And F1enpo > :inEffDate;
          Return fndYrExpDate;
       Else;
          //*** Begin Add *** 03/01/24 *************************************
          If %Subst(%EditC(inEffDate:'X'):4:4) = '0229';
             Return inEffDate + 9999;
          Else;
          //*** End   Add *** 03/01/24 *************************************
             Return inEffDate + 10000;
          //*** Begin Add *** 03/01/24 *************************************
          EndIf;
          //*** End   Add *** 03/01/24 *************************************
       Endif;
      /End-Free
     p GetPolExpDate   e

       //===========================================================
       // Get policy detail
       //===========================================================
     pGetPolicyDetail...
     p                 b

     disEofPolDtl      s              1n   inz
     d##amount         s             11
     d##amount2        s             11

     ddsPolDtl         ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     d stRp                           1
     d faf                            1
     d stam                          11  2
     d anPr                          11  2
     d aaPr                          11  2

     ddsIn             ds                  qualified
     d co#                            3
     d group                          3
     d year                           3
     d emp#                           9
     d div                            5
      /Free

       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build output json
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        Exsr Calculate;
        isShowEmptyRate = *off;
        json += BuildJsonNode('grossWritten'
         :%editc(dsInfoSum.premium.written:'3')) + ',';
        json += BuildJsonNode('earnedPremium'
         :%editc(dsInfoSum.premium.earned:'3')) + ',';
        json += BuildJsonNode('incurredLosses'
         :%editc(dsInfoSum.loss.incurred:'3')) + ',';
        json += BuildJsonNode('lossRatio'
         :%editc(dsInfoSum.loss.ratio:'3')) + ',';
        // We will use BuildPolicyMasterInfo
        json +='"detail": ';
        json += BuildPolicyMasterInfo(0:'*':%int(dsIn.co#):%int(dsIn.group)
         :%int(dsin.emp#):dsIn.div:dsIn.year:0) + ',';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '';
       Endsr;

       //=================================================================
       // Build sql
       //=================================================================
       Begsr BuildSql;
        strSqlCursor = 'with ' + SqlWithPolicyInfo() + ' ' +
         'select ELco#, ELfnd, ELemp#, ELdiv, ELfyr, EMstRp, ' +
         ' ELfaf, ELstam, ELanPr, ELaaPr' +
         ' from policy_info ' +
         '  where ELco# = ' + dsIn.co# + ' and ELfnd = ' + dsIn.group +
         '   and ELfyr = ' + dsIn.year + ' and ELemp# = ' + dsIn.emp#;
        If dsIn.div <> *blanks;
           strSqlCursor += ' and ELdiv = ' + dsIn.div;
        Endif;
        strSqlCursor += ' order by ELco#, ELfnd, ELemp#, Eldiv, ELfyr';
        Exec sql close cursorPolDtl;
        Exec sql prepare sqlPolDtl from :strSqlCursor;
        Exec sql declare cursorPolDtl cursor for sqlPolDtl;
        Exec sql open cursorPolDtl;
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetPolicyDetail}'));
       Endsr;

       //=================================================================
       // Calculate premium/losses
       //=================================================================
       Begsr Calculate;
        CalculatePremiumLosses();
        Leavesr;
        Dou isEofPolDtl;
           Reset dsInfo;
           Exec sql fetch next from cursorPolDtl into :dsPolDtl;
           Select;
           When not IsValidFetch(sqlstt);
              isEofPolDtl = *on;
              Iter;
           Endsl;
           // Get the written premium
           Select;
           When dsPolDtl.stam = 0 and dsPolDtl.faf = 'A';
              dsInfo.premium.written = dsPolDtl.aaPr;
              dsInfoSum.premium.written += dsPolDtl.aaPr;
           When dsPolDtl.stam = 0;
              dsInfo.premium.written = dsPolDtl.anPr;
              dsInfoSum.premium.written += dsPolDtl.anPr;
           Other;
              dsInfo.premium.written = dsPolDtl.stam;
              dsInfoSum.premium.written += dsPolDtl.stam;
           Endsl;
           // Get the earned premium
           ##amount = ' ';
           ##amount2 = ' ';
           SrPrt1(%editc(dsPolDtl.co#:'X'):%editc(dsPolDtl.fnd:'X')
            :%editc(dsPolDtl.fyr:'X'):%editc(dsPolDtl.emp#:'X')
            :%editc(dsPolDtl.div:'X'):%editc(DateTo8(SystemDate):'X')
            :##amount:##amount2:'1');
           Monitor;
              dsInfoSum.premium.earned += %Dec(##amount:11:2)/100;
              dsInfo.premium.earned = %dec(##amount:11:2)/100;
           On-Error;
              dsInfoSum.premium.earned += 0;
           Endmon;
           // Get the total incurred
           dsInfo.loss.incurred = GetIncLoss(dsPolDtl.co#:dsPolDtl.fnd
            :dsPolDtl.emp#:dsPolDtl.div:dsPolDtl.fyr);
           dsInfoSum.loss.incurred += dsInfo.loss.incurred;
           // Calculate loss ratio
           If dsInfo.premium.earned <> 0;
             Monitor;
                dsInfo.loss.ratio =
                 %dech(dsInfo.loss.incurred / dsInfo.premium.earned:5:4) * 100;
             On-Error;
                dsInfo.loss.ratio = 9999999.99;
             EndMon;
           Else;
             dsInfo.loss.ratio = 0;
           EndIf;
           dsInfoSingle(dsPolDtl.div+1) = dsInfo;
        Enddo;

        // Calculate loss ratio
        If dsInfoSum.premium.earned <> 0;
          Monitor;
             dsInfoSum.loss.ratio =
              %dech(dsInfoSum.loss.incurred/dsInfoSum.premium.earned:5:4)*100;
          On-Error;
             dsInfoSum.loss.ratio = 9999999.99;
          EndMon;
        Else;
          dsInfoSum.loss.ratio = 0;
        EndIf;
        isLossCalculated = *on;
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'year';
              dsIn.year = YAJL_get_string(val);
           When key = 'employer';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              dsIn.div = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '},';

        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr Validate;
        If dsIn.co# = *blanks;
           dsIn.co# = %editc(intCo#:'X');
        Endif;

        Monitor;
           If dsIn.group <> *zero and dsIn.group <> *blank;
              Select;
              When isEnvQuote and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Quote');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              When isEnvLive and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Policy');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              Endsl;
           Endif;
        On-error;
        Endmon;
       Endsr;
      /End-Free
     pGetPolicyDetail...
     p                 e

      //============================================================
      // Returns number of waiver detail records if policy flagged
      //  for specific type waivers
      //============================================================
     p GetSpWaivCnt    b
     d GetSpWaivCnt    pi             5
     d  waiverType                    1a   const
     d  inCo#                         3a   const
     d  inFnd                         3a   const
     d  inEmp#                        9a   const
     d  inDiv                         5a   const
     d  inFyr                         3a   const
     d  outCount       s              5  0
      /Free
       If waiverType <> 'S';
          Return *zero;
       Endif;
       Exec Sql
        Select Count(*)
        Into :outCount
        From WMWVP
        Where WVco# = :inCo# And WVfnd = :inFnd
         And WVemp# = :inEmp# And WVdiv = :inDiv
         And WVfyr = :inFyr;
        Return %Editc(outCount:'X');
      /End-Free
     p GetSpWaivCnt    e
       //=================================================================
       // Finds waiver for group/state/effDate and returns info for it
       //=================================================================
     p GetWaiverInfo   b
     d GetWaiverInfo   pi
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inState                        2    const
     d inDate                         7  0 const
     d outWaiverForm                 15
     d outFormDesc                  100
     d outIncExcFlag                  1
     d outManAutoFlag                 1
     d outPrintFlag                   1
      /Free
       Exec Sql
        Select ENform, ENdsc2, ENst, ENmnau, ENprnt
        Into :outWaiverForm, :outFormDesc,
         :outIncExcFlag, :outManAutoFlag, :outPrintFlag
        From WTENP
        Where ENco# = :inCo#
         And ENfnd = :inFnd
         And ENst = :inState
         And ENtype = 'Waiver'
         And ENstat = 'A'
         And ENeddt <= :inDate
         Order by ENeddt desc
         Limit 1;
        If IsValidFetch(sqlStt);
           Return;
        Endif;
       Exec Sql
        Select ENform, ENdsc2, ENst, ENmnau
        Into :outWaiverForm, :outFormDesc,
         :outIncExcFlag, :outManAutoFlag, :outPrintFlag
        From WTENP
        Where ENco# = :inCo#
         And ENfnd = :inFnd
         And ENst = ' '
         And ENtype = 'Waiver'
         And ENstat = 'A'
         And ENeddt <= :inDate
         Order by ENeddt desc
         Limit 1;
        If IsValidFetch(sqlStt);
           Return;
        Endif;
      /End-Free
     p GetWaiverInfo   e
       //=================================================================
       // Use SQL to get WDRRP record
       //=================================================================
     pGetWDRRP         b
     dGetWDRRP         pi
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inState                        2    const
     d inDate                         7  0 const
      /Free
       Reset dsWDRRP;
       Exec SQL
        select * into :dsWDRRP
         from WDRRP
         where RRco# = :inCo# and RRfnd = :inFnd and RRst = :inState
          and RRefDt <= :inDate
         order by RRco#, RRfnd, RRst, RRefDt desc
         limit 1;
       // Load defaults if record not found
       If not IsValidFetch(sqlstt);
          dsWDRRP.RRexSt = 'Y';
       Endif;
       If dsWDRRP.RRdsHd = *blanks;
          dsWDRRP.RRdsHd = 'Stock Discount';
       Endif;
       If dsWDRRP.RRsfHd = *blanks;
          dsWDRRP.RRsfHd = 'Safety Fees';
       Endif;
       If dsWDRRP.RR1AHd = *blanks;
          dsWDRRP.RR1AHd = 'Subject Prem CR 1A';
       Endif;
       If dsWDRRP.RR1BHd = *blanks;
          dsWDRRP.RR1BHd = 'Subject Prem CR 1B';
       Endif;
       If dsWDRRP.RRcCHd = *blanks;
          dsWDRRP.RRcCHd = 'Contractor Credit';
       Endif;
       If dsWDRRP.RRvLCH = *blanks;
          dsWDRRP.RRvLCH = 'Voluntary Labor Chrg';
       Endif;
       If dsWDRRP.RRdv1H = *blanks;
          dsWDRRP.RRdv1H = 'Deviation #1';
       Endif;
       If dsWDRRP.RRdv2H = *blanks;
          dsWDRRP.RRdv2H = 'Deviation #2';
       Endif;
      /End-free
     pGetWDRRP         e

      *=========================================================
      * Request: List Applications
      * Description: Returns an array of applications
      * inParm: agencyName
      * inParm: agencyFein
      * inParm: empDbaName
      * inParm: group
      * inParm: app#
      * inParm: empFein
      * inParm: quoteStatus (description)
      * inParm: appFrDate   (mmddyyyy)
      * inParm: appToDate   (mmddyyyy)
      * inParm: uwFirstName
      * inParm: uwLastName
      * inParm: addLine1
      * inParm: addLine2
      * inParm: addCity
      * inParm: addState
      * inParm: addZip    (00000-0000)
      * inParm: offPhone# (000-000-0000)
      * inParm: faxPhone# (000-000-0000)
      * outJSON: rtnCode  ""/"Error"
      * outJSON: requestParms
      * outJSON: errorMsg (list of strings)
      * outJSON: results: [{
      *          app#
      *          empDbaName
      *          empFein
      *          empAgency
      *          effDate     (mmddyyyy)
      *          createDate  (mmddyyyy)
      *          underwriter (first and last name)
      *          quoteStatus (description)
      *          security    (view/edit)
      *          accessAllowed  (view/edit) }]
      *=========================================================
     p ListApps        b

     d aa_rrn          s              9  0
     d ag_rrn          s              9  0
     d ah_rrn          s              9  0
     d qteFlg          s              1a   inz('Q')
     d comma2          s              1a
     d decYear         s              3  0
     d e1              s             10i 0 inz
     d idx             s             10i 0
     d idx1            s             10i 0
     d idx2            s             10i 0
     d intAgtFein      s              9  0
     d intAgtMod       s              3  0
     d integer         s             15  0
     d node1           s                   Like(Yajl_Val)
     d node2           s                   Like(Yajl_Val)
     d secFlag         s              1a
     d strAccess       s             16a
     d strAgtName      s            100a   varying
     d strApp#         s             15a
     d strAppFrDate    s              7a
     d strAppToDate    s              7a
     d strCrtDate      s              7a
     d strEffDate      s              7a
     d strEmpAgtName   s             40a
     d strEmpName      s            100a
     d strEmpFein      s              9a
     d strGroup        s              3a
     d string1         s            100a
     d string2         s            100a
     d strQteLib       s             10a
     D***  Begin Chg  ***  12/09/21 ****************
     d*strQuoteStatus  s             30a
     d strQuoteStatus  s             30a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d strUsaCrtDate   s              8a
     d strUsaEffDate   s              8a
     d strUWName       s             50a
     d strSecurity     s             16a
     d usaDate         s               d

     d dsIn            ds                  Qualified Inz
     d  agencyName                   40a
     d  agencyFein                    9a
     d  employer                           likeds(dsEmployerIn)
     d  program                       3a
     d  app#                          9a
     d  empFein                       9a
     d  quoteStatus                  30a
     d  appFrDate                     8a
     d  appToDate                     8a
     d  uwFirstName                  50a
     d  uwLastName                   50a
     d  addLine1                     40a
     d  addLine2                     40a
     d  addCity                      25a
     d  addState                      2a
     d  addZip                       10a
     d  offPhone#                    12a
     d  faxPhone#                    12a
     d  entryPoints                  10a   dim(10) varying

     d dsSMCIP       e ds                  extname(SMCIP)
     d dsWDELP       e ds                  ExtName(WDELP)
     d dsWMEMP       e ds                  ExtName(WMEMP)
     d dsWMFIP       e ds                  extname(WMFIP)

     d empDS           ds                  Inz Qualified
     d  empCo#                             like(EMco#)
     d  empFnd                             like(EMfnd)
     d  empEmp#                            like(EMemp#)
     d  empDiv                             like(EMdiv)
     d  empFyr                             like(ELfyr)
     d  empName                            like(EMname)
     d  empName2                           like(EMnam2)
     d  empFein                            like(EMfein)
     d  empEffDate                         like(EMefd1)
     d  empCrtDate                         Like(EMidt)
     d  empUWName                    50
     d  initUser                           Like(EMiUsr)
     d  agtName                            like(AGname)
     d
     dempDSnull        s              5i 0 dim(13)

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr EditParms;
       Endif;
       Select;
       When ioRtnCode <> 'Error';
          Exsr BuildSql;
       Endsl;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;
       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildReturnJSON;
        If ioRtnCode <> 'Error';
           decCount += 1;
           If comma = ',';
              json = %Trim(json) + comma;
           Else;
              json = %Trim(json) + ', "applicationList": [';
              comma = ',';
           Endif;
           json = %Trim(json) +
            '{' +
            %Trim(BuildJsonNode('app#':strApp#)) + ',' +
            BuildJsonNode('company':%editc(empDS.empCo#:'X')) + ',' +
            BuildJsonNode('group':%editc(empDS.empFnd:'X')) + ',' +
            BuildJsonNode('employer':%editc(empDS.empEmp#:'X')) + ',' +
            BuildJsonNode('division':%editc(empDS.empDiv:'X')) + ',' +
            BuildJsonNode('year':%editc(decYear:'X')) + ',' +
            %Trim(BuildJsonNode('empDbaName':strEmpName)) + ',' +
            %Trim(BuildJsonNode('empFein':strEmpFein)) + ',' +
            %Trim(BuildJsonNode('empAgency':empDs.agtName)) + ',' +
            %Trim(BuildJsonNode('effDate':strUsaEffDate:'date')) + ',' +
            %Trim(BuildJsonNode('createDate':strUsaCrtDate:'date')) + ',' +
            %Trim(BuildJsonNode('uwName':empDS.empUWname)) + ',' +
            %Trim(BuildJsonNode('quoteStatus':strQuoteStatus)) + ',' +
            %Trim(BuildJsonNode('security':strSecurity)) + ',' +
            BuildJsonNode('accessAllowed':strAccess) +
            '}';
            If %len(json) >= maxLen;
               maxCount = decCount;
            Endif;
        Else;
           For idx = 1 to %Elem(errMsgArray);
              If errMsgArray(idx) = *Blanks;
                 Leave;
              EndIf;
              If comma2 <> *Blank;
                 array = %Trim(array) + comma2;
              Else;
                 comma2 = ',';
              Endif;
              array = %Trim(array) + errMsgArray(idx);
           EndFor;
           idx = %CheckR(' ':json);
           Select;
           When idx = 0;
           Other;
              If %Subst(json:idx:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ]';
        EndIf;
       Endsr;
       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildSQL;
        Open wieml1;
        Open WMAGP;
        strQteLib = %Trim(RtvQuoteLib('*QTE'));
        // This has been rebuilt in the next line
        strSqlCursor = 'Select EMco#, Emfnd, Ememp#, EMdiv, ELfyr, +
         EMname, EMnam2, EMfein, EMefd1, EMidt, EMuwtr, EMiUsr From ' +
         %Trim(strQteLib) + '/WMEMP Join ' +
         %Trim(strQteLib) + '/WDELP' +
         ' On EMco# = ELco# And EMfnd = ELfnd And' +
         ' EMemp# = ELemp# And EMdiv = ELdiv' +
         ' Where EMco# = ' + strCo# +
         ' And EMstrp = ''Y''';

       //***  Begin Chg  ***  06/24/22  ************************
       //strSqlCursor = 'with ' + SqlWithPolicyInfo() + ', ' +
       //  SqlWithGroupPortalInfo() + ' ' +
       //  'select EMco#, Emfnd, Ememp#, EMdiv, ELfyr, ' +
       //  ' EMname, EMnam2, EMfein, EMefd1, EMidt, UPusNm, EMiUsr, ' +
       //  ' AGname ' +
       //  ' from policy_info ' +
       //  ' left join portalDate d on EMCO#=d.co# and EMFND=d.fnd ' +
       //  ' left join WMUPP ' +
       //  '  on UPco# = EMco# and UPstaf = EMuwtr and UPstaf <> '' ''' +
       //  '  where EMco# = ' + strCo# +
       //  '   and EMstrp = ''Y''';
        strSqlCursor = 'with ' + SqlWithPolicyInfo() + ', ' +
         SqlWithGroupPortalInfo() + ' ' +
         'select EMco#, Emfnd, Ememp#, EMdiv, ELfyr, ' +
         ' EMname, EMnam2, EMfein, EMefd1, EMidt, UPusNm, EMiUsr, ' +
         ' AGname ' +
         ' from policy_info';
        strSqlCursor +=
         ' left join portalDate d on EMCO#=d.co# and EMFND=d.fnd';
        strSqlCursor +=
         ' left join WMUPP ' +
         '  on UPco# = EMco# and UPstaf = EMuwtr and UPstaf <> '' ''';
        strSqlCursor +=
         ' left join WMNBP ' +
         '  on NBco#=EMco# and NBfnd=EMfnd and NBemp#=EMemp#';
        strSqlCursor +=
         ' where EMco# = ' + strCo# +
         '  and EMstrp = ''Y''';
       //***  End   Chg  ***  06/24/22  ************************

        strSqlCursor += ' and ' +
         'ELiDt >= (case ' +
         '           when d.compareDt is null then -1 ' +
         '           else d.compareDt end) ';

        If dsIn.employer.dba <> *Blanks;
           strEmpName = ChkApost(dsIn.employer.dba);
           strSqlCursor = strSqlCursor + ' And' +
           ' (Upper(EMname) Like Upper(''%'+%Trim(strEmpName)+'%'') Or' +
           ' Upper(EMnam2) Like Upper(''%'+%Trim(strEmpName)+'%'') Or' +
           ' Upper(EMfdNm) Like Upper(''%'+%Trim(strEmpName)+'%'') Or' +
           ' Upper(EMfdN2) Like Upper(''%'+%Trim(strEmpName)+'%''))';
        EndIf;

        If dsIn.program <> *Blanks;
           strSqlCursor = strSqlCursor + ' And EMfnd = ' + dsIn.program;
        Endif;

        If dsIn.employer.fein <> *Blanks;
           strSqlCursor = strSqlCursor + ' And EMfein = ' + dsIn.employer.fein;
        Endif;

        If dsIn.employer.number <> *Blanks;
           strSqlCursor += ' And EMemp# = ' + dsIn.employer.number;
           If dsIn.employer.div <> *Blanks;
              strSqlCursor += ' And EMdiv = ' + dsIn.employer.div;
           Endif;
        Endif;

        If dsIn.appFrDate <> *Blanks;
           strAppFrDate = %Char(%Date(dsIn.appFrDate:*Usa0):*Cymd0);
         //***  Begin Chg  ***  06/24/22  ************************
         //strSqlCursor = strSqlCursor + ' And EMefd1 >= ' +
         // strAppFrDate;
           strSqlCursor = strSqlCursor + ' and NBrcDt >= ' +
            strAppFrDate;
         //***  End   Chg  ***  06/24/22  ************************
        Endif;

        If dsIn.appToDate <> *Blanks;
           strAppToDate = %Char(%Date(dsIn.appToDate:*Usa0):*Cymd0);
         //***  Begin Chg  ***  06/24/22  ************************
         //strSqlCursor = strSqlCursor + ' And EMefd1 < ' +
         // strAppToDate;
           strSqlCursor = strSqlCursor + ' and NBrcDt < ' +
            strAppToDate;
         //***  End   Chg  ***  06/24/22  ************************
        Endif;

        If dsIn.addLine1 <> *Blanks;
           strSqlCursor = strSqlCursor + ' And' +
           ' (Upper(EMmad1) Like Upper(''%'+%Trim(dsIn.addLine1)+'%'') Or' +
           ' Upper(EMpad1) Like Upper(''%'+%Trim(dsIn.addLine1)+'%''))';
        Endif;

        If dsIn.addLine2 <> *Blanks;
           strSqlCursor = strSqlCursor + ' And' +
           ' (Upper(EMmad2) Like Upper(''%'+%Trim(dsIn.addLine2)+'%'') Or' +
           ' Upper(EMpad2) Like Upper(''%'+%Trim(dsIn.addLine2)+'%''))';
        Endif;

        If dsIn.addCity <> *Blanks;
           strSqlCursor = strSqlCursor + ' And' +
           ' (Upper(EMmcty) Like Upper(''%'+%Trim(dsIn.addCity)+'%'') Or' +
           ' Upper(EMpcty) Like Upper(''%'+%Trim(dsIn.addCity)+'%''))';
        Endif;

        If dsIn.addState <> *Blanks;
           strSqlCursor = strSqlCursor + ' And' +
           ' (Upper(EMmst) Like Upper(''%'+%Trim(dsIn.addState)+'%'') Or' +
           ' Upper(EMpst) Like Upper(''%'+%Trim(dsIn.addState)+'%''))';
        Endif;

        If dsIn.addZip <> *Blanks;
           strSqlCursor = strSqlCursor +
           ' And '+%Subst(dsIn.addZip:1:5)+' = ' +
           %Subst(%Editc(EMmzip:'X'):1:5) + ' Or '+
           %Subst(dsIn.addZip:6:4) + ' = ' +
           %Subst(%Editc(EMmzip:'X'):6:4);
        Endif;

        If dsIn.offPhone# <> *Blanks;
           strSqlCursor = strSqlCursor +
           ' And '+%Subst(dsIn.offPhone#:1:3)+' = ' +
           %Editc(EMarea:'X') + ' And ' +
           %Subst(dsIn.offPhone#:5:3)+%Subst(dsIn.offPhone#:9:4)+' = '+
           %Editc(EMphn#:'X');
        Endif;

        If dsIn.faxPhone# <> *Blanks;
           strSqlCursor = strSqlCursor +
           ' And '+%Subst(dsIn.faxPhone#:1:3)+' = ' +
           %Editc(EMfaxa:'X') + ' And ' +
           %Subst(dsIn.faxPhone#:5:3)+%Subst(dsIn.faxPhone#:9:4)+' = '+
           %Editc(EMfax#:'X');
        Endif;

        If dsIn.agencyName <> *blanks;
           strAgtName = %trim(ChkApost(%trim(dsIn.AgencyName)));
           strSqlCursor += ' And' +
            ' Upper(AGname) like Upper(''%'+strAgtName+'%'')';
        Endif;

        If dsIn.agencyFein <> *Blanks;
           strSqlCursor += ' And' +
            ' AGfein = ' + dsIn.agencyFein;
        Endif;

        Select;
        When dsIn.uwFirstName <> *blanks and dsIn.uwLastName <> *blanks;
           strSqlCursor += ' And' +
            '  (Upper(UPsnNm) like Upper(''%'+dsIn.uwFirstName+'%'') ' +
            'or Upper(UPsnNm) like Upper(''%'+dsIn.uwLastName+'%''))';
        When dsIn.uwFirstName <> *blanks;
           strSqlCursor += ' And' +
            ' Upper(UPsnNm) like Upper(''%'+dsIn.uwFirstName+'%'')';
        When dsIn.uwLastName <> *Blanks;
           strSqlCursor += ' And' +
            ' Upper(UPsnNm) like Upper(''%'+dsIn.uwLastName+'%'')';
        Endsl;

        If isShowSql;
           json += ',' + BuildJsonNode('sql':strSqlCursor);
        Endif;

        strSqlCursor += ' Order by EMco#, Emfnd, Ememp#, EMdiv, ELfyr Desc';
        Exec SQL Prepare appString From :strSqlCursor;
        Exec Sql Declare a1 Cursor For appString;
        Exec Sql Close a1;  // Should this be here
        Exec Sql Open a1;

        If SqlStt <> '00000';
           AddErrorMessageJson('Open A1 SQL statement failed. ' +
            'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X')
            :ioRtnCode);
           LeaveSr;
        Endif;

        Reset decCount;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql Fetch a1 into :empDS :empDSnull;

           Select;
           When IsValidFetch(sqlStt);
             //  If decCount = maxCount;
             //     json += '], ' + BuildJsonNode('message':
             //      'Maximum number of records reached (' + %char(maxCount) +
             //      ').');
             //     Leave;
             //  Endif;
           When SqlStt = '02000' and comma = ',';
              json = %Trim(json) + ' ]';
              Leave;
           When SqlStt = '02000' and comma = *Blank;
              json = json + ', "applicationList": [ ]';
              Leave;
           Other;
              AddErrorMessageJson('Fetch A1 SQL statement failed. ' +
               'SqlStt: ' + SqlStt + ' SqlCode: ' + %EditC(SqlCode:'X')
               :ioRtnCode);
              Leave;
           Endsl;

           Select;
           When isEnvQuote and
            not UseGroupInPortal(empDs.empCo#:empDs.empFnd:'Quote');
              Iter;
           When isEnvLive and
            not UseGroupInPortal(empDs.empCo#:empDs.empFnd:'Policy');
              Iter;
           Endsl;

           secFlag = InqUpdAccessEmp(empDs.empCo#:empDs.empFnd:
            empDs.empEmp#:empDs.empDiv:inUser);
           Select;
           When secFlag = 'D';
              strSecurity = 'view';
           When strAuthority = '*View';
              strSecurity = 'view';
           When secFlag = 'C';
              strSecurity = 'edit';
           Other;
              Iter;
           Endsl;

           strAccess = strSecurity;
           Select;
           When strSecurity = 'view';
           When dsIn.entryPoints(1) = *blanks;
           When %lookup('*ALL':dsIn.entryPoints) <> *zero;
           When %lookup('WCMS':dsIn.entryPoints) <> *zero
            and empDS.initUser <> 'WCEXPRESS' and empDS.initUser <> 'CSPORTAL';
              strAccess = 'view';
           When %lookup(Upper(empDS.initUser):dsIn.entryPoints) = *zero;
              strAccess = 'view';
           When %lookup('*NONE':dsIn.entryPoints) <> *zero;
              strAccess = 'view';
           Endsl;

           strEmpName = empDs.empName + empDS.empName2;
           strEmpFein = %EditC(empDs.empFein:'X');
           strUsaEffDate = %Char(%Date(empDS.empEffDate:*Cymd):*Usa0);
           Monitor;
              strUsaCrtDate = %Char(%Date(empDS.empCrtDate:*Cymd):*Usa0);
           On-error;
              strUsaCrtDate = *zeros;
           Endmon;

           Reset decYear;
           Clear strApp#;
           Chain(n) (empDs.empCo#:empDs.empFnd:empDs.empEmp#) pl101;
           If dsIn.app# <> *blanks;
              Select;
              When not %found;
                 Iter;
              When %Int(dsIn.app#) <> L1App#;
                 Iter;
              When L1Current <> 'Y';
                 Iter;
              Other;
                 strApp# = %Editc(L1App#:'X');
                 decYear = L1fyr;
              Endsl;
           Else;
              Select;
              When %found and L1Current <> 'Y';
                 Iter;
              Other;
                 If %found;
                    strApp# = %Editc(L1App#:'X');
                    decYear = L1fyr;
                 Else;
                    strSqlCursor = 'select max(ELfyr) ' +
                     'from ' + %trim(strQteLib) + '/WDELP ' +
                     'where ELco# = ' + %char(empDs.empCo#) +
                     ' and ELfnd = ' + %char(empDs.empFnd) +
                     ' and ELemp# = ' + %char(empDs.empEmp#) +
                     ' and ELdiv = ' + %char(empDs.empDiv);
                    strSqlCursor = 'select max(ELfyr) ' +
                     'from WDELP ' +
                     'where ELco# = ' + %char(empDs.empCo#) +
                     ' and ELfnd = ' + %char(empDs.empFnd) +
                     ' and ELemp# = ' + %char(empDs.empEmp#) +
                     ' and ELdiv = ' + %char(empDs.empDiv);
                    If dsIn.employer.year <> ' ';
                       strSqlCursor += ' and ELfyr = ' + dsIn.employer.year;
                    Endif;
                    Exec sql close curso2rWDELP;
                    Exec sql prepare stmt2WDELP from :strSqlCursor;
                    Exec sql declare cursor2WDELP cursor for stmt2WDELP;
                    Exec sql open cursor2WDELP;
                    Exec sql fetch cursor2WDELP into :decYear;
                    If not IsValidFetch(sqlstt);
                       Iter;
                    Endif;
                    Exec sql close cursor2WDELP;
                 Endif;
              Endsl;
           Endif;

         //***  Begin Chg  ***  12/09/21 ****************
         //strQuoteStatus = GetQuoteStsDesc(empDs.empCo#:empDs.empFnd:
         // empDs.empEmp#:empDs.empDiv:empDs.empFyr:qteFlg);
           strQuoteStatus = %trim(GetQuoteStsDesc(empDs.empCo#:empDs.empFnd:
            empDs.empEmp#:empDs.empDiv:empDs.empFyr:qteFlg));
         //***  End   Chg  ***  12/09/21 ****************
           If dsIn.quoteStatus <> *Blanks and
            dsIn.QuoteStatus <> strQuoteStatus;
              Iter;
           Endif;

           Exsr BuildReturnJson;
        Enddo;
        Exec Sql Close a1;
        Close wieml1;
        Close WMAGP;
       Endsr;
       //-------------------------------------------------------
       // Break parms out of JSON
       //-------------------------------------------------------
       Begsr GetParms;
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';

           node = YAJL_object_find(docNode:'agencyName');
           dsIn.agencyName = YAJL_get_string(node);
           BuildGiantJSON(json :'agencyName' :dsIn.agencyName);

           node = YAJL_object_find(docNode:'agencyFein');
           dsIn.agencyFein = YAJL_get_string(node);
           BuildGiantJSON(json :'agencyFein' :dsIn.agencyFein);

           json += ',"employer":{';
           node1 = YAJL_object_find(docNode:'employer');
           Reset i1;
           Dow YAJL_object_loop(node1:i1:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'name';
                 json = %scanrpl('""':'{':json:%len(json)-1);
                 node2 = YAJL_object_find(node1:key);
                 Reset i2;
                 Dow YAJL_object_loop(node2:i2:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'dba';
                       dsIn.employer.dba = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += '}';
              When key = 'fein';
                 dsIn.employer.fein = YAJL_get_string(val);
              When key = 'number';
                 dsIn.employer.number = YAJL_get_string(val);
              When key = 'division';
                 dsIn.employer.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.employer.year = YAJL_get_string(val);
              Endsl;
           Enddo;
           json += '}';

           node = YAJL_object_find(docNode:'group');
           dsIn.program = YAJL_get_string(node);
           BuildGiantJSON(json :'group' :dsIn.program);

           node = YAJL_object_find(docNode:'app#');
           dsIn.app# = YAJL_get_string(node);
           BuildGiantJSON(json :'app#' :dsIn.app#);

           node = YAJL_object_find(docNode:'quoteStatus');
           dsIn.quoteStatus = YAJL_get_string(node);
           BuildGiantJSON(json :'quoteStatus' :dsIn.quoteStatus);

           node = YAJL_object_find(docNode:'appFrDate');
           dsIn.appFrDate = YAJL_get_string(node);
           BuildGiantJSON(json :'appFrDate' :dsIn.appFrDate);

           node = YAJL_object_find(docNode:'appToDate');
           dsIn.appToDate = YAJL_get_string(node);
           BuildGiantJSON(json :'appToDate' :dsIn.appToDate);

           node = YAJL_object_find(docNode:'uwFirstName');
           dsIn.uwFirstName = YAJL_get_string(node);
           BuildGiantJSON(json :'uwFirstName':dsIn.uwFirstName);

           node = YAJL_object_find(docNode:'uwLastName');
           dsIn.uwLastName = YAJL_get_string(node);
           BuildGiantJSON(json :'uwLastName':dsIn.uwLastName);

           node = YAJL_object_find(docNode:'addLine1');
           dsIn.addLine1 = YAJL_get_string(node);
           BuildGiantJSON(json :'addLine1':dsIn.addLine1);

           node = YAJL_object_find(docNode:'addLine2');
           dsIn.addLine2 = YAJL_get_string(node);
           BuildGiantJSON(json :'addLine2':dsIn.addLine2);

           node = YAJL_object_find(docNode:'addCity');
           dsIn.addCity = YAJL_get_string(node);
           BuildGiantJSON(json :'addCity':dsIn.addCity);

           node = YAJL_object_find(docNode:'addState');
           dsIn.addState = YAJL_get_string(node);
           BuildGiantJSON(json :'addState' :dsIn.addState);

           node = YAJL_object_find(docNode:'addZip');
           dsIn.addZip = YAJL_get_string(node);
           BuildGiantJSON(json :'addZip':dsIn.addZip);

           node = YAJL_object_find(docNode:'offPhone#');
           dsIn.offPhone# = YAJL_get_string(node);
           BuildGiantJSON(json :'offPhone#' :dsIn.offPhone#);

           node = YAJL_object_find(docNode:'faxPhone#');
           dsIn.faxPhone# = YAJL_get_string(node);
           BuildGiantJSON(json :'faxPhone#' :dsIn.faxPhone#);

           key = 'editEntryPoints';
           node1 = YAJL_object_find(docNode:key);
           json += ',"' + %trim(key) + '":[ ';
           Reset e1;
           Dow YAJL_array_loop(node1:e1:val);
               dsIn.entryPoints(e1) = YAJL_get_string(val);
               json += '"' + dsIn.entryPoints(e1) + '",';
               dsIn.entryPoints(e1) = Upper(dsIn.entryPoints(e1));
           Enddo;
           json = %subst(json:1:%len(json)-1) + ']';

           val = YAJL_object_find(docNode:'showSql');
           isShowSql = YAJL_get_string(val);

           json = json + '}';
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           AddErrorMessageJson('JSON filter can''t be read.'
            :ioRtnCode);
        Endif;
       Endsr;
       //-------------------------------------------------------
       // Edit values in incoming parms
       //-------------------------------------------------------
       Begsr EditParms;

        Select;
        When dsIn.agencyFein <> *Blanks;
           Monitor;
              integer = %Int(dsIn.agencyFein);
           On-Error;
              AddErrorMessageJson('Agent FEIN has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.program <> *Blanks;
           Monitor;
              integer  = %Int(dsIn.program);
              If integer <> *zero;
                 Select;
                 When isEnvQuote and
                  not UseGroupInPortal(intCo#:integer:'Quote');
                    AddErrorMessageJson('Program ' + %char(integer) +
                     ' is not included for service requested ' +
                     '(' + %trim(inRequest) + ').'
                     :ioRtnCode);
                 When isEnvLive and
                  not UseGroupInPortal(intCo#:integer:'Policy');
                    AddErrorMessageJson('Program ' + %char(integer) +
                     ' is not included for service requested ' +
                     '(' + %trim(inRequest) + ').'
                     :ioRtnCode);
                 Endsl;
              Endif;
           On-Error;
              AddErrorMessageJson('Program has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.app# <> *Blanks;
           Monitor;
              integer  = %Int(dsIn.app#);
           On-Error;
              AddErrorMessageJson('Application number has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.employer.fein <> *Blanks;
           Monitor;
              integer  = %Int(dsIn.employer.fein);
           On-Error;
              AddErrorMessageJson('Employer FEIN has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.appFrDate <> *Blanks;
           Monitor;
              usaDate  = %Date(dsIn.appFrDate:*Usa0);
           On-Error;
              AddErrorMessageJson('Application from date is invalid.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.appToDate <> *Blanks;
           Monitor;
              usaDate  = %Date(dsIn.appToDate:*Usa0);
           On-Error;
              AddErrorMessageJson('Application to date is invalid.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.addZip <> *Blanks;
           Monitor;
              integer  = %Int(%Subst(dsIn.addZip:1:5));
              integer  = %Int(%Subst(dsIn.addZip:7:4));
           On-Error;
              AddErrorMessageJson('Zip code has character data.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.offPhone# <> *Blanks;
           Monitor;
              integer  = %Int(%Subst(dsIn.offPhone#:1:3));
              integer  = %Int(%Subst(dsIn.offPhone#:5:3));
              integer  = %Int(%Subst(dsIn.offPhone#:9:4));
           On-Error;
              AddErrorMessageJson('Office phone number is invalid.'
               :ioRtnCode);
           EndMon;
        Endsl;

        Select;
        When dsIn.faxPhone# <> *Blanks;
           Monitor;
              integer  = %Int(%Subst(dsIn.faxPhone#:1:3));
              integer  = %Int(%Subst(dsIn.faxPhone#:5:3));
              integer  = %Int(%Subst(dsIn.faxPhone#:7:4));
           On-Error;
              AddErrorMessageJson('Faxe phone number is invalid.'
               :ioRtnCode);
           EndMon;
        Endsl;

       Endsr;
      /End-Free

     p ListApps        e

      *=========================================================
      * Request: ListEmployers
      * Description: Returns an array of employers
      *=========================================================
     p ListEmployers   b

     d aa_rrn          s              9  0
     d ag_rrn          s              9  0
     d ah_rrn          s              9  0
     d i1              s             10i 0
     d intFnd          s              3  0
     d isEofEmpList    s              1n
     d strAgtName      s             40a
     d strEmpName      s            100a
     d strEmpFein      s              9a
     d strGroup        s              3a
     d strQuoteStatus  s             30a
     d strRatingSt     s              2a
     d strSQL          s           5000a   varying inz
     d strSQLFields    s           5000a   varying inz
     d strSQLWhere     s           5000a   varying inz
     d strSQLWith      s           5000a   varying inz
     d strUsaCrtDate   s              8a
     d strUsaEffDate   s              8a
     d strUWName       s             50a
     d usaDate         s               d

     d dsIn            ds                  Qualified Inz
     d  quoteStatus                  10a
     d   quoteFlag                    1a   overlay(quoteStatus:1)
     d  employer                           likeds(dsEmployerIn)
     d  state                         2a
     d  group                         3a
     d  agencyName                   40a
     d  agencyFein                    9a
     d  address                     100a
     d  phone#                       12a

     d dsWDELP       e ds                  ExtName(WDELP)
     d dsWMAGP         ds                  likerec(pag0p:*input) inz
     d dsWMEMP       e ds                  ExtName(WMEMP)

     d dsEmp           ds                  Inz Qualified
     d  env                           1a
     d  co#                                like(EMco#)
     d  fnd                                like(EMfnd)
     d  emp#                               like(EMemp#)
     d  div                                like(EMdiv)
     d  fedName                            like(EMname)
     d  dbaName                            like(EMnam2)
     d  fein                               like(EMfein)
     d  effDt                              like(EMefd1)
     d  uw                                 Like(EMuwtr)
     d  mailAdr1                           like(EMmAd1)
     d  mailAdr2                           like(EMmAd2)
     d  mailCity                           like(EMmCty)
     d  mailSt                             like(EMmSt)
     d  mailZip                            like(EMmZip)
     d  physAdr1                           like(EMpAd1)
     d  physAdr2                           like(EMpAd2)
     d  physCity                           like(EMpCty)
     d  physSt                             like(EMpSt)
     d  physZip                            like(EMpZip)
     d  areaCode                           like(EMarea)
     d  phone#                             like(EMphn#)
     d  faxAreaCode                        like(EMfaxA)
     d  fax#                               like(EMfax#)

      /Free
       Exsr GetParms;
       //Exsr BuildStub;
       Exsr BuildSql;
       Exsr ProcessSql;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       If %open(WMAGP);
          Close WMAGP;
       Endif;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListEmployers}'));
       Endsr;

       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildReturnJSON;
        json += '{' +
         BuildJsonNode('company':%editc(dsEmp.co#:'X'))  + ', ' +
         BuildJsonNode('group':%editc(dsEmp.fnd:'X'))  + ', ' +
         BuildJsonNode('employer':%editc(dsEmp.emp#:'X')) + ', ' +
         BuildJsonNode('division':%editc(dsEmp.div:'X')) + ', ' +
         BuildJsonNode('policyNumber':'') + ',' +
         BuildJsonNode('fein':%editc(dsEmp.fein:'X')) + ',' +
         BuildJsonNode('state':strRatingSt) + ',' +
         '"dates":{' +
          BuildJsonNode('effective':%char(dsEmp.effDt):'date') + '}, '+
         '"name":{' +
          %trim(BuildJsonNode('dba':dsEmp.dbaName)) + ', ' +
          %trim(BuildJsonNode('federal':dsEmp.fedName)) +
         '},' +
         '"address":{' +
          BuildJsonAddress(dsEmp.mailAdr1:dsEmp.mailAdr2:
           dsEmp.mailCity:dsEmp.mailSt:%char(dsEmp.mailZip):'mailing') +
          ', ' +
          BuildJsonAddress(dsEmp.physAdr1:dsEmp.physAdr2:
           dsEmp.physCity:dsEmp.physSt:%char(dsEmp.physZip):'physical') +
          '}, ' +
         '"phone":[' +
          '{'+BuildJsonPhone(%char(dsEmp.areaCode):%char(dsEmp.phone#):
           '':'phone1') + '}, ' +
          '{'+BuildJsonPhone(%char(dsEmp.faxAreaCode):%char(dsEmp.fax#):
           'fax':'fax') + '} ' +
          '], ' +
         BuildJsonNode('insuredCode':'') + ',' +
         BuildJsonNode('agencyCode':'') + ',' +
         BuildJsonNode('agentCode':'') + ',' +
         BuildJsonNode('agencyName':strAgtName) + ',' +
         BuildJsonNode('agencyFein':%editc(dsWMAGP.AGfein:'X')) + ',' +
         BuildJsonNode('agencyMod':%editc(dsWMAGP.AGmod:'X')) + '},';
       Endsr;

       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        strSQLWith += 'with ' +
         'allEmp as ' +
         '(select ' +
         ' l.EMCO# livCO#, l.EMFND livFND, l.EMEMP# livEMP#, l.EMDIV livDIV, ' +
         ' q.EMCO# qteCO#, q.EMFND qteFND, q.EMEMP# qteEMP#, q.EMDIV qteDIV ' +
         '  from ' + %trim(RtvQuoteLib('*LIV')) + '/WMEMP l ' +
         '  full join ' +%trim(RtvQuoteLib('*QTE')) + '/WMEMP q on ' +
         '   (l.EMCO#=q.EMCO# and l.EMFND=q.EMFND and ' +
         '    l.EMEMP#=q.EMEMP# and l.EMDIV=q.EMDIV) ' +
         ' ) ';
        strSQLFields += 'EMCO#,EMFND,EMEMP#,EMDIV,EMFDNM,EMNAME,EMFEIN,' +
         'EMEFD1,EMUWTR,' +
         'EMMAD1,EMMAD2,EMMCTY,EMMST,EMMZIP,' +
         'EMPAD1,EMPAD2,EMPCTY,EMPST,EMPZIP,' +
         'EMAREA,EMPHN#,EMFAXA,EMFAX#';
        strSQLWhere += 'where EMco# = ' + %char(UserCo#(inUser));
        If dsIn.quoteFlag <> *blank;
           strSqlWhere += ' and ENV = ''' + dsIn.quoteFlag + '''';
        Endif;
        If dsIn.employer.name <> *blanks;
           dsIn.employer.name = Upper(dsIn.employer.name);
           strSQLWhere +=
            ' and (upper(EMname) like (''%'+%trim(dsIn.employer.name)+'%'') ' +
            'or upper(EMfdNm) like (''%'+%trim(dsIn.employer.name)+'%''))';
        Endif;
        If dsIn.employer.number <> *blanks;
           strSQLWhere += ' and EMemp# = ' + dsIn.employer.number;
        Endif;
        If dsIn.employer.div <> *blanks;
           strSQLWhere += ' and EMdiv = ' + dsIn.employer.div;
        Endif;
        If dsIn.employer.fein <> *blanks;
           strSQLWhere += ' and digits(EMfnd)||digits(EMemp#) in ' +
            '(select digits(EMfnd)||digits(EMemp#) ' +
            '  from WMEMP where EMfein = ' + dsIn.employer.fein + ')';
           //strSQLWhere += ' and EMfein = ' + dsIn.employer.fein;
        Endif;
        If dsIn.group <> *blanks;
           strSQLWhere += ' and EMfnd = ' + dsIn.group;
        Endif;
        If dsIn.address <> *blanks;
           dsIn.address = Upper(dsIn.address);
           strSQLWhere +=
            ' and (upper(EMmAd1) like (''%'+%trim(dsIn.address)+'%'') ' +
            'or upper(EMmAd2) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMmCty) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMmSt) like (''%'+%trim(dsIn.address)+'%'')' +
            'or cast(EMmZip as char(9)) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMpAd1) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMpAd2) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMpCty) like (''%'+%trim(dsIn.address)+'%'')' +
            'or upper(EMpSt) like (''%'+%trim(dsIn.address)+'%'')' +
            'or cast(EMpZip as char(9)) like (''%'+%trim(dsIn.address)+'%''))';
         Endif;
         If dsIn.phone# <> *blanks;
           strSQLWhere += ' and (cast(concat(EMAREA, EMPHN#) as char(10)) ' +
            'like (''%'+%trim(dsIn.phone#)+'%'') ' +
            'or cast(concat(EMFAXA, EMFAX#) as char(10)) ' +
            'like (''%'+%trim(dsIn.phone#)+'%''))';
         Endif;
        strSQL += strSQLWith +
         'select ENV, ' + strSQLFields +
         ' from (' +
         '  select ''Q'' as ENV,' + strSQLFields +
         '   from ' + %trim(RtvQuoteLib('*QTE')) + '/WMEMP' +
         '   where (EMCO#, EMFND, EMEMP#, EMDIV) in' +
         '    (select qteCO#, qteFND, qteEMP#, qteDIV' +
         '      from allEmp' +
         '      where livCO# is null and livFND is null and' +
         '            livEMP# is null and livDIV is null)' +
         '  union' +
         '  select ''L'' as ENV,' + strSQLFields +
         '   from ' + %trim(RtvQuoteLib('*LIV')) + '/WMEMP' +
         '   where (EMCO#, EMFND, EMEMP#, EMDIV) in' +
         '    (select livCO#, livFND, livEMP#, livDIV' +
         '      from allEmp' +
         '      where livCO# is not null and livFND is not null and' +
         '            livEMP# is not null and livDIV is not null)' +
         ' ) as empList ' + strSQLWhere + ' ' +
         'order by EMCO#, EMFND, EMEMP#, EMDIV';
        Exec SQL close cursorEmpList;
        Exec SQL prepare sqlEmpList from :strSql;
        Exec SQL declare cursorEmpList cursor for sqlEmpList;
        Exec SQL open cursorEmpList;
        //json += ',' + BuildJsonNode('sqlEmpList':strSql);
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';
           Dow YAJL_object_loop(docNode:i1:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'quoteStatus';
                 dsIn.quoteStatus = YAJL_get_string(val);
              When key = 'empName';
                 dsIn.employer.name = YAJL_get_string(val);
              When key = 'emp#';
                 dsIn.employer.number = YAJL_get_string(val);
              When key = 'division';
                 dsIn.employer.div = YAJL_get_string(val);
              When key = 'empFein';
                 dsIn.employer.fein = YAJL_get_string(val);
              When key = 'state';
                 dsIn.state = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
              When key = 'agencyName';
                 dsIn.agencyName = YAJL_get_string(val);
              When key = 'agencyFein';
                 dsIn.agencyFein = YAJL_get_string(val);
              When key = 'address';
                 dsIn.address = YAJL_get_string(val);
              When key = 'phone#';
                 dsIn.phone# = YAJL_get_string(val);
              Endsl;
           Enddo;

           json = json + '}';
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;

        Monitor;
           intFnd  = %Int(dsIn.group);
           Select;
           When  dsIn.quoteFlag = 'Q' and
            not UseGroupInPortal(intCo#:intFnd:'Quote');
              AddErrorMessageJson('Group ' + %char(intFnd) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           When dsIn.quoteFlag = 'L' and
            not UseGroupInPortal(intCo#:intFnd:'Policy');
              AddErrorMessageJson('Group ' + %char(intFnd) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           Endsl;
        On-Error;
        EndMon;

       Endsr;

       //========================================================
       // Read through SQL recrods
       //========================================================
       Begsr ProcessSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        json += ',"employerList":[ ';
        Reset isEofEmpList;
        Dou isEofEmpList;
           Exec SQL fetch next from cursorEmpList into :dsEmp;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofEmpList = *on;
              Iter;
           When  dsEmp.env = 'Q' and
            not UseGroupInPortal(dsEmp.co#:dsEmp.fnd:'Quote');
              Iter;
           When dsEmp.env = 'L' and
            not UseGroupInPortal(dsEmp.co#:dsEmp.fnd:'Policy');
              Iter;
           Endsl;
           strRatingSt =
            GetRatingState(dsEmp.co#:dsEmp.fnd:dsEmp.emp#:dsEmp.div);
           If dsIn.state <> *blanks;
              If dsIn.state <> strRatingSt;
                 Iter;
              Endif;
           Endif;
           GetEmpAgt(dsEmp.co#:dsEmp.fnd:dsEmp.emp#:dsEmp.div:0:0
            :strAgtName:ah_rrn:aa_rrn:ag_rrn:dsEmp.env);
           If not %open(WMAGP);
              Open WMAGP;
           Endif;
           Reset dsWMAGP;
           Chain ag_rrn WMAGP dsWMAGP;
           If dsIn.agencyName <> *blanks or dsIn.agencyFein <> *blanks;
              Select;
              When dsIn.agencyName <> *blanks and
               not StringFound(%trim(strAgtName):%trim(dsIn.agencyName):'1');
                 Iter;
              When dsIn.agencyFein <> *blanks and
               dsIn.agencyFein <> %editc(dsWMAGP.AGfein:'X');
                 Iter;
              Endsl;
           Endif;
           Exsr BuildReturnJson;
        Enddo;
        json = %subst(json:1:%len(json)-1);
        json += ']';
       Endsr;
      /End-Free

     p ListEmployers   e

      *=========================================================
      * Request: ListLocations
      * Description: Returns an array of employer locations
      *=========================================================
     p ListLocations   b

     ddecAsOfDt        s              7  0 inz
     disLocEof         s              1n   inz
     dintFnd           s              3  0
     dstrInc           s              5
     dstrSql           s           1500a   varying inz
     dstrSqlAL         s            500a   varying inz
     dstrSqlEM         s            500a   varying inz
     dstrSqlWhere      s            500a   varying inz

     ddsLoc            ds                  qualified inz
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d elid                          10
     d effDt                          7  0
     d expDt                          7  0
     d expDtCmp                       7  0
     d name_dba                     100
     d personal_dba                   1
     d name_fed                     100
     d personal_fed                   1
     d line1_mail                    40
     d line2_mail                    40
     d city_mail                     25
     d st_mail                        2
     d zip_mail                       9  0
     d line1_phy                     40
     d line2_phy                     40
     d city_phy                      25
     d st_phy                         2
     d zip_phy                        9  0

     ddsIn             ds                  qualified
     d co#                            3
     d fnd                            3
     d emp#                           9
     d div                            5
     d incDiv                         1n   inz('1')
     d loc                           10
     d incLoc                         1n   inz('1')
     d asOfDt                         8
      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        json += ',"locationList":[';
        Dou isLocEof;
           Exec sql fetch next from cursorLocations into :dsLoc;
           Select;
           When not IsValidFetch(sqlStt);
              isLocEof = *on;
              Iter;
           Endsl;
           json += '{' +
            BuildJsonNode('division':%char(dsLoc.div)) + ',' +
            BuildJsonNode('location':dsLoc.elid) + ',' +
            '"dates":{' +
             BuildJsonNode('effective':%char(dsLoc.effDt):'date') + ',' +
             BuildJsonNode('expiration':%char(dsLoc.expDt):'date') + '},' +
            '"name":{' +
           //***  Begin Chg  ***  12/09/21  ************** +
           //BuildJsonNode('dbaPersonalName':dsLoc.personal_dba) + ',' +
           //BuildJsonNode('dba':dsLoc.name_dba) + ',' +
           //BuildJsonNode('federalPersonalName':dsLoc.personal_fed) + ',' +
           //BuildJsonNode('federal':dsLoc.name_fed) + '},' +
             BuildJsonNode('dbaPersonalName':%trim(dsLoc.personal_dba))+','+
             BuildJsonNode('dba':%trim(dsLoc.name_dba)) + ',' +
             BuildJsonNode('federalPersonalName':%trim(dsLoc.personal_fed))+','+
             BuildJsonNode('federal':%trim(dsLoc.name_fed)) + '},' +
           //***  End   Chg  ***  12/09/21  ************** +
            '"address":{' +
             BuildJsonAddress(dsLoc.line1_mail:dsLoc.line2_mail
              :dsLoc.city_mail:dsLoc.st_mail:%char(dsLoc.zip_mail):'mailing') +
              ',' +
             BuildJsonAddress(dsLoc.line1_phy:dsLoc.line2_phy
              :dsLoc.city_phy:dsLoc.st_phy:%char(dsLoc.zip_phy):'physical') +
              '}'+
            '},';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Build sql based on incoming parameters
       //=================================================================
       Begsr BuildSql;
        // Division information
        strSqlEM = 'select ' +
         '  EMco# co#, EMfnd fnd, EMemp# emp#, EMdiv div, '' '' as elid, ' +
         '  EMefD1 as effDt, EMcaD1 as expDt, ' +
         '  case when EMcaD1 = 0 then 9999999 else EMcaD1 end as expDtCmp, ' +
         '  EMname || EMnam2, EMnamP, EMfdNm || EMfdN2, EMfdNP, ' +
         '  EMmAd1, EMmAd2, EMmCty, EMmSt, EMmZip, ' +
         '  EMpAd1, EMpAd2, EMpCty, EMpSt, EMpZip ' +
         ' from WMEMP ' +
         '  where EMco# = ' + dsIn.co# + ' and EMfnd = ' + dsIn.fnd +
         '   and EMemp# = ' + dsIn.emp#;
        If dsIn.div <> *blanks;
           strSqlEM += ' and EMdiv = ' + dsIn.div;
        Endif;
        // Location information
        strSqlAL = 'select ' +
         '  ALco# co#, ALfnd fnd, ALemp# emp#, ALdiv div, ALelId elid, ' +
         '  ALefDt as effDt, ALdlDt as expDt, ' +
         '  case when ALdlDt = 0 then 9999999 else ALdlDt end as expDtCmp, ' +
         '  ALname || ALnam2, ALnamP, ALfdNm || ALfdN2, ALfdNP, ' +
         '  ALmAd1, ALmAd2, ALmCty, ALmSt, ALmZip, ' +
         '  ALpAd1, ALpAd2, ALpCty, ALpSt, ALpZip ' +
         ' from WMALP ' +
         '  where ALco# = ' + dsIn.co# + ' and ALfnd = ' + dsIn.fnd +
         '   and ALemp# = ' + dsIn.emp#;
        If dsIn.div <> *blanks;
           strSqlAL += ' and ALdiv = ' + dsIn.div;
        Endif;
        If dsIn.loc <> *blanks;
           strSqlAL += ' and ALelId = ' + dsIn.loc;
        Endif;
        // Full statement
        strSql = 'with ' +
         'DIVs as (' + strSqlEM + ') ' +
         ', ' +
         'LOCs as (' + strSqlAL + ') ';
        If decAsOfDt <> *zeros;
           strSqlWhere += %char(decAsOfDt) + ' between effDt and expDtCmp and ';
        Endif;
        strSqlWhere = %scanrpl(' and ':'':strSqlWhere:%len(strSqlWhere)-4);
        If strSqlWhere <> *blanks;
           strSqlWhere = ' where ' + strSqlWhere;
        Endif;
        strSqlEM = 'select * from DIVs ' + strSqlWhere;
        strSqlAL = 'select * from LOCs ' + strSqlWhere;
        Select;
        When dsIn.incDiv and dsIn.incLoc;
           strSql += strSqlEM + ' union ' + strSqlAL;
        When dsIn.incDiv;
           strSql += strSqlEM;
        When dsIn.incLoc;
           strSql += strSqlAL;
        Endsl;
        strSql += ' order by co#, fnd, emp#, div, elid';
        // Create cursor
        Exec sql close cursorLocations;
        Exec sql prepare sqlLocations From :strSql;
        Exec sql declare cursorLocations Cursor For sqlLocations;
        Exec sql open cursorLocations;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListLocations}');
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           AddErrorMessageJson(strErrorMsg:ioRtnCode);
        Endif;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.fnd = YAJL_get_string(val);
           When key = 'employer';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Reset i1;
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'number';
                    dsIn.div = YAJL_get_string(val);
                 When key = 'includeInList';
                    strInc = YAJL_get_string(val);
                    dsIn.incDiv = strInc = 'Y';
                 Endsl;
              Enddo;
              json += '}';
           When key = 'location';
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Reset i1;
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'number';
                    dsIn.loc = YAJL_get_string(val);
                 When key = 'includeInList';
                    strInc = YAJL_get_string(val);
                    dsIn.incLoc = strInc = 'Y';
                 Endsl;
              Enddo;
              json += '}';
           When key = 'asOfDate';
              dsIn.asOfDt = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
        Exsr Validate;
       Endsr;

       //=================================================================
       // Validate incoming parms
       //=================================================================
       Begsr Validate;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Monitor;
           intCo# = %int(dsIn.co#);
        On-error;
           dsIn.co# = %char(intCo#);
        Endmon;
        Monitor;
           intFnd  = %Int(dsIn.fnd);
           Select;
           When isEnvQuote and
            not UseGroupInPortal(intCo#:intFnd:'Quote');
              AddErrorMessageJson('Group ' + %char(intFnd) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           When isEnvLive and
            not UseGroupInPortal(intCo#:intFnd:'Policy');
              AddErrorMessageJson('Group ' + %char(intFnd) +
               ' is not included for service requested ' +
               '(' + %trim(inRequest) + ').':ioRtnCode);
           Endsl;
        On-Error;
        EndMon;

        Monitor;
           If dsIn.asOfDt = *blank;
              dsIn.asOfDt = %editc(DateTo8(SystemDate()):'X');
           Endif;
           decAsOfDt = DtCYMD(%dec(dsIn.asOfDt:8:0));
        On-error;
           AddErrorMessageJson('Invalid as of date.'
            :ioRtnCode);
        EndMon;
       Endsr;

      /End-free
     p ListLocations   e

      *=========================================================
      * Request: ListPolicyInfo
      * Description: Returns an array(s) of policy division/year records
      *=========================================================
     pListPolicyInfo...
     p                 b

     ddecCo#           s              3  0 inz
     ddecRRN           s             10i 0 inz
     dintDiv           s              5  0 inz
     dintEmp#          s              9  0 inz
     dintFnd           s              3  0 inz
     disEofWDELP       s              1n   inz
     disSummarizeYear  s              1n   inz
     dstrSql           s           5000    varying inz

     ddsWDELP        e ds                  extname(WDELP) inz qualified
     ddsWDELPsave    e ds                  extname(WDELP) inz qualified
     ddsWMEMP        e ds                  extname(WMEMP) inz qualified

     ddsDateRange      ds                  qualified inz
     d from                           8
     d to                             8
     d from8                          8  0 inz(00000000)
     d to8                            8  0 inz(99999999)
     d from7                          7  0 inz(0000000)
     d to7                            7  0 inz(9999999)

     ddsIn             ds                  qualified Inz
     d co#                            3a
     d group                          3a
     d emp#                           9a
     d div                           25a   varying
     d dateRange                           likeds(dsDateRange)
      /Free

       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build cursor
       //=================================================================
       Begsr BuildCursor;
        Exec SQL close cursorYearList;
        Exec SQL prepare sqlYearList from :strSql;
        Exec SQL declare cursorYearList cursor for sqlYearList;
        Exec SQL open cursorYearList;
       Endsr;

       //=================================================================
       // Build output json
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;

        json += '"results":{';
        json += BuildJsonNode('company':dsIn.co#) + ',';
        json += BuildJsonNode('group':dsIn.group) + ',';
        json += BuildJsonNode('employer':dsIn.emp#) + ',';
        json += '"divisionList":' + ElementOpen('[');
        Dou isEofWDELP;
           Exec sql fetch next from cursorInfo into :dsWDELP;
           Select;
           When not IsValidFetch(sqlStt);
              isEofWDELP = *on;
              Iter;
           Endsl;
           If dsWDELP.ELfnd <> dsWDELPsave.ELfnd
            or dsWDELP.ELemp# <> dsWDELPsave.ELemp#;
              dsWDELPsave.ELdiv = -1;
           Endif;
           If dsWDELP.ELdiv <> dsWDELPsave.ELdiv;
              Exsr BuildOutputDiv;
           Endif;
           dsWDELPsave = dsWDELP;
           Exsr BuildOutputYear;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose('*ALL') + '}';
       Endsr;

       //=================================================================
       // Build output json - Division
       //=================================================================
       Begsr BuildOutputDiv;

        intCo# = dsWDELP.ELco#;
        intFnd = dsWDELP.ELfnd;
        intEmp# = dsWDELP.ELemp#;
        intDiv = dsWDELP.ELdiv;
        Reset dsWMEMP;
        Exec sql
         select * into :dsWMEMP
          from WMEMP
          where EMco# = :intCo# and EMfnd = :intFnd
           and EMemp# = :intEmp# and EMdiv = :intDiv;

        // If values is not Y then set to N
        If dsWMEMP.EMnamP <> 'Y';
           dsWMEMP.EMnamP = 'N';
        EndIf;
        If dsWMEMP.EMfdnP <> 'Y';
           dsWMEMP.EMfdnP = 'N';
        EndIf;

        If %scan(',':json:%len(json)-1) > 0;
           json = %scanrpl(',':ElementClose+ElementClose+',':json:%len(json)-1);
        Endif;
        json += ElementOpen('{');
        json += BuildJsonNode('division':%char(dsWDELP.ELdiv)) + ',';
        json +=
         '"masterInformation": {' +
          BuildJsonNode('fein':%char(dsWMEMP.EMfein)) + ', ' +
          '"name":{' +
         //***  Begin Chg  ***  12/09/21  ************** +
         //BuildJsonNode('dbaPersonalName':dsWMEMP.EMnamP) + ', ' +
         //BuildJsonNode('dba':dsWMEMP.EMname + dsWMEMP.EMnam2) + ', ' +
         //BuildJsonNode('federalPersonalName':dsWMEMP.EMfdnP) + ', '+
         //BuildJsonNode('federal':dsWMEMP.EMfdNm  + dsWMEMP.EMfdn2) +
           BuildJsonNode('dbaPersonalName':%trim(dsWMEMP.EMnamP)) + ', ' +
           BuildJsonNode('dba':%trim(dsWMEMP.EMname + dsWMEMP.EMnam2)) + ', ' +
           BuildJsonNode('federalPersonalName':%trim(dsWMEMP.EMfdnP)) + ', '+
           BuildJsonNode('federal':%trim(dsWMEMP.EMfdNm  + dsWMEMP.EMfdn2)) +
         //***  End   Chg  ***  12/09/21  ************** +
          '}},';

        json += '"yearList":' + ElementOpen('[');
       Endsr;

       //=================================================================
       // Build output json - Year
       //=================================================================
       Begsr BuildOutputYear;
        json += ElementOpen('{');
        json += BuildJsonNode('year':%char(dsWDELP.ELfyr)) + ',';
        json += '"policyDates":{' +
         BuildJsonNode('effective':%char(dsWDELP.ELpfDt):'date') + ',' +
         BuildJsonNode('expiration':%char(dsWDELP.ELptDt):'date') + '},';
        strSqlCursor = 'with ' + SqlWithPolicyInfo() + ' ' +
         'select ELco#, ELfnd, ELemp#, ELdiv, ELfyr, EMstRp, ' +
         ' ELfaf, ELstam, ELanPr, ELaaPr' +
         ' from policy_info ' +
         '  where ELco# = ' + %char(dsWDELP.ELco#) +
         '   and ELfnd = ' + %char(dsWDELP.ELfnd) +
         '   and ELfyr = ' + %char(dsWDELP.ELfyr) +
         '   and ELemp# = ' + %char(dsWDELP.ELemp#);
        Select;
        When not isSummarizeYear;
           strSqlCursor += '   and ELdiv = ' + %char(dsWDELP.ELdiv);
        Endsl;
        strSqlCursor += ' order by ELco#, ELfnd, ELemp#, ELdiv, ELfyr';
        Clear dsInfoSum;
        CalculatePremiumLosses();
        If isLossCalculated;
           Select;
           When isSummarizeYear;
              dsInfo = dsInfoSum;
           Other;
              dsInfo = dsInfoSingle(dsWDELP.ELdiv+1);
           Endsl;
           json += BuildJsonNode('grossWritten'
            :%editc(dsInfo.premium.written:'3')) + ',';
           json += BuildJsonNode('earnedPremium'
            :%editc(dsInfo.premium.earned:'3')) + ',';
           json += BuildJsonNode('incurredLosses'
            :%editc(dsInfo.loss.incurred:'3')) + ',';
           json += BuildJsonNode('lossRatio'
            :%editc(dsInfo.loss.ratio:'3')) + ',';
        Endif;
        json = %scanrpl(',':ElementClose+',':json:%len(json)-1);
       Endsr;

       //=================================================================
       // Build sql
       //=================================================================
       Begsr BuildSql;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        strSql = 'select * ' +
         'from WDELP ' +
         ' where ELco# = ' + dsIn.co# + ' and ELfnd = ' + dsIn.group +
         '  and ELemp# = ' + dsIn.emp#;
        Select;
        When isSummarizeYear;
           strSql += ' and ELdiv = (select EMdiv from WMEMP ' +
         ' where EMco# = ' + dsIn.co# + ' and EMfnd = ' + dsIn.group +
         '  and EMemp# = ' + dsIn.emp# + ' and EMstRp = ''Y'')';
        When dsIn.div <> *blanks;
           strSql += ' and ELdiv = ' + dsIn.div;
        Endsl;
        strSql += ' and ' + %char(dsIn.dateRange.from7) + ' <= ELpfDt ' +
         'and ELptDt <= ' + %char(dsIn.dateRange.to7);
        strSql += ' order by ELco#, ELfnd, ELemp#, ELdiv, ELfyr desc';
        Exec sql close cursorInfo;
        Exec sql prepare sqlInfo from :strSql;
        Exec sql declare cursorInfo cursor for sqlInfo;
        Exec sql open cursorInfo;
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListPolicyInfo}'));
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        dsIn.dateRange = dsDateRange;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'employer';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              dsIn.div = YAJL_get_string(val);
           When key = 'dateRange';
              json = %scanrpl('""':'{':json:%len(json)-2);
              node1 = YAJL_object_find(node0:key);
              Reset i1;
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'from';
                    dsIn.dateRange.from = YAJL_get_string(val);
                 When key = 'to';
                    dsIn.dateRange.to = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = json + '}';
           Endsl;
        Enddo;
        json = json + '},';

        // Free up the space by removing the tree
        yajl_tree_free(node0);

        Exsr Validate;
       Endsr;

       //========================================================
       // Validate parms
       //========================================================
       Begsr Validate;
        If dsIn.co# = *blanks;
           dsIn.co# = %editc(intCo#:'X');
        Endif;

        Monitor;
           If dsIn.dateRange.from <> *blanks;
              dsIn.dateRange.from8 = %int(dsIn.dateRange.from);
              dsIn.dateRange.from7 = DtCYMD(dsIn.dateRange.from8);
           Endif;
           If dsIn.dateRange.to <> *blanks;
              dsIn.dateRange.to8 = %int(dsIn.dateRange.to);
              dsIn.dateRange.to7 = DtCYMD(dsIn.dateRange.to8);
           Endif;
        On-error;
           AddErrorMessageJson('Invalid date range.':ioRtnCode);
        Endmon;
        If dsIn.dateRange.to7 < dsIn.dateRange.from7;
           AddErrorMessageJson('Invalid date range.':ioRtnCode);
        Endif;

        isSummarizeYear = Upper(dsIn.div) = '*SUMMARIZE YEAR';
       Endsr;
      /End-free
     pListPolicyInfo...
     p                 e

      *=========================================================
      * Request: ListQuoteStatusHistory
      * Description: Returns an array of status history records
      *=========================================================
     pListQuoteStatusHistory...
     p                 b

     d i1              s             10i 0
     d intCo#          s              3  0
     d isEofHistList   s              1n
     d strStsDesc      s             40a   inz
     d strSQL          s           5000a   varying inz
     d strFile         s             25a   varying inz

     d dsIn            ds                  Qualified Inz
     d  type                         25a   varying
     d  co#                           3a
     d  group                         3a
     d  emp#                          9a
     d  div                           5a

     d dsHist        e ds                  extname(WAQTP) qualified inz
     d dsHistSv      e ds                  extname(WAQTP) qualified inz

      /Free
       Exsr GetParms;
       //Exsr BuildStub;
       Exsr BuildSql;
       Exsr ProcessSql;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       Return;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        If json <> *blanks;
           json += ',';
        Endif;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListQuoteStatusHistory}'));
       Endsr;

       //========================================================
       // Builds Return JSON
       //========================================================
       Begsr BuildReturnJson;
        Reset strStsDesc;
        Exec sql
         select QSDESC into :strStsDesc
          from WTQSP
          where QSco# = :intCo# and QSkey = :dsHist.QTtype
           and QSqte = :dsHist.QTstat;
        json += '{' +
         '"agent":{' +
          BuildJsonNode('number':%editc(dsHist.QTagt#:'Z')) + ',' +
          BuildJsonNode('name':%trim(GetAgtName(dsHist.QTagt#))) +
          '},' +
         BuildJsonNode('carrier':dsHist.QTcarr) + ',' +
         BuildJsonNode('chapter':%editc(dsHist.QTchap:'Z')) + ',' +
         '"dates":{' +
          BuildJsonNode('effective':%char(dsHist.QTefDt):'date') + ',' +
          BuildJsonNode('weekEnding':%char(dsHist.QTwkDt):'date') + ',' +
          BuildJsonNode('newBusiness':%char(dsHist.QTnbDt):'date') + ',' +
          BuildJsonNode('origianalSubmission':%char(dsHist.QTnbDt):'date')+','+
          BuildJsonNode('revised':%char(dsHist.QTrvDt):'date') + ',' +
          BuildJsonNode('fax':%char(dsHist.QTrvDt):'date') +
          '},' +
         BuildJsonNode('premium':%editc(dsHist.QTprem:'J')) + ',' +
         BuildJsonNode('premiumBeforeRevision':%editc(dsHist.QTbfPr:'J')) +','+
         BuildJsonNode('experienceModifier':%editc(dsHist.QTmod:'J')) + ',' +
         BuildJsonNode('arap':%editc(dsHist.QTarap:'J')) + ',' +
         '"lossRatio":{' +
          BuildJsonNode('historical':%editc(dsHist.QThlr:'J')) + ',' +
          BuildJsonNode('projected':%editc(dsHist.QTplr:'J')) +
          '},' +
         BuildJsonNode('finalStatus':dsHist.QTfin) + ',' +
         BuildJsonNode('comment':%trim(dsHist.QTcomm)+ ' ' +dsHist.QTcom2)+','+
         BuildJsonNode('transferStatus':dsHist.QTxfer) + ',' +
         '"status":{' +
          BuildJsonNode('code':dsHist.QTtype) + ',' +
          BuildJsonNode('description':%trim(strStsDesc)) +
          '},' +
         BuildJsonNode('environment':dsHist.QTstat) + ',' +
         BuildJsonNode('autoLost':dsHist.QTauLs) + ',' +
         '"maintenanceInfo":{' +
          BuildJsonNode('user':dsHist.QTuser) + ',' +
          BuildJsonNode('program':dsHist.QTpgm) + ',' +
          BuildJsonNode('date':%char(dsHist.QTdt):'date') + ',' +
          BuildJsonNode('time':%char(dsHist.QTtime):'time') +
          '}' +
         '},';
       Endsr;

       //========================================================
       // Builds Return JSON {employer info}
       //========================================================
       Begsr BuildReturnJsonEmp;
        json = %subst(json:1:%len(json)-1);
        Select;
        When %subst(json:%len(json):1) = '}';
           json = json + ']},';
        Endsl;
        json += '{' +
         BuildJsonNode('company':%char(dsHist.QTco#)) + ',' +
         BuildJsonNode('group':%char(dsHist.QTfnd)) + ',' +
         BuildJsonNode('employer':%char(dsHist.QTemp#)) + ',' +
         BuildJsonNode('division':%char(dsHist.QTdiv)) + ',' +
         '"historyInformation": [';
       Endsr;

       //==================================================
       // Build SQLs
       //==================================================
       Begsr BuildSQL;
        Select;
        When dsIn.type = '*ALL';
           strFile = 'WAQTP_HST';
        When dsIn.type = '*STATUS';
           strFile = 'WAQTP_STS';
        Other;
           strFile = 'WAQTP';
        Endsl;
        strSql = 'select *' +
         ' from ' + strFile + ' ' +
         ' where QTco# = ' + dsIn.co#;
        If dsIn.group <> *blanks;
           strSql += ' and QTfnd = ' + dsIn.group;
        Endif;
        If dsIn.emp# <> *blanks;
           strSql += '  and QTemp# = ' + dsIn.emp#;
        Endif;
        If dsIn.div <> *blanks;
           strSql += ' and QTdiv = ' + dsIn.div;
        Endif;
        strSql += ' order by QTco#, QTfnd, QTemp#, QTdiv';
        Exec SQL close cursorHistList;
        Exec SQL prepare sqlHistList from :strSql;
        Exec SQL declare cursorHistList cursor for sqlHistList;
        Exec SQL open cursorHistList;
       Endsr;

       //========================================================
       // Break parms out of JSON
       //========================================================
       Begsr GetParms;
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';
           Dow YAJL_object_loop(docNode:i1:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'historyType';
                 dsIn.type = YAJL_get_string(val);
              When key = 'company';
                 dsIn.co# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              Endsl;
           Enddo;
           json = json + '}';
           Monitor;
              intCo# = %int(dsIn.co#);
           On-error;
              AddErrorMessageJson('Incoming company not valid.':ioRtnCode);
           Endmon;
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
        Endif;
        Monitor;
           If dsIn.group <> *zero and dsIn.group <> *blank;
              Select;
              When isEnvQuote and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Quote');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              When isEnvLive and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Policy');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              Endsl;
           Endif;
        On-error;
        Endmon;
       Endsr;

       //========================================================
       // Read through SQL recrods
       //========================================================
       Begsr ProcessSQL;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        json += ',"quoteStatusHistoryList":[,';
        Reset dsHist;
        Reset isEofHistList;
        Dou isEofHistList;
           dsHistSv = dsHist;
           Exec SQL fetch next from cursorHistList into :dsHist;
           Select;
           When Not IsValidFetch(sqlStt);
              isEofHistList = *on;
              Iter;
           When isEnvQuote and
            not UseGroupInPortal(dsHist.QTco#:dsHist.QTfnd:'Quote');
              Iter;
           When isEnvLive and
            not UseGroupInPortal(dsHist.QTco#:dsHist.QTfnd:'Policy');
              Iter;
           When dsHist.QTco# <> dsHistSv.QTco#
            or dsHist.QTfnd <> dsHistSv.QTfnd
            or dsHist.QTemp# <> dsHistSv.QTemp#
            or dsHist.QTdiv <> dsHistSv.QTdiv;
              Exsr BuildReturnJsonEmp;
           Endsl;
           Exsr BuildReturnJson;
        Enddo;
        json = %subst(json:1:%len(json)-1);
        json += ']}]';
       Endsr;
     pListQuoteStatusHistory...
     p                 e


       //======================================================================
       // Modify Policy Effective Dates
       //======================================================================
     pModifyPolicyEffectiveDates...
     p                 b

     ddsIn             ds                  qualified Inz
     d app#                           9a
     d co#                            3a
     d group                          3a
     d emp#                           9a
     d div                            5a
     d year                           3a
     d effDt                          8a

     ddsWDELP        e ds                  extname(WDELP) qualified inz
     ddsWIEMP        e ds                  extname(WIEMP) qualified inz

     daryFnd           s              3  0  inz dim(999)
     ddecArcA          s             11  2  inz
     ddecSFMn          s             11  2  inz
     ddecWMin          s             11  2  inz
     dintApp#          s              9  0  inz
     dintDiv           s              5  0  inz
     dintDt7Eff        s              7  0  inz
     dintDt7Exp        s              7  0  inz
     dintEmp#          s              9  0  inz
     dintFnd           s              3  0  inz(-1)
     dintGroup         s              3  0  inz
     dintYear          s              3  0  inz
     disCheckGroups    s              1n    inz('1')
     disDeleteGroups   s              1n    inz('0')
     disEofDatesEmp    s              1n    inz
     disSingleGroup    s              1n    inz('1')

     doutEntry         s             25     inz varying
     doutEntryType     s             25     inz varying
     doutKey           s             10     inz varying
     doutState         s              2     inz
     dinMaxAmt         s             11  2  inz
     dinPremLvl        s              1     inz
      /Free
       Exsr GetParms;
       Exsr ProcessDates;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        //Exsr BuildStub;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
       Endsr;

       //=================================================================
       // Build sql for datesApp
       //=================================================================
       Begsr BuildSqlDatesApp;
        Exec sql
         declare cursorDatesApp cursor for
          select *
          from WIEMP
          where EMco# = :intCo# and EMapp# = :intApp#
          order by EMco#, EMfnd;
        Exec sql open cursorDatesApp;
        Exec sql fetch next from cursorDatesApp into :dsWIEMP;
        Exec sql fetch next from cursorDatesApp into :dsWIEMP;
        isSingleGroup = not IsValidFetch(sqlstt);
        Exec sql close cursorDatesApp;
       Endsr;

       //=================================================================
       // Build sql for datesEmp
       //=================================================================
       Begsr BuildSqlDatesEmp;
        strSqlCursor = 'select * from WDELP ' +
         'where ELco# = ' + %char(intCo#) + ' and ELfnd = ' + %char(intGroup) +
         ' and ELfyr = ' + %char(intYear) + ' and ELemp# = ' + %char(intEmp#);
        If dsIn.div <> *blanks;
           strSqlCursor += ' and ELdiv = ' + %char(intDiv);
        Endif;
        Exec sql close cursorDatesEmp;
        Exec sql prepare sqlDatesEmp from :strSqlCursor;
        Exec sql declare cursorDatesEmp cursor for sqlDatesEmp;
        Exec sql open cursorDatesEmp;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. ' +
         '{ModifyPolicyEffectiveDates}');
       Endsr;

       //=================================================================
       // Edit the application
       //=================================================================
       Begsr EditApplication;
        Exec sql open cursorDatesApp;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorDatesApp into :dsWIEMP;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           intGroup = dsWIEMP.EMfnd;
           intEmp# = dsWIEMP.EMemp#;
           intYear = dsWIEMP.EMfyr;
           Exsr EditPolicy;
        Enddo;
        Exec sql close cursorDatesApp;
       Endsr;

       //=================================================================
       // Edit the policy
       //=================================================================
       Begsr EditPolicy;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exsr BuildSqlDatesEmp;
        Reset isEofDatesEmp;
        Dou isEofDatesEmp;
           Exec sql fetch next from cursorDatesEmp into :dsWDELP;
           Select;
           When not IsValidFetch(sqlstt) and isCheckGroups;
              isCheckGroups = *off;
              Exec sql close cursorDatesEmp;
              Exec sql open cursorDatesEmp;
              Iter;
           When not IsValidFetch(sqlstt);
              isEofDatesEmp = *on;
              Iter;
           Endsl;
           // Check agent assignment dates
           If intFnd <> dsWDELP.ELfnd;
              Exec sql
                select distinct(AHFND) into :intFnd
                 from WMAHP
                 left join WMAAP
                  on AAco# = AHco# and AAfnd = AHfnd and AAagt# = AHagt#
                 left join WMAGP
                  on AGco# = AAco# and AGfein = AAfein and AGmod = AAmod
                 where AHco#=:dsWDELP.ELco# and AHfnd=:dsWDELP.ELfnd and
                  AHemp#=:dsWDELP.ELemp# and AHfyr=:dsWDELP.ELfyr and
                  ((AAEFDT<>0 and AAEFDT>:intDt7Eff) or
                   (AACNDT<>0 and AACNDT<=:intDt7Eff));
              Select;
              When IsValidFetch(sqlstt) and isCheckGroups;
              When IsValidFetch(sqlstt) and isDeleteGroups;
                 DeleteQuote(dsWDELP.ELco#:dsWDELP.ELfnd:dsWDELP.ELfyr
                  :dsWDELP.ELemp#);
              When IsValidFetch(sqlstt);
                 AddErrorMessageJson('Agency not active for carrier(s) ' +
                  'as of the effective date.'
                  :ioRtnCode);
                 Leave;
              Other;
                 isDeleteGroups = *on;
              Endsl;
           Endif;
        Enddo;
        Exec sql close cursorDatesEmp;
       Endsr;

       //=================================================================
       // Get new value based on effective date
       //=================================================================
       Begsr GetNewValues;
        intYear = GetFyr(dsWDELP.ELco#:dsWDELP.ELfnd:intDt7Eff);
        intDt7Exp = GetPolExpDate(dsWDELP.ELco#:dsWDELP.ELfnd:intDt7Eff);
        Exec sql
         select RSjrSt into :outState
          from WMRSP
          where RSco# = :dsWDELP.ELco# and RSrtCd = :dsWDELP.ELrtcd;
        // REINSUR
        decArcA =
         GetMinMaxAmts(dsWDELP.ELco#:dsWDELP.ELfnd:outState:'REINSUR'
          :'Percent':%char(dsWDELP.ELarcp):*blanks:intDt7Eff
          :inMaxAmt:inPremLvl);
        // SAFETYFEE
        decSFMn =
         GetMinMaxAmts(dsWDELP.ELco#:dsWDELP.ELfnd:outState:'SAFETYFEE'
          :'Percent':%char(dsWDELP.ELsfpc):*blanks:intDt7Eff
          :inMaxAmt:inPremLvl);
        // WAIVER
        Select;
        When dsWDELP.ELwvPc <> *zero;
           outEntryType = 'Percent';
           outEntry = %char(dsWDELP.ELwvPc);
        When dsWDELP.ELfChg <> *zero;
           outEntryType = 'Amount';
           outEntry = %char(dsWDELP.ELfChg);
        EndSl;
        dsWDELP.ELwmin =
         GetMinMaxAmts(dsWDELP.ELco#:dsWDELP.ELfnd:outState:'WAIVER'
          :outEntryType:outEntry:dsWDELP.ELwvTp:intDt7Eff
          :inMaxAmt:inPremLvl);
        dsWDELP.ELwmax = inMaxAmt;
        dsWDELP.ELwvfl = inPremLvl;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        // Recieve app# and/or group/emp#/div/fyr
        //  Get app# from policy info, if not passed
        //  If no app# available only process the one policy
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        Reset i0;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'app#';
              dsIn.app# = YAJL_get_string(val);
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'employer';
              dsIn.emp# = YAJL_get_string(val);
           When key = 'division';
              dsIn.div = YAJL_get_string(val);
           When key = 'year';
              dsIn.div = YAJL_get_string(val);
           When key = 'effectiveDate';
              dsIn.effDt = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = json + '}';
        // Free up the space by removing the tree
        yajl_tree_free(node0);
        Exsr Validate;
       Endsr;

       //=================================================================
       // Process application
       //=================================================================
       Begsr ProcessApplication;
        Exec sql open cursorDatesApp;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorDatesApp into :dsWIEMP;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           intGroup = dsWIEMP.EMfnd;
           intEmp# = dsWIEMP.EMemp#;
           intYear = dsWIEMP.EMfyr;
           Exsr ProcessPolicy;
        Enddo;
        Exec sql close cursorDatesApp;
       Endsr;

       //=================================================================
       // Process date changes
       //=================================================================
       Begsr ProcessDates;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Select;
        When intApp# <> *zero;
           Exsr ProcessApplication;
        Other;
           Exsr ProcessPolicy;
        Endsl;
       Endsr;

       //=================================================================
       // Process policy
       //=================================================================
       Begsr ProcessPolicy;
        Exsr BuildSqlDatesEmp;
        Reset isEofDatesEmp;
        Dou isEofDatesEmp;
           Exec sql fetch next from cursorDatesEmp into :dsWDELP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofDatesEmp = *on;
              Iter;
           Endsl;
           Exsr GetNewValues;
           Exec sql
            update WIEMP
             set EMfyr = :intYear,
              EMpgm=:Q1pgm, EMuser=:inUser, EMtime=:sysTime6, EMdt=:sysDate7
             where EMco# = :dsWDELP.ELco# and EMfnd = :dsWDELP.ELfnd
              and EMapp# = :intApp#
              and EMemp# = :dsWDELP.ELemp# and EMfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt) and intApp# <> *zero;
              AddErrorMessageJson
               ('No application cross reference records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMEMP
             set EMefD1 = :intDt7Eff,
              EMpgm=:Q1pgm, EMuser=:inUser, EMtime=:sysTime6, EMdt=:sysDate7
             where EMco# = :dsWDELP.ELco# and EMfnd = :dsWDELP.ELfnd
              and EMemp# = :dsWDELP.ELemp# and EMdiv = :dsWDELP.ELdiv
              and EMefD1 = :dsWDELP.ELpfDt;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No employer master records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMALP
             set ALefDt = :intDt7Eff,
              ALpgm=:Q1pgm, ALuser=:inUser, ALtime=:sysTime6, ALdt=:sysDate7
             where ALco# = :dsWDELP.ELco# and ALfnd = :dsWDELP.ELfnd
              and ALemp# = :dsWDELP.ELemp# and ALdiv = :dsWDELP.ELdiv
              and ALefDt = :dsWDELP.ELpfDt;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No additional location records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMOOP
             set OOefDt = :intDt7Eff,
              OOpgm=:Q1pgm, OOuser=:inUser, OOtime=:sysTime6, OOdt=:sysDate7
             where OOco# = :dsWDELP.ELco# and OOfnd = :dsWDELP.ELfnd
              and OOemp# = :dsWDELP.ELemp# and OOdiv = :dsWDELP.ELdiv
              and OOefDt = :dsWDELP.ELpfDt;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No owner/officer records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMEPP
             set EPefDt = :intDt7Eff, EPfyr = :intYear,
              EPpgm=:Q1pgm, EPuser=:inUser, EPtime=:sysTime6, EPdt=:sysDate7
             where EPco# = :dsWDELP.ELco# and EPfnd = :dsWDELP.ELfnd
              and EPemp# = :dsWDELP.ELemp# and EPdiv = :dsWDELP.ELdiv
              and EPefDt = :dsWDELP.ELpfDt and EPfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No payroll records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv) + ' ' + %char(dsWDELP.ELfyr));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMAHP
             set AHefDt = :intDt7Eff, AHfyr = :intYear,
              AHpgm=:Q1pgm, AHuser=:inUser, AHtime=:sysTime6, AHdt=:sysDate7
             where AHco# = :dsWDELP.ELco# and AHfnd = :dsWDELP.ELfnd
              and AHemp# = :dsWDELP.ELemp# and AHdiv = :dsWDELP.ELdiv
              and AHefDt = :dsWDELP.ELpfDt and AHfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No agent hstory records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv) + ' ' + %char(dsWDELP.ELfyr));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMWVP
             set WVefDt = :intDt7Eff, WVfyr = :intYear,
              WVpgm=:Q1pgm, WVuser=:inUser, WVtime=:sysTime6, WVdt=:sysDate7
             where WVco# = :dsWDELP.ELco# and WVfnd = :dsWDELP.ELfnd
              and WVemp# = :dsWDELP.ELemp# and WVdiv = :dsWDELP.ELdiv
              and WVefDt = :dsWDELP.ELpfDt and WVfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No waiver records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv) + ' ' + %char(dsWDELP.ELfyr));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WMMUP
             set MUeeDt = :intDt7Eff, MUfyr = :intYear,
              MUpgm=:Q1pgm, MUuser=:inUser, MUtime=:sysTime6, MUdt=:sysDate7
             where MUco# = :dsWDELP.ELco# and MUfnd = :dsWDELP.ELfnd
              and MUemp# = :dsWDELP.ELemp# and MUdiv = :dsWDELP.ELdiv
              and MUeeDt = :dsWDELP.ELpfDt and MUfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No endorsement records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv) + ' ' + %char(dsWDELP.ELfyr));
               ioRtnCode = 'Info Only';
           Endif;
           Exec sql
            update WDELP
             set ELpfDt = :intDt7Eff, ELfyr = :intYear, ELptDt = :intDt7Exp,
              ELarcA = :decArcA, ELsFMn = :decSFMN, ELwMin = :decWMin,
              ELcrEf = case when ELcrEf <> 0 then :intDt7Eff else 0 end,
              ELdcEf = case when ELdcEf <> 0 then :intDt7Eff else 0 end,
              ELdvEf = case when ELdvEf <> 0 then :intDt7Eff else 0 end,
              ELd2Ef = case when ELd2Ef <> 0 then :intDt7Eff else 0 end,
              ELemD1 = case when ELemD1 <> 0 then :intDt7Eff else 0 end,
              ELilEf = case when ELilEF <> 0 then :intDt7Eff else 0 end,
              ELarEf = case when ELarEF <> 0 then :intDt7Eff else 0 end,
              ELsdEf = case when ELsdEF <> 0 then :intDt7Eff else 0 end,
              ELmcEf = case when ELmcEF <> 0 then :intDt7Eff else 0 end,
              ELscEf = case when ELscEF <> 0 then :intDt7Eff else 0 end,
              ELbsDt = case when ELbsDt = :dsWDELP.ELpfDt then :intDt7Eff
               else ELbsDt end,
              ELpgm=:Q1pgm, ELuser=:inUser, ELtime=:sysTime6, ELdt=:sysDate7
             where ELco# = :dsWDELP.ELco# and ELfnd = :dsWDELP.ELfnd
              and ELemp# = :dsWDELP.ELemp# and ELdiv = :dsWDELP.ELdiv
              and ELfyr = :dsWDELP.ELfyr;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson
               ('No policy detail records updated. - ' +
               %char(dsWDELP.ELfnd) + ' ' + %char(dsWDELP.ELemp#) + ' ' +
               %char(dsWDELP.ELdiv) + ' ' + %char(dsWDELP.ELfyr));
               ioRtnCode = 'Info Only';
           Endif;
        Enddo;
        Exec sql close cursorDatesEmp;
       Endsr;

       //=================================================================
       // Validate incoming parms
       //=================================================================
       Begsr Validate;

        Select;
        When isEnvQuote;
        Other;
           AddErrorMessageJson('Service requested is not available in ' +
            'current environment ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
           Leavesr;
        Endsl;

        Monitor;
           If dsIn.group <> *zero and dsIn.group <> *blank;
              Select;
              When isEnvQuote and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Quote');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              When isEnvLive and
               not UseGroupInPortal(intCo#:%int(dsIn.group):'Policy');
                 AddErrorMessageJson('Group ' + dsIn.group +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              Endsl;
           Endif;
        On-error;
        Endmon;

        If dsIn.co# <> *blanks;
           Monitor;
              intCo# = %int(dsIn.co#);
           On-Error;
              AddErrorMessageJson('Company has character data.'
               :ioRtnCode);
           EndMon;
        Endif;

        If dsIn.app# <> *blanks;
           Monitor;
              intApp# = %int(dsIn.app#);
           On-Error;
              AddErrorMessageJson('Application has character data.'
              :ioRtnCode);
           Endmon;
        Endif;

        If dsIn.group <> *blanks;
           Monitor;
              intGroup = %int(dsIn.group);
           On-Error;
              AddErrorMessageJson('Group has character data.'
               :ioRtnCode);
           EndMon;
        Endif;

        If dsIn.emp# <> *blanks;
           Monitor;
              intEmp# = %int(dsIn.emp#);
           On-Error;
              AddErrorMessageJson('Employer has character data.'
               :ioRtnCode);
           EndMon;
        Endif;

        If dsIn.div <> *blanks;
           Monitor;
              intDiv = %int(dsIn.div);
           On-Error;
              AddErrorMessageJson('Division has character data.'
               :ioRtnCode);
           EndMon;
        Endif;

        If dsIn.year <> *blanks;
           Monitor;
              intYear = %int(dsIn.year);
           On-Error;
              AddErrorMessageJson('Year has character data.'
               :ioRtnCode);
           EndMon;
        Endif;

        Select;
        When intApp# <> *zero
         and (intGroup <> 0 or intYear <> 0 or intEmp# <> 0 or intDiv <> 0);
           Exec sql
            select * into :dsWIEMP
             from WIEMP
             where EMco# = :intCo# and EMapp# = :intApp# and EMfnd = :intGroup
              and EMemp# = :intEmp# and EMfyr = :intYear
             limit 1;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson('Application and policy information ' +
               'not in sync.'
               :ioRtnCode);
           Endif;
        When intApp# <> *zero;
           Exec sql
            select * into :dsWIEMP
             from WIEMP
             where EMco# = :intCo# and EMapp# = :intApp#
             limit 1;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson('Application does not have any valid ' +
              'policy information.'
               :ioRtnCode);
           Endif;
        Other;
           Exec sql
            select * into :dsWDELP
             from WDELP
             where ELco# = :intCo# and ELfnd = :intGroup
              and ELemp# = :intEmp# and ELdiv = :intDiv and ELfyr = :intYear;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson('Policy information not valid.'
               :ioRtnCode);
           Endif;
           Reset dsWIEMP;
           Exec sql
            select * into :dsWIEMP
             from WIEMP
             where EMco# = :intCo# and EMfnd = :intGroup
              and EMemp# = :intEmp# and EMfyr = :intYear
             limit 1;
           intApp# = dsWIEMP.EMapp#;
        Endsl;

        Monitor;
           intDt7Eff = DtCYMD(%int(dsIn.effDt));
        On-error;
        Endmon;
        If intDt7Eff = *zero;
           AddErrorMessageJson('Invalid effective date.'
            :ioRtnCode);
        Endif;

        Exsr BuildSqlDatesApp;

        Select;
        When intApp# <> *zero;
           Exsr EditApplication;
        Other;
           Exsr EditPolicy;
        Endsl;

        Endsr;

      /End-free
     pModifyPolicyEffectiveDates...
     p                 e


       //======================================================================
       // Print ACORD Form
       //======================================================================
     pPrintACORD       b
     d dsWIACMAIN    e ds                  ExtName(WIACMAIN) Qualified Inz
     d dsWIAPPLOS    e ds                  ExtName(WIAPPLOS) Qualified Inz
     d dsWMECP       e ds                  ExtName(WMECP) Qualified Inz
     d dsWMLDP       e ds                  ExtName(WMLDP) Qualified Inz
     d dsWMOOP       e ds                  ExtName(WMOOP) Qualified Inz
     d dsWTBSP       e ds                  ExtName(WTBSP) Qualified Inz
     d dsWTDDP       e ds                  ExtName(WTDDP) Qualified Inz
     d aryAddress      s            116a   Dim(9999)
     d aryNameLocId    s             60a   Dim(9999)
     d chrDepLvl       s              1a
     d chrPayPlan      s              1a
     d chrQType        s              1a
     d chrRatePlan     s              1a
     d chrResolveToDarts...
     d                 s              1a
     d chr3            s              3a
     d decAccLimit     s             13s 2
     d decAggLimit     s             13s 2
     d decAnnPrm       s             13s 2
     d decArapAmt      s             13s 2
     d decArapFct      s              5s 4
     d decDbCrAmt      s             13s 2
     d decDbCrFct      s              5s 4
     d decDedAmt       s             13s 2
     d decDedPct       s             12s 9
     d decDisLimit     s             13s 2
     d decExpCon       s             13s 2
     d decExpModAmt    s             13s 2
     d decExpModFct    s              5s 4
     d decFeeAmt       s             13s 2
     d decIncLimAmt    s             13s 2
     d decManPrem      s             13s 2
     d decMaPr         s             13s 2
     d decPrmDscFct    s              5s 2
      **** Beg Chg **** 10/21/24 *******************************************
     d***decRate         s              7s 2
     d decRate         s              8s 3
      **** End Chg **** 10/21/24 *******************************************
     d decStdPrm       s             13s 2
     d decStockDisc    s             13s 2
     d decPayAmt       s             13s 2
     d decTotAnnPrem   s             13s 2
     d decTotDeposit   s             13s 2
     d decTotExpCon    s             13s 2
     d decTotManPrem   s             13s 2
     d decTotNrmPrem   s             13s 2
     d decTotStdPrem   s             13s 2
     d dsDivState      ds                  Qualified Dim(99) Inz
     d  state                         2a
     d  division                      5s 0
     d  locID                         5s 0
     d dsFct           ds                  Qualified Dim(99) Inz
     d  State                         2a
     d  MaPr                         13s 2
     d  ILCd                          2a
     d  ILAmt                        13s 2
     d  DDCd                          3a
     d  DDAmt                        13s 2
     d  ExpModFct                     5s 4
     d  ExpModAmt                    13s 2
     d  ArapFct                       5s 4
     d  ArapAmt                      13s 2
     d  DbCrFct                       5s 4
     d  DbCrAmt                      13s 2
     d  StdPrm                       13s 2
     d  PremDisc                     13s 2
     d  PrmDscFct                     5s 4
     d  ExpCon                       13s 2
     d  FeeAmt                       13s 2
     d  AnnPrm                       13s 2
     d eCnt            s              3s 0
     d idx             s              3s 0
     d int3            s              3s 0
     d int4            s              4s 0
     d intAnswerNull   s              5i 0
     d intAgt#         s              5s 0
     d intAnDt         s              7s 0
     d intApp#         s              9s 0
     d intBndDate7     s              7s 0
     d intBSCd         s              3s 0
     d intClas         s              5s 0
     d intDiv          s              5s 0
     d intEfDt         s              7s 0
     d intEmp#         s              9s 0
     d intEmpCnt       s              7s 0
     d intExDt         s              7s 0
     d intEmpFein      s              9s 0
     d intFein         s              9s 0
     d intFnd          s              3s 0
     d intFyr          s              3s 0
     d intInceptDt     s              7s 0
     d intInceptYr     s              4s 0
     d intIncLimDt     s              7s 0
     d intMod          s              3s 0
     d intProd         s              5s 0
     d intRate#        s              3s 0
     d intSeq#         s              7s 0
     d intAcord#       s              7s 0
     d intId#          s              7s 0
     d intParent       s              7s 0
     d intYrsInBus     s              3s 0
     d acordPrintErr   s            100a
     d rsvCode         s              2a
     d rsvType         s              1a
     d rsvTypDesc      s             30a
     d strAddress      s            116a
     d strAgtName      s             40a
     d strAgtMad1      s             40a
     d strAgtMcty      s             25a
     d strAgtMst       s              2a
     d strAgtMzip      s              9a
     d strAgtArea      s              3a
     d strAgtPhn       s              7a
     d strAgtAFax      s              3a
     d strAgtFax       s              7a
     d strAgtEmail     s             30a
     d strAnswer       s             25a
     d strApp#         s              9a
     d strAppName      s            100a
     d strCo#          s              3a
     d strCoName       s             60a
     d strConDesc      s             15a
     d strConDesc1     s             15a
     d strConDesc2     s             15a
     d strConDesc3     s             15a
     d strConFqXRef1   s              2a
     d strConFqXRef2   s              2a
     d strConFqXRef3   s              2a
     d strDCde         s              3a
     d strDedCode      s              3a
     d strEmp#         s              9a
     d strExt          s              5a
     d strFnd          s              3a
     d strFyr          s              3a
     d strILCd         s              2a
     d strIncLim       s              2a
     d strMad1         s             40a
     d strMad2         s             40a
     d strMCty         s             25a
     d strMSt          s              2a
     d strMZip         s              9a
     d strNAIC         s             10a
     d strNameLocId    s             60a
     d strNcciRsk#     s             15a
     d strOffArea      s              3a
     d strOffPhone     s              7a
     d strOffMad1      s             40a
     d strOffMad2      s             40a
     d strOffMCty      s             40a
     d strOffMSt       s              2a
     d strOffMZip      s              9a
     d strOthTypeDesc  s            100a
     d strPad1         s             40a
     d strPad2         s             40a
     d strPCty         s             25a
     d strPSt          s              2a
     d strPZip         s              9a
     d strPayDesc      s            100a
     d strProdName     s             50a
     d strRtCd         s              3a
     d strQuestion     s            500a
     d strState        s              2a
     d strStRisk#      s             15a
     d strTypOfBus     s             10a
     d strSIC          s              6a
     d strNatOfBus     s             40a
     d strUWtr         s              3a
     d strUWtrName     s             50a
     d strWebSite      s             40a
     d workDate        s               d
     D isUseScript     S              1n   inz
     d xxGroupDartCode...
     d                 s              5a
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d xxFlowFile      s             60a
     d xmlLink         s                   like(XPLink)
     d xmlFile         s                   like(XPXML)
     d xmlTFile        s                   like(XPXML)
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('DL')
     d processType     c                   const('ACORD130')

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr GetSelEmpInfo;
          If ioRtnCode <> 'Error';
             Exsr GetXML;
             Exsr WriteFiles;
             Exsr SubmitScriptura;
          Endif;
       Endif;
       Exsr BuildReturnJSON;
       //===================================================
       // Returns document path and name
       //===================================================
       Begsr BuildReturnJSON;
        If ioRtnCode = 'Error' Or ioRtnCode = 'Info Only';
           For idx = 1 to %Elem(errMsgArray);
              If errMsgArray(idx) = *Blanks;
                 Leave;
              EndIf;
              If comma <> *Blank;
                 array = %Trim(array) + comma;
              Else;
                 comma = ',';
              Endif;
              array = %Trim(array) + errMsgArray(idx);
           EndFor;
           idx = %CheckR(' ':json);
           Select;
           When idx = 0;
           Other;
              If %Subst(json:idx:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ]';
        Endif;
        If ioRtnCode <> 'Error';
           json = %trim(json) + ',"documentInfo":{ ' +
             BuildJsonNode('documentPath':%Trim(XPDir) + %Trim(XPfile)) +
            ' }';
        EndIf;
        outJson = json;
       Endsr;
       //===================================================
       // Gets Info for Agent Section
       //===================================================
       Begsr GetAgtInfo;
        Exec Sql
         Select AHagt#, AHprod, AHrt#
         Into :intAgt#, :intProd, :intRate#
         From WMAHP
         Where AHco# = :intCo# And AHfnd = :intFnd
           And AHemp# = :intEmp# And AHdiv = :intDiv
           And AHfyr = :intFyr And AHefdt <= :intEfDt
           And AHstat = 'A'
         Order by AHefdt Desc
         Limit 1;
        Exec Sql
         Select AAfein, AAmod Into :intFein, :intMod From WMAAP
         Where AAco# = :intCo# And AAfnd = :intFnd
           And AAagt# = :intAgt# And AAstat = 'A'
         Order By AAefdt Desc
         Limit 1;
        Exec Sql
         Select AGname, AGmad1, AGmcty, AGmst, AGmzip, Digits(AGare1),
                Digits(AGphn1), Digits(AGare2), Digits(AGphn2), AGemal
         Into :strAgtName, :strAgtMad1, :strAgtMCty, :strAgtMSt, :strAgtMZip,
              :strAgtArea, :strAgtPhn, :strAgtAFax, :strAgtFax, :strAgtEmail
         From WMAGP
         Where AGco# = :intCo# And AGfein = :intFein And AGmod = :intMod;
        Exec Sql
         Select Trim(APfsn) Concat ' ' Concat APlsn
         Into :strProdName
         From WMAPP
         Where APco# = :intCo# And APfein = :intFein
           And APmod = :intMod And APprod = :intProd;
        Exec SQL
         Select QTtype, QTefdt
          Into :chrQType, :intBndDate7
          From WAQTP
          Where QTco# = :intCo#
           And QTfnd = :intFnd
           And QTemp# = :intEmp#
           And QTdiv = :intDiv
           And QTagt# = :intAgt#
           And QTstat = 'Q';
        Exec Sql
         Select ARplan
         Into :chrRatePlan
         From WMARP
         Where ARco# = :intCo#
          And ARfnd = :intFnd
          And ARrt# = :intRate#;
        ACAgtName = strAgtName;
        ACAgtMad1 = strAgtMad1;
        ACAgtMCty = strAgtMcty;
        ACAgtMSt = strAgtMSt;
        ACAgtMZip = strAgtMZip;
        ACAgtArea = strAgtArea;
        ACAgtPhn = strAgtPhn;
        ACAgtAFax = strAgtAFax;
        ACAgtFax = strAgtFax;
        ACAgtEmail = strAgtEmail;
        ACProdName = strProdName;
       Endsr;
       //===================================================
       // Gets Info for Applicant Section (WIACMAIN)
       //===================================================
       Begsr GetApplicantInfo;
        ACCoName = strCoName;
        Exec Sql
         Select UPusnm
         Into :strUWtrName
         From WMUPP
         Where UPstaf = :strUWtr;
        ACUWtrName = strUWtrName;
        ACAppName = strAppName;
        ACOffArea = strOffArea;
        ACOffPhn# = strOffPhone;
        ACOffMad1 = strOffMad1;
        ACOffMad2 = strOffMad2;
        ACOffMcty = strOffMCty;
        ACOffMst  = strOffMst;
        ACOffMzip = strOffMZip;
        Exec Sql
         Select APyib
         Into :intYrsInBus
         From WIAPP
         Where APapp# = :intApp#;
        ACyib = intYrsInBus;
        ACsic = strSIC;
        ACnaic = strNAIC;
        ACnatob = strNatOfBus;
        // website/email?? acwebsite/acemail
        Exsr GetTypOfBus;
        // ACid#??
        ACFEIN = %EditC(intEmpFein:'X'   );
        ACNcciRsk# = strNcciRsk#;
        ACOthRatID = strStRisk#;
        // Status of Submission
        Select;
        When chrQType = 'Q' Or chrQType = 'q';
           ACQuoted = '1';
        When chrQType = 'B' Or chrQType = 'b'
         Or chrQType = 'M' Or chrQType = 'm';
           ACBound = '1';
           ACBndDate = %EditC(dtMDYY(intBndDate7):'X');
           If ACBndDate = '0';
              ACBndDate = *Blanks;
           Endif;
        EndSl;
        // Billing Plan
        Select;
        When chrRatePlan = *Blank;
           ACDirBill = '1';
        When chrRatePlan = 'A';
           ACAgtBill = '1';
        EndSl;
        Exsr GetPayPlan;
        // Audit Plan
        ACExpAud = '1';
        // Policy Information
        ACPFDt = DtMdyy(intEfDt);
        ACPTDt = DtMdyy(intExDt);
        If intAnDt <> 0;
           ACARDt = DtMdyy(intAnDt);
           ACpart = '1';
        EndIf;
        If dsWTBSP.BSlpgm = 'I' or dsWTBSP.BSlpgm = 'P';
           ACretro = dsWTBSP.BSdesc;
        EndIf;
        Exsr GetEmpLimits;
        Exsr GetDedInfo;
        Exsr GetOthCoverage;
        Exsr GetPremTotals;
        Exsr GetContacts;
        // Loss Run Attached Flag ??  ACLossRun
        // Nature of Business Description ?? ACNatOB
        // Remarks for Questions ?? ACRemarks
        // National Producer # ?? ACNatPrd#
        // Addendum ??
       Endsr;
       //===================================================
       // Contact Info
       //===================================================
       Begsr GetContacts;
        Exec Sql
         Declare c4 Cursor For
          Select *
          From WMECP
          Where ECco# = :intCo#
           And ECfnd = :intFnd
           And ECemp# = :intEmp#
           And ECdiv = :intDiv;
        Exec Sql
         Open c4;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c4
            Into :dsWMECP;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           Select;
           When dsWMECP.ECpcnt = *Blank;
              Iter;
           When dsWMECP.ECpcnt = '1' And strConFqXRef1 = 'IN'
            Or dsWMECP.ECpcnt = '2' And strConFqXRef2 = 'IN'
            Or dsWMECP.ECpcnt = '3' And strConFqXRef3 = 'IN';
              Select;
              When dsWMECP.ECfsn <> *Blanks;
                 ACInspName = %Trim(dsWMECP.EClsn) +
                  ', ' + dsWMECP.ECfsn;
              Other;
                 ACInspName = dsWMECP.EClsn;
              Endsl;
              ACInspOPhA = %Editc(dsWMECP.ECarea:'X');
              ACInspOPhN = %Editc(dsWMECP.ECphn#:'X');
              ACInspMPhA = %Editc(dsWMECP.ECcara:'X');
              ACInspMPhN = %Editc(dsWMECP.ECcphn:'X');
              ACInspEMal = dsWMECP.ECemal;
           When dsWMECP.ECpcnt = '1' And strConFqXRef1 = 'CL'
            Or dsWMECP.ECpcnt = '2' And strConFqXRef2 = 'CL'
            Or dsWMECP.ECpcnt = '3' And strConFqXRef3 = 'CL';
              Select;
              When dsWMECP.ECfsn <> *Blanks;
                 ACClmsName = %Trim(dsWMECP.EClsn) +
                  ', ' + dsWMECP.ECfsn;
              Other;
                 ACClmsName = dsWMECP.EClsn;
              Endsl;
              ACClmsOPhA = %Editc(dsWMECP.ECarea:'X');
              ACClmsOPhN = %Editc(dsWMECP.ECphn#:'X');
              ACClmsMPhA = %Editc(dsWMECP.ECcara:'X');
              ACClmsMPhN = %Editc(dsWMECP.ECcphn:'X');
              ACClmsEMal = dsWMECP.ECemal;
           When dsWMECP.ECpcnt = '1' And strConFqXRef1 = 'AC'
            Or dsWMECP.ECpcnt = '2' And strConFqXRef2 = 'AC'
            Or dsWMECP.ECpcnt = '3' And strConFqXRef3 = 'AC';
              Select;
              When dsWMECP.ECfsn <> *Blanks;
                 ACAcctName = %Trim(dsWMECP.EClsn) +
                  ', ' + dsWMECP.ECfsn;
              Other;
                 ACAcctName = dsWMECP.EClsn;
              Endsl;
              ACAcctOPhA = %Editc(dsWMECP.ECarea:'X');
              ACAcctOPhN = %Editc(dsWMECP.ECphn#:'X');
              ACAcctMPhA = %Editc(dsWMECP.ECcara:'X');
              ACAcctMPhN = %Editc(dsWMECP.ECcphn:'X');
              ACAcctEMal = dsWMECP.ECemal;
           Endsl;
        Enddo;
        Exec Sql
         Close c4;
       EndSr;
       //===================================================
       // Deductible Info
       //===================================================
       Begsr GetDedInfo;
        Select;
        When dsWTBSP.BSlpgm = 'L';
           Exec Sql
            Select *
            Into :dsWMLDP
            From WMLDP
            Where LDco# = :intCo#
             And LDfnd  = :intFnd
             And LDemp# = :intEmp#
             And LDdiv  = :intDiv
             And LDfyr  = :intFyr;
           If IsValidFetch(sqlStt);
              ACAmtPct = dsWMLDP.LDpct1;
              For idx = 1 to 10;
                 Clear rsvType;
                 Clear rsvTypDesc;
                 Clear rsvCode;
                 Select;
                 When idx = 1;
                    rsvCode = dsWMLDP.LDr101;
                 When idx = 2;
                    rsvCode = dsWMLDP.LDr102;
                 When idx = 3;
                    rsvCode = dsWMLDP.LDr103;
                 When idx = 4;
                    rsvCode = dsWMLDP.LDr104;
                 When idx = 5;
                    rsvCode = dsWMLDP.LDr105;
                 When idx = 6;
                    rsvCode = dsWMLDP.LDr106;
                 When idx = 7;
                    rsvCode = dsWMLDP.LDr107;
                 When idx = 8;
                    rsvCode = dsWMLDP.LDr108;
                 When idx = 9;
                    rsvCode = dsWMLDP.LDr109;
                 When idx = 10;
                    rsvCode = dsWMLDP.LDr110;
                 EndSl;
                 If rsvCode = *Blank;
                    Leave;
                 EndIf;
                 Exec Sql
                  Select RTtype, RTdesc
                  Into :rsvType, :rsvTypDesc
                  From WTRTP
                  Where RTco# = :intCo#
                   And RTkey = :rsvCode;
                 Select;
                 When rsvType = 'C';
                    ACDedInd = '1';
                 When rsvType = 'M';
                    ACDedMed = '1';
                 When rsvType <> *Blank;
                    ACDedOth = '1';
                    ACDedOthD = rsvTypDesc;
                 EndSl;
                 If ACDedInd = '1' And ACDedMed = '1'
                  And ACDedOth = '1';
                    Leave;
                 EndIf;
              EndFor;
           Endif;
        When strDedCode <> *Blanks;
           Exec SQL
            Select * Into :dsWTDDP From WTDDP
             Where DDco# = :intCo# And DDfnd = :intFnd
               And DDst = :strState And DDcode = :strDedCode
               And DDefdt <= :intEfDt And DDstat = 'A'
             Order by DDefdt Desc
             Limit 1;
           If IsValidFetch(sqlStt);
              ACAmtPct = dsWTDDP.DDpct;
              For idx = 1 to 10;
                 Clear rsvType;
                 Clear rsvTypDesc;
                 Clear rsvCode;
                 Select;
                 When idx = 1;
                    rsvCode = dsWTDDP.DDdc01;
                 When idx = 2;
                    rsvCode = dsWTDDP.DDdc02;
                 When idx = 3;
                    rsvCode = dsWTDDP.DDdc03;
                 When idx = 4;
                    rsvCode = dsWTDDP.DDdc04;
                 When idx = 5;
                    rsvCode = dsWTDDP.DDdc05;
                 When idx = 6;
                    rsvCode = dsWTDDP.DDdc06;
                 When idx = 7;
                    rsvCode = dsWTDDP.DDdc07;
                 When idx = 8;
                    rsvCode = dsWTDDP.DDdc08;
                 When idx = 9;
                    rsvCode = dsWTDDP.DDdc09;
                 When idx = 10;
                    rsvCode = dsWTDDP.DDdc10;
                 EndSl;
                 If rsvCode = *Blank;
                    Leave;
                 EndIf;
                 Exec Sql
                  Select RTtype, RTdesc
                  Into :rsvType, :rsvTypDesc
                  From WTRTP
                  Where RTco# = :intCo#
                   And RTkey = :rsvCode;
                 Select;
                 When rsvType = 'C';
                    ACDedInd = '1';
                 When rsvType = 'M';
                    ACDedMed = '1';
                 When rsvType <> *Blank;
                    ACDedOth = '1';
                    ACDedOthD = rsvTypDesc;
                 EndSl;
                 If ACDedInd = '1' And ACDedMed = '1'
                  And ACDedOth = '1';
                    Leave;
                 EndIf;
              EndFor;
           Endif;
        EndSl;
       Endsr;
       //==================================================
       // Employer Liability Limits
       //==================================================
       Begsr GetEmpLimits;
        Select;
        When strIncLim <> *Blanks;
           Exec Sql
            Select LMacc * 1000, LMagg * 1000, LMdis * 1000
            Into :decAccLimit, :decAggLimit, :decDisLimit
            From WTLMP
            Where LMco# = :intCo# And LMfnd = :intFnd
             And LMst = :strState And LMkey = :strIncLim
             And LMefdt <= :intIncLimDt And LMstat = 'A'
            Order By LMefdt Desc
            Limit 1;
        Other;
           Exec SQL
            Select RRacc * 1000, RRagg * 1000, RRdis * 1000
            Into :decAccLimit, :decAggLimit, :decDisLimit
            From WDRRP
            Where RRco# = :intCo# And RRfnd = :intFnd
             And RRst = :strState And RRefdt <= :intEfDt
            Order By RRefdt Desc
            Limit 1;
        EndSl;
        ACPerAcc = decAccLimit;
        ACDisPol = decAggLimit;
        ACDisEmp = decDisLimit;
       Endsr;
       //===================================================
       // Get Other Coverages for Main Form
       //===================================================
       Begsr GetOthCoverage;
        Clear int3;
        Exec Sql
         Select Count(*)
         Into :int3
         From WDELP
         Join WMEPP
          On ELco# = EPco#
           And ELfnd = EPfnd
           And ELemp# = EPemp#
           And ELdiv = EPdiv
           And ELfyr = EPfyr
         Join WMPRP
          On PRco# = EPco#
           And PRrtcd = ELrtcd
           And PRclas = EPclas
           And PRefdt <= ELpfdt
         Where ELco# = :intCo#
          And Elfnd = :intFnd
          And Elemp# = :intEmp#
          And Elfyr = :intFyr
          And (EPspec = 'F'
          Or PRlhcd = 'Y');
         If int3 > 0;
            ACUslhCov = '1';
         Endif;
       Endsr;
       //===================================================
       // Parses Input JSON
       //===================================================
       Begsr GetParms;
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json = json + ' "requestParms" : {';
           node = YAJL_object_find(docNode:'application');
           strApp# = YAJL_get_string(node);
           BuildGiantJSON(json :'application' :strApp#);
           node = YAJL_object_find(docNode: 'company');
           strCo#  = YAJL_get_string(node);
           BuildGiantJSON(json :'company' :strCo#);
           node = YAJL_object_find(docNode: 'group');
           strFnd  = YAJL_get_string(node);
           BuildGiantJSON(json :'group' :strFnd);
           node = YAJL_object_find(docNode: 'employer');
           strEmp# = YAJL_get_string(node);
           BuildGiantJSON(json :'employer' :strEmp#);
           node = YAJL_object_find(docNode: 'year');
           strFyr  = YAJL_get_string(node);
           BuildGiantJSON(json :'year':strFyr);
           node = YAJL_object_find(docNode: 'resolveToDarts');
           chrResolveToDarts = YAJL_get_string(node);
           BuildGiantJSON(json :'resolveToDarts':chrResolveToDarts);
           json = json + '}';
           If strApp# <> *Blanks;
              Monitor;
                 intApp# = %Int(strApp#);
              On-Error;
                 errMsgArray(1) = '"Application number has character data."';
                 ioRtnCode = 'Error';
              Endmon;
              If intApp# <> 0 and ioRtnCode <> 'Error';
                 Open WIEML;
                 Setll (intCo#:intApp#) pl100;
                 Dou %Eof;
                    Reade (intCo#:intApp#) pl100;
                    If %Eof;
                       Leave;
                    Endif;
                    If L1current = 'Y';
                       intFnd  = L1fnd;
                       intEmp# = L1emp#;
                       intFyr  = L1fyr;
                       Leave;
                    Endif;
                 EndDo;
                 Close WIEML;
              Else;
                 eCnt += 1;
                 errMsgArray(eCnt) = '"Application number does not exist."';
                 ioRtnCode = 'Error';
              Endif;
           Else;
              If strCo# <> *Blanks;
                 Monitor;
                    intCo# = %Int(strCo#);
                 On-Error;
                    eCnt += 1;
                    errMsgArray(eCnt) = '"Company number has character data."';
                    ioRtnCode = 'Error';
                 Endmon;
              Endif;
              If strFnd <> *Blanks;
                 Monitor;
                    intFnd = %Int(strFnd);
                 On-Error;
                    eCnt += 1;
                    errMsgArray(eCnt) = '"Group number has character data."';
                    ioRtnCode = 'Error';
                 Endmon;
              Endif;
              If strEmp# <> *Blanks;
                 Monitor;
                    intEmp# = %Int(strEmp#);
                 On-Error;
                    eCnt += 1;
                    errMsgArray(eCnt) = '"Employer number has character data."';
                    ioRtnCode = 'Error';
                 Endmon;
              Endif;
              If strFyr <> *Blanks;
                 Monitor;
                    intFyr = %Int(strFyr);
                 On-Error;
                    eCnt += 1;
                    errMsgArray(eCnt) = '"Policy year has character data."';
                    ioRtnCode = 'Error';
                 Endmon;
              Endif;
              Open WIEML1;
              Chain (intCo#:intFnd:intEmp#:intFyr) pl101;
              If %Found;
                 intApp# = L1app#;
              Else;
                 eCnt += 1;
                 errMsgArray(eCnt) = '"Employer info does not exist."';
                 ioRtnCode = 'Error';
              EndIf;
              Close WIEML1;
           Endif;
           If chrResolveToDarts = 'Y';
              xxGroupDartCode = GetGroupDartCode(intCo#:intFnd:'ACORD');
              If xxGroupDartCode = *Blank;
                 eCnt += 1;
                 errMsgArray(eCnt) = '"Document can not resolve to iDarts.' +
                   ' ACORD Darts code doesn''t exist."';
                 ioRtnCode = 'Info Only';
              Endif;
           Endif;
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtnCode = 'Error';
        Endif;
       Endsr;
       //===================================================
       // Get Payment Plan
       //===================================================
       Begsr GetPayPlan;
        Exec Sql
         Select *
         Into :dsWTBSP
         From WTBSP
         Where BSco# = :intCo#
          And BSkey = Digits(:intBSCd);
        Select;
        When dsWTBSP.BSsefl = 'E';
           strPayDesc = Upper(dsWTBSP.BSdesc);
           Dou ACOthPay = '1';
              int3 = %Scan('MONTH':strPayDesc);
              If int3 <> 0 And dsWTBSP.BSfreq = 'M';
                 ACOthPay = '1';
                 ACOthPayD = dsWTBSP.BSdesc;
                 Leave;
              EndIf;
              int3 = %Scan('ANNUAL':strPayDesc);
              If int3 <> 0 And dsWTBSP.BSfreq = 'M';
                 ACAnnPay = '1';
                 Leave;
              EndIf;
              int3 = %Scan('SEMI':strPayDesc);
              If int3 <> 0 And dsWTBSP.BSfreq = 'M';
                 ACSemPay = '1';
                 Leave;
              EndIf;
              int3 = %Scan('QUAR':strPayDesc);
              If int3 <> 0;
                 ACQuarPay = '1';
                 Leave;
              EndIf;
              ACOthPay = '1';
              ACOthPayD = dsWTBSP.BSdesc;
           Enddo;
           Select;
           When dsWTBSP.BSEsDp <> 0;
              ACPctDown = dsWTBSP.BSEsDp/100;
           When dsWTBSP.BSEsD2 <> 0;
              ACPctDown = dsWTBSP.BSEsD2/100;
           EndSl;
        When dsWTBSP.BSsefl = 'S';
           ACOthPay = '1';
           ACOthPayD = dsWTBSP.BSdesc;
           Select;
           When dsWTBSP.BSAB01 <> 0;
              ACPctDown = dsWTBSP.BSAB01;
           When dsWTBSP.BSAB02 <> 0;
              ACPctDown = dsWTBSP.BSAB02;
           EndSl;
        EndSl;
       Endsr;
       //===================================================
       // Total Premium in Applicant Section
       //===================================================
       Begsr GetPremTotals;
        // Total Estimated Annual Premium
        Exec Sql
         Select Sum(ELmapr), Sum(ELstpr), Sum(ELenmp),
          Sum(ELanpr), Sum(ELprtd)
         Into :decTotManPrem, :decTotStdPrem, :decTotNrmPrem,
          :decTotAnnPrem, :decTotExpCon
         From WDELP
         Where ELco# = :intCo#
          And ELfnd  = :intFnd
          And ELemp# = :intEmp#
          And ELfyr  = :intFyr;
        ACTotPrem = decTotAnnPrem;
        // Total Deposit
        Select;
        When chrDepLvl = 'M';
           Eval(h) decTotDeposit = decTotManPrem * ACPctDown;
        When chrDepLvl = 'S';
           Eval(h) decTotDeposit = decTotStdPrem * ACPctDown;
        When chrDepLvl = 'N';
           Eval(h) decTotDeposit = decTotNrmPrem * ACPctDown;
        When chrDepLvl = 'A';
           Eval(h) decTotDeposit = (decTotAnnPrem - decTotExpCon) *
            ACPctDown;
        Endsl;
        // Total Min Premium ??  ACMinPrem
       Endsr;
       //===================================================
       // Gets Selected Employer Information
       //===================================================
       Begsr GetSelEmpInfo;
        // Get Selected Policy From App#
        Open WIEML;
        Setll (intCo#:intApp#) pl100;
        Dou %Eof;
           Reade (intCo#:intApp#) pl100;
           If %Eof;
              Leave;
           Endif;
           If L1current = 'Y';
              intFnd  = L1fnd;
              intEmp# = L1emp#;
              intFyr  = L1fyr;
              Leave;
           Endif;
        EndDo;

        Select;
        When isEnvQuote and not UseGroupInPortal(intCo#:intFnd:'Quote');
           eCnt += 1;
           errMsgArray(eCnt) = '"Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ')."';
           ioRtnCode = 'Error';
        When isEnvLive and not UseGroupInPortal(intCo#:intFnd:'Policy');
           eCnt += 1;
           errMsgArray(eCnt) = '"Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ')."';
           ioRtnCode = 'Error';
        Endsl;

        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

        // Get Fund Info
        Exec Sql
         Select FNname
         Into :strCoName
         From WMFNP
         Where FNco# = :intCo# And FNfnd = :intFnd;
        // Get Fund DARTS/Admin Control Info
        Exec Sql
         Select EXcnt1, EXcnt2, EXcnt3,
          EXfqc1, EXfqc2, EXfqc3
         Into :strConDesc1, :strConDesc2, :strConDesc3,
          :strConFqXRef1,:strConFqXRef2,:strConFqXRef3
         From WMEXP
         Where EXco# = :intCo# And EXfnd = :intFnd;
        // Get Fund Year Detail Info
        Exec Sql
         Select F1dps
         Into :chrDepLvl
         From WDF1P
         Where F1co# = :intCo#
          And F1fnd = :intFnd
          And F1fyr = :intFyr;
        // Get Employer Master Info
        Exec Sql
         Select EMdiv, EMuwtr, EMname Concat EMnam2,
          Digits(EMarea), Digits(EMphn#), EMmad1,
          EMmad2, EMmcty, EMmst, Digits(EMmzip),
          EMsic, EMnaic, EMiadr, EMtpob, EMfein,
          EMrsk#, EMsemp, EMefd1, EMnaob
         Into :intDiv, :strUWtr, :strAppName, :strOffArea,
          :strOffPhone, :strOffMad1, :strOffMad2,
          :strOffMCty, :strOffMSt, :strOffMZip,
          :strSIC, :strNAIC, :strWebSite, :strTypOfBus,
          :intEmpFein, :strNcciRsk#, :strStRisk#, :intInceptDt,
          :strNatOfBus
         From WMEMP
         Where EMco# = :intCo# and EMfnd = :intFnd
           And EMemp# = :intEmp# And EMstrp = 'Y';
        // Get inception date and year
        Exec Sql
         Select Min(EMefd1)
         Into :intInceptDt
         From WMEMP
         Where EMco# = :intCo# and EMfnd = :intFnd
          And EMemp# = :intEmp#;
        workDate = %Date(intInceptDt:*Cymd);
        intInceptYr = %Subdt(workDate:*Years);
        // Get Policy Year Detail Info
        Exec Sql
         Select ELpfdt, ELptdt, ELandt, ELrtcd, ELbscd,
          ELilcd, ELilef, ELdcde
         Into :intEfDt, :intExDt, :intAnDt, :strRtCd, :intBSCd,
          :strIncLim, :intIncLimDt, :strDedCode
         From WDELP
         Where ELco# = :intCo# And ELfnd = :intFnd
           And ELemp# = :intEmp# And ELdiv = :intDiv
           And ELfyr = :intFyr;
        // Get rating state
        Exec Sql
         Select RSjrst
         Into :strState
         From WMRSP
         Where RSco# = :intCo# And RSrtcd = :strRtCd;
        Close WIEML;
       Endsr;
       //==================================================
       // Type of Business
       //==================================================
       Begsr GetTypOfBus;
        Select;
        When strTypOfBus = '1';
           ACSolProp = '1';
        When strTypOfBus = '2';
           ACPartner = '1';
        When strTypOfBus = '3';
           ACCorp = '1';
        When strTypOfBus = '9';
           ACJointVen = '1';
        When strTypOfBus = 'A';
           ACLLC = '1';
        When strTypOfBus = 'B';
           ACTrust = '1';
        Other;
           ACOthTypOB = '1';
           Exec Sql
            Select GTdesc
            Into :strOthTypeDesc
            From WTGTP
            Where GTco# = :intCo#
             And GTtbid = 'BUSI'
             And GTkey = :strTypOfBus;
           ACOthTyDes = strOthTypeDesc;
        EndSl;
       Endsr;
       //===================================================
       // Gets Scriptura Information
       //===================================================
       Begsr GetXML;

        Clear xmlFile;
        Clear xmlTFile;
        Clear xxFlowFile;
        Clear xmlLink;

        //* Check if Scriptura is enabled for process.
        isUseScript = UseScriptura(processType);
        If isUseScript;
           xxProcType = processType;
           message = %trim(xxProcType);
           string = ' ';
           xxMass = ' ';

           //* Load the LDA with defaults
           In *Lock *Dtaara;
           Clear svLDA;


           If xxGroupDartCode <> *Blanks And chrResolveToDarts = 'Y';
              xxDart = 'Y';
              xxRmCd = xxGroupDartCode;
           Else;
              xxDart = 'N';
              xxRmCd = *blanks;
           Endif;

           Out *dtaara;

           //* Populate LDA for process
           xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');

           Setll (intCo#:intFnd:intFyr:intEmp#) pel01;
           Reade(n) (intCo#:intFnd:intFyr:intEmp#) pel01;
           x#rrn = dbrrn;

           //* Request XML records
           StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
            string:%editc(intCo#:'X'):%editc(intFnd:'X'):xxMass:xmlLink);

           //* Flow file name to control/submit process
           xxFlowFile = xmlFile;

           Chain (xmlFile) xp100;

           //* Template file name for work files
           xmlTFile = XPXmlT;

           XPData = 'Y';
           XPQte  = 'Q';

           XPPgm  = Q1PGM;
           XPUser = Q1USER;
           XPTime = Timeto6(SystemTime());
           XPDt   = Dateto7(SystemDate());

           Update xp100;

        Endif;

       Endsr;
       //===================================================
       // Writes Scriptura Workfiles
       //===================================================
       Begsr WriteFiles;
        Exsr WriteMainForm;
        Exsr WriteStates;
        Exsr WriteLocations;
        Exsr WritePriorCarrier;
        Exsr WriteStateClassCodes;
        Exsr WriteStatePremiums;
        Exsr WriteOwners;
        Exsr WriteQuestions;
       Endsr;
       //===================================================
       // Prints the document
       //===================================================
       Begsr SubmitScriptura;

        acordPrintErr = ' ';
        Select;
        When xxFlowFile = *Blanks;
        Other;
           Monitor;
              S4401C(xxFlowFile + '$');
           On-error;
              Clear XPFile;
              Clear XPDir;
              acordPrintErr = '"Scriptura or Java Error Encountered."';
           Endmon;
        Endsl;
        //* Return error message if document creation failed.
        If acordPrintErr <> ' ';
           errMsgArray(1) = %trim(acordPrintErr);
           errMsgArray(2) = '"Document Process Key: ' + %trim(XPXML) + '"';
           ioRtnCode = 'Error';
        Endif;

       Endsr;
       //===================================================
       // Writes WIACMAIN
       //===================================================
       Begsr WriteMainForm;
        Open WIACMAIN;
        ACxml = xmlTFile;
        ACco# = intCo#;
        ACapp# = intApp#;
        ACfyr = intFyr;
        ACdt = sysDate7;
        Exsr GetAgtInfo;
        Exsr GetApplicantInfo;
        Write pMain;
        Close WIACMAIN;
       Endsr;
       //===================================================
       // Writes WIACLOCS
       //===================================================
       Begsr WriteLocations;
        Open WIACLOCS;
        LOxml = xmlTFile;
        LOco#  = intCo#;
        LOapp# = intApp#;
        LOfyr  = intFyr;
        LOdt = sysDate7;
        Exec Sql
         Declare c2 Cursor For
         Select EMdiv, RSjrst, EMmad1, EMmad2, EMmcty, EMmst, Digits(EMmzip),
          EMpad1, EMpad2, EMpcty, EMpst, Digits(EMpzip)
         From WMEMP
          Join WDELP
           On EMco# = ELco#
            And EMfnd = ELfnd
            And EMemp# = ELemp#
            And EMdiv = ELdiv
            And ELfyr = :intFyr
          Join WMRSP
           On RSco# = ELco#
            And RSrtcd = ELrtcd
         Where EMco# = :intCo# And EMfnd = :intFnd
          And EMemp# = :intEmp#
         Order by EMdiv;
        Exec Sql
         Open c2;
        idx = 0;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c2
            Into :intDiv, :strState, :strMAd1, :strMAd2, :strMCty, :strMSt,
             :strMZip, :strPAd1, :strPAd2, :strPCty, :strPSt, :strPZip;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           idx += 1;
           Select;
           When strPAd1 <> *Blanks;
              strAddress = strPAd1 + strPAd2 + strPCty + strPSt +
               strPZip;
           When strMAd1 <> *Blanks;
              strAddress = strMAd1 + strMAd2 + strMCty + strMSt +
               strMZip;
           EndSl;
          // Find next available spot in array for new address
           int4 = %LookUp(strAddress:aryAddress);
           If int4 = 0;
              int4 = %LookUp(*Blanks:aryAddress);
              aryAddress(int4) = strAddress;
              LOlocID = int4;
              LOAdd1 = %Subst(strAddress:1:40);
              LOAdd2 = %Subst(strAddress:41:40);
              LOCity = %Subst(strAddress:81:25);
              LOst   = %Subst(strAddress:106:2);
              LOzip  = %Subst(strAddress:108:9);
              Write pLocs;
           Endif;
          // Save division, state, and ID assigned to location for later reference.
           dsDivState(idx).state = strState;
           dsDivState(idx).division = intDiv;
           dsDivState(idx).locID = int4;
        Enddo;
        Exec Sql
         Close c2;
        Exec Sql
         Declare c6 Cursor For
         Select Distinct ALmad1, ALmad2, ALmcty, ALmst, Digits(ALmzip),
          ALpad1, ALpad2, ALpcty, ALpst, Digits(ALpzip)
         From WMALP
         Where ALco# = :intCo# And ALfnd = :intFnd
          And ALemp# = :intEmp# And ALncci = 'Y';
        Exec Sql
         Open c6;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c6
            Into :strMAd1, :strMAd2, :strMCty, :strMSt, :strMZip,
             :strPAd1, :strPAd2, :strPCty, :strPSt, :strPZip;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           Select;
           When strPAd1 <> *Blanks;
              strAddress = strPAd1 + strPAd2 + strPCty + strPSt +
               strPZip;
           When strMAd1 <> *Blanks;
              strAddress = strMAd1 + strMAd2 + strMCty + strMSt +
               strMZip;
           EndSl;
           int4 = %LookUp(strAddress:aryAddress);
           If int4 = 0;
              int4 = %LookUp(*Blanks:aryAddress);
              aryAddress(int4) = strAddress;
              LOlocId = int4;
              LOAdd1 = %Subst(strAddress:1:40);
              LOAdd2 = %Subst(strAddress:41:40);
              LOCity = %Subst(strAddress:81:25);
              LOst   = %Subst(strAddress:106:2);
              LOzip  = %Subst(strAddress:108:9);
              Write pLocs;
           Endif;
        Enddo;
        Exec Sql
         Close c6;
        Close WIACLOCS;
       Endsr;
       //===================================================
       // Writes WIACINDI
       //===================================================
       Begsr WriteOwners;
        Open WIACINDI;
        CIxml = xmlTFile;
        CIco#  = intCo#;
        CIapp# = intApp#;
        CIfyr  = intFyr;
        CIdt = sysDate7;
        Clear intSeq#;
        Exec Sql
         Declare c7 Cursor For
         Select *
         From WMOOP
         Where OOco# = :intCo#
          And OOfnd  = :intFnd
          And OOemp# = :intEmp#
          And OOelid = ' '
          And OOstat = 'A';
        Exec Sql
         Open c7;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c7
            Into :dsWMOOP;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
          // Find state for division
           idx = %LookUp(dsWMOOP.OOdiv:dsDivState(*).division);
          // Find first location ID used for the state
           idx = %LookUp(dsDivState(idx).state:dsDivState(*).state);
           CIlocId = dsDivState(idx).locID;
          // Don't duplicate name if already used for location ID
           strNameLocId = %Editc(CIlocId:'X') +
            dsWMOOP.OOofsn + dsWMOOP.OOolsn;
           idx = %LookUp(strNameLocId:aryNameLocId);
           If idx = 0;
              idx = %LookUp(*Blanks:aryNameLocId);
              aryNameLocId(idx) = strNameLocId;
           Else;
              Iter;
           Endif;
          // Populate workfile
           Select;
           When dsWMOOP.OOofsn <> *Blanks;
              CIname = %Trim(dsWMOOP.OOolsn) + ',' +
               dsWMOOP.OOofsn;
           Other;
              CIname = dsWMOOP.OOolsn;
           EndSl;
           CIdob = 0;
           If dsWMOOP.OOdob <> 0;
              CIdob = dtMDYY(dsWMOOP.OOdob);
           Endif;
           CItitle = dsWMOOP.OOtitl;
           CIpct = dsWMOOP.OOofpc;
           CIinexfl = dsWMOOP.OOexfl;
           CIclass = dsWMOOP.OOclas;
           intSeq# += 1;
           CIScrpSeq = intSeq#;
           // CIState - not stored in CompSuite
           // CIpayroll - not stored in CompSuite
           // CIDuties - not stored in CompSuite
           Write pOwners;
        Enddo;
        Exec Sql
         Close c7;
        Close WIACINDI;
       Endsr;
       //===================================================
       // Writes WIACPRCAR
       //===================================================
       Begsr WritePriorCarrier;
        Open WIACPRCAR;
        PCxml = xmlTFile;
        PCco# = intCo#;
        PCapp# = intApp#;
        PCfyr = intFyr;
        PCdt = sysDate7;
        Exec Sql
         Declare c3 Cursor For
         Select *
         From WIAPPLOS
         Where APco# = :intCo#
          And APapp# = :intApp#
          Order By APyear;
        Exec Sql
         Open c3;
        int3 = 0;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next From c3 Into :dsWIAPPLOS;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           PCYear    = intInceptYr - int3;
           PCAnnPrem = dsWIAPPLOS.APAnnPrem;
           PCClmAmt  = dsWIAPPLOS.AP#Losses;
           PCAmtPaid = dsWIAPPLOS.APPaid;
           PCReserve = dsWIAPPLOS.APReserve;
           Write pPrLoss;
           Int3 += 1;
        Enddo;
        Exec Sql
         Close c3;
        Close WIACPRCAR;
       Endsr;
       //===================================================
       // Writes WIACQST
       //===================================================
       Begsr WriteQuestions;
        Open WIACQST;
        QSxml = xmlTFile;
        QSco# = intCo#;
        QSapp# = intApp#;
        QSfyr = intFyr;
        QSdt = sysDate7;
        Exec Sql
         Declare c9 Cursor For
         Select b.QUAcord#, a.QUid#, a.QUparent, a.QUquestion, APresp
         From WIQUP a Join WIQUPACD b
           On  a.QUco# = b.QUco#
           And a.QUid# = b.QUid#
          Left Outer Join WIAPPANS
           On a.QUco# = APco#
           And a.QUid# = APid# And APapp# = :intApp#
         Where a.QUco# = :intCo# And b.QUacord# <> 0 And a.QUstat <> 'I'
         Order by b.QUAcord#;
        Exec Sql
         Open c9;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c9
            Into :intAcord#, :intId#, :intParent, :strQuestion,
                      :strAnswer :intAnswerNull;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           QSseq    = intAcord#;
           QSid#    = intId#;
           QSparent = intParent;
           QSquest  = strQuestion;
           QSanswer = ' ';
           QSdetail = ' ';
           QSdetail = ' ';
           Select;
           When strAnswer = 'Yes';
              QSanswer  = 'Y';
           When strAnswer = 'No';
              QSanswer  = 'N';
           Other;
              QSdetail = strAnswer;
           Endsl;
           Write pQuest;
        Enddo;
        Exec Sql
         Close c9;
        Close WIACQST;
       Endsr;
       //===================================================
       // Writes WIACSTRAT
       //===================================================
       Begsr WriteStateClassCodes;
        Open WIACSTRAT;
        SRxml = xmlTFile;
        SRco# = intCo#;
        SRapp# = intApp#;
        SRfyr = intFyr;
        SRdt = sysDate7;
        Clear intSeq#;
        Exec Sql
         Declare c5 Cursor For
         Select RSjrst, EPclas, EPext, EParat,
          Sum(EPecnt), Sum(EPeapr), Sum(EPempr)
         From WDELP
         Join WMRSP On ELco# = RSco# And ELrtcd = RSrtcd
         Join WMEPP On ELco# = EPco# And ELfnd = EPfnd
          And ELemp# = EPemp# And ELdiv = EPdiv
          And ELfyr = EPfyr And EPstat = 'A'
         Where ELco# = :intCo#
          And ELfnd  = :intFnd
          And ELemp# = :intEmp#
          And ELfyr  = :intFyr
          Group By RSjrst, EPclas, EPext, EParat
          Order By RSjrst, EPclas, EPext, EParat;
        Exec Sql
         Open c5;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c5
            Into :strState, :intClas, :strExt,
             :decRate, :intEmpCnt, :decPayAmt, :decManPrem;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
          // Find first location ID used for the state
           idx = %LookUp(strState:dsDivState(*).state);
          // Populate workfile
           SRlocId = dsDivState(idx).locID;
           SRst = strState;
           SRclas = intClas;
           SRext = strExt;
           SRdesc = GetPayrollDesc(intCo#:intEfDt:strState:
            strRtCd:intClas:strExt);
           SRftemp = intEmpCnt;
           // SRptemp not stored in CompSuite
           // SRsic not stored at payroll level
           // SRnaic not stored at payroll level
           SReapr = decPayAmt;
           SRrate = decRate;
           SRManPrm = decManPrem;
           intSeq# += 1;
           SRScrpSeq = intSeq#;
           Write pStClas;
        Enddo;
        Exec Sql
         Close c5;
        Close WIACSTRAT;
       Endsr;
       //===================================================
       // Writes WIACSTS
       //===================================================
       Begsr WriteStates;
        Open WIACSTS;
        STxml = xmlTFile;
        STco# = intCo#;
        STapp# = intApp#;
        STfyr = intFyr;
        STtype = '1';
        STdt = sysDate7;
        Exec Sql
         Declare c1 Cursor For
         Select Distinct RSjrst
         From WDELP Join WMRSP
         On ELco# = RSco#
          And ELrtcd = RSrtcd
         Where ELco# = :intCo#
          And ELfnd  = :intFnd
          And ELemp# = :intEmp#
          And ELfyr  = :intFyr
          Order By RSjrst;
        Exec Sql
         Open c1;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c1
            Into :STstate;
           If Not IsValidFetch(sqlStt);
              Leave;
           Else;
              Write pStList;
           Endif;
        Enddo;
        Exec Sql
         Close c1;
        Close WIACSTS;
       Endsr;
       //===================================================
       // Writes WIACSTPRM
       //===================================================
       Begsr WriteStatePremiums;
        Reset dsFct;
        Open WIACSTPRM;
        Exec Sql
         Declare c8 Cursor For
         Select RSjrst, ELmapr, ELilcd, (ELein1 + ELein2),
          ELdcde, (ELedct + ELedd2), ELexm1, (ELexp1 + ELexp2 + ELexp3),
           ELarp1, (ELrpe1 + ELrpe2 + ELrpe3), ELdbcr, ELedc, ELstpr,
           ELedc3, ELprtd, (ELtxdu + EL1aam + EL2aam + EL3aam + EL4aam +
           EL5aam + EL6aam + EL7aam), ELanpr, ELdisp
         From WDELP Join WMRSP
          On ELco# = RSco# And ELRtcd = RSrtcd
         Where ELco# = :intCo# And ELfnd = :intFnd And ELemp# = :intEmp#
          And ELfyr = :intFyr
          Order by RSjrst;
        Exec Sql
         Open c8;
        Dou Not IsValidFetch(sqlStt);
           Exec Sql
            Fetch Next
            From c8
            Into :strState, :decMaPr, :strILCd, :decIncLimAmt,
             :strDCde, :decDedAmt, :decExpModFct, :decExpModAmt,
             :decArapFct, :decArapAmt, :decDbCrFct, :decDbCrAmt, :decStdPrm,
             :decStockDisc, :decExpCon, :decFeeAmt, :decAnnPrm, :decPrmDscFct;
           If Not IsValidFetch(sqlStt);
              Leave;
           Endif;
           // Store Factors and Accumulate Premiums
           // Note: Assigned Risk and CCPAP aren't supported
           //       in CompSuite
           idx = %LookUp(strState:dsFct(*).state);
           If idx = 0;
              idx = %LookUp(*Blanks:dsFct(*).state);
           EndIf;

           dsFct(idx).State = strState;
           dsFct(idx).MaPr += decMaPr;

           dsFct(idx).ILCd   = strILCd;
           dsFct(idx).ILAmt += decIncLimAmt;

           dsFct(idx).DDcd   = strDCde;
           dsFct(idx).DDAmt += decDedAmt;

           dsFct(idx).ExpModFct  = decExpModFct;
           dsFct(idx).ExpModAmt += decExpModAmt;

           dsFct(idx).ArapFct  = decArapFct;
           dsFct(idx).ArapAmt += decArapAmt;

           dsFct(idx).DbCrFct   = decDbCrFct;
           dsFct(idx).DbCrAmt  += decDbCrAmt;

           dsFct(idx).StdPrm   += decStdPrm;
           dsFct(idx).PremDisc += decStockDisc;
           dsFct(idx).PrmDscFct = decPrmDscFct/100;
           dsFct(idx).ExpCon   += decExpCon;
           dsFct(idx).FeeAmt   += decFeeAmt;
           dsFct(idx).AnnPrm   += decAnnPrm;
        Enddo;
        //Write record for each state
        SPxml = xmlTFile;
        SPco# = intCo#;
        SPapp# = intApp#;
        SPfyr = intFyr;
        SPdt = sysDate7;
        For idx = 1 To 99;
           If dsFct(idx).State = *Blanks;
              Leave;
           EndIf;
           SPst = dsFct(idx).State;
           // Total 'Manual Premium'
           SPManPrm = dsFct(idx).MaPr;
           // Increased Limits
           Select;
           When dsFct(idx).ILAmt = 0;
              SPILFct = 0;
              SPILAmt = 0;
           Other;
              chr3 = dsFct(idx).IlCd;
              SPILFct = GetFactor('IL':intCo#:intFnd:SPst:
               chr3:intEfDt)/100;
              SPILAmt = dsFct(idx).ILAmt;
           EndSl;
           // Deductible Discount
           Select;
           When dsFct(idx).DDAmt = 0;
              SPDDFct = 0;
              SPDDAmt = 0;
           Other;
              If dsFct(idx).DDcd <> *Blanks;
                 SPDDFct = GetFactor('DD':intCo#:intFnd:SPst:
                  dsFct(idx).DDCd:intEfDt)/100;
              Else;
                 SPDDFct = dsWMLDP.LDpct1;
              Endif;
              SPDDAmt = dsFct(idx).DDAmt;
           Endsl;
           // Experience Mod
           SPExMFct = dsFct(idx).ExpModFct;
           SPExMAmt = dsFct(idx).ExpModAmt;
           // ARAP
           SPArpFct = dsFct(idx).ArapFct;
           SPArpAmt = dsFct(idx).ArapAmt;
           // Schedule Rating
           SPDCFct = dsFct(idx).DbCrFct;
           SPDCAmt = dsFct(idx).DbCrAmt;
           // Standard Premium
           SPStdPrm = dsFct(idx).StdPrm;
           // Premium Discount
           SPPrmDsc = dsFct(idx).PremDisc;
           SPPrmFct = dsFct(idx).PrmDscFct;
           // Expense Constant
           SPExpCon = dsFct(idx).ExpCon;
           // Taxes/Assessments
           SPFeeAmt = dsFct(idx).FeeAmt;
           // Total Estimated Annual Premium
           SPAnnPrm = dsFct(idx).AnnPrm;
           // Fast Quote doesn't populate Minimum and Deposit Premium
           Write pStPrem;
        EndFor;
        Exec Sql
         Close c8;
        Close WIACSTPRM;
       Endsr;
      /End-Free
     pPrintACORD       e
      //===================================================================
      // Policy Search
      //===================================================================
     pPolicySearch     b
     dPolicySearch     pi

     d dsWMEMP       e ds                  ExtName(wmempDS) Qualified inz
     d dsWMEMPsv     e ds                  ExtName(wmempDS) Qualified inz
     d dsWMEMPhold   e ds                  ExtName(wmempDS) Qualified inz
     d dsWTBSP       e ds                  ExtName(WTBSP) Qualified Inz
     d dsWTQSP       e ds                  ExtName(wtqsp) Qualified inz

     dmaxDiv           c                   999
     d filter          ds                  Qualified inz
     D***  Begin Chg  ***  12/09/21 ****************
     d**groupName                    40a
     d  groupName                    40a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d  group#                        3a
     D***  Begin Chg  ***  12/09/21 ****************
     d**agencyName                   40a
     d  agencyName                   40a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d  agencyFein                    9a
     d  agencyMod                     3a
     D***  Begin Chg  ***  12/09/21 ****************
     d**insuredName                  40a
     d  insuredName                  40a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d  insuredFein                   9a
     d  employer                      9a
     d  division                      5a   dim(maxDiv)
     d  divisionInt                   5  0 dim(maxDiv)
     d  polEff                             likeds(dsDateRange)
     d  polExp                             likeds(dsDateRange)
     d  fromPolEffDat                 8a
     d  toPolEffDate                  8a
     d  fromPolExpDat                 8a
     d  toPolExpDate                  8a
     D***  Begin Chg  ***  12/09/21 ****************
     d**policy#                      25a
     d**policyStatus                 10a
     d  policy#                      25a   varying
     d  policyStatus                 10a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d  quoteStatus                   1a   dim(100)
     d  renewWithinDy                 5a
     d  cancelledDaysFrom...
     d                                5a
     d  cancelledDaysTo...
     d                                5a
     D***  Begin Chg  ***  12/09/21 ****************
     d**lossRatioMore                10a
     d  lossRatioMore                10a   varying
     D***  End   Chg  ***  12/09/21 ****************
     d  selfRptFlag                   1a
     d  currentPeriodSR...
     d                                1a
     d  stateRptFlag                  1a
     d  summarize                     1a

     ddsPrem           ds                  qualified inz
     d written                       13  2
     d earned                        13  2
     ddsLoss           ds                  qualified inz
     d incurred                      13  2
     d ratio                          9  2
     ddsInfo           ds                  qualified inz
     d premium                             likeds(dsPrem)
     d loss                                likeds(dsLoss)
     ddsInfoSum        ds                  likeds(dsInfo) inz

     d group#          s              3  0
     d fein            s              9  0
     d mod             s              3  0
     d polGroup#       s              3  0 Inz(0)
     d polEmp#         s              9  0 Inz(0)
     d polYear         s              3  0 Inz(0)
     d division        s              5  0 Inz(0)
     d employer        s              9  0 Inz(0)
     d badData         s               n   Inz(*Off)
     d dateError       s               n   Inz(*Off)
     d moreRows        s               n   Inz(*On)
     d subset          s               n   Inz(*Off)
     d meetSS          s               n   Inz(*On)
     D policyNumber    s             15a
     D status          s             20a
     d writtenPrem     s             11s 2
     d earnedPrem      s             11s 2
     d incurredLoss    s             11s 2
     d lossRatio       s              9s 2
     d qteFlg          s              1a   inz(' ')
     d renewDays       s              5s 0
     d daysTilRenew    s              5s 0
     d daysSinceCancel...
     d                 s              5s 0
     d cancelledDaysFrom...
     d                 s              5s 0  inz(0)
     d cancelledDaysHold...
     d                 s              5s 0  inz(0)
     d cancelledDaysTo...
     d                 s              5s 0  inz(0)
     d lossRatioIn     s              9s 2
     d count           s              9s 0 Inz(0)
     doutDiv           s              5s 0 inz
     dstrQuoteSts      s            500a   varying inz
     dstrRptDiv        s              5    inz
     dstrSqlStRp       s             25    varying inz
     ddt7Cx            s              7  0 inz

     d jsonDiv         s               a   len(32000) varying inz
     D sqlStmtStr      s          32000a   varying inz
     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')

     d ##co#           s              3
     d ##fnd           s              3
     d ##fyr           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##date          s              8
     d ##date8         s              8  0
     d ##amount        s             11
     d ##amount2       s             11
     d ##term          s              1

      /free
       Exsr GetParameters;
       If errMsg = ' ';
          Exsr BldReturnInfo;
       Else;
          Exsr BldErrorsInfo;
       EndIf;

       outJson = json;

       //=================================================================
       // Build Policy Search
       //=================================================================
       Begsr BldPolicySearch;

          If filter.stateRptFlag = 'Y'and filter.summarize = 'Y';
             strSqlStRp = 'EMstRp <> ''Y'' or ';
          Endif;

          // Start Policy Listing
          json = %trim(json) + ' "policyList":[ ';

          // Build agency sql statement
          sqlStmtStr = 'with ' + SqlWithPolicyInfo +
             'select EMname, ELco#, ELfnd, ELfYr, ELemp#, ELdiv, ' +
             ' ELanPr, ELaaPr, ELstam, ELfaf, ELpfDt, ELptDt,' +
             ' EMstRp, ELbsCd ' +
             'from policy_info ' +
             ' join WMFNP on EMco#=FNco# and EMfnd=FNfnd';

          // Add on Filters
          sqlStmtStr += ' Where ';

          // Group filters;
          If filter.group# <> ' ';
             subset = *On;
             sqlStmtStr += ' EMfnd = ' + filter.group# + ' and ';
          Endif;
          If filter.groupName <> ' ';
             subset = *On;
             sqlStmtStr += ' Upper(FNname) like ''%'' || Upper(''' +
              %Trim(filter.groupName) + ''') || ''%''' + ' and ';
          EndIf;
          // Insured Filters
          If filter.insuredFein <> ' ';
             subset = *On;
             sqlStmtStr += ' digits(EMfnd)||digits(EMemp#) in ' +
              '(select digits(EMfnd)||digits(EMemp#) ' +
              '  from WMEMP where EMfein = ' + filter.insuredFein + ') and ';
             //sqlStmtStr += ' EMfein = ' + filter.insuredFein + ' and ';
          Endif;
          If filter.insuredName <> ' ';
             subset = *On;
             sqlStmtStr += ' Upper(EMname) like ''%'' || Upper(''' +
              %Trim(filter.insuredName) + ''') || ''%''' + ' and ';
          EndIf;
          // Employer Filters
          If filter.employer <> ' ';
             subset = *On;
             sqlStmtStr += ' EMemp# = ' + filter.employer + ' and ';
          EndIf;
          // Division Filters
          If filter.division(1) <> ' ';
             subset = *On;
             sqlStmtStr += ' EMdiv in (';
             For i1 = 1 to maxDiv;
                Select;
                When filter.division(i1) = *blank;
                   Iter;
                Endsl;
                sqlStmtStr += %trim(filter.division(i1)) + ',';
             Endfor;
             sqlStmtStr = %scanrpl(',':'':sqlStmtStr:%len(sqlStmtStr)-1);
             sqlStmtStr += ') and ';
          EndIf;
          // Effective Date Filters
          subset = *On;
          sqlStmtStr += ' ELpFdt between ' + filter.polEff.from.dt7Alpha +
           ' and ' + filter.polEff.to.dt7Alpha + ' and ';
          // Expiration Date Filters
          subset = *On;
          sqlStmtStr += ' ELpTdt between ' + filter.polExp.from.dt7Alpha +
           ' and ' + filter.polExp.to.dt7Alpha + ' and ';
          // Policy # Filters
          If filter.policy# <> ' ';
             subset = *On;
             sqlStmtStr += ' policy# = ''' + %trim(filter.policy#) + ''' and ';
          EndIf;
          // Policy Status
          Select;
          When Upper(filter.policyStatus) = 'ACTIVE';
             sqlStmtStr += %char(sysDate7) + ' between ELpfDt and ELptdt and ' +
              '(' + strSqlStRp +
               ' EMcaD1 = 0  or EMcaD1 >= ' + %char(sysDate7) + ') and ';
          When Upper(filter.policyStatus) = 'INACTIVE';
             sqlStmtStr += '((' + %char(sysDate7) + ' < ELpfDt or ' +
              'ELptdt < ' + %char(sysDate7) + ') or ' +
              '(' + strSqlStRp +
               ' (EMcaD1 <> 0 and EMcaD1 <= ' + %char(sysDate7) + '))) and ';
          Endsl;
          // Agency Filters
          If filter.agencyName <> ' ';
             subset = *on;
             sqlStmtStr += ' Upper(AGname) like ''%'' || Upper(''' +
              %trim(filter.agencyName) + ''') || ''%''' + ' and ';
          Endif;
          If filter.agencyFein <> ' ';
             subset = *on;
             sqlStmtStr += ' AGfein = ' + filter.agencyFein + ' and ';
          Endif;
          If filter.agencyMod <> ' ';
             subset = *on;
             sqlStmtStr += ' AGmod = ' + filter.agencyMod + ' and ';
          Endif;

          // Remove 'and' from last filter; may not exist
          sqlStmtStr = %scanrpl(' and ':'':sqlStmtStr:%len(sqlStmtStr)-4);
          // Remove 'where' if no filters exist
          sqlStmtStr = %scanrpl(' Where ':'':sqlStmtStr:%len(sqlStmtStr)-6);

          sqlStmtStr += ' order by ELCO#, ELFND, ELEMP#, ELFYR, ELDIV';

          Exec SQL close polCursor;

          // Prepare the SQL statement
          Exec SQL prepare polStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare polCursor cursor for polStmt;

          // Run SQL
          Exec SQL open polCursor;

          // Get first record if there is one
          Exec SQL fetch next from polCursor into :dsWMEMP;

          // Check to see if any records were fetched
          If Not IsValidFetch(sqlStt);
             moreRows = *off;
             Exec SQL close polCursor;
          EndIf;

          // If row was fetched then process
          DoU moreRows = *Off;

             dsWMEMPsv = dsWMEMP;
             // Build Record
             Exsr BldPolSearchRcd;

             // Fetch next Record
             Exec SQL fetch next from polCursor into :DSWMEMP;

             Select;
             // check to see if there was another record fetched.
             When Not IsValidFetch(sqlStt);
                moreRows = *Off;
                Exec SQL close polCursor;
             When dsWMEMP.ELco# <> dsWMEMPsv.ELco#
              or dsWMEMP.ELfnd <> dsWMEMPsv.ELfnd
              or dsWMEMP.ELfyr <> dsWMEMPsv.ELfyr
              or dsWMEMP.ELemp# <> dsWMEMPsv.ELemp#;
                Exsr ProcessStRptDiv;
                Reset strRptDiv;
             Endsl;
          EndDo;
          Exsr ProcessStRptDiv;

          // End the Policy Listing
          json = %trim(json) + ' ] ';

       EndSr;

       //=================================================================
       // Build the Policy Search Record
       //=================================================================
       Begsr BldPolSearchRcd;
        Select;
        When isEnvQuote and
         not UseGroupInPortal(dsWMEMP.ELco#:dsWMEMP.ELfnd:'Quote');
           Leavesr;
        When isEnvLive and
         not UseGroupInPortal(dsWMEMP.ELco#:dsWMEMP.ELfnd:'Policy');
           Leavesr;
        Endsl;

          policyNumber = %EditC(dsWMEMP.ELfnd:'X') +
                         %EditC(dsWMEMP.ELemp#:'X') +
                         %EditC(dsWMEMP.ELfYr:'X');

          status       = GetQuoteSts(dsWMEMP.ELco#:
                         dsWMEMP.ELfnd:dsWMEMP.ELemp#:
                         dsWMEMP.ELdiv:dsWMEMP.ELfYr:
                         qteFlg);

          // Complete filter
          meetSS = *On;
          If filter.quoteStatus(1) <> ' ';
             If %lookup(Upper(status):filter.quoteStatus) <= 1;
                meetSS = *Off;
             Endif;
          EndIf;

          // If self reporting flag and state reporting flag is passed
          // Then see if any of the associated divisons match self rpt flag
          If meetSS = *On;
             If filter.stateRptFlag <> ' ';
                If filter.selfRptFlag <> ' ';
                   If Not SelfRptDivMtch(dsWMEMP.ELco#:dsWMEMP.ELfnd:
                           dsWMEMP.ELemp#:dsWMEMP.ELfYr:filter.selfRptFlag);
                      meetSS = *Off;
                   EndIf;
                EndIf;
             Else;
                If filter.selfRptFlag <> ' ';
                   // Get SR Online Flag
                   Exec Sql
                      Select *
                      Into :dsWTBSP
                      From WTBSP
                      Where BSco# = :dsWMEMP.ELco#
                      And BSkey = Digits(:dsWMEMP.ELbsCd);
                   If dsWTBSP.BSonli <> filter.selfRptFlag;
                      meetSS = *Off;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;

          // Check if the policy is effetive for the current S/R period
          If meetSS = *on;
             If filter.currentPeriodSR <> ' ';
                meetSS = SelfRptCurrent(dsWMEMP.ELco#:dsWMEMP.ELfnd:
                 dsWMEMP.ELemp#:dsWMEMP.ELdiv:dsWMEMP.ELfYr);
             EndIf;
          EndIf;

          // Calculate the number of days til renewal
          If meetSS = *on and filter.renewWithinDy <> ' ';
             daysTilRenew = dtDiff(DateTo8(systemDate()):
                  dtMDYY(dsWMEMP.ELpTdt));
             Select;
             When sysDate7 > dsWMEMP.ELptDt;
                meetSS = *off;
             When renewDays <= daysTilRenew;
                meetSS = *off;
             Endsl;
          EndIf;

          // Calculate the number of days from/to cancellation
          If meetSS = *on and
           (filter.cancelledDaysFrom <> ' ' or filter.cancelledDaysTo <> ' ');
             dt7Cx = GetCxDate(dsWMEMP.ELco#:dsWMEMP.ELfnd:dsWMEMP.ELemp#
              :dsWMEMP.ELdiv);
             daysSinceCancel = DtDiff(DtMDYY(dt7Cx):DateTo8(systemDate()));
             Select;
             // Not cancelled
             When dt7Cx = *zero;
                meetSS = *off;
             // Cancelled outside of year period
             When dt7Cx < dsWMEMP.ELpfDt or dt7Cx > dsWMEMP.ELptDt;
                meetSS = *off;
             // Cancelled outside of range
             When cancelledDaysFrom > daysSinceCancel or
              daysSinceCancel > cancelledDaysTo;
                meetSS = *off;
             Endsl;
          EndIf;

          Select;
          When filter.summarize <> 'Y';
             Reset dsInfoSum;
          Endsl;

          // If additional selection criteria not met then exit
          If meetSS = *Off;
             LeaveSR;
          EndIf;

          // Get the written premium
          If dsWMEMP.ELstam = 0;
             If dsWMEMP.ELfaf = 'A';
                dsInfoSum.premium.written += dsWMEMP.ELaaPr;
             Else;
                dsInfoSum.premium.written += dsWMEMP.ELanPr;
             EndIf;
          Else;
             dsInfoSum.premium.written += dsWMEMP.ELstam;
          EndIf;

          // Get the earned premium
          ##co#  = %editc(dsWMEMP.ELco#:'X');
          ##fnd  = %editc(dsWMEMP.ELfnd:'X');
          ##fyr  = %editc(dsWMEMP.ELfyr:'X');
          ##emp# = %editc(dsWMEMP.ELemp#:'X');
          ##div  = %editc(dsWMEMP.ELdiv:'X');
          ##date8 = DateTo8(SystemDate);
          ##date  = %editc(##Date8:'X');
          ##amount = ' ';
          ##amount2 = ' ';
          ##term = '1';

          SrPrt1(##co#:##fnd:##fyr:##emp#:##div:##date:##amount:
           ##amount2:##term);

          Monitor;
             dsInfoSum.premium.earned += %Dec(##amount:11:2)/100;
          On-Error;
             dsInfoSum.premium.earned += 0;
          EndMon;

          // Get the total incurred
          dsInfoSum.loss.incurred += GetIncLoss(dsWMEMP.ELco#:
                                    dsWMEMP.ELfnd:
                                    dsWMEMP.ELemp#:
                                    dsWMEMP.ELdiv:
                                    dsWMEMP.ELfYr);

          // Calculate loss ratio
          If dsInfoSum.premium.earned <> 0;
             Monitor;
                dsInfoSum.loss.ratio =
                %dech(dsInfoSum.loss.incurred/dsInfoSum.premium.earned:5:4)*100;
             On-Error;
                dsInfoSum.loss.ratio = 9999999.99;
             EndMon;
          Else;
             dsInfoSum.loss.ratio = 0;
          EndIf;

          // Loss Ratio greater than
          If meetSS = *on  and filter.lossRatioMore <> ' ';
             lossRatioIn = %Dec(%Trim(filter.lossRatioMore):5:2);
             If dsInfoSum.loss.ratio < lossRatioIn;
                meetSS = *off;
             Endif;
          EndIf;

          Select;
          When filter.stateRptFlag = 'Y' and dsWMEMP.EMstRp = 'Y'
           and filter.summarize = 'Y';
             strRptDiv = %editc(dsWMEMP.ELdiv:'X');
             meetSS = *off;
          When filter.stateRptFlag = 'Y' and dsWMEMP.EMstRp <> 'Y';
             meetSS = *off;
          Endsl;

         If meetSS = *off;
            Leavesr;
         Endif;

         dsInfo.premium.written = dsInfoSum.premium.written;
         dsInfo.premium.earned = dsInfoSum.premium.earned;
         dsInfo.loss.incurred = dsInfoSum.loss.incurred;
         dsInfo.loss.ratio = dsInfoSum.loss.ratio;

         outDiv = dsWMEMP.ELdiv;
         Exsr BldOutput;

       Endsr;

       //=================================================================
       // Build Error Message Response
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json += ProcessErrorMessagesJson('*PRE');
          json = %scanrpl(',':'':json:1:1);

       Endsr;

       //=================================================================
       // Build Output json
       //=================================================================
       Begsr BldOutput;

         status = GetQuoteSts(dsWMEMP.ELco#:
          dsWMEMP.ELfnd:dsWMEMP.ELemp#:outDiv:
          dsWMEMP.ELfYr:qteFlg);
         Exec sql
          select * into :dsWTQSP
            from WTQSP
           where QSco# = :dsWMEMP.ELco# and QSkey = :status;
          If dsWTQSP.QSused <> *blank;
            Exec sql
             select * into :dsWTQSP
              from WTQSP
              where QSco# = :dsWMEMP.ELco# and QSkey = :dsWTQSP.QSused;
         Endif;

         If count > 0;
            json = %Trim(json) + ',';
         EndIf;
         count = count + 1;

         json = %Trim(json) + '{' +
          BuildJsonNode('company':%char(dsWMEMP.ELco#)) +
          ',' +
          BuildJsonNode('group#':%EditC(dsWMEMP.ELfnd:'X')) +
          ',' +
          BuildJsonNode('employer#':%EditC(dsWMEMP.ELemp#:'X')) +
          ',' +
          BuildJsonNode('division':%EditC(outDiv:'X')) +
          ',' +
          PolicyJSONData(dsWMEMP.ELco#:dsWMEMP.ELfnd
           :dsWMEMP.ELemp#:outDiv:dsWMEMP.ELfyr) +
          ',' +
          CxJSONData(dsWMEMP.ELco#:dsWMEMP.ELfnd:dsWMEMP.ELemp#:outDiv) +
          ',' +
          BuildJsonCode(status:dsWTQSP.QSdesc:'quoteStatus') +
          ',' + '"premium":{' +
           BuildJsonNode('written':%EditC(dsInfo.premium.written:'3')) +
           ',' +
           BuildJsonNode('earned':%EditC(dsInfo.premium.earned:'3')) +
           '},' +
          BuildJsonNode('totalIncurred':%EditC(dsInfo.loss.incurred:'3')) +
          ',' +
          BuildJsonNode('lossRatio':%EditC(dsInfo.loss.ratio:'3')) + '}';
         Reset dsInfoSum;
       Endsr;

       //=================================================================
       // Build Request Information
       //=================================================================
       Begsr BldReqInfo;

          json = '"requestParms" : {' +
           '"group":{' +
            BuildJsonNode('name':filter.groupName) + ', ' +
            BuildJsonNode('number':filter.group#)  + '}, ' +
           '"agency":{' +
            BuildJsonNode('name':filter.agencyName) + ', ' +
            BuildJsonNode('fein':filter.agencyFein) + ', ' +
            BuildJsonNode('agencyMod':filter.agencyMod) + '}, ' +
           '"insured":{' +
            BuildJsonNode('name':filter.insuredName) + ', ' +
            BuildJsonNode('fein':filter.insuredFein) + ', ' +
            BuildJsonNode('employer':filter.employer) + ', ' +
            '"division":[' + jsonDiv + ']}, ' +
           '"policyDates":{' +
            '"effective":{' +
             BuildJsonNode('from':filter.polEff.from.dt8Alpha) + ', ' +
             BuildJsonNode('to':filter.polEff.to.dt8Alpha) + '}, ' +
            '"expiration":{' +
             BuildJsonNode('from':filter.polExp.from.dt8Alpha) + ', ' +
             BuildJsonNode('to':filter.polExp.to.dt8Alpha) + '}}, ' +
           '"policy":{' +
            BuildJsonNode('number':filter.policy#) + ', ' +
            BuildJsonNode('status':filter.policyStatus) + ', ' +
            '"quotStatus":[' + strQuoteSts + ']'  + '}, ' +
           BuildJsonNode('renewalWithinDays':filter.renewWithinDy) + ', ' +
           BuildJsonNode('lossRatioMore':filter.lossRatioMore) + ', ' +
           '"stateReporting":{' +
            BuildJsonNode('only':filter.selfRptFlag) + ', ' +
            BuildJsonNode('summarizeAll':filter.summarize) + '},' +
           '"selfReporting":{' +
            BuildJsonNode('only':filter.selfRptFlag) + ', ' +
            BuildJsonNode('currentPeriod':filter.currentPeriodSR) + '} ' +
          '} , ';

       Endsr;

       //=================================================================
       // Build Return Information
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Darts Information
          ExSr BldPolicySearch;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 <> ' ';
             errMsg = %Trim(errMsg500);
             AddErrorMessageJson('JSON filter can''t be read. ' + errMsg
              :ioRtnCode);
             Leavesr;
          Endif;

          // Load the data structure with the parameter values from the tree
          Reset i0;
          Dow YAJL_object_loop(docNode:i0:key:val);
             Select;
             When key = 'group';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'name';
                      filter.groupName = YAJL_get_string(val);
                   When key = 'number';
                       filter.group# = YAJL_get_string(val);
                   Endsl;
                Enddo;
             When key = 'agency';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'name';
                      filter.agencyName  = YAJL_get_string(val);
                   When key = 'fein';
                      filter.agencyFein = YAJL_get_string(val);
                   When key = 'mod';
                      filter.agencyMod  = YAJL_get_string(val);
                   Endsl;
                Enddo;
             When key = 'insured';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'name';
                      filter.insuredName = YAJL_get_string(val);
                   When key = 'fein';
                      filter.insuredFein = YAJL_get_string(val);
                   When key = 'employer';
                      filter.employer = YAJL_get_string(val);
                   When key = 'division';
                      jsonDiv = ' ';
                      node2 = YAJL_object_find(node1:key);
                      i2 = 0;
                      Dow YAJL_array_loop(node2:i2:val);
                         filter.division(i2) = YAJL_get_string(val);
                         jsonDiv += '"' + %trim(filter.division(i2)) + '",';
                      Enddo;
                      If %len(jsonDiv) >= 2;
                         jsonDiv = %scanrpl(',':'':jsonDiv:%len(jsonDiv)-1);
                      Endif;
                   Endsl;
                Enddo;
             When key = 'policyDates';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'effective';
                      node2 = YAJL_object_find(node1:key);
                      Reset i2;
                      Dow YAJL_object_loop(node2:i2:key:val);
                         Select;
                         When key = 'from';
                            filter.polEff.from.dt8Alpha = YAJL_get_string(val);
                         When key = 'to';
                            filter.polEff.to.dt8Alpha = YAJL_get_string(val);
                         Endsl;
                      Enddo;
                   When key = 'expiration';
                      node2 = YAJL_object_find(node1:key);
                      Reset i2;
                      Dow YAJL_object_loop(node2:i2:key:val);
                         Select;
                         When key = 'from';
                            filter.polExp.from.dt8Alpha = YAJL_get_string(val);
                         When key = 'to';
                            filter.polExp.to.dt8Alpha = YAJL_get_string(val);
                         Endsl;
                      Enddo;
                   Endsl;
                Enddo;
             When key = 'policy';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'number';
                      filter.policy# = YAJL_get_string(val);
                   When key = 'status';
                      filter.policyStatus  = YAJL_get_string(val);
                   When key = 'quoteStatus';
                      node2 = YAJL_object_find(node1:key);
                      i2 = 0;
                      Dow YAJL_array_loop(node2:i2:val);
                         filter.quoteStatus(i2) = YAJL_get_string(val);
                         If i2 > 1;
                            strQuoteSts += ',';
                         Endif;
                         strQuoteSts += '"' + filter.quoteStatus(i2) + '"';
                      Enddo;
                   Endsl;
                Enddo;
             When key = 'renewalWithinDays';
                filter.renewWithinDy = YAJL_get_string(val);
             When key = 'cancelledDays';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'from';
                      filter.cancelledDaysFrom = YAJL_get_string(val);
                   When key = 'to';
                      filter.cancelledDaysTo = YAJL_get_string(val);
                   Endsl;
                Enddo;
             When key = 'lossRatioMore';
                filter.lossRatioMore = YAJL_get_string(val);
             When key = 'stateReporting';
                filter.stateRptFlag = YAJL_get_string(val);
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'only';
                      filter.stateRptFlag = YAJL_get_string(val);
                   When key = 'summarizeAll';
                      filter.summarize = YAJL_get_string(val);
                   Endsl;
                Enddo;
             When key = 'selfReporting';
                node1 = YAJL_object_find(docNode:key);
                Reset i1;
                Dow YAJL_object_loop(node1:i1:key:val);
                   Select;
                   When key = 'only';
                      filter.selfRptFlag = YAJL_get_string(val);
                   When key = 'currentPeriod';
                      filter.currentPeriodSR = YAJL_get_string(val);
                   Endsl;
                Enddo;
             Endsl;
          Enddo;

          // Free up the space by removing the tree
          yajl_tree_free(docNode);

          //======= Validate the incoming parameters ====================

          //------------------------------------------------------
          // Group # must be valid numeric value <<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          badData = *Off;
          group#  = 0;
          If filter.group# <> ' ';
             Monitor;
                group# = %Dec(%Trim(filter.group#):3:0);
             On-Error;
                AddErrorMessageJson('Group#: Invalid numeric data.'
                 :ioRtnCode);
                badData = *On;
             EndMon;
             // If the group is valid numeric data then see if it exists
             If badData = *Off And ValidGrp(group#) = *Off;
                AddErrorMessageJson('Invalid group#.'
                 :ioRtnCode);
             EndIf;
          EndIf;
          //------------------------------------------------------
          // Group must be selected for Portal
          //------------------------------------------------------
          If group# <> *zero;
             Select;
             When isEnvQuote and
              not UseGroupInPortal(intCo#:group#:'Quote');
                AddErrorMessageJson('Group ' + %char(group#) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtnCode);
             When isEnvLive and
              not UseGroupInPortal(intCo#:group#:'Policy');
                AddErrorMessageJson('Group ' + %char(group#) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtnCode);
             Endsl;
          EndIf;

          //------------------------------------------------------
          // Agency Fein and mod must be valid numeric value <<<<<
          //------------------------------------------------------
          badData = *Off;
          fein    = 0;
          mod     = 0;
          If filter.agencyFein <> ' ';
             Monitor;
                fein = %Dec(%Trim(filter.agencyFein):9:0);
             On-Error;
                AddErrorMessageJson('Agency Fein: Invalid numeric data.'
                 :ioRtnCode);
                badData = *On;
             EndMon;
          EndIf;

          // Modifier
          If filter.agencyMod <> ' ';
             Monitor;
                mod = %Dec(%Trim(filter.agencyMod):3:0);
             On-Error;
                AddErrorMessageJson('Agency Mod: Invalid numeric data.'
                 :ioRtnCode);
                badData = *On;
             EndMon;
          Else;
             mod = 0;
          EndIf;

          //------------------------------------------------------
          // Validate the agency <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          If badData = *Off And filter.agencyFein <> ' ' And
           ValidFein(fein:mod) = *Off;
             AddErrorMessageJson('Invalid agency Fein/Mod.'
              :ioRtnCode);
          EndIf;

          //------------------------------------------------------
          // Insured Fein must be valid numeric value <<<<<<<<<<<<
          //------------------------------------------------------
          badData = *Off;
          fein    = 0;
          mod     = 0;
          If filter.insuredFein <> ' ';
             Monitor;
                fein = %Dec(%Trim(filter.insuredFein):9:0);
             On-Error;
                AddErrorMessageJson('Insured: Invalid numeric data.'
                 :ioRtnCode);
                badData = *On;
             EndMon;
          EndIf;

          //------------------------------------------------------
          // Employer must be valid numeric value <<<<<<<<<<<<<<<<
          //------------------------------------------------------
          badData  = *Off;
          Reset employer;
          If filter.employer <> ' ';
             Monitor;
                employer = %Dec(%Trim(filter.employer):9:0);
             On-Error;
                AddErrorMessageJson('Employer: Invalid numeric data.'
                 :ioRtnCode);
                badData = *On;
             EndMon;
          EndIf;

          //------------------------------------------------------
          // Division must be valid numeric value <<<<<<<<<<<<<<<<
          //------------------------------------------------------
          badData  = *Off;
          If filter.division(1)<> ' ';
             For i1 = 1 to maxDiv;
                Select;
                When filter.division(i1) = *blanks;
                   Iter;
                When badData;
                   Iter;
                Endsl;
                Monitor;
                   filter.divisionInt(i1) = %int(filter.division(i1));
                On-Error;
                   AddErrorMessageJson('Division: Invalid numeric data.'
                    :ioRtnCode);
                   badData = *On;
                EndMon;
             Endfor;
          EndIf;

          //------------------------------------------------------
          // From Effective date must be valid <<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.polEff.from.dt8Alpha = ' '
           Or filter.polEff.from.dt8Alpha = '00000000';
             filter.polEff.from.dt8 = 0;
             filter.polEff.from.dt7 = 0;
             filter.polEff.from.dt7Alpha = '0000000';
          Else;
             Monitor;
                filter.polEff.from.dt8 = %Dec(filter.polEff.from.dt8Alpha:8:0);
             On-Error;
                AddErrorMessageJson('From Effective Date: Invalid numeric data.'
                 :ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And filter.polEff.from.dt8 <> 0;
                filter.polEff.from.dt7 = dtCYMD(filter.polEff.from.dt8);
                filter.polEff.from.dt7Alpha =
                 %EditC(filter.polEff.from.dt7:'X');
                If filter.polEff.from.dt7 = 0;
                   AddErrorMessageJson('From Effective Date: Invalid Date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          //------------------------------------------------------
          // To Effective date must be valid <<<<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.polEff.to.dt8Alpha = ' '
           Or filter.polEff.to.dt8Alpha = '00000000';
             filter.polEff.to.dt8 = 99999999;
             filter.polEff.to.dt7 = 9999999;
             filter.polEff.to.dt7Alpha = '9999999';
          Else;
             Monitor;
                filter.polEff.to.dt8 = %Dec(filter.polEff.to.dt8Alpha:8:0);
             On-Error;
                AddErrorMessageJson('To Effective Date: Invalid numeric data.'
                 :ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And filter.polEff.to.dt8 <> 0;
                filter.polEff.to.dt7 = dtCYMD(filter.polEff.to.dt8);
                filter.polEff.to.dt7Alpha  = %EditC(filter.polEff.to.dt7:'X');
                If filter.polEff.to.dt7 = 0;
                   AddErrorMessageJson('To Effective Date: Invalid Date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          //------------------------------------------------------
          // From Expiration date must be valid <<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.polExp.from.dt8Alpha = ' '
           Or filter.polExp.from.dt8Alpha = '00000000';
             filter.polExp.from.dt8 = 0;
             filter.polExp.from.dt7 = 0;
             filter.polExp.from.dt7Alpha = '0000000';
          Else;
             Monitor;
                filter.polExp.from.dt8 = %Dec(filter.polExp.from.dt8Alpha:8:0);
             On-Error;
                AddErrorMessageJson('From Expiration Date: ' +
                 'Invalid numeric data.':ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And filter.polExp.from.dt8 <> 0;
                filter.polExp.from.dt7 = dtCYMD(filter.polExp.from.dt8);
                filter.polExp.from.dt7Alpha =
                 %EditC(filter.polExp.from.dt7:'X');
                If filter.polExp.from.dt7 = 0;
                   AddErrorMessageJson('From Expiration Date: Invalid Date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          //------------------------------------------------------
          // To Expiration date must be valid <<<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.polExp.to.dt8Alpha = ' '
            Or filter.polExp.to.dt8Alpha = '00000000';
             filter.polExp.to.dt8 = 99999999;
             filter.polExp.to.dt7 = 9999999;
             filter.polExp.to.dt7Alpha = '9999999';
          Else;
             Monitor;
                filter.polExp.to.dt8 = %Dec(filter.polExp.to.dt8Alpha:8:0);
             On-Error;
                AddErrorMessageJson('To Expiration Date: ' +
                 'Invalid numeric data.':ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And filter.polExp.to.dt8 <> 0;
                filter.polExp.to.dt7 = dtCYMD(filter.polExp.to.dt8);
                filter.polExp.to.dt7Alpha  = %EditC(filter.polExp.to.dt7:'X');
                If filter.polExp.to.dt7 = 0;
                   AddErrorMessageJson('To Expiration Date: Invalid Date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          // Policy # must be extracted
          If filter.policy# <> ' ';
             Monitor;
              //polGroup# = %Dec(%Subst(filter.policy#:1:3):3:0);
              //polEmp#   = %Dec(%Subst(filter.policy#:4:9):9:0);
              //polYear   = %Dec(%Subst(filter.policy#:13:3):3:0);
             On-Error;
                AddErrorMessageJson('Policy #: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

          // Renewal Days must be extracted
          If filter.renewWithinDy <> ' ';
             Monitor;
                renewDays = %Dec(%Trim(filter.renewWithinDy):5:0);
             On-Error;
                AddErrorMessageJson('Renewal Days: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

          // Cancelled Days must be extracted
          If filter.cancelledDaysFrom <> ' ';
             Monitor;
                cancelledDaysFrom =
                 %dec(%trim(filter.cancelledDaysFrom):5:0);
             On-Error;
                AddErrorMessageJson('Cancelled Days (From): ' +
                 'Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;
          If filter.cancelledDaysTo <> ' ';
             Monitor;
                cancelledDaysTo =
                 %dec(%trim(filter.cancelledDaysTo):5:0);
             On-Error;
                AddErrorMessageJson('Cancelled Days (To): ' +
                 'Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;
          // Put from/to values in sequencial order
          If cancelledDaysFrom > cancelledDaysTo;
             cancelledDaysHold = cancelledDaysFrom;
             cancelledDaysFrom = cancelledDaysTo;
             cancelledDaysTo = cancelledDaysHold;
          Endif;

          // Loss Ratio greater than
          If filter.lossRatioMore <> ' ';
             Monitor;
                lossRatioIn = %Dec(%Trim(filter.lossRatioMore):5:2);
             On-Error;
                AddErrorMessageJson('Loss Ratio > Than: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

       Endsr;

       //=================================================================
       // Process the state reporting divsion
       //=================================================================
       Begsr ProcessStRptDiv;
        If strRptDiv = *blanks;
           Leavesr;
        Endif;
        dsWMEMPhold = dsWMEMP;
        dsWMEMP = dsWMEMPsv;
        dsInfo.premium.written = dsInfoSum.premium.written;
        dsInfo.premium.earned = dsInfoSum.premium.earned;
        dsInfo.loss.incurred = dsInfoSum.loss.incurred;
        dsInfo.loss.ratio = dsInfoSum.loss.ratio;
        outDiv = %dec(strRptDiv:5:0);
        Exsr BldOutput;
        dsWMEMP = dsWMEMPhold;
       Endsr;

      /End-free

     pPolicySearch     e
      //===================================================
      // Populate zip code properly if 5 or 9 digits is
      //  passed in
      //===================================================
     pPopZipCode       b
     dPopZipCode       pi             9  0
     d inZipCode                      9  0 Const
      /Free
       If inZipCode < 100000;
          Return inZipCode * 10000;
       Else;
          Return inZipCode;
       Endif;
      /End-Free
     pPopZipCode       e
      //===================================================
      // Saves application data
      //===================================================
     p SaveAppData     b
      *Indentation levels
     d i0              s             10i 0 Inz
     d i1              s             10i 0 Inz
     d i2              s             10i 0 Inz
     d i3              s             10i 0 Inz
     d i4              s             10i 0 Inz
     d i5              s             10i 0 Inz
     d i6              s             10i 0 Inz
     d i7              s             10i 0 Inz
     d i8              s             10i 0 Inz
     d i9              s             10i 0 Inz
     d i10             s             10i 0 Inz
     d i11             s             10i 0 Inz
     d i12             s             10i 0 Inz
     d i13             s             10i 0 Inz
     d ig1             s             10i 0 Inz
     d ig2             s             10i 0 Inz
     d node0           s                   Like(Yajl_Val)
     d node1           s                   Like(Yajl_Val)
     d node2           s                   Like(Yajl_Val)
     d node3           s                   Like(Yajl_Val)
     d node4           s                   Like(Yajl_Val)
     d node5           s                   Like(Yajl_Val)
     d node6           s                   Like(Yajl_Val)
     d node7           s                   Like(Yajl_Val)
     d node8           s                   Like(Yajl_Val)
     d node9           s                   Like(Yajl_Val)
     d node10          s                   Like(Yajl_Val)
     d node11          s                   Like(Yajl_Val)
     d node12          s                   Like(Yajl_Val)
     d node13          s                   Like(Yajl_Val)
      * Stand Alones
     d maxSize         c                   99
     d maxSizeQuest    c                   999
     d*** Begin Add *** 11/15/21 *******************************************
     d aaCount         s              5  0
     d*** End   Add *** 11/15/21 *******************************************
     d aryAgt          s              5  0 Inz Dim(maxSize)
     d aryAgtCxDt      s              7  0 Inz Dim(maxSize)
     d aryAgtEfDt      s              7  0 Inz Dim(maxSize)
     d aryAgtGrp       s              3  0 Inz Dim(maxSize)
     d aryAgtRate      s              3  0 Inz Dim(maxSize)
     d aryAgtUwtr      s              3    Inz Dim(maxSize)
     d aryDiv          s              5  0 Inz(*Hival) Dim(maxSize)
     d aryLoc          s             10  0 Inz(*Hival) Dim(maxSize)
     d aryOO#          s              5  0 Inz(*Hival) Dim(maxSize)
     d aryRtCd         s              3    Inz Dim(maxSize)
     d arySt           s              2    Inz Dim(maxSize)
     d aryWaivCnt      s              5  0 Inz Dim(maxSize)
     d chr1a           s              1a
     d chrIncExcFlag   s              1a
     d chrManAutoFlag  s              1a
     d chrPrintFlag    s              1a
     d chrPrmLvl       s              1a
     d chrRtCd         s              3a   Inz
     d chrSt           s              2a   Inz
     d decCo#          s              3  0 inz
     d decCountWMALP   s             10i 0 inz
     d decDt7          s              7  0
     d decEffDt        s              8  0 Inz
     d decExpDt        s              8  0 Inz
     d decELID         s             10  0 inz(2)
     d decId#          s              7  0 inz
     d decMaxAmount    s             11  2 Inz
     d decPercent      s             15  5 inz
     d decRRN          s              9  0 Inz
     d decYear         s              3  0
     d dt7Eff          s              7  0 inz
     d ia10            s             10i 0 Inz
     d idx             s              5  0 Inz
     d idy             s              5  0 Inz
     d idz             s              5  0 Inz
     d indAppSaved     s               n
     d indAppSaved2    s               n
     d*** Begin Add *** 11/15/21 *******************************************
     d indDefaultChecked...
     d                 s               n
     d indExclCompRatings...
     d                 s               n
     d indExclCompRatingsDefault...
     d                 s               n
     d*** End   Add *** 11/15/21 *******************************************
     d indPolError     s               n
     d*** Begin Add *** 11/15/21 *******************************************
     d indSkipGroup...
     d                 s               n   Inz(*Off)
     d*** End   Add *** 11/15/21 *******************************************
     d indStRptFlg     s               n
     d intAgt          s              5  0 Inz
     d intAgtCxDt      s              7  0 Inz
     d intAgtEfDt      s              7  0 Inz
     d intAgtGrp       s              3  0
     d intAgtUwtr      s              3    Inz
     d intApp#         s              9  0 Inz
     d intAreaCode     s              3  0 inz
     d intConIdx       s              3  0 Inz
     d intCount        s              5  0 Inz
     d intDfltRegion   s              3  0 Inz
     d intDiv          s              5  0 Inz
     d intDivIdx       s              3  0 Inz
     d intDtlIdx       s              3  0 Inz
     d intElid         s             10  0 Inz
     d intEmp#         s              9  0 Inz
     d*** Begin Add *** 11/15/21 *******************************************
     d intFein         s              9  0
     d*** End   Add *** 11/15/21 *******************************************
     d intFnd          s              3  0 Inz
     d intFyr          s              3  0 Inz
     d intGrpIdx       s              3  0 Inz(1)
     d intLocIdx       s              3  0 Inz
     d*** Begin Add *** 11/15/21 *******************************************
     d intMod          s              3  0
     d*** End   Add *** 11/15/21 *******************************************
     d intOffIdx       s              3  0 Inz
     d intPayIdx       s              3  0 Inz
     d intPhone#       s              7  0 inz
     d intProducer     s              5  0 Inz
     d intRate         s              3  0 Inz
     d intSeq#         s              5  0 Inz
     d intSpecWvCnt    s              5  0 Inz
     d intZip          s              5  0 Inz
     d isAddAsClosed   s               n   inz
     d isAddingApp     s               n   Inz
     d isTagged        s               n   Inz
     d isEofWMAAP      s              1n   Inz('0')
       //***  Begin Add  ***  07/11/22  *****************
     d isLocationExists...
     d                 s              1n   inz
       //***  Begin Add  ***  07/11/22  *****************
     d isSavingApp     s               n
     d isUpdate        s              1n
     d noMoreGroups    s               n   Inz(*Off)
     d selectedGroup#  s              3  0 Inz
     d strAgtFein      s              9a
     d strAgtMod       s              3a
     d strDfltOffice   s              5    Inz
     d strApp#         s              9a
     d***  Begin Add  ***  08/05/21  *************************
     d strCategory     s            256    inz varying
     d***  End   Add  ***  08/05/21  *************************
     d strCo#          s              3a
     d strCode         s             10a
     d strCodeSt       s              3a
     d strConAddress1  s             40a
     d strConAddress2  s             40a
     d strConCity      s             25a
     d strConSt        s              2a
     d intConZip       s              9  0
     d str2a           s              2a
     d strDedCode      s              3a
     d strDiv          s              5a
     d strElid         s             10a
     d strEmp#         s              9a
     d strEntry        s             25a   Inz Varying
     d strFsn          s             15a
     d strMid          s              1a
     d strLsn          s             30a
     d strPosn         s             40a
     d strPCnt         s              1a
     d strMinimum      s             25a   Inz Varying
     d strMaximum      s             25a   Inz Varying
     d strType         s             25a   Inz Varying
     d strQuestion     s            500a   inz varying
     d strTypeOfEntry  s             25a   Inz Varying
     d strWaivType     s             25a   Inz Varying
     d strFnd          s              3a
     d strFndName      s             60a   inz varying
     d strJson         s            350    Inz Varying
     d strProducer     s              5a
     d strRateKey      s             10a   Inz Varying
     d strRtCd         s              3    Inz
     d strSeq#         s              5    Inz
     d strSqlWMAAP     s            500
     d strSt           s              2a   Inz
     d strTest         s            100a   varying
     d strWaiverForm   s             15a
     d strWaiverDesc   s            100a
     d***  Begin Add  ***  08/05/21  *************************
     d strYearScoring  s              3  0 inz
     d***  End   Add  ***  08/05/21  *************************
     d str100a         s            100a
     d useDftBillPlan  s               n   Dim(maxSize)
     d useDftRateCode  s               n   Dim(maxSize)
     d wrkCo#          s              3  0 Inz
     d wrkEfd1         s              7  0 Inz
     d wrkFnd          s              3  0 Inz
     d wrkRtCd         s              3a   Inz
     d wrkStPo         s              7  0 Inz
     d*** Begin Add *** 09/04/24 *******************************************
     d strEMpSt        s                   Like(EMpSt)
     d strEMpCty       s                   Like(EMpCty)
     d strEMmSt        s                   Like(EMmSt)
     d strEMmCty       s                   Like(EMmCty)
     d strDfltLossRep  s                   Like(EMlRep)
     d*** End   Add *** 09/04/24 *******************************************
     ddsWMFNP        e ds                  extname(WMFNP) qualified inz
      * Quote Tracking
     ddsWAQTP        e ds                  Extname(WAQTP) Dim(maxSize) Qualified
     d                                     Inz
     ddsWAQTPOut     e ds                  Extname(WAQTP) Inz Qualified
      * Policy Detail
     ddsWDELP          ds                  likeds(dsWDELPdef) dim(maxSize)
     d                                     Inz
     ddsWDELPout     e ds                  Extname(WDELP) inz qualified
      * Application Master
     d dsWIAPP       e ds                  ExtName(WIAPP) Qualified Inz
      * Application Answers
     d dsWIAPPANS    e ds                  ExtName(WIAPPANS) Qualified Inz
     d                                     Dim(maxSizeQuest)
     d required                       1a
     d dsWIAPPAnsOut e ds                  ExtName(WIAPPANS) Qualified Inz
      * Application Loss Detail
     d dsWIAPPLOS    e ds                  ExtName(WIAPPLOS) Qualified Inz
     d                                     Dim(10)
     d dsWIAPPLOSout e ds                  ExtName(WIAPPLOS) Qualified Inz
      * Gradient Information
     d dsWIGSPDRV    e ds                  extname(WIGSPDRV) qualified inz
     d dsWIGSP       e ds                  extname(WIGSP) qualified inz
     d dsWIGSPpre    e ds                  extname(WIGSP) qualified inz
     d dsGradient      ds                  qualified inz dim(999)
     d  WIGSP                              likeds(dsWIGSP)
     d  WIGSPDRV                           likeds(dsWIGSPDRV) dim(10)
     d dsGradientInz   ds                  qualified inz
     d  WIGSP                              likeds(dsWIGSP)
     d  WIGSPDRV                           likeds(dsWIGSPDRV) dim(10)
      *** Begin Add 08/13/21 ********************************************
      * Valen Information
     d dsWIVSPEXP    e ds                  extname(WIVSPEXP) qualified inz
     d dsWIVSP       e ds                  extname(WIVSP) qualified inz
     d dsWIVSPpre    e ds                  extname(WIVSP) qualified inz
     d dsValen         ds                  qualified inz dim(50)
     d  WIVSP                              likeds(dsWIVSP)
     d  WIVSPEXP                           likeds(dsWIVSPexp) dim(50)
     d dsValenInz      ds                  qualified inz
     d  WIVSP                              likeds(dsWIVSP)
     d  WIVSPEXP                           likeds(dsWIVSPexp)
      *** End   Add 08/13/21 ********************************************
      * Cross Reference
     d dsWIEMP       e ds                  ExtName(WIEMP) Qualified Inz
      * Agent History
     d dsWMAHPOut    e ds                  ExtName(WMAHP) Qualified Inz
      * Location Master
     d dsWMALP       e ds                  ExtName(WMALP) Qualified Inz
     d                                     Dim(maxSize)
     d dsWMALPOut    e ds                  ExtName(WMALP) Qualified Inz
      * Contacts
     d dsWMECP       e ds                  ExtName(WMECP) Qualified Inz
     d                                     Dim(maxSize)
     d dsWMECPOut    e ds                  ExtName(WMECP) Qualified Inz
      * Employer Master
     d dsWMEMP       e ds                  ExtName(WMEMP) Qualified Inz
     d                                     Dim(maxSize)
     d dsWMEMPSave   e ds                  ExtName(WMEMP) Qualified Inz
     d                                     Dim(maxSize)
     d dsWMEMPOut    e ds                  ExtName(WMEMP) Qualified Inz
      * Payroll Detail
     ddsWMEPP        e ds                  extname(WMEPP) dim(maxSize) qualified
     d                                     Inz
     ddsWMEPPout     e ds                  extname(WMEPP) inz qualified
      * Large Deductible Detail
     ddsWMLDP        e ds                  extname(WMLDP) dim(maxSize) qualified
     d                                     Inz
     ddsWMLDPout     e ds                  extname(WMLDP) inz qualified
      * New Business Tracking
     ddsWMNBPout     e ds                  Extname(WMNBP) Inz Qualified
      * Employer Owners/Officers
     ddsWMOOP        e ds                  extname(WMOOP) dim(99) qualified inz
     ddsWMOOPout     e ds                  extname(WMOOP) inz qualified
      * Endorsement Form Detail
     ddsWMMUPout     e ds                  extname(WMMUP) inz qualified
      * Waiver Detail
     ddsWMWVPout     e ds                  extname(WMWVP) inz qualified

     ddsWINOTIP1     e ds                  extname(WINOTIP1) qualified inz

     ddsCode           ds                  dim(10) qualified inz
     d code                           2
     d efDt                           8  0
     d exDt                           8  0
     ddsDeductLarge    ds                  dim(3) qualified inz
     d dtEff                          7  0
     d dtExp                          7  0
     d desc                          40
     d perc                          12  9
     d type                           1
     d amp                            9  0
     d amg                            9  0
     d plan                           2
     d vrb                            1
     d ocv                            1
     d tot                            1
     d clas                           5  0
     d plv                            1
     d code                           2    dim(10)
     ddsFactor         ds                  dim(10) qualified inz
     d perc                           5  2
     d efDt                           8  0
     d exDt                           8  0
     ddsModifier       ds                  dim(3) qualified inz
     d mod                            5  4
     d final                          1
     d arap                           5  4
     d efDt                           8  0
     d exDt                           8  0

      /Free
       // Exsr BuildStub;
       Exsr GetParms;
       Exsr GetNextGroup;
       DoU noMoreGroups;
          //*** Begin Add *** 11/15/21 *************************************
          // Check to see if the agency has been
          // assigned to the default group
          If Not indDefaultChecked;
             intFein = dsWIAPP.APAgtFein;
             intMod  = dsWIAPP.APAgtMod;
             Exec Sql Select Count(*) Into :aaCount
                      From wmaap
                      Where AAco# = :intCo# And AAfnd = :selectedGroup# And
                            AAfein = :intFein And AAmod = :intMod;
             If aaCount = 0;
                ioRtnCode = 'Error';
                idx = %Lookup(*Blanks:errMsgArray);
                errMsgArray(idx) = '"Agency is not assigned to' +
                   ' the default group."';
             EndIf;
             indDefaultChecked = *On;
          EndIf;

          // Skip group if:
          // 1) Not the default group and the default group is
          //    excluded from comparative rating
          // 2) Group is excluded from comparative rating
          //    and the default group is not excluded from comparative ratin g
          If Not indSkipGroup;
          //*** End   Add *** 11/15/21 *************************************
             Exsr DataEdit;
             Exsr DataSave;
             Exsr ReturnNewPolicyInfo;
          //*** Begin Add *** 11/15/21 *************************************
          EndIf;
          //*** End   Add *** 11/15/21 *************************************
          intGrpIdx += 1;
          Exsr GetNextGroup;
       EndDo;
       If isSavingApp And Not(isAddingApp);
          Exsr DltTaggedRecs;
       EndIf;
       Exsr BuildReturnJson;
       outJson = json;
       Return;
       //==================================================
       // Build Return JSON
       //==================================================
       Begsr BuildReturnJson;
        Clear comma;
        If ioRtnCode = 'Error' Or ioRtnCode = 'Info Only';
           For idx = 1 to %Elem(errMsgArray);
            //***  Begin Chg  ***  04/21/22
            //If errMsgArray(idx) = *Blanks;
            //   Leave;
            //EndIf;
            //If comma <> *Blank;
            //   array = %Trim(array) + comma;
            //Else;
            //   comma = ',';
            //Endif;
            //array = %Trim(array) + errMsgArray(idx);
              Select;
              When errMsgArray(idx) = *blanks;
                 Leave;
              When (%len(errMsgArray(idx))+%len(array)+1) > %len(array:*max);
                 Leave;
              Endsl;
              array += comma + errMsgArray(idx);
              comma = ',';
            //***  End   Chg  ***  04/21/22
           EndFor;
           idx = %CheckR(' ':json);
           Select;
           When idx = 0;
           Other;
              If %Subst(json:idx:1) <> ',';
                 json = %Trim(json) + ',';
              Endif;
           Endsl;
           json = %trim(json) + ' "errorMsg": [ ' + %trim(array) + ' ]';
        EndIf;
       Endsr;
       //==================================================
       // Build stub results
       //==================================================
       Begsr BuildStub;
        If json <> *blanks;
           json = %trim(json) + ',';
        Endif;
        json = %trim(json) +
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {SaveApplicationData}'));
       Endsr;
       //==================================================
       // Edit data
       //==================================================
       Begsr DataEdit;
        If intCo# = 0;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Company is required in' +
            ' the incoming JSON."';
        Endif;
        If strAgtFein = *Blanks;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Agent FEIN is required."';
        EndIf;
        If strAgtMod = *Blanks;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Agent mod is required."';
        EndIf;
        If strProducer = *Blanks;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Agent producer is required."';
        EndIf;
        If intFnd = 0;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Group is required in' +
            ' the incoming JSON.  No default established for agency or group."';
        Else;
           Exec Sql
            Select Count(*)
            Into :intCount
            From WMFNP
            Where FNco# = :intCo# And FNfnd = :intFnd;
           If intCount <> 1;
              ioRtnCode = 'Error';
              idx = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idx) = '"Invalid group in' +
              ' the incoming JSON."';
           EndIf;
        Endif;
        If i1 > 2;
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"Only the selected group can be passed in' +
            ' the incoming JSON."';
        EndIf;
        If ioRtnCode = 'Error';
           indPolError = *On;
           LeaveSr;
        Endif;
        indPolError = *Off;
        Exsr DataEditWIAPPANS;
        Exsr DataEditWIAPPLOS;
        Exsr DataEditWMEMP;
        Exsr DataEditWMALP;
        Exsr DataEditWMOOP;
        Exsr DataEditWMECP;
        Exsr DataEditWDELP;
        Exsr DataEditWMEPP;
        Exsr DataEditWMLDP;
        Exsr DataEditWIGSP;
       //***  Begin Add  ***  08/05/21  ******************
        Exsr DataEditWIVSP;
       //***  End   Add  ***  08/05/21  ******************
        Select;
        When indPolError = *Off;
        When intFnd = selectedGroup#;
           ioRtnCode = 'Error';
        When intFnd <> selectedGroup#;
           ioRtnCode = 'Info Only';
        EndSl;
        isAddAsClosed = indPolError;
       Endsr;
       //==================================================
       // Edit data for WIAPPANS
       //==================================================
       Begsr DataEditWIAPPANS;
        For idx = 1 to maxSizeQuest;
           If dsWIAPPANS(idx).APid# = 0;
              Leave;
           Endif;
           If dsWIAPPANS(idx).APid# <> 0
            And dsWIAPPANS(idx).APresp = *Blanks
            and dsWIAPPANS(idx).required = 'Y';
            //indPolError = *On;
              ioRtnCode = 'Info Only';
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Question ID ' +
               %Char(dsWIAPPANS(idx).APid#) + ' is not answered."';
              Reset strQuestion;
              decCo# = dsWIAPPANS(idx).APco#;
              decId# = dsWIAPPANS(idx).APid#;
              Exec sql
               select QUquestion into :strQuestion
                from WIQUP
                where QUco# = :intCo#
                 and QUid# = :decId#
                limit 1;
              If strQuestion <> *blank;
                 errMsgArray(idy) =
                  %scanrpl('"':' (' + %trim(RmvExceptJSON(%trim(strQuestion))) +
                   ')"':
                  errMsgArray(idy):%len(errMsgArray(idy))-1);
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WIAPPLOS
       //==================================================
       Begsr DataEditWIAPPLOS;
       Endsr;
       //==================================================
       // Edit data for WIGSP/DRV
       //==================================================
       Begsr DataEditWIGSP;
       Endsr;
       //***  Begin Add  ***  08/05/21  ******************
       //==================================================
       // Edit data for WIVSP/EXP
       //==================================================
       Begsr DataEditWIVSP;
        For ig1 = 1 to %elem(dsValen);
           Select;
           When dsValen(ig1).WIVSP = dsValenInz.WIVSP;
              Iter;
           Endsl;
           dsWIVSP = dsValen(ig1).WIVSP;
           If dsWIVSP.VSrptPath = *blanks;
              Exec sql
               with
               firstMD as
               (select min(trim(char(MDIDT))||char(MDITIM)) as minMD
                 from SMMDP
                 where MDKEY=:dsWIVSP.VSrptKey and MDSEQ#=:dsWIVSP.VSrptSeq#
                 group by MDKEY, MDSEQ#)
               select
                case
                 when CMUNCN <> ' ' then CMUNCN
                 when MDUNCP <> ' ' then MDUNCP
                 else ' '
                end as uncPath into :dsWIVSP.VSrptPath
                from SDCMP
                join SMMDP on CMKEY=MDKEY and CMSEQ#=MDSEQ#
                 and trim(char(MDIDT))||char(MDITIM)=(select minMD from firstMD)
                where CMKEY=:dsWIVSP.VSrptKey and CMSEQ#=:dsWIVSP.VSrptSeq#;
              dsValen(ig1).WIVSP = dsWIVSP;
           Endif;
        Endfor;
       Endsr;
       //***  End   Add  ***  08/05/21  ******************
       //==================================================
       // Edit data for WMALP
       //==================================================
       Begsr DataEditWMALP;
       // Check for duplicate location numbers in a particular division
        For idx = 1 To intLocIdx;
           If dsWMALP(idx).ALelid = *blanks;
              Iter;
           Endif;
           For idy = 1 to intLocIdx;
              If dsWMALP(idx).ALdiv = dsWMALP(idy).ALdiv
               And dsWMALP(idx).ALelid = dsWMALP(idy).ALelid
               And idx <> idy;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Duplicate location ID''s' +
                  ' exist for ' + %Editc(dsWMALP(idx).ALdiv:'X') + '."';
              Endif;
           EndFor;
        EndFor;
        For idx = 1 to intLocIdx;
         // Ensure location effective date >= division effective date
           For idy = 1 to intDivIdx;
              If dsWMEMP(idy).EMdiv = dsWMALP(idx).ALdiv;
                 Leave;
              Endif;
           EndFor;
          // Default eff date when adding or saving app
           Select;
           When Not(isSavingApp Or isAddingApp);
           When dsWMALP(idx).ALefdt = 0;
              dsWMALP(idx).ALefdt = dsWMEMP(idy).EMefd1;
           When isEnvQuote;
              dsWMALP(idx).ALefdt = dsWMEMP(idy).EMefd1;
           EndSl;
          // Effective date edit
           If dsWMALP(idx).ALefdt < dsWMEMP(idy).EMefd1;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
              ' effective date prior to inception date' +
              ' for division ' + %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
         // Ensure location has a name
           If dsWMALP(idx).ALname = *Blank;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' name is required for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
           If dsWMALP(idx).ALMAd1 = *Blanks And
            dsWMALP(idx).ALPAd1 = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' address is required for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
           If dsWMALP(idx).ALPAd1 <> *Blanks And
            (dsWMALP(idx).ALPcty = *Blanks Or
             dsWMALP(idx).ALPst = *Blanks Or
             dsWMALP(idx).ALPzip = *Zeros);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' physical address entry is incomplete for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
           If dsWMALP(idx).ALMAd1 <> *Blanks And
            (dsWMALP(idx).ALMcty = *Blanks Or
             dsWMALP(idx).ALMst = *Blanks Or
             dsWMALP(idx).ALMzip = *Zeros);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' mailing address entry is incomplete for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
         // Ensure location has a FEIN if it is an entity type
           If dsWMALP(idx).ALfein = *Zero And dsWMALP(idx).ALeltp = 'E';
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' FEIN is required for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
         // Default location's FEIN if a 'L'ocation type
           If dsWMALP(idx).ALfein = 0 and dsWMALP(idx).ALeltp = 'L';
              dsWMALP(idx).ALfein = dsWMEMP(idy).EMfein;
           Endif;
         // Ensure location's FEIN matches employer FEIN if location type
         //  is 'L'ocation
           If dsWMALP(idx).ALeltp = 'L' And
            dsWMALP(idx).ALfein <> dsWMEMP(idy).EMfein;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Location ' + dsWMALP(idx).ALelid +
               ' FEIN must match employer FEIN for division ' +
               %Char(dsWMALP(idx).ALdiv) + '."';
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WDELP (Emp Policy Years)
       //==================================================
       Begsr DataEditWDELP;
        For idx = 1 to intDtlIdx;
           strSt = arySt(idx);
           decDt7 = dsWDELP(idx).ELpfDt;
           GetWDRRP(intCo#:intFnd:strSt:decDt7);
          // Check if Rating State is applied to division
          If strSt = *blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Rating State not associated with division.';
              // Check for payrolls
              For idz = 1 to intPayIdx;
                 If dsWMEPP(idz).EPco#    = dsWDELP(idx).ELco#
                  and dsWMEPP(idz).EPfnd  = dsWDELP(idx).ELfnd
                  and dsWMEPP(idz).EPemp# = dsWDELP(idx).ELemp#
                  and dsWMEPP(idz).EPdiv  = dsWDELP(idx).ELdiv
                  and dsWMEPP(idz).EPfyr  = dsWDELP(idx).ELfyr;
                    Leave;
                 Endif;
              EndFor;
              If idz > intPayIdx;
                 errMsgArray(idy) += ' No payrolls associated.';
              Endif;
              errMsgArray(idy) += '"';
          Endif;
          // Check to make sure state is not excluded from Connect
           If dsWDRRP.RRexst = 'Y';
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"' + strSt +
               ' is an excluded state for group ' +
               %Trim(%Char(intFnd)) + '."';
           Endif;
          // Default rate code must exist if rate code not populated
           Select;
           When dsWDRRP.RRdrat = *Blanks and dsWDELP(idx).ELrtcd = *Blanks
            and strSt <> *blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Default rate code doesn''t exist'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              LeaveSr;
           When useDftRateCode(idx) = *On;
              dsWDELP(idx).ELrtcd = dsWDRRP.RRdrat;
           Endsl;
          // Ensure rate set code is not restricted in group setup
           If dsWDELP(idx).ELrtcd <> *Blanks;
              chrRtCd = dsWDELP(idx).ELrtcd;
              Exec Sql
               Select Count(*) Into :intCount From WMGRP
               Where GRfnd = :intFnd And GRstat = 'A';
              If intCount > 0;
                 Exec Sql
                  Select  Count(*) Into :intCount From WMGRP
                  Where GRfnd = :intFnd and GRstat = 'A'
                   And GRrtcd = :chrRtCd;
                 If intCount = 0;
                    indPolError = *On;
                    idy = %Lookup(*Blanks:errMsgArray);
                    errMsgArray(idy) = '"Rate set code ' +
                     chrRtCd + ' is not valid for group ' +
                     %Char(intFnd) + '."';
                 Endif;
              Endif;
           Endif;
           strCodeSt = dsWDELP(idx).ELrtCd;
           // Ensure rate set code is not restricted for producer
           If dsWDELP(idx).ELrtCd <> *Blanks and
            not ValidStateForProducer(intCo#:intFnd:intAgt:intProducer
             :strCodeSt:'RateSet');
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Producer ' + %editc(intProducer:'X') +
               ' is NOT LICENSED for employer rating state ' + %trim(strCodeSt)+
               ' for group ' + %char(intFnd) + '."';
           Endif;
           // Default billing plan must exist if billing plan not populated
           Select;
           When dsWDRRP.RRbscd = *Zero and dsWDELP(idx).ELbscd = *Zero;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Default billing plan doesn''t exist'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           When useDftBillPlan(idx) = *On;
              dsWDELP(idx).ELbscd = dsWDRRP.RRbscd;
           Other;
              strCode = %Editc(dsWDELP(idx).ELbscd:'X');
              intCount = 0;
              Exec SQL
               Select Count(*) Into :intCount From WTBSP
                Where BSco# = :intCo# And BSkey = :strCode And
                BSstat = 'A';
              If intCount = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Billing plan is not valid."';
              Endif;
           Endsl;
          // Policy Dates
           If dsWDELP(idx).ELpfdt = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Policy dates are required."';
           Else;
              dsWDELP(idx).ELfyr =
               GetFyr(intCo#:intFnd:dsWDELP(idx).ELpfdt);
              intFyr = dsWDELP(idx).ELfyr;
           Endif;
           If dsWDELP(idx).ELptdt = 0 And dsWDELP(idx).ELpfdt <> 0;
              dsWDELP(idx).ELptdt =
               GetPolExpDate(intCo#:intFnd:dsWDELP(intDtlIdx).ELpfDt);
              If dsWDELP(idx).ELptdt = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Policy expiration date could' +
                  ' not be determined for group ' + %Char(intFnd) + '."';
              Endif;
           Endif;
           If dsWDELP(idx).ELpfdt > dsWDELP(idx).ELptdt
            And dsWDELP(idx).ELpfdt <> 0 And dsWDELP(idx).ELptdt <> 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Policy expiration date must' +
               ' after policy effective date for group ' + %Char(intFnd) + '."';
           Endif;
          // Contractor's Credit
           If dsWDELP(idx).ELasp1 <> 0 and dsWDRRP.RRcccd = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Contractor''s credit is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Deviation 1 1st period
           If dsWDELP(idx).ELasp2 <> *Zeros and dsWDRRP.RRdv1c = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Deviation 1 is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Deviation 1 2nd period
           If dsWDELP(idx).ELd1s2 <> *Zeros and dsWDRRP.RRdv1c = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Deviation 1 is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Deviation 2 1st period
           If dsWDELP(idx).ELdev2 <> *Zeros and dsWDRRP.RRdv2c = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Deviation 2 is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Deviation 2 2nd period
           If dsWDELP(idx).ELd2s2 <> *Zeros and dsWDRRP.RRdv2c = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Deviation 2 is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Ensure stock discount is valid for group and state
           If dsWDELP(idx).ELdisc <> *Blank;
              chr1a = dsWDELP(idx).ELdisc;
              Reset decRRN;
              Exec Sql
               Select RRN(WMDSP) Into :decRRN From WMDSP
                Where DSco# = :intCo# And DSfnd = :intFnd
                  And DSst = :strSt And DScode = :chr1a
                  And DSefdt <= :decDt7
                  Limit 1;
              If decRRN = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Stock discount code is invalid'  +
                  ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              Endif;
           Endif;
          // Voluntary Labor
           If dsWDELP(idx).ELvlca <> 0 And dsWDRRP.RRvlcc = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Voluntary labor is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Small Deductible
           If dsWDELP(idx).ELdcde <> *Blanks;
              strDedCode = dsWDELP(idx).ELdcde;
              Reset decRRN;
              Exec SQL
               Select RRN(WTDDP) Into :decRRN From WTDDP
                Where DDco# = :intCo# And DDfnd = :intFnd
                  And DDst = :strSt And DDcode = :strDedCode
                  And DDefdt <= :decDt7 And DDstat = 'A'
                Order by DDco#, DDfnd, DDst, DDcode, DDefdt Desc
                Limit 1;
              If decRRN = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Deductible code is invalid'  +
                  ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              EndIf;
              strCode = %Editc(dsWDELP(idx).ELbscd:'X');
              chr1a = *Blank;
              Exec SQL
               Select BSlpgm Into :chr1a From WTBSP
                Where BSco# = :intCo# And BSkey = :strCode;
              If chr1a = 'L';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"The billing plan is not flagged for'  +
                  ' for small deductible."';
              EndIf;
           Endif;
           If dsWDELP(idx).ELdcd2 <> *Blanks;
              strDedCode = dsWDELP(idx).ELdcd2;
              Reset decRRN;
              Exec SQL
               Select RRN(WTDDP) Into :decRRN From WTDDP
                Where DDco# = :intCo# And DDfnd = :intFnd
                  And DDst = :strSt And DDcode = :strDedCode
                  And DDefdt <= :decDt7 And DDstat = 'A'
                Order by DDco#, DDfnd, DDst, DDcode, DDefdt Desc
                Limit 1;
              If decRRN = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Deductible code is invalid'  +
                  ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              EndIf;
           Endif;
          // Safety Fee
           If dsWDELP(idx).ELsfpc <> 0 And dsWDRRP.RRsfap = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Safety fee percentage is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
           If dsWDELP(idx).ELsfmn <> 0 And dsWDRRP.RRsfhd = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Safety fee minimum is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Debit/Credit Factor
           If dsWDELP(idx).ELdbcr < 0 And dsWDRRP.RRcrcd = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Scheduled credit is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
           If dsWDELP(idx).ELdbcr > 0 And dsWDRRP.RRdbcd = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Scheduled debit is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Subject Premium Credit 1a
           If dsWDELP(idx).ELaspm <> 0 And dsWDRRP.RR1acd = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Subject Premium 1a is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Subject Premium Credit 1b
           If dsWDELP(idx).ELasps <> 0 And dsWDRRP.RR1bcd = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Subject Premium 1a is invalid'  +
               ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
           Endif;
          // Increased Limits
           If dsWDELP(idx).ELilcd <> *Blanks;
              strCode = dsWDELP(idx).ELilcd;
              Reset decRRN;
              Exec SQL
               Select Rrn(WTLMP) Into :decRRN
                From WTLMP
                Where LMco# = :intCo#
                 And LMfnd = :intFnd
                 And LMst = :strSt
                 And LMkey = :strCode
                 And LMefdt <= :decDt7
                Order by LMco#, LMfnd, LMst, LMkey, LMefdt desc
                Limit 1;
              If decRRN = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Increased limits code doesn''t exist'  +
                  ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              Endif;
           EndIf;
           If dsWDELP(idx).ELinl2 <> *Blanks;
              strCode = dsWDELP(idx).ELinl2;
              Reset decRRN;
              Exec SQL
               Select Rrn(WTLMP) Into :decRRN
                From WTLMP
                Where LMco# = :intCo#
                 And LMfnd = :intFnd
                 And LMst = :strSt
                 And LMkey = :strCode
                 And LMefdt <= :decDt7
                Order by LMco#, LMfnd, LMst, LMkey, LMefdt desc
                Limit 1;
              If decRRN = 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Increased limits code doesn''t exist'  +
                  ' for group ' + %Char(intFnd) + ' in ' + strSt + '."';
              Endif;
           EndIf;
           // Facultative Reinsurance
           If dsWDELP(idx).ELarcp <> 0 And dsWDRRP.RRarcc = *Zero;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Facultative reinsurance percentage' +
               ' is invalid for group '  + %Char(intFnd) + ' in ' +
               strSt + '."';
           Endif;
           If dsWDELP(idx).ELarca <> 0 And dsWDRRP.RRarcc = *Zero;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Facultative reinsurance minimum' +
               ' is invalid for group '  + %Char(intFnd) + ' in ' +
               strSt + '."';
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WMECP (Emp Contacts)
       //==================================================
       Begsr DataEditWMECP;
        For idx = 1 To intConIdx;
           dsWMECP(idx).ECfnd  = intFnd;
          // Last name required
           If dsWMECP(idx).EClsn = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Contact last name is required'  +
               ' for division ' + %Editc(dsWMECP(idx).ECdiv:'X') + '."';
           Endif;
          // First name or title required
           If dsWMECP(idx).ECfsn = *Blanks
            And dsWMECP(idx).ECtitl = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Contact last name is required'  +
               ' for division ' + %Editc(dsWMECP(idx).ECdiv:'X') + '."';
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WMEMP
       //==================================================
       Begsr DataEditWMEMP;
        // Check for duplicate division numbers
        For idx = 1 to intDivIdx;
           idy = %LookUp(dsWMEMP(idx).EMdiv:aryDiv);
           idy = %LookUp(dsWMEMP(idx).EMdiv:aryDiv:idy + 1);
           If idy <> 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Duplication division numbers exist."';
           Endif;
        EndFor;
        indStRptFlg = *Off;
        For idx = 1 to intDivIdx;
           If GetFyr(intCo#:intFnd:dsWMEMP(idx).EMefd1) = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Inception date not valid' +
               ' for group ' + %EditC(intFnd:'X') + '."';
           Endif;
           If intAgt = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Agent info not valid' +
               ' for group ' + %EditC(intFnd:'X') + '."';
           Endif;
           If intRate = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Default agent rate not' +
               ' available for group ' + %EditC(intFnd:'X') + '."';
           Endif;
           If dsWMEMP(idx).EMfein = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Employer FEIN is required."';
           Endif;
           //*** Beg Add *** 09/04/24 **************************************
           If clientId = '102';
              strEMpSt = dsWMEMP(idx).EMpSt;
              strEMpCty = dsWMEMP(idx).EMpCty;
              strEMmSt = dsWMEMP(idx).EMmSt;
              strEMmCty = dsWMEMP(idx).EMmCty;
              Select;
              When dsWMEMP(idx).EMpad1 <> *blanks;
                 Exec Sql
                  Select CYrgn Into :intDfltRegion
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :strEMpSt
                    And CYcty = :strEMpCty
                    And CYrgn <> 0
                   Limit(1);
                 Exec Sql
                  Select CYlRep Into :strDfltLossRep
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :strEMpSt
                    And CYcty = :strEMpCty
                    And CYlRep <> ' '
                   Limit(1);
              When dsWMEMP(idx).EMmAd1 <> *blanks;
                 Exec Sql
                  Select CYrgn Into :intDfltRegion
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :strEMmSt
                    And CYcty = :strEMmCty
                    And CYrgn <> 0
                   Limit(1);
                 Exec Sql
                  Select CYlRep Into :strDfltLossRep
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :strEMmSt
                    And CYcty = :strEMmCty
                    And CYlRep <> ' '
                   Limit(1);
              Endsl;
           Endif;
           //*** End Add *** 09/04/24 **************************************
           Select;
           When dsWMEMP(idx).EMrgn# <> 0;
           When dsWMEMP(idx).EMrgn# = 0 And intDfltRegion <> 0;
              dsWMEMP(idx).EMrgn# = intDfltRegion;
           Other;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Employer default region is not set up' +
               ' for group ' + %Editc(intFnd:'X') + ' ."';
           Endsl;
           //*** Beg Add *** 09/04/24 **************************************
           If clientId = '102';
              Select;
              When dsWMEMP(idx).EMlRep <> *blanks;
              When dsWMEMP(idx).EMlRep = *blanks And strDfltLossRep <> ' ';
                 dsWMEMP(idx).EMlRep = strDfltLossRep;
              Endsl;
           Endif;
           //*** End Add *** 09/04/24 **************************************
           Select;
           When dsWMEMP(idx).EMoff# <> *Blanks;
           When dsWMEMP(idx).EMoff# = *Blanks And strDfltOffice <> *Blanks;
              dsWMEMP(idx).EMoff# = strDfltOffice;
           Other;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Employer default office is not set up' +
               ' for group ' + %Editc(intFnd:'X') + ' ."';
           Endsl;
           Select;
           When dsWMEMP(idx).EMstrp = 'Y' And indStRptFlg = *On;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Another division is flagged as' +
               ' state reporting."';
           When dsWMEMP(idx).EMstrp = 'Y';
              indStRptFlg = *On;
           Endsl;
           If dsWMEMP(idx).EMname = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Employer DBA Name is required."';
           Endif;
           If dsWMEMP(idx).EMMAd1 = *Blanks And
            dsWMEMP(idx).EMPAd1 = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"An address is required."';
           Endif;
           If dsWMEMP(idx).EMPAd1 <> *Blanks And
            (dsWMEMP(idx).EMPcty = *Blanks Or
             dsWMEMP(idx).EMPst = *Blanks Or
             dsWMEMP(idx).EMPzip = *Zeros);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Incomplete physical address provided."';
           Endif;
           If dsWMEMP(idx).EMMAd1 <> *Blanks And
            (dsWMEMP(idx).EMMcty = *Blanks Or
             dsWMEMP(idx).EMMst = *Blanks Or
             dsWMEMP(idx).EMMzip = *Zeros);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Incomplete mailing address provided."';
           Endif;
        EndFor;
        If indStRptFlg = *Off;
           indPolError = *On;
           idy = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idy) = '"No division is flagged as state reporting."';
        Endif;
       Endsr;
       //==================================================
       // Edit data for WMEPP (Emp Payrolls)
       //==================================================
       Begsr DataEditWMEPP;
        For idx = 1 to intPayIdx;
           If dsWMEPP(idx).EPclas = 0;
              Iter;
           Endif;
           Reset decDt7;
           Reset chrRtCd;
           dsWMEPP(idx).EPfyr  =
            GetFyr(intCo#:intFnd:dsWMEPP(idx).EPefdt);
           For idy = 1 to intDtlIdx;
              If dsWMEPP(idx).EPco#    = dsWDELP(idy).ELco#
               And dsWMEPP(idx).EPfnd  = dsWDELP(idy).ELfnd
               And dsWMEPP(idx).EPemp# = dsWDELP(idy).ELemp#
               And dsWMEPP(idx).EPdiv  = dsWDELP(idy).ELdiv
               And dsWMEPP(idx).EPfyr  = dsWDELP(idy).ELfyr;
                  decDt7 = dsWDELP(idy).ELpfdt;
                  chrRtCd = dsWDELP(idy).ELrtcd;
                  Leave;
               Endif;
           EndFor;
         // Policy year doesn't exist for payroll date
           If decDt7 = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Invalid payroll date for ' +
               ' Group ' + %Editc(intFnd:'X') + ' and division '  +
               %Editc(dsWMEPP(idx).EPdiv:'X') + '."';
           Endif;
         // Go to next record if no rate set code
           If chrRtCd = *Blank;
              Iter;
           EndIf;
         // Verify that payroll exists in table
           dsWMEPPOut = dsWMEPP(idx);
           Reset decRRN;
           Exec Sql
            Select rrn(WMPRP) Into :decRRN
            From WMPRP
            Where PRco# = :intCo#
             And PRrtcd = :chrRtCd
             And PRstat = 'A'
             And PRclas = :dsWMEPPOut.EPclas
             And PRefdt <= :decDt7
            Order by PRco#, PRrtcd, PRclas, PRefdt Desc
            Limit 1;
           If decRRN = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Payroll class ' +
               %Editc(dsWMEPP(idx).EPclas:'X') +
               ' is invalid for group ' + %Editc(intFnd:'X') +
               ' in division ' + %Editc(dsWMEPP(idx).EPdiv:'X') +
               '."';
           EndIf;
          // Ext required for CA and WA if extension record exists
          //  for class or class/state
           If dsWMEPPOut.EPext = *Blanks And
            (arySt(idy) = 'CA' or arySt(idy) = 'WA');
              Reset decRRN;
              chrSt = arySt(idy);
              Exec Sql
               Select rrn(WMPXP) Into :decRRN
               From WMPXP
               Where PXco#  = :intCo#
                And PXclas  = :dsWMEPPOUT.EPclas
                And PXst    = :chrSt
                And PXefdt <= :decDt7
                And PXstat = 'A'
                Order by PXefdt
                Limit 1;
              If decRRN <> 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Payroll class ' +
                  %Editc(dsWMEPP(idx).EPclas:'X') +
                  ' requires extended description ' +
                  ' for group ' + %Editc(intFnd:'X') +
                  ' in division ' + %Editc(dsWMEPP(idx).EPdiv:'X') +
                  '."';
                 Iter;
              Endif;
              chrSt = *Blank;
              Reset decRRN;
              Exec Sql
               Select rrn(WMPXP) Into :decRRN
               From WMPXP
               Where PXco#  = :intCo#
                And PXclas  = :dsWMEPPOUT.EPclas
                And PXst    = :chrSt
                And PXefdt <= :decDt7;
              If decRRN <> 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Payroll class ' +
                  %Editc(dsWMEPP(idx).EPclas:'X') +
                  ' requires extended description ' +
                  ' for group ' + %Editc(intFnd:'X') +
                  ' in division ' + %Editc(dsWMEPP(idx).EPdiv:'X') +
                  '."';
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WMLDP
       //==================================================
       Begsr DataEditWMLDP;
       // Large Deductible Edit Here
        For idx = 1 to intDtlIdx;
           If dsWMLDP(idx).LDefd1 = 0 And dsWMLDP(idx).LDefd2 = 0;
              Iter;
           Endif;
           If strCode <> %Editc(dsWDELP(idx).ELbscd:'X')
            And dsWMLDP(idx).LDefd1 <> 0;
              strCode = %Editc(dsWDELP(idx).ELbscd:'X');
              chr1a = *Blank;
              Exec SQL
               Select BSlpgm Into :chr1a From WTBSP
                Where BSco# = :intCo# And BSkey = :strCode;
              If chr1a <> 'L';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"The billing plan is not flagged for'  +
                  ' for large deductible for division ' +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              EndIf;
           Endif;
        // 2nd deductible can't be set up unless 1st exists
           If dsWMLDP(idx).LDefd2 <> 0 And dsWMLDP(idx).LDefd1 = 0;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"2nd large deductible plan not allowed'  +
               ' unless 1st plan exists for ' +
               %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
           EndIf;
        // Effective dates and expiration dates must be in ascending order
           If dsWMLDP(idx).LDexp1 <> 0
            And dsWMLDP(idx).LDexp1 < dsWMLDP(idx).LDefd1
            Or dsWMLDP(idx).LDexp2 <> 0
            And dsWMLDP(idx).LDexp2 < dsWMLDP(idx).LDefd2;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Large deductible dates must be in' +
               ' ascending order for '  +
               %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
           Endif;
         // Ensure that effective date is populated if any of the other
         //  elements are populated.
           If dsWMLDP(idx).LDefd1 = 0 And
            (dsWMLDP(idx).LDdes1 <> *Blanks
             Or dsWMLDP(idx).LDpct1 <> *Zero
             Or dsWMLDP(idx).LDtyp1 <> *Blank
             Or dsWMLDP(idx).LDamp1 <> *Zero
             Or dsWMLDP(idx).LDamg1 <> *Zero
             Or dsWMLDP(idx).LDpln1 <> *Blank
             Or dsWMLDP(idx).LDvrb1 <> *Blank
             Or dsWMLDP(idx).LDocv1 <> *Blank
             Or dsWMLDP(idx).LDtot1 <> *Blank
             Or dsWMLDP(idx).LDcls1 <> *Zero
             Or dsWMLDP(idx).LDplv1 <> *Blank
             Or dsWMLDP(idx).LDala1 <> *Blank
             Or dsWMLDP(idx).LDrnw1 <> *Blank
             Or dsWMLDP(idx).LDr101 <> *Blank
             Or dsWMLDP(idx).LDr102 <> *Blank
             Or dsWMLDP(idx).LDr103 <> *Blank
             Or dsWMLDP(idx).LDr104 <> *Blank
             Or dsWMLDP(idx).LDr105 <> *Blank
             Or dsWMLDP(idx).LDr106 <> *Blank
             Or dsWMLDP(idx).LDr107 <> *Blank
             Or dsWMLDP(idx).LDr108 <> *Blank
             Or dsWMLDP(idx).LDr109 <> *Blank
             Or dsWMLDP(idx).LDr110 <> *Blank
             Or dsWMLDP(idx).LDadp1 <> *Zero);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"First large deductible effective date -
               must not be zero for '  +
               %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
            Endif;
           If dsWMLDP(idx).LDefd2 = 0 And
            (dsWMLDP(idx).LDdes2 <> *Blanks
             Or dsWMLDP(idx).LDpct2 <> *Zero
             Or dsWMLDP(idx).LDtyp2 <> *Blank
             Or dsWMLDP(idx).LDamp2 <> *Zero
             Or dsWMLDP(idx).LDamg2 <> *Zero
             Or dsWMLDP(idx).LDpln2 <> *Blank
             Or dsWMLDP(idx).LDvrb2 <> *Blank
             Or dsWMLDP(idx).LDocv2 <> *Blank
             Or dsWMLDP(idx).LDtot2 <> *Blank
             Or dsWMLDP(idx).LDcls2 <> *Zero
             Or dsWMLDP(idx).LDplv2 <> *Blank
             Or dsWMLDP(idx).LDala2 <> *Blank
             Or dsWMLDP(idx).LDrnw2 <> *Blank
             Or dsWMLDP(idx).LDr201 <> *Blank
             Or dsWMLDP(idx).LDr202 <> *Blank
             Or dsWMLDP(idx).LDr203 <> *Blank
             Or dsWMLDP(idx).LDr204 <> *Blank
             Or dsWMLDP(idx).LDr205 <> *Blank
             Or dsWMLDP(idx).LDr206 <> *Blank
             Or dsWMLDP(idx).LDr207 <> *Blank
             Or dsWMLDP(idx).LDr208 <> *Blank
             Or dsWMLDP(idx).LDr209 <> *Blank
             Or dsWMLDP(idx).LDr210 <> *Blank
             Or dsWMLDP(idx).LDadp2 <> *Zero);
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Second large deductible effective date -
               must not be zero for '  +
               %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
           Endif;
         // Required elements if first effective date is populated.
           If dsWMLDP(idx).LDefd1 <> *Zero;
            // Blank description
              If dsWMLDP(idx).LDdes1 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible description -
                  is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Gross or Net
              If dsWMLDP(idx).LDtyp1 <> 'G'
               And dsWMLDP(idx).LDtyp1 <> 'N';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible type -
                  is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Deductible amount < 0
              If dsWMLDP(idx).LDamp1 < 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible amount -
                  can not be negative for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Aggregate amount < 0
              If dsWMLDP(idx).LDamg1 < 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible -
                  aggregate can not be negative for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Validate plan type
              If dsWMLDP(idx).LDpln1 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible plan -
                  type is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Occurence flag <> 'C' Or 'O'
              If dsWMLDP(idx).LDvrb1 <> 'C'
               And dsWMLDP(idx).LDvrb1 <> 'O';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible accident -
                  basis must be ''C'' or ''O'' for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Occupational disease flag <> 'C' Or 'O'
              If dsWMLDP(idx).LDocv1 <> 'C'
               And dsWMLDP(idx).LDocv1 <> 'O'
               And dsWMLDP(idx).LDocv1 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible accident -
                  basis must be ''C'' or ''O'' for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Stat code is required
              If dsWMLDP(idx).LDcls1 = *Zero;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible -
                  statistical code is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Total incurred must be 'Y' or *Blank
              If dsWMLDP(idx).LDtot1 <> 'Y'
               And dsWMLDP(idx).LDtot1 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible -
                  total incurred must be ''Y'' or blank for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Premium level required if discount percentage <> 0
              If dsWMLDP(idx).LDpct1 <> 0
               And dsWMLDP(idx).LDplv1 = *Blanks;
                    indPolError = *On;
                    idy = %Lookup(*Blanks:errMsgArray);
                    errMsgArray(idy) = '"First large deductible premium -
                     level is required for ' +
                     %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // First reserve code is required
              If dsWMLDP(idx).LDr101 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible reserve -
                  code is required for ' +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Valid auto renew flag
              If dsWMLDP(idx).LDrnw1 <> 'Y'
               And dsWMLDP(idx).LDrnw1 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"First large deductible auto -
                  renew flag must be ''Y'' or blank for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
           Endif;
         // Required elements if second effective date is populated.
           If dsWMLDP(idx).LDefd2 <> *Zero;
            // Blank description
              If dsWMLDP(idx).LDdes2 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible description -
                  is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Gross or Net
              If dsWMLDP(idx).LDtyp2 <> 'G'
               And dsWMLDP(idx).LDtyp2 <> 'N';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible type -
                  is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Deductible amount < 0
              If dsWMLDP(idx).LDamp2 < 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible amount -
                  can not be negative for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Aggregate amount < 0
              If dsWMLDP(idx).LDamg2 < 0;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible -
                  aggregate can not be negative for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Validate plan type
              If dsWMLDP(idx).LDpln2 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible plan -
                  type is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Occurence flag <> 'C' Or 'O'
              If dsWMLDP(idx).LDvrb2 <> 'C'
               And dsWMLDP(idx).LDvrb2 <> 'O';
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible accident -
                  basis must be ''C'' or ''O'' for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Occupational disease flag <> 'C' Or 'O'
              If dsWMLDP(idx).LDocv2 <> 'C'
               And dsWMLDP(idx).LDocv2 <> 'O'
               And dsWMLDP(idx).LDocv2 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible accident -
                  basis must be ''C'' or ''O'' for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Stat code is required
              If dsWMLDP(idx).LDcls2 = *Zero;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible -
                  statistical code is required for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Total incurred must be 'Y' or *Blank
              If dsWMLDP(idx).LDtot2 <> 'Y'
               And dsWMLDP(idx).LDtot2 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible -
                  total incurred must be ''Y'' or blank for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Premium level required if discount percentage <> 0
              If dsWMLDP(idx).LDpct2 <> 0
               And dsWMLDP(idx).LDplv1 = *Blanks;
                    indPolError = *On;
                    idy = %Lookup(*Blanks:errMsgArray);
                    errMsgArray(idy) = '"Second large deductible premium -
                     level is required for ' +
                     %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Second reserve code is required
              If dsWMLDP(idx).LDr201 = *Blanks;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible reserve -
                  code is required for ' +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
            // Valid auto renew flag
              If dsWMLDP(idx).LDrnw2 <> 'Y'
               And dsWMLDP(idx).LDrnw2 <> *Blank;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Second large deductible auto -
                  renew flag must be ''Y'' or blank for '  +
                  %Editc(dsWMLDP(idx).LDdiv:'X') + '."';
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Edit data for WMOOP (Owners/Officers)
       //==================================================
       Begsr DataEditWMOOP;
       // Check for duplicate sequence numbers in a particular division
        For idx = 1 To intOffIdx;
           For idy = 1 to intOffIdx;
              If dsWMOOP(idx).OOdiv = dsWMOOP(idy).OOdiv
               And dsWMOOP(idx).OOseq# = dsWMOOP(idy).OOseq#
               And idx <> idy;
                 indPolError = *On;
                 idy = %Lookup(*Blanks:errMsgArray);
                 errMsgArray(idy) = '"Duplicate owner officer ID''s' +
                  ' exist for division ' + %Editc(dsWMOOP(idx).OOdiv:'X') +
                  ' in group ' + %Editc(dsWMOOP(idx).OOfnd:'X') + '."';
              Endif;
           EndFor;
        EndFor;
        Reset decPercent;
        For idx = 1 To intOffIdx;
          // Get division's employer info
           For idy = 1 to intDivIdx;
              If dsWMEMP(idy).EMdiv = dsWMOOP(idx).OOdiv;
                 Leave;
              Endif;
           EndFor;
          // Need a last name
           If dsWMOOP(idx).OOolsn = *Blanks;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Owner officer last name is required."';
           EndIf;
          // Must be I/E for sole proprietorship or partnership
           If (dsWMEMP(idy).EMtpob = '1'
            Or dsWMEMP(idy).EMtpob = '2')
            And dsWMOOP(idx).OOexfl = *Blank;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Owner officer must be excluded' +
               ' or included for div ' + %Editc(dsWMOOP(idx).OOdiv:'X') +
               ' in group ' + %Editc(dsWMOOP(idx).OOfnd:'X') + '."';
           EndIf;
          //Default effective dates when adding or saving app
           Select;
           When Not(isSavingApp Or isAddingApp);
           When dsWMOOP(idx).OOefdt = 0;
              dsWMOOP(idx).OOefdt = dsWMEMP(idy).EMefd1;
           When isEnvQuote;
              dsWMOOP(idx).OOefdt = dsWMEMP(idy).EMefd1;
           EndSl;
          // Effective date required if I/E
           If dsWMOOP(idx).OOefdt = 0 And dsWMOOP(idx).OOexfl <> *Blank;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Owner officer effective date is' +
               ' required for div ' + %Editc(dsWMOOP(idx).OOdiv:'X') +
               ' in group ' + %Editc(dsWMOOP(idx).OOfnd:'X') + '."';
           Endif;
          //Effective date edits
           Select;
           When dsWMOOP(idx).OOefdt = 0;
           When dsWMOOP(idx).OOefdt < dsWMEMP(idy).EMefd1;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Owner officer cannot be effective' +
               ' before division ' + %Editc(dsWMOOP(idx).OOdiv:'X') +
               ' in group ' + %Editc(dsWMOOP(idx).OOfnd:'X') + '."';
           When dsWMOOP(idx).OOexdt = 0;
           When dsWMOOP(idx).OOexdt < dsWMOOP(idx).OOefdt;
              indPolError = *On;
              idy = %Lookup(*Blanks:errMsgArray);
              errMsgArray(idy) = '"Owner officer cannot expire' +
               ' before effective date for division ' +
               %Editc(dsWMOOP(idx).OOdiv:'X') +
               ' in group ' + %Editc(dsWMOOP(idx).OOfnd:'X') + '."';
           Endsl;
           decPercent += dsWMOOP(idx).OOofPc;
        EndFor;
        If decPercent > 100;
           indPolError = *On;
           idy = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idy) = '"Owner\/officer total percentage ' +
            'greater than 100."';
        Endif;
       Endsr;
       //==================================================
       // Save data
       //==================================================
       Begsr DataSave;
        If indPolError = *On;
           LeaveSr;
        Endif;
       // Tag files associated with application for possible
       //  deletion.
        If Not(isAddingApp) And isSavingApp And Not(isTagged);
           isTagged = *On;
           Exsr TagForDelete;
        EndIf;
       // Write application files and propagate to other groups
       //  only if saving an application
        Select;
        When Not(isSavingApp) And Not(isAddingApp);
        When Not(indAppSaved);
           indAppSaved = *On;
           Exsr DataSaveWIAPP;
           Exsr DataSaveWIAPPLOS;
        EndSl;
        Exsr DataSaveWMEMP;
        Exsr DataSaveWMALP;
        Exsr DataSaveWMOOP;
        Exsr DataSaveWMECP;
        Exsr DataSaveWAQTP;
        Exsr DataSaveWMNBP;
        Exsr DataSaveWDELP;
        Exsr DataSaveWMEPP;
        Exsr DataSaveWMLDP;
        Exsr DataSaveWaivers;
       Endsr;
       //==================================================
       // Write shell records for specific waiver count
       //==================================================
       Begsr DataSaveWaivers;
        If intApp# = 0;
           LeaveSr;
        EndIf;
        For idx = 1 to intDtlIdx;
           If aryWaivCnt(idx) = 0;
              Iter;
           EndIf;
           dsWDELPout = dsWDELP(idx);
           If dsWDELPout.ELwvtp <> 'S';
              Iter;
           EndIf;
          // Delete endorsement form records for app
           Exec Sql
            Delete From WMMUP
             Where MUco# = :dsWDELPout.ELco#
              And MUfnd = :dsWDELPout.ELfnd
              And MUemp# = :dsWDELPout.ELemp#
              And MUdiv = :dsWDELPout.ELdiv
              And MUfyr = :dsWDELPout.ELfyr;
          // Delete current waivers for app
           Exec Sql
            Delete From WMWVP
             Where WVco# = :dsWDELPout.ELco#
              And WVfnd = :dsWDELPout.ELfnd
              And WVemp# = :dsWDELPout.ELemp#
              And WVdiv = :dsWDELPout.ELdiv
              And WVfyr = :dsWDELPout.ELfyr;
          // Write shell records so waiver premium can be calculated.
          //  These will be deleted when submitted.
           Exec Sql
            Select RSjrst
            Into :strSt
            From WMRSP
            Where RSco# = :intCo# And RSrtcd = :dsWDELPout.ELrtcd;
           For idy to aryWaivCnt(idx);
              dsWMWVPOut.WVco# = dsWDELPout.ELco#;
              dsWMWVPOut.WVfnd = dsWDELPout.ELfnd;
              dsWMWVPOut.WVemp# = dsWDELPout.ELemp#;
              dsWMWVPOut.WVdiv = dsWDELPout.ELdiv;
              dsWMWVPOut.WVfyr = dsWDELPout.ELfyr;
              dsWMWVPOut.WVswpc = dsWDELPout.ELwvpc;
              dsWMWVPOut.WVfchg = dsWDELPout.ELfchg;
              dsWMWVPOut.WVwmin = dsWDELPout.ELwmin;
             // Name is being used so that Quote Transfer can edit
             //  to prevent shell records from transferring to live.
              dsWMWVPOut.WVname = 'Portal Waiver';
              dsWMWVPOut.WVwav# = idy;
              dsWMWVPOut.WVefdt = dsWDELPout.ELpfdt;
              dsWMWVPOut.WVst   = strSt;
              dsWMWVPOut.WVipgm = Q1pgm;
              dsWMWVPOut.WViusr = inUser;
              dsWMWVPOut.WVidt = sysDate7;
              dsWMWVPOut.WVitm = sysTime6;
              dsWMWVPOut.WVpgm = Q1pgm;
              dsWMWVPOut.WVuser = inUser;
              dsWMWVPOut.WVdt = sysDate7;
              dsWMWVPOut.WVtime = sysTime6;
              Exec Sql
               Insert Into WMWVP
               Values(:dsWMWVPout);
            EndFor;
           // Do not duplicate endorsement forms on policy
            GetWaiverInfo(dsWDELPout.ELco#:dsWDELPout.ELfnd:strSt:
             dsWDELPout.ELpfdt:strWaiverForm:strWaiverDesc:
             chrIncExcFlag:chrManAutoFlag:chrPrintFlag);
            intCount = 0;
            Exec Sql
             Select Count(*)
              Into :intCount
              From WMWVP
              Where WVco# = dsWDELPout.ELco#
               And WVfnd = dsWDELPout.ELfnd
               And WVemp# = dsWDELPout.ELemp#
               And WVfyr = dsWDELPout.ELfyr;
            If intCount > 0;
               Iter;
            Endif;
            dsWMMUPOut.MUco# = dsWDELPout.ELco#;
            dsWMMUPOut.MUfnd = dsWDELPout.ELfnd;
            dsWMMUPOut.MUemp# = dsWDELPout.ELemp#;
            dsWMMUPOut.MUdiv =
             GetStRptDv(dsWDELPout.ELco#:dsWDELPout.ELfnd:dsWDELPout.ELemp#);
            dsWMMUPOut.MUfyr = dsWDELPout.ELfyr;
            dsWMMUPOut.MUend# = 0;
            dsWMMUPOut.MUseq# = 0;
            dsWMMUPOut.MUform = strWaiverForm;
            dsWMMUPOut.MUenst = strSt;
            dsWMMUPOut.MUdesc = strWaiverDesc;
            dsWMMUPOut.MUdsc2 = strWaiverDesc;
            dsWMMUPOut.MUeedt = dsWDELPout.ELpfdt;
            dsWMMUPOut.MUexfl = chrIncExcFlag;
            dsWMMUPOut.MUmnau = chrManAutoFlag;
            dsWMMUPOut.MUprnt = 'N';
            dsWMMUPout.MUstat = 'A';
            dsWMMUPOut.MUipgm = Q1pgm;
            dsWMMUPOut.MUiusr = inUser;
            dsWMMUPOut.MUidt = sysDate7;
            dsWMMUPOut.MUpgm = Q1pgm;
            dsWMMUPOut.MUuser = inUser;
            dsWMMUPOut.MUdt = sysDate7;
            dsWMMUPOut.MUtime = sysTime6;
            Exec Sql
             Insert Into WMMUP
             Values(:dsWMMUPout);
        Endfor;
       Endsr;
       //==================================================
       // Save data - WAQTP
       //==================================================
       Begsr DataSaveWAQTP;
        For idx = 1 to intDivIdx;
           intDiv = dsWMEMP(idx).EMdiv;
          //Save current values
           Exec SQL
            Select * Into :dsWAQTPOut From WAQTP
             Where QTco# = :intCo#
              And QTfnd = :intFnd
              And QTemp# = :intEmp#
              And QTdiv = :intDiv;
          // Populate new values
           dsWAQTP(idx) = dsWAQTPOut;
           dsWAQTP(idx).QTco#  = intCo#;
           dsWAQTP(idx).QTfnd  = intFnd;
           dsWAQTP(idx).QTemp# = intEmp#;
           dsWAQTP(idx).QTdiv  = intDiv;
           dsWAQTP(idx).QTagt# = intAgt;
           dsWAQTP(idx).QTname = dsWMEMP(idx).EMname;
           dsWAQTP(idx).QTchap = dsWMEMP(idx).EMchap;
           dsWAQTP(idx).QTefdt = dsWMEMP(idx).EMefd1;
           dsWAQTP(idx).QTwkdt = sysDate7;
           dsWAQTP(idx).QTordt = sysDate7;
           dsWAQTP(idx).QTpgm  = Q1pgm;
           dsWAQTP(idx).QTuser = inUser;
           dsWAQTP(idx).QTdt   = sysDate7;
           dsWAQTP(idx).QTtime = sysTime6;
           dsWAQTPOut = dsWAQTP(idx);
           Select;
           When IsValidFetch(SqlStt);
              Exec Sql
               Update WAQTP
                Set Row = :dsWAQTPOut
                Where QTco# = :intCo#
                 And QTfnd = :intFnd
                 And QTemp# = :intEmp#
                 And QTdiv = :intDiv;
              If sqlstt = conSqlFileMismatch;
                 Exec SQL
                  Select rrn(WAQTP) Into :decRRN From WAQTP
                   Where QTco# = :intCo#
                    And QTfnd = :intFnd
                    And QTemp# = :intEmp#
                    And QTdiv = :intDiv;
                 dsWAQTPdef = dsWAQTPout;
                 zFileMismatch('WAQTP':decRRN);
              Endif;
           Other;
              dsWAQTP(idx).QTtype = 'P';
              dsWAQTP(idx).QTstat = 'Q';
              If isAddAsClosed;
                 dsWAQTP(idx).QTtype = 'S'; // Closed
              Endif;
              dsWAQTPout.QTtype = dsWAQTP(idx).QTtype;
              dsWAQTPout.QTstat = dsWAQTP(idx).QTstat;
              dsWAQTPout.QTipgm = Q1pgm;
              dsWAQTPout.QTiusr = inUser;
              dsWAQTPout.QTidt = sysDate7;
              dsWAQTPout.QTitm = sysTime6;
              Exec Sql
               Insert Into WAQTP
                Values(:dsWAQTPout);
              If sqlstt = conSqlFileMismatch;
                 dsWAQTPdef = dsWAQTPout;
                 zFileMismatch('WAQTP');
              Endif;
           EndSl;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WDELP
       //==================================================
       Begsr DataSaveWDELP;
        For idx = 1 to intDtlIdx;
           Reset decRRN;
           dsWDELP(idx).ELfnd  = intFnd;
           dsWDELP(idx).ELemp# = intEmp#;
           dsWDELP(idx).ELfyr  = intFyr;
           dsWDELPout = dsWDELP(idx);
           Select;
           When dsWDELPout.ELdCde <> *blanks and dsWDELPout.ELcxBl = *blanks;
              dsWDELPout.ELcxBl = 'Y';
           When dsWDELPout.ELdCde = *blanks and dsWDELPout.ELcxBl <> *blanks;
              dsWDELPout.ELcxBl = ' ';
           Endsl;
           dsWDELPout.ELstat = 'N';
           dsWDELPout.ELpgm  = Q1pgm;
           dsWDELPout.ELuser = inUser;
           dsWDELPout.ELdt   = DateTo7(SystemDate());
           dsWDELPout.ELtime = TimeTo6(SystemTime());
           Exec sql
            select rrn(WDELP) into :decRRN
             from WDELP
             where ELco# = :dsWDELPout.ELco#
              and ELfnd  = :dsWDELPout.ELfnd
              and ELemp# = :dsWDELPout.ELemp#
              and ELdiv  = :dsWDELPout.ELdiv
              and ELfyr  = :dsWDELPout.ELfyr;
           Select;
           When decRRN = 0;
              dsWDELPout.ELnwRw = 'N';
              dsWDELPout.ELsrPr = 'P';
              //*** Beg Add ***** 03/28/25 ********************
              dsWDELPout.ELexrt = '3';
              If clientID = '150';
                 dsWDELPout.ELexrt = '2';
                 dsWDELPout.ELncci = 'Y';
              Endif;
              //*** End Add ***** 03/28/25 ********************
              dsWDELPout.ELiPgm = dsWDELPout.ELpgm;
            //dsWDELPout.ELiUsr = 'CSPORTAL';
              dsWDELPout.ELiUsr = strEntryPointGlobal;
              dsWDELPout.ELidt  = dsWDELPout.ELdt;
              dsWDELPout.ELiTm  = dsWDELPout.ELtime;
              Exec sql
               insert into WDELP
                values(:dsWDELPout);
              If sqlstt = conSqlFileMismatch;
                 dsWDELPdef = dsWDELPout;
                 zFileMismatch('WDELP');
              Endif;
           Other;
              Exec sql
               update WDELP
                set ROW = :dsWDELPout
                where rrn(WDELP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWDELPdef = dsWDELPout;
                 zFileMismatch('WDELP':decRRN);
              Endif;
           Endsl;
           Exsr DataSaveWMAHP;
           Exsr DataSaveWDXSP;
        Endfor;
       Endsr;
       //==================================================
       // Save data - WDXSP (Exluded States)
       //==================================================
       Begsr DataSaveWDXSP;
        Chain (intCo#:intFnd:intEmp#:intFyr) pxs00;
        If Not %Found;
           Setll (intCo#:intFnd:0:0) pxs00;
           Dou %Eof(WDXSL);
              Reade (intCo#:intFnd:0:0) pxs00;
              If %Eof;
                 Leave;
              EndIf;
              XSEmp# = intEmp#;
              XSFyr  = intFyr;
              XSIPgm = Q1PGM;
            //XSIUsr = 'CSPORTAL';
              XSIUsr = strEntryPointGlobal;
              XSIDt  = sysDate7;
              XSPgm  = Q1PGM;
              XSUser = inUser;
              XSTime = sysTime6;
              XSDt   = XSIDt;
              Write pxs00;
           Enddo;
        Endif;
       Endsr;
       //==================================================
       // Save data - WIAPP (Application Master)
       //==================================================
       Begsr DataSaveWIAPP;
        Reset decRRN;
        dsWIAPP.APco#  = intCo#;
        dsWIAPP.APuser = inUser;
        dsWIAPP.APpgm  = Q1pgm;
        dsWIAPP.APdt   = sysDate7;
        dsWIAPP.APtime = sysTime6;
        If intApp# = 0;
           intApp# = GetNextApp#();
        Endif;
        dsWIAPP.APapp# = intApp#;
        Exec Sql
         Select rrn(WIAPP), APiusr, APipgm, APidt, APitim
          Into :decRRN, :dsWIAPP.APiusr, :dsWIAPP.APipgm,
           :dsWIAPP.APidt, :dsWIAPP.APitim
          From WIAPP
          Where APco# = :intCo#
           And APapp# = :intApp#;
        If decRRN = 0;
         //dsWIAPP.APiusr = 'CSPORTAL';
           dsWIAPP.APiusr = strEntryPointGlobal;
           dsWIAPP.APipgm = Q1pgm;
           dsWIAPP.APidt  = sysDate7;
           dsWIAPP.APitim = %Dec(%Time():*hms);
           Exec SQL Insert Into WIAPP Values(:dsWIAPP);
           If sqlstt = conSqlFileMismatch;
              dsWIAPPdef = dsWIAPP;
              zFileMismatch('WIAPP');
           Endif;
        Else;
           Exec SQL
            Update WIAPP Set Row = :dsWIAPP
            Where rrn(WIAPP) = :decRRN;
           If sqlstt = conSqlFileMismatch;
              dsWIAPPdef = dsWIAPP;
              zFileMismatch('WIAPP':decrrn);
           Endif;
        Endif;
       Endsr;
       //==================================================
       // Save data - WIAPPANS (Application Answers)
       //==================================================
       Begsr DataSaveWIAPPANS;
        Exec SQL
         Update WIAPPANS
         Set APuser = 'DELETEAPP'
         Where APapp# = :intApp#
         And APfnd  = :intFnd
         And APemp# = :intEmp#
         And APfyr  = :intFyr;
        For idx = 1 to maxSize;
           Select;
           When dsWIAPPANS(idx).APid# = 0;
              Iter;
           When dsWIAPPANS(idx).APresp = *blanks;
              Iter;
           Endsl;
           decId# = dsWIAPPANS(idx).APid#;
           Reset decRRN;
           Reset dsWIAPPANSout;
           Exec Sql
            Select * into :dsWIAPPANSout
            From WIAPPANS
            Where APco#  = :intCo#
              And APapp# = :intApp#
              And APfyr  = :intFyr
              And APid#  = :decId#;
           isUpdate = IsValidFetch(sqlstt);
           dsWIAPPANS(idx).APiusr = dsWIAPPANSout.APiusr;
           dsWIAPPANS(idx).APipgm = dsWIAPPANSout.APipgm;
           dsWIAPPANS(idx).APidt  = dsWIAPPANSout.APidt;
           dsWIAPPANS(idx).APitim = dsWIAPPANSout.APitim;
           dsWIAPPANSout = dsWIAPPANS(idx);
           dsWIAPPANSout.APco#  = intCo#;
           dsWIAPPANSout.APapp# = intApp#;
           dsWIAPPANSout.APfnd  = intFnd;
           dsWIAPPANSout.APemp# = intEmp#;
           dsWIAPPANSout.APfyr  = intFyr;
           dsWIAPPANSout.APuser = inUser;
           dsWIAPPANSout.APpgm  = Q1pgm;
           dsWIAPPANSout.APdt   = sysDate7;
           dsWIAPPANSout.APtime = sysTime6;
           If isUpdate;
              Exec SQL
               Update WIAPPANS Set Row = :dsWIAPPANSout
               Where APco#  = :intCo#
                And APapp# = :intApp#
                And APfyr  = :intFyr
                And APid#  = :decId#;
              If sqlstt = conSqlFileMismatch;
                 Exec SQL
                  select rrn(WIAPPANS) into :decRRN from WIAPPANS
                   where APco#  = :intCo#
                    and APapp# = :intApp#
                    and APfyr  = :intFyr
                    and APid#  = :decId#;
                 dsWIAPPANSdef = dsWIAPPANSout;
                 zFileMismatch('WIAPPANS':decRRN);
              Endif;
           Else;
              dsWIAPPANSout.APiusr = strEntryPointGlobal;
              dsWIAPPANSout.APipgm = Q1pgm;
              dsWIAPPANSout.APidt  = sysDate7;
              dsWIAPPANSout.APitim = sysTime6;
              Exec Sql
               Insert into WIAPPANS Values(:dsWIAPPANSout);
              If sqlstt = conSqlFileMismatch;
                 dsWIAPPANSdef = dsWIAPPANSout;
                 zFileMismatch('WIAPPANS');
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WIAPPLOS (Application Loss Detail)
       //==================================================
       Begsr DataSaveWIAPPLOS;
        Exec Sql
         Update WIAPPLOS
         Set APSTAT = 'I'
         Where APco# = :intCo#
         And APapp# = :intApp#;
        For idx = 1 to 10;
           If dsWIAPPLOS(idx).APyear = 0;
              Iter;
           Endif;
           Reset decRRN;
           Reset dsWIAPPLOSout;
           decYear = dsWIAPPLOS(idx).APyear;
           Exec Sql
            Select * into :dsWIAPPLOSout
            From WIAPPLOS
            Where APco# = :intCo#
              And APapp# = :intApp#
              And APyear = :decYear;
           isUpdate = IsValidFetch(sqlstt);
           dsWIAPPLOS(idx).APiusr = dsWIAPPLOSout.APiusr;
           dsWIAPPLOS(idx).APipgm = dsWIAPPLOSout.APipgm;
           dsWIAPPLOS(idx).APidt  = dsWIAPPLOSout.APidt;
           dsWIAPPLOS(idx).APitim = dsWIAPPLOSout.APitim;
           dsWIAPPLOSout  = dsWIAPPLOS(idx);
           dsWIAPPLOSout.APco#  = intCo#;
           dsWIAPPLOSout.APapp# = intApp#;
           dsWIAPPLOSout.APstat = 'A';
           dsWIAPPLOSout.APuser = inUser;
           dsWIAPPLOSout.APpgm  = Q1pgm;
           dsWIAPPLOSout.APdt   = sysDate7;
           dsWIAPPLOSout.APtime = sysTime6;
           If isUpdate;
              Exec SQL
               Update WIAPPLOS Set Row = :dsWIAPPLOSout
               Where APco# = :intCo#
                 And APapp# = :intApp#
                 And APyear = :decYear;
              If sqlstt = conSqlFileMismatch;
                 Exec SQL
                  select rrn(WIAPPLOS) into :decRRN from WIAPPLOS
                   where APco# = :intCo#
                    and APapp# = :intApp#
                    and APyear = :decYear;
                 dsWIAPPLOSdef = dsWIAPPLOSout;
                 zFileMismatch('WIAPPLOS':decRRN);
              Endif;
           Else;
              dsWIAPPLOSout.APiusr = strEntryPointGlobal;
              dsWIAPPLOSout.APipgm = Q1pgm;
              dsWIAPPLOSout.APidt  = sysDate7;
              dsWIAPPLOSout.APitim = sysTime6;
              Exec Sql
               Insert into WIAPPLOS Values(:dsWIAPPLOSout);
              If sqlstt = conSqlFileMismatch;
                 dsWIAPPLOSdef = dsWIAPPLOSout;
                 zFileMismatch('WIAPPLOS');
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WIEMP (Application Cross Reference)
       //==================================================
       Begsr DataSaveWIEMP;
        Reset decRRN;
        dsWIEMP.EMco#  = intCo#;
        dsWIEMP.EMapp# = intApp#;
        dsWIEMP.EMfnd  = intFnd;
        dsWIEMP.EMemp# = intEmp#;
        dsWIEMP.EMfyr  = GetFyr(intCo#:intFnd:dsWMEMPOut.EMefd1);
        intFyr = dsWIEMP.EMfyr;
        If dsWIEMP.EMfnd = selectedGroup#;
           dsWIEMP.EMcurrent = 'Y';
           Exsr DataSaveWIAPPANS;
        Else;
           dsWIEMP.EMcurrent = ' ';
        Endif;
        dsWIEMP.EMuser = inUser;
        dsWIEMP.EMpgm  = Q1pgm;
        dsWIEMP.EMdt   = sysDate7;
        dsWIEMP.EMtime = sysTime6;
        Reset decRRN;
        Exec Sql
         Select rrn(WIEMP) into :decRRN
         From WIEMP
         Where EMco#  = :intCo#
           And EMapp# = :intApp#
           And EMfnd  = :intFnd
           And EMemp# = :intEmp#
           And EMfyr  = :dsWIEMP.EMfyr;
        If decRRN = 0;
           dsWIEMP.EMstat = 'A';
         //dsWIEMP.EMiusr = 'CSPORTAL';
           dsWIEMP.EMiusr = strEntryPointGlobal;
           dsWIEMP.EMipgm = Q1pgm;
           dsWIEMP.EMidt  = sysDate7;
           dsWIEMP.EMitim = sysTime6;
           Exec Sql
            Insert into WIEMP Values(:dsWIEMP);
           If sqlstt = conSqlFileMismatch;
              dsWIEMPdef = dsWIEMP;
              zFileMismatch('WIEMP');
           Endif;
        Else;
           Exec SQL
            Update WIEMP Set Row = :dsWIEMP
            Where rrn(WIEMP) = :decRRN;
           If sqlstt = conSqlFileMismatch;
              dsWIEMPdef = dsWIEMP;
              zFileMismatch('WIEMP':decRRN);
           Endif;
        Endif;
       EndSr;
       //==================================================
       // Save data - WIGSP
       //==================================================
       Begsr DataSaveWIGSP;
        // Only process for state reporting division
        If dsWMEMPOut.EMstRp <> 'Y';
           Leavesr;
        Endif;
        For ig1 = 1 to %elem(dsGradient);
           Select;
           When dsGradient(ig1) = dsGradientInz;
              Iter;
           Endsl;
           dsWIGSP = dsGradient(ig1).WIGSP;
           dsWIGSP.GSco#  = intCo#;
           dsWIGSP.GSfnd  = intFnd;
           dsWIGSP.GSemp# = intEmp#;
           If dsWIGSP.GSfyr = *zero;
              dsWIGSP.GSfyr = GetFyr(intCo#:intFnd:dsWMEMPOut.EMefd1);
           Endif;
           intFyr = dsWIGSP.GSfyr;
           dsWIGSP.GSuser = inUser;
           dsWIGSP.GSpgm  = Q1pgm;
           dsWIGSP.GSdt   = sysDate7;
           dsWIGSP.GStime = sysTime6;
           Reset dsWIGSPpre;
           Exec sql
            select * into :dsWIGSPpre
             from WIGSP
             where GSco#  = :intCo#
              and GSfnd  = :intFnd
              and GSemp# = :intEmp#
              and GSfyr  = :intFyr;
           If not IsValidFetch(sqlstt);
              dsWIGSP.GSiusr = dsWIGSP.GSuser;
              dsWIGSP.GSipgm = dsWIGSP.GSpgm;
              dsWIGSP.GSidt  = dsWIGSP.GSdt;
              dsWIGSP.GSitim = dsWIGSP.GStime;
              Exec sql
               insert into WIGSP values(:dsWIGSP);
              If sqlstt = conSqlFileMismatch;
                 dsWIGSPdef = dsWIGSP;
                 zFileMismatch('WIGSP');
              Endif;
           Else;
              dsWIGSP.GSiusr = dsWIGSPpre.GSiUsr;
              dsWIGSP.GSipgm = dsWIGSPpre.GSiPgm;
              dsWIGSP.GSidt  = dsWIGSPpre.GSiDt;
              dsWIGSP.GSitim = dsWIGSPpre.GSiTim;
              Exec sql
               update WIGSP set row = :dsWIGSP
                where GSco#  = :intCo#
                 and GSfnd  = :intFnd
                 and GSemp# = :intEmp#
                 and GSfyr  = :intFyr;
              If sqlstt = conSqlFileMismatch;
                 Exec sql
                  select rrn(WIGSP) into :decRRN from WIGSP
                   where GSco#  = :intCo#
                    and GSfnd  = :intFnd
                    and GSemp# = :intEmp#
                    and GSfyr  = :intFyr;
                 dsWIGSPdef = dsWIGSP;
                 zFileMismatch('WIGSP');
              Endif;
           Endif;
           Exec sql
            delete from WIGSPDRV
              where GSco#  = :intCo#
               and GSfnd = :intFnd
               and GSemp# = :intEmp#
               and GSfyr = :intFyr;
           For ig2 = 1 to %elem(dsGradient.WIGSPDRV);
              Select;
              When dsGradient(ig1).WIGSPDRV(ig2).GSdrvVal = *zero and
               dsGradient(ig1).WIGSPDRV(ig2).GSdrvName = *blank;
                 Iter;
              Endsl;
              dsWIGSPDRV = dsGradient(ig1).WIGSPDRV(ig2);
              dsWIGSPDRV.GSco#  = intCo#;
              dsWIGSPDRV.GSfnd  = intFnd;
              dsWIGSPDRV.GSemp# = intEmp#;
              dsWIGSPDRV.GSfyr  = intFyr;
              dsWIGSPDRV.GSuser = inUser;
              dsWIGSPDRV.GSpgm  = Q1pgm;
              dsWIGSPDRV.GSdt   = sysDate7;
              dsWIGSPDRV.GStime = sysTime6;
              dsWIGSPDRV.GSiusr = dsWIGSPDRV.GSuser;
              dsWIGSPDRV.GSipgm = dsWIGSPDRV.GSpgm;
              dsWIGSPDRV.GSidt  = dsWIGSPDRV.GSdt;
              dsWIGSPDRV.GSitim = dsWIGSPDRV.GStime;
              Exec sql
               insert into WIGSPDRV values(:dsWIGSPDRV);
              If sqlstt = conSqlFileMismatch;
                 dsWIGSPDRVdef = dsWIGSPDRV;
                 zFileMismatch('WIGSPDRV');
              Endif;
           Endfor;
        Endfor;
       Endsr;
       //***  Begin Add  ***  08/05/21  ********************
       //==================================================
       // Save data - WIVSP
       //==================================================
       Begsr DataSaveWIVSP;
        // Only process for state reporting division
        If dsWMEMPOut.EMstRp <> 'Y';
           Leavesr;
        Endif;
        For ig1 = 1 to %elem(dsValen);
           Select;
           When dsValen(ig1).WIVSP = dsValenInz.WIVSP;
              Iter;
           Endsl;
           dsWIVSP = dsValen(ig1).WIVSP;
           dsWIVSP.VSco#  = intCo#;
           dsWIVSP.VSfnd  = intFnd;
           dsWIVSP.VSemp# = intEmp#;
           If dsWIVSP.VSfyr = *zero;
              dsWIVSP.VSfyr = GetFyr(intCo#:intFnd:dsWMEMPOut.EMefd1);
           Endif;
           intFyr = dsWIVSP.VSfyr;
           dsWIVSP.VSuser = inUser;
           dsWIVSP.VSpgm  = Q1pgm;
           dsWIVSP.VSdt   = sysDate7;
           dsWIVSP.VStime = sysTime6;
           Reset dsWIVSPpre;
           Exec sql
            select * into :dsWIVSPpre
             from WIVSP
             where VSco#  = :intCo#
              and VSfnd  = :intFnd
              and VSemp# = :intEmp#
              and VSfyr  = :intFyr;
           If not IsValidFetch(sqlstt);
              dsWIVSP.VSiusr = dsWIVSP.VSuser;
              dsWIVSP.VSipgm = dsWIVSP.VSpgm;
              dsWIVSP.VSidt  = dsWIVSP.VSdt;
              dsWIVSP.VSitim = dsWIVSP.VStime;
              Exec sql
               insert into WIVSP values(:dsWIVSP);
              If sqlstt = conSqlFileMismatch;
                 dsWIVSPdef = dsWIVSP;
                 zFileMismatch('WIVSP');
              Endif;
           Else;
              dsWIVSP.VSiusr = dsWIVSPpre.VSiUsr;
              dsWIVSP.VSipgm = dsWIVSPpre.VSiPgm;
              dsWIVSP.VSidt  = dsWIVSPpre.VSiDt;
              dsWIVSP.VSitim = dsWIVSPpre.VSiTim;
              Exec sql
               update WIVSP set row = :dsWIVSP
                where VSco#  = :intCo#
                 and VSfnd  = :intFnd
                 and VSemp# = :intEmp#
                 and VSfyr  = :intFyr;
              If sqlstt = conSqlFileMismatch;
                 Exec sql
                  select rrn(WIVSP) into :decRRN from WIVSP
                   where VSco#  = :intCo#
                    and VSfnd  = :intFnd
                    and VSemp# = :intEmp#
                    and VSfyr  = :intFyr;
                 dsWIVSPdef = dsWIVSP;
                 zFileMismatch('WIVSP');
              Endif;
           Endif;
           Exec sql
            delete from WIVSPEXP
              where VSco#  = :intCo#
               and VSfnd = :intFnd
               and VSemp# = :intEmp#
               and VSfyr = :intFyr;
           For ig2 = 1 to %elem(dsValen.WIVSPEXP);
              Select;
              When dsValen(ig1).WIVSPEXP(ig2).VScategory = *blank;
                 Iter;
              Endsl;
              dsWIVSPEXP = dsValen(ig1).WIVSPEXP(ig2);
              dsWIVSPEXP.VSco#  = intCo#;
              dsWIVSPEXP.VSfnd  = intFnd;
              dsWIVSPEXP.VSemp# = intEmp#;
              dsWIVSPEXP.VSfyr  = intFyr;
              dsWIVSPEXP.VSuser = inUser;
              dsWIVSPEXP.VSpgm  = Q1pgm;
              dsWIVSPEXP.VSdt   = sysDate7;
              dsWIVSPEXP.VStime = sysTime6;
              dsWIVSPEXP.VSiusr = dsWIVSPEXP.VSuser;
              dsWIVSPEXP.VSipgm = dsWIVSPEXP.VSpgm;
              dsWIVSPEXP.VSidt  = dsWIVSPEXP.VSdt;
              dsWIVSPEXP.VSitim = dsWIVSPEXP.VStime;
              Exec sql
               insert into WIVSPEXP values(:dsWIVSPEXP);
              If sqlstt = conSqlFileMismatch;
                 dsWIVSPEXPdef = dsWIVSPEXP;
                 zFileMismatch('WIVSPEXP');
              Endif;
           Endfor;
        Endfor;
       Endsr;
       //***  End   Add  ***  08/05/21  ********************
       //==================================================
       // Save data - WMAHP
       //==================================================
       Begsr DataSaveWMAHP;
        Reset decRRN;
        Exec Sql
         Select rrn(WMAHP) into :decRRN
         From WMAHP
         Where AHco#  = :dsWDELPout.ELco#
           And AHfnd  = :dsWDELPout.ELfnd
           And AHemp# = :dsWDELPout.ELemp#
           And AHdiv  = :dsWDELPout.ELdiv
           And AHfyr  = :dsWDELPout.ELfyr;
        If decRRN = 0;
           dsWMAHPOut.AHCo#  = dsWDELPout.ELco#;
           dsWMAHPOut.AHFnd  = dsWDELPout.ELfnd;
           dsWMAHPOut.AHEmp# = dsWDELPout.ELemp#;
           dsWMAHPOut.AHDiv  = dsWDELPout.ELdiv;
           dsWMAHPOut.AHFyr  = dsWDELPout.ELfyr;
           dsWMAHPOut.AHAgt# = intAgt;
           dsWMAHPOut.AHProd = intProducer;
           dsWMAHPOut.AHRt#  = intRate;
           If sysDate7 > dsWDELPout.ELpfdt;
              dsWMAHPOut.AHEFdt = dsWDELPout.ELpfdt;
           Else;
              dsWMAHPOut.AHEFdt = sysDate7;
           Endif;
           dsWMAHPOut.AHStat = 'A';
           dsWMAHPOut.AHPgm  = q1pgm;
           dsWMAHPOut.AHUser = inUser;
           dsWMAHPOut.AHDt   = sysDate7;
           dsWMAHPOut.AHTime = sysTime6;
         //dsWMAHPOut.AHiusr = 'CSPORTAL';
           dsWMAHPOut.AHiusr = strEntryPointGlobal;
           dsWMAHPOut.AHipgm = Q1pgm;
           dsWMAHPOut.AHidt  = sysDate7;
           dsWMAHPOut.AHitm  = sysTime6;
           Exec Sql
            Insert into WMAHP Values(:dsWMAHPOut);
           If sqlstt = conSqlFileMismatch;
              dsWMAHPdef = dsWMAHPout;
              zFileMismatch('WMAHP');
           Endif;
        Else;
           Reset dsWMAHPOut;
           Exec Sql
            Select * Into :dsWMAHPout
             From WMAHP
             Where AHco#  = :dsWDELPout.ELco#
               And AHfnd  = :dsWDELPout.ELfnd
               And AHemp# = :dsWDELPout.ELemp#
               And AHdiv  = :dsWDELPout.ELdiv
               And AHfyr  = :dsWDELPout.ELfyr;
           dsWMAHPOut.AHAgt# = intAgt;
           dsWMAHPOut.AHProd = intProducer;
           dsWMAHPOut.AHRt#  = intRate;
           If sysDate7 > dsWDELPout.ELpfdt;
              dsWMAHPOut.AHEFdt = dsWDELPout.ELpfdt;
           Else;
              dsWMAHPOut.AHEFdt = sysDate7;
           Endif;
           dsWMAHPOut.AHStat = 'A';
           dsWMAHPOut.AHPgm  = q1pgm;
           dsWMAHPOut.AHUser = inUser;
           dsWMAHPOut.AHDt   = sysDate7;
           dsWMAHPOut.AHTime = sysTime6;
           Exec SQL
            Update WMAHP Set Row = :dsWMAHPOut
            Where rrn(WMAHP) = :decRRN;
           If sqlstt = conSqlFileMismatch;
              dsWMAHPdef = dsWMAHPout;
              zFileMismatch('WMAHP':decRRN);
           Endif;
        Endif;
       Endsr;
       //==================================================
       // Save data - WMALP (Location Master)
       //==================================================
       Begsr DataSaveWMALP;
        If intLocIdx < 1;
           Leavesr;
        Endif;
        Sorta %subarr(dsWMALP(*).ALelId:1:intLocIdx);
        For idx = 1 to intLocIdx;
           Reset str2a;
           dsWMALP(idx).ALfnd  = intFnd;
           dsWMALP(idx).ALemp# = intEmp#;
           dsWMALPOut = dsWMALP(idx);
         //***  Begin Add  ***  07/11/22  *****************
           Select;
           // Determin if auto-location should be used, based on the group setting
           When dsWMALPout.ALelTp = '*' and dsWMFNP.FNaCLR = *blank; // otherwise E/L
              Iter;
           When dsWMALPout.ALelTp = '*';
              dsWMALPout.ALelTp = dsWMFNP.FNaCLR;
           Endsl;
         //***  End   Add  ***  07/11/22  *****************
           If dsWMALPout.ALelID = *blanks or dsWMALPout.ALelID = *zeros;
              Reset decELID;
              Dou strELID <> *blanks;
                 strElid = %editc(decELID:'X');
                 decELID += 1;
                 Reset decRRN;
                 Exec Sql
                  Select rrn(WMALP) into :decRRN
                   From WMALP
                   Where ALco#  = :dsWMALPOut.ALco#
                    And ALfnd  = :dsWMALPOut.ALfnd
                    And ALemp# = :dsWMALPOut.ALemp#
                    And ALdiv  = :dsWMALPOut.ALdiv
                    And ALelid = :strELID;
                 If decRRN <> 0;
                    Clear strELID;
                 Endif;
              Enddo;
              dsWMALPout.ALelID = strELID;
           Endif;
           Select;
           When dsWMALPOut.ALpad1 <> *Blanks;
              intZip = dsWMALP(idx).ALpzip/10000;
              Exec Sql
               Select CZcnty Into :str2a
                From WMCZP
                Where CZco# = :intCo#
                 And CZst = :dsWMALPOut.ALpst
                 And CZzip = :intZip;
           When dsWMALPOut.ALmad1 <> *Blanks;
              intZip = dsWMALP(idx).ALmzip/10000;
              Exec Sql
               Select CZcnty Into :str2a
                From WMCZP
                Where CZco# = :intCo#
                 And CZst = :dsWMALPOut.ALmst
                 And CZzip = :intZip;
           EndSl;
           dsWMALPOut.ALcnty = str2a;
           dsWMALPOut.ALuser = inUser;
           dsWMALPOut.ALpgm  = Q1pgm;
           dsWMALPOut.ALdt   = sysDate7;
           dsWMALPOut.ALtime = sysTime6;

         // Ensure location address for automatically created location stays
         // updated to match employer master.
           If dsWMALPOut.ALncci = 'N';

              For idy = 1 to intDivIdx;
                 If dsWMEMPSave(idy).EMdiv = dsWMALPOut.ALdiv;
                    Leave;
                 Endif;
              EndFor;

              If dsWMALPOut.ALmad1 = dsWMEMPSave(idy).EMmad1 and
               dsWMALPOut.ALmad2   = dsWMEMPSave(idy).EMmad2 and
               dsWMALPOut.ALmcty   = dsWMEMPSave(idy).EMmcty and
               dsWMALPOut.ALmst    = dsWMEMPSave(idy).EMmst  and
               dsWMALPOut.ALmzip   = dsWMEMPSave(idy).EMmzip and
               dsWMALPOut.ALpad1   = dsWMEMPSave(idy).EMpad1 and
               dsWMALPOut.ALpad2   = dsWMEMPSave(idy).EMpad2 and
               dsWMALPOut.ALpcty   = dsWMEMPSave(idy).EMpcty and
               dsWMALPOut.ALpst    = dsWMEMPSave(idy).EMpst  and
               dsWMALPOut.ALpzip   = dsWMEMPSave(idy).EMpzip;

                 dsWMALPOut.ALmad1 = dsWMEMP(idy).EMmad1;
                 dsWMALPOut.ALmad2 = dsWMEMP(idy).EMmad2;
                 dsWMALPOut.ALmcty = dsWMEMP(idy).EMmcty;
                 dsWMALPOut.ALmst  = dsWMEMP(idy).EMmst;
                 dsWMALPOut.ALmzip = dsWMEMP(idy).EMmzip;
                 dsWMALPOut.ALpad1 = dsWMEMP(idy).EMpad1;
                 dsWMALPOut.ALpad2 = dsWMEMP(idy).EMpad2;
                 dsWMALPOut.ALpcty = dsWMEMP(idy).EMpcty;
                 dsWMALPOut.ALpst  = dsWMEMP(idy).EMpst;
                 dsWMALPOut.ALpzip = dsWMEMP(idy).EMpzip;

              Endif;

           Endif;

           Reset decRRN;
           Exec Sql
            Select rrn(WMALP) into :decRRN
            From WMALP
            Where ALco#  = :dsWMALPOut.ALco#
              And ALfnd  = :dsWMALPOut.ALfnd
              And ALemp# = :dsWMALPOut.ALemp#
              And ALdiv  = :dsWMALPOut.ALdiv
              And ALelid = :dsWMALPOut.ALelid;
           If decRRN = 0;
            //dsWMALPOut.ALiusr = 'CSPORTAL';
              dsWMALPOut.ALiusr = strEntryPointGlobal;
              dsWMALPOut.ALipgm = Q1pgm;
              dsWMALPOut.ALidt  = sysDate7;
              dsWMALPOut.ALitm  = sysTime6;
              Exec Sql
               Insert into WMALP Values(:dsWMALPOut);
              If sqlstt = conSqlFileMismatch;
                 dsWMALPdef = dsWMALPout;
                 zFileMismatch('WMALP');
              Endif;
           Else;
              Exec SQL
               Update WMALP Set Row = :dsWMALPOut
               Where rrn(WMALP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMALPdef = dsWMALPout;
                 zFileMismatch('WMALP':decRRN);
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WMECP (Employer Contacts)
       //==================================================
       Begsr DataSaveWMECP;
        For idx = 1 to intConIdx;
           Reset decRRN;
           dsWMECP(idx).ECfnd  = intFnd;
           dsWMECP(idx).ECemp# = intEmp#;
           dsWMECPOut = dsWMECP(idx);
           dsWMECPOut.ECuser = inUser;
           dsWMECPOut.ECpgm  = Q1pgm;
           dsWMECPOut.ECdt   = sysDate7;
           dsWMECPOut.ECtime = sysTime6;
           Exec Sql
            Select rrn(WMECP) into :decRRN
            From WMECP
            Where ECco#  = :dsWMECPOut.ECco#
              And ECfnd  = :dsWMECPOut.ECfnd
              And ECemp# = :dsWMECPOut.ECemp#
              And ECdiv  = :dsWMECPOut.ECdiv
              And EClsn  = :dsWMECPOut.EClsn
              And ECfsn  = :dsWMECPOut.ECfsn
              And ECmid  = :dsWMECPOut.ECmid
              And ECposn = :dsWMECPOut.ECposn
              And ECpcnt = :dsWMECPOut.ECpcnt;
           If decRRN = 0;
            //dsWMECPOut.ECiusr = 'CSPORTAL';
              dsWMECPOut.ECiusr = strEntryPointGlobal;
              dsWMECPOut.ECipgm = Q1pgm;
              dsWMECPOut.ECidt  = sysDate7;
              dsWMECPOut.ECitim  = sysTime6;
              Exec Sql
               Insert into WMECP Values(:dsWMECPOut);
              If sqlstt = conSqlFileMismatch;
                 dsWMECPdef = dsWMECPout;
                 zFileMismatch('WMECP');
              Endif;
           Else;
              Exec SQL
               Update WMECP Set Row = :dsWMECPOut
               Where rrn(WMECP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMECPdef = dsWMECPout;
                 zFileMismatch('WMECP':decRRN);
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WMEMP (Employer Master)
       //==================================================
       Begsr DataSaveWMEMP;
        For idz = 1 to intDivIdx;
           Reset decRRN;
           Reset str2a;
           dsWMEMP(idz).EMfnd  = intFnd;
           dsWMEMP(idz).EMuwtr = intAgtUwtr;
           If intEmp# = 0;
              intEmp# = GetNextQte#(intCo#:intFnd);
           EndIf;
           dsWMEMP(idz).EMemp# = intEmp#;
          // Lock record if this is a portal quote
           If isSavingApp Or isAddingApp;
              // Check if quote should be unlocked or locked
              Reset dsWAQTPOut;
              Exec SQL
               Select * Into :dsWAQTPOut From WAQTP
                Where QTco# = :intCo#
                 And QTfnd = :intFnd
                 And QTemp# = :intEmp#
                 And QTdiv = :intDiv;
              dsWMEMP(idz).EMfqlk =
               SetFQLockSts(intCo#:dsWAQTPout.QTType:strEntryPointGlobal:'P');
              dsWMEMP(idz).EMapp# = intApp#;
           Endif;
           dsWMEMPOut = dsWMEMP(idz);
           Select;
           When dsWMEMPOut.EMpad1 <> *Blanks;
              intZip = dsWMEMP(idz).EMpzip/10000;
              Exec Sql
               Select CZcnty Into :str2a
                From WMCZP
                Where CZco# = :intCo#
                 And CZst = :dsWMEMPOut.EMpst
                 And CZzip = :intZip;
              //*** Beg Add *** 09/04/24 **************************************
              If clientId = '102';
                 Exec Sql
                  Select CYcnty Into :str2a
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :dsWMEMPOut.EMpst
                    And CYcty = :dsWMEMPOut.EMpCty
                    And CYcnty <> ' '
                   Limit(1);
              Endif;
              //*** End Add *** 09/04/24 **************************************
           When dsWMEMPOut.EMmad1 <> *Blanks;
              intZip = dsWMEMP(idz).EMmzip/10000;
              Exec Sql
               Select CZcnty Into :str2a
                From WMCZP
                Where CZco# = :intCo#
                 And CZst = :dsWMEMPOut.EMmst
                 And CZzip = :intZip;
              //*** Beg Add *** 09/04/24 **************************************
              If clientId = '102';
                 Exec Sql
                  Select CYcnty Into :str2a
                   From WMCYP
                   Where CYco# = :intCo#
                    And CYst = :dsWMEMPOut.EMmst
                    And CYcty = :dsWMEMPOut.EMmCty
                    And CYcnty <> ' '
                   Limit(1);
              Endif;
              //*** End Add *** 09/04/24 **************************************
           EndSl;
           dsWMEMPOut.EMcnty = str2a;
           dsWMEMPOut.EMuser = inUser;
           dsWMEMPOut.EMpgm  = Q1pgm;
           dsWMEMPOut.EMdt   = sysDate7;
           dsWMEMPOut.EMtime = sysTime6;
           Exec Sql
            Select rrn(WMEMP) Into :decRRN
            From WMEMP
            Where EMco#  = :dsWMEMPOut.EMco#
              And EMfnd  = :dsWMEMPOut.EMfnd
              And EMemp# = :dsWMEMPOut.EMemp#
              And EMdiv  = :dsWMEMPOut.EMdiv;
           If decRRN = 0;
            //dsWMEMPOut.EMiusr = 'CSPORTAL';
              dsWMEMPOut.EMiusr = strEntryPointGlobal;
              dsWMEMPOut.EMipgm = Q1pgm;
              dsWMEMPOut.EMidt  = sysDate7;
              dsWMEMPOut.EMitim = sysTime6;
            //* If no physical address entered on initial add, copy mailing over.
            //* Control point for this action to be added later.
              If dsWMEMPOut.EMPad1 = ' ';
                 dsWMEMPOut.EMPad1 = dsWMEMPOut.EMMad1;
                 dsWMEMPOut.EMPad2 = dsWMEMPOut.EMMad2;
                 dsWMEMPOut.EMPcty = dsWMEMPOut.EMMcty;
                 dsWMEMPOut.EMPst  = dsWMEMPOut.EMMst;
                 dsWMEMPOut.EMPzip = dsWMEMPOut.EMMzip;
              Endif;
            //* If no mailing address entered on initial add, copy physical over.
            //* Control point for this action to be added later.
              If dsWMEMPOut.EMMad1 = ' ';
                 dsWMEMPOut.EMMad1 = dsWMEMPOut.EMPad1;
                 dsWMEMPOut.EMMad2 = dsWMEMPOut.EMPad2;
                 dsWMEMPOut.EMMcty = dsWMEMPOut.EMPcty;
                 dsWMEMPOut.EMMst  = dsWMEMPOut.EMPst;
                 dsWMEMPOut.EMMzip = dsWMEMPOut.EMPzip;
              Endif;
              Exec Sql
               Insert into WMEMP Values(:dsWMEMPOut);
              If sqlstt = conSqlFileMismatch;
                 dsWMEMPdef = dsWMEMPout;
                 zFileMismatch('WMEMP');
              Endif;
           Else;
              Exec SQL
               Update WMEMP Set Row = :dsWMEMPOut
               Where rrn(WMEMP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMEMPdef = dsWMEMPout;
                 zFileMismatch('WMEMP':decRRN);
              Endif;
           Endif;
         //***  Begin Del  ***  07/11/22  ****************
         //Exsr LoadAutoLocation;
         //***  End   Del  ***  07/11/22  ****************
           Select;
           When Not(isSavingApp) And Not(isAddingApp);
           Other;
              Exsr DataSaveWIEMP;
              Exsr DataSaveWIGSP;
            //***  Begin Add  ***  08/05/21  ***************
              Exsr DataSaveWIVSP;
            //***  End   Add  ***  08/05/21  ***************
           Endsl;
        EndFor;
       Endsr;
       //==================================================
       // Save data - WMEPP
       //==================================================
       Begsr DataSaveWMEPP;
        For idx = 1 to intPayIdx;
           If dsWMEPP(idx).EPclas = 0;
              Iter;
           Endif;
           Reset decRRN;
           dsWMEPP(idx).EPfnd  = intFnd;
           dsWMEPP(idx).EPemp# = intEmp#;
           dsWMEPPout = dsWMEPP(idx);
           dsWMEPPout.EPpgm  = q1pgm;
           dsWMEPPout.EPuser = inUser;
           dsWMEPPout.EPdt   = DateTo7(SystemDate());
           dsWMEPPout.EPtime = TimeTo6(SystemTime());
        //*** begin add ***  08/26/22  **************************************
           dsWMEPPout.EPstat = 'A';
        //*** end add *****  08/26/22  **************************************
           Exec sql
            select rrn(WMEPP) into :decRRN
             from WMEPP
             where EPco# = :dsWMEPPout.EPco#
              and EPfnd  = :dsWMEPPout.EPfnd
              and EPemp# = :dsWMEPPout.EPemp#
              and EPdiv  = :dsWMEPPout.EPdiv
              and EPfyr  = :dsWMEPPout.EPfyr
              and EPefDt = :dsWMEPPout.EPefDt
              and EPclas = :dsWMEPPout.EPclas
              and EPspec = :dsWMEPPout.EPspec
              and EPext  = :dsWMEPPout.EPext;
           Select;
           When decRRN = 0;
              dsWMEPPout.EPiPgm = dsWMEPPout.EPpgm;
            //dsWMEPPout.EPiUsr = 'CSPORTAL';
              dsWMEPPout.EPiUsr = strEntryPointGlobal;
              dsWMEPPout.EPidt  = dsWMEPPout.EPdt;
              dsWMEPPout.EPiTm  = dsWMEPPout.EPtime;
              dsWMEPPout.EPstat = 'A';
              dsWMEPPout.EPsafl = 'E';
              Exec sql
               insert into WMEPP
                values(:dsWMEPPout);
              If sqlstt = conSqlFileMismatch;
                 dsWMEPPdef = dsWMEPPout;
                 zFileMismatch('WMEPP');
              Endif;
           Other;
              Exec sql
               update WMEPP
                set ROW = :dsWMEPPout
                where rrn(WMEPP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMEPPdef = dsWMEPPout;
                 zFileMismatch('WMEPP':decRRN);
              Endif;
           Endsl;
        Endfor;
       Endsr;
       //==================================================
       // Save data - WMLDP
       //==================================================
       Begsr DataSaveWMLDP;
        For idx = 1 to intDtlIdx;
           Select;
           When dsWMLDP(idx).LDefd1 = *Zero;
              Iter;
           Endsl;
           Reset decRRN;
           dsWMLDP(idx).LDfnd  = intFnd;
           dsWMLDP(idx).LDemp# = intEmp#;
           dsWMLDP(idx).LDfyr  = intFyr;
           dsWMLDPout = dsWMLDP(idx);
           dsWMLDPout.LDpgm  = q1pgm;
           dsWMLDPout.LDuser = inUser;
           dsWMLDPout.LDdt   = DateTo7(SystemDate());
           dsWMLDPout.LDtime = TimeTo6(SystemTime());
           Exec sql
            select rrn(WMLDP) into :decRRN
             from WMLDP
             where LDco# = :dsWMLDPout.LDco#
              and LDfnd  = :dsWMLDPout.LDfnd
              and LDemp# = :dsWMLDPout.LDemp#
              and LDdiv  = :dsWMLDPout.LDdiv
              and LDfyr  = :dsWMLDPout.LDfyr;
           Select;
           When decRRN = 0;
              dsWMLDPout.LDiPgm = dsWMLDPout.LDpgm;
              dsWMLDPout.LDiUsr = dsWMLDPout.LDuser;
              dsWMLDPout.LDidt  = dsWMLDPout.LDdt;
              dsWMLDPout.LDiTim = dsWMLDPout.LDtime;
              Exec sql
               insert into WMLDP
                values(:dsWMLDPout);
              If sqlstt = conSqlFileMismatch;
                 dsWMLDPdef = dsWMLDPout;
                 zFileMismatch('WMLDP');
              Endif;
           Other;
              Exec sql
               update WMLDP
                set ROW = :dsWMLDPout
                where rrn(WMLDP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMLDPdef = dsWMLDPout;
                 zFileMismatch('WMLDP':decRRN);
              Endif;
           Endsl;
        Endfor;
       Endsr;
       //==========================================================
       // Save data - WMNBP (New Business Tracking)
       //==========================================================
       Begsr DataSaveWMNBP;
       //Save current values
        Exec SQL
         Select * Into :dsWMNBPOut From WMNBP
          Where NBco# = :intCo#
           And NBfnd = :intFnd
           And NBEmp# = :intEmp#;
        If SqlStt = '02000';
           dsWMNBPOut.NBco#  = intCo#;
           dsWMNBPOut.NBfnd  = intFnd;
           dsWMNBPOut.NBemp# = intEmp#;
           dsWMNBPOut.NBrcdt = sysDate7;
           dsWMNBPOut.NBendt = sysDate7;
           dsWMNBPOut.NBipgm = Q1pgm;
         //dsWMNBPOut.NBiusr = 'CSPORTAL';
           dsWMNBPOut.NBiusr = strEntryPointGlobal;
           dsWMNBPOut.NBidt  = sysDate7;
           dsWMNBPOut.NBitm  = sysTime6;
           dsWMNBPOut.NBipgm = Q1pgm;
           dsWMNBPOut.NBuser = inUser;
           dsWMNBPOut.NBdt   = sysDate7;
           dsWMNBPOut.NBtime = sysTime6;
           Exec Sql Insert Into WMNBP Values(:dsWMNBPOut);
           If sqlstt = conSqlFileMismatch;
              dsWMNBPdef = dsWMNBPout;
              zFileMismatch('WMNBP');
           Endif;
        Endif;
       Endsr;
       //==========================================================
       // Save data - WMOOP (Owner/Officers)
       //==========================================================
       Begsr DataSaveWMOOP;
        For idx = 1 to intOffIdx;
           Reset decRRN;
           dsWMOOP(idx).OOfnd  = intFnd;
           dsWMOOP(idx).OOemp# = intEmp#;
           dsWMOOPOut = dsWMOOP(idx);
           dsWMOOPOut.OOuser = inUser;
           dsWMOOPOut.OOpgm  = Q1pgm;
           dsWMOOPOut.OOdt   = sysDate7;
           dsWMOOPOut.OOtime = sysTime6;
           Exec Sql
            Select rrn(WMOOP) into :decRRN
            From WMOOP
            Where OOco#  = :dsWMOOPOut.OOco#
              And OOfnd  = :dsWMOOPOut.OOfnd
              And OOemp# = :dsWMOOPOut.OOemp#
              And OOdiv  = :dsWMOOPOut.OOdiv
              And OOelid = :dsWMOOPOut.OOelid
              And OOseq# = :dsWMOOPout.OOseq#;
           If decRRN = 0;
              dsWMOOPOut.OOstat = 'A';
            //dsWMOOPOut.OOiusr = 'CSPORTAL';
              dsWMOOPOut.OOiusr = strEntryPointGlobal;
              dsWMOOPOut.OOipgm = Q1pgm;
              dsWMOOPOut.OOidt  = sysDate7;
              dsWMOOPOut.OOitim  = sysTime6;
              Exec Sql
               Insert into WMOOP Values(:dsWMOOPOut);
              If sqlstt = conSqlFileMismatch;
                 dsWMOOPdef = dsWMOOPout;
                 zFileMismatch('WMOOP');
              Endif;
           Else;
              Exec SQL
               Update WMOOP Set Row = :dsWMOOPOut
               Where rrn(WMOOP) = :decRRN;
              If sqlstt = conSqlFileMismatch;
                 dsWMOOPdef = dsWMOOPout;
                 zFileMismatch('WMOOP':decRRN);
              Endif;
           Endif;
        EndFor;
       Endsr;
       //==========================================================
       // Delete records still tagged for delete
       //==========================================================
       Begsr DltTaggedRecs;
        Exec SQL
         Delete From WIAPPLOS
          Where APuser = 'DELETEAPP';
        Exec SQL
         Delete From WIAPPANS
          Where APuser = 'DELETEAPP';
        If Not(%Open(WIEML));
           Open WIEML;
        Endif;
        Setll (intCo#:intApp#) pl100;
        Dou %Eof(WIEML);
           Reade (intCo#:intApp#) pl100;
           If %Eof;
              Leave;
           Endif;
           Exec SQL
            Delete From WDELP
             Where ELuser = 'DELETEAPP'
               And ELco#  = :L1Co#
               And ELfnd  = :L1fnd
               And ELemp# = :L1emp#;
           Exec SQL
            Delete From WDXSP
             Where XSuser = 'DELETEAPP'
               And XSco#  = :L1Co#
               And XSfnd  = :L1fnd
               And XSemp# = :L1emp#;
           Exec SQL
            Delete From WMAHP
             Where AHuser = 'DELETEAPP'
               And AHco#  = :L1Co#
               And AHfnd  = :L1fnd
               And AHemp# = :L1emp#;
           Exec SQL
            Delete From WMALP
             Where ALuser = 'DELETEAPP'
               And ALco#  = :L1Co#
               And ALfnd  = :L1fnd
               And ALemp# = :L1emp#;
           Exec SQL
            Delete From WMECP
             Where ECuser = 'DELETEAPP'
               And ECco#  = :L1Co#
               And ECfnd  = :L1fnd
               And ECemp# = :L1emp#;
           Exec SQL
            Delete From WMEMP
             Where EMuser = 'DELETEAPP'
               And EMco#  = :L1Co#
               And EMfnd  = :L1fnd
               And EMemp# = :L1emp#;
           Exec SQL
            Delete From WMEPP
             Where EPuser = 'DELETEAPP'
               And EPco#  = :L1Co#
               And EPfnd  = :L1fnd
               And EPemp# = :L1emp#;
           Exec SQL
            Delete From WMLDP
             Where LDuser = 'DELETEAPP'
               And LDco#  = :L1Co#
               And LDfnd  = :L1fnd
               And LDemp# = :L1emp#;
           Exec SQL
            Delete From WMOOP
             Where OOuser = 'DELETEAPP'
               And OOco#  = :L1Co#
               And OOfnd  = :L1fnd
               And OOemp# = :L1emp#;
           Exec SQL
            Delete From WMWVP
             Where WVuser = 'DELETEAPP'
               And WVco#  = :L1Co#
               And WVfnd  = :L1fnd
               And WVemp# = :L1emp#;
           If L1user = 'DELETEAPP';
              Delete pl100;
           Endif;
        Enddo;
        Close WIEML;
       Endsr;
       //==========================================================
       // Get group default information
       //==========================================================
       Begsr GetGroupDefaults;
        Reset dsWMFNP;
        Exec SQL
         select * into :dsWMFNP
          from WMFNP
          where FNco# = :intCo# And FNfnd = :intFnd;
        Reset strDfltOffice;
        Reset intDfltRegion;
        //*** Beg Add *** 09/04/24 ********************************************
        Reset strDfltLossRep;
        //*** End Add *** 09/04/24 ********************************************
        Exec SQL
         Select EXdoff, EXdrgn
          Into :strDfltOffice, :intDfltRegion
          From WMEXP
          Where EXco# = :intCo# And EXfnd = :intFnd;
        //*** Beg Add *** 09/04/24 **************************************
        //  For client 102 we will retrieve the default from WMCYP
        If clientId = '102';
           Reset intDfltRegion;
        Endif;
        //*** End Add *** 09/04/24 **************************************
        intAgt  = aryAgt(intGrpIdx);
        intRate = aryAgtRate(intGrpIdx);
        If intAgtUwtr = ' ';
           intAgtUwtr = aryAgtUwtr(intGrpIdx);
        EndIf;
        Reset intEmp#;
        Exec Sql
         Select EMemp# Into :intEmp# from WIEMP
          Where EMco# = :intCo#
           And EMfnd  = :intFnd
           And EMapp# = :intApp#;
       Endsr;
       //==========================================================
       // Determine if another group policy needs to be processed
       //==========================================================
       Begsr GetNextGroup;
        Select;
        When noMoreGroups = *On;
        When ioRtnCode = 'Error';
           noMoreGroups = *On;
        Other;
           intFnd  = aryAgtGrp(intGrpIdx);
           If intFnd = 0;
              noMoreGroups = *On;
              LeaveSr;
           //*** Begin Add *** 11/15/21 ******************************
           Else;
              // Determine if the current group is excluded from Comparative
              // Rating
              indExclCompRatings = GetExcludeCompRatingFlag(intCo#:intFnd);

              indSkipGroup = *Off;
              Select;
              // If the default group is excluded from comparative rating
              // then don't allow other groups to be processed
              When indExclCompRatingsDefault And intFnd <> selectedGroup#;
                 indSkipGroup = *On;
              // If the default group is not excluded from comparative
              // rating then only allow other groups to be selected if they
              // aren't excluded either.
              When Not indExclCompRatingsDefault And
                   intFnd <> selectedGroup# And
                   indExclCompRatings;
                 indSkipGroup = *On;
              EndSl;
        //*** End   Add *** 11/15/21 ***************************************
           Endif;
           //*** Begin Add *** 11/15/21 ************************************
           If Not indSkipGroup;
           //*** End   Add *** 11/15/21 ************************************
              Exsr GetGroupDefaults;
           //*** Begin Add *** 11/15/21 ************************************
           EndIf;
           //*** End   Add *** 11/15/21 ************************************
        Endsl;
       Endsr;
       //==================================================
       // Parse Input JSON
       //==================================================
       Begsr GetParms;
        //*** Begin Add *** 11/15/21 ************************************
        indDefaultChecked = *Off;
        //*** End   Add *** 11/15/21 ************************************
        inJson = inParms;
        docNode = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           json += ' "requestParms" : ';
           StartGiantElement(json);
           json += '"applicationInfo":';
           node0 = YAJL_object_find(docNode:'applicationInfo');
           i0 = 0;
           Dow YAJL_object_loop(node0:i0:key:val);
              Select;
              When key = 'application';
                 StartGiantElement(json);
                 Exsr GetParmsApplication;
                 json += ElementClose;
              When key = 'policies';
                 Exsr GetParmsPolicies;
              Endsl;
           Enddo;
           json += ElementClose;
           // Free up the space by removing the tree
           yajl_tree_free(docNode);
        Else;
           errMsgArray(1) = '"JSON filter can''t be read."';
           ioRtnCode = 'Error';
           Exsr BuildReturnJson;
           outJson = json;
           Return;
        Endif;
        If strAuthority = '*View';
           ioRtnCode = 'Error';
           idx = %Lookup(*Blanks:errMsgArray);
           errMsgArray(idx) = '"User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ')."';
        Endif;
       Endsr;
       //==================================================
       // Parse input JSON for application info
       //==================================================
       Begsr GetParmsApplication;
        node1 = YAJL_object_find(node0:key);
        json += '"application":' + ElementOpen('{');
        i1 = 0;
        Dow YAJL_object_loop(node1:i1:key:val);
           Select;
           When key = 'losses';
           Other;
              BuildGiantJSON(json:key:YAJL_get_string(val));
           EndSl;
           Select;
           When key = 'number';
              strApp# = YAJL_get_string(val);
              If strApp# <> *Blanks;
                 intApp# = %Int(strApp#);
              Endif;
           When key = 'agencyInfo';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-2);
              i2 = 0;
              node2 = YAJL_object_find(node1:key);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'fein';
                    strAgtFein = YAJL_get_string(val);
                    If strAgtFein <> *Blanks;
                       dsWIAPP.APAgtFein = %Int(strAgtFein);
                    Endif;
                 When key = 'mod';
                    strAgtMod = YAJL_get_string(val);
                    If strAgtMod <> *Blanks;
                       dsWIAPP.APAgtMod = %Int(strAgtMod);
                    Endif;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'producerInfo';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-2);
              i2 = 0;
              node2 = YAJL_object_find(node1:key);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'number';
                    strProducer = YAJL_get_string(val);
                    If strProducer <> *Blanks;
                       intProducer = %Int(strProducer);
                       dsWIAPP.APprod = intProducer;
                    Endif;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'yearsInBusiness';
              Monitor;
                 dsWIAPP.APyib = %Int(YAJL_get_string(val));
              On-Error;
                 dsWIAPP.APyib = 0;
              EndMon;
           When key = 'auditFrequency';
              dsWIAPP.APAudFrq = YAJL_get_string(val);
           When key = 'entryPoint';
              dsWIAPP.APiUsr = YAJL_get_string(val);
              strEntryPointGlobal = dsWIAPP.APiUsr;
           When key = 'losses';
              node2 = YAJL_object_find(node1:key);
            //json = json + ',"losses": [{';
              json += ',"losses":' + ElementOpen('[');
              i2 = 0;
              Dow YAJL_array_loop(node2:i2:node3);
                 json += ElementOpen('{');
                 i3 = 0;
                 Dow YAJL_object_loop(node3:i3:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Monitor;
                       Select;
                       When key = 'year';
                          dsWIAPPLOS(i2).APyear = %Int(YAJL_get_string(val));
                       When key = 'annualPremium';
                          dsWIAPPLOS(i2).APannPrem =
                           %Dec(YAJL_get_string(val):13:2);
                        //***  Begin Add  ***  08/05/21  ***********
                          If dsWIAPPLOS(i2).APannPrem = 0;
                             dsWIAPPLOS(i2).APAnnPremZ = 'Y';
                          Endif;
                        //***  End   Add  ***  08/05/21  ***********
                       When key = '#OfLosses';
                          dsWIAPPLOS(i2).AP#Losses = %Int(YAJL_get_string(val));
                        //***  Begin Add  ***  08/05/21  ***********
                          If dsWIAPPLOS(i2).AP#Losses = 0;
                             dsWIAPPLOS(i2).AP#LossesZ = 'Y';
                          Endif;
                        //***  End   Add  ***  08/05/21  ***********
                       When key = 'amountPaid';
                          dsWIAPPLOS(i2).APPaid =
                           %Dec(YAJL_get_string(val):13:2);
                        //***  Begin Add  ***  08/05/21  ***********
                          If dsWIAPPLOS(i2).APPaid = 0;
                             dsWIAPPLOS(i2).APpaidZ = 'Y';
                          Endif;
                        //***  End   Add  ***  08/05/21  ***********
                       When key = 'reserve';
                          dsWIAPPLOS(i2).APReserve =
                           %Dec(YAJL_get_string(val):13:2);
                        //***  Begin Add  ***  08/05/21  ***********
                          If dsWIAPPLOS(i2).APReserve = 0;
                             dsWIAPPLOS(i2).APreserveZ = 'Y';
                          Endif;
                        //***  End   Add  ***  08/05/21  ***********
                       Endsl;
                    On-error;
                    Endmon;
                 Enddo;
                 json += ElementClose + ',';
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'submitter';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-2);
              i2 = 0;
              node2 = YAJL_object_find(node1:key);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'name';
                    dsWIAPP.APsbmUser = YAJL_get_string(val);
                 When key = 'email';
                    dsWIAPP.APsbmEmail = YAJL_get_string(val);
                 When key = 'phone';
                    ExtractJsonPhone(YAJL_object_find(node2:key)
                     :intAreaCode:intPhone#
                     :strType:strJson);
                    dsWIAPP.APsbmPhn# = intAreaCode*10000000+intPhone#;
                    json = %scanrpl('""':'{'+strJson+'}'
                     :json:%len(json)-1);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           Endsl;
        Enddo;
       Endsr;
       //==================================================
       // Parse Input JSON - masterInformation
       //==================================================
       Begsr GetParmsEmpMaster;
       // Save current values
        Reset dsWMEMPOut;
        Exec Sql
         Select * Into :dsWMEMPout
          From WMEMP
          Where EMco#  = :intCo#
            And EMfnd  = :intFnd
            And EMemp# = :intEmp#
            And EMdiv  = :intDiv;
        dsWMEMP(intDivIdx) = dsWMEMPout;
        dsWMEMP(intDivIdx).EMco#  = intCo#;
        dsWMEMP(intDivIdx).EMfnd  = intFnd;
        dsWMEMP(intDivIdx).EMemp# = intEmp#;
        dsWMEMP(intDivIdx).EMdiv  = intDiv;
        //* Save off employer master data for checking on changes
        dsWMEMPSave(intDivIdx) = dsWMEMP(intDivIdx);
       // Retrieve new values
        node5 = YAJL_object_find(node4:key);
        json += ',"masterInformation":' + ElementOpen('{');
        i5 = 0;
        Dow YAJL_object_loop(node5:i5:key:val);
           Select;
           When key = 'ownersOfficers';
           When key = 'locations';
           When key = 'contacts';
           When key = 'name';
           When key = 'dates';
           When key = 'address';
           When key = 'phone';
           When key = 'legalStatus';
           When key = 'SIC';
           When key = 'NAIC';
           When key = 'detailInformation';
           Other;
              BuildGiantJSON(json:key:YAJL_get_string(val));
           EndSl;
           Select;
           When key = 'stateRptFlg';
              dsWMEMP(intDivIdx).EMstrp = YAJL_get_string(val);
           When key = 'fein';
              Monitor;
                 dsWMEMP(intDivIdx).EMfein = %Int(YAJL_get_string(val));
              On-Error;
              EndMon;
           When key = 'name';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'dba';
                    str100a = YAJL_get_string(val);
                    dsWMEMP(intDivIdx).EMname = %Subst(str100a:1:40);
                    dsWMEMP(intDivIdx).EMnam2 = %Subst(str100a:41:60);
                 When key = 'federal';
                    str100a = YAJL_get_string(val);
                    dsWMEMP(intDivIdx).EMfdnm = %Subst(str100a:1:40);
                    dsWMEMP(intDivIdx).EMfdn2 = %Subst(str100a:41:60);
                 When key = 'dbaPersonalName';
                    dsWMEMP(intDivIdx).EMnamP = YAJL_get_string(val);
                 When key = 'federalPersonalName';
                    dsWMEMP(intDivIdx).EMfdnP = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += ElementClose;
           When key = 'dates';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Reset strJson;
              ExtractJsonDates(node6:decEffDt:decExpDt:strJson:'CYYMMDD');
              dsWMEMP(intDivIdx).EMefd1 = decEffDt;
              dt7Eff = decEffDt;
              json += strJson + ElementClose;
           When key = 'address';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 Select;
                 When key = 'mailing';
                    node7 = YAJL_object_find(node6:key);
                    json += '"mailing":' + ElementOpen('{');
                    i7 = 0;
                    Dow YAJL_object_loop(node7:i7:key:val);
                       BuildGiantJSON(json:key:YAJL_get_string(val));
                       Select;
                       When key = 'address1';
                          dsWMEMP(intDivIdx).EMMAd1 = YAJL_get_string(val);
                       When key = 'address2';
                          dsWMEMP(intDivIdx).EMMAd2 = YAJL_get_string(val);
                       When key = 'city';
                          dsWMEMP(intDivIdx).EMMCty = YAJL_get_string(val);
                       When key = 'state';
                          dsWMEMP(intDivIdx).EMMSt = YAJL_get_string(val);
                       When key = 'zip';
                          Clear dsWMEMP(intDivIdx).EMMZip;
                          Monitor;
                             dsWMEMP(intDivIdx).EMMZip =
                              PopZipCode(%Int(YAJL_get_string(val)));
                          On-error;
                          Endmon;
                       Endsl;
                    Enddo;
                    json += ElementClose;
                 When key = 'physical';
                    node7 = YAJL_object_find(node6:key);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
                    i7 = 0;
                    Dow YAJL_object_loop(node7:i7:key:val);
                       BuildGiantJSON(json:key:YAJL_get_string(val));
                       Select;
                       When key = 'address1';
                          dsWMEMP(intDivIdx).EMPAd1 = YAJL_get_string(val);
                       When key = 'address2';
                          dsWMEMP(intDivIdx).EMPAd2 = YAJL_get_string(val);
                       When key = 'city';
                          dsWMEMP(intDivIdx).EMPCty = YAJL_get_string(val);
                       When key = 'state';
                          dsWMEMP(intDivIdx).EMPSt = YAJL_get_string(val);
                       When key = 'zip';
                          Clear dsWMEMP(intDivIdx).EMPZip;
                          Monitor;
                             dsWMEMP(intDivIdx).EMPZip =
                              PopZipCode(%Int(YAJL_get_string(val)));
                          On-error;
                          Endmon;
                       Endsl;
                    Enddo;
                    Select;
                    When dsWMEMP(intDivIdx).EMMAd1 <> *Blanks;
                       dsWMEMP(intDivIdx).EMmpfl = 'M';
                    When dsWMEMP(intDivIdx).EMPAd1 <> *Blanks;
                       dsWMEMP(intDivIdx).EMmpfl = 'P';
                    EndSl;
                    json += ElementClose;
                 Endsl;
              Enddo;
              json += ElementClose;
           When key = 'phone';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_array_loop(node6:i6:node7);
                 json += ElementOpen('{');
                 i7 = 0;
                 Dow YAJL_object_loop(node7:i7:key:val);
                    Select;
                    When key = 'phone1';
                       node8 = YAJL_object_find(node7:key);
                       json += '"phone1":' + ElementOpen('{');
                       i8 = 0;
                       Dow YAJL_object_loop(node8:i8:key:val);
                          BuildGiantJSON(json:key:YAJL_get_string(val));
                          Monitor;
                             Select;
                             When key = 'areacode';
                                dsWMEMP(intDivIdx).EMarea =
                                 %Int(YAJL_get_string(val));
                             When key = 'phone';
                                dsWMEMP(intDivIdx).EMphn# =
                                 %Int(YAJL_get_string(val));
                             When key = 'type';
                             Endsl;
                          On-error;
                          Endmon;
                       Enddo;
                       json += ElementClose;
                    When key = 'fax';
                       node8 = YAJL_object_find(node7:key);
                       json += '"fax":' + ElementOpen('{');
                       i8 = 0;
                       Dow YAJL_object_loop(node8:i8:key:val);
                          BuildGiantJSON(json:key:YAJL_get_string(val));
                          Monitor;
                             Select;
                             When key = 'areacode';
                                dsWMEMP(intDivIdx).EMfaxa =
                                 %Int(YAJL_get_string(val));
                             When key = 'phone';
                                dsWMEMP(intDivIdx).EMfax# =
                                 %Int(YAJL_get_string(val));
                             When key = 'type';
                             Endsl;
                          On-error;
                          Endmon;
                       Enddo;
                       json += ElementClose;
                    Endsl;
                 Enddo;
                 json += ElementClose + ',';
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'legalStatus';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'code';
                    dsWMEMP(intDivIdx).EMtpob = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += ElementClose;
           When key = 'SIC';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'code';
                    dsWMEMP(intDivIdx).EMsic = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += ElementClose;
           When key = 'NAIC';
              node6 = YAJL_object_find(node5:key);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'code';
                    dsWMEMP(intDivIdx).EMnaic = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += ElementClose;
           When key = 'natureOfBusiness';
              dsWMEMP(intDivIdx).EMnaob = YAJL_get_string(val);
           When key = 'otherRatingId';
           When key = 'additionalCoverages';
           When key = 'ownersOfficers';
              Exsr GetParmsOfficers;
           When key = 'contacts';
              Exsr GetParmsContacts;
           When key = 'locations';
              Exsr GetParmsLocations;
           When key = 'detailInformation';
              Exsr GetParmsDetailInformation;
           Endsl;
        Enddo;
        json += ElementClose;
        //***  Begin Add  ***  07/11/21  ***************
        Exsr LoadAutoLocation;
        //***  End   Add  ***  07/11/21  ***************
       Endsr;
       //==================================================
       // Parse Input JSON - gradient information
       //==================================================
       Begsr GetParmsGradientInfo;
        //***  Begin Chg  ***  08/05/21  ************************
        //json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
        //node3 = YAJL_object_find(node2:key);
        //i3 = 0;
        //Dow YAJL_array_loop(node3:i3:node4);
        //   Clear dsGradient(i3);
        //   json += ElementOpen('{');
        //Clear dsGradient(i3);
        //json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
        //   i4 = 0;
        //   Dow YAJL_object_loop(node4:i4:key:val);
        //      BuildGiantJson(json:key:YAJL_get_string(val));
        //      Monitor;
        //         Select;
        //         When key = 'year';
        //            dsGradient(i3).WIGSP.GSfyr =
        //             %dec(YAJL_get_string(val):3:0);
        //         When key = 'predictedLoss';
        //            dsGradient(i3).WIGSP.GSprdLos =
        //             %dec(YAJL_get_string(val):11:2);
        //         When key = 'quotedPremium';
        //            dsGradient(i3).WIGSP.GSqtePrm =
        //             %dec(YAJL_get_string(val):11:2);
        //         When key = 'predictedLossRatio';
        //            dsGradient(i3).WIGSP.GSpLosRat =
        //             %dec(YAJL_get_string(val):5:4);
        //         When key = 'percentile';
        //            dsGradient(i3).WIGSP.GSperc =
        //             %dec(YAJL_get_string(val):7:4);
        //         When key = 'reviewTargets';
        //            dsGradient(i3).WIGSP.GSrvwTrgs =
        //             YAJL_get_string(val);
        //         When key = 'targetPLR';
        //            dsGradient(i3).WIGSP.GStrgPLR =
        //             %dec(YAJL_get_string(val):5:4);
        //         When key = 'targetPremium';
        //            dsGradient(i3).WIGSP.GStrgPrm =
        //             %dec(YAJL_get_string(val):11:2);
        //         When key = 'highRangePremium';
        //            dsGradient(i3).WIGSP.GShRngPrm =
        //             %dec(YAJL_get_string(val):11:2);
        //         When key = 'lowRangePremium';
        //            dsGradient(i3).WIGSP.GSlRngPrm =
        //             %dec(YAJL_get_string(val):11:2);
        //         When key = 'targetFactor';
        //            dsGradient(i3).WIGSP.GStrgFct =
        //             %dec(YAJL_get_string(val):5:4);
        //         When key = 'highRangeFactor';
        //            dsGradient(i3).WIGSP.GShRngFct =
        //             %dec(YAJL_get_string(val):5:4);
        //         When key = 'lowRangeFactor';
        //            dsGradient(i3).WIGSP.GSlRngFct =
        //             %dec(YAJL_get_string(val):5:4);
        //         When key = 'driverList';
        //            json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
        //            node5 = YAJL_object_find(node4:'driverList');
        //            i5 = 0;
        //            Dow YAJL_array_loop(node5:i5:node6);
        //               Clear dsGradient(i3).WIGSPDRV(i5);
        //               json += ElementOpen('{');
        //               i6 = 0;
        //               Dow YAJL_object_loop(node6:i6:key:val);
        //                  BuildGiantJSON(json:key:YAJL_get_string(val));
        //                  Monitor;
        //                     Select;
        //                     When key = 'heading';
        //                        dsGradient(i3).WIGSPDRV(i5).GSdrvName =
        //                         YAJL_get_string(val);
        //                     When key = 'value';
        //                        dsGradient(i3).WIGSPDRV(i5).GSdrvVal =
        //                         %dec(YAJL_get_string(val):5:4);
        //                     Endsl;
        //                  On-error;
        //                  Endmon;
        //               Enddo;
        //               json += ElementClose + ',';
        //            Enddo;
        //            json = %scanrpl(',':'':json:%len(json)-1);
        //            json += ElementClose;
        //         Endsl;
        //      On-error;
        //      Endmon;
        //   Enddo;
        //   json += ElementClose + ',';
        //   Select;
        //   When dsGradient(i3).WIGSP.GSrvwTrgs = 'true';
        //      dsGradient(i3).WIGSP.GSrvwTrgs = 'Y';
        //   When dsGradient(i3).WIGSP.GSrvwTrgs = 't';
        //      dsGradient(i3).WIGSP.GSrvwTrgs = 'Y';
        //   When dsGradient(i3).WIGSP.GSrvwTrgs = 'false';
        //      dsGradient(i3).WIGSP.GSrvwTrgs = 'N';
        //   When dsGradient(i3).WIGSP.GSrvwTrgs = 'f';
        //      dsGradient(i3).WIGSP.GSrvwTrgs = 'N';
        //   Endsl;
        //Enddo;
        //json = %scanrpl(',':'':json:%len(json)-1);
        json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
        node5 = YAJL_object_find(node4:key);
        Clear dsGradient(i3);
        dsGradient(i3).WIGSP.GSfyr = strYearScoring;
        i5 = 0;
        Dow YAJL_object_loop(node5:i5:key:val);
           BuildGiantJson(json:key:YAJL_get_string(val));
           Monitor;
              Select;
              When key = 'predictedLoss';
                 dsGradient(i3).WIGSP.GSprdLos =
                  %dec(YAJL_get_string(val):11:2);
              When key = 'quotedPremium';
                 dsGradient(i3).WIGSP.GSqtePrm =
                  %dec(YAJL_get_string(val):11:2);
              When key = 'predictedLossRatio';
                 dsGradient(i3).WIGSP.GSpLosRat =
                  %dec(YAJL_get_string(val):5:4);
              When key = 'percentile';
                 dsGradient(i3).WIGSP.GSperc =
                  %dec(YAJL_get_string(val):7:4);
              When key = 'reviewTargets';
                 dsGradient(i3).WIGSP.GSrvwTrgs =
                  YAJL_get_string(val);
              When key = 'targetPLR';
                 dsGradient(i3).WIGSP.GStrgPLR =
                  %dec(YAJL_get_string(val):5:4);
              When key = 'targetPremium';
                 dsGradient(i3).WIGSP.GStrgPrm =
                  %dec(YAJL_get_string(val):11:2);
              When key = 'highRangePremium';
                 dsGradient(i3).WIGSP.GShRngPrm =
                  %dec(YAJL_get_string(val):11:2);
              When key = 'lowRangePremium';
                 dsGradient(i3).WIGSP.GSlRngPrm =
                  %dec(YAJL_get_string(val):11:2);
              When key = 'targetFactor';
                 dsGradient(i3).WIGSP.GStrgFct =
                  %dec(YAJL_get_string(val):5:4);
              When key = 'highRangeFactor';
                 dsGradient(i3).WIGSP.GShRngFct =
                  %dec(YAJL_get_string(val):5:4);
              When key = 'lowRangeFactor';
                 dsGradient(i3).WIGSP.GSlRngFct =
                  %dec(YAJL_get_string(val):5:4);
              When key = 'driverList';
                 json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
                 node6 = YAJL_object_find(node5:'driverList');
                 i6 = 0;
                 Dow YAJL_array_loop(node6:i6:node7);
                    Clear dsGradient(i3).WIGSPDRV(i6);
                    json += ElementOpen('{');
                    i7 = 0;
                    Dow YAJL_object_loop(node7:i7:key:val);
                       BuildGiantJSON(json:key:YAJL_get_string(val));
                       Monitor;
                          Select;
                          When key = 'heading';
                             dsGradient(i3).WIGSPDRV(i6).GSdrvName =
                              YAJL_get_string(val);
                          When key = 'value';
                             dsGradient(i3).WIGSPDRV(i6).GSdrvVal =
                              %dec(YAJL_get_string(val):5:4);
                          Endsl;
                       On-error;
                       Endmon;
                    Enddo;
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              Endsl;
           On-error;
           Endmon;
        Enddo;
        Select;
        When dsGradient(i3).WIGSP.GSrvwTrgs = 'true';
           dsGradient(i3).WIGSP.GSrvwTrgs = 'Y';
        When dsGradient(i3).WIGSP.GSrvwTrgs = 't';
           dsGradient(i3).WIGSP.GSrvwTrgs = 'Y';
        When dsGradient(i3).WIGSP.GSrvwTrgs = 'false';
           dsGradient(i3).WIGSP.GSrvwTrgs = 'N';
        When dsGradient(i3).WIGSP.GSrvwTrgs = 'f';
           dsGradient(i3).WIGSP.GSrvwTrgs = 'N';
        Endsl;
        //***  End   Chg  ***  08/05/21  ************************
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse Input JSON - locations
       //==================================================
       Begsr GetParmsLocations;
        node6 = YAJL_object_find(node5:'locations');
        json += ',"locations":' + ElementOpen('[');
       // Find out what location IDs are in the incoming JSON
        Reset aryLoc;
        i6 = 0;
        Dow YAJL_array_loop(node6:i6:node7);
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              Select;
              When key = 'id';
                 strElid = YAJL_get_string(val);
                 If strElid <> *Blanks And strElid <> *Zeros;
                    idx = %Lookup(*Hival:aryLoc);
                    aryLoc(idx) = %Int(strElid);
                 Endif;
                 Leave;
              Endsl;
           Enddo;
        Enddo;
        // aryLoc Inz *Hival
        Sorta(A) aryLoc;
        // Process locations
        i6 = 0;
        Dow YAJL_array_loop(node6:i6:node7);
           json += ElementOpen('{');
           intLocIdx += 1;
          // Get key values
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              Select;
              When key = 'id';
                 strElid = YAJL_get_string(val);
                 Leave;
              Endsl;
           Enddo;
          // Save current values
           Reset dsWMALPOut;
           Exec Sql
            Select * Into :dsWMALPout
             From WMALP
             Where ALco#  = :intCo#
               And ALfnd  = :intFnd
               And ALemp# = :intEmp#
               And ALdiv  = :intDiv
               And ALelid = :strElid;
           dsWMALP(intLocIdx) = dsWMALPout;
           dsWMALP(intLocIdx).ALco#   = intCo#;
           dsWMALP(intLocIdx).ALfnd   = intFnd;
           dsWMALP(intLocIdx).ALemp#  = intEmp#;
           dsWMALP(intLocIdx).ALdiv   = intDiv;
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              Select;
              When key = 'name';
              When key = 'dates';
              When key = 'address';
              When key = 'phone';
              When key = 'SIC';
              When key = 'NAIC';
              Other;
                 BuildGiantJSON(json:key:YAJL_get_string(val));
              EndSl;
              Select;
              When key = 'type';
                 dsWMALP(intLocIdx).ALeltp = YAJL_get_string(val);
              When key = 'id';
                 dsWMALP(intLocIdx).ALelid = YAJL_get_string(val);
              When key = 'fein';
                 Clear dsWMALP(intLocIdx).ALfein;
                 Monitor;
                    dsWMALP(intLocIdx).ALfein = %Int(YAJL_get_string(val));
                 On-error;
                 Endmon;
              When key = 'name';
                 node8 = YAJL_object_find(node7:key);
                 json = json + ',"name":' + ElementOpen('{');
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'dba';
                       str100a = YAJL_get_string(val);
                       dsWMALP(intLocIdx).ALname = %Subst(str100a:1:40);
                       dsWMALP(intLocIdx).ALnam2 = %Subst(str100a:41:60);
                    When key = 'federal';
                       str100a = YAJL_get_string(val);
                       dsWMALP(intLocIdx).ALfdnm = %Subst(str100a:1:40);
                       dsWMALP(intLocIdx).ALfdn2 = %Subst(str100a:41:60);
                    When key = 'dbaPersonalName';
                       dsWMALP(intLocIdx).ALnamP = YAJL_get_string(val);
                    When key = 'federalPersonalName';
                       dsWMALP(intLocIdx).ALfdnP = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'dates';
                 node8 = YAJL_object_find(node7:key);
                 i8 = 0;
                 Clear strJson;
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 ExtractJsonDates(node8:decEffDt:decExpDt:strJson:'CYYMMDD');
                 dsWMALP(intLocIdx).ALefdt = decEffDt;
                 json = %scanrpl('""':'{'+strJson+'}':json:%len(json)-1);
              When key = 'address';
                 node8 = YAJL_object_find(node7:key);
                 json = json + ',"address":' + ElementOpen('{');
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    Select;
                    When key = 'mailing';
                       node9 = YAJL_object_find(node8:key);
                       json = json + '"mailing":' + ElementOpen('{');
                       i9 = 0;
                       Dow YAJL_object_loop(node9:i9:key:val);
                          BuildGiantJSON(json:key:YAJL_get_string(val));
                          Select;
                          When key = 'address1';
                             dsWMALP(intLocIdx).ALMAd1 = YAJL_get_string(val);
                          When key = 'address2';
                             dsWMALP(intLocIdx).ALMAd2 = YAJL_get_string(val);
                          When key = 'city';
                             dsWMALP(intLocIdx).ALMCty = YAJL_get_string(val);
                          When key = 'state';
                             dsWMALP(intLocIdx).ALMSt = YAJL_get_string(val);
                          When key = 'zip';
                             Clear dsWMALP(intLocIdx).ALMZip;
                             Monitor;
                                dsWMALP(intLocIdx).ALMZip =
                                 PopZipCode(%Int(YAJL_get_string(val)));
                             On-error;
                             Endmon;
                          Endsl;
                       Enddo;
                       json += ElementClose;
                    When key = 'physical';
                       node9 = YAJL_object_find(node8:key);
                       json = json + ',"physical":' + ElementOpen('{');
                       i9 = 0;
                       Dow YAJL_object_loop(node9:i9:key:val);
                          BuildGiantJSON(json:key:YAJL_get_string(val));
                          Select;
                          When key = 'address1';
                             dsWMALP(intLocIdx).ALPAd1 = YAJL_get_string(val);
                          When key = 'address2';
                             dsWMALP(intLocIdx).ALPAd2 = YAJL_get_string(val);
                          When key = 'city';
                             dsWMALP(intLocIdx).ALPCty = YAJL_get_string(val);
                          When key = 'state';
                             dsWMALP(intLocIdx).ALPSt = YAJL_get_string(val);
                          When key = 'zip';
                             Clear dsWMALP(intLocIdx).ALPZip;
                             Monitor;
                                dsWMALP(intLocIdx).ALPZip =
                                 PopZipCode(%Int(YAJL_get_string(val)));
                             On-error;
                             Endmon;
                          Endsl;
                       Enddo;
                       json += ElementClose;
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'phone';
                 Clear dsWMALP(intLocIdx).ALarea;
                 Clear dsWMALP(intLocIdx).ALphn#;
                 Clear dsWMALP(intLocIdx).ALfara;
                 Clear dsWMALP(intLocIdx).ALfax#;
                 node8 = YAJL_object_find(node7:key);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
                 i8 = 0;
                 Dow YAJL_array_loop(node8:i8:node9);
                    json += ElementOpen('{');
                    i9 = 0;
                    Dow YAJL_object_loop(node9:i9:key:val);
                       Select;
                       When key = 'phone1';
                          node10 = YAJL_object_find(node9:key);
                          json += '"phone1":' + ElementOpen('{');
                          i10 = 0;
                          Dow YAJL_object_loop(node10:i10:key:val);
                             BuildGiantJSON(json:key:YAJL_get_string(val));
                             Monitor;
                                Select;
                                When key = 'areacode';
                                   dsWMALP(intLocIdx).ALarea =
                                    %Int(YAJL_get_string(val));
                                When key = 'phone';
                                   dsWMALP(intLocIdx).ALphn# =
                                    %Int(YAJL_get_string(val));
                                When key = 'type';
                                Endsl;
                             On-error;
                             Endmon;
                          Enddo;
                          json += ElementClose + ',';
                       When key = 'fax';
                          node10 = YAJL_object_find(node9:key);
                          json += '"fax":' + ElementOpen('{');
                          i10 = 0;
                          Dow YAJL_object_loop(node10:i10:key:val);
                             BuildGiantJSON(json:key:YAJL_get_string(val));
                             Monitor;
                                Select;
                                When key = 'areacode';
                                   dsWMALP(intLocIdx).ALfara =
                                    %Int(YAJL_get_string(val));
                                When key = 'phone';
                                   dsWMALP(intLocIdx).ALfax# =
                                    %Int(YAJL_get_string(val));
                                When key = 'type';
                                Endsl;
                             On-error;
                             Endmon;
                          Enddo;
                          json += ElementClose + ',';
                       Endsl;
                    Enddo;
                    json = %scanrpl(',':'':json:%len(json)-1);
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              When key = 'SIC';
                 node8 = YAJL_object_find(node7:key);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'code';
                       dsWMALP(intLocIdx).ALsic = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'NAIC';
                 node8 = YAJL_object_find(node7:key);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'code';
                       dsWMALP(intLocIdx).ALnaic = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'natureOfBusiness';
              Endsl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
        //***  Begin Add  ***  07/11/22  *****************
        isLocationExists = *on;
        //***  End   Add  ***  07/11/22  *****************
       Endsr;
       //==================================================
       // Parse Input JSON - Owners Officers
       //==================================================
       Begsr GetParmsOfficers;
        node6 = YAJL_object_find(node5:'ownersOfficers');
        json += ',"ownersOfficers":' + ElementOpen('[');
       // Find out what seq# are in the incoming JSON
        Reset aryOO#;
        i6 = 0;
        Dow YAJL_array_loop(node6:i6:node7);
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              If key = 'id';
                 strSeq# = YAJL_get_string(val);
                 If strSeq# <> *Blanks And strSeq# <> *Zeros;
                    idx = %LookUp(*Hival:aryOO#);
                    aryOO#(idx) =  %Int(strSeq#);
                 Endif;
                 Leave;
              Endif;
           Enddo;
        Enddo;
        SortA(A) aryOO#;
        i6 = 0;
       // Process JSON
        Dow YAJL_array_loop(node6:i6:node7);
           intOffIdx += 1;
           json += ElementOpen('{');
          // Get current key
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              If key = 'id';
                 strSeq# = YAJL_get_string(val);
                 If strSeq# <> *Blanks And strSeq# <> *Zeros;
                    intSeq#  = %Int(strSeq#);
                 Else;
                    idx = %LookUp(*Hival:aryOO#);
                    If idx = 1;
                       intSeq# = 1;
                    Else;
                       intSeq# = aryOO#(idx - 1) + 1;
                    Endif;
                    aryOO#(idx) = intSeq#;
                 Endif;
              Endif;
           Enddo;
          // Save current values
           Reset dsWMOOPOut;
           Exec Sql
            Select * Into :dsWMOOPout
             From WMOOP
             Where OOco#  = :intCo#
               And OOfnd  = :intFnd
               And OOemp# = :intEmp#
               And OOdiv  = :intDiv
               And OOseq# = :intSeq#;
           dsWMOOP(intOffIdx) = dsWMOOPout;
           dsWMOOP(intOffIdx).OOco#  = intCo#;
           dsWMOOP(intOffIdx).OOfnd  = intFnd;
           dsWMOOP(intOffIdx).OOemp# = intEmp#;
           dsWMOOP(intOffIdx).OOdiv  = intDiv;
           dsWMOOP(intOffIdx).OOseq# = intSeq#;
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              Select;
              When key = 'name';
              When key = 'dates';
              Other;
                 BuildGiantJSON(json:key:YAJL_get_string(val));
              EndSl;
              Select;
              When key = 'id';
                 dsWMOOP(intOffIdx).OOseq#  = intSeq#;
              When key = 'name';
                 node8 = YAJL_object_find(node7:key);
                 json += ',"name":' + ElementOpen('{');
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'first';
                       dsWMOOP(intOffIdx).OOofsn  = YAJL_get_string(val);
                    When key = 'last';
                       dsWMOOP(intOffIdx).OOolsn = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'title';
                 dsWMOOP(intOffIdx).OOtitl = YAJL_get_string(val);
              When key = 'dob';
                 strTest = YAJL_get_string(val);
                 If strTest <> *Blanks;
                    dsWMOOP(intOffIdx).OOdob =
                     DtCYMD(%Int(YAJL_get_string(val)));
                 Endif;
              When key = 'ssn';
                 dsWMOOP(intOffIdx).OOoss# = YAJL_get_string(val);
              When key = 'ownership';
                 strTest = YAJL_get_string(val);
                 Monitor;
                    Select;
                    When strTest <> *Blanks;
                       dsWMOOP(intOffIdx).OOofpc = %Dec(strTest:5:2);
                    Endsl;
                 On-error;
                    ioRtnCode = 'Error';
                    idx = %Lookup(*Blanks:errMsgArray);
                    errMsgArray(idx) = '"Invalid owner percentage."';
                 Endmon;
              When key = 'coverage';
                 dsWMOOP(intOffIdx).OOexfl = YAJL_get_string(val);
                 If dsWMOOP(intOffIdx).OOexfl = *Blank;
                    dsWMOOP(intOffIdx).OOexfl = 'I';
                 Endif;
              When key = 'dates';
                 node8 = YAJL_object_find(node7:key);
                 json += ',"dates":' + ElementOpen('{');
                 i8 = 0;
                 Clear strJson;
                 ExtractJsonDates(node8:decEffDt:decExpDt:strJson:'CYYMMDD');
                 dsWMOOP(intOffIdx).OOefDt = decEffDt;
                 dsWMOOP(intOffIdx).OOexDt = decExpDt;
                 json += ElementClose;
              When key = 'classCode';
                 strTest = YAJL_get_string(val);
                 If strTest <> *Blanks;
                    dsWMOOP(intOffIdx).OOclas = %Int(strTest);
                 Endif;
              EndSl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse Input JSON - policies
       //==================================================
       Begsr GetParmsPolicies;
        node1 = YAJL_object_find(node0:key);
        json += ',"policies":' + ElementOpen('[');
        i1 = 0;
        Dow YAJL_array_loop(node1:i1:node2);
           json += ElementOpen('{');
        // Get key values
           chr1a = *Blank;
           i2 = 0;
           Dow YAJL_object_loop(node2:i2:key:val);
              Select;
              When key = 'company';
                 strCo# = YAJL_get_string(val);
                 intCo# = intUserCo#;
                 If strCo# <> *Blanks;
                    intCo# = %Int(strCo#);
                 EndIf;
              When key = 'group';
                 node3 = YAJL_object_find(node2:key);
                 i3 = 0;
                 Dow YAJL_object_loop(node3:i3:key:val);
                    Select;
                    When key = 'number';
                       // Get selected group
                       strFnd = YAJL_get_string(val);
                       Clear intFnd;
                       If strFnd <> *Blanks;
                          intFnd = %Int(strFnd);
                       EndIf;
                       intFnd = GetGroupDefault(intCo#:dsWIAPP.APAgtFein:
                       dsWIAPP.APAgtMod:intFnd);
                       If intFnd <> *zero;
                          selectedGroup# = intFnd;
                          //*** Begin Add *** 11/15/21 *********************
                          indExclCompRatingsDefault =
                             GetExcludeCompRatingFlag(intCo#:intFnd);
                          //*** End   Add *** 11/15/21 *********************
                       Endif;
                    When key = 'name';
                       strFndName = YAJL_get_string(val);
                    Endsl;
                 Enddo;
              When key = 'currentlySelected';
                 chr1a = YAJL_get_string(val);
              When key = 'employer';
                 strEmp# = YAJL_get_string(val);
                 If strEmp# <> *Blanks And strEmp# <> *Zeros;
                    intEmp# = %Int(strEmp#);
                 Endif;
              Endsl;
           Enddo;
           Reset dsWINOTIP1;
           Exec sql
            select * into :dsWINOTIP1
             from WINOTIP1
             where N1co# = :intCo#;
           If chr1a = 'Y';
              selectedGroup# = intFnd;
           Endif;
           If (strApp# = *Blanks Or strApp# = *Zeros)
            And (strEmp# = *Blanks Or strEmp# = *Zeros);
              isAddingApp = *On;
           EndIf;
           If strApp# <> *Blanks And strApp# <> *Zeros;
              isSavingApp = *On;
           Endif;
         // Build Parm JSON
           i2 = 0;
           Dow YAJL_object_loop(node2:i2:key:val);
              Select;
              When key = 'group';
                 json += ',"group": ' + ElementOpen('{');
                 node3 = YAJL_object_find(node2:key);
                 i3 = 0;
                 Dow YAJL_object_loop(node3:i3:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                 Enddo;
                 json += ElementClose;
            //***  Begin Chg  ***  08/05/21  *******************
            //When key = 'gradientInfo';
            //   BuildGiantJSON(json:key:YAJL_get_string(val));
            //   Exsr GetParmsGradientInfo;
              When key = 'scoring';
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Exsr GetParmsScoringInfo;
            //***  End   Chg  ***  08/05/21  *******************
              When key = 'questionList';
                 node3 = YAJL_object_find(node2:'questionList');
                 json += ',"questionList": ' + ElementOpen('[');
                 i3 = 0;
                 Clear dsWIAPPANS;
                 Dow YAJL_array_loop(node3:i3:node4);
                    json += ElementOpen('{');
                    i4 = 0;
                    Dow YAJL_object_loop(node4:i4:key:val);
                       BuildGiantJSON(json:key:YAJL_get_string(val));
                       Monitor;
                          Select;
                          When key = 'id';
                             dsWIAPPANS(i3).APid# = %Int(YAJL_get_string(val));
                          When key = 'response';
                             dsWIAPPANS(i3).APresp = YAJL_get_string(val);
                          When key = 'ratingState';
                             dsWIAPPANS(i3).APRtCd = YAJL_get_string(val);
                          When key = 'payrollClass';
                             dsWIAPPANS(i3).APClas = %Int(YAJL_get_string(val));
                          When key = 'required';
                             dsWIAPPANS(i3).required = YAJL_get_string(val);
                          Endsl;
                       On-error;
                       Endmon;
                    Enddo;
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              When key = 'divisions';
                 node3 = YAJL_object_find(node2:'divisions');

                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ',"divisions": ' + ElementOpen('[');
               // Find out what divisions are in the incoming JSON
                 i3 = 0;
                 Dow YAJL_array_loop(node3:i3:node4);
                    i4 = 0;
                    Dow YAJL_object_loop(node4:i4:key:val);
                       If key = 'division';
                          strDiv = YAJL_get_string(val);
                          If strDiv <> *Blanks;
                             idx = %LookUp(*Hival:aryDiv);
                             aryDiv(idx) = %Int(strDiv);
                          Endif;
                          Leave;
                       Endif;
                    Enddo;
                 Enddo;
               // aryDiv is Inz with *Hival
                 SortA(A) aryDiv;
                 i3 = 0;
                 Reset intDivIdx;
                 Reset intOffIdx;
                 Reset intLocIdx;
                 Reset intDtlIdx;
                 Reset intPayIdx;
                 Dow YAJL_array_loop(node3:i3:node4);
                    intDivIdx += 1;
                    json += ElementOpen('{');
                   // Get key values
                    i4 = 0;
                    Dow YAJL_object_loop(node4:i4:key:val);
                       If key = 'division';
                          strDiv = YAJL_get_string(val);
                          If strDiv <> *Blanks;
                             intDiv = %Int(strDiv);
                          Else;
                             idx = %LookUp(*Hival:aryDiv);
                             If idx = 1;
                                intDiv = 0;
                             Else;
                                intDiv = aryDiv(idx - 1) + 1;
                             Endif;
                             aryDiv(idx) = intDiv;
                          Endif;
                          Leave;
                       Endif;
                    Enddo;
                  // Process divisions
                    i4 = 0;
                    Dow YAJL_object_loop(node4:i4:key:val);
                       Select;
                       When key = 'division';
                          BuildGiantJSON(json:key:YAJL_get_string(val));
                       When key = 'masterInformation';
                          Exsr GetParmsEmpMaster;
                       Endsl;
                    Enddo;
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              Other;
                 BuildGiantJSON(json:key:YAJL_get_string(val));
              Endsl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
        json += ElementClose;
        If isSavingApp Or isAddingApp;
           ExSr LoadAgtGroups;
        Else;
           noMoreGroups = *On;
        Endif;
       Endsr;
       //==================================================
       // Parse Input JSON - detailInformation
       //==================================================
       Begsr GetParmsDetailInformation;
        node6 = YAJL_object_find(node5:'detailInformation');
        json = json + ',"detailInformation":' + ElementOpen('[');
        i6 = 0;
        Dow YAJL_array_loop(node6:i6:node7);
           intDtlIdx += 1;
           dsWDELP(intDtlIdx).ELco#  = intCo#;
           dsWDELP(intDtlIdx).ELfnd  = intFnd;
           dsWDELP(intDtlIdx).ELemp# = intEmp#;
           dsWDELP(intDtlIdx).ELdiv  = intDiv;
           json += ElementOpen('{');
           // Get key value(s)
           i7 = 0;
           Dow YAJL_Object_loop(node7:i7:key:val);
              Select;
              When key = 'year';
                 Monitor;
                    dsWDELP(intDtlIdx).ELfyr = %Int(YAJL_get_string(val));
                 On-Error;
                    dsWDELP(intDtlIdx).ELfyr = 0;
                 Endmon;
                 Leave;
              Endsl;
           Enddo;
           If dsWDELP(intDtlIdx).ELfyr = 0;
              dsWDELP(intDtlIdx).ELfyr =
               GetFyr(intCo#:intFnd:dsWMEMP(intDivIdx).EMefd1);
           Endif;
           intFyr = dsWDELP(intDtlIdx).ELfyr;
           // Set array values to current values
           dsWDELPout = dsWDELP(intDtlIdx);
           Exec sql
            select * into :dsWDELPout
             from WDELP
             where ELco# = :dsWDELPout.ELco#
              and ELfnd  = :dsWDELPout.ELfnd
              and ELemp# = :dsWDELPout.ELemp#
              and ELdiv  = :dsWDELPout.ELdiv
              and ELfyr  = :dsWDELPout.ELfyr;
           dsWDELP(intDtlIdx) = dsWDELPout;
           // Retrieve new values
           i7 = 0;
           Dow YAJL_Object_loop(node7:i7:key:val);
              BuildGiantJson(json:key:YAJL_get_string(val));
              Select;
              When key = 'year';
              When key = 'billingPlan';
                 ExtractJsonCode(val:strCodeExtract:strCodeDesc:strCodeDescOth
                  :strJsonExtract600);
                 json = %scanrpl('""':'{'+strJsonExtract600+'}'
                  :json:%len(json)-1);
                 Clear dsWDELP(intDtlIdx).ELbsCd;
                 Monitor;
                    dsWDELP(intDtlIdx).ELbsCd = %Int(strCodeExtract);
                 On-Error;
                 Endmon;
              When key = 'governingClass';
                 Clear dsWDELP(intDtlIdx).ELgovc;
                 Monitor;
                    dsWDELP(intDtlIdx).ELgovc = %Int(YAJL_get_string(val));
                 On-Error;
                 Endmon;
              When key = '#OfEmployees';
                 Clear dsWDELP(intDtlIdx).ELempy;
                 Monitor;
                    dsWDELP(intDtlIdx).ELempy = %Int(YAJL_get_string(val));
                 On-Error;
                 Endmon;
              When key = 'ratingSt';
                 arySt(intDtlIdx) = YAJL_get_string(val);
              When key = 'rateSetCode';
                 dsWDELP(intDtlIdx).ELrtcd = YAJL_get_string(val);
              When key = 'policyDates';
                 Reset strJson;
                 ExtractJsonDates(YAJL_object_find(node7:key)
                  :decEffDt:decExpDt:strJson:'CYYMMDD');
                 dsWDELP(intDtlIdx).ELpfDt = decEffDt;
                 dsWDELP(intDtlIdx).ELbsDt = decEffDt;
                 dsWDELP(intDtlIdx).ELptDt = decExpDt;
                 If decEffDt = 0;
                    dsWDELP(intDtlIdx).ELpfDt = dsWMEMP(intDivIdx).EMefd1;
                    dsWDELP(intDtlIdx).ELbsDt = dsWMEMP(intDivIdx).EMefd1;
                    dsWDELP(intDtlIdx).ELptDt =
                     GetPolExpDate(intCo#:intFnd:dsWMEMP(intDivIdx).EMefd1);
                 EndIf;
                 If dsWDELP(intDtlIdx).ELptDt = 0;
                    dsWDELP(intDtlIdx).ELptDt =
                     GetPolExpDate(intCo#:intFnd:dsWDELP(intDtlIdx).ELpfDt);
                 EndIf;
                 json = %scanrpl('""':'{'+strJson+'}':json:%len(json)-1);
              When key = 'ratingFactors';
                 Exsr GetParmsRatingFactors;
              When key = 'ratingScore';
                 Exsr GetRatingScore;
              When key = 'payroll';
                 Exsr GetParmsPayroll;
              Endsl;
           Enddo;
           json += ElementClose + ',';
           // Use defaults if no value exists in current WDELP and
           //  not populated in request JSON.
           Select;
           When dsWDELPOut.ELbscd = 0 And dsWDELP(intDtlIdx).ELbscd = 0;
              useDftBillPlan(intDtlIdx) = *On;
           When dsWDELPOut.ELbscd <> 0 And dsWDELP(intDtlIdx).ELbscd = 0;
              dsWDELP(intDtlIdx).ELbscd = dsWDELPOut.ELbscd;
           Endsl;
           Select;
           When dsWDELPOut.ELrtcd = *Blanks
            And dsWDELP(intDtlIdx).ELrtcd = *Blanks;
              useDftRateCode(intDtlIdx) = *On;
           When dsWDELPOut.ELrtcd <> *Blanks
            And dsWDELP(intDtlIdx).ELrtcd = *Blanks;
              dsWDELP(intDtlIdx).ELrtcd = dsWDELPOut.ELrtcd;
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse Input JSON - Employer Contacts
       //==================================================
       Begsr GetParmsContacts;
        node6 = YAJL_object_find(node5:'contacts');
        json += ',"contacts":' + ElementOpen('[');
        i6 = 0;
        Dow YAJL_array_loop(node6:i6:node7);
           intConIdx += 1;
           json += ElementOpen('{');
          // Get key values
           i7 = 0;
           Clear strPCnt;
           Clear strFsn;
           Clear strMid;
           Clear strLsn;
           Clear strPosn;
           Dow YAJL_object_loop(node7:i7:key:val);
              node8 = YAJL_object_find(node7:key);
              Select;
              When key = 'designation';
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    Select;
                    When key = 'code';
                       strPCnt = YAJL_get_string(val);
                    Endsl;
                 Enddo;
              When key = 'name';
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    Select;
                    When key = 'first';
                       strFsn = YAJL_get_string(val);
                    When key = 'last';
                       strLsn = YAJL_get_string(val);
                    Endsl;
                 Enddo;
              When key = 'position';
                 strPosn = YAJL_get_string(val);
              Endsl;
           Enddo;
          // Save current values
           Reset dsWMECPOut;
           Exec Sql
            Select * Into :dsWMECPout
             From WMECP
             Where ECco#  = :intCo#
               And ECfnd  = :intFnd
               And ECemp# = :intEmp#
               And ECdiv  = :intDiv
               And EClsn  = :strFsn
               And ECfsn  = :strLsn
               And ECmid  = :strMid
               And ECposn = :strPosn
               And ECpcnt = :strPCnt;
           dsWMECP(intConIdx) = dsWMECPout;
           dsWMECP(intConIdx).ECco#   = intCo#;
           dsWMECP(intConIdx).ECfnd   = intFnd;
           dsWMECP(intConIdx).ECemp#  = intEmp#;
           dsWMECP(intConIdx).ECdiv   = intDiv;
           i7 = 0;
           Dow YAJL_object_loop(node7:i7:key:val);
              Select;
              When key = 'name';
              When key = 'address';
              When key = 'phone';
              When key = 'designation';
              Other;
                 BuildGiantJSON(json:key:YAJL_get_string(val));
              EndSl;
              Select;
              When key = 'name';
                 node8 = YAJL_object_find(node7:key);
                 json += '"name":' + ElementOpen('{');
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    Select;
                    When key = 'first';
                       dsWMECP(intConIdx).ECfsn = YAJL_get_string(val);
                    When key = 'last';
                       dsWMECP(intConIdx).EClsn = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 json += ElementClose;
              When key = 'position';
                 dsWMECP(intConIdx).ECposn= YAJL_get_string(val);
              When key = 'address';
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Reset strJson;
                 ExtractJsonAddress(YAJL_object_find(node7:key)
                  :strConAddress1
                  :strConAddress2
                  :strConCity
                  :strConSt
                  :intConZip
                  :strJson);
                  dsWMECP(intConIdx).ECadr1 = strConAddress1;
                  dsWMECP(intConIdx).ECadr2 = strConAddress2;
                  dsWMECP(intConIdx).ECcty  = strConCity;
                  dsWMECP(intConIdx).ECst   = strConSt;
                  dsWMECP(intConIdx).ECzip  = intConZip;
                  json = %scanrpl('""':'{'+strJson+'}':json:%len(json)-1);
              When key = 'designation';
                 node8 = YAJL_object_find(node7:key);
                 json += ',"designation":' + ElementOpen('{');
                 i8 = 0;
                 Dow YAJL_object_loop(node8:i8:key:val);
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    If key = 'code';
                       dsWMECP(intConIdx).ECpcnt= YAJL_get_string(val);
                    Endif;
                 Enddo;
                 json += ElementClose;
              When key = 'email';
                 dsWMECP(intConIdx).ECemal= YAJL_get_string(val);
              When key = 'phone';
                 node8 = YAJL_object_find(node7:key);
                 json += ',"phone":' + ElementOpen('[');
                 i8 = 0;
                 Dow YAJL_array_loop(node8:i8:node9);
                    json += ElementOpen('{');
                    i9 = 0;
                    Dow YAJL_object_loop(node9:i9:key:val);
                       BuildGiantJSON(json:key:YAJL_get_string(val));
                       Select;
                       When key = 'phone1';
                          Reset strJson;
                          ExtractJsonPhone(YAJL_object_find(node9:key)
                           :dsWMECP(intConIdx).ECarea
                           :dsWMECP(intConIdx).ECphn#
                           :strType:strJson);
                          json = %scanrpl('""':'{'+strJson+'}'
                           :json:%len(json)-1);
                       When key = 'fax';
                          Reset strJson;
                          ExtractJsonPhone(YAJL_object_find(node9:key)
                           :dsWMECP(intConIdx).ECfAra
                           :dsWMECP(intConIdx).ECfPhn
                           :strType:strJson);
                          json = %scanrpl('""':'{'+strJson+'}'
                           :json:%len(json)-1);
                       When key = 'phone2';
                          Reset strJson;
                          ExtractJsonPhone(YAJL_object_find(node9:key)
                           :dsWMECP(intConIdx).ECcAra
                           :dsWMECP(intConIdx).ECcPhn
                           :strType:strJson);
                          json = %scanrpl('""':'{'+strJson+'}'
                           :json:%len(json)-1);
                       Endsl;
                    Enddo;
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              Endsl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse Input JSON - payroll
       //==================================================
       Begsr GetParmsPayroll;
        json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
        node8 = YAJL_object_find(node7:key);
        i8 = 0;
        Dow YAJL_array_loop(node8:i8:node9);
           json += ElementOpen('{');
           intPayIdx += 1;
           dsWMEPP(intPayIdx).EPco#  = intCo#;
           dsWMEPP(intPayIdx).EPfnd  = intFnd;
           dsWMEPP(intPayIdx).EPemp# = intEmp#;
           dsWMEPP(intPayIdx).EPdiv  = intDiv;
           dsWMEPP(intPayIdx).EPfyr  = intFyr;
           // Get key value(s)
           i9 = 0;
           Dow YAJL_Object_loop(node9:i9:key:val);
              Select;
              When key = 'dates';
                 Reset strJson;
                 ExtractJsonDates(YAJL_object_find(node8:key)
                  :decEffDt:decExpDt:strJson:'CYYMMDD');
                 dsWMEPP(intPayIdx).EPefDt = %int(decEffDt);
                 If dsWMEPP(intPayIdx).EPefDt = 0;
                    dsWMEPP(intPayIdx).EPefDt = dsWDELP(intDtlIdx).ELpfDt;
                 Endif;
                 dsWMEPP(intPayIdx).EPcaDt = %int(decExpDt);
              When key = 'code';
               //***  Begin Chg  ***  04/22/22  ******************
               //dsWMEPP(intPayIdx).EPclas = %int(YAJL_get_string(val));
                 Monitor;
                    dsWMEPP(intPayIdx).EPclas = %int(YAJL_get_string(val));
                 On-Error;
                    dsWMEPP(intPayIdx).EPclas = 0;
                 Endmon;
               //***  End   Chg  ***  04/22/22  ******************
              When key = 'extension';
                 dsWMEPP(intPayIdx).EPext = YAJL_get_string(val);
              Endsl;
           Enddo;
           // Set array values to current values
           dsWMEPPout = dsWMEPP(intPayIdx);
           Exec sql
            select * into :dsWMEPPout
             from WMEPP
             where EPco# = :dsWMEPPout.EPco#
              and EPfnd  = :dsWMEPPout.EPfnd
              and EPemp# = :dsWMEPPout.EPemp#
              and EPdiv  = :dsWMEPPout.EPdiv
              and EPfyr  = :dsWMEPPout.EPfyr
              and EPefDt = :dsWMEPPout.EPefDt
              and EPclas = :dsWMEPPout.EPclas
              and EPspec = :dsWMEPPout.EPspec
              and EPext  = :dsWMEPPout.EPext;
           dsWMEPP(intPayIdx) = dsWMEPPout;
           // Retrieve new values
           i9 = 0;
           Dow YAJL_Object_loop(node9:i9:key:val);
              BuildGiantJson(json:key:YAJL_get_string(val));
              Select;
              When key = 'dates';
                 Reset strJson;
                 ExtractJsonDates(YAJL_object_find(node9:key)
                  :decEffDt:decExpDt:strJson:'CYYMMDD');
                 dsWMEPP(intPayIdx).EPefDt = %int(decEffDt);
                 If dsWMEPP(intPayIdx).EPefDt = 0;
                    dsWMEPP(intPayIdx).EPefDt = dsWDELP(intDtlIdx).ELpfDt;
                 Endif;
                 dsWMEPP(intPayIdx).EPcaDt = %int(decExpDt);
                 json =%scanrpl('""':'{'+strJson+'}':json:%len(json)-1);
              When key = 'code';
                 dsWMEPP(intPayIdx).EPclas = %int(YAJL_get_string(val));
              When key = 'extension';
                 dsWMEPP(intPayIdx).EPext = YAJL_get_string(val);
              When key = '#ofEmployees';
                 Clear dsWMEPP(intPayIdx).EPeCnt;
                 Monitor;
                    dsWMEPP(intPayIdx).EPeCnt = %int(YAJL_get_string(val));
                 On-error;
                 Endmon;
              When key = 'amount';
                 Clear dsWMEPP(intPayIdx).EPeAPr;
                 Monitor;
                    dsWMEPP(intPayIdx).EPeAPr = %dec(YAJL_get_string(val):11:2);
                 On-error;
                 Endmon;
              Endsl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse Input JSON - ratingScore
       //==================================================
       Begsr GetRatingScore;
        json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
        node8 = YAJL_object_find(node7:key);
        i8 = 0;
        Clear dsWDELP(intDtlIdx).ELscore;
        Clear dsWDELP(intDtlIdx).ELscoreDt;
        Dow YAJL_Object_loop(node8:i8:key:val);
           BuildGiantJson(json:key:YAJL_get_string(val));
           Monitor;
              Select;
              When key = 'score';
                 dsWDELP(intDtlIdx).ELscore = %dech(
                  YAJL_get_string(val):7:4);
              When key = 'received';
                 dsWDELP(intDtlIdx).ELscoreDt =
                  DtCYMD(%int(YAJL_get_string(val)));
              When key = 'bypass';
                 dsWDELP(intDtlIdx).ELscoreBP = YAJL_get_string(val);
              Endsl;
           On-error;
           Endmon;
        Enddo;
        json += ElementClose;
       Endsr;
       //==================================================
       // Parse input JSON for Rating Factors
       //==================================================
       Begsr GetParmsRatingFactors;
        json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
        node8 = YAJL_object_find(node7:key);
        i8 = 0;
        Dow YAJL_array_loop(node8:i8:node9);
           json += ElementOpen('{');
           i9 = 0;
           Dow YAJL_Object_loop(node9:i9:key:val);
              If key = 'key';
                 strRateKey = YAJL_get_string(val);
                 Leave;
              Endif;
           Enddo;
           i9 = 0;
           Dow YAJL_Object_loop(node9:i9:key:val);
              BuildGiantJson(json:key:YAJL_get_string(val));
              Select;
              When key = 'detail';
                 json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
                 node10 = YAJL_object_find(node9:key);
                 i10 = 0;
                 Dow YAJL_array_loop(node10:i10:node11);
                    json += ElementOpen('{');
                    i11 = 0;
                    Dow YAJL_Object_loop(node11:i11:key:val);
                       Reset strTypeOfEntry;
                       Reset strEntry;
                       Reset strWaivType;
                       Reset chrPrmLvl;
                       i11 = 0;
                       Dow YAJL_Object_loop(node11:i11:key:val);
                          BuildGiantJson(json:key:YAJL_get_string(val));
                          Select;
                          When key = 'typeOfEntry';
                             strTypeOfEntry = YAJL_get_string(val);
                          When key = 'entry';
                             strEntry = YAJL_get_string(val);
                          When key = 'count';
                             aryWaivCnt(intDtlIdx) =
                              %Int(YAJL_get_string(val));
                          When key = 'type';
                             strWaivType = YAJL_get_string(val);
                          EndSl;
                       Enddo;
                       Exsr StoreRates;
                    Enddo;
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              EndSl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose;
       EndSr;
       //***  Begin Add  ***  08/05/21  ******************
       //==================================================
       // Parse Input JSON - scoring information
       //==================================================
       Begsr GetParmsScoringInfo;
        json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
        node3 = YAJL_object_find(node2:key);
        i3 = 0;
        Dow YAJL_array_loop(node3:i3:node4);
           json += ElementOpen('{');
           i4 = 0;
           Dow YAJL_Object_loop(node4:i4:key:val);
              BuildGiantJson(json:key:YAJL_get_string(val));
              Select;
              When key = 'year';
                 strYearScoring = %dec(YAJL_get_string(val):3:0);
              When key = 'vendor';
                 // This is only use for GetApp
              When key = 'gradient';
                 Exsr GetParmsGradientInfo;
              When key = 'valen';
                 Exsr GetParmsValenInfo;
              EndSl;
           Enddo;
           json += ElementClose + ',';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose();
       Endsr;
       //==================================================
       // Parse Input JSON - scoring information
       //==================================================
       Begsr GetParmsValenInfo;
        json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
        node5 = YAJL_object_find(node4:key);
        Clear dsValen(i3);
        dsValen(i3).WIVSP.VSfyr = strYearScoring;
        i5 = 0;
        Dow YAJL_object_loop(node5:i5:key:val);
           BuildGiantJson(json:key:YAJL_get_string(val));
           Select;
           When key = 'info';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              node6 = YAJL_object_find(node5:key);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJson(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'score_id';
                    dsValen(i3).WIVSP.VSscrId = YAJL_get_string(val);
                 When key = 'score_key';
                    dsValen(i3).WIVSP.VSscrKey = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'insured';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              node6 = YAJL_object_find(node5:key);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJson(json:key:YAJL_get_string(val));
                 Monitor;
                    Select;
                    When key = 'exposure_shift_prediction';
                       dsValen(i3).WIVSP.VSexpShft =
                        %dec(YAJL_get_string(val):5:4);
                    When key = 'final_misclass_bin';
                       dsValen(i3).WIVSP.VSmisClsBF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'final_misclass_score';
                       dsValen(i3).WIVSP.VSmisClsSF =
                        %dec(YAJL_get_string(val):7:4);
                    When key = 'final_premium_impact_bin';
                       dsValen(i3).WIVSP.VSprmImpBF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'final_premium_impact_score';
                       dsValen(i3).WIVSP.VSprmImpSF =
                        %dec(YAJL_get_string(val):7:4);
                    When key = 'final_price_score';
                       dsValen(i3).WIVSP.VSpriceSF =
                        %dec(YAJL_get_string(val):7:4);
                    When key = 'final_recommended_company';
                       dsValen(i3).WIVSP.VSrecComF = YAJL_get_string(val);
                    When key = 'final_recommended_tier';
                       dsValen(i3).WIVSP.VSrecTierF = YAJL_get_string(val);
                    When key = 'final_risk_bin';
                       dsValen(i3).WIVSP.VSriskBF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'final_risk_bin_lower_bound';
                       dsValen(i3).WIVSP.VSriskBLF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'final_risk_bin_upper_bound';
                       dsValen(i3).WIVSP.VSriskBUF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'final_risk_interpretation';
                       dsValen(i3).WIVSP.VSriskIntF = YAJL_get_string(val);
                    When key = 'final_risk_score';
                       dsValen(i3).WIVSP.VSriskSF =
                        %dec(YAJL_get_string(val):7:4);
                    When key = 'final_technical_premium';
                       dsValen(i3).WIVSP.VStechPrmF =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'loss_ratio_relativity_prediction';
                       dsValen(i3).WIVSP.VSlossRat =
                        %dec(YAJL_get_string(val):5:4);
                    When key = 'loss_ratio_relativity_prediction_rounded';
                       dsValen(i3).WIVSP.VSlossRatR =
                        %dec(YAJL_get_string(val):5:4);
                    When key = 'misclass_bin';
                       dsValen(i3).WIVSP.VSmisClsB =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'misclass_interpretation';
                       dsValen(i3).WIVSP.VSmisClsI = YAJL_get_string(val);
                    When key = 'misclass_lower_bound';
                       dsValen(i3).WIVSP.VSmisClsL =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'misclass_upper_bound';
                       dsValen(i3).WIVSP.VSmisClsU =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'mod_price_score_factor';
                       dsValen(i3).WIVSP.VSmodPrcS =
                        %dec(YAJL_get_string(val):7:4);
                    When key = 'original_policy_term_number';
                       dsValen(i3).WIVSP.VSorgPol = YAJL_get_string(val);
                    When key = 'pi_bin';
                       dsValen(i3).WIVSP.VSpiB =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'pi_interpretation';
                       dsValen(i3).WIVSP.VSpiI = YAJL_get_string(val);
                    When key = 'pi_lower_bound';
                       dsValen(i3).WIVSP.VSpiL =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'pi_prediction';
                       dsValen(i3).WIVSP.VSpiP =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'pi_upper_bound';
                       dsValen(i3).WIVSP.VSpiU =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'plrr_adjusted';
                       dsValen(i3).WIVSP.VSplrrAdj1 =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'plrr_adjusted_floored';
                       dsValen(i3).WIVSP.VSplrrAdj2 =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'premium_adjusted';
                       dsValen(i3).WIVSP.VSprmAdj =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'price_bin';
                       dsValen(i3).WIVSP.VSpriceB =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'price_bin_lower_bound';
                       dsValen(i3).WIVSP.VSpriceBL =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'price_bin_upper_bound';
                       dsValen(i3).WIVSP.VSpriceBU =
                        %dec(YAJL_get_string(val):11:2);
                    When key = 'price_score';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    When key = 'risk_bin';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    When key = 'risk_interpretation';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    When key = 'risk_lower_bound';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    When key = 'risk_upper_bound';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    When key = 'term_effective_date';
                    // dsValen(i3).WIVSP.VS = YAJL_get_string(val);
                    //*** Begin Add *** 09/14/22 ***************************
                    When key = 'years_w_carrier';
                       dsValen(i3).WIVSP.VSyrsCar =
                        %dec(YAJL_get_string(val):3:0);
                    When key = 'original_premium_manual_initial';
                       dsValen(i3).WIVSP.VSorgMan =
                        %dec(YAJL_get_string(val):15:2);
                    //*** End   Add *** 09/14/22 ***************************
                    Endsl;
                 On-error;
                 Endmon;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           When key = 'explanations';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              Reset ig2;
              node6 = YAJL_object_find(node5:key);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJson(json:key:YAJL_get_string(val));
                 json = %scanrpl('""':ElementOpen('['):json:%len(json)-1);
                 strCategory = key;
                 node7 = YAJL_object_find(node6:key);
                 i7 = 0;
                 Dow YAJL_array_loop(node7:i7:node8);
                    json += ElementOpen('{');
                    ig2 += 1;
                    dsValen(i3).WIVSPEXP(ig2).VScategory = strCategory;
                    i8 = 0;
                    Dow YAJL_object_loop(node8:i8:key:val);
                       BuildGiantJson(json:key:YAJL_get_string(val));
                       Monitor;
                          Select;
                          When key = 'name';
                             dsValen(i3).WIVSPEXP(ig2).VSname =
                              YAJL_get_string(val);
                          When key = 'description';
                             dsValen(i3).WIVSPEXP(ig2).VSdesc =
                              YAJL_get_string(val);
                          When key = 'rank';
                             dsValen(i3).WIVSPEXP(ig2).VSseq# =
                              %int(YAJL_get_string(val));
                          When key = 'importance';
                             dsValen(i3).WIVSPEXP(ig2).VSimprtnc =
                              %dec(YAJL_get_string(val):15:12);
                          Endsl;
                       On-error;
                       Endmon;
                    Enddo;
                    json = %scanrpl(',':'':json:%len(json)-1);
                    json += ElementClose + ',';
                 Enddo;
                 json = %scanrpl(',':'':json:%len(json)-1);
                 json += ElementClose;
              Enddo;
           When key = 'report';
              json = %scanrpl('""':ElementOpen('{'):json:%len(json)-1);
              node6 = YAJL_object_find(node5:key);
              i6 = 0;
              Dow YAJL_object_loop(node6:i6:key:val);
                 BuildGiantJson(json:key:YAJL_get_string(val));
                 Monitor;
                    Select;
                    When key = 'key';
                       dsValen(i3).WIVSP.VSrptKey = YAJL_get_string(val);
                    When key = 'seq';
                       dsValen(i3).WIVSP.VSrptSeq# = %int(YAJL_get_string(val));
                    When key = 'path';
                       dsValen(i3).WIVSP.VSrptPath = YAJL_get_string(val);
                    Endsl;
                 On-error;
                 Endmon;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ElementClose;
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose('2');
       Endsr;
       //***  End   Add  ***  08/05/21  ******************
       //==================================================
       // Adds new policy info to return JSON
       //==================================================
       Begsr ReturnNewPolicyInfo;
        If Not(isAddingApp) Or intFnd <> selectedGroup# Or
         indPolError = *On;
           LeaveSr;
        EndIf;
        json += ',"newApplicationInfo":' + ElementOpen('{') +
         '"applicationNumber": "' + %Editc(intApp#:'X') + '",' +
         '"company":"' + %Editc(intCo#:'X') + '",' +
         '"group":"' + %Editc(intFnd:'X') + '",' +
         '"employer":"' + %Editc(intEmp#:'X') + '",' +
         '"year":"' + %Editc(intFyr:'X') + '",' +
         '"divisions":' + ElementOpen('[');
        For idx = 1 to intDtlIdx;
           json += ElementOpen('{') +
            '"division":"' + %Editc(dsWDELP(idx).ELdiv:'X') + '",' +
            '"rateSetCode":"' + %Trim(dsWDELP(idx).ELrtcd) + '"' +
            ElementClose + ',';
        Endfor;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ElementClose + ElementClose;
       Endsr;
       //==================================================
       // Store Rates in WDELP data structure
       //==================================================
       Begsr StoreRates;
        Select;
        When strRateKey = 'ARAP';
           Monitor;
              dsWDELP(intDtlIdx).ELarp1 = %Dec(strEntry:5:4);
           On-Error;
              dsWDELP(intDtlIdx).ELarp1 = 0;
           EndMon;
        When strRateKey = 'CONCREDIT';
           Monitor;
              dsWDELP(intDtlIdx).ELasp1 = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELasp1 = 0;
              dsWDELP(intDtlIdx).ELcref = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELasp1 = 0;
              dsWDELP(intDtlIdx).ELcref = 0;
           When dsWDELP(intDtlIdx).ELcref = 0;
              dsWDELP(intDtlIdx).ELcref = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'DEDUCT';
           dsWDELP(intDtlIdx).ELdcde = strEntry;
           Select;
           When strEntry = *Blanks;
              dsWDELP(intDtlIdx).ELdcef = 0;
           When dsWDELP(intDtlIdx).ELdcef = 0;
              dsWDELP(intDtlIdx).ELdcef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'DEV1';
           Monitor;
              dsWDELP(intDtlIdx).ELasp2 = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELasp2 = 0;
              dsWDELP(intDtlIdx).ELdvef = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELasp2 = 0;
              dsWDELP(intDtlIdx).ELdvef = 0;
           When dsWDELP(intDtlIdx).ELdvef = 0;
              dsWDELP(intDtlIdx).ELdvef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'DEV2';
           Monitor;
              dsWDELP(intDtlIdx).ELedc2 = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELedc2 = 0;
              dsWDELP(intDtlIdx).ELd2ef = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELedc2 = 0;
              dsWDELP(intDtlIdx).ELd2ef = 0;
           When dsWDELP(intDtlIdx).ELd2ef = 0;
              dsWDELP(intDtlIdx).ELd2ef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'EXPMOD';
           Monitor;
              dsWDELP(intDtlIdx).ELexm1 = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELexm1 = 0;
              dsWDELP(intDtlIdx).ELemd1 = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELexm1 = 0;
              dsWDELP(intDtlIdx).ELemd1 = 0;
           When dsWDELP(intDtlIdx).ELemd1 = 0;
              dsWDELP(intDtlIdx).ELemd1 = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'INCLIMIT';
           Select;
           When strEntry = 'Standard Limits' Or strEntry = *Blanks;
              dsWDELP(intDtlIdx).ELilcd = *Blanks;
              dsWDELP(intDtlIdx).ELilef = 0;
           Other;
              dsWDELP(intDtlIdx).ELilcd = strEntry;
              dsWDELP(intDtlIdx).ELilef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'MEMBERFEE';
           Monitor;
              dsWDELP(intDtlIdx).ELmfee = %Dec(strEntry:11:2);
           On-Error;
              dsWDELP(intDtlIdx).ELmfee = 0;
           EndMon;
        When strRateKey = 'PRMDISC';
           Select;
           When strTypeOfEntry = 'Percent';
              Clear dsWDELP(intDtlIdx).ELdisc;
              Monitor;
                 dsWDELP(intDtlIdx).ELasp3 = %Dec(strEntry:5:2);
              On-Error;
                 dsWDELP(intDtlIdx).ELasp3 = 0;
              EndMon;
           When strTypeOfEntry = 'Code';
              Clear dsWDELP(intDtlIdx).ELasp3;
              dsWDELP(intDtlIdx).ELdisc = strEntry;
           EndSl;
        When strRateKey = 'REINSUR';
           Monitor;
              dsWDELP(intDtlIdx).ELarcp = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELarcp = 0;
              dsWDELP(intDtlIdx).ELaref = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELarcp = 0;
              dsWDELP(intDtlIdx).ELaref = 0;
              dsWDELP(intDtlIdx).ELarca = 0;
              LeaveSr;
           When dsWDELP(intDtlIdx).ELaref = 0;
              dsWDELP(intDtlIdx).ELaref = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
           dsWDELP(intDtlIdx).ELarca =
            GetMinMaxAmts(intCo#:intFnd:arySt(intDtlIdx):strRateKey:
            strTypeOfEntry:strEntry:strWaivType:dsWDELP(intDtlIdx).ELpfDt:
            decMaxAmount:chrPrmLvl);
      //*** begin add ***  11/04/25  ***********************************
        When strRateKey = 'SAFETYFEE' and strTypeOfEntry = 'Amount';
           Monitor;
              dsWDELP(intDtlIdx).ELsfmn = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELsfpc = 0;
              dsWDELP(intDtlIdx).ELsfmn = 0;
              LeaveSr;
           EndMon;
      //*** end add *****  11/04/25  ***********************************
        When strRateKey = 'SAFETYFEE';
           Monitor;
              dsWDELP(intDtlIdx).ELsfpc = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELsfpc = 0;
              dsWDELP(intDtlIdx).ELsfmn = 0;
              LeaveSr;
           EndMon;
           dsWDELP(intDtlIdx).ELsfmn =
            GetMinMaxAmts(intCo#:intFnd:arySt(intDtlIdx):strRateKey:
            strTypeOfEntry:strEntry:strWaivType:dsWDELP(intDtlIdx).ELpfDt:
            decMaxAmount:chrPrmLvl);
        When strRateKey = 'SCHDEBCRED';
           Monitor;
              dsWDELP(intDtlIdx).ELdbcr = %Dec(strEntry:5:2)/100;
           On-Error;
              dsWDELP(intDtlIdx).ELdbcr = 0;
              dsWDELP(intDtlIdx).ELsdef = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELdbcr = 0;
              dsWDELP(intDtlIdx).ELsdef = 0;
              LeaveSr;
           When dsWDELP(intDtlIdx).ELsdef = 0;
              dsWDELP(intDtlIdx).ELsdef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'SUBJECT1A';
           Monitor;
              dsWDELP(intDtlIdx).ELaspm = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELaspm = 0;
              dsWDELP(intDtlIdx).ELmcef = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELaspm = 0;
              dsWDELP(intDtlIdx).ELmcef = 0;
              LeaveSr;
           When dsWDELP(intDtlIdx).ELmcef = 0;
              dsWDELP(intDtlIdx).ELmcef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'SUBJECT1B';
           Monitor;
              dsWDELP(intDtlIdx).ELasps = %Dec(strEntry:5:2);
           On-Error;
              dsWDELP(intDtlIdx).ELasps = 0;
              dsWDELP(intDtlIdx).ELscef = 0;
              LeaveSr;
           EndMon;
           Select;
           When dsWDELP(intDtlIdx).ELasps = 0;
              dsWDELP(intDtlIdx).ELscef = 0;
              LeaveSr;
           When dsWDELP(intDtlIdx).ELscef = 0;
              dsWDELP(intDtlIdx).ELscef = dsWDELP(intDtlIdx).ELpfDt;
           EndSl;
        When strRateKey = 'VOLUNTARY';
           Monitor;
              dsWDELP(intDtlIdx).ELvlca = %Dec(strEntry:11:2);
           On-Error;
              Clear dsWDELP(intDtlIdx).ELvlca;
           Endmon;
        When strRateKey = 'WAIVER';
           dsWDELP(intDtlIdx).ELwvtp = strWaivType;
           Monitor;
              Select;
              When strTypeOfEntry = 'Percent';
                 dsWDELP(intDtlIdx).ELwvpc = %Dec(strEntry:5:2);
                 Clear dsWDELP(intDtlIdx).ELfchg;
              When strTypeOfEntry = 'Amount';
                 dsWDELP(intDtlIdx).ELfchg = %Dec(strEntry:11:2);
                 Clear dsWDELP(intDtlIdx).ELwvpc;
              EndSl;
           On-error;
              Clear dsWDELP(intDtlIdx).ELfchg;
              Clear dsWDELP(intDtlIdx).ELwvpc;
           Endmon;
           dsWDELP(intDtlIdx).ELwmin =
            GetMinMaxAmts(intCo#:intFnd:arySt(intDtlIdx):strRateKey:
            strTypeOfEntry:strEntry:strWaivType:dsWDELP(intDtlIdx).ELpfDt:
            decMaxAmount:chrPrmLvl);
           dsWDELP(intDtlIdx).ELwmax = decMaxAmount;
           dsWDELP(intDtlIdx).ELwvfl = chrPrmLvl;
        EndSl;
       Endsr;
       //==================================================
       // Load Agent Assigned Groups
       //==================================================
       Begsr LoadAgtGroups;
          strSqlWMAAP = 'select AAFND, AAAGT#, AADRT#, AAEFDT, AACNDT, '+
           'AAUWTR ' +
           'from WMAAP join WMFIP On AACO# = FICO# And AAfnd = FIfnd ' +
           'where AACO#  = ' + %char(intCo#) +
           ' and AAFEIN = ' + %char(dsWIAPP.APAgtFein) +
           ' and AAMOD  = ' + %char(dsWIAPP.APAgtMod) +
           ' and AAstat = ''A'' and FIquote = ''Y''' +
           ' and (AAEFDT=0 or AAEFDT<'  + %char(dt7Eff) + ')' +
           ' and (AACNDT=0 or AACNDT>=' + %char(dt7Eff) + ')' +
           ' order by AAFND';
          Exec SQL prepare sqlWMAAP from :strSqlWMAAP;
          Exec SQL declare cursorWMAAP cursor for sqlWMAAP;
          Exec SQL open cursorWMAAP;
          Reset idx;
          DoU isEofWMAAP;
             Exec SQL fetch next from cursorWMAAP into :intAgtGrp,
              :intAgt,:intRate, :intAgtEfDt, :intAgtCxDt, :intAgtUwtr;
             Select;
             When Not IsValidFetch(sqlStt);
                isEofWMAAP = *On;
                Iter;
             When isEnvQuote and not UseGroupInPortal(intCo#:intAgtGrp:'Quote');
                Iter;
             When isEnvLive and not UseGroupInPortal(intCo#:intAgtGrp:'Policy');
                Iter;
             Endsl;
             idx = %LookUp(0:aryAgtGrp);
             aryAgtGrp(idx)  = intAgtGrp;
             aryAgt(idx)     = intAgt;
             aryAgtRate(idx) = intRate;
             aryAgtEfdt(idx) = intAgtEfDt;
             aryAgtCxDt(idx) = intAgtCxDt;
             Select;
             When dsWINOTIP1.N1dftUW = '*NONE';
             When dsWINOTIP1.N1dftUW = *blanks;
             Other;
                aryAgtUwtr(idx) = intAgtUwtr;
             Endsl;
          Enddo;
          Exec SQL close cursorWMAAP;
          // Agent not available in any group
          If idx = *zero;
             idy = %Lookup(*Blanks:errMsgArray);
             errMsgArray(idy) = '"Agency not active for carrier(s) ' +
              'as of the effective date."';
             ioRtnCode = 'Error';
          Endif;
       Endsr;
       //==================================================
       // Load Auto-Locatoin Entry
       // -If the group defines auto-generation and there are no current entries
       //   load a record to the location array
       //==================================================
       Begsr LoadAutoLocation;
        // Does the group require auto-creation
        //***  Begin Chg  ***  07/11/22  *****************
        //If dsWMFNP.FNACLR = *blank; // otherwise E/L
        //   Leavesr;
        //Endif;
        // Do locations already exist
        //Reset decCountWMALP;
        //Exec Sql
        // select count(*) into :decCountWMALP
        //  from WMALP
        //   where ALco#  = :dsWMEMPOut.EMco#
        //    and ALfnd  = :dsWMEMPOut.EMfnd
        //    and ALemp# = :dsWMEMPOut.EMemp#
        //    and ALdiv  = :dsWMEMPOut.EMdiv;
        //If decCountWMALP > 0;
        //   Leavesr;
        //Endif;
        If isLocationExists;
           isLocationExists = *off;
           Leavesr;
        Endif;
        dsWMEMPout = dsWMEMP(intDivIdx);
        //***  End   Chg  ***  07/11/22  *****************
        intLocIdx += 1;
        dsWMALP(intLocIdx).ALco#  = dsWMEMPOut.EMco#;
        dsWMALP(intLocIdx).ALfnd  = dsWMEMPOut.EMfnd;
        dsWMALP(intLocIdx).ALemp# = dsWMEMPOut.EMemp#;
        dsWMALP(intLocIdx).ALdiv  = dsWMEMPOut.EMdiv;
        dsWMALP(intLocIdx).ALelId = '0000000001';
        //***  Begin Chg  ***  07/11/22  *****************
        //dsWMALP(intLocIdx).ALelTp = dsWMFNP.FNaCLR;
        dsWMALP(intLocIdx).ALelTp = '*';
        //***  End   Chg  ***  07/11/22  *****************
        dsWMALP(intLocIdx).ALefDt = dsWMEMPOut.EMefD1;
        dsWMALP(intLocIdx).ALname = dsWMEMPOut.EMname;
        dsWMALP(intLocIdx).ALnam2 = dsWMEMPOut.EMnam2;
        dsWMALP(intLocIdx).ALnamP = dsWMEMPOut.EMnamP;
        dsWMALP(intLocIdx).ALfdNm = dsWMEMPOut.EMfdNm;
        dsWMALP(intLocIdx).ALfdN2 = dsWMEMPOut.EMfdN2;
        dsWMALP(intLocIdx).ALfdNP = dsWMEMPOut.EMfdNP;
        dsWMALP(intLocIdx).ALmAd1 = dsWMEMPOut.EMmAd1;
        dsWMALP(intLocIdx).ALmAd2 = dsWMEMPOut.EMmAd2;
        dsWMALP(intLocIdx).ALmCty = dsWMEMPOut.EMmCty;
        dsWMALP(intLocIdx).ALmSt  = dsWMEMPOut.EMmSt;
        dsWMALP(intLocIdx).ALmZip = dsWMEMPOut.EMmZip;
        dsWMALP(intLocIdx).ALpAd1 = dsWMEMPOut.EMpAd1;
        dsWMALP(intLocIdx).ALpAd2 = dsWMEMPOut.EMpAd2;
        dsWMALP(intLocIdx).ALpCty = dsWMEMPOut.EMpCty;
        dsWMALP(intLocIdx).ALpSt  = dsWMEMPOut.EMpSt;
        dsWMALP(intLocIdx).ALpZip = dsWMEMPOut.EMpZip;
        dsWMALP(intLocIdx).ALtpOB = dsWMEMPOut.EMtpOB;
        dsWMALP(intLocIdx).ALfein = dsWMEMPOut.EMfein;
        dsWMALP(intLocIdx).ALarea = dsWMEMPOut.EMarea;
        dsWMALP(intLocIdx).ALphn# = dsWMEMPOut.EMphn#;
        dsWMALP(intLocIdx).ALfAra = dsWMEMPOut.EMfaxA;
        dsWMALP(intLocIdx).ALfax# = dsWMEMPOut.EMfax#;
        dsWMALP(intLocIdx).ALcnty = dsWMEMPOut.EMcnty;
        dsWMALP(intLocIdx).ALsEmp = dsWMEMPOut.EMsEmp;
        dsWMALP(intLocIdx).ALsic  = dsWMEMPOut.EMsic;
        dsWMALP(intLocIdx).ALue#  = dsWMEMPOut.EMue#;
        dsWMALP(intLocIdx).ALnaob = dsWMEMPOut.EMnaob;
        dsWMALP(intLocIdx).ALshft = dsWMEMPOut.EMshft;
        dsWMALP(intLocIdx).ALnaic = dsWMEMPOut.EMnaic;
        dsWMALP(intLocIdx).ALncci = 'N';
        dsWMALP(intLocIdx).ALstat = 'A';
        //***  Begin Add  ***  07/11/22  *****************
        dsWMALP(intLocIdx).ALpgm = 'Auto';
        //***  End   Add  ***  07/11/22  *****************
       EndSr;
       //==================================================
       // Tag files for possible deletion
       //==================================================
       Begsr TagForDelete;
        If intApp# = 0;
           LeaveSr;
        Endif;
        Exec SQL
         Update WIAPPLOS
         Set APuser = 'DELETEAPP'
         Where APapp# = :intApp#;
        If Not(%Open(WIEML));
           Open WIEML;
        Endif;
        Setll (intCo#:intApp#) pl100;
        Dou %Eof(WIEML);
           Reade (intCo#:intApp#) pl100;
           If %Eof;
              Leave;
           Endif;
           L1user = 'DELETEAPP';
           Update pl100;
           Exec SQL
            Update WDELP
            Set ELuser = 'DELETEAPP'
            Where ELco#  = :L1Co#
              And ELfnd  = :L1fnd
              And ELemp# = :L1emp#;
           Exec SQL
            Update WDXSP
            Set XSuser = 'DELETEAPP'
            Where XSco#  = :L1Co#
              And XSfnd  = :L1fnd
              And XSemp# = :L1emp#;
           Exec SQL
            Update WMAHP
            Set AHuser = 'DELETEAPP'
            Where AHco#  = :L1Co#
              And AHfnd  = :L1fnd
              And AHemp# = :L1emp#;
           Exec SQL
            Update WMALP
            Set ALuser = 'DELETEAPP'
            Where ALco#  = :L1Co#
              And ALfnd  = :L1fnd
              And ALemp# = :L1emp#;
           Exec SQL
            Update WMECP
            Set ECuser = 'DELETEAPP'
            Where ECco#  = :L1Co#
              And ECfnd  = :L1fnd
              And ECemp# = :L1emp#;
           Exec SQL
            Update WMEMP
            Set EMuser = 'DELETEAPP'
            Where EMco#  = :L1Co#
              And EMfnd  = :L1fnd
              And EMemp# = :L1emp#;
           Exec SQL
            Update WMEPP
            Set EPuser = 'DELETEAPP'
            Where EPco#  = :L1Co#
              And EPfnd  = :L1fnd
              And EPemp# = :L1emp#;
           Exec SQL
            Update WMLDP
            Set LDuser = 'DELETEAPP'
            Where LDco#  = :L1Co#
              And LDfnd  = :L1fnd
              And LDemp# = :L1emp#;
           Exec SQL
            Update WMOOP
            Set OOuser = 'DELETEAPP'
            Where OOco#  = :L1Co#
              And OOfnd  = :L1fnd
              And OOemp# = :L1emp#;
           Exec SQL
            Update WMWVP
            Set WVuser = 'DELETEAPP'
            Where WVco#  = :L1Co#
              And WVfnd  = :L1fnd
              And WVemp# = :L1emp#;
        Enddo;
        Close WIEML;
       EndSr;
      /End-Free
     p SaveAppData     e

      //------------------------------------------------------------------------*
      // SelfRptCurrent - Check to see if division is active for current period
      //------------------------------------------------------------------------*
     pSelfRptCurrent   b
     dSelfRptCurrent   pi              n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const

     ddt7From          s              7  0
     ddt7To            s              7  0
     dintDiff          s             10i 0 inz
     dintDOW           s             10i 0 inz
     dintDOWCurrent    s             10i 0 inz
     dintPeriod        s              8  0 inz

     ddsWDELP        e ds                  extname(WDELP) qualified inz
     ddsWTBSP        e ds                  extname(WTBSP) qualified inz

      /Free
       Exec sql
        select * into :dsWDELP
         from WDELP
         where ELco# = :inCo# and ELfnd = :inFnd and
           ELemp# = :inEmp# and ELdiv = :inDiv and ELfyr = :inFyr;
        // BSFREQ, BSDOWK
        Exec sql
         select * into :dsWTBSP
          from WTBSP
          where BSco# = :inCo# and
            BSkey = :dsWDELP.ELbsCd;
        //Load effective range CYYMMDD format
        Select;
        When dsWTBSP.BSfreq = 'M';
           intPeriod=Dt5CYMD(Dt8To6(DateTo8(GetLastDay(%date()-%months(1)))));
           dt7From = %dec(%char(intPeriod)+'01':7:0);
           dt7To = DateTo7(GetLastDay(CvtToDate(dt7From)));
        Other;
           intDOW = DOWnum(dsWTBSP.BSdoWk);
           intDOWCurrent = DOW(DateTo8(%date()));
           intDiff = intDOWCurrent - ((7+intDOWCurrent)-(intDOW-intDOWCurrent));
           Select;
           When intDOWCurrent = 7;
              intDiff += 7;
           When intDOWCurrent > intDOW;
              intDiff += 7;
           Endsl;
           intPeriod = DateTo7(%date() + %days(intDiff));
           dt7From = intPeriod;
           dt7To = DateTo7(CvtToDate(dt7From) - %days(7));
        Endsl;

       Select;
       //*** Beg Chg *** 08/02/23 ****************************************
       //***When dsWDELP.ELpfDt >= dt7To;
       //***   Return *off;
       //***When dsWDELP.ELptDt < dt7From;
       //***   Return *off;
       When dsWDELP.ELpfDt > dt7To;
          Return *off;
       When dsWDELP.ELptDt <= dt7From;
          Return *off;
       //*** End Chg *** 08/02/23 ****************************************
       Other;
          Return *on;
       Endsl;
      /End-Free
     pSelfRptCurrent   e

      //------------------------------------------------------------------------*
      // SelfRptDivMtch - Check to see if any division matches self rpt flag
      //------------------------------------------------------------------------*
     p SelfRptDivMtch  b
     d SelfRptDivMtch  pi              n
     d  inCo#                         3  0 const
     d  inFnd                         3  0 const
     d  inEmp#                        9  0 const
     d  inFyr                         3  0 const
     d  inSrFl                        1    const
       //*** beg chg ***** 04/12/21 ************************
     d*** divisionCount   s              5  0 Inz(0)

       //Exec Sql
       //   Select count(*)
       //   Into :divisionCount
       //   From WDELP join WTBSP on ELco#=BSco# And ELbsCd=BSkey
       //   Where ELco# = :inCo# And ELfnd = :inFnd And
       //         ELemp# = :inEmp# And ELfYr = :inFyr And
       //         BSonli = :inSrFl;
       //
       //Exec sql
       // with
       //  maxEFT as
       //  (select EFco#, EFfnd, EFemp#, EFdiv, EFefdt, EFexdt
       //    from WMEFP
       //    where EFemp#<>0
       //     and :sysDate7>=EFefDt and (EFexDt=0 or :sysDate7<EFexDt)
       //    order by EFfnd, EFemp#, EFdiv, EFefdt),
       //  max3rdParty as
       //  (select P3co#, P3fnd, P3emp#, P3div, P3enbld, P3efdt, P3exdt
       //    from WMP3P
       //    where P3enbld = 'Y' and P3emp#<>0
       //     and :sysDate7>=P3efDt and (P3exDt=0 or :sysDate7<P3exDt)
       //    order by P3fnd, P3emp#, P3div, P3efdt),
       //  qualified as
       //  (select ELFND, ELFYR, ELEMP#, ELDIV, BSONLI, EFefDt, P3efDt
       //    from WDELP
       //    join WTBSP on ELco#=BSco# and ELbsCd=BSkey
       //    left join maxEFT on ELco#=EFco# and ELfnd=EFfnd
       //     and ELemp#=EFemp# and ELdiv=EFdiv
       //    left join max3rdParty on ELco#=P3co# and ELfnd=P3fnd
       //     and ELemp#=P3emp# and ELdiv=P3div
       //    where ELco# = :inCo# And ELfnd = :inFnd
       //     and ELemp# = :inEmp# And ELfYr = :inFyr
       //     and BSonli = :inSrFl)
       //  select count(*) into :divisionCount
       //   from qualified
       //   where EFefDt is not NULL or P3efDt is not NULL;
       //
       //If divisionCount = 0;
       //   Return *Off;
       //Else;
       //   Return *On;
       //EndIf;
       Dcl-S canPayOnline Ind Inz(*Off);
       Dcl-S chrChkType Char(10);
       Dcl-S chrCo# Char(3);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEnv Char(1); // *Blank Or Q
       Dcl-S chrEobType Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S chrDftSRD Char(1); //(Y/ )
       Dcl-S intCo# Packed(3:0);
       Dcl-S intFnd Packed(3:0);
       Dcl-S intEmp Packed(3:0);
       Dcl-S intDiv Packed(3:0);
       Dcl-S intFyr Packed(3:0);
       Dcl-S intEfdt Packed(7:0);
       Dcl-S chr3rdType Char(100) Inz('S/R Online');
       intCo# = inCo#;
       intFnd = inFnd;
       intEmp = inEmp#;
       intFyr = inFyr;
       intEfDt = sysDate7;
       chrCo# = %Editc(inCo#:'X');
       chrFnd = %Editc(inFnd:'X');
       chrEfDt = %Editc(sysDate7:'X');
       Select;
       When isEnvQuote;
          chrEnv = 'Q';
       Other;
          chrEnv = *Blank;
       EndSl;
       Exec SQL Declare divLoop Cursor For
        Select EMstrp, EMdiv
        From WMEMP
        Join WDELP
        On EMco# = ELco# And EMfnd = ELfnd And EMemp# = ELemp# And EMdiv = ELdiv
        Join WTBSP
        On ELco# = BSco# And ELbscd = BSkey
        Where BSonli = :inSrFl And ELfyr = :inFyr
        Order by EMstrp Desc, EMdiv;
       Exec Sql Open divLoop;
       Dou Not IsValidFetch(sqlStt);
          Exec Sql
           Fetch Next From divLoop Into :intDiv;
          chrEftKey = %Editc(intEmp:'X') + %Editc(intDiv:'X');
          Select;
          When Not IsValidFetch(sqlStt);
             Leave;
          When Has3rdPartyAcct(intCo#:intFnd:intEmp:intDiv:chr3rdType:
           intEfdt:intFyr:chrEnv:chrDftSrd) = *On;
             canPayOnline = *On;
             Leave;
          When GetEftStatus(chrCo#:chrEftType:chrFnd:chrEftKey:
           chrEfdt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr:
           chrEnv) = *On;
             canPayOnline = *On;
             Leave;
          EndSl;
       Enddo;
       Exec Sql Close divLoop;
       Return canPayOnline;
       //*** end chg ***** 04/12/21 ***********************
     p SelfRptDivMtch  e

      //------------------------------------------------------------------------*
      // Process input/updates for files mismatched with data-structures
      //------------------------------------------------------------------------*
     pzFileMismatch    b
     dzFileMismatch    pi
     d inFile                        25    const varying
     d inRRN                          9  0 const options(*nopass)

     ddecRRN           s              9  0 inz
      /Free
       If %parms >= 2;
          decRRN = inRRN;
       Endif;

       Select;
       When inFile = 'WAQTP';
          Exsr SrWAQTP;
       When inFile = 'WDELP';
          Exsr SrWDELP;
       When inFile = 'WDXSP';
          Exsr SrWDXSP;
       When inFile = 'WIAPP';
          Exsr SrWIAPP;
       When inFile = 'WIAPPANS';
          Exsr SrWIAPPANS;
       When inFile = 'WIAPPLOS';
          Exsr SrWIAPPLOS;
       When inFile = 'WIEMP';
          Exsr SrWIEMP;
       When inFile = 'WIGSP';
          Exsr SrWIGSP;
       When inFile = 'WIGSPDRV';
          Exsr SrWIGSPDRV;
       //***  Begin Add  ***  08/05/21  *************
       When inFile = 'WIVSP';
          Exsr SrWIVSP;
       When inFile = 'WIVSPEXP';
          Exsr SrWIVSPEXP;
       //***  End   Add  ***  08/05/21  *************
       When inFile = 'WMAHP';
          Exsr SrWMAHP;
       When inFile = 'WMALP';
          Exsr SrWMALP;
       When inFile = 'WMECP';
          Exsr SrWMECP;
       When inFile = 'WMEMP';
          Exsr SrWMEMP;
       When inFile = 'WMEPP';
          Exsr SrWMEPP;
       When inFile = 'WMLDP';
          Exsr SrWMLDP;
       When inFile = 'WMNBP';
          Exsr SrWMNBP;
       When inFile = 'WMOOP';
          Exsr SrWMOOP;
       Endsl;

       Return;

       //-------------
       Begsr SrWAQTP;
        If not %open(WAQTP);
           Open WAQTP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pqt0p dsWAQTPdef;
        Other;
           Chain decRRN pqt0p;
           If %found;
              Update pqt0p dsWAQTPdef;
           Endif;
        Endsl;
        If %open(WAQTP);
           Close WAQTP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWDELP;
        If not %open(WDELP);
           Open WDELP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pel0p dsWDELPdef;
        Other;
           Chain decRRN pel0p;
           If %found;
              Update pel0p dsWDELPdef;
           Endif;
        Endsl;
        If %open(WDELP);
           Close WDELP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWDXSP;
        If not %open(WDXSP);
           Open WDXSP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pxs0p dsWDXSPdef;
        Other;
           Chain decRRN pxs0p;
           If %found;
              Update pxs0p dsWDXSPdef;
           Endif;
        Endsl;
        If %open(WDXSP);
           Close WDXSP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIAPP;
        If not %open(WIAPP);
           Open WIAPP;
        Endif;
        Select;
        When decRRN = *zero;
           Write papip dsWIAPPdef;
        Other;
           Chain decRRN papip;
           If %found;
              Update papip dsWIAPPdef;
           Endif;
        Endsl;
        If %open(WIAPP);
           Close WIAPP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIAPPANS;
        If not %open(WIAPPANS);
           Open WIAPPANS;
        Endif;
        Select;
        When decRRN = *zero;
           Write papipans dsWIAPPANSdef;
        Other;
           Chain decRRN papipans;
           If %found;
              Update papipans dsWIAPPANSdef;
           Endif;
        Endsl;
        If %open(WIAPPANS);
           Close WIAPPANS;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIAPPLOS;
        If not %open(WIAPPLOS);
           Open WIAPPLOS;
        Endif;
        Select;
        When decRRN = *zero;
           Write papiplos dsWIAPPLOSdef;
        Other;
           Chain decRRN papiplos;
           If %found;
              Update papiplos dsWIAPPLOSdef;
           Endif;
        Endsl;
        If %open(WIAPPLOS);
           Close WIAPPLOS;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIEMP;
        If not %open(WIEMP);
           Open WIEMP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pemip dsWIEMPdef;
        Other;
           Chain decRRN pemip;
           If %found;
              Update pemip dsWIEMPdef;
           Endif;
        Endsl;
        If %open(WIEMP);
           Close WIEMP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIGSPDRV;
        If not %open(WIGSPDRV);
           Open WIGSPDRV;
        Endif;
        Select;
        When decRRN = *zero;
           Write pgsipdrv dsWIGSPDRVdef;
        Other;
           Chain decRRN pgsipdrv;
           If %found;
              Update pgsipdrv dsWIGSPDRVdef;
           Endif;
        Endsl;
        If %open(WIGSPDRV);
           Close WIGSPDRV;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIGSP;
        If not %open(WIGSP);
           Open WIGSP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pgsip dsWIGSPdef;
        Other;
           Chain decRRN pgsip;
           If %found;
              Update pgsip dsWIGSPdef;
           Endif;
        Endsl;
        If %open(WIGSP);
           Close WIGSP;
        Endif;
       Endsr;

       //***  Begin Add  ***  08/05/21  *************
       //-------------
       Begsr SrWIVSP;
        If not %open(WIVSP);
           Open WIVSP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pvsip dsWIVSPdef;
        Other;
           Chain decRRN pvsip;
           If %found;
              Update pvsip dsWIVSPdef;
           Endif;
        Endsl;
        If %open(WIVSP);
           Close WIVSP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWIVSPEXP;
        If not %open(WIVSPEXP);
           Open WIVSPEXP;
        Endif;
        Select;
        When decRRN = *zero;
           Write PVSIPEXP dsWIVSPEXPdef;
        Other;
           Chain decRRN pvsipexp;
           If %found;
              Update pvsipexp dsWIVSPEXPdef;
           Endif;
        Endsl;
        If %open(WIVSPEXP);
           Close WIVSPEXP;
        Endif;
       Endsr;
       //***  End   Add  ***  08/05/21  *************

       //-------------
       Begsr SrWMAHP;
        If not %open(WMAHP);
           Open WMAHP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pah0p dsWMAHPdef;
        Other;
           Chain decRRN pah0p;
           If %found;
              Update pah0p dsWMAHPdef;
           Endif;
        Endsl;
        If %open(WMAHP);
           Close WMAHP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMALP;
        If not %open(WMALP);
           Open WMALP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pal0p dsWMALPdef;
        Other;
           Chain decRRN pal0p;
           If %found;
              Update pal0p dsWMALPdef;
           Endif;
        Endsl;
        If %open(WMALP);
           Close WMALP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMECP;
        If not %open(WMECP);
           Open WMECP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pec0p dsWMECPdef;
        Other;
           Chain decRRN pec0p;
           If %found;
              Update pec0p dsWMECPdef;
           Endif;
        Endsl;
        If %open(WMECP);
           Close WMECP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMEMP;
        If not %open(WMEMP);
           Open WMEMP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pem0p dsWMEMPdef;
        Other;
           Chain decRRN pem0p;
           If %found;
              Update pem0p dsWMEMPdef;
           Endif;
        Endsl;
        If %open(WMEMP);
           Close WMEMP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMEPP;
        If not %open(WMEPP);
           Open WMEPP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pep0p dsWMEPPdef;
        Other;
           Chain decRRN pep0p;
           If %found;
              Update pep0p dsWMEPPdef;
           Endif;
        Endsl;
        If %open(WMEPP);
           Close WMEPP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMLDP;
        If not %open(WMLDP);
           Open WMLDP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pld0p dsWMLDPdef;
        Other;
           Chain decRRN pld0p;
           If %found;
              Update pld0p dsWMLDPdef;
           Endif;
        Endsl;
        If %open(WMLDP);
           Close WMLDP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMNBP;
        If not %open(WMNBP);
           Open WMNBP;
        Endif;
        Select;
        When decRRN = *zero;
           Write pnb0p dsWMNBPdef;
        Other;
           Chain decRRN pnb0p;
           If %found;
              Update pnb0p dsWMNBPdef;
           Endif;
        Endsl;
        If %open(WMNBP);
           Close WMNBP;
        Endif;
       Endsr;

       //-------------
       Begsr SrWMOOP;
        If not %open(WMOOP);
           Open WMOOP;
        Endif;
        Select;
        When decRRN = *zero;
           Write poo0p dsWMOOPdef;
        Other;
           Chain decRRN poo0p;
           If %found;
              Update poo0p dsWMOOPdef;
           Endif;
        Endsl;
        If %open(WMOOP);
           Close WMOOP;
        Endif;
       Endsr;
      /End-free
     pzFileMismatch    e
