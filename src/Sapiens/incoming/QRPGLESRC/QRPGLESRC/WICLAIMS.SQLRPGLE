     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h DftActGrp(*No) ActGrp(*Caller)
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     h/Title WICLAIMS - Portal Claims Requests
      *==========================================================
      * Description: Stores procedures to process various
      *              portal requests for claim JSON returns
      *==========================================================
      * Revisions:
      *
      *  05/08/25  C:342 Project C150WGB
      *                  Recompiled only.
      *
     *  08/08/22  C:62  Ticket: 103188(146)
     *                  Deal with invalid data.
      *
     *  12/09/21  C:33  Support: 99529
     *                  Trim financial json (no comments)
      *
      * 07/07/21 C:062 Project WC2781
      *                Add the following services:
      *                -DeleteClaim
      *                -GetClaim
      *                -SaveClaim
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      *==========================================================

      *Entry
     dRequestPgm       pr                  ExtPgm('WICLAIMS')
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

     d*** Begin Add *** 07/07/21 *******************************************
     d ClaimBackfill   pr                  ExtPgm('W9111C')
     d  inQueue                      10
     d  inOption                      1
     d  incopy                        1

     d DartsListing    pr                  ExtPgm('SIDARTS')
     d  inRequestD                  256
     d  inUserD                      10
     d  inParmsD                       a   Len(8500000) Varying
     d  outJSOND                       a   Len(8500000) Varying
     d  ioRtnCodeD                   10
     d*** End   Add *** 07/07/21 *******************************************

     d*---------------------------------------------------------------------
     d* Data Structures
     d*---------------------------------------------------------------------
     d psds          esds                  ExtName(szq1p)

     D dbfds           ds
     D  dbrrn                397    400b 0

     ddsAdjuster     e ds                  extname(WMUPP) qualified inz
     d*** Begin Add *** 07/07/21 ***************************************
     ddsSupervisor   e ds                  extname(WMUPP) qualified inz
     d*** End   Add *** 07/07/21 ***************************************
     ddsWDELP        e ds                  extname(WDELP) qualified inz
     ddsWMCHP        e ds                  extname(WMCHP) qualified inz
     ddsWMCMP        e ds                  extname(WMCMP) qualified inz
     ddsWMLLP        e ds                  extname(WMLLP) qualified inz
     ddsToDate         ds                  qualified inz
     d amount                        15  2
     d paid                          15  2
     d incurred                      15  2
     d recoveries                    15  2
     d netRecoveries                 15  2
     d available                     15  2
     d strAmount                     25    varying
     d strPaid                       25    varying
     d strIncurred                   25    varying
     ddsFinancials     ds                  qualified inz dim(5)
     d code                           1    varying
     d type                           2    varying
     d description                   40    varying
     d inception                           likeds(dsToDate)
     d month                               likeds(dsToDate)
     d#ofFin           c                   10
     daryAR            s              9  2 dim(#ofFin)
     daryRR            s              9  2 dim(#ofFin)
     daryRT            s              2    dim(#ofFin)

     d*---------------------------------------------------------------------
     d* Work Fields
     d*---------------------------------------------------------------------
     d intCo#          s              3  0
     d strAuthority    s             10a
     d strCo#          s              3a
     d strErrorMsg     s            500a   Varying
     d strSqlCursor    s               a   Len(32000) Varying
     d sysDate         s               d
     d sysDate7        s              7  0
     d sysTime6        s              6  0
     d x#RRN           s              9  0 inz(0)

     d array           s               a   Len(32767)
     d clientID        s              3a
     d comma           s              1a
     d errMsgArray     s            500    dim(maxSize)

     d decCount        s             10i 0 inz(0)
     d maxCount        s             10i 0 inz(500)

     d*** Begin Add *** 07/07/21 *******************************************
     d inQueue         s             10    Inz('QPRINT')
     d inOption        s              1    Inz('Y')
     d inCopy          s              1    Inz('N')

     d inRequestD      s            256
     d inUserD         s             10
     d inParmsD        s               a   Len(8500000) Varying
     d outJSOND        s               a   Len(8500000) Varying
     d ioRtnCodeD      s             10

     d* Claim Header Record
     d                 ds                                                       X-REF Claim Numbers
     d  R1Data                      733                                         Record 01
     d  R1rcdType              1      2
     d  R1fnd                  3      5
     d  R1emp#                 6     14
     d  R1div                 15     19
     d  R1ss#                 20     28
     d  R1acdt                29     36
     d  R1clm#                37     76
     d  R1clm7                40     46
     d  R1cap#                77     96
     d  R1uptp                97     97
     d  R1fyr                 98    100
     d  R1case               101    107
     d  R1fill               108    733

     d* Claim Detail Record
     d                 ds                                                       Clm info & reserves
     d  R2Data                      800                                         Record 02
     d  R2rcdType                     2    overlay(R2Data:1)
     d  R2fnd                         3    overlay(R2Data:3)
     d  R2clm#                       40    overlay(R2Data:6)
     d  R2fsn                        15    overlay(R2Data:46)
     d  R2lsn                        30    overlay(R2Data:61)
     d  R2mid                         1    overlay(R2Data:91)
     d  R2adr1                       40    overlay(R2Data:92)
     d  R2adr2                       40    overlay(R2Data:132)
     d  R2cty                        25    overlay(R2Data:172)
     d  R2st                          2    overlay(R2Data:197)
     d  R2zip                         9    overlay(R2Data:199)
     d  R2ctry                        3    overlay(R2Data:208)
     d  R2adr3                       40    overlay(R2Data:211)
     d  R2sex                         1    overlay(R2Data:251)
     d  R2msts                        1    overlay(R2Data:252)
     d  R2dtob                        8    overlay(R2Data:253)
     d  R2ltfl                        1    overlay(R2Data:261)
     d  R2stat                        1    overlay(R2Data:262)
     d  R2acds                       60    overlay(R2Data:263)
     d  R2acd2                       60    overlay(R2Data:323)
     d  R2natr                        2    overlay(R2Data:383)
     d  R2part                        2    overlay(R2Data:385)
     d  R2caus                        2    overlay(R2Data:387)
     d  R2lscv                        2    overlay(R2Data:389)
     d  R2covt                        2    overlay(R2Data:391)
     d  R2jrst                        2    overlay(R2Data:393)
     d  R2rcvd                        8    overlay(R2Data:395)
     d  R2rpdt                        8    overlay(R2Data:403)
     d  R2dod                         8    overlay(R2Data:411)
     d  R2dept                       10    overlay(R2Data:419)
     d  R2mclm                        7    overlay(R2Data:429)
     d  R2clas                        5  0 overlay(R2Data:436)
     d  R2ocds                       40    overlay(R2Data:441)
     d  R2aww                         9  2 overlay(R2Data:481)
     d  R2hidt                        8    overlay(R2Data:490)
     d  R2dtlw                        8    overlay(R2Data:498)
     d  R2lump                        1    overlay(R2Data:506)
     d  R2mtst                        2    overlay(R2Data:507)
     d  R2frau                        1    overlay(R2Data:509)
     d  R2litg                        1    overlay(R2Data:510)
     d  R2cvfl                        1    overlay(R2Data:511)
     d  R2subr                        1    overlay(R2Data:512)
     d  R2dny                         1    overlay(R2Data:513)
     d  R2mctp                        2    overlay(R2Data:514)
     d  R2vorc                        1    overlay(R2Data:516)
     d  R2rt01                        2    overlay(R2Data:517)
     d  R2rr01                        9  2 overlay(R2Data:519)
     d  R2ar01                        9  2 overlay(R2Data:528)
     d  R2rt02                        2    overlay(R2Data:537)
     d  R2rr02                        9  2 overlay(R2Data:539)
     d  R2ar02                        9  2 overlay(R2Data:548)
     d  R2rt03                        2    overlay(R2Data:557)
     d  R2rr03                        9  2 overlay(R2Data:559)
     d  R2ar03                        9  2 overlay(R2Data:568)
     d  R2rt04                        2    overlay(R2Data:577)
     d  R2rr04                        9  2 overlay(R2Data:579)
     d  R2ar04                        9  2 overlay(R2Data:588)
     d  R2rt05                        2    overlay(R2Data:597)
     d  R2rr05                        9  2 overlay(R2Data:599)
     d  R2ar05                        9  2 overlay(R2Data:608)
     d  R2rt06                        2    overlay(R2Data:617)
     d  R2rr06                        9  2 overlay(R2Data:619)
     d  R2ar06                        9  2 overlay(R2Data:628)
     d  R2rt07                        2    overlay(R2Data:637)
     d  R2rr07                        9  2 overlay(R2Data:639)
     d  R2ar07                        9  2 overlay(R2Data:648)
     d  R2rt08                        2    overlay(R2Data:657)
     d  R2rr08                        9  2 overlay(R2Data:659)
     d  R2ar08                        9  2 overlay(R2Data:668)
     d  R2rt09                        2    overlay(R2Data:677)
     d  R2rr09                        9  2 overlay(R2Data:679)
     d  R2ar09                        9  2 overlay(R2Data:688)
     d  R2rt10                        2    overlay(R2Data:697)
     d  R2rr10                        9  2 overlay(R2Data:699)
     d  R2ar10                        9  2 overlay(R2Data:708)
     d  R2dedr                        9  2 overlay(R2Data:717)
     d  R2idt                         8    overlay(R2Data:726)
     d  R2cata                        2    overlay(R2Data:734)
     d  R2lsrv                        2    overlay(R2Data:736)
     d  R2ncci                        2    overlay(R2Data:738)
     d  R2clex                        3    overlay(R2Data:740)
     d  R2supr                        3    overlay(R2Data:743)
     d  R2ctg1                       10    overlay(R2Data:746)
     d  R2ipct                        5    overlay(R2Data:756)

     d* Payment Detail Record
     d  R3Data         ds           733    qualified Dim(9999)                  Record 03
     d  R3rcdType                     2    overlay(R3Data:1)
     d  R3fnd                         3    overlay(R3Data:3)
     d  R3clm#                       40    overlay(R3Data:6)
     d  R3trdt                        8    overlay(R3Data:46)
     d  R3type                        1    overlay(R3Data:54)
     d  R3recd                        2    overlay(R3Data:55)
     d  R3br2                         2    overlay(R3Data:57)
     d  R3chk#                        7  0 overlay(R3Data:59)
     d  R3amt                         9  2 overlay(R3Data:66)
     d  R3amts                        1    overlay(R3Data:75)
     d  R3pnam                       40    overlay(R3Data:76)
     d  R3podt                        8    overlay(R3Data:116)
     d  R3frdt                        8    overlay(R3Data:124)
     d  R3todt                        8    overlay(R3Data:132)
     d  R3link                       12    overlay(R3Data:140)
     d  R3pgtp                        1    overlay(R3Data:152)
     d  R3eck#                       10    overlay(R3Data:153)
     d  R3fill                      569    overlay(R3Data:163)

     d* Reserve History Record
     d  R4Data         ds           733    qualified Dim(9999)                  Record 03
     d  R4rcdType                     2    overlay(R4Data:1)
     d  R4fnd                         3    overlay(R4Data:3)
     d  R4clm#                       40    overlay(R4Data:6)
     d  R4efdt                        8    overlay(R4Data:46)
     d  R4rt01                        2    overlay(R4Data:54)
     d  R4su01                        9  2 overlay(R4Data:56)
     d  R4sn01                        1    overlay(R4Data:65)
     d  R4rt02                        2    overlay(R4Data:66)
     d  R4su02                        9  2 overlay(R4Data:68)
     d  R4sn02                        1    overlay(R4Data:77)
     d  R4rt03                        2    overlay(R4Data:78)
     d  R4su03                        9  2 overlay(R4Data:80)
     d  R4sn03                        1    overlay(R4Data:89)
     d  R4rt04                        2    overlay(R4Data:90)
     d  R4su04                        9  2 overlay(R4Data:92)
     d  R4sn04                        1    overlay(R4Data:101)
     d  R4rt05                        2    overlay(R4Data:102)
     d  R4su05                        9  2 overlay(R4Data:104)
     d  R4sn05                        1    overlay(R4Data:113)
     d  R4rt06                        2    overlay(R4Data:114)
     d  R4su06                        9  2 overlay(R4Data:116)
     d  R4sn06                        1    overlay(R4Data:125)
     d  R4rt07                        2    overlay(R4Data:126)
     d  R4su07                        9  2 overlay(R4Data:128)
     d  R4sn07                        1    overlay(R4Data:137)
     d  R4rt08                        2    overlay(R4Data:138)
     d  R4su08                        9  2 overlay(R4Data:140)
     d  R4sn08                        1    overlay(R4Data:149)
     d  R4rt09                        2    overlay(R4Data:150)
     d  R4su09                        9  2 overlay(R4Data:152)
     d  R4sn09                        1    overlay(R4Data:161)
     d  R4rt10                        2    overlay(R4Data:162)
     d  R4su10                        9  2 overlay(R4Data:164)
     d  R4sn10                        1    overlay(R4Data:173)
     d  R4fill                      560    overlay(R4Data:174)

     d* Claim History Record
     d  R5Data         ds           733    qualified Dim(9999)                  Record 03
     d  R5rcdType                     2    overlay(R5Data:1)
     d  R5fnd                         3    overlay(R5Data:3)
     d  R5clm#                       40    overlay(R5Data:6)
     d  R5ocst                        1    overlay(R5Data:46)
     d  R5efdt                        8    overlay(R5Data:47)
     d  R5fill                      679    overlay(R5Data:55)

     d* Claim Transfer/Deletion Record
     d                 ds                                                       Claim Transfer Chgs
     d  R6Data                      733                                         Record 06
     d  R6rcdType                     2    overlay(R6Data:1)
     d  R6fnd                         3    overlay(R6Data:3)
     d  R6clm#                       40    overlay(R6Data:6)
     d  R6trdt                        8    overlay(R6Data:46)
     d  R6trtp                        1    overlay(R6Data:54)
     d  R6grp#                        3    overlay(R6Data:55)
     d  R6emp#                        9    overlay(R6Data:58)
     d  R6div#                        5    overlay(R6Data:67)
     d  R6ss#                         9    overlay(R6Data:72)
     d  R6acdt                        8    overlay(R6Data:81)
     d  R6pol#                       20    overlay(R6Data:89)

     d* Claim Darts Master Record
     d  R7Data         ds           733    qualified Dim(9999)                  Record 03
     d  R7rcdType                     2    overlay(R7Data:1)
     d  R7fnd                         3    overlay(R7Data:3)
     d  R7clm#                       40    overlay(R7Data:6)
     d  R7seq#                        5  0 overlay(R7Data:46)
     d  R7cdat                        8  0 overlay(R7Data:51)
     d  R7trdt                        8  0 overlay(R7Data:59)
     d  R7trtm                        6  0 overlay(R7Data:67)
     d  R7dcdt                        8  0 overlay(R7Data:73)
     d  R7difl                        1    overlay(R7Data:81)
     d  R7rmcd                        5    overlay(R7Data:82)
     d  R7comm                      275    overlay(R7Data:87)
     d  R7user                       10    overlay(R7Data:362)

     d* Claim Darts Additonal Record
     d  R8Data         ds           733    qualified Dim(9999)                  Record 03
     d  R8rcdType                     2    overlay(R8Data:1)
     d  R8fnd                         3    overlay(R8Data:3)
     d  R8clm#                       40    overlay(R8Data:6)
     d  R8seq#                        5  0 overlay(R8Data:46)
     d  R8asq#                        5  0 overlay(R8Data:51)
     d  R8rmcd                        5    overlay(R8Data:56)
     d  R8cm75                       75    overlay(R8Data:61)
     d*** End   Add *** 07/07/21 *******************************************

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d*** Begin Add *** 07/07/21 *******************************************
     d i4              s             10i 0 inz
     d i5              s             10i 0 inz
     d*** End   Add *** 07/07/21 *******************************************
     d inJson          s               a   Len(2000000) varying
     d json            s               a   len(8500000) varying
     d key             s             50a   varying
     d*** Begin Add *** 07/07/21 *******************************************
     d key0            s             50a   varying
     d*** End   Add *** 07/07/21 *******************************************
     d key1            s             50a   varying
     d key2            s             50a   varying
     d key3            s             50a   varying
     d*** Begin Add *** 07/07/21 *******************************************
     d key4            s             50a   varying
     d key5            s             50a   varying
     d*** End   Add *** 07/07/21 *******************************************
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d*** Begin Add *** 07/07/21 *******************************************
     d node4           s                   Like(yajl_val)
     d node5           s                   Like(yajl_val)
     d*** End   Add *** 07/07/21 *******************************************
     d docNode         s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     d*---------------------------------------------------------------------
     d* Constants
     d*---------------------------------------------------------------------
     d maxSize         c                   999

     d*---------------------------------------------------------------------
     d* Proto Types
     d*---------------------------------------------------------------------
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a


      /Copy sPrAtrium
      /Copy sPrDate
      /Copy sPrJson
      *** Begin Add *** 07/07/21 ****************************************
      /copy sPrRunCmd
      *** End   Add *** 07/07/21 ****************************************
      /Copy sPrSecure
      /Copy sPrSql
      /Copy sPrString
      /copy sPrLetter
      /copy sPrXML
      /Copy wPrAgent
      /Copy wPrClaims
      /Copy wPrClmPmt
      /Copy wPrEmplr
      /Copy wPrGroup
      /copy wPrRateSet
      /copy wPrSecure
      /copy wPrTable
      /Copy wPrUsrPrms

      /Include yajl_h

      *==========================================================
      * Main
      *==========================================================
      /Free

       // Change to Commit *None
       Exec Sql Set Option Commit = *None;

       // Program Start
       Exsr PgmStart;

       Select;
       When inRequest = 'GetClaimSummary';
          GetClaimSummary();
       When inRequest = 'ListClaims';
          ClaimSearch();
       When inRequest = 'ListClaimPayments';
          ListClaimPayments();
       //*** Begin Add *** 07/07/21 ****************************************
       When inRequest = 'DeleteClaim';
          ImportClaim('D');
       When inRequest = 'GetClaim';
          GetClaimSummary('{"includePayments":"true","includeDARTS":"true"}');
       When inRequest = 'SaveClaim';
          ImportClaim('S');
       //*** End   Add *** 07/07/21 ****************************************
       Other;
          Exsr BuildStub;
       EndSl;

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
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       Endsr;
      /End-free

       //===================================================================
       // Claim Search
       //===================================================================
     pClaimSearch      b
     dClaimSearch      pi

     d dsWMCMP       e ds                  ExtName(WMCMPds)
     d WMCMPnull       s              5i 0 dim(33)

     dmaxDiv           c                   999
     d filter          ds                  Qualified
     d  groupName                    40a
     d  group#                        3a
     d  agencyName                   40a
     d  agencyFein                    9a
     d  agencyMod                     3a
     d  insuredName                  40a
     d  insuredFein                   9a
     d  emp#                          9a
     d  division                      5a   dim(maxDiv)
     d  divisionInt                   5  0 dim(maxDiv) inz(-99999)
     d  claimantFSN                  15a
     d  claimantLSN                  30a
     d  claimantMid                   1a
     d  location#                    10a
     d  totalIncurred                12a
     d  policy#                      25a
     d  policyStatus                  1a
     d  claim#                        7a
     d  claimStatus                   1a
     d  fromAccDate                   8a
     d  toAccDate                     8a
     d  recentDays                   10a

     d SRPRT1          pr                  extpgm('SRPRT1')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##fyr                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##date                         8    const
     d ##amount                      11
     d ##amount2                     11
     d ##term                         1

     d group#          s              3  0
     d fein            s              9  0
     d mod             s              3  0
     d polGroup#       s              3  0 Inz(0)
     d polEmp#         s              9  0 Inz(0)
     d polYear         s              3  0 Inz(0)
     d emp#            s              9  0
     d division        s              5  0 Inz(0)
     d case#           s              7  0 Inz(0)
     d badData         s               n   Inz(*Off)
     d dateError       s               n   Inz(*Off)
     d moreRows        s               n   Inz(*On)
     d subset          s               n   Inz(*Off)
     d meetSS          s               n   Inz(*On)
     d fromAccDt7      s              7  0
     d fromAccDt8      s              8  0
     d strFromAccDt7   s              7
     d strRecentDt7    s              7
     d toAccDt7        s              7  0
     d toAccDt8        s              8  0
     d strToAccDt7     s              7
     D policyNumber    s             15a
     D status          s             20a
     d totalInc        s             11s 2
     d totalIncIn      s             11s 2
     d qteFlg          s              1a   inz(' ')
     d recentDays      s             10i 0 inz
     d count           s              9s 0 Inz(0)

     d jsonDiv         s               a   len(32000) varying inz
     D sqlStmtStr      s               a   len(32000) varying

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
       Exsr BldReturnInfo;
       If %len(json) > 1;
          json = %scanrpl(',':'':json:%len(json)-1);
       Endif;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJson = json;

       //=================================================================
       // Build Claim Search                                                quests
       //=================================================================
       Begsr BldClaimSearch;

          // Start Claim Listing
          json = %trim(json) + ' "claimList":[ ';

          // Build agency sql statement
          sqlStmtStr = 'Select a.CMco#, a.CMfnd, a.CMfYr, a.CMcase, ' +
             'a.CMemp#, a.CMdiv, a.CMfsN, a.CMlsN, a.CMmid, a.CMstat, ' +
             'a.CMacDt, a.CMrr01, a.CMrr02, a.CMrr03, a.CMrr04, a.CMrr05, ' +
             'a.CMrr06, a.CMrr07, a.CMrr08, a.CMrr09, a.CMrr10, ' +
             'a.CMar01, a.CMar02, a.CMar03, a.CMar04, a.CMar05, ' +
             'a.CMar06, a.CMar07, a.CMar08, a.CMar09, a.CMar10, ' +
             'a.CMiDt, ' +
             'b.EMname ' +
             'from WMCMP a join WMEMP b on ' +
             'a.CMco#=b.EMco# And a.CMfnd=b.EMfnd And a.CMemp#=b.EMemp# And ' +
             'a.CMdiv=b.EMdiv join WMFNP c on a.CMco#=c.FNco# And ' +
             'a.CMfnd=c.FNfnd';
          sqlStmtStr = 'with ' + SqlWithPolicyInfo + ' ' +
           'Select a.CMco#, a.CMfnd, a.CMfYr, a.CMcase, ' +
           'a.CMemp#, a.CMdiv, a.CMfsN, a.CMlsN, a.CMmid, a.CMstat, ' +
           'a.CMacDt, a.CMrr01, a.CMrr02, a.CMrr03, a.CMrr04, a.CMrr05, ' +
           'a.CMrr06, a.CMrr07, a.CMrr08, a.CMrr09, a.CMrr10, ' +
           'a.CMar01, a.CMar02, a.CMar03, a.CMar04, a.CMar05, ' +
           'a.CMar06, a.CMar07, a.CMar08, a.CMar09, a.CMar10, ' +
           'a.CMiDt, ' +
           'b.EMname ' +
           ' from WMCMP a ' +
           ' join policy_info b on ' +
           '  a.CMco#=b.ELco# And a.CMfnd=b.ELfnd And a.CMemp#=b.ELemp# And ' +
           '  a.CMdiv=b.ELdiv and a.CMfyr=b.ELfyr ' +
           ' join WMFNP c ' +
           '  on a.CMco#=c.FNco# And a.CMfnd=c.FNfnd';

          // Add on Filters
          If filter.group#        <> ' ' Or filter.groupName   <> ' ' Or
             filter.agencyName    <> ' ' Or filter.agencyFein  <> ' ' Or
             filter.agencyMod     <> ' ' Or filter.insuredName <> ' ' Or
             filter.insuredFein   <> ' ' Or filter.division(1) <> ' ' Or
             filter.emp#          <> ' ' or
             filter.claimantFSN   <> ' ' Or filter.claimantLSN <> ' ' Or
             filter.claimantMid   <> ' ' Or filter.location#   <> ' ' Or
             filter.totalIncurred <> ' ' Or filter.policy#     <> ' ' Or
             filter.policyStatus  <> ' ' Or filter.claim#      <> ' ' Or
             filter.claimStatus   <> ' ' Or filter.fromAccDate <> ' ' Or
             filter.toAccDate     <> ' ' or filter.recentDays  <> ' ';

             sqlStmtStr = %Trim(sqlStmtStr) + ' Where ';

             // Group # Filter;
             If filter.group# <> ' ';
                subset = *On;
                sqlStmtStr = %Trim(sqlStmtStr) + ' CMfnd = '  +
                             %Trim(filter.group#);
             Else;
                // Group Name Filter;
                If filter.groupName <> ' ';
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(FNname) like ''%'' || Upper(''' +
                      %Trim(filter.groupName) + ''') || ''%''';
                EndIf;
             EndIf;

             // Agency Filters
             If filter.agencyFein <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                EndIf;
                subset = *on;
                sqlStmtStr += ' AGfein = ' + filter.agencyFein;
                If filter.agencyMod <> ' ';
                   sqlStmtStr += ' and AGmod = ' + filter.agencyMod;
                Endif;
             Endif;

             // Insured Filters
             If filter.insuredFein <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                EndIf;
                subset = *on;
                sqlStmtStr += ' digits(EMfnd)||digits(EMemp#) in ' +
                 '(select digits(EMfnd)||digits(EMemp#) ' +
                 '  from WMEMP where EMfein = ' + filter.insuredFein + ')';
              //sqlStmtStr += ' EMfein = ' + filter.insuredFein;
             Else;
                // Insured Name Filter;
                If filter.insuredName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' Upper(EMname) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   Else;
                      sqlStmtStr = %Trim(sqlStmtStr) +
                         ' And Upper(EMname) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Employer Filters
             If filter.emp# <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                EndIf;
                sqlStmtStr += ' CMemp# = ' + filter.emp#;
                subset = *On;
             EndIf;

             // Division Filters
             If filter.division(1) <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                EndIf;
                sqlStmtStr += ' CMdiv in (';
                For i1 = 1 to maxDiv;
                   Select;
                   When filter.division(i1) = *blank;
                      Iter;
                   Endsl;
                   sqlStmtStr += %trim(filter.division(i1)) + ',';
                Endfor;
                sqlStmtStr = %scanrpl(',':'':sqlStmtStr:%len(sqlStmtStr)-1);
                sqlStmtStr += ')';
                subset = *On;
             EndIf;

             // Claimant First Name Filter;
             If filter.claimantFsn <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(CMfsN) like ''%'' || Upper(''' +
                      %Trim(filter.claimantFsn) + ''') || ''%''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(CMfsN) like ''%'' || Upper(''' +
                      %Trim(filter.claimantFsn) + ''') || ''%''';
                EndIf;
             EndIf;

             // Claimant Last Name Filter;
             If filter.claimantLsn <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(CMlsN) like ''%'' || Upper(''' +
                      %Trim(filter.claimantLsn) + ''') || ''%''';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(CMlsN) like ''%'' || Upper(''' +
                      %Trim(filter.claimantLsn) + ''') || ''%''';
                EndIf;
             EndIf;

             // Claimant Middle Initial Filter;
             If filter.claimantMID <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' Upper(CMmid) = Upper(''' +
                      %Trim(filter.claimantMID) + ''')';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And Upper(CMmid) = Upper(''' +
                      %Trim(filter.claimantMid) + ''')';
                EndIf;
             EndIf;

             // Location Filter;
             If filter.location# <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' CMctg1 = ''' + %Trim(filter.location#) + ''')       ';
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) +
                      ' And CMctg1 = ''' + %Trim(filter.location#) + ''')';
                EndIf;
             EndIf;

             // Policy # Filters
             If filter.policy# <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and';
                Endif;
                sqlStmtStr += ' policy# = ''' + %trim(filter.policy#) + '''';
                subset = *On;
             EndIf;

             // Claim # Filters
             If filter.claim# <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' CMcase = '  +
                                %EditC(case#:'X');
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' And CMcase = '  +
                                %EditC(case#:'X');
                EndIf;
             EndIf;

             // Claim Status
             If filter.claimStatus <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                Endif;
                subset = *On;
                sqlStmtStr += ' CMstat = ''' + %Trim(filter.claimStatus) + '''';
             EndIf;

             // From Accident Date Filters
             If filter.fromAccDate <> ' ' And
                filter.fromAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' And CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                EndIf;
             EndIf;

             // To Accident Date Filters
             If filter.toAccDate <> ' ' And
                filter.toAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                Else;
                   sqlStmtStr = %Trim(sqlStmtStr) + ' And CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                EndIf;
             EndIf;

             // Recent Days Filters
             If strRecentDt7 <> ' ';
                If subset = *on;
                   sqlStmtStr += ' and ';
                Endif;
                subset = *on;
                sqlStmtStr += 'CMiDt >= '  + strRecentDt7;
             EndIf;
          EndIf;

          sqlStmtStr += ' order by CMiDt desc';

          Exec SQL close clmCursor;
          // Prepare the SQL statement
          Exec SQL prepare clmStmt from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare clmCursor cursor for clmStmt;

          // Run SQL
          Exec SQL open clmCursor;

          // If row was fetched then process
          DoU moreRows = *Off;

             // Fetch next Record
             Exec SQL fetch next from clmCursor into :dsWMCMP :WMCMPnull;

             Select;
             When not IsValidFetch(sqlstt);
                moreRows = *off;
                Iter;
             Endsl;

             // Build Record
             Exsr BldClmSearchRcd;

          EndDo;
          Exec SQL close clmCursor;

          // End the Claim Listing
          json = %trim(json) + ' ] ';

       EndSr;

       //=================================================================
       // Build the Claim Search Record
       //=================================================================
       Begsr BldClmSearchRcd;
          If not UseGroupInPortal(CMco#:CMfnd:'Claim');
             Leavesr;
          Endif;

          policyNumber = %EditC(CMfnd:'X') +
                         %EditC(CMemp#:'X') +
                         %EditC(CMfYr:'X');

          status       = GetQuoteSts(CMco#:
                         CMfnd:CMemp#:
                         CMdiv:CMfYr:
                         qteFlg);

          totalInc     = CMrr01 + CMrr02 + CMrr03 + CMrr04 + CMrr05 +
                         CMrr06 + CMrr07 + CMrr08 + CMrr09 + CMrr10 +
                         CMar01 + CMar02 + CMar03 + CMar04 + CMar05 +
                         CMar06 + CMar07 + CMar08 + CMar09 + CMar10;

          // Complete filter
          meetSS = *On;
          If (filter.policyStatus <> ' ' And filter.policyStatus <> status) Or
             (filter.totalIncurred <> ' ' And  totalInc > totalIncIn);
             meetSS = *Off;
             LeaveSR;
          EndIf;

          If count > 0;
             json = %Trim(json) + ',';
          EndIf;
          count = count + 1;

        json += '{';
        json += ClaimJSONData(CMco#:CMfnd:CMfyr:CMcase) + ',';
        json += PolicyJSONData(CMco#:CMfnd:CMemp#:CMdiv:CMfyr) + ',';
        json += '"employer":{' +
         BuildJsonNode('number':%EditC(CMemp#:'X')) + ',' +
         BuildJsonNode('division':%EditC(CMdiv:'X')) +
         '},';
        json += '"group":{' +
         BuildJsonNode('number':%EditC(CMfnd:'X')) +
         '},';
        json += '}';

        json = %scanrpl(',':'':json:%len(json)-1);

       Endsr;

       //=================================================================
       // Build Return Information
       //=================================================================
       Begsr BldReturnInfo;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;

          // Build Claim Search Results
          ExSr BldClaimSearch;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          node0 = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 <> ' ';
             AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
             AddErrorMessageJson(errMsg500:ioRtnCode);
             Leavesr;
          Endif;

        json += ' "requestParms":{';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'group';
              Exsr GetParmsGroup;
           When key = 'agency';
              Exsr GetParmsAgency;
           When key = 'insured';
              Exsr GetParmsInsured;
           When key = 'claimant';
              Exsr GetParmsClaimant;
           When key = 'claim';
              Exsr GetParmsClaim;
           When key = 'policy';
              Exsr GetParmsPolicy;
           When key = 'location#';
              filter.location# = YAJL_get_string(val);
           When key = 'totalIncurred';
              filter.totalIncurred = YAJL_get_string(val);
           When key = 'recentDays';
              filter.recentDays = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';

          // Free up the space by removing the tree
          yajl_tree_free(node0);

          Exsr Validate;
       Endsr;

       //=================================================================
       // Break parms out from inParms - Agency
       //=================================================================
       Begsr GetParmsAgency;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              filter.agencyName  = YAJL_get_string(val);
           When key = 'fein';
              filter.agencyFein = YAJL_get_string(val);
           When key = 'mod';
              filter.agencyMod  = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Claim
       //=================================================================
       Begsr GetParmsClaim;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              filter.claim# = YAJL_get_string(val);
           When key = 'status';
              filter.claimStatus  = YAJL_get_string(val);
           When key = 'accidentDate';
              json = %scanrpl('""':'{':json:%len(json)-2);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'from';
                    filter.fromAccDate = YAJL_get_string(val);
                 When key = 'to';
                    filter.toAccDate  = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Claimant
       //=================================================================
       Begsr GetParmsClaimant;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              json = %scanrpl('""':'{':json:%len(json)-2);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'first';
                    filter.claimantFSN = YAJL_get_string(val);
                 When key = 'last';
                    filter.claimantLSN = YAJL_get_string(val);
                 When key = 'middleInitial';
                    filter.claimantMid = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json += '}';

       Endsr;

       //=================================================================
       // Break parms out from inParms - Group
       //=================================================================
       Begsr GetParmsGroup;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              filter.groupName = YAJL_get_string(val);
           When key = 'number';
              filter.group# = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Insured
       //=================================================================
       Begsr GetParmsInsured;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              filter.insuredName = YAJL_get_string(val);
           When key = 'fein';
              filter.insuredFein = YAJL_get_string(val);
           When key = 'number';
              filter.emp# = YAJL_get_string(val);
           When key = 'division';
              json = %scanrpl('""':'[':json:%len(json)-2);
              node2 = YAJL_object_find(node1:key);
              Reset i2;
              Dow YAJL_array_loop(node2:i2:val);
                 filter.division(i2) = YAJL_get_string(val);
                 json += '"' + %trim(filter.division(i2)) + '",';
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - Policy
       //=================================================================
       Begsr GetParmsPolicy;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              filter.policy# = YAJL_get_string(val);
           When key = 'status';
              filter.policyStatus  = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Validate parms
       //=================================================================
       Begsr Validate;

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
                AddErrorMessageJson('Group#: Invalid numeric data.':ioRtnCode);
                badData = *On;
             EndMon;
             // If the group is valid numeric data then see if it exists
             If badData = *Off And ValidGrp(group#) = *Off;
                AddErrorMessageJson('Invalid group#.':ioRtnCode);
             EndIf;
          EndIf;
          If group# <> 0 and not UseGroupInPortal(intCo#:group#:'Claim');
             AddErrorMessageJson('Group ' + %char(group#) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          Endif;

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
             AddErrorMessageJson('Invalid agency Fein/Mod.':ioRtnCode);
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
          // Employer must be valid numeric value <<<<<<<<<<<<
          //------------------------------------------------------
          badData = *Off;
          emp#    = 0;
          If filter.emp# <> ' ';
             Monitor;
                emp# = %Dec(%Trim(filter.emp#):9:0);
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
          division = 0;
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

          //-----------------------------------------------------------
          // Total incurred must be valid numeric value
          //-----------------------------------------------------------
          If filter.totalIncurred <> ' ';
             Monitor;
                totalIncIn = %Dec(%Trim(filter.totalIncurred):11:2);
             On-Error;
                AddErrorMessageJson('Total Incurred: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

          //-------------------------------------------------------------
          // Policy # must be extracted
          //-------------------------------------------------------------
          If filter.policy# <> ' ';
             Monitor;
              //polGroup# = %Dec(%Subst(filter.policy#:1:3):3:0);
              //polEmp#   = %Dec(%Subst(filter.policy#:4:9):9:0);
              //polYear   = %Dec(%Subst(filter.policy#:13:3):3:0);
             On-Error;
                AddErrorMessageJson('Policy #: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
             If polGroup# <> 0 and
              not UseGroupInPortal(intCo#:polGroup#:'Claim');
                AddErrorMessageJson('Policy group ' + %char(polGroup#) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtnCode);
             Endif;
          EndIf;

          //-------------------------------------------------------------
          // Claim # must be valid numeric value
          //-------------------------------------------------------------
          If filter.claim# <> ' ';
             Monitor;
                case# = %Dec(%Trim(filter.claim#):7:0);
             On-Error;
                AddErrorMessageJson('Claim #: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

          //------------------------------------------------------
          // From Accident date must be valid <<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.fromAccDate = ' ' Or filter.fromAccDate = '00000000';
             fromAccDt8     = 0;
             fromAccDt7     = 0;
             strFromAccDt7  = '0000000';
          Else;
             Monitor;
                fromAccDt8 = %Dec(filter.fromAccDate:8:0);
             On-Error;
                AddErrorMessageJson('From Accident Date: Invalid numeric data.'
                 :ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And fromAccDt8 <> 0;
                fromAccDt7     = dtCYMD(fromAccDt8);
                strfromAccDt7  = %EditC(fromAccDt7:'X');
                If fromAccDt7 = 0;
                   AddErrorMessageJson('From Accident Date: Invalid date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          //------------------------------------------------------
          // To Accident date must be valid <<<<<<<<<<<<<<<<<<<<<
          //------------------------------------------------------
          dateError = *Off;
          If filter.toAccDate = ' ' Or filter.toAccDate = '00000000';
             toAccDt8     = 0;
             toAccDt7     = 0;
             strToAccDt7  = '0000000';
          Else;
             Monitor;
                toAccDt8 = %Dec(filter.toAccDate:8:0);
             On-Error;
                AddErrorMessageJson('To Accident Date: Invalid numeric data.'
                 :ioRtnCode);
                dateError= *On;
             EndMon;

             // Is the date valid?
             If Not dateError And toAccDt8 <> 0;
                toAccDt7     = dtCYMD(toAccDt8);
                strToAccDt7  = %EditC(toAccDt7:'X');
                If toAccDt7 = 0;
                   AddErrorMessageJson('To Accident Date: Invalid date.'
                    :ioRtnCode);
                EndIf;
             EndIf;
          EndIf;

          //-------------------------------------------------------------
          // Recent Days must be valid numeric value
          //-------------------------------------------------------------
          If filter.recentDays <> *blanks;
             Monitor;
                recentDays = %dec(%trim(filter.recentDays):10:0);
                strRecentDt7 =
                 %editc(DateTo7(SystemDate() - %days(recentDays)):'X');
             On-Error;
                AddErrorMessageJson('Recent Days: Invalid numeric data.'
                 :ioRtnCode);
             EndMon;
          EndIf;

       Endsr;

      /End-free

     pClaimSearch      e

       //===================================================================
       // Claim Summary
       //===================================================================
     pGetClaimSummary  b
     dGetClaimSummary  pi
     d*** Begin Add *** 07/07/21 *******************************************
     d inParmsGCS                   100a   Const Options(*nopass)
     d*** End   Add *** 07/07/21 *******************************************

     ddsIn             ds                  qualified inz
     d co#                            3
     d fnd                            3
     d fyr                            3
     d case                           7

     d*** Begin Add *** 07/07/21 *******************************************
     dchrDept          s             10    inz
     dchrDeptDesc      s             40    inz
     d*** End   Add *** 07/07/21 *******************************************
     di1               s             10i 0 inz
     dintFnd           s              3  0 inz
     d*** Begin Add *** 07/07/21 *******************************************
     dintFyr           s              3  0 inz
     dintCase          s              7  0 inz
     dintDiv           s              5  0 inz
     dintEmp#          s              9  0 inz
     d*** End   Add *** 07/07/21 *******************************************
     disEofWMCMP       s              1n
     dstrSql           s          10000    varying inz
     dstrSqlWhere      s           5000    varying inz
     dstrString        s            500    varying inz
     d*** Begin Add *** 07/07/21 *******************************************
     ddartsKey         s             40a
     dincPayments      s              5a
     dincDarts         s              5a
     dsendPayments     s               n   Inz(*On)
     dsendDarts        s               n   Inz(*On)
     d*** End   Add *** 07/07/21 *******************************************
      /Free
       Exsr GetParms;
       Exsr Validate;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;

       //=================================================================
       // Read output json
       //=================================================================
       Begsr BuildJson;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        LoadFinancialArrays();
        Reset dsAdjuster;
        Exec SQL
         select * into :dsAdjuster
          from WMUPP
          where UPstaf = :dsWMCMP.CMclEx
          limit 1;
        //*** Begin Add *** 07/07/21 **************************************
        Reset dsSupervisor;
        Exec SQL
         select * into :dsSupervisor
          from WMUPP
          where UPstaf = :dsWMCMP.CMsupr
          limit 1;
        //*** End   Add *** 07/07/21 **************************************
        Reset dsWDELP;
        Exec SQL
         select * into :dsWDELP
          from WDELP
          where ELco# = :dsWMCMP.CMco# and ELfnd = :dsWMCMP.CMfnd
           and ELemp# = :dsWMCMP.CMemp# and ELdiv <= :dsWMCMP.CMdiv
           and ELfyr = :dsWMCMP.CMfyr
          limit 1;
        Reset dsWMLLP;
        Exec SQL
         select * into :dsWMLLP
          from WMLLP
          where LLco# = :dsWMCMP.CMco# and LLfnd = :dsWMCMP.CMfnd
           and LLfyr = :dsWMCMP.CMfyr and LLcase <= :dsWMCMP.CMcase
          limit 1;
        json += ',' +
         ClaimJSONData(dsWMCMP.CMco#:dsWMCMP.CMfnd:dsWMCMP.CMfyr:dsWMCMP.CMcase)
         + ',';
        // This will allow for extended claim info
        json = %scanrpl('},':',':json:%len(json)-2);
        json += '"class":{' +
         BuildJsonNode('code':%char(dsWMCMP.CMclas)) + ',' +
         BuildJsonNode('description':
          GetPayrollDesc(dsWMCMP.CMco#:dsWMCMP.CMacDt:dsWMCMP.CMjrSt
           :dsWDELP.ELrtCd:dsWMCMP.CMclas:dsWMCMP.CMext)) +
          '},';
        //*** Begin Add *** 07/07/21 ***************************************
        // Department
        intEmp# = dsWMCMP.CMemp#;
        intDiv  = dsWMCMP.CMdiv;
        chrDept = dsWMCMP.CMdept;
        Clear chrDeptDesc;
        Exec Sql Select DPdesc Into :chrDeptDesc
                 From wmdpp
                 Where DPco#=:intCo# And DPfnd=:intFnd And DPemp#=:intEmp#
                   And DPdiv=:intDiv And DPdept=:chrDept;
        json += '"department":{' +
         BuildJsonNode('code':%char(dsWMCMP.CMdept)) + ',' +
         BuildJsonNode('description':chrDeptDesc) +
          '},';
        //*** End   Add *** 07/07/21 ***************************************
        Exsr BuildJsonAccidentInfo;
        json += '"dateReported":{' +
         BuildJsonNode('admin':%char(dsWMCMP.CMrcvd):'date') + ',' +
         BuildJsonNode('employer':%char(dsWMCMP.CMdtRp):'date') + '},';
        //*** Begin Add *** 07/07/21 ***************************************
        // Occupation Description, Average Weekly Wage, Lump Sum Settlement
        json +=
         BuildJsonNode('occupationDescription':dsWMCMP.CMocds) + ',' +
         BuildJsonNode('aww':%EditC(dsWMCMP.CMaww:'3')) +  ',' +
         BuildJsonNode('lumpSum':dsWMLLP.LLlump) + ',';

        // Method of settlement
        json += '"methodOfSettlement":{' +
         BuildJsonNode('code':dsWMLLP.LLmtSt) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'MS':dsWMLLP.LLmtSt:dsWMCMP.CMjrSt)) +
          '},';
        //*** End   Add *** 07/07/21 ***************************************
        json += '"adjustor":{' +
         BuildJsonNode('code':dsWMCMP.CMclEx) + ',' +
         BuildJsonNode('description':dsAdjuster.UPusNm) + ',' +
         '"phone":[{' +
          BuildJsonPhone(%editc(dsAdjuster.UPphnA:'X')
           :%editc(dsAdjuster.UPphn#:'X'):*blank:'phone1') +
          '}],' +
         BuildJsonNode('email':dsAdjuster.UPmail) +
         '},';
        //*** Begin Add *** 07/07/21 ***************************************
        // Supervisor Information
        json += '"supervisor":{' +
         BuildJsonNode('code':dsWMCMP.CMsupr) + ',' +
         BuildJsonNode('description':dsSupervisor.UPusNm) + ',' +
         '"phone":[{' +
          BuildJsonPhone(%editc(dsSupervisor.UPphnA:'X')
           :%editc(dsSupervisor.UPphn#:'X'):*blank:'phone1') +
          '}],' +
         BuildJsonNode('email':dsSupervisor.UPmail) +
         '},';
        // Location and Impairment %
        json +=
         BuildJsonNode('location':dsWMCMP.CMctg1) + ',' +
         BuildJsonNode('impairmentPercent':%EditC(dsWMCMP.CMiPct:'3')) + ',';
        //*** End   Add *** 07/07/21 ***************************************
        json += '"lossType":{' +
         BuildJsonNode('code':dsWMCMP.CMlsCv) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'LC':dsWMCMP.CMlsCv:dsWMCMP.CMjrSt))+'},';
        json += '"injuryType":{' +
         BuildJsonNode('code':dsWMCMP.CMncci) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'NC':dsWMCMP.CMncci:dsWMCMP.CMjrSt))+'},';
        json += '"settlementType":{' +
         BuildJsonNode('code':dsWMLLP.LLmtSt) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'MS':dsWMLLP.LLmtSt:dsWMCMP.CMjrSt))+'},';
        json += '"recoveryType":{' +
         BuildJsonNode('code':dsWMCMP.CMlsRv) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'LSRV':dsWMCMP.CMlsRv))+'},';
        json += '"coverageType":{' +
         BuildJsonNode('code':dsWMCMP.CMcovt) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'CT':dsWMCMP.CMcovT:dsWMCMP.CMjrSt))+'},';
        //*** Begin Add *** 07/07/21 ***************************************
        // Loss Coverage Code
        json += '"lossCoverage":{' +
         BuildJsonNode('code':dsWMCMP.CMlsCv) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'LSCV':dsWMCMP.CMlsCv)) +
          '},';
        // Fraud Indicator Code
        json += '"fraud":{' +
         BuildJsonNode('code':dsWMLLP.LLfrau) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMLLP.LLco#:'FRAU':dsWMLLP.LLfrau)) +
          '},';
        //*** End   Add *** 07/07/21 ***************************************
        json += '"nature":{' +
         BuildJsonNode('code':dsWMCMP.CMnatr) + ',' +
         BuildJsonNode('description':NatureCdDs(dsWMCMP.CMco#:dsWMCMP.CMnatr))
          + '},';
        json += '"part":{' +
         BuildJsonNode('code':dsWMCMP.CMpart) + ',' +
         BuildJsonNode('description':PartCdDs(dsWMCMP.CMco#:dsWMCMP.CMpart))
          + '},';
        json += '"cause":{' +
         BuildJsonNode('code':dsWMCMP.CMcaus) + ',' +
         BuildJsonNode('description':CauseCdDs(dsWMCMP.CMco#:dsWMCMP.CMcaus))
          + '},';
        Exsr BuildJsonClaimFlags;
        Exsr BuildJsonLostTimeStatus;
        //*** Begin Add *** 07/07/21 ***************************************
        // MCO Type
        json += '"mcoType":{' +
         BuildJsonNode('code':dsWMCMP.CMmcTp) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(dsWMCMP.CMco#:'MCO ':dsWMCMP.CMmcTp)) +
          '},';
        //*** End   Add *** 07/07/21 ***************************************
        Exsr BuildJsonPolicyInfo;
        Exsr BuildJsonFinancials;
        json = %scanrpl(',':'}':json:%len(json)-1);
        //*** Begin Add *** 07/07/21 ***************************************
        ExSr BuildJsonDarts;
        ExSr BuildJsonPayments;
        //*** End   Add *** 07/07/21 ***************************************
       Endsr;

       //=================================================================
       // Read output json - Accident Info
       //=================================================================
       Begsr BuildJsonAccidentInfo;
        json += '"accidentInfo":{' +
         BuildJsonNode('description':dsWMCMP.CMacDs) + ',' +
         BuildJsonNode('place':dsWMCMP.CMacNm) + ',' +
         '"county":{' +
          BuildJsonNode('code':dsWMCMP.CMacCy) + ',' +
          BuildJsonNode('description'
           :CountyCdDs(dsWMCMP.CMco#:dsWMCMP.CMjrSt+dsWMCMP.CMacCy)) +
          '},' +
         BuildJsonAddress(dsWMCMP.CMacDr:*blank:dsWMCMP.CMaCty:dsWMCMP.CMaSt
          :%editc(dsWMCMP.CMacZ1:'X')+%editc(dsWMCMP.CMacZ2:'X'):'address') +
         '},';
       Endsr;

       //=================================================================
       // Read output json - Claim Flags
       //=================================================================
       Begsr BuildJsonClaimFlags;
        json += '"claimFlags":{';
        //*** Begin Add *** 07/07/21 ***************************************
        // Litigation
        strString = 'false';
        If dsWMLLP.LLlitg = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('litigation':strString:'boolean') + ',';
        // Vocational Rehab
        strString = 'false';
        If dsWMCMP.CMvorc = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('vocationalRehab':strString:'boolean') + ',';
        //*** End   Add *** 07/07/21 ***************************************
        strString = 'false';
        If dsWMCMP.CMdny = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('denied':strString:'boolean') + ',';
        strString = 'false';
        If dsWMCMP.CMcvFl = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('controverted':strString:'boolean') + ',';
        strString = 'false';
        If dsWMCMP.CMsubr = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('subrogated':strString:'boolean') + ',';
        strString = 'false';
        If dsWMCMP.CMriRp = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('reinsuranceReportable':strString:'boolean')+',';
        strString = 'false';
        If dsWMCMP.CMUSLH = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('uslh':strString:'boolean')+',';
        strString = 'false';
        If dsWMCMP.CMseaE = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('seasonal':strString:'boolean')+',';
        strString = 'false';
        If dsWMCMP.CMsFFF = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('specialDisabilityFund':strString:'boolean')+',';
        strString = 'false';
        If dsWMCMP.CMliaF = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('employerLiability':strString:'boolean')+',';
        strString = 'false';
        If dsWMCMP.CMmBS = 'Y';
           strString = 'true';
        Endif;
        json += BuildJsonNode('medicareBenificiary':strString:'boolean')+',';
        json = %scanrpl(',':'},':json:%len(json)-1);
       Endsr;

       //=================================================================
       // Read output json - Claim Status
       //=================================================================
       Begsr BuildJsonClaimStatus;
        Select;
        When dsWMCMP.CMstat = 'O';
           strString = 'Open';
        When dsWMCMP.CMstat = 'C';
           strString = 'Closed';
        Other;
           strString = 'Undefined';
        Endsl;
        json += '"status":{' +
         BuildJsonNode('code':dsWMCMP.CMstat) + ',' +
         BuildJsonNode('description':strString) + '},';
       Endsr;

       //*** Begin Add *** 07/07/21 ***************************************
       //=================================================================
       // Read output json - Darts
       //=================================================================
       Begsr BuildJsonDarts;

          // Only include Darts information if called to do so
          If sendDarts;
             inRequestD = 'ListRemarks';
             inUserD    = inUser;
             outJsonD   = ' ';
             ioRtnCodeD = ' ';
             dartsKey = %EditC(intCo#:'X') + %EditC(intFnd:'X') +
                        %EditC(intFyr:'X') + %EditC(intCase:'X');
             inParmsD = '{"keyType":"WC","key":"' + %Trim(dartsKey) +
                        '","includeIncomingParms":"false"}';
             DartsListing(inRequestD:inUserD:inParmsD:outJSOND:ioRtncodeD);
             json += ',' + %trim(outJsonD);
          EndIf;

       Endsr;
       //*** End   Add *** 07/07/21 ***************************************

       //=================================================================
       // Read output json - Financials
       //=================================================================
       Begsr BuildJsonFinancials;
        json += '"financials":[';
        For i1 = 1 to %elem(dsFinancials);
           json += '{';
           json += '"reserve":{' +
            BuildJsonNode('code':dsFinancials(i1).code) + ',' +
            BuildJsonNode('type':dsFinancials(i1).type) +  ',' +
            BuildJsonNode('description':dsFinancials(i1).description) +
            '},';
           json += '"inceptionToDate":{' +
            BuildJsonNode('amount'
             :%trim(dsFinancials(i1).inception.strAmount)) + ',' +
            BuildJsonNode('paid'
             :%trim(dsFinancials(i1).inception.strPaid)) + ',' +
            BuildJsonNode('incurred'
             :%trim(dsFinancials(i1).inception.strIncurred)) +
            '},';
           json += '"monthToDate":{' +
            BuildJsonNode('amount'
             :%trim(dsFinancials(i1).month.strAmount)) + ',' +
            BuildJsonNode('paid'
             :%trim(dsFinancials(i1).month.strPaid)) + ',' +
            BuildJsonNode('incurred'
             :%trim(dsFinancials(i1).month.strIncurred)) +
            '},';
           json = %scanrpl(',':'':json:%len(json)-1);
           json += '},';
        Endfor;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '],';
       Endsr;

       //=================================================================
       // Read output json - Lost Time Status
       //=================================================================
       Begsr BuildJsonLostTimeStatus;
        Select;
        When dsWMCMP.CMltFl = 'M';
           strString = 'Medical Only';
        When dsWMCMP.CMltFl = 'I';
           strString = 'Indemnity';
        When dsWMCMP.CMltFl = 'R';
           strString = 'Record Only';
        Other;
           strString = 'Undefined';
        Endsl;
        json += '"LostTimeStatus":{' +
         BuildJsonNode('code':dsWMCMP.CMltFl) + ',' +
         BuildJsonNode('description':strString) + '},';
       Endsr;

       //*** Begin Add *** 07/07/21 ***************************************
       //=================================================================
       // Read output json - Payments
       //=================================================================
       Begsr BuildJsonPayments;

          // Only include payment information if called to do so
          If sendPayments;
             ListClaimPayments('{"includeIncomingParms":"false"}');
             json = %Trim(json);
          EndIf;

       Endsr;
       //*** End   Add *** 07/07/21 ***************************************

       //=================================================================
       // Read output json - Policy Information
       //=================================================================
       Begsr BuildJsonPolicyInfo;
        json += PolicyJSONData(dsWMCMP.CMco#:dsWMCMP.CMfnd:dsWMCMP.CMemp#
         :dsWMCMP.CMdiv:dsWMCMP.CMfyr) + ',';
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        Reset dsWMCMP;
        Exec SQL fetch next from cursorWMCMP into :dsWMCMP;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('Requested claim not found.':ioRtnCode);
        Endif;
        Exsr BuildJson;
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetClaimSummary}');
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSql;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exsr BuildSqlWhere;
        strSql += 'select * from WMCMP ' + strSqlWhere;
        Exec Sql Close cursorWMCMP;
        Exec Sql Prepare sqlWMCMP From :strSql;
        Exec Sql Declare cursorWMCMP Cursor For sqlWMCMP;
        Exec Sql Open cursorWMCMP;
       Endsr;

       //=================================================================
       // Build SQL where statement
       //=================================================================
       Begsr BuildSqlWhere;
        strSqlWhere += ' where CMco# = ' + %char(intCo#) +
         ' and CMfnd = ' + dsIn.fnd + ' and CMfyr = ' + dsIn.fyr +
         ' and CMcase = ' + dsIn.case;
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson : errMsg500);
        If errMsg500 <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        json += ' "requestParms":{';
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.fnd = YAJL_get_string(val);
           When key = 'year';
              dsIn.fyr = YAJL_get_string(val);
           When key = 'claim';
              dsIn.case = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
        // Free up the space by removing the tree
        yajl_tree_free(node0);
        //*** Begin Add *** 07/07/21 ***************************************
        //* Extract incoming parameters into this procedure
        If %parms() > 0;
           node0 = yajl_string_load_tree(inParmsGCS : errMsg500);
           Reset i0;
           Dow YAJL_object_loop(node0:i0:key:val);
              Select;
              When key = 'includePayments';
                 incPayments = YAJL_get_string(val);
                 sendPayments = *Off;
                 If incPayments = 'True' Or incPayments = 'true';
                    sendPayments = *On;
                 EndIf;
              When key = 'includeDARTS';
                 incDarts = YAJL_get_string(val);
                 sendDarts = *Off;
                 If incDarts = 'True' Or incDarts = 'true';
                    sendDarts = *On;
                 EndIf;
              Endsl;
           Enddo;
           yajl_tree_free(node0);
        EndIf;
        //*** End   Add *** 07/07/21 ***************************************
       Endsr;

       //=================================================================
       // Validate request
       //=================================================================
       Begsr Validate;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Monitor;
           intCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        Monitor;
           intFnd = %int(dsIn.fnd);
        On-error;
        Endmon;
        //*** Begin Add *** 07/07/21 ***************************************
        Monitor;
           intFyr = %int(%Trim(dsIn.fyr));
        On-error;
        Endmon;
        Monitor;
           intCase = %int(%Trim(dsIn.case));
        On-error;
        Endmon;
        //*** End   Add *** 07/07/21 ***************************************
        If not UseGroupInPortal(intCo#:intFnd:'Claim');
           AddErrorMessageJson('Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
       Endsr;
      /End-free
     pGetClaimSummary  e

       //===================================================================
       // GetPaid - Get paid  by reserve type.
       //===================================================================
     PGetPaid          b
     DGetPaid          pi            11s 2
     D inType                         1a   const
     D inPeriod                      10a   const varying options(*nopass)

     D i2              s              5  0
     D isEofPaid...
     D                 s              1n   inz
     D paidAmt         s             11s 2 inz(0)
     D strKey          s             10a
     D strPeriod       s             10a   varying inz('*ITD')
     D* *ITD - Inception to Date
     D* *MTD - Month to Date
     D strSql          s            500a   varying inz
     D strType         s              1a

     DdsWDPAP        e ds                  extname(WDPAP) qualified inz
      /Free
       If %parms >= 2 and inPeriod <> *blanks;
          strPeriod = inPeriod;
       Endif;
       Select;
       When strPeriod = '*ITD';
          For i2=1 to #ofFin;
             strKey = aryRT(i2);
             Exec sql
              select RTtype into :strType
               from WTRTP
               where RTco# = :dsWMCMP.CMco# and RTkey = :strKey
              limit 1;
             If IsValidFetch(sqlstt) and strType = inType;
                paidAmt += aryAR(i2);
             EndIf;
          EndFor;
       When strPeriod = '*MTD';
          strSql = 'select * from WDPAP ' +
           'where PAco# = ' + %char(dsWMCMP.CMco#) +
           ' and PAfnd = ' + %char(dsWMCMP.CMfnd) +
           ' and PAfyr = ' + %char(dsWMCMP.CMfyr) +
           ' and PAcase = ' + %char(dsWMCMP.CMcase) +
           ' and PAreTp = ''' + inType + '''' +
           ' and (PAtrDt between ' +
            %char(DateTo7(GetFirstDay(%date))) + ' and ' +
            %char(DateTo7(%date)) + ')';
          Exsr BuildCursor;
          Dou isEofPaid;
             Exec sql fetch next from cursorPaid into :dsWDPAP;
             Select;
             When not IsValidFetch(sqlstt);
                isEofPaid = *on;
                Iter;
             Endsl;
             Select;
             When dsWDPAP.PAtype = 'S'
              or dsWDPAP.PAtype = 'V'
              or dsWDPAP.PAtype = 'R' ;
                paidAmt -= dsWDPAP.PAamt;
             Other;
                paidAmt += dsWDPAP.PAamt;
             Endsl;
          Enddo;
       Endsl;
       Return paidAmt;
       //-------------
       Begsr BuildCursor;
        Exec sql close cursorPaid;
        Exec sql prepare sqlPaid From :strSql;
        Exec sql declare cursorPaid cursor for sqlPaid;
        Exec sql open cursorPaid;
       Endsr;
      /End-Free
     PGetPaid          e

       //===================================================================
       // GetPayRequests -Get payment requests by reserve type.
       //===================================================================
     PGetPayRequests   b
     DGetPayRequests   pi            11s 2
     D inType                         1a   const
     D inPeriod                      10a   const varying options(*nopass)

     D isEofPayRequests...
     D                 s              1n   inz
     D payReqAmt       s             11s 2 inz(0)
     D strDateRange    s             50a   varying inz
     D strPeriod       s             10a   varying inz('*ITD')
     D* *ITD - Inception to Date
     D* *MTD - Month to Date
     D strSql          s            500a   varying inz

     DdsWARPP        e ds                  extname(WARPP) qualified inz

      /Free
       If %parms >= 2 and inPeriod <> *blanks;
          strPeriod = inPeriod;
       Endif;
       strSql = 'select * from WARPP ' +
        'where RPco# = ' + %char(dsWMCMP.CMco#) +
        ' and RPfnd = ' + %char(dsWMCMP.CMfnd) +
        ' and RPfyr = ' + %char(dsWMCMP.CMfyr) +
        ' and RPcase = ' + %char(dsWMCMP.CMcase);
       Select;
       When strPeriod = '*ITD';
       When strPeriod = '*MTD';
          strSql += ' and (RPtrDt between ' +
           %char(DateTo7(GetFirstDay(%date))) + ' and ' +
           %char(DateTo7(%date)) + ')';
       Endsl;
       Exsr BuildCursor;
       Dou isEofPayRequests;
          Exec sql fetch next from cursorPayRequests into :dsWARPP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofPayRequests = *on;
             Iter;
          When dsWARPP.RPreTp = inType;
             If (dsWMCMP.CMstat = 'C' and dsWARPP.RPsts <> 'PY')
              or dsWMCMP.CMstat = 'O';
                payReqAmt += dsWARPP.RPamt;
             EndIf;
          Endsl;
       EndDo;
       Return payReqAmt;
       //-------------
       Begsr BuildCursor;
        Exec sql close cursorPayRequests;
        Exec sql prepare sqlPayRequests From :strSql;
        Exec sql declare cursorPayRequests cursor for sqlPayRequests;
        Exec sql open cursorPayRequests;
       Endsr;
      /End-Free
     PGetPayRequests   e

       //===================================================================
       // GetRecoveries - Get the recoveries by reserve type.
       //===================================================================
     PGetRecoveries    b
     DGetRecoveries    pi            11s 2
     D inType                         1a   const
     D inPeriod                      10a   const varying options(*nopass)

     D isEofRecoveries...
     D                 s              1n   inz
     D recoveryAmt     s             11s 2 inz(0)
     D strPeriod       s             10a   varying inz('*ITD')
     D* *ITD - Inception to Date
     D* *MTD - Month to Date
     D strRcvR         s              1a
     D strSql          s            500a   varying inz

     DdsWDPAP        e ds                  extname(WDPAP) qualified inz
      /Free
       If %parms >= 2 and inPeriod <> *blanks;
          strPeriod = inPeriod;
       Endif;
       strSql = 'select * from WDPAP ' +
        'where PAco# = ' + %char(dsWMCMP.CMco#) +
        ' and PAfnd = ' + %char(dsWMCMP.CMfnd) +
        ' and PAfyr = ' + %char(dsWMCMP.CMfyr) +
        ' and PAcase = ' + %char(dsWMCMP.CMcase);
       Select;
       When strPeriod = '*ITD';
       When strPeriod = '*MTD';
          strSql += ' and (PAtrDt between ' +
           %char(DateTo7(GetFirstDay(%date))) + ' and ' +
           %char(DateTo7(%date)) + ')';
       Endsl;
       Exsr BuildCursor;
       Dou isEofRecoveries;
          Exec sql fetch next from cursorRecoveries into :dsWDPAP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofRecoveries = *on;
             Iter;
          When dsWDPAP.PAtype = 'R' and dsWDPAP.PAreTp = inType;
             Exec sql
              select B2rcvR into :strRcvR
               from WTB2P
               where B2co# = :dsWMCMP.CMco# and B2key = :dsWDPAP.PAbr2
               limit 1;
             If not IsValidFetch(sqlstt) and strRcvR='Y';
                recoveryAmt += dsWDPAP.PAamt;
             EndIf;
          Endsl;
       EndDo;
       Return recoveryAmt;
       //-------------
       Begsr BuildCursor;
        Exec sql close cursorRecoveries;
        Exec sql prepare sqlRecoveries From :strSql;
        Exec sql declare cursorRecoveries cursor for sqlRecoveries;
        Exec sql open cursorRecoveries;
       Endsr;
      /End-Free
     PGetRecoveries    e

       //===================================================================
       // GetReserveAmount - Get the remaining reserves by reserve type.
       //===================================================================
     PGetReserveAmount...
     P                 b
     DGetReserveAmount...
     D                 pi            11s 2
     D inType                         1a   const
     D inPeriod                      10a   const varying options(*nopass)

     d aryAmount       s             13  2 dim(#ofFin) inz
     d aryType         s              2    dim(#ofFin) inz
     D i2              s              5  0
     D isEofReserves...
     D                 s              1n   inz
     D reserveAmt      s             11s 2 inz(0)
     D strKey          s             10a
     D strPeriod       s             10a   varying inz('*ITD')
     D* *ITD - Inception to Date
     D* *MTD - Month to Date
     D strSql          s            500a   varying inz
     D strType         s              1a

     DdsWMRHP        e ds                  extname(WMRHP) qualified inz
      /Free
       If %parms >= 2 and inPeriod <> *blanks;
          strPeriod = inPeriod;
       Endif;
       Select;
       When strPeriod = '*ITD';
          aryType = aryRT;
          aryAmount = aryRR;
          Exsr AccumulateAmounts;
       When strPeriod = '*MTD';
          strSql = 'select * from WMRHP ' +
           'where RHco# = ' + %char(dsWMCMP.CMco#) +
           ' and RHfnd = ' + %char(dsWMCMP.CMfnd) +
           ' and RHfyr = ' + %char(dsWMCMP.CMfyr) +
           ' and RHcase = ' + %char(dsWMCMP.CMcase) +
           ' and (RHefDt between ' +
            %char(DateTo7(GetFirstDay(%date))) + ' and ' +
            %char(DateTo7(%date)) + ')';
          Exsr BuildCursor;
          Dou isEofReserves;
             Exec sql fetch next from cursorReserves into :dsWMRHP;
             Select;
             When not IsValidFetch(sqlstt);
                isEofReserves = *on;
                Iter;
             Endsl;
             aryType( 1) = dsWMRHP.RHRT01;
             aryType( 2) = dsWMRHP.RHRT02;
             aryType( 3) = dsWMRHP.RHRT03;
             aryType( 4) = dsWMRHP.RHRT04;
             aryType( 5) = dsWMRHP.RHRT05;
             aryType( 6) = dsWMRHP.RHRT06;
             aryType( 7) = dsWMRHP.RHRT07;
             aryType( 8) = dsWMRHP.RHRT08;
             aryType( 9) = dsWMRHP.RHRT09;
             aryType(10) = dsWMRHP.RHRT10;
             aryAmount( 1) = dsWMRHP.RHSU01;
             aryAmount( 2) = dsWMRHP.RHSU02;
             aryAmount( 3) = dsWMRHP.RHSU03;
             aryAmount( 4) = dsWMRHP.RHSU04;
             aryAmount( 5) = dsWMRHP.RHSU05;
             aryAmount( 6) = dsWMRHP.RHSU06;
             aryAmount( 7) = dsWMRHP.RHSU07;
             aryAmount( 8) = dsWMRHP.RHSU08;
             aryAmount( 9) = dsWMRHP.RHSU09;
             aryAmount(10) = dsWMRHP.RHSU10;
             Exsr AccumulateAmounts;
          Enddo;
       Endsl;
       Return reserveAmt;
       //-------------
       Begsr AccumulateAmounts;
        For i2=1 to #ofFin;
           strKey = aryType(i2);
           Exec sql
            select RTtype into :strType
             from WTRTP
             where RTco# = :dsWMCMP.CMco# and RTkey = :strKey
             limit 1;
           If IsValidFetch(sqlstt) and strType = inType;
              reserveAmt += aryAmount(i2);
           EndIf;
        EndFor;
       Endsr;
       //-------------
       Begsr BuildCursor;
        Exec sql close cursorReserves;
        Exec sql prepare sqlReserves From :strSql;
        Exec sql declare cursorReserves cursor for sqlReserves;
        Exec sql open cursorReserves;
       Endsr;
      /End-Free
     PGetReserveAmount...
     P                 e
     P*** Begin Add *** 07/07/21 *******************************************
       //===================================================================
       // ImportClaim - Import claim process (delete/add and change)
       //===================================================================
     PImportClaim      b
     DImportClaim      pi
     d inMode                         1a   Const

     d chrCo#          s              3
     d chrComment      s         100000
     d chrDate8        s              8
     d chrFein         s              9
     d chrMod          s              3
     d chrOldSS#       s              9
     d chrRmkCode      s              5
     d chrRsvCode      s              2
     d clmCount        s              5  0
     d createFile      s            200
     d clearFile       s            200
     d copyFile        s           1000
     d dateError       s               n
     d decAmount       s              9  2
     d decFein         s              9  0
     d decMod          s              3  0
     d decOldAcDt7     s              3  0
     d decOldAcDt8     s              3  0
     d decOldDiv       s              5  0
     d decOldEmp#      s              9  0
     d decOldFnd       s              3  0
     d decPct50        s              5  0
     d decPct52        s              5  2
     d diaryFlag       s              1
     d empCount        s              5  0
     d i               s              5  0
     d importRecord    s            800
     d intAcDt         s              7  0
     d intAcDt8        s              8  0
     d intCase         s              7  0
     d intCo#          s              3  0
     d intDiv          s              5  0
     d intFnd          s              3  0
     d intEmp#         s              9  0
     d intFyr          s              3  0
     d isTotal         s               n
     d j               s              5  0
     d totalInc01      s              9  2
     d totalInc02      s              9  2
     d totalInc03      s              9  2
     d totalInc04      s              9  2
     d totalInc05      s              9  2
     d totalInc06      s              9  2
     d totalInc07      s              9  2
     d totalInc08      s              9  2
     d totalInc09      s              9  2
     d totalInc10      s              9  2

     d rsvCodeDs       ds
     d rate1                          2
     d rate2                          2
     d rate3                          2
     d rate4                          2
     d rate5                          2
     d rate6                          2
     d rate7                          2
     d rate8                          2
     d rate9                          2
     d rate10                         2
     D rsvCode                        2    Overlay(rsvCodeDs) Dim(10)

     d dsReserves      ds                  qualified Dim(10) Inz                Record 03
     d   code                         2
     d   remaining                    9  2
     d   paid                         9  2
     d   incurred                     9  2

      /free

       ExSr GetParms;
       If ioRtnCode <> 'Error';
          ExSr CallBackFill;
       EndIf;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;

       //===================================================================
       // Call the Claim Backfill Process
       //===================================================================
       BegSr CallBackFill;

          // Copy CLIMPORT work file to QTEMP
          ExSr CreateWorkFile;

          // Load remaining backfill data
          ExSr LoadRemainingData;

          // Populate the backfill flatfile
          ExSr LoadFlatFile;

          // Copy file to winner so can see.
          ExSr CopyWorkFile;

          // Call Backfill Process
          ClaimBackfill(inQueue:inOption:inCopy);

       EndSr;

       //===================================================================
       // Create Backfill Work file in QTEMP.
       //===================================================================
       BegSr CreateWorkFile;

          createFile = 'SYDUPFILE FROM(CLIMPORT) TO(QTEMP/*SAME) ' +
                       'DATA(*NO)';

          Monitor;
             RunCmd(%Trim(createFile):%Len(%Trim(createFile)));
          On-Error;
          EndMon;

          clearFile = 'CLRPFM FILE(QTEMP/CLIMPORT)';
          Monitor;
             RunCmd(%Trim(clearFile):%Len(%Trim(clearFile)));
          On-Error;
          EndMon;

       EndSr;

       //===================================================================
       // Copy CLIMPORT to WINNER for testing
       //===================================================================
       BegSr CopyWorkFile;

          createFile = 'SYDUPFILE FROM(QTEMP/CLIMPORT) TO(PDSTEMP/*SAME)';

          Monitor;
             RunCmd(%Trim(createFile):%Len(%Trim(createFile)));
          On-Error;
          EndMon;

       EndSr;

       //===================================================================
       // Extract data from json - Level 0
       //===================================================================
       BegSr GetParms;

          // Initialize data structures
          Clear R1Data;
          R1rcdType = '01';
          Clear R2Data;
          R2rr01    = 0;
          R2ar01    = 0;
          R2rr02    = 0;
          R2ar02    = 0;
          R2rr03    = 0;
          R2ar03    = 0;
          R2rr04    = 0;
          R2ar04    = 0;
          R2rr05    = 0;
          R2ar05    = 0;
          R2rr06    = 0;
          R2ar06    = 0;
          R2rr07    = 0;
          R2ar07    = 0;
          R2rr08    = 0;
          R2ar08    = 0;
          R2rr09    = 0;
          R2ar09    = 0;
          R2rr10    = 0;
          R2ar10    = 0;
          R2rcdType = '02';
          Clear R3Data;
          Clear R4Data;
          Clear R5Data;
          Clear R6Data;
          Clear R7Data;
          Clear R8Data;

          // Load the json tree
          inJson = inParms;
          node0 = yajl_string_load_tree(inJson : errMsg500);
          json += ' "requestParms":' + %Trim(inParms);

          If errMsg500 = ' ';
          // Parse the json into the input parms
             reset i0;

             Dow YAJL_object_loop(node0:i0:key0:val);
                Select;
                When key0 = 'claimInfo';
                   ExSr ProcessClaimInfo;
                When key0 = 'remarksListing';
                   ExSr ProcessRemarks;
                When key0 = 'claimPaymentsList';
                   ExSr ProcessPayments;
                Endsl;
             Enddo;

             // Validate json
             ExSr ValidateParms;

          Else;
             AddErrorMessageJson(%Trim(errMsg500):ioRtnCode);
          Endif;

          // Free up the space by removing the tree
          yajl_tree_free(node0);

       EndSr;

       //===================================================================
       // Load Flat File
       //===================================================================
       BegSr LoadFlatFile;

          // Header Record (01)
          Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA) Values(:R1data);

          // Claim Detail Record (02)
          Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA) Values(:R2data);

          // Payment Record (03)
          If inMode <> 'D';
             For i = 1 to 9999;
                importRecord = R3data(i);
                If R3data(i).R3rcdType = ' ';
                   Leave;
                Else;
                   Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA)
                            Values(:importRecord);
                EndIf;
             EndFor;
          EndIf;

          // Reserve Record (04)
          If inMode <> 'D';
             For i = 1 to 9999;
                importRecord = R4data(i);
                If R4data(i).R4rcdType = ' ';
                   Leave;
                Else;
                   Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA)
                            Values(:importRecord);
                EndIf;
             EndFor;
          EndIf;

          // Claim History Record (05)
          If inMode <> 'D';
             For i = 1 to 9999;
                importRecord = R5data(i);
                If R5data(i).R5rcdType = ' ';
                   Leave;
                Else;
                   Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA)
                            Values(:importRecord);
                EndIf;
             EndFor;
          EndIf;

          // Claim Transfer Record (06)
          If inMode = 'D';
             Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA) Values(:R6data);
          EndIf;

          // Darts Master Record (07)
          If inMode <> 'D';
             For i = 1 to 9999;
                importRecord = R7data(i);
                If R7data(i).R7rcdType = ' ';
                   Leave;
                Else;
                   Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA)
                            Values(:importRecord);
                EndIf;
             EndFor;
          EndIf;

          // Darts Detail Record (08)
          If inMode <> 'D';
             For i = 1 to 9999;
                importRecord = R8data(i);
                If R8data(i).R8rcdType = ' ';
                   Leave;
                Else;
                   Exec Sql Insert Into QTEMP/CLIMPORT (VFDATA)
                            Values(:importRecord);
                EndIf;
             EndFor;
          EndIf;

       EndSr;

       //===================================================================
       // Load Remaining Data Elements
       //===================================================================
       BegSr LoadRemainingData;

          // What Type of process are we dealing with: change, add, delete
          If inMode = 'D';
             If R1case = ' ' Or R1case = '0000000';
                Exec Sql Select CMcase Into :intCase  From WMCMP
                     Where CMfnd = :intFnd And CMpds# = :R1clm#;
                R1case = %EditC(intCase:'X');
             EndIf;
             R1uptp = 'D';
          Else;
             If R1case = ' ' Or R1case = '0000000';
                Exec Sql Select Count(*) Into :clmCount From WMCMP
                     Where CMfnd = :intFnd And CMpds# = :R1clm#;
                If clmCount = 0;
                   R1uptp = 'A';
                Else;
                   Exec Sql Select CMcase Into :intCase  From WMCMP
                        Where CMfnd = :intFnd And CMpds# = :R1clm#;
                   R1case = %EditC(intCase:'X');
                   R1uptp = 'C';
                EndIf;
             Else;
                R1uptp = 'C';
             EndIf;
          EndIf;

          // Lets get the carrier policy #
          Exec Sql Select RPcap# Into :R1cap#
                   From WMRPP
                   Where RPco#=:intco# And RPfnd=:intFnd And RPemp#=:intEmp#
                     And RPdiv=:intDiv And RPfyr=:intFyr;

          // Claim Detail (02)
          Exec Sql Select FNrs01, FNrs02, FNrs03, FNrs04, FNrs05,
                          FNrs06, FNrs07, FNrs08, FNrs09, FNrs10
                   Into :rate1, :rate2, :rate3, :rate4, :rate5,
                        :rate6, :rate7, :rate8, :rate9, :rate10
                   From WMFNP
                   Where FNco#=:intco# and FNfnd=:intFnd;

          // Does the order of reserves matter?
          For i=1 to 10;
             If rsvCode(1) = ' ';
                Select;
                When i = 1;
                   R2rt01 = dsReserves(i).code;
                   R2ar01 = dsReserves(i).paid;
                   R2rr01 = dsReserves(i).remaining;
                When i = 2;
                   R2rt02 = dsReserves(i).code;
                   R2ar02 = dsReserves(i).paid;
                   R2rr02 = dsReserves(i).remaining;
                When i = 3;
                   R2rt03 = dsReserves(i).code;
                   R2ar03 = dsReserves(i).paid;
                   R2rr03 = dsReserves(i).remaining;
                When i = 4;
                   R2rt04 = dsReserves(i).code;
                   R2ar04 = dsReserves(i).paid;
                   R2rr04 = dsReserves(i).remaining;
                When i = 5;
                   R2rt05 = dsReserves(i).code;
                   R2ar05 = dsReserves(i).paid;
                   R2rr05 = dsReserves(i).remaining;
                When i = 6;
                   R2rt06 = dsReserves(i).code;
                   R2ar06 = dsReserves(i).paid;
                   R2rr06 = dsReserves(i).remaining;
                When i = 7;
                   R2rt07 = dsReserves(i).code;
                   R2ar07 = dsReserves(i).paid;
                   R2rr07 = dsReserves(i).remaining;
                When i = 8;
                   R2rt08 = dsReserves(i).code;
                   R2ar08 = dsReserves(i).paid;
                   R2rr08 = dsReserves(i).remaining;
                When i = 9;
                   R2rt09 = dsReserves(i).code;
                   R2ar09 = dsReserves(i).paid;
                   R2rr09 = dsReserves(i).remaining;
                When i = 10;
                   R2rt10 = dsReserves(i).code;
                   R2ar10 = dsReserves(i).paid;
                   R2rr10 = dsReserves(i).remaining;
                Endsl;
             Else;
                j = %LookUp(dsReserves(i).code:rsvCode);
                Select;
                When j = 1;
                   R2rt01 = dsReserves(i).code;
                   R2ar01 = dsReserves(i).paid;
                   R2rr01 = dsReserves(i).remaining;
                When j = 2;
                   R2rt02 = dsReserves(i).code;
                   R2ar02 = dsReserves(i).paid;
                   R2rr02 = dsReserves(i).remaining;
                When j = 3;
                   R2rt03 = dsReserves(i).code;
                   R2ar03 = dsReserves(i).paid;
                   R2rr03 = dsReserves(i).remaining;
                When j = 4;
                   R2rt04 = dsReserves(i).code;
                   R2ar04 = dsReserves(i).paid;
                   R2rr04 = dsReserves(i).remaining;
                When j = 5;
                   R2rt05 = dsReserves(i).code;
                   R2ar05 = dsReserves(i).paid;
                   R2rr05 = dsReserves(i).remaining;
                When j = 6;
                   R2rt06 = dsReserves(i).code;
                   R2ar06 = dsReserves(i).paid;
                   R2rr06 = dsReserves(i).remaining;
                When j = 7;
                   R2rt07 = dsReserves(i).code;
                   R2ar07 = dsReserves(i).paid;
                   R2rr07 = dsReserves(i).remaining;
                When j = 8;
                   R2rt08 = dsReserves(i).code;
                   R2ar08 = dsReserves(i).paid;
                   R2rr08 = dsReserves(i).remaining;
                When j = 9;
                   R2rt09 = dsReserves(i).code;
                   R2ar09 = dsReserves(i).paid;
                   R2rr09 = dsReserves(i).remaining;
                When j = 10;
                   R2rt10 = dsReserves(i).code;
                   R2ar10 = dsReserves(i).paid;
                   R2rr10 = dsReserves(i).remaining;
                Endsl;
             EndIf;
          EndFor;

          R2fnd = R1fnd;

          // Payment Record (03)
          For i=1 to 9999;
              If R3data(i).R3rcdType = ' ';
                 Leave;
              Else;
                 R3data(i).R3fnd  = R1fnd;
                 R3data(i).R3clm# = R1clm#;
              EndIf;
          Endfor;

          // Reserve Record (04)
          If R1uptp = 'A';
             // Only need to create reserve history if there are reserves
             If (R2rr01 + R2ar01) <> 0 Or (R2rr02 + R2ar02) <> 0 Or
                (R2rr03 + R2ar03) <> 0 Or (R2rr04 + R2ar04) <> 0 Or
                (R2rr05 + R2ar05) <> 0 Or (R2rr06 + R2ar06) <> 0 Or
                (R2rr07 + R2ar07) <> 0 Or (R2rr08 + R2ar08) <> 0 Or
                (R2rr09 + R2ar09) <> 0 Or (R2rr10 + R2ar10) <> 0;
                R4data(1).R4rcdType = '04';
                R4data(1).R4fnd     = R1fnd;
                R4data(1).R4clm#    = R1clm#;
                R4data(1).R4efdt    = %EditC(DateTo8(systemDate()):'X');
                R4data(1).R4rt01    = R2rt01;
                R4data(1).R4su01    = R2rr01 + R2ar01;
                R4data(1).R4rt02    = R2rt02;
                R4data(1).R4su02    = R2rr02 + R2ar02;
                R4data(1).R4rt03    = R2rt03;
                R4data(1).R4su03    = R2rr03 + R2ar03;
                R4data(1).R4rt04    = R2rt04;
                R4data(1).R4su04    = R2rr04 + R2ar04;
                R4data(1).R4rt05    = R2rt05;
                R4data(1).R4su05    = R2rr05 + R2ar05;
                R4data(1).R4rt06    = R2rt06;
                R4data(1).R4su06    = R2rr06 + R2ar06;
                R4data(1).R4rt07    = R2rt07;
                R4data(1).R4su07    = R2rr07 + R2ar07;
                R4data(1).R4rt08    = R2rt08;
                R4data(1).R4su08    = R2rr08 + R2ar08;
                R4data(1).R4rt09    = R2rt09;
                R4data(1).R4su09    = R2rr09 + R2ar09;
                R4data(1).R4rt10    = R2rt10;
                R4data(1).R4su10    = R2rr10 + R2ar10;
             EndIf;
          Else;
             // Need to see if there are any changes
             Exec Sql Select CMrr01+CMar01, CMrr02+CMar02, CMrr03+CMar03,
                             CMrr04+CMar04, CMrr05+CMar05, CMrr06+CMar06,
                             CMrr07+CMar07, CMrr08+CMar08, CMrr09+CMar09,
                             CMrr10+CMar10
                       Into  :totalInc01, :totalInc02, :totalInc02,
                             :totalInc04, :totalInc05, :totalInc06,
                             :totalInc07, :totalInc08, :totalInc09,
                             :totalInc10
                       From  wmcmp
                       Where CMfnd = :intFnd And CMpds# = :R1clm#;
             // Create Reserve Record if changes occurred
             If (totalInc01 <> R2rr01 + R2ar01) Or
                (totalInc02 <> R2rr02 + R2ar02) Or
                (totalInc03 <> R2rr03 + R2ar03) Or
                (totalInc04 <> R2rr04 + R2ar04) Or
                (totalInc05 <> R2rr05 + R2ar05) Or
                (totalInc06 <> R2rr06 + R2ar06) Or
                (totalInc07 <> R2rr07 + R2ar07) Or
                (totalInc08 <> R2rr08 + R2ar08) Or
                (totalInc09 <> R2rr09 + R2ar09) Or
                (totalInc10 <> R2rr10 + R2ar10);
                R4data(1).R4rcdType = '04';
                R4data(1).R4fnd     = R1fnd;
                R4data(1).R4clm#    = R1clm#;
                R4data(1).R4efdt    = %EditC(DateTo8(systemDate()):'X');
                R4data(1).R4rt01    = R2rt01;
                R4data(1).R4su01    = (R2rr01 + R2ar01) - totalInc01;
                R4data(1).R4rt02    = R2rt02;
                R4data(1).R4su02    = (R2rr02 + R2ar02) - totalInc02;
                R4data(1).R4rt03    = R2rt03;
                R4data(1).R4su03    = (R2rr03 + R2ar03) - totalInc03;
                R4data(1).R4rt04    = R2rt04;
                R4data(1).R4su04    = (R2rr04 + R2ar04) - totalInc04;
                R4data(1).R4rt05    = R2rt05;
                R4data(1).R4su05    = (R2rr05 + R2ar05) - totalInc05;
                R4data(1).R4rt06    = R2rt06;
                R4data(1).R4su06    = (R2rr06 + R2ar06) - totalInc06;
                R4data(1).R4rt07    = R2rt07;
                R4data(1).R4su07    = (R2rr07 + R2ar07) - totalInc07;
                R4data(1).R4rt08    = R2rt08;
                R4data(1).R4su08    = (R2rr01 + R2ar08) - totalInc08;
                R4data(1).R4rt09    = R2rt09;
                R4data(1).R4su09    = (R2rr01 + R2ar09) - totalInc09;
                R4data(1).R4rt10    = R2rt10;
                R4data(1).R4su10    = (R2rr01 + R2ar10) - totalInc10;
             EndIf;
          EndIf;

          // Claim History Record (05)
          For i=1 to 9999;
              If R5data(i).R5rcdType = ' ';
                 Leave;
              Else;
                 R5data(i).R5fnd  = R1fnd;
                 R5data(i).R5clm# = R1clm#;
              EndIf;
          Endfor;

          // Claim Deletion (06)
          If  inMode = 'D';
              R6rcdType = '06';
              R6fnd     = R1fnd;
              R6clm#    = R1clm#;
              R6trDt    = %EditC(DateTo8(systemDate()):'X');
              R6trTp    = 'D';
              R6grp#    = R1fnd;
              R6emp#    = R1emp#;
              R6div#    = R1div;
              R6ss#     = R1ss#;
              R6acDt    = R1acDt;
              R6pol#    = R1cap#;
          Else;
              If R1uptp = 'C';
                 Exec Sql Select CMfnd, CMemp#, CMdiv, CMss#, CMacDt
                          Into :decOldFnd, :decOldEmp#, :decOldDiv,
                               :chrOldSS#, :decOldAcDt7
                          From wmcmp
                          Where CMfnd = :intFnd And CMpds# = :R1clm#;
                 decOldAcDt8 = DateToYYMD(cvtToDate(decOldAcDt7));
                 If %EditC(decOldfnd:'X') <> R1fnd Or
                    %EditC(decOldEmp#:'X') <> R1emp# Or
                    %EditC(decOldDiv:'X') <> R1div Or chrOldSS# <> R1ss# Or
                    %EditC(decOldAcDt8:'X') <> R1acDt;
                    R6rcdType = '06';
                    R6fnd     = R1fnd;
                    R6clm#    = R1clm#;
                    R6trDt    = %EditC(DateTo8(systemDate()):'X');
                    R6trTp    = 'C';
                    R6grp#    = R1fnd;
                    R6emp#    = R1emp#;
                    R6div#    = R1div;
                    R6ss#     = R1ss#;
                    R6acDt    = R1acDt;
                    R6pol#    = R1cap#;
                 EndIf;
              EndIf;
          EndIf;

          // Darts Master Record (07)
          For i=1 to 9999;
              If R7data(i).R7rcdType = ' ';
                 Leave;
              Else;
                 R7data(i).R7fnd  = R1fnd;
                 R7data(i).R7clm# = R1clm#;
              EndIf;
          Endfor;

          // Darts Detail Record (08)
          For i=1 to 9999;
              If R8data(i).R8rcdType = ' ';
                 Leave;
              Else;
                 R8data(i).R8fnd  = R1fnd;
                 R8data(i).R8clm# = R1clm#;
              EndIf;
          Endfor;

       EndSr;

       //===================================================================
       // Process Accident Address - Level 3 (ProcessAccidentInfo)
       //===================================================================
       BegSr ProcessAccidentAddress;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
              Select;
              When key3 = 'address1';
                 // Not used CMACDR
              When key3 = 'address2';
                 // Not used CMACD2
              When key3 = 'city';
                 // Not used CMACTY
              When key3 = 'state';
                 // Not used CMAST;
              When key3 = 'zip';
                 // Not used CMACZ1 And CMACZ2
              When key3 = 'full';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Accident Information - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessAccidentInfo;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'description';
                If %Len(%Trim(YAJL_get_string(val))) > 60;
                   R2acds = %Subst(YAJL_get_string(val):1:60);
                   R2acd2 = %Subst(YAJL_get_string(val):61);
                Else;
                   R2acds = YAJL_get_string(val);
                   R2acd2 = ' ';
                EndIf;
             When key2 = 'place';
                // Not used CMACNM
             When key2 = 'county';
                ExSr ProcessCounty;
             When key2 = 'address';
                ExSr ProcessAccidentAddress;
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Adjustor - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessAdjustor;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2clex = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             When key2 = 'phone';
                ExSr ProcessAdjustorPhone;
             When key2 = 'email';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Adjustor Phone - Level 3/4 (ProcessAdjustor)
       //===================================================================
       BegSr ProcessAdjustorPhone;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key4:val);
                Select;
                When key4 = 'phone1';
                   ExSr ProcessAdjustorPhone1;
                Endsl;
             Enddo;
             // yajl_tree_free(node4);
          Enddo;

       EndSr;

       //===================================================================
       // Process Adjustor Phone 1 - Level 5 (ProcessAdjustorPhone)
       //===================================================================
       BegSr ProcessAdjustorPhone1;

          // Load the json tree
          node5 = YAJL_object_find(node4:key4);
          Reset i5;
          Dow YAJL_object_loop(node5:i5:key5:val);
             Select;
             When key5 = 'areaCode';
                // Not used
             When key5 = 'phone';
                // Not used
             When key5 = 'type';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Attachments - Level 3/4 (ProcessRemarks)
       //===================================================================
       BegSr ProcessAttachments;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key4:val);
                Select;
                When key4 = 'documentPath';
                   // Not used
                When key4 = 'description';
                   // Not used
                Endsl;
             Enddo;
             // yajl_tree_free(node4);
          Enddo;

       EndSr;

       //===================================================================
       // Process Catastrophe Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessCatastrophe;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2cata = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Cause Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessCauseType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2caus = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claim Numbers - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClaim;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'number';
                R1case = YAJL_get_string(val);
                Monitor;
                   intCase = %Dec(%Trim(R1case):7:0);
                On-Error;
                   intCase = 0;
                EndMon;
                R1case = %EditC(intCase:'X');
             When key2 = 'full';
                // Not used
             When key2 = 'external';
                R1clm# = YAJL_get_string(val);
                R2clm# = YAJL_get_string(val);
                R6clm# = YAJL_get_string(val);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claimant Information - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClaimant;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'name';
                ExSr ProcessClaimantName;
             When key2 = 'address';
                ExSr ProcessClaimantAddress;
             When key2 = 'gender';
                R2sex = YAJL_get_string(val);
             When key2 = 'maritalStatus';
                R2mSts = YAJL_get_string(val);
             When key2 = 'dob';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R2dtob = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'claimantSSN';
                R1ss# = YAJL_get_string(val);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claimant Address - Level 3 (ProcessClaimant)
       //===================================================================
       BegSr ProcessClaimantAddress;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'address1';
                R2adr1 = YAJL_get_string(val);
             When key3 = 'address2';
                R2adr2 = YAJL_get_string(val);
             When key3 = 'city';
                R2cty  = YAJL_get_string(val);
             When key3 = 'state';
                R2st   = YAJL_get_string(val);
             When key3 = 'zip';
                R2zip  = YAJL_get_string(val);
             When key3 = 'countryCode';
                R2ctry = YAJL_get_string(val);
             When key3 = 'nonUsa';
                R2adr3 = YAJL_get_string(val);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claimant Name - Level 3 (ProcessClaimant)
       //===================================================================
       BegSr ProcessClaimantName;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'first';
                R2fsn = YAJL_get_string(val);
             When key3 = 'last';
                R2lsn = YAJL_get_string(val);
             When key3 = 'middle';
                R2mid = YAJL_get_string(val);
             When key3 = 'full';
                // Not needed
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process claim dates - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClaimDates;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'accident';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R1acdt = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'entry';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R2iDt  = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'lastWorked';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R2dtlw = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'death';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R2dod  = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'hire';
                chrDate8 = YAJL_get_string(val);
                If chrDate8 = ' ' Or %Subst(chrdate8:7:2) = ' 0';
                   chrDate8 = '00000000';
                EndIf;
                R2hidt = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             Endsl;
          Enddo;


       EndSr;

       //===================================================================
       // Process Claim Flags - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClaimFlags;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'litigation';
                R2litg = YAJL_get_string(val);
             When key2 = 'vocationalRehab';
                R2vorc = 'N';
                If YAJL_get_string(val) = 'true';
                   R2vorc = 'Y';
                EndIf;
             When key2 = 'denied';
                R2dny  = 'N';
                If YAJL_get_string(val) = 'true';
                   R2dny  = 'Y';
                EndIf;
             When key2 = 'controverted';
                R2cvFl = 'N';
                If YAJL_get_string(val) = 'true';
                   R2cvFl = 'Y';
                EndIf;
             When key2 = 'subrogated';
                R2subr = 'Y';
                If YAJL_get_string(val) = 'true';
                   R2subr = 'N';
                EndIf;
             When key2 = 'reinsuranceReportable';
                // Not used CMRIRP
             When key2 = 'uslh';
                // Not used CMUSLH
             When key2 = 'seasonal';
                // Not used CMSEAE
             When key2 = 'specialDisabilityFund';
                // Not used CMSDF
             When key2 = 'employerLiability';
                // Not used CMLIAF
             When key2 = 'medicareBeneficiary';
                // Not used CDMDBN
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claim Information - Level 1 (GetParms)
       //===================================================================
       BegSr ProcessClaimInfo;

          // Initialize numeric fields
          R2clas = 0;
          R2aww  = 0;

          // Load the json tree
          node1 = YAJL_object_find(node0:key0);
          Reset i1;
          Dow YAJL_object_loop(node1:i1:key1:val);
             Select;
             When key1 = 'claim';
                ExSr ProcessClaim;
             When key1 = 'claimant';
                ExSr ProcessClaimant;
             When key1 = 'status';
                ExSr ProcessClaimStatus;
             When key1 = 'dates';
                ExSr ProcessClaimDates;
             When key1 = 'jurisdictionState';
                R2jrst = YAJL_get_string(val);
             When key1 = 'totalIncurred';
                // Not used
             When key1 = 'class';
                ExSr ProcessClassCode;
             When key1 = 'department';
                ExSr ProcessDepartment;
             When key1 = 'accidentInfo';
                ExSr ProcessAccidentInfo;
             When key1 = 'dateReported';
                ExSr ProcessDateReported;
             When key1 = 'occupationDescription';
                R2ocds = YAJL_get_string(val);
             When key1 = 'aww';
                //*** Begin Chg *** 08/08/22 *******************************
                //*** R2aww  = %Dec(YAJL_get_string(val):9:2);
                Monitor;
                   R2aww = %Dec(YAJL_get_string(val):9:2);
                On-Error;
                   R2aww = 0;
                EndMon;
                //*** End   Chg *** 08/08/22 *******************************
             When key1 = 'lumpSum';
                R2lump = YAJL_get_string(val);
             When key1 = 'methodOfSettlement';
                ExSr ProcessSettlement;
             When key1 = 'adjustor';
                ExSr ProcessAdjustor;
             When key1 = 'supervisor';
                ExSr ProcessSupervisor;
             When key1 = 'location';
                R2ctg1 = YAJL_get_string(val);
             When key1 = 'impairmentPercent';
                If YAJL_get_string(val) = ' ';
                   R2iPct = '00000';
                Else;
                   Monitor;
                      decPct52 = %Dec(YAJL_get_string(val):5:2);
                   On-Error;
                      decPct52 = 0;
                   EndMon;
                   decPct50 = decPct52*100;
                   R2iPct = %EditC(decPct50:'X');
                EndIf;
             When key1 = 'lossType';
                ExSr ProcessLossType;
             When key1 = 'injuryType';
                ExSr ProcessInjuryType;
             When key1 = 'settlementType';
                ExSr ProcessSettlementType;
             When key1 = 'recoveryType';
                ExSr ProcessRecoveryType;
             When key1 = 'coverageType';
                ExSr ProcessCoverageType;
             When key1 = 'lossCoverage';
                ExSr ProcessLossCoverage;
             When key1 = 'fraud';
                ExSr ProcessFraud;
             When key1 = 'nature';
                ExSr ProcessNature;
             When key1 = 'part';
                ExSr ProcessPart;
             When key1 = 'cause';
                ExSr ProcessCauseType;
             When key1 = 'claimFlags';
                ExSr ProcessClaimFlags;
             When key1 = 'LostTimeStatus';
                ExSr ProcessLostTime;
             When key1 = 'mcoType';
                ExSr ProcessMcoType;
             When key1 = 'policyInfo';
                ExSr ProcessPolicyInfo;
             When key1 = 'deductibleReimbursement';
                // Not needed ++++++++++
             When key1 = 'catastrophe';
                ExSr ProcessCatastrophe;
             When key1 = 'lossCondition';
                ExSr ProcessLossCondition;
             When key1 = 'financials';
                ExSr ProcessFinancials;
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claim Status - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClaimStatus;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                // Not used
             When key2 = 'description';
                // Not used CHDESC
             When key2 = 'effective';
                // Not used
             When key2 = 'history';
                ExSr ProcessClaimStatusHistory;
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Claim Status History - Level 3 (ProcessClaimStatus)
       //===================================================================
       BegSr ProcessClaimStatusHistory;

          i = 0;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             i += 1;
             R5data(i).R5rcdType = '05';
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key4:val);
                Select;
                When key4 = 'effective';
                   chrDate8 = YAJL_get_string(val);
                   R5data(i).R5efdt = %Subst(chrDate8:5:4) +
                                      %Subst(chrDate8:1:4);
                When key4 = 'code';
                   R5data(i).R5ocSt = YAJL_get_string(val);
                When key4 = 'description';
                   // Not used
                When key4 = 'explanation';
                   // Not used CHEXPL
                Endsl;
             Enddo;
          Enddo;

       EndSr;

       //===================================================================
       // Process Class Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessClassCode;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                //*** Begin Chg *** 08/08/22 *******************************
                //*** R2clas = %Dec(%Trim(YAJL_get_string(val)):5:0);
                Monitor;
                   R2clas = %Dec(%Trim(YAJL_get_string(val)):5:0);
                On-Error;
                   R2clas = 0;
                EndMon;
                //*** End   Chg *** 08/08/22 *******************************
             When key2 = 'description';
                // Not needed
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process County - Level 3 (ProcessAccidentInfo)
       //===================================================================
       BegSr ProcessCounty;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'code';
                // Not used CDCNTY
             When key3 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Coverage Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessCoverageType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2covt = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process DARTS Detail - Level 3 (ProcessRemarks)
       //===================================================================
       BegSr ProcessDartsDetail;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'dartsKey';
                ExSr ProcessDartsKey;
             When key3 = 'sequence';
                //*** Begin Chg *** 08/08/22 *******************************
                //*R7data(i).R7seq# = %Dec(%Trim(YAJL_get_string(val)):5:0);
                Monitor;
                   R7data(i).R7seq# = %Dec(%Trim(YAJL_get_string(val)):5:0);
                On-Error;
                   R7data(i).R7seq# = 0;
                EndMon;
                //*** End   Chg *** 08/08/22 *******************************
             When key3 = 'date';
                chrDate8 = YAJL_get_string(val);
                //*** Begin Chg *** 08/08/22 *******************************
                //*** R7data(i).R7cdat = %Dec((%Subst(chrDate8:5:4) +
                //***                    %Subst(chrDate8:1:4)):8:0);
                Monitor;
                   R7data(i).R7cdat = %Dec((%Subst(chrDate8:5:4) +
                                      %Subst(chrDate8:1:4)):8:0);
                On-Error;
                   R7data(i).R7cdat = 0;
                EndMon;
                //*** End   Chg *** 08/08/22 *******************************
                R7data(i).R7trDt = R7data(i).R7cdat;
             When key3 = 'remarkCode';
                ExSr ProcessRemarkCode;
             When key3 = 'numberOfAttachments';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Darts History - Level 3/4 (ProcessRemarks)
       //===================================================================
       BegSr ProcessDartsHistory;

          j = 0;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key4:val);
                Select;
                When key4 = 'note';
                   chrComment = YAJL_get_string(val);
                   Dow %Subst(chrComment:1:75) <> ' ';
                      j += 1;
                      R8data(j).R8rcdType = '08';
                      R8data(j).R8seq#    = R7data(j).R7seq#;
                      R8data(j).R8aSq#    = j;
                      R8data(j).R8rmcd    = R7data(j).R7rmcd;
                      R8data(j).R8cm75    = %Subst(chrComment:1:75);
                      chrComment = %Subst(chrComment:76);
                   EndDo;
                When key4 = 'user';
                   // Not used
                When key4 = 'date';
                   // Not used
                When key4 = 'time';
                   // Not used
                Endsl;
             Enddo;
          Enddo;

       EndSr;

       //===================================================================
       // Process DARTS Key - Level 4 (ProcessDartsDetail)
       //===================================================================
       BegSr ProcessDartsKey;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
              Select;
              When key4 = 'type';
                 // Not used
              When key4 = 'key';
                 // Not used
              When key4 = 'formatted';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Diary - Level 3 (ProcessRemarks)
       //===================================================================
       BegSr ProcessDiary;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'workItem';
             When key3 = 'completionDate';
                chrDate8 = YAJL_get_string(val);
                R7data(i).R7dcdt = 0;
                If chrDate8 = ' ';
                   //*** Begin Chg *** 08/08/22 ****************************
                   //*** R7data(i).R7dcdt = %Dec((%Subst(chrDate8:5:4) +
                   //***                    %Subst(chrDate8:1:4)):8:0);
                   Monitor;
                      R7data(i).R7dcdt = %Dec((%Subst(chrDate8:5:4) +
                                         %Subst(chrDate8:1:4)):8:0);
                   On-Error;
                      R7data(i).R7dcdt = 0;
                   EndMon;
                   //*** End   Chg *** 08/08/22 ****************************
                EndIf;
             When key3 = 'user';
                R7data(i).R7user = YAJL_get_string(val);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Date Reported - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessDateReported;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'admin';
                chrDate8 = YAJL_get_string(val);
                R2rcvd = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             When key2 = 'employer';
                chrDate8 = YAJL_get_string(val);
                R2rpdt = %Subst(chrDate8:5:4) + %Subst(chrDate8:1:4);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Department - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessDepartment;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2dept = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Divisions - Level 3 (ProcessPolicyInfo)
       //===================================================================
       BegSr ProcessDivisions;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
             Select;
             When key3 = 'number';
                R1div = YAJL_get_string(val);
             When key3 = 'name';
                // Not used
             When key3 = 'display';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Financials - Level 2/3 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessFinancials;

          i = 0;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_array_loop(node2:i2:node3);
             i += 1;
             Reset i3;
             Dow YAJL_object_loop(node3:i3:key3:val);
                If isTotal;
                   Leave;
                Endif;
                Select;
                When key3 = 'reserve';
                   ExSr ProcessFinancialReserves;
                When key3 = 'inceptionToDate';
                   ExSr ProcessFinancialInceptionToDate;
                When key3 = 'monthToDate';
                   ExSr ProcessFinancialMonthToDate;
                Endsl;
             Enddo;
          Enddo;

       EndSr;

       //===================================================================
       // Process Financials Inception to Date - Level 4 (ProcessFinancials)
       //===================================================================
       BegSr ProcessFinancialInceptionToDate;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'amount';
                // Not used;
             When key4 = 'paid';
                Monitor;
                   decAmount = %Dec(%Trim(YAJL_get_string(val)):9:2);
                On-Error;
                   decAmount = 0;
                EndMon;
                dsReserves(i).paid = decAmount;
             When key4 = 'incurred';
                Monitor;
                   decAmount = %Dec(%Trim(YAJL_get_string(val)):9:2);
                On-Error;
                   decAmount = 0;
                EndMon;
                dsReserves(i).incurred = decAmount;
             Endsl;
          Enddo;
          dsReserves(i).remaining = dsReserves(i).incurred -
                                    dsReserves(i).paid;

       EndSr;

       //===================================================================
       // Process Financials Month to Date - Level 4 (ProcessFinancials)
       //===================================================================
       BegSr ProcessFinancialMonthToDate;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
              Select;
              When key4 = 'amount';
                 // Not used
              When key4 = 'paid';
                 // Not used
              When key4 = 'incurred';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Financials Reserve - Level 4 (ProcessFinancials)
       //===================================================================
       BegSr ProcessFinancialReserves;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'code';
                isTotal = *Off;
                If YAJL_get_string(val) <> ' ';
                   dsReserves(i).code = YAJL_get_string(val);
                Else;
                   isTotal = *On;
                EndIf;
             When key4 = 'type';
                // Not used
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Fraud Indicator Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessFraud;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                //*** Begin Chg *** 08/08/22 *******************************
                //*** R2frau = %Subst(YAJL_get_string(val):2:1);
                Select;
                When %Len(YAJL_get_string(val)) = 2;
                   R2frau = %Subst(YAJL_get_string(val):2:1);
                When %Len(YAJL_get_string(val)) = 1;
                   R2frau = YAJL_get_string(val);
                Other;
                   R2frau = ' ';
                EndSL;
                //*** End   Chg *** 08/08/22 *******************************
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Injury Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessInjuryType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
              Select;
              When key2 = 'code';
                 R2ncci = YAJL_get_string(val);
              When key2 = 'description';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Loss Condition - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessLossCondition;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2lsrv = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Loss Coverage - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessLossCoverage;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2lscv = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Lost Time Flag - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessLostTime;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2ltfl = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Loss Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessLossType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2covt = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process MCO Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessMcoType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
              Select;
              When key2 = 'code';
                 R2mcTp = YAJL_get_string(val);
              When key2 = 'description';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Nature Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessNature;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2natr = YAJL_get_string(val);
             When key2 = 'description';
                // Not needed
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Part Code - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessPart;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2part = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Value 1 - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentBR2;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'code';
                R3data(i).R3br2 = YAJL_get_string(val);
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Value 2 - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentBR7;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'code';
                // Not used
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Date - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentDates;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'posting';
                chrDate8 = YAJL_get_string(val);
                R3data(i).R3podt = %Subst(chrDate8:5:4) +
                                   %Subst(chrDate8:1:4);
             When key4 = 'transaction';
                chrDate8 = YAJL_get_string(val);
                R3data(i).R3trdt = %Subst(chrDate8:5:4) +
                                   %Subst(chrDate8:1:4);
             When key4 = 'service';
                ExSr ProcessPaymentServiceDates;
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Information - Level 3 (ProcessPayments)
       //===================================================================
       BegSr ProcessPaymentInfo;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
              Select;
              When key3 = 'record';
                 ExSr ProcessPaymentRecord;
              When key3 = 'reserve';
                 ExSr ProcessPaymentReserves;
              When key3 = 'transaction';
                 ExSr ProcessPaymentTransaction;
              When key3 = 'paymentValue1';
                 ExSr ProcessPaymentBR2;
              When key3 = 'paymentValue2';
                 ExSr ProcessPaymentBR7;
              When key3 = 'dates';
                 ExSr ProcessPaymentDates;
              When key3 = 'eob';
                 // Not used
              When key3 = 'amount';
                 //*** Begin Chg *** 08/08/22 ******************************
                 //*** decAmount = %Dec(%Trim(YAJL_get_string(val)):9:2);
                 Monitor;
                    decAmount = %Dec(%Trim(YAJL_get_string(val)):9:2);
                 On-Error;
                    decAmount = 0;
                 EndMon;
                 //*** End   Chg *** 08/08/22 ******************************
                 If decAmount < 0;
                    R3data(i).R3amt  = decAmount * -1;
                    R3data(i).R3amts = '-';
                 Else;
                    R3data(i).R3amt  = decAmount;
                    R3data(i).R3amts = ' ';
                 EndIf;
              When key3 = 'payee';
                 ExSr ProcessPaymentPayee;
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Payee - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentPayee;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'fein';
                chrFein = YAJL_get_string(val);
                If chrFein = ' ';
                   chrFein = '000000000';
                Else;
                   Monitor;
                      decFein = %Dec(%Trim(chrFein):9:0);
                      chrFein = %EditC(decFein:'X');
                   On-Error;
                      chrFein = '000000000';
                   EndMon;
                Endif;
             When key4 = 'mod';
                chrMod  = YAJL_get_string(val);
                If chrMod = ' ';
                   chrMod = '000';
                Else;
                   Monitor;
                      decMod = %Dec(%Trim(chrMod):3:0);
                      chrMod = %EditC(decMod:'X');
                   On-Error;
                      chrMod = '000';
                   EndMon;
                Endif;
             When key4 = 'name';
                R3data(i).R3pnam = YAJL_get_string(val);
             Endsl;
          Enddo;

          // combine Fein and Mod to make link
          R3data(i).R3link = chrFein + chrMod;

       EndSr;

       //===================================================================
       // Process Payment Record - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentRecord;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'type';
                R3data(i).R3type = YAJL_get_string(val);
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Reserves - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentReserves;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'type';
                // Not used
             When key4 = 'code';
                R3data(i).R3recd = YAJL_get_string(val);
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payments - Level 1/2 (GetParms)
       //===================================================================
       BegSr ProcessPayments;

          i = 0;

          // Load the json tree
          node1 = YAJL_object_find(node0:key0);
          Reset i1;
          Dow YAJL_array_loop(node1:i1:node2);
             Reset i2;
             i += 1;
             R3data(i).R3rcdType = '03';
             Dow YAJL_object_loop(node2:i2:key2:val);
                Select;
                When key2 = 'paymentInfo';
                   ExSr ProcessPaymentInfo;
                Endsl;
             Enddo;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Service - Level 5 (ProcessPaymentDates)
       //===================================================================
       BegSr ProcessPaymentServiceDates;

          // Load the json tree
          node5 = YAJL_object_find(node4:key4);
          Reset i5;
          Dow YAJL_object_loop(node5:i5:key5:val);
             Select;
             When key5 = 'from';
                chrDate8 = YAJL_get_string(val);
                R3data(i).R3frdt = %Subst(chrDate8:5:4) +
                                   %Subst(chrDate8:1:4);
             When key5 = 'to';
                R3data(i).R3todt = %Subst(chrDate8:5:4) +
                                   %Subst(chrDate8:1:4);
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Payment Transaction - Level 4 (ProcessPaymentInfo)
       //===================================================================
       BegSr ProcessPaymentTransaction;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
              Select;
              When key4 = 'code';
                 // Not used
              When key4 = 'description';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Policy Addresses - Level 3 (ProcessPolicyInfo)
       //===================================================================
       BegSr ProcessPolicyAddresses;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_object_loop(node3:i3:key3:val);
              Select;
              When key3 = 'physical';
                 ExSr ProcessPolicyPhysicalAddress;
              When key3 = 'mailing';
                 ExSr ProcessPolicyMailingAddress;
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Policy Information - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessPolicyInfo;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'company';
                chrCo# = YAJL_get_string(val);
             When key2 = 'insuredName';
                // Not used
             When key2 = 'policyNumber';
                // Not used
             When key2 = 'year';
                R1fyr  = YAJL_get_string(val);
             When key2 = 'group';
                R1fnd  = YAJL_get_string(val);
             When key2 = 'employer';
                R1emp# = YAJL_get_string(val);
             When key2 = 'policyNumber';
                // Not needed
             When key2 = 'divisionInfo';
                Exsr ProcessDivisions;
             When key2 = 'address';
                Exsr ProcessPolicyAddresses;
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Policy Mailing Address - Level 4 (ProcessPolicyAddresses)
       //===================================================================
       BegSr ProcessPolicyMailingAddress;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'address1';
                // Not used
             When key4 = 'address2';
                // Not used
             When key4 = 'city';
                // Not used
             When key4 = 'state';
                // Not used
             When key4 = 'zip';
                // Not used
             When key4 = 'full';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Policy Physical Address - Level 4 (ProcessPolicyAddresses)
       //===================================================================
       BegSr ProcessPolicyPhysicalAddress;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
              Select;
              When key4 = 'address1';
                 // Not used
              When key4 = 'address2';
                 // Not used
              When key4 = 'city';
                 // Not used
              When key4 = 'state';
                 // Not used
              When key4 = 'zip';
                 // Not used
              When key4 = 'full';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process DARTs Remark Code - Level 4 (ProcessDartsDetail)
       //===================================================================
       BegSr ProcessRemarkCode;

          // Load the json tree
          node4 = YAJL_object_find(node3:key3);
          Reset i4;
          Dow YAJL_object_loop(node4:i4:key4:val);
             Select;
             When key4 = 'code';
                R7data(i).R7rmcd = YAJL_get_string(val);
                chrRmkcode =  R7data(i).R7rmcd;
                Exec Sql Select CTfl01 Into :diaryFlag
                         From stctp
                         Where CTrmCd = :chrRmkcode;
                R7data(i).R7difl = diaryFlag;
             When key4 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process DARTs Remarks - Level 1/2 (GetParms)
       //===================================================================
       BegSr ProcessRemarks;

          i = 0;

          // Load the json tree
          node1 = YAJL_object_find(node0:key0);
          Reset i1;
          Dow YAJL_array_loop(node1:i1:node2);
             Reset i2;
             i += 1;
             R7data(i).R7rcdType = '07';
             Dow YAJL_object_loop(node2:i2:key2:val);
                Select;
                When key2 = 'details';
                   ExSr ProcessDartsDetail;
                When key2 = 'diary';
                   ExSr ProcessDiary;
                When key2 = 'subject';
                   If %Len(YAJL_get_string(val)) >= 275;
                      R7data(i).R7comm = %Subst(YAJL_get_string(val):1:275);
                   Else;
                      R7data(i).R7comm = YAJL_get_string(val);
                   EndIf;
                When key2 = 'body';
                   If %Len(YAJL_get_string(val)) >= 275;
                      R7data(i).R7comm = %Subst(YAJL_get_string(val):1:275);
                   Else;
                      R7data(i).R7comm = YAJL_get_string(val);
                   EndIf;
                When key2 = 'history';
                   ExSr ProcessDartsHistory;
                When key2 = 'attachments';
                   ExSr ProcessAttachments;
                Endsl;
             Enddo;
             // yajl_tree_free(node2);
          Enddo;

       EndSr;

       //===================================================================
       // Process Settlements - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessSettlement;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2mtst = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Settlement Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessSettlementType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2mtst = YAJL_get_string(val);
             When key2 = 'description';
                // Not sure if used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Recovery Type - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessRecoveryType;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
              Select;
              When key2 = 'code';
                 // Not sure what this
              When key2 = 'description';
                 // Not used
              Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Supervisor - Level 2 (ProcessClaimInfo)
       //===================================================================
       BegSr ProcessSupervisor;

          // Load the json tree
          node2 = YAJL_object_find(node1:key1);
          Reset i2;
          Dow YAJL_object_loop(node2:i2:key2:val);
             Select;
             When key2 = 'code';
                R2supr = YAJL_get_string(val);
             When key2 = 'description';
                // Not used
             When key2 = 'phone';
                ExSr ProcessSupervisorPhone;
             When key2 = 'email';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Process Supervisor Phone # - Level 3/4 (ProcessSupervisor)
       //===================================================================
       BegSr ProcessSupervisorPhone;

          // Load the json tree
          node3 = YAJL_object_find(node2:key2);
          Reset i3;
          Dow YAJL_array_loop(node3:i3:node4);
             Reset i4;
             Dow YAJL_object_loop(node4:i4:key4:val);
                Select;
                When key4 = 'phone1';
                   ExSr ProcessSupervisorPhone1;
                Endsl;
             Enddo;
          Enddo;

       EndSr;

       //===================================================================
       // Process Supervisor Phone 1 - Level 5 (ProcessSupervisorPhone)
       //===================================================================
       BegSr ProcessSupervisorPhone1;

          // Load the json tree
          node5 = YAJL_object_find(node4:key4);
          Reset i5;
          Dow YAJL_object_loop(node5:i5:key5:val);
             Select;
             When key5 = 'areaCode';
                // Not used
             When key5 = 'phone';
                // Not used
             When key5 = 'type';
                // Not used
             Endsl;
          Enddo;

       EndSr;

       //===================================================================
       // Validate incoming data
       //===================================================================
       BegSr ValidateParms;

          // Populate the company#
          Monitor;
             intCo# = %Dec(chrCo#:3:0);
          On-Error;
             intCo# = UserCo#(Q1user);
          EndMon;

          // Validate Group
          Monitor;
             intFnd = %Dec(R1fnd:3:0);
          On-Error;
             AddErrorMessageJson('Invalid Group':ioRtnCode);
          EndMon;

          // Validate Employer #
          Monitor;
             intEmp# = %Dec(R1emp#:9:0);
          On-Error;
             AddErrorMessageJson('Invalid Employer':ioRtnCode);
          EndMon;

          // Validate Division
          Monitor;
             intDiv = %Dec(R1div:5:0);
          On-Error;
             AddErrorMessageJson('Invalid Division':ioRtnCode);
          EndMon;

          // Invalid Policy Year
          Monitor;
             intFyr = %Dec(R1fyr:3:0);
          On-Error;
             AddErrorMessageJson('Invalid Policy Year':ioRtnCode);
          EndMon;

          // Policy Must exist
          Exec Sql Select count(*) Into :empCount
                   From WDELP
                   Where ELco#=:intCo# And ELfnd=:intFnd And
                         ELemp#=:intEmp# And ELdiv=:intDiv And
                         ELfyr=:intFyr;
          If empCount = 0;
             AddErrorMessageJson('Invalid Policy':ioRtnCode);
          Endif;

          // SS# Must be entered
          If R1ss# = ' ';
             AddErrorMessageJson('SS# is required':ioRtnCode);
          EndIf;

          // Accident Date Must be entered
          dateError = *Off;
          Monitor;
             intAcDt8 = %Dec(R1acdt:8:0);
          On-Error;
             AddErrorMessageJson('Accident Date is required':ioRtnCode);
             dateError = *On;
          EndMon;

          // Accident date must be valid
          If Not dateError;
             intAcDt = dtCYMD(intAcDt8);
             If intAcDt = 0;
                AddErrorMessageJson('Accident Date is invalid':ioRtnCode);
             EndIf;
          EndIf;

          // Is this group included in web service processing
          If not UseGroupInPortal(intCo#:intFnd:'Claim');
             AddErrorMessageJson('Group ' + %char(intFnd) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          Endif;

       EndSr;

      /End-Free

     PImportClaim      e
     P*** End   Add *** 07/07/21 *******************************************

       //===================================================================
       // Claim Payments
       //===================================================================
     pListClaimPayments...
     p                 b
     p*** Begin Add *** 07/07/21 *******************************************
     dListClaimPayments...
     d                 pi
     dinParmsLCP                    100a   Const Options(*nopass)
     d*** End   Add *** 07/07/21 *******************************************

     ddsWDRPP        e ds                  extname(WDRPP) qualified inz
     ddsWDPAP        e ds                  extname(WDPAP) qualified inz
     ddsWMPDP        e ds                  extname(WMPDP) qualified inz
     ddsAmountRange    ds                  qualified inz
     d strFrom                       15    varying
     d strTo                         15    varying
     d from                          11  2 inz(*loval)
     d to                            11  2 inz(*hival)
     ddsDateRange      ds                  qualified inz
     d from8                          8
     d to8                            8
     d from7                          7    inz('0000000')
     d to7                            7    inz('9999999')
     ddsPayees         ds                  qualified inz
     d fein                           9
     d mod                            3
     ddsReserves       ds                  qualified inz
     d type                           1
     d code                           2
     ddsFilters        ds                  qualified inz
     d reserve                             likeds(dsReserves) dim(999)
     d payee                               likeds(dsPayees) dim(999)
     d datePost                            likeds(dsDateRange)
     d dateTrans                           likeds(dsDateRange)
     d amount                              likeds(dsAmountRange)
     d transCodes                     2    dim(99)
     ddsFiltersInz     ds                  likeds(dsFilters) inz
     ddsIn             ds                  qualified inz
     d co#                            3
     d fnd                            3
     d fyr                            3
     d case                           7
     d filter                              likeds(dsFilters)

     dintFnd           s              3  0 inz
     disEofWDPAP       s              1n
     ds1               s             10i 0 inz
     dstrSql           s          10000    varying inz
     dstrSqlWhere      s           5000    varying inz
     d*** Begin Add *** 07/07/21 *******************************************
     d chrIncludeIncomingParms...
     d                 s              5a   Inz(' ')
     d indIncludeParms...
     d                 s               n
     d*** End   Add *** 07/07/21 *******************************************
      /Free
       Exsr GetParms;
       Exsr Validate;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);
       outJSON = json;

       //=================================================================
       // Read output json
       //=================================================================
       Begsr BuildJson;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Reset dsWMPDP;
        //Exec sql
        // select * into :dsWMPDP
        //  from WMPDP
        //  where PDfein = :dsWDPAP.PAfein and PDmod = :dsWDPAP.PAmod;
        json += '{"paymentInfo":{';
        json += '"record":{' +
         BuildJsonNode('type':dsWDPAP.PAtype) + ',' +
         BuildJsonNode('description':dsWDPAP.PAdesc) + '},';
        json += '"reserve":{' +
         BuildJsonNode('type':dsWDPAP.PAreTp) + ',' +
         BuildJsonNode('code':dsWDPAP.PAreCd) + ',' +
         BuildJsonNode('description':ResTypCdDs(dsWDPAP.PAco#:dsWDPAP.PAreCd))
          + '},';
        json += '"transaction":{' +
         BuildJsonNode('code':dsWDPAP.PAtrCd) + ',' +
         BuildJsonNode('description':PaymntCdDs(dsWDPAP.PAco#:dsWDPAP.PAtrCd))
          + '},';
        //*** Begin Add *** 07/07/21 ***************************************
        json += '"paymentValue1":{' +
         BuildJsonNode('code':dsWDPAP.PAbr2) + ',' +
         BuildJsonNode('description':BR2CodeDs(dsWDPAP.PAco#:dsWDPAP.PAbr2))
          + '},';
        json += '"paymentValue2":{' +
         BuildJsonNode('code':dsWDPAP.PAbr7) + ',' +
         BuildJsonNode('description':BR7CodeDs(dsWDPAP.PAco#:dsWDPAP.PAbr7))
          + '},';
        //*** End   Add *** 07/07/21 ***************************************
        json += '"dates":{' +
         BuildJsonNode('posting':%char(dsWDPAP.PApoDt):'date') + ',' +
         BuildJsonNode('transaction':%char(dsWDPAP.PAtrDt):'date') + ',' +
         '"service":{' +
          BuildJsonNode('from':%char(dsWDPAP.PAfrDt):'date') + ',' +
          BuildJsonNode('to':%char(dsWDPAP.PAtoDt):'date') + '}},';
        json += BuildJsonNode('eob':%char(dsWDPAP.PAeob#)) + ',';
        json += BuildJsonNode('amount':%editc(dsWDPAP.PAamt:'J')) + ',';
        json += '"payee":{' +
         BuildJsonNode('fein':%char(dsWDPAP.PAfein)) + ',' +
         BuildJsonNode('mod':%char(dsWDPAP.PAmod)) + ',' +
         BuildJsonNode('name'
          :GetCheckPayeeName(dsWDPAP.PAco#:dsWDPAP.PAfnd:dsWDPAP.PAfyr
           :dsWDPAP.PAcase:dsWDPAP.PAseq#))
          + '},';
        json = %scanrpl(',':'':json:%len(json)-2);
        json += '}},';
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        //Exsr BuildStub;
        Exsr BuildSql;
        json += ', "claimPaymentsList": [';
        Dou isEofWDPAP;
           Exec SQL fetch next from cursorWDPAP into :dsWDPAP;
           Select;
           When not IsValidFetch(sqlStt);
              isEofWDPAP = *on;
              Iter;
           Endsl;
           Exsr BuildJson;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-2);
        json += ']';
       Endsr;

       //=================================================================
       // Build stub results
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListClaimPayments}');
       Endsr;

       //=================================================================
       // Build SQL
       //=================================================================
       Begsr BuildSql;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exsr BuildSqlWhere;
        strSql += 'select * from WDPAP ' + strSqlWhere;
        Exec Sql Close cursorWDPAP;
        Exec Sql Prepare sqlWDPAP From :strSql;
        Exec Sql Declare cursorWDPAP Cursor For sqlWDPAP;
        Exec Sql Open cursorWDPAP;
       Endsr;

       //=================================================================
       // Build SQL where statement
       //=================================================================
       Begsr BuildSqlWhere;
        strSqlWhere += ' where PAco# = ' + %char(intCo#) +
         ' and PAfnd = ' + dsIn.fnd + ' and PAfyr = ' + dsIn.fyr +
         ' and PAcase = ' + dsIn.case;
        If dsIn.filter = dsFiltersInz;
           Leavesr;
        Endif;
        strSqlWhere += ' and';
        If dsIn.filter.reserve(1).type <> dsFiltersInz.reserve(1).type;
           strSqlWhere += ' PAreTp in (';
           For s1 = 1 to %elem(dsFiltersInz.reserve);
              If dsIn.filter.reserve(s1).type = *blank;
                 Iter;
              Endif;
              strSqlWhere += '''' + dsIn.filter.reserve(s1).type + ''',';
           Endfor;
           strSqlWhere = %scanrpl(',':') and':strSqlWhere:%len(strSqlWhere)-1);
        Endif;
        If dsIn.filter.reserve(1).code <> dsFiltersInz.reserve(1).code;
           strSqlWhere += ' PAreCd in (';
           For s1 = 1 to %elem(dsFiltersInz.reserve);
              If dsIn.filter.reserve(s1).code = *blank;
                 Iter;
              Endif;
              strSqlWhere += '''' + dsIn.filter.reserve(s1).code + ''',';
           Endfor;
           strSqlWhere = %scanrpl(',':') and':strSqlWhere:%len(strSqlWhere)-1);
        Endif;
        If dsIn.filter.payee(1) <> dsFiltersInz.payee(1);
           strSqlWhere += ' (';
           For s1 = 1 to %elem(dsFiltersInz.payee);
              If dsIn.filter.payee(s1).fein = *blank;
                 Iter;
              Endif;
              strSqlWhere += ' (PAfein = ' + dsIn.filter.payee(s1).fein;
              If dsIn.Filter.payee(s1).mod <> *blank;
                 strSqlWhere += ' and PAmod = ' + dsIn.filter.payee(s1).mod;
              Endif;
              strSqlWhere += ' ) or';
           Endfor;
           strSqlWhere = %scanrpl('or':') and':strSqlWhere:%len(strSqlWhere)-2);
        Endif;
        If dsIn.filter.datePost <> dsFiltersInz.datePost;
           strSqlWhere += ' (PApoDt between ' + dsIn.filter.datePost.from7 +
            ' and ' + dsIn.filter.datePost.to7 + ') and';
        Endif;
        If dsIn.filter.dateTrans <> dsFiltersInz.dateTrans;
           strSqlWhere += ' (PAtrDt between ' + dsIn.filter.dateTrans.from7 +
            ' and ' + dsIn.filter.dateTrans.to7 + ') and';
        Endif;
        If dsIn.filter.amount <> dsFiltersInz.amount;
           strSqlWhere += ' (PAamt between ' + %char(dsIn.filter.amount.from) +
            ' and ' + %char(dsIn.filter.amount.to) + ') and';
        Endif;
        If dsIn.filter.transCodes(1) <> dsFiltersInz.transCodes(1);
           strSqlWhere += ' PAtrCd in (';
           For s1 = 1 to %elem(dsFiltersInz.transCodes);
              If dsIn.filter.transCodes(s1) = *blank;
                 Iter;
              Endif;
              strSqlWhere += '''' + dsIn.filter.transCodes(s1) + ''',';
           Endfor;
           strSqlWhere = %scanrpl(',':') and':strSqlWhere:%len(strSqlWhere)-1);
        Endif;
        strSqlWhere = %scanrpl('and':'':strSqlWhere:%len(strSqlWhere)-3);
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

        //*** Begin Add *** 07/07/21 ***************************************
        //* We need to get the parameter to determine if the request parms
        //* are to be returned.
        indIncludeParms = *On;
        If %Parms() > 0;
           node0 = yajl_string_load_tree(inParmsLCP : errMsg500);
           If errMsg500 = ' ';
              node = YAJL_object_find(node0:'includeIncomingParms');
              chrIncludeIncomingParms = YAJL_get_string(node);
              If chrIncludeIncomingParms = 'false';
                 indIncludeParms = *Off;
              EndIf;
           EndIf;
           yajl_tree_free(node0);
        EndIf;
        //*** End   Add *** 07/07/21 ***************************************

        dsFiltersInz.datePost = dsDateRange;
        dsFiltersInz.dateTrans = dsDateRange;
        dsFiltersInz.amount = dsAmountRange;
        dsIn.filter = dsFiltersInz;
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson : errMsg500);
        If errMsg500 <> *blanks;
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        //*** Begin Chg *** 07/07/21 ***************************************
        //***json += ' "requestParms":{';
        If indIncludeParms;
           json += ' "requestParms":{';
        EndIf;
        //*** End   Chg *** 07/07/21 ***************************************
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           //*** Begin Chg *** 07/07/21 ************************************
           //***BuildGiantJSON(json:key:YAJL_get_string(val));
           If indIncludeParms;
              BuildGiantJSON(json:key:YAJL_get_string(val));
           EndIf;
           //*** End   Chg *** 07/07/21 ************************************
           Select;
           When key = 'company';
              dsIn.co# = YAJL_get_string(val);
           When key = 'group';
              dsIn.fnd = YAJL_get_string(val);
           When key = 'year';
              dsIn.fyr = YAJL_get_string(val);
           When key = 'claim';
              dsIn.case = YAJL_get_string(val);
           When key = 'filters';
              Exsr GetParmsFilters;
           Endsl;
        Enddo;
        //*** Begin Chg *** 07/07/21 ***************************************
        //***json = %scanrpl(',':'':json:%len(json)-1);
        //***json += '}';
        If indIncludeParms;
           json = %scanrpl(',':'':json:%len(json)-1);
           json += '}';
        EndIf;
        //*** End   Chg *** 07/07/21 ***************************************
        // Free up the space by removing the tree
        yajl_tree_free(node0);
       Endsr;

       //=================================================================
       // Break amount range out
       //=================================================================
       Begsr GetParmsAmountRange;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_object_loop(node2:i2:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsAmountRange.strFrom = YAJL_get_string(val);
           When key = 'to';
              dsAmountRange.strTo = YAJL_get_string(val);
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
        Monitor;
           dsAmountRange.from = %dec(dsAmountRange.strFrom:11:2);
        On-error;
        Endmon;
        Monitor;
           dsAmountRange.to = %dec(dsAmountRange.strTo:11:2);
        On-error;
        Endmon;
        dsIn.filter.amount = dsAmountRange;
       Endsr;

       //=================================================================
       // Break date ranges out
       //=================================================================
       Begsr GetParmsDateRanges;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_object_loop(node2:i2:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'posting';
              Exsr GetParmsDateToFrom;
              dsIn.filter.datePost = dsDateRange;
           When key = 'transaction';
              Exsr GetParmsDateToFrom;
              dsIn.filter.dateTrans = dsDateRange;
           Endsl;
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '}';
       Endsr;

       //=================================================================
       // Break date to/from out
       //=================================================================
       Begsr GetParmsDateToFrom;
        Reset dsDateRange;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node3 = YAJL_object_find(node2:key);
        Reset i3;
        Dow YAJL_object_loop(node3:i3:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsDateRange.from8 = YAJL_get_string(val);
           When key = 'to';
              dsDateRange.to8 = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
        Monitor;
           dsDateRange.from7 =
            %char(DateTo7(CvtToDate(%int(dsDateRange.from8))));
        On-error;
        Endmon;
        Monitor;
           dsDateRange.to7 =
            %char(DateTo7(CvtToDate(%int(dsDateRange.to8))));
        On-error;
        Endmon;
       Endsr;

       //=================================================================
       // Break filters out
       //=================================================================
       Begsr GetParmsFilters;
        json = %scanrpl('""':'{':json:%len(json)-2);
        node1 = YAJL_object_find(node0:key);
        Reset i1;
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'reserves';
              Exsr GetParmsReserves;
           When key = 'payees';
              Exsr GetParmsPayees;
           When key = 'dateRanges';
              Exsr GetParmsDateRanges;
           When key = 'amountRange';
              Exsr GetParmsAmountRange;
           When key = 'transactionCodes';
              Exsr GetParmsTransCodes;
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break payees out
       //=================================================================
       Begsr GetParmsPayees;
        json = %scanrpl('""':'[':json:%len(json)-2);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_array_loop(node2:i2:node3);
           json += '{';
           i3 = 0;
           Dow YAJL_object_loop(node3:i3:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'fein';
                 dsIn.filter.payee(i2).fein = YAJL_get_string(val);
              When key = 'mod';
                 dsIn.filter.payee(i2).mod = YAJL_get_string(val);
              Endsl;
           Enddo;
           json += '},';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Break reserves out
       //=================================================================
       Begsr GetParmsReserves;
        json = %scanrpl('""':'[':json:%len(json)-2);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_array_loop(node2:i2:node3);
           json += '{';
           i3 = 0;
           Dow YAJL_object_loop(node3:i3:key:val);
              BuildGiantJSON(json:key:YAJL_get_string(val));
              Select;
              When key = 'type';
                 dsIn.filter.reserve(i2).type = YAJL_get_string(val);
              When key = 'code';
                 dsIn.filter.reserve(i2).code = YAJL_get_string(val);
              Endsl;
           Enddo;
           json += '},';
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Break transaction codes out
       //=================================================================
       Begsr GetParmsTransCodes;
        json = %scanrpl('""':'[':json:%len(json)-2);
        node2 = YAJL_object_find(node1:key);
        Reset i2;
        Dow YAJL_array_loop(node2:i2:val);
           json += '"' + YAJL_get_string(val) + '",';
           dsIn.filter.transCodes(i2) = YAJL_get_string(val);
        Enddo;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']';
       Endsr;

       //=================================================================
       // Validate request
       //=================================================================
       Begsr Validate;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Monitor;
           intCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        Monitor;
           intFnd = %int(dsIn.fnd);
        On-error;
        Endmon;
        If not UseGroupInPortal(intCo#:intFnd:'Claim');
           AddErrorMessageJson('Group ' + %char(intFnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
        Endif;
       Endsr;
      /End-free
     pListClaimPayments...
     p                 e

       //===================================================================
       // Load financial arrays from claim
       //===================================================================
     pLoadFinancialArrays...
     p                 b

     diTotal           c                   5
      /Free
       Exsr LoadArraysCM;
       Exsr LoadFinancialSummaries;
       //======================================================
       Begsr ConvertToStrings;
        For i1 = 1 to %elem(dsFinancials);
           dsFinancials(i1).inception.strAmount =
            %editc(dsFinancials(i1).inception.amount:'J');
           dsFinancials(i1).inception.strPaid =
            %editc(dsFinancials(i1).inception.paid:'J');
           dsFinancials(i1).inception.strIncurred =
            %editc(dsFinancials(i1).inception.incurred:'J');
           dsFinancials(i1).month.strAmount =
            %editc(dsFinancials(i1).month.amount:'J');
           dsFinancials(i1).month.strPaid =
            %editc(dsFinancials(i1).month.paid:'J');
           dsFinancials(i1).month.strIncurred =
            %editc(dsFinancials(i1).month.incurred:'J');
        Endfor;
       Endsr;
       //======================================================
       Begsr LoadArraysCM;
        // AR array
        aryAR( 1) = dsWMCMP.CMAR01;
        aryAR( 2) = dsWMCMP.CMAR02;
        aryAR( 3) = dsWMCMP.CMAR03;
        aryAR( 4) = dsWMCMP.CMAR04;
        aryAR( 5) = dsWMCMP.CMAR05;
        aryAR( 6) = dsWMCMP.CMAR06;
        aryAR( 7) = dsWMCMP.CMAR07;
        aryAR( 8) = dsWMCMP.CMAR08;
        aryAR( 9) = dsWMCMP.CMAR09;
        aryAR(10) = dsWMCMP.CMAR10;
        // RR array
        aryRR( 1) = dsWMCMP.CMRR01;
        aryRR( 2) = dsWMCMP.CMRR02;
        aryRR( 3) = dsWMCMP.CMRR03;
        aryRR( 4) = dsWMCMP.CMRR04;
        aryRR( 5) = dsWMCMP.CMRR05;
        aryRR( 6) = dsWMCMP.CMRR06;
        aryRR( 7) = dsWMCMP.CMRR07;
        aryRR( 8) = dsWMCMP.CMRR08;
        aryRR( 9) = dsWMCMP.CMRR09;
        aryRR(10) = dsWMCMP.CMRR10;
        // RR array
        aryRT( 1) = dsWMCMP.CMRT01;
        aryRT( 2) = dsWMCMP.CMRT02;
        aryRT( 3) = dsWMCMP.CMRT03;
        aryRT( 4) = dsWMCMP.CMRT04;
        aryRT( 5) = dsWMCMP.CMRT05;
        aryRT( 6) = dsWMCMP.CMRT06;
        aryRT( 7) = dsWMCMP.CMRT07;
        aryRT( 8) = dsWMCMP.CMRT08;
        aryRT( 9) = dsWMCMP.CMRT09;
        aryRT(10) = dsWMCMP.CMRT10;
       Endsr;
       //======================================================
       Begsr LoadFinancialSummaries;
        dsFinancials(1).type = 'C';
        dsFinancials(1).description = 'Compensation';
        dsFinancials(2).type = 'M';
        dsFinancials(2).description = 'Medical';
        dsFinancials(3).type = 'E';
        dsFinancials(3).description = 'Expense';
        dsFinancials(4).type = 'L';
        dsFinancials(4).description = 'Legal';
        dsFinancials(iTotal).type = '';
        dsFinancials(iTotal).description = 'Total';
        For i1 = 1 to %elem(dsFinancials);
           Select;
           When dsFinancials(i1).type = *blank;
              Iter;
           Endsl;
           // Get inception information
           dsFinancials(i1).inception.amount =
            GetReserveAmount(dsFinancials(i1).type);
           dsFinancials(i1).inception.paid =
            GetPaid(dsFinancials(i1).type);
           dsFinancials(i1).inception.incurred =
            dsFinancials(i1).inception.amount +
            dsFinancials(i1).inception.paid;
           dsFinancials(i1).inception.recoveries =
            GetRecoveries(dsFinancials(i1).type);
           dsFinancials(i1).inception.netRecoveries =
            dsFinancials(i1).inception.incurred -
            dsFinancials(i1).inception.recoveries;
           dsFinancials(i1).inception.available =
            dsFinancials(i1).inception.amount -
            GetPayRequests(dsFinancials(i1).type);
           // Get monthly information
           dsFinancials(i1).month.amount =
            GetReserveAmount(dsFinancials(i1).type:'*MTD');
           dsFinancials(i1).month.paid =
            GetPaid(dsFinancials(i1).type:'*MTD');
           dsFinancials(i1).month.incurred =
            dsFinancials(i1).month.amount +
            dsFinancials(i1).month.paid;
           dsFinancials(i1).month.recoveries =
            GetRecoveries(dsFinancials(i1).type:'*MTD');
           dsFinancials(i1).month.netRecoveries =
            dsFinancials(i1).month.incurred -
            dsFinancials(i1).month.recoveries;
           dsFinancials(i1).month.available =
            dsFinancials(i1).month.amount -
            GetPayRequests(dsFinancials(i1).type:'*MTD');
           // Accumulate inception totals
           dsFinancials(iTotal).inception.amount +=
            dsFinancials(i1).inception.amount;
           dsFinancials(iTotal).inception.paid +=
            dsFinancials(i1).inception.paid;
           dsFinancials(iTotal).inception.incurred +=
            dsFinancials(i1).inception.incurred;
           dsFinancials(iTotal).inception.recoveries +=
            dsFinancials(i1).inception.recoveries;
           dsFinancials(iTotal).inception.netRecoveries +=
            dsFinancials(i1).inception.netRecoveries;
           dsFinancials(iTotal).inception.available +=
            dsFinancials(i1).inception.available;
           // Accumulate monthly totals
           dsFinancials(iTotal).month.amount +=
            dsFinancials(i1).month.amount;
           dsFinancials(iTotal).month.paid +=
            dsFinancials(i1).month.paid;
           dsFinancials(iTotal).month.incurred +=
            dsFinancials(i1).month.incurred;
           dsFinancials(iTotal).month.recoveries +=
            dsFinancials(i1).month.recoveries;
           dsFinancials(iTotal).month.netRecoveries +=
            dsFinancials(i1).month.netRecoveries;
           dsFinancials(iTotal).month.available +=
            dsFinancials(i1).month.available;
        Endfor;
        Exsr ConvertToStrings;
       Endsr;
      /End-free
     pLoadFinancialArrays...
     p                 e
