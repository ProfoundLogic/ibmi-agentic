     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h DftActGrp(*No) ActGrp(*Caller)
     h Option(*NoDebugIO: *SrcStmt)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     h/Title WICLMRPTS - Portal Claims Report Requests
      *==========================================================
      * Description: Stores procedures to process various
      *              portal requests for claim JSON returns
      *==========================================================
      * Revisions:
      *
      * 02/14/25 C:342 Support: 118201 (FMIC)
      *                Double quotes in a JSON field escaped with a backslash \
      *
      * 01/04/23 C:079 Support: 99/104331
      *                Fixes to sql statement for top20Report
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      *==========================================================
     fwmchl     if   e           k Disk    Rename(pch01:pch00)
     Fwmprl1    if   e           k Disk
      *Entry
     dRequestPgm       pr                  ExtPgm('WICLMRPT')
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

     d*---------------------------------------------------------------------
     d* Data Structures
     d*---------------------------------------------------------------------
     d psds          esds                  ExtName(szq1p)

     D dbfds           ds
     D  dbrrn                397    400b 0

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
     d maxLen          s             10i 0 inz(32000)

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d inJson          s               a   Len(2000000) varying
     d json            s               a   len(8500000) varying
     d key             s             50a   varying
     d key1            s             50a   varying
     d key2            s             50a   varying
     d key3            s             50a   varying
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d docNode         s                   Like(yajl_val)
     d val             s                   Like(yajl_val)
     d*** Beg Add *** 02/14/25 ******************************************
     d chgname         s             20
     d*** End Add *** 02/14/25 ******************************************

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
      /Copy sPrSecure
      /Copy sPrSql
      /Copy sPrString
      /copy sPrLetter
      /copy sPrXML
      /Copy wPrAgent
      /Copy wPrClaims
      /Copy wPrEmplr
      /Copy wPrGroup
      /Copy wPrOffice
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
       When inRequest = 'ReportClaimSize';
          ClaimSizeReport();
       When inRequest = 'ReportLossExperience';
          LossExperienceReport();
       When inRequest = 'ReportLossExperienceAsOf';
          LossExperienceAsOfReport();
       When inRequest = 'ReportTop20Claims';
          Top20ClaimsRpt();
       EndSl;

       *Inlr = *On;

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
       // Claim Size Report
       //===================================================================
     pClaimSizeReport  b
     dClaimSizeReport  pi

     d dsSizeRpt       ds                  qualified
     d  AHco#                         3  0
     d  AHfnd                         3  0
     d  AHemp#                        9  0
     d  AHdiv                         5  0
     d  AHfyr                         3  0
     d  polYear                       4  0
     d  maxDate                       7  0
     d  AHagt#                        5  0
     d  AGname                       40
     d  AGfein                        9  0
     d  AGmod                         3  0
     d  FNname                       60
     d  totPaid                      18  2
     d  totInc                       28  2
     d  CMcase                        7  0
     d  CMstat                        1
     d  EMoff#                        5
     d  OFname                       40
     d  empName                     102
     d  ELpfDt                        7  0
     d  ELptDt                        7  0
     d  EMfein                        9  0
     d  cxDate                        7  0
     d  clmYear                       4  0

     d filter          ds                  Qualified
     d  groupName                    40a   varying
     d  group#                        3a
     d  officeName                   40a   varying
     d  office#                       5a
     d  agencyName                   40a   varying
     d  agencyFein                    9a
     d  agencyMod                     3a
     d  insuredName                  40a   varying
     d  insuredFein                   9a
     d  policyStatus                 25a   varying
     d  claimStatus                   1a
     d  fromAccDate                   8a
     d  toAccDate                     8a
     d  fromPolEffDt                  8a
     d  toPolEffDt                    8a
     d  agencyType                    1a
     d  sortOrder                    40a   varying

     d group#          s              3  0
     d fein            s              9  0
     d mod             s              3  0
     d polGroup#       s              3  0 Inz(0)
     d polEmp#         s              9  0 Inz(0)
     d polYear         s              3  0 Inz(0)
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
     d toAccDt7        s              7  0
     d toAccDt8        s              8  0
     d strToAccDt7     s              7
     d fromPolEf7      s              7  0
     d fromPolEf8      s              8  0
     d strFromPolEf7   s              7
     d toPolEf7        s              7  0
     d toPolEf8        s              8  0
     d strToPolEf7     s              7
     D policyNumber    s             15a
     D status          s             20a
     d totalInc        s             11s 2
     d totalIncIn      s             11s 2
     d qteFlg          s              1a   inz(' ')
     d count           s              9s 0 Inz(0)

     d #clmAgtYr0K     s              5s 0 Inz
     d #clmPolYr0K     s              5s 0 Inz
     d #clmAccYr0K     s              5s 0 Inz
     d #clmOffYr0K     s              5s 0 Inz
     d #clmTotYr0K     s              5s 0 Inz

     d #clmAgtYr5K     s              5s 0 Inz
     d #clmPolYr5K     s              5s 0 Inz
     d #clmAccYr5K     s              5s 0 Inz
     d #clmOffYr5K     s              5s 0 Inz
     d #clmTotYr5K     s              5s 0 Inz

     d #clmAgtYr10K    s              5s 0 Inz
     d #clmPolYr10K    s              5s 0 Inz
     d #clmAccYr10K    s              5s 0 Inz
     d #clmOffYr10K    s              5s 0 Inz
     d #clmTotYr10K    s              5s 0 Inz

     d #clmAgtYr20K    s              5s 0 Inz
     d #clmPolYr20K    s              5s 0 Inz
     d #clmAccYr20K    s              5s 0 Inz
     d #clmOffYr20K    s              5s 0 Inz
     d #clmTotYr20K    s              5s 0 Inz

     d #clmAgtYr50K    s              5s 0 Inz
     d #clmPolYr50K    s              5s 0 Inz
     d #clmAccYr50K    s              5s 0 Inz
     d #clmOffYr50K    s              5s 0 Inz
     d #clmTotYr50K    s              5s 0 Inz

     d #clmAgtYr100K   s              5s 0 Inz
     d #clmPolYr100K   s              5s 0 Inz
     d #clmAccYr100K   s              5s 0 Inz
     d #clmOffYr100K   s              5s 0 Inz
     d #clmTotYr100K   s              5s 0 Inz

     d #clmAgtYr200K   s              5s 0 Inz
     d #clmPolYr200K   s              5s 0 Inz
     d #clmAccYr200K   s              5s 0 Inz
     d #clmOffYr200K   s              5s 0 Inz
     d #clmTotYr200K   s              5s 0 Inz

     d #clmAgtYr250K   s              5s 0 Inz
     d #clmPolYr250K   s              5s 0 Inz
     d #clmAccYr250K   s              5s 0 Inz
     d #clmOffYr250K   s              5s 0 Inz
     d #clmTotYr250K   s              5s 0 Inz

     d paidAgtYr0K     s             15s 2 Inz
     d paidPolYr0K     s             15s 2 Inz
     d paidAccYr0K     s             15s 2 Inz
     d paidOffYr0K     s             15s 2 Inz
     d paidTotYr0K     s             15s 2 Inz

     d paidAgtYr5K     s             15s 2 Inz
     d paidPolYr5K     s             15s 2 Inz
     d paidAccYr5K     s             15s 2 Inz
     d paidOffYr5K     s             15s 2 Inz
     d paidTotYr5K     s             15s 2 Inz

     d paidAgtYr10K    s             15s 2 Inz
     d paidPolYr10K    s             15s 2 Inz
     d paidAccYr10K    s             15s 2 Inz
     d paidOffYr10K    s             15s 2 Inz
     d paidTotYr10K    s             15s 2 Inz

     d paidAgtYr20K    s             15s 2 Inz
     d paidPolYr20K    s             15s 2 Inz
     d paidAccYr20K    s             15s 2 Inz
     d paidOffYr20K    s             15s 2 Inz
     d paidTotYr20K    s             15s 2 Inz

     d paidAgtYr50K    s             15s 2 Inz
     d paidPolYr50K    s             15s 2 Inz
     d paidAccYr50K    s             15s 2 Inz
     d paidOffYr50K    s             15s 2 Inz
     d paidTotYr50K    s             15s 2 Inz

     d paidAgtYr100K   s             15s 2 Inz
     d paidPolYr100K   s             15s 2 Inz
     d paidAccYr100K   s             15s 2 Inz
     d paidOffYr100K   s             15s 2 Inz
     d paidTotYr100K   s             15s 2 Inz

     d paidAgtYr200K   s             15s 2 Inz
     d paidPolYr200K   s             15s 2 Inz
     d paidAccYr200K   s             15s 2 Inz
     d paidOffYr200K   s             15s 2 Inz
     d paidTotYr200K   s             15s 2 Inz

     d paidAgtYr250K   s             15s 2 Inz
     d paidPolYr250K   s             15s 2 Inz
     d paidAccYr250K   s             15s 2 Inz
     d paidOffYr250K   s             15s 2 Inz
     d paidTotYr250K   s             15s 2 Inz

     d tincAgtYr0K     s             15s 2 Inz
     d tincPolYr0K     s             15s 2 Inz
     d tincAccYr0K     s             15s 2 Inz
     d tincOffYr0K     s             15s 2 Inz
     d tincTotYr0K     s             15s 2 Inz

     d tincAgtYr5K     s             15s 2 Inz
     d tincPolYr5K     s             15s 2 Inz
     d tincAccYr5K     s             15s 2 Inz
     d tincOffYr5K     s             15s 2 Inz
     d tincTotYr5K     s             15s 2 Inz

     d tincAgtYr10K    s             15s 2 Inz
     d tincPolYr10K    s             15s 2 Inz
     d tincAccYr10K    s             15s 2 Inz
     d tincOffYr10K    s             15s 2 Inz
     d tincTotYr10K    s             15s 2 Inz

     d tincAgtYr20K    s             15s 2 Inz
     d tincPolYr20K    s             15s 2 Inz
     d tincAccYr20K    s             15s 2 Inz
     d tincOffYr20K    s             15s 2 Inz
     d tincTotYr20K    s             15s 2 Inz

     d tincAgtYr50K    s             15s 2 Inz
     d tincPolYr50K    s             15s 2 Inz
     d tincAccYr50K    s             15s 2 Inz
     d tincOffYr50K    s             15s 2 Inz
     d tincTotYr50K    s             15s 2 Inz

     d tincAgtYr100K   s             15s 2 Inz
     d tincPolYr100K   s             15s 2 Inz
     d tincAccYr100K   s             15s 2 Inz
     d tincOffYr100K   s             15s 2 Inz
     d tincTotYr100K   s             15s 2 Inz

     d tincAgtYr200K   s             15s 2 Inz
     d tincPolYr200K   s             15s 2 Inz
     d tincAccYr200K   s             15s 2 Inz
     d tincOffYr200K   s             15s 2 Inz
     d tincTotYr200K   s             15s 2 Inz

     d tincAgtYr250K   s             15s 2 Inz
     d tincPolYr250K   s             15s 2 Inz
     d tincAccYr250K   s             15s 2 Inz
     d tincOffYr250K   s             15s 2 Inz
     d tincTotYr250K   s             15s 2 Inz

     d holdAgt#        s              5s 0
     d holdOff#        s              5
     d holdPolYr       s              4s 0
     d holdClmYr       s              4s 0

     d agtPrimary      s               n
     d agtSecondary    s               n
     d offPrimary      s               n
     d offSecondary    s               n
     d agtFirst        s               n
     d offFirst        s               n
     d yrFirst         s               n
     d polYearSort     s               n
     d clmYearSort     s               n

     D sqlAgtDate      s           1000a   Varying
     D sqlAgtInfo      s           1000a   Varying
     D sqlAgtInfo1     s           1000a   Varying
     D sqlAgtInfo2     s           1000a   Varying
     D sqlAgtInfo3     s           1000a   Varying
     D sqlAgtName      s           1000a   Varying
     D sqlClmInfo      s           1500a   Varying
     D sqlRptQry       s           1000a   Varying
     D sqlRptWhere     s           1000a   Varying
     D sqlRptOrder     s           1000a   Varying
     D sqlStmtStr      s           5000a
     D sqlStmtPart1    s           1000a
     D sqlStmtPart2    s           1000a
     D sqlStmtPart3    s           1000a

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

       json = %Trim(json) + ' } ';
       outJson = json;


       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ', "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtnCode = 'error';

       Endsr;

       //=================================================================
       // Build the Claim Report Record
       //=================================================================
       Begsr BldReportRcd;

          If count > 0;
          // json = %Trim(json) + ',';
          EndIf;

          Select;
          When filter.sortOrder = 'Agency/Policy Year';

             Select;
             When holdAgt# <> dsSizeRpt.AHagt#;
                If holdAgt# <> 0;
                   ExSr WritePolYearTotals;
                   ExSr WriteAgtTotals;
                EndIf;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WriteAgtInfo;
                ExSr WritePolYearInfo;
             When holdPolYr <> dsSizeRpt.polYear;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WritePolYearTotals;
                ExSr WritePolYearInfo;
             EndSl;

             // Sum up Policy year Information
             ExSr SumPolYrInfo;

          When filter.sortOrder = 'Agency/Accident Year';

             Select;
             When holdAgt# <> dsSizeRpt.AHagt#;
                If holdAgt# <> 0;
                   ExSr WriteAccYearTotals;
                   ExSr WriteAgtTotals;
                EndIf;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAgtInfo;
                ExSr WriteAccYearInfo;
             When holdClmYr <> dsSizeRpt.clmYear;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAccYearTotals;
                ExSr WriteAccYearInfo;
             EndSl;

             // Sum up Claim year Information
             ExSr SumClmYrInfo;

          When filter.sortOrder = 'Agency/Office/Policy Year';

             Select;
             When holdAgt# <> dsSizeRpt.AHagt#;
                If holdAgt# <> 0;
                   ExSr WritePolYearTotals;
                   ExSr WriteOffTotals;
                   ExSr WriteAgtTotals;
                EndIf;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdOff#  = dsSizeRpt.EMoff#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WriteAgtInfo;
                ExSr WriteOffInfo;
                ExSr WritePolYearInfo;
             When holdOff# <> dsSizeRpt.EMoff#;
                ExSr WritePolYearTotals;
                ExSr WriteOffTotals;
                holdOff#  = dsSizeRpt.EMoff#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WriteOffInfo;
                ExSr WritePolYearInfo;
             When holdPolYr <> dsSizeRpt.polYear;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WritePolYearTotals;
                ExSr WritePolYearInfo;
             EndSl;

             // Sum up Policy year Information
             ExSr SumPolYrInfo;

          When filter.sortOrder = 'Agency/Office/Accident Year';

             Select;
             When holdAgt# <> dsSizeRpt.AHagt#;
                If holdAgt# <> 0;
                   ExSr WriteAccYearTotals;
                   ExSr WriteOffTotals;
                   ExSr WriteAgtTotals;
                EndIf;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdOff#  = dsSizeRpt.EMoff#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAgtInfo;
                ExSr WriteOffInfo;
                ExSr WriteAccYearInfo;
             When holdOff# <> dsSizeRpt.EMoff#;
                ExSr WriteAccYearTotals;
                ExSr WriteOffTotals;
                holdOff#  = dsSizeRpt.EMoff#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteOffInfo;
                ExSr WriteAccYearInfo;
             When holdClmYr <> dsSizeRpt.clmYear;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAccYearTotals;
                ExSr WriteAccYearInfo;
             EndSl;

             // Sum up Claim year Information
             ExSr SumClmYrInfo;

          When filter.sortOrder = 'Office/Policy Year';

             Select;
             When holdOff# <> dsSizeRpt.EMoff#;
                If holdOff# <> ' ';
                   ExSr WritePolYearTotals;
                   ExSr WriteOffTotals;
                EndIf;
                holdOff#  = dsSizeRpt.EMoff#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WriteOffInfo;
                ExSr WritePolYearInfo;
             When holdPolYr <> dsSizeRpt.polYear;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WritePolYearTotals;
                ExSr WritePolYearInfo;
             EndSl;

             // Sum up Policy year Information
             ExSr SumPolYrInfo;

          When filter.sortOrder = 'Office/Accident Year';

             Select;
             When holdOff# <> dsSizeRpt.EMoff#;
                If holdOff# <> ' ';
                   ExSr WriteAccYearTotals;
                   ExSr WriteOffTotals;
                EndIf;
                holdOff#  = dsSizeRpt.EMoff#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteOffInfo;
                ExSr WriteAccYearInfo;
             When holdClmYr <> dsSizeRpt.clmYear;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAccYearTotals;
                ExSr WriteAccYearInfo;
             EndSl;

             // Sum up Claim year Information
             ExSr SumClmYrInfo;

          When filter.sortOrder = 'Office/Agency/Policy Year';

             Select;
             When holdOff# <> dsSizeRpt.EMoff#;
                If holdOff# <> ' ';
                   ExSr WritePolYearTotals;
                   ExSr WriteAgtTotals;
                   ExSr WriteOffTotals;
                EndIf;
                holdOff#  = dsSizeRpt.EMoff#;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WriteOffInfo;
                ExSr WriteAgtInfo;
                ExSr WritePolYearInfo;
             When holdAgt# <> dsSizeRpt.AHagt#;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WritePolYearTotals;
                ExSr WriteAgtTotals;
                ExSr WriteAgtInfo;
                ExSr WritePolYearInfo;
             When holdPolYr <> dsSizeRpt.polYear;
                holdPolYr = dsSizeRpt.polYear;
                ExSr WritePolYearTotals;
                ExSr WritePolYearInfo;
             EndSl;

             // Sum up Policy Year Information
             ExSr SumPolYrInfo;

          When filter.sortOrder = 'Office/Agency/Accident Year';

             Select;
             When holdOff# <> dsSizeRpt.EMoff#;
                If holdOff# <> ' ';
                   ExSr WriteAccYearTotals;
                   ExSr WriteAgtTotals;
                   ExSr WriteOffTotals;
                Endif;
                holdOff#  = dsSizeRpt.EMoff#;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteOffInfo;
                ExSr WriteAgtInfo;
                ExSr WriteAccYearInfo;
             When holdAgt# <> dsSizeRpt.AHagt#;
                holdAgt#  = dsSizeRpt.AHagt#;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAccYearTotals;
                ExSr WriteAgtTotals;
                ExSr WriteAgtInfo;
                ExSr WriteAccYearInfo;
             When holdClmYr <> dsSizeRpt.clmYear;
                holdClmYr = dsSizeRpt.clmYear;
                ExSr WriteAccYearTotals;
                ExSr WriteAccYearInfo;
             EndSl;

             // Sum up Claim Year Information
             ExSr SumClmYrInfo;

          EndSl;

       Endsr;

       //=================================================================
       // Build Claim Size Report
       //=================================================================
       Begsr BldReportResults;

          // Set Sort Flag
          ExSr SetSortFlags;

          // Start Size Report
          json = %Trim(json) + ', "reportName":"Claim Size Report",';

          // Build agency sql statement
          sqlAgtDate =
             'With max_Agt_Date as (Select AHco#, AHfnd, ' +
             'AHemp#, AHdiv, AHfyr, AHfyr + 1900, Max(AHefdt) maxDate ' +
             'From wmahp ' +
             'Where AHefdt <= ' + %EditC(DateTo7(SystemDate()):'X') + ' ' +
             'And AHstat <> ''I'' ' +
             'Group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr),';

          sqlAgtInfo1 =
             'Agt_Agt#1 as (Select a.*, b.AHagt# agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt),';

          sqlAgtInfo2 =
             'Agt_Agt#2 as (Select a.*, b.AHsAgt agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt ' +
             'Where b.AHsAgt <> 0),';

          sqlAgtInfo3 =
             'Agt_Agt#3 as (Select a.*, b.AH3Agt agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt ' +
             'Where b.AH3Agt <> 0),';

          Select;
          When filter.agencyType = ' ';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#1 union ' +
                             'Select * from agt_agt#2 union ' +
                             'Select * from agt_agt#3),';
          When filter.agencyType = '1';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#1), ';
          When filter.agencyType = '2';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#2), ';
          When filter.agencyType = '3';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#3), ';
          EndSl;

          sqlAgtName =
             'Agt_Name as (Select a.*, c.AGname, c.AGfein, c.AGmod, ' +
             'd.FNname ' +
             'From Agt_Agt# a join WMAAP b ' +
             'on a.AHco#=b.AAco# And a.AHfnd=b.AAfnd And a.agent#=b.AAagt# ' +
             'join WMAGP c on b.AAfein=c.AGfein And b.AAmod=c.AGmod ' +
             'join WMFNP d on a.AHco#=d.FNco# And a.AHfnd=d.FNfnd),';

          sqlClmInfo =
             'Size_Report as (Select a.*, (CMar01+CMar02+CMar03+CMar04+' +
             'CMar05+CMar06+CMar07+CMar08+CMar09+CMar10) TotPaid, ' +
             '(CMar01+CMar02+CMar03+CMar04+CMar05+' +
             'CMar06+CMar07+CMar08+CMar09+CMar10+' +
             'CMrr01+CMrr02+CMrr03+CMrr04+CMrr05+' +
             'CMrr06+CMrr07+CMrr08+CMrr09+CMrr10) TotInc, ' +
             'b.CMcase, b.CMstat, c.EMoff#, d.OFname, ' +
             'Trim(c.EMname) || c.EMnam2 empName, e.ELpfdt, e.ELptdt, ' +
             'c.EMfein, ' + ZSqlCxDate('b') + ',' +
             'decimal(substr(trim(char(cmacdt+19000000)),1,4),4,0) ' +
             'as clmYear ' +
             'From Agt_Name a join WMCMP b on a.AHco#=b.CMco# And ' +
             'a.AHfnd=b.CMfnd And a.AHemp#=b.CMemp# And ' +
             'a.AHdiv=b.CMdiv And a.AHfyr=b.CMfyr ' +
             'join WMEMP c on c.EMco#=b.CMco# And ' +
             'c.EMfnd=b.CMfnd And c.EMemp#=b.CMemp# And ' +
             'c.EMdiv=b.CMdiv Left Outer Join WMOFP d on ' +
             'c.EMco#=d.OFco# And c.EMoff#=d.OFofc# ' +
             'join WDELP e on e.ELco#=b.CMco# And e.ELfnd=b.CMfnd And ' +
             'e.ELemp#=b.CMemp# And e.ELdiv=b.CMdiv And e.ELfyr=b.CMfyr)';

          sqlRptQry = 'Select * From Size_Report';

          // Add on Filters
          If filter.group#        <> ' ' Or filter.groupName    <> ' ' Or
             filter.officeName    <> ' ' Or filter.office#      <> ' ' Or
             filter.agencyName    <> ' ' Or filter.agencyFein   <> ' ' Or
             filter.agencyMod     <> ' ' Or filter.insuredName  <> ' ' Or
             filter.insuredFein   <> ' ' Or filter.claimStatus  <> ' ' Or
             filter.fromAccDate   <> ' ' Or filter.toAccDate    <> ' ' Or
             filter.fromPolEffDt  <> ' ' Or filter.toPolEffDt   <> ' ' Or
             filter.policyStatus  <> ' ';

             sqlRptWhere = ' Where ';

             // Group # Filter;
             If filter.group# <> ' ';
                subset = *On;
                sqlRptWhere = %Trim(sqlRptWhere) + ' AHfnd = '  +
                             %Trim(filter.group#);
             Else;
                // Group Name Filter;
                If filter.groupName <> ' ';
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(FNname) like ''%'' || Upper(''' +
                      %Trim(filter.groupName) + ''') || ''%''';
                EndIf;
             EndIf;

             // Office # Filter;
             If filter.office# <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMoff# = '''  +
                                 filter.office# + '''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMoff# = '''  +
                                 filter.office# + '''';
                EndIf;
             Else;
                // Office Name Filter;
                If filter.officeName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   Else;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' And Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Agency Fein Filter
             If filter.agencyFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' AGfein=' +
                                 filter.agencyFein;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And AGfein=' +
                                 filter.agencyFein;
                EndIf;
             EndIf;

             // Agency Modifier Filter
             If filter.agencyMod <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' AGmod=' + filter.agencyMod;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And AGmod=' + filter.agencyMod;
                EndIf;
             EndIf;

             // Agency Name Filter
             If filter.agencyName <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(AGname) like ''%' +
                       %Trim(Upper(filter.agencyName)) + '%''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And Upper(AGname) like ''%' +
                      %Trim(Upper(filter.agencyName)) + '%''';
                EndIf;
             EndIf;

             // Insured Filters
             If filter.insuredFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMfein = '  +
                                %Trim(filter.insuredFein);
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMfein = '  +
                                %Trim(filter.insuredFein);
                EndIf;
             Else;
                // Insured Name Filter;
                If filter.insuredName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                         ' Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   Else;
                      sqlrptWhere = %Trim(sqlRptWhere) +
                         ' And Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Claim Status
             If filter.claimStatus <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' CMstat = ''' + %Trim(filter.claimStatus) + ''' ';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And CMstat = ''' + %Trim(filter.claimStatus) + ''' ';
                EndIf;
             EndIf;

             // Policy Status
             If filter.policyStatus <> ' ';
                If subset = *Off;
                   subset = *On;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And';
                EndIf;
                sqlRptWhere = %Trim(sqlRptWhere)
                 + ZSqlWherePolicyStatus(filter.policyStatus);
             EndIf;

             // From Accident Date Filters
             If filter.fromAccDate <> ' ' And
                filter.fromAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                EndIf;
             EndIf;

             // To Accident Date Filters
             If filter.toAccDate <> ' ' And
                filter.toAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                EndIf;
             EndIf;

             // From Policy Effective Date Filters
             If filter.fromPolEffDt <> ' ' And
                filter.fromPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                EndIf;
             EndIf;

             // To Policy Effective Date Filter
             If filter.toPolEffDt <> ' ' And
                filter.toPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                EndIf;
             EndIf;
          EndIf;

          // Establish sort Order
          If filter.sortOrder <> ' ';
             Select;
             When filter.sortOrder = 'Agency/Policy Year';
                sqlRptOrder = 'Order by agent#, AHfyr Desc';
             When filter.sortOrder = 'Agency/Accident Year';
                sqlRptOrder = 'Order by agent#, clmYear Desc';
             When filter.sortOrder = 'Agency/Office/Policy Year';
                sqlRptOrder = 'Order by agent#, EMoff#, AHfyr Desc';
             When filter.sortOrder = 'Agency/Office/Accident Year';
                sqlRptOrder = 'Order by agent#, EMoff#, clmYear Desc';
             When filter.sortOrder = 'Office/Policy Year';
                sqlRptOrder = 'Order by EMoff#, AHfyr Desc';
             When filter.sortOrder = 'Office/Accident Year';
                sqlRptOrder = 'Order by EMoff#, clmYear Desc';
             When filter.sortOrder = 'Office/Agency/Policy Year';
                sqlRptOrder = 'Order by EMoff#, agent#, AHfyr Desc';
             When filter.sortOrder = 'Office/Agency/Accident Year';
                sqlRptOrder = 'Order by EMoff#, agent#, clmYear Desc';
             EndSl;
          EndIf;

          sqlStmtStr = 'with' + ZSqlAgtDate  + ', ' + sqlAgtInfo1 + ' ' +
                       sqlAgtInfo2 + ' ' + sqlAgtInfo3 + ' ' +
                       sqlAgtinfo  + ' ' + ZSqlAgtName(*off)  + ', ' +
                       sqlClmInfo  + ' ' + sqlRptQry   + ' ' +
                       sqlRptWhere + ' ' + sqlRptOrder;

          sqlStmtPart1 = %Subst(sqlStmtStr:1:1000);
          sqlStmtPart2 = %Subst(sqlStmtStr:1001:1000);
          sqlStmtPart3 = %Subst(sqlStmtStr:2001:1000);

          // Prepare the SQL statement
          Exec SQL prepare rptStmt1 from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare rptCursor1 cursor for rptStmt1;

          // Run SQL
          Exec SQL open rptCursor1;

          // Get first record if there is one
          Exec SQL fetch next from rptCursor1 into :dsSizeRpt;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             ExSr FinishReport;
             moreRows = *off;
             Exec SQL close rptCursor1;
          Else;

             // If row was fetched then process
             DoU moreRows = *Off;

                count += 1;

                // Build Report Record
                Exsr BldReportRcd;

                // Fetch next Record
                Exec SQL fetch next from rptCursor1 into :dsSizeRpt;

                // check to see if there was another record fetched.
                If sqlstt <> '00000';
                   ExSr FinishReport;
                   moreRows = *Off;
                   Exec SQL close rptCursor1;
                EndIf;
             EndDo;
          EndIf;

       EndSr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"ClaimSizeReport" : {' +
                 BuildJsonNode('groupName':filter.groupName) +
                 ', ' +
                 BuildJsonNode('group#':filter.group#)  +
                 ', ' +
                 BuildJsonNode('officeName':filter.officeName) +
                 ', ' +
                 BuildJsonNode('office#':filter.office#)  +
                 ', ' +
                 BuildJsonNode('agencyName':filter.agencyName) +
                 ', ' +
                 BuildJsonNode('agencyFein':filter.agencyFein) +
                 ', ' +
                 BuildJsonNode('agencyMod':filter.agencyMod) +
                 ', ' +
                 BuildJsonNode('insuredName':filter.insuredName) +
                 ', ' +
                 BuildJsonNode('insuredFein':filter.insuredFein) +
                 ', ' +
                 BuildJsonNode('policyStatus':filter.policyStatus) +
                 ', ' +
                 BuildJsonNode('claimStatus':filter.claimStatus) +
                 ', ' +
                 BuildJsonNode('fromAccidentDate':%editw(fromAccDt8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toAccidentDate':%editw(toAccDt8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('fromPolicyEffDate':%editw(fromPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyEffDate':%editw(toPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('agencyType':filter.agencyType) +
                 ', ' +
                 BuildJsonNode('sortOrder':filter.sortOrder);

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Size Report Results
          ExSr BldReportResults;

       Endsr;

       //=================================================================
       // Finish The Report                                                 quests
       //=================================================================
       Begsr FinishReport;

          If count > 0;
             Select;
             When filter.sortOrder = 'Agency/Policy Year';
                ExSr WritePolYearTotals;
                ExSr WriteAgtTotals;
             When filter.sortOrder = 'Agency/Accident Year';
                ExSr WriteAccYearTotals;
                ExSr WriteAgtTotals;
             When filter.sortOrder = 'Agency/Office/Policy Year';
                ExSr WritePolYearTotals;
                ExSr WriteOffTotals;
                ExSr WriteAgtTotals;
             When filter.sortOrder = 'Agency/Office/Accident Year';
                ExSr WriteAccYearTotals;
                ExSr WriteOffTotals;
                ExSr WriteAgtTotals;
             When filter.sortOrder = 'Office/Policy Year';
                ExSr WritePolYearTotals;
                ExSr WriteOffTotals;
             When filter.sortOrder = 'Office/Accident Year';
                ExSr WriteAccYearTotals;
                ExSr WriteOffTotals;
             When filter.sortOrder = 'Office/Agency/Policy Year';
                ExSr WritePolYearTotals;
                ExSr WriteAgtTotals;
                ExSr WriteOffTotals;
             When filter.sortOrder = 'Office/Agency/Accident Year';
                ExSr WriteAccYearTotals;
                ExSr WriteAgtTotals;
                ExSr WriteOffTotals;
             EndSl;
          EndIf;

          ExSr WriteRptTotals;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'groupName');
             filter.groupName   = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'group#');
             filter.group#      = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'officeName');
             filter.officeName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'office#');
             filter.office#     = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyName');
             filter.agencyName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyFein');
             filter.agencyFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyMod');
             filter.agencyMod  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredName');
             filter.insuredName = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredFein');
             filter.insuredFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'policyStatus');
             filter.policyStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'claimStatus');
             filter.claimStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromAccidentDate');
             filter.fromAccDate = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toAccidentDate');
             filter.toAccDate  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyEffDate');
             filter.fromPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyEffDate');
             filter.toPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyType');
             filter.agencyType = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'sortOrder');
             filter.sortOrder = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             //------------------------------------------------------
             // Office # must be valid              <<<<<<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.office# <> ' ';
                // If the office # valid?
                If ValidOffice(filter.office#) = *off;
                   errMsg = %Trim(errMsg) + '"Invalid office#."             ';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Fein: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Fein: ' +
                                               'Invalid numeric data"';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             // Modifier
             If filter.agencyMod <> ' ';
                Monitor;
                   mod = %Dec(%Trim(filter.agencyMod):3:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Mod: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Mod: ' +
                                               'Invalid numeric data"';
                   EndIf;
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
                If errMsg = ' ';
                   errMsg = %Trim(errMsg) + '"Invalid agency Fein/Mod."';
                Else;
                   errMsg = %Trim(errMsg) + ',"Invalid agency Fein/Mod."';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Insured: Invalid numeric ' +
                                               'data."';
                   Else;
                      errMsg = %Trim(errMsg) + '",Insured: Invalid numeric '+
                                               'data."';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             //------------------------------------------------------
             // Validate Policy Status                 <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.policyStatus <> ' '
              And filter.policyStatus <> 'All'
              And filter.policyStatus <> 'Active'
              And filter.policyStatus <> 'Cancelled'
              And filter.policyStatus <> 'Expired'
              And filter.policyStatus <> 'Pending'
              And filter.policyStatus <> 'Pending Cancellation';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Policy Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Policy Status." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Claim Status                  <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.claimStatus <> ' ' And filter.claimStatus <> 'O' And
                filter.claimStatus <> 'C';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Claim Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Claim Status." ';
                   EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                                '"From Accident Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"From Accident Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromAccDt8 <> 0;
                   fromAccDt7     = dtCYMD(fromAccDt8);
                   strfromAccDt7  = %EditC(fromAccDt7:'X');
                   If fromAccDt7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Accident Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Accident Date: Invalid Date." ';
                      EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                                '"To Accident Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Accident Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toAccDt8 <> 0;
                   toAccDt7     = dtCYMD(toAccDt8);
                   strToAccDt7  = %EditC(toAccDt7:'X');
                   If toAccDt7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Accident Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Accident Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Effective date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolEffDt = ' ' Or filter.fromPolEffDt = '00000000';
                fromPolEf8     = 0;
                fromPolEf7     = 0;
                strFromPolEf7  = '0000000';
             Else;
                Monitor;
                   fromPolEf8 = %Dec(filter.fromPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Effective Date: Invalid Numeric Data. " ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"From Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEf8 <> 0;
                   fromPolEf7     = dtCYMD(fromPolEf8);
                   strfromPolEf7  = %EditC(fromPolEf7:'X');
                   If fromPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Policy Effective Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Effective Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Effective date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolEffDt = ' ' Or filter.toPolEffDt = '00000000';
                toPolEf8     = 0;
                toPolEf7     = 0;
                strToPolEf7  = '0000000';
             Else;
                Monitor;
                   toPolEf8 = %Dec(filter.toPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Effective Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEf8 <> 0;
                   toPolEf7     = dtCYMD(toPolEf8);
                   strToPolEf7  = %EditC(toPolEf7:'X');
                   If toPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Policy Effective Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Effective Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Agency Type                   <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.agencyType <> ' ' And filter.agencyType <> '1' And
                filter.agencyType <> '2' And filter.agencyType <> '3';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Agency Type."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Agency Type." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Sort Order                    <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.sortOrder <> 'Agency/Policy Year' And
                filter.sortOrder <> 'Agency/Accident Year' And
                filter.sortOrder <> 'Agency/Office/Accident Year' And
                filter.sortOrder <> 'Agency/Office/Policy Year' And
                filter.sortOrder <> 'Office/Policy Year' And
                filter.sortOrder <> 'Office/Accident Year' And
                filter.sortOrder <> 'Office/Agency/Accident Year' And
                filter.sortOrder <> 'Office/Agency/Policy Year';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Sort Order."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Sort Order." ';
                   EndIf;
             EndIf;

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

       //=================================================================
       // Set Sort Flags
       //=================================================================
       Begsr SetSortFlags;

          agtPrimary   = *Off;
          offPrimary   = *Off;
          agtSecondary = *Off;
          offSecondary = *Off;
          polYearSort  = *Off;
          clmYearSort  = *Off;
          agtFirst     = *On;
          offFirst     = *On;
          yrFirst      = *On;

          Select;
          When filter.sortOrder = 'Agency/Policy Year';
             agtPrimary  = *On;
             polYearSort = *On;
          When filter.sortOrder = 'Agency/Accident Year';
             agtPrimary  = *On;
             clmYearSort = *On;
          When filter.sortOrder = 'Agency/Office/Policy Year';
             agtPrimary    = *On;
             offSecondary  = *On;
             polYearSort   = *On;
          When filter.sortOrder = 'Agency/Office/Accident Year';
             agtPrimary    = *On;
             offSecondary  = *On;
             clmYearSort   = *On;
          When filter.sortOrder = 'Office/Policy Year';
             offPrimary    = *On;
             polYearSort   = *On;
          When filter.sortOrder = 'Office/Accident Year';
             offPrimary    = *On;
             clmYearSort   = *On;
          When filter.sortOrder = 'Office/Agency/Policy Year';
             offPrimary    = *On;
             agtSecondary  = *On;
             polYearSort   = *On;
          When filter.sortOrder = 'Office/Agency/Accident Year';
             offPrimary    = *On;
             agtSecondary  = *On;
             clmYearSort   = *On;
          EndSl;

       Endsr;

       //=================================================================
       // Sum Claim Year Info
       //=================================================================
       Begsr SumClmYrInfo;

          Select;
          When dsSizeRpt.totInc >=0 And dsSizeRpt.totInc < 5000;
             #clmTotYr0K += 1;
             paidTotYr0K += dsSizeRpt.totPaid;
             tincTotYr0K += dsSizeRpt.totInc;
             #clmAgtYr0K += 1;
             paidAgtYr0K += dsSizeRpt.totPaid;
             tincAgtYr0K += dsSizeRpt.totInc;
             #clmOffYr0K += 1;
             paidOffYr0K += dsSizeRpt.totPaid;
             tincOffYr0K += dsSizeRpt.totInc;
             #clmAccYr0K += 1;
             paidAccYr0K += dsSizeRpt.totPaid;
             tincAccYr0K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=5000 And dsSizeRpt.totInc < 5000;
             #clmTotYr5K += 1;
             paidTotYr5K += dsSizeRpt.totPaid;
             tincTotYr5K += dsSizeRpt.totInc;
             #clmAgtYr5K += 1;
             paidAgtYr5K += dsSizeRpt.totPaid;
             tincAgtYr5K += dsSizeRpt.totInc;
             #clmOffYr5K += 1;
             paidOffYr5K += dsSizeRpt.totPaid;
             tincOffYr5K += dsSizeRpt.totInc;
             #clmAccYr5K += 1;
             paidAccYr5K += dsSizeRpt.totPaid;
             tincAccYr5K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=10000 And dsSizeRpt.totInc < 20000;
             #clmTotYr10K += 1;
             paidTotYr10K += dsSizeRpt.totPaid;
             tincTotYr10K += dsSizeRpt.totInc;
             #clmAgtYr10K += 1;
             paidAgtYr10K += dsSizeRpt.totPaid;
             tincAgtYr10K += dsSizeRpt.totInc;
             #clmOffYr10K += 1;
             paidOffYr10K += dsSizeRpt.totPaid;
             tincOffYr10K += dsSizeRpt.totInc;
             #clmAccYr10K += 1;
             paidAccYr10K += dsSizeRpt.totPaid;
             tincAccYr10K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=20000 And dsSizeRpt.totInc < 50000;
             #clmTotYr20K += 1;
             paidTotYr20K += dsSizeRpt.totPaid;
             tincTotYr20K += dsSizeRpt.totInc;
             #clmAgtYr20K += 1;
             paidAgtYr20K += dsSizeRpt.totPaid;
             tincAgtYr20K += dsSizeRpt.totInc;
             #clmOffYr20K += 1;
             paidOffYr20K += dsSizeRpt.totPaid;
             tincOffYr20K += dsSizeRpt.totInc;
             #clmAccYr20K += 1;
             paidAccYr20K += dsSizeRpt.totPaid;
             tincAccYr20K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=50000 And dsSizeRpt.totInc < 100000;
             #clmTotYr50K += 1;
             paidTotYr50K += dsSizeRpt.totPaid;
             tincTotYr50K += dsSizeRpt.totInc;
             #clmAgtYr50K += 1;
             paidAgtYr50K += dsSizeRpt.totPaid;
             tincAgtYr50K += dsSizeRpt.totInc;
             #clmOffYr50K += 1;
             paidOffYr50K += dsSizeRpt.totPaid;
             tincOffYr50K += dsSizeRpt.totInc;
             #clmAccYr50K += 1;
             paidAccYr50K += dsSizeRpt.totPaid;
             tincAccYr50K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=100000 And dsSizeRpt.totInc < 200000;
             #clmTotYr100K += 1;
             paidTotYr100K += dsSizeRpt.totPaid;
             tincTotYr100K += dsSizeRpt.totInc;
             #clmAgtYr100K += 1;
             paidAgtYr100K += dsSizeRpt.totPaid;
             tincAgtYr100K += dsSizeRpt.totInc;
             #clmOffYr100K += 1;
             paidOffYr100K += dsSizeRpt.totPaid;
             tincOffYr100K += dsSizeRpt.totInc;
             #clmAccYr100K += 1;
             paidAccYr100K += dsSizeRpt.totPaid;
             tincAccYr100K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=200000 And dsSizeRpt.totInc < 250000;
             #clmTotYr200K += 1;
             paidTotYr200K += dsSizeRpt.totPaid;
             tincTotYr200K += dsSizeRpt.totInc;
             #clmAgtYr200K += 1;
             paidAgtYr200K += dsSizeRpt.totPaid;
             tincAgtYr200K += dsSizeRpt.totInc;
             #clmOffYr200K += 1;
             paidOffYr200K += dsSizeRpt.totPaid;
             tincOffYr200K += dsSizeRpt.totInc;
             #clmAccYr200K += 1;
             paidAccYr200K += dsSizeRpt.totPaid;
             tincAccYr200K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=250000;
             #clmTotYr250K += 1;
             paidTotYr250K += dsSizeRpt.totPaid;
             tincTotYr250K += dsSizeRpt.totInc;
             #clmAgtYr250K += 1;
             paidAgtYr250K += dsSizeRpt.totPaid;
             tincAgtYr250K += dsSizeRpt.totInc;
             #clmOffYr250K += 1;
             paidOffYr250K += dsSizeRpt.totPaid;
             tincOffYr250K += dsSizeRpt.totInc;
             #clmAccYr250K += 1;
             paidAccYr250K += dsSizeRpt.totPaid;
             tincAccYr250K += dsSizeRpt.totInc;
          EndSl;

       EndSr;

       //=================================================================
       // Sum Policy Year Info
       //=================================================================
       Begsr SumPolYrInfo;

          Select;
          When dsSizeRpt.totInc >=0 And dsSizeRpt.totInc < 5000;
             #clmTotYr0K += 1;
             paidTotYr0K += dsSizeRpt.totPaid;
             tincTotYr0K += dsSizeRpt.totInc;
             #clmAgtYr0K += 1;
             paidAgtYr0K += dsSizeRpt.totPaid;
             tincAgtYr0K += dsSizeRpt.totInc;
             #clmOffYr0K += 1;
             paidOffYr0K += dsSizeRpt.totPaid;
             tincOffYr0K += dsSizeRpt.totInc;
             #clmPolYr0K += 1;
             paidPolYr0K += dsSizeRpt.totPaid;
             tincPolYr0K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=5000 And dsSizeRpt.totInc < 10000;
             #clmTotYr5K += 1;
             paidTotYr5K += dsSizeRpt.totPaid;
             tincTotYr5K += dsSizeRpt.totInc;
             #clmAgtYr5K += 1;
             paidAgtYr5K += dsSizeRpt.totPaid;
             tincAgtYr5K += dsSizeRpt.totInc;
             #clmOffYr5K += 1;
             paidOffYr5K += dsSizeRpt.totPaid;
             tincOffYr5K += dsSizeRpt.totInc;
             #clmPolYr5K += 1;
             paidPolYr5K += dsSizeRpt.totPaid;
             tincPolYr5K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=10000 And dsSizeRpt.totInc < 20000;
             #clmTotYr10K += 1;
             paidTotYr10K += dsSizeRpt.totPaid;
             tincTotYr10K += dsSizeRpt.totInc;
             #clmAgtYr10K += 1;
             paidAgtYr10K += dsSizeRpt.totPaid;
             tincAgtYr10K += dsSizeRpt.totInc;
             #clmOffYr10K += 1;
             paidOffYr10K += dsSizeRpt.totPaid;
             tincOffYr10K += dsSizeRpt.totInc;
             #clmPolYr10K += 1;
             paidPolYr10K += dsSizeRpt.totPaid;
             tincPolYr10K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=20000 And dsSizeRpt.totInc < 50000;
             #clmTotYr20K += 1;
             paidTotYr20K += dsSizeRpt.totPaid;
             tincTotYr20K += dsSizeRpt.totInc;
             #clmAgtYr20K += 1;
             paidAgtYr20K += dsSizeRpt.totPaid;
             tincAgtYr20K += dsSizeRpt.totInc;
             #clmOffYr20K += 1;
             paidOffYr20K += dsSizeRpt.totPaid;
             tincOffYr20K += dsSizeRpt.totInc;
             #clmPolYr20K += 1;
             paidPolYr20K += dsSizeRpt.totPaid;
             tincPolYr20K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=50000 And dsSizeRpt.totInc < 100000;
             #clmTotYr50K += 1;
             paidTotYr50K += dsSizeRpt.totPaid;
             tincTotYr50K += dsSizeRpt.totInc;
             #clmAgtYr50K += 1;
             paidAgtYr50K += dsSizeRpt.totPaid;
             tincAgtYr50K += dsSizeRpt.totInc;
             #clmOffYr50K += 1;
             paidOffYr50K += dsSizeRpt.totPaid;
             tincOffYr50K += dsSizeRpt.totInc;
             #clmPolYr50K += 1;
             paidPolYr50K += dsSizeRpt.totPaid;
             tincPolYr50K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=100000 And dsSizeRpt.totInc < 200000;
             #clmTotYr100K += 1;
             paidTotYr100K += dsSizeRpt.totPaid;
             tincTotYr100K += dsSizeRpt.totInc;
             #clmAgtYr100K += 1;
             paidAgtYr100K += dsSizeRpt.totPaid;
             tincAgtYr100K += dsSizeRpt.totInc;
             #clmOffYr100K += 1;
             paidOffYr100K += dsSizeRpt.totPaid;
             tincOffYr100K += dsSizeRpt.totInc;
             #clmPolYr100K += 1;
             paidPolYr100K += dsSizeRpt.totPaid;
             tincPolYr100K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=200000 And dsSizeRpt.totInc < 250000;
             #clmTotYr200K += 1;
             paidTotYr200K += dsSizeRpt.totPaid;
             tincTotYr200K += dsSizeRpt.totInc;
             #clmAgtYr200K += 1;
             paidAgtYr200K += dsSizeRpt.totPaid;
             tincAgtYr200K += dsSizeRpt.totInc;
             #clmOffYr200K += 1;
             paidOffYr200K += dsSizeRpt.totPaid;
             tincOffYr200K += dsSizeRpt.totInc;
             #clmPolYr200K += 1;
             paidPolYr200K += dsSizeRpt.totPaid;
             tincPolYr200K += dsSizeRpt.totInc;
          When dsSizeRpt.totInc >=250000;
             #clmTotYr250K += 1;
             paidTotYr250K += dsSizeRpt.totPaid;
             tincTotYr250K += dsSizeRpt.totInc;
             #clmAgtYr250K += 1;
             paidAgtYr250K += dsSizeRpt.totPaid;
             tincAgtYr250K += dsSizeRpt.totInc;
             #clmOffYr250K += 1;
             paidOffYr250K += dsSizeRpt.totPaid;
             tincOffYr250K += dsSizeRpt.totInc;
             #clmPolYr250K += 1;
             paidPolYr250K += dsSizeRpt.totPaid;
             tincPolYr250K += dsSizeRpt.totInc;
          EndSl;

       Endsr;

       //=================================================================
       // Write Agency Information
       //=================================================================
       Begsr WriteAgtInfo;

          #clmAgtYr0K   = 0;
          #clmAgtYr5K   = 0;
          #clmAgtYr10K  = 0;
          #clmAgtYr20K  = 0;
          #clmAgtYr50K  = 0;
          #clmAgtYr100K = 0;
          #clmAgtYr200K = 0;
          #clmAgtYr250K = 0;

          paidAgtYr0K   = 0;
          paidAgtYr5K   = 0;
          paidAgtYr10K  = 0;
          paidAgtYr20K  = 0;
          paidAgtYr50K  = 0;
          paidAgtYr100K = 0;
          paidAgtYr200K = 0;
          paidAgtYr250K = 0;

          tincAgtYr0K   = 0;
          tincAgtYr5K   = 0;
          tincAgtYr10K  = 0;
          tincAgtYr20K  = 0;
          tincAgtYr50K  = 0;
          tincAgtYr100K = 0;
          tincAgtYr200K = 0;
          tincAgtYr250K = 0;

          If agtPrimary;
             offFirst = *On;
             yrFirst  = *On;
          Endif;

          If agtSecondary;
             yrFirst = *On;
          Endif;

          If agtFirst = *On;
             json += '"AgencySort":[';
             agtFirst = *Off;
          Else;
             json += ',';
          EndIf;

          If agtPrimary;
             offFirst = *On;
          EndIf;

          json += '{' +
             BuildJsonNode('Agency#':%EditC(dsSizeRpt.AHagt#:'X')) +
             ',' +
             BuildJsonNode('AgencyName':%Trim(dsSizeRpt.AGname)) +
             ',';

          If offSecondary;
          // json += '"OfficeSort":[';
          EndIf;

       Endsr;

       //=================================================================
       // Write Agency Totals
       //=================================================================
       Begsr WriteAgtTotals;

          json += '], ' +
             BuildJsonNode('AgencyTotals#ofClaims0-5k':
              %EditC(#clmAgtYr0k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims5-10k':
              %EditC(#clmAgtYr5k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims10-20k':
              %EditC(#clmAgtYr10k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims20-50k':
              %EditC(#clmAgtYr20k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims50-100k':
              %EditC(#clmAgtYr50k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims100-200k':
              %EditC(#clmAgtYr100k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims200-250k':
              %EditC(#clmAgtYr200k:'3')) + ',' +
             BuildJsonNode('AgencyTotals#ofClaims250k':
              %EditC(#clmAgtYr250k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid0-5k':
              %EditC(paidAgtYr0k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid5-10k':
              %EditC(paidAgtYr5k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid10-20k':
              %EditC(paidAgtYr10k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid20-50k':
              %EditC(paidAgtYr20k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid50-100k':
              %EditC(paidAgtYr50k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid100-200k':
              %EditC(paidAgtYr100k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid200-250k':
              %EditC(paidAgtYr200k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalPaid250k':
              %EditC(paidAgtYr250k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred0-5k':
              %EditC(tincAgtYr0k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred5-10k':
              %EditC(tincAgtYr5k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred10-20k':
              %EditC(tincAgtYr10k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred20-50k':
              %EditC(tincAgtYr20k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred50-100k':
              %EditC(tincAgtYr50k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred100-200k':
              %EditC(tincAgtYr100k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred200-250k':
              %EditC(tincAgtYr200k:'3')) + ',' +
             BuildJsonNode('AgencyTotalsTotalIncurred250k':
              %EditC(tincAgtYr250k:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Accident Year Information
       //=================================================================
       Begsr WriteAccYearInfo;


          #clmAccYr0K   = 0;
          #clmAccYr5K   = 0;
          #clmAccYr10K  = 0;
          #clmAccYr20K  = 0;
          #clmAccYr50K  = 0;
          #clmAccYr100K = 0;
          #clmAccYr200K = 0;
          #clmAccYr250K = 0;

          paidAccYr0K   = 0;
          paidAccYr5K   = 0;
          paidAccYr10K  = 0;
          paidAccYr20K  = 0;
          paidAccYr50K  = 0;
          paidAccYr100K = 0;
          paidAccYr200K = 0;
          paidAccYr250K = 0;

          tincAccYr0K   = 0;
          tincAccYr5K   = 0;
          tincAccYr10K  = 0;
          tincAccYr20K  = 0;
          tincAccYr50K  = 0;
          tincAccYr100K = 0;
          tincAccYr200K = 0;
          tincAccYr250K = 0;

          If yrFirst;
             json += '"AccidentYearSort":[';
             yrFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"AccidentYear":"' +
                  %EditC(dsSizeRpt.clmYear:'X') + '",';

       Endsr;

       //=================================================================
       // Write Accident Year Totals
       //=================================================================
       Begsr WriteAccYearTotals;

          json +=
             BuildJsonNode('AccidentYearTotals#ofClaims0-5k':
              %EditC(#clmAccYr0k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims5-10k':
              %EditC(#clmAccYr5k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims10-20k':
              %EditC(#clmAccYr10k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims20-50k':
              %EditC(#clmAccYr20k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims50-100k':
              %EditC(#clmAccYr50k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims100-200k':
              %EditC(#clmAccYr100k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims200-250k':
              %EditC(#clmAccYr200k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotals#ofClaims250k':
              %EditC(#clmAccYr250k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid0-5k':
              %EditC(paidAccYr0k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid5-10k':
              %EditC(paidAccYr5k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid10-20k':
              %EditC(paidAccYr10k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid20-50k':
              %EditC(paidAccYr20k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid50-100k':
              %EditC(paidAccYr50k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid100-200k':
              %EditC(paidAccYr100k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid200-250k':
              %EditC(paidAccYr200k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalPaid250k':
              %EditC(paidAccYr250k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred0-5k':
              %EditC(tincAccYr0k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred5-10k':
              %EditC(tincAccYr5k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred10-20k':
              %EditC(tincAccYr10k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred20-50k':
              %EditC(tincAccYr20k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred50-100k':
              %EditC(tincAccYr50k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred100-200k':
              %EditC(tincAccYr100k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred200-250k':
              %EditC(tincAccYr200k:'3')) + ',' +
             BuildJsonNode('AccidentYearTotalsTotalIncurred250k':
              %EditC(tincAccYr250k:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Office Information
       //=================================================================
       Begsr WriteOffInfo;

          #clmOffYr0K   = 0;
          #clmOffYr5K   = 0;
          #clmOffYr10K  = 0;
          #clmOffYr20K  = 0;
          #clmOffYr50K  = 0;
          #clmOffYr100K = 0;
          #clmOffYr200K = 0;
          #clmOffYr250K = 0;

          paidOffYr0K   = 0;
          paidOffYr5K   = 0;
          paidOffYr10K  = 0;
          paidOffYr20K  = 0;
          paidOffYr50K  = 0;
          paidOffYr100K = 0;
          paidOffYr200K = 0;
          paidOffYr250K = 0;

          tincOffYr0K   = 0;
          tincOffYr5K   = 0;
          tincOffYr10K  = 0;
          tincOffYr20K  = 0;
          tincOffYr50K  = 0;
          tincOffYr100K = 0;
          tincOffYr200K = 0;
          tincOffYr250K = 0;

          If offPrimary;
             agtFirst = *On;
             yrFirst  = *On;
          Endif;

          If offSecondary;
             yrFirst = *On;
          Endif;

          If offFirst = *On;
             json += '"OfficeSort":[';
             offFirst = *Off;
          Else;
             json += ',';
          EndIf;

          If offPrimary;
             agtFirst = *On;
          EndIf;

          json += '{' +
             BuildJsonNode('Office#':dsSizeRpt.EMoff#) +
             ',' +
             BuildJsonNode('OfficeName':%Trim(dsSizeRpt.OFname)) +
             ',';

          If agtSecondary;
          // json += '"AgencySort":[';
          EndIf;

       Endsr;

       //=================================================================
       // Write Office Totals
       //=================================================================
       Begsr WriteOffTotals;

          json += '], ' +
             BuildJsonNode('OfficeTotals#ofClaims0-5k':
              %EditC(#clmOffYr0k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims5-10k':
              %EditC(#clmOffYr5k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims10-20k':
              %EditC(#clmOffYr10k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims20-50k':
              %EditC(#clmOffYr20k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims50-100k':
              %EditC(#clmOffYr50k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims100-200k':
              %EditC(#clmOffYr100k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims200-250k':
              %EditC(#clmOffYr200k:'3')) + ',' +
             BuildJsonNode('OfficeTotals#ofClaims250k':
              %EditC(#clmOffYr250k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid0-5k':
              %EditC(paidOffYr0k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid5-10k':
              %EditC(paidOffYr5k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid10-20k':
              %EditC(paidOffYr10k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid20-50k':
              %EditC(paidOffYr20k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid50-100k':
              %EditC(paidOffYr50k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid100-200k':
              %EditC(paidOffYr100k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid200-250k':
              %EditC(paidOffYr200k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalPaid250k':
              %EditC(paidOffYr250k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred0-5k':
              %EditC(tincOffYr0k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred5-10k':
              %EditC(tincOffYr5k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred10-20k':
              %EditC(tincOffYr10k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred20-50k':
              %EditC(tincOffYr20k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred50-100k':
              %EditC(tincOffYr50k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred100-200k':
              %EditC(tincOffYr100k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred200-250k':
              %EditC(tincOffYr200k:'3')) + ',' +
             BuildJsonNode('OfficeTotalsTotalIncurred250k':
              %EditC(tincOffYr250k:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Policy Year Information
       //=================================================================
       Begsr WritePolYearInfo;

          #clmPolYr0K   = 0;
          #clmPolYr5K   = 0;
          #clmPolYr10K  = 0;
          #clmPolYr20K  = 0;
          #clmPolYr50K  = 0;
          #clmPolYr100K = 0;
          #clmPolYr200K = 0;
          #clmPolYr250K = 0;

          paidPolYr0K   = 0;
          paidPolYr5K   = 0;
          paidPolYr10K  = 0;
          paidPolYr20K  = 0;
          paidPolYr50K  = 0;
          paidPolYr100K = 0;
          paidPolYr200K = 0;
          paidPolYr250K = 0;

          tincPolYr0K   = 0;
          tincPolYr5K   = 0;
          tincPolYr10K  = 0;
          tincPolYr20K  = 0;
          tincPolYr50K  = 0;
          tincPolYr100K = 0;
          tincPolYr200K = 0;
          tincPolYr250K = 0;

          If yrFirst;
             json += '"PolicyYearSort":[';
             yrFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"PolicyYear":"' +  %EditC(dsSizeRpt.polYear:'X') + '",';

       Endsr;

       //=================================================================
       // Write Policy Year Totals
       //=================================================================
       Begsr WritePolYearTotals;

          json +=
             BuildJsonNode('PolicyYearTotals#ofClaims0-5k':
              %EditC(#clmPolYr0k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims5-10k':
              %EditC(#clmTotYr5k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims10-20k':
              %EditC(#clmPolYr10k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims20-50k':
              %EditC(#clmPolYr20k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims50-100k':
              %EditC(#clmPolYr50k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims100-200k':
              %EditC(#clmPolYr100k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims200-250k':
              %EditC(#clmPolYr200k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotals#ofClaims250k':
              %EditC(#clmPolYr250k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid0-5k':
              %EditC(paidPolYr0k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid5-10k':
              %EditC(paidPolYr5k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid10-20k':
              %EditC(paidPolYr10k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid20-50k':
              %EditC(paidPolYr20k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid50-100k':
              %EditC(paidPolYr50k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid100-200k':
              %EditC(paidPolYr100k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid200-250k':
              %EditC(paidPolYr200k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalPaid250k':
              %EditC(paidPolYr250k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred0-5k':
              %EditC(tincPolYr0k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred5-10k':
              %EditC(tincPolYr5k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred10-20k':
              %EditC(tincPolYr10k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred20-50k':
              %EditC(tincPolYr20k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred50-100k':
              %EditC(tincPolYr50k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred100-200k':
              %EditC(tincPolYr100k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred200-250k':
              %EditC(tincPolYr200k:'3')) + ',' +
             BuildJsonNode('PolicyYearTotalsTotalIncurred250k':
              %EditC(tincPolYr250k:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Report Totals
       //=================================================================
       Begsr WriteRptTotals;

          If count > 0;
             json += '],';
          EndIf;

          json +=
             BuildJsonNode('ReportTotals#ofClaims0-5k':
              %EditC(#clmTotYr0k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims5-10k':
              %EditC(#clmTotYr5k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims10-20k':
              %EditC(#clmTotYr10k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims20-50k':
              %EditC(#clmTotYr20k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims50-100k':
              %EditC(#clmTotYr50k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims100-200k':
              %EditC(#clmTotYr100k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims200-250k':
              %EditC(#clmTotYr200k:'3')) + ',' +
             BuildJsonNode('ReportTotals#ofClaims250k':
              %EditC(#clmTotYr250k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid0-5k':
              %EditC(paidTotYr0k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid5-10k':
              %EditC(paidTotYr5k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid10-20k':
              %EditC(paidTotYr10k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid20-50k':
              %EditC(paidTotYr20k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid50-100k':
              %EditC(paidTotYr50k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid100-200k':
              %EditC(paidTotYr100k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid200-250k':
              %EditC(paidTotYr200k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalPaid250k':
              %EditC(paidTotYr250k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred0-5k':
              %EditC(tincTotYr0k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred5-10k':
              %EditC(tincTotYr5k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred10-20k':
              %EditC(tincTotYr10k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred20-50k':
              %EditC(tincTotYr20k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred50-100k':
              %EditC(tincTotYr50k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred100-200k':
              %EditC(tincTotYr100k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred200-250k':
              %EditC(tincTotYr200k:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurred250k':
              %EditC(tincTotYr250k:'3'));

       Endsr;

      /End-free

     pClaimSizeReport  e

       //===================================================================
       // Get Paid This Period Amounts
       //===================================================================
     pGetPaidThisPeriod...
     p                 b
     dGetPaidThisPeriod...
     d                 pi
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inFyr                          3  0 const
     d inCase                         7  0 const
     d inFromDate                     7  0 const
     d intoDate                       7  0 const
     d outMedical                    11  2
     d outComp                       11  2
     d outExpense                    11  2
     d outLegal                      11  2

     d pmtcount        s              9  0

      /free

       // Medical Paid This Period
       Exec SQL
        Select count(*) Into :pmtCount
               From WDPAP
               Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                     PAcase=:inCase and PAtrDt>=:inFromDate And
                     PAtrDt<=:inToDate And PAreTp='M';
       If pmtCount = 0;
          outMedical = 0;
       Else;
          Exec SQL
           Select Sum(Case When PAtype in ('R', 'S', 'V') Then PAamt*-1
                  Else PAamt end) Into :outMedical
                  From WDPAP
                  Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                        PAcase=:inCase and PAtrDt>=:inFromDate And
                        PAtrDt<=:inToDate And PAreTp='M';
       EndIf;

       // Compensation Paid This Period
       Exec SQL
        Select count(*) Into :pmtCount
               From WDPAP
               Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                     PAcase=:inCase and PAtrDt>=:inFromDate And
                     PAtrDt<=:inToDate And PAreTp='C';
       If pmtCount = 0;
          outComp = 0;
       Else;
          Exec SQL
           Select Sum(Case When PAtype in ('R', 'S', 'V') Then PAamt*-1
                  Else PAamt end) Into :outComp
                  From WDPAP
                  Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                        PAcase=:inCase and PAtrDt>=:inFromDate And
                        PAtrDt<=:inToDate And PAreTp='C';
       EndIf;

       // Expense Paid This Period
       Exec SQL
        Select count(*) Into :pmtCount
               From WDPAP
               Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                     PAcase=:inCase and PAtrDt>=:inFromDate And
                     PAtrDt<=:inToDate And PAreTp='E';
       If pmtCount = 0;
          outExpense = 0;
       Else;
          Exec SQL
           Select Sum(Case When PAtype in ('R', 'S', 'V') Then PAamt*-1
                  Else PAamt end) Into :outExpense
                  From WDPAP
                  Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                        PAcase=:inCase and PAtrDt>=:inFromDate And
                        PAtrDt<=:inToDate And PAreTp='E';
       EndIf;

       // Legal Paid This Period
       Exec SQL
        Select count(*) Into :pmtCount
               From WDPAP
               Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                     PAcase=:inCase and PAtrDt>=:inFromDate And
                     PAtrDt<=:inToDate And PAreTp='L';
       If pmtCount = 0;
          outLegal = 0;
       Else;
          Exec SQL
           Select Sum(Case When PAtype in ('R', 'S', 'V') Then PAamt*-1
                  Else PAamt end) Into :outLegal
                  From WDPAP
                  Where PAco#=:inCo# And PAfnd=:inFnd and PAfYr=:inFyr And
                        PAcase=:inCase and PAtrDt>=:inFromDate And
                        PAtrDt<=:inToDate And PAreTp='L';
       EndIf;

      /end-free

     pGetPaidThisPeriod...
     p                 e

       //===================================================================
       // Loss Experience Report
       //===================================================================
     pLossExperienceReport...
     p                 b
     dLossExperienceReport...
     d                 pi

     d dsLossRpt       ds                  qualified
     d  AHco#                         3  0
     d  AHfnd                         3  0
     d  AHemp#                        9  0
     d  AHdiv                         5  0
     d  AHfyr                         3  0
     d  polYear                       4  0
     d  maxDate                       7  0
     d  AHagt#                        5  0
     d  AGname                       40
     d  AGfein                        9  0
     d  AGmod                         3  0
     d  FNname                       60
     d  FNmAd1                       40
     d  FNmAd2                       40
     d  FNmCty                       25
     d  FNmSt                         2
     d  FNmZip                        9  0
     d  FNpAd1                       40
     d  FNpAd2                       40
     d  FNpCty                       25
     d  FNpSt                         2
     d  FNpZip                        9  0
     d  CMrt01                        2
     d  rsvType01                     1
     d  CMrr01                        9  2
     d  CMar01                        9  2
     d  CMrt02                        2
     d  rsvType02                     1
     d  CMrr02                        9  2
     d  CMar02                        9  2
     d  CMrt03                        2
     d  rsvType03                     1
     d  CMrr03                        9  2
     d  CMar03                        9  2
     d  CMrt04                        2
     d  rsvType04                     1
     d  CMrr04                        9  2
     d  CMar04                        9  2
     d  CMrt05                        2
     d  rsvType05                     1
     d  CMrr05                        9  2
     d  CMar05                        9  2
     d  CMrt06                        2
     d  rsvType06                     1
     d  CMrr06                        9  2
     d  CMar06                        9  2
     d  CMrt07                        2
     d  rsvType07                     1
     d  CMrr07                        9  2
     d  CMar07                        9  2
     d  CMrt08                        2
     d  rsvType08                     1
     d  CMrr08                        9  2
     d  CMar08                        9  2
     d  CMrt09                        2
     d  rsvType09                     1
     d  CMrr09                        9  2
     d  CMar09                        9  2
     d  CMrt10                        2
     d  rsvType10                     1
     d  CMrr10                        9  2
     d  CMar10                        9  2
     d  CMltfl                        1
     d  CMlost                        5  0
     d  CMacdt                        7  0
     d  CMclas                        5  0
     d  CMacds                       60
     d  CMacd2                       60
     d  CMlsn                        30
     d  CMfsn                        15
     d  CMmid                         1
     d  CMcase                        7  0
     d  CMstat                        1
     d  CMctg1                       10
     d  EMoff#                        5
     d  OFname                       40
     d  OFadr1                       40
     d  OFadr2                       40
     d  OFcty                        25
     d  OFst                          2
     d  OFzip                         9  0
     d  empName                     100
     d  EMmAd1                       40
     d  EMmAd2                       40
     d  EMmCty                       25
     d  EMmSt                         2
     d  EMmZip                        9  0
     d  EMpAd1                       40
     d  EMpAd2                       40
     d  EMpCty                       25
     d  EMpSt                         2
     d  EMpZip                        9  0
     d  ELpfDt                        7  0
     d  ELptDt                        7  0
     d  ELrtCd                        3
     d  RSjrSt                        2
     d  EMfein                        9  0
     d  cxDate                        7  0
     d  EMcnty                        2
     d  CNdesc                       30
     d  EMrgn#                        3  0
     d  REdesc                       30
     d  EMchap                        3  0
     d  CPdesc                       30
     d  CMdept                       10
     d  DPdesc                       40
     d  ALname                       40
     d  ALmAd1                       40
     d  ALmAd2                       40
     d  ALmCty                       25
     d  ALmSt                         2
     d  ALmZip                        9  0
     ddsLossRptNull    s              5i 0 dim(111)

     d filter          ds                  Qualified
     d  groupName                    40a   varying
     d  group#                        3a
     d  officeName                   40a   varying
     d  office#                       5a
     d  agencyName                   40a   varying
     d  agencyFein                    9a
     d  agencyMod                     3a
     d  insuredName                  40a   varying
     d  insuredFein                   9a
     d  policyStatus                 25a   varying
     d  fromPolEffDt                  8a
     d  toPolEffDt                    8a
     d  fromPolExpDt                  8a
     d  toPolExpDt                    8a
     d  sortOrder                    40a   varying

     d group#          s              3  0
     d fein            s              9  0
     d mod             s              3  0
     d polGroup#       s              3  0 Inz(0)
     d polEmp#         s              9  0 Inz(0)
     d polYear         s              3  0 Inz(0)
     d division        s              5  0 Inz(0)
     d case#           s              7  0 Inz(0)
     d badData         s               n   Inz(*Off)
     d dateError       s               n   Inz(*Off)
     d moreRows        s               n   Inz(*On)
     d subset          s               n   Inz(*Off)
     d meetSS          s               n   Inz(*On)
     d fromPolEf7      s              7  0
     d fromPolEf8      s              8  0
     d strFromPolEf7   s              7
     d toPolEf7        s              7  0
     d toPolEf8        s              8  0
     d strToPolEf7     s              7
     d fromPolEx7      s              7  0
     d fromPolEx8      s              8  0
     d strFromPolEx7   s              7
     d toPolEx7        s              7  0
     d toPolEx8        s              8  0
     d strToPolEx7     s              7
     D policyNumber    s             15a
     D status          s             20a
     d totalInc        s             11s 2
     d totalIncIn      s             11s 2
     d qteFlg          s              1a   inz(' ')
     d count           s              9s 0 Inz(0)
     d insFirst        s               n   Inz(*On)
     d yrFirst         s               n   Inz(*On)
     d divFirst        s               n   Inz(*On)
     d dptFirst        s               n   Inz(*On)
     d locFirst        s               n   Inz(*On)
     d clmFirst        s               n   Inz(*On)
     d holdFnd         s              3  0 Inz(0)
     d holdEmp#        s              9  0 Inz(0)
     d holdPoLYr       s              3  0 Inz(0)
     d holdDivision    s              5  0 Inz(0)
     d holdDept        s             10    Inz(' ')
     d holdDept2       s             10    Inz(' ')
     d holdLocation    s             10    Inz(' ')
     d locationSort    s               n   Inz(*Off)
     d divisionSort    s               n   Inz(*Off)
     d departmentSort  s               n   Inz(*Off)
     d endReport       s               n   Inz(*Off)

     d clmStatus       s             20    Inz(' ')
     d clmPayDesc      s             40    Inz(' ')
     d clmLostTime     s             20    Inz(' ')
     d clmDayOf        s             10    Inz(' ')
     d clmClosedDate   s             10    Inz(' ')

     d inFromDate      s              7  0
     d inToDate        s              7  0
     d outComp         s             11  2
     d outMedical      s             11  2
     d outExpense      s             11  2
     d outLegal        s             11  2
     d paidComp        s             11  2
     d paidMedical     s             11  2
     d paidExpense     s             11  2
     d paidLegal       s             11  2
     d rsvComp         s             11  2
     d rsvMedical      s             11  2
     d rsvExpense      s             11  2
     d rsvLegal        s             11  2
     d incComp         s             11  2
     d incMedical      s             11  2
     d incExpense      s             11  2
     d incLegal        s             11  2

     D sqlAgtDate      s           1000a   varying
     D sqlAgtInfo      s           1000a   varying
     D sqlAgtName      s           1000a   varying
     D sqlClmInfo      s           5000a   varying
     D sqlRptQry       s           1000a   varying
     D sqlRptWhere     s           1000a   varying
     D sqlRptOrder     s           1000a   varying
     D sqlStmtStr      s          11000a   varying

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

     d* Paid To Date
     d ptpDivCmp       s             15s 2 Inz
     d ptpDivExp       s             15s 2 Inz
     d ptpDivLeg       s             15s 2 Inz
     d ptpDivMed       s             15s 2 Inz
     d ptpDivTot       s             15s 2 Inz

     d ptpDptCmp       s             15s 2 Inz
     d ptpDptExp       s             15s 2 Inz
     d ptpDptLeg       s             15s 2 Inz
     d ptpDptMed       s             15s 2 Inz
     d ptpDptTot       s             15s 2 Inz

     d ptpInsCmp       s             15s 2 Inz
     d ptpInsExp       s             15s 2 Inz
     d ptpInsLeg       s             15s 2 Inz
     d ptpInsMed       s             15s 2 Inz
     d ptpInsTot       s             15s 2 Inz

     d ptpLocCmp       s             15s 2 Inz
     d ptpLocExp       s             15s 2 Inz
     d ptpLocLeg       s             15s 2 Inz
     d ptpLocMed       s             15s 2 Inz
     d ptpLocTot       s             15s 2 Inz

     d ptpPolCmp       s             15s 2 Inz
     d ptpPolExp       s             15s 2 Inz
     d ptpPolLeg       s             15s 2 Inz
     d ptpPolMed       s             15s 2 Inz
     d ptpPolTot       s             15s 2 Inz

     d ptpTotCmp       s             15s 2 Inz
     d ptpTotExp       s             15s 2 Inz
     d ptpTotLeg       s             15s 2 Inz
     d ptpTotMed       s             15s 2 Inz
     d ptpTotTot       s             15s 2 Inz

     d* Total Paid
     d paidDivCmp      s             15s 2 Inz
     d paidDivExp      s             15s 2 Inz
     d paidDivLeg      s             15s 2 Inz
     d paidDivMed      s             15s 2 Inz
     d paidDivTot      s             15s 2 Inz

     d paidDptCmp      s             15s 2 Inz
     d paidDptExp      s             15s 2 Inz
     d paidDptLeg      s             15s 2 Inz
     d paidDptMed      s             15s 2 Inz
     d paidDptTot      s             15s 2 Inz

     d paidInsCmp      s             15s 2 Inz
     d paidInsExp      s             15s 2 Inz
     d paidInsLeg      s             15s 2 Inz
     d paidInsMed      s             15s 2 Inz
     d paidInsTot      s             15s 2 Inz

     d paidLocCmp      s             15s 2 Inz
     d paidLocExp      s             15s 2 Inz
     d paidLocLeg      s             15s 2 Inz
     d paidLocMed      s             15s 2 Inz
     d paidLocTot      s             15s 2 Inz

     d paidPolCmp      s             15s 2 Inz
     d paidPolExp      s             15s 2 Inz
     d paidPolLeg      s             15s 2 Inz
     d paidPolMed      s             15s 2 Inz
     d paidPolTot      s             15s 2 Inz

     d paidTotCmp      s             15s 2 Inz
     d paidTotExp      s             15s 2 Inz
     d paidTotLeg      s             15s 2 Inz
     d paidTotMed      s             15s 2 Inz
     d paidTotTot      s             15s 2 Inz

     d* Outstanding Reserves
     d outsDivCmp      s             15s 2 Inz
     d outsDivExp      s             15s 2 Inz
     d outsDivLeg      s             15s 2 Inz
     d outsDivMed      s             15s 2 Inz
     d outsDivTot      s             15s 2 Inz

     d outsDptCmp      s             15s 2 Inz
     d outsDptExp      s             15s 2 Inz
     d outsDptLeg      s             15s 2 Inz
     d outsDptMed      s             15s 2 Inz
     d outsDptTot      s             15s 2 Inz

     d outsInsCmp      s             15s 2 Inz
     d outsInsExp      s             15s 2 Inz
     d outsInsLeg      s             15s 2 Inz
     d outsInsMed      s             15s 2 Inz
     d outsInsTot      s             15s 2 Inz

     d outsLocCmp      s             15s 2 Inz
     d outsLocExp      s             15s 2 Inz
     d outsLocLeg      s             15s 2 Inz
     d outsLocMed      s             15s 2 Inz
     d outsLocTot      s             15s 2 Inz

     d outsPolCmp      s             15s 2 Inz
     d outsPolExp      s             15s 2 Inz
     d outsPolLeg      s             15s 2 Inz
     d outsPolMed      s             15s 2 Inz
     d outsPolTot      s             15s 2 Inz

     d outsTotCmp      s             15s 2 Inz
     d outsTotExp      s             15s 2 Inz
     d outsTotLeg      s             15s 2 Inz
     d outsTotMed      s             15s 2 Inz
     d outsTotTot      s             15s 2 Inz

     d* Total Incurred
     d tincDivCmp      s             15s 2 Inz
     d tincDivExp      s             15s 2 Inz
     d tincDivLeg      s             15s 2 Inz
     d tincDivMed      s             15s 2 Inz
     d tincDivTot      s             15s 2 Inz

     d tincDptCmp      s             15s 2 Inz
     d tincDptExp      s             15s 2 Inz
     d tincDptLeg      s             15s 2 Inz
     d tincDptMed      s             15s 2 Inz
     d tincDptTot      s             15s 2 Inz

     d tincInsCmp      s             15s 2 Inz
     d tincInsExp      s             15s 2 Inz
     d tincInsLeg      s             15s 2 Inz
     d tincInsMed      s             15s 2 Inz
     d tincInsTot      s             15s 2 Inz

     d tincLocCmp      s             15s 2 Inz
     d tincLocExp      s             15s 2 Inz
     d tincLocLeg      s             15s 2 Inz
     d tincLocMed      s             15s 2 Inz
     d tincLocTot      s             15s 2 Inz

     d tincPolCmp      s             15s 2 Inz
     d tincPolExp      s             15s 2 Inz
     d tincPolLeg      s             15s 2 Inz
     d tincPolMed      s             15s 2 Inz
     d tincPolTot      s             15s 2 Inz

     d tincTotCmp      s             15s 2 Inz
     d tincTotExp      s             15s 2 Inz
     d tincTotLeg      s             15s 2 Inz
     d tincTotMed      s             15s 2 Inz
     d tincTotTot      s             15s 2 Inz

      /free
       Exsr GetParameters;
       If errMsg = ' ';
          Exsr BldReturnInfo;
       Else;
          Exsr BldErrorsInfo;
       EndIf;

       json = %Trim(json) + ' } ';
       outJson = json;


       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ', "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtnCode = 'error';

       Endsr;

       //=================================================================
       // Build the Claim Report Record
       //=================================================================
       Begsr BldReportRcd;

          Select;
          When filter.sortOrder = 'Employer/Policy Year';

             Select;
             When holdFnd  <> dsLossRpt.AHfnd Or
                  holdEmp# <> dsLossRpt.AHemp# Or
                  holdPolYr  <> dsLossRpt.AHfYr;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHemp#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                When holdPolYr  <> dsLossRpt.AHfYr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                EndSl;
                holdFnd   = dsLossRpt.AHfnd;
                holdEmp#  = dsLossRpt.AHemp#;
                holdPolYr = dsLossRpt.AHfYr;
             EndSl;

             // Sum up Reserve Amounts
             ExSr SumAmounts;

          When filter.sortOrder = 'Employer/Policy Year/Department';

             If dsLossRpt.CMdept = ' ';
                holdDept2 = 'blank';
             Else;
                holdDept2 = dsLossRpt.CMdept;
             Endif;

             Select;
             When holdFnd      <> dsLossRpt.AHfnd Or
                  holdEmp#     <> dsLossRpt.AHemp# Or
                  holdPolYr    <> dsLossRpt.AHfYr Or
                  holdDept     <> holdDept2;

                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHemp#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WriteDepartmentTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                   ExSr WriteDepartmentInfo;
                When holdPolYr <> dsLossRpt.AHfyr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WriteDepartmentTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                   ExSr WriteDepartmentInfo;
                When holdDept <> holdDept2;
                   If holdDept <> ' ';
                      json += '],';
                      ExSr WriteDepartmentTotals;
                   EndIf;
                   ExSr WriteDepartmentInfo;
                EndSl;
                holdFnd      = dsLossRpt.AHfnd;
                holdEmp#     = dsLossRpt.AHemp#;
                holdPolYr    = dsLossRpt.AHfYr;
                If dsLossRpt.CMdept = ' ';
                   holdDept  = 'blank';
                Else;
                   holdDept  = dsLossRpt.CMdept;
                Endif;
             EndSl;

             // Sum up Policy Information
             ExSr SumAmounts;

          When filter.sortOrder = 'Employer/Policy Year/Division';

             Select;
             When holdFnd      <> dsLossRpt.AHfnd Or
                  holdEmp#     <> dsLossRpt.AHemp# Or
                  holdPolYr    <> dsLossRpt.AHfYr Or
                  holdDivision <> dsLossRpt.AHdiv;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHemp#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WriteDivisionTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                   ExSr WriteDivisionInfo;
                When holdPolYr <> dsLossRpt.AHfyr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WriteDivisionTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                   ExSr WriteDivisionInfo;
                When holdDivision <> dsLossRpt.AHdiv;
                   If not divFirst;
                      json += '],';
                      ExSr WriteDivisionTotals;
                   EndIf;
                   ExSr WriteDivisionInfo;
                EndSl;
                holdFnd      = dsLossRpt.AHfnd;
                holdEmp#     = dsLossRpt.AHemp#;
                holdPolYr    = dsLossRpt.AHfYr;
                holdDivision = dsLossRpt.AHdiv;
             EndSl;

             // Sum up Policy Information
             ExSr SumAmounts;

          When filter.sortOrder = 'Employer/Policy Year/Location';

             Select;
             When holdFnd      <> dsLossRpt.AHfnd Or
                  holdEmp#     <> dsLossRpt.AHemp# Or
                  holdPolYr    <> dsLossRpt.AHfYr Or
                  holdLocation <> dsLossRpt.CMctg1;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHagt#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WriteLocationTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                   ExSr WriteLocationInfo;
                When holdPolYr <> dsLossRpt.AHfyr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WriteLocationTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                   ExSr WriteLocationInfo;
                When holdLocation <> dsLossRpt.CMctg1;
                   If holdLocation <> ' ';
                      json += '],';
                      ExSr WriteLocationTotals;
                   EndIf;
                   ExSr WriteLocationInfo;
                EndSl;
                holdFnd      = dsLossRpt.AHfnd;
                holdEmp#     = dsLossRpt.AHemp#;
                holdPolYr    = dsLossRpt.AHfYr;
                holdLocation = dsLossRpt.CMctg1;
             EndSl;

             // Sum up Policy Information
             ExSr SumAmounts;

          When filter.sortOrder = 'Employer/Policy Year/Location/Department';

             Select;
             When holdFnd      <> dsLossRpt.AHfnd Or
                  holdEmp#     <> dsLossRpt.AHemp# Or
                  holdPolYr    <> dsLossRpt.AHfYr Or
                  holdLocation <> dsLossRpt.CMctg1 Or
                  holdDept     <> dsLossRpt.CMdept;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHagt#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WriteDepartmentTotals;
                      json += '],';
                      ExSr WriteLocationTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                   ExSr WriteLocationInfo;
                   ExSr WriteDepartmentInfo;
                When holdPolYr <> dsLossRpt.AHfyr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WriteDepartmentTotals;
                      json += '],';
                      ExSr WriteLocationTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                   ExSr WriteLocationInfo;
                   ExSr WriteDepartmentInfo;
                When holdLocation <> dsLossRpt.CMctg1;
                   If holdLocation <> ' ';
                      json += '],';
                      ExSr WriteDepartmentTotals;
                      json += '],';
                      ExSr WriteLocationTotals;
                   EndIf;
                   ExSr WriteLocationInfo;
                   ExSr WriteDepartmentInfo;
                When holdDept <> dsLossRpt.CMdept;
                   If holdDept <> ' ';
                      json += '],';
                      ExSr WriteDepartmentTotals;
                   EndIf;
                   ExSr WriteDepartmentInfo;
                EndSl;

                holdFnd        = dsLossRpt.AHfnd;
                holdEmp#       = dsLossRpt.AHemp#;
                holdPolYr      = dsLossRpt.AHfYr;
                holdLocation   = dsLossRpt.CMctg1;
                holdDept       = dsLossRpt.CMdept;

             EndSl;

             // Sum up Policy Information
             ExSr SumAmounts;

          EndSl;

       Endsr;

       //=================================================================
       // Build Loss Experience Report
       //=================================================================
       Begsr BldReportResults;

          // Set Sort Flag
          ExSr SetSortFlags;

          // Start Size Report
          json = %Trim(json) + ',"reportName":"Loss Experience Report",';

          // Build agency sql statement
          sqlAgtDate =
             'With max_Agt_Date as (Select AHco#, AHfnd, ' +
             'AHemp#, AHdiv, AHfyr, Max(AHefdt) maxDate ' +
             'From wmahp ' +
             'Where AHefdt <= ' + %EditC(DateTo7(SystemDate()):'X') + ' ' +
             'And AHstat <> ''I'' ' +
             'Group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr),';

          sqlAgtInfo =
             'Agt_Agt# as (Select a.*, b.AHagt# agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And ' +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt),';

          sqlAgtName =
             'Agt_Name as (Select a.*, c.AGname, c.AGfein, c.AGmod, ' +
             'd.FNname, d.FNmAd1, d.FNmAd2, d.FNmCty, d.FNmSt, d.FNmZip, ' +
             'd.FNpAd1, d.FNpAd2, d.FNpCty, d.FNpSt, d.FNpZip ' +
             'From Agt_Agt# a join WMAAP b ' +
             'on a.AHco#=b.AAco# And a.AHfnd=b.AAfnd And a.AHagt#=b.AAagt# ' +
             'join WMAGP c on b.AAfein=c.AGfein And b.AAmod=c.AGmod ' +
             'join WMFNP d on a.AHco#=d.FNco# And a.AHfnd=d.FNfnd),';

          sqlClmInfo =
             'Loss_Report as (Select a.*, ' +
             'cmrt01, Value(r1.rttype,'' '') rsvType01, cmrr01, cmar01, ' +
             'cmrt02, Value(r2.rttype,'' '') rsvType02, cmrr02, cmar02, ' +
             'cmrt03, Value(r3.rttype,'' '') rsvType03, cmrr03, cmar03, ' +
             'cmrt04, Value(r4.rttype,'' '') rsvType04, cmrr04, cmar04, ' +
             'cmrt05, Value(r5.rttype,'' '') rsvType05, cmrr05, cmar05, ' +
             'cmrt06, Value(r6.rttype,'' '') rsvType06, cmrr06, cmar06, ' +
             'cmrt07, Value(r7.rttype,'' '') rsvType07, cmrr07, cmar07, ' +
             'cmrt08, Value(r8.rttype,'' '') rsvType08, cmrr08, cmar08, ' +
             'cmrt09, Value(r9.rttype,'' '') rsvType09, cmrr09, cmar09, ' +
             'cmrt10, Value(r0.rttype,'' '') rsvType10, cmrr10, cmar10, ' +
             'b.cmltfl, b.cmlost, b.cmacdt, b.cmclas, b.cmacds, b.cmacd2, ' +
             'b.cmlsn, b.cmfsn, b.cmmid, ' +
             'b.CMcase, b.CMstat, b.CMctg1, c.EMoff#, ' +
             'Value(d.OFname,'' '') ofname, Value(d.OFadr1,'' ''), ' +
             'Value(d.OFadr2,'' ''), Value(d.OFcty,'' ''), ' +
             'Value(d.OFst,'' ''), Value(d.OFzip,0), ' +
             'Trim(c.EMname) || c.EMnam2 empName, ' +
             'c.EMmAd1, c.EMmAd2, c.EMmCty, c.EMmSt, c.EMmZip, ' +
             'c.EMpAd1, c.EMpAd2, c.EMpCty, c.EMpSt, c.EMpZip, ' +
             'e.ELpfdt, e.ELptdt, e.ELrtCd, rs.RSjrSt, c.EMfein, ' +
             ZSqlCxDate('b') + ',' +
             'c.EMcnty, Value(cn.CNdesc,'' ''), ' +
             'c.EMrgn#, Value(re.REdesc,'' ''), ' +
             'c.EMchap, Value(cp.CPdesc,'' ''), ' +
             'b.CMdept, Value(dp.DPdesc,'' ''), ' +
             'Value(al.ALname,'' ''), Value(al.ALmad1,'' ''),' +
             'Value(al.ALmad2,'' ''), Value(al.ALmCty,'' ''),' +
             'Value(al.ALmSt,'' ''), Value(al.ALmZip,0) ' +
             'From Agt_Name a ' +
             'join WMEMP c on c.EMco#=a.AHco# And ' +
             'c.EMfnd=a.AHfnd And c.EMemp#=a.AHemp# And ' +
             'c.EMdiv=a.AHdiv ' +
             'Left Outer Join WMOFP d on ' +
             'c.EMco#=d.OFco# And c.EMoff#=d.OFofc# ' +
             'join WDELP e on e.ELco#=a.AHco# And e.ELfnd=a.AHfnd And ' +
             'e.ELemp#=a.AHemp# And e.ELdiv=a.AHdiv And e.ELfyr=a.AHfyr ' +
             'left join WMCMP b on a.AHco#=b.CMco# And ' +
             'a.AHfnd=b.CMfnd And a.AHemp#=b.CMemp# And ' +
             'a.AHdiv=b.CMdiv and a.AHfyr=b.CMfyr ' +
             'left outer join wtrtp r1 on b.CMrt01 = r1.RTkey ' +
             'left outer join wtrtp r2 on b.CMrt02 = r2.RTkey ' +
             'left outer join wtrtp r3 on b.CMrt03 = r3.RTkey ' +
             'left outer join wtrtp r4 on b.CMrt04 = r4.RTkey ' +
             'left outer join wtrtp r5 on b.CMrt05 = r5.RTkey ' +
             'left outer join wtrtp r6 on b.CMrt06 = r6.RTkey ' +
             'left outer join wtrtp r7 on b.CMrt07 = r7.RTkey ' +
             'left outer join wtrtp r8 on b.CMrt08 = r8.RTkey ' +
             'left outer join wtrtp r9 on b.CMrt09 = r9.RTkey ' +
             'left outer join wtrtp r0 on b.CMrt10 = r0.RTkey ' +
             'join wmrsp rs on e.ELrtcd = rs.RSrtcd ' +
             'left outer join wmrep re on c.EMrgn# = re.RErgn# ' +
             'And c.EMfnd=re.REfnd ' +
             'left outer join wmcpp cp on c.EMchap = cp.CPchap ' +
             'And c.EMfnd=cp.CPfnd ' +
             'left outer join wtcnp cn on ' +
             'rs.RSjrst || c.EMcnty = cn.CNkey ' +
             'left outer join wmdpp dp on ' +
             'dp.DPco# = c.EMco# And  dp.DPfnd = c.EMfnd And ' +
             'dp.DPemp# = c.EMemp# And dp.DPdiv = c.EMdiv And ' +
             'dp.DPdept = b.CMdept ' +
             'left outer join wmalp al on ' +
             'al.ALco# = c.EMco# And  al.ALfnd = c.EMfnd And ' +
             'al.ALemp# = c.EMemp# And al.ALdiv = c.EMdiv And ' +
             'al.ALelId = b.CMctg1)';

          sqlRptQry = 'Select * From Loss_Report';

          sqlRptWhere = 'where';

          // From Policy Effective Date Filters
          If filter.fromPolEffDt = ' ';
             fromPolEf7 = 0;
          Endif;
          If filter.toPolEffDt = ' ' or filter.toPolEffDt = '00000000';
             toPolEf7 = 9999999;
          Endif;
          sqlRptWhere += ' ELpfDt between ' + %Trim(%EditC(fromPolEf7:'X')) +
           ' and ' + %Trim(%EditC(toPolEf7:'X')) + ' and';

          // From Policy Expiration Date Filters
          If filter.fromPolExpDt = ' ';
             fromPolEx7 = 0;
          Endif;
          If filter.toPolExpDt = ' ' or filter.toPolExpDt = '00000000';
             toPolEx7 = 9999999;
          Endif;
          sqlRptWhere += ' ELptDt between ' + %Trim(%EditC(fromPolEx7:'X')) +
           ' and ' + %Trim(%EditC(toPolEx7:'X')) + ' and';

          // Group # Filter;
          If filter.group# <> ' ';
             sqlRptWhere += ' AHfnd = ' + %Trim(filter.group#) + ' and';
          Else;
             // Group Name Filter;
             If filter.groupName <> ' ';
                sqlRptWhere += ' Upper(FNname) like ''%'' || Upper(''' +
                 %Trim(filter.groupName) + ''') || ''%'' and';
             EndIf;
          EndIf;

          // Office filters;
          Select;
          When filter.office# <> ' ';
             sqlRptWhere += ' EMoff# = ''' + filter.office# + ''' and';
          When filter.officeName <> ' ';
             sqlRptWhere += ' Upper(OFname) like ''%'' || Upper(''' +
              %Trim(filter.officeName) + ''') || ''%'' and';
          Endsl;

          // Agency Fein Filter
          If filter.agencyFein <> ' ';
             sqlRptWhere += ' AGfein=' + filter.agencyFein + ' and';
          EndIf;

          // Agency Modifier Filter
          If filter.agencyMod <> ' ';
             sqlRptWhere += ' AGmod=' + filter.agencyMod + ' and';
          EndIf;

          // Agency Name Filter
          If filter.agencyName <> ' ';
             sqlRptWhere += ' Upper(AGname) like ''%' +
              %Trim(Upper(filter.agencyName)) + '%'' and';
          EndIf;

          // Insured Filters
          Select;
          When filter.insuredFein <> ' ';
             sqlRptWhere += ' EMfein = ' + %Trim(filter.insuredFein) + ' and';
          When filter.insuredName <> ' ';
             sqlRptWhere += ' Upper(empName) like ''%'' || Upper(''' +
              %Trim(filter.insuredName) + ''') || ''%'' and ';
          Endsl;

          // Policy Status
          If filter.policyStatus <> *blank and filter.policyStatus <> 'All';
             sqlRptWhere += ZSqlWherePolicyStatus(filter.policyStatus) + ' and';
          Endif;

          sqlRptWhere = %scanrpl('and':'':sqlRptWhere:%len(sqlRptWhere)-3);

          // Establish sort Order
          Select;
          When filter.sortOrder = 'Employer/Policy Year';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr';
          When filter.sortOrder = 'Employer/Policy Year/Division';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, AHdiv';
          When filter.sortOrder = 'Employer/Policy Year/Department';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMdept';
          When filter.sortOrder = 'Employer/Policy Year/Location';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMctg1';
          When filter.sortOrder = 'Employer/Policy Year/Location/Department';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMctg1, CMdept';
          EndSl;

          sqlStmtStr = 'with' + ZSqlAgtDate + ', ' + %Trim(sqlAgtInfo) + ' ' +
                       ZSqlAgtName(*on) + ', ' +
                       %Trim(sqlClmInfo) + ' ' +
                       %Trim(sqlRptQry)  + ' ' + %Trim(sqlRptWhere) + ' ' +
                       %Trim(sqlRptOrder);

          // Prepare the SQL statement
          Exec SQL prepare rptStmt2 from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare rptCursor2 cursor for rptStmt2;

          // Run SQL
          Exec SQL open rptCursor2;

          // Get first record if there is one
          Exec SQL fetch next from rptCursor2 into :dsLossRpt :dsLossRptNull;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             ExSr FinishReport;
             moreRows = *off;
             Exec SQL close rptCursor2;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             // Did anything get picked up
             count += 1;

             // Build Report Record
             Exsr BldReportRcd;

             // Fetch next Record
             Exec SQL fetch next from rptCursor2 into :dsLossRpt :dsLossRptNull;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                ExSr FinishReport;
                moreRows = *Off;
                Exec SQL close rptCursor2;
             EndIf;
          EndDo;

       EndSr;

       //=================================================================
       // Build Request Information
       //=================================================================
       Begsr BldReqInfo;

          json = '"LossExperienceReport" : {' +
                 BuildJsonNode('groupName':filter.groupName) +
                 ', ' +
                 BuildJsonNode('group#':filter.group#)  +
                 ', ' +
                 BuildJsonNode('officeName':filter.officeName) +
                 ', ' +
                 BuildJsonNode('office#':filter.office#)  +
                 ', ' +
                 BuildJsonNode('agencyName':filter.agencyName) +
                 ', ' +
                 BuildJsonNode('agencyFein':filter.agencyFein) +
                 ', ' +
                 BuildJsonNode('agencyMod':filter.agencyMod) +
                 ', ' +
                 BuildJsonNode('insuredName':filter.insuredName) +
                 ', ' +
                 BuildJsonNode('insuredFein':filter.insuredFein) +
                 ', ' +
                 BuildJsonNode('policyStatus':filter.policyStatus) +
                 ', ' +
                 BuildJsonNode('fromPolicyEffDate':%editw(fromPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyEffDate':%editw(toPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('fromPolicyExpDate':%editw(fromPolEx8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyExpDate':%editw(toPolEx8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('sortOrder':filter.sortOrder);

       Endsr;

       //=================================================================
       // Build Return Information
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Loss Experience Reports Results
          ExSr BldReportResults;

       Endsr;

       //=================================================================
       // Finish The Report
       //=================================================================
       Begsr FinishReport;

          endReport = *On;
          If count > 0;

             Select;
             When filter.sortOrder = 'Employer/Policy Year';
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             When filter.sortOrder = 'Employer/Policy Year/Department';
                json += '],';
                ExSr WriteDepartmentTotals;
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             When filter.sortOrder = 'Employer/Policy Year/Division';
                json += '],';
                ExSr WriteDivisionTotals;
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             When filter.sortOrder = 'Employer/Policy Year/Location';
                json += '],';
                ExSr WriteLocationTotals;
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             When filter.sortOrder = 'Employer/Policy Year/Location/Department';
                json += '],';
                ExSr WriteDepartmentTotals;
                json += '],';
                ExSr WriteLocationTotals;
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             EndSl;
          EndIf;

          ExSr WriteRptTotals;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'groupName');
             filter.groupName   = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'group#');
             filter.group#      = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'officeName');
             filter.officeName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'office#');
             filter.office#     = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyName');
             filter.agencyName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyFein');
             filter.agencyFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyMod');
             filter.agencyMod  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredName');
             filter.insuredName = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredFein');
             filter.insuredFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'policyStatus');
             filter.policyStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyEffDate');
             filter.fromPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyEffDate');
             filter.toPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyExpDate');
             filter.fromPolExpDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyExpDate');
             filter.toPolExpDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'sortOrder');
             filter.sortOrder = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             //------------------------------------------------------
             // Office # must be valid              <<<<<<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.office# <> ' ';
                // If the office # valid?
                If ValidOffice(filter.office#) = *Off;
                   errMsg = %Trim(errMsg) + '"Invalid office#."             ';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Fein: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Fein: ' +
                                               'Invalid numeric data"';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             // Modifier
             If filter.agencyMod <> ' ';
                Monitor;
                   mod = %Dec(%Trim(filter.agencyMod):3:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Mod: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Mod: ' +
                                               'Invalid numeric data"';
                   EndIf;
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
                If errMsg = ' ';
                   errMsg = %Trim(errMsg) + '"Invalid agency Fein/Mod."';
                Else;
                   errMsg = %Trim(errMsg) + ',"Invalid agency Fein/Mod."';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Insured: Invalid numeric ' +
                                               'data."';
                   Else;
                      errMsg = %Trim(errMsg) + '",Insured: Invalid numeric '+
                                               'data."';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             //------------------------------------------------------
             // Validate Policy Status                 <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.policyStatus <> ' '
              And filter.policyStatus <> 'All'
              And filter.policyStatus <> 'Active'
              And filter.policyStatus <> 'Cancelled'
              And filter.policyStatus <> 'Expired'
              And filter.policyStatus <> 'Pending'
              And filter.policyStatus <> 'Pending Cancellation';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Policy Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Policy Status." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Effective date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolEffDt = ' ' Or filter.fromPolEffDt = '00000000';
                fromPolEf8     = 0;
                fromPolEf7     = 0;
                strFromPolEf7  = '0000000';
             Else;
                Monitor;
                   fromPolEf8 = %Dec(filter.fromPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Effective Date: Invalid Numeric Data. " ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"From Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEf8 <> 0;
                   fromPolEf7     = dtCYMD(fromPolEf8);
                   strfromPolEf7  = %EditC(fromPolEf7:'X');
                   If fromPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Policy Effective Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Effective Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Effective date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolEffDt = ' ' Or filter.toPolEffDt = '00000000';
                toPolEf8     = 0;
                toPolEf7     = 0;
                strToPolEf7  = '0000000';
             Else;
                Monitor;
                   toPolEf8 = %Dec(filter.toPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Effective Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEf8 <> 0;
                   toPolEf7     = dtCYMD(toPolEf8);
                   strToPolEf7  = %EditC(toPolEf7:'X');
                   If toPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Policy Effective Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Effective Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Expiration date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolExpDt = ' ' Or filter.fromPolExpDt = '00000000';
                fromPolEx8     = 0;
                fromPolEx7     = 0;
                strFromPolEx7  = '0000000';
             Else;
                Monitor;
                   fromPolEx8 = %Dec(filter.fromPolExpDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Expiration Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                       ',"From Policy Expiration Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEx8 <> 0;
                   fromPolEx7     = dtCYMD(fromPolEx8);
                   strfromPolEx7  = %EditC(fromPolEx7:'X');
                   If fromPolEx7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                            '"From Policy Expiration Date: Invalid Date. " ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Expiration Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Expiration date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolExpDt = ' ' Or filter.toPolExpDt = '00000000';
                toPolEx8     = 0;
                toPolEx7     = 0;
                strToPolEx7  = '0000000';
             Else;
                Monitor;
                   toPolEx8 = %Dec(filter.toPolExpDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Expiration Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"To Policy Expiration Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEx8 <> 0;
                   toPolEx7     = dtCYMD(toPolEx8);
                   strToPolEx7  = %EditC(toPolEx7:'X');
                   If toPolEx7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                           '"To Policy Expiration Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Expiration Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Sort Order                    <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.sortOrder <> 'Employer/Policy Year' And
                filter.sortOrder <> 'Employer/Policy Year/Division' And
                filter.sortOrder <> 'Employer/Policy Year/Department' And
                filter.sortOrder <> 'Employer/Policy Year/Location' And
                filter.sortOrder <> 'Employer/Policy Year/Location/Department';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Sort Order."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Sort Order." ';
                   EndIf;
             EndIf;

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

       //=================================================================
       // Set Sort Flags
       //=================================================================
       Begsr SetSortFlags;

          locationSort   = *Off;
          divisionSort   = *Off;
          departmentSort = *Off;

          Select;
          When filter.sortOrder = 'Employer/Policy Year/Location';
             locationSort   = *On;
          When filter.sortOrder = 'Employer/Policy Year/Division';
             divisionSort   = *On;
          When filter.sortOrder = 'Employer/Policy Year/Department';
             departmentSort = *On;
          When filter.sortOrder = 'Employer/Policy Year/Location/Department';
             departmentSort = *On;
             locationSort   = *On;
          EndSl;

       Endsr;

       //=================================================================
       // Sum Policy Info
       //=================================================================
       Begsr SumAmounts;

          //===================================================
          // Paid This Period
          //===================================================
          inFromDate = DateTo7(GetFirstDay(systemDate()));
          inToDate   = DateTo7(GetLastDay(systemDate()));
          outMedical = 0;
          outComp    = 0;
          outExpense = 0;
          outLegal   = 0;
          paidMedical = 0;
          paidComp    = 0;
          paidExpense = 0;
          paidLegal   = 0;
          rsvMedical = 0;
          rsvComp    = 0;
          rsvExpense = 0;
          rsvLegal   = 0;
          incMedical = 0;
          incComp    = 0;
          incExpense = 0;
          incLegal   = 0;

          GetPaidThisPeriod(dsLossRpt.AHco#:dsLossRpt.AHfnd:
             dsLossRpt.AHfYr:dsLossRpt.CMcase:inFromDate:inToDate:
             outMedical:outComp:outExpense:outLegal);

          ptpLocCmp += outComp;
          ptpLocTot += outComp;
          ptpDivCmp += outComp;
          ptpDivTot += outComp;
          ptpDptCmp += outComp;
          ptpDptTot += outComp;
          ptpPolCmp += outComp;
          ptpPolTot += outComp;
          ptpInsCmp += outComp;
          ptpInsTot += outComp;
          ptpTotCmp += outComp;
          ptpTotTot += outComp;

          ptpLocMed += outMedical;
          ptpLocTot += outMedical;
          ptpDivMed += outMedical;
          ptpDivTot += outMedical;
          ptpDptMed += outMedical;
          ptpDptTot += outMedical;
          ptpPolMed += outMedical;
          ptpPolTot += outMedical;
          ptpInsMed += outMedical;
          ptpInsTot += outMedical;
          ptpTotMed += outMedical;
          ptpTotTot += outMedical;

          ptpLocExp += outExpense;
          ptpLocTot += outExpense;
          ptpDivExp += outExpense;
          ptpDivTot += outExpense;
          ptpDptExp += outExpense;
          ptpDptTot += outExpense;
          ptpPolExp += outExpense;
          ptpPolTot += outExpense;
          ptpInsExp += outExpense;
          ptpInsTot += outExpense;
          ptpTotExp += outExpense;
          ptpTotTot += outExpense;

          ptpLocCmp += outLegal;
          ptpLocTot += outLegal;
          ptpDivCmp += outLegal;
          ptpDivTot += outLegal;
          ptpDptCmp += outLegal;
          ptpDptTot += outLegal;
          ptpPolCmp += outLegal;
          ptpPolTot += outLegal;
          ptpInsCmp += outLegal;
          ptpInsTot += outLegal;
          ptpTotCmp += outLegal;
          ptpTotTot += outLegal;

          //===================================================
          // Compensation amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar01;
             paidLocCmp += dsLossRpt.CMar01;
             paidLocTot += dsLossRpt.CMar01;
             paidDivCmp += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidDptCmp += dsLossRpt.CMar01;
             paidDptTot += dsLossRpt.CMar01;
             paidPolCmp += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsCmp += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotCmp += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr01;
             outsLocCmp += dsLossRpt.CMrr01;
             outsLocTot += dsLossRpt.CMrr01;
             outsDivCmp += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsDptCmp += dsLossRpt.CMrr01;
             outsDptTot += dsLossRpt.CMrr01;
             outsPolCmp += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsCmp += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotCmp += dsLossRpt.CMrr01;
             outsTotTot += dsLossRpt.CMrr01;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar02;
             paidLocCmp += dsLossRpt.CMar02;
             paidLocTot += dsLossRpt.CMar02;
             paidDivCmp += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidDptCmp += dsLossRpt.CMar02;
             paidDptTot += dsLossRpt.CMar02;
             paidPolCmp += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsCmp += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotCmp += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr02;
             outsLocCmp += dsLossRpt.CMrr02;
             outsLocTot += dsLossRpt.CMrr02;
             outsDivCmp += dsLossRpt.CMrr02;
             outsDptTot += dsLossRpt.CMrr02;
             outsDptCmp += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsPolCmp += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsCmp += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotCmp += dsLossRpt.CMrr02;
             outsTotTot += dsLossRpt.CMrr02;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar03;
             paidLocCmp += dsLossRpt.CMar03;
             paidLocTot += dsLossRpt.CMar03;
             paidDivCmp += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidDptCmp += dsLossRpt.CMar03;
             paidDptTot += dsLossRpt.CMar03;
             paidPolCmp += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsCmp += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotCmp += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr03;
             outsLocCmp += dsLossRpt.CMrr03;
             outsLocTot += dsLossRpt.CMrr03;
             outsDivCmp += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsDptCmp += dsLossRpt.CMrr03;
             outsDptTot += dsLossRpt.CMrr03;
             outsPolCmp += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsCmp += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotCmp += dsLossRpt.CMrr03;
             outsTotTot += dsLossRpt.CMrr03;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar04;
             paidLocCmp += dsLossRpt.CMar04;
             paidLocTot += dsLossRpt.CMar04;
             paidDivCmp += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidDptCmp += dsLossRpt.CMar04;
             paidDptTot += dsLossRpt.CMar04;
             paidPolCmp += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsCmp += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotCmp += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr04;
             outsLocCmp += dsLossRpt.CMrr04;
             outsLocTot += dsLossRpt.CMrr04;
             outsDivCmp += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsDptCmp += dsLossRpt.CMrr04;
             outsDptTot += dsLossRpt.CMrr04;
             outsPolCmp += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsCmp += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotCmp += dsLossRpt.CMrr04;
             outsTotTot += dsLossRpt.CMrr04;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar05;
             paidLocCmp += dsLossRpt.CMar05;
             paidLocTot += dsLossRpt.CMar05;
             paidDivCmp += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidDptCmp += dsLossRpt.CMar05;
             paidDptTot += dsLossRpt.CMar05;
             paidPolCmp += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsCmp += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotCmp += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr05;
             outsLocCmp += dsLossRpt.CMrr05;
             outsLocTot += dsLossRpt.CMrr05;
             outsDivCmp += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsDptCmp += dsLossRpt.CMrr05;
             outsDptTot += dsLossRpt.CMrr05;
             outsPolCmp += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsCmp += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotCmp += dsLossRpt.CMrr05;
             outsTotTot += dsLossRpt.CMrr05;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar06;
             paidLocCmp += dsLossRpt.CMar06;
             paidLocTot += dsLossRpt.CMar06;
             paidDivCmp += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidDptCmp += dsLossRpt.CMar06;
             paidDptTot += dsLossRpt.CMar06;
             paidPolCmp += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsCmp += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotCmp += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr06;
             outsLocCmp += dsLossRpt.CMrr06;
             outsLocTot += dsLossRpt.CMrr06;
             outsDivCmp += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsDptCmp += dsLossRpt.CMrr06;
             outsDptTot += dsLossRpt.CMrr06;
             outsPolCmp += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsCmp += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotCmp += dsLossRpt.CMrr06;
             outsTotTot += dsLossRpt.CMrr06;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar07;
             paidLocCmp += dsLossRpt.CMar07;
             paidLocTot += dsLossRpt.CMar07;
             paidDivCmp += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidDptCmp += dsLossRpt.CMar07;
             paidDptTot += dsLossRpt.CMar07;
             paidPolCmp += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsCmp += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotCmp += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr07;
             outsLocCmp += dsLossRpt.CMrr07;
             outsLocTot += dsLossRpt.CMrr07;
             outsDivCmp += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsDptCmp += dsLossRpt.CMrr07;
             outsDptTot += dsLossRpt.CMrr07;
             outsPolCmp += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsCmp += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotCmp += dsLossRpt.CMrr07;
             outsTotTot += dsLossRpt.CMrr07;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar08;
             paidLocCmp += dsLossRpt.CMar08;
             paidLocTot += dsLossRpt.CMar08;
             paidDivCmp += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidDptCmp += dsLossRpt.CMar08;
             paidDptTot += dsLossRpt.CMar08;
             paidPolCmp += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsCmp += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotCmp += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr08;
             outsLocCmp += dsLossRpt.CMrr08;
             outsLocTot += dsLossRpt.CMrr08;
             outsDivCmp += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsDptCmp += dsLossRpt.CMrr08;
             outsDptTot += dsLossRpt.CMrr08;
             outsPolCmp += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsCmp += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotCmp += dsLossRpt.CMrr08;
             outsTotTot += dsLossRpt.CMrr08;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar09;
             paidLocCmp += dsLossRpt.CMar09;
             paidLocTot += dsLossRpt.CMar09;
             paidDivCmp += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidDptCmp += dsLossRpt.CMar09;
             paidDptTot += dsLossRpt.CMar09;
             paidPolCmp += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsCmp += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotCmp += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr09;
             outsLocCmp += dsLossRpt.CMrr09;
             outsLocTot += dsLossRpt.CMrr09;
             outsDivCmp += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsDptCmp += dsLossRpt.CMrr09;
             outsDptTot += dsLossRpt.CMrr09;
             outsPolCmp += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsCmp += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotCmp += dsLossRpt.CMrr09;
             outsTotTot += dsLossRpt.CMrr09;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar10;
             paidLocCmp += dsLossRpt.CMar10;
             paidLocTot += dsLossRpt.CMar10;
             paidDivCmp += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidDptCmp += dsLossRpt.CMar10;
             paidDptTot += dsLossRpt.CMar10;
             paidPolCmp += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsCmp += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotCmp += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr10;
             outsLocCmp += dsLossRpt.CMrr10;
             outsLocTot += dsLossRpt.CMrr10;
             outsDivCmp += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsDptCmp += dsLossRpt.CMrr10;
             outsDptTot += dsLossRpt.CMrr10;
             outsPolCmp += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsCmp += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotCmp += dsLossRpt.CMrr10;
             outsTotTot += dsLossRpt.CMrr10;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Medical amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar01;
             paidLocMed += dsLossRpt.CMar01;
             paidLocTot += dsLossRpt.CMar01;
             paidDivMed += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidDptMed += dsLossRpt.CMar01;
             paidDptTot += dsLossRpt.CMar01;
             paidPolMed += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsMed += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotMed += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr01;
             outsLocMed += dsLossRpt.CMrr01;
             outsLocTot += dsLossRpt.CMrr01;
             outsDivMed += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsDptMed += dsLossRpt.CMrr01;
             outsDptTot += dsLossRpt.CMrr01;
             outsPolMed += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsMed += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotMed += dsLossRpt.CMrr01;
             outsTotTot += dsLossRpt.CMrr01;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar02;
             paidLocMed += dsLossRpt.CMar02;
             paidLocTot += dsLossRpt.CMar02;
             paidDivMed += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidDptMed += dsLossRpt.CMar02;
             paidDptTot += dsLossRpt.CMar02;
             paidPolMed += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsMed += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotMed += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr02;
             outsLocMed += dsLossRpt.CMrr02;
             outsLocTot += dsLossRpt.CMrr02;
             outsDivMed += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsDptMed += dsLossRpt.CMrr02;
             outsDptTot += dsLossRpt.CMrr02;
             outsPolMed += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsMed += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotMed += dsLossRpt.CMrr02;
             outsTotTot += dsLossRpt.CMrr02;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar03;
             paidLocMed += dsLossRpt.CMar03;
             paidLocTot += dsLossRpt.CMar03;
             paidDivMed += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidDptMed += dsLossRpt.CMar03;
             paidDptTot += dsLossRpt.CMar03;
             paidPolMed += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsMed += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotMed += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr03;
             outsLocMed += dsLossRpt.CMrr03;
             outsLocTot += dsLossRpt.CMrr03;
             outsDivMed += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsDptMed += dsLossRpt.CMrr03;
             outsDptTot += dsLossRpt.CMrr03;
             outsPolMed += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsMed += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotMed += dsLossRpt.CMrr03;
             outsTotTot += dsLossRpt.CMrr03;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar04;
             paidLocMed += dsLossRpt.CMar04;
             paidLocTot += dsLossRpt.CMar04;
             paidDivMed += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidDptMed += dsLossRpt.CMar04;
             paidDptTot += dsLossRpt.CMar04;
             paidPolMed += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsMed += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotMed += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr04;
             outsLocMed += dsLossRpt.CMrr04;
             outsLocTot += dsLossRpt.CMrr04;
             outsDivMed += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsDptMed += dsLossRpt.CMrr04;
             outsDptTot += dsLossRpt.CMrr04;
             outsPolMed += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsMed += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotMed += dsLossRpt.CMrr04;
             outsTotTot += dsLossRpt.CMrr04;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar05;
             paidLocMed += dsLossRpt.CMar05;
             paidLocTot += dsLossRpt.CMar05;
             paidDivMed += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidDptMed += dsLossRpt.CMar05;
             paidDptTot += dsLossRpt.CMar05;
             paidPolMed += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsMed += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotMed += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr05;
             outsLocMed += dsLossRpt.CMrr05;
             outsLocTot += dsLossRpt.CMrr05;
             outsDivMed += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsDptMed += dsLossRpt.CMrr05;
             outsDptTot += dsLossRpt.CMrr05;
             outsPolMed += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsMed += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotMed += dsLossRpt.CMrr05;
             outsTotTot += dsLossRpt.CMrr05;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar06;
             paidLocMed += dsLossRpt.CMar06;
             paidLocTot += dsLossRpt.CMar06;
             paidDivMed += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidDptMed += dsLossRpt.CMar06;
             paidDptTot += dsLossRpt.CMar06;
             paidPolMed += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsMed += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotMed += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr06;
             outsLocMed += dsLossRpt.CMrr06;
             outsLocTot += dsLossRpt.CMrr06;
             outsDivMed += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsDptMed += dsLossRpt.CMrr06;
             outsDptTot += dsLossRpt.CMrr06;
             outsPolMed += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsMed += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotMed += dsLossRpt.CMrr06;
             outsTotTot += dsLossRpt.CMrr06;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar07;
             paidLocMed += dsLossRpt.CMar07;
             paidLocTot += dsLossRpt.CMar07;
             paidDivMed += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidDptMed += dsLossRpt.CMar07;
             paidDptTot += dsLossRpt.CMar07;
             paidPolMed += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsMed += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotMed += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr07;
             outsLocMed += dsLossRpt.CMrr07;
             outsLocTot += dsLossRpt.CMrr07;
             outsDivMed += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsDptMed += dsLossRpt.CMrr07;
             outsDptTot += dsLossRpt.CMrr07;
             outsPolMed += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsMed += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotMed += dsLossRpt.CMrr07;
             outsTotTot += dsLossRpt.CMrr07;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar08;
             paidLocMed += dsLossRpt.CMar08;
             paidLocTot += dsLossRpt.CMar08;
             paidDivMed += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidDptMed += dsLossRpt.CMar08;
             paidDptTot += dsLossRpt.CMar08;
             paidPolMed += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsMed += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotMed += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr08;
             outsLocMed += dsLossRpt.CMrr08;
             outsLocTot += dsLossRpt.CMrr08;
             outsDivMed += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsDptMed += dsLossRpt.CMrr08;
             outsDptTot += dsLossRpt.CMrr08;
             outsPolMed += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsMed += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotMed += dsLossRpt.CMrr08;
             outsTotTot += dsLossRpt.CMrr08;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar09;
             paidLocMed += dsLossRpt.CMar09;
             paidLocTot += dsLossRpt.CMar09;
             paidDivMed += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidDptMed += dsLossRpt.CMar09;
             paidDptTot += dsLossRpt.CMar09;
             paidPolMed += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsMed += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotMed += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr09;
             outsLocMed += dsLossRpt.CMrr09;
             outsLocTot += dsLossRpt.CMrr09;
             outsDivMed += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsDptMed += dsLossRpt.CMrr09;
             outsDptTot += dsLossRpt.CMrr09;
             outsPolMed += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsMed += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotMed += dsLossRpt.CMrr09;
             outsTotTot += dsLossRpt.CMrr09;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotMed += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar10;
             paidLocMed += dsLossRpt.CMar10;
             paidLocTot += dsLossRpt.CMar10;
             paidDivMed += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidDptMed += dsLossRpt.CMar10;
             paidDptTot += dsLossRpt.CMar10;
             paidPolMed += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsMed += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotMed += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr10;
             outsLocMed += dsLossRpt.CMrr10;
             outsLocTot += dsLossRpt.CMrr10;
             outsDivMed += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsDptMed += dsLossRpt.CMrr10;
             outsDptTot += dsLossRpt.CMrr10;
             outsPolMed += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsMed += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotMed += dsLossRpt.CMrr10;
             outsTotTot += dsLossRpt.CMrr10;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Expense amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar01;
             paidLocExp += dsLossRpt.CMar01;
             paidLocTot += dsLossRpt.CMar01;
             paidDivExp += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidDptExp += dsLossRpt.CMar01;
             paidDptTot += dsLossRpt.CMar01;
             paidPolExp += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsExp += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotExp += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr01;
             outsLocExp += dsLossRpt.CMrr01;
             outsLocTot += dsLossRpt.CMrr01;
             outsDivExp += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsDptExp += dsLossRpt.CMrr01;
             outsDptTot += dsLossRpt.CMrr01;
             outsPolExp += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsExp += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotExp += dsLossRpt.CMrr01;
             outsTotTot += dsLossRpt.CMrr01;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar02;
             paidLocExp += dsLossRpt.CMar02;
             paidLocTot += dsLossRpt.CMar02;
             paidDivExp += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidDptExp += dsLossRpt.CMar02;
             paidDptTot += dsLossRpt.CMar02;
             paidPolExp += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsExp += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotExp += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr02;
             outsLocExp += dsLossRpt.CMrr02;
             outsLocTot += dsLossRpt.CMrr02;
             outsDivExp += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsDptExp += dsLossRpt.CMrr02;
             outsDptTot += dsLossRpt.CMrr02;
             outsPolExp += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsExp += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotExp += dsLossRpt.CMrr02;
             outsTotTot += dsLossRpt.CMrr02;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar03;
             paidLocExp += dsLossRpt.CMar03;
             paidLocTot += dsLossRpt.CMar03;
             paidDivExp += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidDptExp += dsLossRpt.CMar03;
             paidDptTot += dsLossRpt.CMar03;
             paidPolExp += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsExp += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotExp += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr03;
             outsLocExp += dsLossRpt.CMrr03;
             outsLocTot += dsLossRpt.CMrr03;
             outsDivExp += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsDptExp += dsLossRpt.CMrr03;
             outsDptTot += dsLossRpt.CMrr03;
             outsPolExp += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsExp += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotExp += dsLossRpt.CMrr03;
             outsTotTot += dsLossRpt.CMrr03;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar04;
             paidLocExp += dsLossRpt.CMar04;
             paidLocTot += dsLossRpt.CMar04;
             paidDivExp += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidDptExp += dsLossRpt.CMar04;
             paidDptTot += dsLossRpt.CMar04;
             paidPolExp += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsExp += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotExp += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr04;
             outsLocExp += dsLossRpt.CMrr04;
             outsLocTot += dsLossRpt.CMrr04;
             outsDivExp += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsDptExp += dsLossRpt.CMrr04;
             outsDptTot += dsLossRpt.CMrr04;
             outsPolExp += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsExp += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotExp += dsLossRpt.CMrr04;
             outsTotTot += dsLossRpt.CMrr04;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar05;
             paidLocExp += dsLossRpt.CMar05;
             paidLocTot += dsLossRpt.CMar05;
             paidDivExp += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidDptExp += dsLossRpt.CMar05;
             paidDptTot += dsLossRpt.CMar05;
             paidPolExp += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsExp += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotExp += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr05;
             outsLocExp += dsLossRpt.CMrr05;
             outsLocTot += dsLossRpt.CMrr05;
             outsDivExp += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsDptExp += dsLossRpt.CMrr05;
             outsDptTot += dsLossRpt.CMrr05;
             outsPolExp += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsExp += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotExp += dsLossRpt.CMrr05;
             outsTotTot += dsLossRpt.CMrr05;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar06;
             paidLocExp += dsLossRpt.CMar06;
             paidLocTot += dsLossRpt.CMar06;
             paidDivExp += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidDptExp += dsLossRpt.CMar06;
             paidDptTot += dsLossRpt.CMar06;
             paidPolExp += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsExp += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotExp += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr06;
             outsLocExp += dsLossRpt.CMrr06;
             outsLocTot += dsLossRpt.CMrr06;
             outsDivExp += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsDptExp += dsLossRpt.CMrr06;
             outsDptTot += dsLossRpt.CMrr06;
             outsPolExp += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsExp += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotExp += dsLossRpt.CMrr06;
             outsTotTot += dsLossRpt.CMrr06;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar07;
             paidLocExp += dsLossRpt.CMar07;
             paidLocTot += dsLossRpt.CMar07;
             paidDivExp += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidDptExp += dsLossRpt.CMar07;
             paidDptTot += dsLossRpt.CMar07;
             paidPolExp += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsExp += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotExp += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr07;
             outsLocExp += dsLossRpt.CMrr07;
             outsLocTot += dsLossRpt.CMrr07;
             outsDivExp += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsDptExp += dsLossRpt.CMrr07;
             outsDptTot += dsLossRpt.CMrr07;
             outsPolExp += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsExp += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotExp += dsLossRpt.CMrr07;
             outsTotTot += dsLossRpt.CMrr07;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar08;
             paidLocExp += dsLossRpt.CMar08;
             paidLocTot += dsLossRpt.CMar08;
             paidDivExp += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidDptExp += dsLossRpt.CMar08;
             paidDptTot += dsLossRpt.CMar08;
             paidPolExp += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsExp += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotExp += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr08;
             outsLocExp += dsLossRpt.CMrr08;
             outsLocTot += dsLossRpt.CMrr08;
             outsDivExp += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsDptExp += dsLossRpt.CMrr08;
             outsDptTot += dsLossRpt.CMrr08;
             outsPolExp += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsExp += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotExp += dsLossRpt.CMrr08;
             outsTotTot += dsLossRpt.CMrr08;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar09;
             paidLocExp += dsLossRpt.CMar09;
             paidLocTot += dsLossRpt.CMar09;
             paidDivExp += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidDptExp += dsLossRpt.CMar09;
             paidDptTot += dsLossRpt.CMar09;
             paidPolExp += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsExp += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotExp += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr09;
             outsLocExp += dsLossRpt.CMrr09;
             outsLocTot += dsLossRpt.CMrr09;
             outsDivExp += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsDptExp += dsLossRpt.CMrr09;
             outsDptTot += dsLossRpt.CMrr09;
             outsPolExp += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsExp += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotExp += dsLossRpt.CMrr09;
             outsTotTot += dsLossRpt.CMrr09;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotExp += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar10;
             paidLocExp += dsLossRpt.CMar10;
             paidLocTot += dsLossRpt.CMar10;
             paidDivExp += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidDptExp += dsLossRpt.CMar10;
             paidDptTot += dsLossRpt.CMar10;
             paidPolExp += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsExp += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotExp += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr10;
             outsLocExp += dsLossRpt.CMrr10;
             outsLocTot += dsLossRpt.CMrr10;
             outsDivExp += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsDptExp += dsLossRpt.CMrr10;
             outsDptTot += dsLossRpt.CMrr10;
             outsPolExp += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsExp += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotExp += dsLossRpt.CMrr10;
             outsTotTot += dsLossRpt.CMrr10;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Legal amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar01;
             paidLocLeg += dsLossRpt.CMar01;
             paidLocTot += dsLossRpt.CMar01;
             paidDivLeg += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidDptLeg += dsLossRpt.CMar01;
             paidDptTot += dsLossRpt.CMar01;
             paidPolLeg += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsLeg += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotLeg += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr01;
             outsLocLeg += dsLossRpt.CMrr01;
             outsLocTot += dsLossRpt.CMrr01;
             outsDivLeg += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsDptLeg += dsLossRpt.CMrr01;
             outsDptTot += dsLossRpt.CMrr01;
             outsPolLeg += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsLeg += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotLeg += dsLossRpt.CMrr01;
             outsTotTot += dsLossRpt.CMrr01;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincLocTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDptTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar02;
             paidLocLeg += dsLossRpt.CMar02;
             paidLocTot += dsLossRpt.CMar02;
             paidDivLeg += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidDptLeg += dsLossRpt.CMar02;
             paidDptTot += dsLossRpt.CMar02;
             paidPolLeg += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsLeg += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotLeg += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr02;
             outsLocLeg += dsLossRpt.CMrr02;
             outsLocTot += dsLossRpt.CMrr02;
             outsDivLeg += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsDptLeg += dsLossRpt.CMrr02;
             outsDptTot += dsLossRpt.CMrr02;
             outsPolLeg += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsLeg += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotLeg += dsLossRpt.CMrr02;
             outsTotTot += dsLossRpt.CMrr02;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincLocTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDptTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar03;
             paidLocLeg += dsLossRpt.CMar03;
             paidLocTot += dsLossRpt.CMar03;
             paidDivLeg += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidDptLeg += dsLossRpt.CMar03;
             paidDptTot += dsLossRpt.CMar03;
             paidPolLeg += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsLeg += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotLeg += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr03;
             outsLocLeg += dsLossRpt.CMrr03;
             outsLocTot += dsLossRpt.CMrr03;
             outsDivLeg += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsDptLeg += dsLossRpt.CMrr03;
             outsDptTot += dsLossRpt.CMrr03;
             outsPolLeg += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsLeg += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotLeg += dsLossRpt.CMrr03;
             outsTotTot += dsLossRpt.CMrr03;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincLocTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDptTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar04;
             paidLocLeg += dsLossRpt.CMar04;
             paidLocTot += dsLossRpt.CMar04;
             paidDivLeg += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidDptLeg += dsLossRpt.CMar04;
             paidDptTot += dsLossRpt.CMar04;
             paidPolLeg += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsLeg += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotLeg += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr04;
             outsLocLeg += dsLossRpt.CMrr04;
             outsLocTot += dsLossRpt.CMrr04;
             outsDivLeg += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsDptLeg += dsLossRpt.CMrr04;
             outsDptTot += dsLossRpt.CMrr04;
             outsPolLeg += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsLeg += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotLeg += dsLossRpt.CMrr04;
             outsTotTot += dsLossRpt.CMrr04;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincLocTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDptTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar05;
             paidLocLeg += dsLossRpt.CMar05;
             paidLocTot += dsLossRpt.CMar05;
             paidDivLeg += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidDptLeg += dsLossRpt.CMar05;
             paidDptTot += dsLossRpt.CMar05;
             paidPolLeg += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsLeg += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotLeg += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr05;
             outsLocLeg += dsLossRpt.CMrr05;
             outsLocTot += dsLossRpt.CMrr05;
             outsDivLeg += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsDptLeg += dsLossRpt.CMrr05;
             outsDptTot += dsLossRpt.CMrr05;
             outsPolLeg += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsLeg += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotLeg += dsLossRpt.CMrr05;
             outsTotTot += dsLossRpt.CMrr05;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincLocTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDptTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar06;
             paidLocLeg += dsLossRpt.CMar06;
             paidLocTot += dsLossRpt.CMar06;
             paidDivLeg += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidDptLeg += dsLossRpt.CMar06;
             paidDptTot += dsLossRpt.CMar06;
             paidPolLeg += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsLeg += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotLeg += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr06;
             outsLocLeg += dsLossRpt.CMrr06;
             outsLocTot += dsLossRpt.CMrr06;
             outsDivLeg += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsDptLeg += dsLossRpt.CMrr06;
             outsDptTot += dsLossRpt.CMrr06;
             outsPolLeg += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsLeg += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotLeg += dsLossRpt.CMrr06;
             outsTotTot += dsLossRpt.CMrr06;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincLocTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDptTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar07;
             paidLocLeg += dsLossRpt.CMar07;
             paidLocTot += dsLossRpt.CMar07;
             paidDivLeg += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidDptLeg += dsLossRpt.CMar07;
             paidDptTot += dsLossRpt.CMar07;
             paidPolLeg += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsLeg += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotLeg += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr07;
             outsLocLeg += dsLossRpt.CMrr07;
             outsLocTot += dsLossRpt.CMrr07;
             outsDivLeg += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsDptLeg += dsLossRpt.CMrr07;
             outsDptTot += dsLossRpt.CMrr07;
             outsPolLeg += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsLeg += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotLeg += dsLossRpt.CMrr07;
             outsTotTot += dsLossRpt.CMrr07;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincLocTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDptTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar08;
             paidLocLeg += dsLossRpt.CMar08;
             paidLocTot += dsLossRpt.CMar08;
             paidDivLeg += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidDptLeg += dsLossRpt.CMar08;
             paidDptTot += dsLossRpt.CMar08;
             paidPolLeg += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsLeg += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotLeg += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr08;
             outsLocLeg += dsLossRpt.CMrr08;
             outsLocTot += dsLossRpt.CMrr08;
             outsDivLeg += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsDptLeg += dsLossRpt.CMrr08;
             outsDptTot += dsLossRpt.CMrr08;
             outsPolLeg += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsLeg += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotLeg += dsLossRpt.CMrr08;
             outsTotTot += dsLossRpt.CMrr08;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincLocTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDptTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar09;
             paidLocLeg += dsLossRpt.CMar09;
             paidLocTot += dsLossRpt.CMar09;
             paidDivLeg += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidDptLeg += dsLossRpt.CMar09;
             paidDptTot += dsLossRpt.CMar09;
             paidPolLeg += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsLeg += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotLeg += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr09;
             outsLocLeg += dsLossRpt.CMrr09;
             outsLocTot += dsLossRpt.CMrr09;
             outsDivLeg += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsDptLeg += dsLossRpt.CMrr09;
             outsDptTot += dsLossRpt.CMrr09;
             outsPolLeg += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsLeg += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotLeg += dsLossRpt.CMrr09;
             outsTotTot += dsLossRpt.CMrr09;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincLocTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDptTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar10;
             paidLocLeg += dsLossRpt.CMar10;
             paidLocTot += dsLossRpt.CMar10;
             paidDivLeg += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidDptLeg += dsLossRpt.CMar10;
             paidDptTot += dsLossRpt.CMar10;
             paidPolLeg += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsLeg += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotLeg += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr10;
             outsLocLeg += dsLossRpt.CMrr10;
             outsLocTot += dsLossRpt.CMrr10;
             outsDivLeg += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsDptLeg += dsLossRpt.CMrr10;
             outsDptTot += dsLossRpt.CMrr10;
             outsPolLeg += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsLeg += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotLeg += dsLossRpt.CMrr10;
             outsTotTot += dsLossRpt.CMrr10;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincLocTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDptTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

         //* Output the claim detail information.
          Select;
          When %scan('"ClaimSort":[':json:%len(json)-14) <> 0;
          When clmFirst;
             clmFirst = *Off;
             json += '"ClaimSort":[';
          When dsLossRpt.CMcase = *zero;
             Leavesr;
          Other;
             json += ',';
          Endsl;

          If dsLossRpt.CMcase = *zero;
             Leavesr;
          Endif;

          Clear clmStatus;
          Clear clmPayDesc;
          Clear clmLostTime;
          Clear clmDayOf;
          Clear clmClosedDate;

          Select;
          When dsLossRpt.CMltfl = 'I';
             clmLostTime = 'Indemnity';
          When dsLossRpt.CMltfl = 'M';
             clmLostTime = 'Medical';
          When dsLossRpt.CMltfl = 'R';
             clmLostTime = 'Record Only';
          Endsl;

          clmDayOf = dowShort(dtMDYY(dsLossRpt.cmacdt));

          SetGt (dsLossRpt.AHCo#:dsLossRpt.ELrtcd:dsLossRpt.CMclas:
           dsLossRpt.ELpfdt) ppr01;
          Readpe (dsLossRpt.AHCo#:dsLossRpt.ELrtcd:dsLossRpt.CMclas)
           ppr01;
          If not %eof;
             clmPayDesc = %trim(PRDesc);
          Endif;

          Select;
          When dsLossRpt.CMstat = 'C';
             clmStatus = 'Closed';
             SetGt (dsLossRpt.AHCo#:dsLossRpt.AHFnd:dsLossRpt.AHFyr:
              dsLossRpt.CMCase:DateTo7(SystemDate())) pch00;
             Readpe (dsLossRpt.AHCo#:dsLossRpt.AHFnd:dsLossRpt.AHFyr:
              dsLossRpt.CMCase) pch00;
             If not %eof;
                clmClosedDate = %editw(dtMDYY(CHEfdt):'  /  /    ');
             Endif;
          When dsLossRpt.CMstat = 'O';
             clmStatus = 'Open';
          Endsl;


          //*** Beg Add *** 02/14/25 ***********************************
          chgname = dsLossRpt.CMfsn;
          chgname = %scanrpl('"': '\"' : chgname);
          //*** End Add *** 02/14/25 ***********************************
          json += '{"Claimant":"' +  %trim(dsLossRpt.CMlsn) +  ', ' +
             //*** Beg Chg *** 02/14/25 ***********************************
             //*** %trim(dsLossRpt.CMfsn) + '",' +
             %trim(chgname) + '",' +
             //*** End Chg *** 02/14/25 ***********************************
             BuildJsonNode('CaseNumber':%Editc(dsLossRpt.CMcase:'X')) +
             ',' +
             BuildJsonNode('InjuryDate':%Editw(dtmdYY(dsLossRpt.CMacdt):
             '  /  /    ')) +
             ',' +
             BuildJsonNode('Status':%trim(clmStatus)) +
             ',' +
             BuildJsonNode('PayrollClass':%Editc(dsLossRpt.CMclas:'X')) +
             ',' +
             BuildJsonNode('PayrollClassDescription':%trim(clmPayDesc)) +
             ',' +
             BuildJsonNode('LostTime':%trim(clmLostTime)) +
             ',' +
             BuildJsonNode('NumberLTDays':%Editc(dsLossRpt.CMlost:'3')) +
             ',' +
             BuildJsonNode('DayofAccident':%trim(clmDayOf)) +
             ',' +
             BuildJsonNode('ClosedDate':%trim(clmClosedDate)) +
             ',' +
             BuildJsonNode('DescriptionOfInjury':%trim(dsLossRpt.CMacds) +
             %trim(dsLossRpt.CMacd2)) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateIndemnity':
             %Editc(paidComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateMedical':
             %Editc(paidMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateExpense':
             %Editc(paidExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateLegal':
             %Editc(paidLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateTotal':
             %Editc((paidComp + paidMedical + paidExpense + paidLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodIndemnity':
             %Editc(outComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodMedical':
             %Editc(outMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodExpense':
             %Editc(outExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodLegal':
             %Editc(outLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodTotal':
             %Editc((outComp + outMedical + outExpense + outLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesIndemnity':
             %Editc(rsvComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesMedical':
             %Editc(rsvMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesExpense':
             %Editc(rsvExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesLegal':
             %Editc(rsvLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesTotal':
             %Editc((rsvComp + rsvMedical + rsvExpense + rsvLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredIndemnity':
             %Editc(incComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredMedical':
             %Editc(incMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredExpense':
             %Editc(incExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredLegal':
             %Editc(incLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredTotal':
             %Editc((incComp + incMedical + incExpense + incLegal):'3')) +
             '}';

       EndSr;

       //=================================================================
       // Write Department Information
       //=================================================================
       Begsr WriteDepartmentInfo;

          ptpDptCmp  = 0;
          ptpDptExp  = 0;
          ptpDptLeg  = 0;
          ptpDptMed  = 0;
          ptpDptTot  = 0;

          paidDptCmp = 0;
          paidDptExp = 0;
          paidDptLeg = 0;
          paidDptMed = 0;
          paidDptTot = 0;

          outsDptCmp = 0;
          outsDptExp = 0;
          outsDptLeg = 0;
          outsDptMed = 0;
          outsDptTot = 0;

          tincDptCmp = 0;
          tincDptExp = 0;
          tincDptLeg = 0;
          tincDptMed = 0;
          tincDptTot = 0;

          clmFirst   = *On;

          If dptFirst;
             json += '"DepartmentSort":[';
             dptFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"Department":"' +  %Trim(dsLossRpt.CMdept) + '",' +
                  '"DepartmentName":"' +  %Trim(dsLossRpt.DPdesc) + '",';

       Endsr;

       //=================================================================
       // Write Department Totals
       //=================================================================
       Begsr WriteDepartmentTotals;

          json += BuildJsonNode('DepartmentTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpDptCmp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaymentThisPeriodMedical':
             %EditC(ptpDptMed:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaymentThisPeriodExpense':
             %EditC(ptpDptExp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaymentThisPeriodLegal':
             %EditC(ptpDptLeg:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaymentThisPeriodTotal':
             %EditC(ptpDptTot:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaidToDateIndemnity':
             %EditC(paidDptCmp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaidToDateMedical':
             %EditC(paidDptMed:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaidToDateExpense':
             %EditC(paidDptExp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaidToDateLegal':
             %EditC(paidDptLeg:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsPaidToDateTotal':
             %EditC(paidDptTot:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsOutstandingReservesIndemnity':
             %EditC(outsDptCmp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsOutstandingReservesMedical':
             %EditC(outsDptMed:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsOutstandingReservesExpense':
             %EditC(outsDptExp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsOutstandingReservesLegal':
             %EditC(outsDptLeg:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsOutstandingReservesTotal':
             %EditC(outsDptTot:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsTotalIncurredIndemnity':
             %EditC(tincDptCmp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsTotalIncurredMedical':
             %EditC(tincDptMed:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsTotalIncurredExpense':
             %EditC(tincDptExp:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsTotalIncurredLegal':
             %EditC(tincDptLeg:'3')) + ',' +
             BuildJsonNode('DepartmentTotalsTotalIncurredTotal':
             %EditC(tincDptTot:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Division Information
       //=================================================================
       Begsr WriteDivisionInfo;

          ptpDivCmp  = 0;
          ptpDivExp  = 0;
          ptpDivLeg  = 0;
          ptpDivMed  = 0;
          ptpDivTot  = 0;

          paidDivCmp = 0;
          paidDivExp = 0;
          paidDivLeg = 0;
          paidDivMed = 0;
          paidDivTot = 0;

          outsDivCmp = 0;
          outsDivExp = 0;
          outsDivLeg = 0;
          outsDivMed = 0;
          outsDivTot = 0;

          tincDivCmp = 0;
          tincDivExp = 0;
          tincDivLeg = 0;
          tincDivMed = 0;
          tincDivTot = 0;

          clmFirst   = *On;

          If divFirst;
             json += '"DivisionSort":[';
             divFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"Division":"' +  %EditC(dsLossRpt.AHdiv:'X') + '",' +
                  '"DivisionName":"' +  %Trim(dsLossRpt.empName) + '",';

       Endsr;

       //=================================================================
       // Write Division Totals
       //=================================================================
       Begsr WriteDivisionTotals;

          json += BuildJsonNode('DivisionTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodMedical':
             %EditC(ptpDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodExpense':
             %EditC(ptpDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodLegal':
             %EditC(ptpDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodTotal':
             %EditC(ptpDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateIndemnity':
             %EditC(paidDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateMedical':
             %EditC(paidDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateExpense':
             %EditC(paidDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateLegal':
             %EditC(paidDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateTotal':
             %EditC(paidDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesIndemnity':
             %EditC(outsDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesMedical':
             %EditC(outsDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesExpense':
             %EditC(outsDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesLegal':
             %EditC(outsDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesTotal':
             %EditC(outsDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredIndemnity':
             %EditC(tincDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredMedical':
             %EditC(tincDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredExpense':
             %EditC(tincDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredLegal':
             %EditC(tincDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredTotal':
             %EditC(tincDivTot:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Insured Information
       //=================================================================
       Begsr WriteInsInfo;

          ptpInsCmp  = 0;
          ptpInsExp  = 0;
          ptpInsLeg  = 0;
          ptpInsMed  = 0;
          ptpInsTot  = 0;

          paidInsCmp = 0;
          paidInsExp = 0;
          paidInsLeg = 0;
          paidInsMed = 0;
          paidInsTot = 0;

          outsInsCmp = 0;
          outsInsExp = 0;
          outsInsLeg = 0;
          outsInsMed = 0;
          outsInsTot = 0;

          tincInsCmp = 0;
          tincInsExp = 0;
          tincInsLeg = 0;
          tincInsMed = 0;
          tincInsTot = 0;

          yrFirst    = *On;
          divFirst   = *On;
          dptFirst   = *On;
          locFirst   = *On;
          clmFirst   = *On;

          If insFirst = *On;
             json += '"InsuredSort":[';
             insFirst = *Off;
          Else;
             json += ',';
          EndIf;

          json += '{ ' +
             BuildJsonNode('InsuredName':%Trim(dsLossRpt.empName)) +
             ',';
             If dsLossRpt.EMmad1 = ' ';
                json += BuildJsonNode('InsuredAddress1':
                 %trim(dsLossRpt.EMMad1)) + ',' +
                 BuildJsonNode('InsuredAddress2':%trim(dsLossRpt.EMMad2)) +
                 ',' +
                 BuildJsonNode('InsuredCity':%trim(dsLossRpt.EMMcty)) +
                 ',' +
                 BuildJsonNode('InsuredState':%trim(dsLossRpt.EMMst)) +
                 ',' +
                 BuildJsonNode('InsuredZip':%editw(dsLossRpt.EMMzip:
                 '     -    '));
             Else;
                json += BuildJsonNode('InsuredAddress1':
                 %trim(dsLossRpt.EMPad1)) + ',' +
                 BuildJsonNode('InsuredAddress2':%trim(dsLossRpt.EMPad2)) +
                 ',' +
                 BuildJsonNode('InsuredCity':%trim(dsLossRpt.EMPcty)) +
                 ',' +
                 BuildJsonNode('InsuredState':%trim(dsLossRpt.EMPst)) +
                 ',' +
                 BuildJsonNode('InsuredZip':%editw(dsLossRpt.EMPzip:
                 '     -    '));
             EndIf;
             json += ',' +
             BuildJsonNode('Insured#':%EditC(dsLossRpt.AHemp#:'X')) +
             ',' +
             BuildJsonNode('Division':%EditC(dsLossRpt.AHdiv:'X')) +
             ',' +
             BuildJsonNode('County':%Trim(dsLossRpt.CNdesc)) +
             ',' +
             BuildJsonNode('Region':%EditC(dsLossRpt.EMrgn#:'X')) +
             ',' +
             BuildJsonNode('RegionName':%Trim(dsLossRpt.REdesc)) +
             ',' +
             BuildJsonNode('Chapter':%EditC(dsLossRpt.EMchap:'X')) +
             ',' +
             BuildJsonNode('ChapterName':%Trim(dsLossRpt.CPdesc)) +
             ',' +
             BuildJsonNode('OfficeName':%Trim(dsLossRpt.OFname)) +
             ',' +
             BuildJsonNode('OfficeAddress1':%trim(dsLossRpt.OFAdr1)) +
             ',' +
             BuildJsonNode('OfficeAddress2':%trim(dsLossRpt.OFAdr2)) +
             ',' +
             BuildJsonNode('OfficeCity':%trim(dsLossRpt.OFCty)) +
             ',' +
             BuildJsonNode('OfficeState':%trim(dsLossRpt.OFSt)) +
             ',' +
             BuildJsonNode('OfficeZip':%editw(dsLossRpt.OFZip:
             '     -    ')) +
             ',' +
             BuildJsonNode('Office#':%Trim(dsLossRpt.EMoff#)) +
             ',' +
             BuildJsonNode('CarrierName':%Trim(dsLossRpt.FNName)) +
             ',';
             If dsLossRpt.FNmad1 = ' ';
                json += BuildJsonNode('CarrierAddress1':
                 %trim(dsLossRpt.FNMad1)) + ',' +
                 BuildJsonNode('CarrierAddress2':%trim(dsLossRpt.FNMad2)) +
                 ',' +
                 BuildJsonNode('CarrierCity':%trim(dsLossRpt.FNMcty)) +
                 ',' +
                 BuildJsonNode('CarrierState':%trim(dsLossRpt.FNMst)) +
                 ',' +
                 BuildJsonNode('CarrierZip':%editw(dsLossRpt.FNMzip:
                 '     -    '));
             Else;
                json += BuildJsonNode('CarrierAddress1':
                 %trim(dsLossRpt.FNPad1)) + ',' +
                 BuildJsonNode('CarrierAddress2':%trim(dsLossRpt.FNPad2)) +
                 ',' +
                 BuildJsonNode('CarrierCity':%trim(dsLossRpt.FNPcty)) +
                 ',' +
                 BuildJsonNode('CarrierState':%trim(dsLossRpt.FNPst)) +
                 ',' +
                 BuildJsonNode('CarrierZip':%editw(dsLossRpt.FNPzip:
                 '     -    '));
             EndIf;
             json += ',' +
             BuildJsonNode('Carrier#':%EditC(dsLossRpt.AHfnd:'X')) +
             ',';

       Endsr;

       //=================================================================
       // Write Insured Totals
       //=================================================================
       Begsr WriteInsTotals;

          json += BuildJsonNode('InsuredTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodMedical':
             %EditC(ptpInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodExpense':
             %EditC(ptpInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodLegal':
             %EditC(ptpInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodTotal':
             %EditC(ptpInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateIndemnity':
             %EditC(paidInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateMedical':
             %EditC(paidInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateExpense':
             %EditC(paidInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateLegal':
             %EditC(paidInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateTotal':
             %EditC(paidInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesIndemnity':
             %EditC(outsInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesMedical':
             %EditC(outsInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesExpense':
             %EditC(outsInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesLegal':
             %EditC(outsInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesTotal':
             %EditC(outsInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredIndemnity':
             %EditC(tincInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredMedical':
             %EditC(tincInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredExpense':
             %EditC(tincInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredLegal':
             %EditC(tincInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredTotal':
             %EditC(tincInsTot:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Location Information
       //=================================================================
       Begsr WriteLocationInfo;

          ptpLocCmp  = 0;
          ptpLocExp  = 0;
          ptpLocLeg  = 0;
          ptpLocMed  = 0;
          ptpLocTot  = 0;

          paidLocCmp = 0;
          paidLocExp = 0;
          paidLocLeg = 0;
          paidLocMed = 0;
          paidLocTot = 0;

          outsLocCmp = 0;
          outsLocExp = 0;
          outsLocLeg = 0;
          outsLocMed = 0;
          outsLocTot = 0;

          tincLocCmp = 0;
          tincLocExp = 0;
          tincLocLeg = 0;
          tincLocMed = 0;
          tincLocTot = 0;

          dptFirst   = *On;
          clmFirst   = *On;

          If locFirst;
             json += '"LocationSort":[';
             locFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"Location":"' +  %Trim(dsLossRpt.CMctg1) + '",' +
                  '"LocationName":"' +  %Trim(dsLossRpt.ALname) + '",';

       Endsr;

       //=================================================================
       // Write Location Totals
       //=================================================================
       Begsr WriteLocationTotals;

          json += BuildJsonNode('LocationTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpLocCmp:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaymentThisPeriodMedical':
             %EditC(ptpLocMed:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaymentThisPeriodExpense':
             %EditC(ptpLocExp:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaymentThisPeriodLegal':
             %EditC(ptpLocLeg:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaymentThisPeriodTotal':
             %EditC(ptpLocTot:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaidToDateIndemnity':
             %EditC(paidLocCmp:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaidToDateMedical':
             %EditC(paidLocMed:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaidToDateExpense':
             %EditC(paidLocExp:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaidToDateLegal':
             %EditC(paidLocLeg:'3')) + ',' +
             BuildJsonNode('LocationTotalsPaidToDateTotal':
             %EditC(paidLocTot:'3')) + ',' +
             BuildJsonNode('LocationTotalsOutstandingReservesIndemnity':
             %EditC(outsLocCmp:'3')) + ',' +
             BuildJsonNode('LocationTotalsOutstandingReservesMedical':
             %EditC(outsLocMed:'3')) + ',' +
             BuildJsonNode('LocationTotalsOutstandingReservesExpense':
             %EditC(outsLocExp:'3')) + ',' +
             BuildJsonNode('LocationTotalsOutstandingReservesLegal':
             %EditC(outsLocLeg:'3')) + ',' +
             BuildJsonNode('LocationTotalsOutstandingReservesTotal':
             %EditC(outsLocTot:'3')) + ',' +
             BuildJsonNode('LocationTotalsTotalIncurredIndemnity':
             %EditC(tincLocCmp:'3')) + ',' +
             BuildJsonNode('LocationTotalsTotalIncurredMedical':
             %EditC(tincLocMed:'3')) + ',' +
             BuildJsonNode('LocationTotalsTotalIncurredExpense':
             %EditC(tincLocExp:'3')) + ',' +
             BuildJsonNode('LocationTotalsTotalIncurredLegal':
             %EditC(tincLocLeg:'3')) + ',' +
             BuildJsonNode('LocationTotalsTotalIncurredTotal':
             %EditC(tincLocTot:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Policy Information
       //=================================================================
       Begsr WritePolicyInfo;

          ptpPolCmp  = 0;
          ptpPolExp  = 0;
          ptpPolLeg  = 0;
          ptpPolMed  = 0;
          ptpPolTot  = 0;

          paidPolCmp = 0;
          paidPolExp = 0;
          paidPolLeg = 0;
          paidPolMed = 0;
          paidPolTot = 0;

          outsPolCmp = 0;
          outsPolExp = 0;
          outsPolLeg = 0;
          outsPolMed = 0;
          outsPolTot = 0;

          tincPolCmp = 0;
          tincPolExp = 0;
          tincPolLeg = 0;
          tincPolMed = 0;
          tincPolTot = 0;

          divFirst   = *On;
          dptFirst   = *On;
          locFirst   = *On;
          clmFirst   = *On;

          If yrFirst;
             json += '"PolicySort":[';
             yrFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"PolicyYear":"' +  %EditC(dsLossRpt.AHfYr:'X') + '",';

          json += BuildJsonNode('Policy#':%EditC(dsLossRpt.AHfnd:'X') +
                                          %EditC(dsLossRpt.AHEmp#:'X') +
                                          %EditC(dsLossRpt.AHfYr:'X')) +
                  ',' +
                  BuildJsonNode('PolicyEffectiveDate':
                   %editw(dtMdyy(dsLossRpt.ELpfDt):'  /  /    ')) +
                  ',' +
                  BuildJsonNode('PolicyExpirationDate':
                   %editw(dtMdyy(dsLossRpt.ELptDt):'  /  /    ')) +
                  ',' +
                  BuildJsonNode('AgencyName':%Trim(dsLossRpt.AGname)) +
                  ',';

       Endsr;

       //=================================================================
       // Write Policy Totals
       //=================================================================
       Begsr WritePolicyTotals;

          json += BuildJsonNode('PolicyTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodMedical':
             %EditC(ptpPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodExpense':
             %EditC(ptpPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodLegal':
             %EditC(ptpPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodTotal':
             %EditC(ptpPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateIndemnity':
             %EditC(paidPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateMedical':
             %EditC(paidPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateExpense':
             %EditC(paidPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateLegal':
             %EditC(paidPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateTotal':
             %EditC(paidPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesIndemnity':
             %EditC(outsPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesMedical':
             %EditC(outsPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesExpense':
             %EditC(outsPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesLegal':
             %EditC(outsPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesTotal':
             %EditC(outsPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredIndemnity':
             %EditC(tincPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredMedical':
             %EditC(tincPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredExpense':
             %EditC(tincPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredLegal':
             %EditC(tincPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredTotal':
             %EditC(tincPolTot:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Report Totals
       //=================================================================
       Begsr WriteRptTotals;

          If count > 0;
             json += '], ';
          EndIf;

          json +=
             BuildJsonNode('ReportTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodMedical':
             %EditC(ptpTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodExpense':
             %EditC(ptpTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodLegal':
             %EditC(ptpTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodTotal':
             %EditC(ptpTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateIndemnity':
             %EditC(paidTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateMedical':
             %EditC(paidTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateExpense':
             %EditC(paidTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateLegal':
             %EditC(paidTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateTotal':
             %EditC(paidTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesIndemnity':
             %EditC(outsTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesMedical':
             %EditC(outsTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesExpense':
             %EditC(outsTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesLegal':
             %EditC(outsTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesTotal':
             %EditC(outsTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredIndemnity':
             %EditC(tincTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredMedical':
             %EditC(tincTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredExpense':
             %EditC(tincTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredLegal':
             %EditC(tincTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredTotal':
             %EditC(tincTotTot:'3'));

       Endsr;

      /End-free

     pLossExperienceReport...
     p                 e
       //===================================================================
       // Loss Experience As Of Report
       //===================================================================
     pLossExperienceAsOfReport...
     p                 b
     dLossExperienceAsOfReport...
     d                 pi

     d dsLossRpt       ds                  qualified
     d  AHco#                         3  0
     d  AHfnd                         3  0
     d  AHemp#                        9  0
     d  AHdiv                         5  0
     d  AHfyr                         3  0
     d  polYear                       4  0
     d  maxDate                       7  0
     d  AHagt#                        5  0
     d  AGname                       40
     d  AGfein                        9  0
     d  AGmod                         3  0
     d  FNname                       60
     d  FNmAd1                       40
     d  FNmAd2                       40
     d  FNmCty                       25
     d  FNmSt                         2
     d  FNmZip                        9  0
     d  FNpAd1                       40
     d  FNpAd2                       40
     d  FNpCty                       25
     d  FNpSt                         2
     d  FNpZip                        9  0
     d  CMrt01                        2
     d  rsvType01                     1
     d  CMrr01                        9  2
     d  CMar01                        9  2
     d  CMrt02                        2
     d  rsvType02                     1
     d  CMrr02                        9  2
     d  CMar02                        9  2
     d  CMrt03                        2
     d  rsvType03                     1
     d  CMrr03                        9  2
     d  CMar03                        9  2
     d  CMrt04                        2
     d  rsvType04                     1
     d  CMrr04                        9  2
     d  CMar04                        9  2
     d  CMrt05                        2
     d  rsvType05                     1
     d  CMrr05                        9  2
     d  CMar05                        9  2
     d  CMrt06                        2
     d  rsvType06                     1
     d  CMrr06                        9  2
     d  CMar06                        9  2
     d  CMrt07                        2
     d  rsvType07                     1
     d  CMrr07                        9  2
     d  CMar07                        9  2
     d  CMrt08                        2
     d  rsvType08                     1
     d  CMrr08                        9  2
     d  CMar08                        9  2
     d  CMrt09                        2
     d  rsvType09                     1
     d  CMrr09                        9  2
     d  CMar09                        9  2
     d  CMrt10                        2
     d  rsvType10                     1
     d  CMrr10                        9  2
     d  CMar10                        9  2
     d  CMltfl                        1
     d  CMlost                        5  0
     d  CMacdt                        7  0
     d  CMclas                        5  0
     d  CMacds                       60
     d  CMacd2                       60
     d  CMlsn                        30
     d  CMfsn                        15
     d  CMmid                         1
     d  CMcase                        7  0
     d  CMstat                        1
     d  CMctg1                       10
     d  EMoff#                        5
     d  OFname                       40
     d  OFadr1                       40
     d  OFadr2                       40
     d  OFcty                        25
     d  OFst                          2
     d  OFzip                         9  0
     d  empName                     100
     d  EMmAd1                       40
     d  EMmAd2                       40
     d  EMmCty                       25
     d  EMmSt                         2
     d  EMmZip                        9  0
     d  EMpAd1                       40
     d  EMpAd2                       40
     d  EMpCty                       25
     d  EMpSt                         2
     d  EMpZip                        9  0
     d  ELpfDt                        7  0
     d  ELptDt                        7  0
     d  ELrtCd                        3
     d  RSjrSt                        2
     d  EMfein                        9  0
     d  cxDate                        7  0
     d  EMcnty                        2
     d  CNdesc                       30
     d  EMrgn#                        3  0
     d  REdesc                       30
     d  EMchap                        3  0
     d  CPdesc                       30

     d filter          ds                  Qualified
     d  groupName                    40a   varying
     d  group#                        3a
     d  officeName                   40a   varying
     d  office#                       5a
     d  agencyName                   40a   varying
     d  agencyFein                    9a
     d  agencyMod                     3a
     d  insuredName                  40a   varying
     d  insuredFein                   9a   varying
     d  policyStatus                 25a   varying
     d  fromPolEffDt                  8a
     d  toPolEffDt                    8a
     d  fromPolExpDt                  8a
     d  toPolExpDt                    8a
     d  sortOrder                    40a   varying
     d  asOfDate                      8a

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
     d division        s              5  0 Inz(0)
     d case#           s              7  0 Inz(0)
     d badData         s               n   Inz(*Off)
     d dateError       s               n   Inz(*Off)
     d moreRows        s               n   Inz(*On)
     d subset          s               n   Inz(*Off)
     d meetSS          s               n   Inz(*On)
     d fromPolEf7      s              7  0
     d fromPolEf8      s              8  0
     d strFromPolEf7   s              7
     d toPolEf7        s              7  0
     d toPolEf8        s              8  0
     d strToPolEf7     s              7
     d fromPolEx7      s              7  0
     d fromPolEx8      s              8  0
     d strFromPolEx7   s              7
     d toPolEx7        s              7  0
     d toPolEx8        s              8  0
     d strToPolEx7     s              7
     d asOfDate7       s              7  0
     d asOfDate8       s              8  0
     d strAsofDate7    s              7
     D policyNumber    s             15a
     D status          s             20a
     d totalInc        s             11s 2
     d totalIncIn      s             11s 2
     d qteFlg          s              1a   inz(' ')
     d count           s              9s 0 Inz(0)
     d insFirst        s               n   Inz(*On)
     d yrFirst         s               n   Inz(*On)
     d divFirst        s               n   Inz(*On)
     d clmFirst        s               n   Inz(*On)
     d holdFnd         s              3  0 Inz(0)
     d holdEmp#        s              9  0 Inz(0)
     d holdPoLYr       s              3  0 Inz(0)
     d holdDivision    s              5  0 Inz(0)
     d hold2Fnd        s              3  0 Inz(0)
     d hold2Emp#       s              9  0 Inz(0)
     d hold2PoLYr      s              3  0 Inz(0)
     d hold2Division   s              5  0 Inz(0)
     d divisionSort    s               n   Inz(*Off)
     d endReport       s               n   Inz(*Off)
     d earnedPrem      s             11  2
     d lossRatio       s              9  2
     d amount1         s             11
     d amount2         s             11
     d term            s              1

     d clmStatus       s             20    Inz(' ')
     d clmPayDesc      s             40    Inz(' ')
     d clmLostTime     s             20    Inz(' ')
     d clmDayOf        s             10    Inz(' ')
     d clmClosedDate   s             10    Inz(' ')

     d inFromDate      s              7  0
     d inToDate        s              7  0
     d outComp         s             11  2
     d outMedical      s             11  2
     d outExpense      s             11  2
     d outLegal        s             11  2
     d paidComp        s             11  2
     d paidMedical     s             11  2
     d paidExpense     s             11  2
     d paidLegal       s             11  2
     d rsvComp         s             11  2
     d rsvMedical      s             11  2
     d rsvExpense      s             11  2
     d rsvLegal        s             11  2
     d incComp         s             11  2
     d incMedical      s             11  2
     d incExpense      s             11  2
     d incLegal        s             11  2

     D sqlAgtDate      s           1000a
     D sqlAgtInfo      s           1000a
     D sqlAgtName      s           1000a
     D sqlClmInfo      s           5000a
     D sqlRptQry       s           1000a
     D sqlRptWhere     s           1000a
     D sqlRptOrder     s           1000a
     D sqlStmtStr      s          11000a

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

     d* Earned Premium
     d earnedPol       s             15s 2 Inz
     d earnedIns       s             15s 2 Inz
     d earnedTot       s             15s 2 Inz

     d* Total Paid
     d paidDiv         s             15s 2 Inz
     d paidPol         s             15s 2 Inz
     d paidIns         s             15s 2 Inz
     d paidTot         s             15s 2 Inz

     d* Paid To Date
     d ptpDivCmp       s             15s 2 Inz
     d ptpDivExp       s             15s 2 Inz
     d ptpDivLeg       s             15s 2 Inz
     d ptpDivMed       s             15s 2 Inz
     d ptpDivTot       s             15s 2 Inz

     d ptpInsCmp       s             15s 2 Inz
     d ptpInsExp       s             15s 2 Inz
     d ptpInsLeg       s             15s 2 Inz
     d ptpInsMed       s             15s 2 Inz
     d ptpInsTot       s             15s 2 Inz

     d ptpPolCmp       s             15s 2 Inz
     d ptpPolExp       s             15s 2 Inz
     d ptpPolLeg       s             15s 2 Inz
     d ptpPolMed       s             15s 2 Inz
     d ptpPolTot       s             15s 2 Inz

     d ptpTotCmp       s             15s 2 Inz
     d ptpTotExp       s             15s 2 Inz
     d ptpTotLeg       s             15s 2 Inz
     d ptpTotMed       s             15s 2 Inz
     d ptpTotTot       s             15s 2 Inz

     d* Total Paid
     d paidDivCmp      s             15s 2 Inz
     d paidDivExp      s             15s 2 Inz
     d paidDivLeg      s             15s 2 Inz
     d paidDivMed      s             15s 2 Inz
     d paidDivTot      s             15s 2 Inz

     d paidInsCmp      s             15s 2 Inz
     d paidInsExp      s             15s 2 Inz
     d paidInsLeg      s             15s 2 Inz
     d paidInsMed      s             15s 2 Inz
     d paidInsTot      s             15s 2 Inz

     d paidPolCmp      s             15s 2 Inz
     d paidPolExp      s             15s 2 Inz
     d paidPolLeg      s             15s 2 Inz
     d paidPolMed      s             15s 2 Inz
     d paidPolTot      s             15s 2 Inz

     d paidTotCmp      s             15s 2 Inz
     d paidTotExp      s             15s 2 Inz
     d paidTotLeg      s             15s 2 Inz
     d paidTotMed      s             15s 2 Inz
     d paidTotTot      s             15s 2 Inz

     d* Outstanding Reserves
     d outsDivCmp      s             15s 2 Inz
     d outsDivExp      s             15s 2 Inz
     d outsDivLeg      s             15s 2 Inz
     d outsDivMed      s             15s 2 Inz
     d outsDivTot      s             15s 2 Inz

     d outsInsCmp      s             15s 2 Inz
     d outsInsExp      s             15s 2 Inz
     d outsInsLeg      s             15s 2 Inz
     d outsInsMed      s             15s 2 Inz
     d outsInsTot      s             15s 2 Inz

     d outsPolCmp      s             15s 2 Inz
     d outsPolExp      s             15s 2 Inz
     d outsPolLeg      s             15s 2 Inz
     d outsPolMed      s             15s 2 Inz
     d outsPolTot      s             15s 2 Inz

     d outsTotCmp      s             15s 2 Inz
     d outsTotExp      s             15s 2 Inz
     d outsTotLeg      s             15s 2 Inz
     d outsTotMed      s             15s 2 Inz
     d outsTotTot      s             15s 2 Inz

     d* Total Incurred
     d tincDivCmp      s             15s 2 Inz
     d tincDivExp      s             15s 2 Inz
     d tincDivLeg      s             15s 2 Inz
     d tincDivMed      s             15s 2 Inz
     d tincDivTot      s             15s 2 Inz

     d tincInsCmp      s             15s 2 Inz
     d tincInsExp      s             15s 2 Inz
     d tincInsLeg      s             15s 2 Inz
     d tincInsMed      s             15s 2 Inz
     d tincInsTot      s             15s 2 Inz

     d tincPolCmp      s             15s 2 Inz
     d tincPolExp      s             15s 2 Inz
     d tincPolLeg      s             15s 2 Inz
     d tincPolMed      s             15s 2 Inz
     d tincPolTot      s             15s 2 Inz

     d tincTotCmp      s             15s 2 Inz
     d tincTotExp      s             15s 2 Inz
     d tincTotLeg      s             15s 2 Inz
     d tincTotMed      s             15s 2 Inz
     d tincTotTot      s             15s 2 Inz

      /free
       Exsr GetParameters;
       If errMsg = ' ';
          Exsr BldReturnInfo;
       Else;
          Exsr BldErrorsInfo;
       EndIf;

       json = %Trim(json) + ' } ';
       outJson = json;


       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ', "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtnCode = 'error';

       Endsr;

       //=================================================================
       // Build the Claim Report Record
       //=================================================================
       Begsr BldReportRcd;

          Select;
          When filter.sortOrder = 'Employer/Policy Year';

             Select;
             When holdFnd  <> dsLossRpt.AHfnd Or
                  holdEmp# <> dsLossRpt.AHemp# Or
                  holdPolYr  <> dsLossRpt.AHfYr;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHemp#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                When holdPolYr  <> dsLossRpt.AHfYr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                EndSl;
                holdFnd   = dsLossRpt.AHfnd;
                holdEmp#  = dsLossRpt.AHemp#;
                holdPolYr = dsLossRpt.AHfYr;
             EndSl;

             // Sum up Reserve Amounts
             ExSr SumAmounts;

          When filter.sortOrder = 'Employer/Policy Year/Division';

             Select;
             When holdFnd      <> dsLossRpt.AHfnd Or
                  holdEmp#     <> dsLossRpt.AHemp# Or
                  holdPolYr    <> dsLossRpt.AHfYr Or
                  holdDivision <> dsLossRpt.AHdiv;
                Select;
                When (holdFnd <> dsLossRpt.AHfnd) Or
                     (holdEmp# <> dsLossRpt.AHemp#);
                   If holdFnd <> 0 And holdEmp# <> 0;
                      json += '],';
                      ExSr WriteDivisionTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                      json += '],';
                      ExSr WriteInsTotals;
                   Endif;
                   ExSr WriteInsInfo;
                   ExSr WritePolicyInfo;
                   ExSr WriteDivisionInfo;
                When holdPolYr <> dsLossRpt.AHfyr;
                   If holdPolYr <> 0;
                      json += '],';
                      ExSr WriteDivisionTotals;
                      json += '],';
                      ExSr WritePolicyTotals;
                   EndIf;
                   ExSr WritePolicyInfo;
                   ExSr WriteDivisionInfo;
                When holdDivision <> dsLossRpt.AHdiv;
                   If not divFirst;
                      json += '],';
                      ExSr WriteDivisionTotals;
                   EndIf;
                   ExSr WriteDivisionInfo;
                EndSl;
                holdFnd      = dsLossRpt.AHfnd;
                holdEmp#     = dsLossRpt.AHemp#;
                holdPolYr    = dsLossRpt.AHfYr;
                holdDivision = dsLossRpt.AHdiv;
             EndSl;

             // Sum up Policy Information
             ExSr SumAmounts;

          EndSl;

       Endsr;

       //=================================================================
       // Build Loss Experience Report                                      quests
       //=================================================================
       Begsr BldReportResults;

          // Set Sort Flag
          ExSr SetSortFlags;

          // Start Size Report
          json = %Trim(json) + ',"reportName":"Loss Experience As Of Report",';

          // Build agency sql statement
          sqlAgtDate =
             'With max_Agt_Date as (Select AHco#, AHfnd, ' +
             'AHemp#, AHdiv, AHfyr, Max(AHefdt) maxDate ' +
             'From wmahp ' +
             'Where AHefdt <= ' + %EditC(DateTo7(SystemDate()):'X') + ' ' +
             'And AHstat <> ''I'' ' +
             'Group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr),';

          sqlAgtInfo =
             'Agt_Agt# as (Select a.*, b.AHagt# agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And ' +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt),';

          sqlAgtName =
             'Agt_Name as (Select a.*, c.AGname, c.AGfein, c.AGmod, ' +
             'd.FNname, d.FNmAd1, d.FNmAd2, d.FNmCty, d.FNmSt, d.FNmZip, ' +
             'd.FNpAd1, d.FNpAd2, d.FNpCty, d.FNpSt, d.FNpZip ' +
             'From Agt_Agt# a join WMAAP b ' +
             'on a.AHco#=b.AAco# And a.AHfnd=b.AAfnd And a.AHagt#=b.AAagt# ' +
             'join WMAGP c on b.AAfein=c.AGfein And b.AAmod=c.AGmod ' +
             'join WMFNP d on a.AHco#=d.FNco# And a.AHfnd=d.FNfnd),';

          sqlClmInfo =
             'Loss_Report as (Select a.*, ' +
             'cmrt01, Value(r1.rttype,'' '') rsvType01, cmrr01, cmar01, ' +
             'cmrt02, Value(r2.rttype,'' '') rsvType02, cmrr02, cmar02, ' +
             'cmrt03, Value(r3.rttype,'' '') rsvType03, cmrr03, cmar03, ' +
             'cmrt04, Value(r4.rttype,'' '') rsvType04, cmrr04, cmar04, ' +
             'cmrt05, Value(r5.rttype,'' '') rsvType05, cmrr05, cmar05, ' +
             'cmrt06, Value(r6.rttype,'' '') rsvType06, cmrr06, cmar06, ' +
             'cmrt07, Value(r7.rttype,'' '') rsvType07, cmrr07, cmar07, ' +
             'cmrt08, Value(r8.rttype,'' '') rsvType08, cmrr08, cmar08, ' +
             'cmrt09, Value(r9.rttype,'' '') rsvType09, cmrr09, cmar09, ' +
             'cmrt10, Value(r0.rttype,'' '') rsvType10, cmrr10, cmar10, ' +
             'b.cmltfl, b.cmlost, b.cmacdt, b.cmclas, b.cmacds, b.cmacd2, ' +
             'b.cmlsn, b.cmfsn, b.cmmid, ' +
             'b.CMcase, b.CMstat, b.CMctg1, c.EMoff#, ' +
             'Value(d.OFname,'' '') ofname, Value(d.OFadr1,'' ''), ' +
             'Value(d.OFadr2,'' ''), Value(d.OFcty,'' ''), ' +
             'Value(d.OFst,'' ''), Value(d.OFzip,0), ' +
             'Trim(c.EMname) || c.EMnam2 empName, ' +
             'c.EMmAd1, c.EMmAd2, c.EMmCty, c.EMmSt, c.EMmZip, ' +
             'c.EMpAd1, c.EMpAd2, c.EMpCty, c.EMpSt, c.EMpZip, ' +
             'e.ELpfdt, e.ELptdt, e.ELrtCd, rs.RSjrSt, c.EMfein, ' +
             ZSqlCxDate('b') + ',' +
             'c.EMcnty, Value(cn.CNdesc,'' ''), ' +
             'c.EMrgn#, Value(re.REdesc,'' ''), ' +
             'c.EMchap, Value(cp.CPdesc,'' '') ' +
             'From Agt_Name a join WMCMP b on a.AHco#=b.CMco# And ' +
             'a.AHfnd=b.CMfnd And a.AHemp#=b.CMemp# And ' +
             'a.AHdiv=b.CMdiv join WMEMP c on c.EMco#=b.CMco# And ' +
             'c.EMfnd=b.CMfnd And c.EMemp#=b.CMemp# And ' +
             'c.EMdiv=b.CMdiv Left Outer Join WMOFP d on ' +
             'c.EMco#=d.OFco# And c.EMoff#=d.OFofc# ' +
             'join WDELP e on e.ELco#=b.CMco# And e.ELfnd=b.CMfnd And ' +
             'e.ELemp#=b.CMemp# And e.ELdiv=b.CMdiv And e.ELfyr=b.CMfyr ' +
             'left outer join wtrtp r1 on b.CMrt01 = r1.RTkey ' +
             'left outer join wtrtp r2 on b.CMrt02 = r2.RTkey ' +
             'left outer join wtrtp r3 on b.CMrt03 = r3.RTkey ' +
             'left outer join wtrtp r4 on b.CMrt04 = r4.RTkey ' +
             'left outer join wtrtp r5 on b.CMrt05 = r5.RTkey ' +
             'left outer join wtrtp r6 on b.CMrt06 = r6.RTkey ' +
             'left outer join wtrtp r7 on b.CMrt07 = r7.RTkey ' +
             'left outer join wtrtp r8 on b.CMrt08 = r8.RTkey ' +
             'left outer join wtrtp r9 on b.CMrt09 = r9.RTkey ' +
             'left outer join wtrtp r0 on b.CMrt10 = r0.RTkey ' +
             'join wmrsp rs on e.ELrtcd = rs.RSrtcd ' +
             'left outer join wmrep re on c.EMrgn# = re.RErgn# ' +
             'And c.EMfnd=re.REfnd ' +
             'left outer join wmcpp cp on c.EMchap = cp.CPchap ' +
             'And c.EMfnd=cp.CPfnd ' +
             'left outer join wtcnp cn on ' +
             'rs.RSjrst || c.EMcnty = cn.CNkey)';

          sqlRptQry = 'Select * From Loss_Report';

          // Add on Filters
          If filter.group#        <> ' ' Or filter.groupName    <> ' ' Or
             filter.officeName    <> ' ' Or filter.office#      <> ' ' Or
             filter.agencyName    <> ' ' Or filter.agencyFein   <> ' ' Or
             filter.agencyMod     <> ' ' Or filter.insuredName  <> ' ' Or
             filter.insuredFein   <> ' ' Or filter.fromPolEffDt <> ' ' Or
             filter.toPolEffDt    <> ' ' Or filter.policyStatus <> ' ';

             sqlRptWhere = ' Where ';

             // Group # Filter;
             If filter.group# <> ' ';
                subset = *On;
                sqlRptWhere = %Trim(sqlRptWhere) + ' AHfnd = '  +
                             %Trim(filter.group#);
             Else;
                // Group Name Filter;
                If filter.groupName <> ' ';
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(FNname) like ''%'' || Upper(''' +
                      %Trim(filter.groupName) + ''') || ''%''';
                EndIf;
             EndIf;

             // Office # Filter;
             If filter.office# <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMoff# = '''  +
                                 filter.office# + '''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMoff# = '''  +
                                 filter.office# + '''';
                EndIf;
             Else;
                // Office Name Filter;
                If filter.officeName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   Else;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' And Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Agency Fein Filter
             If filter.agencyFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' AGfein=' +
                                 filter.agencyFein;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And AGfein=' +
                                 filter.agencyFein;
                EndIf;
             EndIf;

             // Agency Modifier Filter
             If filter.agencyMod <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' AGmod=' + filter.agencyMod;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And AGmod=' + filter.agencyMod;
                EndIf;
             EndIf;

             // Agency Name Filter
             If filter.agencyName <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(AGname) like ''%' +
                       %Trim(Upper(filter.agencyName)) + '%''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And Upper(AGname) like ''%' +
                      %Trim(Upper(filter.agencyName)) + '%''';
                EndIf;
             EndIf;

             // Insured Filters
             If filter.insuredFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMfein = '  +
                                %Trim(filter.insuredFein);
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMfein = '  +
                                %Trim(filter.insuredFein);
                EndIf;
             Else;
                // Insured Name Filter;
                If filter.insuredName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                         ' Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   Else;
                      sqlrptWhere = %Trim(sqlRptWhere) +
                         ' And Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // From Policy Effective Date Filters
             If filter.fromPolEffDt <> ' ' And
                filter.fromPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                EndIf;
             EndIf;

             // To Policy Effective Date Filter
             If filter.toPolEffDt <> ' ' And
                filter.toPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                EndIf;
             EndIf;

             // From Policy Expiration Date Filters
             If filter.fromPolExpDt <> ' ' And
                filter.fromPolExpDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELptDt >= '  +
                                %Trim(%EditC(fromPolEx7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELptDt >= '  +
                                %Trim(%EditC(fromPolEx7:'X'));
                EndIf;
             EndIf;

             // To Policy Effective Date Filter
             If filter.toPolExpDt <> ' ' And
                filter.toPolExpDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELptDt <= '  +
                                %Trim(%EditC(toPolEx7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELptDt <= '  +
                                %Trim(%EditC(toPolEx7:'X'));
                EndIf;
             EndIf;

             // Policy Status
             If filter.policyStatus <> ' ';
                If subset = *Off;
                   subset = *On;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And';
                EndIf;
                sqlRptWhere = %Trim(sqlRptWhere)
                 + ZSqlWherePolicyStatus(filter.policyStatus);
             EndIf;

          EndIf;

          // Establish sort Order
          Select;
          When filter.sortOrder = 'Employer/Policy Year';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr';
          When filter.sortOrder = 'Employer/Policy Year/Division';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, AHdiv';
          When filter.sortOrder = 'Employer/Policy Year/Department';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMdept';
          When filter.sortOrder = 'Employer/Policy Year/Location';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMctg1';
          When filter.sortOrder = 'Employer/Policy Year/Location/Department';
             sqlRptOrder = 'Order by AHfnd, AHemp#, AHfYr, CMctg1, CMdept';
          EndSl;

          sqlStmtStr = 'with' + ZSqlAgtDate + ', ' + %Trim(sqlAgtInfo) + ' ' +
                       ZSqlAgtName(*on) + ', ' +
                       %Trim(sqlClmInfo) + ' ' +
                       %Trim(sqlRptQry)  + ' ' + %Trim(sqlRptWhere) + ' ' +
                       %Trim(sqlRptOrder);

          // Prepare the SQL statement
          Exec SQL prepare rptStmt3 from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare rptCursor3 cursor for rptStmt3;

          // Run SQL
          Exec SQL open rptCursor3;

          // Get first record if there is one
          Exec SQL fetch next from rptCursor3 into :dsLossRpt;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             ExSr FinishReport;
             moreRows = *off;
             Exec SQL close rptCursor3;
          EndIf;

          // If row was fetched then process
          DoW moreRows = *On;

             // Did anything get picked up
             count += 1;

             // Build Report Record
             Exsr BldReportRcd;

             // Fetch next Record
             Exec SQL fetch next from rptCursor3 into :dsLossRpt;

             // check to see if there was another record fetched.
             If sqlstt <> '00000';
                ExSr FinishReport;
                moreRows = *Off;
                Exec SQL close rptCursor3;
             EndIf;
          EndDo;

       EndSr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"LossExperienceReport" : {' +
                 BuildJsonNode('groupName':filter.groupName) +
                 ', ' +
                 BuildJsonNode('group#':filter.group#)  +
                 ', ' +
                 BuildJsonNode('officeName':filter.officeName) +
                 ', ' +
                 BuildJsonNode('office#':filter.office#)  +
                 ', ' +
                 BuildJsonNode('agencyName':filter.agencyName) +
                 ', ' +
                 BuildJsonNode('agencyFein':filter.agencyFein) +
                 ', ' +
                 BuildJsonNode('agencyMod':filter.agencyMod) +
                 ', ' +
                 BuildJsonNode('insuredName':filter.insuredName) +
                 ', ' +
                 BuildJsonNode('insuredFein':filter.insuredFein) +
                 ', ' +
                 BuildJsonNode('policyStatus':filter.policyStatus) +
                 ', ' +
                 BuildJsonNode('fromPolicyEffDate':%editw(fromPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyEffDate':%editw(toPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('fromPolicyExpDate':%editw(fromPolEx8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyExpDate':%editw(toPolEx8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('asOfDate':%editw(asOfDate8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('sortOrder':filter.sortOrder);

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Loss Experience Reports Results
          ExSr BldReportResults;

       Endsr;

       //=================================================================
       // Finish The Report                                                 quests
       //=================================================================
       Begsr FinishReport;

          endReport = *On;
          If count > 0;

             Select;
             When filter.sortOrder = 'Employer/Policy Year';
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             When filter.sortOrder = 'Employer/Policy Year/Division';
                json += '],';
                ExSr WriteDivisionTotals;
                json += '],';
                ExSr WritePolicyTotals;
                json += '],';
                ExSr WriteInsTotals;
             EndSl;
          EndIf;

          ExSr WriteRptTotals;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'groupName');
             filter.groupName   = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'group#');
             filter.group#      = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'officeName');
             filter.officeName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'office#');
             filter.office#     = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyName');
             filter.agencyName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyFein');
             filter.agencyFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyMod');
             filter.agencyMod  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredName');
             filter.insuredName = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredFein');
             filter.insuredFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'policyStatus');
             filter.policyStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyEffDate');
             filter.fromPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyEffDate');
             filter.toPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyExpDate');
             filter.fromPolExpDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyExpDate');
             filter.toPolExpDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'sortOrder');
             filter.sortOrder = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'asOfDate');
             filter.asOfDate = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             //------------------------------------------------------
             // Office # must be valid              <<<<<<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.office# <> ' ';
                // If the office # valid?
                If ValidOffice(filter.office#) = *Off;
                   errMsg = %Trim(errMsg) + '"Invalid office#."             ';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Fein: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Fein: ' +
                                               'Invalid numeric data"';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             // Modifier
             If filter.agencyMod <> ' ';
                Monitor;
                   mod = %Dec(%Trim(filter.agencyMod):3:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Mod: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Mod: ' +
                                               'Invalid numeric data"';
                   EndIf;
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
                If errMsg = ' ';
                   errMsg = %Trim(errMsg) + '"Invalid agency Fein/Mod."';
                Else;
                   errMsg = %Trim(errMsg) + ',"Invalid agency Fein/Mod."';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Insured: Invalid numeric ' +
                                               'data."';
                   Else;
                      errMsg = %Trim(errMsg) + '",Insured: Invalid numeric '+
                                               'data."';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             //------------------------------------------------------
             // Validate Policy Status                 <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.policyStatus <> ' '
              And filter.policyStatus <> 'All'
              And filter.policyStatus <> 'Active'
              And filter.policyStatus <> 'Cancelled'
              And filter.policyStatus <> 'Expired'
              And filter.policyStatus <> 'Pending'
              And filter.policyStatus <> 'Pending Cancellation';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Policy Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Policy Status." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Effective date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolEffDt = ' ' Or filter.fromPolEffDt = '00000000';
                fromPolEf8     = 0;
                fromPolEf7     = 0;
                strFromPolEf7  = '0000000';
             Else;
                Monitor;
                   fromPolEf8 = %Dec(filter.fromPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Effective Date: Invalid Numeric Data. " ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"From Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEf8 <> 0;
                   fromPolEf7     = dtCYMD(fromPolEf8);
                   strfromPolEf7  = %EditC(fromPolEf7:'X');
                   If fromPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Policy Effective Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Effective Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Effective date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolEffDt = ' ' Or filter.toPolEffDt = '00000000';
                toPolEf8     = 0;
                toPolEf7     = 0;
                strToPolEf7  = '0000000';
             Else;
                Monitor;
                   toPolEf8 = %Dec(filter.toPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Effective Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEf8 <> 0;
                   toPolEf7     = dtCYMD(toPolEf8);
                   strToPolEf7  = %EditC(toPolEf7:'X');
                   If toPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Policy Effective Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Effective Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Expiration date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolExpDt = ' ' Or filter.fromPolExpDt = '00000000';
                fromPolEx8     = 0;
                fromPolEx7     = 0;
                strFromPolEx7  = '0000000';
             Else;
                Monitor;
                   fromPolEx8 = %Dec(filter.fromPolExpDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Expiration Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                       ',"From Policy Expiration Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEx8 <> 0;
                   fromPolEx7     = dtCYMD(fromPolEx8);
                   strfromPolEx7  = %EditC(fromPolEx7:'X');
                   If fromPolEx7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                            '"From Policy Expiration Date: Invalid Date. " ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Expiration Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Expiration date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolExpDt = ' ' Or filter.toPolExpDt = '00000000';
                toPolEx8     = 0;
                toPolEx7     = 0;
                strToPolEx7  = '0000000';
             Else;
                Monitor;
                   toPolEf8 = %Dec(filter.toPolExpDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Expiration Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"To Policy Expiration Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEx8 <> 0;
                   toPolEx7     = dtCYMD(toPolEx8);
                   strToPolEx7  = %EditC(toPolEx7:'X');
                   If toPolEx7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                           '"To Policy Expiration Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Expiration Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Sort Order                    <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.sortOrder <> 'Employer/Policy Year' And
                filter.sortOrder <> 'Employer/Policy Year/Division';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Sort Order."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Sort Order." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // As Of Date must be valid                 <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.asOfDate = ' ' Or filter.asOfDate = '00000000        ';
                If errMsg = ' ';
                   errMsg = %Trim(errMsg) +
                     '"As of Date: Invalid Numeric Data." ';
                Else;
                   errMsg = %Trim(errMsg) +
                     ',"As of Date: Invalid Numeric Data." ';
                EndIf;
             Else;
                Monitor;
                   asOfDate8 = %Dec(filter.asofDate:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"As Of Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"As Of Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And asOfDate8 <> 0;
                   asOfDate7     = dtCYMD(asOfDate8);
                   strAsOfDate7  = %EditC(asOfDate7:'X');
                   If asOfDate7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"As Of Date: Invalid Date."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"As of Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

       //=================================================================
       // Set Sort Flags
       //=================================================================
       Begsr SetSortFlags;

          divisionSort = *Off;

          Select;
          When filter.sortOrder = 'Employer/Policy Year/Division';
             divisionSort = *On;
          EndSl;

       Endsr;

       //=================================================================
       // Sum Policy Info
       //=================================================================
       Begsr SumAmounts;

          //* Earned Premium
          If hold2Fnd      <> dsLossRpt.AHfnd Or
           hold2Emp#     <> dsLossRpt.AHemp# Or
           hold2PolYr    <> dsLossRpt.AHfYr Or
           hold2Division <> dsLossRpt.AHdiv;

             // Get as of earned premoium
             srprt1(%EditC(dsLossRpt.AHco#:'X'):%EditC(dsLossRpt.AHfnd:'X'):
                    %EditC(dsLossRpt.AHfYr:'X'):%EditC(dsLossRpt.AHemp#:'X'):
                    %EditC(dsLossRpt.AHdiv:'X'):filter.asOfDate:amount1:
                    amount2:term);

             Monitor;
               earnedPrem = %Dec(amount1:11:2)/100;
             On-Error;
               earnedPrem = 0;
             EndMon;

             earnedPol += earnedPrem;
             earnedIns += earnedPrem;
             earnedTot += earnedPrem;

             hold2Fnd      = dsLossRpt.AHfnd;
             hold2Emp#     = dsLossRpt.AHemp#;
             hold2PolYr    = dsLossRpt.AHfYr;
             hold2Division = dsLossRpt.AHdiv;
          Endif;

          // As Of Loss Ratio
          inFromDate = 0;
          inToDate   = asOfDate7;
          outMedical = 0;
          outComp    = 0;
          outExpense = 0;
          outLegal   = 0;
          GetPaidThisPeriod(dsLossRpt.AHco#:dsLossRpt.AHfnd:
             dsLossRpt.AHfYr:dsLossRpt.CMcase:inFromDate:inToDate:
             outMedical:outComp:outExpense:outLegal);

          paidDiv += (outMedical+outComp+outExpense+outLegal);
          paidPol += (outMedical+outComp+outExpense+outLegal);
          paidIns += (outMedical+outComp+outExpense+outLegal);
          paidTot += (outMedical+outComp+outExpense+outLegal);

          //===================================================
          // Paid This Period
          //===================================================
          inFromDate = DateTo7(GetFirstDay(systemDate()));
          inToDate   = DateTo7(GetLastDay(systemDate()));
          outMedical = 0;
          outComp    = 0;
          outExpense = 0;
          outLegal   = 0;
          paidMedical = 0;
          paidComp    = 0;
          paidExpense = 0;
          paidLegal   = 0;
          rsvMedical = 0;
          rsvComp    = 0;
          rsvExpense = 0;
          rsvLegal   = 0;
          incMedical = 0;
          incComp    = 0;
          incExpense = 0;
          incLegal   = 0;

          GetPaidThisPeriod(dsLossRpt.AHco#:dsLossRpt.AHfnd:
             dsLossRpt.AHfYr:dsLossRpt.CMcase:inFromDate:inToDate:
             outMedical:outComp:outExpense:outLegal);

          ptpDivCmp += outComp;
          ptpDivTot += outComp;
          ptpPolCmp += outComp;
          ptpPolTot += outComp;
          ptpInsCmp += outComp;
          ptpInsTot += outComp;
          ptpTotCmp += outComp;
          ptpTotTot += outComp;

          ptpDivMed += outMedical;
          ptpDivTot += outMedical;
          ptpPolMed += outMedical;
          ptpPolTot += outMedical;
          ptpInsMed += outMedical;
          ptpInsTot += outMedical;
          ptpTotMed += outMedical;
          ptpTotTot += outMedical;

          ptpDivExp += outExpense;
          ptpDivTot += outExpense;
          ptpPolExp += outExpense;
          ptpPolTot += outExpense;
          ptpInsExp += outExpense;
          ptpInsTot += outExpense;
          ptpTotExp += outExpense;
          ptpTotTot += outExpense;

          ptpDivCmp += outLegal;
          ptpDivTot += outLegal;
          ptpPolCmp += outLegal;
          ptpPolTot += outLegal;
          ptpInsCmp += outLegal;
          ptpInsTot += outLegal;
          ptpTotCmp += outLegal;
          ptpTotTot += outLegal;

          //===================================================
          // Compensation amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar01;
             paidDivCmp += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidPolCmp += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsCmp += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotCmp += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr01;
             outsDivCmp += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsPolCmp += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsCmp += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotCmp += dsLossRpt.CMar01;
             outsTotTot += dsLossRpt.CMar01;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotCmp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar02;
             paidDivCmp += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidPolCmp += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsCmp += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotCmp += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr02;
             outsDivCmp += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsPolCmp += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsCmp += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotCmp += dsLossRpt.CMar02;
             outsTotTot += dsLossRpt.CMar02;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar03;
             paidDivCmp += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidPolCmp += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsCmp += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotCmp += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr03;
             outsDivCmp += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsPolCmp += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsCmp += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotCmp += dsLossRpt.CMar03;
             outsTotTot += dsLossRpt.CMar03;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotCmp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar04;
             paidDivCmp += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidPolCmp += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsCmp += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotCmp += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr04;
             outsDivCmp += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsPolCmp += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsCmp += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotCmp += dsLossRpt.CMar04;
             outsTotTot += dsLossRpt.CMar04;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotCmp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar05;
             paidDivCmp += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidPolCmp += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsCmp += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotCmp += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr05;
             outsDivCmp += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsPolCmp += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsCmp += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotCmp += dsLossRpt.CMar05;
             outsTotTot += dsLossRpt.CMar05;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotCmp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar06;
             paidDivCmp += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidPolCmp += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsCmp += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotCmp += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr06;
             outsDivCmp += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsPolCmp += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsCmp += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotCmp += dsLossRpt.CMar06;
             outsTotTot += dsLossRpt.CMar06;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotCmp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar07;
             paidDivCmp += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidPolCmp += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsCmp += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotCmp += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr07;
             outsDivCmp += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsPolCmp += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsCmp += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotCmp += dsLossRpt.CMar07;
             outsTotTot += dsLossRpt.CMar07;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotCmp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar08;
             paidDivCmp += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidPolCmp += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsCmp += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotCmp += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr08;
             outsDivCmp += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsPolCmp += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsCmp += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotCmp += dsLossRpt.CMar08;
             outsTotTot += dsLossRpt.CMar08;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotCmp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar09;
             paidDivCmp += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidPolCmp += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsCmp += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotCmp += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr09;
             outsDivCmp += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsPolCmp += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsCmp += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotCmp += dsLossRpt.CMar09;
             outsTotTot += dsLossRpt.CMar09;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsCmp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotCmp += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'C';
             // Total Paid Amounts
             paidComp   += dsLossRpt.CMar10;
             paidDivCmp += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidPolCmp += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsCmp += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotCmp += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvComp    += dsLossRpt.CMrr10;
             outsDivCmp += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsPolCmp += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsCmp += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotCmp += dsLossRpt.CMar10;
             outsTotTot += dsLossRpt.CMar10;
             // Total Total Incurred Amounts
             incComp    += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotCmp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Medical amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar01;
             paidDivMed += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidPolMed += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsMed += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotMed += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr01;
             outsDivMed += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsPolMed += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsMed += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotMed += dsLossRpt.CMar01;
             outsTotTot += dsLossRpt.CMar01;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotMed += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar02;
             paidDivMed += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidPolMed += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsMed += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotMed += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr02;
             outsDivMed += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsPolMed += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsMed += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotMed += dsLossRpt.CMar02;
             outsTotTot += dsLossRpt.CMar02;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotMed += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar03;
             paidDivMed += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidPolMed += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsMed += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotMed += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr03;
             outsDivMed += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsPolMed += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsMed += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotMed += dsLossRpt.CMar03;
             outsTotTot += dsLossRpt.CMar03;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotMed += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar04;
             paidDivMed += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidPolMed += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsMed += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotMed += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr04;
             outsDivMed += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsPolMed += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsMed += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotMed += dsLossRpt.CMar04;
             outsTotTot += dsLossRpt.CMar04;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotMed += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar05;
             paidDivMed += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidPolMed += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsMed += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotMed += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr05;
             outsDivMed += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsPolMed += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsMed += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotMed += dsLossRpt.CMar05;
             outsTotTot += dsLossRpt.CMar05;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotMed += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar06;
             paidDivMed += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidPolMed += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsMed += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotMed += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr06;
             outsDivMed += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsPolMed += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsMed += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotMed += dsLossRpt.CMar06;
             outsTotTot += dsLossRpt.CMar06;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotMed += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar07;
             paidDivMed += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidPolMed += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsMed += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotMed += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr07;
             outsDivMed += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsPolMed += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsMed += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotMed += dsLossRpt.CMar07;
             outsTotTot += dsLossRpt.CMar07;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotMed += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar08;
             paidDivMed += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidPolMed += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsMed += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotMed += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr08;
             outsDivMed += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsPolMed += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsMed += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotMed += dsLossRpt.CMar08;
             outsTotTot += dsLossRpt.CMar08;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotMed += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar09;
             paidDivMed += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidPolMed += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsMed += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotMed += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr09;
             outsDivMed += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsPolMed += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsMed += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotMed += dsLossRpt.CMar09;
             outsTotTot += dsLossRpt.CMar09;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsMed += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotMed += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'M';
             // Total Paid Amounts
             paidMedical += dsLossRpt.CMar10;
             paidDivMed += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidPolMed += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsMed += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotMed += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvMedical += dsLossRpt.CMrr10;
             outsDivMed += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsPolMed += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsMed += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotMed += dsLossRpt.CMar10;
             outsTotTot += dsLossRpt.CMar10;
             // Total Total Incurred Amounts
             incMedical += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotMed += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Expense amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar01;
             paidDivExp += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidPolExp += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsExp += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotExp += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr01;
             outsDivExp += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsPolExp += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsExp += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotExp += dsLossRpt.CMar01;
             outsTotTot += dsLossRpt.CMar01;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotExp += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar02;
             paidDivExp += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidPolExp += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsExp += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotExp += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr02;
             outsDivExp += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsPolExp += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsExp += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotExp += dsLossRpt.CMar02;
             outsTotTot += dsLossRpt.CMar02;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotExp += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar03;
             paidDivExp += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidPolExp += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsExp += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotExp += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr03;
             outsDivExp += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsPolExp += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsExp += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotExp += dsLossRpt.CMar03;
             outsTotTot += dsLossRpt.CMar03;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotExp += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar04;
             paidDivExp += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidPolExp += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsExp += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotExp += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr04;
             outsDivExp += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsPolExp += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsExp += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotExp += dsLossRpt.CMar04;
             outsTotTot += dsLossRpt.CMar04;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotExp += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar05;
             paidDivExp += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidPolExp += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsExp += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotExp += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr05;
             outsDivExp += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsPolExp += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsExp += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotExp += dsLossRpt.CMar05;
             outsTotTot += dsLossRpt.CMar05;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotExp += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar06;
             paidDivExp += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidPolExp += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsExp += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotExp += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr06;
             outsDivExp += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsPolExp += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsExp += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotExp += dsLossRpt.CMar06;
             outsTotTot += dsLossRpt.CMar06;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotExp += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar07;
             paidDivExp += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidPolExp += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsExp += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotExp += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr07;
             outsDivExp += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsPolExp += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsExp += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotExp += dsLossRpt.CMar07;
             outsTotTot += dsLossRpt.CMar07;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotExp += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar08;
             paidDivExp += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidPolExp += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsExp += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotExp += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr08;
             outsDivExp += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsPolExp += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsExp += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotExp += dsLossRpt.CMar08;
             outsTotTot += dsLossRpt.CMar08;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotExp += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar09;
             paidDivExp += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidPolExp += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsExp += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotExp += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr09;
             outsDivExp += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsPolExp += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsExp += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotExp += dsLossRpt.CMar09;
             outsTotTot += dsLossRpt.CMar09;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsExp += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotExp += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'E';
             // Total Paid Amounts
             paidExpense += dsLossRpt.CMar10;
             paidDivExp += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidPolExp += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsExp += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotExp += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvExpense += dsLossRpt.CMrr10;
             outsDivExp += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsPolExp += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsExp += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotExp += dsLossRpt.CMar10;
             outsTotTot += dsLossRpt.CMar10;
             // Total Total Incurred Amounts
             incExpense += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotExp += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

          //===================================================
          // Legal amounts
          //===================================================
          If dsLossRpt.rsvType01 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar01;
             paidDivLeg += dsLossRpt.CMar01;
             paidDivTot += dsLossRpt.CMar01;
             paidPolLeg += dsLossRpt.CMar01;
             paidPolTot += dsLossRpt.CMar01;
             paidInsLeg += dsLossRpt.CMar01;
             paidInsTot += dsLossRpt.CMar01;
             paidTotLeg += dsLossRpt.CMar01;
             paidTotTot += dsLossRpt.CMar01;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr01;
             outsDivLeg += dsLossRpt.CMrr01;
             outsDivTot += dsLossRpt.CMrr01;
             outsPolLeg += dsLossRpt.CMrr01;
             outsPolTot += dsLossRpt.CMrr01;
             outsInsLeg += dsLossRpt.CMrr01;
             outsInsTot += dsLossRpt.CMrr01;
             outsTotLeg += dsLossRpt.CMar01;
             outsTotTot += dsLossRpt.CMar01;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincDivTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincPolTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincInsTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotLeg += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
             tincTotTot += dsLossRpt.CMrr01 + dsLossRpt.CMar01;
          EndIf;

          If dsLossRpt.rsvType02 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar02;
             paidDivLeg += dsLossRpt.CMar02;
             paidDivTot += dsLossRpt.CMar02;
             paidPolLeg += dsLossRpt.CMar02;
             paidPolTot += dsLossRpt.CMar02;
             paidInsLeg += dsLossRpt.CMar02;
             paidInsTot += dsLossRpt.CMar02;
             paidTotLeg += dsLossRpt.CMar02;
             paidTotTot += dsLossRpt.CMar02;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr02;
             outsDivLeg += dsLossRpt.CMrr02;
             outsDivTot += dsLossRpt.CMrr02;
             outsPolLeg += dsLossRpt.CMrr02;
             outsPolTot += dsLossRpt.CMrr02;
             outsInsLeg += dsLossRpt.CMrr02;
             outsInsTot += dsLossRpt.CMrr02;
             outsTotLeg += dsLossRpt.CMar02;
             outsTotTot += dsLossRpt.CMar02;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincDivTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincPolTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar02;
          EndIf;

          If dsLossRpt.rsvType03 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar03;
             paidDivLeg += dsLossRpt.CMar03;
             paidDivTot += dsLossRpt.CMar03;
             paidPolLeg += dsLossRpt.CMar03;
             paidPolTot += dsLossRpt.CMar03;
             paidInsLeg += dsLossRpt.CMar03;
             paidInsTot += dsLossRpt.CMar03;
             paidTotLeg += dsLossRpt.CMar03;
             paidTotTot += dsLossRpt.CMar03;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr03;
             outsDivLeg += dsLossRpt.CMrr03;
             outsDivTot += dsLossRpt.CMrr03;
             outsPolLeg += dsLossRpt.CMrr03;
             outsPolTot += dsLossRpt.CMrr03;
             outsInsLeg += dsLossRpt.CMrr03;
             outsInsTot += dsLossRpt.CMrr03;
             outsTotLeg += dsLossRpt.CMar03;
             outsTotTot += dsLossRpt.CMar03;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincDivTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincPolTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincInsTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotLeg += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
             tincTotTot += dsLossRpt.CMrr03 + dsLossRpt.CMar03;
          EndIf;

          If dsLossRpt.rsvType04 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar04;
             paidDivLeg += dsLossRpt.CMar04;
             paidDivTot += dsLossRpt.CMar04;
             paidPolLeg += dsLossRpt.CMar04;
             paidPolTot += dsLossRpt.CMar04;
             paidInsLeg += dsLossRpt.CMar04;
             paidInsTot += dsLossRpt.CMar04;
             paidTotLeg += dsLossRpt.CMar04;
             paidTotTot += dsLossRpt.CMar04;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr04;
             outsDivLeg += dsLossRpt.CMrr04;
             outsDivTot += dsLossRpt.CMrr04;
             outsPolLeg += dsLossRpt.CMrr04;
             outsPolTot += dsLossRpt.CMrr04;
             outsInsLeg += dsLossRpt.CMrr04;
             outsInsTot += dsLossRpt.CMrr04;
             outsTotLeg += dsLossRpt.CMar04;
             outsTotTot += dsLossRpt.CMar04;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincDivTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincPolTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincInsTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotLeg += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
             tincTotTot += dsLossRpt.CMrr04 + dsLossRpt.CMar04;
          EndIf;

          If dsLossRpt.rsvType05 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar05;
             paidDivLeg += dsLossRpt.CMar05;
             paidDivTot += dsLossRpt.CMar05;
             paidPolLeg += dsLossRpt.CMar05;
             paidPolTot += dsLossRpt.CMar05;
             paidInsLeg += dsLossRpt.CMar05;
             paidInsTot += dsLossRpt.CMar05;
             paidTotLeg += dsLossRpt.CMar05;
             paidTotTot += dsLossRpt.CMar05;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr05;
             outsDivLeg += dsLossRpt.CMrr05;
             outsDivTot += dsLossRpt.CMrr05;
             outsPolLeg += dsLossRpt.CMrr05;
             outsPolTot += dsLossRpt.CMrr05;
             outsInsLeg += dsLossRpt.CMrr05;
             outsInsTot += dsLossRpt.CMrr05;
             outsTotLeg += dsLossRpt.CMar05;
             outsTotTot += dsLossRpt.CMar05;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincDivTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincPolTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincInsTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotLeg += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
             tincTotTot += dsLossRpt.CMrr05 + dsLossRpt.CMar05;
          EndIf;

          If dsLossRpt.rsvType06 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar06;
             paidDivLeg += dsLossRpt.CMar06;
             paidDivTot += dsLossRpt.CMar06;
             paidPolLeg += dsLossRpt.CMar06;
             paidPolTot += dsLossRpt.CMar06;
             paidInsLeg += dsLossRpt.CMar06;
             paidInsTot += dsLossRpt.CMar06;
             paidTotLeg += dsLossRpt.CMar06;
             paidTotTot += dsLossRpt.CMar06;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr06;
             outsDivLeg += dsLossRpt.CMrr06;
             outsDivTot += dsLossRpt.CMrr06;
             outsPolLeg += dsLossRpt.CMrr06;
             outsPolTot += dsLossRpt.CMrr06;
             outsInsLeg += dsLossRpt.CMrr06;
             outsInsTot += dsLossRpt.CMrr06;
             outsTotLeg += dsLossRpt.CMar06;
             outsTotTot += dsLossRpt.CMar06;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincDivTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincPolTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincInsTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotLeg += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
             tincTotTot += dsLossRpt.CMrr06 + dsLossRpt.CMar06;
          EndIf;

          If dsLossRpt.rsvType07 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar07;
             paidDivLeg += dsLossRpt.CMar07;
             paidDivTot += dsLossRpt.CMar07;
             paidPolLeg += dsLossRpt.CMar07;
             paidPolTot += dsLossRpt.CMar07;
             paidInsLeg += dsLossRpt.CMar07;
             paidInsTot += dsLossRpt.CMar07;
             paidTotLeg += dsLossRpt.CMar07;
             paidTotTot += dsLossRpt.CMar07;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr07;
             outsDivLeg += dsLossRpt.CMrr07;
             outsDivTot += dsLossRpt.CMrr07;
             outsPolLeg += dsLossRpt.CMrr07;
             outsPolTot += dsLossRpt.CMrr07;
             outsInsLeg += dsLossRpt.CMrr07;
             outsInsTot += dsLossRpt.CMrr07;
             outsTotLeg += dsLossRpt.CMar07;
             outsTotTot += dsLossRpt.CMar07;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincDivTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincPolTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincInsTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotLeg += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
             tincTotTot += dsLossRpt.CMrr07 + dsLossRpt.CMar07;
          EndIf;

          If dsLossRpt.rsvType08 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar08;
             paidDivLeg += dsLossRpt.CMar08;
             paidDivTot += dsLossRpt.CMar08;
             paidPolLeg += dsLossRpt.CMar08;
             paidPolTot += dsLossRpt.CMar08;
             paidInsLeg += dsLossRpt.CMar08;
             paidInsTot += dsLossRpt.CMar08;
             paidTotLeg += dsLossRpt.CMar08;
             paidTotTot += dsLossRpt.CMar08;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr08;
             outsDivLeg += dsLossRpt.CMrr08;
             outsDivTot += dsLossRpt.CMrr08;
             outsPolLeg += dsLossRpt.CMrr08;
             outsPolTot += dsLossRpt.CMrr08;
             outsInsLeg += dsLossRpt.CMrr08;
             outsInsTot += dsLossRpt.CMrr08;
             outsTotLeg += dsLossRpt.CMar08;
             outsTotTot += dsLossRpt.CMar08;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincDivTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincPolTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincInsTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotLeg += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
             tincTotTot += dsLossRpt.CMrr08 + dsLossRpt.CMar08;
          EndIf;

          If dsLossRpt.rsvType09 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar09;
             paidDivLeg += dsLossRpt.CMar09;
             paidDivTot += dsLossRpt.CMar09;
             paidPolLeg += dsLossRpt.CMar09;
             paidPolTot += dsLossRpt.CMar09;
             paidInsLeg += dsLossRpt.CMar09;
             paidInsTot += dsLossRpt.CMar09;
             paidTotLeg += dsLossRpt.CMar09;
             paidTotTot += dsLossRpt.CMar09;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr09;
             outsDivLeg += dsLossRpt.CMrr09;
             outsDivTot += dsLossRpt.CMrr09;
             outsPolLeg += dsLossRpt.CMrr09;
             outsPolTot += dsLossRpt.CMrr09;
             outsInsLeg += dsLossRpt.CMrr09;
             outsInsTot += dsLossRpt.CMrr09;
             outsTotLeg += dsLossRpt.CMar09;
             outsTotTot += dsLossRpt.CMar09;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincDivTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincPolTot += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsLeg += dsLossRpt.CMrr09 + dsLossRpt.CMar09;
             tincInsTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotLeg += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
             tincTotTot += dsLossRpt.CMrr02 + dsLossRpt.CMar09;
          EndIf;

          If dsLossRpt.rsvType10 = 'L';
             // Total Paid Amounts
             paidLegal  += dsLossRpt.CMar10;
             paidDivLeg += dsLossRpt.CMar10;
             paidDivTot += dsLossRpt.CMar10;
             paidPolLeg += dsLossRpt.CMar10;
             paidPolTot += dsLossRpt.CMar10;
             paidInsLeg += dsLossRpt.CMar10;
             paidInsTot += dsLossRpt.CMar10;
             paidTotLeg += dsLossRpt.CMar10;
             paidTotTot += dsLossRpt.CMar10;
             // Total Outstanding Reserve Amounts
             rsvLegal   += dsLossRpt.CMrr10;
             outsDivLeg += dsLossRpt.CMrr10;
             outsDivTot += dsLossRpt.CMrr10;
             outsPolLeg += dsLossRpt.CMrr10;
             outsPolTot += dsLossRpt.CMrr10;
             outsInsLeg += dsLossRpt.CMrr10;
             outsInsTot += dsLossRpt.CMrr10;
             outsTotLeg += dsLossRpt.CMar10;
             outsTotTot += dsLossRpt.CMar10;
             // Total Total Incurred Amounts
             incLegal   += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincDivTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincPolTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincInsTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotLeg += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
             tincTotTot += dsLossRpt.CMrr10 + dsLossRpt.CMar10;
          EndIf;

         //* Output the claim detail information.
          Select;
          When %scan('"ClaimSort":[':json:%len(json)-14) <> 0;
          When clmFirst;
             clmFirst = *Off;
             json += '"ClaimSort":[';
          Other;
             json += ',';
          Endsl;

          Clear clmStatus;
          Clear clmPayDesc;
          Clear clmLostTime;
          Clear clmDayOf;
          Clear clmClosedDate;

          Select;
          When dsLossRpt.CMltfl = 'I';
             clmLostTime = 'Indemnity';
          When dsLossRpt.CMltfl = 'M';
             clmLostTime = 'Medical';
          When dsLossRpt.CMltfl = 'R';
             clmLostTime = 'Record Only';
          Endsl;

          clmDayOf = dowShort(dtMDYY(dsLossRpt.cmacdt));

          SetGt (dsLossRpt.AHCo#:dsLossRpt.ELrtcd:dsLossRpt.CMclas:
           dsLossRpt.ELpfdt) ppr01;
          Readpe (dsLossRpt.AHCo#:dsLossRpt.ELrtcd:dsLossRpt.CMclas)
           ppr01;
          If not %eof;
             clmPayDesc = %trim(PRDesc);
          Endif;

          Select;
          When dsLossRpt.CMstat = 'C';
             clmStatus = 'Closed';
             SetGt (dsLossRpt.AHCo#:dsLossRpt.AHFnd:dsLossRpt.AHFyr:
              dsLossRpt.CMCase:DateTo7(SystemDate())) pch00;
             Readpe (dsLossRpt.AHCo#:dsLossRpt.AHFnd:dsLossRpt.AHFyr:
              dsLossRpt.CMCase) pch00;
             If not %eof;
                clmClosedDate = %editw(dtMDYY(CHEfdt):'  /  /    ');
             Endif;
          When dsLossRpt.CMstat = 'O';
             clmStatus = 'Open';
          Endsl;


          //*** Beg Add *** 02/14/25 ***********************************
          chgname = dsLossRpt.CMfsn;
          chgname = %scanrpl('"': '\"' : chgname);
          //*** End Add *** 02/14/25 ***********************************
          json += '{"Claimant":"' +  %trim(dsLossRpt.CMlsn) +  ', ' +
             //*** Beg Chg *** 02/14/25 ***********************************
             //*** %trim(dsLossRpt.CMfsn) + '",' +
             %trim(chgname) + '",' +
             //*** End Chg *** 02/14/25 ***********************************
             BuildJsonNode('CaseNumber':%Editc(dsLossRpt.CMcase:'X')) +
             ',' +
             BuildJsonNode('InjuryDate':%Editw(dtmdYY(dsLossRpt.CMacdt):
             '  /  /    ')) +
             ',' +
             BuildJsonNode('Status':%trim(clmStatus)) +
             ',' +
             BuildJsonNode('PayrollClass':%Editc(dsLossRpt.CMclas:'X')) +
             ',' +
             BuildJsonNode('PayrollClassDescription':%trim(clmPayDesc)) +
             ',' +
             BuildJsonNode('LostTime':%trim(clmLostTime)) +
             ',' +
             BuildJsonNode('NumberLTDays':%Editc(dsLossRpt.CMlost:'3')) +
             ',' +
             BuildJsonNode('DayofAccident':%trim(clmDayOf)) +
             ',' +
             BuildJsonNode('ClosedDate':%trim(clmClosedDate)) +
             ',' +
             BuildJsonNode('DescriptionOfInjury':%trim(dsLossRpt.CMacds) +
             %trim(dsLossRpt.CMacd2)) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodIndemnity':
             %Editc(paidComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodMedical':
             %Editc(paidMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodExpense':
             %Editc(paidExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodLegal':
             %Editc(paidLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaymentThisPeriodTotal':
             %Editc((paidComp + paidMedical + paidExpense + paidLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateIndemnity':
             %Editc(outComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateMedical':
             %Editc(outMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateExpense':
             %Editc(outExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateLegal':
             %Editc(outLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsPaidToDateTotal':
             %Editc((outComp + outMedical + outExpense + outLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesIndemnity':
             %Editc(rsvComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesMedical':
             %Editc(rsvMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesExpense':
             %Editc(rsvExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesLegal':
             %Editc(rsvLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsOutstandingReservesTotal':
             %Editc((rsvComp + rsvMedical + rsvExpense + rsvLegal):'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredIndemnity':
             %Editc(incComp:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredMedical':
             %Editc(incMedical:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredExpense':
             %Editc(incExpense:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredLegal':
             %Editc(incLegal:'3')) +
             ',' +
             BuildJsonNode('ClaimTotalsTotalIncurredTotal':
             %Editc((incComp + incMedical + incExpense + incLegal):'3')) +
             '}';

       EndSr;

       //=================================================================
       // Write Division Information
       //=================================================================
       Begsr WriteDivisionInfo;

          ptpDivCmp  = 0;
          ptpDivExp  = 0;
          ptpDivLeg  = 0;
          ptpDivMed  = 0;
          ptpDivTot  = 0;

          paidDivCmp = 0;
          paidDivExp = 0;
          paidDivLeg = 0;
          paidDivMed = 0;
          paidDivTot = 0;

          outsDivCmp = 0;
          outsDivExp = 0;
          outsDivLeg = 0;
          outsDivMed = 0;
          outsDivTot = 0;

          tincDivCmp = 0;
          tincDivExp = 0;
          tincDivLeg = 0;
          tincDivMed = 0;
          tincDivTot = 0;

          paidDiv = 0;

          clmFirst   = *On;

          If divFirst;
             json += '"DivisionSort":[';
             divFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"Division":"' +  %EditC(dsLossRpt.AHdiv:'X') + '",' +
                  '"DivisionName":"' +  %Trim(dsLossRpt.empName) + '",';

       Endsr;

       //=================================================================
       // Write Division Totals
       //=================================================================
       Begsr WriteDivisionTotals;

          //* Calculate Loss Ratio.
          If earnedPrem <> 0;
             Monitor;
                lossRatio =
                  (paidDiv/earnedPrem)*100;
             On-Error;
                lossRatio = 9999999.99;
             EndMon;
          Else;
             lossRatio = 0;
          Endif;

          json += BuildJsonNode('DivisionTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodMedical':
             %EditC(ptpDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodExpense':
             %EditC(ptpDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodLegal':
             %EditC(ptpDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaymentThisPeriodTotal':
             %EditC(ptpDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateIndemnity':
             %EditC(paidDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateMedical':
             %EditC(paidDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateExpense':
             %EditC(paidDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateLegal':
             %EditC(paidDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsPaidToDateTotal':
             %EditC(paidDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesIndemnity':
             %EditC(outsDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesMedical':
             %EditC(outsDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesExpense':
             %EditC(outsDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesLegal':
             %EditC(outsDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsOutstandingReservesTotal':
             %EditC(outsDivTot:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredIndemnity':
             %EditC(tincDivCmp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredMedical':
             %EditC(tincDivMed:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredExpense':
             %EditC(tincDivExp:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredLegal':
             %EditC(tincDivLeg:'3')) + ',' +
             BuildJsonNode('DivisionTotalsTotalIncurredTotal':
             %EditC(tincDivTot:'3')) +
             ',' +
             BuildJsonNode('DivisionTotalsAsOfEarnedPremium':
             %EditC(earnedPrem:'3')) +
             ',' +
             BuildJsonNode('DivisionTotalsAsOfLossRatio':
             %EditC(lossRatio:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Insured Information
       //=================================================================
       Begsr WriteInsInfo;

          ptpInsCmp  = 0;
          ptpInsExp  = 0;
          ptpInsLeg  = 0;
          ptpInsMed  = 0;
          ptpInsTot  = 0;

          paidInsCmp = 0;
          paidInsExp = 0;
          paidInsLeg = 0;
          paidInsMed = 0;
          paidInsTot = 0;

          outsInsCmp = 0;
          outsInsExp = 0;
          outsInsLeg = 0;
          outsInsMed = 0;
          outsInsTot = 0;

          tincInsCmp = 0;
          tincInsExp = 0;
          tincInsLeg = 0;
          tincInsMed = 0;
          tincInsTot = 0;

          earnedIns = 0;
          paidIns = 0;

          yrFirst    = *On;
          divFirst   = *On;
          clmFirst   = *On;

          If insFirst = *On;
             json += '"InsuredSort":[';
             insFirst = *Off;
          Else;
             json += ',';
          EndIf;

          json += '{ ' +
             BuildJsonNode('InsuredName':%Trim(dsLossRpt.empName)) +
             ',';
             If dsLossRpt.EMmad1 = ' ';
                json += BuildJsonNode('InsuredAddress1':
                 %trim(dsLossRpt.EMMad1)) + ',' +
                 BuildJsonNode('InsuredAddress2':%trim(dsLossRpt.EMMad2)) +
                 ',' +
                 BuildJsonNode('InsuredCity':%trim(dsLossRpt.EMMcty)) +
                 ',' +
                 BuildJsonNode('InsuredState':%trim(dsLossRpt.EMMst)) +
                 ',' +
                 BuildJsonNode('InsuredZip':%editw(dsLossRpt.EMMzip:
                 '     -    '));
             Else;
                json += BuildJsonNode('InsuredAddress1':
                 %trim(dsLossRpt.EMPad1)) + ',' +
                 BuildJsonNode('InsuredAddress2':%trim(dsLossRpt.EMPad2)) +
                 ',' +
                 BuildJsonNode('InsuredCity':%trim(dsLossRpt.EMPcty)) +
                 ',' +
                 BuildJsonNode('InsuredState':%trim(dsLossRpt.EMPst)) +
                 ',' +
                 BuildJsonNode('InsuredZip':%editw(dsLossRpt.EMPzip:
                 '     -    '));
             EndIf;
             json += ',' +
             BuildJsonNode('Insured#':%EditC(dsLossRpt.AHemp#:'X')) +
             ',' +
             BuildJsonNode('Division':%EditC(dsLossRpt.AHdiv:'X')) +
             ',' +
             BuildJsonNode('County':%Trim(dsLossRpt.CNdesc)) +
             ',' +
             BuildJsonNode('Region':%EditC(dsLossRpt.EMrgn#:'X')) +
             ',' +
             BuildJsonNode('RegionName':%Trim(dsLossRpt.REdesc)) +
             ',' +
             BuildJsonNode('Chapter':%EditC(dsLossRpt.EMchap:'X')) +
             ',' +
             BuildJsonNode('ChapterName':%Trim(dsLossRpt.CPdesc)) +
             ',' +
             BuildJsonNode('OfficeName':%Trim(dsLossRpt.OFname)) +
             ',' +
             BuildJsonNode('OfficeAddress1':%trim(dsLossRpt.OFAdr1)) +
             ',' +
             BuildJsonNode('OfficeAddress2':%trim(dsLossRpt.OFAdr2)) +
             ',' +
             BuildJsonNode('OfficeCity':%trim(dsLossRpt.OFCty)) +
             ',' +
             BuildJsonNode('OfficeState':%trim(dsLossRpt.OFSt)) +
             ',' +
             BuildJsonNode('OfficeZip':%editw(dsLossRpt.OFZip:
             '     -    ')) +
             ',' +
             BuildJsonNode('Office#':%Trim(dsLossRpt.EMoff#)) +
             ',' +
             BuildJsonNode('CarrierName':%Trim(dsLossRpt.FNName)) +
             ',';
             If dsLossRpt.FNmad1 = ' ';
                json += BuildJsonNode('CarrierAddress1':
                 %trim(dsLossRpt.FNMad1)) + ',' +
                 BuildJsonNode('CarrierAddress2':%trim(dsLossRpt.FNMad2)) +
                 ',' +
                 BuildJsonNode('CarrierCity':%trim(dsLossRpt.FNMcty)) +
                 ',' +
                 BuildJsonNode('CarrierState':%trim(dsLossRpt.FNMst)) +
                 ',' +
                 BuildJsonNode('CarrierZip':%editw(dsLossRpt.FNMzip:
                 '     -    '));
             Else;
                json += BuildJsonNode('CarrierAddress1':
                 %trim(dsLossRpt.FNPad1)) + ',' +
                 BuildJsonNode('CarrierAddress2':%trim(dsLossRpt.FNPad2)) +
                 ',' +
                 BuildJsonNode('CarrierCity':%trim(dsLossRpt.FNPcty)) +
                 ',' +
                 BuildJsonNode('CarrierState':%trim(dsLossRpt.FNPst)) +
                 ',' +
                 BuildJsonNode('CarrierZip':%editw(dsLossRpt.FNPzip:
                 '     -    '));
             EndIf;
             json += ',' +
             BuildJsonNode('Carrier#':%EditC(dsLossRpt.AHfnd:'X')) +
             ',';

       Endsr;

       //=================================================================
       // Write Insured Totals
       //=================================================================
       Begsr WriteInsTotals;

          //* Calculate Loss Ratio.
          If earnedIns <> 0;
             Monitor;
                lossRatio =
                  (paidIns/earnedIns)*100;
             On-Error;
                lossRatio = 9999999.99;
             EndMon;
          Else;
             lossRatio = 0;
          Endif;

          json += BuildJsonNode('InsuredTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodMedical':
             %EditC(ptpInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodExpense':
             %EditC(ptpInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodLegal':
             %EditC(ptpInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaymentThisPeriodTotal':
             %EditC(ptpInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateIndemnity':
             %EditC(paidInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateMedical':
             %EditC(paidInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateExpense':
             %EditC(paidInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateLegal':
             %EditC(paidInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsPaidToDateTotal':
             %EditC(paidInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesIndemnity':
             %EditC(outsInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesMedical':
             %EditC(outsInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesExpense':
             %EditC(outsInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesLegal':
             %EditC(outsInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsOutstandingReservesTotal':
             %EditC(outsInsTot:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredIndemnity':
             %EditC(tincInsCmp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredMedical':
             %EditC(tincInsMed:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredExpense':
             %EditC(tincInsExp:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredLegal':
             %EditC(tincInsLeg:'3')) + ',' +
             BuildJsonNode('InsuredTotalsTotalIncurredTotal':
             %EditC(tincInsTot:'3')) +
             ',' +
             BuildJsonNode('InsuredTotalsAsOfEarnedPremium':
             %EditC(earnedIns:'3')) +
             ',' +
             BuildJsonNode('InsuredTotalsAsOfLossRatio':
             %EditC(lossRatio:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Policy Information
       //=================================================================
       Begsr WritePolicyInfo;

          ptpPolCmp  = 0;
          ptpPolExp  = 0;
          ptpPolLeg  = 0;
          ptpPolMed  = 0;
          ptpPolTot  = 0;

          paidPolCmp = 0;
          paidPolExp = 0;
          paidPolLeg = 0;
          paidPolMed = 0;
          paidPolTot = 0;

          outsPolCmp = 0;
          outsPolExp = 0;
          outsPolLeg = 0;
          outsPolMed = 0;
          outsPolTot = 0;

          tincPolCmp = 0;
          tincPolExp = 0;
          tincPolLeg = 0;
          tincPolMed = 0;
          tincPolTot = 0;

          earnedPol = 0;
          paidPol = 0;

          divFirst   = *On;
          clmFirst   = *On;

          If yrFirst;
             json += '"PolicySort":[';
             yrFirst = *Off;
          Else;
             json += ',';
          Endif;

          json += '{"PolicyYear":"' +  %EditC(dsLossRpt.AHfYr:'X') + '",';

          json += BuildJsonNode('Policy#':%EditC(dsLossRpt.AHfnd:'X') +
                                          %EditC(dsLossRpt.AHEmp#:'X') +
                                          %EditC(dsLossRpt.AHfYr:'X')) +
                  ',' +
                  BuildJsonNode('PolicyEffectiveDate':
                   %editw(dtMdyy(dsLossRpt.ELpfDt):'  /  /    ')) +
                  ',' +
                  BuildJsonNode('PolicyExpirationDate':
                   %editw(dtMdyy(dsLossRpt.ELptDt):'  /  /    ')) +
                  ',' +
                  BuildJsonNode('AgencyName':%Trim(dsLossRpt.AGname)) +
                  ',';

       Endsr;

       //=================================================================
       // Write Policy Totals
       //=================================================================
       Begsr WritePolicyTotals;

          //* Calculate Loss Ratio.
          If earnedPol <> 0;
             Monitor;
                lossRatio =
                  (paidPol/earnedPol)*100;
             On-Error;
                lossRatio = 9999999.99;
             EndMon;
          Else;
             lossRatio = 0;
          Endif;

          json += BuildJsonNode('PolicyTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodMedical':
             %EditC(ptpPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodExpense':
             %EditC(ptpPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodLegal':
             %EditC(ptpPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaymentThisPeriodTotal':
             %EditC(ptpPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateIndemnity':
             %EditC(paidPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateMedical':
             %EditC(paidPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateExpense':
             %EditC(paidPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateLegal':
             %EditC(paidPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsPaidToDateTotal':
             %EditC(paidPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesIndemnity':
             %EditC(outsPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesMedical':
             %EditC(outsPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesExpense':
             %EditC(outsPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesLegal':
             %EditC(outsPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsOutstandingReservesTotal':
             %EditC(outsPolTot:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredIndemnity':
             %EditC(tincPolCmp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredMedical':
             %EditC(tincPolMed:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredExpense':
             %EditC(tincPolExp:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredLegal':
             %EditC(tincPolLeg:'3')) + ',' +
             BuildJsonNode('PolicyTotalsTotalIncurredTotal':
             %EditC(tincPolTot:'3')) +
             ',' +
             BuildJsonNode('PolicyTotalsAsOfEarnedPremium':
             %EditC(earnedPol:'3')) +
             ',' +
             BuildJsonNode('PolicyTotalsAsOfLossRatio':
             %EditC(lossRatio:'3')) +
             '}';

       Endsr;

       //=================================================================
       // Write Report Totals
       //=================================================================
       Begsr WriteRptTotals;

          //* Calculate Loss Ratio.
          If earnedTot <> 0;
             Monitor;
                lossRatio =
                  (paidTot/earnedTot)*100;
             On-Error;
                lossRatio = 9999999.99;
             EndMon;
          Else;
             lossRatio = 0;
          Endif;

          If count > 0;
             json += '], ';
          EndIf;

          json +=
             BuildJsonNode('ReportTotalsPaymentThisPeriodIndemnity':
             %EditC(ptpTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodMedical':
             %EditC(ptpTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodExpense':
             %EditC(ptpTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodLegal':
             %EditC(ptpTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaymentThisPeriodTotal':
             %EditC(ptpTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateIndemnity':
             %EditC(paidTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateMedical':
             %EditC(paidTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateExpense':
             %EditC(paidTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateLegal':
             %EditC(paidTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsPaidToDateTotal':
             %EditC(paidTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesIndemnity':
             %EditC(outsTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesMedical':
             %EditC(outsTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesExpense':
             %EditC(outsTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesLegal':
             %EditC(outsTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsOutstandingReservesTotal':
             %EditC(outsTotTot:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredIndemnity':
             %EditC(tincTotCmp:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredMedical':
             %EditC(tincTotMed:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredExpense':
             %EditC(tincTotExp:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredLegal':
             %EditC(tincTotLeg:'3')) + ',' +
             BuildJsonNode('ReportTotalsTotalIncurredTotal':
             %EditC(tincTotTot:'3')) +
             ',' +
             BuildJsonNode('ReportTotalsAsOfEarnedPremium':
             %EditC(earnedTot:'3')) +
             ',' +
             BuildJsonNode('ReportTotalsAsOfLossRatio':
             %EditC(lossRatio:'3'));

       Endsr;

      /End-free

     pLossExperienceAsOfReport...
     p                 e

       //===================================================================
       // Top 20 Claims Report
       //===================================================================
     pTop20ClaimsRpt   b
     dTop20ClaimsRpt   pi

     d dsTop20Rpt      ds                  qualified
     d  AHco#                         3  0
     d  AHfnd                         3  0
     d  AHemp#                        9  0
     d  AHdiv                         5  0
     d  AHfyr                         3  0
     d  polYear                       4  0
     d  maxDate                       7  0
     d  AHagt#                        5  0
     d  AGname                       40
     d  AGfein                        9  0
     d  AGmod                         3  0
     d  FNname                       60
     d  totPaid                      18  2
     d  totRemRsv                    18  2
     d  totInc                       28  2
     d  CMcase                        7  0
     d  CMacDt                        7  0
     d  CMstat                        1
     d  EMoff#                        5
     d  OFname                       40
     d  empName                     102
     d  ELpfDt                        7  0
     d  ELptDt                        7  0
     d  EMfein                        9  0
     d  cxDate                        7  0

     d filter          ds                  Qualified
     d  groupName                    40a   varying
     d  group#                        3a
     d  officeName                   40a   varying
     d  office#                       5a
     d  agencyName                   40a   varying
     d  agencyFein                    9a
     d  agencyMod                     3a
     d  insuredName                  40a   varying
     d  insuredFein                   9a
     d  policyStatus                 25a   varying
     d  claimStatus                   1a
     d  fromAccDate                   8a
     d  toAccDate                     8a
     d  fromPolEffDt                  8a
     d  toPolEffDt                    8a
     d  agencyType                    1a
     d  sortOrder                    40a   varying

     d group#          s              3  0
     d fein            s              9  0
     d mod             s              3  0
     d polGroup#       s              3  0 Inz(0)
     d polEmp#         s              9  0 Inz(0)
     d polYear         s              3  0 Inz(0)
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
     d toAccDt7        s              7  0
     d toAccDt8        s              8  0
     d strToAccDt7     s              7
     d fromPolEf7      s              7  0
     d fromPolEf8      s              8  0
     d strFromPolEf7   s              7
     d toPolEf7        s              7  0
     d toPolEf8        s              8  0
     d strToPolEf7     s              7
     D policyNumber    s             15a
     D status          s             20a
     d totalInc        s             11s 2
     d totalIncIn      s             11s 2
     d qteFlg          s              1a   inz(' ')
     d count           s              9s 0 Inz(0)
     d polCount        s              9s 0 Inz(0)

     d totPaidAgt      s             15s 2 Inz
     d totPaidOff      s             15s 2 Inz
     d totPaidRpt      s             15s 2 Inz

     d totIncurredAgt  s             15s 2 Inz
     d totIncurredOff  s             15s 2 Inz
     d totIncurredRpt  s             15s 2 Inz

     d totRemRsvAgt    s             15s 2 Inz
     d totRemRsvOff    s             15s 2 Inz
     d totRemRsvRpt    s             15s 2 Inz

     d holdAgt#        s              5s 0
     d holdOff#        s              5

     d agtPrimary      s               n
     d offPrimary      s               n
     d agtFirst        s               n
     d offFirst        s               n

     D sqlAgtDate      s           1000a   Varying
     D sqlAgtInfo      s           1000a   Varying
     D sqlAgtInfo1     s           1000a   Varying
     D sqlAgtInfo2     s           1000a   Varying
     D sqlAgtInfo3     s           1000a   Varying
     D sqlAgtName      s           1000a   Varying
     D sqlClmInfo      s           2000a   Varying
     D sqlRptQry       s           1000a   Varying
     D sqlRptWhere     s           1000a   Varying
     D sqlRptOrder     s           1000a   Varying
     D sqlStmtStr      s          11000a

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

       json = %Trim(json) + ' } ';
       outJson = json;


       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          // Build the Request Info
          ExSr BldReqInfo;

          // Finish by generating the errors
          json = %Trim(json) + ', "errorMsg": [ ' + %Trim(errMsg) + ' ] ';
          ioRtnCode = 'error';

       Endsr;

       //=================================================================
       // Build the Claim Report Record
       //=================================================================
       Begsr BldReportRcd;

          Select;
          // Agency sort
          When filter.sortOrder = 'Agency';
             If holdAgt# <> dsTop20Rpt.AHagt#;
                If holdAgt# <> 0;
                   json += '],' +
                      '"AgencyTotalsTotalIncurred":"' +
                      %Trim(%EditC(totIncurredAgt:'3'))+ '",' +
                      '"AgencyTotalsTotalPaid":"' +
                      %Trim(%EditC(totPaidAgt:'3'))+ '",' +
                      '"AgencyTotalsTotalRemainingReserves":"' +
                      %Trim(%EditC(totRemRsvAgt:'3')) + '"},';
                Else;
                   json += '"AgencySort":[';
                Endif;

                totPaidAgt     = 0;
                totIncurredAgt = 0;
                totRemRsvAgt   = 0;
                holdAgt#       = dsTop20Rpt.AHagt#;
                polCount       = 0;

                json += '{' +
                   BuildJsonNode('Agency#':%EditC(dsTop20Rpt.AHagt#:'X')) +
                   ',' +
                   BuildJsonNode('AgencyName':%Trim(dsTop20Rpt.AGname)) +
                   ',"ListPolicies":[';

             EndIf;
             totPaidAgt     += dsTop20Rpt.totPaid;
             totIncurredAgt += dsTop20Rpt.totInc;
             totRemRsvAgt   += dsTop20Rpt.TotRemRsv;
          // Office sort
          When filter.sortOrder = 'Office';
             If holdOff# <> dsTop20Rpt.EMoff#;
                If holdOff# <> ' ';
                   json += '],' +
                      '"OfficeTotalsTotalIncurred":"' +
                      %Trim(%EditC(totIncurredOff:'3'))+ '",' +
                      '"OfficeTotalsTotalPaid":"' +
                      %Trim(%EditC(totPaidOff:'3'))+ '",' +
                      '"OfficeTotalsTotalRemainingReserves":"' +
                      %Trim(%EditC(totRemRsvOff:'3')) + '"},';
                Else;
                   json += '"OfficeSort":[';
                Endif;

                totPaidOff     = 0;
                totIncurredOff = 0;
                totRemRsvOff   = 0;
                holdOff#       = dsTop20Rpt.EMoff#;
                polCount       = 0;

                json += '{' +
                   BuildJsonNode('Office#':dsTop20Rpt.EMoff#) +
                   ',' +
                   BuildJsonNode('OfficeName':%Trim(dsTop20Rpt.AGname)) +
                   ',"ListPolicies":[';

             EndIf;
             totPaidOff     += dsTop20Rpt.totPaid;
             totIncurredOff += dsTop20Rpt.totInc;
             totRemRsvOff   += dsTop20Rpt.TotRemRsv;
          Other;
             If count > 0;
                json = %Trim(json) + ',';
             EndIf;
          EndSl;

          totPaidRpt     += dsTop20Rpt.totPaid;
          totIncurredRpt += dsTop20Rpt.totInc;
          totRemRsvRpt   += dsTop20Rpt.TotRemRsv;

          if polCount > 0;
             json += ',';
          EndIf;

          json += '{"Policy#":"' + %EditC(dsTop20Rpt.AHfnd:'X') +
                                   %EditC(dsTop20Rpt.AHemp#:'X') +
                                   %EditC(dsTop20Rpt.AHfyr:'X') + '"' +
                    ',' +
                    '"InsuredName":"' + %Trim(dsTop20Rpt.empName) + '"' +
                    ',' +
                    '"Claim#":"' + %EditC(dsTop20Rpt.CMcase:'X') + '"' +
                    ',' +
                    '"AccidentDate":"' +
                    %EditC(dtMDYY(dsTop20Rpt.CMacDt):'X') + '" ' +
                    ',' +
                    '"TotalIncurred":"' +
                         %Trim(%EditC(dsTop20Rpt.totInc:'3')) + '"' +
                    ',' +
                    '"TotalPaid":"' +
                         %Trim(%EditC(dsTop20Rpt.totPaid:'3')) + '"' +
                    ',' +
                    '"RemainingReserves":"' +
                         %Trim(%EditC(dsTop20Rpt.totRemRsv:'3')) + '"' +
                    '}';


          count    += 1;
          polCount += 1;

       Endsr;

       //=================================================================
       // Build Top 20 Claim Report                                         quests
       //=================================================================
       Begsr BldReportResults;

          // Start Size Report
          json = %Trim(json) + ',"reportName":"Top 20 Claims Report",';

          // Build agency sql statement
          sqlAgtDate =
             'With max_Agt_Date as (Select AHco#, AHfnd, ' +
             'AHemp#, AHdiv, AHfyr, Max(AHefdt) maxDate ' +
             'From wmahp ' +
             'Where AHefdt <= ' + %EditC(DateTo7(SystemDate()):'X') + ' ' +
             'And AHstat <> ''I'' ' +
             'Group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr),';

          sqlAgtInfo1 =
             'Agt_Agt#1 as (Select a.*, b.AHagt# agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt),';

          sqlAgtInfo2 =
             'Agt_Agt#2 as (Select a.*, b.AHsAgt agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt ' +
             'Where b.AHsAgt <> 0),';

          sqlAgtInfo3 =
             'Agt_Agt#3 as (Select a.*, b.AH3Agt agent# ' +
             'From max_Agt_Date a Join WMAHP b on ' +
             'a.AHco#=b.AHco# And a.AHfnd=b.AHfnd And a.AHemp#=b.AHemp# And '  +
             'a.AHdiv=b.AHdiv And a.AHfyr=b.AHfyr And a.maxDate=b.AHefdt ' +
             'Where b.AH3Agt <> 0),';

          Select;
          When filter.agencyType = ' ';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#1 union ' +
                             'Select * from agt_agt#2 union ' +
                             'Select * from agt_agt#3),';
          When filter.agencyType = '1';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#1), ';
          When filter.agencyType = '2';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#2), ';
          When filter.agencyType = '3';
             sqlAgtInfo =
                'Agt_Agt# as (Select * from agt_agt#3), ';
          EndSl;

          sqlAgtName =
             'Agt_Name as (Select a.*, c.AGname, c.AGfein, c.AGmod, ' +
             'd.FNname ' +
             'From Agt_Agt# a join WMAAP b ' +
             'on a.AHco#=b.AAco# And a.AHfnd=b.AAfnd And a.agent#=b.AAagt# ' +
             'join WMAGP c on b.AAfein=c.AGfein And b.AAmod=c.AGmod ' +
             'join WMFNP d on a.AHco#=d.FNco# And a.AHfnd=d.FNfnd),';

          sqlClmInfo =
             'Size_Report as (Select a.*, (CMar01+CMar02+CMar03+CMar04+' +
             'CMar05+CMar06+CMar07+CMar08+CMar09+CMar10) TotPaid, ' +
             '(CMrr01+CMrr02+CMrr03+CMrr04+' +
             'CMrr05+CMrr06+CMrr07+CMrr08+CMar09+CMrr10) TotRemRsv, ' +
             '(CMar01+CMar02+CMar03+CMar04+CMar05+' +
             'CMar06+CMar07+CMar08+CMar09+CMar10+' +
             'CMrr01+CMrr02+CMrr03+CMrr04+CMrr05+' +
             'CMrr06+CMrr07+CMrr08+CMrr09+CMrr10) TotInc, ' +
             'b.CMcase, b.CMacDt, b.CMstat, c.EMoff#, d.OFname, ' +
             'Trim(c.EMname) || c.EMnam2 empName, e.ELpfdt, e.ELptdt, ' +
             //*** Beg Chg *** 01/04/23 *****************************************
             //***'c.EMfein '  + ZSqlCxDate('b') + ',' +
             'c.EMfein, '  + ZSqlCxDate('b') +
             //*** End Chg *** 01/04/23 *****************************************
             'From Agt_Name a join WMCMP b on a.AHco#=b.CMco# And ' +
             'a.AHfnd=b.CMfnd And a.AHemp#=b.CMemp# And ' +
             'a.AHdiv=b.CMdiv And a.AHfyr=b.CMfyr ' +
             'join WMEMP c on c.EMco#=b.CMco# And ' +
             'c.EMfnd=b.CMfnd And c.EMemp#=b.CMemp# And ' +
             'c.EMdiv=b.CMdiv Left Outer Join WMOFP d on ' +
             'c.EMco#=d.OFco# And c.EMoff#=d.OFofc# ' +
             'join WDELP e on e.ELco#=b.CMco# And e.ELfnd=b.CMfnd And ' +
             'e.ELemp#=b.CMemp# And e.ELdiv=b.CMdiv And e.ELfyr=b.CMfyr ' +
             'Order by totInc desc)';

          sqlRptQry = 'Select * From Size_Report';

          // Add on Filters
          If filter.group#        <> ' ' Or filter.groupName    <> ' ' Or
             filter.officeName    <> ' ' Or filter.office#      <> ' ' Or
             filter.agencyName    <> ' ' Or filter.agencyFein   <> ' ' Or
             filter.agencyMod     <> ' ' Or filter.insuredName  <> ' ' Or
             filter.insuredFein   <> ' ' Or filter.claimStatus  <> ' ' Or
             filter.fromAccDate   <> ' ' Or filter.toAccDate    <> ' ' Or
             filter.fromPolEffDt  <> ' ' Or filter.toPolEffDt   <> ' ' Or
             filter.policyStatus  <> ' ';

             sqlRptWhere = ' Where ';

             // Group # Filter;
             If filter.group# <> ' ';
                subset = *On;
                sqlRptWhere = %Trim(sqlRptWhere) + ' AHfnd = '  +
                             %Trim(filter.group#);
             Else;
                // Group Name Filter;
                If filter.groupName <> ' ';
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(FNname) like ''%'' || Upper(''' +
                      %Trim(filter.groupName) + ''') || ''%''';
                EndIf;
             EndIf;

             // Office # Filter;
             If filter.office# <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMoff# = '''  +
                                 filter.office# + '''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMoff# = '''  +
                                 filter.office# + '''';
                EndIf;
             Else;
                // Office Name Filter;
                If filter.officeName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   Else;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                        ' And Upper(OFname) like ''%'' || Upper(''' +
                        %Trim(filter.officeName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Agency Fein Filter
             If filter.agencyFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' AGfein=' +
                                 filter.agencyFein;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And AGfein=' +
                                 filter.agencyFein;
                EndIf;
             EndIf;

             // Agency Modifier Filter
             If filter.agencyMod <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' AGmod=' + filter.agencyMod;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And AGmod=' + filter.agencyMod;
                EndIf;
             EndIf;

             // Agency Name Filter
             If filter.agencyName <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' Upper(AGname) like ''%' +
                       %Trim(Upper(filter.agencyName)) + '%''';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And Upper(AGname) like ''%' +
                      %Trim(Upper(filter.agencyName)) + '%''';
                EndIf;
             EndIf;

             // Insured Filters
             If filter.insuredFein <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' EMfein = '  +
                                %Trim(filter.insuredFein);
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And EMfein = '  +
                                %Trim(filter.insuredFein);
                EndIf;
             Else;
                // Insured Name Filter;
                If filter.insuredName <> ' ';
                   If subset = *Off;
                      subset = *On;
                      sqlRptWhere = %Trim(sqlRptWhere) +
                         ' Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   Else;
                      sqlrptWhere = %Trim(sqlRptWhere) +
                         ' And Upper(empName) like ''%'' || Upper(''' +
                         %Trim(filter.insuredName) + ''') || ''%''';
                   EndIf;
                EndIf;
             EndIf;

             // Claim Status
             If filter.claimStatus <> ' ';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' CMstat = ''' + %Trim(filter.claimStatus) + ''' ';
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) +
                      ' And CMstat = ''' + %Trim(filter.claimStatus) + ''' ';
                EndIf;
             EndIf;

             // Policy Status
             If filter.policyStatus <> ' ';
                If subset = *Off;
                   subset = *On;
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ';
                EndIf;
                sqlRptWhere = %Trim(sqlRptWhere)
                 + ZSqlWherePolicyStatus(filter.policyStatus);
             EndIf;

             // From Accident Date Filters
             If filter.fromAccDate <> ' ' And
                filter.fromAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And CMacDt >= '  +
                                %Trim(%EditC(fromAccDt7:'X'));
                EndIf;
             EndIf;

             // To Accident Date Filters
             If filter.toAccDate <> ' ' And
                filter.toAccDate <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And CMacDt <= '  +
                                %Trim(%EditC(toAccDt7:'X'));
                EndIf;
             EndIf;

             // From Policy Effective Date Filters
             If filter.fromPolEffDt <> ' ' And
                filter.fromPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt >= '  +
                                %Trim(%EditC(fromPolEf7:'X'));
                EndIf;
             EndIf;

             // To Policy Effective Date Filter
             If filter.toPolEffDt <> ' ' And
                filter.toPolEffDt <> '00000000';
                If subset = *Off;
                   subset = *On;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                Else;
                   sqlRptWhere = %Trim(sqlRptWhere) + ' And ELpfDt <= '  +
                                %Trim(%EditC(toPolEf7:'X'));
                EndIf;
             EndIf;
          EndIf;

          // Establish sort Order
          If filter.sortOrder <> ' ';
             Select;
             When filter.sortOrder = 'Agency';
                sqlRptOrder = 'Order by agent#';
             When filter.sortOrder = 'Office';
                sqlRptOrder = 'Order by EMoff#';
             EndSl;
          EndIf;

          //*** Beg Chg *** 01/04/23 ******************************************
          //***sqlStmtStr = ZSqlAgtDate + ', ' + sqlAgtInfo1 + ' ' +
          sqlStmtStr = 'With' + ZSqlAgtDate + ', ' + sqlAgtInfo1 + ' ' +
          //*** End Chg *** 01/04/23 ******************************************
                       sqlAgtInfo2 + ' ' + sqlAgtInfo3 + ' ' +
                       sqlAgtinfo  + ' ' + ZSqlAgtName(*off)  + ', ' +
                       sqlClmInfo  + ' ' + sqlRptQry   + ' ' +
                       sqlRptWhere + ' ' + sqlRptOrder + ' ' +
                       'Limit 20';

          // Prepare the SQL statement
          Exec SQL prepare rptStmt4 from :sqlStmtStr;

          // Declare the SQL cursor
          Exec SQL declare rptCursor4 cursor for rptStmt4;

          // Run SQL
          Exec SQL open rptCursor4;

          // Get first record if there is one
          Exec SQL fetch next from rptCursor4 into :dsTop20Rpt;

          // Check to see if any records were fetched
          If sqlSTT <> '00000';
             ExSr FinishReport;
             moreRows = *off;
             Exec SQL close rptCursor4;
          Else;

             // If row was fetched then process
             DoU moreRows = *Off;

                // Build Report Record
                Exsr BldReportRcd;

                // Fetch next Record
                Exec SQL fetch next from rptCursor4 into :dsTop20Rpt;

                // check to see if there was another record fetched.
                If sqlstt <> '00000';
                   ExSr FinishReport;
                   moreRows = *Off;
                   Exec SQL close rptCursor4;
                EndIf;
             EndDo;
          EndIf;

       EndSr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"Top20ClaimsReport" : {' +
                 BuildJsonNode('groupName':filter.groupName) +
                 ', ' +
                 BuildJsonNode('group#':filter.group#)  +
                 ', ' +
                 BuildJsonNode('officeName':filter.officeName) +
                 ', ' +
                 BuildJsonNode('office#':filter.office#)  +
                 ', ' +
                 BuildJsonNode('agencyName':filter.agencyName) +
                 ', ' +
                 BuildJsonNode('agencyFein':filter.agencyFein) +
                 ', ' +
                 BuildJsonNode('agencyMod':filter.agencyMod) +
                 ', ' +
                 BuildJsonNode('insuredName':filter.insuredName) +
                 ', ' +
                 BuildJsonNode('insuredFein':filter.insuredFein) +
                 ', ' +
                 BuildJsonNode('policyStatus':filter.policyStatus) +
                 ', ' +
                 BuildJsonNode('claimStatus':filter.claimStatus) +
                 ', ' +
                 BuildJsonNode('fromAccidentDate':%editw(fromAccDt8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toAccidentDate':%editw(toAccDt8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('fromPolicyEffDate':%editw(fromPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('toPolicyEffDate':%editw(toPolEf8:
                 '  /  /    ')) +
                 ', ' +
                 BuildJsonNode('agencyType':filter.agencyType) +
                 ', ' +
                 BuildJsonNode('sortOrder':filter.sortOrder);

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // Build the request Parameters
          ExSr BldReqInfo;

          // Build Top 20 Claim Report Results
          ExSr BldReportResults;

       Endsr;

       //=================================================================
       // Finish The Report                                                 quests
       //=================================================================
       Begsr FinishReport;

          If count > 0;
             Select;
             When filter.sortOrder = 'Agency';
                json += '],' +
                   '"AgencyTotalsTotalIncurred":"' +
                   %Trim(%EditC(totIncurredAgt:'3'))+ '",' +
                   '"AgencyTotalsTotalPaid":"' +
                   %Trim(%EditC(totPaidAgt:'3'))+ '",' +
                   '"AgencyTotalsTotalRemainingReserves":"' +
                    %Trim(%EditC(totRemRsvAgt:'3')) + '"}]';
             When filter.sortOrder = 'Office';
                json += '],' +
                   '"OfficeTotalsTotalIncurred":"' +
                   %Trim(%EditC(totIncurredOff:'3'))+ '",' +
                   '"OfficeTotalsTotalPaid":"' +
                   %Trim(%EditC(totPaidOff:'3'))+ '",' +
                   '"OfficeTotalsTotalRemainingReserves":"' +
                    %Trim(%EditC(totRemRsvOff:'3')) + '"}]';
             EndSl;
          EndIf;

          ExSr WriteRptTotals;

       Endsr;

       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParameters;

          // Load the json tree
          inJson = inParms;
          docNode = yajl_string_load_tree(inJson : errMsg500);
          If errMsg500 = ' ';

             // Load the data structure with the parameter values
             // from the tree
             node = YAJL_object_find(docNode: 'groupName');
             filter.groupName   = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'group#');
             filter.group#      = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'officeName');
             filter.officeName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode:'office#');
             filter.office#     = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyName');
             filter.agencyName  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyFein');
             filter.agencyFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyMod');
             filter.agencyMod  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredName');
             filter.insuredName = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'insuredFein');
             filter.insuredFein = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'policyStatus');
             filter.policyStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'claimStatus');
             filter.claimStatus  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromAccidentDate');
             filter.fromAccDate = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toAccidentDate');
             filter.toAccDate  = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'fromPolicyEffDate');
             filter.fromPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'toPolicyEffDate');
             filter.toPolEffDt = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'agencyType');
             filter.agencyType = YAJL_get_string(node);

             node = YAJL_object_find(docNode: 'sortOrder');
             filter.sortOrder = YAJL_get_string(node);

             // Free up the space by removing the tree
             yajl_tree_free(docNode);

             //======= Validate the incoming parameters ====================

             //------------------------------------------------------
             // Office # must be valid              <<<<<<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.office# <> ' ';
                // If the office # valid?
                If ValidOffice(filter.office#) = *Off;
                   errMsg = %Trim(errMsg) + '"Invalid office#."             ';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Fein: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Fein: ' +
                                               'Invalid numeric data"';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             // Modifier
             If filter.agencyMod <> ' ';
                Monitor;
                   mod = %Dec(%Trim(filter.agencyMod):3:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Agency Mod: ' +
                                               'Invalid numeric data"';
                   Else;
                      errMsg = %Trim(errMsg) + ',"Agency Mod: ' +
                                               'Invalid numeric data"';
                   EndIf;
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
                If errMsg = ' ';
                   errMsg = %Trim(errMsg) + '"Invalid agency Fein/Mod."';
                Else;
                   errMsg = %Trim(errMsg) + ',"Invalid agency Fein/Mod."';
                EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) + '"Insured: Invalid numeric ' +
                                               'data."';
                   Else;
                      errMsg = %Trim(errMsg) + '",Insured: Invalid numeric '+
                                               'data."';
                   EndIf;
                   badData = *On;
                EndMon;
             EndIf;

             //------------------------------------------------------
             // Validate Policy Status                 <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.policyStatus <> ' '
              And filter.policyStatus <> 'All'
              And filter.policyStatus <> 'Active'
              And filter.policyStatus <> 'Cancelled'
              And filter.policyStatus <> 'Expired'
              And filter.policyStatus <> 'Pending'
              And filter.policyStatus <> 'Pending Cancellation';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Policy Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Policy Status." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Claim Status                  <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.claimStatus <> ' ' And filter.claimStatus <> 'O' And
                filter.claimStatus <> 'C';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Claim Status."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Claim Status." ';
                   EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                                '"From Accident Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"From Accident Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromAccDt8 <> 0;
                   fromAccDt7     = dtCYMD(fromAccDt8);
                   strfromAccDt7  = %EditC(fromAccDt7:'X');
                   If fromAccDt7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Accident Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Accident Date: Invalid Date." ';
                      EndIf;
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
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                                '"To Accident Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Accident Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toAccDt8 <> 0;
                   toAccDt7     = dtCYMD(toAccDt8);
                   strToAccDt7  = %EditC(toAccDt7:'X');
                   If toAccDt7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Accident Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Accident Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // From Policy Effective date must be valid <<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.fromPolEffDt = ' ' Or filter.fromPolEffDt = '00000000';
                fromPolEf8     = 0;
                fromPolEf7     = 0;
                strFromPolEf7  = '0000000';
             Else;
                Monitor;
                   fromPolEf8 = %Dec(filter.fromPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                        '"From Policy Effective Date: Invalid Numeric Data. " ';
                   Else;
                      errMsg = %Trim(errMsg) +
                        ',"From Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And fromPolEf8 <> 0;
                   fromPolEf7     = dtCYMD(fromPolEf8);
                   strfromPolEf7  = %EditC(fromPolEf7:'X');
                   If fromPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"From Policy Effective Date: Invalid Date." ';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"From Policy Effective Date: Invalid Date." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // To Policy Effective date must be valid <<<<<<<<<<<<<
             //------------------------------------------------------
             dateError = *Off;
             If filter.toPolEffDt = ' ' Or filter.toPolEffDt = '00000000';
                toPolEf8     = 0;
                toPolEf7     = 0;
                strToPolEf7  = '0000000';
             Else;
                Monitor;
                   toPolEf8 = %Dec(filter.toPolEffDt:8:0);
                On-Error;
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"To Policy Effective Date: Invalid Numeric Data." ';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"To Policy Effective Date: Invalid Numeric Data." ';
                   EndIf;
                   dateError= *On;
                EndMon;

                // Is the date valid?
                If Not dateError And toPolEf8 <> 0;
                   toPolEf7     = dtCYMD(toPolEf8);
                   strToPolEf7  = %EditC(toPolEf7:'X');
                   If toPolEf7 = 0;
                      If errMsg = ' ';
                         errMsg = %Trim(errMsg) +
                                '"To Policy Effective Date: Invalid Data."';
                      Else;
                         errMsg = %Trim(errMsg) +
                          ',"To Policy Effective Date: Invalid Data." ';
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Agency Type                   <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.agencyType <> ' ' And filter.agencyType <> '1' And
                filter.agencyType <> '2' And filter.agencyType <> '3';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Agency Type."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Agency Type." ';
                   EndIf;
             EndIf;

             //------------------------------------------------------
             // Validate Sort Order                    <<<<<<<<<<<<<
             //------------------------------------------------------
             If filter.sortOrder <> 'Agency' And
                filter.sortOrder <> 'Office';
                   If errMsg = ' ';
                      errMsg = %Trim(errMsg) +
                         '"Invalid Sort Order."';
                   Else;
                      errMsg = %Trim(errMsg) +
                         ',"Invalid Sort Order." ';
                   EndIf;
             EndIf;

          Else;
             errMsg = '"' + %Trim(errMsg500) + '"';
          EndIf;

       Endsr;

       //=================================================================
       // Write Report Totals
       //=================================================================
       Begsr WriteRptTotals;

          If count > 0;
             json += ',';
          EndIf;

          json +=
             '"ReportTotalsTotalIncurred":"' +
             %Trim(%EditC(totIncurredRpt:'3'))+ '",' +
             '"ReportTotalsTotalPaid":"' +
             %Trim(%EditC(totPaidRpt:'3'))+ '",' +
             '"ReportTotalsTotalRemainingReserves":"' +
              %Trim(%EditC(totRemRsvRpt:'3')) + '"';

       Endsr;

      /End-free

     pTop20ClaimsRpt   e


     p*===================================================================
     p* Create sql portion for Agent Date
     p*===================================================================
     pZSqlAgtDate      b
     dZSqlAgtDate      pi          1000a   varying
     dstrAgtDate       s           1000a   varying
      /Free
       strAgtDate = ' ' +
        'max_Agt_Date as (Select AHco#, AHfnd, ' +
        'AHemp#, AHdiv, AHfyr, AHfyr + 1900, Max(AHefdt) maxDate ' +
        'From wmahp ' +
        'Where AHefdt <= ' + %EditC(DateTo7(SystemDate()):'X') + ' ' +
        'And AHstat <> ''I'' ' +
        'Group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr)';
       Return strAgtDate;
      /End-free
     pZSqlAgtDate      e

     p*===================================================================
     p* Create sql portion for Agent Name
     p*===================================================================
     pZSqlAgtName      b
     dZSqlAgtName      pi          1000a   varying
     d inIncGrpInfo                   1n   const
     dstrAgtName       s           1000a   varying
      /Free
       strAgtName = ' ' +
        'Agt_Name as (Select a.*, c.AGname, c.AGfein, c.AGmod, ' +
        'd.FNname ';
       If inIncGrpInfo;
          strAgtName +=
           ', d.FNmAd1, d.FNmAd2, d.FNmCty, d.FNmSt, d.FNmZip, ' +
           'd.FNpAd1, d.FNpAd2, d.FNpCty, d.FNpSt, d.FNpZip ';
       Endif;
       strAgtName +=
        'From Agt_Agt# a join WMAAP b ' +
        'on a.AHco#=b.AAco# And a.AHfnd=b.AAfnd And a.agent#=b.AAagt# ' +
        'join WMAGP c on b.AAfein=c.AGfein And b.AAmod=c.AGmod ' +
        'join WMFNP d on a.AHco#=d.FNco# And a.AHfnd=d.FNfnd)';
       Return strAgtName;
      /End-free
     pZSqlAgtName      e

     p*===================================================================
     p* Create sql portion for Cancellation Date
     p*===================================================================
     pZSqlCxDate       b
     dZSqlCxDate       pi           500a   varying
     d inWMCMPprefix                 10a   varying options(*nopass) const
     dstrCxDate        s            500a   varying
     dstrWMCMPprefix   s             11a   varying inz
      /Free
       If %parms >= 1;
          strWMCMPprefix = inWMCMPprefix + '.';
       Endif;
       strCxDate = ' ' +
        '(Select' +
        '  Max(case' +
        '   when cx.EMcaD5 <> 0 then cx.EMcaD5' +
        '   when cx.EMcaD4 <> 0 then cx.EMcaD4' +
        '   when cx.EMcaD3 <> 0 then cx.EMcaD3' +
        '   when cx.EMcaD2 <> 0 then cx.EMcaD2' +
        '   when cx.EMcaD1 <> 0 then cx.EMcaD1' +
        '   else 0 end)' +
        ' From WMEMP cx' +
        '  Where' +
        '   cx.EMco# = ' + strWMCMPprefix + 'CMco# and' +
        '   cx.EMfnd = ' + strWMCMPprefix + 'CMfnd and' +
        '   cx.EMemp# = ' + strWMCMPprefix + 'CMemp# and' +
        '   cx.EMstrp = ''Y'') ' +
        'as cxDate ';
       Return strCxDate;
      /End-free
     pZSqlCxDate       e

     p*===================================================================
     p* Create sql portion for where - Policy Status
     p*===================================================================
     pZSqlWherePolicyStatus...
     p                 b
     dZSqlWherePolicyStatus...
     d                 pi           150a   varying
     d inPolicyStatus                25a   varying const
     dstrPolicyStatus  s            150a   varying inz(' ')
      /Free
       Select;
       When inPolicyStatus = 'All';
       When inPolicyStatus = 'Active';
          strPolicyStatus += ' ' + %char(sysDate7) +
           ' between ELpfdt and ELptdt and (cxDate = 0 or ' +
           ' cxDate is null or ' +
           ' cxDate >= ' + %char(sysDate7) + ')';
       When inPolicyStatus = 'Cancelled';
          strPolicyStatus += ' cxDate <> 0';
       When inPolicyStatus = 'Expired';
          strPolicyStatus += ' ELptDt <= ' + %char(sysDate7);
       When inPolicyStatus = 'Pending';
          strPolicyStatus += ' ELpfDt > ' + %char(sysDate7);
       When inPolicyStatus = 'Pending Cancellation';
          strPolicyStatus +=
           ' (cxDate is not null and cxDate > ' + %char(sysDate7) + ')';
       Endsl;
       Return strPolicyStatus;
      /End-free
     pZSqlWherePolicyStatus...
     p                 e
