     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H/TITLE STDASHBRD - Dashboard item processing
     h*option(*nodebugio: *list)
     h bnddir('WTBNDDIR':'YAJL')
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H* Description: Program will return information for indvidual    *
     H*               dashboard items.                                *
     H*                                                               *
     H* Created: 11/16/15  C:33  Project: SY285                       *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  10/15/25 C:079  Support: 119912/099                          *
     H*                  Correct SQL group select for regional losses *
     H*                  grid.                                        *
     H*                                                               *
     H*  12/12/24 C:342  Support: 115559 FMIC                         *
     H*                  1.Get unique records from WMSHP file when    *
     H*                  calculating premium and renew premium.       *
     H*                  2.Replace Null premium values from query     *
     H*                  and show 0 as SQLSTT was failing in loop.    *
     H*                                                               *
     H*  07/16/24 C:340  Project: WC2972                              *
     H*                  Update Hover Help/Tool Tip for Top 10 Agents.*
     H*                  Break out Group from Policy number as a -    *
     H*                  separate column on Policy Expiring Grid,     *
     H*                  Pending Audit grid and Quote Grids.          *
     H*                                                               *
     H*  03/03/22  C:33  Support: 97038
     H*                  Do not use BuildGiantJSON for 'chartJson'
     H*                                                               *
     H* 04/19/21  C:33  Support: 95419 (no comments)                  *
     H*                -Quoted Routine                                *
     H*                 - Qualify file name in RRN                    *
     H*                 - Get library name once                       *
     H*                                                               *
     H* 08/01/16  C:33  Support: 42575                                *
     H*                -PendingAudits                                 *
     H*                 - Omit Audit Request dates (0 & prior to 2001)*
     H*                -CxPolicies                                    *
     H*                 - Omit cancellation from deletion of division *
     H*                -RtvUserInfo                                   *
     H*                 - Return 'staffCode' for clt133               *
     H*****************************************************************
     fsmdbp     if   e             disk    rename(pdb01:pdb0p)

     D*entry
     DDashboardItems   pr                  extpgm('STDASHBRD')
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d outRtnCode                     1A
     DDashboardItems   pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d outRtnCode                     1A
      *
     DBldJsonCategory  pr
     D ioJson                     32767a   options(*varsize)
     D inLabels                   32767    const

     DBldJsonChart     pr
     D ioJson                     32767a   options(*varsize)
     D inOptions                  32767    const
     D inCategory                 32767    const
     D inDataSet                  32767    const

     DBldJsonDataSet   pr
     D ioJson                     32767a   options(*varsize)
     D inSeries                   32767    const

     DBldJsonLabels    pr
     D ioJson                     32767a   options(*varsize)
     D inLabel                      512    const

     DBldJsonSeries    pr
     D ioJson                     32767a   options(*varsize)
     D inTitle                      512    const
     D inData                     32767    const

     DBldJsonValues    pr
     D ioJson                     32767a   options(*varsize)
     D inValue                      512    const

     DBuildSQLInTeam   pr          5000
     D inField                       10    const
     D inPrefix                      25    const
     D inType                        10    const options(*nopass)

     DGetProcPtr       pr              *   procptr
     D inProcName                    15

     dPgmInCallStack   pr             1n
     d inPgm                         10    const

     dRtvUserInfo      pr            50
     d inUserInfo                    50    const
     d inRtnType                     10    const options(*nopass)
     d inType                        10    const options(*nopass)
      *
     d* Processing procedure
     dItemProcedure    pr         32767A   extproc(ptrProc)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dClosedClaims     pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dClosedSettle     pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dCxPolicies       pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dLargeLossRpt     pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dLitigationClaim  pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dNewClaims        pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dNewPremium       pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dPendingAudits    pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dPolicyExpiring   pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DProcessGenParm   pr           512
     D inGeneric                    512    const
     D ioStartPos                     5  0

     dQuoted           pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dRegionalLosses   pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dRenewalPremium   pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dSICLosses        pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     d TestChart       pr                  like(rtnData)
     d  inType                       15

     d TestGrid        pr                  like(rtnData)
     d  inType                       15

     dTop10AgentInfo   pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dTotalIncurred    pr                  like(rtnData)
     d inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     d RtvClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d QtmhWrStout     pr                  extproc('QtmhWrStout')
     d  outData                   32767A   options(*varsize) const
     d  outDataLen                   10I 0 const
     d  inErrorCode                8000A   options(*varsize)

     d #OfProcs        c                   20
      * Any procedure added to this list will need to be added to the
      *  compile time array... in the same order.
     d dsProcList      ds
      // Pointers for list of procedures
     d  ptrTestChart                   *   procptr inz(%paddr(TestChart))
     d  ptrTestGrid                    *   procptr inz(%paddr(TestGrid))
     d  ptrSettelment                  *   procptr inz(%paddr(ClosedSettle))
     d  ptrLargeLoss                   *   procptr inz(%paddr(LargeLossRpt))
     d  ptrLitClaim                    *   procptr inz(%paddr(LitigationClaim))
     d  ptrNewClaims                   *   procptr inz(%paddr(NewClaims))
     d  ptrNewPrem                     *   procptr inz(%paddr(NewPremium))
     d  ptrClosedClms                  *   procptr inz(%paddr(ClosedClaims))
     d  ptrTotalInc                    *   procptr inz(%paddr(TotalIncurred))
     d  ptrCxPolicies                  *   procptr inz(%paddr(CxPolicies))
     d  ptrPendAudits                  *   procptr inz(%paddr(PendingAudits))
     d  ptrExpPolicy                   *   procptr inz(%paddr(PolicyExpiring))
     d  ptrQuoted                      *   procptr inz(%paddr(Quoted))
     d  ptrRegionLoss                  *   procptr inz(%paddr(RegionalLosses))
     d  ptrRenewPrem                   *   procptr inz(%paddr(RenewalPremium))
     d  ptrSICLoss                     *   procptr inz(%paddr(SICLosses))
     d  ptrTop10Agent                  *   procptr inz(%paddr(Top10AgentInfo))
      // Redefine pointers as an array
     d   ptrArray                      *   overlay(dsProcList)
     d                                     procptr dim(#ofProcs)
     D aryProcList     s             15    dim(#ofProcs) ctdata perrcd(1)

     d errCode         ds                  qualified
     d  bytesProv                    10I 0 inz(0)
     d  bytesAvail                   10I 0

     d dsVarying       ds
     d  varyField                 32767A   varying inz
     d   returnLength                 5i 0 overlay(varyField)
     d   returnData               32767A   overlay(varyField:*next)

     d cltId           s              3A
     d rtnData         s          32767A
     d xmlData         s          32767A
     d jsonData        s          32767A
     d ptrProc         s               *   procptr
     d decStartPos     s              5  0 inz(1)
     d isScreenLoad    s              1n
     d isAjaxLoad      s              1n
     D inProc          s             15
     D inType          s             15
     DmaxColTtlWth2    c                   575
     DmaxColTtlWth4    c                   1175
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     D FrDate          s              7
     D ToDate          s              7
     d Group#          s              3
     d strNumbers      s             10    Inz('0123456789')
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     dssDropChoices    c                   '3D Column,Doughnut,3D Pie,2D Bar,-
     d                                     2D Column,Line,Area,2D Pie'
     dssDropValues     c                   'Column3D,Doughnut3D,Pie3D,Bar2D,-
     d                                     Column2D,Line,Area2D,Pie2D'
     dmsDropChoices    c                   '3D Column,2D Bar,2D Column,-
     d                                     Line,Area'
     dmsDropValues     c                   'MSColumn3D,MSBar2D,MSColumn2D,-
     d                                     ZoomLine,MSArea'

     D psds          esds                  extname(szq1p)

     D/copy sPrAtrium
     D/copy sPrDashBrd
     D/copy sPrDate
     D/copy sPrTeam
     D*copy sPrUsrPrms
     D/copy wPrUsrPrms
     D/copy wPrAgent
     D/copy wPrEmplr

      /Free
       RtvClientId(cltId);

       isAjaxLoad   = PgmInCallStack('STAJXPRC');
       isScreenLoad = PgmInCallStack('STUSRDRY');

       //                          1   5   10    5   20    5   30    5   40    5   50
       // input data is in format: ,procedure      ,type           ,

       Clear outJson;
       // Procedure used to build/update item
       inProc = ProcessGenParm(inGenParm:decStartPos);
       // Chart/Grid
       inType = ProcessGenParm(inGenParm:decStartPos);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       // From Date
       FrDate = ProcessGenParm(inGenParm:decStartPos);
       If %Trim(FrDate) = '0';
          FrDate = ' ';
       EndIf;

       If %Check(strNumbers:FrDate) > 0;
          FrDate = ' ';
       EndIf;

       // To Date
       ToDate = ProcessGenParm(inGenParm:decStartPos);
       If %Trim(ToDate) = '0';
          ToDate = ' ';
       EndIf;

       If %Check(strNumbers:ToDate) > 0;
          ToDate = ' ';
       EndIf;

       // Group#
       Group# = ProcessGenParm(inGenParm:decStartPos);
       If %Trim(Group#) = ' ';
          Group# = ' ';
       EndIf;

       If %Check(strNumbers:Group#) > 0;
          Group# = ' ';
       EndIf;

      *** End Add ***** 07/16/24 *** WC2972 ****************************
       If inRRN <> 999999999;
          Clear DBproc;
          Chain inRRN smdbp;
          If DBproc <> *blank;
             inProc = DBproc;
          Endif;
       Endif;
       If outRtnCode = 'X';
          outRtnCode = ' ';
          BuildJSON(outJSON :'outRtnCode' :'GenParm scan error');
          Return;
       Endif;

       If inProc <> *blanks;
          ptrProc = GetProcPtr(inProc);
          If ptrProc = *null;
             BuildJSON(outJSON :'outRtnCode' : %trim(inProc) + ' Not Found');
             BuildJSON(outJSON :'panelValue':'Data Not Found');
             BuildJSON(outJSON :'panelVisibility':'1');
             BuildJSON(outJSON :'gridVisibility':'0');
             Return;
          Endif;
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
          // *** rtnData = %trim(ItemProcedure(inType));
          rtnData = %trim(ItemProcedure(inType:FrDate:ToDate:Group#));
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       Endif;
       *inlr = *on;
       Return;
      /End-free

     p*=====================================================================
     p* BldJsonCategory - Builds the categories for the JSON created chart
     P*=====================================================================
     PBldJsonCategory  b
     DBldJsonCategory  pi
     D ioJson                     32767a   options(*varsize)
     D inLabels                   32767    const

     DstrReturn        s          32767a   varying inz
      /Free
       strReturn += '"categories":[{';
       strReturn += '"category":[';
       strReturn += %trim(inLabels);
       strReturn += ']}]';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonCategory  e

     p*=====================================================================
     p* BldJsonChart - Builds the chart for the JSON created chart
     P*=====================================================================
     PBldJsonChart     b
     DBldJsonChart     pi
     D ioJson                     32767a   options(*varsize)
     D inOptions                  32767    const
     D inCategory                 32767    const
     D inDataSet                  32767    const

     DstrReturn        s          32767a   varying inz
      /Free
       strReturn += '{"chart":';
       strReturn += %trim(inOptions);
       If inCategory <> *blank;
          strReturn += ',' + %trim(inCategory);
       Endif;
       If inDataSet <> *blank;
          strReturn += ',' + %trim(inDataSet);
       Endif;
       strReturn += '}';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonChart     e

     p*=====================================================================
     p* BldJsonDataSet - Builds the data set for the JSON created chart
     P*=====================================================================
     PBldJsonDataSet   b
     DBldJsonDataSet   pi
     D ioJson                     32767a   options(*varsize)
     D inSeries                   32767    const

     DstrReturn        s          32767a   varying inz
      /Free
       strReturn += '"dataset":['+ %trim(inSeries) + ']';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonDataSet   e

     p*=====================================================================
     p* BldJsonLabels - Builds the labels for the JSON created chart
     P*=====================================================================
     PBldJsonLabels    b
     DBldJsonLabels    pi
     D ioJson                     32767a   options(*varsize)
     D inLabel                      512    const

     DstrJson          s          32767a   inz
     DstrReturn        s          32767a   varying
      /Free
       BuildJson(strJson:'label':inLabel);

       strReturn = %trimr(ioJson);
       If strReturn <> *blank;
          strReturn += ',';
       Endif;
       strReturn += '{' + %trim(strJson) + '}';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonLabels    e

     p*=====================================================================
     p* BldJsonSeries - Builds the series for the JSON created chart
     P*=====================================================================
     PBldJsonSeries    b
     DBldJsonSeries    pi
     D ioJson                     32767a   options(*varsize)
     D inTitle                      512    const
     D inData                     32767    const

     DstrJson          s          32767a   inz
     DstrReturn        s          32767a   varying
      /Free
       BuildJson(strJson:'seriesname':inTitle);

       strReturn = %trimr(ioJson);
       If strReturn <> *blank;
          strReturn += ',';
       Endif;
       strReturn += '{' + %trim(strJson) + ',"data":[' + %trim(inData) + ']}';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonSeries    e

     p*=====================================================================
     p* BldJsonValues - Builds the values for the JSON created chart
     P*=====================================================================
     PBldJsonValues    b
     DBldJsonValues    pi
     D ioJson                     32767a   options(*varsize)
     D inValue                      512    const

     DstrJson          s          32767a   inz
     DstrReturn        s          32767a   varying
      /Free
       BuildJson(strJson:'value':inValue);

       strReturn = %trimr(ioJson);
       If strReturn <> *blank;
          strReturn += ',';
       Endif;
       strReturn += '{' + %trim(strJson) + '}';
       ioJson = strReturn;
       Return;
      /End-free
     PBldJsonValues    e

     p*=====================================================================
     p* BuildSQLInTeam - Builds the SQL team inclusion
     p*  Parm1   - Field comparison name
     p*  Parm2   - Prefix (e.g. 'where', 'and', *blank)
     p*  Parm3   - Optional, User (10A) or Code (3A)
     p*  Returns - XXyyyy In ('AA', 'BB',...)
     P*=====================================================================
     PBuildSQLInTeam   b
     DBuildSQLInTeam   pi          5000
     D inField                       10    const
     D inPrefix                      25    const
     D inType                        10    const options(*nopass)

     DstrType          s                   like(inType) inz('Code')
     DarrTeamList      s             10    dim(9999) inz
     DstrTeamList      s                   like(BuildSQLInTeam)
     DstrTeamMbr       s             10    inz
     Dindex            s              5  0 inz
     DintCount         s              5  0 inz
     DptrTeamList      s               *   inz

      /Free
       If %parms >= 3 and inType <> *blanks;
          strType = inType;
       Endif;

       ptrTeamList = %Addr(arrTeamList);
       GetUserTeamList(q1user :' ' :ptrTeamList :intCount :'Y');
       Select;
       When q1User = '/ROGULSKI';
          Return %trim(inField) + ' in (''DEM'')';
          Return *blank;
       When not IsLeader(q1User:' ');
          Clear arrTeamList;
          arrTeamList(1) = q1User;
          intCount = 1;
       When intCount = 0;
          Clear arrTeamList;
        //arrTeamList(1) = q1user;
          intCount = 1;
       Other;
        //Return *blank;
       Endsl;

       For index = 1 to intCount;
          If index > 1;
             strTeamList = %trim(strTeamList) + ',';
          Endif;
          Select;
          When strType = 'User';
             strTeamMbr = arrTeamList(index);
          When strType = 'Code';
             strTeamMbr = GetUsrStaf(arrTeamList(index));
          Other;
             strTeamMbr = strType;
          Endsl;
          strTeamList = %trim(strTeamList) + '''' + %trim(strTeamMbr) + '''';
       Endfor;

       Return %trim(inPrefix) + ' ' +
        %trim(inField) + ' in (' + %trim(strTeamList) + ')';
      /End-free
     PBuildSQLInTeam   e

     p*=====================================================================
     p* Closed claims within last week by adjustor
     p*  Parm1   - type of item
     P*=====================================================================
     PClosedClaims     b                   export
     DClosedClaims     pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D charFromDt      s              7    inz
     D charToDt        s              7    inz
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlWhere1    s           5000
     D strSqlWhere2    s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dtCompare       s              7    inz
     D #ofDays         s              5  0 inz(7)
     D dsData          ds
     D  name                          3
     D  value                         9  0
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 15 * 365;
       Endsl;
       dtCompare = %char((SystemDate-%days(#ofDays)):*cymd0);
       BuildJSON(outJSON :'itemType':'chart');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** strSqlWhere1 = 'where CHEFDT >= ' + dtCompare + ' ' +
      //***  'and CHOCST = ''C''';

       If inFrDt = ' ' and inToDt = ' ';
          inFrDt = dtCompare;
          inToDt = %char(SystemDate:*cymd0);
       EndIf;

       If inGrp# <> ' ';
          strSqlWhere1 = 'where CHEFDT >= ' + inFrDt +
                         ' and  CHEFDT <= ' + inToDt +
                         ' and  CHFND   = ' + inGrp# +
                         ' and  CHOCST = ''C''';
       Else;
          strSqlWhere1 = 'where CHEFDT >= ' + inFrDt +
                         ' and  CHEFDT <= ' + inToDt +
                         ' and  CHFND  in (Select GSFND From WXGSPDSH ' +
                                           'Where GSIEFL = ''I'')' +
                         ' and  CHOCST = ''C''';
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlWhere2 = 'where CMSTAT = ''C'' ' +
        %trim(BuildSQLInTeam('CMCLEX':'and'));
       strSqlCursor = 'with WMCHP_sub as ' +
        '(select CHCO#, CHFND, CHFYR, CHCASE, max(CHEFDT) from WMCHP ' +
        %trim(strSQLWhere1) + ' ' +
        '  group by CHCO#, CHFND, CHFYR, CHCASE)' +
        'select CMCLEX, count(CMCLEX) ' +
        ' from WMCMP ' +
        '  inner join WMCHP_sub ' +
        '   on CHCO# = CMCO# and CHFND  = CMFND  and '+
        '      CHFYR = CMFYR and CHCASE = CMCASE ' +
        %trim(strSQLWhere2) + ' ' +
        'group by CMCLEX ' +
        'order by count(CMCLEX) desc ' +
        'fetch first 10 rows only';

       Exec SQL Prepare sqlClose From :strSqlCursor;
       Exec SQL Declare cursorClose Cursor For sqlClose;
       Exec SQL Open cursorClose;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorClose into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + RtvUserInfo(%trim(name));
            values = %trim(values) + %trim(%editc(value:'Z'));
         Endif;
       Enddo;
       Exec SQL Close cursorClose;

       strToolTip = 'Top ' + %trim(%editc(index:'Z') +
        ' ajusters on your team.');
       if index < 1;
          values = '0';
          names = 'No Closed Claims';
          strToolTip = 'No Closed Claims within your team.';
       Endif;

       BuildJSON(outJSON:'chartNames':%trim(names));
       BuildJSON(outJSON:'chartValues':%trim(values));
       BuildJSON(outJSON:'chartToolTip':%trim(strToolTip));
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       If inFrDt = ' ' and inToDt = ' ';
          BuildJSON(outJSON:'panelValue':%trim(DBdesc) + ' ' +
           %trim(%editc(#ofDays:'Z')) + ' Days (since ' +
           %char(%date(dtCompare:*cymd0):*usa) + ')');
       Else;
          BuildJSON(outJSON:'panelValue': 'Closed Claims: ' +
           %char(%date(inFrDt:*cymd0):*usa) + ' - ' +
           %char(%date(inToDt:*cymd0):*usa) );
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       //BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
          BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PClosedClaims     E

     p*=====================================================================
     p* Closed Settlements in the last 30 days
     p*  Parm1   - type of item
     P*=====================================================================
     PClosedSettle     b                   export
     DClosedSettle     pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D charFromDt      s              7    inz
     D charToDt        s              7    inz
     D dtTo            s               d   inz
     D date7           s              7  0 inz
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlWhere     s           5000
     D strToolTip      s            500    inz
     D names           s           5000    inz
     D values          s            100    inz
     D dsData          ds                  qualified
     D  date                          7  0
     D  value                         9  0
     D#ofDays          s              5  0 inz(30)
     daryDates         s              7  0 dim(31) inz
     daryValues        s              9  0 dim(31) inz(0)
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          dtTo = CvtToDate(05312012);
       Other;
          dtTo = SystemDate;
       Endsl;
       Monitor;
       charFromDt = %char((dtTo-%days(#ofDays)):*cymd0);
       charToDt = %char((dtTo-%days(1)):*cymd0);
       For index = 1 to #ofDays;
          date7 = %int(%char((dtTo-%days(index)):*cymd0));
          aryDates(index) = %int(%char((dtTo-%days(index)):*cymd0));
       Endfor;
       On-Error;
       BuildJSON(outJSON :'fromDate':charFromDt);
       BuildJSON(outJSON :'toDate':charToDt);
       BuildJSON(outJSON :'index':%editc(index:'Z'));
       BuildJSON(outJSON:'panelValue':'Error Captured - Check JSON Return');
       Return *blanks;
       Endmon;

       BuildJSON(outJSON :'itemType':'Line');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** strSqlWhere = 'where LLSTDT between ' + %trim(charFromDt) +
      //***  ' and ' + %trim(charToDt);
       If inGrp# <> ' ';
          strSqlWhere = 'where LLSTDT between ' + %trim(charFromDt) +
           ' and ' + %trim(charToDt) + ' and LLFND = ' + inGrp# + '';
       Else;
          strSqlWhere = 'where LLSTDT between ' + %trim(charFromDt) +
           ' and ' + %trim(charToDt) + ' and LLFND in ' +
                                         '(Select GSFND From WXGSPDSH '+
                                               'Where GSIEFL = ''I'')';
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlCursor = 'select LLSTDT, count(LLSTDT) ' +
        'from WMLLP ' +
        ' inner join WMCMP ' +
        '  on LLCO# = CMCO# and LLFND  = CMFND  and '+
        '     LLFYR = CMFYR and LLCASE = CMCASE ' +
        %trim(strSQLWhere) + ' ' +
        'group by LLSTDT ' +
        'order by count(LLSTDT) desc ';

       Exec SQL Prepare sqlSettle From :strSqlCursor;
       Exec SQL Declare cursorSettle Cursor For sqlSettle;
       Exec SQL Open cursorSettle;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorSettle into :dsData;
          If SQLSTT = '00000';
            index = %lookup(dsData.date:aryDates:1);
            If index > 0;
               aryValues(index) = dsData.value;
            Endif;
         Endif;
       Enddo;
       Exec SQL Close cursorSettle;

       strToolTip = 'Settlements in the last 30 days.';
       For index = #ofDays downto 1;
         If index < #ofDays;
            names = %trim(names) + ',';
            values = %trim(values) + ',';
         Endif;
         names = %trim(names) + AlphaUSA(CvtToDate(aryDates(index)));
         values = %trim(values) + %trim(%editc(aryValues(index):'Z'));
       Endfor;

       BuildJSON(outJSON :'chartNames':%trim(names));
       BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc) + ' - ' +
        %char(%date(charFromDt:*cymd0):*usa) + '-' +
        %char(%date(charToDt:*cymd0):*usa));
       //BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PClosedSettle     E

     p*=====================================================================
     p* Cancellations in the last 30 days by reason code
     p*  Parm1   - type of item
     P*=====================================================================
     PCxPolicies       b                   export
     DCxPolicies       pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     D inFrDt                         7
     D inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlWhere1    s           5000
     D strSqlWhere2    s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dtCompare       s              7    inz
     D #ofDays         s              5  0 inz(30)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     DjsonCategories   s          32767A   inz
     djsonChart        s          32767A   inz
     djsonDataSet      s          32767A   inz
     djsonLabels       s          32767A   inz
     djsonOptions      s          32767A   inz
     D strOptions      s           5000    inz
     DdtCompareFr      s               d   inz
     DdtCompareTo      s               d   inz
     D*** dsData          ds
     D***  name                          2
     D***  value                         9  0

     D dsData          ds
     D  name                         40
     D  value                         9  0
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 15 * 365;
       Endsl;

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** dtCompare = %char((SystemDate-%days(#ofDays)):*cymd0);
      *** BuildJSON(outJSON :'itemType':'chart');
      *** strSqlWhere1 = 'where CXCXDT >= ' + dtCompare + ' ' +
      ***  'and CXREDT = 0 and CXRSN <> '' ''';

      *** strSqlCursor = 'select CXRSN, count(CXRSN) ' +
      ***  'from WDCXP ' +
      ***  %trim(strSQLWhere1) + ' ' +
      ***  'group by CXRSN ' +
      ***  'order by count(CXRSN) desc ' +
      ***  'fetch first 10 rows only';

       BuildJSON(outJSON :'itemType':'chart');

       If inFrDt <> ' ' and inToDt <> ' ';
          dtCompareFr = %Date(inFrDt:*CYMD0);
          dtCompareTo = %Date(inToDt:*CYMD0);
       Else;
          inFrDt      = %char((SystemDate-%days(#ofDays)):*cymd0);
          inToDt      = %char(SystemDate:*cymd0);
          dtCompareFr = %Date(inFrDt:*CYMD0);
          dtCompareTo = %Date(inToDt:*CYMD0);
       EndIf;

       If inGrp# <> ' ';
          strSqlWhere1 = 'where CXCXDT >= ' + inFrDt + ' ' +
            'and CXCXDT <= ' + inToDt + ' '                +
            'and CXFND = ' + inGrp# + ' '                  +
            'and CXREDT = 0 and CXRSN <> '' ''';
       Else;
          strSqlWhere1 = 'where CXCXDT >= ' + inFrDt + ' ' +
            'and CXCXDT <= ' + inToDt + ' ' +
            'and CXFND in (Select GSFND From WXGSPDSH '    +
                          'Where GSIEFL = ''I'') '         +
            'and CXREDT = 0 and CXRSN <> '' ''';
       EndIf;

         BuildJSON(outJSON :'panelValue':%trim(DBdesc) + ': ' +
            %char(dtCompareFr:*usa) + ' - ' +
            %char(dtCompareTo:*usa));

       strSqlCursor = 'With Can_Sum as ' +
        '(Select CXRSN, Count(CXRSN) CanCnt ' +
        'From WDCXP ' +
         %Trim(strSQLWhere1) + ' ' +
        'Group by CXRSN ' +
        'Order by Count(CXRSN) Desc), ' +
        'Can_Rsn as ' +
        '(Select Distinct CRCO#, CRKEY, CRNCCI ' +
        'From WTCRP '                            +
        'Where CRKEY in (select CXRSN From Can_Sum))' +
        'Select GTDesc, CanCnt '                      +
        'From Can_Sum '                               +
        'Inner Join Can_Rsn On CXRSN = CRKEY '        +
        'Inner Join WTGTP On CRNCCI = GTKEY and '     +
        'GTTBID = ''CXCD'' and GTSTAT <> ''I'' '      +
        'Fetch First 10 Rows Only';

      *** End Chg ***** 07/16/24 *** WC2972 ****************************

       Exec SQL Prepare sqlCxP From :strSqlCursor;
       Exec SQL Declare cursorCxP Cursor For sqlCxP;
       Exec SQL Open cursorCxP;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorCxP into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + %trim(name);
            values = %trim(values) + %trim(%editc(value:'Z'));

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
            StartJSONelemnt(jsonDataSet);
            BuildJson(jsonDataSet:'label':names);
            BuildJson(jsonDataSet:'value':%trim(%editc(value:'3')));
            EndJSONelemnt(jsonDataSet);
      *** End Chg ***** 07/16/24 *** WC2972 ****************************

         Endif;
       Enddo;
       Exec SQL Close cursorCxP;

       if index < 1;
          values = '0';
          names = 'No Policies Cancelled During Time Period';
          strToolTip = 'No Cancelled Policies.';
       Endif;

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       // BuildJSON(outJSON :'chartNames':%trim(names));
       // BuildJSON(outJSON :'chartValues':%trim(values));
       Exsr BldJsonData;
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'sqlCursor':strSqlCursor);
       Exsr BuildOptions;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;

      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       //==================================================================
       // Build JSON chart data
       //==================================================================
       Begsr BldJSONData;
        // Options
        StartJSONelemnt(jsonOptions);
        BuildJson(jsonOptions:'labelDisplay':'ROTATE');
        BuildJson(jsonOptions:'slantLabels':'1');
        EndJSONelemnt(jsonOptions);

        // Dataset
        jsonDataSet = '"data":['+ %trim(jsonDataSet) + ']';

        // Chart
        BldJsonChart(jsonChart:jsonOptions:jsonCategories:jsonDataSet);
        outJson = %trim(outJSON) + ', "chartJSON":' + jsonChart;
       Endsr;

       //===================================================================
       Begsr BuildOptions;
        strOptions = %trim(strOptions) + ' labelDisplay=''Stagger''';
        strOptions = %trim(strOptions) + ' staggerLines=''2''';
        BuildJSON(outJSON :'chartOptions':%trim(strOptions));
       Endsr;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
      /End-Free

     PCxPolicies       E

     p*=====================================================================
     p* GetProcPtr  - Get relevant procedure pointer
     p*  Parm1   - Name of the procedure
     p*  Returns - pointer to the procedure
     P*=====================================================================
     PGetProcPtr       b                   export
     DGetProcPtr       pi              *   procptr
     D inProcName                    15
      *
     d decProcPtr#     s              5  0
     D ptrProc         s               *   procptr inz

      /Free
        decProcPtr# = %lookup(inProc:aryProcList:1);
        Select;
        When inProcName = ' ';
        When decProcPtr# = 0;
        Other;
           ptrProc = ptrArray(decProcPtr#);
        EndSl;

        Return ptrProc;
      /End-Free

     PGetProcPtr       E

     p*=====================================================================
     p* Large Loss Report count by adjustor - Top 10
     p*  Parm1   - type of item
     P*=====================================================================
     PLargeLossRpt     b                   export
     DLargeLossRpt     pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D decCompare      s             11  0 inz(10000)
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlSum       s           5000
     D strSqlWhere     s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dsData          ds
     D  name                          3
     D  value                         9  0
      *
      /Free

       BuildJSON(outJSON :'itemType':'chart');
       strSqlSum = 'SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        ' CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10 +' +
        ' CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        ' CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10)';
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** strSqlWhere = 'where CMSTAT = ''O'' ' +
      //***  %trim(BuildSQLInTeam('CMCLEX':'and'));
       If inGrp# <> ' ';
          strSqlWhere = 'where CMSTAT = ''O'' and CMFND = ' + inGrp# + ' ' +
           %trim(BuildSQLInTeam('CMCLEX':'and'));
       Else;
          strSqlWhere = 'where CMSTAT = ''O'' ' +
                        ' and  CMFND  in (Select GSFND From WXGSPDSH ' +
                                          'Where GSIEFL = ''I'') ' +
           %trim(BuildSQLInTeam('CMCLEX':'and'));
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlCursor = 'select a.CMCLEX, count(a.CMCLEX) ' +
        'from WMCMP a ' +
        'where (a.CMCO#,a.CMFND,a.CMFYR,a.CMCASE) in'+
        ' (select b.CMCO#,b.CMFND,b.CMFYR,b.CMCASE' +
        '  from WMCMP b '+
        %trim(strSQLWhere) + ' ' +
        '  group by b.CMCO#,b.CMFND,b.CMFYR,b.CMCASE,b.CMCLEX' +
        '  having ' + %trim(strSqlSum) + ' > ' + %trim(%editc(decCompare:'Z')) +
        '  order by b.CMCO#,b.CMFND,b.CMFYR,b.CMCASE)'+
        'group by CMCLEX ' +
        'order by count(CMCLEX) desc ' +
        'fetch first 10 rows only';

       Exec SQL Prepare sqlLrg From :strSqlCursor;
       Exec SQL Declare cursorLrg Cursor For sqlLrg;
       Exec SQL Open cursorLrg;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorLrg into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + RtvUserInfo(%trim(name));
            values = %trim(values) + %trim(%editc(value:'Z'));
         Endif;
       Enddo;
       Exec SQL Close cursorLrg;

       strToolTip = 'Top ' + %trim(%editc(index:'Z') +
        ' ajusters on your team.');
       if index < 1;
          values = '0';
          names = 'No Incurred';
          strToolTip = 'No Incurred within your team.';
       Endif;

       BuildJSON(outJSON :'chartNames':%trim(names));
       BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc) + ' ' +
        %trim(%editc(decCompare:'J')));
       //BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PLargeLossRpt     E

     p*=====================================================================
     p* Litigation Claims count by adjustor - Top 10
     p*  Parm1   - type of item
     P*=====================================================================
     PLitigationClaim  b                   export
     DLitigationClaim  pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlWhere     s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dsData          ds
     D  name                          3
     D  value                         9  0
      *
      /Free

       BuildJSON(outJSON :'itemType':'chart');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** strSqlWhere = 'where LLLITG = ''Y'' and CMSTAT = ''O'' ' +
      //***  %trim(BuildSQLInTeam('CMCLEX':'and'));

       If inGrp# <> ' ';
          strSqlWhere = 'where LLLITG = ''Y'' and CMSTAT = ''O'' ' +
                        'and CMFND = ' + inGrp# + ' ' +
                        %trim(BuildSQLInTeam('CMCLEX':'and'));
       Else;
          strSqlWhere = 'where LLLITG = ''Y'' and CMSTAT = ''O'' ' +
                        ' and CMFND in (Select GSFND From WXGSPDSH ' +
                                        'Where GSIEFL = ''I'') ' +
                        %trim(BuildSQLInTeam('CMCLEX':'and'));
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlCursor = 'select CMCLEX, count(LLLITG) ' +
        'from WMLLP ' +
        ' inner join WMCMP ' +
        '  on LLCO# = CMCO# and LLFND  = CMFND  and '+
        '     LLFYR = CMFYR and LLCASE = CMCASE ' +
        %trim(strSQLWhere) + ' ' +
        'group by CMCLEX ' +
        'order by count(LLLITG) desc ' +
        'fetch first 10 rows only';

       Exec SQL Prepare sqlLit From :strSqlCursor;
       Exec SQL Declare CursorLit Cursor For sqlLit;
       Exec SQL Open CursorLit;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From CursorLit into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + RtvUserInfo(%trim(name));
            values = %trim(values) + %trim(%editc(value:'Z'));
         Endif;
       Enddo;
       Exec SQL Close CursorLit;

       strToolTip = 'Top ' + %trim(%editc(index:'Z') +
        ' ajusters on your team.');
       if index < 1;
          values = '0';
          names = 'No Litigated Claims';
          strToolTip = 'No Open Litigated Claims within your team.';
       Endif;

       BuildJSON(outJSON :'chartNames':%trim(names));
       BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       //BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PLitigationClaim  E

     p*=====================================================================
     p* New claims within last week by adjustor
     p*  Parm1   - type of item
     P*=====================================================================
     PNewClaims        b                   export
     DNewClaims        pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlWhere1    s           5000
     D strSqlWhere2    s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dtCompare       s              7    inz
     D #ofDays         s              5  0 inz(7)
     D strPanelHdr     s            500    inz
     D dsData          ds
     D  name                          3
     D  value                         9  0
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 15 * 365;
       Endsl;
       dtCompare = %char((SystemDate-%days(#ofDays)):*cymd0);
       BuildJSON(outJSON :'itemType':'chart');
       strSqlWhere1 = 'where CHOCST = ''O''';
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************

       If inFrDt = ' ' and inToDt = ' ';
          inFrDt = %char((SystemDate-%days(#ofDays)):*cymd0);
          inToDt = %char((SystemDate):*cymd0);
       EndIf;

       strPanelHdr = 'New Claims - ' +
                     %char(%date(inFrDt:*cymd0):*usa) + ' - ' +
                     %char(%date(inToDt:*cymd0):*usa) ;

       //*** strSqlWhere2 = 'where CMSTAT = ''O'' ' +
       //***  'and minEFDT >= ' + dtCompare + ' ' +
       //***  %trim(BuildSQLInTeam('CMCLEX':'and'));

       If inGrp# <> ' ';
          strSqlWhere2 = 'where CMSTAT = ''O'' ' +
           'and minEFDT >= ' + inFrDt + ' and minEFDT <= ' + inToDt + ' ' +
           'and CMFND = ' + inGrp# + ' ' +
           %trim(BuildSQLInTeam('CMCLEX':'and'));
       Else;
          strSqlWhere2 = 'where CMSTAT = ''O'' ' +
           'and minEFDT >= ' + inFrDt + ' and minEFDT <= ' + inToDt + ' ' +
           'and CMFND in (Select GSFND From WXGSPDSH ' +
                          'Where GSIEFL = ''I'') '     +
           %trim(BuildSQLInTeam('CMCLEX':'and'));

       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlCursor = 'with WMCHP_sub as ' +
        '(select CHCO#, CHFND, CHFYR, CHCASE, min(CHEFDT) minEFDT from WMCHP ' +
        %trim(strSQLWhere1) + ' ' +
        '  group by CHCO#, CHFND, CHFYR, CHCASE)' +
        'select CMCLEX, count(CMCLEX) ' +
        ' from WMCMP ' +
        '  inner join WMCHP_sub ' +
        '   on CHCO# = CMCO# and CHFND  = CMFND  and '+
        '      CHFYR = CMFYR and CHCASE = CMCASE ' +
        %trim(strSQLWhere2) + ' ' +
        'group by CMCLEX ' +
        'order by count(CMCLEX) desc ' +
        'fetch first 10 rows only';

       Exec SQL Prepare sqlNewC From :strSqlCursor;
       Exec SQL Declare cursorNewC Cursor For sqlNewC;
       Exec SQL Open cursorNewC;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorNewC into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + RtvUserInfo(%trim(name));
            values = %trim(values) + %trim(%editc(value:'Z'));
         Endif;
       Enddo;
       Exec SQL Close cursorNewC;

       strToolTip = 'Top ' + %trim(%editc(index:'Z') +
        ' ajusters on your team.');
       if index < 1;
          values = '0';
          names = 'No New Claims';
          strToolTip = 'No New Claims within your team.';
       Endif;

       BuildJSON(outJSON :'chartNames':%trim(names));
       BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       //*** BuildJSON(outJSON :'panelValue':%trim(DBdesc) + ' ' +
       //***  %trim(%editc(#ofDays:'Z')) + ' Days (since ' +
       //***  %char(%date(dtCompare:*cymd0):*usa) + ')');

       BuildJSON(outJSON :'panelValue':%trim(strPanelHdr));
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PNewClaims        E

     p*=====================================================================
     p* New Premium by Month
     p*  Parm1   - type of item
     P*=====================================================================
     PNewPremium       b                   export
     DNewPremium       pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     D inFrDt                         7
     D inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     Dheading          s           1000    inz
     DstrSqlCursor     s           5000    inz
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     D#ofDays          s              5  0 inz(90)
     DcharCompareFr    s              7    inz
     DcharCompareTo    s              7    inz
     DdtCompareFr      s               d   inz
     DdtCompareTo      s               d   inz
     Dindex            s              5  0 inz
     DjsonCategories   s          32767A   inz
     djsonChart        s          32767A   inz
     djsonDataSet      s          32767A   inz
     djsonLabels       s          32767A   inz
     djsonOptions      s          32767A   inz
     djsonSeries       s          32767A   inz
     djsonValue1       s          32767A   inz
     djsonValue2       s          32767A   inz
     DlastMonth        s              2  0 inz
     DnextMonth        s              2  0 inz

     DdsData           ds
     D dsCYYMM                        5
     D dsPrem                        18  2
     D*dsRnwPrem                     18  2
     DaryData          ds                  dim(12) qualified inz
     D*dsMonth                        2
     D dsMonth                        2  0
     D dsPrem                        18  2
     D*dsRnwPrem                     18  2
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 6 * 365;
       Endsl;

       // Past effective
       dtCompareFr = GetFirstDay((SystemDate-%days(#ofDays)));
       // Forward one year
       dtCompareTo = GetLastDay((dtCompareFr+%months(11)));
       // Convert to character for SQL
       charCompareTo = %char(dtCompareTo:*cymd0);
       charCompareFr = %char(dtCompareFr:*cymd0);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       If inFrDt <> ' ' and inToDt <> ' ';
         charCompareFr = inFrDt;
         charCompareTo = inToDt;
         dtCompareFr = %Date(inFrDt:*CYMD0);
         dtCompareTo = %Date(inToDt:*CYMD0);
       EndIF;

       If inGrp# <> ' ';
          strSqlWhere = ' and ELFND = ' + inGrp# + '';
       Else;
          strSqlWhere = ' and ELFND in ' +
                        '(Select GSFND From WXGSPDSH '       +
                              'Where GSIEFL = ''I'')';
       EndIf;
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       strToolTip = 'One year of new policies effective by month using ' +
        %char(#ofDays) + ' days in the past as the starting point.';
       heading = %trim(DBdesc) + ' (' +
        %char(dtCompareFr:*usa) + '-' + %char(dtCompareTo:*usa) + ')';
       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = ' and ELFND = ' + %editc(UserGrp(q1User):'X');
       Endif;
       strSqlCursor = 'with ' +
        'WDELP_prv as ' +
        ' (select ELCO# prv_Co, ELFND prv_Fnd, ELEMP# prv_Emp, ' +
        '         ELDIV prv_Div, ELFYR prv_Fyr, ELANPR prv_AnPr, ' +
        '         ELFAF prv_FAF, ELAAPR prv_AaPr ' +
        '  from WDELP ' +
        '  inner join WMSHP ' +
        '   on ELCO# = SHCO# and ELFND = SHFND and ELFYR = SHFYR and ' +
        '      ELEMP# = SHEMP# and ELDIV = SHDIV ' +
        '  where SHSTAT in (''3'', ''4'')), ' +
        'nonTestGrps as ' +
        ' (select FNCO#, FNFND ' +
        '  from WMFNP ' +
        '  where FNTEST <> ''Y'') ' +
        'select substring(digits(ELPFDT),1,5) as CYMMD, ' +
        ' sum(case when ELFAF = ''A'' then ELAAPR else ELANPR end) ' +
        '  as premium ' +
        ' from WDELP ' +
        ' inner join WMSHP ' +
        '  on ELCO# = SHCO# and ELFND = SHFND and ELFYR = SHFYR and ' +
        '     ELDIV = SHDIV and ELEMP# = SHEMP# ' +
        ' inner join nonTestGrps ' +
        '  on ELCO# = FNCO# and ELFND = FNFND ' +
        ' left exception join WDELP_prv ' +
        '  on ELCO# = prv_Co and ELFND = prv_Fnd and ' +
        '     ELFYR = prv_Fyr+1 and ' +
        '     ELEMP# = prv_Emp and ELDIV = prv_Div ' +
        ' where ELPFDT between ' + charCompareFr + ' and ' + charCompareTo +
        %trimr(strSQLWhere) + ' ' +
        '  and SHSTAT in (''3'', ''4'') ' +
        ' group by substring(digits(ELPFDT),1,5) ' +
        ' order by CYMMD';

       lastMonth = %dec(%subst(charCompareTo:4:2):2:0);
       nextMonth = %dec(%subst(charCompareFr:4:2):2:0);
       Exec SQL Prepare sqlNewPrm From :strSqlCursor;
       Exec SQL Declare cursorNewPrm Cursor For sqlNewPrm;
       Exec SQL Open cursorNewPrm;
       Dou SQLSTT <> '00000' and SQLSTT <> '01003';
          Exec SQL Fetch next From cursorNewPrm into :dsData;
          If SQLSTT = '00000' or SQLSTT = '01003';
             Dow %dec(%subst(dsCYYMM:4:2):2:0) <> nextMonth;
                index += 1;
                aryData(index).dsMonth = nextMonth;
                Exsr GetNextMonth;
             Enddo;
             index += 1;
             aryData(index).dsMonth    = nextMonth;
             aryData(index).dsPrem     = dsPrem;
           //aryData(index).dsRnwPrem  = dsRnwPrem;
             Exsr GetNextMonth;
          Endif;
       Enddo;
       Exec SQL Close cursorNewPrm;
       Monitor;
       For index = 1 to 12;
          Select;
          When aryData(index).dsMonth <> 0;
             Iter;
          When nextMonth = 13;
             nextMonth = 1;
          Endsl;
          aryData(index).dsMonth = nextMonth;
          Exsr GetNextMonth;
       Endfor;
       On-Error;
       BuildJSON(outJSON :'Error Info - index':%char(index));
       BuildJSON(outJSON :'Error Info - nextMonth':%char(nextMonth));
       BuildJSON(outJSON :'Error Info - lastMonth':%char(lastMonth));
       Return returnInfo;
       Endmon;

       BuildJSON(outJSON :'itemType':'chart');
       BuildJSON(outJSON :'chartType':'Column2D');
       Exsr BuildJSONData;
       //***  Begin Add  ***  03/03/22  ****************
       //BuildBigJSON(outJSON :'chartJSON':jsonChart);
       outJson = %trim(outJSON) + ', "chartJSON":' + jsonChart;
       strToolTip = ' ';
       //***  End   Add  ***  03/03/22  ****************
       BuildJSON(outJSON :'dropValues':ssDropValues);
       BuildJSON(outJSON :'dropChoices':ssDropChoices);
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':heading);
       BuildBigJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       inToDt = charCompareTo;
       inFrDt = charCompareFr;
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;

       //==================================================================
       // Build JSON chart data
       //==================================================================
       Begsr BuildJSONData;
        // Options
        StartJSONelemnt(jsonOptions);
        BuildJson(jsonOptions:'numberPrefix':'$');
       //BuildJson(jsonOptions:'paletteColors':'#0075c2,#1aaf5d,#f2c500');
        BuildJson(jsonOptions:'rotateValues':'1');
        BuildJson(jsonOptions:'placeValuesInside':'1');
        EndJSONelemnt(jsonOptions);

        // Labels and values
        For index = 1 to 12;
           StartJSONelemnt(jsonDataSet);
           BuildJson(jsonDataSet:'label':AlphaMonth(aryData(index).dsMonth));
           BuildJson(jsonDataSet:'value':%editc(aryData(index).dsPrem:'L'));
           EndJSONelemnt(jsonDataSet);
        Endfor;

        // Categories
        BldJsonCategory(jsonCategories:jsonLabels);
        // Dataset
        jsonDataSet = '"data":[' + %trim(jsonDataSet) + ']';
        // Chart
        BldJsonChart(jsonChart:jsonOptions:jsonCategories:jsonDataSet);
       Endsr;
       //==================================================================
       // Get the next month (nextMonth)
       //==================================================================
       Begsr GetNextMonth;
        nextMonth += 1;
        If nextMonth = 13;
           nextMonth = 1;
        Endif;
       Endsr;
      /End-Free

     PNewPremium       E

     p*=====================================================================
     p* Pending Audits
     p*  Parm1   - type of item
     P*=====================================================================
     PPendingAudits    b                   export
     DPendingAudits    pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
     D#ofCols          s              5  0 inz(4)
     D#ofRows          s              5  0 inz(7)
     DdtToday          s              7    inz
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(290)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     D*** colWth2                        5  0 overlay(dsColWth:*next) inz(165)
     D*** colWth3                        5  0 overlay(dsColWth:*next) inz(30)
     D*** colWth4                        5  0 overlay(dsColWth:*next) inz(90)
     D*** colWth5                        5  0 overlay(dsColWth:*next) inz(75)
     D*** colWth6                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(30)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(90)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(75)
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     D colWth7                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
     DdtFirst          s               d   inz(d'2000-01-01')
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free

       dtToday   = %char(SystemDate:*cymd0);

       // Create grid headings
       strToolTip = 'Pending Audits by longest delay.';
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************

       If inFrDt = ' ' and inToDt = ' ';
          inFrDt = %char(dtFirst:*cymd0);
          inToDt = %char(SystemDate:*cymd0);
       EndIf;

      *** headings = %trim(DBdesc) + ',Policy,CX,Request Date';

         headings = %trim(DBdesc) + ': ' +
                    %char(%Date(inFrDt:*CYMD0):*usa) + '-' +
                    %char(%Date(inToDt:*CYMD0):*usa) +
                    ',Group,Policy,CX,Request Date';

       strSqlWhere = 'AURCVD = 0 and AURQDT >= ' + inFrDt + ' and ' +
                                    'AURQDT <= ' + inToDt ;

       //*** If UsrRstFlg(q1User) = 'Y';
       //***   strSqlWhere = 'and AUFND = ' + %editc(UserGrp(q1User):'X');
       //*** Endif;
       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = %Trim(strSqlWhere) + 'and AUFND = ' +
                        %editc(UserGrp(q1User):'X');
       Else;
          If inGrp# <> ' ';
             strSqlWhere = %Trim(strSqlWhere) + ' and AUFND = ' + inGrp# + '';
          Else;
             strSqlWhere = %Trim(strSqlWhere) + ' and AUFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
          EndIf;
       Endif;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       sql = 'with mast_dtl as ' +
        '(select EMNAME, ELCO#, ELFND, ELEMP#, ELDIV, ELFYR, ELPFDT,' +
        '  case' +
        '   when EMEFD5 <= ELPFDT and EMEFD5 <> 0 and EMCAD5 <= ELPTDT' +
        '    and EMCAD5 <> 0 then ''CX''' +
        '   when EMEFD4 <= ELPFDT and EMEFD4 <> 0 and EMCAD4 <= ELPTDT' +
        '    and EMCAD4 <> 0 then ''CX''' +
        '   when EMEFD3 <= ELPFDT and EMEFD3 <> 0 and EMCAD3 <= ELPTDT' +
        '    and EMCAD3 <> 0 then ''CX''' +
        '   when EMEFD2 <= ELPFDT and EMEFD2 <> 0 and EMCAD2 <= ELPTDT' +
        '    and EMCAD2 <> 0 then ''CX''' +
        '   when EMEFD1 <= ELPFDT and EMEFD1 <> 0 and EMCAD1 <= ELPTDT' +
        '    and EMCAD1 <> 0 then ''CX''' +
        '   else ''  ''' +
        '   end as CX,' +
        '   digits(rrn(WDELP)) as rrn_WDELP, ' +
        '   digits(ELCO#) concat digits(ELFND) concat digits(ELEMP#) ' +
        '    concat digits(ELDIV) concat digits(ELFYR) as policy' +
        '  from WDELP' +
        '   inner join WMEMP' +
        '    on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
        '       ELDIV = EMDIV) ' +
        'select ' +
       // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       // 'EMNAME, ' +
       // 'digits(ELFND) concat ''-'' concat digits(ELEMP#) concat ''-''' +
        'EMNAME, ELFND, ' +
        'digits(ELEMP#) concat ''-''' +
       // *** End Chg ***** 07/16/24 *** WC2972 ****************************
        ' concat digits(ELDIV) concat ''-'' concat digits(ELFYR), ' +
        'case' +
        ' when CX <> ''  '' then CX' +
        ' when AUPTDT < ' + dtToday + ' then ''EX''' +
        ' else ''  ''' +
        ' end as CX_EX, '+
        'char(date(timestamp_format(substring(digits(AURQDT),2,6),' +
        '''YYMMDD'')),usa), ' +
        'rrn_wdelp, policy ' +
        'from WMAUP' +
        ' inner join mast_dtl' +
        '  on AUCO# = ELCO# and AUFND = ELFND and AUEMP# = ELEMP# and' +
        '     AUDIV = ELDIV and AUFYR = ELFYR ' +
       // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
        //*** 'where AURCVD = 0 and AURQDT <> 0 and AURQDT >= 1000101 ' +
        //*** %trim(strSqlWhere) + ' ' +
        'Where ' + %trim(strSqlWhere) + ' ' +
       // *** End Chg ***** 07/16/24 *** WC2972 ****************************
        'order by AURQDT';

       #ofCols = #ofCols + 1;

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth2 - ttlColWth;
       colWidth = %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

       BuildJSON(outJSON :'itemType':'grid');
       BuildBigJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks':'1000000000');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** BuildJSON(outJSON :'gridDates':'0001000000');
       BuildJSON(outJSON :'gridDates':'0000100000');
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'PendingAudits');
       BuildJSON(outJSON :'gridToolTip':%trim(strTooltip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'emptyGridHdg':'No pending audits.');
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PPendingAudits    E

     p*=====================================================================
     p* Check for program in call stack
     P*=====================================================================
     PPgmInCallStack   b                   export
     DPgmInCallStack   pi             1n
     D inPgm                         10    const

     DSrChkPgm         pr                  extPgm('SRCHKPGM')
     D outPgmName                    10    const
     D isFound                        1n

     D isFound         s              1n

      /Free
       Callp SrChkPgm(inPgm:isFound);

       Return isFound;
      /End-free

     PPgmInCallStack   E

     p*=====================================================================
     p* Policies Expiring
     p*  Parm1   - type of item
     P*=====================================================================
     PPolicyExpiring   b                   export
     DPolicyExpiring   pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     D*** #ofCols          s              5  0 inz(4)
     D#ofCols          s              5  0 inz(5)
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     D#ofRows          s              5  0 inz(7)
     D#ofDays          s              5  0 inz(90)
     DdtCompare        s              7    inz
     DdtToday          s              7    inz
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(290)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     D*** colWth2                        5  0 overlay(dsColWth:*next) inz(165)
     D*** colWth3                        5  0 overlay(dsColWth:*next) inz(30)
     D*** colWth4                        5  0 overlay(dsColWth:*next) inz(90)
     D*** colWth5                        5  0 overlay(dsColWth:*next) inz(75)
     D*** colWth6                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(30)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(90)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(75)
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     D colWth7                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     DdtCompareFr      s               d   inz
     DdtCompareTo      s               d   inz
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 365;
       Endsl;

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** dtCompare = %char((SystemDate+%days(#ofDays)):*cymd0);
      //*** dtToday   = %char(SystemDate:*cymd0);
       // Create grid headings
      //*** strToolTip = 'Policies expiring in the next ' +
      //***  %trim(%editc(#ofDays:'Z')) + ' days.';
      //*** headings = %trim(DBdesc) + ' - ' +
      //***  %char(%date(dtToday:*cymd0):*usa) + '-' +
      //***  %char(%date(dtCompare:*cymd0):*usa) +
      //*** ',Policy,UW,Expiration Date';

       If inFrDt <> ' ' and inToDt <> ' ';
         dtCompare = inToDt;
         dtToday   = inFrDt;
         dtCompareFr = %Date(inFrDt:*CYMD0);
         dtCompareTo = %Date(inToDt:*CYMD0);
         // Create grid headings
         strToolTip = 'Policies expiring in from ' +
                      %char(dtCompareFr:*usa) + ' to ' +
                      %char(dtCompareTo:*usa);
         headings = %trim(DBdesc) + ': ' +
                    %char(dtCompareFr:*usa) + '-' +
                    %char(dtCompareTo:*usa)   +
                    ',Group,Policy,UW,Expiration Date';
       Else;
         dtCompare = %char((SystemDate+%days(#ofDays)):*cymd0);
         dtToday   = %char(SystemDate:*cymd0);
         // Create grid headings
         strToolTip = 'Policies expiring in the next ' +
                      %trim(%editc(#ofDays:'Z')) + ' days.';
         headings = %trim(DBdesc) + ': ' +
                    %char(%date(dtToday:*cymd0):*usa) + '-' +
                    %char(%date(dtCompare:*cymd0):*usa) +
                    ',Group,Policy,UW,Expiration Date';
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************

       //***If UsrRstFlg(q1User) = 'Y';
       //***   strSqlWhere = 'and ELFND = ' + %editc(UserGrp(q1User):'X');
       //***Endif;

       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = 'and ELFND = ' + %editc(UserGrp(q1User):'X');
       Else;
          If inGrp# <> ' ';
             strSqlWhere = ' and ELFND = ' + inGrp# + '';
          Else;
             strSqlWhere = ' and ELFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
          EndIf;
       Endif;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************

       sql = 'with WDELP_next as ' +
        '(select ELCO# next_Co#, ELFND next_Fnd, ELEMP# next_Emp#,' +
        '        ELDIV next_Div, ELFYR next_Fyr' +
        '  from WDELP' +
        '   inner join WMSHP' +
        '    on ELCO# = SHCO# and ELFND = SHFND and ELEMP# = SHEMP# and' +
        '       ELDIV = SHDIV and ELFYR = SHFYR' +
        '    where SHSTAT in (''3'', ''4''))' +
       // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       // 'select EMNAME, ' +
       // ' digits(ELFND) concat ''-'' concat digits(ELEMP#) concat ''-''' +
        'select EMNAME, ELFND,' +
        ' digits(ELEMP#) concat ''-''' +
       // *** End Chg ***** 07/16/24 *** WC2972 ****************************
        '  concat digits(ELDIV) concat ''-'' concat digits(ELFYR) as Policy,' +
        ' EMUWTR,' +
        ' char(date(timestamp_format(substring(digits(ELPTDT),2,6),' +
        '  ''YYMMDD'')),usa) as to_date, ' +
        ' digits(rrn(WDELP)), ' +
        ' digits(ELCO#) concat digits(ELFND) concat digits(ELEMP#)' +
        '  concat digits(ELDIV) concat digits(ELFYR)' +
        ' from WDELP' +
        '  exception join WDELP_next' +
        '   on ELCO# =  next_Co#  and ELFND = next_Fnd and' +
        '      ELEMP# = next_Emp# and ELDIV = next_Div and' +
        '      ELFYR =  next_Fyr-1' +
        ' inner join WMEMP' +
        '  on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
        '     ELDIV = EMDIV ' +
        'where ELPTDT between ' + dtToday + ' and ' + dtCompare + ' ' +
        %trim(strSqlWhere) + ' ' +
        'order by ELPTDT';

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth2 - ttlColWth;
       colWidth = %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       inToDt = dtCompare;
       inFrDt = dtToday  ;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'itemType':'grid');
       BuildBigJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks':'1000000000');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** BuildJSON(outJSON :'gridDates':'0001000000');
       BuildJSON(outJSON :'gridDates':'0000100000');
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'ExpiringPolicies');
       BuildJSON(outJSON :'gridToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'emptyGridHdg':'No ' + %trim(strToolTip));
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
          BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;
      /End-Free

     PPolicyExpiring   E

     p*=====================================================================
     p* ProcessGenParm - Extract parameter data
     p*  Parm1   - Name of the procedure
     p*  Parm2   - Start position
     p*  Parm2   - End position
     p*  Returns - paramter value
     P*=====================================================================
     PProcessGenParm   b                   export
     DProcessGenParm   pi           512
     D inGeneric                    512    const
     D ioStartPos                     5  0

     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            512    inz

      /Free
       Monitor;
         startPos = %Scan(',':inGeneric:ioStartPos)+1;
         If startPos <= 1;
            Return *blank;
         Endif;
         endPos = %Scan(',':inGeneric:startPos);
         If endPos > 0;
            len = endPos-startPos;
            If len > 0;
               value = %Subst(inGenParm:startPos:len);
            Endif;
         Endif;
         ioStartPos = endPos;
         Return value;
       On-Error;
         outRtnCode = 'X';
       EndMon;
      /End-free
     PProcessGenParm   e

     p*=====================================================================
     p* Quoted Policies
     p*  Parm1   - type of item
     P*=====================================================================
     PQuoted           b                   export
     DQuoted           pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     dstrLib           s             10    inz varying
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     D***#ofCols          s              5  0 inz(5)
     D#ofCols          s              5  0 inz(6)
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     D#ofRows          s              5  0 inz(7)
     D#ofDays          s              5  0 inz(1)
     DdtCompare        s              7    inz
     DdtToday          s              7    inz
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(270)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     D*** colWth2                        5  0 overlay(dsColWth:*next) inz(165)
     D*** colWth3                        5  0 overlay(dsColWth:*next) inz(30)
     D*** colWth4                        5  0 overlay(dsColWth:*next) inz(85)
     D*** colWth5                        5  0 overlay(dsColWth:*next) inz(25)
     D*** colWth6                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(45)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(30)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(85)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(25)
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     D colWth7                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 4 * 365;
       Endsl;
       dtCompare = %char((SystemDate-%days(#ofDays)):*cymd0);
       dtToday   = %char((SystemDate+%days(365)):*cymd0);

      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       If inFrDt = ' ' and inToDt = ' ';
          inFrDt = dtCompare;
          inToDt = dtToday  ;
       EndIf;
      *** End Add ***** 07/16/24 *** WC2972 ****************************

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       // Create grid headings
           strToolTip = 'Quotes effective after ' +
            %char(%date(dtCompare:*cymd0):*usa) + '.';
           headings = %trim(DBdesc) + ' (effective after ' +
            %char(%date(dtCompare:*cymd0):*usa) + ')' +
              ',Group,Policy,UW,Effective Date,N/R';

         //If UsrRstFlg(q1User) = 'Y';
         //   strSqlWhere = 'and ELFND = ' + %editc(UserGrp(q1User):'X');
         //Endif;

       strSqlWhere = 'Where ELPFDT >= ' + inFrDt + ' and ' +
                           'ELPFDT <= ' + inToDt ;

       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = %Trim(strSqlWhere) +' and ELFND = ' +
                        %editc(UserGrp(q1User):'X');
       Else;
          If inGrp# <> ' ';
             strSqlWhere = %Trim(strSqlWhere) + ' and ELFND = '+ inGrp# + '';
          Else;
             strSqlWhere = %Trim(strSqlWhere) + ' and ELFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
          EndIf;
       Endif;
       // *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strLib = %trim(RtvQuoteLib('*QTE'));
       // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       // sql = 'select EMNAME,' +
       // ' digits(ELFND) concat ''-'' concat digits(ELEMP#) concat ''-''' +
       // '  concat digits(ELDIV) concat ''-'' concat digits(ELFYR) as Policy,' +
       // ' EMUWTR,' +
       // char(date(timestamp_format(substring(digits(ELPFDT),2,6),' +
       // '  ''YYMMDD'')),usa) as to_date, ' +
       // ' ELNWRW,' +
       // ' digits(rrn(' + strLib + '.WDELP)), ' +
       // ' digits(ELCO#) concat digits(ELFND) concat digits(ELEMP#)' +
       // '  concat digits(ELDIV) concat digits(ELFYR)' +
       // ' from ' + strLib + '.WDELP' +
       // ' inner join ' + strLib + '.WMEMP' +
       // '  on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
       // '     ELDIV = EMDIV ' +
       // 'where ELPFDT > ' + dtCompare + ' ' +
       // %trim(strSqlWhere) + ' ' +
       // 'order by ELPFDT';

       sql = 'select EMNAME, ELFND,' +
        ' digits(ELEMP#) concat ''-''' +
        '  concat digits(ELDIV) concat ''-'' concat digits(ELFYR) as Policy,' +
        ' EMUWTR,' +
        ' char(date(timestamp_format(substring(digits(ELPFDT),2,6),' +
        '  ''YYMMDD'')),usa) as to_date, ' +
        ' ELNWRW,' +
        ' digits(rrn(' + strLib + '.WDELP)), ' +
        ' digits(ELCO#) concat digits(ELFND) concat digits(ELEMP#)' +
        '  concat digits(ELDIV) concat digits(ELFYR)' +
        ' from ' + strLib + '.WDELP' +
        ' inner join ' + strLib + '.WMEMP' +
        '  on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
        '     ELDIV = EMDIV ' +
           %trim(strSqlWhere) + ' ' +
        'order by ELPFDT';
       // *** End Chg ***** 07/16/24 *** WC2972 ****************************

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth2 - ttlColWth;
       colWidth = %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

       BuildJSON(outJSON :'itemType':'grid');
       BuildBigJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks':'1000000000');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** BuildJSON(outJSON :'gridDates':'0001000000');
       BuildJSON(outJSON :'gridDates':'0000100000');
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'Quotes');
       BuildJSON(outJSON :'gridToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'emptyGridHdg':'No ' + %trim(strToolTip));
       BuildJSON(outJSON :'quoteFlag':'Q');
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;
      /End-Free

     PQuoted           E

     p*=====================================================================
     p* Regional Losses
     p*  Parm1   - type of item
     P*=====================================================================
     PRegionalLosses   b                   export
     DRegionalLosses   pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     DdtFr             s               d   inz
     DdtTo             s               d   inz
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     D*** Beg Add *** 10/15/25 ******************************************
     DstrSqlWhere2     s           5000    inz
     D*** End Add *** 10/15/25 ******************************************
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
NP01 D#ofCols          s              5  0 inz(9)
     D#ofRows          s              5  0 inz(7)
     DdtCompareFr      s              7    inz
     DdtCompareTo      s              7    inz
     DdtFirst          s               d   inz(d'2001-01-01')
     DdtLast           s               d   inz(d'2001-12-31')
     DdtToday          s              7    inz
     DdtYear           s              4  0
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(35)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(250)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth7                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(080)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       If inFrDt <> ' ' and inToDt <> ' ';
         dtFr = %Date(inFrDt:*CYMD0);
         dtTo = %Date(inToDt:*CYMD0);
         dtCompareFr = inFrDt;
         dtCompareTo = inToDt;
         // Create grid headings
         strToolTip = 'Regional Losses for policies effective ' +
         %char(dtFr:*usa) + ' - ' + %char(dtTo:*usa) + '.';
         headings=',Group,Regional Losses,Claims,Open,Reserves,Paid,Incurred,'+
          'Loss Ratio';
       Else;
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       dtToday   = %char(SystemDate:*cymd0);
       dtYear = %subdt(%date():*years);
       dtFirst = dtFirst +
        %years(%subdt(%date():*years)-%subdt(dtFirst:*years));
       dtLast  = dtLast +
        %years(%subdt(%date():*years)-%subdt(dtLast:*years));
       If q1User = '/ROGULSKI';
          dtFirst = d'1901-01-01';
       Endif;
       dtCompareFr = %char(dtFirst:*cymd0);
       dtCompareTo = %char(dtLast :*cymd0);

       // Create grid headings
       strToolTip = 'Regional Losses for policies effective ' +
        %char(dtFirst:*usa) + ' - ' + %char(dtLast:*usa) + '.';
       headings = ',Group,Regional Losses,Claims,Open,Reserves,Paid,Incurred,'+
        'Loss Ratio';
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       EndIf;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** If UsrRstFlg(q1User) = 'Y';
      ***    strSqlWhere = 'and REFND = ' + %editc(UserGrp(q1User):'X');
      *** Endif;

       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = 'and REFND = ' + %editc(UserGrp(q1User):'X');
          // *** Beg Add ***** 10/15/25 ****************************************
          strSqlWhere2 = ' where REFND = ' + %editc(UserGrp(q1User):'X');
          // *** End Add ***** 10/15/25 ****************************************
       Else;
          If inGrp# <> ' ';
             strSqlWhere = ' and ELFND = ' + inGrp# + '';
             // *** Beg Add ***** 10/15/25 ****************************************
             strSqlWhere2 = ' where ELFND = ' + inGrp# + '';
             // *** End Add ***** 10/15/25 ****************************************
          Else;
             strSqlWhere = ' and ELFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
             // *** Beg Add ***** 10/15/25 ****************************************
             strSqlWhere2 = ' where ELFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
             // *** End Add ***** 10/15/25 ****************************************
          EndIf;
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       sql = 'with mast_dtl as ' +
        '(select EMRGN#, ELCO#, ELFND, ELFYR, ELEMP#, ELDIV,' +
        '  case when ELFAF = ''A'' then ELAAPR else ELANPR end as premium' +
        '  from WDELP' +
        '   inner join WMEMP' +
        '    on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
        '       ELDIV = EMDIV' +
        // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
        //   '  where ELPFDT between ' + dtCompareFr + ' and ' + dtCompareTo + '), '+
        '  where ELPFDT between ' + dtCompareFr + ' and ' + dtCompareTo + ' ' +
             %Trim(strSqlWhere) + '), '+
        // *** End Chg ***** 07/16/24 *** WC2972 ****************************
        'open_claims as ' +
        '(select ELFND as openFnd, EMRGN# as openRgn#, count(*) as openCount' +
        '  from WMCMP' +
        '   inner join mast_dtl' +
        '    on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '       CMDIV = ELDIV and CMFYR = ELFYR' +
        '  where CMSTAT = ''O''' +
        '  group by ELFND, EMRGN#), ' +
        'all_claims as ' +
        '(select ELFND as allFnd, EMRGN# as allRgn#, count(*) as allCount ' +
        ' from WMCMP' +
        '  inner join mast_dtl' +
        '   on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '      CMDIV = ELDIV and CMFYR = ELFYR' +
        ' group by ELFND, EMRGN#), '+
        'total_claims as ' +
        '(select ELFND as ttlFnd, EMRGN# as ttlRgn#,' +
        ' decimal(SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        '             CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10 +' +
        '             CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        '             CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10),17,2)' +
        '         as ttlInc,' +
        ' decimal(SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        '             CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10),17,2)' +
        '         as ttlRsv,' +
        ' decimal(SUM(CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        '             CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10),17,2)' +
        '         as ttlPaid,' +
        ' decimal(SUM(premium),17,2) as ttlPrem' +
        ' from WMCMP' +
        '  inner join mast_dtl' +
        '   on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '      CMDIV = ELDIV and CMFYR = ELFYR' +
        ' group by ELFND, EMRGN#) ' +
        'select '''', digits(REFND),                                      ' +
        '       digits(RERGN#) concat '' - '' concat REDESC,' +
        '       Coalesce(allCount,''0''), Coalesce(openCount,''0''),' +
        '       Coalesce(ttlRsv,''0''), Coalesce(ttlPaid,''0''),' +
        '       Coalesce(ttlInc,''0''),' +
        '       Coalesce(decimal(ttlInc/ttlPrem*100,9,2),''0'') as lossRatio,' +
        '   digits(rrn(WMREP)) as rrn_WMREP,' +
        '   digits(RECO#) concat digits(REFND) concat digits(RERGN#)' +
        '    as region' +
        ' from WMREP' +
        '  left outer join all_Claims' +
        '   on REFND = allFnd and RERGN# = allRgn#' +
        '  left outer join open_Claims' +
        '   on REFND = openFnd and RERGN# = openRgn#' +
        '  left outer join total_Claims' +
        '   on REFND = ttlFnd and RERGN# = ttlRgn#' +
        //*** Beg Add *** 10/15/25 *******************************************
        '  where REFND in (Select GSFND From WXGSPDSH '  +
            'Where GSIEFL = ''I'')' +
        //*** End Add *** 10/15/25 *******************************************
        ' order by REFND, RERGN#';

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth4 - ttlColWth;
NP01   colWidth = '0,' + %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

       BuildJSON(outJSON :'itemType':'grid');
       BuildBigJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks'   :'0000000000');
       BuildJSON(outJSON :'gridMonies'  :'0000111000');
       BuildJSON(outJSON :'gridPercents':'0000000100');
       BuildJSON(outJSON :'gridJustify' :'RLLRRRRR00');
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'RegionalLosses');
       BuildJSON(outJSON :'gridToolTip':%trim(strTooltip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'emptyGridHdg':'No regional losses.');
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(dtCompareTo:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(dtCompareFr:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;
      /End-Free

     PRegionalLosses   E

     p*=====================================================================
     p* Renewal Premium by Month
     p*  Parm1   - type of item
     P*=====================================================================
     PRenewalPremium   b                   export
     DRenewalPremium   pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     Dheading          s           1000    inz
     DstrSqlCursor     s           5000    inz
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     D#ofDays          s              5  0 inz(90)
     DcharCompareFr    s              7    inz
     DcharCompareTo    s              7    inz
     DdtCompareFr      s               d   inz
     DdtCompareTo      s               d   inz
     Dindex            s              5  0 inz
     DjsonCategories   s          32767A   inz
     djsonChart        s          32767A   inz
     djsonDataSet      s          32767A   inz
     djsonLabels       s          32767A   inz
     djsonOptions      s          32767A   inz
     djsonSeries       s          32767A   inz
     djsonValue1       s          32767A   inz
     djsonValue2       s          32767A   inz
     DlastMonth        s              2  0 inz
     DnextMonth        s              2  0 inz

     DdsData           ds
     D dsCYYMM                        5
     D dsPrem                        18  2
     D dsRnwPrem                     18  2
     DaryData          ds                  dim(12) qualified inz
     D*dsMonth                        2
     D dsMonth                        2  0
     D dsPrem                        18  2
     D dsRnwPrem                     18  2
      *
      /Free
       Select;
       When q1user = '/ROGULSKI';
          #ofDays = -4 * 365;
       Endsl;

       // Future expirations
       dtCompareTo = GetLastDay((SystemDate+%days(#ofDays)));
       // Back up one year
       dtCompareFr = GetFirstDay((dtCompareTo-%months(11)));
       // Convert to character for SQL
       charCompareTo = %char(dtCompareTo:*cymd0);
       charCompareFr = %char(dtCompareFr:*cymd0);

       strToolTip = 'One year of policies expiring by month using ' +
        %char(#ofDays) + ' days into the future as the ending point.';
       heading = %trim(DBdesc) + ' (' +
        %char(dtCompareFr:*usa) + '-' + %char(dtCompareTo:*usa) + ')';
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** If UsrRstFlg(q1User) = 'Y';
      //***    strSqlWhere = ' and ELFND = ' + %editc(UserGrp(q1User):'X');
      //*** Endif;

       If UsrRstFlg(q1User) = 'Y';
          strSqlWhere = 'ELFND = ' + %editc(UserGrp(q1User):'X');
       Else;
          If inGrp# <> ' ';
             strSqlWhere = 'ELFND = ' + inGrp# + '';
          Else;
             strSqlWhere = 'ELFND in ' +
                           '(Select GSFND From WXGSPDSH '       +
                                 'Where GSIEFL = ''I'')';
          EndIf;
       Endif;

       If inFrDt <> ' ' and inToDt <> ' ';
          charCompareFr = inFrDt;
          charCompareTo = inToDt;
          dtCompareFr = %Date(inFrDt:*CYMD0);
          dtCompareTo = %Date(inToDt:*CYMD0);
       EndIf;

       strSqlWhere = %Trim(strSqlWhere)+' and ELPTDT >= ' + charCompareFr +
                                        ' and ELPTDT <= ' + charCompareTo;

       strToolTip = ' ';

       heading = %trim(DBdesc) + ' (' +
        %char(dtCompareFr:*usa) + '-' + %char(dtCompareTo:*usa) + ')';
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlCursor = 'with ' +
        //*** Beg Add *** 12/12/24 *******************************************
        'WMSHP_UNIQUE as (Select Distinct SHco#, SHfnd, SHemp# , ' +
        'SHdiv, SHfyr, SHstat from WMSHP), ' +
        //*** End Add *** 12/12/24 *******************************************
        'WDELP_next as ' +
        ' (select ELCO# next_Co, ELFND next_Fnd, ELEMP# next_Emp, ' +
        '         ELDIV next_Div, ELFYR next_Fyr, ELANPR next_AnPr, ' +
        '         ELFAF next_FAF, ELAAPR next_AaPr ' +
        '  from WDELP ' +
        //*** Beg Chg *** 12/12/24 *******************************************
        //*** '  inner join WMSHP ' +
        '  inner join WMSHP_UNIQUE ' +
        //*** End Chg *** 12/12/24 *******************************************
        '   on ELCO# = SHCO# and ELFND = SHFND and ELFYR = SHFYR and ' +
        '      ELEMP# = SHEMP# and ELDIV = SHDIV ' +
        '  where SHSTAT in (''3'', ''4'')), ' +
        'nonTestGrps as ' +
        ' (select FNCO#, FNFND ' +
        '  from WMFNP ' +
        '  where FNTEST <> ''Y'') ' +
        'select substring(digits(ELPTDT),1,5) as CYMMD, ' +
        //*** Beg Chg *** 12/12/24 *******************************************
        //*** ' sum(case when ELFAF = ''A'' then ELAAPR else ELANPR end) ' +
        //*** '  as premium, ' +
        //*** ' sum(case when next_FAF = ''A'' then next_AaPr else next_AnPr end) ' +
        //*** '  as renew_premium ' +
        //*** ' from WDELP ' +
        //*** ' inner join WMSHP ' +
        ' sum(case when ELFAF = ''A'' then ' +
        ' case when ELAAPR is NULL then 0 else ELAAPR end ' +
        ' else case when ELANPR  is NULL then 0 else ELANPR end end) ' +
        '  as premium, ' +
        ' sum(case when next_FAF = ''A'' then ' +
        ' case when next_AaPr is NULL then 0 else next_AaPr end ' +
        ' else case when next_AnPr is NULL then 0 else next_AnPr end end) ' +
        '  as renew_premium ' +
        ' from WDELP ' +
        ' inner join WMSHP_UNIQUE ' +
        //*** End Chg *** 12/12/24 *******************************************
        '  on ELCO# = SHCO# and ELFND = SHFND and ELFYR = SHFYR and ' +
        '     ELDIV = SHDIV and ELEMP# = SHEMP# ' +
        ' inner join nonTestGrps ' +
        '  on ELCO# = FNCO# and ELFND = FNFND ' +
        ' left join WDELP_next ' +
        '  on ELCO# = next_Co and ELFND = next_Fnd and ' +
        '     ELFYR = next_Fyr-1 and ' +
        '     ELEMP# = next_Emp and ELDIV = next_Div ' +
        //*** Beg Chg ***** 07/16/24 *** WC2972 ****************************
        //*** ' where ELPTDT between ' + charCompareFr + ' and ' + charCompareTo +
        //*** %trimr(strSQLWhere) + ' ' +
        ' where ' + %trimr(strSQLWhere) + ' ' +
        //*** End Chg ***** 07/16/24 *** WC2972 ****************************
        ' and SHSTAT in (''3'', ''4'') ' +
        ' group by substring(digits(ELPTDT),1,5) ' +
        ' order by CYMMD';

       lastMonth = %dec(%subst(charCompareTo:4:2):2:0);
       nextMonth = %dec(%subst(charCompareFr:4:2):2:0);
       Exec SQL Prepare sqlRnwPrm From :strSqlCursor;
       Exec SQL Declare cursorRnwPrm Cursor For sqlRnwPrm;
       Exec SQL Open cursorRnwPrm;
       Dou SQLSTT <> '00000' and SQLSTT <> '01003';
          Exec SQL Fetch next From cursorRnwPrm into :dsData;
          If SQLSTT = '00000' or SQLSTT = '01003';
             Dow %dec(%subst(dsCYYMM:4:2):2:0) <> nextMonth;
                index += 1;
                aryData(index).dsMonth = nextMonth;
                Exsr GetNextMonth;
             Enddo;
             index += 1;
             aryData(index).dsMonth    = nextMonth;
             aryData(index).dsPrem     = dsPrem;
             aryData(index).dsRnwPrem  = dsRnwPrem;
             Exsr GetNextMonth;
          Endif;
       Enddo;
       Exec SQL Close cursorRnwPrm;
       Monitor;
       For index = 1 to 12;
          Select;
          When aryData(index).dsMonth <> 0;
             Iter;
          When nextMonth = 13;
             nextMonth = 1;
          Endsl;
          aryData(index).dsMonth = nextMonth;
          Exsr GetNextMonth;
       Endfor;
       On-Error;
       BuildJSON(outJSON :'Error Info - index':%char(index));
       BuildJSON(outJSON :'Error Info - nextMonth':%char(nextMonth));
       BuildJSON(outJSON :'Error Info - lastMonth':%char(lastMonth));
       Return returnInfo;
       Endmon;

       BuildJSON(outJSON :'itemType':'chart');
       BuildJSON(outJSON :'chartType':'MSColumn2D');
       Exsr BuildJSONData;
       //***  Begin Add  ***  03/03/22  ****************
       //BuildBigJSON(outJSON :'chartJSON':jsonChart);
       outJson = %trim(outJSON) + ', "chartJSON":' + jsonChart;
       //***  End   Add  ***  03/03/22  ****************
       BuildJSON(outJSON :'dropValues':msDropValues);
       BuildJSON(outJSON :'dropChoices':msDropChoices);
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':heading);
       BuildBigJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       inFrDt = charCompareFr;
       inToDt = charCompareTo;
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;

       //==================================================================
       // Build JSON chart data
       //==================================================================
       Begsr BuildJSONData;
        // Options
        StartJSONelemnt(jsonOptions);
        BuildJson(jsonOptions:'numberPrefix':'$');
       //BuildJson(jsonOptions:'paletteColors':'#0075c2,#1aaf5d,#f2c500');
        BuildJson(jsonOptions:'rotateValues':'1');
        BuildJson(jsonOptions:'placeValuesInside':'1');
        EndJSONelemnt(jsonOptions);

        // Labels and values
        For index = 1 to 12;
           BldJsonLabels(jsonLabels:AlphaMonth(aryData(index).dsMonth));
           BldJsonValues(jsonValue1:%editc(aryData(index).dsPrem:'L'));
           BldJsonValues(jsonValue2:%editc(aryData(index).dsRnwPrem:'L'));
        Endfor;

        // Categories
        BldJsonCategory(jsonCategories:jsonLabels);
        // Series 1
        BldJsonSeries(jsonSeries:'Expiring':jsonValue1);
        // Series 2
        BldJsonSeries(jsonSeries:'Renewed':jsonValue2);
        // Dataset
        BldJsonDataSet(jsonDataSet:jsonSeries);
        // Chart
        BldJsonChart(jsonChart:jsonOptions:jsonCategories:jsonDataSet);
       Endsr;
       //==================================================================
       // Get the next month (nextMonth)
       //==================================================================
       Begsr GetNextMonth;
        nextMonth += 1;
        If nextMonth = 13;
           nextMonth = 1;
        Endif;
       Endsr;
      /End-Free

     PRenewalPremium   E

     p*=====================================================================
     p* RtvUserInfo - Return user name, id, staff code
     p*  Parm1   - relational user info
     p*  Parm2   - type of user info returned (*default,id,name,staffCode)
     p*  Parm3   - type of user info passed in (id,staffCode)
     P*=====================================================================
     PRtvUserInfo      b
     DRtvUserInfo      pi            50
     D inUserInfo                    50    const
     D inRtnType                     10    const options(*nopass)
     D inType                        10    const options(*nopass)

     DdftRtnType       c                   'name'
     DdftType          c                   'staffCode'
     DstrRtnType       s             10    inz('*default')
     DstrType          s             10    inz('*default')
      *
      /Free
       If %parms >= 2 and inRtnType <> *blanks;
          strRtnType = inRtnType;
       Endif;
       If %parms >= 3 and inType <> *blanks;
          strType = inType;
       Endif;
       Select;
       When strRtnType = '*default' and cltId = '133';
          strRtnType = 'staffCode';
       When strRtnType = '*default';
          strRtnType = dftRtnType;
       Endsl;
       If strType = '*default';
          strType = dftType;
       Endif;

       Select;
       When strType = 'staffCode' and strRtnType = 'id';
          Return GetUserId(%trim(inUserInfo));
       When strType = 'staffCode' and strRtnType = 'name';
        //Return GetUserName(GetUserId(%trim(inUserInfo)));
          Return GetUsrName(GetUserId(%trim(inUserInfo)));
       When strType = 'id' and strRtnType = 'staffCode';
          Return GetUsrStaf(%trim(inUserInfo));
       When strType = 'id' and strRtnType = 'name';
          Return GetUsrName(%trim(inUserInfo));
       Other;
          Return inUserInfo;
       Endsl;

      /End-free
     PRtvUserInfo      e

     p*=====================================================================
     p* SIC Losses
     p*  Parm1   - type of item
     P*=====================================================================
     PSICLosses        b                   export
     DSICLosses        pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DdtFr             s               d   inz
     DdtTo             s               d   inz
     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
     D#ofCols          s              5  0 inz(8)
     D#ofRows          s              5  0 inz(7)
     DdtCompareFr      s              7    inz
     DdtCompareTo      s              7    inz
     DdtFirst          s               d   inz(d'2001-01-01')
     DdtLast           s               d   inz(d'2001-12-31')
     DdtToday          s              7    inz
     DdtYear           s              4  0
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(285)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(40)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(125)
     D colWth7                        5  0 overlay(dsColWth:*next) inz(080)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(080)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
     DdtCompare        s              7    inz
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlWhere = '';
       If inGrp# <> ' ';
          strSqlWhere = ' and ELFND = ' + inGrp# + '';
       Else;
          strSqlWhere = ' and ELFND in ' +
                        '(Select GSFND From WXGSPDSH '       +
                              'Where GSIEFL = ''I'')';
       EndIf;

       If inFrDt <> ' ' and inToDt <> ' ';
         dtCompare = inToDt;
         dtToday   = inFrDt;
         dtFr = %Date(inFrDt:*CYMD0);
         dtTo = %Date(inToDt:*CYMD0);
         dtCompareFr = inFrDt;
         dtCompareTo = inToDt;

         // Create grid headings
         strToolTip = 'SIC Losses for policies effective ' +
         %char(dtFr:*usa) + ' - ' + %char(dtTo:*usa) + '.';
       Else;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************

       dtToday   = %char(SystemDate:*cymd0);
       dtYear = %subdt(%date():*years);
       dtFirst = dtFirst +
        %years(%subdt(%date():*years)-%subdt(dtFirst:*years));
       dtLast  = dtLast +
        %years(%subdt(%date():*years)-%subdt(dtLast:*years));
       If q1User = '/ROGULSKI';
          dtFirst = d'1901-01-01';
       Endif;
       dtCompareFr = %char(dtFirst:*cymd0);
       dtCompareTo = %char(dtLast :*cymd0);

       // Create grid headings
       strToolTip = 'SIC Losses for policies effective ' +
        %char(dtFirst:*usa) + ' - ' + %char(dtLast:*usa) + '.';
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       EndIf;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       headings = ',SIC Losses,Claims,Open,Reserves,Paid,Incurred,'+
        'Loss Ratio';
       If UsrRstFlg(q1User) = 'Y';
        //strSqlWhere = 'and **FND = ' + %editc(UserGrp(q1User):'X');
       Endif;
       sql = 'with mast_dtl as ' +
        '(select EMSIC, ELCO#, ELFND, ELFYR, ELEMP#, ELDIV,' +
        '  case when ELFAF = ''A'' then ELAAPR else ELANPR end as premium' +
        '  from WDELP' +
        '   inner join WMEMP' +
        '    on ELCO# = EMCO# and ELFND = EMFND and ELEMP# = EMEMP# and' +
        '       ELDIV = EMDIV' +
        // *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
        //  '  where ELPFDT between ' + dtCompareFr + ' and ' + dtCompareTo + '), '+
        '  where ELPFDT between ' + dtCompareFr + ' and ' + dtCompareTo + ' ' +
           %Trim(strSqlWhere) + '), '+
        // *** End Chg ***** 07/16/24 *** WC2972 ****************************
        'open_claims as ' +
        '(select EMSIC as openSIC, count(*) as openCount' +
        '  from WMCMP' +
        '   inner join mast_dtl' +
        '    on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '       CMDIV = ELDIV and CMFYR = ELFYR' +
        '  where CMSTAT = ''O''' +
        '  group by EMSIC), ' +
        'all_claims as ' +
        '(select EMSIC as allSIC, count(*) as allCount ' +
        ' from WMCMP' +
        '  inner join mast_dtl' +
        '   on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '      CMDIV = ELDIV and CMFYR = ELFYR' +
        ' group by EMSIC), '+
        'total_claims as ' +
        '(select EMSIC as ttlSIC,' +
        ' decimal(SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        '             CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10 +' +
        '             CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        '             CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10),17,2)' +
        '         as ttlInc,' +
        ' decimal(SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        '             CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10),17,2)' +
        '         as ttlRsv,' +
        ' decimal(SUM(CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        '             CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10),17,2)' +
        '         as ttlPaid,' +
        ' decimal(SUM(premium),17,2) as ttlPrem' +
        ' from WMCMP' +
        '  inner join mast_dtl' +
        '   on CMCO# = ELCO# and CMFND = ELFND and CMEMP# = ELEMP# and' +
        '      CMDIV = ELDIV and CMFYR = ELFYR' +
        ' group by EMSIC) ' +
        'select '''', trim(GTKEY) concat '' - '' concat GTDESC,' +
        '       Coalesce(allCount,''0''), Coalesce(openCount,''0''),' +
        '       Coalesce(ttlRsv,''0''), Coalesce(ttlPaid,''0''),' +
        '       Coalesce(ttlInc,''0''),' +
        '       Coalesce(decimal(ttlInc/ttlPrem*100,9,2),''0'') as lossRatio,' +
        '   digits(rrn(WTGTP)) as rrn_WTGTP,' +
        '   digits(GTCO#) concat GTKEY as SIC' +
        ' from WTGTP' +
        '  left outer join all_Claims' +
        '   on trim(GTKEY) = trim(allSIC)' +
        '  left outer join open_Claims' +
        '   on trim(GTKEY) = trim(openSIC)' +
        '  left outer join total_Claims' +
        '   on trim(GTKEY) = trim(ttlSIC)' +
        ' where GTTBID = ''SIC''' +
        ' order by GTKEY';

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth4 - ttlColWth;
       colWidth = '0,' + %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       inToDt = dtCompareTo;
       inFrDt = dtCompareFr;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'itemType':'grid');
       BuildBigJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks'   :'0000000000');
       BuildJSON(outJSON :'gridMonies'  :'0001110000');
       BuildJSON(outJSON :'gridPercents':'0000001000');
       BuildJSON(outJSON :'gridJustify' :'LLLRRRRR00');
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'RegionalLosses');
       BuildJSON(outJSON :'gridToolTip':%trim(strTooltip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'emptyGridHdg':'No SIC losses.');
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       BuildJSON(outJSON :'fltToDate':%Char(%Date(inToDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltFrDate':%Char(%Date(inFrDt:*CYMD0): *USA0));
       BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Return returnInfo;
      /End-Free

     PSICLosses        E

     p*=====================================================================
     p* TestChart - Return data information to test
     p*  Parm1   - type of item
     P*=====================================================================
     PTestChart        b                   export
     DTestChart        pi                  like(rtnData)
     D inType                        15

     DreturnInfo       s                   like(rtnData)
     Djson             s                   like(rtnData)
     DstrToolTip       s            500    inz
     dxmlChart         s          32767A   inz
     djsonCategories   s          32767A   inz
     djsonChart        s          32767A   inz
     djsonDataSet      s          32767A   inz
     djsonLabels       s          32767A   inz
     djsonOptions      s          32767A   inz
     djsonSeries       s          32767A   inz
     djsonValues       s          32767A   inz
      *
      /Free
       returnInfo = %trim(returnInfo) + '<item type>chart</item type>';
       returnInfo = %trim(returnInfo) + '<chart type>Column2D</chart type>';
       returnInfo = %trim(returnInfo) + '<names>  </names>';
       returnInfo = %trim(returnInfo) + '<values>  </values>';

       BuildJSON(outJSON :'itemType':'chart');
       inType = %trim(inType);
       Select;
       When inType = '1';
          Exsr BuildXMLData;
          BuildJSON(outJSON :'chartType':'MSColumn2D');
          BuildBigJSON(outJSON :'chartXML':xmlChart);
          BuildJSON(outJSON :'dropValues':msDropValues);
          BuildJSON(outJSON :'dropChoices':msDropChoices);
       When inType = '2';
          Exsr BuildJSONData;
          BuildJSON(outJSON :'chartType':'MSColumn2D');
          //***  Begin Add  ***  03/03/22  ****************
          //BuildBigJSON(outJSON :'chartJSON':jsonChart);
          outJson = %trim(outJSON) + ', "chartJSON":' + jsonChart;
          //***  End   Add  ***  03/03/22  ****************
          BuildJSON(outJSON :'dropValues':msDropValues);
          BuildJSON(outJSON :'dropChoices':msDropChoices);
       When inType = '1';
          BuildJSON(outJSON :'chartType':'Line');
          BuildJSON(outJSON :'chartNames':'My Assignments,My Team Assignments');
          BuildJSON(outJSON :'chartValues':'2,5');
       When inType = '2';
          BuildJSON(outJSON :'chartType':'Column3D');
          BuildJSON(outJSON :'chartNames':'Today, Last 30 days, Over 90 days');
          BuildJSON(outJSON :'chartValues':'10,3,12');
          BuildJSON(outJSON :'dropValues':ssDropValues);
          BuildJSON(outJSON :'dropChoices':ssDropChoices);
       Endsl;
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':'Test Chart ' + %trim(inType));

       Return returnInfo;
       //==================================================================
       // Build JSON chart data
       //==================================================================
       Begsr BuildJSONData;
        // Options
        StartJSONelemnt(jsonOptions);
        BuildJson(jsonOptions:'numberPrefix':'$');
        BuildJson(jsonOptions:'paletteColors':'#0075c2,#1aaf5d,#f2c500');
        BuildJson(jsonOptions:'rotateValues':'1');
        BuildJson(jsonOptions:'placeValuesInside':'1');
        EndJSONelemnt(jsonOptions);
        // Labels
        BldJsonLabels(jsonLabels:'Jan');
        BldJsonLabels(jsonLabels:'Feb');
        BldJsonLabels(jsonLabels:'Mar');
        BldJsonLabels(jsonLabels:'Apr');
        BldJsonLabels(jsonLabels:'May');
        BldJsonLabels(jsonLabels:'Jun');
        BldJsonLabels(jsonLabels:'Jul');
        BldJsonLabels(jsonLabels:'Aug');
        BldJsonLabels(jsonLabels:'Sep');
        BldJsonLabels(jsonLabels:'Oct');
        BldJsonLabels(jsonLabels:'Nov');
        BldJsonLabels(jsonLabels:'Dec');
        // Categories
        BldJsonCategory(jsonCategories:jsonLabels);
        // Values 1
        Clear jsonValues;
        BldJsonValues(jsonValues:'31222209');
        BldJsonValues(jsonValues:'3549341');
        BldJsonValues(jsonValues:'4532660');
        BldJsonValues(jsonValues:'4232422');
        BldJsonValues(jsonValues:'4804687');
        BldJsonValues(jsonValues:'5687783');
        BldJsonValues(jsonValues:'18408177');
        BldJsonValues(jsonValues:'7140728');
        BldJsonValues(jsonValues:'3919687');
        BldJsonValues(jsonValues:'4761438');
        BldJsonValues(jsonValues:'5639758');
        BldJsonValues(jsonValues:'6414989');
        // Series 1
        BldJsonSeries(jsonSeries:'Expiring':jsonValues);
        // Values 2
        Clear jsonValues;
        BldJsonValues(jsonValues:'31384723');
        BldJsonValues(jsonValues:'3500257');
        BldJsonValues(jsonValues:'3370527');
        BldJsonValues(jsonValues:'3489757');
        BldJsonValues(jsonValues:'4335437');
        BldJsonValues(jsonValues:'4453033');
        BldJsonValues(jsonValues:'15681265');
        BldJsonValues(jsonValues:'6176261');
        BldJsonValues(jsonValues:'3517964');
        BldJsonValues(jsonValues:'3675338');
        BldJsonValues(jsonValues:'4251769');
        BldJsonValues(jsonValues:'5693447');
        // Series 2
        BldJsonSeries(jsonSeries:'Renewed':jsonValues);
        // Dataset
        BldJsonDataSet(jsonDataSet:jsonSeries);
        // Chart
        BldJsonChart(jsonChart:jsonOptions:jsonCategories:jsonDataSet);
       Endsr;
       //==================================================================
       // Build XML chart data
       //==================================================================
       Begsr BuildXMLData;
       //'<graph caption="Pieces Per Labor Hour" xAxisName="Month" ' +
       //'yAxisName="Pieces" yAxisMaxValue="11788" >' +
        xmlChart =
         '<graph>' +
         ' <categories>' +
         '  <category name="JAN" />' +
         '  <category name="FEB" />' +
         ' </categories>' +
         ' <dataset seriesname="2010" color="4D4DFF" showValues="0" >' +
         '  <set value="17503" />' +
         '  <set value="17696" />' +
         ' </dataset>' +
         ' <dataset seriesname="2009" color="45B46A" showValues="0" >' +
         '  <set value="11288" />' +
         '  <set value="22357" />' +
         ' </dataset>' +
         '</graph>';
       //' <trendLines>' +
       //'  <line StartValue="18027" color="660066" displayvalue="Goal"' +
       //'  thickness="5" showOnTop="1" />' +
       //' </trendLines>' +
       //'</graph>';
       Endsr;
      /End-Free

     PTestChart        E

     p*=====================================================================
     p* TestGrid - Return data information to test
     p*  Parm1   - type of item
     P*=====================================================================
     PTestGrid         b                   export
     DTestGrid         pi                  like(rtnData)
     D inType                        15

     Dheadings         s           1000    inz
     DcolWidth         s           1000    inz
     Dsql              s           5000    inz
     DstrSqlSum        s           5000
     DstrSqlWhere      s           5000    inz
     DstrToolTip       s            500    inz
     DreturnInfo       s                   like(rtnData)
     Dgrid#            s              5  0 inz(1)
     D#ofCols          s              5  0 inz(4)
     D#ofRows          s              5  0 inz(7)
     DdsColWth         ds
     D colWth1                        5  0 overlay(dsColWth)       inz(300)
     D colWth2                        5  0 overlay(dsColWth:*next) inz(50)
     D colWth3                        5  0 overlay(dsColWth:*next) inz(50)
     D colWth4                        5  0 overlay(dsColWth:*next) inz(50)
     D colWth5                        5  0 overlay(dsColWth:*next) inz(75)
     D colWth6                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth7                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth8                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth9                        5  0 overlay(dsColWth:*next) inz(0)
     D colWth10                       5  0 overlay(dsColWth:*next) inz(0)
     D colWth                         5  0 overlay(dsColWth) dim(10)
     DlstColWth        s              5  0 inz(15)
     DttlColWth        s              5  0 inz
     DcsvName          s             20    inz('TestGrid')
     Dindex            s              5  0
     Djson             s                   like(rtnData) inz
     DdsData           ds
     D dtConv                        10
     D dtChar                        10
     D dtSubst                       10
     D dtFile                        10
      *
      /Free

       grid# = %dec(%trim(inType):5:0);
       If grid# <= 0;
          Return json;
       Endif;
       grid# = %rem(grid#:2) + 1;

       // Create grid headings
       Select;
       When grid# = 1;
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** #ofCols = 4;
          #ofCols = 5;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
          strToolTip = 'Test Grid - Employer Master records';
          headings = 'Name,Group,Employer,Div';
          sql = 'select ' +
           'EMNAME, char(EMFND), char(EMEMP#), char(EMDIV), ' +
           'digits(rrn(WMEMP)), ' +
           'digits(EMFND) concat digits(EMEMP#) concat digits(EMDIV) ' +
           'from WMEMP where EMFND = 777' ;
       When grid# = 2;
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *** #ofCols = 5;
          #ofCols = 6;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
          strToolTip = 'Test Grid - Employer Detail policy from dates.';
          headings = 'Policy,Converted Date,Character Date,Substring Date,'+
           'File Date';
          sql = 'select ' +
           'digits(ELFND) concat digits(ELEMP#) concat digits(ELDIV)' +
           ' concat digits(ELFYR), ' +
           'char(date(timestamp_format(substring(digits(ELPFDT),2,6),' +
           '''YYMMDD'')),usa), ' +
           'digits(ELPFDT), ' +
           'substring(char(ELPFDT),2,6), ' +
           'ELPFDT, ' +
           'digits(rrn(WDELP)), ' +
           'digits(ELFND) concat digits(ELEMP#) concat digits(ELDIV) ' +
           'ELPFDT from WDELP where ELPFDT <> 0' ;
       Endsl;

       // Create column widths
       For index = 1 to #ofCols;
          If index > 1;
            colWidth = %trim(colWidth) + ',';
          Endif;
          colWidth = %trim(colWidth) + %trim(%editc(colWth(index):'Z'));
          ttlColWth += colWth(index);
       Endfor;
       lstColWth += maxColTtlWth2 - ttlColWth;
       colWidth = %trim(colWidth) + ',' + %trim(%editc(lstColWth:'Z'));

       // Determine # of rows
       #ofRows = GetWidgetHeight(DBtype:DBsizeH);

       BuildJSON(outJSON :'itemType':'grid');
       BuildJSON(outJSON :'sql':%trim(sql));
       BuildJSON(outJSON :'gridLinks':'1001000000');
       BuildJSON(outJSON :'gridHeadings':%trim(headings));
       BuildJSON(outJSON :'gridRows':%trim(%editc(#ofRows:'Z')));
       BuildJSON(outJSON :'gridCols':%trim(%editc(#ofCols+1:'Z')));
       BuildJSON(outJSON :'gridColWidths':%trim(colWidth));
       BuildJSON(outJSON :'gridCsvName':'TestGrid');
       BuildJSON(outJSON :'gridToolTip':%trim(strTooltip));
       BuildJSON(outJSON :'panelVisibility':'0');
       BuildJSON(outJSON :'panelValue':'Test Grid - ' + %trim(inType));
       Return returnInfo;

      /End-Free

     PTestGrid         E

     p*=====================================================================
     p* Agent Premiums from State Reporting Division
     p*  Parm1   - type of item
     P*=====================================================================
     PTop10AgentInfo   b                   export
     DTop10AgentInfo   pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     D inFrDt                         7
     D inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D index           s              9  0 inz
     D decAgt#         s              5  0 inz
     D decCo#          s              3  0
     D decFein         s              9  0
     D decMod          s              3  0
     D isUseCiCenter   s              1n   inz('0')
     DjsonCategories   s          32767A   inz
     djsonChart        s          32767A   inz
     djsonDataSet      s          32767A   inz
     djsonLabels       s          32767A   inz
     djsonOptions      s          32767A   inz
     djsonSeries       s          32767A   inz
     djsonValue1       s          32767A   inz
     djsonValue2       s          32767A   inz
     D strName         s             40    inz
     D strOptions      s           5000    inz
     D strSqlCursor    s           5000
     D strSqlWhere1    s           5000
     D strSqlWhere2    s           5000
     D strToolTip      s            500    inz
     D names           s            425    inz
     D values          s            225    inz
     D dtCompare       s              7    inz
     D #ofDays         s              5  0 inz(7)
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     DdtCompareFr      s               d   inz
     DdtCompareTo      s               d   inz
     DcharCompareFr    s              7    inz
     DcharCompareTo    s              7    inz
     D*** dsData          ds
     D***  name                         20
     D***  value                        18  2
     D dsData          ds
     D  FEIN                         20
     D  Name                         40
     D  Value                        18  2
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
      *
      /Free
       decCo# = UserCo#(q1User);

       Select;
       When q1user = '/ROGULSKI';
          #ofDays = 15 * 365;
       Endsl;

       dtCompare = %char((SystemDate-%days(#ofDays)):*cymd0);

       BuildJSON(outJSON :'itemType':'chart');

      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      *Select;
      *When isUseCiCenter;
      *   strSqlWhere1 = ' ';
      *   strSqlCursor = 'select PAFEIN, sum(POANPR) ' +
      *    'from WHPOP ' +
      *    %trim(strSQLWhere1) + ' ' +
      *    ' inner join WHPAP' +
      *    '  on POPONR = PAPONR ' +
      *    'group by PAFEIN ' +
      *    'order by sum(POANPR) desc ' +
      *    'fetch first 10 rows only';
      *Other;
      *   strSqlWhere1 = ' ';
      *   strSqlCursor = 'with ' +
      *    'WMAHP_sub as' +
      *    ' (select AHCO# lastAHCO#, AHFND lastAHFND, AHEMP# lastAHEMP#,' +
      *    '         AHDIV lastAHDIV, AHFYR lastAHFYR, max(AHEFDT) lastAHEFDT' +
      *    '    from WMAHP' +
      *    '    group by AHCO#, AHFND, AHEMP#, AHDIV, AHFYR), ' +
      *    'stateRptDiv as' +
      *    ' (select EMCO#, EMFND, EMEMP#, EMDIV' +
      *    '   from WMEMP' +
      *    '   where EMSTRP = ''Y''), ' +
      *    'WDELP_all as' +
      *    ' (select ELCO#, ELFND, ELEMP#, ELDIV, ELFYR,' +
      *    '   case when ELFAF = ''A'' then ELAAPR' +
      *    '        else ELANPR' +
      *    '   end as premium' +
      *    '   from WDELP' +
      *    '   inner join stateRptDiv' +
      *    '    on ELCO#  = EMCO#  and ELFND = EMFND and' +
      *    '       ELEMP# = EMEMP# and ELDIV = EMDIV), ' +
      *    'nonTestGrps as' +
      *    ' (select FNCO#, FNFND' +
      *    '   from WMFNP' +
      *    '   where FNTEST <> ''Y'') ' +
      *    'select AAFEIN, sum(premium)' +
      *    ' from WMAHP' +
      *    '  inner join nonTestGrps' +
      *    '   on AHCO# = FNCO# and AHFND = FNFND' +
      *    '  inner join WMAHP_sub' +
      *    '   on AHCO#  = lastAHCO#  and AHFND = lastAHFND and' +
      *    '      AHEMP# = lastAHEMP# and AHDIV = lastAHDIV and' +
      *    '      AHFYR  = lastAHFYR' +
      *    '  inner join WMAAP' +
      *    '   on AHCO#  = AACO#  and AHFND = AAFND and AHAGT# = AAAGT#' +
      *    '  left join WDELP_all' +
      *    '   on AHCO#  = ELCO#  and AHFND = ELFND and' +
      *    '      AHEMP# = ELEMP# and AHDIV = ELDIV and' +
      *    '      AHFYR  = ELFYR' +
      *    ' where premium <> 0' +
      *    ' group by AAFEIN' +
      *    ' order by sum(premium) desc ' +
      *    'fetch first 10 rows only';
      *Endsl;

       If inGrp# <> ' ';
         strSqlWhere1 = 'Where AHFND = ' + inGrp# + ' ';
       Else;
         strSqlWhere1 = 'Where AHFND in (Select GSFND From WXGSPDSH ' +
                                        'Where GSIEFL = ''I'') ';
       EndIf;

       Select;
       When isUseCiCenter;
          strSqlCursor = 'select PAFEIN, AGNAME, sum(POANPR) ' +
           'from WHPOP ' +
           %trim(strSQLWhere1) + ' ' +
           ' inner join WHPAP' +
           '  on POPONR = PAPONR ' +
           ' inner join WMAGP' +
           '  on AGFEIN = PAFEIN' +
           'group by PAFEIN, AGNAME ' +
           'order by sum(POANPR) desc ' +
           'fetch first 10 rows only';
       Other;
          strSqlCursor = 'with ' +
           'WMAHP_sub as' +
           ' (select AHCO# lastAHCO#, AHFND lastAHFND, AHEMP# lastAHEMP#,' +
           '         AHDIV lastAHDIV, AHFYR lastAHFYR, max(AHEFDT) lastAHEFDT' +
           '    from WMAHP ' +
           %trim(strSQLWhere1) + ' ' +
           '    group by AHCO#, AHFND, AHEMP#, AHDIV, AHFYR), ' +
           'stateRptDiv as' +
           ' (select EMCO#, EMFND, EMEMP#, EMDIV' +
           '   from WMEMP' +
           '   where EMSTRP = ''Y''), ' +
           'WDELP_all as' +
           ' (select ELCO#, ELFND, ELEMP#, ELDIV, ELFYR,' +
           '   case when ELFAF = ''A'' then ELAAPR' +
           '        else ELANPR' +
           '   end as premium' +
           '   from WDELP' +
           '   inner join stateRptDiv' +
           '    on ELCO#  = EMCO#  and ELFND = EMFND and' +
           '       ELEMP# = EMEMP# and ELDIV = EMDIV), ' +
           'nonTestGrps as' +
           ' (select FNCO#, FNFND' +
           '   from WMFNP' +
           '   where FNTEST <> ''Y'') ' +
           'select AAFEIN, AGNAME, sum(premium)' +
           ' from WMAHP' +
           '  inner join nonTestGrps' +
           '   on AHCO# = FNCO# and AHFND = FNFND' +
           '  inner join WMAHP_sub' +
           '   on AHCO#  = lastAHCO#  and AHFND = lastAHFND and' +
           '      AHEMP# = lastAHEMP# and AHDIV = lastAHDIV and' +
           '      AHFYR  = lastAHFYR' +
           '  inner join WMAAP' +
           '   on AHCO#  = AACO#  and AHFND = AAFND and AHAGT# = AAAGT#' +
           '  left join WDELP_all' +
           '   on AHCO#  = ELCO#  and AHFND = ELFND and' +
           '      AHEMP# = ELEMP# and AHDIV = ELDIV and' +
           '      AHFYR  = ELFYR' +
           '  inner join WMAGP' +
           '   on AHCO#  = AGCO#  and AGFEIN = AAFEIN' +
           ' where premium <> 0 and AGSTAT = ''A''' +
           ' group by AAFEIN, AGNAME' +
           ' order by sum(premium) desc ' +
           'fetch first 10 rows only';
       Endsl;

      *** End Chg ***** 07/16/24 *** WC2972 ****************************

       Exec SQL Prepare sqlTop10 From :strSqlCursor;
       Exec SQL Declare cursorTop10 Cursor For sqlTop10;
       Exec SQL Open cursorTop10;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorTop10 into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            Exsr ExtractData;
            names  = %trim(names) + %trim(strName);
            values = %trim(values) + %trim(%editc(value:'3'));
            StartJSONelemnt(jsonDataSet);
            BuildJson(jsonDataSet:'label':strName);
            BuildJson(jsonDataSet:'value':%trim(%editc(value:'3')));
            EndJSONelemnt(jsonDataSet);
         Endif;
       Enddo;
       Exec SQL Close cursorTop10;

      *** Beg Del ***** 07/16/24 *** WC2972 ****************************
      *** strToolTip = 'Top ' + %trim(%editc(index:'Z') +
      ***  ' agent premiums from state reporting divisions.');
      *** Beg Del ***** 07/16/24 *** WC2972 ****************************
       if index < 1;
          values = '0';
          names = 'No Agents';
          strToolTip = 'No Agents with Premium.';
       Endif;

       Exsr BldJsonData;
       //BuildJSON(outJSON :'chartNames':%trim(names));
       //BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       BuildJSON(outJSON :'sqlCursor':strSqlCursor);
       Exsr BuildOptions;
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;

       //==================================================================
       // Build JSON chart data
       //==================================================================
       Begsr BldJSONData;
        // Options
        StartJSONelemnt(jsonOptions);
        BuildJson(jsonOptions:'numberPrefix':'$');
        //BuildJson(jsonOptions:'paletteColors':'#0075c2,#1aaf5d,#f2c500');
        BuildJson(jsonOptions:'labelDisplay':'ROTATE');
        BuildJson(jsonOptions:'slantLabels':'1');
        EndJSONelemnt(jsonOptions);

        // Dataset
        jsonDataSet = '"data":['+ %trim(jsonDataSet) + ']';

        // Chart
        BldJsonChart(jsonChart:jsonOptions:jsonCategories:jsonDataSet);

       //***  Begin Add  ***  03/03/22  ****************
       //BuildBigJSON(outJSON :'chartJSON':jsonChart);
        outJson = %trim(outJSON) + ', "chartJSON":' + jsonChart;
       //***  End   Add  ***  03/03/22  ****************
       Endsr;

       //===================================================================
       Begsr BuildOptions;
        //strOptions = ' <rotateLabels>1</rotateLabels>';
        strOptions = %trim(strOptions) + ' labelDisplay=''Stagger''';
        strOptions = %trim(strOptions) + ' staggerLines=''2''';
        BuildJSON(outJSON :'chartOptions':%trim(strOptions));
       Endsr;

       //===================================================================
       Begsr ExtractData;
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
        strName = %subst(name: 1:25);
        // strName = %subst(name: 1:9);
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
        Leavesr;
        Select;
        When isUseCiCenter;
           decFein = %dec(%subst(name: 1:9):9:0);
         //decMod  = %dec(%subst(name:10:3):3:0);
         //decAgt# = GetAgent#(decCo#:decFein:decMod);
        Other;
           decAgt# = %dec(name:5:0);
        Endsl;
        //strName = GetAgtName(decAgt#);
        Select;
        When strName <> *blank;
        When decAgt# <> *zero;
           strName = %editc(decFein:'X');
        Other;
           strName = %subst(name:1:9) + ' ' + %subst(name:10:3);
        Endsl;
       Endsr;
      /End-Free

     PTop10AgentInfo   E

     p*=====================================================================
     p* Total Incurred by adjustor - Top 10
     p*  Parm1   - type of item
     P*=====================================================================
     PTotalIncurred    b                   export
     DTotalIncurred    pi                  like(rtnData)
     D inType                        15
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d inFrDt                         7
     d inToDt                         7
     d inGrp#                         3
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     DreturnInfo       s                   like(rtnData) inz
     Djson             s                   like(rtnData)
     D decCompare      s             11  0 inz(10000)
     D index           s              9  0 inz
     D strSqlCursor    s           5000
     D strSqlSum       s           5000
     D strSqlWhere     s           5000
     D strToolTip      s            500    inz
     D names           s            100    inz
     D values          s            100    inz
     D dsData          ds
     D  name                          3
     D  value                         9  0
      *
      /Free

       BuildJSON(outJSON :'itemType':'chart');
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
      //*** strSqlWhere = 'where CMSTAT = ''O'' ' +
      //***   %trim(BuildSQLInTeam('CMCLEX':'and'));

       If inGrp# <> ' ';
          strSqlWhere = 'where CMSTAT = ''O'' and CMFND = ' + inGrp# + ' ' +
           %trim(BuildSQLInTeam('CMCLEX':'and'));
       Else;
          strSqlWhere = 'where CMSTAT = ''O'' ' +
                        ' and CMFND in (Select GSFND From WXGSPDSH ' +
                                          'Where GSIEFL = ''I'') ' +
           %trim(BuildSQLInTeam('CMCLEX':'and'));
       EndIf;
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
       strSqlSum = 'SUM(CMRR01 + CMRR02 + CMRR03 + CMRR04 + CMRR05 +' +
        ' CMRR06 + CMRR07 + CMRR08 + CMRR09 + CMRR10 +' +
        ' CMAR01 + CMAR02 + CMAR03 + CMAR04 + CMAR05 +' +
        ' CMAR06 + CMAR07 + CMAR08 + CMAR09 + CMAR10)';
       strSqlCursor = 'with totals as ' +
        '(select CMCLEX, ' + %trim(strSqlSum) + ' as total' +
        ' from WMCMP ' +
        %trim(strSQLWhere) + ' ' +
        ' group by CMCLEX) ' +
        'select * from totals' +
        ' where total > 0' +
        ' order by total desc' +
        ' fetch first 10 rows only';

       Exec SQL Prepare sqlInc From :strSqlCursor;
       Exec SQL Declare cursorInc Cursor For sqlInc;
       Exec SQL Open cursorInc;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorInc into :dsData;
          If SQLSTT = '00000';
            index += 1;
            If index > 1;
               names = %trim(names) + ',';
               values = %trim(values) + ',';
            Endif;
            names = %trim(names) + RtvUserInfo(%trim(name));
            values = %trim(values) + %trim(%editc(value:'Z'));
         Endif;
       Enddo;
       Exec SQL Close cursorInc;

       strToolTip = 'Top ' + %trim(%editc(index:'Z') +
        ' ajusters on your team.');
       if index < 1;
          values = '0';
          names = 'No Incurred';
          strToolTip = 'No Incurred within your team.';
       Endif;

       BuildJSON(outJSON :'chartNames':%trim(names));
       BuildJSON(outJSON :'chartValues':%trim(values));
       BuildJSON(outJSON :'chartToolTip':%trim(strToolTip));
       BuildJSON(outJSON :'panelValue':%trim(DBdesc));
       //BuildJSON(outJSON :'sqlCursor':strSqlCursor);
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          BuildJSON(outJSON :'fltGroup':inGrp#);
      *** End Add ***** 07/16/24 *** WC2972 ****************************

       Return returnInfo;
      /End-Free

     PTotalIncurred    E

** Item Procedures
TestChart
TestGrid
ClosedSettle
LargeLossRpt
LitigationClaim
NewClaims
NewPremium
ClosedClaims
TotalIncurred
CxPolicies
PendingAudits
PolicyExpiring
Quoted
RegionalLosses
RenewalPremium
SICLosses
Top10AgentInfo
