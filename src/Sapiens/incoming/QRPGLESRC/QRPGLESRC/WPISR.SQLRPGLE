     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
      *=========================================================================
      * Revisions:                                                             *
      *
      * 09/27/22 C:65  Support: 96520
      *                For ListSRPayroll classes editing, turn off edit 1 for
      *                completed requests - leaving no edits in place.
      *
      * 08/23/22 C:65  Support: 96520
      *                For ListSRPayroll classes editing, turn on edits 3-5 if
      *                no status is found for the period.  Change the edit for
      *                disallowing report when prior period is pending payment
      *                to invoke when checkbox to allow is *not* checked. When
      *                returning prior period status, do not check if past due
      *                or if locked.
      *
      * 07/29/22 C:65  Support: 96520
      *                Allow report to be resubmitted if its marked as closed
      *                if it was previously locked due to config but config
      *                has been changed.
      *
      * 07/19/22 C:33  Support: 96520
      *                Update service edits for 3rd Party Payments.
      *=========================================================================

     ddsWTBSP        e ds                  extname(WTBSP) qualified inz

     ddecReportDt      s              7  0 inz
     ddt7              s              7  0 inz

     d/copy sPrDate
     d/copy sPrJson
     d/copy sPrSql
     d/copy sPrString
     d/copy wPrEmplr
     d/copy wPrSR
     d/include yajl_h

      //========================================================
      // Must be first procedure - only for commit
      //========================================================
     PLeaveAsFirst     b
      /Free
       Exec Sql Set Option Commit = *None;
      /End-free
     PLeaveAsFirst     e


    P*====================================================================
    P* GetSRReportStatus
    P*   co#      - accepts the company #      (required)
    P*   group    - accepts the group          (required)
    P*   employer - accepts the employer       (required)
    P*   division - accepts the division       (required)
    P*   year     - accepts the year           (required)
     P*   reportDt - accepts the reporting date (required)
     P*   period   - accepts the period to chk  (optional)
     P*               *CURRENT {default}
     P*               *PRIOR
    P*====================================================================
    P*
     PGetSRReportStatus...
     P                 b                   export
    P*
     DGetSRReportStatus...
     D                 pi            25    varying
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inReportDt                     7  0 const
     D inPeriod                      25    const varying options(*nopass)

     ddsSR             ds                  qualified inz
     d status                        25
     d stat                           1
     d inv#                          15  0

     disEof            s              1n   inz
     disPastDue        s              1n   inz
     dstrPeriod        s             25    varying inz('*CURRENT')
     dstrOperand       s             25    varying inz
     dstrOperator      s              5    varying inz(' = ')
     dstrSql           s           5000a   varying inz
     dstrSqlSelect     s           1000a   varying inz
     dstrSqlWhere      s           5000a   varying inz
     dstrSqlWhereSR    s           5000a   varying inz
     dstrStatus        s             25    varying inz

       If %parms >= 7 and inPeriod <> *blanks;
          strPeriod = inPeriod;
       Endif;

       Select;
       When Upper(strPeriod) = '*PRIOR';
          strOperator = ' < ';
       Endsl;

       decReportDt = inReportDt;
       Exec sql
        select * into :dsWTBSP
         from WTBSP
          where BSco# = :inCo# and
           BSkey = (select ELBSCD
                     from WDELP
                      where ELco# = :inCo# and ELfnd = :inFnd
                       and ELemp# = :inEmp# and ELdiv = :inDiv
                       and ELfyr = :inFyr);
       Select;
       When dsWTBSP.BSfreq = 'M';
          // Convert to CYYMM
          If decReportDt > 99999;
             decReportDt = decReportDt / 100;
          Endif;
          dt7 = (Dateto7(%date - %months(1)))/100;
          If decReportDt < dt7;
             isPastDue = *on;
          Endif;
          strOperand = 'SRrpMY';
        //strSqlWhereSR = ' where SRrpMY = ' + %char(decReportDt);
       Other;
          If decReportDt < Dateto7(%date - %days(7));
             isPastDue = *on;
          Endif;
          strOperand = 'SRtoDt';
        //strSqlWhereSR = ' where SRtoDt = ' + %char(decReportDt);
       Endsl;
       strSqlWhereSR = ' where ' + strOperand+strOperator + %char(decReportDt);
       strSqlWhereSR +=
        ' and SRco# = ' + %char(inCo#) +
        ' and SRfnd = ' + %char(inFnd) +
        ' and SRemp# = ' + %char(inEmp#) +
        ' and SRdiv = ' + %char(inDiv)+
        ' and SRfyr = ' + %char(inFyr);
       strSqlSelect = ' select SRCO#, SRFND, SREMP#, SRDIV, SRFYR, SRSTAT,' +
        '  SRCLAS, SRSPEC, SREXT, SREFDT, SRRPMY, SRFRDT, SRTODT,' +
        '  case when hex(SRINV#) = ''404040404040404040404040404040'' then 0'+
        '   else SRINV# end as SRINV#';
       strSql = ' with ' +
        'submitted as (' + strSqlSelect + ',' +
        ' ''Submitted'' as SR_status' +
        ' from WASRP ' + strSqlWhereSR + ')' +
        ', ' +
        'pending as (' + strSqlSelect + ',' +
        ' ''Pending'' as SR_status' +
        ' from WASRP_HLD ' + strSqlWhereSR + ')' +
        ', ' +
        'combined as (' +
        ' select * from submitted s' +
        '  where not exists' +
        '  (select * from pending p' +
        '    where s.SRCO#=p.SRCO#  and s.SRFND=p.SRFND' +
        '     and s.SREMP#=p.SREMP# and s.SRDIV=p.SRDIV' +
        '     and s.SRFYR=p.SRFYR   and s.SRCLAS=p.SRCLAS' +
        '     and s.SRSPEC=p.SRSPEC and s.SREXT=p.SREXT' +
        '     and s.SREFDT=p.SREFDT)' +
        ' union select * from pending) ' +
        'select SR_status, SRstat, SRinv# ' +
        '  from combined';
       Exec SQL close cursorGetSRReportStatus;
       Exec SQL prepare sqlGetSRReportStatus from :strSql;
       Exec SQL declare cursorGetSRReportStatus cursor for sqlGetSRReportStatus;
       Exec SQL open cursorGetSRReportStatus;
       Dou isEof;
          Exec SQL fetch next from cursorGetSRReportStatus into :dsSR;
          Select;
          When not IsValidFetch(sqlstt);
             isEof = *on;
             Iter;
          When dsSr.status = 'Pending';
             isEof = *on;
             Iter;
          Endsl;
       Enddo;
       Select;
       //*** begin chg *** 08/23/22  ****************************************
       //*****When dsSR.stat = 'L';
       When dsSR.stat = 'L' and Upper(strPeriod) <> '*PRIOR';
       //*** end chg ***** 08/23/22  ****************************************
          strStatus = 'Closed';
       //*** begin chg *** 08/23/22  ****************************************
       //*****When dsSR.status = 'Pending' and isPastDue;
       When dsSR.status = 'Pending' and isPastDue and
        Upper(strPeriod) <> '*PRIOR';
       //*** end chg ***** 08/23/22  ****************************************
          strStatus = 'Unsubmitted';
       When dsSR.status = 'Pending' and dsSR.inv# <> *zero;
          strStatus = 'Pending Payment';
       When isPastDue;
          strStatus = 'Completed';
       //When dsSr.stat = *blanks;
        //strStatus = 'Not Submitted';
       Other;
          strStatus = dsSR.status;
       Endsl;
       Return strStatus;
     PGetSRReportStatus...
     P                 e


    P*====================================================================
    P* IsValidReportStatus
     P*   process  - accepts the process        (required)
    P*   co#      - accepts the company #      (required)
    P*   group    - accepts the group          (required)
    P*   employer - accepts the employer       (required)
    P*   division - accepts the division       (required)
    P*   year     - accepts the year           (required)
     P*   reportDt - accetps the reporting date (required)
    P*====================================================================
    P*
     PIsValidReportStatus...
     P                 b                   export
    P*
     DIsValidReportStatus...
     D                 pi             1n
     D inProcess                    256    const varying
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inReportDt                     7  0 const

     Ddt7Cx            s              7  0 inz
     DisCx             s              1n   inz
     DisCxPend         s              1n   inz
     DstrMsg           s            500    inz varying
     DstrPeriod        s             10    inz
     DstrStatus        s             25    inz varying
     DstrStatusPrior   s             25    inz varying
     DsysDate7         s              7  0 inz

     D#ofEdits         c                   5
      // 1 - Always
      // 2 - (N1lckRsbmt) Lock Self-Report Resubmit
      // 3 - (N1allwPend) Allow Report if Pending Payment from Last Period (via 3rd party only)
      // 4 - (N1alwCx)    Allow Report on Cancelled Policy
      // 5 - (N1alwCxPnd) Allow Report on Cancel Pending Policy
     DaryEdits         s              1n   dim(#ofEdits) inz

     DdsWINOTIP1     e ds                  extname(WINOTIP1) qualified inz

       sysDate7 = DateTo7(%date);

       Exec sql
        select * into :dsWINOTIP1
         from WINOTIP1
          where N1co# = :inCo#;

       strStatus = GetSRReportStatus(inCo#:inFnd:inEmp#:inDiv:inFyr
        :inReportDt);
       strStatusPrior = GetSRReportStatus(inCo#:inFnd:inEmp#:inDiv:inFyr
        :inReportDt:'*PRIOR');

       Exsr PopulateEdits;

       Select;
       When dsWTBSP.BSfreq = 'M';
          strPeriod = %editw(Dt6MDYY(decReportDt):'    /    ');
       Other;
          strPeriod = %editw(DtMDYY(decReportDt):'  /  /    ');
       Endsl;

       // Check policy cancellation status against S/R flags
       dt7Cx = GetCxDate(inCo#:inFnd:inEmp#:inDiv);
       Reset isCx;
       Reset isCxPend;
       Select;
       When dt7Cx = *zero;
       When dt7Cx > sysDate7;
          isCxPend = *on;
       When dt7Cx <= sysDate7;
          isCx = *on;
       Endsl;

       Reset strMsg;
       Select;
       When aryEdits(1);
          strMsg = 'Period not available for this process due to ' + strStatus +
           ' period status.';
       When aryEdits(2) and dsWINOTIP1.N1lckRsbmt = 'Y';
          strMsg = 'Period not available for this process due to ' + strStatus +
           ' period status.';
       //*** begin chg *** 08/23/22  ****************************************
       //*****When aryEdits(3) and dsWINOTIP1.N1allwPend = 'Y'
       When aryEdits(3) and dsWINOTIP1.N1allwPend <> 'Y'
       //*** end chg ***** 08/23/22  ****************************************
        and strStatusPrior = 'Pending Payment';
          strMsg = 'Period not available for this process due to prior period' +
           ' pending payment.';
       When aryEdits(4) and dsWINOTIP1.N1alwCx <> 'Y' and isCx;
          strMsg = 'Period not available for this process due to ' +
           'policy cancelled.';
       When aryEdits(5) and dsWINOTIP1.N1alwCxPnd <> 'Y' and isCxPend;
          strMsg = 'Period not available for this process due to ' +
           'policy pending cancellation.';
       Endsl;
       If strMsg <> *blank;
          strMsg += '  Division: ' + %char(inDiv) +
           ' Year: ' + %char(inFyr) + ' Period: ' + %trim(strPeriod);
          AddErrorMessageJson(strMsg);
          Return *on;
       Endif;
       Return *off;

       //==================================
       Begsr PopulateEdits;
         //Select;
         //When strStatus = 'Pending';
         //When strStatus = 'Submitted';
         //When strStatus = 'Closed';
         //When strStatus = 'Completed';
         //When strStatus = 'Unsubmitted';
         //When strStatus = 'Pending Payment';
         //When strStatus = 'Delete3rdPartyPayment';
         //When strStatus = 'TransferPayment';
         //Endsl;
        Reset aryEdits;
        Select;
        When inProcess = 'ListSRPayrollClasses';
           Select;
           When strStatus = 'Pending';
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Submitted';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Closed';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Completed';
        //*** begin del *** 09/27/22  ****************************************
        //****aryEdits(1) = *on;
        //*** end del ***** 09/27/22  ****************************************
           When strStatus = 'Unsubmitted';
              aryEdits(1) = *on;
           When strStatus = 'Pending Payment';
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
        //*** begin add *** 08/23/22  ****************************************
           When strStatus = ' ';
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
        //*** end add ***** 08/23/22  ****************************************
           Endsl;
        When inProcess = 'ProcessSRPayments';
           Select;
           When strStatus = 'Pending';
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Submitted';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Closed';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Completed';
              aryEdits(1) = *on;
           When strStatus = 'Unsubmitted';
              aryEdits(1) = *on;
           When strStatus = 'Pending Payment';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           Endsl;
        When inProcess = 'SaveSRPayrolls';
           Select;
           When strStatus = 'Pending';
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Submitted';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           When strStatus = 'Closed';
           //*** begin chg ***  07/29/22  *************************************
           //*****   aryEdits(1) = *on;
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           //*** end chg *****  07/29/22  *************************************
           When strStatus = 'Completed';
              aryEdits(1) = *on;
           When strStatus = 'Unsubmitted';
              aryEdits(1) = *on;
           When strStatus = 'Pending Payment';
              aryEdits(2) = *on;
              aryEdits(3) = *on;
              aryEdits(4) = *on;
              aryEdits(5) = *on;
           Endsl;
        Endsl;
       Endsr;
     PIsValidReportStatus...
     P                 e
