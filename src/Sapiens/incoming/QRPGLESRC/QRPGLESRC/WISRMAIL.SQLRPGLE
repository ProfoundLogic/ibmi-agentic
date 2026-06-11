     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP('CONNECT')
     H BNDDIR('WTBNDDIR':'YAJL')
     H*====================================================================
     H/TITLE WISRMAIL - Email program for S/R Online/Portal
     H*====================================================================
     H*  Description:
     H*
     H*  This program will generate emails for S/R policies serviced by
     H*  S/R online and Portal.
     H*
     H*  Created:  01/22/19  C:65  Project: SY407
     H*====================================================================
     H*  Revisions:
      *
      *  01/20/23 C:344 Project: WC2875
      *                -Inserted execution to ReplaceTokensEmail procedure
      *                 to replace the call to the original
      *                 ReplaceEmailTokens procedure. The following parms
      *                 were added:
      *                 1. String for the email subject or body
      *                 2. RunInfo will contain the notification keys.
      *                 3. RRN from the WDELP record of the quote or
      *                 policy being processed.
      *                 4. RRNType will be Policy.
      *                 5. Report Date which is determined by the SR
      *                 Notification procedures within this program.
      *  12/14/22 C:65  Support: 105481 (UBIC)
      *                -In SendEmail procedure skip processing if
      *                 notification is inactive.
      *
      *  06/07/22 C:33  Support: 102528
      *                -Only call SRONLI once for each employer
      *
      *  04/12/22 C:33  Support: 101929
      *                -Process Daliy Notifications for all divisions
      *                -Read through all contacts
      *                -Include contacts for non-division specific
      *                -Use SRDUEDATE in CanNotify
      *
      *  02/01/22 C:33  Support: 97790
      *                 Encopass email addresses in ''
     H*
     H* 10/26/21 C:33  Support: 99007
     H*                Check group 0 when looking for overall default email
     H*
     H* 11/20/20 C:136 Project: SY407 (DE772)
     H*                Add multiple address functionality.
     H*====================================================================
     fwdell     if   e           k Disk    Rename(pel01:pel00)
      *** Beg Add *** 01/20/2023 *** WC2875 *******************************
     f                                     infds(dbfds)
      *** End Add *** 01/20/2023 *** WC2875 *******************************
     fwidftemll if   e           k Disk    Rename(pem01:pdf00)
     fwinotil   if   e           k Disk    Rename(pno01:pno00)
     fwinotip1  if   e           k Disk    Rename(pn101:pn101)
     fwisrprem  if   e           k Disk    Rename(pre01:pre00)
     fwmecl1    if   e           k Disk    Rename(pec01:pec01)
     fwmeml     if   e           k Disk    Rename(pem01:pem00)
     fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     fwmofl     if   e           k Disk    Rename(pof01:pof00)
     fwmrpl2    if   e           k Disk    Rename(prp01:prp02)
     fwtabl     if   e           k Disk    Include(pbs01)

      *entry
     dRequestPgm       pr                  ExtPgm('WISRMAIL')
     d                              256
     d                               10    Options(*NoPass)
     d                                 a   Len(8500000) Varying Options(*NoPass)
     d                                 a   Len(8500000) Varying Options(*NoPass)
     d                               10    Options(*NoPass)
     dRequestPgm       pi
     d inRequest                    256
     d inUser                        10    Options(*NoPass)
     d inParms                         a   Len(8500000) Varying Options(*NoPass)
     d outJSON                         a   Len(8500000) Varying Options(*NoPass)
     d outRtnCode                    10    Options(*NoPass)

     d cltID           s              3
     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d key             s             50a   varying
     d node            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     d***  Begin Chg  ***  04/12/22  ****************
     d*dateMnthInit    s               d
     d*dateMnthRem     s               d
     d*dateDueRem      s               d
     d dateMnthInit    s               d   inz
     d dateMnthRem     s               d   inz
     d dateDueRem      s               d   inz
     d***  End   Chg  ***  04/12/22  ****************
     d dateToday       s               d
     d daysDue         s              3s 0
     d daysDueW        s              3s 0
     d daysLate        s              3s 0
     d daysLateW       s              3s 0
     d decReportDate   s              8s 0
     d dtaqLib         s             10    Inz(' ')
     d dtaqName        s             10    Inz('SRODATQ')
     d hasMnthDueInfo  s               n
     d hasMnthInInfo   s               n
     d hasMnthRmInfo   s               n
     d hasWeekInInfo   s               n
     d inJson          s               a   Len(2000000) varying
     d intCo#          s                   Like(NOco#)
     d intDayOfMnth    s              2s 0
     d intFnd          s                   Like(FNfnd)
     d intMnthRmDays   s                   Like(NOnumb1)
     d isEnvQuote      s              1n
     d isEnvLive       s              1n
     d isLogoReady     s              1n   inz('1')
     d isSrNotify      s               n
     d keyData         s             25
     d late            s              1a
     d QQCo#           s              3
     d QQDiv           s              5
     d QQEfdt          s              8
     d QQEmp#          s              9
     d QQFnd           s              3
     d QQFyr           s              3
     d strDartsType    s              2
     d strEnv          s              4a
     d strErrorMsg     s            500a   Varying
     d strLogoReplace  s            200a   Varying
     d strUser         s             10a
     d xxCount         s              6s 0
     d xxData          s            100
     d xxDiv           s              5  0
     d xxErr           s              1
     d xxFyr           s              3  0
     d xxMode          s              1
     d xxSysDt         s              7  0
     d xxTime          s              6s 0
     d outsuffix       s              3    Inz
     d outFolderPfx    s             80    Inz

     d                 ds
     d  xxCmpDt                1      7  0
     d  xxCmpYM                1      5  0
     d  xxCmpYr                1      3  0
     d  xxCmpMth               4      5  0
     d  xxCmpDay               6      7  0

     d                 ds
     d  xxPolExpDt             1      7  0
     d  xxPolYM                1      5  0
     d  xxPolYr                1      3  0
     d  xxPolMth               4      5  0
     d  xxPolDay               6      7  0

     d                 ds
     d  x4mode                 1    300
     d  x4id                   1     25
     d  x4emp#                 1      9
     d  x4time                10     15
     d  x4user                16     25
     d  x4aemp                26     34
     d  x4adiv                35     39
     d  x4afyr                40     42
     d  x4freq                43     43
     d  x4splt                44     44
     d  x4frmdt               45     52
     d  x4todt                53     60
     d  x4srin                61     61

     d                 ds
     d***  Begin Chg  ***  04/12/22  ****************
     d*strEmailTo                   630
     d strEmailTo                   630    inz
     d***  End   Chg  ***  04/12/22  ****************
     d  aryEmailTo                   63    Dim(10) Overlay(strEmailTo)

     d W7221R          ds                  dtaara('QTEMP/W7221R') qualified inz
     d  path                        250a

      // Email Tokens
     d eMTkn           s             25    Dim(30) Ctdata Perrcd(1)

     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10

     d AgtInfoAPI      pr                  extpgm('WA002R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##EMP#                       9  0
     d   ##DIV                        5  0
     d   ##FYR                        3  0
     d   ##AGT#                       5  0
     d   ##ANAM                      60
     d   ##PROD                       5  0
     d   ##PNAM                      60

     d SROnline        pr                  extpgm('SRONLI')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##FYR                        3
     d   ##EFDT                       8
     d   ##MODE                       1
     d   ##ERR                        1
     d   ##DATA                     100

     d ReceiveDTAQ     pr                  extpgm('QRCVDTAQ')
     d   ##DTAQ                      10    const
     d   ##LIB                       10    const
     d   ##LEN                        5p 0 const
     d   ##DATA                     300
     d   ##WAIT                       5p 0 const
     d   ##KEYO                       2    const
     d   ##KEYL                       3p 0 const
     d   ##KEY                       25    const
     d   ##SLEN                       3p 0 const
     d   ##SDATA                     10    const

     d GetSuffix       pr                  extpgm('SA030R')
     d   outsuffix                    3
     d   outFolderPfx                80

     dGetClientID      pr                  extpgm('RTVCLTID')
     d outCltID                       3

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a

     d psds          esds                  extname(szq1p)
      //*** Beg Add *** 01/20/23 *** WC2875 ********************************
     d dbfds           ds
     d  dbrrn                397    400b 0
      //*** End Add *** 01/20/23 *** WC2875 ********************************

      /Copy sPrDate
      /Copy sPrJson
      /Copy sPrRunCmd
       //***  Begin Add  ***  04/12/22  ****************
      /copy sPrSql
       //***  End   Add  ***  04/12/22  ****************
      /Copy sPrString
      /Copy wPrUsrPrms
      //*** Beg Add *** 01/20/23 ***  WC2875  *********************************
      /copy sPrEmail
      //*** End Add *** 01/20/23 ***  WC2875  *********************************
      /Include yajl_h

      /Free
       //***  Begin Add  ***  04/12/22  ****************
       Exec Sql Set Option Commit = *None;
       //***  End   Add  ***  04/12/22  ****************
       If %Parms() >= 2;
          strUser = inUser;
       Else;
          strUser = Q1user;
       Endif;
       GetClientID(cltID);
       RtvEnv(strEnv);
       isEnvQuote = strEnv = '*QTE';
       isEnvLive = strEnv = '*LIV';
       dateToday = %Date;
       Select;
       When inRequest = 'ImmediateSRNotifications';
          ImmediateSrNotifications();
       Other;
          DailySrNotifications();
       Endsl;

       *Inlr = *On;
       //===================================================================
       // Pull notification setup from Application Configuration
       //===================================================================
       Dcl-Proc CanNotify;
       Dcl-Pi *N Ind;
       End-Pi;
       //***  Begin Add  ***  04/12/22  *******************
       Reset dateMnthInit;
       Reset dateMnthRem;
       Reset dateDueRem;
       //***  End   Add  ***  04/12/22  *******************
       Chain (intCo#) pn101;
       Chain (intCo#:intFnd) pfn00;
       Chain (intCo#:'N001':'SRINITWKLY') pno00;
       If %Found And NoStat = 'A';
          hasWeekInInfo = *On;
       Endif;
       Chain (intCo#:'N001':'SRINITMTH') pno00;
       If %Found And NoStat = 'A';
          hasMnthInInfo = *On;
         // Find out what day of month to send email from NOnumb1
          intDayOfMnth  = %Subdt(dateToday:*Days);
          dateMnthInit  = dateToday - %Days(intDayOfMnth);
          dateMnthInit  = dateMnthInit + %Days(NOnumb1);
       Endif;
       If FNmidy <> 0;
          Chain (intCo#:'N001':'SRREMINDER') pno00;
          If %Found And NoStat = 'A';
             hasMnthRmInfo = *On;
            // Figure out which day to send reminder email.
            //  Due date - reminder # of days
             intDayOfMnth  = %Subdt(dateToday:*Days);
             dateMnthRem = dateToday - %Days(intDayOfMnth);
             dateMnthRem = dateMnthRem + %Days(FNmidy)
              - %Days(NOnumb1);
          Endif;
        //***  Begin Chg  ***  04/12/22  *************
        //Chain (intCo#:'N001':'SRDUE') pno00;
          Chain (intCo#:'N001':'SRDUEDATE') pno00;
        //***  End   Chg  ***  04/12/22  *************
          If %Found And NoStat = 'A';
             hasMnthDueInfo = *On;
            // Figure out which day to send due date
            //  reminder email
             intDayOfMnth  = %Subdt(dateToday:*Days);
             dateDueRem = dateToday - %Days(intDayOfMnth);
             dateDueRem = dateDueRem + %Days(FNmidy);
          Endif;
       Endif;
       Select;
       When hasWeekInInfo = *On;
          Return *On;
       When hasMnthInInfo = *On;
          Return *On;
       When hasMnthRmInfo = *On;
          Return *On;
       When hasMnthDueInfo = *On;
          Return *On;
       Other;
          Return *Off;
       EndSl;
       End-Proc;
       //===================================================
       // DailySRNotifications - called from job scheduler
       //===================================================
       Dcl-Proc DailySRNotifications;

        //***  Begin Add  ***  04/12/22  *****************
       Dcl-s isCanNotify ind inz(*on);
       Dcl-s isEofDailySRNotifications ind inz;

       Dcl-ds dsWDELP extname('WDELP');
       End-ds;
        //***  End   Add  ***  04/12/22  *****************
       xxSysDt = Dateto7(dateToday);
       xxCmpDt = DateTo7(dateToday - %Months(2));
       GetObjLib(dtaqName:'*DTAQ':dtaqLib);
       If dtaqLib = *Blanks;
          Return;
       EndIf;
        //***  Begin Chg  ***  04/12/22  *****************
        //Read pel00;
        //Dow Not(%Eof);
        //If intCo# <> ELco#
        // Or intFnd <> ELfnd;
        //   intCo# = ELco#;
        //   intFnd = ELfnd;
        //   If CanNotify() = *Off;
        //      Setgt (intCo#:intFnd) pel00;
        //      Read pel00;
        //      Iter;
        //   EndIf;
        //EndIf;
        /// Skip if policy out of date range
        //xxPolExpDt = ELPtdt;
        //If ELpfdt >= xxSysDt Or xxPolYM <= xxCmpYM;
        //   Read pel00;
        //   Iter;
        //Endif;
        /// Skip estimating policies
        //Clear BSsefl;
        //Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
        //If BSsefl <> 'S';
        //   Read pel00;
        //   Iter;
        //Endif;
       //***  Begin Add  ***  06/07/22  ***************
       Clear QQEmp#;
       //***  End   Add  ***  06/07/22  ***************
       Exec sql close cursorDailySRNotifications;
       Exec sql
        declare cursorDailySRNotifications cursor for
         select WDELP.*
          from WDELP
          join WTBSP on ELco# = BSco# and digits(ELbsCd) = BSkey
          where ELpfDt < :xxSysDt and ELptDt/100 > :xxCmpYM
           and BSseFl = 'S'
          order by ELco#, ELfnd, ELemp#, ELdiv, ELfyr;
       Exec sql open cursorDailySRNotifications;
       Dou isEofDailySRNotifications;
          Exec sql fetch next from cursorDailySRNotifications into :dsWDELP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofDailySRNotifications = *on;
             Iter;
          When intCo# <> ELco# or intFnd <> ELfnd;
             Reset isCanNotify;
             intCo# = ELco#;
             intFnd = ELfnd;
             If not CanNotify();
                isCanNotify = *off;
                Iter;
             Endif;
          When not isCanNotify;
             Iter;
        //***  Begin Add  ***  06/07/22  ***************
          When QQEmp# = %editc(ELEmp#:'X');
          // Only process the first divsion (SRONLI will look at all)
             Iter;
        //***  End   Add  ***  06/07/22  ***************
          Endsl;
        //***  End   Chg  ***  04/12/22  *****************
         // Call SRONLI in mode 4 to write to data queue
          QQCo# = %editc(ELCo#:'X');
          QQFnd = %editc(ELFnd:'X');
          QQEmp# = %editc(ELEmp#:'X');
          QQFyr = '000';
        //***  Begin Chg  ***  04/12/22  *****************
        //QQDiv = '00000';
          qqDiv = %editc(ELdiv:'X');
        //***  End   Chg  ***  04/12/22  *****************
          QQEfdt = '00000000';
          xxMode = '4';
          xxErr = ' ';
          xxTime = Timeto6(SystemTime());
          keyData = QQEmp# + %editc(xxTime:'X') + %trim(Q1User);
          xxData = keyData;
          SROnline(QQCo#:QQFnd:QQEmp#:QQDiv:QQFyr:QQEfdt:xxMode:
           xxErr:xxData);
         // SRONLI got a hit for a report due
          If xxData <> ' ';
             Monitor;
                xxCount = %Int(%Subst(xxData:1:6));
             On-Error;
                xxCount = 0;
             Endmon;
          Endif;
         // Read data queue for indicated # of entries.
          Dow xxCount > 0;
             Clear x4Mode;
             ReceiveDtaQ(dtaqName:dtaqLib:300:x4Mode:5:'EQ':25:
              keyData:0:' ');
             If X4ToDt <> ' ';
                 Monitor;
                   decReportDate = %Int(X4ToDt);
                On-Error;
                   decReportDate = 0;
                Endmon;
             Endif;
            // Go to next employer if no report date
             If decReportDate = 0;
                Leave;
             EndIf;
             xxDiv = %Int(x4ADiv);
             xxFyr = %Int(x4AFyr);
            // Don't send email if already reported, else send email
             Select;
             When X4srin = 'Y';
             When X4Freq = 'M';
            // Send monthly initial notification email
                If hasMnthInInfo and dateToday = dateMnthInit;
                   SendEmail(ELCo#:ELFnd:ELEmp#:xxDiv:xxFyr:
                    'N001':'SRINITMTH');
                Endif;
            // Send monthly reminder notification email
                If hasMnthRmInfo and dateToday = dateMnthRem;
                   SendEmail(ELCo#:ELFnd:ELEmp#:xxDiv:xxFyr:
                    'N001':'SRREMINDER');
                Endif;
            // Send due date reminder notification email
                If hasMnthDueInfo and dateToday = dateDueRem;
                   SendEmail(ELCo#:ELFnd:ELEmp#:xxDiv:xxFyr:
                    'N001':'SRDUEDATE');
                Endif;
             When X4Freq = 'W';
               // Send weekly initial email 1 day after report is due
                If hasWeekInInfo and xxSysDt =
                 DtCYMD(DtFuture(decReportDate:1));
                   SendEmail(ELCo#:ELFnd:ELEmp#:xxDiv:xxFyr:
                    'N001':'SRINITWKLY');
                Endif;
             Endsl;
             xxCount = xxCount - 1;
          Enddo;
        //***  Begin Del  ***  04/12/22  *****************
        /// Get next employer
        //Setgt (ELCo#:ELFnd:ELEmp#) pel00;
        //Read pel00;
        //***  End   Del  ***  04/12/22  *****************
       Enddo;
       End-Proc DailySRNotifications;
       //===================================================================
       // Get to address for email
       //  Hierarchy:
       //   Policy Addresses - S/R Contact, Underwriter, etc
       //   Default by Category/Group
       //   Default by Category
       //===================================================================
       Dcl-Proc GetEmailToAddress;
       Dcl-Pi *N;
        inCo# Like(EMco#);
        inFnd Like(EMfnd);
        inEmp# Like(EMemp#);
        inDiv Like(EMdiv);
        inCategory Like(NOctgy);
        inType Like(NOtype);
        inToUnderWriter Like(NOflag1);
       End-Pi;
       Dcl-S idx Packed(2:0);
       //***  Begin Add  ***  04/12/22  ****************
       Dcl-s isEofSRContacts ind inz;

       Dcl-ds dsWMECP extname('WMECP');
       End-ds;

       Reset strEmailTo;
       //***  End   Add  ***  04/12/22  ****************
       Exsr BuildAddressArray;
       Exsr FormatArray;
       Return;
       //-----------------------------
       Begsr BuildAddressArray;
        Select;
       // S/R Notification
        When inCategory = 'N001';
         //***  Begin Chg  ***  04/12/22  ****************
         //Setll (inCo#:inFnd:inEmp#:inDiv) pec01;
         //Dou Not %Eof(WMECL1);
         //Dou %Eof(WMECL1);
         //   Reade (inCo#:inFnd:inEmp#:inDiv) pec01;
         //   If %Eof;
         //      Leave;
         //   Endif;
         //   If ECsrem = 'Y';
         //      Exsr GetIdx;
         //      aryEmailTo(idx) = ECemal;
         //   Endif;
         //Enddo;
           Exec sql close cursorSRContacts;
           Exec sql declare cursorSRContacts cursor for
            select *
             from WMECP
              where ECco#=:inCo# and ECfnd=:inFnd and ECemp#=:inEmp#
               and (ECdiv=:inDiv or ECdivSpec<>'Y') and ECsrEm = 'Y';
           Exec sql open cursorSRContacts;
           Dou isEofSRContacts;
              Exec sql fetch next from cursorSRContacts into :dsWMECP;
              Select;
              When not IsValidFetch(sqlstt);
                 isEofSRContacts = *on;
                 Iter;
              Endsl;
              Exsr GetIdx;
              aryEmailTo(idx) = ECemal;
           Enddo;
         //***  End   Chg  ***  04/12/22  ****************
           If strEmailTo <> *Blanks;
              Leavesr;
           Endif;
       // Change Status and Underwriter Flagged
        When inCategory = 'N003' And inToUnderWriter = 'Y';
           strEmailTo = GetUsrEmail(GetUserID(EMuwtr));
           If strEmailTo <> *Blanks;
              Leavesr;
           Endif;
        EndSl;
       // Return default email address set up in App Config
       //  Return group specific address
        Setll (inCo#:inCategory:inFnd) pdf00;
        Dou %Eof(WIDFTEMLL);
           Reade (inCo#:inCategory:inFnd) pdf00;
           If %Eof;
              Leave;
           Endif;
           Exsr GetIdx;
           aryEmailTo(idx) = EMDftEmail;
        Enddo;
        If strEmailTo <> *Blanks;
           Leavesr;
        Endif;
       //  Return company address
        //***  Begin Chg  ***  10/26/21  ************
        //Setll (inCo#:inCategory) pdf00;
        Setll (inCo#:inCategory:0) pdf00;
        //***  End   Chg  ***  10/26/21  ************
        Dou %Eof(WIDFTEMLL);
         //***  Begin Chg  ***  10/26/21  ************
         //Reade (inCo#:inCategory) pdf00;
           Reade (inCo#:inCategory:0) pdf00;
         //***  End   Chg  ***  10/26/21  ************
           If %Eof;
              Leave;
           Endif;
           Exsr GetIdx;
           aryEmailTo(idx) = EMDftEmail;
        Enddo;
        If strEmailTo <> *Blanks;
           Leavesr;
        Endif;
       Endsr;
       //***  Begin Add  ***  04/12/22  ****************
       //-----------------------------
       Begsr BuildCursor;
       Endsr;
       //***  End   Add  ***  04/12/22  ****************
       //-----------------------------
       Begsr GetIdx;
        idx += 1;
        If idx > 10;
           Exsr FormatArray;
           Return;
        Endif;
       Endsr;
       //-----------------------------
       Begsr FormatArray;
        For idx = 1 to 10;
           If aryEmailTo(idx) <> *blank;
              aryEmailTo(idx) = '''' + CvtRpgToHtml(aryEmailTo(idx));
              %subst(aryEmailTo(idx):62:2) = ''' ';
           Endif;
        Endfor;
       Endsr;
       End-Proc GetEmailToAddress;
       //===================================================================
       // Get logo for email
       //===================================================================
       Dcl-Proc GetLogoAddress;
       Dcl-Pi *N varchar(150);
        inCo# Like(EMco#);
        inFnd Like(EMfnd);
        inEmp# Like(EMemp#);
        inDiv Like(EMdiv);
        inCategory Like(NOctgy);
        inType Like(NOtype);
       End-Pi;
       If not isLogoReady;
          Return *blanks;
       Endif;
      // Return logo set up in App Config
       Chain (inCo#:inCategory:inFnd) pdf00;
       If %Found;
          Return %trim(EMlogo);
       Endif;
       Chain (inCo#:inCategory) pdf00;
       If %Found;
          Return %trim(EMlogo);
       Endif;
       Return *Blanks;
       End-Proc GetLogoAddress;
       //===================================================================
       // Get logo replacement information
       //===================================================================
       Dcl-Proc GetLogoReplace;
       Dcl-Pi *N varchar(200);
        inCo# Like(EMco#);
        inFnd Like(EMfnd);
        inEmp# Like(EMemp#);
        inDiv Like(EMdiv);
        inCategory Like(NOctgy);
        inType Like(NOtype);
       End-Pi;
       Dcl-s strReplace varchar(200);
       strReplace =
        GetLogoAddress(inCo#:inFnd:inEmp#:inDiv:inCategory:inType);
       If strReplace <> *blanks;
          strReplace = '<img src="' + strReplace + '"/>';
       Endif;
       Return strReplace;
       End-Proc GetLogoReplace;
       //===================================================================
       // Process immediate SR Notifications
       // - incoming JSON (inParms)
       // {
       //      "category":"",
       //      "notificationType": "",
       //      "company": "",
       //      "group": "",
       //      "employer": "",
       //      "division": "",
       //      "year": "",
       //      "reportDate": "" (optional)
       // }
       //===================================================================
       Dcl-Proc ImmediateSRNotifications;
       Dcl-ds dsIn qualified inz;
        category char(4);
        type char(10);
        co# varchar(3);
        group varchar(3);
        emp# varchar(9);
        div varchar(5);
        year varchar(3);
        reportDate varchar(8);
        attachment varChar(250);
       End-ds;
       Dcl-s decCo# packed(3);
       Dcl-s decDiv packed(5);
       Dcl-s decEmp# packed(9);
       Dcl-s decFnd packed(3);
       Dcl-s decFyr packed(3);

       Exsr GetParms;
       Exsr ProcessInfo;

       //-------------------------------------------------------
       Begsr GetParms;
       //-------------------------------------------------------
        inJson = inParms;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg = ' ';
           //json = json + ' "requestParms" : {';
           Dow YAJL_object_loop(node0:i0:key:val);
              Select;
              When key = 'notificationCategory';
                 dsIn.category = YAJL_get_string(val);
              When key = 'notificationType';
                 dsIn.type = YAJL_get_string(val);
              When key = 'company';
                 dsIn.co# = YAJL_get_string(val);
              When key = 'group';
                 dsIn.group = YAJL_get_string(val);
              When key = 'employer';
                 dsIn.emp# = YAJL_get_string(val);
              When key = 'division';
                 dsIn.div = YAJL_get_string(val);
              When key = 'year';
                 dsIn.year = YAJL_get_string(val);
              When key = 'reportDate';
                 dsIn.reportDate = YAJL_get_string(val);
              When key = 'attachment';
                 dsIn.attachment = YAJL_get_string(val);
              Endsl;
           Enddo;
           Yajl_Tree_Free(node0);
        Else;
           AddErrorMessageJson('Request JSON could not be read.':outRtnCode);
        Endif;
        Monitor;
           decCo# = %Int(dsIn.co#);
           decFnd = %Int(dsIn.group);
           decEmp# = %Int(dsIn.emp#);
           decDiv = %Int(dsIn.div);
           decFyr = %Int(dsIn.year);
           decReportDate = %Int(dsIn.reportDate);
        On-error;
           AddErrorMessageJson('Character data in numeric fields.':outRtnCode);
        Endmon;
        Chain (decCo#:decFnd:decEmp#:decDiv) pem00;
        If Not %Found;
           AddErrorMessageJson('Employer not found.':outRtnCode);
           LeaveSr;
        Endif;
        Chain (decCo#:decFnd:decEmp#:decDiv:decFyr) pel00;
        If Not %Found;
           AddErrorMessageJson('Policy year not found.':outRtnCode);
           LeaveSr;
        Endif;
       // Get global values like the IP Address
        Chain (decCo#) pn101;
       Endsr;

       //---------------------------------------------------
       Begsr ProcessInfo;
       //---------------------------------------------------
        If outRtnCode = 'Error';
           LeaveSr;
        Endif;
        SendEmail(decCo#:decFnd:decEmp#:decDiv:decFyr:
         dsIn.category:dsIn.type:dsIn.attachment);
       Endsr;

       End-Proc;
       //======================================================================
       // Replace tokens in input string with data
       //======================================================================
       Dcl-Proc ReplaceEmailTokens;
       Dcl-Pi *N;
        inSubject Like(NOsubj);
        inBody Like(NOBody);
       End-Pi;
       Dcl-S chrFrequency Char(1);
       Dcl-S decAgt#  Packed(5:0);
       Dcl-S decPostAmt Packed(9:0);
       Dcl-S decProd# Packed(5:0);
       Dcl-S decSubmitterPhone zoned(10:0) inz;
       Dcl-S idx Zoned(2:0);
       Dcl-S ix1 Zoned(7:0);
       Dcl-S ix2 Zoned(7:0);
       Dcl-S outString Like(NObody);
       Dcl-S intDayOfMth Zoned(2:0);
       Dcl-S intPostCymd Packed(7:0);
       Dcl-s strAgtName Char(60);
       Dcl-S strPolicy# Like(RPcap#);
       Dcl-S strProdName Char(60);
       Dcl-S strReportPeriod Char(23);
       Dcl-S strSubmitterEmail varchar(100) inz;
       Dcl-S strSubmitterName  varchar(100) inz;
       Dcl-S strSubmitterPhone varchar(100) inz;
       Dcl-S strToken VarChar(25);
       Dcl-S workBegDate Date(*USA);
       Dcl-S workEndDate Date(*USA);
       Dcl-S workPolFrDt Date(*USA);
       Dcl-S workDueDate Date(*USA);
       // Get agent info
       AgtInfoAPI(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:decAgt#
        :strAgtName:decProd#:strProdName);
       Exec sql
        select APSBMUSER, APSBMEMAIL, APSBMPHN#
         into :strSubmitterName, :strSubmitterEmail, :decSubmitterPhone
         from WIAPP
         join WIEMP on APCO# = EMCO# and APAPP# = EMAPP#
         where EMCO# = :ELco# and EMFND = :ELfnd and EMEMP# = :ELemp#
          and EMFYR = :ELfyr;
       If decSubmitterPhone <> *zero;
          strSubmitterPhone = %editc(decSubmitterPhone:'Z');
          Select;
          When %len(strSubmitterPhone) > 7;
             strSubmitterPhone = '(' + %subst(strSubmitterPhone:1:3) + ')' +
              %subst(strSubmitterPhone:4:3) + '-' +
              %subst(strSubmitterPhone:7:4);
          Other;
             strSubmitterPhone = %subst(strSubmitterPhone:1:3) + '-' +
              %subst(strSubmitterPhone:4:4);
          Endsl;
       Endif;
       // Convert report date to CYMD
       Monitor;
          intPostCymd = %Dec(%Char(%Date(decReportDate:*Usa):*Cymd0):7:0);
       On-Error;
          intPostCymd = 0;
       EndMon;
       // Pull A/R numbers from WISRPREM workfile
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pre00;
       // Loop through notification tokens. Process if the
       //  subject or body has a match.
       For idx = 1 to %Elem(eMTkn);
          If eMTkn(idx) = *blanks;
             Iter;
          Endif;
          strToken = %Trim(eMTkn(idx));
          ix1 = %Scan(strToken:inSubject);
          ix2 = %Scan(strToken:inBody);
          Select;
          When ix1 = 0 And ix2 = 0;
          When eMTkn(idx) = '#agency#';
             inSubject = %ScanRpl('#agency#':%Trim(strAgtName):inSubject);
             inBody = %ScanRpl('#agency#':%Trim(strAgtName):inBody);
          When eMTkn(idx) = '#company name#';
             Chain (ELco#:ELfnd) pfn00;
             inSubject = %ScanRpl('#company name#':%Trim(FNname):inSubject);
             inBody = %ScanRpl('#company name#':%Trim(FNname):inBody);
          When eMTkn(idx) = '#current billed#' And NOctgy = 'N001';
             Select;
             When %Scan('#current billed#':inSubject) <> 0;
             When %Scan('#current billed#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
            // Pull current billed from WISRPREM workfile.
             If %Found(WISRPREM);
                decPostAmt = REprem - REpprm;
             Else;
                decPostAmt = 0;
             EndIf;
             inSubject = %ScanRpl('#current billed#':
              %EditC(decPostAmt:'J':'$'):inSubject);
             inBody = %ScanRpl('#current billed#':
              %EditC(decPostAmt:'J':'$'):inBody);
          When eMTkn(idx) = '#current date#';
             inSubject = %ScanRpl('#current date#':
              %Char(dateToday:*Usa/):inSubject);
             inBody = %ScanRpl('#current date#':
              %Char(dateToday:*Usa/):inBody);
          When eMTkn(idx) = '#division number#';
             inSubject = %ScanRpl('#division number#':
              %Editc(EMdiv:'X'):inSubject);
             inBody = %ScanRpl('#division number#':
              %Editc(EMdiv:'X'):inBody);
          When eMTkn(idx) = '#due date#' And NOctgy = 'N001';
             Select;
             When %Scan('#due date#':inSubject) <> 0;
             When %Scan('#due date#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
             Exec Sql
              Select BSfreq
              Into :chrFrequency
              From WTBSP
              Where BSco# = :ELco#
               And BSkey = :ELbscd;
             If FNmidy <> 0 and decReportDate <> 0
              And chrFrequency <> 'W';
                workDueDate = %Date(decReportDate:*Usa) + %Days(FNmidy);
                inSubject = %ScanRpl('#due date#':
                 %Char(workDueDate:*Usa/):inSubject);
                inBody = %ScanRpl('#due date#':
                 %Char(workDueDate:*Usa/):inBody);
             Endif;
          When eMTkn(idx) = '#insured fein#';
             inSubject = %ScanRpl('#insured fein#':
              %Editc(EMfein:'X'):inSubject);
             inBody = %ScanRpl('#insured fein#':
              %Editc(EMfein:'X'):inBody);
          When eMTkn(idx) = '#insured name#';
             inSubject = %ScanRpl('#insured name#':
              %Trim(EMname + Emnam2):inSubject);
             inBody = %ScanRpl('#insured name#':
              %Trim(EMname + Emnam2):inBody);
          When eMTkn(idx) = '#ip#';
             inSubject = %ScanRpl('#ip#':%trim(N1portIP):inSubject);
             inBody = %ScanRpl('#ip#':%trim(N1portIP):inBody);
          When eMTkn(idx) = '#logo#';
             inSubject = %ScanRpl('#logo#':'':inSubject);
             inBody = %ScanRpl('#logo#':strLogoReplace:inBody);
          When eMTkn(idx) = '#policy effective date#';
             inSubject = %ScanRpl('#policy effective date#':
              %Char(%Date(ELpfdt:*Cymd):*Usa/):inSubject);
             inBody = %ScanRpl('#policy effective date#':
              %Char(%Date(ELpfdt:*Cymd):*Usa/):inBody);
          When eMTkn(idx) = '#policy expiration date#';
             inSubject = %ScanRpl('#policy expiration date#':
              %Char(%Date(ELptdt:*Cymd):*Usa/):inSubject);
             inBody = %ScanRpl('#policy expiration date#':
              %Char(%Date(ELptdt:*Cymd):*Usa/):inBody);
          When eMTkn(idx) = '#policy number#';
             Select;
             When %Scan('#policy number#':inSubject) <> 0;
             When %Scan('#policy number#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
             Chain (Elco#:ELfnd:ELemp#:ELdiv:ELfyr) prp02;
             If %Found And RPcap# <> *Blanks;
                strPolicy# = RPcap#;
             Else;
                strPolicy# = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') +
                 %EditC(ELfyr:'X');
             Endif;
             inSubject = %ScanRpl('#policy number#':
              %Trim(strPolicy#):inSubject);
             inBody = %ScanRpl('#policy number#':
              %Trim(strPolicy#):inBody);
          When eMTkn(idx) = '#posting date#' And NOctgy = 'N001';
             inSubject = %ScanRpl('#posting date#':
              %Char(%Date():*Usa/):inSubject);
             inBody = %ScanRpl('#posting date#':
              %Char(%Date():*Usa/):inBody);
          When eMTkn(idx) = '#previous balance#';
             Select;
             When %Scan('#previous balance#':inSubject) <> 0;
             When %Scan('#previous balance#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
            // Pull prev bal from WISRPREM workfile.
             If %Found(WISRPREM);
                decPostAmt = REprev;
             Else;
                decPostAmt = 0;
             EndIf;
             inSubject = %ScanRpl('#previous balance#':
              %EditC(decPostAmt:'J':'$'):inSubject);
             inBody = %ScanRpl('#previous balance#':
              %EditC(decPostAmt:'J':'$'):inBody);
          When eMTkn(idx) = '#producer#';
             inSubject = %ScanRpl('#producer#':%Trim(strProdName):inSubject);
             inBody = %ScanRpl('#producer#':%Trim(strProdName):inBody);
         // Currently, report date is passed in the request JSON
          When eMTkn(idx) = '#report period#' And NOctgy = 'N001';
             Select;
             When %Scan('#report period#':inSubject) <> 0;
             When %Scan('#report period#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
             Exec Sql
              Select BSfreq
              Into :chrFrequency
              From WTBSP
              Where BSco# = :ELco#
               And BSkey = :ELbscd;
             workPolFrDt = %Date(ELpfdt:*Cymd);
             workEndDate = %Date(decReportDate:*Usa);
             Select;
             When sqlStt <> '00000';
                strReportPeriod = 'Report period could not be determined.';
             When chrFrequency = 'W';
                workBegDate = workEndDate - %Days(6);
                If workBegDate < workPolFrDt;
                    workBegDate = workPolFrDt;
                EndIf;
             Other;
                intDayOfMth = %Subdt(workEndDate:*Days);
                intDayOfMth -= 1;
                workBegDate = workEndDate - %Days(intDayOfMth);
                If workBegDate < workPolFrDt;
                    workBegDate = workPolFrDt;
                EndIf;
             EndSl;
             If sqlStt = '00000';
                strReportPeriod = %Char(workBegDate) +
                 ' - ' +
                 %Char(workEndDate);
             Endif;
             inSubject = %ScanRpl('#report period#':
              strReportPeriod:inSubject);
             inBody = %ScanRpl('#report period#':
              strReportPeriod:inBody);
          When eMTkn(idx) = '#submitter email#';
             inSubject =
              %ScanRpl('#submitter email#':strSubmitterEmail:inSubject);
             inBody =
              %ScanRpl('#submitter email#':strSubmitterEmail:inBody);
          When eMTkn(idx) = '#submitter name#';
             inSubject =
              %ScanRpl('#submitter name#':strSubmitterName:inSubject);
             inBody =
              %ScanRpl('#submitter name#':strSubmitterName:inBody);
          When eMTkn(idx) = '#submitter phone#';
             inSubject =
              %ScanRpl('#submitter phone#':strSubmitterPhone:inSubject);
             inBody =
              %ScanRpl('#submitter phone#':strSubmitterPhone:inBody);
          When eMTkn(idx) = '#total period payment#' And NOctgy = 'N001';
             Select;
             When %Scan('#total period payment#':inSubject) <> 0;
             When %Scan('#total period payment#':inBody) <> 0;
             Other;
                Iter;
             EndSl;
            // Pull total payment from WISRPREM workfile.
             If %Found(WISRPREM);
                decPostAmt = REprem - REpprm + REtax - REptax + REprev;
             Else;
                decPostAmt = 0;
             EndIf;
             inSubject = %ScanRpl('#total period payment#':
              %EditC(decPostAmt:'J':'$'):inSubject);
             inBody = %ScanRpl('#total period payment#':
              %EditC(decPostAmt:'J':'$'):inBody);
          When eMTkn(idx) = '#userID#';
             inSubject = %ScanRpl('#userID#':%Trim(inUser):inSubject);
             inBody = %ScanRpl('#userID#':%Trim(inUser):inBody);
          Endsl;
       EndFor;
       End-Proc;
       //===================================================================
       // Send email for indicated employer/division.
       //===================================================================
       Dcl-Proc SendEmail;
       Dcl-Pi *N;
        inCo# Like(EMco#);
        inFnd Like(EMfnd);
        inEmp# Like(EMemp#);
        inDiv Like(EMdiv);
        inFyr Like(ELfyr);
        inCategory Like(NOctgy) Const;
        inType Like(NOtype) Const;
        inAttachment Char(250) Const Options(*NoPass);
       End-Pi;
       //*** Beg Add ***  01/20/23 *** WC2875 *****************************
       // Data Structure for adding Other Info subfields.
       Dcl-DS dsOtherInfo qualified;
          dsRptDate Char(8);
       End-DS;
       //*** End Add ***  01/20/23 *** WC2875 *****************************
       Dcl-S strAttachment Char(250);
       Dcl-S strCmd Char(6000);
       Dcl-S strCategory Like(NOctgy);
       Dcl-S strType Like(NOtype);
       Dcl-S strDartsKey Char(40);
       Dcl-S strDartsType Char(2);
       //***  Begin Chg  ***  02/01/22  *************
       //Dcl-S strEmailFrom Like(ECemal);
       Dcl-S strEmailFrom varchar(80);
       //***  End   Chg  ***  02/01/22  *************
       Dcl-S strEmailSubj Like(Nosubj);
       Dcl-S strEmailBody Like(Nobody);
       //*** Beg Add ***  01/20/23 *** WC2875 *****************************
       Dcl-S strSubject Like(Nosubj);
       Dcl-S strBody Like(Nobody);
       Dcl-S strRunInfo Char(14) inz;
       Dcl-S intRrn Zoned(9:0) inz;
       Dcl-S strRrnTyp Char(100) inz;
       Dcl-S strOtherInfo Char(16000) inz;
       //*** End Add ***  01/20/23 *** WC2875 *****************************
       Chain (inCo#:inFnd:inEmp#:inDiv) pem00;
       Chain (inCo#:inFnd:inEmp#:inDiv:inFyr) pel00;
       //*** Beg Add ***  01/20/23 *** WC2875 *****************************
       Clear intRrn;
       If %found(WDELL);
         intRrn  = dbrrn;
       Endif;
       //*** End Add ***  01/20/23 *** WC2875 *****************************

       // retrieve client suffix
       GetSuffix(outSuffix:outFolderPfx);

       // iDarts email server will get user's email address
       strEmailFrom = '*NONE';
       strCategory = inCategory;
       strType = inType;
       Chain (inCo#:inCategory:inType) pno00;
       //*** begin chg ***  12/14/22  *****************************
       //*****If Not(%Found);
       If Not(%Found) or (%Found and NOStat <> 'A');
       //*** end chg *****  12/14/22  *****************************
          Return;
       Endif;
       GetEmailToAddress(inCo#:inFnd:inEmp#:inDiv:
        strCategory:strType:NOflag1);
       strLogoReplace =
        GetLogoReplace(inCo#:inFnd:inEmp#:inDiv:strCategory:strType);
       If strEmailTo = *Blanks;
          Return;
       EndIf;
       strEmailSubj = NOsubj;
       strEmailBody = NObody;
       //*** Beg Add ***  01/20/23  **** WC2875 *****************
       strRunInfo = NOctgy + NOtype;
       strRrnTyp = 'Policy';
       dsOtherInfo.dsRptDate = %editc(decReportDate : 'X');
       strOtherInfo = dsOtherInfo;
       //*** End Add ***  01/20/23  **** WC2875 *****************
       //*** Beg Chg ***  01/20/23  **** WC2875 *****************
       //***ReplaceEmailTokens(strEmailSubj:strEmailBody);
       strSubject =
       ReplaceTokensEmail(strEmailSubj:strRunInfo:intRrn:strRrnTyp:
        strOtherInfo);
       strBody =
       ReplaceTokensEmail(strEmailBody:strRunInfo:intRrn:strRrnTyp:
        strOtherInfo);
       //*** End Chg ***  01/20/23  **** WC2875 *****************
       strEmailFrom = CvtRpgToHtml(strEmailFrom);
       //*** Beg Chg ***  01/20/23  **** WC2875 *****************
       //***strEmailSubj = CvtRpgToHtml(strEmailSubj);
       strSubject= CvtRpgToHtml(strSubject);
       //***strEmailBody = CvtRpgToHtml(strEmailBody);
       strBody = CvtRpgToHtml(strBody);
       //*** End Chg ***  01/20/23  **** WC2875 *****************
       If %Parms >= 8;
          strAttachment = inAttachment;
       Endif;
       //***  Begin Chg  ***  02/01/22  ************* +
       //strCmd = 'SNDEMAIL FROM(' + %Trim(strEmailFrom) + ') ' +
       strEmailFrom = '''' + %trim(strEmailFrom) + '''';
       strCmd = 'SNDEMAIL FROM(' + strEmailFrom + ') ' +
       //***  End   Chg  ***  02/01/22  ************* +
        'TO(' + %Trim(strEmailTo) + ') ' +
       //*** Beg Chg ***  01/20/23  **** WC2875 *****************
       //'SUBJECT(''' + %Trim(strEmailSubj) + ''') ' +
        'SUBJECT(''' + %Trim(strSubject) + ''') ' +
       //'BODY(''' + %Trim(strEmailBody) + ''') ' +
        'BODY(''' + %Trim(strBody) + ''') ' +
       //*** End Chg ***  01/20/23  **** WC2875 *****************
        'SUFFIX(' + outSuffix + ') ' +
        'RETRY(' + %char(N1retries) +') ';
       If strAttachment <> *Blanks;
          strCmd = %Trim(strCmd) +
           ' ATTACH((' + '*NONE ''' +
           %Trim(strAttachment) + '''))';
       Endif;
       If NOdart <> *Blanks;
          strDartsKey =
           %Editc(inCo#:'X') + %Editc(inFnd:'X') +
           %Editc(inEmp#:'X') + %Editc(inDiv:'X');
          strDartsType = 'WE';
          If isEnvQuote;
             strDartsType = 'WQ';
             %subst(strDartsKey:40:1) = 'Q';
          Endif;
          strCmd = %Trim(strCmd) +
           ' DARTS(''' + strDartsKey + ''' ' +
           strDartsType + ' ' + NOdart +  ' ' +
           strUser + ')';
       Endif;
       RunCmd(strCmd:%Len(%Trim(strCmd)));
       End-Proc;
** Email Tokens
#agency#
#company name#
#current billed#
#current date#
#division number#
#due date#
#insured fein#
#insured name#
#ip#
#logo#
#policy effective date#
#policy expiration date#
#policy number#
#posting date#
#previous balance#
#producer#
#report period#
#submitter email#
#submitter name#
#submitter phone#
#total period payment#
#userID#
