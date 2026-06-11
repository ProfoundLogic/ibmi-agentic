     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTPOLSSCD - WCPOLS Scheduling Configuration
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Show the WCPOLS job schedules.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 12/21/2020 by C:062 for Project: WC2761
     h**********************************************************************
     h*  Revisions:
     h*
     h**********************************************************************
     fwtpolsscd cf   e             workstn extfile('WTPOLSSCD')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPolSchd:rrnPolSchd)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwdf2l     if   e           k disk    rename(pf201:pf200)
     fwtwpp     uf a e             disk    rename(pwp01:pwp0p)
     fwtwpl1    if   e           k disk    Prefix(W1:2)
     fwtwpl2    if   e           k disk    rename(pwp01:pwp02) Prefix(W2:2)
     fwtwpl3    if   e           k disk    rename(pwp01:pwp03)
     f                                     InfDs(dbfds1)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fssylf     if   e           K disk
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d jobSchedInfo    ds            50    dtaara(WCPOLSSCHD)
     d jobtime                 1      8
     d jobDays                 9     43

     d jobSchedUser    ds            10    dtaara(WCPOLSUSER)
     d jobUser                 1     10

     d intCount        s              3  0
     d @@mode          s              1
     d @@TabIndex      s              5
     d inMessage       s            256
     d inMode          s              1
     d inTabIndex      s              5  0
     d inTitle         s             50
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d clientID        s              3    inz('000')
     d meetPay         s              1
     d ##desc          s            100
     d ##pdsc          s            100
     d ##start         s              3  0 inz(1)
     d ##strlen        s              3  0 inz(100)
     d ##scnres        s              3  0
     d ##patlen        s              3  0 inz(40)
     d ##trans         s              1    inz('0')
     d ##trim          s              1    inz('1')
     d ##wild          s              1    inz(' ')
     d ##mode          s              1
     d intRRN          s              9  0
     d isValid         s               n
     d inIfsName       s            256
     d outFound        s               n
     d count           s              5  0

     D                 ds
     D  days                   1     28
     D                                     DIM(7)
     D  scSun                  1      4
     D  scMon                  5      8
     D  scTue                  9     12
     D  scWed                 13     16
     D  scThu                 17     20
     D  scFri                 21     24
     D  scSat                 25     28

     d* Check IFS Object
     d ChkIfsObj       pr                  extpgm('CHKIFSOBJ')
     d  inIfsName                   256
     d  outFound                       n

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Get Suffix
     d GetSuffix       pr                  extpgm('SA030R')
     d  inSuffix                      3

     d ##CltId         s              3
     d inSuffix        s              3

     d/copy SPrCompany
     d/copy sPrDate
     d/copy sPrEnv
     d/copy sprGenPop
     d/copy sprModule
     d/copy sPrRunCmd
     d/copy sPrString
     d/copy wPrUsrPrms
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    @@Mode
     c                   parm                    @@TabIndex

      /free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;

      /end-free
       //===================================================================
       // AddJobSchedule - Add Job Schedule
       //===================================================================
     p AddJobSchedule  b
     d AddJobSchedule  pi
      /Free
       clear RcdSchdJob;
       isValid = *On;

       btnSchdJob = *Off;
       btnCanSchd = *Off;
       isDataCtr  = CheckDataCenter();

       Dou BtnCanSchd = *On or (BtnSchdJob = *On and isValid = *On);

          If isValid = *On;
             // Load Current Values
             In jobSchedInfo;
             Unlock jobSchedInfo;
             In jobSchedUser;
             Unlock jobSchedUser;

             If jobTime = ' ';
                scSchdTime = 0;
             Else;
               scSchdTime = %Dec(%Subst(jobTime:1:2) + %Subst(jobTime:4:2) +
                             %Subst(jobTime:7:2):6:0);
             Endif;

             If %Scan('*SUN':jobDays:1) <> 0;
                scSun = '*SUN';
             EndIf;

             If %Scan('*MON':jobDays:1) <> 0;
                scMon = '*MON';
             EndIf;

             If %Scan('*TUE':jobDays:1) <> 0;
                scTue = '*TUE';
             EndIf;

             If %Scan('*WED':jobDays:1) <> 0;
                scWed = '*WED';
             EndIf;

             If %Scan('*THU':jobDays:1) <> 0;
                scThu = '*THU';
             EndIf;

             If %Scan('*FRI':jobDays:1) <> 0;
                scFri = '*FRI';
             EndIf;

             If %Scan('*SAT':jobDays:1) <> 0;
                scSat = '*SAT';
             EndIf;

             scSchdUser = jobUser;
          EndIf;

          Exfmt RcdSchdJob;

          erSun      = *Off;
          erMon      = *Off;
          erTue      = *Off;
          erWed      = *Off;
          erThu      = *Off;
          erFri      = *Off;
          erSat      = *Off;
          erSchdTime = *Off;
          erSchdUser = *Off;

          Select;
             When BtnSchdJob = *On;
                If ValidateJobSchedule() = *On;
                   CreateJobSchedule();
                Endif;
             When BtnCanSchd = *On;
                BtnCanSchd = *Off;
                Leave;
          Endsl;

       Enddo;

      /End-Free
     p AddJobSchedule  e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnPolSchd = 0;
       clrPolSchd = *On;
       dspPolSchd = *Off;
       Write RcdPolGrid;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       //  CreateJobSchedule - Create Job Schedule Entry
       //===================================================================
     p CreateJobSchedule...
     p                 b
     d CreateJobSchedule...
     d                 pi

     d addJobScde      s            256
     d chgJobScde      s            256
     d rmvJobScde      s            256
     d chrTime6        s              6    Inz(' ')
     d chrTime         s              8    Inz(' ')
     d chrDays         s             35    Inz(' ')
     d chrJobDesc      s             20    Inz(' ')
     d i               s              1  0

      /Free

       // List selected days
       For i=1 to 7;
          If days(i) <> ' ';
             If chrDays = ' ';
                chrdays = days(i);
             Else;
                chrDays = %Trim(chrDays) + ' ' + days(i);
             EndIf;
          EndIf;
       EndFor;

       // Prepare the time
       chrTime6 = %EditC(scSchdTime:'X');
       chrTime  = %Subst(chrTime6:1:2) + ':' + %Subst(chrTime6:3:2) + ':' +
                  %Subst(chrTime6:5:2);

       // Generate Job Description
       getSuffix(inSuffix);
       If inSuffix = ' ';
          chrJobDesc = 'PDSOBJ/CICJOBD';
       Else;
          chrJobDesc = 'PDSOBJ' + inSuffix + '/CICJOBD';
       EndIf;

       // Prepare job schedule commands
       addJobScde = 'ADDJOBSCDE JOB(WCPOLS) CMD(CALL PGM(WTWCPOLS)) ' +
                    'FRQ(*WEEKLY) SCDDATE(*NONE) SCDDAY(' +
                    %Trim(chrDays) + ') SCDTIME(''' + chrTime + ''') ' +
                    'JOBD(' + %Trim(chrJobDesc) + ') USER(' +
                    %Trim(scSchdUser) + ') TEXT(''WCPOLS Job Schedule'')';

       chgJobScde = 'CHGJOBSCDE JOB(WCPOLS) CMD(CALL PGM(WTWCPOLS)) ' +
                    'FRQ(*WEEKLY) SCDDATE(*NONE) SCDDAY(' +
                    %Trim(chrDays) + ') SCDTIME(''' + chrTime + ''') ' +
                    'JOBD(' + %Trim(chrJobDesc) + ') USER(' +
                    %Trim(scSchdUser) + ') TEXT(''WCPOLS Job Schedule'')';

       rmvJobScde = 'RMVJOBSCDE JOB(WCPOLS)';

       If jobUser <> scSchdUser and jobUser <> ' ';
          Monitor;
             runCmd(%Trim(rmvJobScde):%Len(%Trim(rmvJobScde)));
          On-Error;
          EndMon;
       EndIf;

       // Run command
       // Try to change job schedule.  If fails then add job schedule
       Monitor;
          runCmd(%Trim(chgJobScde):%Len(%Trim(chgJobScde)));
       On-Error;
          Monitor;
             runCmd(%Trim(addJobScde):%Len(%Trim(addJobScde)));
          On-Error;
             mgSchdUser = 'You don''t have authority to use this ' +
                          'user profile.';
             erSchdUser = *On;
             isValid = *Off;
          EndMon;
       EndMon;

       If isValid = *On;
          In *Lock jobSchedInfo;
          jobTime = chrTime;
          jobDays = chrDays;
          Out jobSchedInfo;

          In *Lock jobSchedUser;
          jobUser = scSchdUser;
          Out jobSchedUser;
       EndIf;

      /End-Free

     p CreateJobSchedule...
     p                 e
       //===================================================================
       // EditAddPaySched - Edit/Add WCPols Schedule Information
       //===================================================================
     p EditAddPaySched...
     p                 b
     d EditAddPaySched...
     d                 pi
     d inRRN                          9  0 options(*nopass)

      /Free
       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %Parms() >  0 And inRRN <> 0;
          ##mode  = 'C';
          intRRN  = inRRN;
       Else;
          ##mode  = 'A';
          intRRN  = *zeros;
       Endif;

       ProcessSchedule();

       intRRN  = *zeros;
       return;
      /End-Free

     p EditAddPaySched...
     p                 e
       //===================================================================
       // Load Payment Value 1 code grid
       //===================================================================
     p LoadPolSched    b
     d LoadPolSched    pi

     d strTbid         s                   like (GTtbid)

      /free

       // clear the subfile.
       clear sflPolSchd;
       clrPolSchd = *Off;
       dspPolSchd = *On;
       clear dbRrn1;

       If inmode = 'D';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       Setll intCo# pwp03;
       Reade intCo# pwp03;
       dow not %eof(wtwpl3);

          // Write Policy Schedule Records
          scPScdName = WPname;

          If WPst = ' ';
             scPScdBur = 'NCCI';
          Else;
             scPScdBur = WPst;
          EndIf;

          Select;
          When WPer = ' ';
             scPScdER   = 'All';
          When WPer = '1';
             scPScdER   = 'Interstate';
          When WPer = '3';
             scPScdER   = 'Intrastate';
          EndSl;

          If WPpeep = 'Y';
             scPScdPEEP = 'Yes';
          Else;
             scPScdPEEP = 'No';
          EndIf;

          If WPcar# <> 0;
             scPScdSel = 'C:' + %Editc(WPcar#:'4');
          Else;
             scPScdSel = 'G:' + %Editc(WPfnd:'4');
          EndIf;

          scPScdFile = WPfile;
          scPScdDir  = WPdir;

          // Status
          If WPstat = 'A';
             scStsHdr = 'Active';
          Else;
             scStsHdr = 'Inactive';
          Endif;

          scPSCDRRN = dbRrn1;

          rrnPolSchd = rrnPolSchd + 1;
          If rrnPolSchd = 1;
             scPolRowSl = 'A';
          Else;
             clear scPolRowSl;
          Endif;

          Write sflPolSchd;

          Reade intCo# pwp03;
       Enddo;

       rrnPolSchd = 1;

      /end-free
     p LoadPolSched    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Policy Schedule
          LoadPolSched();

          // Process Payment Value 1 code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add WCPols Schedule Info
          When scJobSAdd = *On;
             EditAddPaySched();

          // Add Job Schedule
          When scJobSched = *On;
             AddJobSchedule();

          Other;
             ProcOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdPolSchd;
          Exfmt rcdPolGrid;
          Read rcdComPnl;
          Read rcdPolSchd;
       EndSl;

       Endsr;

      /end-free
     p Main            e


       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     p MoveDbSc        b
     d MoveDbSc        pi

      /free

       If ##Mode = 'A';
          scSchdName = ' ';
          scSchdSt   = ' ';
          scSchdER   = ' ';
          scSchdPEEP = ' ';
          scSelType  = 'C';
          scSchdGC   = 0;
          scSchdFile = ' ';
          scSchdDir  = ' ';
          scSchdStat = 'A';
       Else;
          scSchdName = WPname;
          scSchdSt   = WPst;
          scSchdER   = WPer;
          scSchdPEEP = WPpeep;

          If WPcar# = 0;
             scSchdGC   = WPfnd;
             scSelType  = 'G';
          Else;
             scSchdGC   = WPcar#;
             scSelType  = 'C';
          EndIf;

          scSchdFile = WPfile;
          scSchdDir  = WPdir;
          scSchdStat = WPstat;
       EndIf;

      /end-free
     p MoveDbSc        e

       //===================================================================
       // MoveScDb - Move database record to screen
       //===================================================================
     p MoveScDb        b
     d MoveScDb        pi

      /free

       Chain intRrn pwp0p;

       WPname = scSchdName;
       WPst   = scSchdSt;
       WPer   = scSchdER;
       WPpeep = scSchdPEEP;

       If scSelType = 'C';
          WPcar# = scSchdGC;
          WPfnd  = 0;
       Else;
          WPcar# = 0;
          WPfnd  = scSchdGC;
       EndIf;

       WPfile = scSchdFile;
       WPdir  = scSchdDir;
       WPstat = scSchdStat;
       WPpgm  = Q1pgm;
       WPuser = Q1user;
       WPdt   = Dateto7(SystemDate());
       WPtime = Timeto6(SystemTime());

       If ##mode = 'A';
          WPco#  = userCo#(q1user);
          WPiPgm = Q1pgm;
          WPiUsr = Q1user;
          WPiDt  = Dateto7(SystemDate());
          WPiTim = Timeto6(SystemTime());
          Write pwp0p;
       Else;
          Update pwp0p;
       Endif;

      /end-free
     p MoveScDb        e

       //===================================================================
       // ProcessSchedule - Process WCPols Schedule Information
       //===================================================================
     p ProcessSchedule...
     p                 b
     d ProcessSchedule...
     d                 pi

      /free
       clear RcdEdtSchd;

       // Loop if add next is used.
       DoU btnAddNxt = *Off;

          btnSaveRcd = *Off;
          btnCanRcd  = *Off;
          btnAddNxt  = *Off;
          indChgMode = *Off;
          indDspMode = *Off;
          indAddMode = *Off;

          // processing for Edit mode
          If ##mode = 'C' and intRrn <> 0;
             Chain(n) intRrn pwp0p;
             If %found;
                indChgMode = *On;
                MoveDbSc();
             Endif;
          Endif;

          // processing for Add mode
          If ##mode = 'A';
             MoveDbSc();
             scSchdStat = 'A';
             indAddMode = *On;
          Endif;

          Dou BtnCanRcd = *On or (BtnSaveRcd = *On and isValid = *On) Or
              (BtnAddNxt = *On and isValid = *On);

             Exfmt RcdEdtSchd;

             isValid    = *Off;
             erSchdName = *Off;
             erSchdSt   = *Off;
             erSchdER   = *Off;
             erSelType  = *Off;
             erSchdGC   = *Off;
             erSchdStat = *Off;
             erSchdFile = *Off;
             erSchdDir  = *Off;

             Select;
                When BtnSaveRcd = *On Or BtnAddNxt = *On;
                   If ValidateSchedule() = *On;
                      MoveSCDB();
                   Endif;
                When BtnCanRcd = *On;
                   BtnCanRcd = *Off;
                   Leave;
             Endsl;

          Enddo;
       Enddo;

      /end-free
     p ProcessSchedule...
     p                 e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If Q1prms >= 1;
          inmode = @@Mode;
       Endif;

       Select;
       When inMode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When inMode = 'A';
          dspVisb = *On;
       When inMode = 'C';
          dspVisb = *On;
       Endsl;

       // Tab Index Mode.
       If Q1prms >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##rrn = 0;
       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /free

       Readc sflPolSchd;

       Dow not %eof;
          Select;
          When scPScdEdit = *On;
             scPScdEdit = *Off;
             ##rrn = scPSCDRRN;
             EditAddPaySched(##rrn);
          When scPSchdSts = *On;
             scPSchdSts = *Off;
             Chain scPScdRRN pwp0p;
             If %Found(wtwpp);
                If WPstat = 'I';
                   Unlock wtwpp;
                   If ValidStsChg(WPco#:WPst:WPfnd:WPcar#:WPer);
                      Chain scPScdRRN pwp0p;
                      If %Found(wtwpp);
                         WPstat = 'A';
                      Endif;
                   Else;
                      inMessage = 'There is already an active entry ' +
                                  'for the same group/carrier.';
                      inTitle   = 'Unable to Change Status';
                      PopOneBtn(inMessage:inTitle);
                      Chain scPScdRRN pwp0p;
                   Endif;
                Else;
                   WPstat = 'I';
                EndIf;
                Update pwp0p;
             EndIf;
          EndSl;

          Readc sflPolSchd;
       Enddo;

      /end-free
     p ProcOther       E

       //===================================================================
       // ValidateJobSchedule - Validate Job Schedule Information
       //===================================================================
     p ValidateJobSchedule...
     p                 b
     d ValidatejobSchedule...
     d                 pi              n

     d validJobSched   s               n   Inz(*On)
     d time            s               t

      /free

       validJobSched = *On;

       // At least one day must be checked
       If scSun = ' ' And scMon = ' ' And scTue = ' ' And scWed = ' ' And
        scThu = ' ' And scFri = ' ' And scSat = ' ';
          mgSun = 'At least one day must be selected.';
          erSun = *On;
          validJobSched = *Off;
       EndIf;

       // Validate the time
       Monitor;
          time = %Time(scSchdTime);
       On-Error;
          mgSchdTime = 'Invalid time entry.';
          erSchdTime = *On;
          validJobSched = *Off;
       EndMon;

       // User must be entered
       If scSchdUser = ' ';
          mgSchdUser = 'User ID must be entered.';
          erSchdUser = *On;
          validJobSched = *Off;
       EndIf;

       isValid = validJobSched;
       Return validJobSched;

      /end-free
     p ValidateJobSchedule...
     p                 e

       //===================================================================
       // ValidateSched - Validate Add/Edit screen
       //===================================================================
     p ValidateSchedule...
     p                 b
     d ValidateSchedule...
     d                 pi              n

     d ##stat          s              1
     d validRecord     s               n   Inz(*On)
     d company#        s                   Like(WPco#)
     d schdErAll       s              1    Inz(' ')

      /free

       validRecord = *On;
       If ##mode = 'A';

          // Scheduled Name must be entered
          If scSchdName = ' ';
             mgSchdName = 'Name must be entered.';
             erSchdName = *On;
             validRecord = *Off;
          Else;
             // Can't have more than one entry with this schedule name
             Exec Sql Select count(*) Into :count From WTWPP
                      Where WPname = :scSchdName;
             If count > 0;
                mgSchdName = 'Schedule name must be unique.' ;
                erSchdName = *On;
                validRecord = *Off;
             EndIf;
          EndIf;

          // Group or Carrier must be selected
          If scSelType <> 'G' And scSelType <> 'C';
             mgSelType = 'Please select group or carrier.';
             erSelType = *On;
             validRecord = *Off;
          Else;

             // Group must be entered if group selected
             If scSelType = 'G' And scSchdGC = 0;
                mgSchdGC = 'Group # must be entered.';
                erSchdGC = *On;
                validRecord = *Off;
             EndIf;

             // Carrier# must be entered if carrier# selected
             If scSelType = 'C' And scSchdGC = 0;
                mgSchdGC = 'Carrier# must be entered.';
                erSchdGC = *On;
                validRecord = *Off;
             EndIf;
          EndIf;

          // Prevent duplicate or overlapping schedule entries
          If erSchdGC = *Off;
             company# = userCo#(q1user);
             Select;
             // Make sure group isn't already being used in other schedules
             When scSelType = 'G';

                If scSchdER = ' ';
                   // Lets see if there is  record with this group/state/er
                   Chain (company#:scSchdSt:scSchdGC) pwp02;
                   If %Found(wtwpl2) And W2stat <> 'I';
                      mgSchdGC = 'Group is already being used with same ' +
                                 'state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Let's see if any schedule records using carrier #
                   // match the group entered
                   Chain (company#:scSchdGC) pf200;
                   If %Found(wdf2l);
                      Chain (company#:scSchdSt:F2car#) pwp01;
                      If %Found(wtwpl1) And W1stat <> 'I';
                         mgSchdGC = 'Group is already being used with ' +
                                    'same state/carrier#/ER.';
                         erSchdGC = *On;
                         validRecord = *Off;
                      EndIf;
                   EndIf;
                Else;
                   // Lets see if there is record with this group/state/er
                   Chain (company#:scSchdSt:scSchdGC:scSchdER) pwp02;
                   If %Found(wtwpl2) And W2stat <> 'I';
                      mgSchdGC = 'Group is already being used with same ' +
                                 'state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Need to check again but for a blank ER
                   Chain (company#:scSchdSt:scSchdGC:schdErAll) pwp02;
                   If %Found(wtwpl2) And W2stat <> 'I';
                      mgSchdGC = 'Group is already being used with same ' +
                                 'state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Let's see if any schedule records using carrir #
                   // match the group entered
                   Chain (company#:scSchdGC) pf200;
                   If %Found(wdf2l);
                      Chain (company#:scSchdSt:F2car#:scSchdER) pwp01;
                      If %Found(wtwpl1) And W1stat <> 'I';
                         mgSchdGC = 'Group is already being used with ' +
                                    'same state/carrier#/ER.';
                         erSchdGC = *On;
                         validRecord = *Off;
                      EndIf;
                   EndIf;

                   // Need to check again but for a blank ER
                   Chain (company#:scSchdGC) pf200;
                   If %Found(wdf2l);
                      Chain (company#:scSchdSt:F2car#:schdErAll) pwp01;
                      If %Found(wtwpl1) And W1stat <> 'I';
                         mgSchdGC = 'Group is already being used with ' +
                                    'same state/carrier#/ER.';
                         erSchdGC = *On;
                         validRecord = *Off;
                      EndIf;
                   EndIf;
                EndIf;

             // Make sure group isn't already being used in other schedules
             // by checking the groups associated with carrier
             When scSelType = 'C';

                If scSchdER = ' ';
                   // Make sure the carrier# isn't being used by st/ER
                   Chain (company#:scSchdSt:scSchdGC) pwp01;
                   If %Found(wtwpl1) And W1stat <> 'I';
                      mgSchdGC = 'Carrier# is already being used with ' +
                                 'same state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Make sure schedules don't exist that use a group
                   // associated with this carrier #/state/ER
                   Chain (company#) pf200;
                   If %Found(wdf2l);
                      DoU %Eof(wdf2l);
                         If F2car# = scSchdGC;
                            Chain (company#:scSchdSt:F2fnd) pwp02;
                            If %Found(wtwpl2) And W2stat <> 'I';
                               mgSchdGC = 'Group is already being used ' +
                                          'with same state/ER.';
                               erSchdGC = *On;
                               validRecord = *Off;
                               Leave;
                            EndIf;
                         EndIf;
                         ReadE (company#) pf200;
                      EndDo;
                   EndIf;
                Else;
                   // Make sure the carrier# isn't being used by st/ER
                   Chain (company#:scSchdSt:scSchdGC:scSchdER) pwp01;
                   If %Found(wtwpl1) And W1stat <> 'I';
                      mgSchdGC = 'Carrier# is already being used with ' +
                                 'same state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Need to check again but for a blank ER
                   Chain (company#:scSchdSt:scSchdGC:schdErAll) pwp01;
                   If %Found(wtwpl1) And W1stat <> 'I';
                      mgSchdGC = 'Carrier# is already being used with ' +
                                 'same state/ER.';
                      erSchdGC = *On;
                      validRecord = *Off;
                   EndIf;

                   // Make sure schedules don't exist that use a group
                   // associated with this carrier #/state/ER
                   Chain (company#) pf200;
                   If %Found(wdf2l);
                      DoU %Eof(wdf2l);
                         If F2car# = scSchdGC;
                            Chain (company#:scSchdSt:F2fnd:scSchdER) pwp02;
                            If %Found(wtwpl2) And W2stat <> 'I';
                               mgSchdGC = 'Group is already being used ' +
                                          'with same state/ER.';
                               erSchdGC = *On;
                               validRecord = *Off;
                               Leave;
                            EndIf;
                            // Need to check again but for a blank ER
                            Chain (company#:scSchdSt:F2fnd:schdErAll) pwp02;
                            If %Found(wtwpl2) And W2stat <> 'I';
                               mgSchdGC = 'Group is already being used ' +
                                          'with same state/ER.';
                               erSchdGC = *On;
                               validRecord = *Off;
                               Leave;
                            EndIf;
                         EndIf;
                         ReadE (company#) pf200;
                      EndDo;
                   EndIf;
                EndIf;
             EndSl;
          EndIf;
       Else;
          // If Status is being changed from inactive to active check to
          // make sure no entries already active with same group/carrier#
          If WPstat <> scSchdStat And WPstat = 'I';
             If Not ValidStsChg(WPco#:WPst:WPfnd:WPCar#:WPer);
                mgSchdStat = 'There is already an active entry ' +
                             'for the same group/carrier.';
                erSchdStat = *On;
                validRecord = *Off;
             Endif;
          Endif;
       Endif;


       // Directory must be entered and must exist
       If scSchdDir = ' ';
          mgSchdDir = 'Directory Must be entred.';
          erSchdDir = *On;
          validRecord = *Off;
       Else;
          inIfsName = scSchdDir;
          outFound  = *Off;
          ChkIfsObj(inIfsName:outFound);
          If outFound = *Off;
             mgSchdDir = 'Directory does not exist.';
             erSchdDir = *On;
             validRecord = *Off;
          EndIf;
       EndIf;

       // File must be entered and can't already be used.
       If scSchdFile = ' ';
          mgSchdFile = 'File Must be entered.';
          erSchdFile = *On;
          validRecord = *Off;
       Else;
          // Can't have more than one entry with this document name
          //Exec Sql Select count(*) Into :count From WTWPP
          //   Where WPfile = :scSchdFile;
          //If (##mode = 'A' And count > 0) Or (##mode = 'C' And count > 1);
          //   mgSchdFile = 'File name must be unique.' ;
          //   erSchdFile = *On;
          //   validRecord = *Off;
          //EndIf;
          // File Extension Must be entered
          If %Scan('.TXT':scSchdFile) = 0 And %Scan('.txt':scSchdFile) = 0;
             mgSchdFile = 'Must use file extension .txt.' ;
             erSchdFile = *On;
             validRecord = *Off;
          EndIf;
       EndIf;

       isValid = validRecord;
       Return validRecord;

      /end-free
     p ValidateSchedule...
     p                 e

       //===================================================================
       // ValidStsChg - Make sure changing inactive status to acitve doesn't
       // conflict with other schedules
       //===================================================================
     p ValidStsChg     b
     d ValidStsChg     pi              n
     d inCompany#                          Like(WPco#)
     d inState                             Like(WPst)
     d inGroup#                            Like(WPfnd)
     d inCarrier#                          Like(WPcar#)
     d inER                                Like(WPer)

     d ##stat          s              1
     d validStsChg     s               n   Inz(*On)
     d company#        s                   Like(WPco#)
     d schdErAll       s              1    Inz(' ')

      /free

       validStsChg = *On;

       // Prevent duplicate or overlapping schedule entries
       Select;
       // Make sure group isn't already being used in other schedules
       When inGroup# <> 0;

          If inER = ' ';
             // Lets see if there is  record with this group/state/er
             Chain (inCompany#:inState:inGroup#) pwp02;
             If %Found(wtwpl2);
                DoU %Eof(wtwpl2);
                   If W2stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inGroup#) pwp02;
                Enddo;
             EndIf;

             // Let's see if any schedule records using carrier # match
             // the group entered
             Chain (inCompany#:inGroup#) pf200;
             If %Found(wdf2l);
                Chain (inCompany#:inState:F2car#) pwp01;
                If %Found(wtwpl1);
                   DoU %Eof(wtwpl1);
                      If W1stat <> 'I';
                         validStsChg = *Off;
                         Leave;
                      EndIf;
                      ReadE (inCompany#:inState:F2car#) pwp01;
                   EndDo;
                EndIf;
             EndIf;
          Else;
             // Lets see if there is  record with this group/state/er
             Chain (inCompany#:inState:inGroup#:inER) pwp02;
             If %Found(wtwpl2);
                DoU %Eof(wtwpl2);
                   If W2stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inGroup#:inER) pwp02;
                Enddo;
             EndIf;

             // Need to try again for blank ER
             Chain (inCompany#:inState:inGroup#:schdErAll) pwp02;
             If %Found(wtwpl2);
                DoU %Eof(wtwpl2);
                   If W2stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inGroup#:schdErAll) pwp02;
                Enddo;
             EndIf;

             // Let's see if any schedule records using carrier # match
             // the group entered
             Chain (inCompany#:inGroup#) pf200;
             If %Found(wdf2l);
                Chain (inCompany#:inState:F2car#:inER) pwp01;
                If %Found(wtwpl1);
                   DoU %Eof(wtwpl1);
                      If W1stat <> 'I';
                         validStsChg = *Off;
                         Leave;
                      EndIf;
                      ReadE (inCompany#:inState:F2car#:inER) pwp01;
                   EndDo;
                EndIf;
                // Try again with blank ER
                Chain (inCompany#:inState:F2car#:schdErAll) pwp01;
                If %Found(wtwpl1);
                   DoU %Eof(wtwpl1);
                      If W1stat <> 'I';
                         validStsChg = *Off;
                         Leave;
                      EndIf;
                      ReadE (inCompany#:inState:F2car#:schdErAll) pwp01;
                   EndDo;
                EndIf;
             EndIf;
          EndIf;

       // Make sure group isn't already being used in other schedules
       // by checking the groups associated with carrier
       When inCarrier# <> 0;

          If inER = ' ';
             // Make sure the carrier# isn't being used by st/ER
             Chain (inCompany#:inState:inCarrier#) pwp01;
             If %Found(wtwpl1);
                DoU %Eof(wtwpl1);
                   If W1stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inCarrier#) pwp01;
                EndDo;
             EndIf;

             // Make sure schedules don't exist that use a group
             // associated with this carrier #/state/ER
             Chain (inCompany#) pf200;
             If %Found(wdf2l);
                DoU %Eof(wdf2l);
                   If f2Car# = inCarrier#;
                      Chain (inCompany#:inState:F2fnd) pwp02;
                      If %Found(wtwpl2);
                         DoU %Eof(wtwpl2);
                            If W2stat <> 'I';
                               validStsChg = *Off;
                               Leave;
                            EndIf;
                            ReadE (inCompany#:inState:F2fnd) pwp02;
                         Enddo;
                      EndIf;
                   EndIf;
                   ReadE (inCompany#) pf200;
                EndDo;
             EndIf;
          Else;
             // Make sure the carrier# isn't being used by st/ER
             Chain (inCompany#:inState:inCarrier#:inER) pwp01;
             If %Found(wtwpl1);
                DoU %Eof(wtwpl1);
                   If W1stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inCarrier#:inER) pwp01;
                EndDo;
             EndIf;

             // Need to try again for blank ER
             Chain (inCompany#:inState:inCarrier#:schdErAll) pwp01;
             If %Found(wtwpl1);
                DoU %Eof(wtwpl1);
                   If W1stat <> 'I';
                      validStsChg = *Off;
                      Leave;
                   EndIf;
                   ReadE (inCompany#:inState:inCarrier#:schdErAll) pwp01;
                EndDo;
             EndIf;

             // Make sure schedules don't exist that use a group
             // associated with this carrier #/state/ER
             Chain (inCompany#) pf200;
             If %Found(wdf2l);
                DoU %Eof(wdf2l) Or validStsChg = *Off;
                   If f2Car# = inCarrier#;
                      Chain (inCompany#:inState:F2fnd:inER) pwp02;
                      If %Found(wtwpl2);
                         DoU %Eof(wtwpl2);
                            If W2stat <> 'I';
                               validStsChg = *Off;
                               Leave;
                            EndIf;
                            ReadE (inCompany#:inState:F2fnd:inER) pwp02;
                         Enddo;
                      EndIf;
                      // Need to try again for blank ER
                      Chain (inCompany#:inState:F2fnd:schdErAll) pwp02;
                      If %Found(wtwpl2);
                         DoU %Eof(wtwpl2);
                            If W2stat <> 'I';
                               validStsChg = *Off;
                               Leave;
                            EndIf;
                            ReadE (inCompany#:inState:F2fnd:schdErAll) pwp02;
                         Enddo;
                      EndIf;
                   EndIf;
                   ReadE (inCompany#) pf200;
                EndDo;
             EndIf;
          EndIf;
       EndSl;

       Return validStsChg;

      /end-free
     p ValidStsChg     e
