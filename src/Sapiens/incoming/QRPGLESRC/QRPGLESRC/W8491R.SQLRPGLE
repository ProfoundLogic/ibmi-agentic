     hCopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      //********************************************************************************************
     h/TITLE W8491R - Territorial Differential Upload
     h option(*nodebugio:*srcstmt)
      //********************************************************************************************
      //  DESCRIPTION:  This program reads the flat file WXTZP, edits the records, and outputs an
      //                error report or a successful upload verification report and new payroll
      //                classification report. This program can process one header and one trailer
      //                record in each flat file only.
      //
      //  Called from:  W8491C - Override
      //  Calls      :  None
      //********************************************************************************************
      //  Created on 08/23/2019 by c:291 for Project 150WEA
      //********************************************************************************************
      // Revisions:
      //
      //********************************************************************************************
     fwxtzp     if   e             disk    usropn rename(ptz01:ptz0px)
     f                                     infds(dbWxtzp)
     fwmtzl1    uf a e           k disk    prefix(z)
     fsmorl     if   e           k disk
     fstabl     if   e           k disk
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     fwmfnl     if   e           k disk
     fqsysprt   o    f  140        printer oflind(*inoa)                        error report
     fqsysprt1  o    f  105        printer oflind(*inob)                        upload report
      //============================================================================================
      // Prototypes
      //============================================================================================
      *Entry
     dW8491R           pr
     d eFnd                           3
     d eCar#                          5
     d eQue                          10
     d eUpld                          1
     d eRpDt                          7
     dW8491R           pi
     d eFnd                           3
     d eCar#                          5
     d eQue                          10
     d eUpld                          1
     d eRpDt                          7

     d BadDataCheck    pr
     d CheckDuplicate  pr              n
     d DetailEdit      pr              n
     d ErrorCheckFile  pr
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     d GetNumOfHead    pr             6s 0
     d GetNumOfTrail   pr             6s 0
     d GetRecordCnt    pr             6s 0
     d InvalidChars    pr              n
     d  valueToCheck                 11a   const
     d  nameOfField                  25a   const
     d  isRequired                     n   const
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value
     d PrintUpload     pr
     d ProgramStart    pr
     d SetBadData      pr
     d  prBadData                      n   const
      //============================================================================================
      // Stand alone fields
      //============================================================================================
     d clientID        s              3    inz(*blanks)
     d recType         s              1
     d badData         s              1n   inz(*off)
     d dtlError        s              1n   inz(*off)
     d companyNm       s             60
     d empCount        s              6  0 inz(*zero)
     d errorCount      s              6  0 inz(*zero)
     d hasError        s              1n   inz(*off)
     d message         s             60
     d userName        s             50
     d recCount        s              6  0 inz(*zero)
     d BadRecCount     s              6  0 inz(*zero)
     d headerCount     s              6  0 inz(*zero)
     d trailerCount    s              6  0 inz(*zero)
     d headerFirst     s              1n   inz(*off)
     d trailerLast     s              1n   inz(*off)
     d errorFlag       s              1n   inz(*off)
     d sys_date        s               d   datfmt(*usa)
     d sysDateCymd     s              7s 0
     d sys_time        s               t   timfmt(*hms:)
     d sysTimeHms      s              6s 0
     d reportDate      s              7s 0
     d errReason       s             60
     d isRequired      s               n   inz(*on)
     d isNotRequired   s               n   inz(*off)
     d kkco#           s              3  0
     d kkfnd           s              3  0
     d kkcar#          s                   like(zTZCAR#)
     d kkefdt          s                   like(zTZefdt)
     d kkZip           s                   like(zTZzip)
     d xxque           s                   like(eQue)
     d group#          s                   like(zTZFND)
     d orTitl1         s                   like(ORTITL)
     d orTitl2         s                   like(ORTITL)
     d xxfil#          s                   like(ORFIL#)
     d UPusnm          s                   like(userName)
     d kkkey           s                   like(stkey)
     d line            s            135
     d line1           s            105
     d recordType      s              7
     d sumdigitchk     s              3  0
     d xxCarrier#      s                   like(F2car#)
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxUserOutQ      s             10
     d d_row           s              7a

     d dbwxtzp         ds
     d  rrnwxtzp             397    400b 0
     d tzData        e ds                  extname(wxtzp)
      //============================================================================================
      // Constants
      //============================================================================================
     d digits          c                   '0123456789'
     d title           c                   'Territorial Differential Upload-
     d                                      Error Report'
     d title1          c                   'Territorial Differential Upload-
     d                                      Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** No records exist in WXTZP ***'
     d message3        c                   '*** Blank record ***'
     d message4        c                   '*** Header/trailer is missing ***'
     d message5        c                   '*** Duplicate records ***'
     d message6        c                   '*** Multiple headers ***'
     d message7        c                   '*** Multiple trailers ***'
     d message8        c                   '*** No header record ***'
     d message9        c                   '*** No trailer record ***'
     d message10       c                   '*** No match on record count ***'
     d#brnd1           s             80    inz
     d#brnd2           s             80    inz
     d/copy SPrRtvBrnd
      //============================================================================================
      // Arrays
      //============================================================================================
     d err             s             60    dim(25) ctdata perrcd(1)             error msg array
     d digitCheck      s              3  0 dim(5)
      //============================================================================================
      // Data structures
      //============================================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d checkHeader     ds
     d  chkh_Rec_Cnt                  9    overlay(checkHeader:2)

     d header          ds
     d  hRecType                      1    overlay(header)
     d  Rec_Count                     9    overlay(header:2)

     d detail          ds
     d  d_RecType                     1    overlay(detail)
     d  d_RecCount                    9    overlay(detail:2)
     d  d_ZipCode                     5    overlay(detail:11)
     d  d_EffDate                     8    overlay(detail:16)
     d  d_ExpDate                     8    overlay(detail:24)
     d  d_Factor                      6    overlay(detail:32)

     d trailer         ds
     d  tRecType                      1    overlay(trailer)

     d psds          esds                  extname(szq1p)
      //====================================================
      // Prototypes
      //====================================================
     d/copy sprcompany
     d/copy sprdate
     d/copy sprstring
     d/copy wprgroup
     d/copy wprusrprms
      //============================================================================================
      // KLISTS
      //============================================================================================
     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

      //============================================================================================
      // Main Processing
      //============================================================================================
      /Free

       Open WXTZP;
       ProgramStart();
       ErrorCheckFile();
       If errorCount = *zero;
        //message = message1;
        //PrintError(0:*off);   //successful upload
          Setll 1 ptz0px;
          Read ptz0px;
          Dow Not %Eof;
             BadDataCheck();
             If Not badData;
                If TZrect = 'D';
                   detail = TZdata;
                   dtlError = DetailEdit();
                   If dtlError = *off;
                      WriteWMTZP();
                      recCount = recCount + 1;            //Count number of uploaded records
                      eUpld = 'Y';
                   else;
                      dtlError = *off;
                      BadRecCount = BadRecCount + 1;       //Count number of bad records
                   Endif;
                Endif;
             Else;
                badData = *off;
             Endif;
             Read ptz0px;
          Enddo;
          PrintUpload();        //upload report
          eRpDt = %Editc(sysDateCymd:'X');
       Else;
          eUpld = 'N';
       Endif;

       If hasError;
          Select;
          When eUpld = 'Y';                         //Upload with error
             eUpld = 'W';
          When eUpld = ' ';                         //All Details are bad data
             eUpld = 'N';
          Endsl;
       Endif;

       If hasError;
          xxfil# = '0';
          Chain orky por01;
          If %found;
             Except route;
          Endif;
       Else;
          xxfil# = '1';
          Chain orky por01;
          If %found;
             Except route1;
          Endif;
       Endif;

       Close WXTZP;
       *Inlr = *On;

      /End-Free
      *================================================================
      * Output files
      *================================================================
     o/copy scopyrout

     oqsysprt   e            route          1                                   error report

     o          e            route          1 59
     o                       #brnd1
     o          e            route          1
     o                       #brnd2
     //
     // Report Heading
     //
     o          e            head           1 06
     o                       sys_time
     o                       companyNm           95
     o                       sys_date           130
     o          e            head           2
     o                       q1pgm
     o                       ORTITL              95
     o                                          120 'PAGE:'
     o                       page          z    130
     o          e            head           0
     o                                            3 'Row'
     o                                         +006 'Rec type'
     o                                         +002 'Zip Code'
     o                                         +002 'Effective Date'
     o                                         +002 'Expiration Date'
     o                                         +002 'Factor'
     o                                         +003 'Error'
     o          e            head           2
     o                       line               135
     o          e            det            1
     o               20      d_Row               07
     o               20      recordType        +002
     o               20      d_Zipcode         +005
     o               20      d_EffDate         +006
     o               20      d_ExpDate         +008
     o               20      d_Factor          +006
     o               20      errReason         +003
     o              n20      message             60

     o/copy scopyrout1

     oqsysprt1  e            route1         1                                   successful upload

     o          e            route1         1 59
     o                       #brnd1
     o          e            route1         1
     o                       #brnd2
     //
     // Report Heading
     //
     o          e            head1          1 06
     o                       sys_time
     o                       companyNm           88
     o                       sys_date           105
     o          e            head1          2
     o                       q1pgm
     o                       ORTITL1             88
     o                                          100 'PAGE:'
     o                       page1         z    105
     o          e            head1          0
     o                                           12 'Report Date'
     o                                         +002 'Uploaded Record Count'
     o                                         +002 'Error Record Count'
     o          e            head1          2
     o                       line1              105
     o          e            det1           1
     o                       sys_date            12
     o                       recCount          +002
     o                       BadRecCount       +017
      //------------------------------------------------------------------------*
      // badDataCheck - checks for bad data and passes back inidicator
      //------------------------------------------------------------------------*
     p BadDataCheck    b
     d BadDataCheck    pi

      /Free
       Select;
       When TZrect = 'H';
          SetBadData(InvalidChars(TZrCnt:'Rec Count':isRequired));
          If badData = *On;
             Return;
          Endif;
          If %Int(TZrCnt) <> GetRecordCnt;
             badDATA = *on;
             message = message10;
             PrintError(0:*off);
             Return;
          Endif;
       When TZrect = 'D';
          SetBadData(InvalidChars(TZzip:'Zip code':isRequired));
          SetBadData(InvalidChars(TZefdt:'Eff Date':isRequired));
          SetBadData(InvalidChars(TZexdt:'Exp Date':isNotRequired));
          SetBadData(InvalidChars(TZTDF:'Factor':isRequired));
       When TZrect = 'T';
       Other;
          badData = *on;
          PrintError(2:*on);
          Return;
       Endsl;
      /End-Free

     p BadDataCheck    e
      //------------------------------------------------------------------------*
      // checkDuplicate - checks for duplicates
      //------------------------------------------------------------------------*
     p CheckDuplicate  b
     d CheckDuplicate  pi              n

     d zipcode         s              5a
     d EffDate         s              8a
     d Expdate         s              8a
     d Factor          s              6a

      /Free
       Exec Sql Declare Project Cursor For
        Select TZzip, TZefdt, TZexdt, TZtdf
        From wxtzp
        Where TZrect = 'D'
        Group By TZzip, TZefdt, TZexdt, TZtdf
        Having Count(*) > 1
        Order By TZzip, TZefdt, TZexdt, TZtdf;
       Exec Sql Open Project;
       If Sqlstt = '00000';
          Exec Sql Fetch Next From Project
           Into  :zipcode, :EffDate, :Expdate, :Factor;
       Endif;
       If Sqlstt = '00000';
          Return *On;
       Else;
          Return *off;
       Endif;
       Exec Sql Close Project;
      /End-Free

     p CheckDuplicate  e
      //----------------------------------------------------------------============================
      // detailEdit - edit detail record
      //----------------------------------------------------------------============================
     p DetailEdit      b
     d DetailEdit      pi              n

     d effDt7          s                   like(zTZefdt)
     d expDt7          s                   like(zTZefdt)
     d detailError     s              1n   inz(*off)

      /Free

       d_row = %char(%Int(rrnwxtzp) + 1);

       // Validate Zip code
       If %Int(TZzip) = 0;
          PrintError(8:*on);
          Return *on;     // No reason to continue with edits
       Endif;

       // Validate effective date
       Monitor;
          effDt7 = %dec(%char(%date(d_EffDate:*iso0):*cymd0):7:0);
       On-Error;
          PrintError(6:*on);
          Return *on;  // No reason to continue with edits
       EndMon;

       // Validate expiration date
       If d_ExpDate <> *Blank and d_ExpDate <> *all'0';
          Monitor;
             expDt7 = %dec(%char(%date(d_EffDate:*iso0):*cymd0):7:0);
          On-Error;
             PrintError(6:*on);
             Return *on; // No reason to continue with edits
          EndMon;
       Endif;

       // Territorial Factor must be a numeric value greater than 0 and less than 10
       Monitor;
          zTZTDF = %dec(TZTDF:5:4);
       On-Error;
          PrintError(3:*on);
          Return *on;  // No reason to continue with edits
       EndMon;

       If %dec(d_Factor:5:4) <= 0 or %dec(d_Factor:5:4) > 10;
          PrintError(3:*on);
          Return *on;  // No reason to continue with edits
       Endif;

       // Expiration date must be later than or equal to the Effective date
       If d_ExpDate <> *Blank and d_ExpDate <> *all'0';
          If CvtToDate(%int(d_ExpDate)) < CvtToDate(%int(d_EffDate));
             PrintError(5:*on);
             Return *on;  // No reason to continue with edits
          Endif;
       Endif;

       //Territorial Differential Factor cannot already exist
       kkzip = %Int(TZzip);
       Chain(n) (kkcar#:kkfnd:kkzip:effDt7) ptz01;
       If %Found(WMtzl1);
          PrintError(7:*on);
          Return *on;  //No reason to continue with edits
       Endif;

       Return *off;

      /End-Free

     p DetailEdit      e
      //------------------------------------------------------------------------*
      // errorCheckFile - Reads thru WXTZP and looks for errors
      //------------------------------------------------------------------------*
     p ErrorCheckFile  b
     d ErrorCheckFile  pi

      /Free
       // If any of these set of errors are encountered, output a
       //  message to the exception report and return control
       // Check for empty file
       Setll 1 ptz0px;
       Read ptz0px;
       If %Eof;
          message = message2;
          PrintError(0:*off);                                                 //error report
          Return;
       Endif;
       //
       // Check for the number of headers
       //
       headerCount = getNumOfHead;
       Select;
       When headerCount = 0;
          message = message8;
          printError(0:*off);                                                 //error report
          Return;
       When headerCount > 1;
          message = message6;
          printError(0:*off);                                                 //error report
          Return;
       Endsl;
       //
       // Check for the number of trailers
       //
       trailerCount = getNumOfTrail;
       Select;
       When trailerCount = 0;
          message = message9;
          printError(0:*off);                                                 //error report
          Return;
       When trailerCount > 1;
          message = message7;
          printError(0:*off);                                                 //error report
          Return;
       Endsl;
       //
       // Is the header the first
       //
       Setll 1 ptz0px;
       Read ptz0px;
       If Not %Eof;
          recType = tzRect;
          If recType = 'H';
             headerFirst = *on;
          Endif;
       Endif;
       //
       // Is the trailer last
       //
       Setgt *hival ptz0px;
       Readp ptz0px;
       If Not %Eof;
          recType = tzrect;
          If recType = 'T';
             trailerLast = *on;
          Endif;
       Endif;

       If headerFirst = *off or
              trailerLast = *off;
          message = message4;
          PrintError(0:*off);                                                 //error report
          Return;
       Endif;
       //
       // Check for duplicates
       //
       badData = CheckDuplicate;
       If badData;
          message = message5;
          PrintError(0:*off);
          Return;
       Endif;

      /End-Free

     p ErrorCheckFile  E
      //------------------------------------------------------------------------*
      // GetNumOfHead   - gets the number of headers
      //------------------------------------------------------------------------*
     p GetNumOfHead    b
     d GetNumOfHead    pi             6s 0
     d hCount          s              6s 0 inz(0)

      /Free
       Exec Sql
       Declare Project2 Cursor For
       Select Count(*)
       From wxtzp
       Where TZrect = 'H';
       Exec Sql
       Open Project2;
       If Sqlstt = '00000';
          Exec Sql
          Fetch Next
          From Project2
          Into  :Hcount;
       Endif;
       Exec Sql
       Close Project2;
       Return hCount;
      /End-Free

     p GetNumOfHead    e
      //------------------------------------------------------------------------*
      // getNumOfTrail  - gets the number of trailers
      //------------------------------------------------------------------------*
     p getNumOfTrail   b
     d getNumOfTrail   pi             6s 0
     d tcount          s              6s 0 inz(0)

      /Free
       Exec Sql
       Declare Project3 Cursor For
       Select Count(*)
       From wxtzp
       Where TZrect = 'T';
       Exec Sql
       Open Project3;
       If Sqlstt = '00000';
          Exec Sql
          Fetch Next
          From Project3
          Into  :tCount;
       Endif;
       Exec Sql
       Close Project3;
       Return tCount;
      /End-Free

     p GetNumOfTrail   E
      //------------------------------------------------------------------------*
      // getRecordCnt  - gets the number of records
      //------------------------------------------------------------------------*
     p GetRecordCnt    b
     d GetRecordCnt    pi             6s 0
     d rCount          s              6s 0 inz(0)

      /Free
       Exec Sql
       Declare Project4 Cursor For
       Select Count(*)
       From WXTZP;
       Exec Sql
        Open Project4;
       If Sqlstt = '00000';
          Exec Sql
           Fetch Next
           From Project4
           Into  :Rcount;
       Endif;
       Exec Sql
       Close Project4;
       Return rCount;
      /End-Free

     p GetRecordCnt    e
      //----------------------------------------------------------------=========
      // InvalidChars - look for character data in numeric field
      //----------------------------------------------------------------=========
     p InvalidChars    b
     d InvalidChars    pi              n
     d  valueToCheck                 11    const
     d  nameOfField                  25    const
     d  isRequired                     n   const
     d  aNumber        s             11s 0

      /Free
       Monitor;
          aNumber = %Int(valueToCheck);
       On-Error;
          Select;
          When Not(isRequired) and valueToCheck = *blanks;
          Other;
             aNumber = %Int(rrnwxtzp) + 1;
             message = 'Row ' + %Char(aNumber) + ' - ' +
              %trim(nameOfField) + ' has character data.';
             PrintError(0:*Off);
             Return *On;
          Endsl;
       EndMon;
       Return *Off;
      /End-Free

     p InvalidChars    e
      //------------------------------------------------------------------------*
      // printError - Prints errors to a report
      //------------------------------------------------------------------------*
     p printError      b
     d printError      pi
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value

      /Free
       If errorNumber <> 0;
          errReason = err(errorNumber);
       Endif;

       *in20 = *off;
       If errorFlag = *on;
          *in20 = *on;
       Endif;

       If eQue = *blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;

       xxque    = eQue;
       userName = GetUsrName(q1user);
       UPusnm   = UserName;
       line = *all'_';
      //
      // Get the distribution information for the error report.
      //
       If not hasError;
          clear ORTITL;

          xxfil#   = '0';
          Chain orky por01;

          If ORtitl = *blank;
             ORtitl = title;
          Endif;

          ORtitl = centerFld(ORtitl);
          hasError = *on;
          Except head;
       Endif;

       ORtitl = centerFld(ORtitl);
       If *inoa = *on;
          *inoa = *off;
          Except head;
       Endif;

       Select;
       When TZrect = 'H';
          recordType = 'Header';
       When TZrect = 'D';
          recordType = 'Detail';
       When TZrect = 'T';
          recordType = 'Trailer';
       Other;
          recordType = 'Invalid';
       Endsl;

       Except det;

       If message <> message1 or errorNumber <> 0;
          errorCount = errorCount + 1;
       Endif;
      /End-Free

     p PrintError      e
      //----------------------------------------------------------------============================
      // PrintUpload - prints successful upload report
      //----------------------------------------------------------------============================
     p PrintUpload     b
     d PrintUpload     pi

      /Free
       xxque    = eQue;
       If xxQue = *Blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;
       userName = GetUsrName(q1user);
       UPUSNM   = userName;
       line1 = *all'_';
       Clear ORTITL;
       xxfil#   = '1';
       Chain orky por01;

       If ORTITL = *blank;
          ORTITL1 = title1;
       Else;
          ORTITL1 = ORTITL;
       Endif;

       ORtitl1 = LeftJstify(ORtitl1);
       ORtitl1 = CenterFld(ORtitl1);
       Except head1;
       Except det1;
      /End-Free

     p PrintUpload     e
      //------------------------------------------------------------------------*
      // programStart - Initialization
      //------------------------------------------------------------------------*
     p ProgramStart    b
     d programStart    pi

      /Free
       GetClientID(clientID);
       sys_Date = %date();
       sys_Time = %time();
       sysDateCymd = %Dec(sys_Date:*cymd);
       sysTimeHms  = %Dec(sys_Time:*hms);
       kkco# = UserCo#(q1user);
       companyNm = GetCompNam(kkco#);
       companyNm = CenterFld(companyNm);
       kkfnd  = %Dec(efnd:3:0);
       kkcar# = %Dec(ecar#:5:0);
       Chain fnky pfn01;
       RtvBrnd('@':#Brnd1:#brnd2);
       xxUserOutQ = GetUsrOutQ(Q1user);
      /End-Free

     p ProgramStart    e
      //----------------------------------------------------------------
      // SetBadData - sets badData *on if not already *on
      //----------------------------------------------------------------
     p SetBadData      b
     d SetBadData      pi
     d  prBadData                      n   const

      /Free
       If prBadData = *On;
          badData = *On;
       Endif;
      /End-Free

     p SetBadData      e
      //----------------------------------------------------------------
      // WriteWMTZP - writes to WMTZP
      //----------------------------------------------------------------
     p WriteWMTZP      b
     d WriteWMTZP      pi

      /Free


       Clear ptz01;
       zTZcar# = kkCar#;
       zTZfnd  = kkfnd;
       zTZzip  = %int(TZzip);
       zTZEfDt = %Dec(%Char(%Date(d_EffDate:*iso0):*Cymd0):7:0);
       If d_ExpDate <> *Blank and d_ExpDate <> *all'0';
          zTZExDt = %Dec(%Char(%Date(d_ExpDate:*iso0):*Cymd0):7:0);
       Endif;
       zTZTDF  = %dec(TZTDF:5:4);
       zTZStat = 'A';
       zTZuser = Q1user;
       zTZpgm  = Q1pgm;
       zTZdt   = sysDateCymd;
       zTZtime = sysTimeHms;
       zTZiusr = Q1user;
       zTZipgm = Q1pgm;
       zTZidt  = sysDateCymd;
       zTZitim  = sysTimeHms;
       Write ptz01;

      /End-Free

     p WriteWMTZP      e

** Error messages
Numeric fields have character information (bad data).                 1
Record type is invalid. It must be a 'H','D', or 'T'.                 2
Factor must be greater than 0 and less than 10.                       3
Expiration date is invalid.                                           4
Expiration date must be greater than Effective date.                  5
Eeffective date is invalid.                                           6
Carrier/Group/Zipcode/Efdt exists, it cannot be added.                7
Record type is invalid.                                               8
<Not used>                                                            9
<Not used>                                                           10
<Not used>                                                           11
<Not used>                                                           12
<Not used>                                                           13
<Not used>                                                           14
<Not used>                                                           15
<Not used>                                                           16
<Not used>                                                           17
<Not used>                                                           18
