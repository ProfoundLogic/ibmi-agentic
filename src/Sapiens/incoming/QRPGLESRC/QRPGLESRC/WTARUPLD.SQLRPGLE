     hcopyright('(c) 2021 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*                                                                     ****
     h/TITLE WTARUPLD - A/R Batch Upload
     h dftActGrp(*no) actGrp(*caller)
     h option(*nodebugio:*srcstmt)
     h bndDir('WTBNDDIR')
     h**************************************************************************
     h*  DESCRIPTION:  This program reads the file WXARUPLD, edits the records.
     h*                Creates an error report and a successful upload
     h*                verification report. This program can process one heade
     h*                record in each file only.
     h*
     h*  Called from:  WTARUPOVR - A/R Batch Upload - Override
     h*  Calls      :  None
     h*
     h**************************************************************************
     h*  Created on 05/09/2017 by c:62 for Project 129WUR
     h**************************************************************************
     h* Revisions:
     h*
     h* 05/18/23 C:300 Support: 110252 (CoveRisk)
     h*                Update the process so that the Post Date has no restrictions
     h*                and the transaction date is required to be the current date
     h*
     h* 10/14/21 C:291 Project 129WVY
     h*                Change A/R Batch upload to allow value of 'L' for
     h*                EFT flag to designate Lockbox batch.
     h*                Call Cash Dissolve in Lockbox mode for Lockbox
     h*                batches.
     h*
     h* 12/08/17 c:62 Ticket 51556(129)
     h*               Correct check# definition
     h*
     h**************************************************************************
     fwxarupld  if   e             disk    ReName(pau01:pau00)
     fsmorl     if   e           k disk    Rename(por01:por00)
     fwdeap     o    e           k disk    Rename(pea01:pea0p) InfDS(dbfDS)
     fwdeal     if   e           k disk    Rename(pea01:pea00)
     fwdeal4    uf   e           k disk    Rename(pea01:pea04)
     fqsysprt   o    f  132        printer oflind(*inoa)
     fqsysprt1  o    f  132        printer oflind(*inob)
     d*=========================================================================
     d* Prototypes
     d*=========================================================================
     d* Check for bad data (Invalid characters, formats etc)
     d BadDataCheck    pr              n

     d* Compare the check amount with the total posted by check#
     d ChkAmtBalances  pr              n
     d  checkNumber                   7  0
     d  CheckAmount                   9  2

     d* Create Cash Disolved Record
     d CrtCashDisRcd   pr                  ExtPgm('WA055R')
     d  mode                          1a   Const
     d  wdeapRRN                      9  0

     d* Edit the detail line for valid information
     d DetailEdit      pr

     d* Delete Reserved Batch Record
     d DltRsvRcd       pr

     d* Error check the Data, detail and summary
     d ErrorCheckData  pr

     d* Error check File Integrity (Header/Detail/Trailer/Bad Data)
     d ErrorCheckFile  pr

     d* Get A/R Batch #
     d GetBatch#       pr             9  0

     d* Get the client data
     d GetCltID        pr                  ExtPgm('RTVCLTID')
     d  cltID                         3a

     d* Get the number of header records
     d GetNumOfHead    pr             6s 0

     d* Get the number of trailer records
     d GetNumOfTrail   pr             6s 0

     d* Get the total number of records in the file
     d GetRecordCnt    pr             6s 0

     d* Get the System Code
     d GetSystemCode   pr             2a

     d* Import the Upload File
     d ImportArUpld    pr

     d* Print to the Error Report
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value

     d* Print to the Upload Report
     d PrintUpload     pr

     d* Initialization
     d ProgramStart    pr

     d* Reserve Next Batch #
     d RsvNxtBat#      pr

     d* Run Batch Edit Report
     d BatchEditRpt    pr                  ExtPgm('W9201C')
     d  chrCompany#                   3a
     d  chrBatch#                     9a
     d  chrQueue                     10a

     d* Summary Edits that are applied over multiple records
     d SummaryEdit     pr

     d* Update Batch Amount
     d UpdBatchAmt     pr

     d* Write A/R Detail
     d WriteWDEAP      pr
     d*=========================================================================
     d* Stand alone fields
     d*=========================================================================
     d ePath           s            255
     d eQueue          s             10
     d eErrorDesc      s             10

     d cltID           s              3    Inz(*Blanks)
     d recType         s              1
     d recCount        s              9  0
     d companyNm       s             40
     d userName        s             50
     d message         s             40

     d recordCount     s              6  0 Inz(*Zero)
     d empCount        s              6  0 Inz(*Zero)
     d errorCount      s              6  0 Inz(*Zero)
     d headerCount     s              6  0 Inz(*Zero)
     d trailerCount    s              6  0 Inz(*Zero)
     d hCount          s              6s 0 Inz(*Zero)
     d tCount          s              6s 0 Inz(*Zero)
     d rCount          s              6s 0 Inz(*Zero)

     d headerFirst     s              1n   Inz(*Off)
     d trailerLast     s              1n   Inz(*Off)
     d errorFlag       s              1n   Inz(*Off)
     d once            s              1n   Inz(*Off)
     d noFyr           s              1n   Inz(*Off)
     d badAmounts      s              1n   INz(*Off)
     d failed          s              1n   INz(*Off)

     d company#        s                   Like(EAco#)
     d group#          s                   Like(EAfnd)
     d year            s                   Like(EAfYr)
     d employer#       s                   Like(EAemp#)
     d division        s                   Like(EAdiv)

     d postDate        s              7  0
     d check#          s              7  0
     d checkAmt        s              9  2
     d postedAmt       s              9  2
     d tranDate        s              7  0
     d grpRRN          s              9  0
     d grpYrRRN        s              9  0
     d empRRN          s              9  0
     d empDtlRRN       s              9  0
     d batch#          s              9  0
     d batchAmt        s             11  2 Inz(*Zero)
     d systemCode      s             10    Inz(*Blank)
     d wdeapRRN        s              9  0
     d row             s              9  0
     d chrCompany#     s              3
     d chrBatch#       s              9

     d xxQueue         s                   Like(eQueue)
     D ORtitl1         s                   Like(ORtitl)
     D ORtitl2         s                   Like(ORtitl)
     d UPusNm          s                   Like(userName)
     d sys_date        s               d   DatFmt(*usa)
     d sys_time        s               t   TimFmt(*hms:)
     d reportDate      s              7s 0
     d line            s            132
     d line1           s            132
     d recordType      s             10
     d errReason       s             60

      *=========================================================================
      * Constants
      *=========================================================================
     d digits          c                   '0123456789'
     d title           c                   'A/R Batch Upload Error Report'
     d title1          c                   'A/R Batch Upload Upload Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** No records exist ***'
     d message3        c                   '*** Blank record ***'
     d message4        c                   '*** Header is not first record ***'
     d message5        c                   '*** Trailer is not last record ***'
     d message6        c                   '*** Multiple headers ***'
     d message7        c                   '*** Multiple trailers ***'
     d message8        c                   '*** No header record ***'
     d message9        c                   '*** No trailer record ***'
     d message10       c                   '*** No match on record count ***'
     d message11       c                   '*** Record Count bad data ***'
     d#brnd1           s             80    inz
     d#brnd2           s             80    inz
      *=========================================================================
      * Arrays
      *=========================================================================
     d err             s             60    dim(30) ctdata perrcd(1)
      *=========================================================================
      * Data structures
      *=========================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d psds          esds                  extname(szq1p)

     D dbfDS           ds
     D  dbRRN                397    400B 0

     d/copy SPrRtvBrnd
     d/copy SPrDate
     d/copy SPrCompany
     d/copy SPrString
     d/copy WPrAr
     d/copy WPrEmplr
     d/copy WPrGroup
     d/copy WPrTable
     d/copy WPrUsrPrms
      *=========================================================================
      * PLISTS
      *=========================================================================
     c     *entry        plist
     c                   parm                    ePath
     c                   parm                    eQueue
     c                   parm                    eErrorDesc

      /free
        //=========================================================================
        //  Main Processing
        //=========================================================================
        // Initialization
        ProgramStart();

        // Lets check for errors
        ErrorCheckFile();

        // If no errors then lets upload
        If errorCount = *zero;

           // Print in error report that there aren't any errors
           message = message1;
           PrintError(0:*off);

           // Import data
           ImportArUpld();

           // A/R Batch Edit Report
           chrCompany# = %EditC(EAco#:'X');
           chrBatch#   = %EditC(EAbat#:'X');
           BatchEditRpt(chrCompany#:chrBatch#:eQueue);

        Else;
           // Batch Failed to Upload
           Except summary;
           failed = *On;
           eErrorDesc = 'BAD DATA';
        EndIf;

        // Upload print
        PrintUpload();

        // Print Routing Pages
        If Not once;
           // Error Report
           Chain (q1Pgm:'0') por00;
           If %Found(smorl);
              Except Route;
           Endif;
        Endif;

        // Upload Report
        Chain (q1Pgm:'1') por00;
        If %Found(smorl);
           Except Route1;
        Endif;

      /end-free
     C                   SetON                                        lr
      *=========================================================================
      * Output files
      *=========================================================================
     o/copy scopyrout

     oqsysprt   e            route          1

     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sys_time             8
     o                       companyNm           86
     o                       sys_date           132
     o          e            head           2
     o                       q1pgm               10
     o                       ORtitl              96
     o                                          127 'Page:'
     o                       page          z    132
     o          e            head           0
     o                                           25 'Rec type     Error Reason'
     o                                           50 '                         '
     o                                           75 '                         '
     o                                          100 '      Row                '
     o          e            head           2
     o                       line               132
     o          e            det            1
     o               20      recordType          10
     o               20      errReason         +003
     o               20      row           4   +002
     o              n20                          10 '          '
     o              n20      message           +003
     o          e            summary        2
     o                                            7 'Errors:'
     o                       errorCount    4   +002
     o/copy scopyrout1

     oqsysprt1  e            route1         1

     o          e            route1         1 59
     o                       #brnd1
     o          e            route1         1
     o                       #brnd2
    o*
    o* Report Heading
    o*
     o          e            head1          1 06
     o                       sys_time             8
     o                       companyNm           86
     o                       sys_date           132
     o          e            head1          2
     o                       q1pgm               10
     o                       ORtitl1             96
     o                                          127 'Page:'
     o                       page1         z    132
     o          e            head1          0
     o                                           11 'Report Date'
     o                                           25 'Record Count'
     o                                           36 '  Batch #'
     o          e            head1          2
     o                       line1              132
     o          e            det1           1
     o                       sys_date            11
     o              n21      recordCount   4     25
     o              n21      EAbat#        4     36
     o               21                          36 'Batch failed to upload.'
     c*------------------------------------------------------------------------*
     c* BadDataCheck - checks for bad data and passes back inidicator
     c*------------------------------------------------------------------------*
     p BadDataCheck    B
     d BadDataCheck    PI              n

     d isError         s               n   Inz(*Off)
     d badData         s               n   Inz(*Off)

      /free

       recType = UPLDrType;

       // Check to see if the record is blank
       If UPLDrtype = ' ' And UPLDcount = ' ' And UPLDco#  = ' ' And
          UPLDfnd   = ' ' And UPLDfYr   = ' ' And UPLDemp# = ' ' And
          UPLDdiv   = ' ' And UPLDarCd  = ' ' And UPLDdesc = ' ' And
          UPLDpoDt  = ' ' And UPLDchk#  = ' ' And UPLDckAm = ' ' And
          UPLDamt   = ' ' And UPLDtrDt  = ' ' And UPLDlate = ' ' And
          UPLDachF  = ' ' And UPLDcash  = ' ';
          message = message3;
          PrintError(0:*Off);
          Return *On;
       EndIf;

       // Check to see if numeric fields have valid numeric data
       Select;
       // Header Check
       When recType = 'H';
          // Is the header record count a valid number?
          Monitor;
             recCount = %Int(%Trim(UPLDcount));
          On-Error;
             message = message11;
             PrintError(0:*Off);
             Return *On;
          EndMon;
          // Does the record count match what's in the header
          If recCount <> GetRecordCnt();
             message = message10;
             PrintError(0:*Off);
             Return *On;
          EndIf;

       // Detail Check
       When recType = 'D';

          // Company # - Bad Data
          Monitor;
             company# = %Int(%Trim(UPLDco#));
          On-Error;
             PrintError(3:*On);
             badData = *On;
          EndMon;

          // Group # - Bad Data
          Monitor;
             group# = %Int(%Trim(UPLDfnd));
          On-Error;
             PrintError(4:*On);
             badData = *On;
          EndMon;

          // Year - Bad Data
          Monitor;
             year = %Int(%Trim(UPLDfYr));
          On-Error;
             PrintError(5:*On);
             badData = *On;
          EndMon;

          // Employer/Division - Bad Data
          Monitor;
             employer# = %Int(%Trim(UPLDemp#));
             division  = %Int(%Trim(UPLDdiv));
          On-Error;
             PrintError(6:*On);
             badData = *On;
          EndMon;

          // Check # - Bad Data
          If UPLDchk# <> ' ';
             Monitor;
                check# = %Dec(%Trim(UPLDchk#):7:0);
             On-Error;
                PrintError(7:*On);
                badAmounts = *On;
                badData    = *On;
             EndMon;
          EndIf;

          // Check Amount - Bad Data
          Monitor;
             isError = *Off;
             checkAmt = %Dec(%Trim(UPLDckAm):9:2);
          On-Error;
             PrintError(7:*On);
             badAmounts = *On;
             badData    = *On;
             isError    = *On;
          EndMon;
          If isError = *Off;
             // Decimal must be present in Check Amount
             If %Scan('.':UPLDckAm) = 0;
                PrintError(18:*On);
                badAmounts = *On;
                badData    = *On;
             EndIf;
          EndIf;

          // Posted Amount - Bad Data
          Monitor;
             isError = *Off;
             postedAmt = %Dec(%Trim(UPLDamt):9:2);
          On-Error;
             PrintError(8:*On);
             badAmounts = *On;
             badData    = *On;
             isError    = *On;
          EndMon;
          If isError = *Off;
             // Decimal must be present in Posted Amount
             If %Scan('.':UPLDamt) = 0;
                PrintError(20:*On);
                badAmounts = *On;
                badData    = *On;
                isError    = *On;
             EndIf;
          EndIf;

          If badData = *On;
             Return *On;
          EndIf;

       // Trailer Check
       When recType = 'T';

       // Bad record Types
       Other;
             PrintError(2:*On);
             Return *On;
       EndSl;

       Return *Off;

      /end-free

     p BadDataCheck    E
     c*---------------------------------------------------------------------------------
     c* ChkAmtBalances - Checks to see if check amount equals the total posted by check#
     c*---------------------------------------------------------------------------------
     p ChkAmtBalances  B
     fwxarupld  if   e             disk    ReName(pau01:pau0p) Static
     d ChkAmtBalances  PI              n
     d  inCheck#                      7  0
     d  inCheckAmt                    9  2

     d arUpldDS        ds                   LikeRec(pau0p)
     d*** Begin Chg *** 12/08/17 *****************************
     d*checkNumber     s              7  2
     d checkNumber     s              7  0
     d*** End   Chg *** 12/08/17 *****************************
     d checkAmount     s              9  2
     d match           s               n   Inz(*Off)
     d totalPosted     s              9  2 Inz(*Zero)

      /Free

       checkNumber = inCheck#;
       checkAmount = inCheckAmt;

       SetLL 1 pau0p;
       Read pau0p arUpldDS;
       DoW Not %Eof(wxarupld);
          If arUpldDS.UPLDrType = 'D' And
             %Dec(arUpldDS.UPLDchk#:7:0) = checkNumber;
             totalPosted += %Dec(arUpldDS.UPLDamt:9:2);
          EndIf;
          Read pau0p arUpldDS;
       EndDo;

       If totalPosted = checkAmount;
          match = *On;
       EndIf;

       Return match;

      /end-free

     p ChkAmtBalances  E
     c*----------------------------------------------------------------=========
     c* DetailEdit - edit single detail record
     c*----------------------------------------------------------------=========
     p DetailEdit      B
     d DetailEdit      PI

     d isError         s               n   Inz(*Off)
     d isValidYear     s               n   Inz(*Off)
     d isValidCo#      s               n   Inz(*Off)
     d isCodeError     s               n   Inz(*Off)
     d code            s             10
     d codeRRN         s              9  0 Inz
     d num92           s              9  2
     d numDate8        s              8  0

      /Free

       // Company # must be Valid <================================
       If ValidCo#(company#) = *Off;
          PrintError(10:*On);
       Else;
          // Group # must be Valid <===============================
          isValidCo# = *On;
          grpRRN = 0;
          If ValidGrp(group#:grpRRN:company#) = *Off;
             PrintError(11:*On);
          Else;
             // Year must be Valid <===============================
             grpYrRRN = 0;
             If ValidGrpYear(group#:year:grpYrRRN:company#) = *Off;
                PrintError(12:*On);
             Else;
                isValidYear = *On;
             EndIf;
             // Employer/Division must be Valid <======================
             empRRN = 0;
             If ValidEmp(employer#:division:empRRN:group#:
                         company#) = *Off;
                PrintError(13:*On);
             Else;
                If isValidYear = *On;
                   // Employer Policy Year must be Valid <=============
                   empDtlRRN = 0;
                   If ValidEmpYr(year:employer#:division:empRRN:
                                   group#:company#) = *Off;
                      PrintError(14:*On);
                   EndIf;
                EndIf;
             EndIf;
          EndIf;
          // A/R Code must be valid
          If ValidCode(%Trim(UPLDarCd):codeRRN:company#) = *Off;
             PrintError(15:*On);
             isCodeError = *On;
          EndIf;
       EndIf;

       // Posting Date must be Valid
       Monitor;
          isError = *Off;
          postDate = %dec(%char(%date(%Trim(UPLDpoDt):*usa/):*cymd0):7:0);
       On-Error;
          PrintError(16:*On);
          isError = *On;
       EndMon;
       //*** Beg Del ***** 05/18/23 ********************************
       //*** If isError = *Off;
       //***    // Posting Date must be in Current Month/Year
       //***    numDate8 = postDate;
       //***    If GetFirstDay(CvtToDate(numDate8)) < GetFirstDay(SystemDate());
       //***       PrintError(17:*On);
       //***    EndIf;
       //*** EndIf;
       //*** End Del ***** 05/18/23 ********************************

       // There must be an amount if Cash dissolved is set
       If UPLDcash = 'Y' And checkAmt = 0;
          PrintError(19:*On);
       EndIf;

       // Transaction Date must be valid if entered
       If UPLDtrDt <> ' ';
          Monitor;
             isError = *Off;
             tranDate = %dec(%char(%date(%Trim(UPLDtrDt):*usa/):*cymd0):7:0);
          On-Error;
             PrintError(21:*On);
             isError = *On;
          EndMon;
          If isError = *Off;
             // Transaction Date must be in Current Month/Year
             numDate8 = tranDate;
             //*** Beg Chg ***** 05/18/23 ********************************
             // Transaction Date must be equal to System Date
             //*** If GetFirstDay(CvtToDate(numDate8)) < GetFirstDay(SystemDate());
             //***    PrintError(22:*On);
             If CvtToDate(numDate8) <> SystemDate();
                PrintError(30:*On);
             //*** End Chg ***** 05/18/23 ********************************
             EndIf;
          EndIf;
       EndIf;

       // Late Flag must be Valid
       If UPLDlate <> 'Y' And UPLDlate <> ' ';
          PrintError(23:*On);
       EndIf;

       // Eft Flag must be Valid
       //*** Beg Chg ***** 10/14/21 ********************************
       //If UPLDachF <> 'A' And UPLDachF <> ' ';
       If UPLDachF <> 'A' And UPLDachF <> ' ' and UPLDachF <> 'L';
       //*** End Chg ***** 10/14/21 ********************************
          PrintError(24:*On);
       EndIf;

       // Cash Dissolved Flag must be Valid
       If UPLDcash <> 'Y' And UPLDcash <> ' ';
          PrintError(25:*On);
       Else;
          If UPLDcash = 'Y' And isCodeError = *Off;
             // A/R Code must be Payment Type if Cash Dissolved
             If PayType(GetArType(%Trim(UPLDarCd))) = *Off;
                PrintError(29:*On);
             EndIf;
          EndIf;
       EndIf;

      /end-free

     p DetailEdit      E
     c*----------------------------------------------------------------=========
     c* DltRsvRcd - Delete Reserved Batch# Record
     c*----------------------------------------------------------------=========
     p DltRsvRcd       B
     d DltRsvRcd       PI

      /Free

       Delete (EAco#:EAbat#:999:999999999:99999:999) pea04;

      /end-free

     p DltRsvRcd       E
     c*------------------------------------------------------------------------*
     c* ErrorCheckData - error checks fields
     c*------------------------------------------------------------------------*
     p ErrorCheckData  B
     d ErrorCheckData  PI

      /free

       recordCount = recordCount + 1;

       If UPLDrType = 'H' Or UPLDrType = 'D' Or UPLDrType = 'T';
          If UPLDrType = 'D';
             DetailEdit();
          EndIf;
       EndIf;

      /end-free

     p ErrorCheckData  E
     c*------------------------------------------------------------------------*
     c* ErrorCheckFile - Reads thru WXPMP and looks for errors
     c*------------------------------------------------------------------------*
     p ErrorCheckFile  B
     d ErrorCheckFile  PI

     d fileError       s               n   Inz(*Off)
      /free

       // Check for empty file
       SetLL 1 pau00;
       Read pau00;
       If %Eof(wxarupld);
          message = message2;
          PrintError(0:*off);
          fileError = *On;
       EndIf;

       // Check for the number of headers
       headerCount = getNumOfHead();

       Select;
       When headerCount = 0;
          message = message8;
          PrintError(0:*off);
          fileError = *On;
       When headerCount > 1;
          message = message6;
          PrintError(0:*off);
          fileError = *On;
       EndSl;

       // Check for the number of trailers
       trailerCount = getNumOfTrail();

       Select;
       When trailerCount = 0;
          message = message9;
          PrintError(0:*off);
          fileError = *On;
       When trailerCount > 1;
          message = message7;
          PrintError(0:*off);
          fileError = *On;
       EndSl;

       // Is the header the first
       SetLL 1 pau00;
       Read pau00;
       If Not %Eof(wxarupld);
          If UPLDrType = 'H';
             headerFirst = *On;
          EndIf;
       EndIf;

       // Is the trailer last
       SetLL *hival pau00;
       ReadP pau00;
       If Not %Eof(wxarupld);
          If UPLDrType = 'T';
             trailerLast = *On;
          EndIf;
       EndIf;

       // If header isn't first: Error
       If headerFirst = *Off;
          message = message4;
          PrintError(0:*off);
          fileError = *On;
       EndIf;

       // If trailer isn't last: Error
       If trailerLast = *Off;
          message = message5;
          PrintError(0:*off);
          fileError = *On;
       EndIf;

       // If File Errors found then skip data edits
       If fileError = *On;
          Return;
       Endif;

       // Run through file the first time checking for errors. If error is found,
       // the error report and continue checking the file for other errors. Preven
       // further processing.
       SetLL 1 pau00;
       Read pau00;
       DoW Not %Eof(wxarupld);
           row += 1;
           If BadDataCheck() = *Off;
              ErrorCheckData();
           Endif;
           Read pau00;
       EndDo;

       // Multiple Line Edits
       SummaryEdit();

      /end-free

     p ErrorCheckFile  E
     c*------------------------------------------------------------------------*
     c* GetBatch# - Get Batch #
     c*------------------------------------------------------------------------*
     p GetBatch#       B
     d GetBatch#       PI             9  0

     d batch#          s              9  0 Inz
     d batch#1         s              9  0 Inz
     d batch#2         s              9  0 Inz
      /free

       SetGT  (company#:' ') pea00;
       ReadPE (company#)     pea00;
       If Not %Eof(wdeal);
          batch#1 = EAbat# + 1;
       EndIf;

       SetGT  (company#:'T') pea00;
       ReadPE (company#)     pea00;
       If Not %Eof(wdeal);
          batch#2 = EAbat# + 1;
       EndIf;

       If batch#1 > batch#2;
          batch# = batch#1;
       Else;
          batch# = batch#2;
       Endif;

       If batch# = 0;
          batch# = 1;
       Endif;

       Return batch#;

      /end-free

     p GetBatch#       E
     c*------------------------------------------------------------------------*
     c* GetNumOfHead   - gets the number of headers
     c*------------------------------------------------------------------------*
     p GetNumOfHead    B
     d GetNumOfHead    PI             6s 0

      /free

       Exec SQL
          Select Count(*) Into :hCount
          From wxarupld
          Where UPLDrType = 'H';

       Return hCount;

      /end-free

     p GetNumOfHead    E
     c*------------------------------------------------------------------------*
     c* GetNumOfTrail  - gets the number of trailers
     c*------------------------------------------------------------------------*
     p GetNumOfTrail   B
     d GetNumOfTrail   PI             6s 0

      /free

       Exec SQL
          Select Count(*) Into :tCount
          From wxarupld
          Where UPLDrType = 'T';

       Return tCount;

      /end-free

     p GetNumOfTrail   E
     c*------------------------------------------------------------------------*
     c* GetRecordCnt  - gets the number of records
     c*------------------------------------------------------------------------*
     p GetRecordCnt    B
     d GetRecordCnt    PI             6s 0

      /free

       Exec SQL
          Select Count(*) Into :rCount
          From wxarupld;

       Return rCount;

      /end-free

     p GetRecordCnt    E
     c*------------------------------------------------------------------------*
     c* GetSystemCode - Get the System Code
     c*------------------------------------------------------------------------*
     p GetSystemCode   B
     d GetSystemCode   PI             2

      /free

       Exec SQL
          Select Min(SYkey) Into :systemCode
          From wtsyp
          Where SYtype = 'Y' And SYstat <> 'I';

       Return %Subst(systemCode:1:2);

      /end-free

     p GetSystemCode   E
      *=========================================================================
      * ImportArUpld - Import the flat file.
      *=========================================================================
     p ImportArUpld    B
     d ImportArUpld    PI

      /free

       // Create Reserve Record and get Batch#
       RsvNxtBat#();

       // Write Batch Records
       SetLL 1 pau00;
       Read pau00;
       DoW Not %Eof(wxarupld);
          recType = UPLDrType;
          If recType = 'D';
             writeWDEAP();
          EndIf;
          Read pau00;
       EndDo;

       // Update Batch Amount
       UpdBatchAmt();

       // Delete A/R Batch Reserve Record
       DltRsvRcd();

      /end-free

     p ImportArUpld    E
     c*------------------------------------------------------------------------*
     c* PrintError - Prints errors to a report
     c*------------------------------------------------------------------------*
     p PrintError      B
     d PrintError      PI
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value

      /free

       If errorNumber <> 0;
          errReason = err(errorNumber);
       EndIf;

       *in20 = *Off;
       If errorFlag = *On;
          *in20 = *On;
       EndIf;
    
      // Get user name if applicable.
       If eQueue = ' ';
          *in08 = *On;
          *in09 = *Off;
       Else;
          *in08 = *Off;
          *in09 = *Off;
       EndIf;
    
       xxQueue  = eQueue;
       userName = getUsrName(q1User);
       UPusNm   = userName;
       line     = *All'_';
    
      // Get the distribution information for the error report.
       If Not once;
          ORtitl = ' ';

          Chain (q1pgm:'0') por00;
          If ORtitl = ' ';
             ORtitl = title;
          EndIf;

          ORtitl = leftJstify(ORtitl);
          ORtitl = centerFld(ORtitl);

          once   = *On;
          Except head;
       EndIf;

       If *inOA = *On;
          *inOA = *Off;
          Except head;
       EndIf;

       Select;
       When UPLDrType = 'H';
          recordType = 'Header';
       When UPLDrType = 'D';
          recordType = 'Detail';
       When UPLDrType = 'T';
          recordType = 'Trailer';
       When UPLDrType = '*';
          recordType = 'All Detail';
       Other;
          recordType = 'Invalid';
       EndSl;

       Except det;

       If message <> message1 or errorNumber <> 0;
          errorCount = errorCount + 1;
       EndIf;

      /end-free

     p PrintError      e
      *----------------------------------------------------------------=========
      * PrintUpload - prints successful upload report
      *----------------------------------------------------------------=========
     p PrintUpload     B
     d PrintUpload     PI

      /free
    
      // Get user name if applicable.
       *in08 = *Off;
       *in09 = *Off;
       If eQueue = ' ';
          *in08 = *On;
       Else;
          *in09 = *On;
       EndIf;
    
       xxQueue  = eQueue;
       userName = GetUsrName(q1user);
       UPusNm   = userName;
       line1    = *all'_';
    
      // Get the distribution information for the upload report.
       Clear ORtitl;

       Chain (q1Pgm:'1') por00;
       If ORtitl = *blanks;
          ORtitl1 = title1;
       Else;
          ORtitl1 = ORtitl;
       EndIf;

       ORtitl1 = leftJstify(ORtitl1);
       ORtitl1 = centerFld(ORtitl1);

       *in21 = *Off;
       If failed = *On;
          *in21 = *On;
       EndIf;

       Except head1;
       Except det1;

      /end-free

     p PrintUpload     E
     c*------------------------------------------------------------------------*
     c* ProgramStart - Initialization
     c*------------------------------------------------------------------------*
     p ProgramStart    B
     d ProgramStart    PI

      /free

       // Get report info.
       sys_Time  = SystemTime();
       sys_Date  = SystemDate();
       companyNm = CenterFld(GetCompNam(UserCo#(q1User)));
       RtvBrnd('@':#Brnd1:#Brnd2);
       GetCltID(cltID);

      /end-free

     p ProgramStart    E
     c*----------------------------------------------------------------=========
     c* RsvNxtBat# - Create Reserved Batch# Record
     c*----------------------------------------------------------------=========
     p RsvNxtBat#      B
     d RsvNxtBat#      PI

      /Free

       // Write an A/R batch dummy record, to hold the bat#.
       Clear pea0p;

       EAco#  = company#;
       EAbat# = GetBatch#();
       EAbatA = 0;
       EAfnd  = *HiVal;
       EAfYr  = *Hival;
       EAemp# = *HiVal;
       EAdiv  = *HiVal;
       EAarCd = *Blanks;
       EAcTyp = *Blanks;
       EAdesc = *Blanks;
       EApoDt = *Zeros;
       EAchk# = *Zeros;
       EAckAm = *Zeros;
       EAamt  = *Zeros;
       EAsyCd = *Blanks;
       EAtfAp = 'N';
       EAtfFl = 'N';
       EAtfDt = *Zeros;
       EAtfAm = *Zeros;
       EAstat = *Blanks;
       EAinv# = *Zeros;
       EAtrDt = *Zeros;
       EAlate = *Zeros;

       EAuser = Q1user;
       EApgm  = Q1pgm;
       EAdt   = DateTo7(SystemDate());
       EAtime = TimeTo6(SystemTime());

       EAiUsr = q1User;
       EAiPgm = q1Pgm;
       EAiDt  = EAdt;

       Write pea0p;

       batch# = EAbat#;

      /end-free

     p RsvNxtBat#      E
     c*------------------------------------------------------------------------*
     c* SummaryEdit - Edit detail across multiple records
     c*------------------------------------------------------------------------*
     p SummaryEdit     B
     d SummaryEdit     PI

     d eftFlag         s              1    Inz
     d cashFlag        s              1    Inz
     d firstDetail     s               n   Inz(*On)
     d eftMixed        s               n   Inz(*Off)
     d cashMixed       s               n   Inz(*Off)
     d checkAmt        s              9  2
     d check#          s              7  0

      /free

       // Find out if EFT or Cash Dissolved have mixed values
       SetLL 1 pau00;
       Read pau00;
       DoW Not %Eof(wxarupld);
          If UPLDrType = 'D';
             If firstDetail = *On;
                eftFlag = UPLDachF;
                cashFlag = UPLDcash;
                firstDetail = *Off;
             Else;
                If UPLDachF <> eftFlag;
                   eftMixed = *On;
                EndIf;
                If UPLDcash <> cashFlag;
                   cashMixed = *On;
                EndIf;
             Endif;
          EndIf;
          Read pau00;
       EndDo;

       // EFT Flag can't be mixed
       If eftMIxed = *On;
          UPLDrType  = '*';
          row        = 0;
          PrintError(26:*On);
       EndIf;

       // Cash Dissolved Flag can't be mixed
       If cashMIxed = *On;
          UPLDrType  = '*';
          row        = 0;
          PrintError(27:*On);
       EndIf;

       // Check amount must equal the sum of the posted amounts by check#
       // No sense checking balances if the amounts and the check # are bad
       If badAmounts = *Off;
          SetLL 1 pau00;
          Read pau00;
          DoW Not %Eof(wxarupld);
             If UPLDrType = 'D' And UPLDchk# <> ' ';
                check#   = %Dec(%Trim(UPLDchk#):7:0);
                checkAmt = %Dec(%Trim(UPLDckAm):9:2);
                If ChkAmtBalances(check#:checkAmt) = *Off;
                   recordType = 'All Detail';
                   row        = 0;
                   PrintError(28:*On);
                EndIf;
             EndIf;
             Read pau00;
          EndDo;
       EndIf;

      /end-free

     p SummaryEdit     E
     c*----------------------------------------------------------------
     c* UpdBatchAmt - Update Batch Amount
     c*----------------------------------------------------------------
     p UpdBatchAmt     B
     d UpdBatchAmt     PI

      /free

       Chain (EAco#:EAbat#) pea04;
       If %Found(wdeal4);
          DoU %Eof(wdeal4);
             EAbatA = batchAmt;
             Update Pea04;
             ReadE (EAco#:EAbat#) pea04;
          EndDo;
       EndIf;

      /end-free

     p UpdBatchAmt     E
     c*----------------------------------------------------------------
     c* WriteWDEAP - writes to WDEAP
     c*----------------------------------------------------------------
     p WriteWDEAP      B
     d WriteWDEAP      PI

     d codeRRN         s              9  0 Inz

      /free

       Clear pea0p;

       EAco#  = %Int(%Trim(UPLDco#));
       EAfnd  = %Int(%Trim(UPLDfnd));
       EAfYr  = %Int(%Trim(UPLDfYr));
       EAemp# = %Int(%Trim(UPLDemp#));
       EAdiv  = %Int(%Trim(UPLDdiv));
       EAarCd = UPLDarCd;
       EAdesc = UPLDdesc;
       //*** beg add ***** 10/14/21 *************
       EAtfap = 'N';
       EAtffl = 'N';
       //*** end add ***** 10/14/21 *************


       If EAdesc = ' ';
          EAdesc = GetARdesc(EAarCd:codeRRN:EAco#);
       EndIf;

       EApoDt = %dec(%char(%date(%Trim(UPLDpoDt):*usa/):*cymd0):7:0);

       If UPLDchk# = ' ';
          EAchk# = 0;
       Else;
          EAchk# = %Dec(%Trim(UPLDchk#):7:0);
       EndIf;

       EAckAm = %Dec(%Trim(UPLDckAm):9:2);
       EAamt  = %Dec(%Trim(UPLDamt):9:2);

       If UPLDtrDt <> ' ';
          EAtrDt = %dec(%char(%date(%Trim(UPLDtrDt):*usa/):*cymd0):7:0);
       EndIf;

       EAlate = UPLDlate;
       EAachF = UPLDachF;
       EAcash = UPLDcash;

       EAbat# = batch#;
       EAsyCd = GetSystemCode();

       EAuser = Q1user;
       EApgm  = Q1pgm;
       EAdt   = DateTo7(SystemDate());
       EAtime = TimeTo6(SystemTime());

       EAiUsr = q1User;
       EAiPgm = q1Pgm;
       EAiDt  = EAdt;

       Write pea0p;

       //*** Beg Chg ***** 10/14/21 *************************
       //If EAcash = 'Y';
       //   wdeapRRN = dbRRN;
       //   CrtCashDisRcd('U':wdeapRRN);
       //EndIf;
       Select;
       When EAachf = 'L';
          wdeapRRN = dbRRN;
          CrtCashDisRcd('L':wdeapRRN);
       When EAcash <> 'Y';
       Other;
          wdeapRRN = dbRRN;
          CrtCashDisRcd('U':wdeapRRN);
       Endsl;
       //*** End Chg ***** 10/14/21 *************************

       batchAmt += EAamt;

      /end-free

     p WriteWDEAP      E

** Error messages
Numeric fields have character information (bad data).                          1
Record type is invalid. It must be a 'H','D', or 'T'.                          2
Company # - Bad Data                                                           3
Group # - Bad Data                                                             4
Group Year - Bad Data                                                          5
Employer/Division - Bad Data                                                   6
Check # - Bad Data                                                             7
Check Amount - Bad Data                                                        8
Posted Amount - Bad Data                                                       9
Invalid Company #.                                                            10
Invalid Group #.                                                              11
Invalid Group Year.                                                           12
Invalid Employer/Division.                                                    13
Invalid Policy Year.                                                          14
Invalid A/R Code.                                                             15
Invalid Post Date.                                                            16
Post Date can't be in prior month.                                            17
Check Amount is missing decimal.                                              18
Check Amount is required when Cash Dissolved Flag is set.                     19
Posted Amount is missing decimal.                                             20
Invalid Transaction Date                                                      21
Transaction Date can't be in prior month.                                     22
Invalid Late Flag (Y/ ).                                                      23
Invalid EFT flag (A/L/ )                                                      24
Invalid Cash Dissolved flag (Y/ ).                                            25
Can't have mixed EFT flags.                                                   26
Can't have mixed Cash Dissolved flags.                                        27
Sum of Posted Amount must equal the Check Amount.                             28
If Cash Dissolved, then the A/R Code must be a payment type.                  29
Transaction Date should be Current Date.                                      30 05/18/23
