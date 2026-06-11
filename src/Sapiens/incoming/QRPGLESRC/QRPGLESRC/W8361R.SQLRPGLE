     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h/TITLE W8361R - New S/R Payroll Upload
     h option(*nodebugio:*srcstmt)
     h bnddir('WTBNDDIR')
      //===========================================================================
      //  DESCRIPTION:  This program reads WXPUPA, edits the records, and outputs
      //                an error report or a successful upload verification report.
      //                This program can process one header and one trailer record
      //                in each flat file only.
      //
      //  Called from:  W8361C - Override
      //  Calls      :  None
      //  Created on 07/18/2017 by C:136 for Project 150WAN (Rewrite)
      //===========================================================================
      // Revisions:
      //                                                               *
      // 05/02/24 C:333 Support: 111739                                *
      //                Only display error "Self-report not allowed    *
      //                after cancellation Date" on a cancelled policy *
      //                otherwise it doesn't allow to enter Reporting  *
      //                period on active policy.                       *
      //
      // 05/18/23 C:342 Support: 107237
      //                Show error in error report for cancelled policy while using
      //                SR payroll upload process. If S/R upload report date is
      //                greater than cancellation date of policy then error report
      //                is generated with error message as below.
      //                "Self-report not allowed after cancellation date".
      //
      // 04/11/23 C:342 Support: 103044
      //                Modify error condition If a month/division/class has
      //                already been reported and not flagged as replace.
      //
      // 08/18/22 C:65  Support: 103315 (146 - SCS)
      //                When policy expiration date is passed in for reporting date
      //                verify which year it applies to as it could be valid for
      //                renewal year (when policy ends on last day of reporting
      //                period) or just for prior year (when policy ends in middle
      //                of a reporting period).  In first case, the policy year
      //                will be required to denote which year is being reported -
      //                either 1st day of renewal year or remainder of month on
      //                prior year.
      //
      // 05/24/22 C:291 Support: 97530
      //                Correct logic to negative payroll entries, PUEXT value
      //                was changed for client 150 before GetUpldTotal and
      //                GetUpldCount was getting executed. Because of different
      //                PUEXT value, SQL didn't find record in WXPUPA and retrurned
      //                zero in employeeTotal and employeeCount which allowed
      //                negative payroll entries.
      //
      // 08/06/20 C:062 Support: 90657(146)
      //                Chain using the correct carrier logical.            date
      //
      // 07/27/20 C:136 Support: 90303
      //                Allow policy expiration date to be used as a report date
      //
      // 01/07/19 C:291 Support: 64181
      //                Add Employer Link code and State to the error report.
      //
      // 10/05/18 C:136 Support: 73583
      //                Correction for 71706
      //
      // 09/17/18 C:136 Support: 73207
      //                Correct logic for negative payroll entries.
      //
      // 09/10/18 C:136 Support: 71706
      //                Accumulate previously reported payroll into payroll amount
      //                so that A/R calculates correctly.
      //
      // 08/02/18 C:136 Support: 72428
      //                Allow reports to cx'd divisions if another active eligible
      //                division doesn't exist if emp link and state are used.
      //
      // 06/27/18 C:136 Project: 150WAQ
      //                Add processing for Collateral for Sunz
      //
      // 05/18/18 C:136 Support: 70636
      //                Correct logic error in cx date routine.
      //                Enhance total report.
      //
      // 03/21/18 C:136 Support: 69830, 69919
      //                Correct writing of A/R records (WXSRP)
      //                Change to allow negative entry if the total
      //                payroll for the employee or policy will cover it
      //
      // 02/23/18 C:136 Support: 63987  (150WAN)
      //                Don't force use of payroll extensions for Sunz
      //
      // 01/02/18 C:136 Support: 63398
      //                Rechain to correct WDELP record in case later employer
      //                number exists in WDELP
      //
      //===========================================================================
     fwxpupa    if   e             disk    rename(ppu01:pup0a)
     f                                     infds(dbWxpupa)
     fsmorl     if   e           k disk
     fstabl     if   e           k disk
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwdell2    if   e           k disk    rename(pel01:pel02)
     fwdvhl1    if   e           k disk    rename(pvh01:pvh01)
     fwasrl14   uf a e           k disk    rename(psr01:psr14)
     fwmeel4    uf a e           k disk    rename(pee01:pee04)
     fwmeml18   if   e           k disk    rename(pem01:pem18)
     fwmepl7e   uf   e           k disk    rename(pep01:pep07e)
     fwmepl7    if   e           k disk    rename(pep01:pep07)
     fwmfnl     uf   e           k disk
     f*** Begin Chg *** 08/06/20 *******************************************
     f**********wmrpl1    if   e           k disk    rename(prp01:prp01)
     fwmrpl3    if   e           k disk    rename(prp01:prp03)
     f*** End   Chg *** 08/06/20 *******************************************
     fwmprl1    if   e           k disk
     fwmpxl3    if   e           k disk    rename(ppx01:ppx03)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwmvpl     if   e           k disk    rename(pvp01:pvp00)
     fwtabl     if   e           k disk    include(pbs01)
     fwxsrl     uf a e           k disk    rename(psr01:psr00) prefix(x_)
     fqsysprt   o    f  210        printer oflind(*inoa)                        error report
     fqsysprt1  o    f  105        printer oflind(*inob)                        upload report
      //=============================================================
      // Prototypes
      //=============================================================
      *Entry
     dW8361R           pr
     d inFnd                          3
     d inVdr                          5
     d inOutQ                        10
     d inReturn                       1
     dW8361R           pi
     d inFnd                          3
     d inVdr                          5
     d inOutQ                        10
     d inReturn                       1

      *** beg add ***** 09/10/18 **************************
     d AddPrvManP      pr            11  0
      *** end add ***** 09/10/18 **************************
     d BadDataCheck    pr
     d DetailEdit      pr
     d ErrorCheckData  pr
     d ErrorCheckFile  pr
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     d GetDivYear      pr              n
     d  returnCode                    1  0
     d GetEmpVendor    pr             5  0
     d GetNumOfHead    pr             6s 0
     d GetNumOfTrail   pr             6s 0
     d GetPayTotal     pr            15s 2
     d  hasEmpInfo                     n
     d GetPolicyInfo   pr
      *** beg add ***** 03/21/18 *************
     d GetUpldTotal    pr            15s 2
     d  hasEmpInfo                     n
      *** end add ***** 03/21/18 *************
     d GetRecordCnt    pr             6s 0
      *** beg add ***** 03/21/18 *************
     d GetRptTotals    pr
     d GetUpldCount    pr             3  0
     d  hasEmpInfo                     n
      *** end add ***** 03/21/18 *************
     d GetVendorInfo   pr
     d  vendor#                       5  0 const
     d InvalidChars    pr              n
     d  valueToCheck                 11a   const
     d  nameOfField                  25a   const
     d  isRequired                     n   const
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value
     d PrintUpload     pr
     d ProcessWXPUPA   pr
     d ProgramStart    pr
     d SetBadData      pr
     d  prBadData                      n   Const
     d UpdateWmepp     pr
     d WriteFiles      pr
     d WriteWasrp      pr
     d WriteWmeep      pr
     d WriteWxsrp      pr
      *** beg add ***** 03/21/18 *************
     d  replaceFlag                   1    Const
      *** end add ***** 03/21/18 *************
      //=============================================================
      // Stand alone fields
      //=============================================================
      *** beg chg ***** 03/21/18 ***********************
     d*** amtReplaced  s                   like(SRamtp)
     d payAmtReplaced  s                   like(SRamtp)
     d prmAmtReplaced  s             11s 0
      *** beg add ***** 05/18/18 **************************
     d totPayInFile    s             +2    like(SRamtp)
      *** end add ***** 05/18/18 **************************
     d totPayReplaced  s             +2    like(SRamtp)
     d totPrmReplaced  s             +2    like(SRManPrm)
     d totPayroll      s             +2    like(SRamtp)
     d totManPrem      s             +2    like(SRManPrm)
      *** Beg chg ***** 01/07/19 ***********************
     d*** errPolicyInfo   s             24
     d errPolicyInfo   s             40
      *** End chg ***** 01/07/19 ***********************
     d filePath        s             80
     d pathNameDta     ds                  dtaara('SRPAYIMPTH')
     d  lastPathName           1    256
      *** end chg ***** 03/21/18 ***********************
     d clientID        s              3    inz(*blanks)
     d recType         s              1
     d badData         s              1n   inz(*off)
     d companyNm       s             60
     d empCount        s              6  0 inz(*zero)
     d errorCount      s              6  0 inz(*zero)
     d hasError        s              1n   inz(*off)
     d message         s             60
     d userName        s             50
     d recCount        s              6  0 inz(*zero)
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
     d company         s                   like(ELCO#)
     d errReason       s             70
     d intRowNumber    s              7  0
     d intVendor       s              5  0
     d isRequired      s               n   inz(*on)
     d isNotRequired   s               n   inz(*off)
     d kkco#           s                   like(ELCO#)
     d kkfnd           s                   like(ELFND)
     d kkemp#          s                   like(ELEMP#)
     d kkdiv           s                   like(ELDIV)
     d kkefdt          s                   like(EPefdt)
     d kkfyr           s                   like(ELFYR)
     d kkclas          s                   like(PRCLAS)
     d kkspec          s                   like(EPSPEC)
     d kkext           s                   like(EPext)
     d kkrtcd          s                   like(ELRTCD)
     d kkVdr           s                   like(VPvdr#)
     d xxque           s                   like(ORoutq)
     d group#          s                   like(ELFND)
     d orTitl1         s                   like(ORTITL)
     d orTitl2         s                   like(ORTITL)
     d rptDate         s               d
     d rpDt5           s                   like(SRrpmy)
     d rpDt7           s                   like(EPefdt)
     d rpDt8           s              8  0
     d xxfil#          s                   like(ORFIL#)
     d UPusnm          s                   like(userName)
     d kkkey           s                   like(stkey)
     d kkpeol          s                   like(empeol)
     d kkpad1          s                   like(empad1)
     d kkpcty          s                   like(empcty)
     d kkrst           s                   like(empst)
     d line            s            135
     d line1           s            105
     d recordType      s              7
     d sumdigitchk     s              3  0
      *** Beg Add ***** 05/24/22 ***************************************
     d strExt          s                   like(EPext)
      *** End Add ***** 05/24/22 ***************************************
      *** Beg Add ***** 05/18/23 ***************************************
     d xxCxDt7         s              7  0 inz
      *** End Add ***** 05/18/23 ***************************************

     d dbWxpupa        ds
     d  rrnWxpupa            397    400b 0
      //===============================================================
      // Constants
      //===============================================================
     d digits          c                   '0123456789'
     d title           c                   'PEO Payroll Upload Error Report'
     d title1          c                   'PEO Payroll Upload Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** No records exist in WXPUPA ***'
     d message3        c                   '*** Blank record ***'
     d message4        c                   '*** Header/trailer is missing ***'
     d message6        c                   '*** Multiple headers ***'
     d message7        c                   '*** Multiple trailers ***'
     d message8        c                   '*** No header record ***'
     d message9        c                   '*** No trailer record ***'
     d message10       c                   '*** No match on record count ***'
     d#brnd1           s             80    inz
     d#brnd2           s             80    inz
     d/copy SPrRtvBrnd
      //===============================================================
      // Arrays
      //===============================================================
     d err             s             70    dim(30) ctdata perrcd(1)
     d digitCheck      s              3  0 dim(5)
      //===============================================================
      // Data structures
      //===============================================================
     d psds          esds                  extname(szq1p)
      //===============================================================
      // Prototypes
      //===============================================================
     d/copy sprdate
     d/copy sprcompany
     d/copy sprstring
     d/copy wpremplr
     d/copy wprgroup
     d/copy wprusrprms
      //===============================================================
      // Main Processing
      //===============================================================
      /Free

       ProgramStart();
       ErrorCheckFile();
       If errorCount = *zero;
          message = message1;
          errPolicyInfo = *Blanks;
          PrintError(0:*off);   //successful upload
          inReturn = 'Y';       //will be changed to 'S' if A/R is to be written
          ProcessWXPUPA();
          PrintUpload();        //upload report
       Else;
          inReturn = 'N';
       Endif;
       //*** beg chg ***** 09/10/18 ************
       //***If hasError;
       //***   xxfil# = '0';
       //***  Chain (Q1pgm:xxfil#) por01;
       //***  If %found;
       //***      Except route;
       //***  Endif;
       //***Else;
       //***   xxfil# = '1';
       //***   Chain (Q1pgm:xxfil#) por01;
       //***   If %found;
       //***      Except route1;
       //***   Endif;
       //Endif;

       xxfil# = '0';
       Chain (Q1pgm:xxfil#) por01;
       If %found;
          Except route;
       Endif;
       If Not(hasError);
          xxfil# = '1';
          Chain (Q1pgm:xxfil#) por01;
          If %found;
             Except route1;
          Endif;
       Endif;
       //*** end chg ***** 09/10/18 ************

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
      *** beg chg ***** 03/21/18 *************************
     o***          e            head           2
     o***                       q1pgm
     o***                       ORTITL              95
     o***                                          120 'PAGE:'
     o***                       page          z    130
     o          e            head           1
     o                       q1pgm
     o                       ORTITL              95
     o                                          120 'PAGE:'
     o                       page          z    130
     o          e            head           2
     o                                              'File Uploaded:'
     o                       filePath          +002
      *** end chg ***** 03/21/18 *************************
     o          e            head           0
     o                                         +002 'Type   '
     o                                         +002 'Row    '
      *** beg add ***** 03/21/18 *************************
      *** beg chg ***** 01/07/19 *************************
      ***                                      +002 'Grp/Emp/Div/Fyr         '
     o                                         +002 'Policy Identifier       '
      *** end chg ***** 01/07/19 *************************
      *** end add ***** 03/21/18 *************************
      *** beg chg ***** 01/07/19 *************************
     o***                                      +005 'Error reason'
     o                                         +021 'Error reason'
      *** end chg ***** 01/07/19 *************************
     o          e            head           2
     o                       line               135
     o          e            det            1
     o               20      recordType        +002
     o               20      intRowNumber      +002
      *** beg add ***** 03/21/18 *************************
     o                       errPolicyInfo     +002
      *** end add ***** 03/21/18 *************************
     o               20      errReason         +005
     o              n20      message             83

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
      *** beg chg ***** 03/21/18 *************************
     o***          e            head1          2
     o***                       q1pgm
     o***                       ORTITL1             88
     o***                                          100 'PAGE:'
     o***                       page1         z    105
     o          e            head1          2
     o                       q1pgm
     o                       ORTITL1             88
     o                                          100 'PAGE:'
     o                       page1         z    105
     o          e            head1          1
     o                                              'Group  . . . :'
     o                       kkFnd         p   +002
     o                       FNname            +002
     o          e            head1          1
     o                                              'File Uploaded:'
     o                       filePath          +002
      *** end chg ***** 03/21/18 *************************
     o          e            head1          2
     o                       line1              105
     o          e            det1           1
     o                                              'Record Count  . . . . :'
     o                       recCount      n   +009
      *** beg add ***** 05/18/18 **************************
     o          e            det1           1
     o                                              'Total Payroll in File :'
     o                       totPayInFile  n   +002
      *** end add ***** 05/18/18 **************************
     o          e            det1           1
     o                                              'Total Payroll Loaded  :'
     o                       totPayroll    n   +002
     o          e            det1           1
     o                                              'Total Premium Loaded  :'
     o                       totManPrem    n   +002
     o          e            det1           1
     o                                              'Total Payroll Replaced:'
     o                       totPayReplacedn   +002
     o          e            det1           1
     o                                              'Total Premium Replaced:'
     o                       totPrmReplacedn   +002
      *** beg add ***** 09/10/18 ***************************
      //------------------------------------------------------------------------*
      // AddPrvManP - Write WXSRP records from previous reports.  Skip classes
      //              that are processed in this upload.
      //------------------------------------------------------------------------*
     p AddPrvManPrm    b
     d AddPrvManPrm    pi
      /Free
       Setll (x_SRco#:x_SRfnd:x_SRemp#:x_SRdiv:x_SRfyr) psr14;
       If Not(%Found);
          Return;
       Endif;
       Dou %Eof(WASRL14);
          Reade (x_SRco#:x_SRfnd:x_SRemp#:x_SRdiv:x_SRfyr) psr14;
          If %Eof;
             Leave;
          Endif;
         //*** beg chg ***** 10/05/18 **********************
         //*** If x_SRtodt <> SRtodt and x_SRrpmy <> SRrpmy;
          If x_SRtodt <> SRtodt or x_SRrpmy <> SRrpmy;
         //*** end chg ***** 10/05/18 **********************
             Iter;
          Endif;
          Chain(n) (SRco#:SRfnd:SRemp#:SRdiv:SRfyr:SRtodt:SRrpmy:
           SRclas:SRspec:SRext) psr00;
          If %Found;
             Iter;
          Endif;
          x_SRco#  = SRco#;
          x_SRfnd  = SRfnd;
          x_SRemp# = SRemp#;
          x_SRdiv  = SRdiv;
          x_SRfyr  = SRfyr;
          x_SRefdt = SRefdt;
          x_SRrpmy = SRrpmy;
          x_SRtodt = SRtodt;
          x_SRamtp = SRamtp;
          x_SRManPrm = SRManPrm;
          x_SRclas = SRclas;
          x_SRspec = SRspec;
          x_SRext  = SRext;
          x_SRRepFlg = 'A';
          x_SRuser = Q1user;
          x_SRpgm  = Q1pgm;
          x_SRdt   = sysDateCymd;
          x_SRtime = sysTimeHms;
          Write psr00;
       Enddo;
      /End-Free
     p AddPrvManPrm    e
      *** end add ***** 09/10/18 ***************************
      //------------------------------------------------------------------------*
      // badDataCheck - checks for bad data and passes back inidicator
      //------------------------------------------------------------------------*
     p BadDataCheck    b
     d BadDataCheck    pi

      /Free
       Select;
       When PURecType = 'H';
          SetBadData(InvalidChars(PURecCnt:'Rec Count':isRequired));
          If badData = *On;
             Return;
          Endif;
          If %Int(PURecCnt) <> GetRecordCnt;
             badDATA = *on;
             message = message10;
             PrintError(0:*off);
             Return;
          Endif;
       When PURecType = 'D';
          SetBadData(InvalidChars(PUemp#:'Employer #':isNotRequired));
          SetBadData(InvalidChars(PUdiv:'Divsion #':isNotRequired));
          SetBadData(InvalidChars(PUfyr:'Policy Year':isNotRequired));
          SetBadData(InvalidChars(PUfein:'FEIN':isNotRequired));
          SetBadData(InvalidChars(PURpDt:'Report Date':isNotRequired));
          SetBadData(InvalidChars(PUCkDt:'Check Date':isNotRequired));
          SetBadData(InvalidChars(PUPayAmt:'Payroll Amount':isRequired));
          SetBadData(InvalidChars(PUclass:'Payroll Class':isRequired));
       When PURecType = 'T';
       Other;
          badData = *on;
          PrintError(2:*on);
          Return;
       Endsl;
      /End-Free

     p BadDataCheck    e
      //----------------------------------------------------------------
      // detailEdit - edit detail record
      //----------------------------------------------------------------
     p DetailEdit      b
     d DetailEdit      pi
     d ckDt7           s                   like(EPefdt)
     d cxDate7         s                   like(EPefdt)
     d kkSt            s                   like(RSJrSt)
     d returnCode      s              1  0
     d hasEmpInfo      s               n
     d isMissingInfo   s               n

      /Free
       //*** beg chg ***** 03/21/18 *****************
       //*** If PUReplFlag <> 'Y' and PUReplFlag <> *blank;
       //***    PrintError(18:*on);
       //*** Endif;
       //*** If PUReplFlag = 'Y' and %Int(PUPayAmt) < 0;
       //***    PrintError(11:*on);
       //*** Endif;
       errPolicyInfo = *Blanks;
       //*** end chg ***** 03/21/18 *****************
       //
       // Validate report date
       //
       Monitor;
          rptDate  = %date(PURpDt:*iso0);
          rpDt7 = %dec(rptDate:*cymd);
          rpDt8 = %dec(rptDate:*usa);
         //*** beg add ***** 09/10/18 ******************
          rpDt5 = %Int(%Subst(%Editc(rpDt7:'X'):1:5));
         //*** end add ***** 09/10/18 ******************
       On-Error;
          PrintError(14:*on);
          Return;     // No reason to continue with edits
       EndMon;
       //
       // Either link code/state or policy#/div or emp#/div/fyr is required
       //
       Select;
       When PUEmpLink <> *blanks and PUJrSt <> *blanks;
       When PUpolicy# <> *blanks and PUdiv <> *blanks;
       When PUemp# <> *blanks and PUdiv <> *blanks and PUfyr <> *blanks;
       Other;
          PrintError(3:*on);
          Return;     // No reason to continue with edits
       Endsl;
       //
       // In order:
       //  Validate emp#/div/fyr
       //  Validate carrier/div
       //  Validate link code/rating state
       //
       Select;
       When PUemp# <> *blanks and PUdiv <> *blanks and PUfyr <> *blanks;
          //*** beg add ***** 01/07/19 ************************
          errPolicyInfo = 'Emp/Div/Yr: ' + %Trim(PUEmp#) + '/' +
           %Trim(PUdiv) + '/' + %Trim(PUfyr);
          //*** end add ***** 01/07/19 ************************
          Chain (kkCo#:kkFnd:%Int(PUfyr):%Int(PUemp#):%Int(PUdiv)) pel01;
          If Not(%Found);
             PrintError(4:*On);
             Return;
         //*** beg del ***** 07/27/20 ********************************/
         //**Else;
         //**If rpDt7 < ELpfdt or rpDt7 >= ELptdt;
         //**   PrintError(15:*On);
         //**   Return;
         //**Endif;
         //*** end del ***** 07/27/20 ********************************/
          Endif;
       When PUpolicy# <> *blanks and PUdiv <> *blanks;
          //*** beg add ***** 01/07/19 ************************
          errPolicyInfo = 'Pol#/Div: ' + %Trim(PUpolicy#) + '/' +
           %Trim(PUDiv);
          //*** end add ***** 01/07/19 ************************
          //*** Begin Chg *** 08/06/20 *************************************
          //*** Setll (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Reade (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Dow Not(%Eof);
          //***    If RPdiv = %Int(PUdiv);
          //***       Leave;
          //***    Endif;
          //*** Reade (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Enddo;
          Setll (kkCo#:PUpolicy#:kkFnd) prp03;
          Reade (kkCo#:PUpolicy#:kkFnd) prp03;
          Dow Not(%Eof);
             If RPdiv = %Int(PUdiv);
                Leave;
             Endif;
          Reade (kkCo#:PUpolicy#:kkFnd) prp03;
          Enddo;
          //*** End   Chg *** 08/06/20 *************************************
          If %Eof;
             PrintError(5:*On);
             Return;
          Else;
             Chain (RPco#:RPfnd:RPfyr:RPemp#:RPdiv) pel01;
         //*** beg del ***** 07/27/20 ********************************/
         //**If rpDt7 < ELpfdt or rpDt7 >= ELptdt;
         //**   PrintError(15:*On);
         //**   Return;
         //**Endif;
         //*** end del ***** 07/27/20 ********************************/
          Endif;
       When PUEmpLink <> *blanks and PUjrst <> *blanks;
          //*** beg add ***** 01/07/19 ************************
          errPolicyInfo = 'Link/St:  ' + %Trim(PUEmpLink) + '/' + PUjrst;
          //*** end add ***** 01/07/19 ************************
          If GetDivYear(returnCode) = *Off;
             Select;
             When inVdr <> *Zeros and returnCode = 1;
                PrintError(26:*on);  //No link/state exists for vendor
                Return;     // No reason to continue with edits
             When returnCode = 1;
                PrintError(7:*on);  //No link/state exists
                Return;     // No reason to continue with edits
             When returnCode = 2;
                PrintError(8:*on);  //Multiple policies exist
                Return;     // No reason to continue with edits
             Endsl;
          Endif;
       Endsl;

       //*** beg add ***** 03/21/18 ************************
       //*** beg del ***** 01/07/19 ************************
       //errPolicyInfo = %Trim(%Char(ELfnd)) + '/' + %Trim(%Char(ELemp#))
       // + '/' + %Trim(%Char(ELdiv)) + '/' +  %Trim(%Char(ELfyr));
       //*** End del ***** 01/07/19 ************************
       //
       // Replacement Flag must by 'Y' or *blank
       //
       If PUReplFlag <> 'Y' and PUReplFlag <> *blank;
          PrintError(18:*on);
       Endif;
       //
       // Payroll amount can't be negative if replacing payroll
       //
       If PUReplFlag = 'Y' and %Int(PUPayAmt) < 0;
          PrintError(11:*on);
       Endif;
       //*** end add ***** 03/21/18 *****************
       //
       // FEIN must match policy FEIN if provided
       //
       If PUfein <> *Blanks and PUfein <> %Editc(EMfein:'X');
          PrintError(25:*On);
       Endif;

       //*** beg chg ***** 02/23/18 ***********************
       //Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
       // %Int(PUclass):PUspec:PUext:rpDt7) pep07e;
       //Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
       // %Int(PUclass):PUspec:PUext) pep07e;
       //Select;
       //When %Eof;
       //   PrintError(12:*On);
       //Endsl;
       //
       // Payroll class code must exist on policy as of the report date.
       //  This logical excludes inactive records.
       //
       //*** Beg Add ***** 05/24/22 **********************************
       strExt = PUext;
       //*** End Add ***** 05/24/22 **********************************
       Select;
       When clientID = '150' and PUext = *Blanks;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:rpDt7) pep07;
          Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec) pep07;
          If %Eof;
             PrintError(12:*On);
          Else;
             PUext = EPext;
          EndIf;
       Other;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext:rpDt7) pep07e;
          Readpe(n) (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext) pep07e;
          If %Eof;
             PrintError(12:*On);
          EndIf;
       EndSl;
       //*** end chg ***** 02/23/18 ***********************
       //
       // Ensure policy is flagged for self reporting
       //
       Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
       If BSsefl <> 'S';
          PrintError(19:*On);
       Endif;
       //
       // Report date must be in policy year date range.
       //
      //*** beg chg ***** 07/27/20 ****************
      //*** If rpDt7 < ELpfdt or rpDt7 >= ELptdt;
       If rpDt7 < ELpfdt or rpDt7 > ELptdt;
      //*** end chg ***** 07/27/20 ****************
          PrintError(22:*On);
       EndIf;
       //
       // Ensure report date is appropriate for the reporting period
       //  Monthly report date must be an end of month day
       //  Weekly report date must be on designated day of week
       // Exception - policy exp date can be used.
       Select;
       When BSsefl <> 'S';
       //*** beg add ***** 07/27/20 ****************
       When rpDt7 = ELptdt;
       //*** end add ***** 07/27/20 ****************
       When BSfreq <> 'W' and rptDate <> GetLastDay(rptDate);
          PrintError(20:*On);
       When BSfreq  = 'W';
          If %Subst(Upper(DowShort(rpDt8)):1:2) <> BSdowk;
             PrintError(21:*On);
          Endif;
       EndSl;
       //*** beg add ***** 09/10/18 ***********************
       //
       // Print Info Msg if class has been previously reported
       //
       Select;
       When BSsefl <> 'S';
       When BSfreq <> 'W';
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
           %Int(PUclass):PUspec:PUext:0:rpDt5) psr14;
          If %Found;
             PrintError(29:*On);
          Endif;
       When BSfreq  = 'W';
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
           %Int(PUclass):PUspec:PUext:rpDt7:0) psr14;
          If %Found;
             PrintError(29:*On);
          Endif;
       EndSl;
       //*** end add ***** 09/10/18 ***********************
       //
       // Ensure policy vendor assignment matches vendor if passed in
       //
       intVendor = GetEmpVendor();
       If intVendor <> 0;
          GetVendorInfo(intVendor);
       Endif;
       If inVdr <> *Zeros and
          %Int(inVdr) <> intVendor;
          PrintError(17:*On);
          Return;  // No reason to continue with edits
       EndIf;
       //
       // Ensure no employee information is missing if required
       //  or is complete if any emp info is provided.
       //
       If PULastName <> *blanks or PUFrstName <> *blanks or
        PUMidInit <> *blanks or PUtitle <> *blanks or
        PUposition <> *blanks or PUss# <> *blanks;
          hasEmpInfo = *On;
       Endif;
       If PULastName = *blanks or PUFrstName = *blanks or
        PUss# = *blanks;
          isMissingInfo = *On;
       Endif;
       Select;
       When intVendor <> 0 and VPEmpReq = 'Y' and isMissingInfo;
          PrintError(16:*On);
          hasEmpInfo = *Off;
       When hasEmpInfo and isMissingInfo = *On;
          PrintError(23:*On);
          hasEmpInfo = *Off;
       Endsl;
       //
       // Ensure that negative amount doesn't cause total reported payroll
       //  to go negative for employee or class
       //
       //*** beg chg ***** 03/21/18 **************
       //If (GetPayTotal(hasEmpInfo) + %Int(PUPayAmt) < 0);
       //*** PrintError(24:*On);
       //Endif;
       If (GetPayTotal(hasEmpInfo) + GetUpldTotal(hasEmpInfo)) < 0;
          If hasEmpInfo;
             PrintError(27:*On);
          Else;
             PrintError(24:*On);
          Endif;
       Endif;
       //
       // The replacement record can be the only record
       //  for an employee payroll and/or policy payroll
       //  if entry is flagged for replacement
       //
       If PUReplFlag = 'Y' and GetUpldCount(hasEmpInfo) > 1;
          PrintError(28:*On);
       Endif;
       //*** end chg ***** 03/21/18 **************
       //
       // Validate check date (if provided)
       //
       If PuCkDt <> *blanks;
          Monitor;
             ckDt7 = %dec(%char(%date(PUCkDt:*iso0):*cymd0):7:0);
          On-Error;
             PrintError(10:*on);
             Return;     // No reason to continue with edits
          EndMon;
       Endif;
       //
       // Ensure check date is within '# of check days issuance'
       //
       If PUCkDt <> *blanks and VP#OfDays <> 0;
       //*** beg chg ***** 05/18/18 *********************
       //***cxDate7 = GetCxDate(EMco#:EMfnd:EMemp#:EMdiv);
          cxDate7 = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
       //*** end chg ***** 05/18/18 *********************
          If cxDate7 <> 0 and
           %Date(PUCkDt:*iso0) > (%Date(cxDate7:*Cymd) + %days(VP#OfDays));
             PrintError(13:*On);
          Endif;
       Endif;
       //*** beg chg ***** 05/18/23 *********************
       xxCxDt7 = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
       //*** Beg Add ******* 05/02/24 ***********************
       If xxCxDt7 <> 0;
       //*** End Add ******* 05/02/24 ***********************
          If rpDt7 > xxCxDt7;
             PrintError(30:*On);
          Endif;
       //*** Beg Add ******* 05/02/24 ***********************
       Endif;
       //*** End Add ******* 05/02/24 ***********************
       //*** end chg ***** 05/18/23 *********************
      /End-Free

     p DetailEdit      e
      //------------------------------------------------------------
      // errorCheckData - error checks fields
      //------------------------------------------------------------
     p ErrorCheckData  b
     d ErrorCheckData  pi

      /Free
       Select;
       When PURecType = 'H';
          recCount = recCount + 1;
       When PURecType = 'D';
          recCount = recCount + 1;
          DetailEdit();
       When PURecType = 'T';
          recCount = recCount + 1;
       Endsl;
      /End-Free

     p ErrorCheckData  e
      //------------------------------------------------------------
      // errorCheckFile - Reads thru WXPUPA and looks for errors
      //------------------------------------------------------------
     p ErrorCheckFile  b
     d ErrorCheckFile  pi

      /Free
       // If any of these set of errors are encountered, output a
       //  message to the exception report and return control
       // Check for empty file
       Read pup0a;
       If %Eof;
          message = message2;
          PrintError(0:*off);
          Return;
       Endif;
       //
       // Check for the number of headers
       //
       headerCount = getNumOfHead;
       Select;
       When headerCount = 0;
          message = message8;
          printError(0:*off);
          Return;
       When headerCount > 1;
          message = message6;
          printError(0:*off);
          Return;
       Endsl;
       //
       // Check for the number of trailers
       //
       trailerCount = getNumOfTrail;
       Select;
       When trailerCount = 0;
          message = message9;
          printError(0:*off);
          Return;
       When trailerCount > 1;
          message = message7;
          printError(0:*off);
          Return;
       Endsl;
       //
       // Is the header the first
       //
       Setll 1 pup0a;
       Read pup0a;
       If Not %Eof;
          If PURecType = 'H';
             headerFirst = *on;
          Endif;
       Endif;
       //
       // Is the trailer last
       //
       Setgt *hival pup0a;
       Readp pup0a;
       If Not %Eof;
          If PURecType = 'T';
             trailerLast = *on;
          Endif;
       Endif;

       If headerFirst = *off or
              trailerLast = *off;
          message = message4;
          PrintError(0:*off);
          Return;
       Endif;
       //
       // Run through file the first time checking for errors. If error is found, except the line
       // the error report and continue checking the file for other errors. Prevent the batch fro
       // further processing.
       //
       Setll 1 pup0a;
       Read pup0a;
       Dow Not %Eof;
          BadDataCheck();
          If Not badData;
             ErrorCheckData();
          Else;
             badData = *off;
          Endif;
       Read pup0a;
       Enddo;
      /End-Free

     p ErrorCheckFile  E
      //----------------------------------------------------------------
      // GetDivYear - Return *On if division and year found for link/state
      //----------------------------------------------------------------
     p GetDivYear      b
     d GetDivYear      pi              n
     d  returnCode                    1  0

     d svFyr           s                   like(ELfyr)
     d svEmp#          s                   like(EMemp#)
     d svDiv           s                   like(EMdiv)
     d isFound         s               n
     d isCanceled      s               n
     d svCxFyr         s                   like(ELfyr)
     d svCxEmp#        s                   like(EMemp#)
     d svCxDiv         s                   like(EMdiv)
     d svEMcad1        s                   like(EMcad1)
     d*** begin add ***  08/18/22  **********************************
     d wkFyr           s                   like(ELfyr)
     d*** end add *****  08/18/22  **********************************

      /Free
       //*** begin add ***  08/18/22  **********************************
       wkFyr = 0;
       If PUFyr <> *blanks;
          Monitor;
             wkFyr = %int(pufyr);
          On-error;
          Endmon;
       Endif;
       //*** end add *****  08/18/22  **********************************
       // Get year from PEO Master
       // Look for division with rating state and vendor (if provided)
       Setll (kkCo#:kkFnd:PUEmpLink) pem18;
       Reade (kkCo#:kkFnd:PUEmpLink) pem18;
       Dow Not(%Eof);
          Setgt (EMco#:EMfnd:EMemp#:Emdiv:rpDt7) pel02;
          Readpe (EMco#:EMfnd:EMemp#:Emdiv) pel02;
          //*** begin chg ***  08/18/22  **********************************
          //*****Select;
          //*** beg chg ***** 07/27/20 *********************
          //*** When Not(%Eof) and ELpfdt <= rpDt7 and ELptdt > rpDt7;
          //*****When Not(%Eof) and ELpfdt <= rpDt7 and ELptdt >= rpDt7;
          //* Ensure reporting date falls within the policy year and
          //* if its equal to policy eff or exp dates then verify which
          //* year it applies to based on 1) year passed in or 2) its
          //* equal to a reporting date.
          Dow not %eof(wdell2);
             Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
             If ELpfdt <= rpDt7 and ELptdt >= rpDt7 and
              ((rpDt7 <> ELPfdt and rpDt7 <> ELPtdt) or
              ((rpDt7 = ELPfdt or rpDt7 = ELPtdt) and
              ((wkFyr <> 0 and wkFyr = ELFyr) or (rpDt7 = ELPtdt or
              (BSfreq <> 'W' and rptDate = GetLastDay(rptDate)) or
              (BSFreq = 'W'and %Subst(Upper(DowShort(rpDt8)):1:2) =
              BSdowk)))));
                Leave;
             Else;
                Readpe (EMco#:EMfnd:EMemp#:Emdiv) pel02;
             Endif;
          Enddo;
          //* Proceed if valid policy year found for reporting date.
          If not %eof(wdell2);
          //*** end chg *****  08/18/22  **********************************
          //*** end chg ***** 07/27/20 *********************
             // Look for active vendor history assignment of
             //  vendor if passed in
             If inVdr <> *Zeros;
                If %Int(inVdr) <> GetEmpVendor();
                   Reade (kkCo#:kkFnd:PUEmpLink) pem18;
                   Iter;
                Endif;
             Endif;

             // Check if division matches rating state. Save off
             //  emp#/div/year if it does.  Prevent upload if another
             //  eligible emp/div exists for link/state. Save off
             //  and report to latest prior canceled division in case an
             //  active division is not found.
             Chain (kkCo#:ELrtcd) prs00;
             If %Found and RSjrst = PUjrst;
                Select;
                When EMcad1 <= rpDt7 and EMcad1 > svEMcad1;
                   isCanceled = *On;
                   svCxEmp# = ELemp#;
                   svCxDiv  = ELdiv;
                   svCxFyr  = ELfyr;
                   svEMcad1 = EMcad1;
                   Reade (kkCo#:kkFnd:PUEmpLink) pem18;
                   Iter;
                When Not(isFound);
                   isFound = *On;
                   svEmp# = ELemp#;
                   svDiv  = ELdiv;
                   svFyr  = ELfyr;
                When svEmp# <> EMemp# or svDiv <> EMdiv;
                   returnCode = 2;
                   Return *Off;
                Endsl;
             Endif;
          //*** begin chg ***  08/18/22  **********************************
          //*****Endsl;
          Endif;
          //*** end chg *****  08/18/22  **********************************
       Reade (kkCo#:kkFnd:PUEmpLink) pem18;
       Enddo;

       Select;
       When isFound;
          Chain (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          Return *On;
       When isCanceled;
          Chain (kkCo#:kkFnd:svCxFyr:svCxEmp#:svCxDiv) pel01;
          Return *On;
       Other;
          returnCode = 1;
          Return *Off;
       EndSl;
      /End-Free

     p GetDivYear      e
      //---------------------------------------------------------------
      // Gets Vendor for Policy
      //---------------------------------------------------------------
     p GetEmpVendor    b
     d GetEmpVendor    pi             5  0

      /Free
       rpDt7 = %dec(%char(%date(PURpDt:*iso0):*cymd0):7:0);
       Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:rpDt7) pvh01;
       Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pvh01;
       Dow Not(%Eof);
          If VHexdt = 0 or VHexdt <> 0 and VHexdt > rpDt7;
             Return VHvdr#;
          Endif;
       Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pvh01;
       Enddo;
       Return *Zeros;
      /End-Free

     p GetEmpVendor    e
      //---------------------------------------------------------------
      // GetNumOfHead   - gets the number of headers
      //---------------------------------------------------------------
     p GetNumOfHead    b
     d GetNumOfHead    pi             6s 0
     d hCount          s              6s 0 inz(0)

      /Free
       Exec Sql
       Declare Project2 Cursor For
       Select Count(*)
       From WXPUPA
       Where PURecType = 'H';
       Exec Sql
       Open Project2;
       If Sqlstt = '00000';
          Exec Sql
          Fetch Next
          From Project2
          Into  :hCount;
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
       From WXPUPA
       Where PURecType = 'T';
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

     p GetNumOfTrail   e
      //------------------------------------------------------------------------*
      // Returns total payroll from Employee file
      //------------------------------------------------------------------------*
     p GetPayTotal     b
     d GetPayTotal     pi            15s 2
     d  hasEmpInfo                     n
     d payTotal        s             15s 2

      /Free
       //*** beg chg ***** 03/21/18 ************************
       //*** If hasEmpInfo;
       //***    Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
       //***     PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) pee04;
       //***    Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
       //***     PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) pee04;
       //***    Dow Not(%Eof);
       //***    Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
       //***     PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) pee04;
       //***       payTotal += EEsapr;
       //***    Enddo;
       //***    Return payTotal;
       //*** Else;
       //***   Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) psr14;
       //***    Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) psr14;
       //***    Dow Not(%Eof);
       //***       payTotal += SRamtp;
       //***    Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
       //***     %Int(PUclass):PUspec:PUext:rpDt7) psr14;
       //***    Enddo;
       //***    Return payTotal;
       //*** EndIf;
       If hasEmpInfo;
          Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
           PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
           %Int(PUclass):PUspec:PUext) pee04;
          Dou %Eof;
             Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
              PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
              %Int(PUclass):PUspec:PUext) pee04;
             If %Eof;
                Leave;
             Endif;
             payTotal += EEsapr;
          Enddo;
          Return payTotal;
       Else;
          Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
           %Int(PUclass):PUspec:PUext) psr14;
          Dou %Eof;
          Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
           %Int(PUclass):PUspec:PUext) psr14;
             If %Eof;
                Leave;
             Endif;
             payTotal += SRamtp;
          Enddo;
          Return payTotal;
       EndIf;
       //*** end chg ***** 03/21/18 ************************
      /End-Free

     p GetPayTotal     e
      //------------------------------------------------------------------------*
      // GetPolicyInfo - Sets WDELP to policy key being processed
      //------------------------------------------------------------------------*
     p GetPolicyInfo   b
     d GetPolicyInfo   pi
     d  returnCode     s              1  0

      /Free
       //
       // Find WDELP record
       //
       Select;
       When PUemp# <> *blanks and PUdiv <> *blanks and PUfyr <> *blanks;
          Chain (kkCo#:kkFnd:%Int(PUfyr):%Int(PUemp#):%Int(PUdiv)) pel01;
       When PUpolicy# <> *blanks and PUdiv <> *blanks;
          //*** Begin Chg *** 08/06/20 *************************************
          //*** Setll (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Reade (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Dow Not(%Eof);
          //***    If RPdiv = %Int(PUdiv);
          //***       Leave;
          //***    Endif;
          //*** Reade (kkCo#:PUpolicy#:kkFnd) prp01;
          //*** Enddo;
          Setll (kkCo#:PUpolicy#:kkFnd) prp03;
          Reade (kkCo#:PUpolicy#:kkFnd) prp03;
          Dow Not(%Eof);
             If RPdiv = %Int(PUdiv);
                Leave;
             Endif;
          Reade (kkCo#:PUpolicy#:kkFnd) prp03;
          Enddo;
          //*** End   Chg *** 08/06/20 *************************************
          Chain (RPco#:RPfnd:RPfyr:RPemp#:RPdiv) pel01;
       When PUEmpLink <> *blanks and PUjrst <> *blanks;
          GetDivYear(returnCode);
       Endsl;
       //
       // Find out billing frequency
       //
       Chain (ELco#:%Editc(ELbscd:'X')) pbs01;
       //
       // Find out if Upload is supposed to generate A/R
       //
       intVendor = GetEmpVendor();
       If intVendor <> 0;
          GetVendorInfo(intVendor);
       Else;
          VPpyup = *blank;
        //*** beg add ***** 06/27/18 *******
          VPWrtColl = *Blank;
        //*** end add ***** 06/27/18 *******
       EndIf;
      /End-Free

     p GetPolicyInfo   e
      //---------------------------------------------------
      // getRecordCnt  - gets the number of records
      //---------------------------------------------------
     p GetRecordCnt    b
     d GetRecordCnt    pi             6s 0
     d rCount          s              6s 0 inz(0)

      /Free
       Exec Sql
       Declare Project4 Cursor For
       Select Count(*)
       From WXPUPA;
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
      *** beg add ***** 03/21/18 **************************
      //----------------------------------------------------
      // Get Report Totals - Gets payroll and manual premium
      //  totals from WXSRP
      //----------------------------------------------------
     p GetRptTotals    b

      /Free
       Exec Sql
       Select Sum(SRamtp), Sum(SRManPrm)
        Into :totPayroll, :totManPrem
        From WXSRP
        Where SRRepFlg = ' ';

       Exec Sql
       Select Sum(SRamtp), Sum(SRManPrm)
        Into :totPayReplaced, :totPrmReplaced
        From WXSRP
        Where SRRepFlg = 'Y';
      /End-Free

     p GetRptTotals    e
      //----------------------------------------------------
      // GetUpldCount - Find out how many records exist for
      //                a class/spec/ext for a report date
      //----------------------------------------------------
     p GetUpldCount    b
     d GetUpldCount    pi             3  0
     d  hasEmpInfo                     n
     d employeeCount   s              3  0
     d policyCount     s              3  0

      /Free

       //*** beg chg ***** 09/17/18 ************
       //Exec Sql
       //Select Count(*)
       // Into :employeeCount
       // From WXPUPA
       // Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //  and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //  and PUDiv = :PUDiv and PUFyr = :PUFyr
       //  and PULastName = :PULastName and PUFrstName = PUFrstName
       //  and PUMidInit = :PUMidInit and PUTitle = :PUTitle
       //  and PUTitle = :PUTitle and PUPosition = :PUPOsition
       //  and PUSS# = :PUSS# and PUClass = :PUClass
       //  and PUSpec = :PUSpec and PUExt = :PUext
       //  and PURpDt = :PURpDt and PULastName <> ' ';
       //
       //Exec Sql
       //Select Count(*)
       // Into :policyCount
       // From WXPUPA
       // Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //  and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //  and PUDiv = :PUDiv and PUFyr = :PUFyr
       //  and PUClass = :PUClass and PUSpec = :PUSpec
       //  and PUExt = :PUext and PURpDt = :PURpDt
       //  and PULastName = ' ';

       //*** Beg Chg ***** 05/24/22 **********************************
       //If hasEmpInfo = *On;
       //   Exec Sql
       //   Select Count(*)
       //    Into :employeeCount
       //    From WXPUPA
       //    Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //     and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //     and PUDiv = :PUDiv and PUFyr = :PUFyr
       //     and PULastName = :PULastName and PUFrstName = PUFrstName
       //     and PUMidInit = :PUMidInit and PUTitle = :PUTitle
       //     and PUTitle = :PUTitle and PUPosition = :PUPOsition
       //     and PUSS# = :PUSS# and PUClass = :PUClass
       //     and PUSpec = :PUSpec and PUExt = :PUext
       //     and PURpDt = :PURpDt and PULastName <> ' ';
       //Else;
       //   Exec Sql
       //   Select Count(*)
       //    Into :policyCount
       //    From WXPUPA
       //    Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //     and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //     and PUDiv = :PUDiv and PUFyr = :PUFyr
       //     and PUClass = :PUClass and PUSpec = :PUSpec
       //     and PUExt = :PUext and PURpDt = :PURpDt;
       //Endif;
       If hasEmpInfo = *On;
          Exec Sql
          Select Count(*)
           Into :employeeCount
           From WXPUPA
           Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
            and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
            and PUDiv = :PUDiv and PUFyr = :PUFyr
            and PULastName = :PULastName and PUFrstName = :PUFrstName
            and PUMidInit = :PUMidInit and PUTitle = :PUTitle
            and PUTitle = :PUTitle and PUPosition = :PUPOsition
            and PUSS# = :PUSS# and PUClass = :PUClass
            and PUSpec = :PUSpec and PUExt = :strExt
            and PURpDt = :PURpDt and PULastName <> ' ';
       Else;
          Exec Sql
          Select Count(*)
           Into :policyCount
           From WXPUPA
           Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
            and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
            and PUDiv = :PUDiv and PUFyr = :PUFyr
            and PUClass = :PUClass and PUSpec = :PUSpec
            and PUExt = :strExt and PURpDt = :PURpDt;
       Endif;
       //*** End Chg ***** 05/24/22 **********************************
       //*** end chg ***** 09/17/18 ************

       Return employeeCount + policyCount;
      /End-Free

     p GetUpldCount    e
      //------------------------------------------------------------------------*
      // GetUPldTotal - Returns accumulated totals for employee and policy
      //  class/spec/ext (not including replacement records)
      //------------------------------------------------------------------------*
     p GetUpldTotal    b
     d GetUpldTotal    pi            15s 2
     d  hasEmpInfo                     n
     d employeeTotal   s             15  2
     d policyTotal     s             15  2
     d uploadTotal     s             15  2

      /Free
       //*** beg chg ***** 09/17/18 ************
       //If hasEmpInfo;
       //   Exec Sql
       //   Select Sum(Dec(PUPayAmt,15,2))
       //    Into :employeeTotal
       //    From WXPUPA
       //    Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //     and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //     and PUDiv = :PUDiv and PUFyr = :PUFyr
       //     and PULastName = :PULastName and PUFrstName = PUFrstName
       //     and PUMidInit = :PUMidInit and PUTitle = :PUTitle
       //     and PUTitle = :PUTitle and PUPosition = :PUPOsition
       //     and PUSS# = :PUSS# and PUClass = :PUClass
       //     and PUSpec = :PUSpec and PUExt = :PUext
       //     and PUReplFlag = ' ' and PULastName <> ' ';
       //Endif;
       //
       //Exec Sql
       //Select Sum(Dec(PUPayAmt,15,2))
       // Into :policyTotal
       // From WXPUPA
       // Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //  and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //  and PUDiv = :PUDiv and PUFyr = :PUFyr
       //  and PULastName = ' ' and PUClass = :PUClass
       //  and PUSpec = :PUSpec and PUExt = :PUext
       //  and PUReplFlag = ' ';

       //*** Beg Chg ***** 05/24/22 **********************************
       //If hasEmpInfo;
       //   Exec Sql
       //   Select Sum(Dec(PUPayAmt,15,2))
       //    Into :employeeTotal
       //    From WXPUPA
       //    Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //     and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //     and PUDiv = :PUDiv and PUFyr = :PUFyr
       //     and PULastName = :PULastName and PUFrstName = PUFrstName
       //     and PUMidInit = :PUMidInit and PUTitle = :PUTitle
       //     and PUTitle = :PUTitle and PUPosition = :PUPOsition
       //     and PUSS# = :PUSS# and PUClass = :PUClass
       //     and PUSpec = :PUSpec and PUExt = :PUext
       //     and PUReplFlag = ' ' and PULastName <> ' ';
       //Else;
       //   Exec Sql
       //   Select Sum(Dec(PUPayAmt,15,2))
       //    Into :policyTotal
       //    From WXPUPA
       //    Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
       //     and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
       //     and PUDiv = :PUDiv and PUFyr = :PUFyr
       //     and PUClass = :PUClass and PUSpec = :PUSpec
       //     and PUExt = :PUext and PUReplFlag = ' ';
       //Endif;

       Reset employeeTotal;
       If hasEmpInfo;
          Exec Sql
          Select Sum(Dec(PUPayAmt,15,2))
           Into :employeeTotal
           From WXPUPA
           Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
            and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
            and PUDiv = :PUDiv and PUFyr = :PUFyr
            and PULastName = :PULastName and PUFrstName = :PUFrstName
            and PUMidInit = :PUMidInit and PUTitle = :PUTitle
            and PUTitle = :PUTitle and PUPosition = :PUPOsition
            and PUSS# = :PUSS# and PUClass = :PUClass
            and PUSpec = :PUSpec and PUExt = :strExt
            and PUReplFlag = ' ' and PULastName <> ' ';
       Else;
          Exec Sql
          Select Sum(Dec(PUPayAmt,15,2))
           Into :policyTotal
           From WXPUPA
           Where PUEmpLink = :PUEmpLink and PUJrSt = :PUJrSt
            and PUPolicy# = :PUPolicy# and PUEmp# = :PUEmp#
            and PUDiv = :PUDiv and PUFyr = :PUFyr
            and PUClass = :PUClass and PUSpec = :PUSpec
            and PUExt = :strExt and PUReplFlag = ' ';
       Endif;
       //*** End Chg ***** 05/24/22 **********************************
       //*** end chg ***** 09/17/18 ************

       Return employeeTotal + policyTotal;
      /End-Free

     p GetUpldTotal    e
      *** end add ***** 03/21/18 **************************
      //----------------------------------------------------------------
      // GetVendorInfo - retrieve vendor information
      //----------------------------------------------------------------
     p GetVendorInfo   b
     d GetVendorInfo   pi
     d  inVdr#                        5  0 const

      /Free
       Chain (kkCo#:inVdr#) pvp00;
      /End-Free

     p GetVendorInfo   e
      //----------------------------------------------------------------
      // InvalidChars - look for character data in numeric field
      //----------------------------------------------------------------
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
             aNumber = %Int(rrnWxpupa) + 1;
             message = 'Row ' + %Char(aNumber) + ' - ' +
              %trim(nameOfField) + ' has character data.';
             PrintError(0:*Off);
             Return *On;
          Endsl;
       EndMon;
       Return *Off;
      /End-Free

     p InvalidChars    e
      //---------------------------------------------------------------
      // printError - Prints errors to a report
      //            - hasError controls what routing prints
      //            - *In20 controls which output to print in the
      //               error report.
      //            - errorCount indicates whether an upload is
      //               successful or not
      //---------------------------------------------------------------
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

       If inOutq = *blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;

       xxque    = inOutq;
       userName = GetUsrName(q1user);
       UPusnm   = UserName;
       line = *all'_';
      //
      // Get the distribution information for the error report.
      //
       If not hasError;
          clear ORTITL;

          xxfil#   = '0';
          Chain (Q1pgm:xxfil#) por01;

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
       When PURecType = 'H';
          recordType  = 'Header';
       When PURecType = 'D';
          recordType  = 'Detail';
       When PURecType = 'T';
          recordType  = 'Trailer';
       Other;
          recordType  = 'Invalid';
       Endsl;

       intRowNumber = %Int(rrnWxpupa) + 1;
       Except det;

       //*** beg chg ***** 09/10/18 ****************
       //***If message <> message1 or errorNumber <> 0;
       //***   errorCount = errorCount + 1;
       //***Endif;
       Select;
       When message = message1;
       //*** beg chg ***** 04/11/23 ****************
       //*** When errorNumber = 29;
       When errorNumber = 29 and PUReplFlag = 'Y';
       //*** end chg ***** 04/11/23 ****************
       Other;
          errorCount = errorCount + 1;
       EndSl;
       //*** end chg ***** 09/10/18 ****************
      /End-Free

     p PrintError      e
      //---------------------------------------------------------------
      // PrintUpload - prints successful upload report
      //---------------------------------------------------------------
     p PrintUpload     b
     d PrintUpload     pi

      /Free
       xxque    = inOutq;
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
       Chain (Q1pgm:xxfil#) por01;

       If ORTITL = *blank;
          ORTITL1 = title1;
       Else;
          ORTITL1 = ORTITL;
       Endif;

       ORtitl1 = LeftJstify(ORtitl1);
       ORtitl1 = CenterFld(ORtitl1);
       //*** beg add ***** 03/21/18 ************
       //*** beg del**** 05/18/18 ************
       ////GetRptTotals();
       //*** end del ***** 05/18/18 ************
       //*** end add ***** 03/21/18 ************
       Except head1;
       Except det1;
      /End-Free

     p PrintUpload     e
      //===============================================================
      // processWXPUPA - processe import file
      //===============================================================
     p ProcessWXPUPA   b
     d ProcessWXPUPA   pi

      /Free
       // Process replace records first
       Setll 1 pup0a;
       Read pup0a;
       Dow Not %Eof;
          If PURecType = 'D' and PUReplFlag = 'Y';
             WriteFiles();
          Endif;
          Read pup0a;
       Enddo;

       Setll 1 pup0a;
       Read pup0a;
       Dow Not %Eof;
          If PURecType = 'D' and PUReplFlag <> 'Y';
             WriteFiles();
          Endif;
          Read pup0a;
       Enddo;

       //*** beg add ***** 09/10/18 **************
       Setll kkCo# psr00;
       Dou %Eof(WXSRL);
          Reade kkCo# psr00;
          If %Eof;
             Leave;
          Endif;
          AddPrvManPrm();
          Setgt (kkCo#:kkFnd:x_SRemp#:x_SRdiv:x_SRfyr:x_SRtodt:
           x_SRrpmy) psr00;
       Enddo;
       //*** end add ***** 09/10/18 **************
      /End-Free

     p ProcessWXPUPA   e
      //---------------------------------------------------------------
      // programStart - Initialization
      //---------------------------------------------------------------
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
       kkFnd  = %Int(inFnd);
       Chain(n) (kkco#:kkfnd) pfn01;
       RtvBrnd('@':#Brnd1:#brnd2);
       If inVdr <> *Zeros;
          GetVendorInfo(%Int(inVdr));
       Endif;
       //*** beg add ***** 03/21/18 *********
       // Print path name on error report
       In pathNameDta;
       filePath = lastPathName;
       //*** end add ***** 03/21/18 *********
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
      // Update Employer Payroll Detail
      //----------------------------------------------------------------
     p UpdateWMEPP     b

      /Free
       If PUReplFlag = 'Y';
         //*** beg chg ***** 03/21/18 ********
          EPsapr -= payAmtReplaced;
         //*** end chg ***** 03/21/18 ********
       Endif;
       EPsapr += %Dec(PUPayAmt:15:2);
       EPuser = Q1user;
       EPpgm  = Q1pgm;
       EPdt   = sysDateCymd;
       EPtime = sysTimeHms;
       Update pep07e;
      /End-Free

     p UpdateWMEPP     e
      //----------------------------------------------------------------
      // WriteFiles - writes out files
      //----------------------------------------------------------------
     p WriteFiles      b
     d WriteFiles      pi
     d  retCode        s              1  0
     d  epFound        s               n
     d  prFound        s               n

      /Free
       payAmtReplaced = 0;
       //*** beg chg ***** 05/18/18 *********
       ////GetPolicyInfo();
       ////rptDate = %date(PURpDt:*iso0);
       ////rpDt7   = %dec(rptDate:*cymd);
       ////rpDt5 = %Int(%Subst(%Editc(rpDt7:'X'):1:5));
       totPayInFile += %Dec(PUPayAmt:15:2);
       rptDate = %date(PURpDt:*iso0);
       rpDt7   = %dec(rptDate:*cymd);
       rpDt5 = %Int(%Subst(%Editc(rpDt7:'X'):1:5));
       GetPolicyInfo();
       //*** end chg ***** 05/18/18 *********
       //*** beg chg ***** 02/23/18 ***********************
       //Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
       // %Int(PUclass):PUspec:PUext:rpDt7) pep07e;
       //Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
       // %Int(PUclass):PUspec:PUext) pep07e;
       // Get Employer Payroll Detail record
       Select;
       When clientID = '150' and PUext = *Blanks;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:rpDt7) pep07;
          Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec) pep07;
          PUext = EPext;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext:rpDt7) pep07e;
          Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext) pep07e;
       Other;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext:rpDt7) pep07e;
          Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:
           %Int(PUclass):PUspec:PUext) pep07e;
       EndSl;
       //*** end chg ***** 02/23/18 ***********************
       epFound = Not(%Eof);
       // Get per capita flag
       Setgt  (ELco#:ELrtcd:%Int(PUclass):EPefdt) ppr01;
       Readpe (ELco#:ELrtcd:%Int(PUclass)) ppr01;
       prFound = Not(%Eof);
       If epFound and prFound;
          If PUSS# <> *Blanks;
             WriteWMEEP();
          Endif;
          WriteWASRP();
          UpdateWMEPP();
       Endif;
      /End-Free

     p WriteFiles      e
      //------------------------------------------------------
      // Writes/Updates WASRP
      //------------------------------------------------------
     p WriteWASRP      b
      *** beg add ***** 03/21/18 *************************
     d intManPrem      s             11s 0
      *** end add ***** 03/21/18 *************************
      *** beg add ***** 05/18/18 *************************
     d intPayAmDelta   s             15s 2
     d intPrmAmDelta   s             11s 0
      *** end add ***** 05/18/18 *************************
      /Free
       //*** beg add ***** 05/18/18 *************************
       intPayAmDelta = %Dec(PUPayAmt:15:2);
       //*** end add ***** 05/18/18 *************************
       If BSfreq = 'W';
          rpDt5 = 0;
       Else;
          rpDt7 = 0;
       Endif;
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
        %Int(PUclass):PUspec:PUext:rpDt7:rpDt5) psr14;
       If %Found;
          If PUss# <> *Blanks;
             If PUReplFlag = 'Y';
                SRamtp -= payAmtReplaced;
             Endif;
             SRamtp += intPayAmDelta;
          Else;
             If PUReplFlag = 'Y';
                payAmtReplaced = SRamtp;
                SRamtp =  intPayAmDelta;
             Else;
                SRamtp += intPayAmDelta;
             Endif;
          Endif;
          //*** beg chg ***** 03/21/18 **************
          //*** Select;
          //*** When PRpcf = 'P';
          //***    Eval(h) SRManPrem = SRamtp * EParat/100;
          //*** When PRpcf = 'C' and clientId <> '150';
          //***    Eval(h) SRManPrm = SRamtp * EParat;
          //*** When PRpcf = 'C' and clientId  = '150';
          //***    SRamtp = 0;
          //*** Endsl;
          Select;
          When PRpcf = 'P';
             Eval(h) intManPrem = SRamtp * EParat/100;
             Eval(h) prmAmtReplaced = payAmtReplaced * EParat/100;
           //*** beg add ***** 05/18/18 *************************
             Eval(h) intPrmAmDelta  = intPayAmDelta * EParat/100;
           //*** end add ***** 05/18/18 *************************
          When PRpcf = 'C' and clientId <> '150';
             Eval(h) intManPrem = SRamtp * EParat;
             Eval(h) prmAmtReplaced = payAmtReplaced * EParat;
           //*** beg add ***** 05/18/18 *************************
             Eval(h) intPrmAmDelta  = intPayAmDelta * EParat;
           //*** end add ***** 05/18/18 *************************
          When PRpcf = 'C' and clientId  = '150';
             SRamtp = 0;
             SRManPrm = 0;
           //*** beg add ***** 05/18/18 *************************
             intPayAmDelta = 0;
             intPrmAmDelta = 0;
          //*** end add ***** 05/18/18 *************************
          Endsl;
          SRManPrm = intManPrem;
          //*** end chg ***** 03/21/18 **************
          //*** beg add ***** 05/18/18 **************
          totPayroll += intPayAmDelta;
          totManPrem += intPrmAmDelta;
          //*** end add ***** 05/18/18 **************
          SRuser = Q1user;
          SRpgm  = Q1pgm;
          SRdt   = sysDateCymd;
          SRtime = sysTimeHms;
          Update psr14;
          //*** beg add ***** 03/21/18 **************
          WriteWXSRP(' ');
          If payAmtReplaced <> 0;
             SRamtp = payAmtReplaced;
             SRManPrm = prmAmtReplaced;
          //*** beg add ***** 05/18/18 **************
             totPayReplaced += SRamtp;
             totPrmReplaced += SRManPrm;
          //*** end add ***** 05/18/18 **************
             WriteWXSRP('Y');
          Endif;
          //*** end add ***** 03/21/18 **************
       Else;
          Clear psr14;
          SRco#  = ELco#;
          SRfnd  = ELfnd;
          SRfyr  = ELfyr;
          SRemp# = ELemp#;
          SRdiv  = ELdiv;
          SRclas = %Int(PUclass);
          SRspec = PUspec;
          SRext  = PUext;
          SRefdt = EPefdt;
          SRtodt = rpDt7;
          SRrpmy = rpDt5;
          SRamtp = %Dec(PUPayAmt:15:2);
          //*** beg chg ***** 03/21/18 **************
          //*** Select;
          //*** When PRpcf = 'P';
          //***    Eval(h) SRManPrm = SRamtp * EParat/100;
          //*** When PRpcf = 'C' and clientId <> '150';
          //***    Eval(h) SRManPrm = SRamtp * EParat;
          //*** When PRpcf = 'C' and clientId  = '150';
          //***    SRamtp = 0;
          //*** Endsl;
          Select;
          When PRpcf = 'P';
             Eval(h) intManPrem = SRamtp * EParat/100;
          When PRpcf = 'C' and clientId <> '150';
             Eval(h) intManPrem = SRamtp * EParat;
          When PRpcf = 'C' and clientId  = '150';
             SRamtp = 0;
             SRManPrm = 0;
          Endsl;
          SRManPrm = intManPrem;
          //*** end chg ***** 03/21/18 **************
          //*** beg add ***** 05/18/18 **************
          totPayroll += SRamtp;
          totManPrem += SRManPrm;
          //*** end add ***** 05/18/18 **************
          SRtyfl = 'S';
          SRuser = Q1user;
          SRpgm  = Q1pgm;
          SRdt   = sysDateCymd;
          SRtime = sysTimeHms;
          SRiusr = Q1user;
          SRipgm = Q1pgm;
          SRidt  = sysDateCymd;
          SRitm  = sysTimeHms;
          Write psr14;
       //*** beg chg ***** 03/21/18 **************
       //*** Endif;
       //*** WriteWXSRP();
          WriteWXSRP(' ');
       Endif;
       //*** end chg ***** 03/21/18 **************
      /End-Free

     p WriteWASRP      e
      //-----------------------------------------------------------------
      // Writes/Updates Employee Information
      //-----------------------------------------------------------------
     p WriteWMEEP      b

      /Free
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:PULastName:
        PUFrstName:PUMidInit:PUss#:PUtitle:PUposition:
        %Int(PUclass):PUspec:PUext:rpDt7) pee04;
       If %Found;
          If PUReplFlag = 'Y';
             payAmtReplaced = EEsapr;
             EEsapr = %Dec(PUPayAmt:15:2);
          Else;
             EEsapr = EEsapr + %Dec(PUPayAmt:15:2);
          Endif;
          EEuser = Q1user;
          EEpgm  = Q1pgm;
          EEdt   = sysDateCymd;
          EEtime = sysTimeHms;
          Update pee04;
       Else;
          EEco#  = ELco#;
          EEfnd  = ELfnd;
          EEemp# = ELemp#;
          EEdiv  = ELdiv;
          EEfyr  = ELfyr;
          EElsn  = PULastName;
          EEfsn  = PUFrstName;
          EEmid  = PUMidInit;
          EEss#  = PUss#;
          EEtitl = PUTitle;
          EEposn = PUPosition;
          EEclas = %Int(PUclass);
          EEspec = PUspec;
          EEext  = PUext;
          EEedt  = rpDt7;
          EEsapr = %Dec(PUPayAmt:15:2);
          If BSfreq = 'W';
             EEsdt = %dec(%char(rptDate-%days(7):*cymd0):7:0);
          Else;
             EEsdt = %dec(%char(rptDate-%months(1)+%days(1):*cymd0):7:0);
          Endif;
          If ELpfdt > EEsdt;
             EEsdt = ELpfdt;
          Endif;
          If PUCkDt <> *blanks;
             EEckdt = %dec(%char(%date(PUCkDt:*iso0):*cymd0):7:0);
          Endif;
          EEuser = Q1user;
          EEpgm  = Q1pgm;
          EEdt   = sysDateCymd;
          EEtime = sysTimeHms;
          EEiusr = Q1user;
          EEipgm = Q1pgm;
          EEidt  = sysDateCymd;
          EEitm  = sysTimeHms;
          Write pee04;
       Endif;
      /End-Free

     p WriteWMEEP      e
      //-----------------------------------------------------------------
      // Writes to the Qtemp S/R Workfile for the premium calculation
      //-----------------------------------------------------------------
     p WriteWXSRP      b
      *** beg add ***** 03/21/18 *************
     d WriteWXSRP      pi
     d kkRepFlag                      1    Const
      *** end add ***** 03/21/18 *************

     d xxDiv           s                   like(ELdiv)

      /Free
       If VPpyup <> 'Y';
          Return;
       EndIf;
       // Flags W8361C to call W8362R
       inReturn = 'S';
       // Inherit values from WASRP record
       //*** beg chg ***** 03/21/18 **************
       //*** Chain (ELarsm:ELco#:ELfnd:ELfyr:ELemp#:ELdiv) psr00;
       Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
        rpDt7:rpDt5:%Int(PUclass):PUspec:PUext:kkRepFlag) psr00;
       //*** end chg ***** 03/21/18 **************
       If %Found;
         // Only use payroll based classes for terrorism calc in W8362R
          If PRpcf <> 'P';
             SRamtp = 0;
          Endif;
          x_SRManPrm = SRManPrm;
          If kkRepFlag = 'Y';
             x_SRamtp   += SRamtp;
             x_SRManPrm += SRMAnPrm;
          Else;
             x_SRamtp    = SRamtp;
             x_SRManPrm  = SRManPrm;
          Endif;
          Update psr00;
       Else;
          x_SRco#  = SRco#;
          x_SRfnd  = SRfnd;
          x_SRemp# = SRemp#;
          x_SRdiv  = SRdiv;
          x_SRfyr  = SRfyr;
          x_SRefdt = SRefdt;
          x_SRrpmy = SRrpmy;
          x_SRtodt = SRtodt;
         // Only use payroll based classes for terrorism calc in W8362R
          If PRpcf = 'P';
             x_SRamtp = SRamtp;
          Endif;
          x_SRManPrm = SRManPrm;
         //*** beg add ***** 03/21/18 *********
          x_SRclas = SRclas;
          x_SRspec = SRspec;
          x_SRext  = SRext;
          x_SRRepFlg = kkRepFlag;
         //*** end add ***** 03/21/18 *********
          x_SRarsm = ELarsm;
          x_SRautr = VPautr;
        //*** beg add ***** 06/27/18 **********
          x_SRWrtColl = VPWrtColl;
        //*** beg add ***** 06/27/18 **********
          x_SRtranPay = VPtranPay;
          x_SRpbcd = VPpbcd;
          x_SRppcd = VPppcd;
          x_SRvdr# = VPvdr#;
          x_SRGenPrm = VPGenPrm;
          If x_SRtodt <> 0;
             x_SRfreq = 'W';
          Else;
             x_SRfreq = 'M';
          Endif;
          x_SRuser = SRuser;
          x_SRpgm  = SRpgm;
          x_SRdt   = SRdt;
          x_SRtime = SRtime;
          Write psr00;
       Endif;
      /End-Free

     p WriteWXSRP      e


** Error messages
Numeric fields have character information (bad data).                          1
Record type is invalid. It must be a 'H','D', or 'T'.                          2
Policy can't be located with information provided.                             3
Employer/Div/Policy Year is invalid.                                           4
Policy#/Div is invalid.                                                        5
Report date is invalid.                                                        6
A policy year can't be found for the link/state/report date.                   7
Multiple policy years exist for link/state/report date.                        8
Payroll/Spec/Ext does not exist on policy as of the report date.               9
Check date is invalid.                                                        10
Payroll amount can't be negative if replacing.                                11
Payroll/Spec/Ext does not exist on policy as of the report date.              12
Check date is not within # of days for check issuance.                        13
Report date is invalid.                                                       14
< Not Used >                                                                  15
Employee information is reqired.                                              16
Policy vendor doesn't match vendor passed in.                                 17
Replacment flag must 'Y' or blank.                                            18
Policy is not flagged for self reporting.                                     19
Report date must be last day of month.                                        20
Report date is not on correct day of week.                                    21
Report date not in policy date range.                                         22
Employee info is provided but is not complete.                                23
Negative entry/s exceed positive entries for this policy/class.               24     chg 03/21/18
FEIN doesn't match policy information.                                        25
Policy link/state not found for specified vendor.                             26
Negative entry/s exceed positive entries for this employee/class.             27     chg 03/21/18
No other entries can exist for a payoll if replacing it.                      28     chg 03/21/18
Info Only: This payroll class has been previously reported.                   29     add 09/10/18
Self-report not allowed after cancellation date.                              30     add 05/18/23
