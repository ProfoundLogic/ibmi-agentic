     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      *============================================================================================
     H/TITLE W8421R - UPLOAD EMPLOYER PAYROLL AUDIT INFORMATION
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H bnddir('WTBNDDIR')
      *============================================================================================
      * Description:  This program will edit the upload of audit payroll and update/write
      *                WMEPP and WABLP
      * Called From: W8420C - Audit Payroll Upload CL
      * Calls To:  None
      *============================================================================================
      * Revisions:
      *
      * 07/15/25 C:333 Support: 118424 (SunZ)
      *                New screen design for Audited Payroll Entry
      *                (V25R0 ebf'd to V24R1).
      *
      * 02/17/25 C:340 Project: WC3001
      *                Process Group Field on each row.
      *
      * 12/14/23 C:342 Support: 113258
      *                Populate effective date in WABLP file
      *
      * 10/27/22 C:291 Project: 148WDX
      *                Zero Audit any un-audited payroll records on a policy
      *                after it has been processed through upload, For AmWins.
      *
      * 06/11/21 C:136 Support: 95945
      *                Don't write Audit Mass Billing record unless all payrolls
      *                for the policy are audited.
      *
      * 06/19/20 C:136 Support: 89304
      *                Fix info messages. They aren't displaying the correct
      *                policy info.
      *
      * 12/17/19 C:180 Project:150WEH
      *                Add SR payroll audit.
      *
      * 08/08/19 C:136 Support: 81839
      *                The badData indicator needs to be set and
      *                not cleared with every call of SetBadData
      *
      * 04/18/18 C:136 Project: WC2479
      *               -Convert to CompSuite - rewrite (not commented)
      *               -Add flags to prompt to control zeroing out missing payroll on import and
      *                adding of new classes. Show these on exception report.
      *============================================================================================
     fsmorl     if   e           k disk
     fwabll1    if a e           k disk    rename(pbl01:pbl0l1)
      *** Begin Change *** 12/17/19 ********************************
     f**wdell1    if   e           k disk    rename(pel01:pel01)
     fwdell1    uf   e           k disk    rename(pel01:pel01)
      *** Begin Change *** 12/17/19 ********************************
     fwdell2    if   e           k disk    rename(pel01:pel02)
     fwmepl7e   uf a e           k disk    rename(pep01:pep07e)
     fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
     fwmcmlz    if   e           k disk    rename(pcm01:pcm0z)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmeml18   if   e           k disk    rename(pem01:pem18)
     fwmprl1    if   e           k disk    rename(ppr01:ppr01)
     fwmrpl3    if   e           k disk    rename(prp01:prp03)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwmpxl1    if   e           k disk    rename(ppx01:ppx01)
     fwxadl     if   e             disk    rename(pad01:pad00)
     fwxadp     uf a e             disk    rename(pad01:pad0p)
     f                                     infds(dbWxadp)
      *** Beg Add ***** 10/27/22 ******************************************
     fwxadl1    if   e           k disk
      *** End Add ***** 10/27/22 ******************************************
     fqsysprt   o    f  160        printer oflind(*inof)
     fqsysprt1  o    f  160        printer oflind(*inoa)
      *=====================================================
      * Entry
      *=====================================================
     dw8421r           pr
     d eFnd                           3
     d eQue                          10
     d eZeroPayroll                   1
     d eNewClass                      1
     d eUpld                          1
     d eRptDt                         7
     dw8421r           pi
     d eFnd                           3
     d eQue                          10
     d eZeroPayroll                   1
     d eNewClass                      1
     d eUpld                          1
     d eRptDt                         7
      //====================================================
      // Constants
      //====================================================
     d digits          c                   '0123456789'
     d title           c                   'Audit Payroll Upload Error Report'
     d title1          c                   'Audit Payroll Upload Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** Upload file is empty. ***'
     d message3        c                   '*** Blank record ***'
     d message4        c                   '*** Header/trailer is missing ***'
     d message5        c                   '*** Duplicate records ***'
     d message6        c                   '*** Multiple headers ***'
     d message7        c                   '*** Multiple trailers ***'
     d message8        c                   '*** No header record ***'
     d message9        c                   '*** No trailer record ***'
     d message10       c                   '*** No match on record count ***'
      *=====================================================
      * Data structures and arrays
      *=====================================================
     d err             s             60    dim(25) ctdata perrcd(1)
     d upld            s             60    dim(2)  ctdata perrcd(1)
     d psds          esds                  extname(szq1p)
     d dbWxadp         ds
     d  rrnWxadp             397    400b 0
      *======================================================
      * Stand alone fields
      *======================================================
     d @@que           s                   like(ORoutq)
     d xxque           s                   like(ORoutq)
     d xxfil#          s                   like(ORfil#)
     d line            s            157
     d upusnm          s             50
     d name            s             40
     d x               s              2  0
     d y               s              2  0
     d z               s              2  0
     d elRRN           s              9  0
     d intRow#         s              7  0 inz(*zero)
     d intEfDt7        s              7  0
     d errorFlag       s              1n   inz(*off)
     d xxCo#           s                   like(ELco#)
     d svFnd           s                   like(ELfnd)
     d svEmp#          s                   like(ELemp#)
     d svEmplink       s                   like(ADEmpLink)
     d svDiv           s                   like(ELdiv)
     d svFyr           s                   like(ELfyr)
     d svJrst          s                   like(ADjrst)
     d svCap#          s                   like(RPcap#)
     d intADrCnt       s             11  0 inz(*zero)
     d chrAdDiv        s              5    inz
     d chrAdEfDt       s             10    inz
     d chrAdEmp#       s              9    inz
     d chrAdFyr        s              3    inz
     d chrADjrSt       s                   like(ADjrst)
     d chrADCap#       s                   like(ADCap#)
     d chrADclas       s                   like(ADclas)
     d chrADspec       s                   like(ADspec)
     d chrADext        s                   like(ADext)
     d chrEmpLink      s                   like(ADEmpLink)
      *** Beg Add ***** 07/15/25 *************************
     d chrAdFnd        s                   like(ADFnd)
      *** End Add ***** 07/15/25 *************************
     d intNewClasCnt   s             11  0 inz(*zero)
     d intMisClasCnt   s             11  0 inz(*zero)
     d intZeroClmCnt   s             11  0 inz(*zero)
     d totPayAmt       s             +2    like(EPaapr)
     d is1stPrt        s               n
     d kkCo#           s                   like(ELco#)
     d kkFnd           s                   like(ELfnd)
     d kkClas          s                   like(EPclas)
     d xxEfdt          s                   like(ELpfdt)
     d x1Efdt          s                   like(ELPfdt)
     d errFlg          s               n
     d prtOkMsg        s               n   inz('1')
     d #brnd1          s             80    inz
     d #brnd2          s             80    inz
     d pClient         s              3    inz(*blanks)
     d recType         s              1
     d badData         s              1n   inz(*off)
     d companyNm       s             60
     d empCount        s              6  0 inz(*zero)
     d errCount        s              6  0 inz(*zero)
     d infCount        s              6  0 inz(*zero)
     d message         s             60
     d userName        s             50
     d sys_date        s               d   datfmt(*usa)
     d sysDateCymd     s              7s 0
     d sys_time        s               t   timfmt(*hms:)
     d sysTimeHms      s              6s 0
     d reportDate      s              7s 0
     d company         s                   like(ELCO#)
     d errMessage      s             60
     d infMessage      s             60
     d isRequired      s               n   inz(*on)
     d isNotRequired   s               n   inz(*off)
     d orTitl1         s                   like(ORTITL)
     d                                     inz('Successful Upload Report')
     d orTitl2         s                   like(ORTITL)
     d strRecType      s              7
      *** Beg Chg *** 02/17/25 *** WC3001 *************************************
     d*strPolID        s             40
     d strPolID        s             30
      *** End Chg *** 02/17/25 *** WC3001 *************************************
     d filePath        s             80
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
     d strGroup        s              3
     d Idx             s              3s 0 Inz(0)
     d Pos             s              3s 0 Inz(0)
      *** End Add *** 02/17/25 *** WC3001 *************************************
      *** Beg Add ***** 10/27/22 ******************************
     d clientID        s              3    Inz(' ')
      *** End Add ***** 10/27/22 ******************************
     d
     d pathNameDta     ds                  dtaara('AUDPAYPATH')
     d  lastPathName           1    256
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
     d arrUplDetail    DS                  Qualified  Dim(100)
     d  @Group                        3A
     d  @TotPayroll                        Like(intADrCnt)
     d  @NewClasCnt                        Like(intNewClasCnt)
     d  @MisClasCnt                        Like(intMisClasCnt)
     d  @ZeroClmCnt                        Like(intZeroClmCnt)
     d  @totPayAmt                         Like(totPayAmt)
      *** End Add *** 02/17/25 *** WC3001 *************************************
     d*=====================================================
     d*  procedure prototypes
     d*=====================================================
     d/copy sprRtvBrnd
     d/copy sprCompany
     d/copy sprDate
     d/copy wprUsrPrms
     d/copy sprString
     d/copy wprEmplr
     d BadDataCheck    pr
     d DetailEdit      pr
     d ErrorCheckFile  pr
     d GetAgent        pr                  extpgm('WA002R')
     d   ##co#                        3  0
     d   ##fnd                        3  0
     d   ##emp#                       9  0
     d   ##div                        5  0
     d   ##fyr                        3  0
     d   ##agt#                       5  0
     d   ##anam                      60
     d   ##prod                       5  0
     d   ##pnam                      60
     d GetDivYear      pr              n
     d  returnCode                    1  0
     d InvalidChars    pr              n
     d  valueToCheck                 11a   const
     d  nameOfField                  25a   const
     d  isRequired                     n   const
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  isError                        n   value
     d PrintUplRpt     pr
     d ProcessWXADP    pr
     d ProgramStart    pr
     d RptMissingPay   pr
     d UpdWrtWMEPP     pr
     d ValidNewClass   pr              n
     d WriteWABLP      pr
      *** Beg Add ***** 10/27/22 ****************************
     d zeroAuditPayroll...
     d                 pr
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d   clientID                     3
      *** End Add ***** 10/27/22 ****************************
      //====================================================
      // Main
      //====================================================
      /Free
       ProgramStart();
       ErrorCheckFile();
       If errCount = *zero;
          infMessage = message1;
          If infCount = 0;
             PrintError(0:*off);   // No errors to report...
          Endif;
          ProcessWXADP();
          PrintUplRpt();
          eUpld = 'Y';
          eRptDt = %Editc(sysDateCymd:'X');
       Else;
          eUpld = 'N';
       Endif;

       xxfil# = '0';
       Chain (Q1pgm:xxFil#) por01;
       If %found;
          Except route;
       Endif;

       If errCount = 0;
          xxfil# = '1';
          Chain (Q1pgm:xxFil#) por01;
          If %found;
             Except route1;
          Endif;
       Endif;
       *Inlr = *On;
      /End-Free
      *================================================
      * Output files
      *================================================
     o/copy scopyrout

     oqsysprt   e            route          2
     o                                        +  06 'Run Date:'
     o                       sys_Date         +   1
     o                       sys_Time         +   2
     o          e            route          2
     o                                        +  06 'Write new payrolls:'
     o                       eNewClass        +   2
     o          e            route          2
     o                                        +  06 'Zero missing payrolls:'
     o                       eZeroPayroll     +   2
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
     O***       e            route          1 58
     o***                    #brnd1
     o***       e            route          1
     o***                    #brnd2
      *** End Del *** 02/17/25 *** WC3001 *************************************
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sys_Time
     o                       companyNm          110
     o                       sys_Date           160
     o          e            head           2
     o                       q1pgm
     o                       ORtitl             110
     o                                          154 'PAGE:'
     o                       page          z    160
     o          e            head           1
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
     o***                                           'Group  . . . :'
     o***                    kkFnd         p   +002
     o***                    FNname            +002
      *** End Del *** 02/17/25 *** WC3001 *************************************
     o          e            head           2
     o                                              'File Uploaded:'
     o                       filePath          +002
     o          e            errHead        0
     o                                            8 'Rec type'
     o                                         +007 'Row'
     o                                         +002 'Policy Identifier'
     o                                         +022 'State'
     o                                         +002 'Class'
     o                                         +003 'Spec'
     o                                         +002 'Ext'
     o                                         +006 'Eff Date'
     o                                         +002 'Error Reason'
     o          e            errHead        2
     o                       line
     o          e            errDetail      1
     o                       strRecType        +002
     o                       intRow#       3   +002
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
     o                       strGroup          +002
      *** End Add *** 02/17/25 *** WC3001 *************************************
     o                       strPolID          +002
     o                       chrADJrSt         +002
     o                       chrADClas         +003
     o                       chrADspec         +002
     o                       chrADext          +005
     o                       chrADefdt         +002
     o                       errMessage        +002
     o          e            infDetail      1
     o                       strRecType        +002
     o                       intRow#       3   +002
     o                       infMessage        +046

     o/copy scopyrout1
     oqsysprt1  e            route1         2
     o                                        +  06 'Run Date:'
     o                       sys_Date         +   1
     o                       sys_Time         +   2
     o          e            route1         2
     o                                        +  06 'Write new payrolls:'
     o                       eNewClass        +   2
     o          e            route1         2
     o                                        +  06 'Zero missing payrolls:'
     o                       eZeroPayroll     +   2
     o          e            route1         1 58
     o                       #brnd1
     o          e            route1         1
     o                       #brnd2
    o*
    o* Report Heading
    o*
     o          e            uplHead        1 06
     o                       sys_Time
     o                       companyNm          110
     o                       sys_Date           160
     o          e            uplHead        2
     o                       q1pgm
     o                       ORtitl1            110
     o                                          154 'PAGE:'
     o                       page          z    160
      *** Beg Chg *** 02/17/25 *** WC3001 *************************************
     o***       e            uplHead        1
     o***                                           'Group  . . . :'
     o***                    kkFnd         p   +002
     o***                    FNname            +002
     o***       e            uplHead        1
     o***                                           'File Uploaded:'
     o***                    filePath          +002
     o***       e            uplHead        2
     o***                    line              +002
     o***       e            uplDetail      2
     o***                                      +002 'Number Of Payrolls Loaded:'
     o***                    intADrCnt     3   +002
     o***       e            uplDetail      2
     o***                                      +002 'New Class Count  . . . . :'
     o***                    intNewClasCnt 3   +002
     o***       e            uplDetail      2
     o***                                      +002 'Missing Class Count  . . :'
     o***                    intMisClasCnt 3   +002
     o***       e            uplDetail      2
     o***                                      +002 'Zero W/Claims Count  . . :'
     o***                    intZeroClmCnt 3   +002
     o***       e            uplDetail      2
     o***                                      +002 'Total Payroll Amount . . :'
     o***                    totPayAmt         +002 ' ,   ,   ,   , $0.  '
      *
      * Convert Rows to Columns
      *
     o          e            uplHead        1
     o                                              'File Uploaded:'
     o                       filePath          +002
     o          e            uplHead        2
     o                       line
      *
     o          e            uplHead        1
     o                                              'Group'
     o                                         +002 'Number Of Payrolls Loaded'
     o                                         +002 'New Class Count'
     o                                         +002 'Missing Class Count'
     o                                         +002 'Zero W/Claims Count'
     o                                         +002 'Total Payroll Amount'
     o          e            uplDetail      2
     o                       kkFnd         p      5
     o                       intADrCnt     3     32
     o                       intNewClasCnt 3     49
     o                       intMisClasCnt 3     70
     o                       intZeroClmCnt 3     91
     o                       totPayAmt          113 ' ,   ,   ,   , $0.  '
      *
      *** End Chg *** 02/17/25 *** WC3001 *************************************
      //------------------------------------------------------------------------*
      // badDataCheck - checks for bad data
      //------------------------------------------------------------------------*
     p BadDataCheck    b
     d BadDataCheck    pi

      /Free
       Select;
       When ADrect = 'H';
          SetBadData(InvalidChars(ADrCnt:'Rec Count':isRequired));
          If badData = *On;
             Return;
          Endif;
       When ADrect = 'D';
          Select;
          When ADemp# <> *Blanks;
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
             SetBadData(InvalidChars(ADfnd :'Group'          :isRequired));
      *** End Add *** 02/17/25 *** WC3001 *************************************
             SetBadData(InvalidChars(ADemp#:'Employer Number':isRequired));
             SetBadData(InvalidChars(ADdiv :'Division Number':isRequired));
             SetBadData(InvalidChars(ADfyr :'Policy Year'    :isRequired));
          When ADcap# <> *Blanks;
             SetBadData(InvalidChars(ADemp#:'Employer Number':isNotRequired));
             SetBadData(InvalidChars(ADdiv :'Division Number':isRequired));
             SetBadData(InvalidChars(ADfyr :'Policy Year'    :isNotRequired));
          When ADEmpLink <> *Blanks;
             SetBadData(InvalidChars(ADemp#:'Employer Number':isNotRequired));
             SetBadData(InvalidChars(ADdiv :'Division Number':isNotRequired));
             SetBadData(InvalidChars(ADfyr :'Policy Year'    :isNotRequired));
          Endsl;
          SetBadData(InvalidChars(ADclas:'Class Code'  :isRequired));
          SetBadData(InvalidChars(ADamt :'Audit Amount':isRequired));
          SetBadData(InvalidChars(ADefdt:'Eff Date'    :isRequired));
          SetBadData(InvalidChars(ADcxdt:'Cx Date'     :isNotRequired));
          SetBadData(InvalidChars(ADacnt:'Emp Count'   :isNotRequired));
       When ADrect = 'T';
       Other;
          badData = *on;
          PrintError(2:*On);
          Return;
       Endsl;
      /End-Free

     p BadDataCheck    e
      //*** Begin Add *** 12/17/19 **************************************
      //----------------------------------------------------------------
      // Clear SR Audit Flag
      //----------------------------------------------------------------
     p ClearAudSrFlag  b
      /Free
       Chain(n) (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
       If %Found(Wdell1) and ELaudsr  ='Y';
          Chain (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          ELaudsr = *blanks;
          ELuser = Q1user;
          ELpgm  = Q1pgm;
          ELdt   = sysDateCymd;
          ELtime = sysTimeHms;
          Update Pel01;
       Endif;
      /End-Free
     p ClearAudSrFlag  e
      //*** End   Add *** 12/17/19 **************************************
      //------------------------------------------------------------------------*
      // detailEdit - edit detail record
      //------------------------------------------------------------------------*
     p DetailEdit      b
     d DetailEdit      pi
     d intCxDt7        s              7  0
     d returnCode      s              1  0

      /Free
       chrEmpLink = ADEmpLink;
       chrADCap# = ADCap#;
       chrAdEmp# = ADemp#;
       chrAdDiv  = ADdiv;
       chrAdFyr  = ADfyr;
       chrADjrSt = ADjrSt;
       chrADclas = ADclas;
       chrADspec = ADspec;
       chrADext  = ADext;
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
       //  Group/Fund should be populated.
       If ADFnd = ' ';
          PrintError(24:*On);
       //*** Beg Add ***** 07/15/25 ***************************
       Else;
          chrAdFnd = ADfnd;
       //*** End Add ***** 07/15/25 ***************************
       Endif;
      *** End Add *** 02/17/25 *** WC3001 *************************************
       //
       //  Audit amount less than zero
       //
       If %Int(ADamt) < 0;
          PrintError(8:*On);
       Endif;
       //
       //  Number of employees less than zero
       //
       If %Int(ADacnt) < 0;
          PrintError(9:*On);
       Endif;
       //
       //  Cx Date must be valid
       //
       If ADcxdt <> *Blanks;
          Monitor;
             intCxDt7 = %Dec(%Char(%Date(ADcxdt:*Iso0):*Cymd0):7:0);
          On-Error;
             PrintError(13:*on);
          EndMon;
       Endif;
       //
       //  Edit effective date.
       //
       Monitor;
          intEfDt7 = %Dec(%Char(%Date(ADefdt:*Iso0):*Cymd0):7:0);
       On-Error;
          PrintError(3:*on);
          Return;
       EndMon;
       chrADefDt = %Char(%Date(ADefDt:*Iso0):*Usa/);
       //
       //  Either link code/state or policy#/div or emp#/div/fyr is required
       //  Validate and use emp#/div/fyr first if populated.
       //  Validate carrier/div second if populated.
       //  Validate link code/rating state last.
       //
       Select;
       When ADemp# <> *blanks and ADdiv <> *blanks and ADfyr <> *blanks;
       //*** Begin Change *** 12/17/19 ********************************
          //Chain (kkCo#:kkFnd:%Int(ADfyr):%Int(ADemp#):%Int(ADdiv)) pel01;
          Chain(n) (kkCo#:kkFnd:%Int(ADfyr):%Int(ADemp#):%Int(ADdiv)) pel01;
       //*** End   Change *** 12/17/19 ********************************
          If Not(%Found);
             PrintError(2:*On);
             Return;
          Endif;
       When ADcap# <> *blanks and ADdiv <> *blanks;
          Setll (kkCo#:ADcap#:kkFnd) prp03;
          Reade (kkCo#:ADcap#:kkFnd) prp03;
          Dow Not(%Eof);
             If RPdiv = %Int(ADdiv);
                Leave;
             Endif;
          Reade (kkCo#:ADcap#:kkFnd) prp03;
          Enddo;
          If %Eof;
             PrintError(12:*On);
             Return;
          Else;
          //*** Begin Change *** 12/17/19 ********************************
             //Chain (RPco#:RPfnd:RPfyr:RPemp#:RPdiv) pel01;
             Chain(n) (RPco#:RPfnd:RPfyr:RPemp#:RPdiv) pel01;
          //*** End   Change *** 12/17/19 ********************************
          Endif;
       When ADEmpLink <> *blanks and ADjrst <> *blanks;
          If GetDivYear(returnCode) = *Off;
             Select;
             When returnCode = 1;
                PrintError(17:*On);  //No link/state exists
             Return;
             When returnCode = 2;
                PrintError(18:*On);  //Multiple policies exist
             Return;
             Endsl;
          Endif;
       Other;
          PrintError(1:*On);
          Return;
       Endsl;
       //
       // Can't upload if audit bill has been run
       //
       If ELfaf = 'A';
          PrintError(11:*On);
       Endif;
       //
       // Effective date must be in policy date range.
       //
       If intEfDt7 < ELpfdt or intEfDt7 >= ELptdt;
          PrintError(5:*On);
       Endif;
       //
       // Don't allow audit before expiration date unless cancelled
       //  prior to system date.
       //
       Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
       If sysDateCymd < ELptdt and (EMcad1 = 0 Or sysDateCymd < EMcad1);
          PrintError(16:*On);
       Endif;
       //
       // Only allow new classes if flagged to do so
       //
       Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:%Int(ADclas):
        ADspec:ADext:intEfDt7) pep07e;
       Select;
       When Not(%Found) and eNewClass <> 'Y';
          PrintError(10:*On);
       When Not(%Found);
          ValidNewClass();
       Endsl;
       //
       //  Info Msg: Claims exist for class with zero exposure?
       //
       If %Int(ADamt) = 0;
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:%Int(ADclas)) pcm0z;
          If %Found;
             PrintError(21:*Off);
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
          Pos = %LookUp(ADFnd:arrUplDetail(*).@Group:1:%Elem(arrUplDetail));
          If Pos > 0;
             arrUplDetail(Pos).@ZeroClmCnt += 1;
          EndIf;

      *** End Add *** 02/17/25 *** WC3001 *************************************
          Endif;
       Endif;
       //
       // Update WXADP with emp/div/fyr
       //
       ADemp# = %Editc(ELemp#:'X');
       ADdiv  = %Editc(ELdiv :'X');
       ADfyr  = %Editc(ELfyr :'X');
       Update pad0p;
      /End-Free

     p DetailEdit      e
      //------------------------------------------------------------------------*
      // errorCheckFile - Reads thru WXADP and looks for errors
      //------------------------------------------------------------------------*
     p ErrorCheckFile  b
     d ErrorCheckFile  pi

     d headerCount     s              6  0 inz(*zero)
     d headerFirst     s              1n   inz(*off)
     d rcdCount        s             11  0 inz(*zero)
     d trailerCount    s              6  0 inz(*zero)
     d trailerLast     s              1n   inz(*off)

      /Free
       // Check for empty file
       Read pad0p;
       Select;
       When %Eof;
          infMessage = message2;
          PrintError(0:*On);
          Return;
       When ADrect = 'H';
          headerFirst = *On;
          headerCount += 1;
          BadDataCheck();
          If not badData;
             intADrcnt = %Int(ADrcnt);
          Endif;
       Endsl;
       rcdCount += 1;
       Dou %Eof(WXADP);
          Read pad0p;
          If %Eof;
             If ADrect = 'T';
                trailerLast = *On;
             Endif;
             Leave;
          Endif;
          rcdCount += 1;
          Select;
          When ADrect = 'H';
             BadDataCheck();
             headerCount  += 1;
             If headerCount > 1;
                infMessage = message6;
                PrintError(0:*On);
             Endif;
          When ADrect = 'T';
             trailerCount += 1;
             If trailerCount > 1;
                infMessage = message7;
                PrintError(0:*On);
             Endif;
          Other;
             BadDataCheck();
             If Not badData;
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
             KKFnd = %Int(ADfnd);
             Pos = %LookUp(ADfnd:arrUplDetail(*).@Group:1:%Elem(arrUplDetail));
             If Pos > 0;
                arrUplDetail(Pos).@TotPayroll += 1;
             Else;
                Idx = Idx + 1;
                arrUplDetail(Idx).@Group      = ADfnd;
                arrUplDetail(Idx).@TotPayroll =  1;
                arrUplDetail(Idx).@NewClasCnt =  0;
                arrUplDetail(Idx).@MisClasCnt =  0;
                arrUplDetail(Idx).@ZeroClmCnt =  0;
                arrUplDetail(Idx).@totPayAmt  =  0;
             EndIf;
      *** End Add *** 02/17/25 *** WC3001 *************************************
                DetailEdit();
             Endif;
          Endsl;
       Enddo;
       If headerCount = 0;
          infMessage = message8;
          PrintError(0:*On);
       Endif;
       If trailerCount = 0;
          infMessage = message9;
          PrintError(0:*On);
       Endif;
       If headerFirst = *off or trailerLast = *off;
          infMessage = message4;
          PrintError(0:*On);
       Endif;
       If intADrcnt <> rcdCount;
          infMessage = message10;
          PrintError(0:*on);
       Endif;
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

      /Free
       Setll (kkCo#:kkFnd:ADEmpLink) pem18;
       Dou %Eof(WMEML18);
          Reade (kkCo#:kkFnd:ADEmpLink) pem18;
          If %Eof;
             Leave;
          Endif;
          Setgt (EMco#:EMfnd:EMemp#:Emdiv:intEfDt7) pel02;
          Readpe (EMco#:EMfnd:EMemp#:Emdiv) pel02;
          Select;
          When %Eof;
             Iter;
          When ELfaf = 'A';
             Iter;
          When sysDateCymd < ELptdt and (EMcad1 = 0 or sysDateCymd < EMcad1);
             Iter;
          Other;
          // Check if division matches rating state. Save off
          //  emp#/div/year if it does.  Prevent upload if another
          //  eligible emp/div exists for link/state
             Chain (kkCo#:ELrtcd) prs00;
             If %Found and RSjrst = ADjrst and not(isFound);
                isFound = *On;
                svEmp# = EMemp#;
                svDiv  = EMdiv;
                svFyr  = ELfyr;
             Endif;
             If RSjrst = ADjrst and (svEmp# <> EMemp# or svDiv <> EMdiv);
                returnCode = 2;  // Multiple employers active for jrst
                Return *Off;
             Endif;
          Endsl;
       Enddo;

       If isFound;
          //*** Begin Change *** 12/17/19 ********************************
          //Chain (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          Chain(n) (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          //*** End   Change *** 12/17/19 ********************************
          Return *On;
       Else;
          returnCode = 1;
          Return *Off;
       Endif;
      /End-Free

     p GetDivYear      e
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
             aNumber = %Int(rrnWxadp) + 1;
             infMessage = 'Row ' + %Char(aNumber) + ' - ' +
              %trim(nameOfField) + ' has character data.';
             PrintError(0:*On);
             Return *On;
          Endsl;
       EndMon;
       Return *Off;
      /End-Free

     p InvalidChars    e
      //---------------------------------------------------*
      // printError - Prints errors to a report
      //---------------------------------------------------*
     p PrintError      b
     d PrintError      pi
     d  errorNumber                   3s 0 value
     d  isError                        n   value

      /Free
       If eQue = *blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;
       xxque    = eQue;
       userName = GetUsrName(Q1user);
       UPusnm   = userName;
       line = *all'_';
       Clear ORTITL;
       xxFil#   = '0';
       Chain (Q1pgm:xxFil#) por01;
       If ORtitl = *blank;
          ORtitl = title;
       Endif;
       ORtitl = centerFld(ORtitl);
       Select;
       When ADrect = 'H';
          strRecType = 'Header';
       When ADrect = 'D';
          strRecType = 'Detail';
       When ADrect = 'T';
          strRecType = 'Trailer';
       Other;
          strRecType = 'Invalid';
       Endsl;
       If Not is1stPrt;
          is1stPrt = *on;
          Except head;
          Except errHead;
       Endif;
       If *inof = *on;
          *inof = *off;
          Except head;
          Except errHead;
       Endif;
       If (infMessage <> *Blanks and Not(isError))
          Or errorNumber = 19 Or errorNumber = 20
          Or errorNumber = 22 Or errorNumber = 23;
          strRecType = 'N/A';
          intRow# = 0;
       Else;
          intRow# = %Int(rrnWxadp) + 1;
       Endif;
       Select;
       When errorNumber <> 0;
          Select;
          When chrEmpLink <> *Blanks;
             strPolID = 'Link/St: ' + %Trim(chrEmpLink) + '/' + chrADjrSt;
          When chrADcap# <> *Blanks;
             strPolID = 'Pol#/Div: ' + %Trim(chrADcap#) + '/' + %Trim(chrADdiv);
          When chrADemp# <> *Blanks;
             strPolID = 'Emp/Div/Yr: ' + %Trim(chrADemp#) + '/' +
              %Trim(chrADdiv) + '/' + %Trim(chrADfyr);
          Endsl;
          errMessage = err(errorNumber);
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
          //*** Beg Chg *** 07/15/25 ************************
          //***strGroup = %Editc(KKFnd:'X');
          strGroup = chrAdFnd;
          //*** End Chg *** 07/15/25 ************************
      *** End Add *** 02/17/25 *** WC3001 *************************************
          Except errDetail;
       Other;
          Except infDetail;
       Endsl;
       Select;
       When isError;
          errCount += 1;
       Other;
          infCount += 1;
       Endsl;
      /End-Free

     p PrintError      e
      //---------------------------------------------------*
      // Prints Upload Report
      //---------------------------------------------------*
     p PrintUplRpt     b

      /Free
       intADrcnt = intADrcnt - 2; //Exclude 'H' and 'T' recs
       ORtitl1 = centerFld(ORtitl1);
       Except uplHead;
      *** Beg Chg *** 02/17/25 *** WC3001 *************************************
       //*** Except uplDetail;
       For Idx = 1 to %Elem(arrUplDetail);
           If arrUplDetail(Idx).@Group <> ' ';
              kkFnd         = %int(arrUplDetail(Idx).@Group);
              intADrCnt     = arrUplDetail(Idx).@TotPayroll;
              intNewClasCnt = arrUplDetail(Idx).@NewClasCnt;
              intMisClasCnt = arrUplDetail(Idx).@MisClasCnt;
              intZeroClmCnt = arrUplDetail(Idx).@ZeroClmCnt;
              totPayAmt     = arrUplDetail(Idx).@totPayAmt ;
              Except uplDetail;
           EndIf;
       EndFor;

      *** End Chg *** 02/17/25 *** WC3001 *************************************
      /End-Free

     p PrintUplRpt     e
      //---------------------------------------------------*
      // ProcessWXADP
      //---------------------------------------------------*
     p ProcessWXADP    b

     d firstRead       s               n   Inz(*On)

      /Free
       Dou %Eof(WXADL);
          Read pad00;
          If %Eof;
             //*** Beg Del ***** 07/15/25 *************************
             //***RptMissingPay();
             //***WriteWABLP();
             //*** End Del ***** 07/15/25 *************************
             Leave;
          Endif;
          If ADrect <> 'D';
             Iter;
          Endif;
          //*** Beg Del ***** 07/15/25 ***********************
          //***If firstRead;
          //***   svEmpLink = ADEmpLink;
          //***   svCap# = ADCap#;
          //***   svEmp# = %Int(ADemp#);
          //***   svDiv  = %Int(ADdiv);
          //***   svFyr  = %Int(ADfyr);
          //***   svJrSt = ADjrst;
          //***   firstRead = *Off;
          //***Endif;
          //*** End Del ***** 07/15/25 ***********************
          UpdWrtWMEPP();
          If %Int(ADemp#) <> svEmp# or %Int(ADdiv) <> svDiv or
           %Int(ADfyr) <> svFyr;
           //*** beg chg ***** 06/11/21 *******************
           //WriteWABLP();
           //RptMissingPay();
             RptMissingPay();
             WriteWABLP();
           //*** end chg ***** 06/11/21 *******************
             svEmpLink = ADEmpLink;
             svCap# = ADCap#;
             svEmp# = %Int(ADemp#);
             svDiv  = %Int(ADdiv);
             svFyr  = %Int(ADfyr);
             svJrSt = ADjrst;
          Endif;
       Enddo;
       //*** Beg Add ***** 10/27/22 *************************************
       If clientID = '148';
          Clear svEmp#;
          Clear svFyr;
          firstRead = *On;
          Setll *loval pad01;
          Dou %Eof(WXADL1);
             Read pad01;
             If %Eof;
                ZeroAuditPayroll();
                Leave;
             Endif;
             If ADrect <> 'D';
                Iter;
             Endif;
             If firstRead;
                svEmp# = %Int(ADemp#);
                svFyr  = %Int(ADfyr);
                firstRead = *Off;
             Endif;
             If %Int(ADemp#) <> svEmp# or %Int(ADfyr) <> svFyr;
                ZeroAuditPayroll();
                svEmp# = %Int(ADemp#);
                svFyr  = %Int(ADfyr);
             Endif;
          Enddo;
       Endif;
       //*** End Add ***** 10/27/22 *************************************

      /End-Free

     p ProcessWXADP    e
      //---------------------------------------------------*
      // programStart - Initialization
      //---------------------------------------------------*
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       sys_Date = %date();
       sys_Time = %time();
       sysDateCymd = %Dec(sys_Date:*cymd);
       sysTimeHms  = %Dec(sys_Time:*hms);
       kkco# = UserCo#(q1user);
       companyNm = GetCompNam(kkco#);
       companyNm = CenterFld(companyNm);
      *** Beg Del *** 02/17/25 *** WC3001 *************************************
       //*** kkFnd  = %Int(eFnd);
       //*** Chain(n) (kkco#:kkfnd) pfn0l;
       //*** RtvBrnd('@':#Brnd1:#Brnd2);
      *** End Del *** 02/17/25 *** WC3001 *************************************
       In pathNameDta;
       filePath = lastPathName;
       //*** Beg Add ***** 10/27/22 ******************************
       RtvCltId(clientId);
       //*** End Add ***** 10/27/22 ******************************
      /End-Free

     p ProgramStart    e
      //----------------------------------------------------*
      // Report non-audited payrolls missing from upload file
      //----------------------------------------------------*
     p RptMissingPay   b

      /Free
       //*** Beg Chg ***** 07/15/25 *************************
       //***Setll (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pep07e;
       //***Dou %Eof;
       //***   Reade (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pep07e;
       Setll (kkCo#:%Int(ADfnd):%Int(ADfyr):%Int(ADemp#):%Int(ADdiv)) pep07e;
       Dou %Eof;
          Reade (kkCo#:%Int(ADfnd):%Int(ADfyr):%Int(ADemp#):%Int(ADdiv)) pep07e;
       //*** End Chg ***** 07/15/25 *************************
          If %Eof;
             Leave;
          Endif;
          If EPsafl <> 'A';
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
          Pos = %LookUp(ADfnd:arrUplDetail(*).@Group:1:%Elem(arrUplDetail));
          If Pos > 0;
             arrUplDetail(Pos).@MisClasCnt += 1;
          EndIf;

      *** End Add *** 02/17/25 *** WC3001 *************************************
             If eZeroPayroll = 'Y';
                EPaapr    = 0;
                EPsafl    = 'A';
                EPacnt    = 0;
                EPuser    = Q1user;
                EPpgm     = Q1pgm;
                EPdt      = sysDateCymd;
                EPtime    = sysTimeHms;
                Update pep07e;
             Endif;
             //*** Beg Chg ***** 07/15/25 ****************************
             //***chrEmpLink = svEmpLink;
             //***chrAdCap# = svCap#;
             //***chrAdEmp# = %Editc(svEmp#:'4');
             //***chrAdDiv  = %Editc(svDiv:'4');
             //***chrAdFyr  = %Editc(svFyr:'4');
             chrEmpLink = ADEmpLink;
             chrAdCap# = ADCap#;
             chrAdEmp# = %Triml(ADemp#:'0');
             If %Triml(ADdiv:'0') = '';
                chrAdDiv = '0';
             Else;
                chrAdDiv = %Triml(ADdiv:'0');
             Endif;
             chrAdFyr  = ADfyr;
             chrAdFnd  = ADfnd;
             //*** End Chg ***** 07/15/25 ****************************
             chrADjrst = svJrSt;
             chrADclas = %Trim(%Editc(EPclas:'4'));
             chrADspec = EPspec;
             chrADext  = EPext;
             chrAdEfDt = %Char(%Date(EPefdt:*cymd):*usa);
             If eZeroPayroll = 'Y';
                PrintError(19:*Off);
             Else;
                PrintError(23:*Off);
             Endif;
             //*** Beg Chg ***** 07/15/25 **********************
             //***Chain (kkCo#:kkFnd:svEmp#:svDiv:svFyr:EPclas) pcm0z;
             Chain (kkCo#:%Int(ADfnd):%Int(ADemp#):%Int(ADdiv):%Int(ADfyr)
              :EPclas) pcm0z;
             //*** End Chg ***** 07/15/25 **********************
             If %Found;
                intZeroClmCnt += 1;
                PrintError(22:*Off);
             Endif;
          Endif;
       Enddo;
      /End-Free

     p RptMissingPay   e
      //----------------------------------------------------------------
      // SetBadData - sets badData *on if not already *on
      //----------------------------------------------------------------
     p SetBadData      b
     d SetBadData      pi
     d  inBadData                      n   const

      /Free
       If inBadData = *On;
          badData = *On;
       Endif;
      /End-Free

     p SetBadData      e
      //----------------------------------------------------------------
      // Update/Write Payrolls
      //----------------------------------------------------------------
     p UpdWrtWMEPP     b

      /Free
       intEfDt7 = %Dec(%Char(%Date(ADefdt:*Iso0):*Cymd0):7:0);
       //*** Beg Chg ***** 07/15/25 *****************************
       //***Chain (kkCo#:kkFnd:%Int(ADfyr):%Int(ADemp#):%Int(ADdiv):
       Chain (kkCo#:%Int(ADFnd):%Int(ADfyr):%Int(ADemp#):%Int(ADdiv):
       //*** End Chg ***** 07/15/25 *****************************
        %Int(ADclas):ADspec:ADext:intEfDt7) pep07e;
       Select;
       When %Found;
          EPsafl = 'A';
          EPaapr = %Dec(ADamt:11:2);
          EPacnt = %Int(ADacnt);
          EPuser = Q1user;
          EPpgm  = Q1pgm;
          EPdt   = sysDateCymd;
          EPtime = sysTimeHms;
          Update pep07e;
       When eNewClass = 'Y';
      *** Beg Add *** 02/17/25 *** WC3001 *************************************
          Pos = %LookUp(ADfnd:arrUplDetail(*).@Group:1:%Elem(arrUplDetail));
          If Pos > 0;
             arrUplDetail(Pos).@NewClasCnt += 1;
          EndIf;
      *** End Add *** 02/17/25 *** WC3001 *************************************
          Clear pep07e;
          EPco#  = kkCo#;
          //*** Beg Chg ***** 07/15/25 ****************************
          //***EPfnd  = kkFnd;
          EPfnd  = %Int(ADfnd);
          //*** End Chg ***** 07/15/25 ****************************
          EPemp# = %Int(ADemp#);
          EPdiv  = %Int(ADdiv);
          EPfyr  = %Int(ADfyr);
          EPclas = %Int(ADclas);
          EPspec = ADspec;
          EPext  = ADext;
          EPefdt = %Dec(%Char(%Date(ADefdt:*Iso0):*Cymd0):7:0);
          If ADcxdt <> *Blanks;
             EPcadt = %Dec(%Char(%Date(ADcxdt:*Iso0):*Cymd0):7:0);
          Endif;
          EPaapr = %Dec(ADamt:11:2);
          EPacnt = %Int(ADacnt);
          EPsafl = 'A';
          EPstat = 'A';
          EPuser = Q1user;
          EPpgm  = Q1pgm;
          EPdt   = sysDateCymd;
          EPtime = sysTimeHms;
          EPiusr = Q1user;
          EPipgm = Q1pgm;
          EPidt  = sysDateCymd;
          EPitm  = sysTimeHms;
          Write pep07e;
          chrEmpLink = ADEmpLink;
          chrADCap# = ADCap#;
          chrAdEmp# = ADemp#;
          chrAdDiv  = ADdiv;
          chrAdFyr  = ADfyr;
          chrADjrSt = ADjrSt;
          chrADclas = ADclas;
          chrADspec = ADspec;
          chrADext  = ADext;
          chrADefDt = %Char(%Date(ADefDt:*Iso0):*Usa/);
          //*** Beg Add ***** 07/15/25 ********************************
          chrAdFnd  = ADfnd;
          //*** End Add ***** 07/15/25 ********************************
          PrintError(20:*Off);
       Endsl;
      *** Beg Add *** 02/17/25 *** WC3001 *************************************

       Pos = %LookUp(ADfnd:arrUplDetail(*).@Group:1:%Elem(arrUplDetail));
       If Pos > 0;
          arrUplDetail(Pos).@totPayAmt += EPaapr;
       EndIf;

      *** End Add *** 02/17/25 *** WC3001 *************************************
      /End-Free

     p UpdWrtWMEPP     e
      //------------------------------------------------------------------------*
      // Return *On if new payroll class info is good
      //------------------------------------------------------------------------*
     p ValidNewClass   b
     d ValidNewClass   pi              n
     d kkClas          s                   like(PRclas)
     d kkExt           s                   like(EPext)
     d kkEfDt          s                   like(PRefdt)
     d kkSt            s              2a

      /Free
       kkClas = %Int(ADclas);
       kkExt = ADext;
       kkEfDt = intEfDt7;
       Setgt (ELco#:ELrtcd:kkClas:kkEfDt) ppr01; //excludes 'I' records
       Readpe (ELco#:ELrtcd:kkClas) ppr01;
       Select;
       When %Eof;
          PrintError(6:*On);
          Return *Off;
       When kkEfDt >= PRexdt and PRexdt <> 0;  //bad class
          PrintError(7:*On);
          Return *Off;
       Endsl;
       //
       // Validate extension if populated.
       //
       If ADext <> *blanks;
         // Check for record with rating state
          kkSt = ADjrst;
          Setgt (kkCo#:kkClas:kkExt:kkst:intEfDt7) ppx01;
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Dow not(%Eof) and PXstat = 'I';
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Enddo;
          If %Eof;
            // Check for record for blank state
             kkSt = *Blanks;
             Setgt (kkCo#:kkClas:kkExt:kkst:intEfDt7) ppx01;
             Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
             Dow not(%Eof) and PXstat = 'I';
             Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
             Enddo;
             If %Eof;
                PrintError(14:*on);
                Return *Off; // No reason to continue edits
             Endif;
          Endif;
       Endif;
       //
       // Extension is required for CA or WA if
       //  definition exists
       //
       kkSt = ADjrst;
       If (kkSt = 'CA' or kkSt = 'WA') and ADext = *blanks;
          Setgt (kkCo#:kkClas:kkExt:kkst:kkEfDt) ppx01;
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Dow not(%Eof) and PXstat = 'I';
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Enddo;
          If %Eof;
             PrintError(15:*on);
             Return *Off;
          Endif;
       Endif;
       Return *On;
      /End-Free

     p ValidNewClass   e
      //----------------------------------------------------------------
      // Write Mass Billing Record
      //----------------------------------------------------------------
     p WriteWABLP      b

     d prAgent         s                   like(EPdiv)
     d prFiller        s             60
     d prProd          s                   like(EPdiv)

      /Free
       //*** beg add ***** 06/11/21 *******************
       // Don't write mass billing record if non-audited
       //  payrolls exist.
       If intMisClasCnt > 0 And eZeroPayroll <> 'Y';
          Return;
       Endif;
       //*** end add ***** 06/11/21 *******************
       Setll (kkCo#:kkFnd:svEmp#:svDiv:svFyr) pbl0l1;
       Reade (kkCo#:kkFnd:svEmp#:svDiv:svFyr) pbl0l1;
       Dow Not(%Eof) And BLflg2 <> 'Y';
       Reade (kkCo#:kkFnd:svEmp#:svDiv:svFyr) pbl0l1;
       Enddo;
       If %Eof;
          Clear pbl0l1;
          BLco#  = kkCo#;
          BLfnd  = kkFnd;
          BLemp# = svEmp#;
          BLdiv  = svDiv;
          BLfyr  = svFyr;
          BLflg2 = 'Y';
          //*** Begin Change *** 12/17/19 ********************************
          //Chain (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          Chain(n) (kkCo#:kkFnd:svFyr:svEmp#:svDiv) pel01;
          //*** End   Change *** 12/17/19 ********************************
          BLrflg = ELnwrw;
          //*** Beg Add *** 12/14/23 ********************************
          BLpfdt = ELpfdt;
          BLstat = *blanks;
          //*** End Add *** 12/14/23 ********************************
          GetAgent(kkCo#:kkFnd:svEmp#:svDiv:svFyr:prAgent:
           prFiller:prProd:prFiller);
          BLagt# = prAgent;
          BLuser = Q1user;
          BLpgm  = Q1pgm;
          BLdt   = sysDateCymd;
          BLtime = sysTimeHms;
          Write pbl0l1;

      //*** Begin Add *** 12/17/19 **************************************
          ClearAudSrFlag();
      //*** End   Add *** 12/17/19 **************************************
       Endif;
      /End-Free

     p WriteWABLP      e
      //*** Beg Add ***** 10/27/22 **************************
      //----------------------------------------------------*
      // Zero Audit any un-Audited payroll Records
      //----------------------------------------------------*
     p ZeroAuditPayroll...
     p                 b

      /Free
       Setll (kkCo#:kkFnd:svFyr:svEmp#) pel01;
       Reade(n) (kkCo#:kkFnd:svFyr:svEmp#) pel01;
       Dow not %Eof(wdell1);
          Setll (kkCo#:kkFnd:ELFyr:ELEmp#:ELDiv) pep07e;
          Reade (kkCo#:kkFnd:ELFyr:ELEmp#:ELDiv) pep07e;
          Dow not %Eof(wmepl7e);
             If EPsafl <> 'A';
                EPaapr = 0;
                EPsafl = 'A';
                EPuser    = Q1user;
                EPpgm     = Q1pgm;
                EPdt      = sysDateCymd;
                EPtime    = sysTimeHms;
                Update pep07e;
             Endif;
             Reade (kkCo#:kkFnd:ELFyr:ELEmp#:ELDiv) pep07e;
          Enddo;
          Reade(n) (kkCo#:kkFnd:svFyr:svEmp#) pel01;
       Enddo;
      /End-Free

     p ZeroAuditPayroll...
     p                 e
      //*** End Add ***** 10/27/22 *************************

** Error messages  Err(x)                                                       x value
Required information missing.                                                   1
Employer/Division/Year values don't exist.                                      2
Invalid effective date.                                                         3
Invalid effective date on existing payroll class.                               4
Effective date not within policy date range.                                    5
Payroll class is invalid.                                                       6
Payroll class is expired as of this effective date.                             7
Audit amount is less than zero.                                                 8
Employee count is less than zero.                                               9
Upload not flagged to add new payroll classes.                                 10
An audit bill has already been processed.                                      11
Carrier policy and division number don't exist.                                12
Cancellation date is invalid.                                                  13
Payroll extension is invalid.                                                  14
Payroll extension is required.                                                 15
Policy can't be audited before its expiration date.                            16
Employer link code and state can't be found to audit.                          17
Multiple employers eligible for link code and state.                           18
Info Only - Payroll class was missing and audited to zero.                     19
Info Only - New payroll class added to policy.                                 20
Info Only - Claim attached and audit amount is zero.                           21
Info Only - Claim attached and audit amount is zero.                           22
Info Only - Payroll class missing from audit payroll upload.                   23
Group Number is required.                                                      24
** Upload Message - prints at end - upLd(1 or 2)
******* Audit import file was successfully uploaded. *******
***Audit import file was NOT uploaded because of error/s.***
