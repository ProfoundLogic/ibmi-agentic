     hCopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      //********************************************************************************************
     h/TITLE W8396R - New PEO Payroll Upload
     h option(*nodebugio:*srcstmt)
      //********************************************************************************************
      //  DESCRIPTION:  This program reads the flat file WXPPP, edits the records, and outputs an
      //                error report or a successful upload verification report and new payroll
      //                classification report. This program can process one header and one trailer
      //                record in each flat file only.
      //
      //  Called from:  W8396C - Override
      //  Calls      :  None
      //********************************************************************************************
      //  Created on 04/19/2005 by c:62 for Project 135WCD
      //********************************************************************************************
      // Revisions:
      //
      // 06/17/24 C:344 Project: WC2952
      //                Increase rate field to 8.3s for GetRate procedure.
      //
      // 06/20/19 C:136 Support: 80359
      //                Fix 74129
      //
      // 01/12/19 C:300 Support: 74129
      //                Add edit to PEO Payroll Import to not allow
      //                load of expired class codes
      //
      // 04/11/18 C:136 Support: 70008
      //                Add logic to correct payrolls if flagged to replace
      //                and flagged to correct payrolls.
      //
      // 04/09/18 C:136 Support: 69810
      //                Beef up upload to replace renewal payrolls
      //
      // 01/11/17 C:136 Support: 63629
      //                Prevent midterm add from going into endless loop
      //
      // 11/28/17 C:136 Project: 150WAN
      //                Add functionality to load payrolls to non PEO groups
      //
      // 08/30/17 C:136 Project: 150WAM Priss
      //               -Correct edit for payroll extension
      //               -Account for re-use of emp link/state
      //
      // 08/12/17 C:136 Project: 150WAM
      //               -Add processing for Misc Endorsement for Sunz only
      //               -Zero out per capita payrolls for Sunz only
      //
      // 04/05/17 C:136 Support: 46562
      //                Adjust payroll extension edits in detailEdit
      //
      // 02/01/17 C:136 Project: WC2479
      //               -Added new fields and edits for:
      //               --Extension
      //               --Effective date
      //               --Cancellation date
      //               --Replacement Flag
      //               -Convert to free form
      //
      // 08/05/15 C:307 Project: SY354
      //                -Adding 'Route To:'
      //
      // 08/26/14 C:307 Project: SY354
      //                -Implement Routing Page Changes.
      //
      // 08/23/10 C:257 Project: WC1928
      //                - Close all open cursor and declare WXPPP as user open file.
      //
      // 07/20/10 C:220 Project: WC1928
      //                - Changed the logic in the getRate subprocedure.
      //
      // 01/28/10 C:249 Project: SY215
      //          C:189 Retrieve brand Name
      //                                                               *
      // 04/01/08 C:183 PROJECT: WC1619
      //                Call SREFFD to set the effective date to be
      //                used on SETGT.
      //
      //********************************************************************************************
     fwxppp     uf   e             disk    usropn
     f                                     infds(dbWxppp)
     fsmorl     if   e           k disk
     fstabl     if   e           k disk
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwdell2    if   e           k disk    rename(pel01:pel02)
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     fwdhel     if   e           k disk    rename(phe01:phe00)
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
     fwmepl1e   uf a e           k disk    rename(pep01:pep01e)
     fwmeml18   if   e           k disk    rename(pem01:pem18)
     fwmenl6    uf a e           k disk    rename(pen01:pen06)
     fwmfnl     uf   e           k disk
     fwmmul6    if a e           k disk    rename(pmu01:pmu06)
     fwmprl1    if   e           k disk
     fwmpxl1    if   e           k disk    rename(ppx01:ppx01)
     fwmpxl3    if   e           k disk    rename(ppx01:ppx03)
     fwmrsl     if   e           k disk    rename(prs01:prs00)
     fwmshl1    if   e           k disk    rename(psh01:psh01)
     fqsysprt   o    f  140        printer oflind(*inoa)                        error report
     fqsysprt1  o    f  105        printer oflind(*inob)                        upload report
      //============================================================================================
      // Prototypes
      //============================================================================================
      *Entry
     dW8396R           pr
     d eFnd                           3
     d eQue                          10
     d eUpld                          1
     d eRpDt                          7
     dW8396R           pi
     d eFnd                           3
     d eQue                          10
     d eUpld                          1
     d eRpDt                          7

     d BadDataCheck    pr
     d CheckDuplicate  pr              n
     d DetailEdit      pr
     d ErrorCheckData  pr
     d ErrorCheckFile  pr
     d GetCarrier#     pr             5s 0
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     d GetDivYear      pr              n
     d GetMiscDartCd   pr             5a
     d GetNextEnd#     pr             5  0
     d GetNumOfHead    pr             6s 0
     d GetNumOfTrail   pr             6s 0
     d**** Beg Chg **** 06/17/24 *******************************************
     d***GetRate         pr             7s 2
     d GetRate         pr             8s 3
     d**** End Chg **** 06/17/24 *******************************************
     d GetRecordCnt    pr             6s 0
     d InvalidChars    pr              n
     d  valueToCheck                 11a   const
     d  nameOfField                  25a   const
     d  isRequired                     n   const
     d PolPgExists     pr              n
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value
     d PrintUpload     pr
     d ProcessMidterm  pr
     d ProcessWXPPP    pr
     d ProgramStart    pr
     d SetBadData      pr
     d  prBadData                      n   const
     d SbmtMidTrmJob   pr                  extpgm('W7050C1')
     d  outCo#                        3    const
     d  outFnd                        3    const
     d  outEmp#                       9    const
     d  outDiv                        5    const
     d  outFyr                        3    const
     d  outJob#                       6    const
     d  outEnd#                       5    const
     d  outQueue                     10    const
      *** beg add ***** 04/11/18 *********************
     d GetLossCostMul  pr                  extpgm('SRLCM')
     d  outCo#                        3    const
     d  outFnd                        3    const
     d  outRateCode                   3    const
     d  outClass                      5    const
     d  outEffDate                    7    const
     d  inLossCostMul                 5
      *** beg add ***** 04/11/18 *********************
     d UpdateWMENL6    pr
     d WriteWMENL6     pr
     d WriteWMEPP      pr
     d WriteWMMUL6     pr
      //============================================================================================
      // Stand alone fields
      //============================================================================================
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
     d errReason       s             60
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
     d xxque           s                   like(eQue)
     d group#          s                   like(ELFND)
     d orTitl1         s                   like(ORTITL)
     d orTitl2         s                   like(ORTITL)
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
     d svFnd           s                   like(ENFnd)
     d svEmp#          s                   like(ENEmp#)
     d svDiv           s                   like(ENdiv)
     d svFyr           s                   like(ENfyr)
     d isUseScript     s              1n   inz
     d processType     c                   const('Mid-Term Endorsement')
     d xxCarrier#      s                   like(F2car#)
     d xxMiscDartsCd   s              5a
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d xxUserOutQ      s             10

     d dbWxppp         ds
     d  rrnWxppp             397    400b 0
     d ppData        e ds                  extname(wxppp)
      //============================================================================================
      // Constants
      //============================================================================================
     d digits          c                   '0123456789'
     d title           c                   'PEO Payroll Upload Error Report'
     d title1          c                   'PEO Payroll Upload Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** No records exist in WXPPP ***'
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

     d checkDetail     ds
     d  chkd_Class                    5    overlay(checkDetail:84)
     d  chkd_Amount                  11    overlay(checkDetail:90)

     d header          ds
     d  hRecType                      1    overlay(header)
     d  Rec_Count                     9    overlay(header:2)

     d detail          ds
     d  dRecType                      1    overlay(detail)
     d  Dtl_Rec_Count                 9    overlay(detail:2)
     d  PEO_Link_Code                15    overlay(detail:11)
     d  Rating_State                  2    overlay(detail:26)
     d  Payroll_Class                 5    overlay(detail:28)
     d  Payroll_Spec                  1    overlay(detail:33)
     d  Payroll_Ext                   5    overlay(detail:34)
     d  Est_Amount                   11    overlay(detail:39)
     d  Eff_Date                      8    overlay(detail:50)
     d  Cx_Date                       8    overlay(detail:58)
     d  Emp_Count                     7    overlay(detail:66)
     d  Replace_Flag                  1    overlay(detail:73)
      *** beg chg ***** 04/11/18 *********************************
     d  Correct_Rate                  1    overlay(detail:74)
      *** end chg ***** 04/11/18 *********************************

     dsvLDA            ds                  dtaara(*lda)
     d ldEnd#                  1      5  0
     d ldEnda                  1      5
     d ldMyRr                  6     14  0
     d ldEeDt                 15     22  0
     d ldFyr                  23     25  0
     d ldPyRl                 26     26  0
     d xxDart                         1    overlay(svlda:251)
     d xxRmCd                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

     d trailer         ds
     d  tRecType                      1    overlay(trailer)

     d psds          esds                  extname(szq1p)
      //====================================================
      // Prototypes
      //====================================================
     d GetRateDate     pr                  extpgm('SREFFD')
     d  ##co#                         3    const
     d  ##fnd                         3    const
     d  ##fyr                         3    const
     d  ##emp#                        9    const
     d  ##div                         5    const
     d  ##date                        7
     d  ##type                        1    const
     d  ##clas                        5    const options(*nopass)
     d/copy sprcompany
     d/copy sprdate
     d/copy sprstring
     d/copy wpragent
     d/copy wprgroup
     d/copy wprusrprms
     d/copy sPrLetter
     d/copy sPrXML
      //============================================================================================
      // KLISTS
      //============================================================================================
     c     epky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkclas
     c                   kfld                    kkspec

     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkpeol
     c                   kfld                    kkrst

     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

     c     prky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    kkclas
     c                   kfld                    kkefdt

     c     prky2         klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    kkclas

     c     stky          klist
     c                   kfld                    kkkey
      //============================================================================================
      // Main Processing
      //============================================================================================
      /Free

       Open WXPPP;
       ProgramStart();
       ErrorCheckFile();
       If errorCount = *zero;
          message = message1;
          PrintError(0:*off);   //successful upload
          ProcessWXPPP();
          PrintUpload();        //upload report
          eUpld = 'Y';
          eRpDt = %Editc(sysDateCymd:'X');
       Else;
          eUpld = 'N';
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

       Close WXPPP;
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
     o                                            8 'Rec type'
     o                                         +002 'PEO link code  '
     o                                         +005 'State'
     o                                         +002 'Class'
     o                                         +002 'Spec'
     o                                         +002 'Ext'
     o                                         +004 'Error Reason'
     o          e            head           2
     o                       line               135
     o          e            det            1
     o               20      recordType          07
     o               20      PEO_Link_Code     +003
     o               20      Rating_State      +008
     o               20      Payroll_Class     +002
     o               20      Payroll_Spec      +005
     o               20      Payroll_Ext       +002
     o               20      errReason         +002
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
     o                                         +002 'Record Count'
     o          e            head1          2
     o                       line1              105
     o          e            det1           1
zzzz o                       sys_date            12
     o                       recCount          +002
      //------------------------------------------------------------------------*
      // badDataCheck - checks for bad data and passes back inidicator
      //------------------------------------------------------------------------*
     p BadDataCheck    b
     d BadDataCheck    pi

      /Free
       Select;
       When PPrect = 'H';
          SetBadData(InvalidChars(PPrCnt:'Rec Count':isRequired));
          If badData = *On;
             Return;
          Endif;
          If %Int(PPrCnt) <> GetRecordCnt;
             badDATA = *on;
             message = message10;
             PrintError(0:*off);
             Return;
          Endif;
       When PPrect = 'D';
          SetBadData(InvalidChars(PPclas:'Class Code':isRequired));
          SetBadData(InvalidChars(PPeamt:'Est Amount':isRequired));
          SetBadData(InvalidChars(PPefdt:'Eff Date':isRequired));
          SetBadData(InvalidChars(PPcxdt:'Cx Date':isNotRequired));
          SetBadData(InvalidChars(PPecnt:'Emp Count':isNotRequired));
       When PPrect = 'T';
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

     d peol            s             15a
     d rst             s              2a
     d pclas           s              5a
     d pspec           s              1a
     d pext            s              5a
     d pefdt           s              8a

      /Free
       Exec Sql Declare Project Cursor For
        Select PPpeol, PPrst, PPclas, PPspec, PPext, PPefdt
        From Wxppp
        Where PPrect = 'D'
        Group By PPpeol, PPrst, PPclas, PPspec, PPext, PPefdt
        Having Count(*) > 1
        Order By PPpeol, PPrst, PPclas, PPspec, PPext, PPefdt;
       Exec Sql Open Project;
       If Sqlstt = '00000';
          Exec Sql Fetch Next From Project
           Into  :peol, :rst, :pclas, :pspec, :pext, :pefdt;
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
     d DetailEdit      pi

     d indExtReqd      s               n
     d kkSt            s                   like(RSJrSt)
     d effDt7          s                   like(EPefdt)
     d charEfDt        s              7

      /Free
       //
       // Replacement Flag must by 'Y' or *blank
       //
       If Replace_Flag <> 'Y' and Replace_Flag <> *blank;
          PrintError(18:*on);
       Endif;
       //
       // Estimated amount must be >= zero
       //
       If %Int(Est_Amount) <  0;
          PrintError(11:*on);
       Endif;
       //
       // Ensure Employee_Count >= 0
       //
       If Emp_Count <> *blanks and %Int(Emp_Count) < 0;
          PrintError(16:*on);
       Endif;
       //
       // Validate cancellation date if populated
       //
       If Cx_Date <> *blanks and %Int(Cx_Date) <> 0;
          Monitor;
             effDt7 = %dec(%char(%date(Cx_Date:*iso0):*cymd0):7:0);
          On-Error;
             PrintError(15:*on);
          EndMon;
       Endif;
       //
       // PEO link code is required
       //
       If PEO_Link_Code = *blanks;
          PrintError(3:*on);
          Return;     // No reason to continue with edits
       Endif;
       //
       // Rating state must be valid
       //
       kkkey = Rating_State;
       Chain stky pst01;
       If Not %found;
          PrintError(6:*on);
          Return;     // No reason to continue with edits
       Endif;
       //
       // Validate effective date
       //
       Monitor;
          effDt7 = %dec(%char(%date(Eff_Date:*iso0):*cymd0):7:0);
       On-Error;
          PrintError(14:*on);
          Return;     // No reason to continue with edits
       EndMon;
       //
       // A division and year must exist for the link/rating state
       //
       If GetDivYear() = *Off;
          PrintError(7:*on);
          Return;     // No reason to continue with edits
       Endif;
       //
       // Get Rate Date
       //
       charEfDt  = %Editc(EffDt7:'X');
       GetRateDate(%editc(ELco# :'X'):
                   %editc(ELfnd :'X'):
                   %editc(ELfyr :'X'):
                   %editc(ELemp#:'X'):
                   %editc(ELdiv :'X'):
                   charEfDt:
                   'P');
       kkEfDt    = %Int(charEfDt);
       //
       // Payroll class code must be valid
       //
       kkRtCd = ELRtCd;
       EvalR Payroll_Class = %TrimR(Payroll_Class);
       kkClas = %Int(Payroll_Class);
       Setgt  prky1 ppr01;
       Readpe prky2 ppr01;
       Dow Not(%Eof) and PRstat = 'I';
       Readpe prky2 ppr01;
       Enddo;
       //*** beg chg ***** 01/12/19 ***************
       //*** If %Eof;
       //***    PrintError(9:*on);
       //***    Return;  // No reason to continue with edits
       //*** Endif;
       Select;
       When %Eof;
          PrintError(9:*on);
          Return;  // No reason to continue with edits
       When PRexdt = 0;
       When PRexdt <= kkEfDt;
          PrintError(8:*on);   // set up new error message at pos 8 (currently Not Used)
          Return;  // No reason to continue with edits
       Endsl;
       //*** end chg ***** 01/12/19 ***************
       //
       // If replacement flag is set, class code must exist;
       //  Else class code can't already exist
       //
       kkSpec = Payroll_Spec;
       kkExt  = Payroll_Ext;
       Chain(n) (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:kkClas:
        kkSpec:kkExt:effDt7) pep01e;
       If Not(%Found) and Replace_Flag = 'Y';
          PrintError(12:*on);
          Return;  // No reason to continue with edits
       Endif;
       If %Found and Replace_Flag = *blank;
          PrintError(10:*on);
          Return;  // No reason to continue with edits
       Endif;
       //
       // Zero out per capita amounts for Sunz only
       //
       If clientID = '150' and PRpcf <> 'P';
          PPeamt = *Zeros;
          Update ppp01;
       EndIf;
       //
       // Validate extension if populated.
       //
       If Payroll_Ext <> *blanks;
         // Check for record with a blank state
          Setgt (kkCo#:kkClas:kkExt:kkst:kkEfDt) ppx01;
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Dow not(%Eof) and PXstat = 'I';
          Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
          Enddo;
          If %Eof;
            // Check for record for rating state
             kkSt = Rating_State;
             Setgt (kkCo#:kkClas:kkExt:kkst:kkEfDt) ppx01;
             Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
             Dow not(%Eof) and PXstat = 'I';
             Readpe  (kkCo#:kkClas:kkExt:kkst) ppx01;
             Enddo;
             If %Eof;
                PrintError(17:*on);
                Return; // No reason to continue edits
             Endif;
          Endif;
       Endif;
       //
       // Extension is required for CA or WA if
       //  definition exists
       //
       indExtReqd = *Off;
       kkSt = Rating_State;
       If (kkSt = 'CA' or kkSt = 'WA') and Payroll_Ext = *blanks;
          Setll (kkCo#:kkClas:kkSt) ppx03;
          Reade (kkCo#:kkClas:kkst) ppx03;
          Dow not(%Eof);
             If PXstat <> 'I' and PXefdt <= kkEfDt;
                indExtReqd = *On;
                Leave;
             Endif;
          Reade (kkCo#:kkClas:kkst) ppx03;
          Enddo;
          If indExtReqd;
             PrintError(13:*on);
          Endif;
       Endif;
      /End-Free

     p DetailEdit      e
      //------------------------------------------------------------------------*
      // errorCheckData - error checks fields
      //------------------------------------------------------------------------*
     p ErrorCheckData  b
     d ErrorCheckData  pi

      /Free
       Select;
       When PPrect = 'H';
          recCount = recCount + 1;
       When PPrect = 'D';
          detail = ppdata;
          recCount = recCount + 1;
          DetailEdit();
       When PPrect = 'T';
          recCount = recCount + 1;
       Endsl;
      /End-Free

     p ErrorCheckData  e
      //------------------------------------------------------------------------*
      // errorCheckFile - Reads thru WXPPP and looks for errors
      //------------------------------------------------------------------------*
     p ErrorCheckFile  b
     d ErrorCheckFile  pi

      /Free
       // If any of these set of errors are encountered, output a
       //  message to the exception report and return control
       // Check for empty file
       Setll 1 ppp01;
       Read ppp01;
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
       Setll 1 ppp01;
       Read ppp01;
       If Not %Eof;
          recType = pprect;
          If recType = 'H';
             headerFirst = *on;
          Endif;
       Endif;
       //
       // Is the trailer last
       //
       Setgt *hival ppp01;
       Readp ppp01;
       If Not %Eof;
          recType = pprect;
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
       //
       // Run through file the first time checking for errors. If error is found, except the line
       // the error report and continue checking the file for other errors. Prevent the batch fro
       // further processing.
       //
       Setll 1 ppp01;
       Read ppp01;
       Dow Not %Eof;
          BadDataCheck();
          If Not badData;
             ErrorCheckData();
          Else;
             badData = *off;
          Endif;
          Read ppp01;
       Enddo;
      /End-Free

     p ErrorCheckFile  E
      //----------------------------------------------------------------
      // Get Carrier #
      //----------------------------------------------------------------
     p GetCarrier#     b
     d GetCarrier#     pi             5s 0

      /Free
       Chain (kkCo#:kkFnd) pf200;
       Return F2car#;
      /End-Free

     p GetCarrier#     e
      //----------------------------------------------------------------
      // GetDivYear - Return *On if division and year found for link/state
      //----------------------------------------------------------------
     p GetDivYear      b
     d GetDivYear      pi              n
     d kkFyr           s                   like(ELfyr)
     d kkPfDt          s                   like(ELpfdt)

      /Free
       //*** beg chg ***** 11/28/17 ******************
       //***kkPfDt = %Dec(%Char(%Date(Eff_Date:*iso0):*Cymd0):7:0);
       //***Setgt  (kkCo#:kkFnd:1:0:kkPfdt) pel02;
       //***Readpe (kkCo#:kkFnd:1:0)        pel02;
       //***If Not(%Eof) and ELptdt > kkPfDt;
       //***   kkFyr = ELfyr;
       //***Else;
       //***   Return *Off;
       //***Endif;
       //***Setll (kkCo#:kkFnd:Peo_Link_Code) pem18;
       //***Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
       //***Dow Not(%Eof);
       //***Chain (kkCo#:kkFnd:kkFyr:EMemp#:EMdiv) pel01;
       //***If %Found;
       //*** Chain (kkCo#:ELrtcd) prs00;
       //*** If %Found and RSjrst = Rating_State and
       //***  (EMcad1 = 0 or EMcad1 < kkPfDt);
       //***    Return *On;
       //*** Endif;
       //***Endif;

       // Get year from PEO Master
       kkPfDt = %Dec(%Char(%Date(Eff_Date:*iso0):*Cymd0):7:0);
       Setll (kkCo#:kkFnd:Peo_Link_Code) pem18;
       Dou %Eof(WMEML18);
          Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
          If %Eof;
             Leave;
          Endif;
          If EMcad1 <> 0 and EMcad1 <= kkPfDt;
             Iter;
          Endif;
          Setgt (EMco#:EMfnd:EMemp#:EMdiv:kkPfDt) pel02;
          Dou %Eof;
             Readpe (EMco#:EMfnd:EMemp#:EMdiv) pel02;
             If %Eof or ELptdt <= kkPfDt;
                Leave;
             Endif;
             Chain (kkCo#:ELrtcd) prs00;
             If %Found and RSjrst = Rating_State;
                Return *On;
             Endif;
          Enddo;
       //*** end chg ***** 11/28/17 ******************
       Enddo;
       Return *Off;
      /End-Free

     p GetDivYear      e
      //------------------------------------------------------------------------*
      // GetMiscDartCd - returns misc Darts Code
      //------------------------------------------------------------------------*
     p GetMiscDartCd   b
     d GetMiscDartCd   pi             5a

      /Free
       Chain (kkCo#:kkfnd) pdx00;
       Return DXmisc;
      /End-Free

     p GetMiscDartCd   e
      //------------------------------------------------------------------------*
      // GetNextEnd# - gets the next available endorsement #
      //------------------------------------------------------------------------*
     p GetNextEnd#     b
     d GetNextEnd#     pi             5  0

      /Free
       Setgt (kkCo#:kkFnd:svFyr:svEmp#) pmu06;
       Readpe (kkCo#:kkFnd:svFyr:svEmp#) pmu06;
       Return MUend# + 1;
      /End-Free

     p GetNextEnd#     e
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
       From Wxppp
       Where Pprect = 'H';
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
       From WXPPP
       Where PPrect = 'T';
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
      // GetRate - Get the rate
      //------------------------------------------------------------------------*
     p GetRate         b
      **** Beg Chg **** 06/17/24 *******************************************
     d***GetRate         pi             7s 2
     d GetRate         pi             8s 3
      **** End Chg **** 06/17/24 *******************************************
     d charEfDt        s              7
      *** beg add ***** 04/11/18 ***************
     d charLCM         s              5
     d decLCM          s              5s 4
      *** end add ***** 04/11/18 ***************

      /Free
       charEfDt  = %char(%date(Eff_Date:*iso0):*cymd0);
       //*** beg chg ***** 04/11/18 ***************
       //*** GetRateDate(%editc(EPco# :'X'):
       //***             %editc(EPfnd :'X'):
       //***             %editc(EPemp#:'X'):
       //***             %editc(EPdiv :'X'):
       //***             %editc(EPfyr :'X'):
       //***             charEfDt:
       //***             'P');
       GetRateDate(%editc(EPco# :'X'):
                   %editc(EPfnd :'X'):
                   %editc(EPfyr :'X'):
                   %editc(EPemp#:'X'):
                   %editc(EPdiv :'X'):
                   charEfDt:
                   'P');
       //*** end chg ***** 04/11/18 ***************
       kkEfDt = %Int(charEfDt);
       kkrtcd = ELrtcd;
       kkclas = EPclas;
       Setgt  prky1 ppr01;
       Readpe prky2 ppr01;
       //*** beg chg ***** 04/11/18 ***************
       //*** Dow Not(%Eof) and PRstat <> 'I';
       Dow Not(%Eof) and PRstat = 'I';
       //*** end chg ***** 04/11/18 ***************
       Readpe prky2 ppr01;
       Enddo;
       If Not(%Eof);
       //*** beg chg ***** 04/11/18 ***************
       //*** Return PRrt;
        // Don't change rate if A-Rated or Occupational
        //  Disease or Non-Rated
          If (ELstat = 'Y' and Replace_Flag = 'Y' and
            PRarfl = 'Y') or PRoccd = 'Y' or PRnrat = 'Y';
              Return EParat;
          Endif;
          GetLossCostMul(%editc(EPco# :'X'):
                         %editc(EPfnd :'X'):
                         ELrtcd:
                         %editc(EPclas:'X'):
                         charEfDt:
                         charLCM);
          decLCM = %Int(charLCM) * .0001;
          Eval(h) PRrt = PRrt * decLCM;
          If EPspec = 'F' and PRlhcd = 'N';
             Chain (ELco#:ELrtcd) prs00;
             Setgt (ELco#:ELfnd:RSjrst:kkEfDt) phe00;
             Readpe (ELco#:ELfnd:RSjrst) phe00;
             If Not(%Eof);
                Eval(h) PRrt = PRrt * HElhnf;
             Endif;
          Endif;
          Return PRrt;
       //*** end chg ***** 04/11/18 ***************
       Else;
          Return *Zero;
       Endif;
      /End-Free

     p GetRate         e
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
       From WXPPP;
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
             aNumber = %Int(rrnWxppp) + 1;
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
      // PolPgNotRun - Checks status of policy page before submitting midterm end
      //------------------------------------------------------------------------*
     p PolPgExists     b
     d PolPgExists     pi              n
      /Free
       If clientID <> '150';
          Return *Off;
       EndIf;
       Chain (ELco#:ELfnd:ELemp#:ELfyr:'P') psh01;
       If %Found and SHstat > '2';
          Return *On;
       Else;
          Return *Off;
       Endif;
      /End-Free
     p PolPgExists     e
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
       When PPrect = 'H';
          recordType = 'Header';
       When PPrect = 'D';
          recordType = 'Detail';
       When PPrect = 'T';
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
      //----------------------------------------------------------------
      // Process Midterm Endorsement for policy
      //----------------------------------------------------------------
     p ProcessMidterm  b
     d ProcessMidterm  pi

      /Free
       isUseScript = UseScriptura(processType);
       If isUseScript;
          xxProcType = processType;
          In *Lock *Dtaara;
          If xxMiscDartsCd <> *Blanks;
             xxDart = 'Y';
             xxRmCd = xxMiscDartsCd;
          Else;
             xxDart = 'N';
             xxRmCd = *blanks;
          Endif;
          ldEnd# = MUend#;
          ldEeDt = MUeedt;
          Out *dtaara;
          xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');
       Endif;
       SbmtMidTrmJob(%Editc(MUco#:'X'):
                     %Editc(MUfnd:'X'):
                     %Editc(MUemp#:'X'):
                     %Editc(MUdiv:'X'):
                     %Editc(MUfyr:'X'):
                     %Editc(Q1jobn:'X'):
                     %Editc(MUend#:'X'):
                     xxUserOutQ);
      /End-Free

     p ProcessMidterm  e
      //============================================================================================
      // processWXPPP - processes the flat file.
      //============================================================================================
     p ProcessWXPPP    b
     d ProcessWXPPP    pi

      /Free
       Setll 1 ppp01;
       Read ppp01;
       Dow Not %Eof;
          recType = PPrect;
          If recType = 'D';
             detail = PPdata;
             WriteWMEPP();
          Endif;
          Read ppp01;
       Enddo;
       UpdateWMENL6();
      /End-Free

     p ProcessWXPPP    e
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
       Chain(n) fnky pfn01;
       RtvBrnd('@':#Brnd1:#brnd2);
       xxCarrier# = GetCarrier#();
       xxMiscDartsCd = GetMiscDartCd();
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
      // UpdateWMENL6 - Populates end#/req#/seq# after all WMENP records
      //  have been written
      //----------------------------------------------------------------
     p UpdateWMENL6    b
     d UpdateWMENL6    pi

     d xxNextEnd#      s                   like(ENend#)
     d xxNextSeq#      s                   like(ENseq#)

      /Free
       If clientID <> '150';
          Return;
       Endif;
       // Get policy that this job wrote payrolls to
       //  into save fields before starting process
       Setll (Q1jobn:kkCo#:kkFnd) pen06;
       Reade (Q1jobn:kkCo#:kkFnd) pen06;
       If %Eof;
          Return;
       Endif;
       svEmp# = ENemp#;
       svDiv  = ENdiv;
       svFyr  = ENfyr;
       Dow Not(%Eof);
          If svEmp# <> ENemp# or
           svDiv <> ENdiv or svFyr <> ENfyr;
             Exsr UpdatePolicyRcds;
             svEmp# = ENemp#;
             svDiv  = ENdiv;
             svFyr  = ENfyr;
          Endif;
       Reade (Q1jobn:kkCo#:kkFnd) pen06;
          If %Eof;
             Exsr UpdatePolicyRcds;
          Endif;
       Enddo;
       //------------------------------------
       // Assigns the sequence numbers for each policy/div
       //------------------------------------
       Begsr UpdatePolicyRcds;
       xxNextEnd# = GetNextEnd#();
       xxNextSeq# = 0;
       Setll (Q1jobn:kkCo#:kkFnd:svEmp#:svDiv:svFyr) pen06;
       Reade (Q1jobn:kkCo#:kkFnd:svEmp#:svDiv:svFyr) pen06;
       Dow Not(%Eof);
          ENend# = xxNextEnd#;
          ENreq# = 1;
          xxNextSeq# += 1;
          ENseq# = xxNextSeq#;
          Update pen06;
       Reade (Q1jobn:kkCo#:kkFnd:svEmp#:svDiv:svFyr) pen06;
       Enddo;
       If xxNextSeq# > 0;
          WriteWMMUL6();
          ProcessMidterm();
       Endif;
       // Reset pointer
       Setgt (Q1jobn:kkCo#:kkFnd:svEmp#:svDiv:svFyr) pen06;
       Reade (Q1jobn:kkCo#:kkFnd) pen06;
       EndSr;
      /End-Free

     p UpdateWMENL6    e
      //----------------------------------------------------------------
      // WriteWMENL6 - writes to Midterm Workfile
      //----------------------------------------------------------------
     p WriteWMENL6     b
     d WriteWMENL6     pi

     d strPayrollDesc  s                   like(ENalin)

      /Free
       If clientID <> '150';
          Return;
       Endif;
       Clear pen06;
       ENco#  = ELco#;
       ENfnd  = ELfnd;
       ENemp# = ELemp#;
       ENdiv  = ELdiv;
       ENfyr  = ELfyr;
       ENeedt = EPefdt;
       ENjob  = Q1jobn;
       ENnam1 = EMname;
       ENrmcd = xxMiscDartsCd;
       ENmsfl = 'Y';
       ENfdn1 = EMfdnm;
       ENenm1 = EMname + %subst(EMnam2:1:10);
       ENenm2 = %subst(EMnam2:11:50);
       ENefn1 = EMfdnm + %subst(EMfdn2:1:10);
       ENefn2 = %subst(EMfdn2:11:50);
       ENcon1 = EMcont;
       ENatt1 = EMattn;
       ENcn31 = EMcnt3;
       ENfen1 = EMfein;
       ENtob1 = EMtpob;
       ENma11 = EMmad1;
       ENma21 = EMmad2;
       ENmct1 = EMmcty;
       ENmst1 = EMmst;
       ENmzp1 = EMmzip;
       ENpa11 = EMpad1;
       ENpa21 = EMpad2;
       ENpct1 = EMpcty;
       ENpst1 = EMpst;
       ENpzp1 = EMpzip;
       ENnam2 = EMname;
       ENanm1 = EMname + %subst(EMnam2:1:10);
       ENanm2 = %subst(EMnam2:11:50);
       ENfdn2 = EMfdnm;
       ENafn1 = EMfdnm + %subst(EMfdn2:1:10);
       ENafn2 = %subst(EMfdn2:11:50);
       ENfen2 = EMfein;
       ENtob2 = EMtpob;
       ENfnnm = GetGrpName(EMfnd);
       ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:sysDateCymd:ENagnm);
       ENcar1 = xxCarrier#;
       ENpfd1 = ELpfdt;
       ENptd1 = ELptdt;
       ENdsc1 = 'PEO Payroll Import Midterm Add';
       Select;
       When EPspec = *Blank and EPext = *Blanks;
          strPayrollDesc = 'Payroll class - ' +
           %Editc(EPclas:'X');
       When EPspec <> *Blank and EPext = *Blanks;
          strPayrollDesc = 'Payroll class/special designation - ' +
           %Editc(EPclas:'X') + '/' + EPext;
       When EPspec <> *Blank and EPext <> *Blanks;
          strPayrollDesc = 'Payroll class/special designation/extension - ' +
           %Editc(EPclas:'X') + '/' + EPspec + '/' + EPext;
       EndSl;
       ENalin = strPayrollDesc;
       ENdt   = sysDateCymd;
       ENtime = sysTimehms;
       Write pen06;
      /End-Free

     p WriteWMENL6     e
      //----------------------------------------------------------------
      // WriteWMEPP - writes to WMEPP
      //----------------------------------------------------------------
     p WriteWMEPP      b
     d WriteWMEPP      pi
      *** beg add ***** 04/10/18 **************************
     d kkEfDt          s                   like(EPefdt)
      *** end add ***** 04/10/18 **************************

      /Free
       GetDivYear();
       EvalR Payroll_Class = %TrimR(Payroll_Class);
       kkClas = %Int(Payroll_Class);
       kkSpec = Payroll_Spec;
       kkExt  = Payroll_Ext;
       //*** beg add ***** 04/10/18 **************************
       kkEfDt = %Dec(%Char(%Date(Eff_Date:*iso0):*Cymd0):7:0);
       //*** eng add ***** 04/10/18 **************************
       //
       // Replace payroll amount if flagged to do so
       //
       If Replace_Flag = 'Y';
       //*** beg chg ***** 04/10/18 **************************
       //*** Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:kkClas:
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:kkClas:
       //*** end chg ***** 04/10/18 **************************
           kkSpec:kkExt:kkEfdt) pep01e;
          EvalR Est_Amount = %TrimR(Est_Amount);
          EPeapr = %Dec(Est_Amount:11:2);
       //*** beg chg ***** 04/10/18 **************************
       //*** If Cx_Date <> *blanks;
          If Cx_Date <> *blanks and CX_Date <> *Zeros;
       //*** end chg ***** 04/10/18 **************************
             EPcadt = %Dec(%Char(%Date(Cx_Date:*Iso0):*Cymd0):7:0);
          Endif;
          If Emp_Count <> *blanks;
             EPecnt = %Int(Emp_Count);
          Endif;
       //*** beg add ***** 04/11/18 **************************
          If Correct_Rate = 'Y';
             EParat = GetRate();
          Endif;
       //*** end add ***** 04/11/18 **************************
          EPuser = Q1user;
          EPpgm  = Q1pgm;
          EPdt   = sysDateCymd;
          EPtime = sysTimeHms;
          Update pep01e;
          Return;
       Endif;
       Clear pep01e;
       EPco#  = EMco#;
       EPfnd  = EMfnd;
       EPfyr  = ELfyr;
       EPemp# = EMemp#;
       EPdiv  = EMdiv;
       EPclas = kkClas;
       EPspec = Payroll_Spec;
       EPext  = Payroll_Ext;
       //*** beg chg ***** 04/10/18 **************************
       //*** EPefdt = %Dec(%Char(%Date(Eff_Date:*Iso0):*Cymd0):7:0);
       EPefdt = kkEfDt;
       //*** end chg ***** 04/10/18 **************************
       EParat = GetRate();
       EvalR Est_Amount = %TrimR(Est_Amount);
       EPeapr = %Dec(Est_Amount:11:2);
       If Cx_Date <> *blanks;
          EPcadt = %Dec(%Char(%Date(Cx_Date:*Iso0):*Cymd0):7:0);
       Endif;
       If Emp_Count <> *blanks;
          EPecnt = %Int(Emp_Count);
       Endif;
       EPsafl = 'E';
       EPstat = 'A';
       EPuser = Q1user;
       EPpgm  = Q1pgm;
       EPdt   = sysDateCymd;
       EPtime = sysTimeHms;
       EPiusr = EPuser;
       EPipgm = EPpgm;
       EPidt  = EPdt;
       EPitm  = EPtime;
       Write pep01E;

       If PolPgExists();
          WriteWMENL6();
       Endif;
      /End-Free

     p WriteWMEPP      e
      //----------------------------------------------------------------
      // WriteWMMUL6 - writes the employer forms record
      //----------------------------------------------------------------
     p WriteWMMUL6     b
     d WriteWMMUL6     pi

      /Free
       MUco#  = ENco#;
       MUfnd  = ENfnd;
       MUemp# = ENemp#;
       MUdiv  = ENdiv;
       MUfyr  = ENfyr;
       MUend# = ENend#;
       MUseq# = 0;
       MUform = 'MID-YEAR';
       MUenst = *Blanks;
       MUdesc = 'PEO Payroll Import';
       MUdsc2 = 'PEO Payroll Import';
       MUeedt = ENeedt;
       MUexfl = 'I';
       MUmnau = 'M';
       MUprnt = 'N';
       MUstat = 'A';
       MUuser = Q1user;
       MUpgm  = Q1pgm;
       MUtime = sysTimeHms;
       MUdt   = sysDateCymd;
       MUiusr = Q1user;
       MUipgm = Q1pgm;
       MUidt  = sysDateCymd;
       MUtime = sysTimeHms;
       Write pmu06;

      /End-Free
     p WriteWMMUL6     e

** Error messages
Numeric fields have character information (bad data).                 1
Record type is invalid. It must be a 'H','D', or 'T'.                 2
PEO link code is blank.                                               3
<Not used>                                                            4
<Not used>                                                            5
Rating state is invalid.                                              6
No associated employer link/state for effective date.                 7
Payroll class is expired.                                             8                  01/12/19
Invalid payroll class.                                                9
Payroll/Spec/Ext/Efdt exists, it cannot be added.                    10
Estimated payroll amount must be >= 0.                               11
Payroll/Spec/Ext/Efdt does not exist, it cannot be replaced.         12
Payroll extension required.                                          13
Payroll effective date is invalid.                                   14
Payroll cancellation date is invalid.                                15
Employee count cannot be negative.                                   16
Payroll extension is not valid.                                      17
Replacment flag must 'Y' or blank.                                   18
