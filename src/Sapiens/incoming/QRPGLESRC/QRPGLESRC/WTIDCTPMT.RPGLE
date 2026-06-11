     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE IDC Transactions- Prompt
     H*********************************************************************
   H*  DESCRIPTION:  This program prompts for selection criteria to run *
    H*                the IDC Transactional process                      *
     H*                                                                   *
    H*  CALLED FROM:  StoneRiver New Menu items >> IDC Transactional Proc*
    H*    MENU                                                           *
     H*                                                                   *
    H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
    H*  Project:               C:079     PROJECT: WC2604                 *
     H***********************************************************************
    H* REVISIONS:                                                         *
     H* 04/27/20  C:180  Support 88508 Fix Submission time                 *
     H* 03/26/20  C:180  Support 88141 Fix Submission File Id              *
     H*
     H***********************************************************************
    H* INDICATORS:
     H***********************************************************************
     * Files Declaration                                                   *
     fwtidctpmt cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflIDCTran:rrnIDCTran)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
    f* IDC Submission Control Files
     fwmidcscl2 if   e           k disk
     F                                     infds(ccFds)
     fwmidcsc   uf a e             disk    rename(pcc01:pcc02)
     f                                     infds(ccFds2)
     fwmidctrl1 if   e           k disk
     fwdf2l     if   e           k disk
     fwxgsl     if   e           k disk
     f*
    f* User Parameters File
     fwmupl     if   e           k disk    rename(pup01:pup00)

     * Prototypes

     d/copy sPrDate
     d/copy sPrModule
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy wPrUsrprms
     d/copy sPrRunCmd
     d/copy wprIDC
     d
     d/copy scopypr01d
     DdsFileInfo       ds
     D rrn                   397    400b 0
     DccFds            ds                  likeds(dsFileInfo)
     DccFds2           ds                  likeds(dsFileInfo)
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d isExit          s               n   inz(*off)
     d
     d intRrn          s              9  0 inz(0)
     d intTime         s              6  0 inz
     d intCo#          s                   Like(UPco#)
     d
     d inMode          s              1
     d inCall          s              1
     d
     d inCompName      s             40    inz
     d inco#           s              3    inz
     d inTime          s              6    inz
     d inOutQ          s             10    inz
     d xxDate          s              8  0
     d xxYear          s              4  0
     d strMsg          s            256a
     d strCmd          s          15000a
     d strJob          s             12a
     d #IndGrEvt       s               n   inz(*off)
     d xxCurDate       s              8s 0
     D xxTrnDate       s              7  0 INZ
     D xxQtrFrmDt      s              7  0 inz
     D xxQtrEndDt      s              7  0 INZ
     D isFoundTrRcd    s              1
     D xxRptYr         s              4
     D xxRptQtr        s              1
     d
     *---------------------------------------------------------------------
     * Internal subprocedure Prototypes.
     *---------------------------------------------------------------------

     * Main Processing
     d Main            pr

     * Program Start
     d ProgramStart    pr

     * Clear the subfiles
     d ClrSubFiles     pr

     * Loading Prior Transactional Submissions Grid
     d LoadTransGrd    pr

     * Screen validation procedure.
     d ValidateScr     pr              n

     * Write to IDC Transaction File
     d WriteWmIDCTsc   pr
     d*
     *---------------------------------------------------------------------
     * External subprocedure Prototypes.
     *---------------------------------------------------------------------

     * Main Processing
     d IDCTransProc    pr                  ExtPgm('WTIDCTPRC')
     d                                9  0

     d dsMemberDta     ds                  dtaara('W7944DTA')
     d  charMember                   10

     ***===================================================================
     ** Recieving entry parameters.
     ***===================================================================
     c     *entry        plist
     c                   parm                    @@Prds           40
     c                   parm                    @@Call            1
     c
      /Free

      // Start Of Program
       ProgramStart();

      // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

      //===================================================================
      // Main - Main logic is here.
      //===================================================================
     p Main            b
     d OutRrn          s              9  0
     d OutRrn2         s              9  0
      /Free
       Dow btnCancel = *off and isExit = *off;

         // Executing the screen record format.
          Exsr ExfmtScreens;

          Select;

         // Submit button
          When btnSubmit = *on;
             btnSubmit = *off;
             If ValidateScr();
                ExSr SubmitProcess;
          //*** isExit = *on;
                refrshData = *on;
             Endif;


         // Refresh button
          When refrshData = *On;
             refrshData = *Off;
            // Clearing the record format
             Clear RcdIDCTran;
            // Intailize all that indicator stuff.
             ProgramStart();

          Other;
             Exsr procOther;

          EndSl;

       EndDo;

      // Executing all the screen Record Format.
       BegSr ExfmtScreens;

        TabHeight = 600;
        ApnlHeight = TabHeight+30;
        Write RcdComPnl;
        Write RcdTabIDC ;
        Exfmt RcdIDCTran;
        Read RcdComPnl;
        Read RcdTabIDC ;

       EndSr;

      // Submitter job subroutine.
       BegSr SubmitProcess;

       // Read through the transactional file and determine if there are
       // any records for the selected quarter that are ready to be
       // submitted.  If no records found then give a warning message
          isFoundTrRcd = 'N';
          Setll *zeros ptr01;
          If %equal(wmidctrl1);
             reade *zeros ptr01;
             Dow not %eof(wmidctrl1);
                xxTrnDate = %dec(%char(%date(TRtrnDate:*ISO0):*cymd0):7:0);
                xxRptQtr = %Char(scRptQtr);
                xxRptYr = %Char(scRptYr);
                xxQtrFrmDt = GetQtrFrmDt(xxRptQtr:xxRptYr);
                xxQtrEndDt = GetQtrEndDt(xxRptQtr:xxRptYr);
                If xxTrnDate >= xxQtrFrmDt and xxTrnDate <= xxQtrEndDt;
                   isFoundTrRcd = 'Y';
                   Leave;
                Endif;
                reade *zeros ptr01;
             Enddo;
          Endif;

          inco#  =  %editc(intCo#:'X');

          Select;
          When isFoundTrRcd <> 'Y';
             strMsg = 'You have requested to submit an orig' +
                'inal file but there are no eligible transactions.  ' +
                'Press Cancel to Return to the previous screen.';
             PopOneBtn(strMsg:'Submit IDC Original Transacactions':
                    'Cancel');
          Other;
             strMsg = 'You have requested to submit an orig' +
                'inal file.  Press Submit to begin the process o' +
                'r Cancel to return to the previous screen';
          Endsl;
          If isFoundTrRcd = 'Y';
             If PopTwoBtn(strMsg:'Submit IDC Original Transacactions':
                       'Submit':'Cancel');
                WriteWmIDCTsc();
                strJob = 'WTIDCTPRC';
                outRrn = ccFds2.rrn;
                outRrn2 = *Zeros;
             //*** Get the member from WXGSP
                In dsMemberDta;
                strCmd = 'SBMJOB ' + 'CMD(CALL PGM(WTIDCGSOVR) ' +
                'PARM(' + '''' + %EditC(outRRN:'X') + '''' + ' ' +
                '''' + %Trim(charMember) + '''' + ' ' +
                '''' + %Trim('T') + '''' + ' ' + '''' + %EditC(outRRN2:'X')
                + '''' + '))' +
                ' JOB(' + %trim(strJob) + ') ';

                RunCmd(strCmd:%len(%trim(strCmd)));

             Else;
                Callp Main();
             Endif;
          Else;
             Callp Main();
          Endif;
       EndSr;

       //===================================================================
       // ProcOther - Process Other options
       //===================================================================
       BegSr ProcOther;

        ReadC sflIDCTran;

        Select;

        // processing for Replace link
        When scGrRep = *on;
           scGrRep = *off;
           #IndGrEvt = *on;
           Update sflIDCTran;
           Exsr ReplaceGridProc;

        // processing for Delete link
        When scGrDel = *on;
           scGrDel = *off;
           #IndGrEvt = *on;
           Update sflIDCTran;
           Exsr DeleteGridProc;

        // processing for Re-Send link
        When scGrRes = *on;
           scGrRes = *off;
           #IndGrEvt = *on;
           Update sflIDCTran;
           Exsr ResendGridProc;

        Endsl;

       EndSr;

      // Submitter job Replace subroutine.
       BegSr ReplaceGridProc;

          inco#  =  %editc(intCo#:'X');
          strMsg = 'You have requested to replace an orig' +
          'inal file.  Submission will replace the selected file at ' +
          'NCCI.  Press Submit to begin the process o' +
          'r Cancel to return to the previous screen';

          If PopTwoBtn(strMsg:'Submit IDC Replacement Transacactions':
                 'Submit':'Cancel');
       // Create a new replacement record from the original record
             Chain scRrn pcc02;
             If %Found(wmidcsc);
                CCsubTyCd  = 'R';
                CCrunT = 'R';
                CCrecTot = *blanks;
                CCsubDate = %Char(%Dec(%Date));
                //*** Begin Change *** 04/27/20 *************************
                   //CCsubTime = %Char(%Dec(%Time));
                CCsubTime = %Editc((%Dec(%Time)):'X');
                //*** End   Change *** 04/27/20 *************************
                CCfiller  = *blanks;
                CCsts     = *blank;
                CCiUsr    = Q1User;
                CCiDt     = DateTo7(%Date);
                CCiTim    = %Dec(%Time);
                CCiPgm    = Q1Pgm;
                CCuser    = Q1User;
                CCdt      = CCiDt;
                CCtime    = CCiTim;
                CCpgm     = Q1Pgm;

                Write pcc02;

                strJob = 'WTIDCTPRC';
                outRrn = ccFds2.rrn;
                outRrn2 = *zeros;
                charMember = *blanks;

                strCmd = 'SBMJOB ' + 'CMD(CALL PGM(WTIDCGSOVR) ' +
                'PARM(' + '''' + %EditC(outRRN:'X') + '''' + ' ' +
                '''' + %Trim(charMember) + '''' + ' ' +
                '''' + %Trim('T') + '''' + ' ' + '''' + %EditC(outRRN2:'X') +
                '''' + '))' +
                ' JOB(' + %trim(strJob) + ') ';

                RunCmd(strCmd:%len(%trim(strCmd)));
             Endif;
          Else;
             Callp Main();
          Endif;
       EndSr;

      // Submitter job Resend subroutine.
       BegSr ResendGridProc;

          inco#  =  %editc(intCo#:'X');
          strMsg = 'You have requested to resend an orig' +
          'inal file.  Submission will re-send the selected file to ' +
          'the folder. This is done to send a file that had never been ' +
          'received by NCCI.  Press Submit to begin the process o' +
          'r Cancel to return to the previous screen';

          If PopTwoBtn(strMsg:'Submit IDC Replacement Transacactions':
                 'Submit':'Cancel');
       // Re-send the batch
             Chain scRrn pcc02;
             If %Found(wmidcsc);
                CCrunT = 'S';
                CCuser    = Q1User;
                CCdt      = DateTo7(%Date);
                CCtime    = %Dec(%Time);
                CCpgm     = Q1Pgm;
                Update pcc02;

                strJob = 'WTIDCTPRC';
                outRrn = ccFds2.rrn;
                outRrn2 = *Zeros;
                charMember = *blanks;

                strCmd = 'SBMJOB ' + 'CMD(CALL PGM(WTIDCGSOVR) ' +
                'PARM(' + '''' + %EditC(outRRN:'X') + '''' + ' ' +
                '''' + %Trim(charMember) + '''' + ' ' +
                '''' + %Trim('T') + '''' + ' ' + '''' + %EditC(outRRN2:'X') +
                '''' + '))' +
                ' JOB(' + %trim(strJob) + ') ';

                RunCmd(strCmd:%len(%trim(strCmd)));
             Endif;
          Else;
             Callp Main();
          Endif;
       EndSr;

      // Submitter job Delete subroutine.
       BegSr DeleteGridProc;

          inco#  =  %editc(intCo#:'X');
          strMsg = 'You have requested to delete an orig' +
          'inal file.  Submission will delete the selected file at ' +
          'NCCI.  Press Submit to begin the process o' +
          'r Cancel to return to the previous screen';

          If PopTwoBtn(strMsg:'Submit IDC Replacement Transacactions':
                 'Submit':'Cancel');
       // Delete the batch
             Chain scRrn pcc02;
             If %Found(wmidcsc);
                CCsubTyCd  = 'R';
                CCrunT = 'D';
                CCrecTot = *blanks;
                CCsubDate = %Char(%Dec(%Date));
                //*** Begin Change *** 04/27/20 *************************
                //CCsubTime = %Char(%Dec(%Time));
                CCsubTime = %Editc((%Dec(%Time)):'X');
                //*** END   Change *** 04/27/20 *************************
                CCfiller  = *blanks;
                CCiUsr    = Q1User;
                CCiDt     = DateTo7(%Date);
                CCiTim    = %Dec(%Time);
                CCiPgm    = Q1Pgm;
                CCuser    = Q1User;
                CCdt      = CCiDt;
                CCtime    = CCiTim;
                CCpgm     = Q1Pgm;

                Write pcc02;

                strJob = 'WTIDCTPRC';
                outRrn = ccFds2.rrn;
                outRrn2 = *zeros;
                charMember = *blanks;

                strCmd = 'SBMJOB ' + 'CMD(CALL PGM(WTIDCGSOVR) ' +
                'PARM(' + '''' + %EditC(outRRN:'X') + '''' + ' ' +
                '''' + charMember + '''' + ' ' +
                '''' + %Trim('T') + '''' + ' ' + '''' + %EditC(outRRN2:'X') +
                '''' + '))' +
                ' JOB(' + %trim(strJob) + ') ';

                RunCmd(strCmd:%len(%trim(strCmd)));
             Endif;
          Else;
             Callp Main();
          Endif;
       EndSr;

      /End-Free
     p Main            e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

      // Intialize the Screen indicators and fields.
       btnCancel = *off;
       btnSubmit = *off;
       refrshData = *off;
       scTestFile = 'N';
       scGrRep = *off;
       scGrRes = *off;
       scGrDel = *off;

      // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

      // Read the user parameter file to retrive the output queue.
       Chain q1User pup00;
       If %found(wmupl);
          inOutq = UPoutQ;
       EndIf;

      // Get the current time.
       inTime =  %char(%Time():*ISO0);
       intTime = %int(inTime);


      // Clearing the Subfile before loading.
       ClrSubfiles();
      // Loading the Prior Transactional Submissions Grid
       LoadTransGrd();

       xxYear = %Dec(%Subst(%char(%date()):1:4):4:0);

      /End-Free
     p ProgramStart    e

      //===================================================================
      // Write to the Transaction History file
      //===================================================================
     p WriteWmIDCTsc   b
     d WriteWmIDCTsc   pi

      /Free

       CCrecTyCd  = '03';
       CCsubTyCd  = 'O';

       // Get group carrier code
       CCcarGpCd  = *blanks;

       Read pgs01;
       Dow Not %Eof;
          If GSiEfl = 'I';
             Chain (intCo#:gsfnd) pf201;
             If %Found(wdf2l);
                CCcarGpCd = %EditC(F2nCarGrp:'X');
                Leave;
             Endif;
          Endif;
          Read pgs01;
       Enddo;

       CCrptQtrCd = %Char(scRptQtr);
       CCrptYear  = %Char(scRptYr);

       if scTestFile = 'N';
          //*** Begin Change ** 03/26/20 **************************
          //CCsubFilID = 'IDCTRANS' + %Editc(%Dec(%Date):'X') +
          //         %Editc(%Dec(%Time):'X') + '.txt';
          CCsubFilID = 'IDCTRANS' + %Editc(%Dec(%Date):'X') +
                        %Editc(%Dec(%Time):'X') + 'txt';
          //*** End   Change ** 03/26/20 **************************
       Else;
          //*** Begin Change ** 03/26/20 **************************
          //CCsubFilID = 'IDCTRANS' + %Editc(%Dec(%Date):'X') +
          //           %Editc(%Dec(%Time):'X') + '.tst';
          CCsubFilID = 'IDCTRANS' + %Editc(%Dec(%Date):'X') +
                        %Editc(%Dec(%Time):'X') + 'tst';
          //*** End   Change ** 03/26/20 **************************
       Endif;

       CCsubDate = %Char(%Dec(%Date));
       //*** Begin Change *** 04/27/20 *************************
          //CCsubTime = %Char(%Dec(%Time));
       CCsubTime = %Editc((%Dec(%Time)):'X');
       //*** End   Change *** 04/27/20 *************************
       CCrecTot  = *zeros;
       CCfiller  = *blanks;
       CCbchID   = %Editc(%Dec(%Date):'X')+ %Editc(%Dec(%Time):'X');
       CCsts     = *blank;
       CCrunT    = 'O';
       CCiUsr    = Q1User;
       CCiDt     = DateTo7(%Date);
       CCiTim    = %Dec(%Time);
       CCiPgm    = Q1Pgm;
       CCuser    = Q1User;
       CCdt      = CCiDt;
       CCTIME    = CCiTim;
       CCpgm     = Q1Pgm;

       Write pcc02;

      /End-Free

     p WriteWmIDCTsc   e


      //===================================================================
      // Validate Procedure. All the screen field validation
      //===================================================================
     p ValidateScr     b
     d ValidateScr     pi              n
     d**
     disError          s               n   inz(*off)

      /Free
       isError    = *off;
       ErRptYr    = *off;
       ErRptQtr   = *off;
       ErDupTrans = *off;

       msgRptYr   = *blanks;
       // Validate the reporting year
       If scRptYr < 2019 or scRptYr > xxYear;
          isError = *on;
          ErRptYr = *on;
          MsgRptYr = 'Invalid Reporting Year';
       Endif;

       // Reporting quarter cannot be blank
       If isError = *off and scRptQtr = *zeros;
          isError = *on;
          ErRptQtr = *on;
          MsgRptQtr = 'Reporting Quarter Cannot be Blank';
       Endif;

       If isError = *On;
          Return *off;
       Else;
          Return *on;
       EndIf;

      /End-Free
     p ValidateScr     e

      //===================================================================
      // ClrSubFiles - Clears SubFile
      //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free
       RrnIDCTran = 0;
       clrIDCSfl  = *on;
       dspIDCTran = *off;
       dspIDCCtl  = *off;
       Write RcdIDCTran;
      /End-Free
     p ClrSubFiles     e

     //===================================================================
     // Load Prior Transactional Submissions Grid Procedure
     //===================================================================
     p LoadTransGrd    b
      /Free
       rrnIDCTran = *zeros;
       clrIDCsfl = *Off;
       dspIDCTran = *On;
       dspIDCCtl  = *On;
       scRRN  = 0;

       Setll *hival pcc01;
       Read wmidcscl2;
       Dow Not %Eof(wmidcscl2);
          If %Subst(CCSubFilID:1:8) = 'IDCTRANS';
             scBchID = CCBchID;
             scSubFilID = CCSubFilID;
             xxDate = %Dec(CCsubDate:8:0);
             scSubDt = cvtToDate(xxDate);
             Select;
             When CCsubTyCd = 'O';
                scSubFtyCd = 'Original';
             When CCsubTyCd = 'R';
                scSubFtyCd = 'Replace';
             Endsl;
             Select;
             When CCrptQtrCd = '1';
                scPrRptQtr = '1st';
             When CCrptQtrCd = '2';
                scPrRptQtr = '2nd';
             When CCrptQtrCd = '3';
             scPrRptQtr = '3rd';
             When CCrptQtrCd = '4';
                scPrRptQtr = '4th';
             Endsl;

             Select;
             When CCrunt = 'O' and CCsts = 'S';
                scStat = 'Sent';
             When CCrunt = 'S' and CCsts = 'S';
                scStat = 'Resent';
             When CCrunt = 'R' and CCsts = 'R';
                scStat = 'Replaced';
             When CCrunt = 'D' and CCsts = 'D';
                scStat = 'Deleted';
             Other;
                scStat = 'Not sent';
             Endsl;

             scRunType = CCrunT;
             scCarrGC = CCcarGpCd;
             rrnIDCTran = rrnIDCTran + 1;
             scRRN  = ccFds.rrn;
             write sflIDCTran;
          Endif;
          Read wmidcscl2;
       Enddo;
       rrnIDCTran = 1;
      /End-Free
     p LoadTransGrd    e

