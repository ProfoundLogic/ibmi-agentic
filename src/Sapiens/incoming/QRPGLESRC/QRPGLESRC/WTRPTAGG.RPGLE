     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H***********************************************************************
     H/TITLE WTRPTAGG - PROMPT FOR AGING REPORT
     H***********************************************************************
     H*                                                                    *
     H*  DESCRIPTION:                                                      *
     H*    This program prompts for policy effect dt range, balance date,  *
     H*    sort sequence, account type and status, audit status and A/R    *
     H*    codes. It then passes this info on so as to produce a corresp-  *
     H*    onding aging report.                                            *
     H*                                                                    *
     H*    Aging report was established as a separate report, taken from   *
     H*    Employer Late Notice report.                                    *
     H*                                                                    *
     H*  CALLED FROM:                                                      *
     H*    Menu                                                            *
     H*  CALLS TO:                                                         *
     H*    W7290C1                                                         *
     H*                                                                    *
     H*  CREATED: 06/17/16  PROGRAMMER- C:308 PROJECT: WC2425              *
     H*                                                                    *
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 05/13/25  C:291 Support: 119048                                    *
     H*                 Increased length of Choices selection variable     *
     H*                 scARCodSel on A/R codes select box in display file.*
     H*                 Program recompile only.                            *
     H*                                                                    *
     H* 02/17/17  C:306 Support 45613                                      *
     H*                 Adding a conditional check to fix the issue.       *
     H*                                                                    *
     H***********************************************************************
     fWTRPTAGG  cf   e             workstn extfile('WTRPTAGG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwtctl     if   e           k disk    Rename(pct01:pct00)
     fwxgsl     if   e           k disk    Rename(pgs01:pgs00)
     fwtavl     if   e           k disk    Rename(pav01:pav00)
     fwtabl     if   e           k disk    include(ptt01)

     dpsds           esds                  extname(szq1p)

      * Prototype Declaration
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter

      * Report Submit Program CL
     d AggRptPr        pr                  ExtPgm('W7290C1')
     d outCo#                         3
     d outFnd                         3
     d outPolEftDt                    8
     d outPolExpDt                    8
     d outBalDte                      8
     d outOutQ                       10
     d outMode                        1
     d outPrmSrt                      2
     d outSecSrt                      2
     d outEmpFr                       2
     d outEmpTo                       2
     d outEmpNbr                      9
     d outEmpNbrTo                    9
     d outAgtNmFr                     2
     d outAgtNmTo                     2
     d outAgtNbrFr                    5
     d outAgtNbrTo                    5
     d outAccSts                      1
     d outAccType                     1
     d outAgRpt                       1
     d outAudSts                      1
     d outCode                       60
     d outCdeSel                      1
     d outMinCrBal                    9
     d outMinDbBal                    9
     d outEpActSts                    1
     d outActStsCd                   10
     d outCatSel                      1
     d outARCatTyp                   10
     d outBillType                    1
     d outARPosdte                    1
     d dsActSts        DS
     d dsActSts1                      2
     d dsActSts2                      2
     d dsActSts3                      2
     d dsActSts4                      2
     d dsActSts5                      2
      * Load Activity Status Codes
     d LoadActStsCd    pr

      * Load A/R category Types
     d LoadARCatTyp    pr

      * Load A/R Codes
     d LoadARCodes     pr

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process Screen input values
     d ProcessScreen   pr

     d SA030R          pr                  extpgm('SA030R')
     d outsuffix                      3

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

     d WtRptAggR       pr                  Extpgm('WTRPTAGG')
     d  inExt                         1A
     d  inCnl                         1A

     d WtRptAggR       pi
     d  inExt                         1A
     d  inCnl                         1A

     d intPos          S              2  0
     d strMsg          s            100
     d strrptl         s            200    inz(*Blanks)
     d strARCdDsc      s             30    inz(*Blanks)
     d strARTyDsc      s             30    inz(*Blanks)
     d strAVTDsc       s             30    inz(*Blanks)
     d outsuffix       s              3
     d outCo#          s              3
     d outFnd          s              3
     d outPolEftDt     s              8
     d outPolExpDt     s              8
     d outBalDte       s              8
     d outCode         s             60
     d outOutQ         s             10
     d outMode         s              1
     d outPrmSrt       s              2
     d outSecSrt       s              2
     d outAgtNmFr      s              2
     d outAgtNmTo      s              2
     d outAgtNbrFr     s              5
     d outAgtNbrTo     s              5
     d outEmpFr        s              2
     d outEmpTo        s              2
     d outEmpNbr       s              9
     d outEmpNbrTo     s              9
     d outAccSts       s              1
     d outAccType      s              1
     d outAgRpt        s              1
     d outAudSts       s              1
     d outCdeSel       s              1
     d outMinCrBal     s              9
     d outMinDbBal     s              9
     d outEpActSts     s              1
     d outActStsCd     s             10
     d outCatSel       s              1
     d outARCatTyp     s             10
     d outBillType     s              1
     d outARPosdte     s              1
     d intCo#          s              3  0
     d intGrp          s              3  0
     d intGrpCnt       s              5  0
     d isVisbEmp       s              1n

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // Validate the screen fields for error
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n
      /Free
       // initialize the error indicator
       erBalDte  = *Off;
       erAgrpt   = *Off;
       erPolEftDt= *Off;
       erPolExpDt= *Off;
       erAccType = *Off;
       erAccSts  = *Off;
       erAudSts  = *Off;
       erPrmSrt  = *Off;
       erSecSrt  = *Off;
       erSecSrt  = *Off;
       erEmpTo   = *Off;
       erEmpNbrTo= *Off;
       erAgtNmTo  =*Off;
       erAgtNmFr  =*Off;
       erEmpFr   = *Off;
       erEmpTo   = *Off;
       erArPosDte =*Off;
       erCatSel   =*Off;
       erCdeSel   =*Off;
       erARCatTyp =*Off;
       erARCodes  =*Off;
       erCdeSel   =*Off;
       erActStsCd =*Off;
       erEpActSts =*Off;
       msBalDte  = *Blanks;
       msAgrpt   = *Blanks;
       msPolEftDt= *Blanks;
       msPolExpDt= *Blanks;
       msAccType = *Blanks;
       msAccSts  = *Blanks;
       msAudSts  = *Blanks;
       msPrmSrt  = *Blanks;
       msSecSrt  = *Blanks;
       msSecSrt  = *Blanks;
       msEmpTo   = *Blanks;
       msEmpNbrTo= *Blanks;
       msAgtNmTo  =*Blanks;
       msAgtNmFr  =*Blanks;
       msEmpFr   = *Blanks;
       msEmpTo   = *Blanks;
       msArPosDte =*Blanks;
       msCatSel   =*Blanks;
       msCdeSel   =*Blanks;
       msARCatTyp =*Blanks;
       msARCodes  =*Blanks;
       msCdeSel   =*Blanks;
       msActStsCd =*Blanks;
       msEpActSts =*Blanks;

        // validate Policy effective date
       erPolEftDt = ValidDate(scPolEftDt:strMsg);
       If erPolEftDt = *On;
          msPolEftDt = 'WCR0500';
          Return *off;
       EndIf;

        // validate Policy Expiry date
       erPolExpDt = ValidDate(scPolExpDt:strMsg);
       If erPolExpDt = *On;
          msPolExpDt = 'WCR0500';
          Return *off;
       EndIf;

       erPolEftDt = AscendDates(scPolEftDt:scPolExpDt:strMsg);
       If erPolEftDt = *On;
          erPolExpDt = erPolEftDt;
          msPolEftDt = 'WCR0702';
          msPolExpDt = 'WCR0702';
          Return *off;
       EndIf;

        // validate Balance date
       erBalDte   = ValidDate(scBalDte:strMsg);
       If erBalDte = *On;
          msBalDte = 'WCR0500';
          Return *off;
       EndIf;

        // validate Account Type
       If scAccType = *Blanks;
          erAccType = *On;
          msAccType = 'WCR0503';
          Return *off;
       EndIf;

        // validate Account Status
       If scAccSts  = *Blanks;
          erAccSts  = *On;
          msAccSts  = 'WCR0503';
          Return *off;
       EndIf;

        // validate Audit Status
       If scAudSts  = *Blanks;
          erAudSts  = *On;
          msAudSts  = 'WCR0503';
          Return *off;
       EndIf;

        // validate Report Type
       If scAgRpt   = *Blanks;
          erAgRpt   = *On;
          msAgRpt   = 'WCR0503';
          Return *off;
       EndIf;

        // validate Primary Sort
       If scPrmSrt  = *Blanks;
          erPrmSrt  = *On;
          msPrmSrt  = 'WCR0503';
          Return *off;
       EndIf;

        // validate AR posting date selection
       If scArPosDte <> *Blank and scArPosDte <> 'Y';
          erArPosDte = *On;
          msArPosDte = 'WCR1241';
          Return *off;
       EndIf;

        // validate Employer range either name or number
       If scEmpTo <> *Blanks;
          scEmpFr = Upper(scEmpFr);
          scEmpTo = Upper(scEmpTo);
          If scEmpNbrTo <> *zeros;
             erEmpTo    = *On;
             erEmpNbrTo = *On;
             erEmpNbr   = *On;
             erEmpNbrTo = *On;
             msEmpTo    = 'WCR0918';
             msEmpNbrTo = 'WCR0918';
             msEmpNbr   = 'WCR0918';
             msEmpNbrTo = 'WCR0918';
             Return *off;
          EndIf;
       EndIf;

        // validate Employer name range
       If scEmpFr > scEmpTo;
          erEmpFr   = *On;
          erEmpTo   = *On;
          msEmpFr   = 'WCR0750';
          msEmpTo   = 'WCR0750';
          Return *off;
       EndIf;

        // validate Employer number range
       If scEmpNbr > scEmpNbrTo;
          erEmpNbr   = *On;
          erEmpNbrTo = *On;
          msEmpNbr   = 'WCR0750';
          msEmpNbrTo = 'WCR0750';
          Return *off;
       EndIf;

        // validate Agent range either name or number
       If scAgtNmTo <> *Blanks;
          scAgtNmFr  = Upper(scAgtNmFr);
          scAgtNmTo  = Upper(scAgtNmTo);
          If scAgtNbrFr <> *zeros;
             erAgtNmTo  = *On;
             erAgtNmFr  = *On;
             erAgtNbrTo  = *On;
             erAgtNbrFr  = *On;
             msAgtNmTo  = 'WCR0918';
             msAgtNmFr  = 'WCR0918';
             msAgtNbrTo  = 'WCR0918';
             msAgtNbrFr  = 'WCR0918';
             Return *off;
          EndIf;
       EndIf;

        // validate Agent name range
       If scAgtNmFr > scAgtNmTo;
          erAgtNmFr = *On;
          erAgtNmTo = *On;
          msAgtNmFr = 'WCR0750';
          msAgtNmTo = 'WCR0750';
          Return *off;
       EndIf;

        // validate Agent number range
       If scAgtNbrFr > scAgtNbrTo;
          erAgtNbrFr = *On;
          erAgtNbrTo = *On;
          msAgtNbrFr = 'WCR0750';
          msAgtNbrTo = 'WCR0750';
          Return *off;
       EndIf;

        // validate Employer Activity Status code
       If scEpActSts <> *Blanks and scActStsCd = *Blank;
          erActStsCd = *On;
          msActStsCd = 'WCR1130';
          Return *off;
       EndIf;

       //validate blank is not selected with other values
       If scEpActSts <> *Blanks;
          If scActStsCd <> *Blanks;
             intPos = %scan(',':scActStsCd);
             If intPos = 1 ;
                If %len(%trim(scActStsCd)) > 1;
                   erActStsCd = *On;
                   msActStsCd = 'WCR4820';
                   Return *off;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

       // Validate and load activity status code
       //*** Beg Add ***** 02/17/17 ***************//
       If scActStsCd <> *blanks;
       //*** End Add ***** 02/17/17 ***************//
       If %len(%trim(scActStsCd)) >= 15;
          erActStsCd = *on;
          msActStsCd = 'WCR4819';
          Return *off;
       Else;
          dsActSts1 = ParseString(scActStsCd:',':1);
          dsActSts2 = ParseString(scActStsCd:',':2);
          dsActSts3 = ParseString(scActStsCd:',':3);
          dsActSts4 = ParseString(scActStsCd:',':4);
          dsActSts5 = ParseString(scActStsCd:',':5);
          If ParseString(scActStsCd:',':6) <> *Blanks;
             erActStsCd = *on;
             msActStsCd = 'WCR4819';
             Return *off;
          EndIf;
       EndIf;
       //*** Beg Add ***** 02/17/17 ***************//
       EndIf;        // scActStsCd <> *blanks;
       //*** End Add ***** 02/17/17 ***************//

        // validate AR codes are selected with category select
       If scCatSel <> *Blanks and scARCatTyp = *Blank;
          erARCatTyp = *On;
          msARCatTyp = 'WCR1130';
          Return *off;
       EndIf;

       If scCatSel = *Blanks and scARCatTyp <> *Blank;
          erARCatTyp = *On;
          msARCatTyp = 'WCR1128';
          Return *off;
       EndIf;

        //validate blank is not selected with other values in A/R category
       If scCatSel <> *Blanks;
          intPos = %scan(',':scARCatTyp);
          If intPos = 1 ;
             If %len(%trim(scARCatTyp)) > 1;
                erARCatTyp = *On;
                msARCatTyp = 'WCR4820';
                Return *off;
             EndIf;
          EndIf;
       EndIf;

       If %len(%trim(scARCatTyp)) > 19;
          erARCatTyp = *On;
          msARCatTyp = 'WCR4818';
          Return *off;
       EndIf;

       intPos  = *zeros;

        // validate AR codes are selected with category select
       If scCdeSel <> *Blanks and scARCodes = *Blank;
          erARCodes  = *On;
          msARCodes  = 'WCR1130';
          Return *off;
       EndIf;

       If scCatSel = *Blanks and scARCatTyp <> *Blank;
          erARCodes  = *On;
          msARCodes  = 'WCR1128';
          Return *off;
       EndIf;

       //validate blank is not selected with other values in A/R codes
       If scCdeSel <> *Blanks;
          intPos = %scan(',':scARCodes);
          If intPos = 1 ;
             If %len(%trim(scARCodes)) > 1;
                erARCodes  = *On;
                msARCodes  = 'WCR4820';
                Return *off;
             EndIf;
          EndIf;
       EndIf;

       If %len(%trim(scARCodes)) > 90;
          erARCodes = *On;
          msARCodes = 'WCR4816';
          Return *off;
       EndIf;


       Dow %scan(',':%trimR(scARCodes)) > *zeros;
          scARCodes = %Replace ('':scARCodes:%scan(',':%trimR(scARCodes)):1);
       Enddo;

       If %Subst(scARCodes:59:2) = *Blanks;
          scARCodes = %Replace ('@@':scARCodes:59:2);
       EndIf;

       Dow %scan(',':%trimR(scARCatTyp)) > *zeros;
          scARCatTyp = %Replace ('':scARCatTyp:%scan(',':%trimR(scARCatTyp)):1);
       Enddo;

       Return *On;

      /End-Free

     p ValidPnl        e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInitVal();

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       // Load Output Queue for user.
       outOutQ = GetUsrOutq(q1user);

       Setll (intCo#:*loval) pgs00;
       ReadE intCo# pgs00;
       Dow not %eof(wxgsl);
          If GSIeFl = 'I';
             intGrp = GSfnd;
             intGrpCnt = intGrpCnt + 1;
          Endif;

          If intGrpCnt > 1;
             isVisbEmp = *on;
             Leave;
          EndIf;
            ReadE intCo# pgs00;
       EndDo;

       // Loading the Activity Status Codes.
       LoadActStscd();

       // Loading the A/R category type values.
       LoadARCatTyp();

       // Loading the A/R Codes values.
       LoadARCodes();

      /End-Free
     p ProgramStart    e
       //===================================================================
       // LoadInitVal - Initializing variables
       //===================================================================
     p LoadInitVal     b

      /Free
       Clear RcdAgRptR;
       scBalDte  = DateTo8(%Date());

       Sa030r(outSuffix);
       strrptl = 'PDSRPT' + outSuffix ;
       scRptLib = 'This report will produce output file WFAGP in library '+
                  %trim(strrptl) + '.';
      /End-Free

     p LoadInitVal     e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
             When BtnSubmit = *On;
                If ValidPnl() = *On;
                   ProcessScreen();
                EndIf;

             //Do nothing - inPolExpDt lick event
             When tabClick = *on;
                tabClick = *off;

             When BtnRefresh = *On;
                BtnRefresh =*Off;
                ProgramStart();

          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = 830;

          Write rcdComPnl;
          Write RcdTabAgR;
          Exfmt RcdAgRptR;
          Read rcdComPnl;
          Read RcdTabAgR;

       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // Load Activity Status Codes
       //===================================================================
     p LoadActStscd    b
     d LoadActStscd    pi

      /Free
       scAvtsel  = 'All';
       scAvtVal  = *Blanks;
       strAvtDsc = *Blanks;

       // Read WTAVP file from starting
       Setll intCo# pav00;
       ReadE intCo# pav00;
       Dow not %Eof(wtavl);
          If AVstat = 'A';
             strAvtDsc = TitleCase(%xlate(',':' ':AVdesc));
             scAvtSel = %trim(scAvtSel)+','+%trim(AVkey)+'-'+
               %trim(strAvtDsc);
             scAvtVal = %trim(scAvtVal)+','+%trim(AVkey);
          endif;
       ReadE intCo# pav00;
       enddo;

      /End-Free

     p LoadActStscd    e
       //===================================================================
       // Load ARCodes to Screen
       //===================================================================
     p LoadARCodes     b
     d LoadARCodes     pi

      /Free
       scARCodSel = 'All';
       scARCodVal = *Blanks;
       strARCdDsc = *Blanks;

       // Read WTTTP file from starting
       Setll intCo# ptt01;
       ReadE intCo# ptt01;
       Dow not %Eof(wtabl);
          If TTstat = 'A';
             strARCdDsc = TitleCase(TTdesc);
             scARCodSel= %trim(scARCodSel)+','+%trim(TTkey)+'-'+
              %trim(strARCdDsc);
             scARCodVal = %trim(scARCodVal)+','+%trim(TTkey);
          endif;
       ReadE intCo# ptt01;
       enddo;

      /End-Free

     p LoadARCodes     e
       //===================================================================
       // Load ARCategories to Screen
       //===================================================================
     p LoadARCatTyp    b
     d LoadARCatTyp    pi

      /Free
       scARTypSel = 'All';
       scARTypVal = *Blanks;
       strARTyDsc = *Blanks;

       // Read WTCTL file from starting
        Setll intCo# pct00;
        ReadE intCo# pct00;
        Dow not %Eof(wtctl);
           If CTstat = 'A';
              strARTyDsc = TitleCase(CTdesc);
              scARTypSel = %trim(scARTypSel)+','+%trim(CTctyp)+'-'+
                           %trim(strARTyDsc);
              scARTypVal = %trim(scARTypVal)+','+%trim(CTcTyp);
           EndIf;
        ReadE intCo# pct00;
        EndDo;
      /End-Free

     p LoadARCatTyp    e
       //===================================================================
       // Process user inputs on screen
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi
      /Free

       // Move the parameter values to call the report CL program
        outCo#          = %editC(intCo#:'X');
        outFnd          = *zeros;
        outPolEftDt     = %editC(scPolEftDt:'X');
        outPolExpDt     = %editC(scPolExpDt:'X');
        outBalDte       = %editC(scBalDte:'X');
        outCode         = %Subst(scARCodes:1:60);
        outMode         = 'D';
        outPrmSrt       = scPrmSrt;
        outSecSrt       = scSecSrt;
        outAgtNmFr      = scAgtNmFr;
        outAgtNmTo      = scAgtNmTo;
        outAgtNbrFr     = %editC(scAgtNbrFr:'X');
        outAgtNbrTo     = %editC(scAgtNbrTo:'X');
        outEmpFr        = scEmpFr;
        outEmpTo        = scEmpTo;
        outEmpNbr       = %editC(scEmpNbr:'X');
        outEmpNbrTo     = %editC(scEmpNbrTo:'X');
        outAccSts       = scAccSts;
        outAccType      = scAccType;
        outAgRpt        = scAgRpt;
        outAudSts       = scAudSts;
        outCdeSel       = scCdeSel;
        outMinCrBal     = %editC(scMinCrBal:'X');
        outMinDbBal     = %editC(scMinDbBal:'X');
        outEpActSts     = scEpActSts;
        outActStsCd     = dsActSts;
        outCatSel       = scCatSel;
        outARCatTyp     = %Subst(scARCatTyp:1:10);
        outBillType     = scBillType;
        outARPosdte     = scARPosDte;
        ExitNow         = *on;

        AggRptpr(outCo#:outFnd:outPolEftDt:outPolExpDt:outBalDte:outOutQ:
         outMode:outPrmSrt:outSecSrt:outEmpFr:outEmpTo:outEmpNbr:outEmpNbrTo:
         outAgtNmFr:outAgtNmTo:outAgtNbrFr:outAgtNbrTo:outAccSts:outAccType:
         outAgRpt:outAudSts:outCode:outCdeSel:outMinCrBal:outMinDbBal:
         outEpActSts:outActStsCd:outCatSel:outARCatTyp:outBillType:outARPosdte);

      /End-Free
     p ProcessScreen   e
      /Free
