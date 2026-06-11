     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title SCRMARKLST - Prompt for Remarks Listing
     h dftname(SCRMARKLST)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 02/22/24                                         *
      //  Programmer: C:333                                            *
      //  Project   : WC2950 - Redesign of Remarks Listing Prompt      *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fSCRMARKLSTcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRmkKey:RrnKey)
     f                                     infds(fids01)
     fWTACTPNL  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

      * User Parameter File
     fSMUPL     if   e           k disk

      * Remark Type Table File
     fSTCTL1    if   e           k disk

      * Remark Summary Code Table File
     fSTSML     if   e           k disk

      * DARTS Security Master File
     fSMSLL     if   e           k disk

      * Remark Detail File
     fSDCMP     if   e             disk

      * Remark Detail File
       Dcl-F sdcmla disk usage(*INPUT) keyed usropn infds(DBFDS1)
        rename(pcm01:pcm0l);

     d charMsg         s            100
     d intCo#          s              3P 0
     d qqKey           s             40
     d qqUser          s             10
     d qqFrDt          s              7
     d qqToDt          s              7
     d qqSmCd          s              4
     d qqRmTy          s              5
     d qqDcId          s             12
     d qqType          s              1
     d qqDiTy          s              1
     d qqSort          s              2
     d ##rtcd          s              1
     d qqmode          s              1    Inz('D')
     d ##rrna          s              9    Inz('000000000')
     d ##rrn           s              9p 0

     d fids01        e ds                  extname(szq2p)

     d psds          esds                  extname(szq1p)

     d DBFDS1          ds
     d  dbrrn1               397    400b 0

      * Clear Error/Message fields
     d ClrErr          pr

      * Key definition convert Sub-Program
     d KeyConvert      pr                  extpgm('SRKCVT')
     d                               10
     d                               79
     d                                1

      * Intialize the Screen fields
     d LoadInVal       pr

      * Load Remark Key Search
     d LoadRemarkKey   pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Search Key
     d Search_Key      pr
     d                                9

      * Remarks Print - Submitter
     d Submitter_A     pr                  Extpgm('SC711C')
     d                               40
     d                               10
     d                                7
     d                                7
     d                                4
     d                                5
     d                               12
     d                                2
     d                               10
     d                                1

      * Remarks Print, Diary Items Only - Submitter
     d Submitter_D     pr                  Extpgm('SC716C')
     d                               40
     d                               10
     d                                7
     d                                7
     d                                4
     d                                5
     d                               12
     d                                1
     d                                2
     d                               10
     d                                1

     * Validate edits
     d ValidSel        pr              n

      *
      *
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrPolErr
     d/copy wPrUsrPrms

      /Free
       Chain (Q1user) psl01;
       If SLracc <> 'Y';
          // Start Of Program
          ProgramStart();

          // Main Procesing
          Main();
       Endif;
       *InLr = *on;

      /End-Free

       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
       // Initialize all the messageId and error indicators.
       errFrDt = *off;
       msFrDt = *blanks;
       errToDt = *off;
       msToDt = *blanks;
       errMrkType = *off;
       msRmrkType = *blanks;
       errDairyT1 = *off;
       msDairyTy1 = *blanks;
       errSortOpt = *off;
       msSortOpt = *blanks;
       errKey = *off;

      /End-Free

     p ClrErr          e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scKey = *Blanks;
       scUser = *Blanks;
       scSmCd = *Blanks;
       scFrDt = 0;
       scToDt = 0;
       scRmTy = *Blanks;
       scDocId = *Blanks;
       scSort = *Blanks;
       scRmrkType = *Blanks;
       scDairyTyp = *Blanks;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // Load Remark Detail Master Subfile
       //===================================================================
     p LoadRemarkKey   b
     d LoadRemarkKey   pi
     d ##pnam          s             10
     d ##key           s             79
     d ##rtcd          s              1
     d hldKey          s                   like(cmKey)

        Open sdcmla;

        Read pcm0l;
        Dow not %eof(sdcmla);
           If cmKey <> hldKey;
              scRmarkKey = cmKey;
              hldKey = cmKey;
              ##pnam = cmPNam;
              ##key = cmKey;
              ##rtcd = ' ';

              KeyConvert(##pnam:##key:##rtcd);

              If ##rtcd <> 'N';
                 scKeyDefi = ##key;
              Else;
                 scKeyDefi = ' ';
              Endif;

              scKeyRrn = DBrrn1;
              rrnKey = rrnKey + 1;
              Write sflRmkKey;
           Endif;
           Read pcm0l;
        Enddo;

        Close sdcmla;

        If rrnKey > *Zero;
           dspKeySfl = *On;
        Else;
           dspKeySfl = *Off;
        Endif;

        rrnKey = 1;

     p LoadRemarkKey   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnScrCan = *On;

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             ClrErr();
             // Validate Edits
             If ValidSel();
                Exsr Process;
                Leave;
             EndIf;

          When BtnScrCan = *on;
             Leave;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Intialize the Screen fields
             LoadInVal();
             // Clear Errors
             ClrErr();
             // Load Action panel with the Company Name.
             intCo# = userCo#(q1user);
             scCompName = GetCompNam(intCo#);

          When selKeySrch = *On;
             selKeySrch = *Off;
             Search_Key(##rrna);
             ##rrn = %int(##rrna);

             If ##rrn > *Zeros;
                Chain ##rrn pcm01;
                If %found(SDCMP);
                   scKey = cmKey;
                Endif;
             Endif;
sau          ClrErr();

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;
           PnlHeight = 400;
           APnlHeight = PnlHeight + 60;

           Write rcdComPnl;
           Write RcdPanel;

           Exfmt RcdRmrkLst;

           Read rcdComPnl;
           Read RcdPanel;

        EndSl;

       Endsr;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr Process;

        qqKey = scKey;
        qqUser = scUser;
        If scFrDt = *Zeros;
           qqFrDt = '0000101';
        Else;
           qqFrDt = %char(dtCYMD(scFrDt));
        Endif;
        If scToDt = *Zeros;
           qqToDt = '1991231';
        Else;
           qqToDt = %char(dtCYMD(scToDt));
        Endif;
        qqSmCd = scSmCd;
        qqRmTy = scRmTy;
        qqDcId = scDocId;
        qqType = scRmrkType;
        qqDiTy = scDairyTyp;
        qqSort = scSort;

        // Submit "Regular" DARTS Report - scRmrkType = A
        If scRmrkType = 'A';

           Submitter_A(qqKey:qqUser:qqFrDT:qqToDt:qqSmCd:qqRmTy:
                       qqDcId:qqSort:upOutQ:qqMode);
        Else;
        // Submit "Dairy" DARTS Report - scRmrkType = D
           Submitter_D(qqKey:qqUser:qqFrDT:qqToDt:qqSmCd:qqRmTy:
                       qqDcId:qqDiTy:qqSort:upOutQ:qqMode);

        Endif;

       EndSr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInVal();

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       Chain (q1user) pup01;

       Return;

      /End-Free

     p ProgramStart    e

       //===================================================================
       // Search_Key - Remarks Key Search
       //===================================================================
     p Search_Key      b
     d Search_Key      pi
     d @@RRNA                         9

       //Clear screens
       clrKey = *On;
       rrnKey = 0;
       dspKeySfl = *Off;
       dspKeyCtl = *Off;
       Write RcdKeySrch;
       dspKeySfl = *On;
       dspKeyCtl = *On;
       clrKey = *Off;

       //Main Loop
       Dou ExitNow= *On;
          //Load Remarks Detail Key subfile
          LoadRemarkKey();

          Exfmt RcdKeySrch;

          //Process Screens
          Exsr ProcSfl;
       Enddo;

       //===================================================================
       // Process Subfile
       //===================================================================
       Begsr ProcSfl;
        Readc SflRmkKey;

        Select;
         When BtnCancel = *On;
            @@rrna = '000000000';
            ExitNow = *On;

         When BtnSelect = *On;
            If Not %eof;
               @@rrna = %editc(scKeyRrn:'X');
            Endif;
            ExitNow = *On;

        Endsl;
       Endsr;

     p Search_Key      e
      //===================================================================
      // Validate the Return Fund Checks Register
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidSel        b
     d ValidSel        pi              n
     d isError         s               n   inz
      /Free

        isError = *off;

        // Validate Remark Key
        If scKey <> *Blanks;
           Open sdcmla;
           Chain (scKey) pcm0l;
           If not %found();
              errKey = *on;
              isError = *on;
           Endif;
           Close sdcmla;
        Endif;

        // Validate From Date - if entered
        If scFrDt <> 0;
           errFrDt = ValidDate(scFrDt:charMsg);
           If errFrDt = *on;
              msFrDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        // Validate To Date - if entered
        If scToDt <> 0;
           errToDt = ValidDate(scToDt:charMsg);
           If errToDt = *on;
              msToDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        // Dates must be in Ascending Order - if entered
        If (errFrDt <> *on) and (errToDt <> *on);
           If AscendDates(scFrDt:scToDt:charMsg) = *on;
              errFrDt = *on;
              errToDt = *on;
              msFrDt = charMsg;
              msToDt = charMsg;
              isError = *on;
           Endif;
        Endif;

        // Type of Remark must be A or D
        If scRmrkType = *blanks;
           errMrkType = *on;
           msRmrkType = 'WCR0503';
           isError = *on;
        Endif;

        // Type of Diary Remark must be N, C or B - if SCTYPE = D
        If scRmrkType = 'D' and scDairyTyp = *blanks;
           errDairyT1 = *on;
           msDairyTy1 = 'WCR0503';
           isError = *on;
        Endif;

        // Validate Sort Sequence
        If scSort = *blanks;
           errSortOpt = *on;
           msSortOpt = 'WCR0503';
           isError = *on;
        Endif;

       // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidSel        e


