     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WI3RDPRTY - 3rd Party Payment Processing
     H DFTNAME(WI3RDPRTY)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR':'YAJL')
      *====================================================================*
      *  Created on 12/03/2020 by c:33 for Project SY407                   *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  04/05/22 C:33  Support: 96520                                     *
      *                 Allow for deletions to process without SR payment  *
      *                  edits                                             *
      *                                                                    *
      *  03/17/22 C:33  Support: 101510                                    *
      *                -Only include rcdComPnl when opening action panel   *
      *                -ProcessThirdPartyPayments is now in WIPOLPMT       *
      *                                                                    *
      *  09/16/21 C:300 Support: 98005                                     *
      *                 Opening Third Party Payments screen on GO error    *
      *                 Fix: Just Recompiled over recent WTACTPNL changes  *
      *                                                                    *
      *====================================================================*
     fwi3rdprty cf   e             workstn extfile('WI3RDPRTY')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflIncPmnt:rrnIncPmnt)
     fwtactpnl  cf   e             workstn
     f***  Begin Add  ***  03/17/22  ********************
     f                                     include(rcdComPnl)
     f***  End   Add  ***  03/17/22  ********************
     f                                     handler('PROFOUNDUI(HANDLER)')

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     dmode             s              1    inz
     drrn              s              9  0 inz

     dintCo#           s              3  0 inz
     dstrBrand         s             80    inz
     dstrJSON          s               a   Len(8500000) varying
     dstrErrorMsg      s            500a   varying
     dstrParms         s               a   Len(8500000) varying
     dstrPgm           s             10    inz
     dstrRequest       s            256    inz
     dstrRtnCode       s             10

     ddsInv#           ds                  inz qualified
     d company                        3  0
     d group                          3  0
     d employer                       9  0
     d division                       5  0
     d year                           3  0
     d number                        10  0

      * system data structure
     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0

     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrJson
     d/copy sPrModule
     d/copy sPrRtvBrnd
     d/copy sPrRunCmd
     d/copy sPrString
     d/copy sPrSql
     d/copy wPrEmplr
     d/copy wPrGroup
     d/copy wPrUsrPrms

     d/include yajl_h

     dRequestPgm       pr                  extpgm(strPgm)
     d outRequest                   256    const
     d outUser                       10    const
     d outParms                        a   Len(8500000) Varying
     d inJSON                          a   Len(8500000) Varying
     d ioRtnCode                     10

      *Entry
     dWI3RDPARTY       pr                  extpgm('WI3RDPARTY')
     d                                9    options(*nopass)
     d                                1    options(*nopass)
     d                               10    options(*nopass)
     d                                5    options(*nopass)
     dWI3RDPARTY       pi
     d@@rrn                           9    options(*nopass)
     d@@mode                          1    options(*nopass)
     d@@next                         10    options(*nopass)
     d@@TabInd                        5    options(*nopass)

      /Free
       Exec SQL set option commit = *NONE;
       RtvBrnd('I':strBrand);

       // start program processing
       Exsr ProgramStart;
       Main();

       *InLr = *On;
       Return;

       //===================================================================
       // Initialize
       //===================================================================
       Begsr ProgramStart;

        // Program Mode.
        If %parms >= 2;
           mode = @@mode;
        Endif;

        isDspMode = *Off;
        Select;
        When mode = 'C'; // Change Mode
        When mode = 'D'; // Display Mode
           isDspMode = *On;
        EndSl;

        // Tab Index Mode.
        TabIndex = 0;

        // Get Company no.
        intCo# = UserCo#(q1user);

        // Get Company name
        scCompName = getCompNam(intCo#);

        // Panel Heading
        scPnlHdg = 'Third Party Payments';

        //***  Begin Del  ***  03/17/22  *************
        //Check for applicable options
        //scMenuChoi = *blank;
        //scMenuValu = *blank;
        //***  End   Del  ***  03/17/22  *************
       Endsr;
      /End-free

      //===================================================================
     PIncompletePaymentsLoad...
     P                 b
     DIncompletePaymentsLoad...
     D                 pi

     ddsInv            ds                  inz qualified
     d group                          3  0
     d employer                       9  0
     d division                       5  0
     d year                           3  0
     d number                        40
      /Free
       strPgm = 'WISRPAY';
       scStubHdg = 'Incomplete Payments';
       selIncPmnt = *on;
       dspIncPmnt = *on;
       clrIncPmnt = *off;
       Exsr ListGet;
       Exsr ListParse;
       //===========================================
       Begsr ListGet;
        // Build json to request incomplete payments - WPISRPAY/ListIncompleteSRPayments
        strParms = '{}';
        RequestPgm('ListIncompleteSRPayments'
         :q1User:strParms:strJson:strRtnCode);
        If strRtnCode <> *blanks;
           Leavesr;
        Endif;
        // The hub processor would normally add the brackets
        strJson = '{' + %trim(strJson) + '}';
       Endsr;
       //===========================================
       Begsr ListParse;
        // Parse json to build incomplete payments
        Reset i0;
        node0 = yajl_string_load_tree(strJson:strErrorMsg);
        Dow YAJL_object_loop(node0:i0:key:val);
           Select;
           When key = 'incompleteInvoices';
              Reset i1;
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_array_loop(node1:i1:node2);
                 Reset dsInv;
                 Reset i2;
                 Dow YAJL_object_loop(node2:i2:key:val);
                    Select;
                    When key = 'group';
                       dsInv.group = %int(YAJL_get_string(val));
                    When key = 'employer';
                       dsInv.employer = %int(YAJL_get_string(val));
                    When key = 'division';
                       dsInv.division = %int(YAJL_get_string(val));
                    When key = 'year';
                       dsInv.year = %int(YAJL_get_string(val));
                    When key = 'invoice#';
                       dsInv.number = YAJL_get_string(val);
                    Endsl;
                 Enddo;
                 Exsr LoadSubfile;
              Enddo;
           Endsl;
        Enddo;
       Endsr;
       //===========================================
       Begsr LoadSubfile;
        scPmntName = GetEmpName(dsInv.employer:dsInv.division:rrn:dsInv.group);
        scPmntNbr = %editc(dsInv.group:'X') + '-' + %editc(dsInv.employer:'X') +
         '-' + %editc(dsInv.division:'X');
        scPmntYr = dsInv.year;
        scPmntInv# = dsInv.number;
        rrnIncPmnt += 1;
        Write sflIncPmnt;
        selIncPmnt = *off;
       Endsr;
      /End-free
     PIncompletePaymentsLoad...
     P                 e

      //===================================================
      // Process notifications
      //===================================================
     pIncompletePaymentsProcess...
     p                 b
     dIncompletePaymentsProcess...
     d                 pi              n

     ddecPeriod        s              8  0
     dstrError         s            256

     ddsWASRP          ds                  inz qualified
     d fyr                            3  0
     d rpMY                           5  0
     d toDt                           7  0

      /Free
       Select;
       When TabIndex <> 0;
          Return *off;
       Other;
          Dou %eof;
             Readc sflIncPmnt;
             If %eof;
                Leave;
             EndIf;
             Select;
             When isDltInv = *On;
                isDltInv = *Off;
                If PopTwoBtn('Invoice: ' + scPmntInv#:'Confirm Deletion':
                 'Delete':'Cancel');
                   Exsr DeleteData;
                Endif;
                Return *on;
             EndSl;
          Enddo;
       EndSl;
       Return *off;

       //--------------------------------
       Begsr DeleteData;
        // Build json to call WIPOLPMT/ProcessThirdPartyPayment as FAILURE
        //***  Begin Add  ***  03/17/22  ********************
        strPgm = 'WIPOLPMT';
        //***  End   Add  ***  03/17/22  ********************
        strParms = '{';
        strParms += BuildJsonNode('status':'FAILURE') + ', ';
        strParms += BuildJsonNode('invoiceNumber':scPmntInv#);
        //***  Begin Add  ***  04/05/22  ********************
        strParms += ', ' + BuildJsonNode('altRequest':'Delete3rdPartyPayment');
        //***  End   Add  ***  04/05/22  ********************
        strParms += '}';
        RequestPgm('ProcessThirdPartyPayment'
         :q1User:strParms:strJson:strRtnCode);
        // The hub processor would normally add the brackets
        strJson = '{' + %trim(strJson) + '}';
        Select;
        When strRtnCode <> *blanks;
           Exsr ProcessError;
        Other;
           PopOneBtn('Invoice: ' + scPmntInv#:'Invoice Deleted');
        Endsl;
       Endsr;

       //--------------------------------
       Begsr ProcessError;
        // Parse json to determine error message
        Reset i0;
        node0 = yajl_string_load_tree(strJson:strErrorMsg);
        Dow YAJL_object_loop(node0:i0:key:val);
           Select;
           When key = 'errorMsg';
              Reset i1;
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_array_loop(node1:i1:val);
                 strError = yajl_get_string(val);
                 PopOneBtn('Deletion Error: ' + strError
                  :'Invoice: ' + scPmntInv#);
                 Leave;
              Enddo;
           Endsl;
        Enddo;
       Endsr;
      /End-free
     pIncompletePaymentsProcess...
     p                 e

       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

     disEof            s              1n   inz('1')
     disExitNow        s              1n   inz
     dsaveTabIndex     s                   like(tabIndex)

      /Free

       // Process until exit button is pressed.
       Dou isExitNow;
          Exsr LoadTabs;
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Process Incomplete Payments
          When IncompletePaymentsProcess() = *on;

          //***  Begin Del  ***  03/17/22  *************
          // Menu Choices
          //When scSelOpt <> *blanks;
          //   @@next = scSelOpt;
          //   Leave;
          //***  End   Del  ***  03/17/22  *************

          // do nothing - catch tab click event
          When tabClick = *on;
             If tabIndex <> saveTabIndex;
             Endif;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh = *Off;

          When btnSave = *on;
             btnSave = *off;

          Endsl;

          // Clear Subfiles
          ExSr ClrSubFiles;

       Enddo;

       //===================================================================
       // Clear Subfiles
       //===================================================================
       Begsr ClrSubFiles;

        rrnIncPmnt = 0;
        clrIncPmnt = *On;
        dspIncPmnt = *Off;
        write rcdIncPmnt;

       Endsr;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        //***  Begin Del  ***  03/17/22  *************
        //aMenHeight = 120;
        //***  End   Del  ***  03/17/22  *************

        Select;
        When TabIndex = 0;
           TabHeight = 895;
           APnlHeight = TabHeight + 60;
         //***  Begin Del  ***  03/17/22  *************
         //apFootHght = aMenHeight + 125;
         //apShowMenu = *on;
         //***  End   Del  ***  03/17/22  *************
           Write rcdComPnl;
           Write rcdPanel;
           Exfmt RcdIncPmnt;
           //Exfmt RcdStub;
           Read rcdComPnl;
           Read rcdPanel;
        Endsl;

       Endsr;

       //===================================================================
       // Load Screen(s)
       //===================================================================
       Begsr LoadTabs;

        saveTabIndex = tabIndex;

        Select;
        When TabIndex = 0;
           IncompletePaymentsLoad();
        EndSl;

       Endsr;

      /End-free
     P Main            E
