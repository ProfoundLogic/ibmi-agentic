     h**********************************************************************
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h/title WTVPAYHIST - V-PAY History
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display V-Pay Offload History
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 9/03/2020 by C:062 for Project: WC2749
     h**********************************************************************
     h*  Revisions:
     h*
     h*  03/25/22 C:333 Project: SY434                                   *
     h*                 - Update Grids for Sorting Filtering Upgrade     *
     h**********************************************************************

     fwtvpayhistcf   e             workstn extfile('WTVPAYHIST')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflVPayHst:rrnVPayHst)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwcvpayhstlif   e           k disk    rename(pvh01:pvh00)
     f                                     InfDs(dbfds1)
     fsmcol     if   e           k disk    rename(pco01:pco00)

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d intCount        s              3  0
     d @@mode          s              1
     d @@TabIndex      s              5
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d clientID        s              3    inz('000')
     d extDate         s                   Like(VHextDate) Inz(*Hival)
     d extTime         s                   Like(VHextTime) Inz(*Hival)
     d ##CltId         s              3

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d/copy sPrDate
     d/copy SPrCompany
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
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %parms() >= 1;
          inmode = @@Mode;
       Endif;

       // Tab Index Mode.
       If %parms() >= 2;
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

      /end-free
     p ProgramStart    e
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

          // Load Payment Value 1 code grid
          LoadVPayHist();

          // Process Payment Value 1 code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          Other;

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
          Write rcdTabVPay;
          Exfmt rcdVPayHst;
          Read rcdComPnl;
          Read rcdTabVPay;
       EndSl;

       Endsr;

      /end-free
     p Main            e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnVPayHst  = 0;
       clrVPayHst = *On;
       dspVPayHst = *Off;
       Write rcdVPayHst;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // Load Payment Value 1 code grid
       //===================================================================
     p LoadVPayHist    b
     d LoadVPayHist    pi

      /free

       // clear the subfile.
       clear sflVPayHst;
       clrVPayHst = *Off;
       dspVPayHst = *On;
       clear dbRrn1;

       SetGT (extDate:extTime) pvh00;
       Read  pvh00;
       dow not %eof(wcvpayhstl);

          // Write VPay History

          scDocID    = VHdocName;
          scExtDate  = dtMDYY(VHextDate);
          scExtTime  = VHextTime;
          scHD001Cnt = VHhd001Cnt;
          scVR001Cnt = VHvr001Cnt;
          scVR002Cnt = VHvr002Cnt;
          scVR003Cnt = VHvr003Cnt;
          scTR999Cnt = VHtr999Cnt;
          scTotalCnt = VHtotalCnt;

          scVPayRRN  = dbRRN1;
          rrnVPayHst += 1;

          Write sflVPayHst;

          Read pvh00;
       Enddo;

      /end-free
     p LoadVPayHist    e
