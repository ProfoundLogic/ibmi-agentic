     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h/title MTPBULP1 - Uploaded Bill Transfer History
     h dftname(MTPBULTRHS)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/11/20                                         *
      *  Programmer: C:212                                            *
      *  Project   : MC437 - Redesign of PB Upload process            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
     *  05/30/24 C:342   Support: 115676                             *
     *                   DSPF Changes:                               *
     *                   1.Changes Label Heading from "Invalid Claim *
     *                    /Provider Errors" to "Invalid Claim /      *
     *                    Provider or Duplicate Payment Error"       *
      *                                                               *
     *  05/10/22 C:333   Project: MC442                              *
     *                   - Adding new Grid (sflDupVen) for Displaying*
     *                    duplicate Bill IDs.                        *
     *                   - Hide Duplicate Vendor Bill IDs count field*
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fmtpbultrhscf   e             workstn extfile('MTPBULTRHS')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTfrHst:rrnTfrHst)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      * company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * upload bill trans info
     ffau9p     if   e             disk    rename(pu901:pu90p)
      * upload bill trans info
     ffau9l4    if   e           k disk    rename(pu901:pu902)
     f                                     infds(dbfds)

     d* procedure declaration
     d*
     d* clear subfiles
     d clrSubFiles     pr

     d* get client id
     d getClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* load transfer history info
     d loadTrnfrHist   pr

     d* main processing
     d main            pr

     d* program start
     d programStart    pr
     d processMe                       n

     d/copy sPrGenPop
     d/copy sPrUsrPrms

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  rrnfau9p             397    400b 0

     d dspFilter       ds                  likerec(rcdTHFltr) inz

     d* array definitions
     d*
     d* standalone variables
     d*
     d intCo#          s              3  0 inz
     d outCltId        s              3    inz
     d kkName          s             10    inz
     d kkDate          s               d   inz
     d process         s               n   inz(*off)
     d isCallP         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isFirstLoad     s               n   inz(*off)
     d meetFltr        s               n   inz(*off)

     c* entry of program.
     c     *Entry        Plist
     c                   parm                    @@call            1            wdelp
     c                   parm                    @@mode            1            mode
     c                   parm                    @@tabind          5            Tab index

      /free

       // main program processing

       programStart(process);

       If process;
          main();
       Endif;

       *inlr = *on;
       Return;

      /end-free

       //==================================================================
       // CheckFilter - Determine if the records meet the Filter criteria
       //==================================================================
     p checkFilter     b
     d checkFilter     pi
      /free

       // check From Upload date
       kkDate = %Date(U9uldt:*cymd);
       If scFltFUpDt <> *loval and
         kkDate < scFltFUpDt;
          meetFltr = *off;
       Endif;

       // check To Upload date
       kkDate = %Date(U9uldt:*cymd);
       If scFltTUpDt <> *loval and
         kkDate > scFltTUpDt;
          meetFltr = *off;
       Endif;

      /end-free
     p checkFilter     e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p clrSubFiles     b
     d clrSubFiles     pi
      /Free

       rrnTfrHst = 0;
       clrTHSub = *On;
       dspTHSub = *Off;
       dspTHCtl = *Off;
       //***** Beg Add *** 05/10/22 ***************************************t
       dspThDup  = *Off;
       //***** End Add *** 05/10/22 ***************************************t
       Write rcdTfrHst;

      /End-Free
     p ClrSubfiles     e

       //===================================================================
       // LoadClrFlt - Clears the Filter
       //===================================================================
     p loadClrFlt      b
     d loadClrFlt      pi

      /free

       // if first time screen load,
       If isFirstLoad = *on;
          isFirstLoad = *off;
       EndIf;

       // display clear filter button
       If dspFilter.scFltFUpDt = *loval and
          dspFilter.scFltTUpDt = *loval;
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p loadClrFlt      e

       //===================================================================
       // Load Uploaded Bill Transfer History Information
       //===================================================================
     P loadTrnfrHist   b
     d loadTrnfrHist   pi
      /free

       // set subfile indicators
       clrTHSub = *Off;
       dspTHSub = *On;
       dspTHCtl = *On;
       //***** Beg Add *** 05/10/22 ***************************************t
       dspThDup  = *On;
       //***** End Add *** 05/10/22 ***************************************t

       // read file and load the subfile
       kkName = *blanks;

       Setll *start fau9l4;
       Read pu902;
       Dow not %Eof(fau9l4);
          kkName = U9name;

          // validate for filter
          meetFltr = *on;
          checkFilter();
          If meetFltr = *on;

             clear sflTfrHst;

             scTHMbrNm = U9name;
             scTHBatch = U9mbr#;
             Monitor;
              scTHUplDt = %Date(U9uldt:*cymd);
             On-error;
              scTHUplDt = *loval;
             EndMon;
             scTHUplTm = U9ultm;
             Monitor;
              scTHPrDt  = %Date(U9prdt:*cymd);
             On-error;
              scTHPrDt  = *loval;
             EndMon;
             scTHPrTm  = U9prtm;
             scTHTCnt  = U9tcnt;

             scTHRRN# = rrnfau9p;

             rrnTfrHst = rrnTfrHst + 1;
             If rrnTfrHst = 1;
                scTHRowSl = 'A';
             Else;
                clear scTHRowSl;
             Endif;

             Write sflTfrHst;
          Endif;

          Read pu902;
       Enddo;

       rrnTfrHst = 1;

      /end-free
     p loadTrnfrHist   e

      //===================================================================
      // main - Main processing
      //===================================================================
     p main            b
     d main            pi

      /free

       // process until exit button is pressed.
       Dou ExitNow = *On;

          // load clear filter button
          loadClrFlt();

          // clear Sub-files
          clrSubfiles();

          // load transfer history information
          loadTrnfrHist();

          // process transfer history screen
          Exsr exfmtscreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // filter processing.
          When btnFilter = *on;
             Dou btnFltrCan = *on or btnFltrApp = *on;

                ExFmt rcdTHFltr;
                If btnFltrApp = *on;
                   dspFilter.scFltFUpDt = scFltFUpDt;
                   dspFilter.scFltTUpDt = scFltTUpDt;
                Endif;

                If btnFltrCan = *on;
                   scFltFUpDt = dspFilter.scFltFUpDt;
                   scFltTUpDt = dspFilter.scFltTUpDt;
                Endif;
             Enddo;

          // filter clear processing
          When btnClrFltr = *on;
             Clear dspFilter;
             Clear rcdTHFltr;
             dspClrFltr = *off;

          When btnRefresh = *on;
             btnRefresh =*Off;

          // cancel button or F12 pressed
          When scTHCancel = *on;
             scTHCancel = *off;
             ExitNow = *on;

          // other options processing
          Other;
             //procOther();

          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr exfmtScreens;

       Select;

       When tabindex = 0;
          pnlHeight = 1050;
          aPnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdTabPan;
          Exfmt rcdTfrHst;
          Read rcdTabPan;
          Read RcdSysTbl;

       Other;
          // default at tab 0
          tabindex = 0;
       EndSl;

       Endsr;

      /end-free
     p main            e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p programStart    b
     d programStart    pi
     d processMe                       n

      /free

       processMe = *on;

       If q1prms >= 1;
          If @@call = 'P';
             isCallP = *on;
          Else;
             isCallP = *off;
          EndIf;
       Endif;

       // Program Mode
       If q1prms >= 5;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
       Endif;

       tabindex = 0;
       If q1prms >= 6;
          If @@tabind <> ' ';
             tabindex = %Int(@@tabind);
          Endif;
       Endif;

       // apShowMenu = *on;
       aMenHeight = 145;
       apFootHght = aMenHeight + 125;

       // get company of the user
       intCo# = getUserInfo(q1user);
       Chain intCo# pco00;
       If COname <> ' ';
          scCompName = COName;
       Endif;

       // Get Client ID
       getClientID(outCltId);

       If not %Open(STACTPNL);
          Open STACTPNL;
       Endif;

      /end-free
     p programStart    e

