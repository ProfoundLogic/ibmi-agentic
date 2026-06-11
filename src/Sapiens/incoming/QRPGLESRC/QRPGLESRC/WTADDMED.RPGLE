     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE WTADDMED - Generate Additional Medical Data
     H*****************************************************************
     H* Program Description:  This program populates the Additional   *
     H*                       medical data file.                      *
     H*                                                               *
     H* Called From:  STLETTER                                        *
     H* Calls To:     None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 06/12/2018 by c:62 for Project 102WHM
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 03/21/19 C:053 Support:77455
     H*
     H*****************************************************************
     Fsmcol     if   e           k Disk    Rename(pco01:pco00)
     Fsmupl1    if   e           k Disk
     Fsxaip     o    e             Disk    Rename(pai01:pai0p)
     Fwdell     if   e           k DisK    Rename(pel01:pel00)
     Fwdf3l     if   e           k Disk    Rename(pf301:pf300)
     Fwdpal     if   e           k Disk    Rename(ppa01:ppa00)
     Fwmaal1    if   e           k Disk
     Fwmagl     if   e           k Disk    Rename(pag01:pag00)
     Fwmahl     if   e           k Disk    Rename(pah01:pah00)
     Fwmall     if   e           k Disk    Rename(pal01:pal00)
     Fwmcdl     if   e           k Disk    Rename(pcd01:pcd00)
     Fwmcml1    if   e           k Disk
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
     Fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     Fwmnil     if   e           k Disk    Rename(pni01:pni00)
     Fwmrpl     if   e           k Disk    Rename(prp01:prp00)
     Fwmtpl1    if   e           k Disk
     Fwtabl     if   e           k Disk    Include(pca01)
     F                                     Include(pna01)
     F                                     Include(ppa01)
     Fwtgtl     if   e           k Disk    Rename(pgt01:pgt00)
     Fxmlprel1  if   e           k Disk    Rename(xp100:pxp01)

     D psds          esds                  ExtName(SZQ1P)

     d* Retrieve client id
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d   clientID                     3

     d* Get last check date
     d GetLastChkDate  pr             8  0
     d   inTranCode                   2    Const

     d* Get medical payment amount
     d GetMedPayAmt    pr            11  2

     d* Get payment Information
     d GetPayInfo      pr
     d   inPayVal1                    2    Const
     d   outWeeks                     5  0
     d   outDays                      5  0
     d   outFromDt                    8  0
     d   outToDt                      8  0
     d   outAmount                   11  2

     d* Get Title
     d GetTitle        pr            15
     d   inSuper                      3
     d   outSupName                  50

     d clientID        s              3    Inz(' ')
     d kkPcnt          s              1    Inz('1')
     d jurState        s              2    Inz('A1')
     d chrTbID         s                   Like(GTtbID)
     d chrKey          s                   Like(GTkey)
     d inXML           s                   Like(XPxml)
     d inPayVal1       s              2
     d outWeeks        s              5  0
     d outDays         s              5  0
     d outFromDt       s              8  0
     d outToDt         s              8  0
     d outAmount       s             11  2
     d outAmount2      s             11  2
     d outSupName      s             50
     d superName       s             50

     D/copy sPrModule
     D/copy sPrXML
     D/copy sPrDate
      *
      *
     C*
     C     *Entry        plist
     C                   parm                    inXML
     C*
      /free
       Chain inXML pxp01;
       If %Found(xmlprel1);
          Chain (XPco#:XPfnd:XPfYr:XPcase) pcm01;
          If %Found(wmcml1);
             ExSr PgmStart;
             ExSr WrtSXAIP;
          EndIf;
       EndIf;

       *inLR = *On;
      //-------------------------------------------------------------
      // PgmStart - Program Start
      //--------------------------------------------------------------
         Begsr PgmStart;

            // Get Client ID
            RtvCltID(clientID);

            // Access Company Master
            Chain (CMco#) pco00;

            // Access Group EDI File
            Chain (CMco#:CMfnd:CMjrSt) pf300;

            // Access Group Master
            Chain (CMco#:CMfnd) pfn00;

            // Access Employer Master and Employer Detail
            Chain (CMco#:CMfnd:CMemp#:CMdiv) pem00;
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pel00;

            // Access Carrier/State Reporting
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) prp00;

            // Access Agency Information
            SetGt  (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
            ReadPE (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
            DoW Not %Eof(wmahl) And AHstat = 'I';
               ReadPE (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
               If %Eof(wmahl);
                  AHagt# = 0;
                  AGname = ' ';
               EndIf;
            EndDo;
            // Now Access Agent Assignment File
            If AHagt# <> 0;
               Chain (CMco#:CMfnd:AHagt#) paa01;
               If %Found(wmaal1);
                  Chain (CMco#:AAfein:AAmod) pag00;
               EndIf;
            EndIf;

            // Access Employer Address Location
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMctg1) pal00;

            // Access Claim Additional Information
            Chain (CMco#:CMfnd:CMfYr:CMcase:jurState) pni00;

            // Access Treating Physician Information
            Chain (CMco#:CMfnd:CMfYr:CMcase) ptp01;

            // Access Claim Demographics
            Chain (CMss#) pcd00;

         EndSr;
      //-------------------------------------------------------------
      // WrtSXAIP - Write to Scriptura Additional Medical Info File
      //--------------------------------------------------------------
         Begsr WrtSXAIP;

            Clear pai0p;

            // Header Information
            AIxml    = XPxmlT;
            AIco#    = CMco#;
            AIfnd    = CMfnd;
            AIfYr    = CMfyr;
            AIcase   = CMcase;
            AIacl#   = CMacl#;
            AIclm#   = %EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') +
                       '-' + %EditC(CMcase:'X');
            AIeFei   = EMfein;

            // Claim Information
            AIcFsn   = CDfsn;
            AIcMid   = CDmid;
            AIcLsn   = CDlsn;
            AIcAd1   = CDadr1;
            AIcAd2   = CDadr2;
            AIcCty   = CDcty;
            AIcSt    = CDst;
            AIcZip   = CDzip;
            AIcArea  = CDarea;
            AIcPhn#  = CDphn#;
            AIwArea  = CDwAra;
            AIwPhn#  = CDwPh#;
            AIss#    = CDss#;
            AIsex    = CDsex;

            Monitor;
               AIdtob = %dec(%date(CDdtob:*cymd):*usa);
            On-Error;
               AIdtob = 0;
            EndMon;

            // Employer Information
            AIeNam   = EMname;
            AIeArea  = EMarea;
            AIePhn#  = EMphn#;
            AIeMad1  = EMmAd1;
            AIeMad2  = EMmAd2;
            AIeMcty  = EMmCty;
            AIeMst   = EMmSt;
            AIeMzip  = EMmZip;
            AIePad1  = EMpAd1;
            AIePad2  = EMpAd2;
            AIePcty  = EMpCty;
            AIePst   = EMpSt;
            AIePzip  = EMpZip;

            // Insurance Carrier Information
            AIfName  = FNname;
            AIfMad1  = FNmAd1;
            AIfMad2  = FNmAd2;
            AIfMcty  = FNmCty;
            AIfMst   = FNmSt;
            AIfMzip  = FNmZip;
            AIfPad1  = FNpAd1;
            AIfPad2  = FNpAd2;
            AIfPcty  = FNpCty;
            AIfPst   = FNpSt;
            AIfPzip  = FNpZip;
            AIfArea  = FNph1A;
            AIfPhn#  = FNph1;
            AIfFaxA  = FNfaxA;
            AIfFax   = FNfax;

            // 1. Date of Accident
            Monitor;
               AIacDt = %dec(%date(CMacdt:*cymd):*usa);
            On-Error;
               AIacDt = 0;
            EndMon;

            // 2. Full salary
            AIfSal = CMfSal;

            // 3. Number of weeks temporary total
            GetPayInfo('TT':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIweeksTT = outWeeks;
            AIdaysTT  = outDays;
            AIfrDtTT  = outFromDt;
            AItoDtTT  = outToDt;
            AIamtTT   = outAmount;

            // 4. Number of weeks temporary partial
            GetPayInfo('TP':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIweeksTP = outWeeks;
            AIdaysTP  = outDays;
            AIfrDtTP  = outFromDt;
            AItoDtTP  = outToDt;
            AIamtTP   = outAmount;

            // 5. Number of weeks permanent partial
            GetPayInfo('PP':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIweeksPP = outWeeks;
            AIdaysPP  = outDays;
            AIfrDtPP  = outFromDt;
            AItoDtPP  = outToDt;
            AIamtPP   = outAmount;

            // 6. Disfigurement amount paid
            GetPayInfo('DF':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtDF   = outAmount;

            // 7. Death benefits paid
            GetPayInfo('DB':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtDB   = outAmount;

            // 9. Total of lines 3 thru 8
            GetPayInfo('CA':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtCmp   = AIamtTT + AIamtTP + AIamtPP + AIamtDF +  AIamtDB +
                        outAmount;
            outAmount2 = outAmount;

            // 10. Compromise settlement agreement amount
            GetPayInfo('CL':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtCL   = outAmount;
            AIamtCLCA = outAmount + outAmount2;

            // 11a. Total medical paid
            AIamtMP = GetMedPayAmt();

            // 11b. Total rehabilitation paid
            GetPayInfo('MR':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtRH = outAmount;
            GetPayInfo('VR':outWeeks:outDays:outFromDt:outToDt:outAmount);
            AIamtRH += outAmount;

            // 12. Total of lines 9, 10, 11a and 11b
            AIamtTot = AIamtCmp + AIamtCL + AIamtMP + AIamtRH;

            // 13. Miscellaneous payments
               // Funeral Benefits
               GetPayInfo('FE':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIfunBen = outAmount;

               // Hearing Costs
               GetPayInfo('CR':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIhearCst = outAmount;
               GetPayInfo('DP':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIhearCst += outAmount;
               GetPayInfo('EF':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIhearCst += outAmount;
               GetPayInfo('IC':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIhearCst += outAmount;
               GetPayInfo('ME':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIhearCst += outAmount;

               // Expert Witness
               GetPayInfo('EW':outWeeks:outDays:outFromDt:outToDt:outAmount);
               AIexpWit = outAmount;

               // Total Miscellaneous
               AItotMsc = AIfunBen + AIhearCst + AIexpWit;

            // 15. Date last compensation check forwarded
            AIlastCmp = GetLastChkDate('CP');

            // 16. Date last medical compensation paid
            AIlastMed = GetLastChkDate('MP');

            // Footer Information
            AIcoNm    = COname;
            AItitle   = GetTitle(CMsupr:superName);
            AIsupName = superName;
            AIsysDt   = DateTo8(%date());

            // Populate Initial Date
            AIiDt = %dec(%date():*cymd);

            Write pai0p;

         Endsr;

      /End-free
       //===================================================================
       // GetLastChkDate - Get the last check issued per payment code
       //===================================================================
     P GetLastChkDate  B
     D GetLastChkDate  PI             8  0
     d   inTranCode                   2    Const

     d lastDate        s                   Like(CMacDt) Inz(0)
     d lastDate8       s              8  0 Inz(0)

      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa00;
       If %Found(wdpal);
          DoU %Eof(wdpal);
             If PAtrCd = inTranCode;
                If PApoDt > lastDate;
                   lastDate = PApoDt;
                EndIf;
             Endif;
             ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa00;
          EndDo;
       EndIf;

       Monitor;
          lastDate8 = %dec(%date(lastDate:*cymd):*usa);
       On-Error;
          lastDate8 = 0;
       EndMon;

       Return lastDate8;

      /End-Free

     P GetLastChkDate  E
       //===================================================================
       // GetMedPayAmt - Medical Amount
       //===================================================================
     P GetMedPayAmt    B
     D GetMedPayAmt    PI            11  2

     d totalAmount     s             11  2 Inz(0)

      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa00;
       If %Found(wdpal);
          DoU %Eof(wdpal);
             If PAtrCd = 'MP' And PAbr2 <> 'MR' And PAbr2 <> 'VR';
                // Total Amount
                If PAtype = 'R' or PAtype = 'S' or PAtype = 'V';
                   PAamt = PAamt * -1;
                EndIf;
                totalAmount += PAamt;
             Endif;
             ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa00;
          EndDo;
       EndIf;

       // Return Amount
       Return totalAmount;

      /End-Free

     P GetMedPayAmt    E
       //===================================================================
       // GetPayInfo - Retrieve Payment Information
       //===================================================================
     P GetPayInfo      B
     D GetPayInfo      PI
     d   inPayVal1                    2    Const
     d   outWeeks                     5  0
     d   outDays                      5  0
     d   outFromDt                    8  0
     d   outToDt                      8  0
     d   outAmount                   11  2

     d fromDate        s                   Like(CMacDt) Inz(0)
     d toDate          s                   Like(CMacDt) Inz(0)
     d fromDate8       s              8  0 Inz(0)
     d toDate8         s              8  0 Inz(0)
     d weeks           s              5  0 Inz(0)
     d days            s              5  0 Inz(0)
     d totalAmount     s             11  2 Inz(0)

      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa00;
       If %Found(wdpal);
          DoU %Eof(wdpal);
             If PAbr2 = inPayVal1;
         //*** beg del ***03/21/19 *******************************
                // Weeks
                // weeks += PAwks;
         //*** end del ***03/21/19 *******************************
                // From Date
                If fromDate = 0 Or PAfrDt < fromDate;
                   fromDate = PAfrDt;
                EndIf;
                // To Date
                If PAtoDt > toDate;
                   toDate = PAtoDt;
                EndIf;
                // Total Amount
                If PAtype = 'R' or PAtype = 'S' or PAtype = 'V';
                   PAamt  = PAamt * -1;
         //*** beg add ***03/21/19 *******************************
                   PAwks  = PAwks * -1;
                   PAdays = PAdays * -1;
         //*** end add ***03/21/19 *******************************
                EndIf;
         //*** beg add ***03/21/19 *******************************
                If PAtype = 'A' and PAamt < 0;
                   PAwks  = PAwks * -1;
                   PAdays = PAdays * -1;
                EndIf;
         //*** end add ***03/21/19 *******************************
                totalAmount += PAamt;
         //*** beg add ***03/21/19 *******************************
                // Weeks
                weeks += PAwks;
                days  += PAdays;
         //*** end add ***03/21/19 *******************************
             Endif;
             ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa00;
          EndDo;
       EndIf;

       Monitor;
          fromDate8 = %dec(%date(fromDate:*cymd):*usa);
       On-Error;
          fromDate = 0;
       EndMon;

       Monitor;
          toDate8 = %dec(%date(toDate:*cymd):*usa);
       On-Error;
          toDate8 = 0;
       EndMon;

       outWeeks  = weeks + %Div(days:7);
       outDays   = %Rem(days:7);
       outFromDt = fromDate8;
       outToDt   = toDate8;
       outAmount = totalAmount;

      /End-Free

     P GetPayInfo      E
       //===================================================================
       // GetTitle - Get Title
       //===================================================================
     P GetTitle        B
     D GetTitle        PI            15
     d   inSuper                      3
     d   outSupName                  50

     d title           s             15    Inz(' ')

      /Free

       Chain (inSuper) pup01;
       If %Found(smupl1);
          title      = UPtitl;
          outSupName = UPusNm;
       EndIf;

       // Return Title
       Return title;

      /End-Free

     P GetTitle        E
