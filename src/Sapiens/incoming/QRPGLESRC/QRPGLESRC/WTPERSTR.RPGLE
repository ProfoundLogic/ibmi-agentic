     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE WTPERSTR - Form 18 Periodic Rept - Form 19 Sts Rept & Comp Rcpt
     H*****************************************************************
     H* Program Description:  This program populates the Additional   *
     H*                       medical data file.                      *
     H*                                                               *
     H* Called From:  STLETTER                                        *
     H* Calls To:     None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 10/31/188 by C180   Project 102WHL                 *
     H*                                Cloned and adapted from pgm    *
     H*                                WTADDMED.                      *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 04/25/19 C:180 Support 77983  Include days as part of weeks
     H*                               count.
     H*
     H*****************************************************************
     Fxmlprel1  if   e           k Disk
     Fsmupl1    if   e           k Disk
     Fwdpal     if   e           k Disk
     Fwmcdl     if   e           k Disk
     Fwmcml1    if   e           k Disk
     Fwmeml     if   e           k Disk
     Fwmfnl     if   e           k Disk
     Fwtabl     if   e           k Disk    Include(pb201)
     Fsxprsr    o    e             Disk

     D psds          esds                  ExtName(SZQ1P)

     d* Retrieve client id
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d   clientID                     3

     d* Get medical payment amount
     d GetMedPayAmt    pr            11  2

     d* Get payment Information
     d GetPayInfo      pr
     d   inPayVal1                    2    Const
     d   outDesc                     20A
     d   outWeeks                     5  0
     d   outFrDt                      8  0
     d   outToDt                      8  0
     d   outTrDt                      8  0
     d   outAmount                   11  2

     d* Get Supervisor Name
     d GetSupNam       pr            50
     d   inSuper                      3
     d   outSupPhArea                 3s 0
     d   outSupPhone                  7s 0

     d clientID        s              3    Inz(' ')
     d inXML           s                   Like(XPxml)
     d inPayVal1       s              2    Inz
     d outDesc         s             20A    Inz
     d outWeeks        s              5  0 Inz
     d outFrDt         s              8  0 Inz
     d outToDt         s              8  0 Inz
     d outTrDt         s              8  0 Inz
     d outAmount       s             11  2 Inz
     d outSupName      s             50    Inz
     d outSupPhArea    s              3s 0 Inz
     d outSupPhone     s              7s 0 Inz
     d superName       s             50    Inz
     d superPhArea     s              3s 0 Inz
     d superPhone      s              7s 0 Inz
     d trDtTT          s              8  0 Inz
     d trDtTP          s              8  0 Inz

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
       Chain inXML xp100;
       If %Found(xmlprel1);
          Chain (XPco#:XPfnd:XPfYr:XPcase) pcm01;
          If %Found(wmcml1);
             ExSr PgmStart;
             ExSr WrtSXPRSR;
          EndIf;
       EndIf;

       *inLR = *On;
       //-------------------------------------------------------------
       // PgmStart - Program Start
       //-------------------------------------------------------------
         Begsr PgmStart;

            // Get Client ID
            RtvCltID(clientID);

            // Access Group Master
            Chain (CMco#:CMfnd) pfn01;

            // Access Employer Master
            Chain (CMco#:CMfnd:CMemp#:CMdiv) pem01;

            // Access Claim Demographics
            Chain (CMss#) pcd01;

         EndSr;
       //-------------------------------------------------------------
       // WrtSXPRSR - Write to Scriptura Additional Medical Info File
       //-------------------------------------------------------------
         Begsr WrtSXPRSR;

            Clear PPR01;

            // Header Information
            PRxml    = XPxmlt;
            PRco#    = CMco#;
            PRfnd    = CMfnd;
            PRfYr    = CMfyr;
            PRcase   = CMcase;
            PRacl#   = CMacl#;
            PRclm#   = %EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') +
                       '-' + %EditC(CMcase:'X');
            PReFei   = EMfein;

            // Claim Information
            PRcFsn   = CDfsn;
            PRcMid   = CDmid;
            PRcLsn   = CDlsn;
            PRcAd1   = CDadr1;
            PRcAd2   = CDadr2;
            PRcCty   = CDcty;
            PRcSt    = CDst;
            PRcZip   = CDzip;
            PRcArea  = CDarea;
            PRcPhn#  = CDphn#;
            PRwArea  = CDwAra;
            PRwPhn#  = CDwPh#;

            // Employer Information
            PReNam   = EMname;
            PReArea  = EMarea;
            PRePhn#  = EMphn#;
            PReMad1  = EMmAd1;
            PReMad2  = EMmAd2;
            PReMcty  = EMmCty;
            PReMst   = EMmSt;
            PReMzip  = EMmZip;
            PRePad1  = EMpAd1;
            PRePad2  = EMpAd2;
            PRePcty  = EMpCty;
            PRePst   = EMpSt;
            PRePzip  = EMpZip;

            //Other Information
            PRfName  = FNname;
            Monitor;
               PRacDt = %dec(%date(CMacdt:*cymd):*usa);
            On-Error;
               PRacDt = 0;
            EndMon;


            //Temporary total
            GetPayInfo('TT':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRttDesc = outDesc;
            PRweekTT = outWeeks;
            PRfrDtTT = outFrDt;
            PRtoDtTT = outToDt;
            trDtTT  =  outTrDt;
            PRamtTT  = outAmount;

            //Temporary partial
            GetPayInfo('TP':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRtpDesc = outDesc;
            PRweekTP = outWeeks;
            PRfrDtTP = outFrDt;
            PRtoDtTP = outToDt;
            trDtTP  =  outTrDt;
            PRamtTP  = outAmount;

            //Total weeks Comp Paid
            PRweekcmp = PRweekTT + PRweekTP;


            //Date of First Payment
            If trDtTP < trDtTT;
               PRdtFstPmt = trDtTP;
            Else;
               PRdtFstPmt = trDtTT;
            Endif;

            //Total Comp Amount
            PRcmpAmt   = PRamtTT + PRamtTP;

            //Total Medical Amount
            PRmedAmt = GetMedPayAmt();

            //Permanent partial
            GetPayInfo('PP':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRweekPP  = outWeeks;
            PRfrDtPP  = outFrDt;
            PRtoDtPP  = outToDt;
            PRamtPP   = outAmount;

            //Disfigurement amount paid
            GetPayInfo('DF':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRweekDF  = outWeeks;
            PRamtDF   = outAmount;

            //Agreement and Final Release Amount (Settlement agreement amt)
            GetPayInfo('CL':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRamtAFR = outAmount;

            //Funeral Benefits
            GetPayInfo('FE':outDesc:outWeeks:outFrDt:outToDt:outTrDt:outAmount);
            PRamtFNR = outAmount;

            //Total Comp Amount 1
            PRcmpAmt1   = PRamtTT + PRamtTP + PRamtPP + PRamtDF + PRamtAFR;

            // Footer Information
            PRsupName = GetSupNam(CMsupr:SuperPhArea:SuperPhone);
            PRsupFona = SuperPhArea;
            PRsupFon  = SuperPhone;

            PRsysDt   = DateTo8(%date());

            Write ppr01;

         Endsr;

      /End-free
       //===================================================================
       // GetMedPayAmt - Medical Amount
       //===================================================================
     P GetMedPayAmt    B
     D GetMedPayAmt    PI            11  2

     d totalAmount     s             11  2 Inz(0)

      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa01;
       If %Found(wdpal);
          DoU %Eof(wdpal);
             If PAtrCd = 'MP' And PAbr2 <> 'MR' And PAbr2 <> 'VR';
                // Total Amount
                If PAtype = 'R' or PAtype = 'S' or PAtype = 'V';
                   PAamt = PAamt * -1;
                EndIf;
                totalAmount += PAamt;
             Endif;
             ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa01;
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
     d   outDesc                     20A
     d   outWeeks                     5  0
     d   outFrDt                      8  0
     d   outToDt                      8  0
     d   outTrDt                      8  0
     d   outAmount                   11  2

     d weeks           s              5  0 Inz(0)
     d fromDate        s                   Like(CMacDt) Inz(0)
     d toDate          s                   Like(CMacDt) Inz(0)
     d trDate          s                   Like(CMacDt) Inz(0)
     d fromDate8       s              8  0 Inz(0)
     d toDate8         s              8  0 Inz(0)
     d trDate8         s              8  0 Inz(0)
     d totalAmount     s             11  2 Inz(0)
     d PayCode         s             10A   Inz
      *** Begin Add *** 04/25/19 ***********************************************
     d days            s              5  0 inz
      *** End   Add *** 04/25/19 ***********************************************

      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa01;
       If %Found(wdpal);
          DoU %Eof(wdpal);
             If PAbr2 = inPayVal1;
                // Weeks
                weeks += PAwks;

                //** Begin Add *** 04/25/19 ***********************************
                days +=PAdays;
                //** End   Add *** 04/25/19 ***********************************

                // From Date
                If fromDate = 0 Or PAfrDt < fromDate;
                   fromDate = PAfrDt;
                EndIf;

                // To Date
                If PAtoDt > toDate;
                   toDate = PAtoDt;
                EndIf;

                // Transaction Date
                If trDate = 0 Or PAtrDt < trDate;
                   trDate = PAtrDt;
                EndIf;

                // Total Amount
                If PAtype = 'R' or PAtype = 'S' or PAtype = 'V';
                   PAamt = PAamt * -1;
                EndIf;
                totalAmount += PAamt;
             Endif;
             ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa01;
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

       Monitor;
          trDate8 = %dec(%date(trDate:*cymd):*usa);
       On-Error;
          trDate8 = 0;
       EndMon;

       PayCode   = inPayVal1;
       Chain (CMco#:PayCode) PB201;
       If %found(wtabl);
          outDesc = B2desc;
       Else;
          outDesc = *blanks;
       Endif;
       //*** Begin change *** 04/25/19 *****************************************
       //outWeeks  = weeks;
       outWeeks  = weeks + days/7;
       //*** End   change *** 04/25/19 *****************************************
       outFrDt = fromDate8;
       outToDt   = toDate8;
       outTrDt   = trDate8;
       outAmount = totalAmount;

      /End-Free

     P GetPayInfo      E
       //===================================================================
       // GetSupNam - Get Supervisor Name
       //===================================================================
     P GetSupNam       B
     D GetSupNam       PI            50
     d   inSuper                      3
     d   outSupPhArea                 3S 0
     d   outSupPhone                  7S 0

     d   outSupPName   s                   LIKE(UPUSNM) INZ

      /Free
       OutSupPhArea = *zeros;
       OutSupPhone = *zeros;

       Chain (inSuper) pup01;
       If %Found(smupl1);
          outSupName = UPusNm;
          outSupPhArea = UPphNa;
          outSupPhone = UPphN#;
       EndIf;

       // Return Supervisor Name
       Return outSupName;

      /End-Free

     P GetSupNam       E
