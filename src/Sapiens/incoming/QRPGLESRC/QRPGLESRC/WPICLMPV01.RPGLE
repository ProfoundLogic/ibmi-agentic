     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for PV1 codes
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WPICLMPV
     h*  Calls to:
     h*
     h*  Created on 08/04/2014 by C:300 for Project WC2247
     h**********************************************************************
     h* Revisions:
     h*
     h*  03/02/23 C:340 Project: WC2430
     h*                 PV1 screen fixes : Convert CheckBoxes
     h*
     h* 12/10/20 C: 062 Project: 89WCR
     h*                 Add Exclude From ALAE.
     h*
     h* 12/06/19 C: 079 Project: 129WVK
     h*                 Add Vocational Rehab Paid Under Comp
     h*
     h* 08/19/19 C: 079 Project: WC2604
     h*                 Add IDC Benefit Type Code
     h*
     h**********************************************************************
     fwtb2p     if   e             disk    rename(pb201:pb20p)
     fwtabl     if   e           k disk
     fwtgtl     if   e           k disk
     fstctl1    if   e           k disk

      /copy sPrAtrium

      * *ENTRY
     dwpiClmPV01       pr                  extpgm('WPICLMPV01')
     d                                9  0
     d                            32767
     d                                1

     dwpiClmPV01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strPV1          s             10
     d strDesc         s             50
     d strPlCd         s              1
     d strSts          s             10
     d strPayEdi       s             50
     d strEdiRcd       s             50
     d strEftFlg       s              5
     d strCaFraud      s              5
     d strMinCd        s             50
     d strWkDy         s              5
     d strDay2Dt       s              5
     d strDartRmk      s             50
     d strBenInd       s             50
     d strCWcStat      s              5
     d strDgnRcv       s              5
     d*** Beg Add *** 08/19/19 ********************************************
     d strBenTypeCode  s             50
     d*** End Add *** 08/19/19 ********************************************
     d*** Beg Add *** 12/10/20 ********************************************
     d strExcALAE      s              5
     d*** End Add *** 12/10/20 ********************************************

     d strCo1          s             50
     d strCo2          s             50
     d strCo3          s             50
     d strCo4          s             50
     d strCo5          s             50
     d strCo6          s             50
     d strCo7          s             50
     d strCo8          s             50
     d strCo9          s             50
     d strCo10         s             50

     d ##resvcd        s             10
     d ##tbid          s              4
     d ##key           s             10


     c     keyWTRTP      Klist
     c                   Kfld                    B2co#
     c                   Kfld                    ##resvcd

     c     keyWTGTL      Klist
     c                   Kfld                    B2co#
     c                   Kfld                    ##tbid
     c                   Kfld                    ##key

      /Free

       Chain inRRN pb20p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlPayCode':%trim(strPV1));
       BuildJSON(outJSON :'OutDtlpayDesp':%trim(strDesc));
       BuildJSON(outJSON :'OutDtlpayPlm':%trim(strPlCd));
       BuildJSON(outJSON :'OutDtlpayStatus' :%trim(strSts));
       BuildJSON(outJSON :'OutDtlPayEDI' :%trim(strPayEdi));
       BuildJSON(outJSON :'OutDtlPayEDIrcd' :%trim(strEdiRcd));
       //*** Beg Del *** 03/02/23 *** WC2430 ******************************
       // BuildJSON(outJSON :'OutDtlEftflag' :%trim(strEftFlg));
       //*** Beg Add *** 12/10/20 *****************************************
       // BuildJSON(outJSON :'OutDtlExcludeALAE' :%trim(strExcALAE));
       //*** End Add *** 12/10/20 *****************************************
       // BuildJSON(outJSON :'OutDtlprtCAfraud' :%trim(strCaFraud));
       //*** End Del *** 03/02/23 *** WC2430 ******************************
       BuildJSON(outJSON :'OutDtlPayMincd' :%trim(strMinCd));
       //*** Beg Del *** 03/02/23 *** WC2430 ******************************
       // BuildJSON(outJSON :'OutDtlPayWkdy' :%trim(strWkDy));
       //*** End Del *** 03/02/23 *** WC2430 ******************************
       BuildJSON(outJSON :'OutDtlDay2dt' :%trim(strDay2Dt));
       BuildJSON(outJSON :'OutDtlPayDartRmk' :%trim(strDartRmk));
       BuildJSON(outJSON :'OutDtlPayBenInd' :%trim(strBenInd));
       //*** Beg Del *** 03/02/23 *** WC2430 ******************************
       // BuildJSON(outJSON :'OutDtlcostWCstat' :%trim(strCWcStat));
       // BuildJSON(outJSON :'OutDtlDgnRecovery' :%trim(strDgnRcv));
       //*** End Del *** 03/02/23 *** WC2430 ******************************
       BuildJSON(outJSON :'OutDtlco1' :%trim(strCo1));
       BuildJSON(outJSON :'OutDtlco2' :%trim(strCo2));
       BuildJSON(outJSON :'OutDtlco3' :%trim(strCo3));
       BuildJSON(outJSON :'OutDtlco4' :%trim(strCo4));
       BuildJSON(outJSON :'OutDtlco5' :%trim(strCo5));
       BuildJSON(outJSON :'OutDtlco6' :%trim(strCo6));
       BuildJSON(outJSON :'OutDtlco7' :%trim(strCo7));
       BuildJSON(outJSON :'OutDtlco8' :%trim(strCo8));
       BuildJSON(outJSON :'OutDtlco9' :%trim(strCo9));
       BuildJSON(outJSON :'OutDtlco10' :%trim(strCo10));
       //*** Beg Add *** 08/19/19 ******************************************
       BuildJSON(outJSON :'OutDtlBenTypeCode' :%trim(strBenTypeCode));
       //*** End Add *** 08/19/19 ******************************************
       //*** Beg Add *** 12/06/19 ******************************************
       BuildJSON(outJSON :'CbxDtlVocRehabPaidUnderComp' :%trim(B2vHPc));
       //*** End Add *** 12/06/19 ******************************************
       //*** Beg Add *** 03/02/23 *** WC2430 ******************************
       BuildJSON(outJSON :'CbxDtlEftFlag' :%trim(B2Eft));
       BuildJSON(outJSON :'CbxDtlExcludeALAE' :%trim(B2ExALAE));
       BuildJSON(outJSON :'CbxDtlcostWCstat' :%trim(B2Ccw));
       BuildJSON(outJSON :'CbxDtlDgnRecovery' :%trim(B2rcvr));
       BuildJSON(outJSON :'CbxDtlprtCAfraud' :%trim(B2Flg1));
       BuildJSON(outJSON :'CbxDtlPayWkdy' :%trim(B2exwdc));
       BuildJSON(outJSON :'CbxDtlLumpSum' :%trim(B2LmpSum));
       //*** End Add *** 03/02/23 *** WC2430 ******************************


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Payment Value 1 Code
       strPV1 = B2key;

       // Description
       strDesc = B2desc;

       // Placement Code
       strPlCd = B2plCd;

       // Status
       If B2stat = 'A';
          strSts = 'Active';
       Else;
          strSts = 'Inactive';
       Endif;


       // populate Flag fields

       // EFT Flag
       If B2eft = 'Y';
          strEftFlg = 'Yes';
       Else;
          strEftFlg = 'No';
       Endif;

       //*** Beg Add *** 12/10/20 *****************************************
       // Exclude From ALAE
       If B2exALAE = 'Y';
          strExcALAE = 'Yes';
       Else;
          strExcALAE = 'No';
       Endif;
       //*** End Add *** 12/10/20 *****************************************

       // Print CA Fraud flag
       If B2flg1 = 'Y';
          strCaFraud = 'Yes';
       Else;
          strCaFraud = 'No';
       Endif;

       // Exclude from Week/Days Calculation Flag
       If B2exwdc = 'Y';
          strWkDy = 'Yes';
       Else;
          strWkDy = 'No';
       Endif;

       // Cost containment for wcstat Flag
       If B2ccw = 'Y';
          strCWcStat = 'Yes';
       Else;
          strCWcStat = 'No';
       Endif;

       // Designate as Recovery Regardless of Payment Type flag
       If B2rcvr = 'Y';
          strDgnRcv = 'Yes';
       Else;
          strDgnRcv = 'No';
       Endif;


       // Number of Days to Due Date
       strDay2Dt = %char(B2nddt);

       // Populate codes with description

       // EDI Payment/Adjustment code
       If B2pyCd <> *blanks;
          ##tbid = 'PADJ';
          ##key = B2pyCd;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strPayEdi = B2pyCd + ' - ' + %trim(GTdesc);
          Else;
             strPayEdi = B2pyCd;
          Endif;
       Endif;

       // EDI Recovery code
       If B2ercv <> *blanks;
          ##tbid = 'PADJ';
          ##key = B2ercv;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strEdiRcd = B2ercv + ' - ' + %trim(GTdesc);
          Else;
             strEdiRcd = B2ercv;
          Endif;
       Endif;

       // Minor Payment Code
       If B2minr <> *blanks and B2minr <> '000';
          ##tbid = 'MPMT';
          ##key = B2minr;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strMinCd = B2minr + ' - ' + %trim(GTdesc);
          Else;
             strMinCd = B2minr;
          Endif;
       Endif;

       // Benefit Indicator
       If B2bnft <> *blanks;
          ##tbid = 'BIND';
          ##key = B2bnft;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strBenInd = B2bnft + ' - ' + %trim(GTdesc);
          Else;
             strBenInd = B2bnft;
          Endif;
       Endif;

       // DARTS Remark
       If B2rmCd <> *blanks;
          Chain B2rmCd pct01;
          If %Found(STCTL1);
             strDartRmk = B2rmCd + ' - ' + %trim(CTdesc);
          Else;
             strDartRmk = B2rmCd;
          Endif;
       Endif;


       // Get Reserve code Description
       If B2rc01 <> *blanks;
          ##resvcd = b2rc01;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo1 = B2rc01 + ' - ' + %trim(RTdesc);
          Else;
             strCo1 = B2rc01;
          Endif;
       Endif;

       If B2rc02 <> *blanks;
          ##resvcd = b2rc02;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo2 = B2rc02 + ' - ' + %trim(RTdesc);
          Else;
             strCo2 = B2rc02;
          Endif;
       Endif;

       If B2rc03 <> *blanks;
          ##resvcd = b2rc03;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo3 = B2rc03 + ' - ' + %trim(RTdesc);
          Else;
             strCo3 = B2rc03;
          Endif;
       Endif;

       If B2rc04 <> *blanks;
          ##resvcd = b2rc04;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo4 = B2rc04 + ' - ' + %trim(RTdesc);
          Else;
             strCo4 = B2rc04;
          Endif;
       Endif;

       If B2rc05 <> *blanks;
          ##resvcd = b2rc05;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo5 = B2rc05 + ' - ' + %trim(RTdesc);
          Else;
             strCo5 = B2rc05;
          Endif;
       Endif;

       If B2rc06 <> *blanks;
          ##resvcd = b2rc06;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo6 = B2rc06 + ' - ' + %trim(RTdesc);
          Else;
             strCo6 = B2rc06;
          Endif;
       Endif;

       If B2rc07 <> *blanks;
          ##resvcd = b2rc07;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo7 = B2rc07 + ' - ' + %trim(RTdesc);
          Else;
             strCo7 = B2rc07;
          Endif;
       Endif;

       If B2rc08 <> *blanks;
          ##resvcd = b2rc08;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo8 = B2rc08 + ' - ' + %trim(RTdesc);
          Else;
             strCo8 = B2rc08;
          Endif;
       Endif;

       If B2rc09 <> *blanks;
          ##resvcd = b2rc09;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo9 = B2rc09 + ' - ' + %trim(RTdesc);
          Else;
             strCo9 = B2rc09;
          Endif;
       Endif;

       If B2rc10 <> *blanks;
          ##resvcd = b2rc10;
          Chain keyWTRTP prt01;
          If %Found(WTABL);
             strCo10 = B2rc10 + ' - ' + %trim(RTdesc);
          Else;
             strCo10 = B2rc10;
          Endif;
       Endif;
       //*** Beg Add *** 08/19/19 ******************************************
       // IDC Benefit Type Code
       If B2iBtC <> *blanks;
          ##tbid = 'IDCB';
          ##key = B2iBtC;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strBenTypeCode = B2iBtC + ' - ' + %trim(GTdesc);
          Else;
             strBenTypeCode = B2iBtC;
          Endif;
       Endif;
       //*** End Add *** 08/19/19 ******************************************

       endsr;
      /End-Free
