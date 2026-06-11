       CTL-OPT COPYRIGHT('(c) 2025 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  WTBNKACCT1 - Ajax program to populate Bank Account Master      *
       //               Maitenance/Inquiry Detail.                        *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program populates the addItional detail of the            *
       //  Bank Account Master Maint/Inq subfile.                         *
       //                                                                 *
       //  Called from: WTBNKACCT                                         *
       //                                                                 *
       //  Created: 08/20/20  C:180  Project:  WC2731                     *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //  03/17/25 C:291 Project: WC3002                                 *
       //                 Add Checkbox "Pad incomplete Blocks of 10 with  *
       //                 9s".                                            *
       //                                                                 *
       //  09/14/22 C:291 Project: 129WXH                                 *
       //                 Add Checkbox "Default recurring Payments to     *
       //                 claimant/dependents to EFT if EFT is set up".   *
       //                                                                 *
       //  04/11/22 C:340 Project: WC2663                                 *
       //                 Invoice Cloud - Add Invoice Type ID             *
       //                                                                 *
       // 10/07/21  C:300 Support: 98537                                  *
       //                 Leading Blank is displayed as *ZERO, need to    *
       //                 show as Blank                                   *
       //                                                                 *
       // 06/10/21  C:180 Project:133WSO                                  *
       //                 Add security fields for changing payment        *
       //                 request from Epay to MICR type.                 *
       //                                                                 *
       //                                                                 *
       //******************************************************************
       DCL-PI *N;
          @@rrn packed(9:0);
          ##outJSON  char(32767);
          ##rtnCode  char(1);
       END-PI;

       DCL-F wsckp disk usage(*INPUT);
       DCL-F wtabl disk ignore(PRT01) usage(*INPUT) keyed ;
       DCL-F wsrtl disk usage(*INPUT) keyed ;
       DCL-F wtgtl disk usage(*INPUT) keyed ;
       //*** Begin Add *** 06/09/21 ******************************************
       Dcl-F wsckl1sec disk usage(*INPUT) keyed;
       //*** End   Add *** 06/09/21 ******************************************

       DCL-PR RTVCLTID  EXTPGM;
          strCltId char(3);
       END-PR;

       DCL-S intRrn packed(9:0);
       DCL-S strACCT like(CKacct);
       DCL-S strAI01 like(CKai01);
       DCL-S strAI02 like(CKai02);
       DCL-S strAI03 like(CKai03);
       DCL-S strState char(2);
       DCL-S strZip char(10);
       DCL-S strEFDT char(10);
       DCL-S strLCHK char(7);
       DCL-S strCRUP like(CKcrup);
       DCL-S strRPCD char(35);
       DCL-S strDPCD char(35);
       DCL-S strEXDT char(10);
       DCL-S strVCHR like(CKvchr);
       DCL-S strVCHR# char(35);
       DCL-S strRPVS char(35);
       DCL-S strDPVS char(35);
       DCL-S strTRNS like(CKtrns);
       DCL-S strBNK# like(CKbnk#);
       DCL-S strCKDG char(1);
       DCL-S strAXOU like(CKaxou);
       DCL-S strONUS like(CKonus);
       DCL-S strRTUP like(CKrtup);
       DCL-S strRTLW like(CKrtlw);
       DCL-S strNAM1 like(CKnam1);
       DCL-S strNAM2 like(CKnam2);

       DCL-S strIDST  char(10);
       DCL-S strIORG  char(10);
       DCL-S strBNAM  like(CKbnam);
       DCL-S strCNAM  like(CKconm);
       DCL-S strCEDS  like(CKceds);

       DCL-S strRFCD like(CKrfcd);
       DCL-S strORDI char(8);
       DCL-S strSVCL char(35);
       DCL-S strEFTF like(CKeftf);
       DCL-S strACID char(2);
       DCL-S strBKLT char(2);
       DCL-S strEFSP char(35);

       DCL-S strBYPN like(CKbypn);
       DCL-S strSPAD like(RTspad);
       DCL-S strCMWC like (RTcmwc);
       DCL-S strCMWP like(RTcmwp);
       DCL-S strCMAG like(RTcmag);
       DCL-S strEFT# char(7);
       DCL-S strWPP like(CKWPP);
       DCL-S strWBK# char(4);
       DCL-S strCltId char(3);
       DCL-S strLblVchr char(20);
       //*** Beg Add ***** 09/14/22 *******************
       DCL-S strOvrEPay like(CKOvrEPay);
       //*** End Add ***** 09/14/22 *******************
       //*** Beg Add ***** 03/17/25 *******************
       DCL-S strPad9 like(CKPad9);
       //*** End Add ***** 03/17/25 *******************


       DCL-S intKeyCo# packed(3:0);
       DCL-S strKey10 char(10);
       DCL-S strEPayVendor Char(5);
       DCL-S strNextEPaySeq# Char(7);
       DCL-DS KeyWSRTL LIKEREC(PRT01:*KEY);
       //*** Begin Add *** 06/10/21 *************************************************
       Dcl-Ds KeyWSCKL1SEC likerec(PCKS01:*KEY);
       Dcl-S strMicrChgSecLevel1 char(3) inz;
       Dcl-S strMicrChgSecLevel2 char(3) inz;
       Dcl-S strMicrChgSecLevel3 char(3) inz;
       Dcl-S strMicrChgSecLevel4 char(3) inz;
       Dcl-S strMicrChgSecLevel5 char(3) inz;
       Dcl-S strMicrChgSecLevel6 char(3) inz;
       Dcl-S strMicrChgSecLevel7 char(3) inz;
       Dcl-S strMicrChgSecLevel8 char(3) inz;
       Dcl-S strMicrChgSecLevel9 char(3) inz;
       Dcl-S strMicrChgSecLevelA char(3) inz;
       Dcl-S strMicrChgSecType1 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType2 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType3 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType4 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType5 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType6 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType7 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType8 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecType9 like(CKSePmiSt1) inz;
       Dcl-S strMicrChgSecTypeA like(CKSePmiSt1) inz;
       //*** End   Add *** 06/10/21 *************************************************
C340   //*** Begin Add *** 04/11/22 *************************************************
       Dcl-S strInvTypId Char(15);
C340   //*** End   Add *** 04/11/22 *************************************************

      /Free
      /copy sPrAtrium
      /End-Free

       RtvCltId(strCltId);
       exsr BuildData;

       ##outJSON = *blanks;

       //Populate Bank Account Master Subfile Additional Information

       BuildJSON(##outJSON :'OutBankAccount':%trim(strACCT));
       BuildJSON(##outJSON :'OutBankName':%trim(strAI01));
       BuildJSON(##outJSON :'OutBankAddress':%trim(strAI02));
       BuildJSON(##outJSON :'OutBankCityStateZip' :%trim(strAI03));
       BuildJSON(##outJSON :'OutEffectiveDate' :%trim(strEFDT));
       BuildJSON(##outJSON :'OutNextAvailCheckNo' :%trim(strLCHK));
       BuildJSON(##outJSON :'CbxCheckReconcUpdate1' :%trim(strCRUP));
       BuildJSON(##outJSON :'OutReturnPremCheckARcode' :%trim(strRPCD));
       BuildJSON(##outJSON :'OutReturnDepositCheckARcode' :%trim(strDPCD));
       BuildJSON(##outJSON :'OutExpirationDate' :%trim(strEXDT));
       BuildJSON(##outJSON :'CbxVoucher1' :%trim(strVCHR));
       BuildJSON(##outJSON :'OutReturnPremVoidStopARcode' :%trim(strRPVS));
       BuildJSON(##outJSON :'OutReturnDepositVoidStopARcode' :%trim(strDPVS));

       BuildJSON(##outJSON :'OutRoutingMICR' :%trim(strTRNS));
       BuildJSON(##outJSON :'OutTransitNumber' :%trim(strBNK#));
       BuildJSON(##outJSON :'OutCheckDigit' :%trim(strCKDG));
       BuildJSON(##outJSON :'OutAuxiliaryOnUs' :%trim(strAXOU));
       BuildJSON(##outJSON :'OutOnUs' :%trim(strONUS));
       BuildJSON(##outJSON :'OutRoutingEntryUpper' :%trim(strRTUP));
       BuildJSON(##outJSON :'OUtRoutingEntryLower' :%trim(strRTLW));
       BuildJSON(##outJSON :'OutFirstSignature' :%trim(strNAM1));
       BuildJSON(##outJSON :'OutSecondSignature' :%trim(strNAM2));

       //EFT
       //*** Begin chg *** 10/07/21 *************************************************
       //*** BuildJSON(##outJSON :'OutImmediateDestinationNo' :%trim(strIDST));
       //*** BuildJSON(##outJSON :'OutImmediateOriginNo' :%trim(strIORG));
       BuildJSON(##outJSON :'OutImmediateDestinationNo' :strIDST);
       BuildJSON(##outJSON :'OutImmediateOriginNo' :strIORG);
       //*** End   chg *** 10/07/21 *************************************************
       BuildJSON(##outJSON :'OutDestination' :%trim(strBNAM));
       BuildJSON(##outJSON :'OutCompanyName' :%trim(strCNAM));
       BuildJSON(##outJSON :'OutCompanyEnteredDescription' :%trim(strCEDS));

       BuildJSON(##outJSON :'OutReferenceCode' :%trim(strRFCD));
       BuildJSON(##outJSON :'OutOriginatingDFIid' :%trim(strORDI));
       BuildJSON(##outJSON :'OutServiceClassCode' :%trim(strSVCL));
       BuildJSON(##outJSON :'OutDownloadEFTfileName' :%trim(strEFTF));
       BuildJSON(##outJSON :'OutACHinterimDays' :%trim(strACID));
       BuildJSON(##outJSON :'OutBankLeadTime' :%trim(strBKLT));
       BuildJSON(##outJSON :'OutEFTspecialProcessing' :%trim(strEFSP));

       BuildJSON(##outJSON :'CbxBypassPrenoteProcessing' :%trim(strBYPN));
       BuildJSON(##outJSON :'CbxSuppressAddendaRecords' :%trim(strSPAD));
       BuildJSON(##outJSON :'CbxClaimaintsProviders' :%trim(strCMWC));
       BuildJSON(##outJSON :'CbxPremium' :%trim(strCMWP));
       BuildJSON(##outJSON :'CbxAgents' :%trim(strCMAG));
       BuildJSON(##outJSON :'OutNextAvailableEFTseqNo' :%trim(strEFT#));

       BuildJSON(##outJSON :'OutPositivePayDownloadId' :%trim(strWPP));
       BuildJSON(##outJSON :'OutPositivePayBankNumber' :%trim(strWBK#));
       BuildJSON(##outJSON :'LblVoucher' :%trim(strLblVchr));
       BuildJSON(##outJSON :'OutProcessingMethod' :%trim(strVCHR#));
       BuildJSON(##outJSON :'outEPayVendor' :%trim(strEPayVendor));
       BuildJSON(##outJSON :'outNextEPaySeq#':%trim(strNextEPaySeq#));
C340   //*** Begin Add *** 04/11/22 ***************************************************
       BuildJSON(##outJSON :'OutInvTypId':%trim(strInvTypId));
C340   //*** End   Add *** 04/11/22 ***************************************************
       //*** Begin Add *** 06/10/21 ***************************************************
       BuildJSON(##outJSON :'OutMicrChgSecType1':%trim(strMicrChgSecType1));
       BuildJSON(##outJSON :'OutMicrChgSecType2':%trim(strMicrChgSecType2));
       BuildJSON(##outJSON :'OutMicrChgSecType3':%trim(strMicrChgSecType3));
       BuildJSON(##outJSON :'OutMicrChgSecType4':%trim(strMicrChgSecType4));
       BuildJSON(##outJSON :'OutMicrChgSecType5':%trim(strMicrChgSecType5));
       BuildJSON(##outJSON :'OutMicrChgSecType6':%trim(strMicrChgSecType6));
       BuildJSON(##outJSON :'OutMicrChgSecType7':%trim(strMicrChgSecType7));
       BuildJSON(##outJSON :'OutMicrChgSecType8':%trim(strMicrChgSecType8));
       BuildJSON(##outJSON :'OutMicrChgSecType9':%trim(strMicrChgSecType9));
       BuildJSON(##outJSON :'OutMicrChgSecTypeA':%trim(strMicrChgSecTypeA));
       BuildJSON(##outJSON :'OutMicrChgSecLevel1':%trim(strMicrChgSecLevel1));
       BuildJSON(##outJSON :'OutMicrChgSecLevel2':%trim(strMicrChgSecLevel2));
       BuildJSON(##outJSON :'OutMicrChgSecLevel3':%trim(strMicrChgSecLevel3));
       BuildJSON(##outJSON :'OutMicrChgSecLevel4':%trim(strMicrChgSecLevel4));
       BuildJSON(##outJSON :'OutMicrChgSecLevel5':%trim(strMicrChgSecLevel5));
       BuildJSON(##outJSON :'OutMicrChgSecLevel6':%trim(strMicrChgSecLevel6));
       BuildJSON(##outJSON :'OutMicrChgSecLevel7':%trim(strMicrChgSecLevel7));
       BuildJSON(##outJSON :'OutMicrChgSecLevel8':%trim(strMicrChgSecLevel8));
       BuildJSON(##outJSON :'OutMicrChgSecLevel9':%trim(strMicrChgSecLevel9));
       BuildJSON(##outJSON :'OutMicrChgSecLevelA':%trim(strMicrChgSecLevelA));
       //*** End   Add *** 06/10/21 ***************************************************
       //*** Beg Add ***** 09/14/22 *******************************
       BuildJSON(##outJSON :'CbxOverEPay' :%trim(strOvrEpay));
       //*** End Add ***** 09/14/22 *******************************
       //*** Beg Add ***** 03/17/25 *******************************
       BuildJSON(##outJSON :'CbxPadBlkOf10With9' :%trim(strPad9));
       //*** End Add ***** 03/17/25 *******************************
       ##rtnCode = ' ';
       return;

       //*---------------------------------------------------------------------
       // Build data
       //*---------------------------------------------------------------------
       Begsr BuildData;

       intRrn = @@rrn;
       chain intRrn PCK01;
       strACCT = %trim(CKacct);
       strAI01 = %trim(CKai01);
       strAI02 = %trim(CKai02);
       strAI03 = %trim(CKai03);
       If CKefdt > *Zero;
          strEFDT = %char(%date(CKefdt:*CYMD):*USA);
       Else;
          strEFDT = *blanks;
       Endif;

       strLCHK = %char(CKlchk);

       If CKcrup = 'Y' or CKcrup = 'N';
          strCRUP = CKcrup;
       Else;
          strCRUP = 'N';
       Endif;

       intKeyCo# = CKco#;
       strKey10 =  CKrpcd;
       chain (intKeyCo#:strKey10) PTT01;
       If %found(WTABL);
          strRPCD = CKrpcd + ' - ' + %trim(TTdesc);
       Endif;

       strKey10 = CKrdcd;
       chain (intKeyCo#:strKey10) PTT01;
       If %found(WTABL);
          strDPCD =CKrdcd + ' - ' + %trim(TTdesc);
       Endif;

       If CKexdt > *Zero;
          strEXDT = %char(%date(CKexdt:*CYMD):*USA);
       Else;
          strEXDT = *blanks;
       Endif;

       If CKvchr = 'Y' or CKvchr = 'N';
          strVCHR = CKvchr;
       Else;
          strVCHR = 'N';
       Endif;

       If CKvchr = *blanks;
          strVCHR = 'N';
       Endif;

       //Override voucher checkbox by Client Id
       If strCltId = '114';
          strLblVchr = 'Processing Method';
          strVCHR  = 'N';
          Select;
          When CKvchr = 'Y';
             strVCHR# = 'V - Voucher';
          When CKvchr = 'N';
             strVCHR# = 'P - Paper';
          When CKvchr = 'X';
             strVCHR# = 'X - XML File';
          Other;
             strVCHR# = CKvchr + ' - ' ;
          Endsl;
       Else;
          strLblVchr = 'Voucher          ';
          If CKvchr = 'Y';
             strVCHR# = 'Voucher';
          Else;
             strVCHR# = *Blanks;
          Endif;
       Endif;

       strKey10 =  CKrpvs;
       chain (intKeyCo#:strKey10) PTT01;
       If %found(WTABL);
          strRPVS = CKrpvs + ' - ' + %trim(TTdesc);
       Endif;

       strKey10 = CKrdvs;
       chain (intKeyCo#:strKey10) PTT01;
       If %found(WTABL);
          strDPVS = CKrdvs + ' - ' + %trim(TTdesc);
       Endif;

       strTRNS = CKtrns;
       strBNK# = CKbnk#;
       strCKDG = %char(CKckdg);
       strAXOU = CKaxou;
       strONUS = CKonus;
       strRTUP = CKrtup;
       strRTLW = CKrtlw;
       strNAM1 = CKnam1;
       strNAM2 = CKnam2;

       //EFT
       //*** Begin chg *** 10/07/21 *************************************************
       //*** strIDST = %editc(CKidst:'X');
       //*** strIORG = %editc(CKiorg:'X');
       strIDST = CKidsa;
       strIORG = CKiora;
       //*** End   chg *** 10/07/21 *************************************************
       strBNAM = CKbnam;
       strCNAM = CKconm;
       strCEDS = CKceds;

       strRFCD = CKrfcd;
       //*** Begin chg *** 10/07/21 *************************************************
       //*** strORDI = %editc(CKordi:'X');
       strORDI = %char(CKordi);
       //*** End   chg *** 10/07/21 *************************************************

       clear strKey10;
       //*** Begin chg *** 10/07/21 *************************************************
       //*** strKey10 = %editc(CKsvcl:'X');
       //*** chain (CKco#:'SCLS':strKey10) PGT01;
       //*** If %found(WTGTL);
       //***    strSVCL = %editc(CKsvcl:'X') + ' - ' + %Trim(GTDESC);
       //*** Else;
       //***    strSVCL = %editc(CKsvcl:'X');
       //*** Endif;
       strKey10 = %char(CKsvcl);
       chain (CKco#:'SCLS':strKey10) PGT01;
       If %found(WTGTL);
          strSVCL = %char(CKsvcl) + ' - ' + %Trim(GTDESC);
       Else;
          strSVCL = %char(CKsvcl);
       Endif;
       //*** End   chg *** 10/07/21 *************************************************

       strEFTF = CKeftf;
       //*** Begin chg *** 10/07/21 *************************************************
       //*** strACID = %editc(CKacid:'X');
       //*** strBKLT = %editc(CKbklt:'X');
       strACID = %char(CKacid);
       strBKLT = %char(CKbklt);
       //*** End   chg *** 10/07/21 *************************************************

       strKey10 = CKefsp;
       chain (CKco#:'BNK':strKey10) PGT01;
       If %found(WTGTL);
          strEFSP = CKEFSP + ' - ' + %Trim(GTDESC);
       Else;
          strEFSP = CKEFSP;
       Endif;


       strBYPN = CKbypn;
       //*** Beg Add ***** 09/14/22 ***********************
       strOvrEPay = CKOvrEPay;
       //*** End Add ***** 09/14/22 ***********************
       //*** Beg Add ***** 03/17/25 ***********************
       strPad9 = CKPad9;
       //*** End Add ***** 03/17/25 ***********************

       Clear RTspad;
       Clear RTcmwc;
       Clear RTcmwp;
       Clear RTcmag;

       If  CKidsa <> *blanks;
          KeyWSRTL.RTkey = CKidsa ;
       Else;
          KeyWSRTL.RTkey = %Editc(CKidst : 'X');
       Endif;

       If  KeyWSRTL.RTkey = *blanks;
          KeyWSRTL.RTkey = CKTRNS + CKBNK# + %Editc(CKckdg : 'X');
       Endif;

       KeyWSRTL.RTco# = CKco#;

       Chain(n) %kds(KeyWSRTL) PRT01;
       if %found();
          strSPAD = RTspad;
          strCMWC = RTcmwc;
          strCMWP = RTcmwp;
          strCMAG = RTcmag;
       Endif;

       strEFT# = %char(CKeft#);

       //Positive Pay - Applicable to certain clients
       If strCltId = '114';
          clear strKey10;
          strKey10 = CKWPP;
          chain (CKco#:'PPBK':strKey10) PGT01;
          If %found(WTGTL);
             strWPP = CKWPP + ' - ' + %Trim(GTDESC);
          Else;
             strWPP = CKWPP;
          Endif;
       //*** Begin chg *** 10/07/21 *************************************************
       //***   strWBK# = %editc(CKwbk#:'X');
          strWBK# = %char(CKwbk#);
       //*** End   chg *** 10/07/21 *************************************************
       Endif;

       If strWPP = *Blanks;
          strWPP = 'N - ';
       Endif;

       // E-Pay Info
       strEPayVendor = CKePayVN;
       //*** Begin chg *** 10/07/21 *************************************************
       //*** strNextEPaySeq# = %EditC(CKePay#:'X');
       strNextEPaySeq# = %char(CKePay#);
       //*** End   chg *** 10/07/21 *************************************************
       //*** Begin chg *** 04/11/22 *************************************************
       strInvTypId = %Trim(%EditC(CkInvTypID:'K'));
       //*** End   chg *** 04/11/22 *************************************************

       //*** Begin Add *** 06/10/21 ******************************
       KeyWSCKL1SEC.CKSePMiCo# = CKco#;
       KeyWSCKL1SEC.CKSePMiAcc = CKacct;
       KeyWSCKL1SEC.CKSePMiId  = 'CHGMICR';
       Chain %kds(KeyWSCKL1SEC) PCKS01;

       If %Found();
          If CKSePmiSl1 > *Zero;
             strMicrChgSecLevel1 = %Char(CKSePmiSl1);
          Endif;

          If CKSePmiSl2 > *Zero;
             strMicrChgSecLevel2 = %Char(CKSePmiSl2);
          Endif;

          If CKSePmiSl3 > *Zero;
             strMicrChgSecLevel3 = %Char(CKSePmiSl3);
          Endif;

          If CKSePmiSl4 > *Zero;
             strMicrChgSecLevel4 = %Char(CKSePmiSl4);
          Endif;

          If CKSePmiSl5 > *Zero;
             strMicrChgSecLevel5 = %Char(CKSePmiSl5);
          Endif;

          If CKSePmiSl6 > *Zero;
             strMicrChgSecLevel6 = %Char(CKSePmiSl6);
          Endif;

          If CKSePmiSl7 > *Zero;
             strMicrChgSecLevel7 = %Char(CKSePmiSl7);
          Endif;

          If CKSePmiSl8 > *Zero;
             strMicrChgSecLevel8 = %Char(CKSePmiSl8);
          Endif;

          If CKSePmiSl9 > *Zero;
             strMicrChgSecLevel9 = %Char(CKSePmiSl9);
          Endif;

          If CKSePmiSlA > *Zero;
             strMicrChgSecLevelA = %Char(CKSePmiSlA);
          Endif;

          strMicrChgSecType1  = CKSePmiSt1;
          strMicrChgSecType2  = CKSePmiSt2;
          strMicrChgSecType3  = CKSePmiSt3;
          strMicrChgSecType4  = CKSePmiSt4;
          strMicrChgSecType5  = CKSePmiSt5;
          strMicrChgSecType6  = CKSePmiSt6;
          strMicrChgSecType7  = CKSePmiSt7;
          strMicrChgSecType8  = CKSePmiSt8;
          strMicrChgSecType9  = CKSePmiSt9;
          strMicrChgSecTypeA  = CKSePmiStA;
       Endif;
       //*** End  Add *** 06/10/21 ******************************
       Endsr;
