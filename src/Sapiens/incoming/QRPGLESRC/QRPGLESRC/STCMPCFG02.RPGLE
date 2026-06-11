     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*  Description: Response program to serve Vendor Maintenance detail box
     h*               in Company Configuration.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*  Called from: STCMPCFG
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/08/18 C:136 Project: 150WAQ
     h*                Add flags to control collateral processing (for
     h*                Sunz only)
     h*
     H* 05/10/18 C:TXO PROJECT: PHW70121
     H*                AutoTransfer A/R billing batch
     H*                Auto transfer A/R batch payments
     h*
     H* 07/21/17 C:267 PROJECT: 150WAN
     H*                ADD EMPLOYEE INFO REQUIRED CHECKBOX
     H*                ADD '# OF DAYS FOR CHECK ISSUANCE'
     H*                ADD GENERATE PREMIUM NOTICE CHECKBOX
     H*                to Vendor Information below grid
      **********************************************************************
      * Payroll Vendor File
     fwmvpp     if   e             disk    rename(pvp01:pvp0p)
     f
     fwtabl     if   e           k disk
     f                                     include(ptt01)
     d/copy sPrAtrium
     d/copy sPrString
     d/copy sPrDate
      * *ENTRY
     DVendorDtl01      pr                  ExtPgm('WTCMPCFG02')
     D                                9  0
     D                            32767
     D                                1
      *
     DVendorDtl01      pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     d strAdL1         s             40    inz
     d strAdL2         s             40    inz
     d strAdL3         s             40    inz
     d strvalue        s             40    inz
      *
      /Free

       Chain(n) inRRN pvp0p;

       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlVenNum' : %editc(VPvdr#:'X'));
       BuildJSON(outJSON :'OutDtlVenName' : %trim(VPname));

       FormatAddress(VPadr1:VPadr2:VPcity:VPst:VPzip:strAdL1:strAdL2:
        strAdL3);
       BuildJSON(outJSON :'OutDtlVenAdd1' : %trim(strAdL1));
       BuildJSON(outJSON :'OutDtlVenAdd2' : %trim(strAdL2));
       BuildJSON(outJSON :'OutDtlVenCity' : %trim(strAdL3));

       BuildJSON(outJSON :'OutDtlVenIntAdd' : %trim(VPiAdr));
       BuildJSON(outJSON :'OutDtlVenEmlAd' : %trim(VPemal));
       BuildJSON(outJSON :'OutDtlVenEffDate' :
                         alphaUSA(cvtToDate(dtMDYY(VPefDt))));

       BuildJSON(outJSON :'OutDtlVenExpDate' :
                         alphaUSA(cvtToDate(dtMDYY(VPexDt))));

       BuildJSON(outJSON :'OutDtlPrmExp' : VPexNp);
       BuildJSON(outJSON :'OutDtlAddDivExp' : VPexAl);
       BuildJSON(outJSON :'OutDtlWrtPolHdr' : VPpTyc);

       Select;
        When VPpyFr = 'W';
           strvalue = 'W-Weekly';
        When VPpyFr = 'M';
           strvalue = 'M-Monthly';
        When VPpyFr = 'A';
           strvalue = 'A-Annually';
        other;
           strvalue = ' ';
       EndSl;
       BuildJSON(outJSON :'OutDtlVenPUFrq' : strvalue);

       strvalue = ' ';
       Select;
        When VPpmFr = 'W';
           strvalue = 'W-Weekly';
        When VPpmFr = 'M';
           strvalue = 'M-Monthly';
        other;
           strvalue = ' ';
       EndSl;
       BuildJSON(outJSON :'OutDtlVenPrmFreq' : strvalue);

       strvalue = ' ';
       Select;
        When VPdnLf = 'D';
           strvalue = 'D-Daily';
        When VPdnLf = 'W';
           strvalue = 'W-Weekly';
        When VPdnLf = 'M';
           strvalue = 'M-Monthly';
        other;
           strvalue = ' ';
       EndSl;

       BuildJSON(outJSON :'OutDtlVenDwnFreq' : strvalue);
       BuildJSON(outJSON :'OutDtlBldRate' : VPblnd);
       BuildJSON(outJSON :'OutDtlVenDateFmt' : VPdFmt);
       BuildJSON(outJSON :'OutDtlVenFileTyp' : VPfTyp);
       BuildJSON(outJSON :'OutDtlVenDataTyp' : VPdTyp);

       strvalue = ' ';
       If VPppCd <> *blanks;
          chain (VPCO#:VPppCd) ptt01;
          If %found;
             strvalue  = VPppCd + '-' + %trim(TTdesc);
          Else;
             strvalue  = *blanks;
          EndIf;
       EndIf;
       BuildJSON(outJSON :'OutDtlVenPrmCd' : strvalue);

       strvalue = ' ';
       If VPcpCd <> *blanks;
          chain (VPCO#:VPcpCd) ptt01;
          If %found;
             strvalue  = VPcpCd + '-' + %trim(TTdesc);
          Else;
             strvalue  = *blanks;
          EndIf;
       EndIf;
       BuildJSON(outJSON :'OutDtlVenCmCd' : strvalue);

       strvalue = ' ';
       If VPpbcd <> *blanks;
          chain (VPCO#:VPpbcd) ptt01;
          If %found;
             strvalue  = VPpbcd + '-' + %trim(TTdesc);
          Else;
             strvalue  = *blanks;
          EndIf;
       EndIf;
       BuildJSON(outJSON :'OutDtlVenBlCd' : strvalue);

       //** Begin Add *** 05/10/18 ****************************************
       //BuildJSON(outJSON :'OutDtlVenAutoTrf' : VPauTr);
       BuildJSON(outJSON :'OutDtlVenAutoBillTrf' : VPauTr);
       BuildJSON(outJSON :'OutDtlVenAutoPayTrf' : VPTRANPAY);
       //** End   Add *** 05/10/18 ****************************************
       BuildJSON(outJSON :'OutDtlGenPayUp' : VPpyUp);
       BuildJSON(outJSON :'OutDtlVenNtRate' : VPnet);
       BuildJSON(outJSON :'OutDtlVenIncMth' : VPmtHi);
       //** Begin Add *** 07/21/17 ****************************************
       BuildJSON(outJSON :'OutEmpInfReq'    : VPEMPREQ);
       BuildJSON(outJSON :'OutGenPrmNotice' : VPGENPRM);
       BuildJSON(outJSON :'OutDysCkIssue'   : %editc(VP#OFDAYS:'X'));
       //** End   Add *** 07/21/13 ****************************************
       //*** Beg Add ***** 06/08/18 ************************
       BuildJSON(outJSON :'OutGenerateCollateralA/R'          : VPWRTCOLL );
       BuildJSON(outJSON :'OutAutoTransferCollateralBillings' : VPAUTRCBIL);
       BuildJSON(outJSON :'OutAutoTransferCollateralPayments' : VPAUTRCPAY);
       //*** End Add ***** 06/08/18 ************************

       RtnCode = *blanks;
       Return;

      /End-Free
