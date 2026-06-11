     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Cancellation Reason Codes
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTTBLCXCOD
     h*  Calls to:
     h*
     h*  Created on 11/25/2015 by C:212 for Project WC2399
     h**********************************************************************
     h* Revisions:
     h*
     h* 07/28/25 C:062  Project 153WBJ
     h*                 Add late fee, late fee billing code and              edit issue
     h*                 reinstatement billing code
     h*
     h* 09/18/24 C:333  Support: 117544
     h*                          Cancellation reason code configuration date edit issue
     h*
     h**********************************************************************
     fwtcrp     if   e             disk    rename(pcr01:pcr0p)
     fwtgtl     if   e           k disk
     f*** Begin Add *** 07/28/25 *******************************************
     fwtabl     if   e           k disk
     f                                     Ignore(prs01)
     f                                     Ignore(pcc01)
     f                                     Ignore(pcn01)
     f*** End   Add *** 07/28/25 *******************************************

      /copy sPrAtrium

      * *ENTRY
     dWTTBLCXCD1       pr                  extpgm('WTTBLCXCD1')
     d                                9  0
     d                            32767
     d                                1

     dWTTBLCXCD1       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strState        s              2
      *** Beg Del ***** 09/18/24 *****************************
     d*strCode         s             10
     d*strEfDt         s             10
      *** End Del ***** 09/18/24 *****************************
     d strDesc         s             60
     d strCdSts        s             10
     d strNCCI         s              2
     d strNCCIDes      s             40
     d strSurPct       s             10
     d strSurCd        s              5
     d strCertNot      s              3
     d strNotDay       s              5
     d strAdlDay       s              5
     d strCertLtr      s              3
     d strNewCxlDay    s              5
     d strNewAdlDay    s              5
     d strCxlOvr       s              3
     d strReinFee      s             10
     d strIncpNewDay   s              5
     d strAutoRein     s              3
     d strCxReinDay    s              5
     d strAudTrk       s              3
     d strPctBalRein   s             10
     d strBilThruDt    s              3
     d strAutoCxlCfm   s              3
     d strAutoCnfDay   s              5
     d*** Begin Add *** 07/28/25 *******************************************
     d strLateFeeAmt   s             10
     d strLateFeeCode  s             40
     d strReinFeeCode  s             40
     d*** End   Add *** 07/28/25 *******************************************

     d ##tbid          s              4
     d ##key           s             10

     c     keyWTGTL      Klist
     c                   Kfld                    CRco#
     c                   Kfld                    ##tbid
     c                   Kfld                    ##key

      /Free

       Chain inRRN pcr0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlCxCdState'     : %trim(strState));
       //*** Beg Del ***** 09/18/24 *****************************
       //***BuildJSON(outJSON :'OutDtlCxCdCode'      : %trim(strCode));
       //***BuildJSON(outJSON :'OutDtlCxCdEfDt'      : %trim(strEfDt));
       //*** End Del ***** 09/18/24 *****************************
       BuildJSON(outJSON :'OutDtlCxCdDesc'      : %trim(strDesc));
       BuildJSON(outJSON :'OutDtlCxCdStatus'    : %trim(strCdSts));
       BuildJSON(outJSON :'OutDtlCxCdNCCI'      : %trim(strNCCI));
       BuildJSON(outJSON :'OutDtlCxCdNCCIDes'   : %trim(strNCCIDes));
       BuildJSON(outJSON :'OutDtlCxCdSurPct'    : %trim(strSurPct));
       BuildJSON(outJSON :'OutDtlCxCdSurCd'     : %trim(strSurCd));
       BuildJSON(outJSON :'OutDtlCxCdCertNot'   : %trim(strCertNot));
       BuildJSON(outJSON :'OutDtlCxCdNotDay'    : %trim(strNotDay));
       BuildJSON(outJSON :'OutDtlCxCdAdlDay'    : %trim(strAdlDay));
       BuildJSON(outJSON :'OutDtlCxCdCertLtr'   : %trim(strCertLtr));
       BuildJSON(outJSON :'OutDtlCxCdNewCxlDay' : %trim(strNewCxlDay));
       BuildJSON(outJSON :'OutDtlCxCdNewAdlDay' : %trim(strNewAdlDay));
       BuildJSON(outJSON :'OutDtlCxCdCxlOvr'    : %trim(strCxlOvr));
       BuildJSON(outJSON :'OutDtlCxCdReinFee'   : %trim(strReinFee));
       BuildJSON(outJSON :'OutDtlCxCdIncpNewDay': %trim(strIncpNewDay));
       BuildJSON(outJSON :'OutDtlCxCdAutoRein'  : %trim(strAutoRein));
       BuildJSON(outJSON :'OutDtlCxCdCxReinDay' : %trim(strCxReinDay));
       BuildJSON(outJSON :'OutDtlCxCdAudTrk'    : %trim(strAudTrk));
       BuildJSON(outJSON :'OutDtlCxCdPctBalRein': %trim(strPctBalRein));
       BuildJSON(outJSON :'OutDtlCxCdBilThruDt' : %trim(strBilThruDt));
       BuildJSON(outJSON :'OutDtlCxCdAutoCxlCfm': %trim(strAutoCxlCfm));
       BuildJSON(outJSON :'OutDtlCxCdAutoCnfDay': %trim(strAutoCnfDay));
       //*** Begin Add *** 07/28/25 ****************************************
       BuildJSON(outJSON :'OutDtlCxCdLateFee': %trim(strLateFeeAmt));
       BuildJSON(outJSON :'OutDtlCxCdReinFeeARCode': %trim(strReinFeeCode));
       BuildJSON(outJSON :'OutDtlCxCdLateFeeARCode': %trim(strLateFeeCode));
       //*** End   Add *** 07/28/25 ****************************************


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // State
       strState = CRst;
       //*** Beg Del ***** 09/18/24 *****************************
       //***strCode =  CRkey;
       //***strEfDt =  %char(%date(CRefdt:*cymd):*mdy);
       //*** End Del ***** 09/18/24 *****************************
       strDesc =  CRdesc;
       If CRstat = 'A';
          strCdSts = 'Active';
       Else;
          strCdSts = 'Inactive';
       Endif;
       strNCCI =  CRNCCI;
       If CRNCCI <> *blanks;
          ##tbid = 'CXCD';
          ##key = CRNCCI;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strNCCIDes = %trim(GTdesc);
          Else;
             strNCCIDes = CRNCCI;
          Endif;
       Endif;
       strSurPct = %char(CRupsp);
       strSurCd =  %char(CRupsc);
       If CRcert = 'Y';
          strCertNot = 'Yes';
       Else;
          strCertNot = 'No';
       Endif;
       strNotDay =  %char(CRnotc);
       strAdlDay =  %char(CRaddm);
       If CRpchl = 'Y';
          strCertLtr = 'Yes';
       Else;
          strCertLtr = 'No';
       Endif;
       strNewCxlDay = %char(CRntnp);
       strNewAdlDay = %char(CRmdnp);
       strCxlOvr = CRidov;
       strReinFee = %char(CRrfee);
       strIncpNewDay = %char(CRndnw);
       If CRarei = 'Y';
          strAutoRein = 'Yes';
       Else;
          strAutoRein = 'No';
       Endif;
       strCxReinDay = %char(CRrday);
       If CRatrk = 'Y';
          strAudTrk = 'Yes';
       Else;
          strAudTrk = 'No';
       Endif;
       strPctBalRein = %char(CRpcnt);
       strBilThruDt  = CRice;
       If CRacnf = 'Y';
          strAutoCxlCfm = 'Yes';
       Else;
          strAutoCxlCfm = 'No';
       Endif;
       strAutoCnfDay = %char(CRcday);

       //*** Begin Add *** 07/28/25 ****************************************
       //
       // Reinstatement Billing Code
       Chain (CRco#:CRrfCd) ptt01;
       If %Found(wtabl);
          strReinFeeCode = %Trim(TTkey) + '-' + %Trim(TTdesc);
       EndIf;

       //
       // Late fee
       strLateFeeAmt = %EditC(CRlFee:'3');

       //
       // Late Fee Billing Code
       Chain (CRco#:CRlfCd) ptt01;
       If %Found(wtabl);
          strLateFeeCode = %Trim(TTkey) + '-' + %Trim(TTdesc);
       EndIf;
       //*** End   Add *** 07/28/25 ****************************************


       endsr;
      /End-Free
