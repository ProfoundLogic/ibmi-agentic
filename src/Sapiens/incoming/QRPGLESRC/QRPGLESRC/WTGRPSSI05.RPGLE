     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Small Deductibles Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPSSI
     h*  Calls to:
     h*
     h*  Created on 05/29/2020 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h* 08/08/22 C: 079 Project: WC2859
rp   h*                 Return JSON nodes for PV1 Codes
     h*
     h**********************************************************************
     fwtddp     if   e             disk    rename(pdd01:pdd0s)
     fwtgtl     if   e           k disk
     fwtrtl1    if   e           k disk
     f*** Beg Add *** 08/08/22 *********************************************
     fwtabl     if   e           k disk
     f                                     ignore(prt01)
     f*** End Add *** 08/08/22 *********************************************

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpSSI05       pr                  extpgm('WTGRPSSI05')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpSSI05       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strStatus       s             10
     d strPlnDesc      s            200
     d strDc           s            200
     d*** Beg Add *** 08/08/22 ***********************************************
       Dcl-s strPv1 Char(200) Inz;
     d*** End Add *** 08/08/22 ***********************************************
     d*

     c     KeyWtgtl      klist
     c                   kfld                    DDco#
     c                   kfld                    kktbid            4
     c                   kfld                    kkkeyGT          10

     c     KeyWtrtl1     klist
     c                   kfld                    DDco#
     c                   kfld                    kkkeyRT          10
     c*** Beg Add *** 08/08/22 ***********************************************
     c     KeyWtB2       klist
     c                   kfld                    DDco#
     c                   kfld                    kkkeyB2          10
     c*** End Add *** 08/08/22 ***********************************************

       Chain inRRN pdd0s;
       ExSr  BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateSD':%trim(DDst));
       BuildJSON(outJSON :'OutDtlEffDateSD':
                         alphaUSA(cvtToDate(dtMDYY(DDefdt))));
       BuildJSON(outJSON :'OutDtlDiscountCodeSD':%trim(DDcode));
       BuildJSON(outJSON :'OutDtlDescriptionSD':%trim(DDdesc));
       If DDtype = 'G';
          BuildJSON(outJSON :'OutDtlGrossNet':'Gross');
       Endif;
       If DDtype = 'N';
          BuildJSON(outJSON :'OutDtlGrossNet':'Net');
       Endif;
       BuildJSON(outJSON :'OutDtlDiscountPercentage':%editc(DDpct:'N'));
       BuildJSON(outJSON :'OutDtlAmountPerOccurence':%editc(DDampo:'N'));
       BuildJSON(outJSON :'OutDtlAmountAggregate':%editc(DDamag:'N'));
       BuildJSON(outJSON :'CbxDtlVariable':%trim(DDvara));
       BuildJSON(outJSON :'OutDtlClassCode':%char(DDclas));
       BuildJSON(outJSON :'OutDtlDeductiblePlan':%trim(strPlnDesc));
       If DDverb = 'C';
          BuildJSON(outJSON :'OutDtlVerbiagePerClmOrPerOccur':'Per Claim');
       Endif;
       If DDverb = ' ';
          BuildJSON(outJSON :'OutDtlVerbiagePerClmOrPerOccur':'Per Occurrence');
       Endif;
       BuildJSON(outJSON :'CbxDtlIncludeALAE':%trim(DDalae));
       BuildJSON(outJSON :'CbxDtlUseTotalIncurredCalDeductAmt':%trim(DDtoti));
       If DDIncMdIn = 'B' or DDIncMdIn = ' ';
          BuildJSON(outJSON:'OutDtlIncldMedIndemnity':'Medical and Indemnity');
       Endif;
       If DDIncMdIn = 'M';
          BuildJSON(outJSON:'OutDtlIncldMedIndemnity':'Medical only');
       Endif;
       If DDIncMdIn = 'I';
          BuildJSON(outJSON:'OutDtlIncldMedIndemnity':'Indemnity only');
       Endif;
       If DDrnw = *Blanks;
          BuildJSON(outJSON :'CbxDtlAutoRenew':'Y');
       Else;
          BuildJSON(outJSON :'CbxDtlAutoRenew':DDrnw);
       Endif;

       If DDdc01 <> *Blanks;
          kkkeyRT = DDdc01;
          StrDc = '(1) ';
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves1':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves1':*Blanks);
       Endif;

       If DDdc02 <> *Blanks;
          StrDc = '(2) ';
          kkkeyRT = DDdc02;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves2':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves2':*Blanks);
       Endif;

       If DDdc03 <> *Blanks;
          StrDc = '(3) ';
          kkkeyRT = DDdc03;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves3':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves3':*Blanks);
       Endif;

       If DDdc04 <> *Blanks;
          StrDc = '(4) ';
          kkkeyRT = DDdc04;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves4':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves4':*Blanks);
       Endif;

       If DDdc05 <> *Blanks;
          StrDc = '(5) ';
          kkkeyRT = DDdc05;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves5':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves5':*Blanks);
       Endif;

       If DDdc06 <> *Blanks;
          StrDc = '(6) ';
          kkkeyRT = DDdc06;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves6':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves6':*Blanks);
       Endif;

       If DDdc07 <> *Blanks;
          StrDc = '(7) ';
          kkkeyRT = DDdc07;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves7':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves7':*Blanks);
       Endif;

       If DDdc08 <> *Blanks;
          StrDc = '(8) ';
          kkkeyRT = DDdc08;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves8':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves8':*Blanks);
       Endif;

       If DDdc09 <> *Blanks;
          StrDc = '(9) ';
          kkkeyRT = DDdc09;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves9':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves9':*Blanks);
       Endif;

       If DDdc10 <> *Blanks;
          StrDc = '(10) ';
          kkkeyRT = DDdc10;
          Chain KeyWtrtl1 prt01;
          If %found(wtrtl1) ;
          strDc = %trim(strDc) + ' '+Rtkey + ' - '  + RTdesc
          + '(' + RTtype + ')';
             BuildJSON(outJSON :'OutDtlDeductReserves10':%trim(StrDc));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeductReserves10':*Blanks);
       Endif;
       //*** Beg Add *** 08/08/22 *********************************************
       If DDpv1Db01 <> *Blanks;
          StrPv1 = '(1) ';
          kkkeyB2 = DDpv1Db01;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv101':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv101':*Blanks);
       Endif;
       If DDpv1Db02 <> *Blanks;
          StrPv1 = '(2) ';
          kkkeyB2 = DDpv1Db02;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv102':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv102':*Blanks);
       Endif;
       If DDpv1Db03 <> *Blanks;
          StrPv1 = '(3) ';
          kkkeyB2 = DDpv1Db03;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv103':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv103':*Blanks);
       Endif;
       If DDpv1Db04 <> *Blanks;
          StrPv1 = '(4) ';
          kkkeyB2 = DDpv1Db04;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv104':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv104':*Blanks);
       Endif;
       If DDpv1Db05 <> *Blanks;
          StrPv1 = '(5) ';
          kkkeyB2 = DDpv1Db05;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv105':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv105':*Blanks);
       Endif;
       If DDpv1Db06 <> *Blanks;
          StrPv1 = '(6) ';
          kkkeyB2 = DDpv1Db06;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv106':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv106':*Blanks);
       Endif;
       If DDpv1Db07 <> *Blanks;
          StrPv1 = '(7) ';
          kkkeyB2 = DDpv1Db07;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv107':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv107':*Blanks);
       Endif;
       If DDpv1Db08 <> *Blanks;
          StrPv1 = '(8) ';
          kkkeyB2 = DDpv1Db08;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv108':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv108':*Blanks);
       Endif;
       If DDpv1Db09 <> *Blanks;
          StrPv1 = '(9) ';
          kkkeyB2 = DDpv1Db09;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv109':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv109':*Blanks);
       Endif;
       If DDpv1Db10 <> *Blanks;
          StrPv1 = '(10) ';
          kkkeyB2 = DDpv1Db10;
          Chain KeyWtB2 pb201;
          If %found(WTABL) and B2stat <> 'I';
          strPv1 = %trim(strPv1) + ' ' + B2key + ' - '  + B2desc;
             BuildJSON(outJSON :'OutDtlDeathBenefitPv110':%trim(StrPv1));
          Endif;
       Else;
          BuildJSON(outJSON :'OutDtlDeathBenefitPv110':*Blanks);
       Endif;
       //*** End Add *** 08/08/22 *********************************************
       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Deductible Plan Description;
       strPlnDesc = %trim(DDplan);
       If DDplan <> *Blanks;
          kktbid = %trim(DDst)+'DT';
          kkkeyGT = DDplan;
          Chain KeyWtgtl pgt01;
          IF %found(WTGTL) ;
             strPlnDesc = %trim(strPlnDesc) + ' - '  + GTdesc;
          Else;
             kktbid = '  DT';
             Chain KeyWtgtl pgt01;
             If %found(WTGTL) ;
                strPlnDesc = %trim(strPlnDesc) + ' - '  + GTdesc;
             Endif;
          EndIf;
       EndIf;


       endsr;
      /End-Free
