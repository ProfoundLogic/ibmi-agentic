     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Billing plan detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTBILLPLAN
     h*  Calls to:
     h*
     h**********************************************************************
     h* Revisions:
     h*
     h* 09/17/21 C: 079 Project: CN024
     h*                 Add portal connection
     h*
     h**********************************************************************
      * Billing Schedule Code Table File
     fwtbsp     if   e             disk    rename(pbs01:pbs0p)
     f
     d
     d/copy sPrAtrium
     d/copy sPrDate
     d
     dstrBillType      s             25a   inz(' ')
     dstrBillStat      s             10a   inz(' ')
     dstrBillPlan      s             35a   inz(' ')
     dstrEFTFlg        s              3a   inz(' ')
     dstrSROnln        s              3a   inz(' ')
     dstrfreq          s             15a   inz(' ')
     dstrWeek          s             10a   inz(' ')
     d
      * *ENTRY
     DBillPlDtlBx      pr                  ExtPgm('WTBILLPL01')
     D                                9  0
     D                            32767
     D                                1
      *
     DBillPlDtlBx      pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
      *
      /Free

       Chain(n) inRRN pbs0p;
       outJSON = ' ';

      // Billing Type
       Select;
        When BSseFL = 'E';
           strBillType = 'Estimating';
        When BSseFL = 'S';
           strBillType = 'Self-Reporting';
       EndSl;
       BuildJSON(outJSON :'OutDtlBillType': %trim(strBillType));

      // Billing Plan status
       Select;
        When BSstat = 'A';
            strBillStat = 'Active';
        When BSstat = 'I';
            strBillStat = 'In-Active';
       EndSl;
       BuildJSON(outJSON :'OutDtlBillStat' : %trim(strBillStat));

      // Billing Plan Type
       Select;
        When BSlPgm = 'I';
           strBillPlan = 'Incurred Loss Retro';
        When BSlPgm = 'P';
           strBillPlan = 'Paid Loss Retro';
        When BSlPgm = 'L';
           strBillPlan = 'Large Deductible';
        When BSlPgm = 'R';
           strBillPlan = 'Retro Paid';
        Other;
           strBillPlan= *blanks;
       Endsl;
       BuildJSON(outJSON :'OutDtlPlanType' : %trim(strBillPlan));

      // EFT Flag.
       Select;
        When BSeftF  = 'Y';
           strEFTFlg = 'Yes';
        When BSeftF  = 'N';
           strEFTFlg = 'No';
        Other;
           strEFTFlg = 'No';
       Endsl;
       BuildJSON(outJSON :'OutDtlEFTOnly' : %trim(strEFTFlg));

      // Online Self Reporting
       Select;
        When BSonli  = 'Y';
           strSROnln = 'Yes';
        When BSonli  = 'N';
           strSROnln = 'No';
        Other;
           strSROnln = 'No';
       Endsl;
       BuildJSON(outJSON :'OutDtlOnlSelfRep' : %trim(strSROnln));

      // Billing Frequency
       Select;
        When BSfreq  = 'M';
           strfreq   = 'Monthly';
        When BSfreq  = 'W';
           strfreq   = 'Weekly';
        When BSfreq  = 'T';
           strfreq   = 'Twice a Month';
        Other;
           strfreq   = '  ';
       Endsl;
       BuildJSON(outJSON :'OutDtlBillFreq' : %trim(strfreq));

      // Day of week.
       Select;
        When BSdoWk  = 'MO';
           strWeek   = 'Monday';
        When BSdoWk  = 'TU';
           strWeek   = 'Tuesday';
        When BSdoWk  = 'WE';
           strWeek   = 'Wednesday';
        When BSdoWk  = 'TH';
           strWeek   = 'Thursday';
        When BSdoWk  = 'FR';
           strWeek   = 'Friday';
        When BSdoWk  = 'SA';
           strWeek   = 'Saturday';
        When BSdoWk  = 'SU';
           strWeek   = 'Sunday';
        Other;
           strWeek   = ' ';
       Endsl;
       BuildJSON(outJSON :'OutDtlDayweek' : %trim(strWeek));

      // Billing Plan code
       BuildJSON(outJSON :'OutDtlBillPlanCod' : %trim(BSkey));
      // Billing Plan description
       BuildJSON(outJSON :'OutDtlDesc' : %trim(BSdesc));
      // EFT Starting Period
       If BSdbSd > *Zeros;
          BuildJSON(outJSON :'OutDtlEFTStrtPrd' : %char(BSdbSd));
       Else;
          BuildJSON(outJSON :'OutDtlEFTStrtPrd' : ' ');
       EndIf;

      // Billing Code rollover
       If BSrlCd <> '000';
          BuildJSON(outJSON :'OutDtlBillCodRoll' : %trim(BSrlCd));
       Else;
          BuildJSON(outJSON :'OutDtlBillCodRoll' : ' ');
       EndIf;

       BuildJSON(outJSON :'OutDtlDepP1' : ' ');
       BuildJSON(outJSON :'OutDtlDepP2' : ' ');

      // Estimating Deposit % Period 1 and Period 2.
       If BSseFL = 'E';
          BuildJSON(outJSON :'OutDtlDepP1' : %char(%dec(BSesdp:7:4)));
          BuildJSON(outJSON :'OutDtlDepP2' : %char(%dec(BSesd2:7:4)));
       EndIf;

      // Self Reporting Deposit % Period 1 and Period 2.
       If BSseFL = 'S';
          BuildJSON(outJSON :'OutDtlDepP1' : %char(%dec(BSab01*100:9:6)));
          BuildJSON(outJSON :'OutDtlDepP2' : %char(%dec(BSab02*100:9:6)));
       EndIf;

      // Estimating Billing % per Period
       If BSseFL = 'E';
          BuildJSON(outJSON :'LblDtlEstBilling%': 'Estimating Billing % -
           Per Period');
          BuildJSON(outJSON :'LblDtlEstBilling%P1' : 'Period 1');
          BuildJSON(outJSON :'LblDtlEstBilling%P2' : 'Period 2');
          BuildJSON(outJSON :'LblDtlEstBilling%P3' : 'Period 3');
          BuildJSON(outJSON :'LblDtlEstBilling%P4' : 'Period 4');
          BuildJSON(outJSON :'LblDtlEstBilling%P5' : 'Period 5');
          BuildJSON(outJSON :'LblDtlEstBilling%P6' : 'Period 6');
          BuildJSON(outJSON :'LblDtlEstBilling%P7' : 'Period 7');
          BuildJSON(outJSON :'LblDtlEstBilling%P8' : 'Period 8');
          BuildJSON(outJSON :'LblDtlEstBilling%P9' : 'Period 9');
          BuildJSON(outJSON :'LblDtlEstBilling%P10': 'Period 10');
          BuildJSON(outJSON :'LblDtlEstBilling%P11': 'Period 11');
          BuildJSON(outJSON :'LblDtlEstBilling%P12': 'Period 12');
          BuildJSON(outJSON :'OutDtlEstBill%P1' :%char(%dec(BSab01*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P2' :%char(%dec(BSab02*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P3' :%char(%dec(BSab03*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P4' :%char(%dec(BSab04*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P5' :%char(%dec(BSab05*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P6' :%char(%dec(BSab06*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P7' :%char(%dec(BSab07*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P8' :%char(%dec(BSab08*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P9' :%char(%dec(BSab09*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P10':%char(%dec(BSab10*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P11':%char(%dec(BSab11*100:9:6)));
          BuildJSON(outJSON :'OutDtlEstBill%P12':%char(%dec(BSab12*100:9:6)));
       Else;
          BuildJSON(outJSON :'LblDtlEstBilling%'   : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P1' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P2' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P3' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P4' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P5' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P6' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P7' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P8' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P9' : ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P10': ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P11': ' ');
          BuildJSON(outJSON :'LblDtlEstBilling%P12': ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P1' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P2' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P3' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P4' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P5' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P6' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P7' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P8' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P9' : ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P10': ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P11': ' ');
          BuildJSON(outJSON :'OutDtlEstBill%P12': ' ');
       EndIf;

      // Maximum deposit Amount
       BuildJSON(outJSON:'OutDtlMaxDepAmt':%char(BSmaxDep));

       //*** Beg Add *** 09/17/21 ***************************************
       // Portal
       BuildJSON(outJSON:'OutDtlPortal':BSportal);
       //*** End Add *** 09/17/21 ***************************************

       RtnCode = *blanks;
       Return;

      /End-Free
