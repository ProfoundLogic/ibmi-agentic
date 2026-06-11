     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Agent Rate Master Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPPOL
     h*  Calls to:
     h*
     h*  Created on 05/29/2020 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwmarp     if   e             disk    rename(par01:par0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dWTGrpPol04       pr                  extpgm('WTGRPPOL04')
     d                                9  0
     d                            32767
     d                                1

     dWTGrpPol04       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strPlnType      s             20


      /Free

       Chain inRRN par0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlRateNumber' :%trim(%char(ARrt#)));
       BuildJSON(outJSON :'OutDtlDescription':%trim(ARdesc));
       BuildJSON(outJSON :'OutDtlNewRate':%trim(%char(ARnwrt)));
       BuildJSON(outJSON :'OutDtlRenewRate':%trim(%char(ARrnrt)));
       BuildJSON(outJSON :'OutDtlRateEffDate' :
                         alphaUSA(cvtToDate(dtMDYY(ARefdt))));
       BuildJSON(outJSON :'OutDtlPlanType' :%trim(strPlnType));
       BuildJSON(outJSON :'OutCbxExcludeExpenseConstant' :%trim(ARexpconst));
       BuildJSON(outJSON :'OutDtlType1' :%trim(ARsectyp01));
       BuildJSON(outJSON :'OutDtlType2' :%trim(ARsectyp02));
       BuildJSON(outJSON :'OutDtlType3' :%trim(ARsectyp03));
       BuildJSON(outJSON :'OutDtlType4' :%trim(ARsectyp04));
       BuildJSON(outJSON :'OutDtlType5' :%trim(ARsectyp05));
       BuildJSON(outJSON :'OutDtlType6' :%trim(ARsectyp06));
       BuildJSON(outJSON :'OutDtlType7' :%trim(ARsectyp07));
       BuildJSON(outJSON :'OutDtlType8' :%trim(ARsectyp08));
       BuildJSON(outJSON :'OutDtlType9' :%trim(ARsectyp09));
       BuildJSON(outJSON :'OutDtlType10':%trim(ARsectyp10));

       BuildJSON(outJSON :'OutDtlLevel1' :%trim(%char(ARseclvl01)));
       BuildJSON(outJSON :'OutDtlLevel2' :%trim(%char(ARseclvl02)));
       BuildJSON(outJSON :'OutDtlLevel3' :%trim(%char(ARseclvl03)));
       BuildJSON(outJSON :'OutDtlLevel4' :%trim(%char(ARseclvl04)));
       BuildJSON(outJSON :'OutDtlLevel5' :%trim(%char(ARseclvl05)));
       BuildJSON(outJSON :'OutDtlLevel6' :%trim(%char(ARseclvl06)));
       BuildJSON(outJSON :'OutDtlLevel7' :%trim(%char(ARseclvl07)));
       BuildJSON(outJSON :'OutDtlLevel8' :%trim(%char(ARseclvl08)));
       BuildJSON(outJSON :'OutDtlLevel9' :%trim(%char(ARseclvl09)));
       BuildJSON(outJSON :'OutDtlLevel10':%trim(%char(ARseclvl10)));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        strPlnType = *Blanks;
        If ARplan= 'A';
           strPlnType = 'Agency Bill';
        ElseIf  ARplan = 'W';
           strPlnType = 'Written Premium';
        Endif;

       endsr;
      /End-Free
