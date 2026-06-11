     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Increased Limit Information
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
     h**********************************************************************
     fwtlmp     if   e             disk    rename(plm01:plm0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpSSI03       pr                  extpgm('WTGRPSSI03')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpSSI03       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10

     c     *dtaara       define    minprm        usrdat

      /Free

       Chain inRRN plm0s;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateIL':%trim(LMst));
       BuildJSON(outJSON :'OutDtlEffDateIL':
                         alphaUSA(cvtToDate(dtMDYY(LMefdt))));
       BuildJSON(outJSON :'OutDtlLimitCode':%trim(LMkey));
       BuildJSON(outJSON :'OutDtlDescriptionIL':%trim(LMdesc));
       BuildJSON(outJSON :'OutDtlBodilyInjurybyAccident':%editc(LMacc:'N'));
       BuildJSON(outJSON :'OutDtlBodilyInjurybyDisease':%editc(LMdis:'N'));
       BuildJSON(outJSON :'OutDtlBodilyInjurybyDiseaseAgg':%editc(LMagg:'N'));
       In usrdat;
       If usrid <> *blank;
          BuildJSON(outJSON :'OutDtlMinimumPremium':%editc(LMmine:'N'));
       Else;
          BuildJSON(outJSON :'OutDtlMinimumPremium':%editc(LMmin:'N'));
       Endif;
       BuildJSON(outJSON :'OutDtlPercentageIL':%editc(LMpct:'N'));
       If LMrnw = *Blanks;
          BuildJSON(outJSON :'CbxDtlAutoRenew':'Y');
       Else;
          BuildJSON(outJSON :'CbxDtlAutoRenew':'LMrnw');
       Endif;
       BuildJSON(outJSON :'OutDtlStatisticalReportingCode':%char(LMclas));

       rtnCode = ' ';
       Return;

      /End-Free
