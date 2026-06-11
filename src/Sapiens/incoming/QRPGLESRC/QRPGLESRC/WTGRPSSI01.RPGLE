     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Expense Constant Information
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
     fwdecp     if   e             disk    rename(pec01:pec0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpSSI01       pr                  extpgm('WTGRPSSI01')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpSSI01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strStatus       s             10
     d strPlnType      s             20


      /Free

       Chain inRRN pec0s;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateEC':%trim(ECst));
       BuildJSON(outJSON :'OutDtlAmount':%char(ECexco));
       BuildJSON(outJSON :'OutDtlMinAmount':%char(ECmpec));
       BuildJSON(outJSON :'OutDtlEffDateEC':
                         alphaUSA(cvtToDate(dtMDYY(ECefdt))));
       BuildJSON(outJSON :'CbxDtlChgFullOnShortTerm':%trim(ECsful));
       BuildJSON(outJSON :'OutDtlStandardPremium1':%editc(ECstpr:'N'));
       BuildJSON(outJSON :'OutDtlStandardPremium2':%editc(ECstp3:'N'));
       BuildJSON(outJSON :'OutDtlExpenseConstant1':%char(ECexc2));
       BuildJSON(outJSON :'OutDtlExpenseConstant2':%char(ECexc3));

       rtnCode = ' ';
       Return;

      /End-Free
