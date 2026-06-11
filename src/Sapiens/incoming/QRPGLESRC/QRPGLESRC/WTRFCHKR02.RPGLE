     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to get visibility Information
     h*               for return fund check requests
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRFCHKREQ
     h*  Calls to:
     h*
     h*  Created on 10/19/2023 by C:342 for Project WC2676
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwar4p     if   e             disk    rename(pr401:pr40p)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtRfChkR02       pr                  extpgm('WTRFCHKR02')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dwtRfChkR02       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d visbAgent       s              1    inz
     d visbPFiPTyp     s              1    inz
     d visbARStMt      s              1    inz
     d visbCustID      s              1    inz
     d isnotIndpdt     s              1    inz

      /Free

       Chain inRRN pr40p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       //Check visibility of Agent/Premium Fin/Payee type
       If R4acfl = 'Y';
          visbAgent = 'Y';
          visbPFiPTyp = 'N';
       Else;
          visbAgent = 'N';
          visbPFiPTyp = 'Y';
       Endif;

       //Check visibility of Customer ID
       If R4btyp <> 'A';
          visbCustID = 'Y';
       Else;
          visbCustID = 'N';
       Endif;

       If R4btyp <> 'I';
          isnotIndpdt = 'Y';
       Else;
          isnotIndpdt = 'N';
       Endif;

       //Check visibility of AR code and Statement.
       If visbAgent = 'Y' and isnotIndpdt = 'Y';
          visbARStMt = 'Y';
       Else;
          visbARStMt = 'N';
       Endif;

       BuildJSON(outJSON :'Points' :visbAgent);
       BuildJSON(outJSON :'Points1' :visbPFiPTyp);
       BuildJSON(outJSON :'Points2' :visbARStMt);
       BuildJSON(outJSON :'Points3' :visbCustID);

       RtnCode = ' ';
       Return;

      /End-Free
