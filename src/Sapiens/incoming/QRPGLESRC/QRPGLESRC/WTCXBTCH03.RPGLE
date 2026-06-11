     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Cancellation Batch Detail grid.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCXBTCH
     h*  Calls to:
     h*
     h*  Created on 07/03/2023 by C:342 for Project WC2906
     h**********************************************************************
     h* Revisions:
     h*
     h* 07/28/25 c:62 Project 153WBJ
     h*               Populate batch# and Late Fee
     h*
     h**********************************************************************
     fwxcnp     if   e             disk    rename(pcn01:pcn0s)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwtcrl1    if   e           k disk

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtCxBtch03       pr                  extpgm('WTCXBTCH03')
     d                                9  0
     d                            32767
     d                                1

     dwtCxBtch03       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strAnnPay       s             20
     d strCanRsn       s             65


      /Free

       Chain inRRN pcn0s;
       Exsr BuildData;

       outJSON = ' ';
       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlEmpName' :%trim(CNenam));
       BuildJSON(outJSON :'OutDtlAnnPay' :%trim(strAnnPay));
       BuildJSON(outJSON :'OutDtlACHFlag' :%trim(CNach));
       BuildJSON(outJSON :'OutDtlNegAdj' :%trim(CNadj));
       BuildJSON(outJSON :'OutDtlNumberofCX' :%trim(%Char(CNcnc#)));
       BuildJSON(outJSON :'OutDtlVIP' :%trim(CNvip));
       BuildJSON(outJSON :'DtlOutCXR' :%trim(strCanRsn));
       //*** Begin Add *** 07/28/25 ****************************************
       BuildJSON(outJSON :'DtlOutCXBatch#' :%trim(%EditC(CNbat#:'4')));
       BuildJSON(outJSON :'DtlOutLateFee' :%trim(%EditC(CNlFee:'4')));
       //*** End   Add *** 07/28/25 ****************************************
       If CNselc = 'Y';
          BuildJSON(outJSON :'OutDtlSelCXBD' :('Yes'));
       Else;
          BuildJSON(outJSON :'OutDtlSelCXBD' :('No'));
       Endif;
       BuildJSON(outJSON :'DtlOutReinFee' :%trim(%Char(CNrfee)));
       BuildJSON(outJSON :'OutDtlComm1' :%trim(CNcom1));
       BuildJSON(outJSON :'OutDtlComm2' :%trim(CNcom2));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Annual Install Plan
       strAnnPay = *Blanks;
       If CNaip <> *Blanks;
          strAnnPay = CNaip;
          IF CNaip = 'A';
             strAnnPay = 'Annual';
          EndIf;
       EndIf;

       // Cancel Reason
       strCanRsn = *Blanks;
       If CNrecd <> *Blanks;
          strCanRsn = CNrecd;
          Chain (CNco#:CNrecd) pcr01;
          IF %found(wtcrl1);
             strCanRsn = %trim(strCanRsn) + ' - '  + %trim(CRdesc);
          EndIf;
       EndIf;



       endsr;
      /End-Free
