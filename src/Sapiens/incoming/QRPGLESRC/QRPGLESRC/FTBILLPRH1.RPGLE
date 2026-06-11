     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Upload History Screen (FTBILLPRXH)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: FTBILLPRXH
     h*  Calls to:
     h*
     h*  Created on 04/12/2016 by C:309 for Project 114MXO
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     ffak9p     if   e             disk

      /copy sPrAtrium

      * *ENTRY
     dFTBILLPRX1       pr                  extpgm('FTBILLPRX1')
     d                                9  0
     d                            32767
     d                                1

     dFTBILLPRX1       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strTtlRCount    s              9
     d strNbrOfDup     s              9
     d strClmErCount   s              9
     d strBillTrnsfrd  s              9
     d strdMbr         s             10
     d strUpldDate     s              7
     d strUpldTime     s              6
     d strPrcdDate     s              7
     d strPrcsdTime    s              6
      /Free

        Chain inRRN pk901;
        Exsr BuildData;
        outJSON = ' ';

       // Populate Detail Box information.
        BuildJSON(outJSON :'DtlOutFldMbr':%trim(strdMbr));
        BuildJSON(outJSON :'DtlOutUpldDate':%trim(strUpldDate));
        BuildJSON(outJSON :'DtlOutUpldTime':%trim(strUpldTime));
        BuildJSON(outJSON :'DtlOutPrcdDate':%trim(strPrcdDate));
        BuildJSON(outJSON :'DtlOutPrcsdTime':%trim(strPrcsdTime));
        BuildJSON(outJSON :'DtlOutTtlRcCount':%trim(strTtlRCount));
        BuildJSON(outJSON :'DtlOutNbrOfDup':%trim(strNbrOfDup));
        BuildJSON(outJSON :'DtlOutClmErCount':%trim(strClmErCount));
        BuildJSON(outJSON :'DtlOutBillTnsfrd':%trim(strBillTrnsfrd));

        rtnCode = ' ';
        Return;

      //===================================================================
      // Buld Data that will be passed on to AJAX
      //===================================================================
       Begsr BuildData;

         strdMbr          =  %Trim(K9MBR);
         strUpldDate      =  %Trim(%Char(K9ULDT));
         strUpldTime      =  %Trim(%Char(K9ULTM));
         strPrcdDate      =  %Trim(%Char(K9PRDT));
         strPrcsdTime     =  %Trim(%Char(K9PRTM));
         strTtlRCount     =  %Trim(%Char(K9CNT));
         strNbrOfDup      =  %Trim(%Char(K9DUP));
         strClmErCount    =  %Trim(%Char(K9ERR));
         strBillTrnsfrd   =  %Trim(%Char(K9TRCT));

       endsr;
      /End-Free
