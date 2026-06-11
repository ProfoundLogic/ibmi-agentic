     h*copyright('(c) 2024 Sapiens, Inc. All rights reserved. -
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*                     for Provider Bill Maintenance
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes s
     h*
     h*  Called from: MTPROVBILL
     h*  Calls to:
     h*
     h*  Created on 06/18/2024 by C:079 for Project MC448
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h**********************************************************************
     ffaeop     if   e             disk
     fststl     if   e           k disk
     fftabl     if   e           k disk
     fftgtl     if   e           k disk

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrMask

      * *entry
     dMTPROVBIL1       pr                  extpgm('MTPROVBIL1')
     d                                9  0
     d                            32767
     d                                1

     dMTPROVBIL1       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strClmtName     s             40a   inz
     d strCaseNumber   s             40a   inz
     d strSsn          s             11a   inz
     d strProvider     s             15a   inz
     d strBillNPI      s             15a   inz
     d strAdjState     s             20a   inz
     d strPolicyState  s             20a   inz
     d strAdjCode      s             25a   inz
     d strBillType     s              8a   inz
     d strBillSubType  s             25a   inz
     d strDtRcvdCarr   s             10a   inz
     d strDtSentCarr   s             10a   inz
     d strStatus       s             10a   inz
     d strChrgAmt      s             14a   inz
     d strPaidAmt      s             14a   inz

       Chain(n) inRrn peo01;
       If %Found(FAEOP);
          Exsr builddata;

          outJSON = ' ';

          // populate detail box information.
          BuildJSON(outJSON :'OutDtlClaimantName' : %Trim(strClmtName));
          BuildJSON(outJSON :'OutDtlCaseNumber' : %Trim(strCaseNumber));
          BuildJSON(outJSON :'OutDtlSocialSecurityNumber' :
           %Trim(strSsn));
          BuildJSON(outJSON :'OutDtlProvider' : %Trim(strProvider));
          BuildJSON(outJSON :'OutDtlBillProviderNpi' : %Trim(EOBNPI));
          BuildJSON(outJSON :'OutDtlAdjState' : %Trim(strAdjState));
          BuildJSON(outJSON :'OutDtlPolicyState' : %Trim(strPolicyState));
          BuildJSON(outJSON :'OutDtlAdjudicationCode' : %Trim(strAdjCode));
          BuildJSON(outJSON :'OutDtlBillType' : %Trim(strBillType));
          BuildJSON(outJSON :'OutDtlBillSubType' : %Trim(strBillSubType));
          BuildJSON(outJSON :'OutDtlDateReceivedByCarrier' :
           %Trim(strDtRcvdCarr));
          BuildJSON(outJSON :'OutDtlDateSentToCarrier' :
           %Trim(strDtSentCarr));
          BuildJSON(outJSON :'OutDtlStatus' : %Trim(strStatus));
          BuildJSON(outJSON :'OutDtlChargedAmount' : %Trim(strChrgAmt));
          BuildJSON(outJSON :'OutDtlPaidAmount' : %Trim(strPaidAmt));
       Endif;
       rtnCode = ' ';
       Return;

       //===================================================================
       // Build Data That will be passed on to AJAX
       //===================================================================
       Begsr builddata;

        // Claimant Name
        strClmtName   = %Trim(EOlsn) + ', ' + %Trim(EOfsn) + ' ' + EOmid;

        // Claimant Case Number
        strCaseNumber = %Editc(EOFnd:'X') + '-' + %Editc(EOFyr:'X') +
         '-' + %Editc(EOcase:'X');

        // Claimant Social Security Number
        strSsn = MaskData('SSN':'CMSS#':EOSS#);

        // Provider FEIN
        strProvider = %Editc(EOfein:'X') + '-' + %Editc(EOmod:'X');

        // Adjudication State
        Chain (EOast) pst01;
        If %Found(STSTL);
           strAdjState = %Trim(STname);
        Endif;

        // Policy State
        Chain (EOpost) pst01;
        If %Found(STSTL);
           strPolicyState = %Trim(STname);
        Endif;

        // Adjudication Code
        Chain (EOco#:EOcsCd) pcs01;
        If %Found(FTABL);
           strAdjCode = %Trim(EOcsCd) + '-' + %Trim(CSdesc);
        Endif;

        // Bill Type
        Chain (EOco#:'BTYP':EObtyp) pgt01;
        If %Found(FTGTL);
           strBillType = %Trim(GTdesc);
        Endif;

        // Bill Sub-Type
        Chain (EOco#:'STYP':EOsTyp) pgt01;
        If %Found(FTGTL);
           strBillSubType = %Trim(GTdesc);
        Endif;

        // Date Carrier Received Bill
        Monitor;
        strDtRcvdCarr = AlphaUsa(CvtToDate(EOcrDt));
        On-error;
           strDtRcvdCarr = *blanks;
        Endmon;

        // Date Bill Sent to Bill
        Monitor;
        strDtSentCarr = AlphaUsa(CvtToDate(EOcsDt));
        On-error;
           strDtSentCarr = *blanks;
        Endmon;

        // Bill Status
        Select;
        When EOblSt = 'C';
           strStatus = 'Completed';
        When EOblSt = 'P';
           strStatus = 'Pended';
        When EOblSt = 'U';
           strStatus = 'Un-Pended';
        When EOblSt = 'X';
           strStatus = 'Cancelled';
        When EOblSt = 'D';
           strStatus = 'Deleted';
        Other;
           strstatus = EOblSt;
        Endsl;

        // Charged Amount
        strChrgAmt = %Editc(EOchrg:'J');

        // Paid Amount
        strPaidAmt = %Editc(EOpaid:'J');

       Endsr;
