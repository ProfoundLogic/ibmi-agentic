     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h Option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX Program to Populate the Detail box
     h*               for Recovery Tracking.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRCVTRK
     h*  Calls to:
     h*
     h*  Created on 05/25/2023 by C:340 for Project WC2430
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h*
     h**********************************************************************
     fWMRRP     IF   E             Disk
     fWMPDL     IF   E           K Disk
     fWMCML1    IF   E           K Disk

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrMask

      * *ENTRY
     dWTRCVTRK01       pr                  extpgm('WTRCVTRK01')
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     dWTRCVTRK01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

       Dcl-s strPayee                  char(50);
       Dcl-s strPayeeName              char(50);
       Dcl-s strPayeeAddress1          char(50);
       Dcl-s strPayeeAddress2          char(50);
       Dcl-s strAttorney               char(50);
       Dcl-s strAttorneyName           char(50);
       Dcl-s strAttorneyAddress1       char(50);
       Dcl-s strAttorneyAddress2       char(50);
       Dcl-s strComments1              char(50);
       Dcl-s strComments2              char(50);
       Dcl-s strReqNumber              char(10);
       Dcl-s strEstReimAmt             char(15);
       Dcl-s strRefAmt                 char(15);
       Dcl-s strDateReq                char(10);
       Dcl-s strDateNot                char(10);
       Dcl-s strRecAmt                 char(15);
       Dcl-s strStatus                 char(10);
       Dcl-s strDateRec                char(10);

      /Free

       Chain inRRN PRR01;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlPayee' :%trim(strPayee));
       BuildJSON(outJSON :'OutDtlPayeeName' :%trim(strPayeeName));
       BuildJSON(outJSON :'OutDtlPayeeAddress1' :%trim(strPayeeAddress1));
       BuildJSON(outJSON :'OutDtlPayeeAddress2' :%trim(strPayeeAddress2));
       BuildJSON(outJSON :'OutDtlAttorney' :%trim(strAttorney));
       BuildJSON(outJSON :'OutDtlAttorneyName' :%trim(strAttorneyName));
       BuildJSON(outJSON :'OutDtlAttorneyAddress1' :%trim(strAttorneyAddress1));
       BuildJSON(outJSON :'OutDtlAttorneyAddress2' :%trim(strAttorneyAddress2));
       BuildJSON(outJSON :'OutDtlComments1' :%trim(strComments1));
       BuildJSON(outJSON :'OutDtlComments2' :%trim(strComments2));
       BuildJSON(outJSON :'OutDtlReqNumber' :%trim(strReqNumber));
       BuildJSON(outJSON :'OutDtlEstReimbursementAmt' :%trim(strEstReimAmt));
       BuildJSON(outJSON :'OutDtlRefundAmtReq' :%trim(strRefAmt));
       BuildJSON(outJSON :'OutDtlDateRequsted' :%trim(strDateReq));
       BuildJSON(outJSON :'OutDtlDateNotified' :%trim(strDateNot));
       BuildJSON(outJSON :'OutDtlAmtRecovered' :%trim(strRecAmt));
       BuildJSON(outJSON :'OutDtlStatus' :%trim(strStatus));
       BuildJSON(outJSON :'OutDtlDateRefundRec' :%trim(strDateRec));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Payee
       If RrFein <> 0;
          strPayee = %EditC(RrFein : 'Z') + ' ' +
                     %EditC(RrMod  : 'Z')        ;
       Else;
          Chain (RrCO# : RrFnd : RrFyr : RrCase) WMCML1;
          If %Found and %Trim(CmLsn) <> ' ' and  %Trim(CmFsn) <> ' ' ;
             If %Trim(CmMid) <> ' ';
                strPayee = %Trim(CmLsn) +', '+ %Trim(CmFsn) + ', '+
                           %Trim(CmMid) ;
             Else;
                strPayee = %Trim(CmLsn) +', '+ %Trim(CmFsn) ;
             EndIf;
          EndIf;
       Endif;

       // Payee / Mod
       Chain (RrFein : RrMod) WmPdl;
       If %Found;
       strPayeeName     = %Trim(PdName) + ', '+  %Trim(PdFsn);
       strPayeeAddress1 = %Trim(PdAdr1) + ', '+  %Trim(PdAdr2);
          If %Len(%Editc(PdZip:'X')) > 5              and
             %Subst(%Editc(PdZip:'X'):6:4) <> '0000';
                   strPayeeAddress2 = %Trim(PdCty ) + ', '+
                                      %Subst(%Editc(PdZip:'X'):1:5) + '-'+
                                      %Subst(%Editc(PdZip:'X'):6:4) ;
          Else;
                   strPayeeAddress2 = %Trim(PdCty ) + ', '+
                                      %Subst(%Editc(PdZip:'X'):1:5) ;
          EndIf;
       EndIf;

       // Attorney Mode
       strAttorney = %EditC(RRATTY : 'Z') + ' ' +
                     %EditC(RRAMOD : 'Z')        ;
       Chain (RrAtty : RrAMod) WmPdl;
       If %Found;
          strAttorneyName     = %Trim(PdName) + ', '+  %Trim(PdFsn);
          strAttorneyAddress1 = %Trim(PdAdr1) + ', '+  %Trim(PdAdr2);
          If %Len(%Editc(PdZip:'X')) > 5              and
             %Subst(%Editc(PdZip:'X'):6:4) <> '0000';
                strAttorneyAddress2 = %Trim(PdCty ) + ', '+
                                      %Subst(%Editc(PdZip:'X'):1:5) + '-'+
                                      %Subst(%Editc(PdZip:'X'):6:4) ;
          Else;
                strAttorneyAddress2 = %Trim(PdCty ) + ', '+
                                      %Subst(%Editc(PdZip:'X'):1:5) ;
          EndIf;
       EndIf;

       // Comments
       strComments1 = RRCom1;
       strComments2 = RRCom2;

       // Request Number
       strReqNumber =  %EditC(RrReq# : 'Z');

       // Refund Amount
       strRefAmt = %EditC(RRRAMT : '1');

       // Estimated Refund Amount
       strEstReimAmt = %EditC(RrEamt : '1');

       // Recovered Amount
       strRecAmt = %EditC(RrRcvd : '1');

       // Date Refund Filed/Notified
       If RrDtre <> 0;
          strDateNot = %EditC(dtMDYY(RrDtre):'Y');
       Else;
          strDateNot = ' ';
       EndIf;

       // Date Refund Requested
       If RrDtrq <> 0;
          strDateReq = %EditC(dtMDYY(RrDtrq):'Y');
       Else;
          strDateReq = ' ';
       EndIf;

       // Date Last Refund Received
       If RrDtrc <> 0;
          strDateRec =  %EditC(dtMDYY(RrDtrc):'Y');
       Else;
          strDateRec = ' ';
       EndIf;

       // Status
       If RRStat = 'A';
          strStatus = 'Active';
       Else;
          strStatus = 'Inactive';
       Endif;

       endsr;
      /End-Free
