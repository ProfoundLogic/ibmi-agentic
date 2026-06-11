     h*copyright('(c) 2020 Sapiens, Inc. All rights reserved. -
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*                     for Uploaded Provider Bills Errors screen
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: MTPBULERR
     h*  Calls to:
     h*
     h*  Created on 05/11/2020 by C:212 for Project MC437
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h*  09/07/23  C:079 Support: 146/113770                               *
     h*                  Edit Provider ID                                  *
     h*                                                                    *
     h*  01/25/21  C:079 Support : 152/94138                               *
     h*                  Add Monitor to date conversions                   *
     h*                                                                    *
     h*  06/09/20  C:212 Project : SY420                                   *
     h*                  Mask SSN                                          *
     h*                                                                    *
     h**********************************************************************
     ffau1p     if   e             disk    rename(pu101:pu10p)

     d/copy sPrAtrium
     d/copy sPrDate

      /copy sPrMask

      * *entry
     dMTPBULER01       pr                  extpgm('MTPBULER01')
     d                                9  0
     d                            32767
     d                                1

     dMTPBULER01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d U1prov          ds
     d  u1fn1#                 1      9  0
     d  u1md1                 10     12

     d strUlTm         s              8    inz
     d strPrTm         s              8    inz
     d strDOI          s             10    inz
     d strFrDt         s             10    inz
     d strToDt         s             10    inz
     d strFein         s              9    inz
     d strMod          s              3    inz
     d strSS#          s              9    inz
      *** Beg Add ***** 09/07/23 ******************************
     d decProv         s             13s 0
     d isInvProv       s               n
      *** End Add ***** 09/07/23 ******************************
     C*

      /Free
       Chain(n) inRrn pu10p;
       Exsr builddata;

       outJSON = ' ';

       // populate detail box information.
       BuildJSON(outJSON :'OutDtlUplDt' :
                           alphaUSA(cvtToDate(dtMDYY(U1uldt))));
       BuildJSON(outJSON :'OutDtlBill#' :%Trim(U1trid));
       BuildJSON(outJSON :'OutDtlClmntNm' :%Trim(U1clsn) + ', ' +
                           %Trim(U1cfsn));
       BuildJSON(outJSON :'OutDtlErrors' :%Trim(U1prfl));
       BuildJSON(outJSON :'OutDtlStatus' :%Trim(U1ests));
       BuildJSON(outJSON :'OutDtlClmNbr' :%Trim(U1clm#));
       BuildJSON(outJSON :'OutDtlSSN#' :%Trim(strSS#));
       BuildJSON(outJSON :'OutDtlDOI' : %Trim(strDOI));
       BuildJSON(outJSON :'OutDtlPFEINMod' :%Trim(strFein) + '-'            +
                                            %Trim(strMod));
       BuildJSON(outJSON :'OutDtlFrSrvDt' : %Trim(strFrDt));
       BuildJSON(outJSON :'OutDtlToSrvDt' : %Trim(strToDt));
       BuildJSON(outJSON :'OutDtlCharges':%Trim(%Char(%Dec(U1chrg:11:0)*0.01)));
       BuildJSON(outJSON :'OutDtlFSRed' : %Trim(%Char(%Dec(U1MRA:11:0)*0.01)));
       BuildJSON(outJSON :'OutDtlNtwkRed':%Trim(%Char(%Dec(U1ndis:11:0)*0.01)));
       BuildJSON(outJSON:'OutDtlOther2Red' :
                                      %Trim(%Char(%Dec(U1oth2:11:0)*0.01)));
       BuildJSON(outJSON:'OutDtlPaidAmt' :%Trim(%Char(%Dec(U1paid:11:0)*0.01)));
       BuildJSON(outJSON :'OutDtlFeeAmt' :%Trim(%Char(%Dec(U1cost:9:0)*0.01)));
       BuildJSON(outJSON :'OutDtlOrgVnd' :%Trim(U1ntwk));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Build Data That will be passed on to AJAX
       //===================================================================
       Begsr builddata;

        Monitor;
        strDOI = %Char(%Date(U1doi:*iso):*usa/);
        On-error;
           strDOI = *blanks;
        Endmon;
        Monitor;
        strFrDt = %Char(%Date(U1frdt:*iso):*usa/);
        On-error;
           strFrDt = *blanks;
        Endmon;
        Monitor;
        strToDt = %Char(%Date(U1frdt:*iso):*usa/);
        On-error;
           strToDt = *blanks;
        Endmon;

        If U1prov <> *blanks and
          U1prov <> '0000000000000';
          //*** Beg Add *** 09/07/23 ***************************************
          // Edit the provider ID for non numeric
          isInvProv = *off;
          Monitor;
             decProv = %Dec(U1prov:13:0);
          On-Error;
             strFein = *zeros;
             strMod =  *zeros;
             isInvProv = *on;
          EndMon;
          //*** End Add *** 09/07/23 ***************************************
          //*** Beg Add *** 09/07/23 ***************************************
          If isInvProv = *off;
          //*** End Add *** 09/07/23 ***************************************
             strFein = %EditC(U1fn1#:'X');
             strMod =  U1md1;
          //*** Beg Add *** 09/07/23 ***************************************
          Endif;
          //*** End Add *** 09/07/23 ***************************************
        Else;
           strFein = %EditC(U1fein:'X');
           strMod = %EditC(U1mod:'X');
        EndIf;

        strSS# = MaskData('SSN':'U1SS#':U1SS#);

       Endsr;

      /end-free
