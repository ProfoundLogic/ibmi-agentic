     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STTBLRMTP
     h*  Calls to:
     h*
     h*  Created on 08/14/2017 by C:062 for Project WC2501
      **********************************************************************
      * Revisions:
      *                                                               *
      * 07/07/22 C:079 Project : 152WBV                               *
      *                Add Immediate Lock of Remark field             *
      *                                                               *
      * 03/14/22 C:300 Support : 101390                               *
      *                Not showing the value of '# of Days to Change  *
      *                                                               *
      * 03/14/22 C:300 Support : 101390                               *
      *                Not showing the value of '# of Days to Change  *
      *                Additional Remarks'                            *
      *
      **********************************************************************
      * Customer Master File
     Fstctp     if   e             disk    rename(pct01:pct0p)
     fstsml     if   e           k Disk    ReName(psm01:psm00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DRemarkType01     pr                  ExtPgm('STTBLRMTP1')
     D                                9  0
     D                            32767
     D                                1
      *
     DRemarkType01     pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     d remarkCode      s             50    Inz(' ')
     d summaryCode     s             50    Inz(' ')
     d status          s             10    Inz(' ')
     d period          s             20    Inz(' ')
     d periodType      s             10    Inz(' ')

      /Free

       Chain(n) inRRN pct0p;

       exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'OutRemarkTypeDtl' :remarkCode);
       BuildJSON(outJSON :'OutSummaryCodeDtl' :summaryCode);
       BuildJSON(outJSON :'OutStatusDtl' :status);
       BuildJSON(outJSON :'OutDataEntryPgmDtl' :CTdePg);
       BuildJSON(outJSON :'OutPurgeDaysDtl' :%EditC(CTdPrg:'X'));
       //*** Beg Chg *****  03/14/22  ****************************************
       // BuildJSON(outJSON :'OutChangeAddlRmrksDtl' :%EditC(CTdPrg:'X'));
       BuildJSON(outJSON :'OutChangeAddlRmrksDtl' :%EditC(CT#dch:'X'));
       //*** End Chg *****  03/14/22  ****************************************
       BuildJSON(outJSON :'OutSubAreaCodeDtl' :CTtype);
       BuildJSON(outJSON :'CbxDiaryTypeDtl' :CTfl01);
       BuildJSON(outJSON :'CbxAllowFoldDocChangeDtl' :CTfl03);
       BuildJSON(outJSON :'CbxDisallowChangeDtl' :CTfl04);
       BuildJSON(outJSON :'OutDefaultSecLevelDtl' :%EditC(CTdsLv:'X'));
       BuildJSON(outJSON :'OutDeleteSecLevelDtl' :%EditC(CTdlSl:'X'));
       BuildJSON(outJSON :'OutCompleteSecLevelDtl' :%EditC(CTcmSl:'X'));
       BuildJSON(outJSON :'OutCopySecLevelDtl' :%EditC(CTcpSl:'X'));
       BuildJSON(outJSON :'OutDiaryMessageDtl' :CTdMsg);
       BuildJSON(outJSON :'OutRecurrencePeriod' : period);
       BuildJSON(outJSON :'OutNumOfRecurrences' :%EditC(CTrd#:'4'));
       BuildJSON(outJSON :'CbxCreateAll' :CTcrAd);
       //*** Beg Add *****  07/07/22  ****************************************
       BuildJSON(outJSON :'OutImmediateLockofRemark' :CTimdLock);
       //*** End Chg *****  07/07/22  ****************************************

       RtnCode = ' ';
       Return;
       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

          remarkCode  = %Trim(CTrmCd) + '-' + CTdesc;

          Chain CTsmCd psm00;
          summaryCode = %Trim(CTsmCd) + '-' + SMdesc;

          If CTstat = 'I';
             status = 'Inactive';
          Else;
             status = 'Active';
          EndIf;

          Select;
          When CTrdPt = 'D';
             If CTrdPl = 1;
                periodType = 'Day';
             Else;
                periodType = 'Days';
             EndIf;
          When CTrdPt = 'W';
             If CTrdPl = 1;
                periodType = 'Week';
             Else;
                periodType = 'Weeks';
             EndIf;
          When CTrdPt = 'M';
             If CTrdPl = 1;
                periodType = 'Month';
             Else;
                periodType = 'Months';
             EndIf;
          EndSl;

          period = %EditC(CTrdPl:'4') + ' ' + periodType;

       endsr;
      /End-Free
