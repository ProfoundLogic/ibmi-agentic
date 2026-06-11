     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Cancellation Batches
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCXBTCH
     h*  Calls to:
     h*
     h*  Created on 06/21/2023 by C:300 for Project WC2906
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwxccp     if   e             disk    rename(pcc01:pcc0s)
     fwmfnl     if   e           k disk
     fwtcrl1    if   e           k disk
     fwmofl     if   e           k disk
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtCxBtch01       pr                  extpgm('WTCXBTCH01')
     d                                9  0
     d                            32767
     d                                1

     dwtCxBtch01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strGroup        s             70
     d strCanRsn       s             65
     d strRptFreq      s             10
     d strOffNum       s             50
     d strStateIE      s             30


      /Free

       Chain inRRN pcc0s;

       Exsr BuildData;

       // Get Flags value from Workers Comp control file (SWCPF)
       Chain 'WC' SWC01;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlGrp' :%trim(strGroup));
       BuildJSON(outJSON :'OutDtlReasonCode' :%trim(strCanRsn));
       BuildJSON(outJSON :'OutDtlFromDate' :
                         alphaUSA(cvtToDate(dtMDYY(CCfrdt))));
       BuildJSON(outJSON :'OutDtlToDate' :
                         alphaUSA(cvtToDate(dtMDYY(CCtodt))));
       BuildJSON(outJSON :'OutCbxInclDeposit' :CCindp);
       // BuildJSON(outJSON :'OutCbxInclCurrAcc' :CCsrpy);
       // BuildJSON(outJSON :'OutCbxInclAgencyBill' :CCabil);
       BuildJSON(outJSON :'OutDtlExcludeDate' :
                         alphaUSA(cvtToDate(dtMDYY(CCxadt))));
       BuildJSON(outJSON :'OutDtlReportFrequency' :%trim(strRptFreq));
       BuildJSON(outJSON :'OutDtlExcludeDateMonth' :%trim(%Char(CCday)));
       BuildJSON(outJSON :'OutDtlOffice' :%trim(strOffNum));
       // BuildJSON(outJSON :'OutDtlIssueDate' :
       //                  alphaUSA(cvtToDate(dtMDYY(CCisdt))));
       BuildJSON(outJSON :'OutDtlComment1' :%trim(CCcom1));
       BuildJSON(outJSON :'OutDtlComment2' :%trim(CCcom2));
       BuildJSON(outJSON :'OutDtlStateIE' :%trim(strStateIE));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Group Number
       strGroup = *Blanks;
       Chain (CCco#:CCfnd) pfn01;
       IF %found(wmfnl);
          strGroup = %char(CCfnd) + ' - '  + %trim(FNname);
       EndIf;

       // Cancel Reason
       strCanRsn = *Blanks;
       If CCrsn <> *Blanks;
          Chain (CCco#:CCrsn) pcr01;
          IF %found(wtcrl1);
             strCanRsn = CCrsn + ' - '  + %trim(CRdesc);
          EndIf;
       EndIf;

       // Reporting Frequency
       strRptFreq = *Blanks;
       If CCfreq <> *Blanks;
          Select;
          When CCfreq = 'M';
             strRptFreq = 'Monthly';
          When CCfreq = 'W';
             strRptFreq = 'Weekly';
          When CCfreq = 'B';
             strRptFreq = 'Both';
          Endsl;
       EndIf;

       // Office Number
       strOffNum = *Blanks;
       strOffNum = CCoff#;
       If CCoff# <> *Blanks;
          Chain (CCco#:CCoff#) pof01;
          IF %found(wmofl);
             strOffNum = %trim(strOffNum) + ' - '  + %trim(OFname);
          EndIf;
       EndIf;

       // States Inclusion/Exclusion
       strStateIE = *Blanks;
       Select;
       When CCstinex = *Blanks;
          strStateIE = 'All';
       When CCstinex = 'I';
          strStateIE = 'Include selected state(s)';
       When CCstinex = 'E';
          strStateIE = 'Exclude selected state(s)';
       Endsl;



       endsr;
      /End-Free
