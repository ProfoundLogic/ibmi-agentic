     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency Group Assignments Detail Box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTASN
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 03/01/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmaap     if   e             disk    rename(paa01:paa0p)
     fwmarl     if   e           k disk
     fsmupl1    if   e           k disk
     fwmcpl     if   e           k disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DWtAgtAsn01       pr                  ExtPgm('WTAGTASN01')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtAsn01       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d DftRate         s             40
     d strUwtr         s             60
     d strChap         s             40

     d* Load Group Assignments Detail.
     d LoadAgtAsnDtl   pr

      *
      /Free

       Chain inRRN paa0p;
       LoadAgtAsnDtl();

       outJSON = ' ';

       BuildJSON(outJSON :'DtlCbxPortalDefault' :%Trim(AAPortDft));
       BuildJSON(outJSON :'DtlCbxCommMail' :%Trim(AAcmfl));
       BuildJSON(outJSON :'DtlCbxRegularMail' :%Trim(AAmlfl));
       BuildJSON(outJSON :'DtlCbxProspectMail' :%Trim(AAprfl));
       BuildJSON(outJSON :'DtlOutCommitAmt' :%editc(AAcmmt:'N'));
       BuildJSON(outJSON :'DtlOutCommitDate' :
                         alphaUSA(cvtToDate(dtMDYY(AAcmdt))));
       BuildJSON(outJSON :'DtlOutDftRate#' :%Trim(DftRate));
       BuildJSON(outJSON :'DtlOutDftRateExpDate' :
                         alphaUSA(cvtToDate(dtMDYY(AAexdt))));
       BuildJSON(outJSON :'DtlOutUnderwriter' :%Trim(strUwtr));
       BuildJSON(outJSON :'DtlOutChapter' :%Trim(strChap));


       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Return Fund Detail
       //===================================================================
     P LoadAgtAsnDtl   B
     D LoadAgtAsnDtl   PI
      /free

       // Default Rate #
       DftRate = *blanks;
       If AAdrt# <> 0;
          Setgt (AAco#:AAfnd:AAdrt#) par01;
          Readpe (AAco#:AAfnd:AAdrt#) par01;
          If not %eof(wmarl);
             DftRate = %char(AAdrt#) + ' - ' + %trim(ARdesc);
          Endif;
       Endif;

       // Underwriter
       strUwtr = *blanks;
       If AAuwtr <> ' ';
          Chain AAuwtr pup01;
          If %found(smupl1);
             strUwtr = AAuwtr + ' - ' + %trim(UPusnm);
          Endif;
       Endif;

       // Chapter
       strChap = *blanks;
       If AAchap <> 0;
          Chain (AAco#:AAfnd:AAchap) pcp01;
          If %found(wmcpl);
             strChap = %trim(%char(AAchap)) + ' - ' + %trim(CPdesc);
          Endif;
       Endif;


      /end-free
     P LoadAgtAsnDtl   E




