     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim detail                 box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 05/03/2012 by C:062 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  04/20/15 C:300 Project: WC2043
     h*                 Modified Diagnosis Code in Diagnosis Information grid
     h*                 to load ICD-9 or ICD-10 code based on date of injury.
     h*
     h*
     h**********************************************************************
      * Customer Master File
     Fsadap     if   e             disk    rename(pda01:pda0p)
     Fsadal     if   e           k Disk    ReName(pda01:pda00)
     fsmdil     if   e           k Disk    ReName(pdi01:pdi00)
      ***** beg add ******* 04/20/15 ***************************************
     fwmicd10l  if   e           k disk    rename(pic01:pic00)
      ***** end add ******* 04/20/15 ***************************************
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DClaimDetail01    pr                  ExtPgm('WTCLMDTL01')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimDetail01    pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
      *
      ***** beg chg ******* 04/20/15 ***************************************
     D****** diagOut         s              5
     D diagOut         s              7
      ***** end chg ******* 04/20/15 ***************************************
     D typeOut         s              1
     D primaryOut      s              1
     D descOut         s             40
     D statusOut       s             40
     D locationOut     s             40
     D effDateOut      s             10
     D siteOut         s             40
     D expDateOut      s             10
     D kkCo#           s                   like(GTco#)
     D kkTbId          s                   like(GTtbId)
     D kkKey           s                   like(GTkey)

     C     keySMDIL      Klist
     C                   Kfld                    DAicd9
     C                   Kfld                    DAlost

     C     keyWTGTL      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey
      *
      /Free

       Chain(n) inRRN pda0p;

       exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'OutDiagOut' :diagOut);
       BuildJSON(outJSON :'OutTypeOut' :typeOut);
       BuildJSON(outJSON :'CbxPrimaryOut' :primaryOut);
       BuildJSON(outJSON :'OutDescOut' :descOut);
       BuildJSON(outJSON :'OutStatusOut' :statusOut);
       BuildJSON(outJSON :'OutLocationOut' :locationOut);
       BuildJSON(outJSON :'OutEffDateOut' :effDateOut);
       BuildJSON(outJSON :'OutSiteOut' :siteOut);
       BuildJSON(outJSON :'OutExpDateOut' :expDateOut);

       RtnCode = ' ';
       Return;
       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

          diagOut=DAicd9;
          typeOut=DAlosT;
          primaryOut=DAprim;

          Chain keySMDIL pdi00;
          If %Found(SMDIL);
             descOut=DIdsc1;
          Else;
             descOut=*Blanks;
          EndIf;

          //****** beg add ****** 04/20/15 *********************************
          If diagOut = *blanks;
             diagOut = DAicd1001;

                Chain (DAicd1001:DAlost) pic00;
                If %found(WMICD10L);
                   descOut=ICdsc1;
                Else;
                   descOut=*Blanks;
                EndIf;
          EndIf;
          //****** end add ****** 04/20/15 *********************************

          kkCo#=DAco#;
          kkTbId='ISTS';
          kkKey=DAiSts;
          Chain keyWTGTL pgt00;
          If %Found(WTGTL);
             statusOut=%Trim(DAiSts) + ' - ' + %Trim(GTdesc);
          Else;
             statusOut=DAiSts;
          EndIf;

          kkTbId='ILOC';
          kkKey=DAiLoc;
          Chain keyWTGTL pgt00;
          If %Found(WTGTL);
             locationOut=%Trim(DAiLoc) + ' - ' + %Trim(GTdesc);
          Else;
             locationOut=DAiLoc;
          EndIf;

          effDateOut=alphaUSA(cvtToDate(DAisDt));

          kkTbId='ISIT';
          kkKey=DAiSit;
          Chain keyWTGTL pgt00;
          If %Found(WTGTL);
             siteOut=%Trim(DAiSit) + ' - ' + %Trim(GTdesc);
          Else;
             siteOut=DAiSit;
          EndIf;

          expDateOut=alphaUSA(cvtToDate(DAexDt));

       endsr;
      /End-Free
