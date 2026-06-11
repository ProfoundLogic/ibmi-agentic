     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency History, Billing Location
     h*               detail box - TAB 2.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLSUM
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmahp     if   e             disk    rename(pah01:pah0p)
     fwmagl     if   e           k disk    rename(pag01:pag01)
     fwmapl     if   e           k disk    rename(pap01:pap01)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmarl     if   e           k disk    rename(par01:par01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DPolicySumm03     pr                  ExtPgm('WTPOLSUM03')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm03     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d intCo#          s                   like(AHco#)
     d intFnd          s                   like(AHfnd)
     d intFyr          s                   like(AHfyr)
     d intdiv          s                   like(AHdiv)
     d agentName       s             16
     d prodName        s             16
     d pgmName         s             45
     d intPgmName      s             45
     d lstMntUser      s             15
     d intCrtUser      s             15
     d newPercent      s                   like(ARrnrt)
     d renPercent      s                   like(ARrnrt)
     d paymentPln      s             25
     d effDate         s                   like(AHefdt)
     d ##last          s             56
     d ##frst          s             15
     d ##term          s              1
     d kkagt#          s                   like(AHagt#)
     d kkprod          s                   like(AHprod)
     d kkrt#           s                   like(AHsrt#)
     d controlAgency   s              3
     d lstMntDate      s             10
     d intCrtDate      s             10
     d intCrtTime      s              8
     d lstMntTime      s              8

     d SrName          pr                  extpgm('SRNAME')
     d ##last                        56
     d ##frst                        15
     d ##term                         1

     c* Agent History master file
     c     keyWMAHL      Klist
     c                   Kfld                    AHCo#
     c                   Kfld                    AHFnd
     c                   Kfld                    AHEmp#
     c                   Kfld                    AHDiv

     c* Agent master file
     c     keyWMAGL      Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod

     c* Agent Assignment file
     c     keyWMAAL1     Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    kkagt#

     c* Agent Producer file
     c     keyWMAPL      Klist
     c                   Kfld                    AACo#
     c                   Kfld                    AAfein
     c                   Kfld                    AAmod
     c                   Kfld                    KKprod

     c* Agent Producer file
     c     keyWMARL      Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    kkrt#
     c                   Kfld                    effDate

     c     key1WMARL     Klist
     c                   Kfld                    AHco#
     c                   Kfld                    AHfnd
     c                   Kfld                    kkrt#

      *
      /Free

       Chain(n) inRRN pah0p;

       outJSON = ' ';

       // Populate Primary Agency Details.
       kkAgt# = AHagt#;
       kkprod = AHprod;
       KKrt#  = AHrt#;
       Exsr BuildData;
       BuildJSON(outJSON :'OutPrmAgNm' :%trimr(%editc(AHagt#:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutPrmPrNm' :%trimr(%editc(AHprod:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutPrmPrRt' :%editc(AHrt#:numEdtcde));
       BuildJSON(outJSON :'OutPrmPrNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutPrmAgRnw' :%editc(renPercent:numEdtcde));

       If AHcaf1 = 'Y';
          controlAgency = 'Yes';
       Else;
          controlAgency = ' ';
       Endif;

       BuildJSON(outJSON :'OutPrmAgCAg' :(controlAgency));
       BuildJSON(outJSON :'OutPrmAgPTy' :(paymentPln));


       // Populate Secondary Details.
       kkAgt# = AHsagt;
       KKprod = AHsprd;
       KKrt# = AHsrt#;
       Exsr BuildData;
       BuildJSON(outJSON :'OutSndAgNm' :%trimr(%editc(AHsagt:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutSndPrNm' :%trimr(%editc(AHsprd:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutSndPrRt' :%editc(AHsrt#:numEdtcde));
       BuildJSON(outJSON :'OutSndPrNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutSndAgRnw' :%editc(renPercent:numEdtcde));

       If AHcaf2 = 'Y';
          controlAgency = 'Yes';
       Else;
          controlAgency = ' ';
       Endif;

       BuildJSON(outJSON :'OutSndAgCAg' :(controlAgency));
       BuildJSON(outJSON :'OutSndAgPTy' :(paymentPln));


       // Populate Tertiary Agency Details.
       kkAgt# = AH3agt;
       KKprod = AH3prd;
       KKrt#  = AH3rt#;
       Exsr BuildData;
       BuildJSON(outJSON :'OutTerAgNm' :%trimr(%editc(AH3agt:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutTerPrNm' :%trimr(%editc(AH3prd:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutTerPrRt' :%editc(AH3rt#:numEdtcde));
       BuildJSON(outJSON :'OutTerPrNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutTerAgRnw' :%editc(renPercent:numEdtcde));

       BuildJSON(outJSON :'OutTerAgCAg' :(controlAgency));
       BuildJSON(outJSON :'OutTerAgPTy' :(paymentPln));

       // Populate Agency of Record/Primary details.
       kkAgt# = AHagrc;
       kkprod = AHprrc;
       kkrt#  = AHrtrc;
       Exsr BuildData;
       BuildJSON(outJSON :'OutAorPNm' :%trimr(%editc(AHagrc:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutAorPPrNm' :%trimr(%editc(AHprrc:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutAorPRt' :%editc(AHrtrc:numEdtcde));
       BuildJSON(outJSON :'OutAorPrNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutAorPRnw' :%editc(renPercent:numEdtcde));

       If AHcrf1 = 'Y';
          controlAgency = 'Yes';
       Else;
          controlAgency = ' ';
       Endif;

       BuildJSON(outJSON :'OutAorPCAg' :(controlAgency));
       BuildJSON(outJSON :'OutAorPPTy' :(paymentPln));

       // Populate Agency of Record/Secondary
       kkAgt# = AH2arc;
       kkprod = AH2prc;
       kkrt#  = AH2rrc;
       Exsr BuildData;
       BuildJSON(outJSON :'OutAorSNm' :%trimr(%editc(AH2arc:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutAorSPrNm' :%trimr(%editc(AH2prc:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutAorSRt' :%editc(AH2rrc:numEdtcde));
       BuildJSON(outJSON :'OutAorSnNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutAorSRnw' :%editc(renPercent:numEdtcde));

       If AHcrf2 = 'Y';
          controlAgency = 'Yes';
       Else;
          controlAgency = ' ';
       Endif;

       BuildJSON(outJSON :'OutAorSCAg' :(controlAgency));
       BuildJSON(outJSON :'OutAorSPTy' :(paymentPln));

       // Populate Agency of Record/Tertiary
       kkAgt# = AH3arc;
       kkprod = AH3prc;
       kkrt#  = AH3rrc;
       Exsr BuildData;
       BuildJSON(outJSON :'OutAorTNm' :%trimr(%editc(AH3arc:numEdtcde) +
        '/' + agentName));
       BuildJSON(outJSON :'OutAorTPrNm' :%trimr(%editc(AH3prc:numEdtcde) +
        '/' + prodName));
       BuildJSON(outJSON :'OutAorTRt' :%editc(AH3rrc:numEdtcde));
       BuildJSON(outJSON :'OutAorTrNw' :%editc(newPercent:numEdtcde));
       BuildJSON(outJSON :'OutAorTRnw' :%editc(renPercent:numEdtcde));

       BuildJSON(outJSON :'OutAorTCAg' :(controlAgency));
       BuildJSON(outJSON :'OutAorTPTy' :(paymentPln));


       // Load Last Maintenance details.
       Exsr LstMntDetails;
       BuildJSON(outJSON :'OutLmUser' :%trimR(lstMntUser));
       BuildJSON(outJSON :'OutLmPgmNm' :%trimR(pgmName));
       BuildJSON(outJSON :'OutLmDate' :lstMntDate);
       BuildJSON(outJSON :'OutLmTime' :lstMntTime);

       // Load Initial creation details.
       Exsr IntCrtDetails;
       BuildJSON(outJSON :'OutIcUser' :%trimR(intCrtUser));
       BuildJSON(outJSON :'OutIcPgmNm' :%trimR(intPgmName));
       BuildJSON(outJSON :'OutIcDate' :intCrtDate);
       BuildJSON(outJSON :'OutIcTime' :intCrtTime);

       rtnCode = ' ';
       Return;
      /End-Free

      *************************************************************
      * BuildData for detail box
      *************************************************************
      /Free
       Begsr  BuildData;
       Clear prodName;
       Clear agentName;
       Clear ##last;
       Clear ##frst;
       Clear newPercent;
       Clear renPercent;
       Clear paymentPln;

       //* Load Agent name
       Chain(n) keyWMAAL1 PAA01;
       If %found;
          Chain(n) keyWMAGL PAG01;
          If %found;
             agentName  = AGname;
          Endif;
          Chain(n) KeyWMAPL PAP01;
          If %found;
             If APfsn = ' ';
                prodName = APlsn;
             Else;
                ##last = APlsn;
                ##frst = APfsn;
                ##term = '1';
                SrName(##last:##frst:##term);
                prodName = ##last;
             Endif;
          Endif;

          effDate = DateTo7(SystemDate);
          Setgt  keyWMARL  PAR01;
          Readpe key1WMARL PAR01;
          DoW not %eof and ARstat = 'I';
              Readpe key1WMARL PAR01;
          Enddo;

          newPercent = ARnwrt;
          renPercent = ARrnrt;
          Select;
          When ARplan = 'A';
               paymentPln = ARplan + '-' + 'Agency bill';
          When ARplan = 'D';
               paymentPln = ARplan + '-' + 'Direct bill (pre-paid)';
          When ARplan = 'P';
               paymentPln = ARplan + '-' + 'Based on A/R payments';
          Endsl;

       Endif;
       EndSr;
      /End-Free

      *************************************************************
      * BuildData for Last Maintenance
      *************************************************************
      /Free
       Begsr LstMntDetails;

       lstMntUser   = GetUsrName(AHuser);
       Select;
       When AHpgm = 'W0553R02';
          pgmName = 'Agent History Information';
       When AHpgm = 'W8700R';
          pgmName = 'Transfer Emp Quotes to Emp Master File';
       When AHpgm = 'W7820R';
          pgmName = 'Commercial Rollover';
       When AHpgm = 'W3091R';
          pgmName = 'Agent Transfer Update & Report';
       When AHpgm = 'W8552R';
          pgmName = 'Agent History Update for Multi Employers';
       When AHpgm = 'W0050R03';
          pgmName = 'Copy Employer / Division';
       When AHpgm = 'W8393R';
          pgmName = 'New PEO Master Upload';
       Other;
          pgmName = AHpgm;
       Endsl;

       lstMntDate = alphaUSA(CvtToDate(AHdt));
       lstMntTime = %editw(AHtime:'  :  :  ');

       EndSr;
      /End-Free
      *************************************************************
      * BuildData for Last Maintenance
      *************************************************************
      /Free
       Begsr IntCrtDetails;

       intCrtUser   = GetUsrName(AHiusr);
       Select;
       When AHpgm = 'W0553R02';
          intPgmName = 'Agent History Information';
       When AHpgm = 'W8700R';
          intPgmName = 'Transfer Emp Quotes to Emp Master File';
       When AHpgm = 'W7820R';
          intPgmName = 'Commercial Rollover';
       When AHpgm = 'W3091R';
          intPgmName = 'Agent Transfer Update & Report';
       When AHpgm = 'W8552R';
          intPgmName = 'Agent History Update for Multi Employers';
       When AHpgm = 'W0050R03';
          intPgmName = 'Copy Employer / Division';
       When AHpgm = 'W8393R';
          intPgmName = 'New PEO Master Upload';
       Other;
          intPgmName = AHpgm;
       Endsl;

       intCrtDate = alphaUSA(CvtToDate(AHidt));
       intCrtTime = %editw(AHitm:'  :  :  ');

       EndSr;
      /End-Free
