     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to get the Toolbar Glags.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTACTPNL
     h*  Calls to:
     h*
     h*  Created on 02/05/2013 by C:279 for Project : 146WFJ
     h**********************************************************************
     h* Revisions:
     h*
     h*  04/20/23 C:340 Project: WC2430
     h*                 Add Module Check for 'NewPayments'
     h*
     h*  04/05/22 C:33  Support: 101772
     h*                 Clear WTGETTBFLG data regardless of usage
     h*
     h*  06/07/21 C:33  Support: 93938
     h*                 Get the parms from a file
     h*
     h* 03/19/21  C:33  Project: WC2663
     h*                 Add ThirdPartyPayment information
     h*
     h*  01/11/21 C:33  Proejct: WC2732
     h*                 Add MulitpleClaims
     h*
     h*  06/18/15 C:062 Support: 35406 (25)
     h*                 -Don't check mod if TASB
     h*                 -Don't display if record only claim
     h*
     H*  03/06/15 C:242 Project: 114WGU
     H*                 Determine if Visit Tracking installed
     h*
     h*  12/01/14 C:300 Project: 114WLF
     h*                 Return the value of the Reserve Worksheet
     h*                 system module flag.
     h*
     h*  07/24/14 C:65  Project: WC2277
     h*                 Return notifications module flag.
     h*
     h*  04/23/13 C:189 Project: 147WBQ
     h*                 Allow reocurring comp on OptaComp groups
     h*
     h*  05/17/13 C:189 Redmine: 2253
     h*                 There is a system flag that indicates if a client has image installed
     h*
     h**********************************************************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fsdrpf     if   e             disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrGroup
      /copy sPrModule
       //***  Begin Add  ***  01/11/21  *************************
      /copy sPrSecure
       //***  End   Add  ***  01/11/21  *************************
       //***  Begin Add  ***  06/07/21  *************************
      /copy sPrSql
       //***  End   Add  ***  06/07/21  *************************
       //***  Begin Add  ***  03/19/21  *************************
      /copy wPrPmtSvc
       //***  End   Add  ***  03/19/21  *************************
     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      * *entry
     dwtgettbflg       pr                  ExtPgm('WTGETTBFLG')
     d                                9  0
     d                                1
     d                            32767
     d                                1
      *
     dwtgettbflg       pi
     d inRRN                          9  0
     d inMode                         1
     d outJSON                    32767
     d RtnCode                        1
     d* System Flag API
     d GetSystemFlag   pr                  extpgm('SA046R')
     d   outSysFlag                   1
     D
     d notifyFlag      s              1    Inz(' ')

       //***  Begin Add  ***  06/07/21  *************************
     d decRrn          s              9  0
       //***  End   Add  ***  06/07/21  *************************
     d is3rdPtyBlg     s              1    Inz(' ')
     d isOptraComp     s              1    Inz(' ')
     d isImgFlg        s              1    Inz(' ')
     D***  Begin Add  ***  03/19/21  ****************************
     D isThirdPartyPayment...
     D                 s              1    inz
     D strThirdPartyPaymentBranding...
     D                 s            256    inz varying
     D***  End   Add  ***  03/19/21  ****************************
     d intCo#          s                   like(ELco#)
       //***  Begin Add  ***  01/11/21  *************************
     d intCount        s             10  0  inz
       //***  End   Add  ***  01/11/21  *************************
     d intfnd          s                   like(ELfnd)
     d strclmwrk       s             20
       //***  Begin Add  ***  06/07/21  *************************
     d strType         s             40
       //***  End   Add  ***  06/07/21  *************************
     d isClmWrkFlg     s              1    Inz(' ')
     d isVisitTrack    s              1    Inz(' ')
     d   clientId      s              3
       //***  Beg Add  ***  04/20/23  *** WC2430 **************
     d IsNewPayments   s              1    Inz(' ')
       //***  End Add  ***  04/20/23  *** WC2430 **************

     D***  Begin Add  ***  03/19/21  ****************************
     DdsWMEXP        e ds                  extname(WMEXP) inz
     D***  End   Add  ***  03/19/21  ****************************
       //***  Begin Add  ***  06/07/21  *************************
     D psds          esds                  ExtName(szq1p)
       //***  End   Add  ***  06/07/21  *************************

      /Free
       //***  Begin Add  ***  01/11/21  *************************
       // This needs to be the first line
       Exec Sql Set Option Commit = *None;

       Clear CMco#;
       Clear CMfnd;
       Clear CMfyr;
       Clear CMcase;
       //***  End   Add  ***  01/11/21  *************************
       //***  Begin Add  ***  03/19/21  *************************
       Clear ELemp#;
       Clear ELdiv;
       //***  End   Add  ***  03/19/21  *************************

       //***  Begin Add  ***  06/07/21  *************************
       If inMode = '*';
          Exec sql
           select TBrrn, TBrrnType into :decRrn, :strType
            from WTGETTBFLG
            where TBuser = :q1user
            order by rrn(WTGETTBFLG) desc
            limit 1;
          If IsValidFetch(sqlstt);
             // Override incoming parms
             inRrn = decRrn;
             Select;
             When strType = 'Claim';
                inMode = 'C';
             When strType = 'Group';
                inMode = 'G';
             When strType = 'Quote' or IsQuoteEnv();
                inMode = 'Q';
             When strType = 'Policy';
                inMode = 'P';
             Endsl;
          Endif;
        //***  Begin Del  ***  04/05/22  *************************
        //Exec sql
        // delete
        //  from WTGETTBFLG
        //  where TBuser = :q1user;
        //***  End   Del  ***  04/05/22  *************************
       Endif;
       //***  End   Add  ***  06/07/21  *************************
       //***  Begin Add  ***  04/05/22  *************************
       Exec sql
        delete
         from WTGETTBFLG
         where TBuser = :q1user;
       //***  End   Add  ***  04/05/22  *************************

       GetClientId(clientId);

       outJSON = ' ';

       Select;
       // Processing for Policy Toolbar Flags
       When inMode = 'P';
          Chain inRrn pel0p;
          intCo# = ELco#;
          intFnd = ELfnd;

       // Processing for Quote Toolbar Flags
       When inMode = 'Q';
          Chain inRrn pel0p;
          intCo# = ELco#;
          intFnd = ELfnd;

       // Processing for Claims Toolbar Flags
       When inMode = 'C';
          Chain inRrn pcm0p;
          intCo# = CMco#;
          intFnd = CMfnd;

       //***  Begin Add  ***  06/07/21  *************************
       // Processing for Claims Toolbar Flags
       When inMode = 'G';
          Exec sql
           select FNco#, FNfnd into :intCo#, :intFnd
            from WMFNP
            where rrn(WMFNP) = :inRrn;
       //***  End   Add  ***  06/07/21  *************************
       Endsl;

       //***  Begin Add  ***  03/19/21  ****************************
       Reset dsWMEXP;
       Exec sql
        select * into :dsWMEXP
         from WMEXP
         where EXco# = :intCo# and EXfnd = :intFnd;
       //***  End   Add  ***  03/19/21  ****************************

          Exsr GetThrdPtyBlg;
       //***  Begin Add  ***  03/19/21  *************************
          Exsr GetThirdPartyPaymentInfo;
       //***  End   Add  ***  03/19/21  *************************
       // Processing under Claims Toolbar to check assosiation for OptaComp
          Exsr GetGrpAss;
          Exsr GetImgFlg;
          GetSystemFlag(notifyFlag);
          If clientID = '025';
             If CMltFl <> 'R';
                isClmWrkFlg = *On;
             EndIf;
          Else;
       // Check if Claims Worksheet module is enabled
             strclmwrk = 'CLAIMS WORKSHEET';
             isClmWrkFlg = IsModEnabled(strclmwrk);
          EndIf;
          if isModEnabled('VISIT TRACKING');
             isVisitTrack = 'Y';
          endif;

       //***  Beg Add  ***  04/20/23  *** WC2430 **************
       // Check if New Payments are Enabled
             IsNewPayments  = IsModEnabled('New Payments');
       //***  End Add  ***  04/20/23  *** WC2430 **************

       BuildJSON(outJSON :'OptaCompFlg' :isOptraComp);
       BuildJSON(outJSON :'ThrdPtyBlgFlg' :is3rdPtyBlg);
       BuildJSON(outJSON :'imageActive':isImgFlg);
       BuildJSON(outJSON :'NotifyFlag': notifyFlag);
       BuildJSON(outJSON :'ReserveWorksheet': isClmWrkFlg);
       BuildJSON(outJSON :'VisitTrack': isVisitTrack);
       //***  Begin Add  ***  01/11/21  *************************
       BuildJSON(outJSON :'MultipleClaim': IsMultipleClaim);
       //***  End   Add  ***  01/11/21  *************************
       //***  Begin Add  ***  03/19/21  *************************
       BuildJSON(outJSON:'ThirdPartyPayment':isThirdPartyPayment);
       BuildJSON(outJSON:'ThirdPartyPaymentBranding'
        :strThirdPartyPaymentBranding);
       //***  End   Add  ***  03/19/21  *************************
       //***  Beg Add  ***  04/20/23  *** WC2430 **************
       BuildJSON(outJSON :'NewPayments': IsNewPayments);
       //***  End Add  ***  04/20/23  *** WC2430 **************
       RtnCode = ' ';
       Return;

       //===================================================================
       // Get Third Party Billing Flag
       //===================================================================
       BegSr GetThrdPtyBlg;

          If %found;
             is3rdPtyBlg = Get3rdPtyFlag(intCo#:intFnd);
          Endif;

       EndSr;

       //***  Begin Add  ***  03/19/21  *************************
       //===================================================================
       // Get Third Party Payment Information
       //===================================================================
       Begsr GetThirdPartyPaymentInfo;

       isThirdPartyPayment =
        IsModEnabled('Invoice Cloud') and inMode = 'Q' and ELfQte = 'Y'
        and EXicQte = 'Y' and
        not QuotePaymentRecieved(intCo#:intFnd:ELemp#:ELdiv);
       strThirdPartyPaymentBranding = GetModuleProperty('Invoice Cloud':'Name');
       If isThirdPartyPayment = '1' and strThirdPartyPaymentBranding = *blank;
          strThirdPartyPaymentBranding = 'Invoice Cloud';
       Endif;

       EndSr;
       //***  End   Add  ***  03/19/21  *************************

       //===================================================================
       // Get Group Assosiation Flag
       //===================================================================
       BegSr GetGrpAss;

          If %found;
             isOptraComp = GetGrpAssFlg(intCo#:intFnd);
          Endif;

       EndSr;
       //===================================================================
       // Get Image flag
       //===================================================================
       BegSr GetImgFlg;

          Read sdr01;
          If not %eof;
             isImgFlg = DRFL01;
          Endif;

       EndSr;
      /End-Free
       //***  Begin Add  ***  01/11/21  *************************
     PIsMultipleClaim  b
     DIsMultipleClaim  pi             1

      /Free
       Exec sql
        select count(*) into :intCount
         from WAMCP
         where MCco# = :CMco# and MCfnd = :CMfnd
          and MCfyr = :CMfyr and MCcase = :CMcase;
       Select;
       When intCount > 0;
          Return ' ';
       When CheckSecLevel('MultAddClm');
          Return '1';
       Endsl;
       Return ' ';
      /End-free
     PIsMultipleClaim  e
       //***  End   Add  ***  01/11/21  *************************

       //***  Begin Add  ***  06/07/21  *************************
     PIsQuoteEnv       b
     DIsQuoteEnv       pi             1n

     dstrEnv           s              4a

     dRtvEnv           pr                  extpgm('RTVENV')
     d outEnv                         4a
      /Free
       RtvEnv(strEnv);
       Return strEnv = '*QTE';
      /End-free
     PIsQuoteEnv       e
       //***  End   Add  ***  06/07/21  *************************
