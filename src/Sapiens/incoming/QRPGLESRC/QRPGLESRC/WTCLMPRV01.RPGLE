     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMPRV
     h*  Calls to:
     h*
     h*  Created on 05/03/2012 by C:189 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/09/20  C:33  Project: SY420
     h*                 Mask SSN
     h*
     h* 12/07/18  C:079 Support: 90/75255
     h*                 If First Name present then display it.
     h*
     h* 12/04/13  C:279 Support: WC099 84315
     h*                 Modified to populate dependency correctly.
     h*
     h* 08/21/14  C:295 Redmine: 4594
     h*                 Modified the ajax population to populate the mail id properly.
     h*
     h**********************************************************************
      * Additional Payee File
     Fwdacp     if   e             disk    ReName(pac01:pac0p)
     Fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)

      /copy sPrDate
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DClaimPrv01       pr                  ExtPgm('WTCLMPRV01')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimPrv01       pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
     D ##TbId          s                   like(GTtbid)
     D ##Key           s             10
      *
     D strDetCheck     s              9
     D strDetDepR      s             40
     D strDetSts       s             10
      *
     D xxPname         s                   like(ACpnam)
      *
     C     keyWTGTL      klist
     C                   Kfld                    ACco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY
      /Free
       Chain(n) inRRN pac0p;

       outJSON = ' ';
       BuildJSON(outJSON :'EmailHyperlink2' :ACemdc);
       If ACfNam = *blanks;
          BuildJSON(outJSON :'OutDetName' :ACpnam);
       Else;
          xxPname = %Trim(ACfNam) + ' ' + %Trim(ACpNam);
          BuildJSON(outJSON :'OutDetName' :xxPname);
       Endif;
       BuildJSON(outJSON :'OutDetAdr1' :ACadr1);
       BuildJSON(outJSON :'OutDetAdr2':ACadr2);
       BuildJSON(outJSON :'OutDetCity':%TrimR(ACcty));
       BuildJSON(outJSON :'OutDetSt':ACst);
       BuildJSON(outJSON :'OutDetZip':%EditW(ACzip:'     -    '));
       BuildJSON(outJSON :'OutDetCC':ACctry);
       BuildJSON(outJSON :'OutDetProv':ACadr3);
       //***  Begin Chg  ***  06/09/20  ***********************
       //BuildJSON(outJSON :'OutDetSS#':ACdss#);
       BuildJSON(outJSON :'OutDetSS#'
        :DisplaySS#(MaskData('SSN':'DetailACdss#':ACdss#)));
       //***  End   Chg  ***  06/09/20  ***********************
       If ACdpcy = 'P';
         BuildJSON(outJSON :'OutDetDep':'Partial');
       Elseif ACdpcy = 'T';
         BuildJSON(outJSON :'OutDetDep':'Total');
       Else;
         BuildJSON(outJSON :'OutDetDep':' ');
       EndIf;
       If ACckal = 'Y';
         BuildJSON(outJSON :'OutDetAllCheck':'Yes');
       Else;
         BuildJSON(outJSON :'OutDetAllCheck':'No');
       EndIf;
       BuildJSON(outJSON :'OutDetDepBO':ACord);
       BuildJSON(outJSON :'OutDetDepDOB': %EditW(dtMDYY(ACddob):'  /  /    '));

       exsr BuildData;

       BuildJSON(outJSON :'OutDetAllCheck': strDetCheck);
       BuildJSON(outJSON :'OutDetDepReln':strDetDepR);
       BuildJSON(outJSON :'OutDetSts':strDetSts);

       RtnCode = ' ';
       Return;
       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       If  ACckal <>  *blanks;
          If  ACckal = 'Y';
             strDetCheck  =  'Yes';
          Else;
             strDetCheck  =  'No';
          Endif;
       Endif;

       ##Tbid = 'DPRL';
       ##Key = ACrel;
       Chain keyWTGTL pgt00;
       If %Found(wtgtl);
          strDetDepR = GTdesc;
       Else;
          strDetDepR = *blanks;
       EndIf;

       If ACstat <> *blanks;
          If ACstat = 'A';
             strDetSts   = 'Active';
          Else;
             strDetSts   = 'Inactive';
          Endif;
       Endif;

       endsr;
      /End-Free
