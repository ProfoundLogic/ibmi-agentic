     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Upload Certificate Holders screen (WTRPTCERTH)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRPTCERTH
     h*  Calls to:
     h*
     h*  Created on 10/21/2016 by C:310 for Project WC2435
     h**********************************************************************
     h* Revisions:
     h*
     h* 03/13/17 C:136 Project: 148WAD
     h*                Cert name was being populated to outName instead of
     h*                outName1 which put the cert name in the group name field
     h*
     h**********************************************************************
     fWMCEP     if   e             disk

      /copy sPrAtrium

      * *ENTRY
     dWTRPTCER01       pr                  extpgm('WTRPTCER01')
     d                                9  0
     d                            32767
     d                                1

     dWTRPTCER01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strCertNbr      s              5
     d strRevNbr       s              9
     d strName         s             40
     d strAddress      s             40
     d strAddress2     s             40
     d strCity         s             25
     d strState        s              2
     d strZip          s             10
     d strCntryCod     s              5
     d strProviSt      s             40
     d strAttention    s             60
     d strRemark1      s             60
     d strRemark2      s             60
     d strRemark3      s             60
     d strRemark4      s             60
     d strRemark5      s             60
     d strRollover     s              3
     d strOffcrExl     s              3
     d strThrdPaty     s              3
     d strSpclJob      s              3
      /Free

        Chain inRRN WMCEP;
        Exsr BuildData;
        outJSON = ' ';

       // Populate Detail Box information.
        BuildJSON(outJSON :'OutCertiFicate':%trim(strCertNbr));
        BuildJSON(outJSON :'OutRevisionNbr':%trim(strRevNbr));
        BuildJSON(outJSON :'OutName1':%trim(strName));
        BuildJSON(outJSON :'OutAddressLine1':%trim(strAddress));
        BuildJSON(outJSON :'OutAddressLine2':%trim(strAddress2));
        BuildJSON(outJSON :'OutCity':%trim(strCity));
        BuildJSON(outJSON :'OutState':%trim(strState));
        BuildJSON(outJSON :'OutZip':%trim(strZip));
        BuildJSON(outJSON :'OutCntryCode':%trim(strCntryCod));
        BuildJSON(outJSON :'OutProvncePost':%trim(strProviSt));
        BuildJSON(outJSON :'OutAttention':%trim(strAttention));
        BuildJSON(outJSON :'OutRemark1':%trim(strRemark1));
        BuildJSON(outJSON :'OutRemark2':%trim(strRemark2));
        BuildJSON(outJSON :'OutRemark3':%trim(strRemark3));
        BuildJSON(outJSON :'OutRemark4':%trim(strRemark4));
        BuildJSON(outJSON :'OutRemark5':%trim(strRemark5));
        BuildJSON(outJSON :'OutRollover':%trim(strRollover));
        BuildJSON(outJSON :'OutOfficerExc':%trim(strOffcrExl));
        BuildJSON(outJSON :'OutThirdparty':%trim(strThrdPaty));
        BuildJSON(outJSON :'OutSpecialJob':%trim(strSpclJob));

        rtnCode = ' ';
        Return;

      //===================================================================
      // Buld Data that will be passed on to AJAX
      //===================================================================
       Begsr BuildData;

         strCertNbr       =  %Trim(%Char(CECER#));
         strRevNbr        =  %Trim(%Char(CEREV#));
         strName          =  %Trim(CENAME);
         strAddress       =  %Trim(CEADR1);
         strAddress2      =  %Trim(CEADR2);
         strCity          =  %Trim(CECTY);
         strState         =  %Trim(CEST);
         strZip           =  %subst(%editc(CEZip:'X'):1:5) + '-' +
                             %subst(%editc(CEZip:'X'):6:4);
         strCntryCod      =  %Trim(CECTRY);
         strProviSt       =  %Trim(CEPROV);
         strAttention     =  %Trim(CEATTN);
         strRemark1       =  %Trim(CEREM1);
         strRemark2       =  %Trim(CEREM2);
         strRemark3       =  %Trim(CEREM3);
         strRemark4       =  %Trim(CEREM4);
         strRemark5       =  %Trim(CEREM5);
         If CERLFL = 'Y';
            strRollover = 'Yes';
         Else;
            strRollover = 'No';
         Endif;

         If CEEOFL = 'Y';
            strOffcrExl = 'Yes';
         Else;
            strOffcrExl = 'No';
         Endif;

         If CE3PTY = 'Y';
            strThrdPaty = 'Yes';
         Else;
            strThrdPaty = 'No';
         Endif;

         If CETYPE = 'Y';
            strSpclJob  = 'Yes';
         Else;
            strSpclJob  = 'No';
         Endif;

       endsr;
      /End-Free
