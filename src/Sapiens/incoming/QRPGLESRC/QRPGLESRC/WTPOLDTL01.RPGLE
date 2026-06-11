     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Endorsement Form Info
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTPOLDTL
     h*  Calls to:
     h*
     h*  Created on 03/08/2024 by C:300 for WC2433
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h*
     h**********************************************************************
      * Endorsement Form table
     fwtenl     if   e           k disk    rename(pen01:pen00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrModule

      *entry
     dWtPolDtl01       pr                  ExtPgm('WTPOLDTL01')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dWTPOLDTL01       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d comp1           s                   like(ENco#)
     d fnd1            s                   like(ENfnd)
     d form1           s                   like(ENform)

     d strprnt         s             10    inz(*blanks)

     d startPos        s              5  0 inz
     d endPost         s              5  0 inz
     d len             s              5  0 inz
     d value           s             20    inz


     d psds          esds                  extname(szq1p)


     c     enky          klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    form1




      /Free
       //                          1   5  8  9
       // input data is in format: ,ccc,ggg,form
        // ccc = Co#, ggg - group; form - Endorsement Form

       Exsr ProcessGenParm;
       if RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       endif;

       strprnt = *blanks;

       chain enky pen00;
       if %found(wtenl);
          if ENprnt = 'Y';
             strprnt = 'Yes';
          else;
             strprnt = 'No';
          endif;

          BuildJSON(outJSON :'Points1' :%trim(ENdsc2));
          BuildJSON(outJSON :'Points2' :%trim(ENst));
          BuildJSON(outJSON :'Points3' :%trim(strprnt));
       endif;


       RtnCode = ' ';
       Return;

       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;
       value = *blanks;
       if startPos > 0;
         endPost = %Scan(',':inGenParm:startPos);
         if endPost > 0;
           len = endPost-startPos;
           if len > 0;
             value = %Subst(inGenParm:startPos:len);
           endif;
         endif;
       endif;
       EndSr;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
       startPos = 0;
       endPost = 0;
       Monitor;
       // extract Company No.
         startPos = %Scan(',':inGenParm)+1;
         if startPos <= 1;
           LeaveSr;
         endif;
         Exsr ExtractValue;
         comp1= %int(%trim(value));
       // extract fund
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         fnd1 = %int(%trim(value));
       // extract endorsement form
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         form1 = %trim(value);

         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
