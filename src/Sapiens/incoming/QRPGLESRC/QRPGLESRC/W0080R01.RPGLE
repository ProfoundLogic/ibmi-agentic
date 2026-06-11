     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to Return value of flag (exclude
     h*               from Weekd/Days Calculation) for Payment Value Code 1
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: W0080F and W0120F
     h*  Calls to:
     h*
     h*  Created on 08/04/2014 by C:300 for Project WC2247
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwtabl     if   e           k disk    include(pb201)

      /copy sPrAtrium

      * *Entry
     dW0080R01         pr                  extpgm('W0080R01')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dW0080R01         pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 const
     d inGenParm                    512    const

     d inStart         s              5  0 inz
     d intCO#          s              3  0 inz
     d xxCO#           s              3    inz
     d xxKEY           s              2
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20
     d strxldflg       s              1

      /free
       // input data is in format: ,ccc,kk,
       // ccc: Company #
       // kk: Payment Value code 1

       outJSON  = ' ';

       Exsr ProcessGenParm;

       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Chain (intCO# : xxKEY) pb201;
       If %Found(wtabl);
          strxldflg = B2exwdc;
       Endif;

       If strxldflg <> 'Y';
          strxldflg = 'N';
       Endif;

       BuildJSON(outJSON :'Points' :strxldflg);

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = 'X';
       Monitor;

          // Extract Company Number (1st Parameter)
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxCO# = ExtractValue(startPos : inGenParm);

          // Extract Payment value Code1 (2nd Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxKEY = ExtractValue(startPos : inGenParm);

          Monitor;
            intCO# = %Int(%trim(xxCO#));
          On-Error;
             intCO# = 0;
             xxKEY = *blanks;
          EndMon;

          Rtncode = ' ';
       On-Error;
          Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free

     p*------------------------------------------------------
     p* Procedure name: ExtractValue
     p* Purpose:        Extract the Value of the Gen Params
     p* Returns:
     p*------------------------------------------------------
     pExtractValue     B
     dExtractValue     pi            20
     d inStart                        5  0 const
     d inGenParm                    512    const

     d outValue        s             20
      /Free
       outValue = *blanks;
       If inStart > 0;
         endPos = %Scan(',':inGenParm:inStart);
         If endPos > 0;
           len = endPos-inStart;
           If len > 0;
             outValue = %Trim(%Subst(inGenParm:inStart:len));
           Endif;
         Endif;
       Endif;
       Return outValue;
      /End-Free
     pExtractValue     E
