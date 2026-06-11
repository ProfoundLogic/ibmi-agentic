     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to populate the Dart Remarks for Group
     h*
     h*  Called from: W7230R
     h*  Calls to:
     h*
     h*  Created on 05/05/2015 by C:295 for Project 114WMZ: Priss# 15
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmexl     if   e           k disk    ReName(pex01:pex00)
     fstctl1    if   e           k disk

      /copy sPrAtrium

      * *Entry
     dDefaultPoints    pr                  extpgm('W7230R01')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d intCO#          s              3  0 inz
     d intFND#         s              3  0 inz
     d xxCO#           s              3    inz
     d xxFND#          s              3    inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d charDrtRmk      s             80
     d outValue        s             20

      /free
       // input data is in format: ,ccc,ggg
       // ccc: Company #
       // ggg: Group #

       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       If xxFND# <> '000';
          Chain (intCO# : intFND#) wmexl;
          If %found(Wmexl) and EXauRq <> *blanks;
             Chain EXauRq pct01;
             If %found(stctl1);
                charDrtRmk = %Trim(CTrmCd) + ' - ' + %Trim(CTdesc);
             Endif;
          Endif;
       Endif;

       BuildJSON(outJSON :'Points' :%trim(charDrtRmk));

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

          // Extract Group Number (2nd Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxFND# = ExtractValue(startPos : inGenParm);

          Monitor;
            intCO# = %Int(%trim(xxCO#));
            intFND# = %Int(%trim(xxFND#));
          On-Error;
             intCO# = 0;
             intFND# = 0;
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
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

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
