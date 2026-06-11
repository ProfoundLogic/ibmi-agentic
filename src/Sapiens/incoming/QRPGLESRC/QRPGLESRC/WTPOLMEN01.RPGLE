     h**********************************************************************
     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to get Suppress Schedule flag
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLMEND
     h*  Calls to:
     h*
     h*  Created on 01/20/2020 by C:298 for Project: WC2703
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwtmyl     if   e           k disk    rename(pmy01:pmy00)

      /copy sPrAtrium
      * Entry
     dDefaultPoints    pr                  ExtPgm('WTPOLMEN01')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d inStart         s              5  0 inz
     d intCo#          s              3  0 inz
     d intFnd          s              3  0 inz
     d charMidE        s              3    inz
     d intMidE         s              3  0 inz
     d xxMidE          s              3    inz
     d charSuppFlg     s              1    inz
     D value           s             10    inz
     d xxCo#           s              3    inz
     d xxFnd#          s              3    inz
     d xxQte           s              1    inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20

      /free
       // input data is in format: ,ccc,fff,mmm
       // ccc - Co#,fff - Fnd#,mmm - Mid Term Endorsement selected

       intCo# = 0;
       intFnd  = 0;
       charMidE = ' ';
       outJSON  = ' ';
       charSuppFlg = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       intMidE = %int(charMidE);
       Chain(n) (intCo#:intFnd:intMidE) pmy00;
       If %found(wtmyl);
          If MYpgm1 = 'W0051R11' or MYpgm1   = 'W0051R12' or
           MYpgm1   = 'W0051R13' or MYpgm1   = 'W0051R15' or
           MYpgm1   = 'W0051R19' or MYpgm1   = 'W0051R20' or
           MYpgm1   = 'W0051R21' or MYpgm1   = 'W0051R40' or
           MYpgm1   = 'W0051R55' or MYpgm1   = 'W0051R65' or
           MYpgm1   = 'ADDNAMEINS';

             BuildJSON(outJSON :'VsbSuppS' : 'Y');
          Else;
             BuildJSON(outJSON :'VsbSuppS' : 'N');
          EndIf;
          charSuppFlg = MYschd;
       Endif;
       BuildJSON(outJSON :'Points' :charSuppFlg);

       RtnCode = ' ';
       Return;

       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;

       StartPos = 0;
       EndPos = 0;
       Rtncode = 'X';
       Monitor;

          // Extract Company Number (1st Parameter)
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxCo# = ExtractValue(startPos : inGenParm);

          // extract fnd
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxFnd# = ExtractValue(startPos : inGenParm);

          // extract Mid Term Endorsement value selected
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxMidE = ExtractValue(startPos : inGenParm);

          Monitor;
            intCo# = %Int(%trim(xxCO#));
            intFnd  = %Int(%trim(xxFND#));
            charMidE = %trim(xxMidE);
          On-Error;
             intCo# = 0;
             intFnd  = 0;
             charMidE = *blanks;
          EndMon;

          Rtncode = ' ';
       On-Error;
          Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
     p*------------------------------------------------------   ************
     p* Procedure name: ExtractValue
     p* Purpose:        Extract the Value of the Gen Params     ************
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

