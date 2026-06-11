     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to show asterisk(*) mark on NAICS field
     h*               on selecting a particular state.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*  Called from: WPILOCMST
     h*  Calls to:
     h*
     h*  Created on 06/01/15 by C:307 for Support PHW34938
     h**********************************************************************
     h* Revisions:
     h*
     h* 04/29/24 C:340 Project: WC2965
     h*                Email Address Required configuration.
     h*
     h* 03/04/24 C:079 Project: WC2951
     h*                Recompile over WPILOCMST screen changes
     h*
     h* 03/28/23 C:342 Support: 107744 (Sunz)
     h*                Recompile over WPILOCMST screen changes
     h*
     h**********************************************************************
     fwdrrl1    if   e           k disk
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     fwdrrl     if   e           k disk    rename(prr01:prr0p)

      /copy sprAtrium
      /copy wprLocMst

      * *Entry
     dDefaultPoints    pr                  extpgm('WTPOLLMS05')
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

     d inStart         s              5  0 inz
     d intCo#          s              3  0 inz
     d intFnd#         s              3  0 inz
     D phyState        s              2    inz
     D mailState       s              2    inz
     d charQte         s              1    inz
     D state           s              2    inz
     D value           s             10    inz
     d xxCo#           s              3    inz
     d xxFnd#          s              3    inz
     d xxQte           s              1    inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20

      /free
       // input data is in format: ,ccc,fff,pp,mm,q
       // ccc - Co#; fff - Fnd#; pp - physical state; mm - mailing state,q - NAICS flag quote

       intCo# = 0;
       intFnd# = 0;
       phyState = *Blanks;
       mailState = *Blanks;
       charQte = *Blanks;
       outJSON = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       // set key values
       If phyState <> *Blanks;
         state = phyState;
       Else;
         state = mailState;
       Endif;
       Chain(n) (intCo#:intFnd#:state) PRR01;
       If %found(wdrrl1) and RRnaic = 'Y' and charQte <> 'Q';
          BuildJSON(outJSON :'VsbNaics' : 'Y');
       Else;
          BuildJSON(outJSON :'VsbNaics' : 'N');
       EndIf;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************

          If mailState <> ' ' Or phyState <> ' ';
             SetGT (intCo#:intFnd#:mailState) WDRRL;
             ReadPE (intCo#:intFnd#:mailState) WDRRL;
             If Not %EOF and RREmail = 'Y';
                BuildJSON(outJSON :'VsbEmail': 'Y');

                SetGT (intCo#:intFnd#:phyState) WDRRL;
                ReadPE (intCo#:intFnd#:phyState) WDRRL;
                If Not %EOF and RREmail = 'Y';
                   BuildJSON(outJSON :'VsbEmail': 'Y');
                Else;
                   BuildJSON(outJSON :'VsbEmail': 'N');
                EndIf;

             Else;
                BuildJSON(outJSON :'VsbEmail': 'N');
             EndIf;
          EndIf;

       //*** End Add ***  04/29/24  *** WC2965 *************************

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
          xxCo# = ExtractValue(startPos : inGenParm);

          // extract fnd
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxFnd# = ExtractValue(startPos : inGenParm);

          // extract phy state
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          phyState = ExtractValue(startPos : inGenParm);

          // extract mail state
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          mailState = ExtractValue(startPos : inGenParm);

          // extract flgQte
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          charQte = ExtractValue(startPos : inGenParm);

          Monitor;
            intCo# = %Int(%trim(xxCO#));
            intFnd# = %Int(%trim(xxFND#));
            If phyState <> *blanks;
               state = %trim(phyState);
            Else;
               state = %trim(mailState);
            EndIf;
            charQte = %trim(xxQte);
          On-Error;
             intCo# = 0;
             intFnd# = 0;
             phyState = *blanks;
             mailState = *blanks;
             charQte = *blanks;
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
