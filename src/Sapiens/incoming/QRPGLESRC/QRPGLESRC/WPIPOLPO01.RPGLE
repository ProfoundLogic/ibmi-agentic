     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to Points
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WPIPOLPOP
     h*  Calls to:
     h*
     h*  Created on 07/30/2013 by C:282 for Redmine 2930
     h**********************************************************************
     h* Revisions:
     h* 12/06/16 C:307 Support: 44507                                 *
     h*                Corrected error when attempting to enter       *
     h*                Aircraft Class 9108                            *
     h**********************************************************************
     Fwdrrl     if   e           k disk
      *
      /copy sPrAtrium
      /copy sprdate
      * *ENTRY
     dDefaultAirCft    pr                  ExtPgm('WPIPOLPO01')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dDefaultAirCft    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1
      *
     d intCo#          s              3  0
     d intFnd          s              3  0
     d intEffDt        s              7  0
     d intEffDt8       s              8  0
     d charEffDt8      s              8
     d charEffDt10     s             10
     D intClassCd      s              5  0
     D slash1          s              2  0 inz
     D slash2          s              2  0 inz
     D charClassExt    s              5
     d charState       s              2
     D charAirCftDs    s             30
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D dashPos         s              5  0 inz
     D len             s              5  0 inz
     D value           s             41    inz
      /Free

       //                          1   5   8  11       20    26
       // input data is in format: ,ccc,ggg,ss,dddddddd,cdcdc,
       // ccc - Co#; ggg - group; ss - State; dddddddd - Eff. Date; cdcdc - class code

       intCo#       = 0;
       intFnd       = 0;
       intEffDt     = 0;
       intEffDt8    = 0;
       intClassCd   = 0;
       charClassExt = ' ';
       charEffDt8   = ' ';
       charEffDt10  = ' ';
       charState    = ' ';
       charAirCftDs = ' ';
       outJSON      = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       intEffDt = dtCYMD(intEffDt8);
       SetGt (intCo#:intFnd:charState:intEffDt) prr01;
       Readpe (intCo#:intFnd:charState) prr01;
       If Not %EOF(wdrrl);
            If intClassCd  = RRdccd;
               If charClassExt <> ' ';
                  BuildJSON(outJSON :'AirCraftExist':'Y');
                  BuildJSON(outJSON :'AirCraftDesc':%Trim(charAirCftDs));
               Else;
                  BuildJSON(outJSON :'AirCraftExist':'Y');
               Endif;
            Else;
               BuildJSON(outJSON :'AirCraftExist':'N');
            Endif;
       Endif;

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

       // Extract company number
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          intCo# = %Int(value);
         On-Error;
          intCo# = 0;
         EndMon;

       // Extract Group
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          intFnd    = %Int(value);
         On-Error;
          intFnd    = 0;
         EndMon;

       // Extract State
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          charState = value;
         On-Error;
          charState = *Blanks;
         EndMon;

       // Extract Effective Date
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
         charEffDt10= value;
         If %scan('/':charEffDt10:1) <> 0;
            slash1 = %scan('/':charEffDt10:1);
            slash2 = %scan('/':charEffDt10:slash1+1);
            charEffDt8 = %subst(charEffDt10:1:slash1-1)  +
                         %subst(charEffDt10:slash1+1:slash2-slash1-1)  +
                         %subst(charEffDt10:slash2+1);
            intEffDt8 = %Int(charEffDt8);
          Else;
            intEffDt8 = %Int(%trim(charEffDt10));
          Endif;
         On-Error;
          intEffDt8 = 0;
         EndMon;

       // Extract Class Code Ext
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          charClassExt = value;
         On-Error;
          charClassExt = *Blanks;
         EndMon;

       // Extract Class Code
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
         dashPos = %scan('-':value);
       //***** Beg Chg ******* 12/06/16 ********************************
         If dashPos > 0;
       //***** End Chg ******* 12/06/16 ********************************
           intClassCd   = %int(%trim(%subst(value:1:dashPos-1)));
           charAirCftDs = %trim(%subst(value:dashPos+1));
       //***** Beg Chg ******* 12/06/16 ********************************
         Else;
           intClassCd   = %int(%trim(%subst(value:1:5)));
           charAirCftDs = *blanks;
         EndIf;
       //***** End Chg ******* 12/06/16 ********************************
         On-Error;
          intClassCd    = 0;
          charAirCftDs  = ' ';
         EndMon;
         Rtncode = ' ';

       On-Error;
         Rtncode = 'X';
       EndMon;

       EndSr;
       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;
      /End-Free
