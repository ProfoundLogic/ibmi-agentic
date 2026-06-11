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
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 06/10/2013 by C:282 for Redmine 3034
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/21/13 C:65  Project: SY266 Redmine #3034
     h*                Change the returned county to be the value, not desc.
     h*
     h**********************************************************************
     Fwmczl     if   e           k disk
     Fwtcnl1    if   e           k disk
      *
      /copy sPrAtrium
      * *ENTRY
     dDefaultCounty    pr                  ExtPgm('WTCLMCLI02')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dDefaultCounty    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1
      *
     D intCo#          s              3  0 inz
     D intZip          s              5  0 inz
     D charState       s              2
     D charCounty      s              2
     D charStCnty      s             10
     D charCntyDesc    s             30
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D dashPos         s              5  0 inz
     D len             s              5  0 inz
     D value           s             41    inz
      /Free

       //                          1   5  7
       // input data is in format: ,ccc,ss,zzzzz-zzzz,
       // ccc - Co#; ss - State; zzzzz-zzzz - Zip

       intCo#       = 0;
       intZip       = 0;
       charState    = ' ';
       charCounty   = ' ';
       charStCnty   = ' ';
       charCntyDesc = ' ';
       outJSON      = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Chain (intCo#:charState:intZip) pcz01;
       If %found(wmczl);
         charCounty = CZcnty;
         // Concatenate the State and County value to retrieve the
         // description value from WTCNP.
         charStCnty = charState + charCounty;
         Chain (intCo#:charStCnty) pcn01;
         If %found(wtcnl1);
            charCntyDesc = CNdesc;
            //*** begin chg ***  06/21/13  *********************************
            //*****BuildJSON(outJSON :'County' :%Trim(charCntyDesc));
            BuildJSON(outJSON :'County' :%Trim(charStCnty));
            //*** end chg *****  06/21/13  *********************************
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
          intCo# = %Int(%trim(value));
         On-Error;
          intCo# = 0;
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

       // Extract Zip
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
         dashPos = %scan('-':value);
           intZip  = %int(%trim(%subst(value:1:dashPos-1)));
         On-Error;
          intZip  = 0;
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
