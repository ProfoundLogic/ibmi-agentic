     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Taxes and Assessment Tab
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPSTRAT
     h*  Calls to:
     h*
     h*  Created on 02/03/2021 by C:298 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwdrrp     if   E             disk    rename(prr01:prr0p)
     fwtgtl     if   E           k disk    rename(pgt01:pgt00)

     d/copy sPrDate
     d/copy sPrAtrium

      * *Entry
     d WTGRPSTR01      pr                  extpgm('WTGRPSTR01')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     d WTGRPSTR01      pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     ddsAss            ds                                           inz
     dstrAss                         40    inz
     dSvstp                           9  6 inz
     dSvstf                           9  6 inz
     dSvstl                           1    inz
     dSvsth                          20    inz
     dSvstc                           5  0 inz
     dSvilf                           1    inz
     dSvxfc                           1    inz
     dSvstm                           1    inz
     dSvidv                           1    inz
     dSvstb                           1    inz
     dSvatd                           1    inz
     dSvidd                           1    inz
     dSvatr                           1    inz

     d intrrn          s              9  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d startPos        s              5  0 inz
     d value           s             41    inz
     d svAppTo         s             40    inz
     d svIncLmt        s             40    inz
     d svIncTer        s             80    inz
     d xxKey           s             10    inz
     d xxTBID          s              4    inz
     d charStc         s              5    inz

      /Free
       // input data is in format: ,nnnnnnnnn +' '+ nnnnnnnnn

       intRrn = 0;
       outJSON  = ' ';

       Exsr ProcessGenParm;

       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Chain(n) inRrn prr0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       Exsr BuildData;

       BuildJSON(outJSON :'Points' :%trim(strAss));
       BuildJSON(outJSON :'Point1' :%editc(Svstp:'N'));
       BuildJSON(outJSON :'Point2' :%editc(Svstf:'N'));
       BuildJSON(outJSON :'Point3' :%trim(svAppTo));
       BuildJSON(outJSON :'Point4' :%trim(Svsth));
       BuildJSON(outJSON :'Point5' :charStc);
       BuildJSON(outJSON :'Point6' :%trim(SvIncLmt));
       BuildJSON(outJSON :'Point7' :Svxfc);
       BuildJSON(outJSON :'Point8' :Svstm);
       BuildJSON(outJSON :'Point9' :Svidv);
       BuildJSON(outJSON :'Point10' :Svstb);
       BuildJSON(outJSON :'Point11' :Svatd);
       BuildJSON(outJSON :'Point12' :Svidd);
       BuildJSON(outJSON :'Point13' :svIncTer);

       RtnCode = ' ';
       Return;

       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
        startPos = 0;
        endPos = 0;

        Monitor;
           // Extract
           startPos = %Scan(',':inGenParm)+1;
           If startPos <= 1;
              LeaveSr;
           Endif;
           Exsr ExtractValue;
           intRrn = %int(value);

           Rtncode = ' ';
        On-Error;
           Rtncode = 'X';
        EndMon;
       EndSr;

       //*********************************************************************
       // Extract values from genparm
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

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;
        Select;
        When intRrn = 1;
           strAss = 'First Assessment';
           Svstp = RR1stp;
           Svstf = RR1stf;
           Svstl = RR1stl;
           Svsth = RR1sth;
           Svstc = RR1stc;
           Svilf = RR1ilf;
           Svxfc = RR1xfc;
           Svstm = RR1stm;
           Svidv = RR1idv;
           Svstb = RR1stb;
           Svatd = RR1atd;
           Svidd = RR1idd;
           Svatr = RR1atr;

        When intRrn= 2;
           strAss = 'Second Assessment';
           Svstp = RR2ndp;
           Svstf = RR2ndf;
           Svstl = RR2ndl;
           Svsth = RR2ndh;
           Svstc = RR2ndc;
           Svilf = RR2ilf;
           Svxfc = RR2xfc;
           Svstm = RR2ndm;
           Svidv = RR2idv;
           Svstb = RR2ndb;
           Svatd = RR2atd;
           Svidd = RR2idd;
           Svatr = RR2atr;

        When intRrn= 3;
           strAss = 'Third Assessment';
           Svstp = RR3rdp;
           Svstf = RR3rdf;
           Svstl = RR3rdl;
           Svsth = RR3rdh;
           Svstc = RR3rdc;
           Svilf = RR3ilf;
           Svxfc = RR3xfc;
           Svstm = RR3rdm;
           Svidv = RR3idv;
           Svstb = RR3rdb;
           Svatd = RR3atd;
           Svidd = RR3idd;
           Svatr = RR3atr;

        When intRrn= 4;
           strAss = 'Fourth Assessment';
           Svstp = RR4thp;
           Svstf = RR4thf;
           Svstl = RR4thl;
           Svsth = RR4thh;
           Svstc = RR4thc;
           Svilf = RR4ilf;
           Svxfc = RR4xfc;
           Svstm = RR4thm;
           Svidv = RR4idv;
           Svstb = RR4thb;
           Svatd = RR4atd;
           Svidd = RR4idd;
           Svatr = RR4atr;

        When intRrn= 5;
           strAss = 'Fifth Assessment';
           Svstp = RR5thp;
           Svstf = RR5thf;
           Svstl = RR5thl;
           Svsth = RR5thh;
           Svstc = RR5thc;
           Svilf = RR5ilf;
           Svxfc = RR5xfc;
           Svstm = RR5thm;
           Svidv = RR5idv;
           Svstb = RR5thb;
           Svatd = RR5atd;
           Svidd = RR5idd;
           Svatr = RR5atr;

        When intRrn= 6;
           strAss = 'Sixth Assessment';
           Svstp = RR6thp;
           Svstf = RR6thf;
           Svstl = RR6thl;
           Svsth = RR6thh;
           Svstc = RR6thc;
           Svilf = RR6ilf;
           Svxfc = RR6xfc;
           Svstm = RR6thm;
           Svidv = RR6idv;
           Svstb = RR6thb;
           Svatd = RR6atd;
           Svidd = RR6idd;
           Svatr = RR6atr;

        When intRrn = 7;
           strAss = 'Seventh Assessment';
           Svstp = RR7thp;
           Svstf = RR7thf;
           Svstl = RR7thl;
           Svsth = RR7thh;
           Svstc = RR7thc;
           Svilf = RR7ilf;
           Svxfc = RR7xfc;
           Svstm = RR7thm;
           Svidv = RR7idv;
           Svstb = RR7thb;
           Svatd = RR7atd;
           Svidd = RR7idd;
           Svatr = RR7atr;
        Endsl;

        Select;
        When SvStl = 'J';
           svAppTo = 'J - Subject';
        When SvStl = 'E';
           svAppTo = 'E - Modified';
        When SvStl = 'S';
           svAppTo = 'S - Standard';
        When SvStl = 'D';
           svAppTo = 'D - Discounted';
        When SvStl = 'X';
           svAppTo = 'X - Annual';
        Other;
           svAppTo = ' ';
        Endsl;

        Select;
        When Svilf = 'I';
           svIncLmt = 'Increased Limits Premium';
        When Svilf = 'W';
           svIncLmt = 'Premium Without Increased Limits';
        Other;
           svIncLmt = ' ';
        Endsl;

        Clear xxKey;
        xxTbid = 'TERA';
        xxKey = Svatr;
        Chain(n) (RRCo#:xxTbid:xxKey) pgt00;
        If %found(wtgtl);
           svIncTer = GTdesc;
        Endif;

        If svStc = 0;
           charStc = ' ';
        Else;
           charStc = %trim(%char(svStc));
        Endif;
       Endsr;
      /End-free
