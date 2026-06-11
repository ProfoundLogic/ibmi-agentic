     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to populate the address lines
     h*               Legal informatin Defence/Plaintiff Attorney .
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 12/11/2013 by C:295 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 10/22/20 C: 079 Support: 133/90795
     h*                 Add Point8 for Firm Name
     h*
     h**********************************************************************
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)

      /copy sPrAtrium

      * *Entry
     dDefaultPoints    pr                  extpgm('WTCLMDTL05')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     d charPoints      s             40    inz
     d charFid         s              9    inz
     d charMod         s              3    inz
     d dashPos         s              5  0 inz
     d endPos          s              5  0 inz
     d intFId#         s              9  0 inz
     d intMod          s              3  0 inz
     d intPhn#         s             10  0 inz
     d intLen          s              5  0 inz
     d initPos         s              5  0 inz
     d pos             s              5  0 inz
     d len             s              5  0 inz
     d startPos        s              5  0 inz
     d strDtlZip1      s             10    inz(*blanks)
     d strDtlphn#      s             30    inz(*blanks)
     d value           s             41    inz

     d                 Ds
     d  intzip                 1      9  0
     d  intzip1                1      5  0
     d  intzip2                6      9  0

      /free
       // input data is in format: ,nnnnnnnnn +' '+ nnn
       // nnnnnnnnn +' '+ nnn: FEDERAL TAX ID+' '+MODIFIER

       intFId#  = 0;
       intMod   = 0;
       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Chain(n) (intFId#:intMod) ppd00;
       If %found(wmpdl);
          charPoints = %trim(PDfsn) + ' ' + %trim(PDname);
          intzip = PDzip;
          strDtlZip1 = %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');

          strDtlphn# = '(' + %editc(PDarea:'X') + ')' +
          %subst(%editc(PDphn#:'X'):1:3) + '-' +
          %subst(%editc(PDphn#:'X'):4:4);

          BuildJSON(outJSON :'Points' :charPoints);
          BuildJSON(outJSON :'Point1' :%trim(PDadr1));
          BuildJSON(outJSON :'Point2' :%trim(PDadr2));
          BuildJSON(outJSON :'Point3' :%trim(PDcty));
          BuildJSON(outJSON :'Point4' :%trim(PDst));
          BuildJSON(outJSON :'Point5' :%trim(strDtlZip1));
          BuildJSON(outJSON :'Point6' :%trim(strDtlphn#));
          BuildJSON(outJSON :'Point7' :%trim(PDemad));
          //*** Beg Add *** 10/22/20 **********************************
          BuildJSON(outJSON :'Point8' :%trim(PDRFNM));
          //*** End Add *** 10/22/20 **********************************
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

          // Extract Federal Id
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          Exsr ExtractValue;
          Exsr ExtractFienMod;

          Monitor;
             intFId# = %Int(%trim(charFid));
             intMod  = %Int(%trim(charMod));
          On-Error;
             intFId# = 0;
             intMod  = 0;
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
             value = %Trim(%Subst(inGenParm:startPos:len));
           Endif;
         Endif;
       Endif;
       EndSr;
       //*********************************************************************
       BegSr ExtractFienMod;
       If value <> *blanks;
         initPos = 1;
         pos = %Scan(' ':value:initPos);

         If Pos > 0;
           len = pos-initPos;
           If len > 0;
             charFid   = %Trim(%Subst(value:initPos:len));
             pos = pos + 1;
             intLen = %Scan('-':value:pos);
             len = intLen - pos;
             charMod   = %Trim(%Subst(value:pos:len));
           Endif;
         Endif;
       Endif;
       EndSr;
      /end-free
