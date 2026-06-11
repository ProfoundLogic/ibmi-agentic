     h**********************************************************************
     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to determine if Jurisdiction state
     h*               and Accident date meets the criteria for Catastrophy 13.
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 04/18/2022 by C:291 for Project WC2852
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium

      * *Entry
     dCatastrophe13    pr                  extpgm('WTCLMDTL07')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dCatastrophe13    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d inStart         s              5  0 inz
     d intAccDt        s              7  0 inz
     d intCount        s              5  0 inz
     d xxAccDt         s              7    inz
     d xxJrSt          s              2a   inz
     d xxCat13St       s              1a   inz
     d xxCatCd         s              5    inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20
     d intCode         s              5s 0 inz
     d strDesc         s             40a   inz
     d strChoices      s            200a   inz
     d strChoVal       s            200a   inz

      /free
       Exec SQL
        set option commit = *none;

       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Reset intCount;
       Exec Sql
        Select Count(*)
        Into :intCount
        From wtelecat
        Where CAst = :xxJrst and
          :intAccDt >= CAstdt and :intAccDt <= CAendt;

       xxCat13St = ' ';
       If intCount > 0;
          xxCat13St = 'Y';
       Endif;

       Exec Sql
        Select CAcode, CAdesc
        Into :intCode, :strDesc
        From wtelecat
        Where CAst = :xxJrst and
          :intAccDt >= CAstdt and :intAccDt <= CAendt
        Limit 1;

       If xxCatCd = *Blank or xxCatCd = '0' or %Trim(xxCatCd) = %Char(intCode);
          strChoVal  = ' ' + ',' + %Char(intCode);
          strChoices = ' ' + ',' + %Char(intCode) + ' - ' + %trim(strDesc);
       Else;
          strChoVal  = ' ' + ',' + %Trim(xxCatCd) + ',' + %Char(intCode);
          strChoices = ' ' + ',' + xxCatCd + ',' + %Char(intCode) + ' - ' +
           %trim(strDesc);
       Endif;

       BuildJSON(outJSON :'Cat13St' :%trim(xxCat13St));
       BuildJSON(outJSON :'Cat13Cho' :%trim(strChoices));
       BuildJSON(outJSON :'Cat13ChoVal' :%trim(strChoVal));

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

          // Extract Jurisdiction State (1st Parameter)
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxJrSt = ExtractValue(startPos : inGenParm);

          // Extract Accident Date (2nd Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxAccDt = ExtractValue(startPos : inGenParm);

          // Extract Cat code (3rd Parameter)
          startPos = %Scan(',' : inGenParm : startPos) + 1;
          xxCatCd = ExtractValue(startPos : inGenParm);

          Monitor;
            intAccDt = %Int(%trim(xxAccDt));
          On-Error;
            intAccDt = 0;
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
