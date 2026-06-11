     h**********************************************************************
     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to determine Default Template, Default
     h*               Path, Default summary Template, Default Summary Path
     h*
     h*  Called from: WSDOCBILL
     h*  Calls to:
     h*
     h*  Created on 05/04/2022 by C:291 for Project WC2670
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium

      * *Entry
     dDocBillSetup     pr                  extpgm('WSDOCBIL01')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dDocBillSetup     pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 CONST
     d inGenParm                    512    CONST

     d inStart         s              5  0 inz
     d intCo#          s              3  0 inz
     d xxBillType      s              1a   inz
     d xxDoc           s             60a   inz
     d xxPath          s            250a   inz
     d xxSumd          s             60a   inz
     d xxSump          s            250a   inz
     d xxDocR          s             60a   inz
     d xxPathR         s            250a   inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20

     d psds          esds                  extname(szq1p)
     d/copy wPrUsrPrms

      /free
       Exec SQL
        set option commit = *none;
       outJSON  = ' ';
       intCo# = UserCo#(q1user);

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Exec Sql
        Select BLdoc, BLpath, BLsumd, BLsump
        Into :xxDoc, :xxPath, :xxSumd, :xxSump, :xxDocR, :xxPathR
        From wsblp1
        Where BLco# = :intCo# and BLfnd = 0 and BLst =' ' and
         BLtype = :xxBillType and BLefdt = (Select Max(BLefdt) From wsblp1
         where BLco# = :intCo# and BLtype = :xxBillType and BLfnd = 0
         and BLst =' ');

       BuildJSON(outJSON :'Doc' :%trim(xxDoc));
       BuildJSON(outJSON :'Path' :%trim(xxPath));
       BuildJSON(outJSON :'Sumd' :%trim(xxSumd));
       BuildJSON(outJSON :'Sump' :%trim(xxSump));
       BuildJSON(outJSON :'DocR' :%trim(xxDocR));
       BuildJSON(outJSON :'PathR' :%trim(xxPathR));

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

          // Extract Company (1st Parameter)
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          xxBillType = ExtractValue(startPos : inGenParm);

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
