     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to Return posting date for the
     h*               invoice number.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRPTPRNTC
     h*  Calls to:
     h*
     h*  Created on 08/17/2021 by C:079 for Project WC2663
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      /copy sPrDate
      * *Entry
     dWTRPTPRNT1       pr                  extpgm('WTRPTPRNT1')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dWTRPTPRNT1       pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dExtractValue     pr            20
     d inStart                        5  0 const
     d inGenParm                    512    const

     d inStart         s              5  0 inz
     d intCo#          s              3  0 inz
     d intFnd          s              3  0 inz
     d intEmp#         s              9  0 inz
     d intDiv          s              5  0 inz
     d intFyr          s              3  0 inz
     d intInv#         s             10  0 inz
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d outValue        s             20
     d outDate         s              8
     d arPostDate      s              7s 0

      /free
       // input data is in format:

       outJSON  = ' ';

       Exsr ProcessGenParm;

       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Exec Sql Select EApoDt Into :arPostDate From WMEAP
                   Where EAco#=:intCo# And EAfnd=:intFnd And
                         EAemp#=:intEmp# And EAdiv=:intDiv And
                         EAfYr = :intFyr and EAninv=:intInv#
                   Limit 1;

          If arPostDate <> 0;
             outDate = %EditC(dtMDYY(arPostDate):'X');
          EndIf;

       BuildJSON(outJSON :'Date' :outDate);

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
          intCo# = %Int(%Trim(ExtractValue(startPos : inGenParm)));

          // Extract Fund (2nd Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          intFnd = %Int(%Trim(ExtractValue(startPos : inGenParm)));

          // Extract Employer (3rd Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          intEmp# = %Int(%Trim(ExtractValue(startPos : inGenParm)));

          // Extract Division (4th Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          intDiv = %Int(%Trim(ExtractValue(startPos : inGenParm)));

          // Extract Fund Year (5th Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          intFyr = %Int(%Trim(ExtractValue(startPos : inGenParm)));

          // Extract Invoice# (6th Parameter)
          startPos = %Scan(',':inGenParm:startPos)+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          intInv# = %Int(%Trim(ExtractValue(startPos : inGenParm)));

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
