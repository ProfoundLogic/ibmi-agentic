     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get Reinstatement Fee
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLCXRN
     h*  Calls to:
     h*
     h*  Created on 08/21/2013 by C:279 for Support: 83046
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwtcrl     if   e           k disk    rename(pcr01:pcr00)

      /copy sPrAtrium
      /copy sPrDate
      /copy wPrEmplr

      * *ENTRY
     dDefaultPoints    pr                  ExtPgm('WTPOLCXR02')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

      * work variables
     d dashPos         s              5  0 inz
     d endPos          s              5  0 inz
     d intAmt          s             11  2 inz
     d intDate         s              7  0 inz
     d isMode          s              5    inz
     d intCo#          s              3  0 inz
     d len             s              5  0 inz
     D numEdtCde       c                   const('N')
     d startPos        s              5  0 inz
     d value           s             41    inz
     d xxSt            s              2    inz
     d xxKey           s             10    inz
     d xxEfdt          s                   like(CRefdt)

      /free
       //                          1   5
       // input data is in format: ,ccc,xxxxx,yy,zzzzzzz
       // ccc - Co#; x - (mode); y - reason code; z - Cancellation date

       intCo# = 0;
       intAmt = 0;
       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Chain inRrn pel0p;
       If %found;
          xxSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);

          If %trim(isMode) = 'true';
             xxEfdt = DateTo7(SystemDate);
          Endif;

          SetGt (intCo#:xxSt:xxKey:xxEfDt) pcr00;
          Readpe (intCo#:xxSt:xxKey) pcr00;
          Dow not %eof and CRstat = 'I';
             Readpe (intCo#:xxSt:xxKey) pcr00;
          Enddo;
          intAmt = CRrfee;
          BuildJSON(outJSON :'Points' :%EditC(intAmt:numEdtcde));
       Else;
          intAmt = 0;
          BuildJSON(outJSON :'Points' :%EditC(intAmt:numEdtcde));
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

          // Extract Mode
          startPos = endPos+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          Exsr ExtractValue;

          Monitor;
             isMode = %trim(value);
          On-Error;
             isMode = 'false';
          EndMon;

          // Extract reason code
          startPos = endPos+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          Exsr ExtractValue;

          Monitor;
             xxKey = %trim(value);
          On-Error;
             xxKey = ' ';
          EndMon;

          // Extract date
          startPos = endPos+1;
          If startPos <= 1;
             LeaveSr;
          Endif;
          Exsr ExtractValue;

          Monitor;
             xxEfDt = %int(%trim(value));
          On-Error;
             xxEfDt = 0;
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
      /end-free
