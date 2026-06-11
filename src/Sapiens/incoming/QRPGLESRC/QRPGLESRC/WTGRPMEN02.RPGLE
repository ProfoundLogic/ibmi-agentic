     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Midterm Endorsements
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPMEND
     h*  Calls to:
     h*
     h*  Created on 09/27/2018 by C:267 for Project WC2584
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * file
     fwtm2p     uf   e             Disk    reName(pm201:pm20p)

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable

      * *Entry
     dMidtermEndors02  pr                  ExtPgm('WTGRPMEN02')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dMidtermEndors02  pi
     d inRRN                          9  0
     D inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d strMESts        s             10    inz(*blanks)
     d strMEFile       s              6    inz(*blanks)

     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz
     D valueOut        s             10a

     d psds          esds                  extname(szq1p)

      /free

       //                          1   2
       // input data is in format: ,S
       // S = MENDStatus

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Select;

       When strMEFile = 'WTM2P ';
          Chain inRRN pm20p;
          If not %found;
             BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
              %char(inRRN) + 'for WTM2P file');
             Return;
          Endif;
          ExSr UpdMENDStaInfo;
       EndSl;

       outJSON = ' ';
       BuildJSON(outJSON :'MEStatus' : %trim(ValueOut) );
       unlock WTM2P;

       RtnCode = ' ';
       Return;

       //=================================================================
       // extract values from genparm
       //=================================================================
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

       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract Midterm Endorsement status field value
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         strMESts  = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;

       startPos = 0;
       Monitor;
       // extract Midterm Endorsement file name
         startPos = %Scan(',':inGenParm:endPos)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         strMEFile = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;

      //========================================================================
       // UpdMENDStaInfo - Revise Midterm Endorsement Specific State status info
      //========================================================================
       BegSr UpdMENDStaInfo;

       // Status
       Select;
       When strMESts = 'Active';
            M2stat = 'I';
            ValueOut = 'inActive';
       When strMESts = 'inActive';
            M2stat = 'A';
            ValueOut = 'Active';
       Endsl;

       M2USER = q1User;
       M2PGM  = q1Pgm;
       M2DT   = Dateto7(SystemDate());
       M2TIME = Timeto6(SystemTime());

       Update pm20p;

       EndSr;
      /end-free
