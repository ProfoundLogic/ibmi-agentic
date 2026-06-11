     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve file member list
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: Ajax call from system toolbar
     h*  Calls to:
     h*
     h*  Created on 04/24/2013 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      /copy sPrString
     D/copy scpyUsrDry
     D/copy sprDate
      * *ENTRY
     DDiaryAlertCnt    pr                  ExtPgm('STDRYALRT')
     D                                9  0
     D                            32767
     D                                1
      *
     DDiaryAlertCnt    pi
     D inRrn                          9  0
     D outJSON                    32767
     D RtnCode                        1
     D*
     D psds          esds                  extname(szq1p)
     D*
     D strUser         s             10    inz
     D dateToday       s              7  0 inz
     D diaryCount      s              9  0 inz
     D todayCount      s              9  0 inz
     D pastCount       s              9  0 inz
     D alertCount      s              9  0 inz
      /Free

       //                          1   5  8  9
       // No genparm, RRN is defaulted to 1

       outJSON = ' ';
       Monitor;
         strUser = Q1user;
         dateToday = DateTo7(SystemDate());
         diaryCount = GetDartCount(strUser:dateToday:'>');
         todayCount = GetDartCount(strUser:dateToday:'=');
         pastCount = GetDartCount(strUser:dateToday:'<');
         alertCount = todayCount + pastCount;
         BuildJSON(outJSON:'diaryCount':%Editc(diaryCount:'X'));
         BuildJSON(outJSON:'alertCount':%Editc(alertCount:'X'));
         RtnCode = ' ';
       On-Error;
         BuildJSON(outJSON :'RtnCode' :'Diary/alert count not retrieved');
         RtnCode = 'X';
       EndMon;

       Return;
