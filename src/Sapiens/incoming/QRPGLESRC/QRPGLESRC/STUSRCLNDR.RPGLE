     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve User calender items
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STUSRDRY
     h*  Calls to:
     h*
     h*  Created on 10/16/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 02/04/15 c:62 Project 114WGU
     h*               Add Visit Tracking
     h*
     h**********************************************************************
     f*** Begin Chg *** 02/04/15 *********************************
     f*******smupp     if   e             disk    rename(pup01:pup0p)
     fwmupp     if   e             disk    rename(pup01:pup0p)
     f*** End   Chg *** 02/04/15 *********************************
     fsdcml4    if   e           k disk    rename(pcm01:pcm04)
     f*** Begin Add *** 02/04/15 *********************************
     fwmvil4    if   e           k disk    rename(pvi01:pvi04)
     f*** End   Add *** 02/04/15 *********************************

      /copy sPrAtrium
      /copy sPrDate
     D/copy sprTeam
      * *ENTRY
     DPopulateUserCal  pr                  ExtPgm('STUSRCLNDR')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DPopulateUserCal  pi
     D inRrn                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D month           ds                  inz
     D   mmyyyy                       6  0 overlay(month)
     D   mm                           2  0 overlay(month)
     D   yyyy                         4  0 overlay(month:3)

     D curDateDS       ds
     D curDate                        7  0 overlay(curDateDS)
     D curCYY                         3  0 overlay(curDateDS)
     D curMM                          2  0 overlay(curDateDS:4)
     D curDD                          2  0 overlay(curDateDS:6)

     D date8           s              8  0 inz
     D startDate       s              7  0 inz
     D endDate         s              7  0 inz
     D curCount        s              7  0 inz
     D curUser         s                   inz like(UPusr)
     D teamMode        s              1    inz
     D intCount        s              5  0 inz
     D intI            s              5  0 inz
     D ptrTeamList     s               *   inz

     D arrTeamList     s                   dim(9999) inz  like(UPusr)
     D arrMthCount     s              7  0 dim(31) inz ascend
     D*** Begin Add *** 02/04/15 *********************************
     D arrMthCountV    s              7  0 dim(31) inz ascend
     D*** End   Add *** 02/04/15 *********************************

      /Free

       Chain(n) inRrn pup0p;
       Monitor;
         mmyyyy = %Int(%TrimR(%SubSt(inGenParm:1:6)));
       On-Error;
         mmyyyy = 0;
       EndMon;
       teamMode = %SubSt(inGenParm:7:1);

       outJSON = ' ';
       //*** Begin Chg *** 02/04/15 **********************************
       //*** If not %Found(smupp) Or mmyyyy= 0;
       If not %Found(wmupp) Or mmyyyy= 0;
       //*** End   Chg *** 02/04/15 **********************************
         BuildJSON(outJSON :'RtnCode' :'Invalid input values');
         RtnCode = 'X';
         Return;
       Endif;

       //Build Start and End Dates
       date8 = 1000000*mm+10000+yyyy;
       startDate = DtCymd(date8);
       endDate = DateTo7((CvtToDate(date8)+ %Months(1) - %Days(1)));

       Clear arrMthCount;

       If teamMode = *Blanks;
         curUser = UPusr;
         Exsr GetUserCounts;
       Else;
         ptrTeamList = %Addr(arrTeamList);
         GetUserTeamList(UPusr :' ' :ptrTeamList :intCount :'Y');
         For intI = 1 to intCount;
           curUser =  arrTeamList(intI);
           Exsr GetUserCounts;
         EndFor;
       Endif;

       outJSON = %Trim(outJSON) + '"diary":';
       StartJSONarray(outJSON);
       Exsr LoadCountsToJSON;
       EndJSONarray(outJSON);
       //*** Begin Add *** 02/04/15 *********************************
       outJSON = %Trim(outJSON) + ', "visits":';
       StartJSONarray(outJSON);
       Exsr LoadVstCntToJSON;
       EndJSONarray(outJSON);
       //*** End   Add *** 02/04/15 *********************************

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Get User counts
       //**************************************************************
       BegSr GetUserCounts;
       curDate = 0;
       curCount = 0;
       Setll (curUser:startDate) pcm04;
       DoU %Eof(sdcml4);
         Reade(n) curUser pcm04;
         If %Eof(sdcml4) Or CMcdat > endDate;
           Exsr LoadCounts;
           Leave;
         Endif;
         If curDate = 0;
           curDate = CMcdat;
         Endif;
         If curDate <> CMcdat;
           // Record break process
           Exsr LoadCounts;
         EndIf;
         curCount = curCount + 1;
       EndDo;
       //*** Begin Add *** 02/04/15 *********************************
       // Establish Visit Counts
       //
       curDate = 0;
       curCount = 0;
       Setll (UPco#:curUser:startDate) pvi04;
       DoU %Eof(wmvil4);
         Reade(n) (UPco#:curUser) pvi04;
         If %Eof(wmvil4) Or VIvsDt > endDate;
           Exsr LoadVstCounts;
           Leave;
         Endif;
         IF VIstat = 'A';
            If curDate = 0;
              curDate = VIvsDt;
            Endif;
            If curDate <> VIvsDt;
              // Record break process
              Exsr LoadVstCounts;
            EndIf;
            curCount = curCount + 1;
         ENDIF;
       EndDo;
       //*** End   Add *** 02/04/15 *********************************
       EndSr;
       //**************************************************************
       // Load Count to arrays
       //**************************************************************
       BegSr LoadCounts;
         If curCount = 0;
           LeaveSr;
         Endif;
         arrMthCount(curDD) += curCount;
         curDate = CMcdat;
         curCount = 0;
       EndSr;
       //**************************************************************
       // LoadCountsToJSON
       //**************************************************************
       BegSr LoadCountsToJSON;
         curDate = startDate;
         For curDD = 1 to 31;
           If arrMthCount(curDD) > 0;
             curCount =  arrMthCount(curDD);
             Exsr ProcessDate;
           Endif;
         EndFor;
       EndSr;
       //*** Begin Add *** 02/09/15 **********************************
       //**************************************************************
       // Load Visit Count to arrays
       //**************************************************************
       BegSr LoadVstCounts;
         If curCount = 0;
           LeaveSr;
         Endif;
         arrMthCountV(curDD) += curCount;
         curDate = VIvsDt;
         curCount = 0;
       EndSr;
       //**************************************************************
       // LoadVstCntToJSON
       //**************************************************************
       BegSr LoadVstCntToJSON;
         curDate = startDate;
         For curDD = 1 to 31;
           If arrMthCountV(curDD) > 0;
             curCount =  arrMthCountV(curDD);
             Exsr ProcessDate;
           Endif;
         EndFor;
       EndSr;
       //*** End   Add *** 02/09/15 **********************************
       //**************************************************************
       // Process Date
       //**************************************************************
       BegSr ProcessDate;
         StartJSONelemnt(outJSON);
         BuildJSON(outJSON :'date' :%EditW(dtMdyy(curDate):'  /  /    '));
         BuildJSON(outJSON :'items' :%Editc(curCount:'X'));
         EndJSONelemnt(outJSON);
       EndSr;
