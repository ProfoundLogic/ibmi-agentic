     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Return Fund - TAB 7.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLARINQ
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwar4p     if   e             disk    rename(pr401:pr40p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DPolicyInq02      pr                  ExtPgm('WTPOLARI02')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicyInq02      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d xxKey           s             10
     d paymentPlan     s             60
     d payeeType       s             30
     d custmerId       s             30
     d fndEmpDiv       s             30
     d empName         s            100
     d comment1        s             60
     d comment2        s             60
     d comment3        s             60
     d nameType        s              3
     d x#RRN           s              9  0

     d* Load Return Fund Detail.
     d LoadRtnFndDtl   pr

      *
      /Free

       Chain(n) inRRN pr40p;
       LoadRtnFndDtl();

       outJSON = ' ';

       // Populate Policy Period.
       BuildJSON(outJSON :'OutPayeeType' :%TrimR(payeeType));
       BuildJSON(outJSON :'OutCusID' :%TrimR(custmerId));
       BuildJSON(outJSON :'OutfndEmpDiv' :%TrimR(fndEmpDiv));
       BuildJSON(outJSON :'OutempName' :%TrimR(empName));
       BuildJSON(outJSON :'Outcomment1' :%TrimR(comment1));
       BuildJSON(outJSON :'Outcomment2' :%TrimR(comment2));
       BuildJSON(outJSON :'Outcomment3' :%TrimR(comment3));


       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Return Fund Detail
       //===================================================================
     P LoadRtnFndDtl   B
     D LoadRtnFndDtl   PI
      /free

       Select;
       When R4ptfl = 'E';
         payeeType = 'Employer';
       When R4ptfl = 'A';
         payeeType = 'Agent';
       When R4ptfl = 'G';
         payeeType = 'Group';
       When R4ptfl = 'P';
         payeeType = 'Premium Finance Company';
       When R4ptfl = ' ';
         payeeType = ' ';
       Endsl;

       custmerId = R4cust;
       fndEmpDiv = %editc(R4fnd:'X') + ' - ' + %editc(R4Emp#:'X') +
                     ' - ' + %editc(R4Div:'X');
       //  DBA Name
       nameType= 'DBA';
       empName  = GetEmpFullName(R4Emp#:R4Div:x#RRN:R4fnd:R4co#:nameType);

       comment1 = R4com1;
       comment2 = R4com2;
       comment3 = R4com3;

      /end-free
     P LoadRtnFndDtl   E




