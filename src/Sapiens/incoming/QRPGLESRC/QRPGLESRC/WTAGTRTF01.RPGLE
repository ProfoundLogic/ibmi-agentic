     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Return Fund - TAB 7.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTRTFND
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 12/31/21                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwar4p     if   e             disk    rename(pr401:pr40p)
     fwmfnl     if   e           k disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DWtAgtRtf01       pr                  ExtPgm('WTAGTRTF01')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtRtf01       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d payeeType       s             30
     d fndEmpDiv       s             30
     d empName         s            100
     d nameType        s              3
     d x#RRN           s              9  0

     d* Load Return Fund Detail.
     d LoadRtnFndDtl   pr

      *
      /Free

       Chain inRRN pr40p;
       Chain (R4co#:R4fnd) pfn01;
       LoadRtnFndDtl();

       outJSON = ' ';

       // Populate Policy Period.
       BuildJSON(outJSON :'OutGroupNum' :%trim(%Char(R4fnd)));
       BuildJSON(outJSON :'OutGroupName' :%trim(FNname));
       BuildJSON(outJSON :'OutPolYear' :%EditC(R4fyr:'X'));
       BuildJSON(outJSON :'OutPayeeType' :%Trim(payeeType));
       If R4btyp <> 'A';
       BuildJSON(outJSON :'DtlLblCusID' :'Customer ID');
       BuildJSON(outJSON :'OutCusID' :%Trim(R4cust));
       Endif;
       If R4ptfl = 'P';
       BuildJSON(outJSON :'DtlLblPremFin' :'Premium Finance');
       BuildJSON(outJSON :'DtlOutPremFin' :%Trim(R4pfin));
       Endif;
       BuildJSON(outJSON :'OutfndEmpDiv' :%Trim(fndEmpDiv));
       BuildJSON(outJSON :'OutempName' :%Trim(empName));
       BuildJSON(outJSON :'Outcomment1' :%Trim(R4com1));
       BuildJSON(outJSON :'Outcomment2' :%Trim(R4com2));
       BuildJSON(outJSON :'Outcomment3' :%Trim(R4com3));


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

       fndEmpDiv = %editc(R4fnd:'X') + ' - ' + %editc(R4Emp#:'X') +
                     ' - ' + %editc(R4Div:'X');
       // DBA Name
       nameType= 'DBA';
       empName = GetEmpFullName(R4Emp#:R4Div:x#RRN:R4fnd:R4co#:nameType);

      /end-free
     P LoadRtnFndDtl   E




