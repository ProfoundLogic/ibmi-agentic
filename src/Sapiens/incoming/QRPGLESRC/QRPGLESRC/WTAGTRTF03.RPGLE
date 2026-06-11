     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h*
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Account Current Statement Detail
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTRTFND
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 01/24/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwdabp     if   e             disk    rename(pab01:pab0p)
     fwmfnl     if   e           k disk
     fwtabl     if   e           k disk    include(ptt01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr
     d/copy sPrString

      * *ENTRY
     DWtAgtRtf03       pr                  ExtPgm('WTAGTRTF03')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtRtf03       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *

     d xxKey           s             10
     d fndEmpDiv       s             30
     d empName         s            100
     d nameType        s              3
     d x#RRN           s              9  0
     d code            s             60

     d* Load Account Current Statement Detail info.
     d  LoadCurStmD    pr

     c     keyTable      klist
     c                   kfld                    ABco#
     c                   kfld                    xxKey
      *
      /Free
       Chain(n) inRRN pab0p;
       Chain (ABco#:ABfnd) pfn01;
       LoadCurStmD();

       outJSON = ' ';

       BuildJSON(outJSON :'OutARGroupNum' :%trim(%Char(ABfnd)));
       BuildJSON(outJSON :'OutARGroupName' :%trim(FNname));
       BuildJSON(outJSON :'OutARfndEmpDiv' :%Trim(fndEmpDiv));
       BuildJSON(outJSON :'OutARempName' :%Trim(empName));
       BuildJSON(outJSON :'OutARProducer' :%EditC(ABprod:'X'));
       BuildJSON(outJSON :'DtlOutARTrncode' :%Trim(code));
       BuildJSON(outJSON :'OutARcomment' :%Trim(ABcom1));

       BuildJSON(outJSON :'OutOpenItem'  :' ');
       If ABopen = 'Y';
          BuildJSON(outJSON :'OutOpenItem'  :'Yes');
       Endif;

       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Account Current Statement Detail info.
       //===================================================================
     P LoadCurStmD     B
     D LoadCurStmD     PI
      /free

       Clear xxKey;
       xxKey = ABarcd;
       Chain keyTable PTT01;
       Code = %trimr(xxKey) + ' - ' + TTdesc;

       // Fund, Employer, Division
       fndEmpDiv = %editc(ABfnd:'X') + ' - ' + %editc(ABEmp#:'X') +
                     ' - ' + %editc(ABDiv:'X');
       // DBA Name
       nameType= 'DBA';
       empName  = GetEmpFullName(ABEmp#:ABDiv:x#RRN:ABfnd:ABco#:nameType);

      /end-free
     P LoadCurStmD     E
