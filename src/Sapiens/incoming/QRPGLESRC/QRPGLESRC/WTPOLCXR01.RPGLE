     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Cancellation History Detail box.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLCXRN
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwdcxp     if   e             disk    rename(pcx01:pcx0p)
     fwdxhl3    if   e           k disk    rename(pxh01:pxh03)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DPolicyCxRn01     pr                  ExtPgm('PolicyCxRn01')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicyCxRn01     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d userName        s             60
     d revised         s             10

     c     keyWDXHL3     klist
     c                   kfld                    CXco#
     c                   kfld                    CXfnd
     c                   kfld                    CXemp#
     c                   kfld                    CXdiv
     c                   kfld                    CXfyr
     c                   kfld                    CXreq#
      *
      /Free

       Chain(n) inRRN pcx0p;
       userName  = GetUsrName(CXuser);

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'OutComment1' :%TrimR(CXcom1));
       BuildJSON(outJSON :'OutComment2' :%TrimR(CXcom2));
       BuildJSON(outJSON :'OutReinsFee' :%editc(CXrfee:numEdtcde));
       BuildJSON(outJSON :'OutGenCrtHldCert' :%TrimR(CXccrt));
       BuildJSON(outJSON :'OutIssueUser' :%TrimR(userName));

       Setgt  keyWDXHL3 PXH03;
       Readpe keyWDXHL3 PXH03;
       If %Found;
          If XHRVFL = 'R';
             revised = 'Yes';
          Endif;
          BuildJSON(outJSON :'OutRevised' :%TrimR(revised));
       Endif;

       rtnCode = ' ';
       Return;
      /End-Free

