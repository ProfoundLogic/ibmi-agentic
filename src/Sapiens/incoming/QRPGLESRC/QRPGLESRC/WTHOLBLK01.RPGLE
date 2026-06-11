     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('STBNDDIR')
     H**********************************************************************
     H/TITLE WTHOLBLK01 - AJAX Program to Change Staus of Blackout Dates   *
     H**********************************************************************
     H*
     H* PROGRAM DESCRIPTION:
     H*  This is the response program to update Blackout Date status (A/I).
     H*  Program Notes:
     H*    - Activation group should be *CALLER
     H*    - Do not seton LR, just do a return
     H*    - This process should not lock any file
     H*
     H*  Called from: WTHBLKDT
     H*  Calls to:
     H*
     H*  Created on 10/01/2024 by C:342 for WC2939
     H**********************************************************************
     H* Revisions:
     H*
     H*
     H**********************************************************************
     fwtcbp     uf   e             disk    rename(pcb01:pcb0p)

      /copy sprdate
      /copy wprusrprms
      /copy spratrium
      /copy wprtable

      *entry
     dOUTBLKDT         pr                  ExtPgm('WTHOLBLK01')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dOUTBLKDT         pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     d blkStat         s                   like(CBstat)
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d value           s             10    inz

     d psds          esds                  extname(szq1p)

       // input data is in format: ,A,
       // A = Active / I = Inactive

       Exsr ProcessGenparm;
       If rtnCode = 'X';
          rtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;
       Chain inRRN pcb0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If blkStat = 'A';
          CBstat = 'I';
       Else;
          CBstat = 'A';
       Endif;
       CBusr  = q1User;
       CBpgm  = q1Pgm;
       CBdat  = DateTo7(SystemDate);
       CBtm   = TimeTo6(SystemTime);

       Update pcb0p;

       BuildJSON(outJSON :'RtnCode' : CBdesc + ' updated.');

       rtnCode = ' ';
       Return;

       //=================================================================
       // Extract values from genparm
       //=================================================================
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
          endPos = %Scan(',':inGenParm:startPos);
          If endPos > 0;
             len = endPos - startPos;
             If len > 0;
                value = %Subst(inGenParm:startPos:len);
             Endif;
          Endif;
       Endif;
       EndSr;
       //=================================================================
       // Process Genparm
       //=================================================================
       Begsr ProcessGenparm;
       startPos = 0;
       endPos = 0;
       Monitor;
          // Extract Region Status
          startPos = %Scan(',':inGenParm)+1;
          If startPos <= 1;
             Leavesr;
          Endif;
          Exsr ExtractValue;
          blkStat = %trim(value);
          rtncode = ' ';
       On-Error;
          rtncode = 'X';
       EndMon;
       Endsr;
