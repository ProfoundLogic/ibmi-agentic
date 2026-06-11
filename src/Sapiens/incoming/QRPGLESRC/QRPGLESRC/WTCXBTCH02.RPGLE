     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Select box values for States List
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTCXBTCH
     h*  Calls to:
     h*
     h*  Created on 06/21/2023 by C:300 for Project WC2906
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Cancellation Batch File
     fwxccp     if   e             disk    rename(pcc01:pcc0s)
      * State Code File
     fststl5    if   e           k disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrString

      *entry
     DwtCxBtch02       pr                  ExtPgm('WTCXBTCH02')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DwtCxBtch02       pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     d psds          esds                  extname(szq1p)

     djson             s               a   len(8500000) varying
     d strStList       s           5000

      /Free
       Chain inRRN pcc0s;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       LoadSTcodes();

       outJSON = ' ';
       BuildGiantJSON(json :'Points1' :%trim(strStList));
       outJSON = %trim(json);

       RtnCode = ' ';
       Return;

      *//===================================================================
      *// LoadSTCodes - Load State codes dropdown
      *//===================================================================
     p LoadSTCodes     b
     d LoadSTCodes     pi
     d strSTdesc       s             40
     D i               s              3  0 inz
     D kkSt            s             10    inz
     D StateListDS     DS           150
     D   xxStList                   150
     D   AryStList                    2    DIM(75) OVERLAY(xxStList)
      /Free

       xxStList = ccStList;
       Clear strStList;
       Clear strSTdesc;

       If CCStInEx <> *blanks;
          For i = 1 to 75;
             kkst = AryStList(i);
             If kkst <> *blanks;
                chain kkSt PST01;
                If %found(STSTL5);
                   strSTdesc = TitleCase(STname);
                   If strStList = *Blanks;
                      strStList = %trim(STkey)+'-'+%trim(strSTdesc);
                   else;
                      strStList =%trim(strStList)+','+%trim(STkey)+'-'+
                              %trim(strSTdesc);
                   endif;
                endif;
             endif;
          Endfor;

       Else;
          Setll *loval STSTL5;
          Read PST01;
          Dow not %eof(STSTL5);
             strSTdesc = TitleCase(STname);
             If strStList = *Blanks;
                strStList = %trim(STkey)+'-'+%trim(strSTdesc);
             else;
                strStList=%trim(strStList)+','+%trim(STkey)+'-'+
                        %trim(strSTdesc);
             endif;
             Read PST01;
          Enddo;
       Endif;

      /End-Free

     p LoadSTCodes     e

      /End-Free
