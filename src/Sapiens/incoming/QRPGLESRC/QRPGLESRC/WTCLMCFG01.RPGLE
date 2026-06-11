     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for State Headings Grid of Claims Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMCFG
     h*  Calls to:
     h*
     h*  Created on 12/01/2014 by C:300 for Project 114WLF
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwdcwp     if   e             disk    rename(pcw01:pcw0s)

      /copy sPrAtrium

      * *ENTRY
     dwtClmCfg01       pr                  extpgm('WTCLMCFG01')
     d                                9  0
     d                            32767
     d                                1

     dwtClmCfg01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strState        s             10
     d strSts          s             10
     d strBFH1         s             40
     d strBFH2         s             40
     d strBFH3         s             40
     d strBFH4         s             40
     d strBFH5         s             40
     d strBFHP         s             40
     d strIFH1         s             40
     d strIFH2         s             40
     d strIFH3         s             40
     d strIFH4         s             40
     d strMFH1         s             40
     d strMFH2         s             40
     d strMFH3         s             40
     d strMFH4         s             40
     d strEFH1         s             40
     d strEFH2         s             40
     d strEFH3         s             40
     d strEFH4         s             40
     d strLFH1         s             40
     d strLFH2         s             40
     d strLFH3         s             40
     d strLFH4         s             40

      /Free

       Chain inRRN pcw0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlstate' :%trim(strState));
       BuildJSON(outJSON :'OutDtlStatus' :%trim(strSts));
       BuildJSON(outJSON :'OutDtlBHdg1'  :%trim(strBFH1));
       BuildJSON(outJSON :'OutDtlBHdg2'  :%trim(strBFH2));
       BuildJSON(outJSON :'OutDtlBHdg3'  :%trim(strBFH3));
       BuildJSON(outJSON :'OutDtlBHdg4'  :%trim(strBFH4));
       BuildJSON(outJSON :'OutDtlBHdg5'  :%trim(strBFH5));
       //If strBFHP <> *Blanks;
       //   BuildJSON(outJSON :'OutDtlBHdg6'  :('% ' + %trim(strBFHP)));
       //Endif;
       BuildJSON(outJSON :'OutDtlBHdg6'  :%trim(strBFHP));
       BuildJSON(outJSON :'OutDtlFHdg1'  :%trim(strIFH1));
       BuildJSON(outJSON :'OutDtlFHdg2'  :%trim(strIFH2));
       BuildJSON(outJSON :'OutDtlFHdg3'  :%trim(strIFH3));
       BuildJSON(outJSON :'OutDtlFHdg4'  :%trim(strIFH4));
       BuildJSON(outJSON :'OutDtlMHdg1'  :%trim(strMFH1));
       BuildJSON(outJSON :'OutDtlMHdg2'  :%trim(strMFH2));
       BuildJSON(outJSON :'OutDtlMHdg3'  :%trim(strMFH3));
       BuildJSON(outJSON :'OutDtlMHdg4'  :%trim(strMFH4));
       BuildJSON(outJSON :'OutDtlEHdg1'  :%trim(strEFH1));
       BuildJSON(outJSON :'OutDtlEHdg2'  :%trim(strEFH2));
       BuildJSON(outJSON :'OutDtlEHdg3'  :%trim(strEFH3));
       BuildJSON(outJSON :'OutDtlEHdg4'  :%trim(strEFH4));
       BuildJSON(outJSON :'OutDtlLHdg1'  :%trim(strLFH1));
       BuildJSON(outJSON :'OutDtlLHdg2'  :%trim(strLFH2));
       BuildJSON(outJSON :'OutDtlLHdg3'  :%trim(strLFH3));
       BuildJSON(outJSON :'OutDtlLHdg4'  :%trim(strLFH4));


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;


       // State
       strState = CWst;
       If strState = *blanks;
          strState = 'Default';
       EndIf;

       // Status
       If CWstat = 'A';
          strSts = 'Active';
       Else;
          strSts = 'Inactive';
       Endif;

       // Populate Indemnity Benefit Headings Fields
       strBFH1 = CWbfh1;
       strBFH2 = CWbfh2;
       strBFH3 = CWbfh3;
       strBFH4 = CWbfh4;
       strBFH5 = CWbfh5;
       strBFHP = CWbfhP;
       If strBFHP <> *Blanks;
       strBFHP = '% ' + %trim(strBFHP);
       Endif;

       // Populate Indemnity Flexible Headings Fields
       strIFH1 = CWifh1;
       strIFH2 = CWifh2;
       strIFH3 = CWifh3;
       strIFH4 = CWifh4;

       // Populate Medical Flexible Headings Fields
       strMFH1 = CWmfh1;
       strMFH2 = CWmfh2;
       strMFH3 = CWmfh3;
       strMFH4 = CWmfh4;

       // Populate Expense Flexible Headings Fields
       strEFH1 = CWefh1;
       strEFH2 = CWefh2;
       strEFH3 = CWefh3;
       strEFH4 = CWefh4;

       // Populate Legal Flexible Headings Fields
       strLFH1 = CWlfh1;
       strLFH2 = CWlfh2;
       strLFH3 = CWlfh3;
       strLFH4 = CWlfh4;

       endsr;
      /End-Free
