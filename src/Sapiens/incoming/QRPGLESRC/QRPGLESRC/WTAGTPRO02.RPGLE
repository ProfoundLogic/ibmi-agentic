     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h*
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Non-Resident State Detail Box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTPROD
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 02/03/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmpsp     if   e             disk    rename(pps01:pps0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr
     d/copy sPrString

      * *ENTRY
     DWtAgtPro02       pr                  ExtPgm('WTAGTPRO02')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtPro02       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
      /Free
       Chain(n) inRRN pps0p;

       outJSON = ' ';

       BuildJSON(outJSON :'DtlCbxResident' :' ');
       BuildJSON(outJSON :'DtlCbxLicFlag' :PSlflg);
       BuildJSON(outJSON :'DtlLicType2' :%trim(PSltp2));
       BuildJSON(outJSON :'DtlLicType3' :%trim(PSltp3));
       BuildJSON(outJSON :'DtlLicType4' :%trim(PSltp4));
       BuildJSON(outJSON :'DtlLicType5' :%trim(PSltp5));
       BuildJSON(outJSON :'DtlOutComment1' :%Trim(PScom1));
       BuildJSON(outJSON :'DtlOutComment2' :%Trim(PScom2));
       BuildJSON(outJSON :'DtlOutFileWCarrDate' :
                         alphaUSA(cvtToDate(dtMDYY(PSfidt))));
       BuildJSON(outJSON :'DtlOutMailToStDate' :
                         alphaUSA(cvtToDate(dtMDYY(PSmldt))));
       BuildJSON(outJSON :'DtlOutConfirmedDate' :
                         alphaUSA(cvtToDate(dtMDYY(PScfdt))));
       BuildJSON(outJSON :'DtlOutDeniedDate' :
                         alphaUSA(cvtToDate(dtMDYY(PSdndt))));

       rtnCode = ' ';
       Return;
      /End-Free
