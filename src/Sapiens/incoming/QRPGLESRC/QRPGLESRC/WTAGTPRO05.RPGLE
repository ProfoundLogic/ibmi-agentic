     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h*
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency Producers-Resident State Detail Box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTPROD
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 02/10/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmapp     if   e             disk    rename(pap01:pap0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr
     d/copy sPrString

      * *ENTRY
     DWtAgtPro05       pr                  ExtPgm('WTAGTPRO05')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtPro05       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
      /Free
       Chain(n) inRRN pap0p;

       outJSON = ' ';
       BuildJSON(outJSON :'DtlCbxResident' :'Y');
       BuildJSON(outJSON :'DtlOutProdNPNNum' :%trim(%char(APnpn)));
       BuildJSON(outJSON :'DtlOutLicType2' :%trim(APltp2));
       BuildJSON(outJSON :'DtlOutLicType3' :%trim(APltp3));
       BuildJSON(outJSON :'DtlOutLicType4' :%trim(APltp4));
       BuildJSON(outJSON :'DtlOutLicType5' :%trim(APltp5));
       BuildJSON(outJSON :'DtlOutComment1' :%Trim(APcomm));
       BuildJSON(outJSON :'DtlOutComment2' :%Trim(APcom2));
       BuildJSON(outJSON :'DtlOutFileWCarrDate' :
                         alphaUSA(cvtToDate(dtMDYY(APfidt))));
       BuildJSON(outJSON :'DtlOutMailToStDate' :
                         alphaUSA(cvtToDate(dtMDYY(APmldt))));
       BuildJSON(outJSON :'DtlOutConfirmedDate' :
                         alphaUSA(cvtToDate(dtMDYY(APcfdt))));
       BuildJSON(outJSON :'DtlOutDeniedDate' :
                         alphaUSA(cvtToDate(dtMDYY(APdndt))));

       rtnCode = ' ';
       Return;
      /End-Free
