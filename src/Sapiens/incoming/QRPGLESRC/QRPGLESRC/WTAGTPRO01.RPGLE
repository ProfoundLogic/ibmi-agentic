     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h*
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency Producers Detail Box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTPROD
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 02/08/22                                         *
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
     d/copy sPrMask

      * *ENTRY
     DWtAgtPro01       pr                  ExtPgm('WTAGTPRO01')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtPro01       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1

     d strAdd1         s             40
     d strAdd2         s             40
     d charSS          s              9
     d charSS#         s              9
      *
      /Free
       Chain(n) inRRN pap0p;

       Exsr BuildData;

       outJSON = ' ';

       BuildJSON(outJSON :'OutDtlEmailAddr' :%trim(APemal));
       BuildJSON(outJSON :'OutDtlProdAdd1' :%trim(strAdd1));
       BuildJSON(outJSON :'OutDtlProdAdd2' :%trim(strAdd2));
       // Social Security Number
       If APss# <> 0;
          clear charSS#;
          charSS  = %EditC(APss#:'X');
          charSS# = MaskData('SSN':'APss#':charSS);
          BuildJSON(outJSON :'OutDtlProdSSN' :%trim(charSS#));
       Endif;
       BuildJSON(outJSON :'OutDtlProdDOB' :
                         alphaUSA(cvtToDate(dtMDYY(APdob))));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        // Mailing Address
        strAdd1 = %Trim(APmad1);
        strAdd2 = %Trim(APmcty) + ', ' + APmst + ' ' +
         %Subst(%EditC(APmzip:'X'):1:5);
        If %Subst(%EditC(APmzip:'X'):6:4) <> *Blanks;
           strAdd2 = %Trim(strAdd2) + '-' +
            %Subst(%EditC(APmzip:'X'):6:4);
        EndIf;
        If APmzip = *zeros;
           strAdd1 = *Blanks;
           strAdd2 = *Blanks;
        EndIf;

       endsr;
      /End-Free
