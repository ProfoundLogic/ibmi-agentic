     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Agent Rate Master Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPDTA
     h*  Calls to:
     h*
     h*  Created on 05/29/2020 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwdf3p     if   e             disk    rename(pf301:pf30s)
     fssylf     if   e           k disk
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpDat02       pr                  extpgm('WTGRPDAT02')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpDat02       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strAdd1         s             40
     d strAdd2         s             40
     d strAdd3         s             40
     d strPhnNum       s             14    Inz
     d strFaxNum       s             14    Inz
     d strAltZip       s             10


      /Free

       Chain inRRN pf30s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlKeyState':%trim(F3kst));
       BuildJSON(outJSON :'OutDtlClmAdmFEIN':%trim(%char(F3fein)));
       BuildJSON(outJSON :'OutDtlClmAdmName':%trim(F3name));
       BuildJSON(outJSON :'OutDtlClmAdmAddr1' :%trim(strAdd1));
       BuildJSON(outJSON :'OutDtlClmAdmAddr2' :%trim(strAdd2));
       BuildJSON(outJSON :'OutDtlClmAdmAddr3' :%trim(strAdd3));
       If F3ph# <> *Blanks and %dec(F3ph#:15:0) <> *Zeros;
          BuildJSON(outJSON :'OutDtlClmAdmPhn' :%trim(strPhnNum));
       Else;
          BuildJSON(outJSON :'OutDtlClmAdmPhn' :*Blanks);
       Endif;
       If F3fax <> *Blanks and %dec(F3fax:10:0) <> *Zeros;
          BuildJSON(outJSON :'OutDtlClmAdmFax' :%trim(strFaxNum));
       Else;
          BuildJSON(outJSON :'OutDtlClmAdmFax' :*Blanks);
       Endif;
       If F3azp <> *Zeros;
          BuildJSON(outJSON :'OutDtlClmAdmAltZip' :%trim(strAltZip));
       Else;
          BuildJSON(outJSON :'OutDtlClmAdmAltZip' :*Blanks);
       Endif;
       BuildJSON(outJSON :'OutDtlClmAdmAttention':%trim(F3info));
       BuildJSON(outJSON :'OutDtlInsurerFEIN':%trim(%char(F3ifei)));
       BuildJSON(outJSON :'OutDtlInsurerName':%trim(F3inam));
       BuildJSON(outJSON :'OutDtlInsurerCode':%trim(F3icod));
       BuildJSON(outJSON :'OutDtlTPACode':%trim(F3tcod));
       BuildJSON(outJSON :'OutDtlEDIstartOverride':
                         alphaUSA(cvtToDate(dtMDYY(F3ovdt))));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strAdd1 = %Trim(F3adr1);
       strAdd2 = %Trim(F3adr2);
       strAdd3 = %Trim(F3cty) + ', ' + F3st + ' ' +
        %Subst(%EditC(F3zip:'X'):1:5);
       If %Subst(%EditC(F3zip:'X'):6:4) <> *Blanks;
          strAdd3 = %Trim(strAdd3) + '-' +
           %Subst(%EditC(F3zip:'X'):6:4);
       EndIf;
       If strAdd2 = *Blanks;
          strAdd2 = strAdd3;
          strAdd3 = *Blanks;
       EndIf;
       If F3zip = *zeros;
          strAdd1 = *Blanks;
          strAdd2 = *Blanks;
          strAdd3 = *Blanks;
       EndIf;

       // Phone
        strPhnNum = '(' + %Subst(F3ph#:1:3) + ')';
        strPhnNum = %trim(strPhnNum) +' '+ %Subst(F3ph#:4:3) +
         '-' + %Subst(F3ph#:7:4);
       // Fax
        strFaxNum = '(' + %Subst(F3fax:1:3) + ')';
        strFaxNum = %trim(strFaxNum) +' '+ %Subst(F3fax:4:3) +
        '-' + %Subst(F3fax:7:4);
       // Alt Zip
       StrAltZip = %Subst(%EditC(F3azp:'X'):1:5);
       If %Subst(%EditC(F3azp:'X'):6:4) <> *Blanks;
           StrAltZip = %Trim(StrAltZip) + '-' +
           %Subst(%EditC(F3azp:'X'):6:4);
       EndIf;


       endsr;
      /End-Free
