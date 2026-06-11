     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for MCO Group Codes
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPCLMCG
     h*  Calls to:
     h*
     h*  Created on 06/19/2020 by C:295 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwmmgp     if   e             disk    rename(pmg01:pmg0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpClm01       pr                  extpgm('WTGRPCLM01')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpClm01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strAdd1         s             40
     d strAdd2         s             40
     d strAdd3         s             40
     d strPhnNum       s             14

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*

      /Free

       Chain inRRN pmg0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlMcoCd' :%trim(MGmco));
       BuildJSON(outJSON :'OutDtlMcoEffDate':
                           alphaUSA(cvtToDate(dtMDYY(MGefdt))));
       BuildJSON(outJSON :'OutDtlMcoExpDate':
                           alphaUSA(cvtToDate(dtMDYY(MGexdt))));
       BuildJSON(outJSON :'OutDtlMcoName':%trim(MGname));
       BuildJSON(outJSON :'OutDtlMcoRep':%trim(MGrep));
       BuildJSON(outJSON :'OutDtlMcoAdd1':%trim(strAdd1));
       BuildJSON(outJSON :'OutDtlMcoAdd2':%trim(strAdd2));
       BuildJSON(outJSON :'OutDtlMcoAdd3':%trim(strAdd3));
       BuildJSON(outJSON :'OutDtlMcoPhn':%trim(strPhnNum));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strAdd1 = %Trim(MGadr1);
       strAdd2 = %Trim(MGadr2);
       strAdd3 = %Trim(MGcty) + ', ' + MGst + ' ' +
        %Subst(%EditC(MGzip:'X'):1:5);
       If %Subst(%EditC(MGzip:'X'):6:4) <> *Blanks;
          strAdd3 = %Trim(strAdd3) + '-' +
           %Subst(%EditC(MGzip:'X'):6:4);
       EndIf;
       If strAdd2 = *Blanks;
          strAdd2 = strAdd3;
          strAdd3 = *Blanks;
       EndIf;
       If MGzip = *zeros;
          strAdd1 = *Blanks;
          strAdd2 = *Blanks;
          strAdd3 = *Blanks;
       EndIf;

       // Phone
       If MGarea <> *zeros;
          strPhnNum = '(' + %Subst(%EditC(MGarea:'X'):1:3) + ')';
       EndIf;
       If MGphn# <> *zeros;
          strPhnNum = %trim(strPhnNum) +' '+ %Subst(%EditC(MGphn#:'X'):1:3) +
          '-' + %Subst(%EditC(MGphn#:'X'):4:4);
       EndIf;

       endsr;
      /End-Free
