     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Premium finance company
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPPOLCG
     h*  Calls to:
     h*
     h*  Created on 05/13/2020 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwmpfp     if   e             disk    rename(ppf01:ppf0s)
     fwtgtl     if   e           k disk

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpPol02       pr                  extpgm('WTGRPPOL02')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpPol02       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strAdd1         s             40
     d strAdd2         s             40
     d strAdd3         s             40
     d strPhnNum       s             14
     d strComType      s             53

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    PFco#
     C                   KFLD                    kktbid            4
     C                   KFLD                    kkkey            10


      /Free

       Chain inRRN ppf0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlFinCompCode' :%trim(PFfcod));
       BuildJSON(outJSON :'OutDtlCompName' :%trim(PFname));
       BuildJSON(outJSON :'OutDtlContactName':%trim(PFcont));
       BuildJSON(outJSON :'OutDtlCompAdd1' :%trim(strAdd1));
       BuildJSON(outJSON :'OutDtlCompAdd2' :%trim(strAdd2));
       BuildJSON(outJSON :'OutDtlCompAdd3' :%trim(strAdd3));
       BuildJSON(outJSON :'OutDtlPhnNum' :%trim(strPhnNum));
       BuildJSON(outJSON :'OutDtlCompEffDate' :
                         alphaUSA(cvtToDate(dtMDYY(PFefdt))));
       BuildJSON(outJSON :'OutDtlCompExpDate' :
                         alphaUSA(cvtToDate(dtMDYY(PFexdt))));
       BuildJSON(outJSON :'OutDtlCompanyType' :%trim(strComType));


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strAdd1 = %Trim(PFadr1);
       strAdd2 = %Trim(PFadr2);
       strAdd3 = %Trim(PFcty) + ', ' + PFst + ' ' +
        %Subst(%EditC(PFzip:'X'):1:5);
       If %Subst(%EditC(PFzip:'X'):6:4) <> *Blanks;
          strAdd3 = %Trim(strAdd3) + '-' +
           %Subst(%EditC(PFzip:'X'):6:4);
       EndIf;
       If strAdd2 = *Blanks;
          strAdd2 = strAdd3;
          strAdd3 = *Blanks;
       EndIf;
       If PFzip = *zeros;
          strAdd1 = *Blanks;
          strAdd2 = *Blanks;
          strAdd3 = *Blanks;
       EndIf;

       // Phone
       If PFarea <> *zeros;
          strPhnNum = '(' + %Subst(%EditC(PFarea:'X'):1:3) + ')';
       EndIf;
       If PFphn# <> *zeros;
          strPhnNum = %trim(strPhnNum) +' '+ %Subst(%EditC(PFphn#:'X'):1:3) +
          '-' + %Subst(%EditC(PFphn#:'X'):4:4);
       EndIf;

       // Premium Finace Company Type
       strComType = %trim(PFtype);
       IF PFtype <> *Blanks;
          kktbid = 'PFIN';
          kkkey =  PFtype;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat ='A';
             strComType = %trim(strComType) + ' - '  + GTdesc;
          EndIf;
       EndIf;


       endsr;
      /End-Free
