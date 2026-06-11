     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Office Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: MTGRPEMP
     h*  Calls to:
     h*
     h*  Created on 06/10/2024 by C:342 for Project MC447
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     ffmemp     if   e             disk    rename(pem01:pem0p)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dmtGrpEmp01       pr                  extpgm('MRGRPEMP01')
     d                                9  0
     d                            32767
     d                                1

     dmtGrpEmp01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strMailAdd1     s             40    inz(*blanks)
     d strMailAdd2     s             40    inz(*blanks)
     d strMailAdd3     s             40    inz(*blanks)
     d strPhnNum       s             14    inz(*blanks)
     d strFaxNum       s             14    inz(*blanks)
     d strPhyAdd1      s             40    inz(*blanks)
     d strPhyAdd2      s             40    inz(*blanks)
     d strPhyAdd3      s             40    inz(*blanks)

      /Free

       Chain inRRN pem0p;
       Exsr BuildData;
       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlMailAdd1' :%trim(strMailAdd1));
       BuildJSON(outJSON :'OutDtlMailAdd2' :%trim(strMailAdd2));
       BuildJSON(outJSON :'OutDtlMailAdd3' :%trim(strMailAdd3));
       BuildJSON(outJSON :'OutDtlPhyAdd1' :%trim(strPhyAdd1));
       BuildJSON(outJSON :'OutDtlPhyAdd2' :%trim(strPhyAdd2));
       BuildJSON(outJSON :'OutDtlPhyAdd3' :%trim(strPhyAdd3));
       BuildJSON(outJSON :'OutDtlPhnNum' :%trim(strPhnNum));
       BuildJSON(outJSON :'OutDtlFaxNum' :%trim(strFaxNum));
       BuildJSON(outJSON :'DtlOutGroup':%trim(%Char(EMfnd)));
       BuildJSON(outJSON :'DtlOutBiz':%trim(EMdba));
       BuildJSON(outJSON :'OutDtlFedID':%trim(%Char(EMfein)));
       BuildJSON(outJSON :'DtlOutBizNature':%trim(EMnob));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strMailAdd1 = %Trim(EMmad1);
       strMailAdd2 = %Trim(EMmad2);
       strMailAdd3 = %Trim(EMmcty) + ', ' + EMmst + ' ' +
        %Subst(%EditC(EMmzip:'X'):1:5);
       If %Subst(%EditC(EMmzip:'X'):6:4) <> *Blanks;
          strMailAdd3 = %Trim(strMailAdd3) + '-' +
           %Subst(%EditC(EMmzip:'X'):6:4);
       EndIf;
       If strMailAdd2 = *Blanks;
          strMailAdd2 = strMailAdd3;
          strMailAdd3 = *Blanks;
       EndIf;
       If EMmzip = *zeros;
          strMailAdd1 = *Blanks;
          strMailAdd2 = *Blanks;
          strMailAdd3 = *Blanks;
       EndIf;

       // Physical Address
       strPhyAdd1 = %Trim(EMpad1);
       strPhyAdd2 = %Trim(EMpad2);
       strPhyAdd3 = %Trim(EMpcty) + ', ' + EMpst + ' ' +
        %Subst(%EditC(EMpzip:'X'):1:5);
       If %Subst(%EditC(EMpzip:'X'):6:4) <> *Blanks;
          strPhyAdd3 = %Trim(strPhyAdd3) + '-' +
           %Subst(%EditC(EMpzip:'X'):6:4);
       EndIf;
       If strPhyAdd2 = *Blanks;
          strPhyAdd2 = strPhyAdd3;
          strPhyAdd3 = *Blanks;
       EndIf;
       If EMpzip = *zeros;
          strPhyAdd1 = *Blanks;
          strPhyAdd2 = *Blanks;
          strPhyAdd3 = *Blanks;
       EndIf;

       // Phone 1
       If EMarea <> *zeros;
          strPhnNum = '(' + %Subst(%EditC(EMarea:'X'):1:3) + ')';
       EndIf;
       If EMphn# <> *zeros;
          strPhnNum = %trim(strPhnNum) + ' ' + %Subst(%EditC(EMphn#:'X'):1:3) +
          '-' + %Subst(%EditC(EMphn#:'X'):4:4);
       EndIf;

       // Fax Number
       If EMfara <> *zeros;
          strFaxNum = '(' + %Subst(%EditC(EMfara:'X'):1:3) + ')';
       EndIf;
       If EMfax# <> *zeros;
          strFaxNum = %trim(strFaxNum) + ' ' + %Subst(%EditC(EMfax#:'X'):1:3) +
          '-' + %Subst(%EditC(EMfax#:'X'):4:4);
       EndIf;
       endsr;
      /End-Free
