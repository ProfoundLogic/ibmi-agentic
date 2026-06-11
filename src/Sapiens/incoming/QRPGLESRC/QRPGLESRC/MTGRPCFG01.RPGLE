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
     h*  Called from: MTGRPCFG
     h*  Calls to:
     h*
     h*  Created on 06/10/2024 by C:342 for Project MC447
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     ffmfnp     if   e             disk    rename(pfn01:pfn0p)
     ffmf1l     if   e           k disk    rename(pf101:pf100)
     fststl     if   e           k disk    rename(pst01:pst00)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dmtGrpCfg01       pr                  extpgm('MRGRPCFG01')
     d                                9  0
     d                            32767
     d                                1

     dmtGrpCfg01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strMailAdd1     s             40    inz(*blanks)
     d strMailAdd2     s             40    inz(*blanks)
     d strMailAdd3     s             40    inz(*blanks)
     d strPhnNum1      s             14    inz(*blanks)
     d strPhnNum2      s             14    inz(*blanks)
     d strFaxNum       s             14    inz(*blanks)
     d strAdjSt        s             60    inz(*blanks)
     d strStatus       s             10    inz(*blanks)
     d kkst            s             10    inz(*blanks)

     d strPhyAdd1      s             40    inz(*blanks)
     d strPhyAdd2      s             40    inz(*blanks)
     d strPhyAdd3      s             40    inz(*blanks)

     d strAltrAdd1     s             40    inz(*blanks)
     d strAltrAdd2     s             40    inz(*blanks)
     d strAltrAdd3     s             40    inz(*blanks)
     d strAltrPhn      s             14    inz(*blanks)

      /Free

       Chain inRRN pfn0p;
       Chain (FNco#:FNsub#:FNfnd) pf100;
       Exsr BuildData;
       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlMailAdd1' :%trim(strMailAdd1));
       BuildJSON(outJSON :'OutDtlMailAdd2' :%trim(strMailAdd2));
       BuildJSON(outJSON :'OutDtlMailAdd3' :%trim(strMailAdd3));
       BuildJSON(outJSON :'OutDtlPhyAdd1' :%trim(strPhyAdd1));
       BuildJSON(outJSON :'OutDtlPhyAdd2' :%trim(strPhyAdd2));
       BuildJSON(outJSON :'OutDtlPhyAdd3' :%trim(strPhyAdd3));
       BuildJSON(outJSON :'OutDtlPhnNum1' :%trim(strPhnNum1));
       BuildJSON(outJSON :'OutDtlPhnNum2' :%trim(strPhnNum2));
       BuildJSON(outJSON :'OutDtlFaxNum' :%trim(strFaxNum));
       BuildJSON(outJSON :'OutDtlAdjState':%trim(strAdjSt));
       BuildJSON(outJSON :'OutDtlAltrName' :%trim(F1anme));
       BuildJSON(outJSON :'OutDtlAltrAdd1' :%trim(strAltrAdd1));
       BuildJSON(outJSON :'OutDtlAltrAdd2' :%trim(strAltrAdd2));
       BuildJSON(outJSON :'OutDtlAltrAdd3' :%trim(strAltrAdd3));
       BuildJSON(outJSON :'OutDtlAltrPhn' :%trim(strAltrPhn));
       BuildJSON(outJSON :'CbxUseAltAdd' :%trim(F1flg1));
       BuildJSON(outJSON :'OutDtlAltFedID':%trim(%Char(F1afei)));
       BuildJSON(outJSON :'CbxStandalone':%trim(F1outa));
       BuildJSON(outJSON :'CbxWCChkIss':%trim(F1fl04));
       BuildJSON(outJSON :'CbxTestGrp':%trim(FNtest));
       BuildJSON(outJSON :'CbxExcGrpFrBCEE':%trim(FNclelig));
       BuildJSON(outJSON :'CbxNWCGrp':%trim(FNnwcg));
       BuildJSON(outJSON :'OutDtlPymtLmt':%trim(%EditC(FNmpln:'X')));
       BuildJSON(outJSON :'OutDtlEOBCd':%trim(FNoEob));
       BuildJSON(outJSON :'DtlOutStatus':%trim(strStatus));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strMailAdd1 = %Trim(FNmad1);
       strMailAdd2 = %Trim(FNmad2);
       strMailAdd3 = %Trim(FNmcty) + ', ' + FNmst + ' ' +
        %Subst(%EditC(FNmzip:'X'):1:5);
       If %Subst(%EditC(FNmzip:'X'):6:4) <> *Blanks;
          strMailAdd3 = %Trim(strMailAdd3) + '-' +
           %Subst(%EditC(FNmzip:'X'):6:4);
       EndIf;
       If strMailAdd2 = *Blanks;
          strMailAdd2 = strMailAdd3;
          strMailAdd3 = *Blanks;
       EndIf;
       If FNmzip = *zeros;
          strMailAdd1 = *Blanks;
          strMailAdd2 = *Blanks;
          strMailAdd3 = *Blanks;
       EndIf;

       //Adjudication State
       kkst = FNast;
       chain kkst pst00;
       If %found(ststl);
          strAdjSt = %trim(STkey) + ' - ' + %trim(STname);
       Endif;

       //Status
       If FNstat <> 'I';
          strStatus = 'Active';
       Else;
          strStatus = 'Inactive';
       Endif;

       // Physical Address
       strPhyAdd1 = %Trim(FNpad1);
       strPhyAdd2 = %Trim(FNpad2);
       strPhyAdd3 = %Trim(FNpcty) + ', ' + FNpst + ' ' +
        %Subst(%EditC(FNpzip:'X'):1:5);
       If %Subst(%EditC(FNpzip:'X'):6:4) <> *Blanks;
          strPhyAdd3 = %Trim(strPhyAdd3) + '-' +
           %Subst(%EditC(FNpzip:'X'):6:4);
       EndIf;
       If strPhyAdd2 = *Blanks;
          strPhyAdd2 = strPhyAdd3;
          strPhyAdd3 = *Blanks;
       EndIf;
       If FNpzip = *zeros;
          strPhyAdd1 = *Blanks;
          strPhyAdd2 = *Blanks;
          strPhyAdd3 = *Blanks;
       EndIf;

       // Phone 1
       If FNara1 <> *zeros;
          strPhnNum1 = '(' + %Subst(%EditC(FNara1:'X'):1:3) + ')';
       EndIf;
       If FNph1 <> *zeros;
          strPhnNum1 = %trim(strPhnNum1) + ' ' + %Subst(%EditC(FNph1:'X'):1:3) +
          '-' + %Subst(%EditC(FNph1:'X'):4:4);
       EndIf;

       // Phone 2
       If FNarea <> *zeros;
          strPhnNum2 = '(' + %Subst(%EditC(FNarea:'X'):1:3) + ')';
       EndIf;
       If FNph2 <> *zeros;
          strPhnNum2 = %trim(strPhnNum2) + ' ' + %Subst(%EditC(FNph2:'X'):1:3) +
          '-' + %Subst(%EditC(FNph2:'X'):4:4);
       EndIf;

       // Fax Number
       If FNfara <> *zeros;
          strFaxNum = '(' + %Subst(%EditC(FNfara:'X'):1:3) + ')';
       EndIf;
       If FNfax <> *zeros;
          strFaxNum = %trim(strFaxNum) + ' ' + %Subst(%EditC(FNfax:'X'):1:3) +
          '-' + %Subst(%EditC(FNfax:'X'):4:4);
       EndIf;

       // Alternate Address
       strAltrAdd1 = %Trim(F1aad1);
       strAltrAdd2 = %Trim(F1aad2);
       strAltrAdd3 = %Trim(F1acty) + ', ' + F1ast + ' ' +
        %Subst(%EditC(F1azip:'X'):1:5);
       If %Subst(%EditC(F1azip:'X'):6:4) <> *Blanks;
          strAltrAdd3 = %Trim(strAltrAdd3) + '-' +
           %Subst(%EditC(F1azip:'X'):6:4);
       EndIf;
       If strAltrAdd2 = *Blanks;
          strAltrAdd2 = strAltrAdd3;
          strAltrAdd3 = *Blanks;
       EndIf;
       If F1azip = *zeros;
          strAltrAdd1 = *Blanks;
          strAltrAdd2 = *Blanks;
          strAltrAdd3 = *Blanks;
       EndIf;

       // Alt Phone
       If F1aara <> *zeros;
          strAltrPhn = '(' + %Subst(%EditC(F1aara:'X'):1:3) + ')';
       EndIf;
       If F1aphn<> *zeros;
          strAltrPhn = %trim(strAltrPhn) + ' ' + %Subst(%EditC(F1aphn:'X'):1:3)+
          '-' + %Subst(%EditC(F1aphn:'X'):4:4);
       EndIf;
       endsr;
      /End-Free
