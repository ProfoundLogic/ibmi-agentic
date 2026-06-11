     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Agency Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTCFG
     h*  Calls to:
     h*
     h*  Created on 30/08/2021 by C:300 for Project WC2395
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwmagp     if   e             disk    rename(pag01:pag0s)
     fsmupl1    if   e           k disk
     fwtabl     if   e           k disk    include(pat01)                       WTATP
     f                                     include(pcn01)                       WTCNP
     fwtgtl     if   e           k disk
     fwtagl     if   e           k disk
     fwmael     if   e           k disk

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtAgtCfg01       pr                  extpgm('WTAgtCFG01')
     d                                9  0
     d                            32767
     d                                1

     dwtAgtCfg01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d clientID        s              3    inz('000')
     d visbC114        s               n   inz(*off)
     d strMailAdd1     s             40
     d strMailAdd2     s             40
     d strPhyAdd1      s             40
     d strPhyAdd2      s             40
     d strPhnNum1      s             14
     d strPhnNum2      s             14
     d strFaxNum       s             14
     d strSrvRep       s             60
     d strMktRep       s             60
     d strMktReg       s             55
     d strAgtTypCd     s             55
     d strAgtGrpCd     s             55
     d strCountyCd     s             45
     d strAreaCd       s             40

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    AGco#
     C                   KFLD                    kktbid            4
     C                   KFLD                    kkkey            10


      /Free

       Chain inRRN pag0s;

       Exsr BuildData;

       GetClientID(clientID);
       visbC114 = *off;
       If clientID = '114';
          visbC114 = *on;
       Endif;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlAgencyName' :%trim(AGname));
       BuildJSON(outJSON :'OutDtlFedName' :%trim(AGfdnm));
       BuildJSON(outJSON :'OutDtlAgencyFEIN' :%trim(%Char(AGfein)));
       BuildJSON(outJSON :'OutDtlAgencyModifier' :%trim(%Char(AGmod)));
       BuildJSON(outJSON :'OutDtlShortName' :%trim(AGshnm));
       BuildJSON(outJSON :'OutDtlMailAdd1' :%trim(strMailAdd1));
       BuildJSON(outJSON :'OutDtlMailAdd2' :%trim(strMailAdd2));
       BuildJSON(outJSON :'OutDtlPhyAdd1' :%trim(strPhyAdd1));
       BuildJSON(outJSON :'OutDtlPhyAdd2' :%trim(strPhyAdd2));
       BuildJSON(outJSON :'OutDtlPhnNum1' :%trim(strPhnNum1));
       BuildJSON(outJSON :'OutDtlPhnNum2' :%trim(strPhnNum2));
       BuildJSON(outJSON :'OutDtlFaxNum' :%trim(strFaxNum));
       BuildJSON(outJSON :'OutDtlIntAdd' :%trim(AGiadr));
       BuildJSON(outJSON :'OutDtlEmailadd' :%trim(AGemal));
       BuildJSON(outJSON :'OutDtlAgencyPhone' :%trim(strPhnNum1));
       BuildJSON(outJSON :'OutDtlAgencyPhone2' :%trim(strPhnNum2));
       BuildJSON(outJSON :'OutDtlFaxNumber' :%trim(strFaxNum));
       BuildJSON(outJSON :'OutDtlContact' :%trim(AGhcon));
       BuildJSON(outJSON :'OutDtlServRep' :%trim(strSrvRep));
       BuildJSON(outJSON :'OutDtlMarketRep' :%trim(strMktRep));
       If visbC114 = *off;
          BuildJSON(outJSON :'OutDtlSecretaryName' :%trim(AGsecy));
          BuildJSON(outJSON :'OutDtlMarketContact' :%trim(AGcont));
       EndIf;
       BuildJSON(outJSON :'OutDtlMarketRegion' :%trim(strMktReg));
       BuildJSON(outJSON :'OutDtlComments1' :%trim(AGcomm));
       BuildJSON(outJSON :'OutDtlComments2' :%trim(AGcom2));
       BuildJSON(outJSON :'OutDtlAgtTypCode' :%trim(strAgtTypCd));
       BuildJSON(outJSON :'OutAgtGrpCode' :%trim(strAgtGrpCd));
       BuildJSON(outJSON :'OutDtlCountyCode' :%trim(strCountyCd));
       BuildJSON(outJSON :'OutDtlAreaCode' :%trim(strAreaCd));
       BuildJSON(outJSON :'OutDtlW9SentDate' :
                         alphaUSA(cvtToDate(dtMDYY(AGw9se))));
       BuildJSON(outJSON :'OutDtlW9ReceivDate' :
                         alphaUSA(cvtToDate(dtMDYY(AGw9rc))));
       BuildJSON(outJSON :'OutDtlAccCurDueDate' :%trim(%Char(AGacdu)));
       BuildJSON(outJSON :'OutDtl1099MailtoMod' :%trim(%Char(AG109m)));
       BuildJSON(outJSON :'OutDtlChecksMailtoMod' :%trim(%Char(AGcckm)));
       BuildJSON(outJSON :'OutCbxBackupWithhold' :AGbkup);
       BuildJSON(outJSON :'OutCbxSplAgtProcess' :AGspa);
       BuildJSON(outJSON :'OutCbx1099Flag' :AG1099);
       BuildJSON(outJSON :'OutCbxSS#' :AGss#);
       BuildJSON(outJSON :'OutCbxExclExport' :AGeexp);
       BuildJSON(outJSON :'OutDtlBkChkCompDate' :
                         alphaUSA(cvtToDate(dtMDYY(AGbkch))));
       If visbC114 = *on;
          BuildJSON(outJSON :'OutCbxEOCov' :AGsecy);
          BuildJSON(outJSON :'OutDtlEOCovExpDate' :%trim(AGcont));
       EndIf;

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Mailing Address
       strMailAdd1 = %Trim(AGmad1);
       strMailAdd2 = %Trim(AGmcty) + ', ' + AGmst + ' ' +
        %Subst(%EditC(AGmzip:'X'):1:5);
       If %Subst(%EditC(AGmzip:'X'):6:4) <> *Blanks;
          strMailAdd2 = %Trim(strMailAdd2) + '-' +
           %Subst(%EditC(AGmzip:'X'):6:4);
       EndIf;
       If AGmzip = *zeros;
          strMailAdd1 = *Blanks;
          strMailAdd2 = *Blanks;
       EndIf;

       // Physical Address
       strPhyAdd1 = %Trim(AGpad1);
       strPhyAdd2 = %Trim(AGpcty) + ', ' + AGpst + ' ' +
        %Subst(%EditC(AGpzip:'X'):1:5);
       If %Subst(%EditC(AGpzip:'X'):6:4) <> *Blanks;
          strPhyAdd2 = %Trim(strPhyAdd2) + '-' +
           %Subst(%EditC(AGpzip:'X'):6:4);
       EndIf;
       If AGpzip = *zeros;
          strPhyAdd1 = *Blanks;
          strPhyAdd2 = *Blanks;
       EndIf;

       // Phone 1
       If AGare1 <> *zeros;
          strPhnNum1 = '(' + %Subst(%EditC(AGare1:'X'):1:3) + ')';
       EndIf;
       If AGphn1 <> *zeros;
          strPhnNum1 = %trim(strPhnNum1) + ' ' + %Subst(%EditC(AGphn1:'X'):1:3)+
          '-' + %Subst(%EditC(AGphn1:'X'):4:4);
       EndIf;

       // Phone 2
       If AGare2 <> *zeros;
          strPhnNum2 = '(' + %Subst(%EditC(AGare2:'X'):1:3) + ')';
       EndIf;
       If AGphn2 <> *zeros;
          strPhnNum2 = %trim(strPhnNum2) + ' ' + %Subst(%EditC(AGphn2:'X'):1:3)+
          '-' + %Subst(%EditC(AGphn2:'X'):4:4);
       EndIf;

       // Fax Number
       If AGfaxa <> *zeros;
          strFaxNum = '(' + %Subst(%EditC(AGfaxa:'X'):1:3) + ')';
       EndIf;
       If AGfax# <> *zeros;
          strFaxNum = %trim(strFaxNum) + ' ' + %Subst(%EditC(AGfax#:'X'):1:3)+
          '-' + %Subst(%EditC(AGfax#:'X'):4:4);
       EndIf;

       // Get Services Representative Name from SMUPL1
       strSrvRep = AGsvcr;
       If AGsvcr <> *Blanks;
          Chain AGsvcr pup01;
          IF %found(smupl1);
             strSrvRep = %trim(strSrvRep) + ' - '  + UPusnm;
          EndIf;
       EndIf;

       // Get Marketing Representative Name from SMUPL1
       strMktRep = AGmkrp;
       If AGmkrp <> *Blanks;
          Chain AGmkrp pup01;
          IF %found(smupl1);
             strMktRep = %trim(strMktRep) + ' - '  + UPusnm;
          EndIf;
       EndIf;

       // Marketing Region Description
       strMktReg = AGmkrg;
       IF AGmkrg <> *Blanks;
          kktbid = 'AMKR';
          kkkey =  AGmkrg;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat = 'A';
             strMktReg = %trim(strMktReg) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // Agency Type Code Description
       strAgtTypCd = AGtpc2;
       IF AGtpc2 <> *Blanks;
          kkkey =  AGtpc2;
          Chain (AGco#:kkkey) pat01;
          IF %found and ATstat = 'A';
             strAgtTypCd = %trim(strAgtTypCd) + ' - '  + ATdesc;
          EndIf;
       EndIf;

       // Agency Group Code Description
       strAgtGrpCd = AGgrcd;
       IF AGgrcd <> *Blanks;
          kkkey =  AGgrcd;
          Chain (AGco#:kkkey) pag01;
          IF %found(WTAGL) and AGstat = 'A';
             strAgtGrpCd = %trim(strAgtGrpCd) + ' - '  + AGdesc;
          EndIf;
       EndIf;

       // County Code Description
       IF AGcnty <> *Blanks;
          strCountyCd = AGpst + AGcnty;
          IF AGpst = *Blanks;
             strCountyCd = AGmst + AGcnty;
          EndIf;
          kkkey = %trim(strCountyCd);
          Chain (AGco#:kkkey) pcn01;
          IF %found and CNstat = 'A';
             strCountyCd = %trim(strCountyCd) + ' - '  + CNdesc;
          EndIf;
       EndIf;

       // Area Code Description
       strAreaCd = %char(AGarea);
       IF AGarea <> 0;
          Chain (AGco#:AGarea) pae01;
          IF %found(WMAEL) and AEstat = 'A';
             strAreaCd = %trim(strAreaCd) + ' - '  + AEdesc;
          EndIf;
       EndIf;



       endsr;
      /End-Free
