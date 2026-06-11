     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Office Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTOFFICE
     h*  Calls to:
     h*
     h*  Created on 10/19/2023 by C:342 for Project WC2940
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwmofp     if   e             disk    rename(pof01:pof0p)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtOffice01       pr                  extpgm('WTOFFICE01')
     d                                9  0
     d                            32767
     d                                1

     dwtOffice01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strOfcAdd1      s             40    inz(*blanks)
     d strOfcAdd2      s             40    inz(*blanks)
     d strOfcAdd3      s             40    inz(*blanks)
     d strOfcPhn       s             14    inz(*blanks)
     d strOfcFax       s             14    inz(*blanks)

     d strAccAdd1      s             40    inz(*blanks)
     d strAccAdd2      s             40    inz(*blanks)
     d strAccAdd3      s             40    inz(*blanks)
     d strAccPhn       s             14    inz(*blanks)
     d strAccFax       s             14    inz(*blanks)

     d strClmAdd1      s             40    inz(*blanks)
     d strClmAdd2      s             40    inz(*blanks)
     d strClmAdd3      s             40    inz(*blanks)
     d strClmPhn       s             14    inz(*blanks)

      /Free

       Chain inRRN pof0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'DtlOutOffice#' :%trim(OFofc#));
       BuildJSON(outJSON :'DtlOutOfficeMail' :%trim(OFemail));

       BuildJSON(outJSON :'DtlOutOfcName' :%trim(OFname));
       BuildJSON(outJSON :'DtlOutOfcAddress1' :%trim(strOfcAdd1));
       BuildJSON(outJSON :'DtlOutOfcAddress2' :%trim(strOfcAdd2));
       BuildJSON(outJSON :'DtlOutOfcAddress3' :%trim(strOfcAdd3));
       BuildJSON(outJSON :'DtlOutOfcPhnNum' :%trim(strOfcPhn));
       BuildJSON(outJSON :'DtlOutOfcFaxNum' :%trim(strOfcFax));


       BuildJSON(outJSON :'DtlOutAccName' :%trim(OFacnm));
       BuildJSON(outJSON :'DtlOutAccAddress1' :%trim(strAccAdd1));
       BuildJSON(outJSON :'DtlOutAccAddress2' :%trim(strAccAdd2));
       BuildJSON(outJSON :'DtlOutAccAddress3' :%trim(strAccAdd3));
       BuildJSON(outJSON :'DtlOutAccPhnNum' :%trim(strAccPhn));
       BuildJSON(outJSON :'DtlOutAccFaxNum' :%trim(strAccFax));


       BuildJSON(outJSON :'DtlOutClmName' :%trim(OFcnam));
       BuildJSON(outJSON :'DtlOutClmAddress1' :%trim(strClmAdd1));
       BuildJSON(outJSON :'DtlOutClmAddress2' :%trim(strClmAdd2));
       BuildJSON(outJSON :'DtlOutClmAddress3' :%trim(strClmAdd3));
       BuildJSON(outJSON :'DtlOutClmPhnNum' :%trim(strClmPhn));


       BuildJSON(outJSON :'DtlOutPremNotDir' :%trim(OFprnm));
       BuildJSON(outJSON :'DtlOutMthIntDir' :%trim(OFminm));
       BuildJSON(outJSON :'DtlOutLateNotDir' :%trim(OFlnnm));
       BuildJSON(outJSON :'DtlOutChkPayee' :%trim(OFcknm));
       BuildJSON(outJSON :'DtlOutEFTVerbi' :%trim(OFeftv));
       BuildJSON(outJSON :'DtlOut3rdPVerb' :%trim(OF3rdvrb));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Office Address
       strOfcAdd1 = %Trim(OFadr1);
       strOfcAdd2 = %Trim(OFadr2);
       strOfcAdd3 = %Trim(OFcty) + ', ' + OFst + ' ' +
        %Subst(%EditC(OFzip:'X'):1:5);
       If %Subst(%EditC(OFzip:'X'):6:4) <> *Blanks;
          strOfcAdd3 = %Trim(strOfcAdd3) + '-' +
           %Subst(%EditC(OFZip:'X'):6:4);
       EndIf;
       If strOfcAdd2 = *Blanks;
          strOfcAdd2 = strOfcAdd3;
          strOfcAdd3 = *Blanks;
       EndIf;
       If OFzip = *zeros;
          strOfcAdd1 = *Blanks;
          strOfcAdd2 = *Blanks;
          strOfcAdd3 = *Blanks;
       EndIf;

       // Phone Number
       If OFarea <> *zeros;
          strOfcPhn = '(' + %Subst(%EditC(OFarea:'X'):1:3) + ')';
       EndIf;
       If OFphn# <> *zeros;
          strOfcPhn = %trim(strOfcPhn) + ' ' + %Subst(%EditC(OFphn#:'X'):1:3)+
          '-' + %Subst(%EditC(OFphn#:'X'):4:4);
       EndIf;

       // Fax Number
       If OFfara <> *zeros;
          strOfcFax = '(' + %Subst(%EditC(OFfara:'X'):1:3) + ')';
       EndIf;
       If OFfax# <> *zeros;
          strOfcFax = %trim(strOfcFax) + ' ' + %Subst(%EditC(OFfax#:'X'):1:3)+
          '-' + %Subst(%EditC(OFfax#:'X'):4:4);
       EndIf;


       // Accounting Company Address
       strAccAdd1 = %Trim(OFaad1);
       strAccAdd2 = %Trim(OFaad2);
       strAccAdd3 = %Trim(OFacty) + ', ' + OFast + ' ' +
        %Subst(%EditC(OFazip:'X'):1:5);
       If %Subst(%EditC(OFazip:'X'):6:4) <> *Blanks;
          strAccAdd3 = %Trim(strAccAdd3) + '-' +
           %Subst(%EditC(OFazip:'X'):6:4);
       EndIf;
       If strAccAdd2 = *Blanks;
          strAccAdd2 = strAccAdd3;
          strAccAdd3 = *Blanks;
       EndIf;
       If OFazip = *zeros;
          strAccAdd1 = *Blanks;
          strAccAdd2 = *Blanks;
          strAccAdd3 = *Blanks;
       EndIf;

       // Phone Number
       If OFaara <> *zeros;
          strAccPhn = '(' + %Subst(%EditC(OFaara:'X'):1:3) + ')';
       EndIf;
       If OFaphn <> *zeros;
          strAccPhn = %trim(strAccPhn) + ' ' + %Subst(%EditC(OFaphn:'X'):1:3)+
          '-' + %Subst(%EditC(OFaphn:'X'):4:4);
       EndIf;

       // Fax Number
       If OFafxa <> *zeros;
          strAccFax = '(' + %Subst(%EditC(OFafxa:'X'):1:3) + ')';
       EndIf;
       If OFafx# <> *zeros;
          strAccFax = %trim(strAccFax) + ' ' + %Subst(%EditC(OFafx#:'X'):1:3)+
          '-' + %Subst(%EditC(OFafx#:'X'):4:4);
       EndIf;

       // Claim Office Address
       strClmAdd1 = %Trim(OFcad1);
       strClmAdd2 = %Trim(OFcad2);
       strClmAdd3 = %Trim(OFccty) + ', ' + OFcst + ' ' +
        %Subst(%EditC(OFczp:'X'):1:5);
       If %Subst(%EditC(OFczp:'X'):6:4) <> *Blanks;
          strClmAdd3 = %Trim(strClmAdd3) + '-' +
           %Subst(%EditC(OFczp:'X'):6:4);
       EndIf;
       If strClmAdd2 = *Blanks;
          strClmAdd2 = strClmAdd3;
          strClmAdd3 = *Blanks;
       EndIf;
       If OFczp = *zeros;
          strClmAdd1 = *Blanks;
          strClmAdd2 = *Blanks;
          strClmAdd3 = *Blanks;
       EndIf;

       // Phone Number
       If OFcara <> *zeros;
          strClmPhn = '(' + %Subst(%EditC(OFcara:'X'):1:3) + ')';
       EndIf;
       If OFcph1 <> *zeros or OFcph2 <> *zeros;
          strClmPhn = %trim(strClmPhn) + ' ' + %Subst(%EditC(OFcph1:'X'):1:3)+
          '-' + %Subst(%EditC(OFcph2:'X'):1:4);
       EndIf;

       endsr;
      /End-Free
