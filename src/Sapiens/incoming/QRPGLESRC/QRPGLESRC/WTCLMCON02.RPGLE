     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for policy contacts
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMCON
     h*  Calls to:
     h*
     h*  Created on 03/04/2024 by C:342 for Project WC2826
     h**********************************************************************
     h* Revisions:
     h*
     h* 05/26/25  C:333 Project: WC3009
     h*           Clms, Policy, Agncy contacts: Maint Info
     h*           - Adding LastUpdate and LastUpdatedBy
     h*                                                                *
     h*
     h**********************************************************************
     fwmecp     if   e             disk    rename(pec01:pec0p)
     fwma5p     if   e             disk    rename(pac01:pac0p)
     fwmapp     if   e             disk    rename(pap01:pap0p)
     fsmupp     if   e             disk    rename(pup01:pup0p)
     fwmagl     if   e           k disk    rename(pag01:pag00)
       //*** Beg Add *** 05/26/25 *******************************************
     fsmupl     if   e           k Disk    ReName(pup01:pup0l)
       //*** End Add *** 05/26/25 *******************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtClmCon02       pr                  extpgm('WTCLMCON02')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dwtClmCon02       pi
     d inRRN                          9  0
     d inConfile                    512
     d outJSON                    32767
     d rtnCode                        1

     d intZip          s              9  0 inz(*zeros)
     d strPolAdd1      s             40    inz(*blanks)
     d strPolAdd2      s             40    inz(*blanks)
     d strPolAdd3      s             40    inz(*blanks)
     d strPolFax       s             15    inz(*blanks)
     d strEmail        s             60    inz(*blanks)
      ******** Beg Add **** 05/26/25 *************************
     D strLastUpd      s             10
     D strLastUpdBy    s             50
      ******** End Add **** 05/26/25 *************************

     d  ConFileDS      ds           512
     d  wkConFile              1     10
     d  wkFiller              11    512

      /copy sPrString

      /Free

       // determine which type of contact record to display
       ConFileDS = inConFile;
       If %trim(wkConfile) = 'WMECL';
          LoadEmpCon();
       Elseif %trim(wkConfile) = 'WMA5P';
          LoadAgtCon();
       Elseif %trim(wkConfile) = 'WMAPL';
          LoadProdCon();
       Elseif %trim(wkConfile) = 'SMUPL1';
          LoadUsrParm();
       Endif;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlPolAdrLn1' :%trim(strPolAdd1));
       BuildJSON(outJSON :'OutDtlPolAdrLn2' :%trim(strPolAdd2));
       BuildJSON(outJSON :'OutDtlPolAdrLn3' :%trim(strPolAdd3));
       BuildJSON(outJSON :'DtlOutDtlPolFax#' :%trim(strPolFax));
       BuildJSON(outJSON :'DtlOutPolEmailId' :%trim(strEmail));
       //*** Beg Add ***** 05/26/25 *****************************
       BuildJSON(outJSON :'DtlOutPLastUpdated' :%trim(strLastUpd));
       BuildJSON(outJSON :'DtlOutPLastUpdatedBy' :%trim(strLastUpdBy));
       //*** End Add ***** 05/26/25 *****************************

       rtnCode = ' ';
       Return;

       //===================================================================
       // LoadEmpCon - Load Employer Contact info detail box
       //===================================================================
     P LoadEmpCon      B
     D LoadEmpCon      PI
      /Free

       Chain inRRN pec0p;
       If %found(Wmecp);

          // Policy Contact Address
          strPolAdd1 = %Trim(ECadr1);
          strPolAdd2 = %Trim(ECadr2);
          intZip      = %int(ECzip);
          strPolAdd3 = %Trim(ECcty) + ', ' + ECst + ' ' +
           %Subst(%EditC(intZip:'X'):1:5);
          If %Subst(%EditC(intZip:'X'):6:4) <> *Blanks;
             strPolAdd3 = %Trim(strPolAdd3) + '-' +
              %Subst(%EditC(intZip:'X'):6:4);
          EndIf;
          If strPolAdd2 = *Blanks;
             strPolAdd2 = strPolAdd3;
             strPolAdd3 = *Blanks;
          EndIf;
          If ECzip = *zeros;
             strPolAdd1 = *Blanks;
             strPolAdd2 = *Blanks;
             strPolAdd3 = *Blanks;
          EndIf;

          // Fax Number
          If ECfara <> *zeros;
             strPolFax = '(' + %Subst(%EditC(ECfara:'X'):1:3) + ')';
          EndIf;
          If ECfphn <> *zeros;
             strPolFax = %trim(strPolFax) + ' ' + %Subst(%EditC(ECfphn:'X'):1:3)
             + '-' + %Subst(%EditC(ECfphn:'X'):4:4);
          EndIf;
          strEmail = %trim(ECemal);
          //*** Beg Add ***** 05/26/25 *************************
          strLastUpd = %Trim(%EditW(dtMDYY(ECdt):'  /  /    '));
          Chain ECuser pup0l;
          If %found(SMUPL);
             strLastUpdBy = UPusnm;
          Endif;
          //*** End Add ***** 05/26/25 *************************
       Endif;

      /End-Free

     P LoadEmpCon      E

       //===================================================================
       // LoadAgtCon - Load Agenct Contact info detail box
       //===================================================================
     P LoadAgtCon      B
     D LoadAgtCon      PI
      /Free

       Chain inRRN pac0p;
       If %found(Wma5p);

          // Agent Contact Address
          strPolAdd1 = %Trim(A5adr1);
          strPolAdd2 = %Trim(A5adr2);
          intZip      = %int(A5zip);
          strPolAdd3 = %Trim(A5cty) + ', ' + A5st + ' ' +
           %Subst(%EditC(intZip:'X'):1:5);
          If %Subst(%EditC(intZip:'X'):6:4) <> *Blanks;
             strPolAdd3 = %Trim(strPolAdd3) + '-' +
              %Subst(%EditC(intZip:'X'):6:4);
          EndIf;
          If strPolAdd2 = *Blanks;
             strPolAdd2 = strPolAdd3;
             strPolAdd3 = *Blanks;
          EndIf;
          If A5zip = *zeros;
             strPolAdd1 = *Blanks;
             strPolAdd2 = *Blanks;
             strPolAdd3 = *Blanks;
          EndIf;

          // Fax Number
          If A5fara <> *zeros;
             strPolFax = '(' + %Subst(%EditC(A5fara:'X'):1:3) + ')';
          EndIf;
          If A5fphn <> *zeros;
             strPolFax = %trim(strPolFax) + ' ' + %Subst(%EditC(A5fphn:'X'):1:3)
             + '-' + %Subst(%EditC(A5fphn:'X'):4:4);
          EndIf;
          strEmail = %trim(A5emal);
          //*** Beg Add ***** 05/26/25 *************************
          strLastUpd = %Trim(%EditW(dtMDYY(A5dt):'  /  /    '));
          Chain A5user pup0l;
          If %found(SMUPL);
             strLastUpdBy = UPusnm;
          Endif;
          //*** End Add ***** 05/26/25 *************************
       Endif;

      /End-Free

     P LoadAgtCon      E

       //===================================================================
       // LoadEmpCon - Load Producer Contact info detail box
       //===================================================================
     P LoadProdCon     B
     D LoadProdCon     PI
      /Free

       Chain inRRN pap0p;
       If %found(Wmapp);

           // contact address
           FormatAddress(APmad1:strPolAdd2:APmcty:APmst:APmzip:strPolAdd1
            :strPolAdd2:strPolAdd3);

           strEmail = %trim(APemal);

           // Phone#
           Chain (APco#:APfein:APmod) pag00;
           If %Found(wmagl);
              If AGfaxa <> *zeros;
                 strPolFax = '(' + %Subst(%EditC(AGfaxa:'X'):1:3) + ')';
              EndIf;
              If AGfax# <> *zeros;
                 strPolFax = %trim(strPolFax) + ' ' +
                 %Subst(%EditC(AGfax#:'X'):1:3)
                 + '-' + %Subst(%EditC(AGfax#:'X'):4:4);
              EndIf;
           EndIf;
          //*** Beg Add ***** 05/26/25 *************************
          strLastUpd = %Trim(%EditW(dtMDYY(APdt):'  /  /    '));
          Chain APuser pup0l;
          If %found(SMUPL);
             strLastUpdBy = UPusnm;
          Endif;
          //*** End Add ***** 05/26/25 *************************

       Endif;

      /End-Free

     P LoadProdCon     E

       //=========================================================================
       // LoadUsrParm- Load Internal Policy Contact from SMUPP User Parameter File
       //=========================================================================
     P LoadUsrParm     B
     D LoadUsrParm     PI
      /Free

       Chain inRRN pup0p;
       If %found(Smupp);

          // Agent Contact Address
          strPolAdd1 = %Trim(UPadr1);
          strPolAdd2 = %Trim(UPadr2);
          intZip      = %int(UPzip);
          strPolAdd3 = %Trim(UPcty) + ', ' + UPst + ' ' +
           %Subst(%EditC(intZip:'X'):1:5);
          If %Subst(%EditC(intZip:'X'):6:4) <> *Blanks;
             strPolAdd3 = %Trim(strPolAdd3) + '-' +
              %Subst(%EditC(intZip:'X'):6:4);
          EndIf;
          If strPolAdd2 = *Blanks;
             strPolAdd2 = strPolAdd3;
             strPolAdd3 = *Blanks;
          EndIf;
          If UPzip = *zeros;
             strPolAdd1 = *Blanks;
             strPolAdd2 = *Blanks;
             strPolAdd3 = *Blanks;
          EndIf;

          // Fax Number
          If UPfaxa <> *zeros or UPfax# <> *zeros;
             strPolFax = '(' + %editc(UPfaxa:'X') + ')' +
              %subst(%editc(UPfax#:'X'):1:3) + '-' +
              %subst(%editc(UPfax#:'X'):4:4);
          Endif;
          strEmail = %trim(UPmail);
       Endif;

      /End-Free

     P LoadUsrParm     E

