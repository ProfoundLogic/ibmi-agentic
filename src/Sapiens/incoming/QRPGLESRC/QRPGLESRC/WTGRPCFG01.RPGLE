     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Group Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPCFG
     h*  Calls to:
     h*
     h*  Created on 10/10/2019 by C:300 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*   05/08/25 C:333 Project: 153WBD                               *
     h*                  Note of Cancel Format-Contact Info Updt.      *
     h*                  - Adding Alternate Company Phone#/Email to    *
     h*                    Detail box.                                 *
     h*
      //  04/29/24 C:340 Project: WC2965                               *
      //                 Add Email Address Required.                   *
     h*
     h*   05/08/24 C:053 Support: 115213
     h*                  Add ASO flag for EDI (PEO/ASO)
     h*
     h*   11/15/21 C:062 Project: 146WJV                               *
     h*                 -Add Exclude from comparative Rating to Detail *
     h*            c:33 -Always populate Connect checkboxes            *
     h*                                                                *
     h*   06/18/21 C:300 Support:96286                                 *
     h*                  Remove 'Summarize A/R for Deductible Billing' *
     h*                  Checkbox from Group Master Information screen *
     h*
     h**********************************************************************
     fwmfnp     if   e             disk    rename(pfn01:pfn0s)
     fwtgtl     if   e           k disk
     fstgtl     if   e           k disk    rename(pgt01:pgt0s)
     fwmfip     if   e           k disk
     fsmcip     if   e           k disk
     fwmvpl     if   e           k disk

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtGrpCfg01       pr                  extpgm('WTGRPCFG01')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpCfg01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strMailAdd1     s             40
     d strMailAdd2     s             40
     d strMailAdd3     s             40
     d strPhyAdd1      s             40
     d strPhyAdd2      s             40
     d strPhyAdd3      s             40
     d strPhnNum1      s             14
     d strPhnNum2      s             14
     d strFaxNum       s             14
     d strOrgType      s             45
     d strAuthType     s             45
     d strEDIfrmt      s             45
     d strAltrAdd1     s             40
     d strAltrAdd2     s             40
     d strAltrAdd3     s             40
     d strPremVerb     s             15
     d strPEOPolGrp    s             15
     d strAutCrtLoc    s             15
     d strSchOprOrd    s             30
     d strPortalFlags  s             50
     d strVendor       s             50
     d strGrpAsc       s             55
        //*** Beg Add ***  05/08/24  *** WC2965 *************************
     d strDftEmail     s             60
        //*** End Add ***  05/08/24  *** WC2965 *************************
        //*** Beg Add ***** 05/08/25 ************************************
     d strAltCPhn#     s             20
        //*** End Add ***** 05/08/25 ************************************

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    FNco#
     C                   KFLD                    kktbid            4
     C                   KFLD                    kkkey            10


      /Free

       Chain inRRN pfn0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlGrp' :%trim(%Char(FNfnd)));
       BuildJSON(outJSON :'OutDtlGroupName' :%trim(FNname));
       BuildJSON(outJSON :'OutDtlShortName' :%trim(FNshnm));
       BuildJSON(outJSON :'CbxDtlPortDft' :FNportdft);
       BuildJSON(outJSON :'OutDtlMailAdd1' :%trim(strMailAdd1));
       BuildJSON(outJSON :'OutDtlMailAdd2' :%trim(strMailAdd2));
       BuildJSON(outJSON :'OutDtlMailAdd3' :%trim(strMailAdd3));
       BuildJSON(outJSON :'OutDtlPhyAdd1' :%trim(strPhyAdd1));
       BuildJSON(outJSON :'OutDtlPhyAdd2' :%trim(strPhyAdd2));
       BuildJSON(outJSON :'OutDtlPhyAdd3' :%trim(strPhyAdd3));
       BuildJSON(outJSON :'OutDtlPhnNum1' :%trim(strPhnNum1));
       BuildJSON(outJSON :'OutDtlPhnNum2' :%trim(strPhnNum2));
       BuildJSON(outJSON :'OutDtlFaxNum' :%trim(strFaxNum));
       BuildJSON(outJSON :'OutDtlIntAdd' :%trim(FNiadr));
       BuildJSON(outJSON :'OutDtlEffDate' :
                         alphaUSA(cvtToDate(dtMDYY(FNefdt))));
       BuildJSON(outJSON :'OutDtlExpDate' :
                         alphaUSA(cvtToDate(dtMDYY(FNexdt))));
       BuildJSON(outJSON :'CbxDtlCommerGrp' :FNcgfl);
       BuildJSON(outJSON :'OutDtlMaxExtYr' :%trim(%Char(FNmxex)));
       BuildJSON(outJSON :'CbxDtlAuAsEmpNum' :FNaute);
       BuildJSON(outJSON :'CbxDtlAuAsDivNum' :FNautd);
       BuildJSON(outJSON :'CbxDtlAlwDivZer' :FNdiv0);
       BuildJSON(outJSON :'OutDtlNextCaseNum' :%trim(%char(FNnacs)));
       BuildJSON(outJSON :'OutDtlNextEmpNum' :%trim(%char(FNnaen)));
       BuildJSON(outJSON :'OutDtlNextQuotNum' :%trim(%char(FNnaqn)));
       BuildJSON(outJSON :'OutDtlDftVendNum' :%trim(strVendor));
       BuildJSON(outJSON :'OutDtlClmResvType1' :%trim(FNrs01));
       BuildJSON(outJSON :'OutDtlClmResvType2' :%trim(FNrs02));
       BuildJSON(outJSON :'OutDtlClmResvType3' :%trim(FNrs03));
       BuildJSON(outJSON :'OutDtlClmResvType4' :%trim(FNrs04));
       BuildJSON(outJSON :'OutDtlClmResvType5' :%trim(FNrs05));
       BuildJSON(outJSON :'OutDtlClmResvType6' :%trim(FNrs06));
       BuildJSON(outJSON :'OutDtlClmResvType7' :%trim(FNrs07));
       BuildJSON(outJSON :'OutDtlClmResvType8' :%trim(FNrs08));
       BuildJSON(outJSON :'OutDtlClmResvType9' :%trim(FNrs09));
       BuildJSON(outJSON :'OutDtlClmResvType10' :%trim(FNrs10));
       BuildJSON(outJSON :'OutDtlMedTransCod' :%trim(FNmetr));
       BuildJSON(outJSON :'OutDtlMedPmtVal' :%trim(FNmep1));
       BuildJSON(outJSON :'OutDtlCompTransCod' :%trim(FNcotr));
       BuildJSON(outJSON :'OutDtlCompPmtVal' :%trim(FNcop1));
       BuildJSON(outJSON :'OutDtlExpTransCod' :%trim(FNextr));
       BuildJSON(outJSON :'OutDtlExpPmtVal' :%trim(FNexp1));
       BuildJSON(outJSON :'OutDtlLegTransCod' :%trim(FNletr));
       BuildJSON(outJSON :'OutDtlLegPmtVal' :%trim(FNlep1));
       BuildJSON(outJSON :'OutDtlMedResvCod' :%trim(FNdfrc));
       BuildJSON(outJSON :'OutDtlMedResvAmt' :%editc(FNdfra:'N'));
       BuildJSON(outJSON :'OutDtlExpResvCod' :%trim(FNdfxc));
       BuildJSON(outJSON :'OutDtlExpResvAmt' :%editc(FNdfxa:'N'));
       BuildJSON(outJSON :'OutDtlFromCheck' :%editc(FNchk1:'N'));
       BuildJSON(outJSON :'OutDtlToCheck' :%editc(FNchk2:'N'));
       BuildJSON(outJSON :'CbxDtlEmpSelfIns' :FNsfin);
       BuildJSON(outJSON :'OutDtlOrgType' :%trim(strOrgType));
       BuildJSON(outJSON :'OutDtlAuthType' :%trim(strAuthType));
       BuildJSON(outJSON :'OutDtlPEOPolGrp' :%trim(strPEOPolGrp));
       BuildJSON(outJSON :'CbxDtlCharExpCon' :FNpeoe);
       BuildJSON(outJSON :'CbxDtlCalStkDis' :FNpeoa);
       //*** Beg Add *** 05/08/24 ****************************
       BuildJSON(outJSON :'CbxAsoGroupFlag' :FNasof);
       //*** End Add *** 05/08/24 ****************************
       BuildJSON(outJSON :'CbxDtlExsInsGrp' :FNexins);
       //***** Beg Del ***** 06/18/21 ****************************
       //*** BuildJSON(outJSON :'CbxDtlSumDedBil' :FNsumded);
       //***** End Del ***** 06/18/21 ****************************
       BuildJSON(outJSON :'CbxDtlTestGrp' :FNtest);
       BuildJSON(outJSON :'CbxDtlIncCICenter' :FNcicf);
       BuildJSON(outJSON :'CbxDtlExlEDIProc' :FNedi);
       BuildJSON(outJSON :'CbxDtlExlClmElg' :FNclelig);
       BuildJSON(outJSON :'OutDtlEDIfrmt' :%trim(strEDIfrmt));
       BuildJSON(outJSON :'OutDtlDueDtMthPay' :%trim(%Char(FNmidy)));
       BuildJSON(outJSON :'OutDtlDueDtWkPay' :%trim(%Char(FNwidy)));
       BuildJSON(outJSON :'OutDtlDayToLost' :%trim(%Char(FNlost)));
       BuildJSON(outJSON :'OutDtlAltrName' :%trim(FNanam));
       BuildJSON(outJSON :'OutDtlAltrAdd1' :%trim(strAltrAdd1));
       BuildJSON(outJSON :'OutDtlAltrAdd2' :%trim(strAltrAdd2));
       BuildJSON(outJSON :'OutDtlAltrAdd3' :%trim(strAltrAdd3));
       BuildJSON(outJSON :'CbxDtlPrtSchOpr' :FNpde);
       BuildJSON(outJSON :'CbxDtlAutEndorse' :FNaer);
       BuildJSON(outJSON :'CbxDtlSbmMTEnd' :FNpeos);
       BuildJSON(outJSON :'OutDtlPremVerb' :%trim(strPremVerb));
       BuildJSON(outJSON :'OutDtlAttName' :%trim(FNattn));
       BuildJSON(outJSON :'OutDtlGrpAsc' :%trim(strGrpAsc));
       BuildJSON(outJSON :'OutDtlAutCrtLoc' :%trim(strAutCrtLoc));
       BuildJSON(outJSON :'OutDtlSchOprOrd' :%trim(strSchOprOrd));

        //*** Beg Add ***  05/08/24  *** WC2965 *************************
       BuildJSON(outJSON :'OutDtlDftEmailAdd':%trim(strDftEmail));
        //*** End Add ***  05/08/24  *** WC2965 *************************
        //*** Beg Add ***** 05/08/25 ************************************
       BuildJSON(outJSON :'OutDtlAltrComPhn#':%trim(strAltCPhn#));
       BuildJSON(outJSON :'OutDtlAltrComEmail':%trim(FNamail));
        //*** End Add ***** 05/08/25 ************************************

      // Get values of Group Portal flags
       IF IsModEnabled('Portal Module');
          strPortalFlags = 'Include Group for ' +
           GetModuleProperty('Portal Module':'Name');
          BuildJSON(outJSON :'HdgPortalFlags' :%trim(strPortalFlags));
        //***  Begin Add  ***  11/15/21  **********************
          Clear FIquote;
          Clear FIfroi;
          Clear FIsr;
          Clear FIpolicy;
          Clear FIclaim;
          Clear FIasOf;
          Clear FIdays;
          Clear FIexcCmpRt;
        //***  End   Add  ***  11/15/21  **********************
          Chain (FNco#:FNfnd) pfi01;
        //***  Begin Del  ***  11/15/21  **********************
        //IF %found(wmfip);
        //***  End   Del  ***  11/15/21  **********************
             BuildJSON(outJSON :'CbxDtlPortQuote' :FIquote);
             //*** Begin Add *** 11/15/21 *******************************
             BuildJSON(outJSON:'CbxDtlExcludeComparativeRating':FIexcCmpRt);
             //*** End   Add *** 11/15/21 *******************************
             BuildJSON(outJSON :'CbxDtlPortFROI' :FIfroi);
             BuildJSON(outJSON :'CbxDtlPortSR' :FIsr);
             BuildJSON(outJSON :'CbxDtlPortPolicy' :FIpolicy);
             BuildJSON(outJSON :'CbxDtlPortClaim' :FIclaim);
             BuildJSON(outJSON :'OutDtlDtAsOfDate' :
                            alphaUSA(cvtToDate(dtMDYY(FIasof))));
             BuildJSON(outJSON :'OutDtlLookbackDays' :%trim(%Char(FIdays)));
        //***  Begin Del  ***  11/15/21  **********************
        //EndIf;
        //***  End   Del  ***  11/15/21  **********************
         // Use company level defaults if group values are not defined
          If FIasOf = 0 and FIdays = 0;
             Chain FNco# pci01;
             If %found(smcip);
                BuildJSON(outJSON :'OutDtlDtAsOfDate' :
                            alphaUSA(cvtToDate(dtMDYY(CIasof))));
                BuildJSON(outJSON :'OutDtlLookbackDays' :%trim(%Char(CIdays)));
             EndIf;
          EndIf;
       EndIf;


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
       If FNph1a <> *zeros;
          strPhnNum1 = '(' + %Subst(%EditC(FNph1a:'X'):1:3) + ')';
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
       If FNfaxa <> *zeros;
          strFaxNum = '(' + %Subst(%EditC(FNfaxa:'X'):1:3) + ')';
       EndIf;
       If FNfax <> *zeros;
          strFaxNum = %trim(strFaxNum) + ' ' + %Subst(%EditC(FNfax:'X'):1:3) +
          '-' + %Subst(%EditC(FNfax:'X'):4:4);
       EndIf;

        //*** Beg Add ***  05/08/24  *** WC2965 *************************
       strDftEmail = FNemail;
        //*** End Add ***  05/08/24  *** WC2965 *************************

        //*** Beg Add ***** 05/08/25 ************************************
       strAltCPhn# = '(' + %EditC(FNaara:'X') + ')' +
                      %Subst(%EditC(FNaphn:'X'):1:3) + '-' +
                      %Subst(%EditC(FNaphn:'X'):4:4);
        //*** End Add ***** 05/08/25 ************************************

       // Self-Insured Organisation Type
       strOrgType = %trim(FNsior);
       IF FNsior <> *Blanks;
          kktbid = 'SIOR';
          kkkey =  FNsior;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat ='A';
             strOrgType = %trim(strOrgType) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // Self-Insured Authorization Type
       strAuthType = %trim(FNsiau);
       IF FNsiau <> *Blanks;
          kktbid = 'SIAU';
          kkkey =  FNsiau;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat ='A';
             strAuthType = %trim(strAuthType) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // EDI Format ID
       strEDIfrmt = %trim(FNedif);
       IF FNedif <> *Blanks;
          kktbid = 'EDIF';
          kkkey =  FNedif;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat ='A';
             strEDIfrmt = %trim(strEDIfrmt) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // Alternate Address
       strAltrAdd1 = %Trim(FNaad1);
       strAltrAdd2 = %Trim(FNaad2);
       strAltrAdd3 = %Trim(FNacty) + ', ' + FNast + ' ' +
        %Subst(%EditC(FNazip:'X'):1:5);
       If %Subst(%EditC(FNazip:'X'):6:4) <> *Blanks;
          strAltrAdd3 = %Trim(strAltrAdd3) + '-' +
           %Subst(%EditC(FNazip:'X'):6:4);
       EndIf;
       If strAltrAdd2 = *Blanks;
          strAltrAdd2 = strAltrAdd3;
          strAltrAdd3 = *Blanks;
       EndIf;
       If FNazip = *zeros;
          strAltrAdd1 = *Blanks;
          strAltrAdd2 = *Blanks;
          strAltrAdd3 = *Blanks;
       EndIf;

       // Premium/Contribution Verbiage
       If FNprmv = 'P';
          strPremVerb = 'P-Premium';
       EndIf;
       If FNprmv = 'C';
          strPremVerb = 'C-Contribution';
       EndIf;

       // PEO Policy Group
       Select;
          When FNpeof = 'S';
             strPEOPolGrp = 'S-Single';
          When FNpeof = 'M';
             strPEOPolGrp = 'M-Multiple';
          When FNpeof = 'C';
             strPEOPolGrp = 'C-Combined';
       EndSl;

       // Auto-Create Entity/Location on Employer
       Select;
          When FNaclr = 'E';
             strAutCrtLoc = 'E-Entity';
          When FNaclr = 'L';
             strAutCrtLoc = 'L-Location';
       EndSl;

       // Schedule of Operations Division & Entity/Location Print Order (A/D)
       Select;
          When FNdepo = 'A';
             strSchOprOrd = 'A-Alphabetic by City';
          When FNdepo = 'D';
             strSchOprOrd = 'D-Division #';
       EndSl;

       // Get Vendor Descrition from WMVPL file
       strVendor = %char(FNdftvend#);
       If FNdftvend# <> 0;
          Chain (FNco#:FNdftvend#) wmvpl;
          IF %found(wmvpl);
             strVendor = %trim(strVendor) + ' - '  + VPname;
          EndIf;
       EndIf;

       // Group Association
       strGrpAsc = %trim(FNgrpasc);
       IF FNgrpasc <> *Blanks;
          kktbid = 'GRPA';
          kkkey = FNgrpasc;
          Chain GTKY STGTL;
          IF %found(STGTL) and GTstat ='A';
             strGrpAsc = %trim(strGrpAsc) + ' - '  + GTdesc;
          EndIf;
       EndIf;


       endsr;
      /End-Free
