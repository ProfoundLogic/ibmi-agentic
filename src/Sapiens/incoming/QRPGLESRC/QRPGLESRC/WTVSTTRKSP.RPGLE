      *===============================================================*
      *  Revisions:                                                   *
      *
      *  09/28/23 C:333 Support: 113925
      *                 - ASDP07: RNQ0202 The call to LOADVISITG ended
      *                   in error (C G D F).
      *
      *  08/02/23 C:291 Support: 113485
      *                 Correct Visit Tracking grid load upto 9999
      *                 records.
      *
      *  06/08/23 C:342 Project: WC2747
      *                 Add error handling when associated required
      *                 field is 'C' and Service Request Completion
      *                 date is enterred.
      *
      *  04/06/23 C:333 Support: 107461
      *                 - Changing Tab Name from 'Visit Track Report' to
      *                   'Service Request Tracking Report'.
      *
      *  04/05/23 C:053 Support: 108793
      *                 - Correct asterisk positioning
      *
      *  10/17/22 C:291 Support: 103956
      *                 - Comment out ParseString procedure use in
      *                   VisitSubset to improve performance.
      *                 - Stop loading Service Request Tracking grid when
      *                   it reaches 9999 records. It will display popup
      *                   window to use filter to display other records.
      *                   This change only applies to WTVSTTRK.
      *                 - Load ssVstSts in InzSetup based on Application
      *                   Config/Service Request Tracking default values.
      *                   ssVstSts is 5A field so if Active and Inactive is
      *                   selected as default, ssVstSts value would be 'A,I'.
      *                   This change will properly set the Fiter based on
      *                   Service Request Tracking default values.
      *
      *  06/24/22 C:300 Project: WC2395
      *                 1) Agency links in Service Request Tracking Grid should not be
      *                 visible when called from Work with Agent or Agency Maintenace
      *                 2) Check If record has been updated by another user in Edit Mode
      *
      *  06/23/21 C:318 Project WC2302 - Auto tab closure for
      *                deletion.
      *
      *  06/21/21 C:298 Project: SY426                                     *
      *                 Re-Brand and Config for Visit Tracking             *
      *                                                                    *
      *  06/09/20  C:298 Project: SY420                                    *
      *                  Mask SSN                                          *
      *                                                                    *
      *  09/25/19 C:291 Project: 129WVN                               *
      *                 Allow the ability to add multiple visits for a*
      *                 single visit type with same date and assigned *
      *                 resource if the check box "Allow multiple     *
      *                 entries" is checked in Allplication config/   *
      *                 Visit Tracking.                               *
      *                                                               *
      *  08/08/19 C:212 Support: 81206
      *                 Fix to show only active locations on Visit Tracking - add visit
      *
      *  06/18/19 C:300 Support: 80204 (LUBA)
      *                 Policy/Claim links in Visit Tracking menu call should
      *                 use the security available in Application Configuration.
      *                 If user does not have any access then link should not appear.
      *                 If user have access, select the link then they should go to
      *                 policy or claim in inquiry or edit depending on app config.
      *                                                               *
      * 02/04/19 C:298 PHW76725                                       *
      *                Fix incorrect comparison of dates.             *
      *                                                               *
      * 12/11/18 C:180 PHW74876                                       *
      *                Fix policy link to point to correct env.       *
      *                                                               *
      * 11/21/18 C:33  PHW74689                                       *
      *                Poplulate bound message fields as screen is    *
      *                 expecting.                                    *
      *                                                               *
      * 03/16/16 C:310 PHW40305                                       *
      *                Modified code to prevent showing detail part   *
      *                when grid is empty at visit tab at home page   *
      *                                                               *
      * 02/10/16 C:242 PW37116                                        *
      *                DBA/Fed Names and Links for Quote not working  *
      *                correctly.                                     *
      *                                                               *
      * 09/09/15 C:242 Phaseware: #37116                              *
      *                1. Claim Detail not populating                 *
      *                2. Policy/Claims link to be Update/Display     *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*
       //===================================================================
       // Add Visit Detail
       //===================================================================
     p AddVisitDtl     b
     d AddVisitDtl     pi

      /free
       CLEAR rcdVstEdt;
       CLEAR pvi0p;
       vstEdtCx = *off;
       vstEdtSv = *off;
       //***** Beg Chg ***** 06/21/21 *********************************
       //*****pnlVstHdg = 'Add Visit Detail';
       pnlVstHdg = 'Add Service Request Detail';
       //***** End Chg ***** 06/21/21 *********************************
       dtlVstCat = @@Category;
       ClearErrors();
       dtlVstCat = @@Category;

       //*** Begin Add *** 06/23/21 **********************************
       outRRn = $$RRN;
       //*** End   Add *** 06/23/21 **********************************
       //first select Visit Type, then plot the rest
       DOU (vstAddCx = *on) or (DtlVstTyp <>' ');
          PlotVisitAdd();
          EXFMT rcdVstEdt;
       ENDDO;
       IF vstAddCx = *off;
          SELECT;
          WHEN @@Category = 'Policy';
            edtVstPol = *on;
            vstEmpSet = *on;
            scQteFlag = @@Qte;
            overrideEmp(scQteFlag);
            CHAIN $$RRN pel0p;
            IF %found;
              VIco#  = ELco#;
              VIfnd  = ELfnd;
              VIemp# = ELemp#;
              VIfyr  = ELfyr;
              VIQte  = @@Qte;
              VIDiv  = polTrakDiv;
            ENDIF;
            delOverrideEmp(scQteFlag);
          WHEN @@Category = 'Claims';
            edtVstClm = *on;
            vstClmSet = *on;
            CHAIN $$RRN pcm0p dsWMCMP;
            IF %found;
              VIco#  = dsWMCMP.CMco#;
              VIfnd  = dsWMCMP.CMfnd;
              VIemp# = dsWMCMP.CMemp#;
              VIfyr  = dsWMCMP.CMfyr;
              VIcase = dsWMCMP.CMcase;
          //*** Begin Add *** 09/09/15 **********************************
              VIdiv  = dsWMCMP.CMdiv;
          //*** End   Add *** 09/09/15 **********************************
            ENDIF;
          WHEN @@Category = 'Agency';
            edtVstAgt = *on;
            vstAgtSet = *on;
            CHAIN $$RRN wmagp;
            IF %found;
              VIco#  = AGco#;
              VIfein = AGfein;
              VImod  = AGmod;
              VIagt# = AAagt#;
            ENDIF;
          ENDSL;

          VICO# = intCo#;
          VITYPE = dtlVstTyp;
          VICAT  = dtlVstCat;
          PlotVisitHdr('Add');
          PlotVisitDtl('Add');
          DOU (vstEdtCx = *on) or (vstEdtSv = *on);
             EXFMT rcdVstEdt;
             SELECT;
             WHEN (vstEdtCx = *on);
                LEAVE;
             WHEN (vstEdtSv = *on);
                ClearErrors();
                //*** Beg Chg ***** 09/25/19 *********************
                //ValidateVisit();
                ValidateVisit('Add');
                //*** End Chg ***** 09/25/19 *********************
                IF not error;
                   SaveVisit(0:'Add');
                ELSE;
                   vstEdtSv = *off;
                ENDIF;
             ENDSL;
          ENDDO;
       ENDIF;
      /end-free
     p AddVisitDtl     e

       //*************************************************************
       //* BuildData for Agency Detail box
       //*************************************************************
     p AgencyDetail    b
     d AgencyDetail    pi
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inFein                        9  0 const
     d  inMod                         3  0 const
      /free
       IF (inCat = 'A');
         fSetAgtDtl = *on;
         dtlBoxYoffset = 715;
       ENDIF;

       outAgtFed# = %editw(infein:'   -  -    ') +'/'+ %editc(inMod:'X');
       CHAIN (inCo#:infein:inmod) WMagl;
       IF %found;
         outAgtDBA  = AGname;
         outAgtFedN = AGfdnm;
         outAgtEMai = AGemal;
         IF OutAgtEMai <> *blanks;
            mlAgtEmai = OutAgtEMai;
         ELSE;
            mtAgtEmai = Email_NA;
         ENDIF;
         OutAgtPhn# = AGare1*10000000 + AGphn1;
         outAgtFax = AGfaxa*10000000 + AGfax#;
         IF (AGmad1 <> *blanks);
            FormatAddress(AGmad1:@@mad@:AGmcty:AGmst:AGmzip:
                                    AdL1:AdL2:AdL3);
         ELSE;
            FormatAddress(AGpad1:@@mad@:AGpcty:AGpst:AGpzip:
                                    AdL1:AdL2:AdL3);
         ENDIF;
         outAgtMlg1 = ADL1;
         outAgtMlg2 = ADL2;
         outAgtMlg3 = ADL3;
         IF AGpst <> ' ';
            pcnkey = AGpst + AGcnty;
         ELSE;
            pcnkey = AGmst + AGcnty;
         ENDIF;
         CHAIN (inCo#:pcnKey) pcn00;
         IF %found;
            outAgtCnty = CNdesc;
         ENDIF;
         outAgtWebP = agiadr;
         Chain (inCo#:AGtpc2) pat00;
         IF %found;
            OutAgtTyp  = %trim(ATdesc);
         ELSE;
            OutAgtTyp  = %trim(AGtpc2);
         ENDIF;
         Chain (inCo#:AGgrcd) pag01;
         IF %found;
            OutAgtGrp  = %trim(AGdesc);
         ELSE;
            OutAgtGrp  = %trim(AGgrcd);
         ENDIF;
         Chain (inCo#:AGarea) pae01;
         IF %found;
            OutAgtArea = %trim(AEdesc);
         ELSE;
            OutAgtArea = %editc(AGarea:'X');
         ENDIF;
         Chain (inCo#:'AMKR':AGmkrg) pgt01;
         IF %found;
            outAgtMktR = %trim(GTdesc);
         ELSE;
            outAgtMktR = %trim(AGmkrg);
         ENDIF;
       ENDIF;

       IF (inCat = 'A');
         //populate fields specific to Agency Detail
         CHAIN (AGco#:AGfein:AGmod) paa00;
         IF %Found(WMAAL);
            outAgency# = %editc(AAagt#:'X');
         ENDIF;
         outAgtSRep = GetUsrName(GetUserId(AGsvcr));
         outAgtMRep = GetUsrName(GetUserId(AGmkrp));

         rrnAgtPrd = 0;
         rrnAgtUnd = 0;

         //Agent Underwriters Assigned.
         clrAgtUnd = *off;
         dspAgtUnd = *on;
         CLEAR agtUndArr;
         index = 0;

         SETLL (inCo#:inFein:inMod) paa00;
         READE (inCo#:inFein:inMod) paa00;
         DOW not %eof;
           IF (AAuwtr <> *blanks);
             agtUndIndex = %lookup(AAuwtr:agtUndArr);
             IF (agtUndIndex = 0);
               index +=1;
               agtUndArr(index) = AAuwtr;
               agtUndWrt = GetUsrName(GetUserId(AAuwtr));
               agtUndEml = GetUsrEmail(GetUserId(AAuwtr));
               IF (agtUndEml = *blanks);
                 agtUndEmlT = Email_NA;
               ENDIF;
               rrnAgtUnd = rrnAgtUnd + 1;
               WRITE sflAgtUndw;
             ENDIF;
           ENDIF;

           READE (inCo#:inFein:inMod) paa00;
         ENDDO;

         //Agent Producers.
         clrAgtPrd = *off;
         dspAgtPrd = *on;

         SETLL (inCo#:inFein:inMod) pap00;
         READE (inCo#:inFein:inMod) pap00;
         DOW not %eof;
           agtProduce= %trim(APLSN) +', '+ %trim(APFSN);
           agtPrdEml = APemal;
           IF (agtPrdEml = *blanks);
             agtPrdEmlT = Email_NA;
           ENDIF;
           rrnAgtPrd = rrnAgtPrd + 1;
           WRITE sflAgtProd;
           READE (inCo#:inFein:inMod) pap00;
         ENDDO;

       ENDIF;
       RETURN;
      /end-free
     p AgencyDetail    e

       //*************************************************************
       //* BuildData for Claims Detail box
       //*************************************************************
     p ClaimsDetail    b
     d ClaimsDetail    pi
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inGroup                       3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inFyr                         3  0 const
     d  inCase                        7  0 const
      /free
       IF (inCat = 'C');
         fSetClmDtl = *on;
         dtlBoxYoffset = 970;
       ENDIF;
       CHAIN (inCo#:inGroup:inEmp#:inDiv:inFyr:inCase) pcm00 dsWMCML;
       IF %found;
         CHAIN dsWMCML.CMss# wmcdl;
         IF %found;
           mlClaimant= CDemad;
           IF (mlClaimant = *blanks);
             mtClaimant = Email_NA;
           ENDIF;
           IF cdMid<>*Blanks;
              outClmtNm=%Trim(CDlsn) + ', ' +
              %Trim(CDfsn) + ' ' + %Trim(CDmid) + '.';
           ELSE;
              outClmtNm=%Trim(CDlsn) + ', ' + %Trim(CDfsn);
           ENDIF;
           outClmtPh = CDarea*10000000 + CDphn#;
           FormatAddress(CDAdr1:CDAdr2:CDCty:CDSt:CDZip:AdL1:AdL2:AdL3);
           outClmtAd1 = ADL1;
           outClmtAd2 = ADL2;
           outClmtAd3 = ADL3;
         ENDIF;
         outClaim# = %editc(inGroup:'X') + '-' + %editc(inFyr:'X')
                      + '-' + %editc(inCase:'X');
         outDtInjur= dtMDYY(dsWMCML.CMACDT);
         //***  Begin Chg  ***  06/09/20  **********************
         //outClmSSN = 'xxx-xx-' + %Subst(dsWMCML.CMss#:6:4);
         outClmSSN = DisplaySS#(MaskData('SSN':'CMss#':dsWMCML.CMss#));
         //***  End   Chg  ***  06/09/20  **********************
         SELECT;
         WHEN dsWMCML.Cmstat = 'O';
           outClmSts = 'Open';
         WHEN dsWMCML.Cmstat = 'C';
           outClmSts = 'Closed';
         ENDSL;
         outStClm# = dsWMCML.CMacl#;
         SELECT;
         WHEN dsWMCML.CMltFl='M';
           outClmTyp ='Med Only';
         WHEN dsWMCML.CMltFl='I';
           outClmTyp ='Indemnity';
         WHEN dsWMCML.CMltFl='R';
           outClmTyp ='Record Only';
         ENDSL;
         CHAIN (inCo#:dsWMCML.CMnatr) pna00;
         IF %found;
           outClmNat = %trim(dsWMCML.CMnatr) + ' - ' + NAdesc;
         ELSE;
           outClmNat = dsWMCML.CMnatr;
         ENDIF;
         CHAIN (inCo#:dsWMCML.CMpart) ppa00;
         IF %found;
           outClmPart = %Trim(dsWMCML.CMpart) + ' - ' + PAdesc;
         ELSE;
           outClmPart = dsWMCML.CMpart;
         ENDIF;
         CHAIN (inCo#:dsWMCML.CMcaus) pca00;
         IF %found;
           outClmCaus = %Trim(dsWMCML.CMcaus) + ' - ' + CAdesc;
         ELSE;
           outClmCaus = dsWMCML.CMcaus;
         ENDIF;
         STkey = dsWMCML.CMjrst;
         CHAIN STkey pst00;
         IF %found;
           outJuriSt = STname;
         ELSE;
           outJuriSt = *blanks;
         ENDIF;
         outClmMgr = GetUsrName(GetUserId(dsWMCML.CMaMan));
         mlClmMgr = GetUsrEmail(GetUserId(dsWMCML.CMaMan));
         IF mlClmMgr = *blanks;
            mtClmMgr = Email_NA;
         ENDIF;
         outClmSup = GetUsrName(GetUserId(dsWMCML.CMsupr));
         mlClmSup = GetUsrEmail(GetUserId(dsWMCML.CMsupr));
         IF mlClmSup = *blanks;
            mtClmSup = Email_NA;
         ENDIF;
         outClmAdj = GetUsrName(GetUserId(dsWMCML.CMclEx));
         mlClmAdj = GetUsrEmail(GetUserId(dsWMCML.CMclEx));
         IF mlClmAdj = *blanks;
            mtClmAdj = Email_NA;
         ENDIF;
         outCaseNur= GetUsrName(GetUserId(dsWMCML.CMnrse));
         mlCaseNur= GetUsrEmail(GetUserId(dsWMCML.CMnrse));
         IF mlCaseNur = *blanks;
            mtCaseNur = Email_NA;
         ENDIF;
         outClmOAdj= GetUsrName(GetUserId(dsWMCML.CMfeAd));
         mlClmOAdj= GetUsrEmail(GetUserId(dsWMCML.CMfeAd));
         IF mlClmOAdj = *blanks;
            mtClmOAdj = Email_NA;
         ENDIF;
       ENDIF;

       PolicyDetail(inCat:inCo#:inGroup:inEmp#:inDiv:'  ':inFyr:' ');

       RETURN;
      /end-free
     p ClaimsDetail    e

       //===================================================================
       // Clear the errors for Add/Edit Visit Entries
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi
      /free

       error = *off;

       CLEAR mgVstAsgn;
       CLEAR mgVstDte;
       CLEAR mgCmpDte;
       CLEAR mgDte01;
       CLEAR mgDte02;
       CLEAR mgDte03;
       CLEAR mgDte04;
       CLEAR mgDte05;
       CLEAR mgDte06;
       CLEAR mgDte07;
       CLEAR mgDte08;
       CLEAR mgDte09;
       CLEAR mgDte10;
       CLEAR mgItg01;
       CLEAR mgItg02;
       CLEAR mgItg03;
       CLEAR mgItg04;
       CLEAR mgItg05;
       CLEAR mgItg06;
       CLEAR mgItg07;
       CLEAR mgItg08;
       CLEAR mgItg09;
       CLEAR mgItg10;
       CLEAR mgDec01;
       CLEAR mgDec02;
       CLEAR mgDec03;
       CLEAR mgDec04;
       CLEAR mgDec05;
       CLEAR mgDec06;
       CLEAR mgDec07;
       CLEAR mgDec08;
       CLEAR mgDec09;
       CLEAR mgDec10;
       CLEAR mgTxt01;
       CLEAR mgTxt02;
       CLEAR mgTxt03;
       CLEAR mgTxt04;
       CLEAR mgTxt05;
       CLEAR mgTxt06;
       CLEAR mgTxt07;
       CLEAR mgTxt08;
       CLEAR mgTxt09;
       CLEAR mgTxt10;

       erVstAsgn = *off;
       erVstDte = *off;
       //***** Beg add ***** 06/08/23 ***************************
       erCmpDte = *off;
       //***** End add ***** 06/08/23 ***************************
       erDte01  = *off;
       erDte02  = *off;
       erDte03  = *off;
       erDte04  = *off;
       erDte05  = *off;
       erDte06  = *off;
       erDte07  = *off;
       erDte08  = *off;
       erDte09  = *off;
       erDte10  = *off;
       erItg01  = *off;
       erItg02  = *off;
       erItg03  = *off;
       erItg04  = *off;
       erItg05  = *off;
       erItg06  = *off;
       erItg07  = *off;
       erItg08  = *off;
       erItg09  = *off;
       erItg10  = *off;
       erDec01  = *off;
       erDec02  = *off;
       erDec03  = *off;
       erDec04  = *off;
       erDec05  = *off;
       erDec06  = *off;
       erDec07  = *off;
       erDec08  = *off;
       erDec09  = *off;
       erDec10  = *off;
       erTxt01  = *off;
       erTxt02  = *off;
       erTxt03  = *off;
       erTxt04  = *off;
       erTxt05  = *off;
       erTxt06  = *off;
       erTxt07  = *off;
       erTxt08  = *off;
       erTxt09  = *off;
       erTxt10  = *off;

      /end-free
     p ClearErrors     e

       //===================================================================
       // Clear Visit Details
       //===================================================================
     p ClearVisitDtl   b
     d ClearVisitDtl   pi
      /free
        fSetPolDtl = *off;
        fSetClmDtl = *off;
        fSetAgtDtl = *off;
        fSetVstDtl = *off;

        dtlCMPDTE  = *zero;
        dtlVSTDTE  = *zero;
        outAGENCY# = *blanks;
        outAGTAREA = *blanks;
        outCLAIM#  = *blanks;
        outDTINJUR = *zero;
        outFEDID#  = *blanks;
        RRNAGTPRD  = *zero;
        RRNAGTUND  = *zero;
        DetailBoxDsA = *blanks;
        //*** Beg Add ***** 09/25/19 *******************************
        dtlVstSeq# = *zero;
        //*** End Add ***** 09/25/19 *******************************

        outDte01L = *blanks;
        outDte01I = *off;
        outDte02L = *blanks;
        outDte02I = *off;
        outDte03L = *blanks;
        outDte03I = *off;
        outDte04L = *blanks;
        outDte04I = *off;
        outDte05L = *blanks;
        outDte05I = *off;
        outDte06L = *blanks;
        outDte06I = *off;
        outDte07L = *blanks;
        outDte07I = *off;
        outDte08L = *blanks;
        outDte08I = *off;
        outDte09L = *blanks;
        outDte09I = *off;
        outDte10L = *blanks;
        outDte10I = *off;

        outChk01V = *blanks;
        outChk01I = *off;
        outChk02V = *blanks;
        outChk02I = *off;
        outChk03V = *blanks;
        outChk03I = *off;
        outChk04V = *blanks;
        outChk04I = *off;
        outChk05V = *blanks;
        outChk05I = *off;
        outChk06V = *blanks;
        outChk06I = *off;
        outChk07V = *blanks;
        outChk07I = *off;
        outChk08V = *blanks;
        outChk08I = *off;
        outChk09V = *blanks;
        outChk09I = *off;
        outChk10V = *blanks;
        outChk10I = *off;

        outItg01V = *zero;
        outItg01i = *off;
        outItg02V = *zero;
        outItg02i = *off;
        outItg03V = *zero;
        outItg03i = *off;
        outItg04V = *zero;
        outItg04i = *off;
        outItg05V = *zero;
        outItg05i = *off;
        outItg06V = *zero;
        outItg06i = *off;
        outItg07V = *zero;
        outItg07i = *off;
        outItg08V = *zero;
        outItg08i = *off;
        outItg09V = *zero;
        outItg09i = *off;
        outItg10V = *zero;
        outItg10i = *off;

        outDec01V = *zero;
        outDec01i = *off;
        outDec02V = *zero;
        outDec02i = *off;
        outDec03V = *zero;
        outDec03i = *off;
        outDec04V = *zero;
        outDec04i = *off;
        outDec05V = *zero;
        outDec05i = *off;
        outDec06V = *zero;
        outDec06i = *off;
        outDec07V = *zero;
        outDec07i = *off;
        outDec08V = *zero;
        outDec08i = *off;
        outDec09V = *zero;
        outDec09i = *off;
        outDec10V = *zero;
        outDec10i = *off;

        outTxt01V = *blanks;
        outTxt01i = *off;
        outTxt02V = *blanks;
        outTxt02i = *off;
        outTxt03V = *blanks;
        outTxt03i = *off;
        outTxt04V = *blanks;
        outTxt04i = *off;
        outTxt05V = *blanks;
        outTxt05i = *off;
        outTxt06V = *blanks;
        outTxt06i = *off;
        outTxt07V = *blanks;
        outTxt07i = *off;
        outTxt08V = *blanks;
        outTxt08i = *off;
        outTxt09V = *blanks;
        outTxt09i = *off;
        outTxt10V = *blanks;
        outTxt10i = *off;

      /end-free
     p ClearVisitDtl   e

       //===================================================================
       // Clear Visit Types Filter
       //===================================================================
     p ClrVisitFilter  b
     d ClrVisitFilter  pi
      /free
       //CLEAR rcdVstFltr;
       ssVstCat = @@Category;
       ssVstTyp = *blanks;
       ssVstTpCat = *blanks;
       ssVstAsgn= *blanks;
       ssVstSts = *blanks;
       ssVstFrDt= *zero;
       ssVstToDt= *zero;
       ssCmpFrDt= *zero;
       ssCmpToDt= *zero;
       IF $$RRN = *zero;  //Retain if Agency/Claims/Policy passed in.
          ssGroup  = *zero;
          ssEmpDiv = *blanks;
          ssEmpDivNm= *blanks;
          ssLocation= *blanks;
          ssClaim# = *blanks;
          ssClmLnm  = *blanks;
          ssClmFnm  = *blanks;
          ssAgtFein= *blanks;
          ssAgtName = *blanks;
          ssQteFlag = *blanks;
       ENDIF;
       clrVstFlt = *Off;
       showVstClr= *Off;
       //*** Beg Add ***** 10/17/22 ***************************************
       isSubset9999 = *Off;
       //*** End Add ***** 10/17/22 ***************************************
      /end-free
     p ClrVisitFilter  e

       //===================================================================
       // Clear Subfiles
       //===================================================================
     p ClrVstSubFiles  b
     d ClrVstSubFiles  pi

      /free
       // Clear the subfile.
       dspVst = *off;
       clrVst = *on;
       dspAgtUnd = *off;
       clrAgtUnd = *on;
       dspAgtPrd = *off;
       clrAgtPrd = *on;
       WRITE rcdVstTrk;

      /end-free
     p ClrVstSubFiles  e

       //===================================================================
       // Delete/Undelete Visit
       //===================================================================
     p DeleteUndelete  b
     d DeleteUndelete  pi
     d inVstRRN                       9  0 const

     D xxRRN           s              9  0 inz(0)
      /free
       xxRRN = inVstRRN;
       CHAIN xxRRN pvi0p;
       IF %found;
         SELECT;
         WHEN Vistat = 'A';
           VIstat = 'I';
         WHEN Vistat = 'I';
           VIstat = 'A';
         ENDSL;
         Update pvi0p;
         btnRefresh = *on;
       ENDIF;

      /end-free
     p DeleteUndelete  e

       //===================================================================
       // DelOverrideEmp - Delete Employer Overrdes and Close Files
       //===================================================================
     p DelOverrideEmp  b
     d DelOverrideEmp  pi
     d  quoteType                     1

     d mode            s              1

      /free

       If empUsrOpn = *On;
          mode = quoteType;

          // Close Employer Files
          IF %open(WDELP);
             Close WDELP;
          ENDIF;
          //*** Begin Add *** 02/10/16 **********************************
          IF %open(WDELLQ);
             Close WDELLQ;
          ENDIF;
          //*** End   Add *** 02/10/16 **********************************

          IF %open(WMEML);
             Close WMEML;
          ENDIF;

          IF %open(WMALL);
             Close WMALL;
          ENDIF;

          // Delete Employer File Overrides IF Quote Employer
          IF mode = 'Q';
             strCommand = 'DLTOVR FILE(WDELP)';
             Callp QCmdExc(strCommand:%len(strCommand));

          //*** Begin Add *** 02/10/16 **********************************
             strCommand = 'DLTOVR FILE(WDELLQ)';
             Callp QCmdExc(strCommand:%len(strCommand));
          //*** End   Add *** 02/10/16 **********************************

             strCommand = 'DLTOVR FILE(WMEML)';
             Callp QCmdExc(strCommand:%len(strCommand));

             strCommand = 'DLTOVR FILE(WMALL)';
             Callp QCmdExc(strCommand:%len(strCommand));
          ENDIF;
       EndIf;

      /end-free

     p DelOverrideEmp  e

       //===================================================================
       // Edit Visit Detail
       //===================================================================
     p EditVisitDtl    b
     d EditVisitDtl    pi
     d inVstRRN                       9  0 const

     D xxRRN           s              9  0 inz(0)
      /free
       xxRRN = inVstRRN;
       CLEAR rcdVstEdt;
       //*** Begin Add *** 06/23/21 **********************************
       outRRn = $$RRN;
       //*** End   Add *** 06/23/21 **********************************
       vstEdtCx = *off;
       vstEdtSv = *off;
       vstEmpSet  = *on;
       vstDivSet  = *on;
       vstLocSet  = *on;
       vstClmSet  = *on;
       vstAgtSet  = *on;
       //***** Beg Chg ***** 06/21/21 *********************************
       //*****pnlVstHdg = 'Edit Visit Detail';
       pnlVstHdg = 'Edit Service Request Detail';
       //***** End Chg ***** 06/21/21 *********************************
       ClearErrors();
       CHAIN(n) xxRRN pvi0p;
       dtlVstTyp  = VITYPE   ;
       dtlVstCat  = VICAT    ;
       dtlVstAsgn = VIASSIGN ;
       scQteFlag  = VIqte;
       //*** Begin Add *** 06/24/22 **********************************
       saveDT = VIdt;
       saveTM = VItime;
       //*** End   Add *** 06/24/22 **********************************

       PlotVisitHdr('Edit');
       PlotVisitDtl('Edit');

       DOU (vstEdtCx = *on) or (vstEdtSv = *on);
         EXFMT rcdVstEdt;
         IF (vstEdtSv = *on);
            //*** Begin Add *** 06/24/22 **********************************
            // Check if WMVIP updated since screen displayed (Visit Tracking Detail)
            flgRcdUpdated=RcdUpdatedErr('WMVIP':vilRRN8:'VIdt':'VItime':
             'VIuser':saveDT:saveTM:'1');
            if flgRcdUpdated = '2'; // Error with Refresh Button
               vstEdtSv = *off;
               CHAIN(n) xxRRN pvi0p;
               dtlVstTyp  = VITYPE   ;
               dtlVstCat  = VICAT    ;
               dtlVstAsgn = VIASSIGN ;
               scQteFlag  = VIqte;
               saveDT = VIdt;
               saveTM = VItime;

               PlotVisitHdr('Edit');
               PlotVisitDtl('Edit');
            EndIf;
            if flgRcdUpdated = '1'; // Error
               vstEdtSv = *off;
            EndIf;
            if flgRcdUpdated = '0'; // No error
            //*** End   Add *** 06/24/22 **********************************
            ClearErrors();
            //*** Beg Chg ***** 09/25/19 *********************
            //ValidateVisit();
            ValidateVisit('Edit');
            //*** End Chg ***** 09/25/19 *********************
            IF not error;
               SaveVisit(xxRRN:'Edit');
               btnRefresh = *on;
            ELSE;
               vstEdtSv = *off;
            ENDIF;
            //*** Begin Add *** 06/24/22 **********************************
            Endif;
            //*** End   Add *** 06/24/22 **********************************
         ENDIF;
       ENDDO;
      /end-free
     p EditVisitDtl    e

       //===================================================================
       // Initialize Setup Fields
       //===================================================================
     p InzSetup        b
     d InzSetup        pi

      /free
       //Check IF Visit Tracking Installed
       isVstInst = *off;
       isVstInst = IsModEnabled('VISIT TRACKING');
       //***** Beg Chg ***** 06/21/21 ********************************
       //ssVstSts = 'A';
       Setll intco# pvi0p_cfg;
       Reade(n) intco# pvi0p_cfg;
       Dow not %eof(wtvipcfg);
          If cfg_VIStat = 'A';
             If (cfg_VIcat = @@Category and cfg_VIcfgId = 'FilterVisitStatus'
              and cfg_VIcfgval <> ' ');
                If ssVstSts = ' ';
                   //*** Beg Chg ***** 10/17/22 ************************
                   //***ssVstSts = cfg_VIcfgVal;
                   ssVstSts = %Trim(%Subst(cfg_VIcfgVal:1:1));
                   //*** End Chg ***** 10/17/22 ************************
                Else;
                   //*** Beg Chg ***** 10/17/22 ************************
                   //***ssVstSts = %trim(ssVstSts) + ',' + %trim(cfg_VIcfgVal);
                   ssVstSts = %trim(ssVstSts) +
                    ',' + %Trim(%Subst(cfg_VIcfgVal:1:1));
                   //*** End Chg ***** 10/17/22 ************************
                Endif;
             Endif;
          Endif;
          Reade(n) intco# pvi0p_cfg;
       Enddo;
       //***** End Chg ***** 06/21/21 ********************************
       showVstClr= *On;
       noFltrCat  = *on;
       noFltrGrp  = *on;
       noFltrEmp  = *on;
       noFltrLoc  = *on;
       noFltrClm  = *on;
       noFltrAgt  = *on;

       visVstAdd  = *Off;
       visVstEdit = *Off;
       visVstDel  = *Off;
       visVstRpt  = *off;

       isVstSFLnk = *on;

       SELECT;
       WHEN isVstInst = *off;
          showVstClr = *off;
          fSetAgtDtl = *off;
          fSetClmDtl = *off;
          fSetPolDtl = *off;
          fSetVstDtl = *off;

       WHEN @@Category = 'Agency';
          IF  $$Rrn  <> 0;
             ssAgtFein=%EditC(AGfein:'X') + %EditC(AGmod:'X') +
                 %EditC(AAagt#:'X');
             isVstSFLnk = *off;
          ENDIF;
             visVstAdd  = isEditable;
             visVstEdit = isEditable;
             visVstDel  = isEditable;
       WHEN @@Category = 'Claims';
          IF  $$Rrn  <> 0;
             ssGroup  = CMfnd;
             ssClaim# = %EditC(CMfnd:'X') + %EditC(CMfyr:'X') +
                        %EditC(CMcase:'X');
             ssClmLnm = CMlsn;
             ssClmFnm = CMfsn;
             isVstSFLnk = *off;
          ENDIF;
             visVstAdd  = isEditable;
             visVstEdit = isEditable;
             visVstDel  = isEditable;
       WHEN @@Category = 'Policy';
          IF  $$Rrn  <> 0;
             ssGroup  = ELfnd;
             ssEmpDiv = %EditC(ELfnd:'X') + %EditC(ELemp#:'X') +
                        %EditC(ELdiv:'X');
             polTrakDiv = ELdiv;
             isVstSFLnk = *off;
          ENDIF;
             visVstAdd  = isEditable;
             visVstEdit = isEditable;
             visVstDel  = isEditable;
       Other;
          noFltrCat  = *off;
          visVstEdit = isEditable;
          visVstDel  = isEditable;
          visVstRpt  = *on;
          //***** Beg Chg *** 04/06/23 **********************************
          //btnRptLnk  = CreateURL('STDRVPGM':'Visit Track Report':' ':'C'
               //:conReportPgm);
          btnRptLnk  = CreateURL('STDRVPGM':'Service Request Tracking Report'
               :' ':'C':conReportPgm);
          //***** End Chg *** 04/06/23 **********************************
       ENDSL;

       //****** Begin Add *** 06/18/19 **********************************
       isAuthEr = *off;
       isPolUpd = *off;
       isQteUpd = *off;
       isClmUpd = *off;
       isPolInq = *off;
       isQteInq = *off;
       isClmInq = *off;

       // Get user sec level/type
       inULVL = %Editc(GetUsrLvl(Q1user:inUTYP):'X');

       // Claim Update
       keySDACL = 'ClmUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isClmUpd = *on;
       Endif;

       // Policy Update
       keySDACL = 'PolUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isPolUpd = *on;
       Endif;

       // Quote Update
       keySDACL = 'QteUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isQteUpd = *on;
       Endif;

       // Claim Inquiry
       keySDACL = 'ClmInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isClmInq = *on;
       Endif;

       // Policy Inquiry
       keySDACL = 'PolInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isPolInq = *on;
       Endif;

       // Quote Inquiry
       keySDACL = 'QteInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isQteInq = *on;
       Endif;

       // Check Authority
       Begsr CheckAuth;
       Chain keySDACL pac04;
       CheckSecurity(inUTYP:inULVL:ACstp1:%Editc(ACslv1:'X'):'1':outRes
        :ACstp2:ACstp3:ACstp4:ACstp5:ACstp6:%Editc(ACslv2:'X')
        :%Editc(ACslv3:'X'):%Editc(ACslv4:'X'):%Editc(ACslv5:'X')
        :%Editc(ACslv6:'X'):ACstp7:ACstp8:ACstp9:ACstp0:%Editc(ACslv7:'X')
        :%Editc(ACslv8:'X'):%Editc(ACslv9:'X'):%Editc(ACslv0:'X'));
       Endsr;
       //****** End   Add *** 06/18/19 **********************************

      /end-free
     p InzSetup        e

       //===================================================================
       // Initialize Subset Fields
       //===================================================================
     p InzSubset       b
     d InzSubset       pi

      /free
       IF (@@Category <> ' ');
          ssVstCat = @@Category;
       ENDIF;
       disVstAsgn = *Off;
       disVstDate = *Off;
       IF (vstClndrCall = 'Y');
          disVstAsgn = *On;
          // Subset to user for MyWork, but Not for MyTeamWork
          IF isTeam = *off;
             ssVstAsgn = @@Assign;
          ENDIF;

          // Subset to visit date range IF passed in.
          IF @@VstFrDt <> ' ';
             ssVstFrDt = %Dec(@@VstFrDt:8:0);
             ssVstToDt = %Dec(@@VstToDt:8:0);
             disVstDate = *On;
          ENDIF;
       ENDIF;

      /end-free

     p InzSubset       e

       //===================================================================
       // Load Visit Details & Respective Detail boxes below grid
       //===================================================================
     p LoadVisitDtl    b
     d LoadVisitDtl    pi
     d  inVstRrn                      9  0 Const

     d  xCounter       s              2  0
      /free
       ClearVisitDtl();
       CHAIN(n) inVstRrn pvi0p;
       IF %found;
       SELECT;
       WHEN (VICat = 'Policy');
          PolicyDetail('P':VIco#:VIfnd:VIemp#:VIdiv:VIlocid:VIfyr:VIqte);
       WHEN (VICat = 'Claims');
          ClaimsDetail('C':VIco#:VIfnd:VIemp#:VIdiv:VIfyr:VIcase);
       WHEN (VICat = 'Agency');
          AgencyDetail('A':VIco#:VIfein:VImod);
       ENDSL;
       PlotVisitHdr('View');
       PlotVisitDtl('View');
       ENDIF;

       RETURN;

      /end-free
     p LoadVisitDtl    e

       //===================================================================
       // Load Visit Types Grid.
       //===================================================================
     p LoadVisitGrid   b
     d LoadVisitGrid   pi
     d teamPtrIn                       *   Options(*Nopass)
     d teamCntIn                      5  0 Options(*Nopass)

     d ptrToTeamLst    s               *
     D arrTeamList     s             10    dim(9999) based(ptrToTeamLst)
     d teamCount       s              5  0
     D intI            s              5  0 inz

     D kcIn            s              1    inz('0')
     D klIn            s              1    inz('0')
     D callIn          s              1    inz('1')
     D viewIn          s              1    inz('1')
     D quoteIn         s              1    inz(' ')
     D modeIn          s              1    inz(' ')
      *** Beg Add ***** 10/17/22 ********************************
     Dstr9999Msg       s            256
     D isOver9999      s              1    inz('0')
      *** End Add ***** 10/17/22 ********************************
      /free

       clrVst = *off;
       dspVst = *on;
       rrnVst = 0;
       //*** Beg Add ***** 10/17/22 ****************************
       isFilter = *Off;
       //*** End Add ***** 10/17/22 ****************************

       // Handle Team Listing
       IF %Parms() >= 1;

          ptrToTeamLst = teamPtrIn;
          teamCount = teamCntIn;

          For intI = 1 to teamCount;

             // Load the Visit Type subfile For the team
             SETLL (intCo#:arrTeamList(intI)) pvi04;
             READE (intCo#:arrTeamList(intI)) pvi04;
             DOW not %eof;

                IF VisitSubset() = *On;
                   outVstRRN  = vilRRN4;
                   exsr WriteVisitRow;
                   //*** Beg Add ***** 10/17/22 ****************************
                   If isOver9999 = *On;
                      Leave;
                   Endif;
                   //*** End Add ***** 10/17/22 ****************************
                ENDIF;

                READE (intCo#:arrTeamList(intI)) pvi04;
             ENDDO;
          EndFor;

       ELSE;

          // Load the Visit Type subfile.
          SETLL intCo# pvi06;
          READE intCo# pvi06;

          DOW not %eof;

             IF VisitSubset() = *On;
                outVstRRN  = vilRRN6;
                exsr WriteVisitRow;
                //*** Beg Add ***** 10/17/22 ****************************
                If isOver9999 = *On;
                   Leave;
                Endif;
                //*** End Add ***** 10/17/22 ****************************
             ENDIF;

             READE intCo# pvi06;
          ENDDO;
       ENDIF;

       IF rrnVstTop <> 0;
          rrnVst = rrnVstTop;
       ELSE;
          rrnVst = 1;
       ENDIF;

       //================================================================== ************************
       //  WRITE Visit Row:- WRITE Records to Visit Subfile
       //================================================================== ************************
       Begsr WriteVisitRow;

       //*** Begin Add *** 06/18/19 **********************************
       isVstSFLnk = *on;
       //*** End   Add *** 06/18/19 **********************************
          SELECT;
          WHEN VICat = 'Policy';
             overrideEmp(VIqte);
             ProcRRN = 0;
             outVstName = %trim(GetEmpName(VIemp#:VIdiv:ProcRRN:
               VIFnd:VICo#:VIqte)) + ' DIV-' + %trim(%editc(VIDIV:'3'));
             outVstNmTT = %editc(VIFND:'X') +'-'+ %trim(%editc(VIEMP#:'4'));
          //*** Begin Chg *** 02/10/16 **********************************
          //***   SETGT (VIco#:VIfnd:VIemp#:VIdiv) pel00;
          //***  READPE(n) (VIco#:VIfnd:VIemp#:VIdiv) pel00;
          //***  ProcRRN = ELrrn;

          //*** Begin Chg *** 12/11/18 *********************************
          // SETGT (VIco#:VIfnd:VIemp#:VIdiv) pel00q;
          // READPE(n) (VIco#:VIfnd:VIemp#:VIdiv) pel00q;
          // ProcRRN = ELrrnQ;
          //*** End   Chg *** 02/10/16 **********************************
          If VIqte = 'Q';
             SETGT (VIco#:VIfnd:VIemp#:VIdiv) pel00q;
             READPE(n) (VIco#:VIfnd:VIemp#:VIdiv) pel00q;
             ProcRRN = ELrrnQ;
          Else;
             SETGT (VIco#:VIfnd:VIemp#:VIdiv) pel00;
             READPE(n) (VIco#:VIfnd:VIemp#:VIdiv) pel00;
             ProcRRN = ELrrn;
          Endif;
          //*** End   Chg *** 12/11/18 *********************************
             ProcRRNC = %editc(ProcRRN:'X');
          //*** Begin Chg *** 09/09/15 **********************************
          // sflVstLnk = CreateURL('STDRVPGM':outVstName:ProcRRNC:'D'
          //           :conPolicyPgm);
             If @@Mode <> 'D';
          //*** Begin Chg *** 06/18/19 **********************************
          //*** modeIn = InqUpdEmp(VIco#:VIfnd:VIemp#:VIdiv);
                If (isPolUpd = *on and VIQte <> 'Q') OR
                 (isQteUpd = *on and VIQte = 'Q');
                   modeIn = 'C';
                Else;
                   modeIn = 'D';
                Endif;
          //*** End   Chg *** 06/18/19 **********************************
             Else;
                modeIn = 'D';
             Endif;
          //*** Begin Chg *** 02/10/16 **********************************
             IF (VIQte <> 'Q');
                sflVstLnk = CreateURL('STDRVPGM':outVstName:ProcRRNC:modeIn
                            :conPolicyPgm);
             ELSE;
                sflVstLnk = CreateQteURL('STDRVPGM':outVstName:ProcRRNC:modeIn
                       :conPolicyPgm:'00000':VIqte);
             ENDIF;
          //*** End   Chg *** 02/10/16 **********************************
          //*** End   Chg *** 09/09/15 **********************************
          //*** Begin Add *** 06/18/19 **********************************
             If (isPolInq = *off and VIQte <> 'Q') OR
              (isQteInq = *off and VIQte = 'Q');
                sflVstLnk = *Blanks;
                isVstSFLnk = *off;
             Endif;
          //*** End   Add *** 06/18/19 **********************************
             delOverrideEmp(VIqte);
          WHEN VICat = 'Claims';
             ProcRrn = GetClaimRRN(VIco#:VIfnd:VIfyr:VIcase);
             ProcRRNC = %editc(ProcRRN:'X');
             outVstName = %trim(GetClaimName(VICo#:VIFnd:VIFyr:
                                VICase));
             outVstNmTT = %editc(VIFND:'X') + '-' + %editc(VIFYR:'X')
                           + '-' + %editc(VICASE:'X');
          //*** Begin Chg *** 09/09/15 **********************************
          // sflVstLnk = CreateURL('STDRVPGM':outVstName:ProcRRNC:'D'
          //           :conClaimPgm);
             If @@Mode <> 'D';
          //*** Begin Chg *** 06/18/19 **********************************
          //*** modeIn = InqUpdClm(VIco#:VIfnd:VIfyr:VIcase:VIemp#:VIdiv);
                If isClmUpd = *on;
                   modeIn = 'C';
                Else;
                   modeIn = 'D';
                Endif;
          //*** End   Chg *** 06/18/19 **********************************
             Else;
                modeIn = 'D';
             Endif;
             sflVstLnk = CreateURL('STDRVPGM':outVstName:ProcRRNC:modeIn
                       :conClaimPgm);
          //*** End   Chg *** 09/09/15 **********************************
          //*** Begin Add *** 06/18/19 **********************************
             If isClmInq = *off;
                sflVstLnk = *Blanks;
                isVstSFLnk = *off;
             Endif;
          //*** End   Add *** 06/18/19 **********************************
          WHEN VICat = 'Agency';
             ProcRRN = GetMstrRRN(VIfein:VImod:VIco#);
             ProcRRN = 0;
             ProcRRNC = %editc(ProcRRN:'X');
             CHAIN (VICo#:VIfein:VImod) WMagl;
             outVstName = AGname;
             outVstNmTT = ' ';
          //*** Begin Add *** 09/09/15 **********************************
             modeIn = ' ';
          //*** End   Add *** 09/09/15 **********************************
             sflVstLnk = CreateURL('W0040RW':outVstName:kcIn:klIn:callIn
                         :ProcRRNC:viewIn:quoteIn:modeIn);
          //*** Begin Add *** 06/24/22 **********************************
             callpgm = *off;
             callpgm2 = *off;
             ChkStack('WTAGTSRVRT':CallPgm);
             ChkStack('W0040R':CallPgm2);
             If callpgm = *on or callpgm2 = *on;
                sflVstLnk = *Blanks;
                isVstSFLnk = *off;
             Endif;
          //*** End   Add *** 06/24/22 **********************************
          ENDSL;

          outVstTyp  = VIType;
          outVstCat  = VICat;
          outVstAsgn = GetUsrName(VIAssign);
          outVstDt   = dtMDYY(VIVsdt);
          outVstCoDt = dtMDYY(VICmdt);

          SELECT;
          WHEN VIstat = 'C';
             outVstSts  = 'Completed';
             outVstDUd  = *blanks;
          WHEN VIstat = 'A';
             outVstSts  = 'Active';
             outVstDUd  = 'Delete';
          WHEN VIstat = 'I';
             outVstSts  = 'Inactive';
             outVstDUd  = 'UnDelete';
          Other;
             outVstSts  = *blanks;
          ENDSL;
          IF (VIstat  = 'A') and (Vivsdt <= Today);
             outVstAlrt = *on;
          ELSE;
             outVstAlrt = *off;
          ENDIF;

          IF onLoadRRN = 0;
             onLoadRRN  = outVstRRN;
          ENDIF;
          IF onLoadRRN = outVstRRN;
             SelVstRow = *on;
          ELSE;
             SelVstRow = *off;
          ENDIF;
          edtVstDtl = *off;
          scDelVst = *off;

          rrnVst = rrnVst + 1;
          //*** Beg Add ***** 10/17/22 ****************************
          ChkStack('WTVSTTRK':CallPgm);
          //*** Beg Chg ***** 08/02/23 ****************************
          //***If rrnVst >= 4 and callPgm = *on;
          //*** Beg Chg ***** 09/28/23 ****************************
          //***If rrnVst >= 9999 and callPgm = *on;
          If rrnVst >= 9999 ;
          //*** End Chg ***** 09/28/23 ****************************
          //*** End Chg ***** 08/02/23 ****************************
             If isSubset9999 = *Off;
                //*** Beg Chg ***** 09/28/23 **********************
                //***str9999msg = 'There are too many records for Service ' +
                //*** 'Request Tracking. ' +
                //*** 'Click on Results to see first 9999 records Or, ' +
                //*** 'click Filter to narrow down your search results.';
                //***isFilter = PopTwoBtn(str9999msg:'Service Request Tracking':
                //*** 'Filter':'Results');
                If callPgm = *on;
                   str9999msg = 'There are too many records for Service ' +
                    'Request Tracking. ' +
                    'Click on Results to see first 9999 records Or, ' +
                    'click Filter to narrow down your search results.';
                   isFilter = PopTwoBtn(str9999msg:'Service Request Tracking':
                    'Filter':'Results');
                Else;
                   str9999msg = 'There are more than 9999 records for Service '+
                    'Request Tracking (under "Service Request" tab). ' +
                    'As of now you can see first 9999 records in "Service ' +
                    'Request Tracking". Please use Filter option to narrow ' +
                    'down your search results.';
                   isFilter = PopOneBtn(str9999msg:'Service Request Tracking':
                    'Ok');
                Endif;
                //*** End Chg ***** 09/28/23 **********************
                isSubset9999 = *On;
             Else;
                isFilter = *Off;
             Endif;
             isOver9999 = *On;
          Else;
          //*** End Add ***** 10/17/22 ****************************
             WRITE sflVstTrk;
          //*** Beg Add ***** 10/17/22 ****************************
          Endif;
          //*** End Add ***** 10/17/22 ****************************
       Endsr;

      /end-free
     p LoadVisitGrid   e

       //===================================================================
       // OverrideEmp - Override Employer Files and Open Files
       //===================================================================
     p OverrideEmp     b
     d OverrideEmp     pi
     d  quoteType                     1

     d mode            s              1

      /free
       If empUsrOpn = *On;
          mode = quoteType;

          // Override Employer Files  IF Quote Employer
          IF mode = 'Q';
             strCommand = 'OVRDBF FILE(WDELP) TOFILE(' +
              %trim(RtvQuoteLib('*QTE')) + '/WDELP)';
             Callp QCmdExc(strCommand:%len(strCommand));

          //*** Begin Add *** 02/10/16 **********************************
             strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
              %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
             Callp QCmdExc(strCommand:%len(strCommand));
          //*** End   Add *** 02/10/16 **********************************

             strCommand = 'OVRDBF FILE(WMEML) TOFILE(' +
              %trim(RtvQuoteLib('*QTE')) + '/WMEML)';
             Callp QCmdExc(strCommand:%len(strCommand));

             strCommand = 'OVRDBF FILE(WMALL) TOFILE(' +
              %trim(RtvQuoteLib('*QTE')) + '/WMALL)';
             Callp QCmdExc(strCommand:%len(strCommand));
          ENDIF;

          // Open Employer Files. Close first, if already open.
          //*** Begin Chg *** 02/10/16 **********************************
          //****IF not %open(WDELP);
          IF %open(WDELP);
             Close WDELP;
          ENDIF;
             Open WDELP;
          //****ENDIF;

          //*** Begin Add *** 02/10/16 **********************************
          IF %open(WDELLQ);
             Close WDELLQ;
          ENDIF;
             Open WDELLQ;
          //*** End   Add *** 02/10/16 **********************************

          //****IF not %Open(WMEML);
          IF %Open(WMEML);
             Close WMEML;
          ENDIF;
             Open WMEML;
          //****ENDIF;

          //****IF not %Open(WMALL);
          IF %Open(WMALL);
             Close WMALL;
          ENDIF;
             Open WMALL;
          //****ENDIF;
       EndIf;

      /end-free

     p OverrideEmp     e

       //*************************************************************
       //* Plot Visit Add Header
       //*************************************************************
     p PlotVisitAdd    b
     d PlotVisitAdd    pi

     d  xCounter       s              2  0
      /free
       fSetVstAdd = *on;
       pnlVstHdrX = 255;
       pnlVstHdrY = 135;
       fSetVstDtx = pnlVstHdrX;
       fSetVstDty = pnlVstHdrY;
       outVarX = fSetVstDtX +10;
       outVarY = fSetVstDty +20;

       outVarY +=10;
       LblVstTypX = outVarX;
       LblVstTypY = outVarY;
       dtlVstTypX = LblVstTypX;
       dtlVstTypY = LblVstTypY +15;
       fSetVstTyp = *on;
       dtlVstTypS = 'VICAT = ''' + %trim(DtlVstCat) + ''' AND VISTAT=''A''';

       // *** Beg Chg *** 04/05/23 *************************************
       //  LblVstTyRX = LblVstTypX +60;
       LblVstTyRX = LblVstTypX +130;
       // *** End Chg *** 04/05/23 *************************************
       LblVstTyRY = LblVstTypY;

       outVarY += 35;
       fSetCxlX = outVarX +800;
       fSetCxlY = outVarY;
       outVarY = outVarY +45;
       fSetVstDtH = outVarY-fSetVstDty +20;
       pnlVstHdrH = fSetVstDtH +20;

      /end-free
     p PlotVisitAdd    e

       //*************************************************************
       //* Plot Visit Entry Detail
       //*************************************************************
     p PlotVisitDtl    b
     d PlotVisitDtl    pi
     d  inMode                       10    const Options(*nopass)

     d  xCounter       s              2  0
     d  xOffSet        s              3  0
      /free

       xCounter = 0;
       xOffSet = 190;
       exsr ResetVisitDtlXY;

       // Only build for fields specified in WTVIP
       CHAIN (VICO#:VICAT:VITYPE) wtvil;
       IF %found(wtvil);

          IF VIDTHD1 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte01i = *on;
             outDte01Lx = outVarX + xCounter*xOffSet;
             outDte01Ly = outVarY;
             outDte01L = %trim(VIDTHD1);
             outDte01Vx = outDte01Lx;
             outDte01Vy = outDte01Ly+15;
             outDte01V = dtMDYY(VIdt01);
             IF VIDTRQ1 = 'Y';
                outDte01R = *on;
                outDte01Rx = outDte01Lx+ %len(%trim(outDte01L))*7 +5;
                outDte01Ry = outDte01Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD2 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte02i = *on;
             outDte02Lx = outVarX + xCounter*xOffSet;
             outDte02Ly = outVarY;
             outDte02L = %trim(VIDTHD2);
             outDte02Vx = outDte02Lx;
             outDte02Vy = outDte02Ly+15;
             outDte02V = dtMDYY(VIdt02);
             IF VIDTRQ2 = 'Y';
                outDte02R = *on;
                outDte02Rx = outDte02Lx+%len(%trim(outDte02L))*7 +5;
                outDte02Ry = outDte02Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD3 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte03i = *on;
             outDte03Lx = outVarX + xCounter*xOffSet;
             outDte03Ly = outVarY;
             outDte03L = %trim(VIDTHD3);
             outDte03Vx = outDte03Lx;
             outDte03Vy = outDte03Ly+15;
             outDte03V = dtMDYY(VIdt03);
             IF VIDTRQ3 = 'Y';
                outDte03R = *on;
                outDte03Rx = outDte03Lx+%len(%trim(outDte03L))*7 +5;
                outDte03Ry = outDte03Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD4 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte04i = *on;
             outDte04Lx = outVarX + xCounter*xOffSet;
             outDte04Ly = outVarY;
             outDte04L = %trim(VIDTHD4);
             outDte04Vx = outDte04Lx;
             outDte04Vy = outDte04Ly+15;
             outDte04V = dtMDYY(VIdt04);
             IF VIDTRQ4 = 'Y';
                outDte04R = *on;
                outDte04Rx = outDte04Lx+%len(%trim(outDte04L))*7 +5;
                outDte04Ry = outDte04Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD5 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte05i = *on;
             outDte05Lx = outVarX + xCounter*xOffSet;
             outDte05Ly = outVarY;
             outDte05L = %trim(VIDTHD5);
             outDte05Vx = outDte05Lx;
             outDte05Vy = outDte05Ly+15;
             outDte05V = dtMDYY(VIdt05);
             IF VIDTRQ5 = 'Y';
                outDte05R = *on;
                outDte05Rx = outDte05Lx+%len(%trim(outDte05L))*7 +5;
                outDte05Ry = outDte05Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD6 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte06i = *on;
             outDte06Lx = outVarX + xCounter*xOffSet;
             outDte06Ly = outVarY;
             outDte06L = %trim(VIDTHD6);
             outDte06Vx = outDte06Lx;
             outDte06Vy = outDte06Ly+15;
             outDte06V = dtMDYY(VIdt06);
             IF VIDTRQ6 = 'Y';
                outDte06R = *on;
                outDte06Rx = outDte06Lx+%len(%trim(outDte06L))*7 +5;
                outDte06Ry = outDte06Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD7 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte07i = *on;
             outDte07Lx = outVarX + xCounter*xOffSet;
             outDte07Ly = outVarY;
             outDte07L = %trim(VIDTHD7);
             outDte07Vx = outDte07Lx;
             outDte07Vy = outDte07Ly+15;
             outDte07V = dtMDYY(VIdt07);
             IF VIDTRQ7 = 'Y';
                outDte07R = *on;
                outDte07Rx = outDte07Lx+%len(%trim(outDte07L))*7 +5;
                outDte07Ry = outDte07Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD8 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte08i = *on;
             outDte08Lx = outVarX + xCounter*xOffSet;
             outDte08Ly = outVarY;
             outDte08L = %trim(VIDTHD8);
             outDte08Vx = outDte08Lx;
             outDte08Vy = outDte08Ly+15;
             outDte08V = dtMDYY(VIdt08);
             IF VIDTRQ8 = 'Y';
                outDte08R = *on;
                outDte08Rx = outDte08Lx+%len(%trim(outDte08L))*7 +5;
                outDte08Ry = outDte08Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD9 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte09i = *on;
             outDte09Lx = outVarX + xCounter*xOffSet;
             outDte09Ly = outVarY;
             outDte09L = %trim(VIDTHD9);
             outDte09Vx = outDte09Lx;
             outDte09Vy = outDte09Ly+15;
             outDte09V = dtMDYY(VIdt09);
             IF VIDTRQ9 = 'Y';
                outDte09R = *on;
                outDte09Rx = outDte09Lx+%len(%trim(outDte09L))*7 +5;
                outDte09Ry = outDte09Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDTHD10<> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDte10i = *on;
             outDte10Lx = outVarX + xCounter*xOffSet;
             outDte10Ly = outVarY;
             outDte10L = %trim(VIDTHD10);
             outDte10Vx = outDte10Lx;
             outDte10Vy = outDte10Ly+15;
             outDte10V = dtMDYY(VIdt10);
             IF VIDTRQ10 = 'Y';
                outDte10R = *on;
                outDte10Rx = outDte10Lx+%len(%trim(outDte10L))*7 +5;
                outDte10Ry = outDte10Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VICBHD1 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk01I = *on;
             outChk01Vx = outVarX + xCounter*xOffSet;
             outChk01Vy = outVarY + 15;
             outChk01L = %trim(VICBHD1);
             outChk01V = VICB01;
             xcounter+=1;
          ENDIF;

          IF VICBHD2 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk02I = *on;
             outChk02Vx = outVarX + xCounter*xOffSet;
             outChk02Vy = outVarY + 15;
             outChk02L = %trim(VICBHD2);
             outChk02V = VICB02;
             xcounter+=1;
          ENDIF;

          IF VICBHD3 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk03I = *on;
             outChk03Vx = outVarX + xCounter*xOffSet;
             outChk03Vy = outVarY + 15;
             outChk03L = %trim(VICBHD3);
             outChk03V = VICB03;
             xcounter+=1;
          ENDIF;

          IF VICBHD4 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk04I = *on;
             outChk04Vx = outVarX + xCounter*xOffSet;
             outChk04Vy = outVarY + 15;
             outChk04L = %trim(VICBHD4);
             outChk04V = VICB04;
             xcounter+=1;
          ENDIF;

          IF VICBHD5 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk05I = *on;
             outChk05Vx = outVarX + xCounter*xOffSet;
             outChk05Vy = outVarY + 15;
             outChk05L = %trim(VICBHD5);
             outChk05V = VICB05;
             xcounter+=1;
          ENDIF;

          IF VICBHD6 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk06I = *on;
             outChk06Vx = outVarX + xCounter*xOffSet;
             outChk06Vy = outVarY + 15;
             outChk06L = %trim(VICBHD6);
             outChk06V = VICB06;
             xcounter+=1;
          ENDIF;

          IF VICBHD7 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk07I = *on;
             outChk07Vx = outVarX + xCounter*xOffSet;
             outChk07Vy = outVarY + 15;
             outChk07L = %trim(VICBHD7);
             outChk07V = VICB07;
             xcounter+=1;
          ENDIF;

          IF VICBHD8 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk08I = *on;
             outChk08Vx = outVarX + xCounter*xOffSet;
             outChk08Vy = outVarY + 15;
             outChk08L = %trim(VICBHD8);
             outChk08V = VICB08;
             xcounter+=1;
          ENDIF;

          IF VICBHD9 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk09I = *on;
             outChk09Vx = outVarX + xCounter*xOffSet;
             outChk09Vy = outVarY + 15;
             outChk09L = %trim(VICBHD9);
             outChk09V = VICB09;
             xcounter+=1;
          ENDIF;

          IF VICBHD10 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outChk10I = *on;
             outChk10Vx = outVarX + xCounter*xOffSet;
             outChk10Vy = outVarY + 15;
             outChk10L = %trim(VICBHD10);
             outChk10V = VICB10;
             xcounter+=1;
          ENDIF;

          IF VIINHD1 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg01i = *on;
             outItg01Lx = outVarX + xCounter*xOffSet;
             outItg01Ly = outVarY;
             outItg01L = %trim(VIINHD1 );
             outItg01Vx = outItg01Lx;
             outItg01Vy = outItg01Ly+15;
             outItg01V = VIINT01;
             IF VIINRQ1 = 'Y';
                outItg01R = *on;
                outItg01Rx = outItg01Lx+%len(%trim(outItg01L))*7 +5;
                outItg01Ry = outItg01Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD2 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg02i = *on;
             outItg02Lx = outVarX + xCounter*xOffSet;
             outItg02Ly = outVarY;
             outItg02L = %trim(VIINHD2 );
             outItg02Vx = outItg02Lx;
             outItg02Vy = outItg02Ly+15;
             outItg02V = VIINT02;
             IF VIINRQ2 = 'Y';
                outItg02R = *on;
                outItg02Rx = outItg02Lx+%len(%trim(outItg02L))*7 +5;
                outItg02Ry = outItg02Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD3 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg03i = *on;
             outItg03Lx = outVarX + xCounter*xOffSet;
             outItg03Ly = outVarY;
             outItg03L = %trim(VIINHD3 );
             outItg03Vx = outItg03Lx;
             outItg03Vy = outItg03Ly+15;
             outItg03V = VIINT03;
             IF VIINRQ3 = 'Y';
                outItg03R = *on;
                outItg03Rx = outItg03Lx+%len(%trim(outItg03L))*7 +5;
                outItg03Ry = outItg03Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD4 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg04i = *on;
             outItg04Lx = outVarX + xCounter*xOffSet;
             outItg04Ly = outVarY;
             outItg04L = %trim(VIINHD4 );
             outItg04Vx = outItg04Lx;
             outItg04Vy = outItg04Ly+15;
             outItg04V = VIINT04;
             IF VIINRQ4 = 'Y';
                outItg04R = *on;
                outItg04Rx = outItg04Lx+%len(%trim(outItg04L))*7 +5;
                outItg04Ry = outItg04Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD5 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg05i = *on;
             outItg05Lx = outVarX + xCounter*xOffSet;
             outItg05Ly = outVarY;
             outItg05L = %trim(VIINHD5 );
             outItg05Vx = outItg05Lx;
             outItg05Vy = outItg05Ly+15;
             outItg05V = VIINT05;
             IF VIINRQ5 = 'Y';
                outItg05R = *on;
                outItg05Rx = outItg05Lx+%len(%trim(outItg05L))*7 +5;
                outItg05Ry = outItg05Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD6 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg06i = *on;
             outItg06Lx = outVarX + xCounter*xOffSet;
             outItg06Ly = outVarY;
             outItg06L = %trim(VIINHD6 );
             outItg06Vx = outItg06Lx;
             outItg06Vy = outItg06Ly+15;
             outItg06V = VIINT06;
             IF VIINRQ6 = 'Y';
                outItg06R = *on;
                outItg06Rx = outItg06Lx+%len(%trim(outItg06L))*7 +5;
                outItg06Ry = outItg06Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD7 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg07i = *on;
             outItg07Lx = outVarX + xCounter*xOffSet;
             outItg07Ly = outVarY;
             outItg07L = %trim(VIINHD7 );
             outItg07Vx = outItg07Lx;
             outItg07Vy = outItg07Ly+15;
             outItg07V = VIINT07;
             IF VIINRQ7 = 'Y';
                outItg07R = *on;
                outItg07Rx = outItg07Lx+%len(%trim(outItg07L))*7 +5;
                outItg07Ry = outItg07Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD8 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg08i = *on;
             outItg08Lx = outVarX + xCounter*xOffSet;
             outItg08Ly = outVarY;
             outItg08L = %trim(VIINHD8 );
             outItg08Vx = outItg08Lx;
             outItg08Vy = outItg08Ly+15;
             outItg08V = VIINT08;
             IF VIINRQ8 = 'Y';
                outItg08R = *on;
                outItg08Rx = outItg08Lx+%len(%trim(outItg08L))*7 +5;
                outItg08Ry = outItg08Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD9 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg09i = *on;
             outItg09Lx = outVarX + xCounter*xOffSet;
             outItg09Ly = outVarY;
             outItg09L = %trim(VIINHD9 );
             outItg09Vx = outItg09Lx;
             outItg09Vy = outItg09Ly+15;
             outItg09V = VIINT09;
             IF VIINRQ9 = 'Y';
                outItg09R = *on;
                outItg09Rx = outItg09Lx+%len(%trim(outItg09L))*7 +5;
                outItg09Ry = outItg09Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIINHD10<> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outItg10i = *on;
             outItg10Lx = outVarX + xCounter*xOffSet;
             outItg10Ly = outVarY;
             outItg10L = %trim(VIINHD10);
             outItg10Vx = outItg10Lx;
             outItg10Vy = outItg10Ly+15;
             outItg10V = VIINT10;
             IF VIINRQ10 = 'Y';
                outItg10R = *on;
                outItg10Rx = outItg10Lx+%len(%trim(outItg10L))*7 +5;
                outItg10Ry = outItg10Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD1 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec01i = *on;
             outDec01Lx = outVarX + xCounter*xOffSet;
             outDec01Ly = outVarY;
             outDec01L = %trim(VIDCHD1 );
             outDec01Vx = outDec01Lx;
             outDec01Vy = outDec01Ly+15;
             outDec01V = VIDEC01;
             IF VIDCRQ1 = 'Y';
                outDec01R = *on;
                outDec01Rx = outDec01Lx+%len(%trim(outDec01L))*7 +5;
                outDec01Ry = outDec01Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD2 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec02i = *on;
             outDec02Lx = outVarX + xCounter*xOffSet;
             outDec02Ly = outVarY;
             outDec02L = %trim(VIDCHD2 );
             outDec02Vx = outDec02Lx;
             outDec02Vy = outDec02Ly+15;
             outDec02V = VIDEC02;
             IF VIDCRQ2 = 'Y';
                outDec02R = *on;
                outDec02Rx = outDec02Lx+%len(%trim(outDec02L))*7 +5;
                outDec02Ry = outDec02Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD3 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec03i = *on;
             outDec03Lx = outVarX + xCounter*xOffSet;
             outDec03Ly = outVarY;
             outDec03L = %trim(VIDCHD3 );
             outDec03Vx = outDec03Lx;
             outDec03Vy = outDec03Ly+15;
             outDec03V = VIDEC03;
             IF VIDCRQ3 = 'Y';
                outDec03R = *on;
                outDec03Rx = outDec03Lx+%len(%trim(outDec03L))*7 +5;
                outDec03Ry = outDec03Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD4 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec04i = *on;
             outDec04Lx = outVarX + xCounter*xOffSet;
             outDec04Ly = outVarY;
             outDec04L = %trim(VIDCHD4 );
             outDec04Vx = outDec04Lx;
             outDec04Vy = outDec04Ly+15;
             outDec04V = VIDEC04;
             IF VIDCRQ4 = 'Y';
                outDec04R = *on;
                outDec04Rx = outDec04Lx+%len(%trim(outDec04L))*7 +5;
                outDec04Ry = outDec04Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD5 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec05i = *on;
             outDec05Lx = outVarX + xCounter*xOffSet;
             outDec05Ly = outVarY;
             outDec05L = %trim(VIDCHD5 );
             outDec05Vx = outDec05Lx;
             outDec05Vy = outDec05Ly+15;
             outDec05V = VIDEC05;
             IF VIDCRQ5 = 'Y';
                outDec05R = *on;
                outDec05Rx = outDec05Lx+%len(%trim(outDec05L))*7 +5;
                outDec05Ry = outDec05Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD6 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec06i = *on;
             outDec06Lx = outVarX + xCounter*xOffSet;
             outDec06Ly = outVarY;
             outDec06L = %trim(VIDCHD6 );
             outDec06Vx = outDec06Lx;
             outDec06Vy = outDec06Ly+15;
             outDec06V = VIDEC06;
             IF VIDCRQ6 = 'Y';
                outDec06R = *on;
                outDec06Rx = outDec06Lx+%len(%trim(outDec06L))*7 +5;
                outDec06Ry = outDec06Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD7 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec07i = *on;
             outDec07Lx = outVarX + xCounter*xOffSet;
             outDec07Ly = outVarY;
             outDec07L = %trim(VIDCHD7 );
             outDec07Vx = outDec07Lx;
             outDec07Vy = outDec07Ly+15;
             outDec07V = VIDEC07;
             IF VIDCRQ7 = 'Y';
                outDec07R = *on;
                outDec07Rx = outDec07Lx+%len(%trim(outDec07L))*7 +5;
                outDec07Ry = outDec07Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD8 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec08i = *on;
             outDec08Lx = outVarX + xCounter*xOffSet;
             outDec08Ly = outVarY;
             outDec08L = %trim(VIDCHD8 );
             outDec08Vx = outDec08Lx;
             outDec08Vy = outDec08Ly+15;
             outDec08V = VIDEC08;
             IF VIDCRQ8 = 'Y';
                outDec08R = *on;
                outDec08Rx = outDec08Lx+%len(%trim(outDec08L))*7 +5;
                outDec08Ry = outDec08Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD9 <> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec09i = *on;
             outDec09Lx = outVarX + xCounter*xOffSet;
             outDec09Ly = outVarY;
             outDec09L = %trim(VIDCHD9 );
             outDec09Vx = outDec09Lx;
             outDec09Vy = outDec09Ly+15;
             outDec09V = VIDEC09;
             IF VIDCRQ9 = 'Y';
                outDec09R = *on;
                outDec09Rx = outDec09Lx+%len(%trim(outDec09L))*7 +5;
                outDec09Ry = outDec09Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          IF VIDCHD10<> *blanks;
             IF xcounter>4;
                xcounter = 0;
                outVarY += 35;
             ENDIF;
             outDec10i = *on;
             outDec10Lx = outVarX + xCounter*xOffSet;
             outDec10Ly = outVarY;
             outDec10L = %trim(VIDCHD10);
             outDec10Vx = outDec10Lx;
             outDec10Vy = outDec10Ly+15;
             outDec10V = VIDEC10;
             IF VIDCRQ10 = 'Y';
                outDec10R = *on;
                outDec10Rx = outDec10Lx+%len(%trim(outDec10L))*7 +5;
                outDec10Ry = outDec10Ly;
             ENDIF;
             xcounter+=1;
          ENDIF;

          outVarY += 20;

          IF VITFHD1 <> *blanks;
             outVarY += 25;
             outTxt01i = *on;
             outTxt01Lx = outVarX;
             outTxt01Ly = outVarY;
             outTxt01L = %trim(VITFHD1 );
             outVarY += 15;
             outTxt01Vx = outTxt01Lx;
             outTxt01Vy = outVarY;
             outTxt01V = %trim(VITF01);
             IF VITFRQ1 = 'Y';
                outTxt01r = *on;
                outTxt01Rx = outTxt01Lx +150;
                outTxt01Ry = outTxt01Ly;
             ENDIF;
          ENDIF;

          IF VITFHD2 <> *blanks;
             outVarY += 25;
             outTxt02i = *on;
             outTxt02Lx = outVarX;
             outTxt02Ly = outVarY;
             outTxt02L = %trim(VITFHD2 );
             outVarY += 15;
             outTxt02Vx = outTxt02Lx;
             outTxt02Vy = outVarY;
             outTxt02V = %trim(VITF02);
             IF VITFRQ2 = 'Y';
                outTxt02r = *on;
                outTxt02Rx = outTxt02Lx +150;
                outTxt02Ry = outTxt02Ly;
             ENDIF;
          ENDIF;

          IF VITFHD3 <> *blanks;
             outVarY += 25;
             outTxt03i = *on;
             outTxt03Lx = outVarX;
             outTxt03Ly = outVarY;
             outTxt03L = %trim(VITFHD3 );
             outVarY += 15;
             outTxt03Vx = outTxt03Lx;
             outTxt03Vy = outVarY;
             outTxt03V = %trim(VITF03);
             IF VITFRQ3 = 'Y';
                outTxt03r = *on;
                outTxt03Rx = outTxt03Lx +150;
                outTxt03Ry = outTxt03Ly;
             ENDIF;
          ENDIF;

          IF VITFHD4 <> *blanks;
             outVarY += 25;
             outTxt04i = *on;
             outTxt04Lx = outVarX;
             outTxt04Ly = outVarY;
             outTxt04L = %trim(VITFHD4 );
             outVarY += 15;
             outTxt04Vx = outTxt04Lx;
             outTxt04Vy = outVarY;
             outTxt04V = %trim(VITF04);
             IF VITFRQ4 = 'Y';
                outTxt04r = *on;
                outTxt04Rx = outTxt04Lx +150;
                outTxt04Ry = outTxt04Ly;
             ENDIF;
          ENDIF;

          IF VITFHD5 <> *blanks;
             outVarY += 25;
             outTxt05i = *on;
             outTxt05Lx = outVarX;
             outTxt05Ly = outVarY;
             outTxt05L = %trim(VITFHD5 );
             outVarY += 15;
             outTxt05Vx = outTxt05Lx;
             outTxt05Vy = outVarY;
             outTxt05V = %trim(VITF05);
             IF VITFRQ5 = 'Y';
                outTxt05r = *on;
                outTxt05Rx = outTxt05Lx +150;
                outTxt05Ry = outTxt05Ly;
             ENDIF;
          ENDIF;

          IF VITFHD6 <> *blanks;
             outVarY += 25;
             outTxt06i = *on;
             outTxt06Lx = outVarX;
             outTxt06Ly = outVarY;
             outTxt06L = %trim(VITFHD6 );
             outVarY += 15;
             outTxt06Vx = outTxt06Lx;
             outTxt06Vy = outVarY;
             outTxt06V = %trim(VITF06);
             selTxt06  = 'GTTBID=''' + VItfTb6 + ''' AND GTSTAT<>''I''';
             IF VITFRQ6 = 'Y';
                outTxt06r = *on;
                outTxt06Rx = outTxt06Lx +150;
                outTxt06Ry = outTxt06Ly;
             ENDIF;
          ENDIF;

          IF VITFHD7 <> *blanks;
             outVarY += 25;
             outTxt07i = *on;
             outTxt07Lx = outVarX;
             outTxt07Ly = outVarY;
             outTxt07L = %trim(VITFHD7 );
             outVarY += 15;
             outTxt07Vx = outTxt07Lx;
             outTxt07Vy = outVarY;
             outTxt07V = %trim(VITF07);
             selTxt07  = 'GTTBID=''' + VItfTb7 + ''' AND GTSTAT<>''I''';
             IF VITFRQ7 = 'Y';
                outTxt07r = *on;
                outTxt07Rx = outTxt07Lx +150;
                outTxt07Ry = outTxt07Ly;
             ENDIF;
          ENDIF;

          IF VITFHD8 <> *blanks;
             outVarY += 25;
             outTxt08i = *on;
             outTxt08Lx = outVarX;
             outTxt08Ly = outVarY;
             outTxt08L = %trim(VITFHD8 );
             outVarY += 15;
             outTxt08Vx = outTxt08Lx;
             outTxt08Vy = outVarY;
             outTxt08V = %trim(VITF08);
             selTxt08  = 'GTTBID=''' + VItfTb8 + ''' AND GTSTAT<>''I''';
             IF VITFRQ8 = 'Y';
                outTxt08r = *on;
                outTxt08Rx = outTxt08Lx +150;
                outTxt08Ry = outTxt08Ly;
             ENDIF;
          ENDIF;

          IF VITFHD9 <> *blanks;
             outVarY += 25;
             outTxt09i = *on;
             outTxt09Lx = outVarX;
             outTxt09Ly = outVarY;
             outTxt09L = %trim(VITFHD9 );
             outVarY += 15;
             outTxt09Vx = outTxt09Lx;
             outTxt09Vy = outVarY;
             outTxt09V = %trim(VITF09);
             selTxt09  = 'GTTBID=''' + VItfTb9 + ''' AND GTSTAT<>''I''';
             IF VITFRQ9 = 'Y';
                outTxt09r = *on;
                outTxt09Rx = outTxt09Lx +150;
                outTxt09Ry = outTxt09Ly;
             ENDIF;
          ENDIF;

          IF VITFHD10<> *blanks;
             outVarY += 25;
             outTxt10i = *on;
             outTxt10Lx = outVarX;
             outTxt10Ly = outVarY;
             outTxt10L = %trim(VITFHD10);
             outVarY += 15;
             outTxt10Vx = outTxt10Lx;
             outTxt10Vy = outVarY;
             outTxt10V = %trim(VITF10);
             selTxt10  = 'GTTBID=''' + VItfTb10 + ''' AND GTSTAT<>''I''';
             IF VITFRQ10 = 'Y';
                outTxt10r = *on;
                outTxt10Rx = outTxt10Lx +150;
                outTxt10Ry = outTxt10Ly;
             ENDIF;
          ENDIF;

       ENDIF;

       IF (inMode <> 'View');
          outVarY += 35;
          fSetSavX = outVarX + 700;
          fSetSavY = outVarY;
          fSetCxlX = outVarX + 800;
          fSetCxlY = outVarY;
          fSetVstDtH = outVarY-fSetVstDty + 20;
          pnlVstHdrH = fSetVstDtH + 20;
       ELSE;
          fSetVstDtH = outVarY + 25;
       ENDIF;


       //*************************************************************
       //* Reset all x/y coordinates to 0,0
       //*************************************************************
       Begsr ResetVisitDtlXY;

          outDte01LX=0;
          outDte01LY=0;
          outDte01VX=0;
          outDte01VY=0;
          outDte02LX=0;
          outDte02LY=0;
          outDte02VX=0;
          outDte02VY=0;
          outDte03LX=0;
          outDte03LY=0;
          outDte03VX=0;
          outDte03VY=0;
          outDte04LX=0;
          outDte04LY=0;
          outDte04VX=0;
          outDte04VY=0;
          outDte05LX=0;
          outDte05LY=0;
          outDte05VX=0;
          outDte05VY=0;
          outDte06LX=0;
          outDte06LY=0;
          outDte06VX=0;
          outDte06VY=0;
          outDte07LX=0;
          outDte07LY=0;
          outDte07VX=0;
          outDte07VY=0;
          outDte08LX=0;
          outDte08LY=0;
          outDte08VX=0;
          outDte08VY=0;
          outDte09LX=0;
          outDte09LY=0;
          outDte09VX=0;
          outDte09VY=0;
          outDte10LX=0;
          outDte10LY=0;
          outDte10VX=0;
          outDte10VY=0;

          outChk01VX=0;
          outChk01VY=0;
          outChk02VX=0;
          outChk02VY=0;
          outChk03VX=0;
          outChk03VY=0;
          outChk04VX=0;
          outChk04VY=0;
          outChk05VX=0;
          outChk05VY=0;
          outChk06VX=0;
          outChk06VY=0;
          outChk07VX=0;
          outChk07VY=0;
          outChk08VX=0;
          outChk08VY=0;
          outChk09VX=0;
          outChk09VY=0;
          outChk10VX=0;
          outChk10VY=0;

          outItg01LX=0;
          outItg01LY=0;
          outItg01VX=0;
          outItg01VY=0;
          outItg02LX=0;
          outItg02LY=0;
          outItg02VX=0;
          outItg02VY=0;
          outItg03LX=0;
          outItg03LY=0;
          outItg03VX=0;
          outItg03VY=0;
          outItg04LX=0;
          outItg04LY=0;
          outItg04VX=0;
          outItg04VY=0;
          outItg05LX=0;
          outItg05LY=0;
          outItg05VX=0;
          outItg05VY=0;
          outItg06LX=0;
          outItg06LY=0;
          outItg06VX=0;
          outItg06VY=0;
          outItg07LX=0;
          outItg07LY=0;
          outItg07VX=0;
          outItg07VY=0;
          outItg08LX=0;
          outItg08LY=0;
          outItg08VX=0;
          outItg08VY=0;
          outItg09LX=0;
          outItg09LY=0;
          outItg09VX=0;
          outItg09VY=0;
          outItg10LX=0;
          outItg10LY=0;
          outItg10VX=0;
          outItg10VY=0;

          outDec01LX=0;
          outDec01LY=0;
          outDec01VX=0;
          outDec01VY=0;
          outDec02LX=0;
          outDec02LY=0;
          outDec02VX=0;
          outDec02VY=0;
          outDec03LX=0;
          outDec03LY=0;
          outDec03VX=0;
          outDec03VY=0;
          outDec04LX=0;
          outDec04LY=0;
          outDec04VX=0;
          outDec04VY=0;
          outDec05LX=0;
          outDec05LY=0;
          outDec05VX=0;
          outDec05VY=0;
          outDec06LX=0;
          outDec06LY=0;
          outDec06VX=0;
          outDec06VY=0;
          outDec07LX=0;
          outDec07LY=0;
          outDec07VX=0;
          outDec07VY=0;
          outDec08LX=0;
          outDec08LY=0;
          outDec08VX=0;
          outDec08VY=0;
          outDec09LX=0;
          outDec09LY=0;
          outDec09VX=0;
          outDec09VY=0;
          outDec10LX=0;
          outDec10LY=0;
          outDec10VX=0;
          outDec10VY=0;

          outTxt01LX=0;
          outTxt01LY=0;
          outTxt01VX=0;
          outTxt01VY=0;
          outTxt02LX=0;
          outTxt02LY=0;
          outTxt02VX=0;
          outTxt02VY=0;
          outTxt03LX=0;
          outTxt03LY=0;
          outTxt03VX=0;
          outTxt03VY=0;
          outTxt04LX=0;
          outTxt04LY=0;
          outTxt04VX=0;
          outTxt04VY=0;
          outTxt05LX=0;
          outTxt05LY=0;
          outTxt05VX=0;
          outTxt05VY=0;
          outTxt06LX=0;
          outTxt06LY=0;
          outTxt06VX=0;
          outTxt06VY=0;
          outTxt07LX=0;
          outTxt07LY=0;
          outTxt07VX=0;
          outTxt07VY=0;
          outTxt08LX=0;
          outTxt08LY=0;
          outTxt08VX=0;
          outTxt08VY=0;
          outTxt09LX=0;
          outTxt09LY=0;
          outTxt09VX=0;
          outTxt09VY=0;
          outTxt10LX=0;
          outTxt10LY=0;
          outTxt10VX=0;
          outTxt10VY=0;

       ENDSR;

      /end-free
     p PlotVisitDtl    e

       //*************************************************************
       //* Plot Visit Entry Header
       //*************************************************************
     p PlotVisitHdr    b
     d PlotVisitHdr    pi
     d  inMode                       10    const Options(*nopass)

     d  xCounter       s              2  0
     d***** Beg Add *** 08/08/19 *************************************
     d xCurDt70        s              7  0 inz
     d***** End Add *** 08/08/19 *************************************
      /free

       vstTypSet  = *on;
       fSetVstDtl = *on;
       fSetVstAdd = *off;
       SELECT;
       WHEN inMode = 'View';
         fSetVstDtx = vstScreenX;
         fSetVstDty = dtlBoxYoffset;
         outVarX = fSetVstDtX +10;
         outVarY = 265;
       WHEN inMode = 'Edit';
         pnlVstHdrX = vstScreenX+5;
         pnlVstHdrY = 135;
         fSetVstDtx = pnlVstHdrX;
         fSetVstDty = pnlVstHdrY;
         outVarX = fSetVstDtX +10;
         outVarY = fSetVstDty +20;
       WHEN inMode = 'Add';
         pnlVstHdrX = vstScreenX+5;
         pnlVstHdrY = 135;
         fSetVstDtx = pnlVstHdrX;
         fSetVstDty = pnlVstHdrY;
         outVarX = fSetVstDtX +10;
         outVarY = fSetVstDty +20;
       ENDSL;

       IF (inMode = 'View');
         outVarY +=10;
         LblNotesX  = outVarX;
         LblNotesY  = outVarY;
         outVarY +=15;
         dtlVstNotX = outVarX;
         dtlVstNotY = outVarY;
         dtlVstNote = %trim(VINotes);
         outVarY +=120;
       ENDIF;

       outVarY +=10;
       LblVstTypX = outVarX;
       LblVstTypY = outVarY;
       dtlVstTypX = LblVstTypX;
       dtlVstTypY = LblVstTypY +15;
       dtlVstTyp = %trim(VIType);
       fSetVstTyp = *on;

       IF (inMode = 'View' or inMode = 'Edit');
         LblVstCatX = outVarX +350;
         LblVstCatY = outVarY;
         dtlVstCatX = LblVstCatX;
         dtlVstCatY = LblVstCatY +15;
         dtlVstCat = %trim(VICat);
         fSetVstCat = *on;
       ENDIF;

       outVarY +=35;
       LblVstAsnX = outVarX;
       LblVstAsnY = outVarY;
       dtlVstAsnX = LblVstAsnX;
       dtlVstAsnY = LblVstAsnY +15;
       dtlVstAsgn = VIAssign;
       dt2VstAsgn = GetUsrName(VIAssign);
       // *** Beg Chg *** 04/05/23 *************************************
       //  LblVstAsRX = LblVstAsnX+100;
       LblVstAsRX = LblVstAsnX+170;
       // *** End Chg *** 04/05/23 *************************************
       LblVstAsRY = LblVstAsnY;

       LblVstDteX = outVarX +350;
       LblVstDteY = outVarY;
       dtlVstDteX = LblVstDteX;
       dtlVstDteY = LblVstDteY +15;
       dtlVstDte  = dtMDYY(VIVsdt);
       // *** Beg Chg *** 04/05/23 *************************************
       //  LblVstDtRX = LblVstDteX+60;
       LblVstDtRX = LblVstDteX+130;
       // *** End Chg *** 04/05/23 *************************************
       LblVstDtRY = LblVstDteY;

       LblCmpDteX = outVarX +550;
       LblCmpDteY = outVarY;
       dtlCmpDteX = LblCmpDteX;
       dtlCmpDteY = LblCmpDteY +15;
       dtlCmpDte  = dtMDYY(VICmdt);

       //*** Beg Add ***** 09/25/19 *******************************
       dtlVstSeq# = VISeq#;
       //*** End Add ***** 09/25/19 *******************************

       IF (inMode <> 'View');
         //Load the Policy/Claim/Agency header here
         outVarY +=35;
         SELECT;
           WHEN (DtlVstCat = 'Policy');
             overrideEmp(scQteFlag);
             edtVstPol = *on;
             LblEmp#X = outVarX;
             LblEmp#Y = outVarY;
             dtlEmp#X = LblEmp#X;
             dtlEmp#Y = LblEmp#Y +15;
             scEdtEmp# = %editc(VIFND:'X') + %editc(VIEMP#:'X')
                         + %editc(VIFYR:'X');

             LblEmpDivX = outVarX +150;
             LblEmpDivY = outVarY;
             dtlEmpDivX = LblEmpDivX;
             dtlEmpDivY = LblEmpDivY +15;
             LblEmpDiRX = LblEmpDivX+50;
             LblEmpDiRY = LblEmpDivY;
             IF scQteFlag = 'Q';
                scDivFile = %Trim(RtvQuoteLib('*QTE')) + '/WMEMP';
                wdEmpLib  = RtvQuoteLib('*QTE');
             ELSE;
                scDivFile = %Trim(RtvQuoteLib('*LIV')) + '/WMEMP';
                wdEmpLib  = RtvQuoteLib('*LIV');
             ENDIF;
             scEdtDiv = VIDIV;

             //Division Options
             CLEAR scDivOptns;
             scDivOptns = 'DIGITS(EMDIV) || (CASE EMSTRP WHEN ''Y'''           +
               ' THEN ''*'' ELSE '' '' END) || (CASE (SELECT ELRTCD FROM '     ;
             scDivOptns = %trim(scDivOptns) + ' ' + %trim(wdEmpLib) +'/WDELP';
             scDivOptns = %trim(scDivOptns) + ' WHERE ELCO# = WMEMP.EMCO# AND' +
               ' ELFND = WMEMP.EMFND AND ELEMP# = WMEMP.EMEMP#  AND ELDIV = '  +
               ' WMEMP.EMDIV AND ELFYR = (SELECT MAX(ELFYR) FROM ';
             scDivOptns = %trim(scDivOptns) + ' ' + %trim(wdEmpLib) +'/WDELP';
             scDivOptns = %trim(scDivOptns) + ' WHERE ELCO# = WMEMP.EMCO# AND' +
               ' ELFND = WMEMP.EMFND AND ELEMP# = WMEMP.EMEMP# AND ELDIV ='    +
               ' WMEMP.EMDIV)) WHEN '' '' THEN ''-'' ELSE '' ('' || (SELECT'   +
               ' RSJRST FROM WMRSP WHERE RSCO# = WMEMP.EMCO# AND RSRTCD ='     +
               ' (SELECT ELRTCD FROM ';
             scDivOptns = %trim(scDivOptns) + ' ' + %trim(wdEmpLib) +'/WDELP';
             scDivOptns = %trim(scDivOptns) + ' WHERE ELCO# = WMEMP.EMCO# AND' +
               ' ELFND = WMEMP.EMFND AND ELEMP# = WMEMP.EMEMP# AND ELDIV ='    +
               ' WMEMP.EMDIV AND ELFYR = (SELECT  MAX(ELFYR) FROM ';
             scDivOptns = %trim(scDivOptns) + ' ' + %trim(wdEmpLib) +'/WDELP';
             scDivOptns = %trim(scDivOptns) + ' WHERE ELCO# = WMEMP.EMCO# AND' +
               ' ELFND = WMEMP.EMFND  AND ELEMP# = WMEMP.EMEMP# AND ELDIV ='   +
               ' WMEMP.EMDIV))) || '')'' || '' - '' END) || TRIM(EMNAME)';

             IF (inMode = 'Add');
                CLEAR scDivSel;
                scDivSel = 'EMCO# = ' + %char(VICO#) + ' AND EMFND = '
                          + %char(VIFND) + ' AND EMEMP# = ' + %char(VIEMP#);
             ELSE;
                CLEAR scDivSel;
                scDivSel = 'EMCO# = ' + %char(VICO#) + ' AND EMFND = '
                          + %char(VIFND) + ' AND EMEMP# = ' + %char(VIEMP#) +
                          ' AND EMDIV = ' + %char(VIDIV);
             ENDIF;

             //Location / Entity
             LblEmpLocX = outVarX +500;
             LblEmpLocY = outVarY;
             dtlEmpLocX = LblEmpLocX;
             dtlEmpLocY = LblEmpLocY +15;
             IF scQteFlag = 'Q';
                scLocFile = %Trim(RtvQuoteLib('*QTE')) + '/WMALP';
             ELSE;
                scLocFile = %Trim(RtvQuoteLib('*LIV')) + '/WMALP';
             ENDIF;
             scEdtLoc = VILOCID;
             //Location Select
             scLocList='ALELID || ''-'' || TRIM(ALNAME) || ''-'' || ' +
                'CASE WHEN ALPAD1<>'' '' THEN TRIM(ALPAD1) || '' '' || ' +
                'TRIM(ALPAD2) || '' '' || TRIM(ALPCTY) || '', '' || ' +
                'ALPST || '' '' || SUBSTR(DIGITS(ALPZIP),1,5) ELSE ' +
                'TRIM(ALMAD1) || '' '' || ' +
                'TRIM(ALMAD2) || '' '' || TRIM(ALMCTY) || '', '' || ' +
                'ALMST || '' '' || SUBSTR(DIGITS(ALMZIP),1,5) END';
             //Select Where
             IF (inMode = 'Add');
             //***** Beg Dlt *** 08/08/19 **********************************
             //***  scLocSel='ALFND=' + %Subst(scEdtEmp#:1:3) + ' AND ALEMP#=' +
             //***   %Subst(scEdtEmp#:4:9) + ' AND ALDIV=' + %editc(scEdtDiv:'X') +
             //***   ' AND ALNCCI = ''Y''';
             //***  //Enable Location if NCCI reported Location/Entity exist
             //***  vstLocSet = *on;
             //***  SETLL (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             //***  READE (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             //***  DOW not %eof;
             //***     IF ALncci = 'Y';
             //***        vstLocSet = *off;
             //***     ENDIF;
             //***     READE (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             //***  ENDDO;
             //***** End Dlt *** 08/08/19 **********************************

             //***** Beg Add *** 08/08/19 **********************************
             // Show location irrespective of NCCI flag. Do not show locations
             // that are expired before current date

               xCurDt70 = %dec(%char(%date():*cymd0):7:0);

               scLocSel='ALFND=' + %Subst(scEdtEmp#:1:3) + ' AND ALEMP#=' +
                %Subst(scEdtEmp#:4:9) + ' AND ALDIV=' + %editc(scEdtDiv:'X') +
                ' AND (ALDLDT>=' + %char(xCurDt70) + ' OR ALDLDT=0)';

               vstLocSet = *off;
             //***** End Add *** 08/08/19 **********************************
             ELSE;
               scLocSel='ALFND=' + %Subst(scEdtEmp#:1:3) + ' AND ALEMP#=' +
                %Subst(scEdtEmp#:4:9) + ' AND ALDIV=' + %editc(scEdtDiv:'X') +
                ' AND ALELID = ' + VILOCID;
               vstLocSet = *on;
             ENDIF;
             delOverrideEmp(scQteFlag);

           WHEN (DtlVstCat = 'Claims');
             edtVstClm = *on;
             LblClm#X = outVarX;
             LblClm#Y = outVarY;
             dtlClm#X = LblClm#X;
             dtlClm#Y = LblClm#Y +15;
             scEdtClm# = %editc(VIFND:'X') + '-' + %editc(VIFYR:'X')
                          + '-' + %editc(VICASE:'X');
             scEdtEmp# = %editc(VIFND:'X') + %editc(VIEMP#:'X')
                         + %editc(VIFYR:'X');
          //*** Begin Add *** 09/09/15 **********************************
             scEdtDiv = VIDIV;
          //*** End   Add *** 09/09/15 **********************************

           WHEN (DtlVstCat = 'Agency');
             edtVstAgt = *on;
             LblAgt#X = outVarX;
             LblAgt#Y = outVarY;
             dtlAgt#X = LblAgt#X;
             dtlAgt#Y = LblAgt#Y +15;
                scEdtAgtDt = %editc(VIFEIN:'X') +'/'+ %editc(VIMOD:'X')
                           +'/'+ %editc(VIagt#:'X');
                scEdtAgt# = %editc(VIagt#:'X');
         ENDSL;
         //followed by Notes here
         outVarY +=35;
         LblNotesX  = outVarX;
         LblNotesY  = outVarY;
         dtlVstNotX = LblNotesX;
         dtlVstNotY = LblNotesY +15;
         dtlVstNote = %trim(VINotes);
         outVarY +=120;
       ENDIF;

       // Start at one x,y coordinate and then manipulate the rest
       outVarY = outVarY + 45;
       IF (inMode = 'View');
          outVarY = 260;
          outVarX = 10;
       ENDIF;

      /end-free
     p PlotVisitHdr    e

       //*************************************************************
       //* BuildData for Policy Detail box
       //*************************************************************
     p PolicyDetail    b
     d PolicyDetail    pi
     d  inCat                         1    const
     d  inCo#                         3  0 const
     d  inGroup                       3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inLoc#                       10    const
     d  inFYR                         3  0 Const
     d  inQte                         1    Const

     d QteFlag         s              1

     d address1        s             40
     d address2        s             40
     d city            s             25
     d state           s              2
     d zipcode         s              9  0

      /free

       IF (inCat = 'P');
         fSetPolDtl = *on;
         dtlBoxYoffset = 855;
       ENDIF;

       QteFlag = inQte;
       overrideEmp(QteFlag);
       CHAIN (inCo#:inGroup:inEmp#:inDiv) pem00 dsWMEML;
       IF %found;
          //*** Begin Add *** 02/10/16 **********************************
         //***outDBANm = GetEmpFullName(inEmp#:inDiv:ProcRrn:inGroup:inCo#:'DBA');
         outDBANm = GetEmpFullName(inEmp#:inDiv:ProcRrn:inGroup:inCo#:'DBA':
                                   inQte);
         //***outFedNm = GetEmpFullName(inEmp#:inDiv:ProcRrn:inGroup:inCo#:'FED');
         outFedNm = GetEmpFullName(inEmp#:inDiv:ProcRrn:inGroup:inCo#:'FED':
                                   inQte);
          //*** Begin Add *** 02/10/16 **********************************
         outEmp# = %editc(inGroup:'X')+'-'+%editc(inEmp#:'X')
                                    +'-'+%editc(inDiv:'X');
        // Load Physical Address.
         address1 = dsWMEML.EMpad1;
         address2 = dsWMEML.EMpad2;
         city     = dsWMEML.EMpCty;
         state    = dsWMEML.EMpSt;
         zipCode  = dsWMEML.EMpZip;
         FormatAddress(address1:address2:city:state:zipCode:AdL1:AdL2:AdL3);
         outPhyLn1  = ADL1;
         outPhyLn2  = ADL2;
         outPhyLn3  = ADL3;
        // Load Mailing Address.
         address1 = dsWMEML.EMmad1;
         address2 = dsWMEML.EMmad2;
         city     = dsWMEML.EMmCty;
         state    = dsWMEML.EMmSt;
         zipCode  = dsWMEML.EMmZip;
         FormatAddress(address1:address2:city:state:zipCode:AdL1:AdL2:AdL3);
         outMlgLn1  = ADL1;
         outMlgLn2  = ADL2;
         outMlgLn3  = ADL3;

         CLEAR pcnKey;
         IF ALpst <> ' ';
            pcnkey = dsWMEML.EMpst + dsWMEML.EMcnty;
         ELSE;
            pcnkey = dsWMEML.EMmst + dsWMEML.EMcnty;
         ENDIF;
         CHAIN (inCo#:pcnKey) pcn00;
         outCounty  = CNdesc;

         outFedID#  = %editc(dsWMEML.EMFEIN:'X');
         outLocEnt# = inLoc#;
         CHAIN (inCo#:'BUSI':dsWMEML.EMTPOB) pgt01;
         IF %found;
            outTypBus = GTdesc;
         ELSE;
            outTypBus = %trim(EMtpob);
         ENDIF;
         outSIC     = dsWMEML.EMSIC;
         outNAICS   = dsWMEML.EMNAIC;
         OutPhone# = dsWMEML.EMAREA*10000000 + dsWMEML.EMPHN#;
         OutFax# = dsWMEML.EMFAXA*10000000 + dsWMEML.EMFAX#;
         CHAIN (inCo#:inGroup:inEmp#:inDiv:inFYR) wmrpl;
         IF %found;
            outCarPol# = RPcap#;
         ENDIF;
         IF (inCat = 'P');
           //Policy Assignments
           CHAIN dsWMEML.EMuwtr PUP01;
           IF %found;
              outUndwrt  = UPusnm;
              IF UPMail <> *blanks;
                 mlPolUW = UPMail;
              ELSE;
                 mtPolUW = Email_NA;
              ENDIF;
           ENDIF;
           CHAIN dsWMEML.EMpsrv PUP01;
           IF %Found;
              outPolSvc  = UPusnm;
              IF UPMail <> *blanks;
                 mlPolServ = UPMail;
              ELSE;
                 mtPolServ = Email_NA;
              ENDIF;
           ENDIF;
           CHAIN dsWMEML.EMpacc PUP01;
           IF %Found;
              outPrmAct  = UPusnm;
              IF UPMail <> *blanks;
                 mlPolPrmAc = UPMail;
              ELSE;
                 mtPolPrmAc = Email_NA;
              ENDIF;
           ENDIF;
           CHAIN dsWMEML.EMlrep PUP01;
           IF %Found;
              outLosCtl = UPusnm;
              IF UPMail <> *blanks;
                 mlPolLsCtr = UPMail;
              ELSE;
                 mtPolLsCtr = Email_NA;
              ENDIF;
           ENDIF;
           CHAIN dsWMEML.EMamgr PUP01;
           IF %Found;
              outAudMgr = UPusnm;
              IF UPMail <> *blanks;
                 mlPolAMgr = UPMail;
              ELSE;
                 mtPolAMgr = Email_NA;
              ENDIF;
           ENDIF;
           CHAIN dsWMEML.EMarvw PUP01;
           IF %Found;
              outAudRvw = UPusnm;
              IF UPMail <> *blanks;
                 mlPolAudRv = UPMail;
              ELSE;
                 mtPolAudRv = Email_NA;
              ENDIF;
           ENDIF;
         ENDIF;
         //Policy Agency Information
         Setgt (inCo#:inGroup:inEmp#:inDiv) Wmahl;
         Readpe (inCo#:inGroup:inEmp#:inDiv) Wmahl;
         IF %found;     //AHagt# = Primary Agent    AHprod=Producer
            outAgency# = %editc(AHagt#:'X');
            CHAIN (inCo#:inGroup:AHagt#) WMaal1;
            AgencyDetail(inCat:inCo#:AAFein:AAMod);
            //Producer
            IF %found(WMaal1);
               CHAIN (inCo#:AAfein:AAmod:AHprod) WMapl;
               IF %found;
                 IF APfsn = ' ';
                    outAgtPrd = %editc(AHprod:'X')+'/'+APlsn;
                 ELSE;
                    ##last = APlsn;
                    ##frst = APfsn;
                    ##term = '1';
                    SrName(##last:##frst:##term);
                    outAgtPrd = %editc(AHprod:'X')+'/'+##last;
                 ENDIF;
                 outPrdEmal = APemal;
                 IF OutPrdEmal <> *blanks;
                    mlPrdEmal = OutPrdEmal;
                 ELSE;
                    mtPrdEmal = Email_NA;
                 ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;
       ENDIF;
       delOverrideEmp(QteFlag);

       RETURN;
      /end-free
     p PolicyDetail    e

       //===================================================================
       // ProcessSFL  - Process Subfile for RowClick, Edit, Delete/Undelete
       //===================================================================
     P ProcessSFL      B
     D ProcessSFL      PI

     D***** Beg Add *** 03/16/16 *******************************************
     D isDel           s               n   inz(*off)
     D***** End Add *** 03/16/16 *******************************************

      /Free

       //**** Beg Add *** 03/16/16 *****************************************
       isDel = *off;
       //**** End Add *** 03/16/16 *****************************************
       ReadC sflVstTrk;
       DOW Not %Eof;
         SELECT;
         WHEN edtVstDtl = *On; // Edit Visit Detail
             editVisitDtl(outVstRRN);
             edtVstDtl = *Off;
         WHEN scDelVst = *On; // Active/Inactive Visit Type
             DeleteUndelete(outVstRRN);
             scDelVst = *Off;
            //**** Beg Add *** 03/16/16 ************************************
             isDel = *on;
            //**** End Add *** 03/16/16 ************************************
         ENDSL;
         ReadC sflVstTrk;
       ENDDO;

       //If row selected, or edited, populated details
       CHAIN RRNVstRow sflvsttrk;
       OnLoadRrn = outVstRrn;
       btnRefresh = *On;

       //**** Beg Add *** 03/16/16 *****************************************
       If isDel = *on;
          OnLoadRrn = *zeros;
          btnRefresh = *on;
       EndIf;
       //**** End Add *** 03/16/16 *****************************************

      /End-Free
     P ProcessSFL      E

       //===================================================================
       // Save the Visit Entry for Add/Edit Mode.
       //===================================================================
     p SaveVisit       b
     d SaveVisit       pi
     D inVstRRN                       9  0 const
     D inVstMode                     10    const
      *** Beg Add ***** 09/25/19 **************************
     D intLatestSeq#   s              5  0
      *** End Add ***** 09/25/19 **************************
      /free

       IF inVstMode = 'Add';
          CLEAR pvi0p;
          VICO#    = intCo#;
          VITYPE   = dtlVstTyp;
          VICAT    = dtlVstCat;
          SELECT;
             WHEN (VICAT = 'Policy');
                VIFND    = %dec(%subst(scEdtEmp#:1:3):3:0);
                VIEMP#   = %dec(%subst(scEdtEmp#:4:9):9:0);
                VIFYR    = %dec(%subst(scEdtEmp#:13:3):3:0);
                VIDIV    = scEdtDiv;
                VIQTE    = @@Qte;
                VILOCID  = scEdtLoc;
                //*** Beg Add ***** 09/25/19 ************************************
                //Get latest sequence #
                If VIdupEntry = 'Y';
                   intLatestSeq# = 0;
                   chkvstDte = dtCYMD(DtlVstDte);
                   Setgt  (VIco#:VIcat:VItype:VIfnd:VIemp#:chkvstDte) WMvil7;
                   Readpe (VIco#:VIcat:VItype:VIfnd:VIemp#:chkvstDte)
                    WMvil7 dsVivl7;
                   If not %eof;
                      intLatestSeq# = dsVivl7.VIseq#;
                   Endif;
                   VISEQ# = intLatestSeq# + 1;
                Else;
                   VISEQ# = 0;
                Endif;
                //*** End Add ***** 09/25/19 ************************************
             WHEN (VICAT = 'Claims');
                VIFND    = %dec(%subst(scEdtClm#:1:3):3:0);
                VIFYR    = %dec(%subst(scEdtClm#:5:3):3:0);
                VICASE   = %dec(%subst(scEdtClm#:9:7):7:0);
                VIEMP#   = %dec(%subst(scEdtEmp#:4:9):9:0);
                VIDIV    = scEdtDiv;
                //*** Beg Add ***** 09/25/19 ************************************
                //Get latest sequence #
                intLatestSeq# = 0;
                chkvstDte = dtCYMD(DtlVstDte);
                Setgt  (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase:
                 chkvstDte) WMvil2;
                Readpe (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase:
                 chkvstDte) WMvil2 dsVivl2;
                If not %eof;
                   intLatestSeq# = dsVivl2.VIseq#;
                Endif;
                VISEQ# = intLatestSeq# + 1;
                //*** End Add ***** 09/25/19 ************************************
             WHEN (VICAT = 'Agency');
                VIFEIN   = %dec(%subst(scEdtAgtDt:1:9):9:0);
                VIMOD    = %dec(%subst(scEdtAgtDt:11:3):3:0);
                VIAGT#   = %dec(%subst(scEdtAgtDt:15:5):5:0);
                //*** Beg Add ***** 09/25/19 ************************************
                //Get latest sequence #
                intLatestSeq# = 0;
                chkvstDte = dtCYMD(DtlVstDte);
                Setgt  (VIco#:VIcat:VItype:VIfein:VImod:VIagt#:
                 chkvstDte) WMvil1;
                Readpe (VIco#:VIcat:VItype:VIfein:VImod:VIagt#:
                 chkvstDte) WMvil1 dsVivl1;
                If not %eof;
                   intLatestSeq# = dsVivl1.VIseq#;
                Endif;
                VISEQ# = intLatestSeq# + 1;
                //*** End Add ***** 09/25/19 ************************************

          ENDSL;
          VIIUSR   = Q1USER;
          VIIPGM   = Q1PGM;
          VIIDT    = Dateto7(SystemDate());
          VIITIM   = Timeto6(SystemTime());
       ELSE;
          CHAIN inVstRRN pvi0p;
       ENDIF;
          VIUSER   = Q1USER;
          VIPGM    = Q1PGM;
          VIDT     = Dateto7(SystemDate());
          VITIME   = Timeto6(SystemTime());
          //*** Begin Add *** 06/24/22 **********************************
          saveDT = VIdt;
          saveTM = VItime;
          //*** End   Add *** 06/24/22 **********************************

          //move screen fields to file fields
          VIVSDT   = dtCYMD(DtlVstDte);
          VICMDT   = dtCYMD(DtlCmpDte);
          IF (VIcmdt > 0);
            VISTAT   = 'C';
          ELSE;
            VISTAT   = 'A';
          ENDIF;

          VIASSIGN = dtlVstAsgn;
          VINotes = dtlVstNote;
          VIDT01  = dtCYMD(OutDte01V);
          VIDT02  = dtCYMD(OutDte02V);
          VIDT03  = dtCYMD(OutDte03V);
          VIDT04  = dtCYMD(OutDte04V);
          VIDT05  = dtCYMD(OutDte05V);
          VIDT06  = dtCYMD(OutDte06V);
          VIDT07  = dtCYMD(OutDte07V);
          VIDT08  = dtCYMD(OutDte08V);
          VIDT09  = dtCYMD(OutDte09V);
          VIDT10  = dtCYMD(OutDte10V);
          VICB01  = outChk01V;
          VICB02  = outChk02V;
          VICB03  = outChk03V;
          VICB04  = outChk04V;
          VICB05  = outChk05V;
          VICB06  = outChk06V;
          VICB07  = outChk07V;
          VICB08  = outChk08V;
          VICB09  = outChk09V;
          VICB10  = outChk10V;
          VIINT01 = outItg01V;
          VIINT02 = outItg02V;
          VIINT03 = outItg03V;
          VIINT04 = outItg04V;
          VIINT05 = outItg05V;
          VIINT06 = outItg06V;
          VIINT07 = outItg07V;
          VIINT08 = outItg08V;
          VIINT09 = outItg09V;
          VIINT10 = outItg10V;
          VIDEC01 = outDec01V;
          VIDEC02 = outDec02V;
          VIDEC03 = outDec03V;
          VIDEC04 = outDec04V;
          VIDEC05 = outDec05V;
          VIDEC06 = outDec06V;
          VIDEC07 = outDec07V;
          VIDEC08 = outDec08V;
          VIDEC09 = outDec09V;
          VIDEC10 = outDec10V;
          VITF01  = outTxt01V;
          VITF02  = outTxt02V;
          VITF03  = outTxt03V;
          VITF04  = outTxt04V;
          VITF05  = outTxt05V;
          VITF06  = outTxt06V;
          VITF07  = outTxt07V;
          VITF08  = outTxt08V;
          VITF09  = outTxt09V;
          VITF10  = outTxt10V;

       IF inVstMode = 'Add';
          WRITE pvi0p;
       ELSE;
          Update pvi0p;
       ENDIF;

      /end-free
     p SaveVisit       e

       //===================================================================
       // SearchAgt# - Agency Search
       //===================================================================
     p SearchAgt#      b
     d SearchAgt#      pi

     D kcIn            s              1    inz('0')
     D klIn            s              1    inz('0')
     D callIn          s              1    inz('2')
     D rrnOut          s              9  0 inz(0)
     D viewIn          s              1  0 inz(1)
     D quoteIn         s              1    inz(' ')
     D modeIn          s              1    inz(' ')
     D nameIn          s             32

      /free

       nameIn=ssAgtName;

       AgtSearch(kcIn:klIn:callIn:rrnOut:viewIn:quoteIn:modeIn:nameIn);
       IF rrnOut<>0;
          CHAIN rrnOut pag0p;
          IF %Found(WMAGP);
             ssAgtName=AGname;
             // Get the Agency Number
             CHAIN (AGco#:AGfein:AGmod) paa00;
             IF Not %Found(WMAAL);
                AAagt# = 0;
             ENDIF;
             ssAgtFein=%EditC(AGfein:'X') + %EditC(AGmod:'X') +
                 %EditC(AAagt#:'X');
          ENDIF;
       ENDIF;

      /end-free

     p SearchAgt#      e

       //===================================================================
       // SearchClm# - Claim Search
       //===================================================================
     p SearchClm#      b
     d SearchClm#      pi

     D modeIn          s              1    inz('C')
     D callIn          s              1    inz('2')
     D rrnOut          s              9    inz('000000000')
     D clmRrn          s              9  0 inz(0)
     D selectIn        s           1024    inz(' ')

      /free

       CLEAR dsSrchCrit;
       CLEAR selectIn;
       dsClmGrp=0;
       dsClmLname=ssClmLnm;
       dsClmFname=ssClmFnm;
       selectIn=dsSrchCrit;
       modeIn = 'C';
       rrnOut = '000000000';

       ClmSearch(modeIn:callIn:rrnOut:selectIn);
       IF rrnOut<>'000000000';
          clmRrn=%Dec(rrnOut:9:0);
          CHAIN clmRrn pcm0p dsWMCMP;
          IF %Found(WMCMP);
             ssClmLnm=dsWMCMP.CMlsn;
             ssClmFnm=dsWMCMP.CMfsn;
             ssClaim#=%EditC(dsWMCMP.CMfnd:'X') +
                 %EditC(dsWMCMP.CMfyr:'X') +
                 %EditC(dsWMCMP.CMcase:'X');
          ENDIF;
       ENDIF;

      /end-free
     p SearchClm#      e

       //===================================================================
       // SearchEmp# - Employer Search
       //===================================================================
     p SearchEmp#      b
     d SearchEmp#      pi

     D modeIn          s              1    inz('E')
     D callIn          s              1    inz('2')
     D rrnOut          s              9    inz('000000000')
     D empRrn          s              9  0 inz(0)
     D selectIn        s           1024    inz(' ')

     D kkCo#           s                   like(EMco#)
     D kkFnd           s                   like(EMfnd)
     D kkEmp#          s                   like(EMemp#)
     D kkDiv           s                   like(EMdiv)

     C     keyWMEML      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkFnd
     C                   Kfld                    kkEmp#
     C                   Kfld                    kkDiv

      /free

       CLEAR dsSrchCrit;
       CLEAR selectIn;
       dsEmpGrp=0;
       dsEmpName=ssEmpDivNm;
       selectIn=dsSrchCrit;
       modeIn = 'E';
       rrnOut = '000000000';

       EmpSearch(modeIn:callIn:rrnOut:selectIn);
       IF rrnOut<>'000000000';

          // Set Quote Flag
          IF modeIn = 'Q';
             scQteFlag = 'Q';
          ELSE;
             scQteFlag = ' ';
          ENDIF;

          // Apply Overrides and Open Files
          overrideEmp(scQteFlag);

          empRrn=%Dec(rrnOut:9:0);
          CHAIN empRrn pel0p;
          IF %Found(WDELP);
             kkCo#=ELco#;
             kkFnd=ELfnd;
             kkEmp#=ELEmp#;
             kkDiv=ELdiv;
             CHAIN keyWMEML pem00 dsWMEML;
             IF %Found(WMEML);
                ssEmpDivNm=dsWMEML.EMname;
                ssEmpDiv=%EditC(dsWMEML.EMfnd:'X') +
                           %EditC(dsWMEML.EMemp#:'X') +
                           %EditC(dsWMEML.EMdiv:'X');
             ENDIF;
          ENDIF;

          // Delete Employer Overrides IF any and close files
          delOverrideEmp(scQteFlag);
       ENDIF;

      /end-free
     p SearchEmp#      e

       //===================================================================
       // Set Visit Types Filter
       //===================================================================
     p SetVisitFilter  b
     d SetVisitFilter  pi
      /free
       svVstCat  =  ssVstCat;
       tpVstCat  =  ssVstCat;
       svVstTyp  =  ssVstTyp;
       svVstTpCat=  ssVstTpCat;
       tpVstTpCat=  ssVstTpCat;
       svVstAsgn =  ssVstAsgn;
       svVstSts  =  ssVstSts;
       svVstFrDt =  ssVstFrDt;
       svVstToDt =  ssVstToDt;
       svCmpFrDt =  ssCmpFrDt;
       svCmpToDt =  ssCmpToDt;
       svGroup   =  ssGroup;
       svEmpDivNm=  ssEmpDivNm;
       svEmpDiv  =  ssEmpDiv;
       svLocation=  ssLocation;
       svClmLnm  =  ssClmLnm;
       svClmFnm  =  ssClmFnm;
       svClaim#  =  ssClaim#;
       svAgtName =  ssAgtName;
       svAgtFein =  ssAgtFein;
       svQteFlag =  ssQteFlag;
       //*** Beg Add ***** 10/17/22 ***************************************
       isSubset9999 = *Off;
       //*** End Add ***** 10/17/22 ***************************************

       DOU (vstFltrCx = *on) or (vstFltrSv = *on);
          if (ssVstCat <> ' ');
             selVstTyp = 'VIcat = ''' + %Trim(ssVstCat) + '''';
          ELSE;
             selVstTyp = 'VIcat in (''Agency'', ''Claims'', ''Policy'')';
          ENDIF;
          SELECT;
          WHEN $$RRN <> 0;
             //Do NOT activate the lookups when called from
             //specific Agency/Claims/Policy record.
          WHEN SSVstCat = 'Agency';
             noFltrAgt = *off;
          WHEN SSVstCat = 'Claims';
             noFltrClm = *off;
             noFltrGrp = *off;
          WHEN SSVstCat = 'Policy';
             noFltrEmp = *off;
             noFltrGrp = *off;
             IF ssEmpDiv <> ' ';
                noFltrLoc = *off;
             ENDIF;
          ENDSL;
          EXFMT rcdVstFltr;

          SELECT;
          WHEN vstFltrSv = *on;
             vstFltrSv = validateFilter();
             IF vstFltrSv = *on;
                showVstClr= *On;
             ENDIF;
          WHEN vstFltrCx = *on;
             ssVstCat  =  svVstCat;
             ssVstTyp  =  svVstTyp;
             ssVstTpCat=  svVstTpCat;
             ssVstAsgn =  svVstAsgn;
             ssVstSts  =  svVstSts;
             ssVstFrDt =  svVstFrDt;
             ssVstToDt =  svVstToDt;
             ssCmpFrDt =  svCmpFrDt;
             ssCmpToDt =  svCmpToDt;
             ssGroup   =  svGroup;
             ssEmpDivNm=  svEmpDivNm;
             ssEmpDiv  =  svEmpDiv;
             ssLocation=  svLocation;
             ssClmLnm  =  svClmLnm;
             ssClmFnm  =  svClmFnm;
             ssClaim#  =  svClaim#;
             ssAgtName =  svAgtName;
             ssAgtFein =  svAgtFein;
             ssQteFlag =  svQteFlag;
          WHEN selAgtSrch = *On;
             SearchAgt#();
          WHEN selClmSrch = *On;
             SearchClm#();
          WHEN selPolSrch = *On;
             SearchEmp#();
             //set Location dropdown
             IF ssQteFlag = 'Q';
                ssLocFile = %Trim(RtvQuoteLib('*QTE')) + '/WMALP';
             ELSE;
                ssLocFile = %Trim(RtvQuoteLib('*LIV')) + '/WMALP';
             ENDIF;
             //Location Select
             ssLocList='ALELID || ''-'' || TRIM(ALNAME) || ''-'' || ' +
                'CASE WHEN ALPAD1<>'' '' THEN TRIM(ALPAD1) || '' '' || ' +
                'TRIM(ALPAD2) || '' '' || TRIM(ALPCTY) || '', '' || ' +
                'ALPST || '' '' || SUBSTR(DIGITS(ALPZIP),1,5) ELSE ' +
                'TRIM(ALMAD1) || '' '' || ' +
                'TRIM(ALMAD2) || '' '' || TRIM(ALMCTY) || '', '' || ' +
                'ALMST || '' '' || SUBSTR(DIGITS(ALMZIP),1,5) END';
             noFltrLoc = *on;
             //Select Where
             ssLocSel='ALFND=' + %Subst(ssEmpDiv:1:3) + ' AND ALEMP#=' +
              %Subst(ssEmpDiv:4:9) + ' AND ALDIV=' +
              %Subst(ssEmpDiv:13:5) + ' AND ALNCCI = ''Y''';
             //Enable Location if NCCI reported Location/Entity exist
             overrideEmp(ssQteFlag);
             SETLL (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             READE (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             DOW not %eof;
                IF ALncci = 'Y';
                   noFltrLoc = *off;
                ENDIF;
                READE (ELco#:ELfnd:ELemp#:ElDiv) WMall;
             ENDDO;
             delOverrideEmp(ssQteFlag);
          ENDSL;

          IF ssVstTpCat <> tpVstTpCat;
             tpVstTpCat = ssVstTpCat;
             ssVstTyp = %subst(ssVstTpCat:1:40);
             ssVstCat = %subst(ssVstTpCat:41:10);
          ENDIF;
          IF ssVstCat <> tpVstCat;
             tpVstCat = ssVstCat;
             IF ssVstCat <> %subst(ssVstTpCat:41:10);
                ssVstTpCat = ' ';
                tpVstTpCat = ' ';
                ssVstTyp = ' ';
             ENDIF;
             ssgroup    = 0;
             ssEmpDiv   = ' ';
             ssQteFlag  = ' ';
             ssEmpDivNm = ' ';
             ssLocation = ' ';
             ssClaim#   = ' ';
             ssClmLnm   = ' ';
             ssClmFnm   = ' ';
             ssAgtFein  = ' ';
             ssAgtName  = ' ';
             noFltrGrp  = *on;
             noFltrEmp  = *on;
             noFltrLoc  = *on;
             noFltrClm  = *on;
             noFltrAgt  = *on;
          ENDIF;
       ENDDO;

       vstTypFlt = *Off;
      /end-free
     p SetVisitFilter  e

       //===================================================================
       // Validate Filter
       //===================================================================
     p ValidateFilter  b
     d ValidateFilter  pi              n

     d validFilter     s               N   inz(*on)
      /free
       validFilter = *on;
       erVstAsgn = *Off;
       erVstFrDt = *Off;
       erVstToDt = *Off;
       erCmpFrDt = *Off;
       erCmpToDt = *Off;
       erEmpDivNm= *off;
       erClmLnm  = *off;
       erClmFnm  = *off;
       erAgtName = *off;

       // Visit Assignment Must be Valid IF Entered
       IF ssVstAsgn <> ' ';
          CHAIN ssVstAsgn pup00;
          IF not %Found(SMUPL) or (%Found(SMUPL) And UPstat='I');
             mgVstAsgn = 'Invalid Staff Code.';
             erVstAsgn = *On;
             validFilter = *Off;
          ENDIF;
       ENDIF;

       IF ssVstFrDt<>0 Or ssVstToDt<>0;
          // Visit From date must be valid.
          IF dtCheck(ssVstFrDt)=0;
             mgVstFrDt = 'Invalid date entered.';
             erVstFrDt = *On;
             validFilter = *Off;
          ENDIF;

          // Visit To date must be valid.
          IF dtCheck(ssVstToDt)=0;
           //***  Begin Chg  ***  11/21/18  **************************
           //mgVstToDt = 'Invalid date entered.';
             mgVstToDt = 'WCR0500';
           //***  End   Chg  ***  11/21/18  **************************
             erVstToDt = *On;
             validFilter = *Off;
          ENDIF;

          // IF dates entered then thye must be in ascending order
          IF erVstFrDt = *Off And erVstToDt = *Off;
             IF dtCYMD(ssVstToDt) < dtCYMD(ssVstFrDt);
                mgVstFrDt = 'Dates must be in ascending order.';
                erVstFrDt = *On;
                mgVstToDt = 'Dates must be in ascending order.';
                erVstToDt = *On;
                validFilter = *Off;
             ENDIF;
          ENDIF;
       ENDIF;

       IF ssCmpFrDt<>0 Or ssCmpToDt<>0;
          // Completion From date must be valid.
          IF dtCheck(ssCmpFrDt)=0;
             mgCmpFrDt = 'Invalid date entered.';
             erCmpFrDt = *On;
             validFilter = *Off;
          ENDIF;

          // Completion To date must be valid.
          IF dtCheck(ssCmpToDt)=0;
           //***  Begin Chg  ***  11/21/18  ************************
           //mgCmpToDt = 'Invalid date entered.';
             mgCmpToDt = 'WCR0500';
           //***  End   Chg  ***  11/21/18  ************************
             erCmpToDt = *On;
             validFilter = *Off;
          ENDIF;

          // IF dates entered then thye must be in ascending order
          IF erCmpFrDt = *Off And erCmpToDt = *Off;
             IF dtCYMD(ssCmpToDt) < dtCYMD(ssCmpFrDt);
                mgCmpFrDt = 'Dates must be in ascending order.';
                erCmpFrDt = *On;
                mgCmpToDt = 'Dates must be in ascending order.';
                erCmpToDt = *On;
                validFilter = *Off;
             ENDIF;
          ENDIF;
       ENDIF;

       IF ssEmpDiv = ' ' and ssEmpDivNm <> ' ';
          mgEmpDivNm = 'Invalid Employer. Use Lookup to set Valid Policy.';
          erEmpDivNm = *on;
          validFilter = *Off;
       ENDIF;

       IF ssClaim# = ' ' and ssClmLnm <> ' ';
          mgClmLnm = 'Invalid Claim. Use Lookup to set Valid Claim.';
          erClmLnm = *on;
          validFilter = *Off;
       ENDIF;

       IF ssClaim# = ' ' and ssClmFnm <> ' ';
          mgClmFnm = 'Invalid Claim. Use Lookup to set Valid Claim.';
          erClmFnm = *on;
          validFilter = *Off;
       ENDIF;

       IF ssAgtFein= ' ' and ssAgtName<> ' ';
          mgAgtName= 'Invalid Agent. Use Lookup to set Valid Agent.';
          erAgtName= *on;
          validFilter = *Off;
       ENDIF;

       IF validFilter;
         onLoadRrn = 0;
         outVstRrn = 0;
       ENDIF;

       Return validFilter;

      /end-free
     p ValidateFilter  e

       //===================================================================
       // Validate Visit detail in Add/Edit Mode.
       //===================================================================
     p ValidateVisit   b
     d ValidateVisit   pi
      *** Beg Add ***** 09/25/19 ********************************
     d inVstMode                     10    const
      *** End Add ***** 09/25/19 ********************************
     d x               s               d
     d y               s               d
      /free
       IF (DtlVstAsgn = *blanks);
         mgVstAsgn = 'This field can not be blank.';
         erVstAsgn = *on;
         error = *On;
       ELSE;
         CHAIN dtlVstAsgn pup00;
         IF not %found(SMUPL) or (%found And UPstat='I');
            mgVstAsgn = 'Invalid Staff Assignment.';
            erVstAsgn = *On;
            error = *On;
         ENDIF;
       ENDIF;

       //*** Beg Chg ***** 09/25/19 *******************************
       //IF (DtlVstDte = 0);
       //  mgVstDte = 'Visit Date is Required.';
       //  erVstDte = *on;
       //  error = *On;
       //ELSE;
       //   IF dtCheck(DtlVstDte)=0;
       //     mgVstDte = 'Invalid date entered.';
       //     erVstDte = *On;
       //     error = *On;
       //   ELSE;
       //     //Check if Visit Type & Date record already exists.
       //     chkvstDte = dtCYMD(DtlVstDte);
       //     SELECT;
       //     WHEN @@Category = 'Agency';
       //        SETLL (VIco#:VIcat:VItype:VIfein:VImod:VIagt#) WMvil1;
       //        READE (VIco#:VIcat:VItype:VIfein:VImod:VIagt#) WMvil1 dsVivl1;
       //        DOW not %eof;
       //           IF (outVstRrn <> VIlRrn1) and (dsVivl1.VIvsDt = chkVstDte);
       //              mgVstDte = 'This Visit type already exists for this date';
       //              erVstDte = *On;
       //              error = *On;
       //           ENDIF;
       //           READE (VIco#:VIcat:VItype:VIfein:VImod:VIagt#) WMvil1 dsVivl1;
       //        ENDDO;
       //     WHEN @@Category = 'Claims';
       //        SETLL (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase) WMvil2;
       //        READE (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase) WMvil2 dsVivl2;
       //        DOW not %eof;
       //           IF (outVstRrn <> VIlRrn2) and (dsVivl2.VIvsDt = chkVstDte);
       //              mgVstDte = 'This Visit type already exists for this date';
       //              erVstDte = *On;
       //              error = *On;
       //           ENDIF;
       //           READE (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase) WMvil2 dsVivl2;
       //        ENDDO;
       //     WHEN @@Category = 'Policy';
       //        SETLL (VIco#:VIcat:VItype:VIfnd:VIemp#) WMvil3;
       //        READE (VIco#:VIcat:VItype:VIfnd:VIemp#) WMvil3 dsVivl3;
       //        DOW not %eof;
       //           IF (outVstRrn <> VIlRrn3) and (dsVivl3.VIvsDt = chkVstDte);
       //              mgVstDte = 'This Visit type already exists for this date';
       //              erVstDte = *On;
       //              error = *On;
       //           ENDIF;
       //           READE (VIco#:VIcat:VItype:VIfnd:VIemp#) WMvil3 dsVivl3;
       //        ENDDO;
       //     ENDSL;
       //   ENDIF;
       //ENDIF;
       Select;
       When DtlVstDte = 0;
          mgVstDte = 'Visit Date is Required.';
          erVstDte = *on;
          error = *On;
       When dtCheck(DtlVstDte) = 0;
          mgVstDte = 'Invalid date entered.';
          erVstDte = *On;
          error = *On;
       When VIdupEntry <> 'Y';
          //Check if Visit Type & Date record already exists for this Assignment type
          chkvstDte = dtCYMD(DtlVstDte);
          Select;
          When @@Category = 'Agency';
             Setll (VIco#:VIcat:VItype:VIfein:VImod:VIagt#:ChkvstDte) WMvil1;
             Reade (VIco#:VIcat:VItype:VIfein:VImod:VIagt#:ChkvstDte)
              WMvil1 dsVivl1;
             Dow not %eof;
                If (inVstMode = 'Edit' and dsVivl1.VIAssign = DtlVstAsgn
                 and outVstRrn <> VIlRrn1) or
                 (inVstMode = 'Add' and dsVivl1.VIAssign = DtlVstAsgn);
                   mgVstDte = 'This Visit type already exists for this date';
                   erVstDte = *On;
                   error = *On;
                Endif;
                Reade (VIco#:VIcat:VItype:VIfein:VImod:VIagt#:ChkvstDte)
                 WMvil1 dsVivl1;
             Enddo;
          When @@Category = 'Claims';
             Setll (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase:ChkvstDte) WMvil2;
             Reade (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase:ChkvstDte)
              WMvil2 dsVivl2;
             Dow not %eof;
                If (inVstMode = 'Edit' and dsVivl2.VIAssign = DtlVstAsgn
                 and outVstRrn <> VIlRrn2) or
                 (inVstMode = 'Add' and dsVivl2.VIAssign = DtlVstAsgn);
                   mgVstDte = 'This Visit type already exists for this date';
                   erVstDte = *On;
                   error = *On;
                Endif;
                Reade (VIco#:VIcat:VItype:VIfnd:VIfyr:VIcase:ChkvstDte)
                 WMvil2 dsVivl2;
             Enddo;
          When @@Category = 'Policy';
             Setll (VIco#:VIcat:VItype:VIfnd:VIemp#:ChkvstDte) WMvil7;
             Reade (VIco#:VIcat:VItype:VIfnd:VIemp#:ChkvstDte) WMvil7 dsVivl7;
             Dow not %eof;
                If (inVstMode = 'Edit' and dsVivl7.VIAssign = DtlVstAsgn
                 and outVstRrn <> VIlRrn7) or
                 (inVstMode = 'Add' and dsVivl7.VIAssign = DtlVstAsgn);
                   mgVstDte = 'This Visit type already exists for this date';
                   erVstDte = *On;
                   error = *On;
                Endif;
                Reade (VIco#:VIcat:VItype:VIfnd:VIemp#:ChkvstDte)
                 WMvil7 dsVivl7;
             Enddo;
          Endsl;
       Endsl;
       //*** End Chg ***** 09/25/19 *******************************

       IF (DtlCmpDte <> 0) and dtCheck(DtlCmpDte)=0;
         mgCmpDte = 'Invalid date entered.';
         erCmpDte = *On;
         error = *On;
       ENDIF;

       //*** Beg Del *** 06/08/23 **************************************
       //*** Beg Chg *** 02/04/19 **********************************
       //***IF (DtlCmpDte <> 0) and (DtlCmpDte < dtlVstDte);
       //*** IF (DtlCmpDte <> 0) and (cvtToDate(DtlCmpDte) < cvtToDate(DtlVstDte));
       //*** End Chg *** 02/04/19 **********************************
       //***   mgCmpDte = 'Completion date cannot be prior to Visit Date';
       //***   erCmpDte = *On;
       //***   error = *On;
       //*** ENDIF;
       //*** End Del *** 06/08/23 **************************************

       //***** Beg Add ***** 06/08/23 **************************************
       IF (DtlCmpDte <> 0) and (cvtToDate(DtlCmpDte) > SystemDate());
       //*** End Chg *** 02/04/19 **********************************
         mgCmpDte = 'Completion Date cannot be a future date';
         erCmpDte = *On;
         error = *On;
       ENDIF;
       //***** End End ***** 06/08/23 **************************************
       //***** Beg Chg ***** 06/08/23 **************************************
       // Adding here comman comment for below required fields.
       // Modify existing error conditions if Associated reuqired
       // field is 'C' and Service Request Completion Date is entered.
       //***** End Chg ***** 06/08/23 **************************************
       //***** IF (VIDtRq1='Y') and (OutDte01V=0);
       IF (VIDtRq1='Y' or (VIDtRq1='C' and DtlCmpDte <> 0)) and OutDte01V=0;
         mgDte01 = 'This is a Required field';
         erDte01 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq2='Y') and (OutDte02V=0);
       IF (VIDtRq2='Y' or (VIDtRq2='C' and DtlCmpDte <> 0)) and OutDte02V=0;
         mgDte02 = 'This is a Required field';
         erDte02 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq3='Y') and (OutDte03V=0);
       IF (VIDtRq3='Y' or (VIDtRq3='C' and DtlCmpDte <> 0)) and OutDte03V=0;
         mgDte03 = 'This is a Required field';
         erDte03 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq4='Y') and (OutDte04V=0);
       IF (VIDtRq4='Y' or (VIDtRq4='C' and DtlCmpDte <> 0)) and OutDte04V=0;
         mgDte04 = 'This is a Required field';
         erDte04 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq5='Y') and (OutDte05V=0);
       IF (VIDtRq5='Y' or (VIDtRq5='C' and DtlCmpDte <> 0)) and OutDte05V=0;
         mgDte05 = 'This is a Required field';
         erDte05 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq6='Y') and (OutDte06V=0);
       IF (VIDtRq6='Y' or (VIDtRq6='C' and DtlCmpDte <> 0)) and OutDte06V=0;
         mgDte06 = 'This is a Required field';
         erDte06 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq7='Y') and (OutDte07V=0);
       IF (VIDtRq7='Y' or (VIDtRq7='C' and DtlCmpDte <> 0)) and OutDte07V=0;
         mgDte07 = 'This is a Required field';
         erDte07 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq8='Y') and (OutDte08V=0);
       IF (VIDtRq8='Y' or (VIDtRq8='C' and DtlCmpDte <> 0)) and OutDte08V=0;
         mgDte08 = 'This is a Required field';
         erDte08 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq9='Y') and (OutDte09V=0);
       IF (VIDtRq9='Y' or (VIDtRq9='C' and DtlCmpDte <> 0)) and OutDte09V=0;
         mgDte09 = 'This is a Required field';
         erDte09 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDtRq10='Y') and (OutDte10V=0);
       IF (VIDtRq10='Y' or (VIDtRq10='C' and DtlCmpDte <> 0)) and OutDte10V=0;
         mgDte10 = 'This is a Required field';
         erDte10 = *On;
         error = *On;
       ENDIF;

       IF (OutDte01V <> 0) and dtCheck(OutDte01V)=0;
         mgDte01 = 'Invalid date entered.';
         erDte01 = *On;
         error = *On;
       ENDIF;
       IF (OutDte02V <> 0) and dtCheck(OutDte02V)=0;
         mgDte02 = 'Invalid date entered.';
         erDte02 = *On;
         error = *On;
       ENDIF;
       IF (OutDte03V <> 0) and dtCheck(OutDte03V)=0;
         mgDte03 = 'Invalid date entered.';
         erDte03 = *On;
         error = *On;
       ENDIF;
       IF (OutDte04V <> 0) and dtCheck(OutDte04V)=0;
         mgDte04 = 'Invalid date entered.';
         erDte04 = *On;
         error = *On;
       ENDIF;
       IF (OutDte05V <> 0) and dtCheck(OutDte05V)=0;
         mgDte05 = 'Invalid date entered.';
         erDte05 = *On;
         error = *On;
       ENDIF;
       IF (OutDte06V <> 0) and dtCheck(OutDte06V)=0;
         mgDte06 = 'Invalid date entered.';
         erDte06 = *On;
         error = *On;
       ENDIF;
       IF (OutDte07V <> 0) and dtCheck(OutDte07V)=0;
         mgDte07 = 'Invalid date entered.';
         erDte07 = *On;
         error = *On;
       ENDIF;
       IF (OutDte08V <> 0) and dtCheck(OutDte08V)=0;
         mgDte08 = 'Invalid date entered.';
         erDte08 = *On;
         error = *On;
       ENDIF;
       IF (OutDte09V <> 0) and dtCheck(OutDte09V)=0;
         mgDte09 = 'Invalid date entered.';
         erDte09 = *On;
         error = *On;
       ENDIF;
       IF (OutDte10V <> 0) and dtCheck(OutDte10V)=0;
         mgDte10 = 'Invalid date entered.';
         erDte10 = *On;
         error = *On;
       ENDIF;

       //***** IF (VIInRq1='Y') and (OutItg01V=0);
       IF (VIInRq1='Y' or (VIInRq1='C' and DtlCmpDte <> 0)) and OutItg01V=0;
         mgItg01 = 'This is a Required field';
         erItg01 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq2='Y') and (OutItg02V=0);
       IF (VIInRq2='Y' or (VIInRq2='C' and DtlCmpDte <> 0)) and OutItg02V=0;
         mgItg02 = 'This is a Required field';
         erItg02 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq3='Y') and (OutItg03V=0);
       IF (VIInRq3='Y' or (VIInRq3='C' and DtlCmpDte <> 0)) and OutItg03V=0;
         mgItg03 = 'This is a Required field';
         erItg03 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq4='Y') and (OutItg04V=0);
       IF (VIInRq4='Y' or (VIInRq4='C' and DtlCmpDte <> 0)) and OutItg04V=0;
         mgItg04 = 'This is a Required field';
         erItg04 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq5='Y') and (OutItg05V=0);
       IF (VIInRq5='Y' or (VIInRq5='C' and DtlCmpDte <> 0)) and OutItg05V=0;
         mgItg05 = 'This is a Required field';
         erItg05 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq6='Y') and (OutItg06V=0);
       IF (VIInRq6='Y' or (VIInRq6='C' and DtlCmpDte <> 0)) and OutItg06V=0;
         mgItg06 = 'This is a Required field';
         erItg06 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq7='Y') and (OutItg07V=0);
       IF (VIInRq7='Y' or (VIInRq7='C' and DtlCmpDte <> 0)) and OutItg07V=0;
         mgItg07  = 'This is a Required field';
         erItg07 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq8='Y') and (OutItg08V=0);
       IF (VIInRq8='Y' or (VIInRq8='C' and DtlCmpDte <> 0)) and OutItg08V=0;
         mgItg08  = 'This is a Required field';
         erItg08 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq9='Y') and (OutItg09V=0);
       IF (VIInRq9='Y' or (VIInRq9='C' and DtlCmpDte <> 0)) and OutItg09V=0;
         mgItg09 = 'This is a Required field';
         erItg09 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIInRq10='Y') and (OutItg10V=0);
       IF (VIInRq10='Y' or (VIInRq10='C' and DtlCmpDte <> 0)) and OutItg10V=0;
         mgItg10 = 'This is a Required field';
         erItg10 = *On;
         error = *On;
       ENDIF;

       //***** IF (VIDcRq1='Y') and (OutDec01V=0);
       IF (VIDcRq1='Y' or (VIDcRq1='C' and DtlCmpDte <> 0)) and OutDec01V=0;
         mgDec01 = 'This is a Required field';
         erDec01 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq2='Y') and (OutDec02V=0);
       IF (VIDcRq2='Y' or (VIDcRq2='C' and DtlCmpDte <> 0)) and OutDec02V=0;
         mgDec02 = 'This is a Required field';
         erDec02 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq3='Y') and (OutDec03V=0);
       IF (VIDcRq3='Y' or (VIDcRq3='C' and DtlCmpDte <> 0)) and OutDec03V=0;
         mgDec03 = 'This is a Required field';
         erDec03 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq4='Y') and (OutDec04V=0);
       IF (VIDcRq4='Y' or (VIDcRq4='C' and DtlCmpDte <> 0)) and OutDec04V=0;
         mgDec04 = 'This is a Required field';
         erDec04 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq5='Y') and (OutDec05V=0);
       IF (VIDcRq5='Y' or (VIDcRq5='C' and DtlCmpDte <> 0)) and OutDec05V=0;
         mgDec05 = 'This is a Required field';
         erDec05 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq6='Y') and (OutDec06V=0);
       IF (VIDcRq6='Y' or (VIDcRq6='C' and DtlCmpDte <> 0)) and OutDec06V=0;
         mgDec06 = 'This is a Required field';
         erDec06 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq7='Y') and (OutDec07V=0);
       IF (VIDcRq7='Y' or (VIDcRq7='C' and DtlCmpDte <> 0)) and OutDec07V=0;
         mgDec07 = 'This is a Required field';
         erDec07 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq8='Y') and (OutDec08V=0);
       IF (VIDcRq8='Y' or (VIDcRq8='C' and DtlCmpDte <> 0)) and OutDec08V=0;
         mgDec08 = 'This is a Required field';
         erDec08 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq9='Y') and (OutDec09V=0);
       IF (VIDcRq9='Y' or (VIDcRq9='C' and DtlCmpDte <> 0)) and OutDec09V=0;
         mgDec09 = 'This is a Required field';
         erDec09 = *On;
         error = *On;
       ENDIF;
       //***** IF (VIDcRq10='Y') and (OutDec10V=0);
       IF (VIDcRq10='Y' or (VIDcRq10='C' and DtlCmpDte <> 0)) and OutDec10V=0;
         mgDec10 = 'This is a Required field';
         erDec10 = *On;
         error = *On;
       ENDIF;

       //***** IF (VITfRq1='Y') and (OutTxt01V=*blanks);
       IF (VITfRq1='Y' or (VITfRq1='C' and DtlCmpDte <> 0)) and OutTxt01V=' ';
         mgTxt01 = 'This is a Required field';
         erTxt01 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq2='Y') and (OutTxt02V=*blanks);
       IF (VITfRq2='Y' or (VITfRq2='C' and DtlCmpDte <> 0)) and OutTxt02V=' ';
         mgTxt02 = 'This is a Required field';
         erTxt02 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq3='Y') and (OutTxt03V=*blanks);
       IF (VITfRq3='Y' or (VITfRq3='C' and DtlCmpDte <> 0)) and OutTxt03V=' ';
         mgTxt03 = 'This is a Required field';
         erTxt03 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq4='Y') and (OutTxt04V=*blanks);
       IF (VITfRq4='Y' or (VITfRq4='C' and DtlCmpDte <> 0)) and OutTxt04V=' ';
         mgTxt04 = 'This is a Required field';
         erTxt04 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq5='Y') and (OutTxt05V=*blanks);
       IF (VITfRq5='Y' or (VITfRq5='C' and DtlCmpDte <> 0)) and OutTxt05V=' ';
         mgTxt05 = 'This is a Required field';
         erTxt05 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq6='Y') and (OutTxt06V=*blanks);
       IF (VITfRq6='Y' or (VITfRq6='C' and DtlCmpDte <> 0)) and OutTxt06V=' ';
         mgTxt06 = 'This is a Required field';
         erTxt06 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq7='Y') and (OutTxt07V=*blanks);
       IF (VITfRq7='Y' or (VITfRq7='C' and DtlCmpDte <> 0)) and OutTxt07V=' ';
         mgTxt07 = 'This is a Required field';
         erTxt07 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq8='Y') and (OutTxt08V=*blanks);
       IF (VITfRq8='Y' or (VITfRq8='C' and DtlCmpDte <> 0)) and OutTxt08V=' ';
         mgTxt08 = 'This is a Required field';
         erTxt08 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq9='Y') and (OutTxt09V=*blanks);
       IF (VITfRq9='Y' or (VITfRq9='C' and DtlCmpDte <> 0)) and OutTxt09V=' ';
         mgTxt09 = 'This is a Required field';
         erTxt09 = *On;
         error = *On;
       ENDIF;
       //***** IF (VITfRq10='Y') and (OutTxt10V=*blanks);
       IF (VITfRq10='Y' or (VITfRq10='C' and DtlCmpDte <> 0)) and OutTxt10V=' ';
         mgTxt10 = 'This is a Required field';
         erTxt10 = *On;
         error = *On;
       ENDIF;

      /end-free
     p ValidateVisit   e
       //===================================================================
       // VisitSubset - Subset Visit Processing
       //===================================================================
     p VisitSubset     b
     d VisitSubset     pi              n
      *** Beg Add ***** 10/17/22 **************************
     d xxPos           s              3  0
      *** End Add ***** 10/17/22 **************************

      /free

          InzSubset();
          IF ((ssVstCat<>VICat) and (ssVstCat<>' '));
            Return *Off;
          ENDIF;
          IF (ssVstTyp <> ' ');
             IF (%scan(%xlate(lo:up:%trim(ssVstTyp)):
                 %xlate(lo:up:VIType))=0);
                Return *Off;
             ENDIF;
          ENDIF;
          IF (ssVstAsgn<> ' ');
             IF (%scan(%xlate(lo:up:%trim(ssVstAsgn)):
                             %xlate(lo:up:VIAssign))=0);
                Return *Off;
             ENDIF;
          ENDIF;
          //***** Beg Chg ***** 06/21/21 ********************************
          //IF ((ssVstSts<>VISTAT) and (ssVstSts<>' ') and (ssVstSts<>'*'));
             //Return *Off;
          //ENDIF;
          flgVstSts = *off;
          Select;
          When ssVstSts <> ' ';
             varVstSts = ssVstSts;
             Dow varVstSts <> ' ';
                Clear strVstSts;
                //*** Beg Chg ***** 10/17/22 **************************
                ///***strVstSts = ParseString(varVstSts:',':1:'1');
                xxPos = %scan(',':varVstSts);
                If xxPos > 0;
                   strVstSts = %Subst(varVstSts:1:xxPos-1);
                   varVstSts = %Subst(varVstSts:xxPos+1);
                Else;
                   strVstSts = varVstSts;
                   varVstSts = *Blank;
                Endif;
                //*** End Chg ***** 10/17/22 **************************
                If (%subst(strVstSts:1:1) <> VIstat and
                 %subst(strVstSts:1:1) <> '*');
                   Iter;
                Else;
                   flgVstSts = *on;
                   Leave;
                Endif;
             Enddo;
          When ssVstSts  = ' ';
             flgVstSts = *on;
          Endsl;
          If flgVstSts = *off;
             Return *off;
          Endif;
          //***** End Chg ***** 06/21/21 ********************************

          IF (ssVstFrDt <> 0) and (dtCYMD(ssVstFrDt)>VIVSDT);
             Return *Off;
          ENDIF;
          IF (ssVstToDt <> 0) and (dtCYMD(ssVstToDt)<VIVSDT);
             Return *Off;
          ENDIF;
          IF (ssCmpFrDt <> 0) and (dtCYMD(ssCmpFrDt)>VICMDT);
             Return *Off;
          ENDIF;
          IF (ssCmpToDt <> 0) and (dtCYMD(ssCmpToDt)<VICMDT);
             Return *Off;
          ENDIF;
          IF (ssGroup <> 0) and (VIfnd <> ssGroup);
             Return *Off;
          ENDIF;
          IF (ssEmpDiv<> ' ') and
                       (VIfnd <> %dec(%subst(ssEmpDiv:1:3):3:0) or
                        VIemp# <> %dec(%subst(ssEmpDiv:4:9):9:0));
             Return *Off;
          ENDIF;
          IF (ssLocation<> ' ') and (VILocID <> ssLocation);
             Return *Off;
          ENDIF;
          IF (ssClaim# <> ' ') and
                        (VIfnd <> %dec(%subst(ssClaim#:1:3):3:0) or
                         VIfyr <> %dec(%subst(ssClaim#:4:3):3:0) or
                         VIcase <> %dec(%subst(ssClaim#:7:7):7:0));
             Return *Off;
          ENDIF;
          IF (ssAgtFein <> ' ') and
                       (VIfein<> %dec(%subst(ssAgtFein:1:9):9:0) or
                        VImod <> %dec(%subst(ssAgtFein:10:3):3:0) or
                        VIagt#<> %dec(%subst(ssAgtFein:13:5):5:0));
             Return *Off;
          ENDIF;

          Return *On;

      /end-free

     p VisitSubset     e
      ********* Begin Add *** 09/09/15 **********************************
     P*====================================================================
     P* Return the employer access flag
     P*====================================================================
     PGetEmpAccess     b
     DGetEmpAccess     pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
      /Free

       If GroupAuth(inCo#:inFnd:q1User:grpAccess) = '1' and
          EmpAuth(inCo#:inFnd:inEmp#:q1User:empAccess) = '1';

          If grpAccess = 'I' or empAccess = 'I';
             Return 'D';
          Endif;

          Return 'C';
       Endif;

       Return 'D';

      /End-free
     PGetEmpAccess     e

     P*====================================================================
     P* Return the group access flag
     P*====================================================================
     PGetGrpAccess     b
     DGetGrpAccess     pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const

     DoutAccess        s              1
      /Free

       If GroupAuth(inCo#:inFnd:q1User:outAccess) = '1';

          If outAccess = 'I';
             Return 'D';
          Endif;

          Return 'C';
       Endif;

       Return 'D';

      /End-free
     PGetGrpAccess     e

     P*====================================================================
     P* Return the Inquiry or Update call flag for the claim
     P*====================================================================
     PInqUpdClm        b
     DInqUpdClm        pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inCase                         7  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
     DgrpExpired       s              1n   Inz
      /Free

       grpAccess = GetGrpAccess(inCo#:inFnd);
       empAccess = GetEmpAccess(inCo#:inFnd:inEmp#:inDiv);
       grpExpired = ExpiredGrp (inCo#:inFnd);

       Select;
       When grpAccess = 'D' or empAccess = 'D' or grpExpired = *On ;
          Return 'D';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdClm        e

     P*====================================================================
     P* Return the Inquiry or Update call flag for the employer
     P*====================================================================
     PInqUpdEmp        b
     DInqUpdEmp        pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
     DGrpExpired       s              1n   Inz
      /Free

       grpAccess = GetGrpAccess(inCo#:inFnd);
       empAccess = GetEmpAccess(inCo#:inFnd:inEmp#:inDiv);
       GrpExpired = ExpiredGrp (inCo#:inFnd);

       Select;
       When grpAccess = 'D' or empAccess = 'D' or grpExpired = *On ;
          Return 'D';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdEmp        e
