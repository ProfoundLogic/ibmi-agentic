     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim contact detail         box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - Process System contact files first instead of WMPDP.
     h*
     h*  Called from: WTCLMCON
     h*  Calls to:
     h*
     h*  Created on 09/03/2013 by C:267 for Redmine #2255 (Haifa Kealy)
     h**********************************************************************
     h* Revisions:
     h*
     h* 05/26/25  C:333 Project: WC3009
     h*           Clms, Policy, Agncy contacts: Maint Info
     h*           - Adding LastUsed and LastUsedBy
     h*
     h* 07/30/24  C:342 WC2826/115874/116975/116976
     h*                 Claims Contacts.
     h*                 1. Populate detail box for WMCDP file
     h*
     h* 03/04/24  C:342 Project: WC2826
     h*           Claims Contacts
     h*           Added coed to load Responsible Party/Insurance Company/
     h*           Additional Payee/Claims Contacts.
     h*           Cahngeprol Payee/Claims Contacts.
     h*
     h* 02/28/14  C:290 Support: 84927
     h*           Changed Ajax call to display dates corresponding to the
     h*           Attorney selected from the Contacts Information grid.
     h*           Also chnaged the conditons to avoid printing of comma in
     h*           Address Line 2 of contacts detail box.
     h*
     h**********************************************************************
      * Customer Master File
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
ok   fwmllp     if   e             Disk    ReName(pll01:pll0p)
     fwmtpp     if   e             Disk    ReName(ptp01:ptp0p)
     fwmecp     if   e             Disk    ReName(pec01:pec0p)
     fwmlll     if   e           k Disk    ReName(pll01:pll00)
     f                                     InfDs(dbfds)
     fwmtpl1    if   e           k Disk    InfDs(dbfds2)
     fwmecl     if   e           k Disk    ReName(pec01:pec00)
     f                                     InfDs(dbfds3)
     Fwmexl     if   e           k disk    ReName(pex01:pex00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
ok   fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
       //*** Beg Add *** 03/04/24 *******************************************
     fwmsbp     if   e             Disk    ReName(psb01:psb0p)
     fwdacp     if   e             Disk    ReName(pac01:pac0p)
     fwmcnp     if   e             Disk    ReName(pcn01:pcn0p)
     fsmupp     if   e             Disk    ReName(pup01:pup0p)
       //*** End Add *** 03/04/24 *******************************************
       //*** End Add *** 07/30/24 *******************************************
     fwmcdp     if   e             Disk    ReName(pcd01:pcd0p)
       //*** End Add *** 07/30/24 *******************************************
       //*** Beg Add *** 05/26/25 *******************************************
     fsmupl     if   e           k Disk    ReName(pup01:pup0l)
       //*** End Add *** 05/26/25 *******************************************

      /copy sPrDate
      /copy sPrAtrium

     D* System Data Structures
     D dbfds           ds
     D  rrnWmlll             397    400b 0

     D dbfds2          ds
     D  rrnWmtpl1            397    400b 0

     D dbfds3          ds
     D  rrnWmecl             397    400b 0

     D  ConFileDS      ds           512
     D  wkConFile              1     10
     D  wkFiller              11    512

     d* Proto-types

     d* Load Defense Attorney Contact Info detail box
     d LoadDefAtt      pr

     d* Load Plaintiff Attorney Contact Info detail box
     d LoadPlnAtt      pr

     d* Load Treating Physian Contact Info detail box
     d LoadTrtPhy      pr

     d* Load Employer Contact Info detail box
     d LoadEmpCon      pr

      * *ENTRY

     DClaimConDtl01    pr                  ExtPgm('WTCLMCON01')
     D                                9  0
hmk  D                              512
     D                            32767
     D                                1
      *
     DClaimConDtl01    pi
     D inRRN                          9  0
hmk  D inConfile                    512
     D outJSON                    32767
     D RtnCode                        1
      *
     D Fax#Out         s             13
     D AsnDtOut        s             10
     D RspDtOut        s             10
     D ExpDtOut        s             10
     D EffDtOut        s             10
     D AdrLn1Out       s             40
     D AdrLn2Out       s             40
     D AdrLn3Out       s             40
     D ##fein          s              9  0
     D ##mod           s              3  0
      ******** Beg Add **** 02/28/14 *************************
     D AsnDt           s             20
     D RspDt           s             20
     D ExpDt           s             20
     D EffDt           s             20
      ******** End Add **** 02/28/14 *************************
      ******** Beg Add **** 03/04/24 *************************
     D EmailAdd        s             60
      ******** End Add **** 03/04/24 *************************
      ******** Beg Add **** 05/26/25 *************************
     D LastUpd         s             10
     D LastUpdBy       s             50
      ******** End Add **** 05/26/25 *************************

     C     keyWMPDL      Klist
     C                   Kfld                    ##Fein
     C                   Kfld                    ##Mod

     C     keyWMEXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
      *
      /Free
       // Buld Data That will be passed on to AJAX
          // determine which type of contact record to display
          ConFileDS = inConFile;
          If %trim(wkConfile) = 'WMLLLDF';
             LoadDefAtt();
          Elseif %trim(wkConfile) = 'WMLLLPF';
             LoadPlnAtt();
          Elseif %trim(wkConfile) = 'WMTPL1';
             LoadTrtPhy();
          Elseif %trim(wkConfile) = 'WMECL';
             LoadEmpCon();
          //*** Beg Add *** 03/04/24 *******************************************
          Elseif %trim(wkConfile) = 'WMSBLRF';
             LoadResPar();
          Elseif %trim(wkConfile) = 'WMSBLIF';
             LoadInsCom();
          Elseif %trim(wkConfile) = 'WDACL';
             LoadAddPay();
          Elseif %trim(wkConfile) = 'SMUPL1';
             LoadUsrParm();
          Elseif %trim(wkConfile) = 'WMCNL';
             LoadClmCon();
          //*** End Add *** 03/04/24 *******************************************
          //*** Beg Add *** 07/30/24 *******************************************
          Elseif %trim(wkConfile) = 'WMCDL';
             LoadClaimant();
          //*** End Add *** 07/30/24 *******************************************
          Endif;


       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlFax#'   :Fax#Out);
       BuildJSON(outJSON :'OutDtlAsnDt'  :AsnDtOut);
       BuildJSON(outJSON :'OutDtlRspDt'  :RspDtOut);
       BuildJSON(outJSON :'OutDtlExpDt'  :ExpDtOut);
       BuildJSON(outJSON :'OutDtlEffDt'  :EffDtOut);
       BuildJSON(outJSON :'OutDtlAdrLn1' :AdrLn1Out);
       BuildJSON(outJSON :'OutDtlAdrLn2' :AdrLn2Out);
       BuildJSON(outJSON :'OutDtlAdrLn3' :AdrLn3Out);
       //***** Beg Add **** 02/28/14 *************************
       BuildJSON(outJSON :'LblDtlAsnDt'  :AsnDt);
       BuildJSON(outJSON :'LblDtlRspDt'  :RspDt);
       BuildJSON(outJSON :'LblDtlExpDt'  :ExpDt);
       BuildJSON(outJSON :'LblDtlEffDt'  :EffDt);
       //***** End Add **** 02/28/14 *************************
       //***** Beg Add **** 03/04/24 *************************
       BuildJSON(outJSON :'DtlOutEmailId':EmailAdd);
       //***** End Add **** 03/04/24 *************************
       //***** Beg Add **** 05/26/25 *************************
       BuildJSON(outJSON :'DtlOutLastUpdated':LastUpd);
       BuildJSON(outJSON :'DtlOutLastUpdatedBy':LastUpdBy);
       //***** End Add **** 05/26/25 *************************

       RtnCode = ' ';
       Return;

      /End-Free

       //===================================================================
       // LoadDefAtt - Load Defense Attorney Contact info detail box
       //===================================================================
     P LoadDefAtt      B
     D LoadDefAtt      PI
      /Free

       //***** Beg Add **** 03/04/24 *************************
       Chain inRRN pll0p;
       If %found(Wmllp);

          AsnDtOut = alphaUSA(CvtToDate(dtMDYY(LLdedt)));
          AsnDt = 'Assignment Date';
          AdrLn1Out   = LLdad1;
          AdrLn2Out   = LLdad2;
          If AdrLn2Out <>*Blanks;
             AdrLn3Out  = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
              %EditW(LLdZip:'     -    ');
          ElseIf LLdCty <> *Blanks OR LLdSt <> *Blanks OR LLdZip <> *Zeros;
             AdrLn2Out  = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
              %EditW(LLdZip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          If LLdefe <> *zeros or LLdeff <> *zeros;
             Fax#Out  = '(' + %editc(LLdefe:'X') + ')' +
              %subst(%editc(LLdeff:'X'):1:3) + '-' +
              %subst(%editc(LLdeff:'X'):4:4);
          Endif;
          EmailAdd  = LLemdd;

          ##fein = LLdefa;
          ##mod  = LLdefm;
          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             If AdrLn1Out = *blanks and AdrLn2Out = *blanks
                and AdrLn3Out = *blanks;
                If PDdba1 = 'Y';
                   AdrLn1Out = PDadr2;
                Else;
                   AdrLn1Out = PDadr1;
                   AdrLn2Out = PDadr2;
                Endif;

                If AdrLn2Out <>*Blanks;
                   AdrLn3Out = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
                   AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                   AdrLn3Out  = *Blanks;
                EndIf;
             Endif;

             If Fax#Out = *blanks;
                If PDfxar <> *zeros or PDfax# <> *zeros;
                   Fax#Out  = '(' + %editc(PDfxar:'X') + ')' +
                    %subst(%editc(PDfax#:'X'):1:3) + '-' +
                    %subst(%editc(PDfax#:'X'):4:4);
                Endif;
             Endif;
             If EmailAdd = *blanks;
                EmailAdd = PDemad;
             Endif;
          Endif;
       Endif;
       //***** End Add **** 03/04/24 *************************

       //***** Beg Del **** 03/04/24 *************************
       //*** Chain inRRN pll0p;
       //*** If %found(Wmllp);

       //***    AsnDtOut = alphaUSA(CvtToDate(dtMDYY(LLdedt)));
       //***** Beg Add **** 02/28/14 *************************
       //***    AsnDt = 'Assignment Date';
       //***** End Add **** 02/28/14 *************************

       //***    ##fein = LLdefa;
       //***    ##mod  = LLdefm;

       //***    Chain keyWMPDL ppd00;
       //***    If %found(Wmpdl);
       //***       If PDdba1 = 'Y';
                //*** DtlConName = PDadr1;
       //***          AdrLn1Out = PDadr2;
       //***       Else;
                //*** DtlConName = PDname;
       //***          AdrLn1Out = PDadr1;
       //***          AdrLn2Out = PDadr2;
       //***       Endif;
       //***       If AdrLn2Out <>*Blanks;
       //***          AdrLn3Out = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
             //****Else;
       //***       ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
       //***          AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
       //***          AdrLn3Out  = *Blanks;
       //***       EndIf;

       //***       If PDfxar <> *zeros or PDfax# <> *zeros;
       //***          Fax#Out  = '(' + %editc(PDfxar:'X') + ')' +
       //***           %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***           %subst(%editc(PDfax#:'X'):4:4);
       //***       Endif;
       //***    Else;
             //*** DtlConName = LLdefn;
       //***       AdrLn1Out   = LLdad1;
       //***       AdrLn2Out   = LLdad2;
       //***       If AdrLn2Out <>*Blanks;
       //***          AdrLn3Out  = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
       //***           %EditW(LLdZip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
             //****Else;
       //***       ElseIf LLdCty <> *Blanks OR LLdSt <> *Blanks OR LLdZip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
       //***          AdrLn2Out  = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
       //***           %EditW(LLdZip:'     -    ');
       //***          AdrLn3Out  = *Blanks;
       //***       EndIf;
       //***       If LLdefe <> *zeros or LLdeff <> *zeros;
       //***          Fax#Out  = '(' + %editc(LLdefe:'X') + ')' +
       //***           %subst(%editc(LLdeff:'X'):1:3) + '-' +
       //***           %subst(%editc(LLdeff:'X'):4:4);
       //***       Endif;
       //***    Endif;
       //*** Endif;
       //***** End Del **** 03/04/24 *************************

      /End-Free
     P LoadDefAtt      E
       //===================================================================
       // LoadPlnAtt - Load Plaintiff Attorney Contact info detail box
       //===================================================================
     P LoadPlnAtt      B
     D LoadPlnAtt      PI
      /Free

       //***** Beg Add **** 03/04/24 *************************
       Chain inRRN pll0p;
       If %found(Wmllp);

          RspDtOut = alphaUSA(CvtToDate(dtMDYY(LLdtap)));
          RspDt = 'Representation Date';
          AdrLn1Out   = LLpad1;
          AdrLn2Out   = LLpad2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
              %EditW(LLpZip:'     -    ');
          ElseIf LLpCty <> *Blanks OR LLpSt <> *Blanks OR LLpZip <> *Zeros;
             AdrLn2Out  = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
              %EditW(LLpZip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          If LLplae <> *zeros or LLplaf <> *zeros;
             Fax#Out = '(' + %editc(LLplae:'X') + ')' +
              %subst(%editc(LLplaf:'X'):1:3) + '-' +
              %subst(%editc(LLplaf:'X'):4:4);
          Endif;
          EmailAdd  = LLemdp;

          ##fein = LLplaa;
          ##mod  = LLplam;
          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             If AdrLn1Out = *blanks and AdrLn2Out = *blanks
                and AdrLn3Out = *blanks;
                If PDdba1 = 'Y';
                   AdrLn1Out  = PDadr2;
                Else;
                   AdrLn1Out  = PDadr1;
                   AdrLn2Out  = PDadr2;
                Endif;
                If AdrLn2Out <> *Blanks;
                   AdrLn3Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
                   AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                   AdrLn3Out  = *Blanks;
                EndIf;
             Endif;

             If Fax#Out = *blanks;
                If PDfxar <> *zeros or PDfax# <> *zeros;
                   Fax#Out = '(' + %editc(PDfxar:'X') + ')' +
                    %subst(%editc(PDfax#:'X'):1:3) + '-' +
                    %subst(%editc(PDfax#:'X'):4:4);
                Endif;
             Endif;
             If EmailAdd = *blanks;
                EmailAdd = PDemad;
             Endif;
          Endif;
       Endif;
       //***** End Add **** 03/04/24 *************************

       //***** Beg Del **** 03/04/24 *************************
       //*** Chain inRRN pll0p;
       //*** If %found(Wmllp);

       //***    RspDtOut = alphaUSA(CvtToDate(dtMDYY(LLdtap)));
       //***** Beg Add **** 02/28/14 *************************
       //***    RspDt = 'Representation Date';
       //***** End Add **** 02/28/14 *************************
       //***    ##fein = LLplaa;
       //***    ##mod  = LLplam;

       //***    Chain keyWMPDL ppd00;
       //***    If %found(Wmpdl);
       //***       If PDdba1 = 'Y';
                 //*** DtlConName = PDadr1;
       //***          AdrLn1Out  = PDadr2;
       //***       Else;
                //*** DtlConName = PDname;
       //***          AdrLn1Out  = PDadr1;
       //***          AdrLn2Out  = PDadr2;
       //***       Endif;
       //***       If AdrLn2Out <> *Blanks;
       //***          AdrLn3Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
             //****Else;
       //***       ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
       //***          AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
       //***          AdrLn3Out  = *Blanks;
       //***       EndIf;

       //***       If PDfxar <> *zeros or PDfax# <> *zeros;
       //***          Fax#Out = '(' + %editc(PDfxar:'X') + ')' +
       //***           %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***           %subst(%editc(PDfax#:'X'):4:4);
       //***       Endif;
       //***    Else;
       //***       AdrLn1Out   = LLpad1;
       //***       AdrLn2Out   = LLpad2;
       //***       If AdrLn2Out <> *Blanks;
       //***          AdrLn3Out  = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
       //***           %EditW(LLpZip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
             //****Else;
       //***       ElseIf LLpCty <> *Blanks OR LLpSt <> *Blanks OR LLpZip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
       //***          AdrLn2Out  = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
       //***           %EditW(LLpZip:'     -    ');
       //***          AdrLn3Out  = *Blanks;
       //***       EndIf;
       //***     If LLplae <> *zeros or LLplaf <> *zeros;
       //***          Fax#Out = '(' + %editc(LLplae:'X') + ')' +
       //***           %subst(%editc(LLplaf:'X'):1:3) + '-' +
       //***           %subst(%editc(LLplaf:'X'):4:4);
       //***       Endif;
       //***    Endif;

       //*** Endif;
       //***** End Del **** 03/04/24 *************************

      /End-Free
     P LoadPlnAtt      E
       //===================================================================
       // LoadTrtPhy - Load Treating Physian Contact info detail box
       //===================================================================
     P LoadTrtPhy      B
     D LoadTrtPhy      PI
      /Free

       //***** Beg Add **** 03/04/24 *************************
       Chain inRRN ptp0p;
       If %found(Wmtpp);

          EffDtOut  = alphaUSA(CvtToDate(dtMDYY(TPfrdt)));
          ExpDtOut  = alphaUSA(CvtToDate(dtMDYY(TPtodt)));
          EffDt = 'Effective Date';
          ExpDt = 'Expiration Date';
          AdrLn1Out   = TPphya;
          AdrLn2Out  = %Trim(TPphyC) + ', ' + TPphyS + ' ' +
           %EditW(TPphyZ:'     -    ');
          AdrLn3Out  = *Blanks;
          If TPfxar <> *zeros or TPfax# <> *zeros;
             Fax#Out = '(' + %editc(TPfxar:'X') + ')' +
              %subst(%editc(TPfax#:'X'):1:3) + '-' +
              %subst(%editc(TPfax#:'X'):4:4);
          Endif;
          EmailAdd = TPemad;

          ##fein = TPphyf;
          ##mod  = TPphym;
          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             If AdrLn1Out = *blanks and AdrLn2Out = *blanks
                and AdrLn3Out = *blanks;
                If PDdba1 = 'Y';
                   AdrLn1Out  = PDadr2;
                Else;
                   AdrLn1Out  = PDadr1;
                   AdrLn2Out  = PDadr2;
                Endif;
                If AdrLn2Out <> *Blanks;
                   AdrLn3Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
                   AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
                    %EditW(PDzip:'     -    ');
                   AdrLn3Out  = *Blanks;
                EndIf;
             EndIf;
             If Fax#Out = *blanks;
                If PDfxar <> *zeros or PDfax# <> *zeros;
                   Fax#Out  = '(' + %editc(PDfxar:'X') + ')' +
                    %subst(%editc(PDfax#:'X'):1:3) + '-' +
                    %subst(%editc(PDfax#:'X'):4:4);
                Endif;
             Endif;
             If EmailAdd = *blanks;
                EmailAdd = PDemad;
             Endif;
          Endif;
       Endif;
       //***** End Add **** 03/04/24 *************************

       //***** Beg Del **** 03/04/24 *************************
       //*** Chain inRRN ptp0p;
       //*** If %found(Wmtpp);

       //***    EffDtOut  = alphaUSA(CvtToDate(dtMDYY(TPfrdt)));
       //***    ExpDtOut  = alphaUSA(CvtToDate(dtMDYY(TPtodt)));
       //***** Beg Add **** 02/28/14 *************************
       //***    EffDt = 'Effective Date';
       //***    ExpDt = 'Expiration Date';
       //***** End Add **** 02/28/14 *************************

       //***    ##fein = TPphyf;
       //***    ##mod  = TPphym;
       //***    Chain keyWMPDL ppd00;
       //***    If %found(Wmpdl);
       //***       If PDdba1 = 'Y';
                //*** DtlConName = PDadr1;
       //***          AdrLn1Out  = PDadr2;
       //***       Else;
                //*** DtlConName = PDname;
       //***          AdrLn1Out  = PDadr1;
       //***          AdrLn2Out  = PDadr2;
       //***       Endif;
       //***       If AdrLn2Out <> *Blanks;
       //***          AdrLn3Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
             //****Else;
       //***       ElseIf PDcty <> *Blanks OR PDst <> *Blanks OR PDzip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
       //***          AdrLn2Out  = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
       //***          AdrLn3Out  = *Blanks;
       //***       EndIf;
       //***       If PDfxar <> *zeros or PDfax# <> *zeros;
       //***          Fax#Out  = '(' + %editc(PDfxar:'X') + ')' +
       //***           %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***           %subst(%editc(PDfax#:'X'):4:4);
       //***       Endif;
       //***    Else;
             //*** DtlConName = TPphyn;
       //***       AdrLn1Out   = TPphya;
       //***       AdrLn2Out  = %Trim(TPphyC) + ', ' + TPphyS + ' ' +
       //***        %EditW(TPphyZ:'     -    ');
       //***       AdrLn3Out  = *Blanks;
       //***       If TPfxar <> *zeros or TPfax# <> *zeros;
       //***          Fax#Out = '(' + %editc(TPfxar:'X') + ')' +
       //***           %subst(%editc(TPfax#:'X'):1:3) + '-' +
       //***           %subst(%editc(TPfax#:'X'):4:4);
       //***       Endif;
       //***    Endif;
       //*** Endif;
       //***** End Del **** 03/04/24 *************************

      /End-Free
     P LoadTrtPhy      E
       //===================================================================
       // LoadEmpCon - Load Employer Contact info detail box
       //===================================================================
     P LoadEmpCon      B
     D LoadEmpCon      PI
      /Free

       Chain inRRN pec0p;
       If %found(Wmecp);

          AdrLn1Out   = ECadr1;
          AdrLn2Out   = ECadr2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(ECcty) + ', ' + ECst + ' ' +
              %EditW(ECzip:'     -    ');
        //******** Beg Chg **** 02/28/14 *************************
          //****Else;
             ElseIf ECcty <> *Blanks OR ECst <> *Blanks OR ECzip <> *Zeros;
        //******** End Chg **** 02/28/14 *************************
             AdrLn2Out  = %Trim(ECcty) + ', ' + ECst + ' ' +
              %EditW(ECzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          If ECfara <> *zeros or ECfphn <> *zeros;
             Fax#Out  = '(' + %editc(ECfara:'X') + ')' +
              %subst(%editc(ECfphn:'X'):1:3) + '-' +
              %subst(%editc(ECfphn:'X'):4:4);
          Endif;
       Endif;
      /End-Free
     P LoadEmpCon      E

       //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // LoadResPar - Load Responsible Party detail box
       //===================================================================
     P LoadResPar      B
     D LoadResPar      PI
      /Free

       Chain inRRN psb0p;
       If %found(Wmsbp);
          AdrLn1Out = SBrad1;
          AdrLn2Out = SBrad2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(SBrcty) + ', ' + SBrst + ' ' +
              %EditW(SBrzip:'     -    ');
             ElseIf SBrcty <> *Blanks OR SBrst <> *Blanks OR SBrzip <> *Zeros;
             AdrLn2Out  = %Trim(SBrcty) + ', ' + SBrst + ' ' +
              %EditW(SBrzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          EmailAdd  = SBrmad;
       Endif;

      /End-Free
     P LoadResPar      E

       //===================================================================
       // LoadInsCom - Load Insurance Compnay detail box
       //===================================================================
     P LoadInsCom      B
     D LoadInsCom      PI
      /Free

       Chain inRRN psb0p;
       If %found(Wmsbp);
          AdrLn1Out = SBiad1;
          AdrLn2Out = SBiad2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(SBicty) + ', ' + SBist + ' ' +
              %EditW(SBizip:'     -    ');
             ElseIf SBicty <> *Blanks OR SBist <> *Blanks OR SBizip <> *Zeros;
             AdrLn2Out  = %Trim(SBicty) + ', ' + SBist + ' ' +
              %EditW(SBizip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          EmailAdd  = SBimad;
       Endif;

      /End-Free
     P LoadInsCom      E

       //===================================================================
       // LoadAddPay - Load Additional Payee detail box
       //===================================================================
     P LoadAddPay      B
     D LoadAddPay      PI
      /Free

       Chain inRRN pac0p;
       If %found(Wdacp);
          AdrLn1Out = ACadr1;
          AdrLn2Out = ACadr2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(ACcty) + ', ' + ACst + ' ' +
              %EditW(ACzip:'     -    ');
             ElseIf ACcty <> *Blanks OR ACst <> *Blanks OR ACzip <> *Zeros;
             AdrLn2Out  = %Trim(ACcty) + ', ' + ACst + ' ' +
              %EditW(ACzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          EmailAdd  = ACemdc;
       Endif;

      /End-Free
     P LoadAddPay      E

       //===========================================================================
       // LoadUsrParm - Load User Parameter file detail box - Internal Claim Contact
       //===========================================================================
     P LoadUsrParm     B
     D LoadUsrParm     PI
      /Free

       Chain inRRN pup0p;
       If %found(Smupp);
          AdrLn1Out = UPadr1;
          AdrLn2Out = UPadr2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(UPcty) + ', ' + UPst + ' ' +
              %EditW(UPzip:'     -    ');
             ElseIf UPcty <> *Blanks OR UPst <> *Blanks OR UPzip <> *Zeros;
             AdrLn2Out  = %Trim(UPcty) + ', ' + UPst + ' ' +
              %EditW(UPzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          If UPfaxa <> *zeros or UPfax# <> *zeros;
             Fax#Out  = '(' + %editc(UPfaxa:'X') + ')' +
              %subst(%editc(UPfax#:'X'):1:3) + '-' +
              %subst(%editc(UPfax#:'X'):4:4);
          Endif;
          EmailAdd  = UPmail;
       Endif;

      /End-Free
     P LoadUsrParm     E

       //===================================================================
       // LoadClmCon - Load Claim Contacts detail box (Non-System)
       //===================================================================
     P LoadClmCon      B
     D LoadClmCon      PI
      /Free

       Chain inRRN pcn0p;
       If %found(Wmcnp);
          AdrLn1Out = CNadr1;
          AdrLn2Out = CNadr2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(CNcty) + ', ' + CNst + ' ' +
              %EditW(CNzip:'     -    ');
             ElseIf CNcty <> *Blanks OR CNst <> *Blanks OR CNzip <> *Zeros;
             AdrLn2Out  = %Trim(CNcty) + ', ' + CNst + ' ' +
              %EditW(CNzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          EmailAdd  = CNemal;
          If CNfara <> *zeros or CNfphn <> *zeros;
             Fax#Out  = '(' + %editc(CNfara:'X') + ')' +
              %subst(%editc(CNfphn:'X'):1:3) + '-' +
              %subst(%editc(CNfphn:'X'):4:4);
          Endif;
          //*** Beg Add ***** 05/26/25 *************************
          LastUpd = %Trim(%EditW(dtMDYY(CNdt):'  /  /    '));
          Chain CNuser pup0l;
          If %found(SMUPL);
             LastUpdBy = UPusnm;
          Endif;
          //*** End Add ***** 05/26/25 *************************
       Endif;

      /End-Free
     P LoadClmCon      E
       //*** End Add *** 03/04/24 *******************************************
       //*** Beg Add *** 07/30/24 *******************************************
       //===================================================================
       // LoadClaimant - Load Claim Contacts detail box (System)
       //===================================================================
     P LoadClaimant    B
     D LoadClaimant    PI
      /Free

       Chain inRRN pcd0p;
       If %found(Wmcdp);
          AdrLn1Out = CDadr1;
          AdrLn2Out = CDadr2;
          If AdrLn2Out <> *Blanks;
             AdrLn3Out  = %Trim(CDcty) + ', ' + CDst + ' ' +
              %EditW(CDzip:'     -    ');
             ElseIf CDcty <> *Blanks OR CDst <> *Blanks OR CDzip <> *Zeros;
             AdrLn2Out  = %Trim(CDcty) + ', ' + CDst + ' ' +
              %EditW(CDzip:'     -    ');
             AdrLn3Out  = *Blanks;
          EndIf;
          EmailAdd  = CDemad;
       Endif;

      /End-Free
     P LoadClaimant    E
       //*** End Add *** 07/30/24 *******************************************
