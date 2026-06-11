     *========================================================================*
     h*copyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h*StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h**********************************************************************
     h*
     h*  Description:  This copybook will be used to declare the procedure
     h*  Interface which will load Claim action panel in Tranform screens
     h*
     h*
     h*  Created on 3/7/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h* 04/04/21 C:180  Project: WC2732
     h*                 Change multi claim link to call new multi claim
    h*
    h* 06/09/20  C:298 Project: SY420
    h*                 Mask SSN
    h*
     h* 04/25/20  C:295 Support: 88232
     h*                 Added Employer number on Action Panel.
     h*
     h* 07/18/16  C:308 Project: C114WXY
     h*                 Add ISO Score fields loading from WMCMP file.
    h*
     h* 12/14/15  C:310 Support: 38826
     h*                 Modified code by substring the name field that cause
     h*                 job to abended due to mismatch of length of field.
     h* 05/28/15  C:298 Support: 33721
     h*                 Correted Employer Address on Action Panel.
     h*
     h* 03/19/15  C:298 Support: 32825
     h*                 Added Date of Birth for claimant on Action Panel.
     h*
    h*  04/29/15 C:65  Project: SY251
    h*                 Added hidden field for Scriptura Module flag.
     h*
     h*  09/04/14 C:279 Project: 114WOE
     h*                 Modified the action panel to have Claim Handling
     h*                 Instructions Link.
     h*
     h*  07/28/14 C:295 Project: 114WMD
     h*                 Modified the action panel to use to load
     h*                 Claim Action Plan Tab.
     h*
     h*  02/10/14 C:279 Redmine: 3882
     h*                 Modified to open 'Additional Claims' link in
     h*                 inquiry mode.
     h*
     h*  12/13/13 C:290 SUPPORT WC090 83881
     h*                 Added Location Information in Action Panel.
     h*
     h*  05/10/13 C:279 Redmine: 2872
     h*                 change for Insured Party Link.
     h*
     h*  04/16/13 C:279 Redmine: 2501
     h*                 change for Agency name link.
     h*
     h**********************************************************************

     P LoadActPan      B
     D LoadActPan      PI
      ****** Beg Add ******** 07/28/14 **************************************
     D inClmPln                       1n   options(*nopass)
      ****** End Add ******** 07/28/14 **************************************
      /Free
       //****** Beg Add ******** 07/28/14 **************************************
       indClmDis = *off;
       If %parms > 0;
          If inClmPln  = *on;
             indClmDis = *on;
          Endif;
       Endif;
       //****** End Add ******** 07/28/14 **************************************

       // Claim Demographic Information
       scClmName =  %Trim((getClaimantN(cmss#)));

       //*** begin add ***  04/29/15  *******************************************
       GetScriptura(scScriptur);
       //*** end addd ****  04/29/15  *******************************************

       // Claim Information
       scClm#=%EditC(CMfnd:'X') + '-' + %EditC(CMfyr:'X') + '-' +
       %EditC(CMcase:'X');
       scClmAccDt= AlphaUSA(CvtToDate(cmAcDt));

       //***  Begin Chg  ***  06/09/20  **********************
       //scSS#=%Subst(CMss#:1:3) + '-' + %Subst(CMss#:4:2) + '-' +
       //%Subst(CMss#:6:4);
       scSS# = DisplaySS#(MaskData('SSN':'CMss#':CMss#));
       //***  End Chg  ***  06/09/20  **********************


       // Group Information
       //**** Beg Add ******** 12/14/15 ****************************        *
       // scGroup=%EditC(CMfnd:'X') + ' ' + getGrpName(CMfnd:xxRrn:CMco#) ;
       scGroup=%EditC(CMfnd:'X') + ' ' +
                      %subst(getGrpName(CMfnd:xxRrn:CMco#):1:36);
       //**** End Add ******** 12/14/15 *****************************       *

       // Employer Information
       Chain keyWMEML pem00;
       If %Found(wmeml);
          xxPhn# = GetEmpPhn#(CMco#:CMfnd:CMemp#:CMdiv:xxArea);
          scEmpPhn#='(' + %EditC(XXArea:'X') + ') ' +
          %Subst(%EditC(xxPhn#:'X'):1:3) + '-' +
          %Subst(%EditC(xxPhn#:'X'):4:4);

       // get the Polciy effective date
          scEmpEfDt = dtMDYY(RtvPolFrDt(CMco#:CMfnd:CMemp#:CMDiv:CMfyr));

       // get the Cancellation effective date
          xxCxDate = GetCxDate(CMco#:CMfnd:CMemp#:CMdiv);

          If xxCxDate <> 0;
             scEmpCxDt1= dtMDYY(xxCxDate);
             cancelled=*on;
          EndIf;
          //****** beg chg ***** 05/10/13 **************************
          // scEmpName=GetEmpName(CMemp#:CMdiv:xxEmRRN1:CMfnd:CMco#);
          clear strEmpName;
          strEmpName = GetEmpName(CMemp#:CMdiv:xxEmRRN1:CMfnd:CMco#);
          scEmpName = strEmpName;
          scEmNmTlTp = 'Policy Summary for ' + %trim(strEmpName);
          //****** end chg ***** 05/10/13 **************************
          //****** Beg Add ***** 04/25/20 **************************
          intRRN =  GetEmpYrRR(0:CMemp#:CMdiv:CMfnd:CMco#:' ');
          If scEmpName <> *blanks;
             scHdgFile# = 'Policy Number';
          //*** scFile# = %editc(CMFnd : 'X') + %editc(CMEmp# : 'X')
          //*** %editc(CMFyr : 'X');
           scFile# = getEmpPol#(intRRN:'E');
          Endif;
          //****** End Add ***** 04/25/20 **************************
          //****** Beg Add ***** 05/28/15 **************************
          If EMpAd1 <> *blanks;
             scEmpAdr1=EMpAd1;
             scEmpAdr2=EMpAd2;
             scEmpAdr3=*Blanks;

             If scEmpAdr2<>*Blanks;
                If %Subst(%EditC(EMpZip:'X'):6:4)='0000';
                   scEmpAdr3=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                   %Subst(%EditC(EMpZip:'X'):1:5);
                Else;
                  scEmpAdr3=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                  %Subst(%EditC(EMpZip:'X'):1:5) + '-' +
                  %Subst(%EditC(EMpZip:'X'):6:4);
                EndIf;
             Else;
                If %Subst(%EditC(EMpZip:'X'):6:4)='0000';
                   scEmpAdr2=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                   %Subst(%EditC(EMpZip:'X'):1:5);
                Else;
                  scEmpAdr2=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                  %Subst(%EditC(EMpZip:'X'):1:5) + '-' +
                  %Subst(%EditC(EMpZip:'X'):6:4);
                EndIf;
             EndIf;
          Else;
          //****** End Add ***** 05/28/15 **************************
          scEmpAdr1=EMmAd1;
          scEmpAdr2=EMmAd2;
          scEmpAdr3=*Blanks;

          If scEmpAdr2<>*Blanks;
             If %Subst(%EditC(EMmZip:'X'):6:4)='0000';
                scEmpAdr3=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %Subst(%EditC(EMmZip:'X'):1:5);
             Else;
               scEmpAdr3=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %Subst(%EditC(EMmZip:'X'):1:5) + '-' +
                 %Subst(%EditC(EMmZip:'X'):6:4);
             EndIf;
          Else;
             If %Subst(%EditC(EMmZip:'X'):6:4)='0000';
                scEmpAdr2=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %Subst(%EditC(EMmZip:'X'):1:5);
             Else;
               scEmpAdr2=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %Subst(%EditC(EMmZip:'X'):1:5) + '-' +
                 %Subst(%EditC(EMmZip:'X'):6:4);
             EndIf;
          EndIf;
          //****** Beg Add ***** 05/28/15 **************************
          Endif;
          //****** End Add ***** 05/28/15 **************************
       EndIf;

       // Contact  Information

       kkPcnt='1';
       scEmpEmail = %trim(GetEmpEmail(CMCo#:CMFnd:CMemp#:CMdiv:kkPcnt));
       If scEmpEmail=*Blanks;
          scEmalTT=noEmail;
       Else;
          scEmalTT=scEmpEmail;
       EndIf;

       //***** Beg Chg ***** 04/16/13 *************************************
       // Location Information
       //locFound=*off;
       //locNotFnd=*off;

       // Get mailing address info for location
       //
       //xxmode = 'M' ;
       //locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:xxName:
       //           xxMad1:xxMzip:xxMcty:xxMst:xxphn#:xxArea);

       // Get Physical address info for location
       xxmode = 'P' ;
       //locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:xxName:
       //           xxPad1:xxPzip:xxPcty:xxPst:xxphn#:xxArea);

       //If locFound=*on;
          // scLocName=xxName;
          //If xxPAd1<>*Blanks;
          //   scLocAdr1=xxPad1;


       // Physical city, state, zip
       //      If %Subst(%EditC(xxPzip:'X'):6:4)='0000';
       //         scLocCSZ=%Trim(xxPcty) + ', ' + xxPst + ' ' +
       //         %Subst(%EditC(xxPzip:'X'):1:5);
       //      Else;
       //         scLocCSZ=%Trim(xxPcty) + ', ' + xxPst + ' ' +
       //         %Subst(%EditC(xxPzip:'X'):1:5) + '-' +
       //         %Subst(%EditC(xxPzip:'X'):6:4);
       //      EndIf;
       //   Else;
       //   scLocAdr1=xxMad1;

       // Mailing city, state, zip
       //      If %Subst(%EditC(xxMZip:'X'):6:4)='0000';
       //         scLocCSZ=%Trim(xxMcty) + ', ' + xxMst + ' ' +
       //         %Subst(%EditC(xxMzip:'X'):1:5);
       //      Else;
       //         scLocCSZ=%Trim(xxMcty) + ', ' + xxMst + ' ' +
       //         %Subst(%EditC(xxMZip:'X'):1:5) + '-' +
       //         %Subst(%EditC(xxMZip:'X'):6:4);
       //      EndIf;
       //   EndIf;

       //   If xxPhn#<>*Zeros;
       //      scLocPhn='(' + %EditC(xxArea:'X') + ') ' +
       //      %Subst(%EditC(xxPhn#:'X'):1:3) + '-' +
       //      %Subst(%EditC(xxphn#:'X'):4:4);
       //   EndIf;
       //Else;
       //   locNotFnd=*on;
       //EndIf;

       //If scLocEmail =*Blanks;
       //   scLmalTT=noEmail;
       //Else;
       //   scLmalTT=scLocEmail;
       //Endif;
       //***** End Chg ***** 04/16/13 ***********************************
       //***** Beg Add ***** 12/13/13 *************************************
       // Location Information
          locFound=*off;
          locNotFnd=*off;

       // Get mailing address info for location

          xxmode = 'M' ;
          locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:xxName:
                    xxMad1:xxMzip:xxMcty:xxMst:xxphn#:xxArea);

       // Get Physical address info for location
          xxmode = 'P' ;
          locFound = GetLocInfo(CMco#:CMfnd:CMemp#:CMdiv:CMctg1:xxMode:xxName:
                    xxPad1:xxPzip:xxPcty:xxPst:xxphn#:xxArea);

          If locFound=*on;
             scLocName=xxName;
             If xxPAd1<>*Blanks;
                scLocAdr1=xxPad1;


       // Physical city, state, zip
                If %Subst(%EditC(xxPzip:'X'):6:4)='0000';
                   scLocCSZ=%Trim(xxPcty) + ', ' + xxPst + ' ' +
                   %Subst(%EditC(xxPzip:'X'):1:5);
                Else;
                   scLocCSZ=%Trim(xxPcty) + ', ' + xxPst + ' ' +
                   %Subst(%EditC(xxPzip:'X'):1:5) + '-' +
                   %Subst(%EditC(xxPzip:'X'):6:4);
                EndIf;
             Else;
                scLocAdr1=xxMad1;

       // Mailing city, state, zip
                If %Subst(%EditC(xxMZip:'X'):6:4)='0000';
                   scLocCSZ=%Trim(xxMcty) + ', ' + xxMst + ' ' +
                   %Subst(%EditC(xxMzip:'X'):1:5);
                Else;
                   scLocCSZ=%Trim(xxMcty) + ', ' + xxMst + ' ' +
                   %Subst(%EditC(xxMZip:'X'):1:5) + '-' +
                   %Subst(%EditC(xxMZip:'X'):6:4);
                EndIf;
             EndIf;

             If xxPhn#<>*Zeros;
                scLocPhn='(' + %EditC(xxArea:'X') + ') ' +
                %Subst(%EditC(xxPhn#:'X'):1:3) + '-' +
                %Subst(%EditC(xxphn#:'X'):4:4);
             EndIf;
          Else;
             locNotFnd=*on;
          EndIf;

       //***** End Add ***** 12/13/13 ***********************************
       //***** beg add ***** 09/04/14 ***********************************
       If isInstExist(CMco#:CMfnd:CMemp#:CMdiv) = *on;
          isClmInstV = *on;
       Else;
          isClmInstV = *off;
       Endif;
       //***** end add ***** 09/04/14 ***********************************

       // Agent Information
       GetAgtinfo(cmco#:cmfnd:cmemp#:cmdiv:scAgtClmTt:scAgEm:scAgtPhn#);
       scAgent = scAgtClmTt;

       If scAgEm=*Blanks;
          scAgEmTT=noEmail;
       Else;
          scAgEmTT=scAgEm;
       EndIf;

       // Only Show the limits if the claim is flagged as Employer Liability
       If CMliaF='Y';
          // Populate increased limits
           GetIncLimit(CMco#:CMfnd:CMemp#:CMdiv:Cmfyr:CMjrst:strIncLt1
           :strIncLt2:intInEffDt1:intInEffDt2:strLiblmtD:intPolEfdt:
           OutincLt1D:OutincLt2D);

           If  strIncLt1 <> *blanks  or strIncLt2 <> *blanks;
              scLimitHd1='Increased Limit #1';
              scLimitHd2='Increased Limit #2';
              scInEffDt1  = DTMDYY(intInEffDt1);
              scInclt1    = OutincLt1D;
              scInEffDt2  = DTMDYY(intInEffDt2);
              scInclt2    = OutincLt2D;
           Else;
              scLimitHd1='Increased Limit Factor';
              scLimitHd2='Policy Effective Date';
              scliblmtD   = strLiblmtD;
              scPolEffdt  = DTMDYY(intPolEfdt);
           Endif;
        Else;
           Clear scLimitHd1;
           Clear scLimitHd2;
           Clear scInEffDt1;
           Clear scInclt1;
           Clear scInEffDt2;
           Clear scInclt2;
           Clear scliblmtD;
           Clear scPolEffdt;
        Endif;

       // Check whether the claim exists as multiclaim
        MultClm# = Getmulticlm(CMco#:Cmfnd:Cmfyr:Cmcase);

       // URL Info Employer (Actual)
       //***** Beg Del **** 04/25/20 ***********************************
       //intRRN =  GetEmpYrRR(0:CMemp#:CMdiv:CMfnd:CMco#:' ');
       //***** End Del **** 04/25/20 ***********************************
       strRRN = %Editc(intRRN:'X');
       //***** beg chg ****** 05/10/13 *********************************
       //scEmURL = CreateURL('STDRVPGM':scEmpName:strRRN:'D':'WTPOLSUM');
       scEmURL = CreateURL('STDRVPGM':strEmpName:strRRN:'D':'WTPOLSUM');
       //***** end chg ****** 05/10/13 *********************************

       // URL Info AGENT
       //***** Beg Chg ***** 04/16/13 ***********************************
       //intRRN =  GetAgtRRN(CMfnd:1:CMco#);
       //strRRN = %Editc(intRRN:'X');
       //scAgURL = CreateURL('STDRVPGM':scAgtClmTt:strRRN:'D':'STDEFAULT');

       sysDate7 = DateTo7(SystemDate);
       xxname = scAgtClmTt;
       GetEmpAgt(CMco#:CMfnd:CMemp#:CMdiv:CMfyr:sysDate7:xxname:intRrn);
       clear ##kc#;
       clear ##kl#;
       ##call = '1';
       ##mode1 = '*';
       ##qte = ' ';
       strRRN = %Editc(intRRN:'X');
       ##view = '1';
       scAgUrl = CreateURL('W0040RW':scAgtClmTt:##kc#:##kl#:##call:strRrn:
        ##view:##qte:##mode1);

       //***** End Chg ***** 04/16/13 ***********************************

       //Multi Claim Link (Dummy)
       //*** Begin Change *** 04/04/21 ************************************
       //scMlURL=CreateURL('STDRVPGM':'Multiple Claims':@@rrn:'D':'W0321RW   ');
       If @@Mode = 'D';
          scMlURL = CreateURL('STDRVPGM':'Multiple Claims':@@rrn:
                              'D':'W0321RW   ');
       Else;
          scMlURL = CreateURL('STDRVPGM':'Multiple Claims':@@rrn:
                              'F':'W0321RW   ');
       Endif;
       //*** End   Change *** 04/04/21 ************************************

       //Additional Claim Link
       If chkDupSS#(CMco#:CMss#)=*On;
          dupClaim=*On;
          programIn='WTSEARCH';
          tabNameIn='Additional Claims';
          clear dsSrchCrit;
          clear selectIn;
          dsClmSS#=CMss#;
          selectIn=dsSrchCrit;
          //******** beg chg ******** 02/10/14 *******************************
          // callIn = '2';
          callIn = '1';
          //******** end chg ******** 02/10/14 *******************************
          scAdUrl=CreateBIGUrl(programIn:tabNameIn:modeIn:callIn:
          rrnOut:selectIn);
       Else;
          dupClaim=*Off;
       EndIf;

       // Claimant DOB
       scClmDOB = AlphaUSA(CvtToDate(ClaimDOB(CMss#)));

       //***** Beg Add ***** 07/18/16 ***********************************
       //Get the ISO Module Flag
       IsISO = IsModEnabled('ISOWorkComp');
       If IsISO = *On;
          scISOCtScr = CMisoC;
          scISOPrScr = CMisoP;
          scISOInScr = CMisoI;
       Endif;
       //***** End Add ***** 07/18/16 ***********************************
      /End-Free
     P LoadActPan      E

