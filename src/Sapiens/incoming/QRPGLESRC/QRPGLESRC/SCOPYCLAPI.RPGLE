     *========================================================================*
     h*copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h* StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*/TITLE WTCLMACTPI - Profound Claim Action Panel Load PI
     h**********************************************************************
     h*
     h*  Description:  This copybook will be used to declare the procedure
     h*  Interface which will load Claim action panel in Tranform  screens
     h*
     h*  Called from: Many prograns
     h*  Calls to:    None
     h*
     h*
     h*  Created on 3/7/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     P LodActPnlC      B
     D LodActPnlC      PI
      /Free

       // Claim Demographic Information
       Chain CMss# pcd00;
       If %Found(wmcdl);

          If cdMid<>*Blanks;
             scClmName=%trim(CDlsn) + ', ' +
              %trim(CDfsn) + ' ' + %trim(CDmid) + '.';
          Else;
             scClmName=%trim(CDlsn) + ', ' + %trim(CDfsn);
          EndIf;

       Else;
          scClmName=*Blanks;
       EndIf;


       // Claim Information

       scClm#=%EditC(CMfnd:'X') + '-' + %EditC(CMfyr:'X') + '-' +
        %EditC(CMcase:'X');

       date = %Date(CMacDt:*cymd);
       scAcDt=%char(date);

       scSS#=%Subst(CMss#:1:3) + '-' + %Subst(CMss#:4:2) + '-' +
        %Subst(CMss#:6:4);


       // Group Information
       //
       Chain keyWMFNL pfn00;
       If %Found(wmfnl);
          scGroup=%EditC(FNfnd:'X') + ' ' + FNname;
       EndIf;



       // Employer Information
       //
       Chain keyWMEML pem00;
       If %Found(wmeml);
          If EMphn#<>0;
             scEphn#='(' + %EditC(EMarea:'X') + ') ' +
              %Subst(%EditC(EMphn#:'X'):1:3) + '-' +
              %Subst(%EditC(EMphn#:'X'):4:4);
          EndIf;



       //
       //  Check the employer effective and cancellation dates
       //    get the most currect effective date
       //
          y=%Lookup(0:ed);
          If y=0;
             y=5;
          Else;
             y=y-1;
          EndIf;


       //
       // Convert effective date from cymd to mdcy
       //
          If y>0;
             If cd(y)<>0;
                date=%Date(cd(y):*cymd);
                scECxD=%Char(date);
                cancelled=*on;
             EndIf;
          EndIf;

          scEmpl=EMname;
          scEadr1=EMmAd1;
          scEadr2=EMmAd2;

          If %Subst(%EditC(EMmZip:'X'):6:4)='0000';
             scECSZ=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
              %Subst(%EditC(EMmZip:'X'):1:5);
          Else;
             scECSZ=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
              %Subst(%EditC(EMmZip:'X'):1:5) + '-' +
              %Subst(%EditC(EMmZip:'X'):6:4);
          EndIf;
       EndIf;


       kkPcnt='1';
       Chain keyWMECL1 pec01;
       If %Found(wmecl1);
          If ecfsn<>*Blanks;
             scEmal=ECeMal;
          EndIf;
          If scEmal=*Blanks;
             scEmalTT=noEmail;
          EndIf;
       EndIf;



       //
       // Location Information
       //
       locFound=*off;
       locNotFnd=*off;
       Chain keyWMALL pal00;
       If %Found(wmall);
          locFound=*on;
          scAnam=ALname;

          If ALpAd1<>*Blanks;
             scLAdr1=ALpAd1;


       //
       // Physical city, state, zip
       //

             If %Subst(%EditC(ALpZip:'X'):6:4)='0000';
                scLCSZ=%Trim(ALpCty) + ', ' + ALpSt + ' ' +
                 %Subst(%EditC(ALpZip:'X'):1:5);
             Else;
                scLCSZ=%Trim(ALpCty) + ', ' + ALpSt + ' ' +
                 %Subst(%EditC(ALpZip:'X'):1:5) + '-' +
                 %Subst(%EditC(ALpZip:'X'):6:4);
             EndIf;
          Else;
             scladr1=ALmAd1;



       //
       // Mailing city, state, zip
       //
             If %Subst(%EditC(ALmZip:'X'):6:4)='0000';
                scLCSZ=%Trim(ALmCty) + ', ' + ALmSt + ' ' +
                 %Subst(%EditC(ALmZip:'X'):1:5);
             Else;
                sclcsz=%Trim(ALmCty) + ', ' + ALmSt + ' ' +
                 %Subst(%EditC(ALmZip:'X'):1:5) + '-' +
                 %Subst(%EditC(ALmZip:'X'):6:4);
             EndIf;
          EndIf;

          If ALphn#<>*Zeros;
             scLPhn='(' + %EditC(ALarea:'X') + ') ' +
              %Subst(%EditC(ALphn#:'X'):1:3) + '-' +
              %Subst(%EditC(ALphn#:'X'):4:4);
          EndIf;
       Else;
          locNotFnd=*on;
       EndIf;

       If scLmal=*Blanks;
          scLmalTT=*Blanks;
       Endif;



       //
       // Agent Information
       //
       SetGt keyWMAHL pah00;
       ReadPe keyWMAHL pah00;
       Chain keyWMAAL1 paa01;
       Chain keyWMAGL pag00;
       If %Found(wmagl);
       //
          If AGphn1<>*Zeros;
             scAPhn='(' + %EditC(AGare1:'X') + ') ' +
              %Subst(%EditC(AGphn1:'X'):1:3) + '-' +
              %Subst(%EditC(AGphn1:'X'):4:4);
          EndIf;
          scAgEm=AGeMal;
          If scAgEm=*Blanks;
             scAgEmTT=noEmail;
          EndIf;
          scAgent=AGname;
       EndIf;
       //
       //

      /End-Free
     P LodActPnlC      E

