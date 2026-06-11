     *========================================================================*
     h*copyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h*StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h**********************************************************************
     h*
     h*  Description:  This copybook will be used to declare the procedure
     h*  Interface which will load MC action panel in Tranform screens
     h*
     h*
     h*  Created on 5/22/2019 by C:212 for Project MC430
     h**********************************************************************
     h* Revisions:
    h*
     h*
     h*  06/09/20  C:295 Project : SY420
     h*                  Mask SSN
     h*
     h**********************************************************************

     P LoadMCActPan    B
     D LoadMCActPan    PI
     D inDsbActMnu                    1n   options(*nopass)

     D kkst            s             10    inz
      ***** Beg Add *** 06/09/20 *******************************************
     D SSN             s             11    inz
      ***** Beg Add *** 06/09/20 *******************************************

      /Free
       vsbActMnu = *on;
       dsbActMnu = *off;
       If %parms > 0;
          If inDsbActMnu = *on;
             vsbActMnu = *off;
             dsbActMnu = *on;
          Endif;
       Endif;

       // Claim Demographic Information
       scClmName =  %Trim(EOlsn) + ', ' + %trim(EOfsn) + ' ' + %trim(EOmid);

       // Claim Information
       scClm# = %EditC(EOsub#:'X') + '-' + %EditC(EOfnd:'X') + '-' +
                %EditC(EOfyr:'X') + '-' + %EditC(EOcase:'X');
       Monitor;
        scClmAccDt= %date(EOacdt:*cymd);
       On-Error;
        scClmAccDt= *loval;
       EndMon;

       // EOB Number
       scEOB# = %Char(EOeob#);

       // Group Information
       Chain(n) (EOco#:EOsub#:EOfnd) pfn01;
       scGroup=%EditC(EOfnd:'X') + ' ' + %Trim(FNname);

       // Provider Information
       Chain(n) (EOco#:EOsub#:EOfnd:EOfyr:EOcase) pcm01;
       Chain(n) (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:EOrcn) pea01;
       Chain(n) (EOfein:EOmod) ppd01;
       scProv# = %EditC(EOfein:'X') + '-' + %EditC(EOmod:'X');
       scProvNm = %Trim(EOpnam);
       scProvPhn# = '(' + %EditC(PDarea:'X') + ') ' +
       %Subst(%EditC(PDphn#:'X'):1:3) + '-' +
       %Subst(%EditC(PDphn#:'X'):4:4);

       // Social Security Number
       //*** Beg Chg *** 06/09/20 *******************************************
       //*** scSS#=%Subst(CMss#:1:3) + '-' + %Subst(CMss#:4:2) + '-' +
       //*** %Subst(CMss#:6:4);
        scSS# =%Subst(CMss#:1:3) + '-' + %Subst(CMss#:4:2) + '-' +
        %Subst(CMss#:6:4);
        scSS# = MaskData('SSN':'scSS#':scSS#);
       //*** End Chg *** 06/09/20 *******************************************

       // Adjudication State
       kkst = EOast;
       Chain(n) kkst pst01;
       scAdjSt = EOast + '-' + %Trim(STname);

       // Service Zip
       scSrvZip = EOflic;

       // State Claim Number
       scStClm# = CMscl#;

      /End-Free
     P LoadMCActPan    E

