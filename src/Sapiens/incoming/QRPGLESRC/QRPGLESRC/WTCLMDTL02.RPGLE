     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim detail                 box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 05/03/2012 by C:062 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Customer Master File
     Fwmtpp     if   e             disk    rename(ptp01:ptp0p)
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DClaimDetail02    pr                  ExtPgm('WTCLMDTL02')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimDetail02    pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
      *
     D physFeinO       s              9
     D physModO        s              3
     D physNameO       s             40
     D physAdrO        s             40
     D physCityO       s             25
     D physStateO      s              2
     D physZipO        s             11
     D physPhoneO      s             15
     D physFaxO        s             15
     D physRoleO       s             40
     D physDtRangeO    s             25
     D endDateO        s              1
     D clinicNameO     s             30
     D kkCo#           s                   like(GTco#)
     D kkTbId          s                   like(GTtbId)
     D kkKey           s                   like(GTkey)

     C     keyWTGTL      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey
      *
      /Free

       Chain(n) inRRN ptp0p;

       exsr BuildData;

       outJSON = ' ';
       BuildJSON(outJSON :'OutPhysFeinO' :physFeinO);
       BuildJSON(outJSON :'OutPhysModO' :physModO);
       BuildJSON(outJSON :'OutPhysNameO' :physNameO);
       BuildJSON(outJSON :'OutPhysAdrO' :physAdrO);
       BuildJSON(outJSON :'OutPhysCityO' :physCityO);
       BuildJSON(outJSON :'OutPhysStateO' :physStateO);
       BuildJSON(outJSON :'OutPhysZipO' :physZipO);
       BuildJSON(outJSON :'OutPhysPhoneO' :physPhoneO);
       BuildJSON(outJSON :'OutPhysFaxO' :physFaxO);
       BuildJSON(outJSON :'OutPhysRoleO' :physRoleO);
       BuildJSON(outJSON :'OutPhysDtRangeO' :physDtRangeO);
       BuildJSON(outJSON :'CbxPhysEndDateO' :endDateO);
       BuildJSON(outJSON :'OutClinicNameO' :clinicNameO);
       BuildJSON(outJSON :'LnkTreatingPhysEmailO' :TPemAd);

       RtnCode = ' ';
       Return;
       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

          physFeinO=%EditC(TPphyF:'4');
          physModO=%EditC(TPphyM:'4');
          physNameO=TPphyN;
          physAdrO=TPphyA;
          physCityO=TPphyC;
          physStateO=TPphyS;
          physZipO=%EditW(TPphyZ:'     -    ');

          If TPphyR<>0;
             physPhoneO=%Trim(%EditW(TPphyR:'0(   )')) + ' ' +
              %Trim(%EditW(TPphyP:'0   -    '));
          Else;
             If TPphyP<>0;
                physPhoneO=%Trim(%EditW(TPphyP:'0   -    '));
             Else;
                physPhoneO=*Blanks;
             EndIf;
          EndIf;

          If TPfxAr<>0;
             physFaxO=%Trim(%EditW(TPfxAr:'0(   )')) + ' ' +
              %Trim(%EditW(TPfax#:'0   -    '));
          Else;
             If TPfax#<>0;
                physFaxO=%Trim(%EditW(TPfax#:'0   -    '));
             Else;
                physFaxO=*Blanks;
             EndIf;
          EndIf;

          kkCo#=TPco#;
          kkTbId='PHCD';
          kkKey=TPpRol;
          Chain keyWTGTL pgt00;
          If %Found(WTGTL);
             physRoleO=%Trim(TPpRol) + ' - ' + %Trim(GTdesc);
          Else;
             physRoleO=TPpRol;
          EndIf;
          physDtRangeO=%Trim(%EditW(dtMDYY(TPfrDt):'  /  /    ')) + '-' +
           %Trim(%EditW(dtMDYY(TPtoDt):'  /  /    '));
          endDateO=TPeDat;
          clinicNameO=TPclin;

       endsr;
      /End-Free
