     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve List of secured areas on display file
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: App toolbar
     h*  Calls to:
     h*
     h*  Created on 10/31/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  05/19/14 C:283 RedMine: RedMine 4145 FCC.
     h*                 If environment is not passed as parameter, then
     h*                 default it to Live i.e. 'L'.
     h*
     h*  06/14/13 C:136 Project: SY307 Redmine 3168
     h*                 Adjust logic to check VIEW security first. If allowed,
     h*                 check EDIT; else, return 'HIDE'.
     h*
     h*  05/20/13 C:136 Project: SY307
     h*                 Security level/type are now pulled from SAASP which
     h*                 is keyed by CO#/AREA/ENV
     h**********************************************************************
     fsmasl     if   e           k disk    rename(pas01:pas00)
     fsdasl     if   e           k disk    rename(pas01:pds00)
     f                                     prefix(d)
      *** beg add ***** 05/20/13 *******************************************
     fsaasl     if   e           k disk    rename(pas01:pas0l)
      *** end add ***** 05/20/13 *******************************************

      /copy sPrAtrium
      /copy sprUsrPrms
      * *ENTRY
     DListSecFlds      pr                  ExtPgm('STSECFLDS')
     D                              512
     D                            32767
     D                                1
      *
     DListSecFlds      pi
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTYP                         1    const
     D inCLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
     D inCLV2                         3    const
     D inCLV3                         3    const
     D inCLV4                         3    const
     D inCLV5                         3    const
     D inCLV6                         3    const

     D psds          esds                  extname(szq1p)

     D strDspf         s             10    inz
     D strFormat       s             10    inz
     D strAccess       s              4    inz
     D strUser         s             10    inz
     D strUserName     s             50    inz
     D strOutQ         s             10    inz
     D strStaffCd      s              3    inz
     D strSecType      s              1    inz
     D strSecLevel     s              3    inz
     D intSecLevel     s              3  0 inz
     D intCo#          s              3  0 inz
     D flgRes          s              1    inz
      *** beg add ***** 05/20/13 *******************************************
     D outMessage      s            200
     D flgEnv          s              1    inz
      *** end add ***** 05/20/13 *******************************************
      /Free

       outJSON = ' ';

       //Extract params from GenParm format; ddddddddddFFFFFFFFFF
       strDspf = %Trim(%Subst(inGenParm:1:10));
       strFormat = %Trim(%Subst(inGenParm:11:10));

       If strDspf = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank display file name');
         RtnCode = 'X';
         Return;
       Endif;
       If strFormat = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank format name');
         RtnCode = 'X';
         Return;
       Endif;

       strUser = Q1user;
       intCo# = GetUserInfo(strUser :strUserName :strOutQ :strStaffCd
        :strSecType :intSecLevel);
       strSecLevel = %Editc(intSeclevel:'X');
       //*** beg add ***** 05/20/13 *******************************************
       flgEnv =  %Trim(%Subst(inGenParm:31:1));
       //*** end add ***** 05/20/13 *******************************************
       //*** beg add ***** 05/19/14 *******************************************
       If flgEnv = *Blanks;
          flgEnv = 'L';
       Endif;
       //*** end add ***** 05/19/14 *******************************************

       outJSON = '"secured_fields":';
       StartJSONarray(outJSON);

       //Process request
       Setll (intCo#:strDspf:strFormat) pas00;
       DoU %Eof(smasl);
         Reade (intCo#:strDspf:strFormat) pas00;
         If %Eof(smasl);
           Leave;
         Endif;

       //*** beg add ***** 05/20/13 **********************************
       Chain (intCo#:ASarea:flgEnv) pas0l;
       Select;
         When %found;
         When flgEnv = 'L';
           outMessage = 'No security information exists for ' + ASAREA +
            ' in Live';
           BuildJSON(outJSON :'RtnCode' : outMessage);
           RtnCode = 'X';
           Return;
         When flgEnv = 'Q';
           flgEnv = 'L';
           Chain (intCo#:ASarea:flgEnv) pas0l;
           If not(%found);
           outMessage = 'No security information exists for ' + ASAREA +
            ' in Live';
             BuildJSON(outJSON :'RtnCode' : outMessage);
             RtnCode = 'X';
             Return;
           Endif;
       Endsl;
       //*** end add ***** 05/20/13 **********************************

       //*** beg chg ***** 06/14/13 **********************************
         //***CheckSecurity(strSecType:strSeclevel:ASest1:%Editc(ASesl1:'X'):'1'
         //*** :flgRes:ASest2:ASest3:ASest4:ASest5:ASest6:%Editc(ASesl2:'X')
         //*** :%Editc(ASesl3:'X'):%Editc(ASesl4:'X'):%Editc(ASesl5:'X')
         //*** :%Editc(ASesl6:'X'));
         //***
         //***If flgRes = '0';
         //***  strAccess = 'EDIT';
         //***Else;
         //***  CheckSecurity(strSecType:strSeclevel:ASvst1:%Editc(ASvsl1:'X'):'1'
         //***   :flgRes:ASvst2:ASvst3:ASvst4:ASvst5:ASvst6:%Editc(ASvsl2:'X')
         //***   :%Editc(ASvsl3:'X'):%Editc(ASvsl4:'X'):%Editc(ASvsl5:'X')
         //***   :%Editc(ASvsl6:'X'));
         //***  If flgRes = '0';
         //***    strAccess = 'VIEW';
         //***  Else;
         //***    strAccess = 'HIDE';
         //***  Endif;
         //***Endif;

         //Check VIEW security level first
         CheckSecurity(strSecType:strSeclevel:ASvst1:%Editc(ASvsl1:'X'):'1'
          :flgRes:ASvst2:ASvst3:ASvst4:ASvst5:ASvst6:%Editc(ASvsl2:'X')
          :%Editc(ASvsl3:'X'):%Editc(ASvsl4:'X'):%Editc(ASvsl5:'X')
          :%Editc(ASvsl6:'X'));
         If flgRes = '0';
           strAccess = 'VIEW';
         //Then EDIT security level if allowed in VIEW
           CheckSecurity(strSecType:strSeclevel:ASest1:%Editc(ASesl1:'X'):'1'
            :flgRes:ASest2:ASest3:ASest4:ASest5:ASest6:%Editc(ASesl2:'X')
            :%Editc(ASesl3:'X'):%Editc(ASesl4:'X'):%Editc(ASesl5:'X')
            :%Editc(ASesl6:'X'));
           If flgRes = '0';
             strAccess = 'EDIT';
           Endif;
         Else;
           strAccess = 'HIDE';
         Endif;
       //*** end chg ***** 06/14/13 **********************************

       //*** beg chg ***** 05/20/13 **********************************
       //*** Setll (ASco#:ASdspf:ASfrmt:ASarea) pds00;
       //*** DoU %Eof(sdasl);
       //***   Reade (ASco#:ASdspf:ASfrmt:ASarea) pds00;
         Setll (ASco#:ASarea) pds00;
         DoU %Eof(sdasl);
           Reade (ASco#:ASarea) pds00;
       //*** end chg ***** 05/20/13 **********************************
           If %Eof(sdasl);
             Leave;
           Endif;
           StartJSONelemnt(outJSON);
           BuildJSON(outJSON :'field_id' :%Trim(DASfld));
           BuildJSON(outJSON :'field_access' :strAccess);
           EndJSONelemnt(outJSON);
         Enddo;

       Enddo;
       EndJSONarray(outJSON);

       RtnCode = ' ';
       Return;
