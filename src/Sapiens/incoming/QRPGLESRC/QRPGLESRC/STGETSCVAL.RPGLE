     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve current security setting for area
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
     h*  05/20/13 C:136 Project: SY307
     h*                 Security level/type are now pulled from SAASP which
     h*                 is keyed by CO#/AREA/ENV
     h**********************************************************************
     fsmasl     if   e           k disk    rename(pas01:pas00)
      *** beg add ***** 05/20/13 *******************************************
     fsaasl     if   e           k disk    rename(pas01:pas0l)
      *** end add ***** 05/20/13 *******************************************

      /copy sPrAtrium
      /copy sprUsrPrms
      * *ENTRY
     DGetSecurity      pr                  ExtPgm('STGETSCVAL')
     D                              512
     D                            32767
     D                                1
      *
     DGetSecurity      pi
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D psds          esds                  extname(szq1p)

     D strDspf         s             10    inz
     D strFormat       s             10    inz
     D strArea         s             10    inz
     D strChangeKey    s             13    inz
     D intCo#          s              3  0 inz
      *** beg add ***** 05/20/13 *******************************************
     D outMessage      s            200
     D flgEnv          s              1    inz
      *** end add ***** 05/20/13 *******************************************
      /Free

       outJSON = ' ';

       //Extract params from GenParm format; ddddddddddFFFFFFFFFFaaaaaaaaaa
       strDspf = %Trim(%Subst(inGenParm:1:10));
       strFormat = %Trim(%Subst(inGenParm:11:10));
       strArea = %Trim(%Subst(inGenParm:21:10));

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
       If strArea = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank Area ID');
         RtnCode = 'X';
         Return;
       Endif;

       intCo# = GetUserInfo(Q1user);
       flgEnv =  %Trim(%Subst(inGenParm:31:1));
       //Process request
       Chain (intCo#:strDspf:strFormat:strArea) pas00;
       If %Found(smasl);
       //** beg add ***** 05/20/13 *******************************************
         Chain (intCo#:ASarea:flgEnv) pas0l;
         Select;
           When %found(SAASL);
           When flgEnv = 'L';
             outMessage = 'No security information exists for ' + ASAREA +
              ' in Live';
             BuildJSON(outJSON :'RtnCode' : outMessage);
             RtnCode = 'X';
             Return;
           When flgEnv = 'Q';
             flgEnv = 'L';
             Chain (intCo#:ASarea:flgEnv) pas0l;
             If Not(%Found(SAASL));
               outMessage = 'No security information exists for ' + ASAREA +
                ' in Live';
               BuildJSON(outJSON :'RtnCode' : outMessage);
               RtnCode = 'X';
               Return;
             Endif;
         Endsl;
         BuildJSON(outJSON :'Environment' :flgEnv);
       //** end add ***** 05/20/13 *******************************************
         BuildJSON(outJSON :'ViewSecurityType1' :ASvst1);
         BuildJSON(outJSON :'ViewSecurityType2' :ASvst2);
         BuildJSON(outJSON :'ViewSecurityType3' :ASvst3);
         BuildJSON(outJSON :'ViewSecurityType4' :ASvst4);
         BuildJSON(outJSON :'ViewSecurityType5' :ASvst5);
         BuildJSON(outJSON :'ViewSecurityType6' :ASvst6);

         BuildJSON(outJSON :'ViewSecurityLevel1' :%Editc(ASvsl1:'X'));
         BuildJSON(outJSON :'ViewSecurityLevel2' :%Editc(ASvsl2:'X'));
         BuildJSON(outJSON :'ViewSecurityLevel3' :%Editc(ASvsl3:'X'));
         BuildJSON(outJSON :'ViewSecurityLevel4' :%Editc(ASvsl4:'X'));
         BuildJSON(outJSON :'ViewSecurityLevel5' :%Editc(ASvsl5:'X'));
         BuildJSON(outJSON :'ViewSecurityLevel6' :%Editc(ASvsl6:'X'));

         BuildJSON(outJSON :'EditSecurityType1' :ASest1);
         BuildJSON(outJSON :'EditSecurityType2' :ASest2);
         BuildJSON(outJSON :'EditSecurityType3' :ASest3);
         BuildJSON(outJSON :'EditSecurityType4' :ASest4);
         BuildJSON(outJSON :'EditSecurityType5' :ASest5);
         BuildJSON(outJSON :'EditSecurityType6' :ASest6);

         BuildJSON(outJSON :'EditSecurityLevel1' :%Editc(ASesl1:'X'));
         BuildJSON(outJSON :'EditSecurityLevel2' :%Editc(ASesl2:'X'));
         BuildJSON(outJSON :'EditSecurityLevel3' :%Editc(ASesl3:'X'));
         BuildJSON(outJSON :'EditSecurityLevel4' :%Editc(ASesl4:'X'));
         BuildJSON(outJSON :'EditSecurityLevel5' :%Editc(ASesl5:'X'));
         BuildJSON(outJSON :'EditSecurityLevel6' :%Editc(ASesl6:'X'));

         strChangeKey = %Editc(ASdt:'X') + %Editc(AStime:'X');
         BuildJSON(outJSON :'LastChangeKeyInfo' : strChangeKey);
       Else;
         BuildJSON(outJSON :'RtnCode' :'Area not found!');
       Endif;

       RtnCode = ' ';
       Return;
