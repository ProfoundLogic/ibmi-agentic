     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to save updated security setting and return new values
     h*               - Activation group should be *CALLER
     h*         Exceptions to standard AJAX program rules in this program:
     h*               - This program does seton LR since file is being updated
     h*               - Process needs to lock a file for update
     h*               - This process requires to update a file
     h*
     h*  Called from: App toolbar
     h*  Calls to:
     h*
     h*  Created on 05/22/2013 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fsaasl     if a e           k disk    rename(pas01:pas00)

      /copy sPrAtrium
      /copy sPrDate
      /copy sprUsrPrms
      * *ENTRY
     DGetSecurity      pr                  ExtPgm('STADDSCVAL')
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
     D intCo#          s              3  0 inz
     D arrViewType     s              1    dim(6) inz
     D arrViewLvl      s              3  0 dim(6) inz
     D arrEditType     s              1    dim(6) inz
     D arrEditLvl      s              3  0 dim(6) inz
     D i               s              2  0 inz
     D pos             s              5  0 inz
     D flgEnv          s              1    inz
      /Free

       outJSON = ' ';
       *Inlr = *On;

       //Extract params from GenParm format; ddddddddddFFFFFFFFFFaaaaaaaaaaLLLLLLLLLLLLL<View Sec><E
       strDspf = %Trim(%Subst(inGenParm:1:10));
       strFormat = %Trim(%Subst(inGenParm:11:10));
       strArea = %Trim(%Subst(inGenParm:21:10));
       flgEnv =  %Trim(%Subst(inGenParm:31:1));
       pos = 45;
       For i = 1 to 6;
         arrViewType(i) = %Subst(inGenParm:pos:1);
         pos += 1;
       EndFor;
       For i = 1 to 6;
         Monitor;
           arrViewLvl(i) = %Int(%Subst(inGenParm:pos:3));
         On-Error;
           arrViewLvl(i) = 0;
         EndMon;
         pos += 3;
       EndFor;
       For i = 1 to 6;
         arrEditType(i) = %Subst(inGenParm:pos:1);
         pos += 1;
       EndFor;
       For i = 1 to 6;
         Monitor;
           arrEditLvl(i) = %Int(%Subst(inGenParm:pos:3));
         On-Error;
           arrEditLvl(i) = 0;
         EndMon;
         pos += 3;
       EndFor;

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
       //Process request
       Chain(n) (intCo#:strArea:flgEnv) pas00;
       If %Found(saasl);
         BuildJSON(outJSON :'RtnCode' :'Record Already Exist');
         RtnCode = 'X';
         Return;
       Else;
         ASCO# = intCo#;
         ASAREA= strArea;
         ASSTAT= 'A';
         ASenv =  flgEnv;
         ASvst1 = arrViewType(1);
         ASvst2 = arrViewType(2);
         ASvst3 = arrViewType(3);
         ASvst4 = arrViewType(4);
         ASvst5 = arrViewType(5);
         ASvst6 = arrViewType(6);

         ASvsl1 = arrViewLvl(1);
         ASvsl2 = arrViewLvl(2);
         ASvsl3 = arrViewLvl(3);
         ASvsl4 = arrViewLvl(4);
         ASvsl5 = arrViewLvl(5);
         ASvsl6 = arrViewLvl(6);

         ASest1 = arrEditType(1);
         ASest2 = arrEditType(2);
         ASest3 = arrEditType(3);
         ASest4 = arrEditType(4);
         ASest5 = arrEditType(5);
         ASest6 = arrEditType(6);

         ASesl1 = arrEditLvl(1);
         ASesl2 = arrEditLvl(2);
         ASesl3 = arrEditLvl(3);
         ASesl4 = arrEditLvl(4);
         ASesl5 = arrEditLvl(5);
         ASesl6 = arrEditLvl(6);

         ASuser = Q1user;
         ASpgm = Q1pgm;
         ASdt = DateTo7(SystemDate());
         AStime = TimeTo6(SystemTime());

         Write pas00;
         BuildJSON(outJSON :'RtnCode' :'Data written sucessfully');
       Endif;

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

       RtnCode = ' ';
       Return;
