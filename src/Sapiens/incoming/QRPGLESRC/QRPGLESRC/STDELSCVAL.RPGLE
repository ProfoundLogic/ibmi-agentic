     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to delete security setting and return.
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
     fsaasl     uf   e           k disk    rename(pas01:pas00)

      /copy sPrAtrium
      /copy sPrDate
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

     D strArea         s             10    inz
     D intCo#          s              3  0 inz
     D flgEnv          s              1    inz
      /Free

       outJSON = ' ';
       *Inlr = *On;

       //Extract params from GenParm format; ddddddddddFFFFFFFFFFaaaaaaaaaaLLLLLLLLLLLLL<View Sec><E
       strArea = %Trim(%Subst(inGenParm:21:10));
       flgEnv =  %Trim(%Subst(inGenParm:31:1));

       If strArea = *Blanks;
         BuildJSON(outJSON :'RtnCode' :'Blank Area ID');
         RtnCode = 'X';
         Return;
       Endif;

       intCo# = GetUserInfo(Q1user);
       //Process request
       Chain (intCo#:strArea:flgEnv) pas00;
       If %Found(saasl);
           Delete pas00;
           BuildJSON(outJSON :'RtnCode' :'Data deleted sucessfully');
       Else;
           BuildJSON(outJSON :'RtnCode' :'Record not found for Deletion');
       Endif;

       RtnCode = ' ';
       Return;
