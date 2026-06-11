     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve User Security check
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
     h* 11/11/20 C:33  Project: WC2671
     h*                Check Audit Request in inquiry mode
     h*
     h* 06/20/13 C:136 Project: SY307
     h*                Pull in security level and type from STPRP
     h*                for restricting access to Security Editor (APPSECFNC)
     h*
     h*    03/04/13  C:191  Redmine: 1548
     h*                      Include option# when chain to file
     h*
     h**********************************************************************
     fsdofl1    if   e           k disk

      /copy sPrAtrium
      /copy sprUsrPrms
      /copy sprPrprty
      //***  Begin Add  ***  11/11/20  **************
      /copy sPrSecure
      //***  End   Add  ***  11/11/20  **************
      * *ENTRY
     DCheckUserSecLvl  pr                  ExtPgm('STCHKSECLV')
     D                              512
     D                            32767
     D                                1
      *
     DCheckUserSecLvl  pi
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

     D strOption       s             10    inz
     D flgRes          s              1    inz
     D flgQte          s              1    inz
     D flgType         s              1    inz
     D intOption#      s              2  0 inz
     D intCo#          s              3  0 inz
      //***  Begin Add  ***  11/11/20  **************
     dstrAccess        s             25    varying inz('DENY')
      //***  End   Add  ***  11/11/20  **************
     D strUser         s             10    inz
     D strUserName     s             50    inz
     D strOutQ         s             10    inz
     D strStaffCd      s              3    inz
     D strSecType      s              1    inz
     D strSecLevel     s              3    inz
     D intSecLevel     s              3  0 inz
     d strPrptyReturn  s            256

      /Free

       outJSON = ' ';

       //Extract params from GenParm format; uuuuuuuuuuOOOOOOOOOOqTnn
       strUser = %Trim(%Subst(inGenParm:1:10));
       strOption = %Trim(%Subst(inGenParm:11:10));
       flgQte = %Trim(%Subst(inGenParm:21:1));
       flgType = %Trim(%Subst(inGenParm:22:1));
       Monitor;
         intOption# = %Int(%Subst(inGenParm:23:2));
       On-Error;
         intOption# = 0;
       EndMon;

       // Set defaults if not passed in
       If strUser = *Blanks;
         strUser = Q1user;
       Endif;

       //Retrieve user information
       intCo# = GetUserInfo(strUser :strUserName :strOutQ :strStaffCd
        :strSecType :intSecLevel);
       strSecLevel = %Editc(intSeclevel:'X');

       If flgQte = ' ';
         flgQte = 'L';
       Endif;

       Select;
       When strOption = 'APPSECFNC';
          strPrptyReturn = SysProperty('SecurityEditorLevelType');
          If strPrptyReturn  <> *blanks;
             OFlvl  = %int(%subst(strPrptyReturn : 1 : 3));
             OFty01 = %subst(strPrptyReturn : 4 : 1);
          Else;
             OFlvl = 999;
             Ofty01 = '*';
          Endif;
       //***  Begin Add  ***  11/11/20  *********
       When strOption = 'AUDRQSTINQ';
          If CheckSecLevel('AudRqstInq':strSecType:intSeclevel);
             strAccess    = 'ALLOW';
          Endif;
          BuildJSON(outJSON :'access':strAccess);
          Return;
       //***  End   Add  ***  11/11/20  *********
       Other;
          Chain(n) (strOption:flgQte:flgType:intOption#) pof01;
          If not(%found) and flgQte = 'Q';
             flgQte = 'L';
             Chain(n) (strOption:flgQte:flgType:intOption#) pof01;
          Endif;
          If not %Found(sdofl1);
            BuildJSON(outJSON :'access':'ALLOW');
            BuildJSON(outJSON :'information' :'Security setup not found');
            Return;
          Endif;
       Endsl;

       CheckSecurity(strSecType:strSeclevel:OFty01:%Editc(OFlvl:'X'):'1'
        :flgRes:OFty02:OFty03:OFty04:OFty05:OFty06:%Editc(OFlv02:'X')
        :%Editc(OFlv03:'X'):%Editc(OFlv04:'X'):%Editc(OFlv05:'X')
        :%Editc(OFlv06:'X'));

       If flgRes = '0';
         BuildJSON(outJSON :'access':'ALLOW');
       Else;
         BuildJSON(outJSON :'access':'DENY');
       Endif;

       RtnCode = ' ';
       Return;
