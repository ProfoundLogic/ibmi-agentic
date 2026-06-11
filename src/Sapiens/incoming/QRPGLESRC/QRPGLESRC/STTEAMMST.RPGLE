     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('STBNDDIR')
     H/TITLE STTEAMMST- Work with teams
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Work With Teams                         *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 02/14/12  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     h*                                                               *
     h* 09/04/23  C:342 Support: 113738/152                           *
     h*                 Modiifed code to avoid record lock on file    *
     h*                 SDTSL                                         *
     h*                                                               *
     h* 03/21/22  C:333 Project: SY434                                *
     h*                 - Update Grids for Sorting Filtering Upgrade  *
     h*                                                               *
     h* 07/17/14  C:279 Support: OP099 84759                          *
     h*                 Modiifed to correct authority validation.     *
     h*                                                               *
     h* 06/12/14  C:279 Support: OP099 84759                          *
     h*                 Modiifed to remove authority validation for   *
     h*                 loading team master maintenance subfile.      *
     h*                                                               *
     h* 03/25/14  C:279 Support: OP099 84759                          *
     h*                 Modiifed to add edit for updating/adding      *
     h*                 deallocated team leader.                      *
     h*                                                               *
     H*****************************************************************

     Fstteammst cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(scr01s:RRNt)
     fsmtel     uf a e           k disk    rename(pte01:pte00)
     f                                     infds(dbfds)
     fsdtsl     uf a e           k disk    rename(pts01:pts00)
     fsmtep     if   e             disk    rename(pte01:pte0p)
     fsdtsp     if a e             disk    rename(pts01:pts0p)
     D*
     D***ENTRY
     DTeamMaintenance  pr                  extpgm('STTEAMMST')
     D                                9
     D                                1
     D                               10
     DTeamMaintenance  pi
     D inTeamRRN                      9
     D inMode                         1
     D inNextPgm                     10
     D*
     DChkAuthority     pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTYP                         1    const
     D inCLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D*
     DTeamMembers      pr                  EXTPGM('STTEAMMBR')
     D inTeamRRN                      9
     D inMode                         1    const
     D inNextPgm                     10    const
     D*
     DAddTeam          pr             9  0
     D inCO#                          3  0
     D inTeamName                    20
     D inLeaderId                    10
     D inDescription                 60
     D*
     DAddMember        pr             1
     D inCo#                          3  0
     D inTeamId                       5  0
     D inLeaderId                    10
     D inRole                        15    const
     D*
     DUpdateLeader     pr
     D inCo#                          3  0
     D inTeamId                       5  0
     D inLeaderId                    10
     D inOldLeaderId                 10
      *
     DValidated        pr              n
     D inMode                         5    const
     D*
     DTeamAction       pr
     D inTeamID                       5  0 const
     D inAction                      10    const
     D*
     DisAuthorized     PR              n
     D inUserLevel                    3  0 const
     D inUserType                     1    const
     D inLeaderID                    10    const
     D outLeaderNm                   50
     D*
     D/copy sprDate
     D/copy sprUsrPrms
     D/copy sprTeam
     D/copy sprAtrium
      ****** beg add ******** 03/25/14 ***********************************
     D/copy sprGenPop
      ****** end add ******** 03/25/14 ***********************************
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     D psds          esds                  extname(szq1p)
     D*
     D inTeamID        S              5  0
     D inAction        S             10
     D inUserLevel     S              3  0
     D inUserType      S              1
     D inLeaderID      S             10
     D*
     D isError         S               n   inz
     D isAddTeamError  S               n   inz
     D intFilterTeam   S              5  0 inz
     D intCo#          S              3  0 inz
     D intTeamRRN      S              9  0 inz
     D strLdrName      S             50
     D strName         S             50
     D strUser         S                   like(Q1USER)
     D strPGM          S                   like(Q1USER)
     D RRNt            S              5  0
     D strRRN          S              9    inz
     D strOutQ         s             10
     D strStaff        s              3
     D strData         s            512
     D strlength       s              5  0
     D svLeaderId      s                   like(scLeaderId)

      /Free

         Exsr Initialize;
         DoU 1=0;
           Exsr LoadTeamsSubfile;

           ExFmt scr01c;
           Select;
           When lnkAdd = *On;
             Exsr CreateTeam;
           Other;
             ExSr CheckSFLOptions;
           Endsl;
         EndDo;

         *InLr = *On;
         return;

       //*******************************************************************************************
       //  Initialize  Subroutine
       //*******************************************************************************************
       BegSr Initialize;

         strUser = q1user;
         strPGM = q1PGM;

         intCo# = GetUserInfo(strUser:strName:strOutQ:strStaff:
                   inUserType:inUserLevel);
         SCTitle = strName;

         If %parms > 0;
           intTeamRRN = %int(inTeamRRN);
           chain intTeamRRN pte0p;
           intFilterTeam = TEid;
         EndIf;
       EndSr;

       //*******************************************************************************************
       //  The LoadTeamsSubfile Subroutine
       //*******************************************************************************************
       BegSr LoadTeamsSubfile;

         clrTeam = *On;
         Write scr01c;
         clrTeam = *Off;
         RRNt = 0;

         If intFilterTeam = 0;
           Setll *loval pte00;
           Read(n) pte00;
         Else;
           Setll intFilterTeam pte00;
           Reade(n) intFilterTeam pte00;
         Endif;
         Dow Not %EOF(smtel);
           //****** beg del ****** 06/12/14 ***********************************
           // if isAuthorized(inUserLevel:inUserType:temgr:strLdrName);
           //****** end del ****** 06/12/14 ***********************************
             //****** beg add ****** 07/17/14 ***********************************
             isAuthorized(inUserLevel:inUserType:temgr:strLdrName);
             //****** end add ****** 07/17/14 ***********************************
             RRNt +=1;
             scTmRRN = dbrrn;
             scTmID = TEID;
             scTmName = TENAME;
             scTmMgrID = TEMGR;
             scTmMgrNm = strLdrName;
             scTmDesc = TEDESC;
             Write scr01s;
           //****** beg del ****** 06/12/14 ***********************************
           // Endif;
           //****** end del ****** 06/12/14 ***********************************
           If intFilterTeam = 0;
             Read(n) pte00;
           Else;
             Reade(n) intFilterTeam pte00;
           Endif;
         Enddo;
       EndSr;

       //*******************************************************************************************
       //  The CheckSFLOptions Subroutine
       //*******************************************************************************************
       BegSr CheckSFLOptions;

       DoU %Eof;
         ReadC scr01s;
         If %Eof;
           Leave;
         EndIf;
         Select;
         When lnkView = *On;
           TeamAction(scTmID:'VIEW');
           lnkView = *Off;
           Leave;
         When lnkEdit = *On;
           TeamAction(scTmID:'EDIT');
           lnkEdit = *Off;
           Leave;
         When lnkMembers = *On;
           // Call members program here
           strRRN = %Editc(GetTeamRRN(scTmId):'X');
           TeamMembers(strRRN:'C':' ');
           lnkMembers = *Off;
           Leave;
         EndSl;
       EndDo;

       EndSr;
       //*******************************************************************************************
       //  ClearScreen 02 values
       //*******************************************************************************************
       BegSr ClrScr02Values;
         clear scr02;
         scAddName = ' ';
         scAddDesc = ' ';
         scLeaderID = ' ';
         scSecLvl1 = 0;
         scSecLvl2 = 0;
         scSecLvl3 = 0;
         scSecLvl4 = 0;
         scSecLvl5 = 0;
         scSecLvl6 = 0;
         scSecTyp1 = ' ';
         scSecTyp2 = ' ';
         scSecTyp3 = ' ';
         scSecTyp4 = ' ';
         scSecTyp5 = ' ';
         scSecTyp6 = ' ';
         indDisable = '0';
         indVisible= '1';
         scBtnText = 'Add';
       EndSr;
       //*******************************************************************************************
       //  CreateTeam Subroutine
       //*******************************************************************************************
       BegSr CreateTeam;
         Exsr ClrScr02Values;
         scPnlTitle = 'Add Team';
         DoU isError = *Off;
           Exfmt scr02;
           isError = *Off;
           Select;
           When btnCancel = *On;
             Leave;
           When btnAdd = *On;
             If Validated('ADD');
               strRRN=%Editc(AddTeam(intCo#:scAddName:scLeaderID:scAddDesc)
                :'X');
               // If Add was sucessful - call members program, else set error
               If strRRN <> '000000000';
                  TeamMembers(strRRN:'A':' ');
               Else;
                 isError = *On;
                 isAddTeamError = *On;
               Endif;
             Else;
               isError = *On;
             Endif;
           EndSl;
         EndDo;
       EndSr;
      /End-Free
       //*******************************************************************************************
       // TeamAction - Perform actions on team
       //*******************************************************************************************
     PTeamAction       b
     DTeamAction       PI
     D inTeamID                       5  0 const
     D inAction                      10    const
      /Free
       Select;
       When inAction = 'VIEW';
         indDisable = '1';
         indVisible = '0';
         scBtnText = ' ';
         scPnlTitle = 'View Team';
       When inAction = 'EDIT';
         indDisable = '0';
         indVisible = '1';
         scBtnText = 'Update';
         scPnlTitle = 'Edit Team';
       EndSl;
       // Load Values
       Exsr LoadScr02Values;
       DoU btnCancel = *On;
         Exfmt scr02;
         Select;
         When btnCancel = *On;
           Leave;
         When btnAdd = *On;
         // Validate screen inputs
           If Validated(inAction);
             Exsr LoadScrToFile;
             Leave;
           EndIf;
         EndSl;
       EndDo;

       //*******************************************************************************************
       //  Load Screen 02 values from File
       //*******************************************************************************************
       BegSr LoadScr02Values;
         chain inTeamId pte00;
         If %Found;
           scAddName = TEName;
           scAddDesc = TEDesc;
           strLdrName = GetUserName(TEMgr);
           scLeaderID = TEMgr;
           svLeaderId = TEMgr;
           scSecLvl1 = TESCL1;
           scSecLvl2 = TESCL2;
           scSecLvl3 = TESCL3;
           scSecLvl4 = TESCL4;
           scSecLvl5 = TESCL5;
           scSecLvl6 = TESCL6;
           scSecTyp1 = TESCT1;
           scSecTyp2 = TESCT2;
           scSecTyp3 = TESCT3;
           scSecTyp4 = TESCT4;
           scSecTyp5 = TESCT5;
           scSecTyp6 = TESCT6;
         EndIf;
         //****** beg add ******** 03/25/14 **********************************
         scTmNmId = inTeamId;
         //****** end add ******** 03/25/14 **********************************
       EndSr;
       //*******************************************************************************************
       //  Load Screen 02 values to File
       //*******************************************************************************************
       BegSr LoadScrToFile;
           TEMgr = scLeaderID;
           // If Leader Id changed then add new leader to team (if not already there)
           If scLeaderId <> svLeaderId;
             UpdateLeader(TEco#:TEid:scLeaderId:svLeaderId);
           Endif;
           TEName = scAddName;
           TEDesc = scAddDesc;
           TESCL1 =  scSecLvl1;
           TESCL2 =  scSecLvl2;
           TESCL3 =  scSecLvl3;
           TESCL4 =  scSecLvl4;
           TESCL5 =  scSecLvl5;
           TESCL6 =  scSecLvl6;
           TESCT1 =  scSecTyp1;
           TESCT2 =  scSecTyp2;
           TESCT3 =  scSecTyp3;
           TESCT4 =  scSecTyp4;
           TESCT5 =  scSecTyp5;
           TESCT6 =  scSecTyp6;

           teuser = strUser;
           tepgm = strPGM;
           tetime = TimeTo6(SystemTime);
           tedt = DateTo7(SystemDate);
           Update pte00;
       EndSr;
      /End-Free
     PTeamAction       e
       //*******************************************************************************************
       // isAuthorized - procedure to test user authority on team
       //*******************************************************************************************
     PisAuthorized     b
     DisAuthorized     PI             1n
     D inLevel                        3  0 const
     D inType                         1    const
     D inID                          10    const
     D outLeaderNm                   50
     D*
     D strInLvl        s              3
     D strUpLvl        s              3
     D intUpLvl        s              3  0 inz
     D strType         s              1
     D flgRes          s               n   inz('1')
     D intTmpCo#       s              3  0 inz
      /Free
         strInLvl = %editc(inLevel:'X');

         intTmpCo# = GetUserInfo(inID:outLeaderNm:strOutQ:strStaff:
                      strType:intUpLvl);
         if intTmpCo# <> 0;
           strUpLvl = %editc(intUplvl:'X');
           callp ChkAuthority(inType:strInLvl:strtype:strUpLvl:'1':flgRes);
         endif;
         return not flgRes;
      /End-Free
     PisAuthorized     e

       //*******************************************************************************************
       // AddTeam - procedure to Add new Team
       //*******************************************************************************************
     PAddTeam          b
     DAddTeam          PI             9  0
     D inCO#                          3  0
     D inTeamName                    20
     D inLeaderId                    10
     D inDescription                 60
     D*
     D intAddTeamID    s              5  0
     D intAddTeamRRN   s              9  0
      /Free
         //Get latest team Id available;
       monitor;
         setgt *hival pte00;
         readp(n) pte00;
         if not %eof;
           intAddTeamID = TEID + 1;
         else;
           intAddTeamId = 1;
         endif;

         teco# = inCo#;
         teid = intAddTeamID;
         tename = inTeamName;
         teDesc = inDescription;
         teMgr = inLeaderID;
         teStat = 'A';

         teiusr = strUser;
         teipgm = strPGM;
         teitm = TimeTo6(SystemTime);
         teidt = DateTo7(SystemDate);

         teuser = teiusr;
         tepgm = teipgm;
         tetime = teitm;
         tedt = teidt;

         write pte00;
         intAddTeamRRN = dbrrn;

         // Add Leader to staff file;
         AddMember(inCo#:intAddTeamid:inLeaderId:'Team Leader');
         return intAddTeamRRN;
       on-error;
         return 0;
       endmon;
      /End-Free
     PAddTeam          e
       //*******************************************************************************************
       // AddMember - procedure to Add new Team member
       //*******************************************************************************************
     PAddMember        b
     DAddMember        PI             1
     D inCo#                          3  0
     D inTeamId                       5  0
     D inLeaderId                    10
     D inRole                        15    const
     D*
      /Free
         // Add Leader to staff file;
         tsco# = inco#;
         tsusr = inLeaderId;
         tstid = inTeamId;
         tstype = ' ';
         tsRole = inRole;
         tsStat = 'A';

         tsiusr = strUser;
         tsipgm = strPGM;
         tsitm = TimeTo6(SystemTime);
         tsidt = DateTo7(SystemDate);

         tsuser = tsiusr;
         tspgm = tsipgm;
         tstime = tsitm;
         tsdt = tsidt;

         write pts0p;
         Return *On;
      /End-Free
     PAddMember        e
       //*******************************************************************************************
       // UpdateLeader - procedure to Update leader on team
       //*******************************************************************************************
     PUpdateLeader     b
     DUpdateLeader     PI
     D inCo#                          3  0
     D inTeamId                       5  0
     D inLeaderId                    10
     D inOldLeaderId                 10
      /Free
        Chain (inTeamId:inOldLeaderId) pts00;
        If %Found(sdtsl);
          tsRole = GetUsrTitl(inOldLeaderId);
          tsuser = strUser;
          tspgm = strPGM;
          tstime = TimeTo6(SystemTime);
          tsdt = DateTo7(SystemDate);
          Update pts00;
        Endif;
        Chain (inTeamId:inLeaderId) pts00;
        If %Found(sdtsl);
          tsRole = 'Team Leader';
          tsuser = strUser;
          tspgm = strPGM;
          tstime = TimeTo6(SystemTime);
          tsdt = DateTo7(SystemDate);
          //****** beg add ****** 03/25/14 ***********************************
          If TSstat = 'R';
             TSstat = 'A';
          Endif;
          //****** end add ****** 03/25/14 ***********************************
          Update pts00;
        Else;
          AddMember(inCo#:inTeamid:inLeaderId:'Team Leader');
        EndIf;
      /End-Free
     PUpdateLeader     e
       //*******************************************************************************************
       // Validated - Validate entry values
       //*******************************************************************************************
     PValidated        b
     DValidated        pi              n
     D inMode                         5    const
     D*
     D foundError      s               n   inz
      ******* beg add ********* 03/25/14 **********************************
     D strMsg          s            256
      ******* end add ********* 03/25/14 **********************************
      /Free
          foundError = *Off;
          If scAddName = *blanks;
            erAddName = *On;
            mgAddName = 'Team name must be entered';
            foundError = *On;
          EndIf;

          If scAddDesc = *blanks;
            erAddDesc = *On;
            mgAddDesc = 'Team Desc must be entered';
            foundError = *On;
          EndIf;

          If scLeaderId = *blanks;
            erLeaderId = *On;
            mgLeaderId = 'Leader Id must be entered';
            foundError = *On;
          Else;
            If GetUserName(scLeaderId) = *blanks;
              erLeaderId = *On;
              mgLeaderId = 'Leader Id not found';
              foundError = *On;
            EndIf;
          EndIf;

          //***** beg add ******* 03/25/14 ******************************
          If foundError = *off;
             //***  Beg Chg  **** 09/04/23 *******************************
             //*** Chain (scTmNmId:scLeaderId) pts00;
             Chain(n) (scTmNmId:scLeaderId) pts00;
             //***  End Chg  **** 09/04/23 *******************************
             If %Found(sdtsl) and TSstat = 'R';
                strMsg = 'Selected Leader is currently deallocated from +
                 this team. Assigning user as Leader will change status +
                 to reallocated.';
                If PopTwoBtn(strMsg:' ':'Continue':'Cancel') = *off;
                   foundError = *on;
                Endif;
             Endif;
          Endif;
          //***** end add ******* 03/25/14 ******************************

          Return not foundError;
      /End-Free
     PValidated        e
