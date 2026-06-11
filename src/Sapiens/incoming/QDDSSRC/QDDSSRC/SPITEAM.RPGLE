     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H nomain
     h**********************************************************************
     h* Revisions:
     H*
     H* 12/14/20  C:33  Support: 93439
     H*                 Account for recursive Team assignments.
     H*
     H* 05/22/19  C:300 Support: 79584 (S1 Medical)
     H*                 When change to My teams Work on the Home page,
     H*                 Assignment Tab on Home page is not pulling any
     H*                 people from sub-team.
     h*
     h* 10/16/17  C:33  Support: 40271
     h*                 Do not return team members w/o user parm record
     h*
     h* 06/24/14  C:180 Redmine: 4367
     h*                 Modified to prevent infinite loop when loading
     h*                 groups for allocation.
     h*
     h* 12/04/13  C:279 Support: OP099 84293
     h*                 Modiifed to not add Team Id as user Id in user team
     h*                 list.
     h*
     h* 10/17/13  C:267 Redmine: 1243
     h*                 Create new API IsLeader.  If user is NOT a leader
     h*                 disable My Work/My Team's Work radio buttons on the
     h*                 user's home page.
     h*
     h**********************************************************************
     Fsmtel     if   e           k disk    rename(pte01:pte00)
     F                                     infds(tefds)
     Fsmtel1    if   e           k disk    rename(pte01:pte01)
     Fsdtsl     if   e           k disk    rename(pts01:pts00)
     F                                     infds(tsfds)
     Fsdtsl1    if   e           k disk    rename(pts01:pts01)
     Fsmupl     if   e           k disk    rename(pup01:pup00)

     D***  Begin Add  ***  12/14/20  **********
     D arrTeamSub      s             10    dim(9999)
     D intCountSub     s              5  0 inz
     D***  End   Add  ***  12/14/20  **********
     D*
     D/copy sprTeam
     D*
     D tefds           ds
     D te_rrn                397    400b 0
     D*
     D tsfds           ds
     D ts_rrn                397    400b 0
     P*=====================================================================
     P* GetTeamName - Returns TeamName for given team ID
     P*  Returns - Team Name
     P*=====================================================================
     PGetTeamName      B                   Export
     DGetTeamName      PI                  Like(TEname)
     D inTeamID                            Like(TEid) const
     D
     D strTeamID       s                   Like(TEid)   inz
     D strTeamNm       s                   Like(TEname)  inz
      /Free
          strTeamId = inTeamId;
          Chain strTeamID pte00;
          If %Found;
             strTeamNm = TEname;
          EndIf;
          Return strTeamNm;
      /End-Free
     PGetTeamName      E
     P*=====================================================================
     P* GetTeamRRN - Returns Team RRN for given team ID
     P*  Returns - Team RRN
     P*=====================================================================
     PGetTeamRRN       B                   Export
     DGetTeamRRN       pi             9  0
     D inTeamID                            Like(TEid) const
     D
     D strTeamID       s                   Like(TEid)   inz
     D intTeamRRN      s              9  0 inz
      /Free
          strTeamId = inTeamId;
          Chain strTeamID pte00;
          If %Found;
             intTeamRRN = te_rrn;
          EndIf;
          Return intTeamRRN;
      /End-Free
     PGetTeamRRN       E
     P*=====================================================================
     P* OnTeam - procedure to check is user is already on team
     P*   Team ID (5, 0)
     P*   User ID (10 A)
     P*   Returns indicator - *On if found on team
     P*=====================================================================
     POnTeam           b                   export
     DOnTeam           pi             1n
     D inTID                          5  0 const
     D inUserID                      10    const
     D inChkSubTeams                   n   const
     D*
     D strCurrId       s             10    inz
     D*
      /Free
       chain(n) (inTID:inUserID) pts00;
       If %Found;
         Return *On;
       EndIf;

       // Check subteams with recursive calls
       If inChkSubTeams;
         //sdtsl1 has select for teams only - so only teams will be read in this loop
         Setll inTID  pts01;
         Reade(n) inTID pts01;
         Dow not %eof(sdtsl1);
           strCurrId = TSusr;
           If OnTeam(%Int(TSusr):inUserId:inChkSubTeams);
             Return *On;
           EndIf;
           //Reset pointer since recursive OnTeam call will mess it up
           Setgt (inTID:strCurrId) pts01;
           Reade(n) inTID pts01;
         EndDo;
       EndIf;

       // Not found return *Off
       Return *Off;
      /End-Free
     POnTeam           e
     P*=====================================================================
     P* GetUserTeamList - procedure to get User's Team List
     P*   User ID (10 A)
     P*   User Type (1 A)  (sub-team (T) or User)
     P*   Team array (10A 9999)
     P*   Staff counter (5 0)
     P*   Full team Flag (1 A) - optional, blanks/nopass for direct. Y for full Hierachy
     P*=====================================================================
     PGetUserTeamList  b                   export
     DGetUserTeamList  pi
     D inUserId                      10    const
     D inType                          n   const
     D inPtrTeam                       *
     D inCount                        5  0
     D inFull                         1    const options(*nopass)
     D***  Begin Add  ***  12/14/20  **********
     D inInz                          1n   const options(*nopass)
     D***  End   Add  ***  12/14/20  **********
     D*
     D ptrTeam         s               *   inz
     D arrTeam         s             10    dim(9999) based(ptrTeam)
     D flgFull         s              1    inz
     D intTeam         s              5  0 inz
     D***  Begin Add  ***  12/14/20  **********
     D isInz           s              1n   inz('1')
     D***  End   Add  ***  12/14/20  **********
     D strUser         s             10    inz
     D*
      /Free
       If %Parms > 4;
         flgFull = inFull;
       EndIf;
       //***  Begin Add  ***  12/14/20  **************
       If %parms >= 6;
          isInz = inInz;
       Endif;
       If isInz;
          Clear arrTeamSub;
          Clear intCountSub;
       Endif;
       //***  End   Add  ***  12/14/20  **************

       ptrTeam = inPtrTeam;

       If %LookUp(inUserId :arrTeam) = 0 and inType <> 'T';
         inCount = inCount + 1;
         arrTeam(inCount) = inUserId;
       EndIf;

       If inType = 'T';
         intTeam = %Int(inUserId);
         ExSr ReadStaffFile;
       Else;
         //Read thru teams file to see if user is manager to any team
         Setll inUserID pte01;
         DoU %eof(smtel1);
           Reade inUserID pte01;
           If %eof(smtel1);
             Leave;
           EndIf;
           intTeam = TEid;
           ExSr ReadStaffFile;
         EndDo;
       EndIf;
       return;
       //********************************************************************************
       //* Read team-staff file
       //********************************************************************************
       BegSr ReadStaffFile;
         // Read thru team staff file to retrieve users
         Setll intTeam pts00;
         DoU %eof(sdtsl);
           Reade intTeam pts00;
           If %eof(sdtsl);
             Leave;
           EndIf;
           // Verify record exists in user parms
           Chain TSusr smupl;
         //***  Begin Chg  ***  12/14/20  *****************
         ////***  Begin Chg  ***  05/22/19  *****************************
         ////*****  If not %found;
         //If not %found and TStype = *Blanks;
         ////***  End   Chg  ***  05/22/19  *****************************
         //   Iter;
         //Endif;
           Select;
           When not %found and TStype = *Blanks;
              Iter;
           When TStype = 'T' and %lookup(TSusr:arrTeamSub) <> 0;
              Iter;
           Endsl;
         //***  End   Chg  ***  12/14/20  *****************

           // Add user to team list
           If TStype = *Blanks;
             If TSStat = 'A' And %LookUp(TSusr :arrTeam) = 0;
               inCount = inCount + 1;
               arrTeam(inCount) = TSusr;
             EndIf;
           Elseif flgFull = 'Y' And TSStat = 'A';
             //Active sub-team found and full hierarchy requested
           //***  Begin Add  ***  12/14/20  *****************
             intCountSub += 1;
             arrTeamSub(intCountSub) = TSusr;
           //***  End   Add  ***  12/14/20  *****************
             strUser = TSusr;
             ptrTeam = %Addr(arrTeam);
           //***  Begin Chg  ***  12/14/20  *****************
           //GetUserTeamList(TSusr :'T' :ptrTeam :inCount :flgFull);
             GetUserTeamList(TSusr :'T' :ptrTeam :inCount :flgFull :*off);
           //***  End   Chg  ***  12/14/20  *****************
             SetGt (intTeam:strUser) pts00;
           EndIf;
         EndDo;
       EndSr;
      /End-Free
     PGetUserTeamList  e
     P*=====================================================================
     P* OnUserTeams - Procedure to check if one user is already on other user's team
     P*   Lead ID (10 A)
     P*   User ID (10 A)
     P*   Returns indicator - *On if found on team
     P*=====================================================================
     POnUserTeams      b                   export
     DOnUserTeams      pi             1n
     D inLeadId                      10    const
     D inUserID                      10    const
      *
     D ptrTeamList     s               *   inz
     D intCount        s              5  0 inz
     D index           s              5  0 inz
     D arrTeamList     s             10    dim(9999) inz
     D rtcd            s               n   inz
      /Free
         rtcd = *Off;
       // get team list for lead
         ptrTeamList = %Addr(arrTeamList);
         GetUserTeamList(inLeadId :' ' :ptrTeamList :intCount :'Y');
         If %lookup(inUserId:arrTeamList:1:intCount) > 0;
           rtcd = *On;
         Endif;
         Return rtcd;
      /End-Free
     POnUserTeams      e
     P*=====================================================================
     P* IsLeader - procedure to check is user is a team leader
     P*   User ID (10 A)
     P*   Returns indicator - *On if found as a leader
     P*=====================================================================
     PIsLeader         b                   export
     DIsLeader         pi             1n
     D inUserID                      10    const
     D inChkSubTeams                   n   const
     D*
     D strCurrId       s             10    inz
     D*
      /Free
       chain (inUserID) pte01;
       If %Found;
         Return *On;
       Else;
       // Not found return *Off
        Return *Off;
       EndIf;
      /End-Free
     PIsLeader         e
