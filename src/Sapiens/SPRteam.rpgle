     D*=====================================================================
     D* Team structure related procedure prototypes
     D*=====================================================================
     D*
     D* Revisions:
     D*
     D* 10/17/13  C:267 Redmine: 1243
     D*                 Create new API IsLeader.  If user is NOT a leader
     D*                 disable My Work/My Team's Work radio buttons on the
     D*                 user's home page.
     D*
     P*=====================================================================
     P* GetTeamName - Return team name for input team id
     P*          Parm 1 = Team ID (5 0)
     P*          Returns = Team Name (20A)
     P*=====================================================================
     DGetTeamName      pr            20
     D inTeamId                       5  0 const
     P*=====================================================================
     P* GetTeamRRN - Return team RRN for input team id
     P*          Parm 1 = Team ID (5 0)
     P*          Returns = Team RRN (9 0)
     P*=====================================================================
     DGetTeamRRN       pr             9  0
     D inTeamId                       5  0 const
     P*=====================================================================
     P* OnTeam - procedure to check is user is already on team
     P*   Team ID (5, 0)
     P*   User ID (10 A)
     P*   Returns indicator - *On if found on team
     P*=====================================================================
     DOnTeam           pr             1n
     D inTID                          5  0 const
     D inUserID                      10    const
     D inChkSubTeams                   n   const
     D*
     P*=====================================================================
     P* GetUserTeamList - procedure to get User's Team List
     P*   User ID (10 A)
     P*   User Type (1 A)  (sub-team (T) or User)
     P*   Team array (10A 9999)
     P*   Staff counter (5 0)
     P*   Full team Flag (1 A) - optional, blanks/nopass for direct. Y for full Hierachy
     P*=====================================================================
     DGetUserTeamList  pr
     D inUserId                      10    const
     D inType                          n   const
     D inPtrTeam                       *
     D inCount                        5  0
     D inFull                         1    const options(*nopass)
     D*
     P*=====================================================================
     P* OnUserTeams - Procedure to check if one user is already on other user's team
     P*   Lead ID (10 A)
     P*   User ID (10 A)
     P*   Returns indicator - *On if found on team
     P*=====================================================================
     DOnUserTeams      pr             1n
     D inLeadId                      10    const
     D inUserID                      10    const
      *
     P**** Beg Add 10/17/13 ************************************************
     P*=====================================================================
     P* IsLeader - procedure to check is user is a team leader
     P*   User ID (10 A)
     P*   Returns indicator - *On if found as team leader
     P*=====================================================================
     DIsLeader         pr             1n
     D inUserID                      10    const
     D inChkLeader                     n   const
     D*
     P**** End Add 10/17/13 ************************************************
