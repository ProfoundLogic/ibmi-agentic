     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hbnddir('SPIUSRPRMS')
     H nomain
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 10/24/18  C:180 Project: 129WVH                                                    *
     H*                 Return contacts update                                             *
     H*                                                                                    *
     H* 02/15/18  C:33  Support: 51241                                                     *
     H*                 Return CO# if only one exists                                      *
     H*                                                                                    *
     H* 11/13/17  C:33  Support: 49909                                                     *
     H*                 Get x-ref record, if not found                                     *
     H**************************************************************************************
     Fsmupl     if   e           k disk     infds(upfds)
     Fsmupl1    if   e           k disk    Rename(pup01:pup02)
      ***  Begin Add  ***  11/13/17  *************************************
     Fsxupl     if   e           k disk    rename(pup01:xup00)
      ***  End   Add  ***  11/13/17  *************************************
     D*
     D upfds           ds
     D uprrn                 397    400b 0

     D/copy sprusrprms
     D***  Begin Add  ***  02/15/18  **************************************
     D/copy sprcompany
     D***  End   Add  ***  02/15/18  **************************************
     P*=====================================================================
     P* getNbrUsrs - retrieves the number of non-PDS users
     P*  Returns - number of users
     P*=====================================================================
     PgetNbrUsrs       B                   Export
     DgetNbrUsrs       PI             3  0
     D
     D users           s              3  0
     C*
     C                   read      smupl                                  99
     C                   dow       *in99 = *off
     C                   if        %subst(upusr:1:7) <> 'PDSPGMR' and
     C                             upusr <> 'WUSERU' and
     C                             upusr <> 'FSUSER' and
     C                             upusr <> 'FSCLT'
     C                   eval      users = users + 1
     C                   endif
     C                   read      smupl                                  99
     C                   enddo
     C
     C                   return    users
     PgetNbrUsrs       E

     P*=====================================================================
     P* GetUserName - Returns UserName for given User ID
     P*  Returns - User Name
     P*=====================================================================
     PGetUserName      B                   Export
     DGetUserName      PI                  Like(UPUSNM)
     D inUserID                            Like(UPUSR) const
     D
     D strUserID       s                   Like(UPUSR)   inz
     D strUserNm       s                   Like(UPUSNM)  inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strUserNm = UPUSNM;
       //***  Begin Add  ***  11/13/17  **********************************
          Else;
             Clear UPnewUsr;
             Chain (strUserId) sxupl;
             If UPnewUsr <> *blank;
                Clear UPusNm;
                Chain (UPnewUsr) pup01;
                strUserNm = UPusNm;
             Endif;
       //***  End   Add  ***  11/13/17  **********************************
          Endif;
          Return strUserNm;
      /End-Free
     PGetUserName      E
     P*=====================================================================
     P* GetUserInfo - Returns User company number along with general information
     P*  Returns - Company #
     P*=====================================================================
     PGetUserInfo      B                   Export
     DGetUserInfo      PI                  Like(UPCO#)
1    D inUserID                            Like(UPUSR) const
2    D outUserName                         Like(UPUSNM) options(*nopass)
3    D outUserOutQ                         Like(UPOUTQ) options(*nopass)
4    D outUserStaff                        Like(UPSTAF) options(*nopass)
5    D outUserType                         Like(UPTYPE) options(*nopass)
6    D outUserLvl                          Like(UPLVL) options(*nopass)
     D
     D strUserID       s                   Like(UPUSR)  inz
     D intCo#          s                   Like(UPCO#)  inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             intCo# = UPCO#;
             If %Parms > 5;
                outUserLvl = UPLVL;
             EndIf;
             If %Parms > 4;
                outUserType= UPTYPE;
             EndIf;
             If %Parms > 3;
                outUserStaff = UPSTAF;
             EndIf;
             If %Parms > 2;
                outUserOutQ = UPOUTQ;
             EndIf;
             If %Parms > 1;
                outUserName = UPUSNM;
             EndIf;
          Endif;
          //***  Begin Add  ***  02/15/18  ********************************
          If intCo# = *zero;
             intCo# = GetOnlyCo#;
          Endif;
          //***  End   Add  ***  02/15/18  ********************************

          Return intCo#;
      /End-Free
     PGetUserInfo      E
     P*=====================================================================
     P* GetUserEmail- Returns UserEmail for given User ID
     P*  Returns - User Email
     P*=====================================================================
     PGetUserEmail     B                   Export
     DGetUserEmail     PI                  Like(UPMAIL)
     D inUserID                            Like(UPUSR) const
     D
     D strUserID       s                   Like(UPUSR)   inz
     D strUserEmail    s                   Like(UPMAIL)  inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strUserEmail = UPMAIL;
          Endif;
          Return strUserEmail;
      /End-Free
     PGetUserEmail     E
     P*=====================================================================
     P* GetUserId - Returns UserId for given Staff Code
     P*  Returns - User Id
     P*=====================================================================
     PGetUserId        B                   Export
     DGetUserId        PI                  Like(UPUSR)
     D inStaffCode                         Like(UPSTAF) const
     D
     D strStaffCode    s                   Like(UPSTAF)  inz
     D strUserId       s                   Like(UPUSR)   inz
      /Free
          strStaffCode = inStaffCode;
          Chain strStaffCode pup02;
          If %found;
             strUserId = UPUSR;
          Endif;
          Return strUserId;
      /End-Free
     PGetUserId        E

     P*=====================================================================
     P* GetUsrStaf - returns user staff code
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrStaf       B                   Export
     DGetUsrStaf       PI             3
     D inUserID                            Like(UPUSR) const
     D
     D strUserID       s                   Like(UPUSR)   inz
     D strUserStaf     s                   Like(UPSTAF)  inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strUserStaf = UPSTAF;
          Endif;
          Return strUserStaf;
      /End-Free
     PGetUsrStaf       E
     P*=====================================================================
     P* GetSecLvl - returns user security level
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetSecLvl        B                   Export
     DGetSecLvl        PI             3  0
     D inUserID                            Like(UPUSR) const
     D
     D strUserID       s                   Like(UPUSR)   inz
     D strSecLvl       s                   Like(UPLVL)   inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strSecLvl = UPLVL;
          Endif;
          Return strSecLvl;
      /End-Free
     PGetSecLvl        E
     P*=====================================================================
     P* GetSecType - returns user security type
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetSecType       B                   Export
     DGetSecType       PI             1
     D inUserID                            Like(UPUSR) const
     D
     D strUserID       s                   Like(UPusr)   inz
     D strSecType      s                   Like(UPtype)   inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strSecType = UPtype;
          Endif;
          Return strSecType;
      /End-Free
     PGetSecType       E

     P*=====================================================================
     P* GetUsrRRN - returns RRN for WMUPP
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrRRN        B                   Export
     P*
     DGetUsrRRN        PI             9  0
     D user                                like(UPUSR) const

     D rrn             s                   like(uprrn)
     D strUserID       s                   Like(UPUSR)   inz
      /Free
          strUserId = user;
          Chain strUserID pup01;
          If %found;
             rrn = UPrrn;
          Endif;
          Return rrn;
      /End-Free
     PGetUsrRRN        E

     P*=====================================================================
     P* GetUsrTitl - returns user title
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrTitl       B                   Export
     DGetUsrTitl       PI            15
     D user                                like(UPUSR) const

     D userTitle       s                   like(UPTITL)

      /Free
          Chain user pup01;
          If %found;
             userTitle = UPtitl;
          Endif;
          Return userTitle;
      /End-Free

     PGetUsrTitl       E
     P*=====================================================================
     P* GetContUpd - returns contacts update
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetContUpd       B                   Export
     DGetContUpd       PI             1
     D inUserID                            Like(UPUSR) const

     D strUserID       s                   Like(UPusr)   inz
     D strContUpd      s                   Like(UPContUpd) inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strContUpd = UPcontUpd;
          Endif;
          Return strContUpd;
      /End-Free
     PGetContUpd       E
