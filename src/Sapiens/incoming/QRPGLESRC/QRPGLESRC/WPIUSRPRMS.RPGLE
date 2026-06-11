     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 03/14/22  C:79  Project: 150WFR                                                    *
     H*                 Use Group Short Name on Policy/Quote Clearance                     *
     H*                                                                                    *
     H* 02/15/18  C:33  Support: 51241                                                     *
     H*                 Return CO# if only one exists                                      *
     H*                                                                                    *
     H* 11/13/17  C:33  Support: 49909                                                     *
     H*                 Get x-ref record, if not found                                     *
     H*                                                                                    *
     H* 03/24/15  C:180 Support 33808                                                      *
     H*                 Validate cumulative reserve code                                   *
     H*                                                                                    *
     H* 12/01/14  C:62  Support OP114 87046                                                *
     H*                 Create new procedure to determine what kind of reserve limits.     *
     H*                                                                                    *
     H* 11/04/14  C:62  Support OP114 87236                                                *
     H*                 Correct single group restriction without group restriction file    *
     H*                                                                                    *
     H* 04/23/14  C:62  Support WC114 85413                                                *
     H*                 Build group select sql statementy and return group                 *
     H*                                                                                    *
     H* 11/01/12  C:62  Project: WC2115                                                    *
     H*                 Get Change Address Flag                                            *
     H*                                                                                    *
     H* 10/10/12  C:33  Project: WC2116                                                    *
     H*                 Employer restriction flag.                                         *
     H*                 User employer.                                                     *
     H*                 Division restriction flag.                                         *
     H*                 User division.                                                     *
     H*                                                                                    *
     H* 07/13/12  C:062 Project: WC2115                                                    *
     H*                 Add procedure to get reserve authority limit                      *
     H*                                                                                    *
     H* 06/05/12  C:191 Project: Transform                                                 *
     H*                 Add procedure to validate staff code                              *
     H*                                                                                    *
     H* 06/26/07  C:189 Support: 58273                                                     *
     H*                 If the restrictions have been set to allow for                     *
     H*                 multiple groups, the flag used also must be set.                   *
     H*                                                                                    *
     H* 04/29/03  C:132 Support: 37011                                                     *
     H*                 Added Proc to retrive the User Restrict Group Flag                 *
     H*                                                                                    *
     H* 03/15/02  C:69  Support: 29710                                                     *
     H*                 Rewrote service program to only get record when the key fields     *
     H*                 have changed. (Changes were not commented.)                        *
     H*                                                                                    *
     H**************************************************************************************
     Fwmupl     if   e           k disk    rename(pup01:pup00) infds(upfds)
     Fwmupl1    if   e           k disk
     FSMAZL     if   e           k disk
     Fsxupl     if   e           k disk    rename(pup01:xup00)
      ***  Begin Add  ***  03/14/22  *************************************
     Fwmpal1    if   e           k disk
      ***  End   Add  ***  03/14/22  *************************************
     D upfds           ds
     D up_rrn                397    400b 0
     D  @RsvTypDs      ds                  INZ
     D  uprsv1
     D  uprsv2
     D  uprsv3
     D  uprsv4
     D  @RsvTyp                       1    Overlay(@RsvTypDs) Dim(4)

     D/copy sprcompany
     D/copy wprusrprms
     D***  Begin Add  ***  03/14/22  **************************************
     D/copy sprJobInfo
     D***  End   Add  ***  03/14/22  **************************************
     D
     DgetRecord        PR
     D                               10    const
     DgetStfRcd        PR
     D                                3    const

     D*====================================================================
     D* Standalone Top
     D*====================================================================
     D savUser         s             10
     D savStaff        s              3
     D***  Begin Add  ***  03/14/22  **************************************
     D isPolicyEntry   s              1n
     D***  End   Add  ***  03/14/22  **************************************
     D*====================================================================
     D* Standalone Bottom
     D*====================================================================
     P*=====================================================================
     P* GetUserId  - returns the user id for the staff code
     P*  staffCode - passed in for gathering info (required)
     P*=====================================================================
     PGetUserId        B                   Export
     P*
     DGetUserId        PI            10
     D staffCode                           like(UPSTAF) const

     D user            s                   like(UPUSR)

     C                   callp     getStfRcd(staffCode)
     C  n99              eval      user = UPUSR

     C                   return    user

     PGetUserId        E
     P*=====================================================================
     P* GetUsrName - returns user name
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrName       B                   Export
     P*
     DGetUsrName       PI            50
     D user                                like(UPUSR) const

     D userName        s                   like(UPUSNM)

     C                   callp     getRecord(user)
     C  n99              eval      userName = UPUSNM

     C                   return    userName

     PGetUsrName       E
     P*=====================================================================
     P* GetUsrOutq - returns user output queue
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrOutq       B                   Export
     P*
     DGetUsrOutq       PI            10
     D user                                like(UPUSR) const

     D outq            s                   like(UPOUTQ)

     C                   callp     getRecord(user)
     C  n99              eval      outq  = UPOUTQ

     C                   return    outq

     PGetUsrOutq       E
     P*=====================================================================
     P* GetUsrRRN - returns RRN for WMUPP
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrRRN        B                   Export
     P*
     DGetUsrRRN        PI             9  0
     D user                                like(UPUSR) const

     D rrn             s                   like(up_rrn)

     C                   callp     getRecord(user)
     C  n99              eval      rrn  = up_rrn

     C                   return    rrn

     PGetUsrRRN        E



     P*=====================================================================
     P* GetUsrLvl - Returns user security level
     P*  user     - passed in for gathering info (required)
     P*  type     - returns the user security type
     P*=====================================================================
     PGetUsrLvl        B                   Export
     P*
     DGetUsrLvl        PI             3  0
     D user                                like(UPUSR) const
     D type                                like(UPTYPE) options(*nopass)

     D userLevel       s                   like(UPLVL)

     C                   callp     getRecord(user)
     C                   if        *in99 = *off
     C                   eval      userLevel = UPLVL
     C*
     C                   if        %parms >= 2
     C                   eval      type = UPTYPE
     C                   endif
     C                   endif

     C                   return    userLevel

     PGetUsrLvl        E



     P*=====================================================================
     P* GetUsrType - Returns user security type
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrType       B                   Export
     P*
     DGetUsrType       PI             1
     D user                                like(UPUSR) const

     D userType        s                   like(UPTYPE)

     C                   callp     getRecord(user)
     C  n99              eval      userType = UPTYPE

     C                   return    userType

     PGetUsrType       E
     P*=====================================================================
     P* GetUsrStaf - returns user staff code
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrStaf       B                   Export
     P*
     DGetUsrStaf       PI             3
     D user                                like(UPUSR) const

     D staffCode       s                   like(UPSTAF)

     C                   callp     getRecord(user)
     C  n99              eval      staffCode = UPSTAF

     C                   return    staffCode

     PGetUsrStaf       E
     P*=====================================================================
     P* GetUsrTitl - returns user title
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrTitl       B                   Export
     P*
     DGetUsrTitl       PI            15
     D user                                like(UPUSR) const

     D userTitle       s                   like(UPTITL)

     C                   callp     getRecord(user)
     C  n99              eval      userTitle = UPTITL

     C                   return    userTitle

     PGetUsrTitl       E



      //  Begin Add  ***  10/10/12  **************************************
     P*=====================================================================
     P* UsrDivRstFlg - returns user restrict division flag
     P*                returns user parm restrict flag ('Y' or 'N')
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PUsrDivRstFlg     B                   Export
     P*
     DUsrDivRstFlg     PI             1
     D user                                like(UPusr) const

     D divFlag         s                   like(UPfl5) inz('N')
     D company         s              3p 0

      /Free
       Callp GetRecord(user);
       If UPfl5 <> *blank;
          divFlag = UPfl5;
       Endif;

       Return divFlag;

      /End-free

     PUsrDivRstFlg     E
     P*=====================================================================
     P* UsrEmpRstFlg - returns user restrict employer flag
     P*                returns user parm restrict flag ('N','Y','S' or 'M')
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PUsrEmpRstFlg     B                   Export
     P*
     DUsrEmpRstFlg     PI             1
     D user                                like(UPusr) const

     D empFlag         s                   like(UPxEmp) inz('N')
     D moreRecords     s              7p 0 inz
     D company         s              3p 0

      /Free
       Callp GetRecord(user);

       company = UserCo#(user);

       Setll (company:user) SMAZL;
       Dou %eof(SMAZL) or moreRecords > 1;
          Reade (company:user) SMAZL;
          If not %eof(SMAZL) and (AZfEmp <> *zero or AZtEmp <> *zero);
             moreRecords = moreRecords + 1;
          Endif;
       Enddo;

       Select;
       When UPxEmp = 'Y' and moreRecords > 1;
          Return 'M';
       When UPxEmp = 'Y' and moreRecords = 1;
          Return 'S';
       When UPxEmp = 'Y';
          Return 'Y';
       Other;
          Return 'N';
       Endsl;

      /End-free

     PUsrEmpRstFlg     E
      //  End   Add  ***  10/10/12  **************************************



      ***** BEG ADD ***** 04/29/03 ***************************************
     P*=====================================================================
     P* UsrRstFlg - returns user restrict group flag
     P*  user     - passed in for gathering info (required)
     P*  grp_flag - returns user parm restrict flag ('Y' or Blank)
     P*=====================================================================
     PUsrRstFlg        B                   Export
     P*
     DUsrRstFlg        PI             1
     D user                                like(UPUSR) const

     D grp_flag        s                   like(UPXFND)
     D More_Rec        S              7p 0
     D Company         S              3p 0

     C                   callp     getRecord(user)
     C  n99              eval      grp_flag = UPXFND
     C                   eval      More_Rec = 0
     C                   eval      company = UserCo#(user)

     C     theKey1       klist
     C                   kfld                    company
     C                   kfld                    UPUSR

     C     theKey1       Setll     SMAZL
     C     theKey1       Reade     SMAZL
     C                   Dow       not %EOF(SMAZL)
     C                   Eval      More_Rec = More_Rec + 1
     C     theKey1       Reade     SMAZL
     C                   Enddo

     C                   select
     c     grp_flag      wheneq    'Y'
     c     More_Rec      Andgt     1
     C  n99              eval      grp_flag = 'M'
     c     grp_flag      wheneq    'Y'
     c     More_Rec      Andeq     1
     C  n99              eval      grp_flag = 'S'
     c     grp_flag      wheneq    'Y'
     c     More_Rec      Andeq     0
     C  n99              eval      grp_flag = 'Y'
     C                   endsl

     C                   return    grp_flag

     PUsrRstFlg        E


     P*=====================================================================
     P* UserCo#  - returns user parm company #
     P*  user    - passed in for gathering info (required)
     P*  co_name - returns user parm company name
     P*=====================================================================
     PUserCo#          B                   Export
     P*
     DUserCo#          PI             3  0
     D user                                like(UPUSR) const
     D co_name                       40                options(*nopass)

     D company#        s                   like(UPCO#)

     C                   callp     getRecord(user)
     C  n99              eval      company# = UPCO#
      /Free
       If company# = *zero;
          company# = GetOnlyCo#;
       Endif;
      /End-free

     C                   return    company#

     PUserCo#          E
     P*=====================================================================
     P* UserDiv   - returns user parm division
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PUserDiv          B                   Export
     P*
     DUserDiv          PI             9  0
     D user                                like(UPUSR) const

      /Free
       Callp GetRecord(user);
       Return UPdiv;
      /End-free

     PUserDiv          E




     P*=====================================================================
     P* UserEmp#  - returns user parm employer
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     PUserEmp#         B                   Export
     P*
     DUserEmp#         PI             9  0
     D user                                like(UPUSR) const

      /Free
       Callp GetRecord(user);
       Return UPemp#;
      /End-free

     PUserEmp#         E
      //  End   Add  ***  10/10/12  **************************************
     P*=====================================================================
     P* UserGrp   - returns user parm group
     P*  user     - passed in for gathering info (required)
     P*  grp_name - returns user parm company name
     P*=====================================================================
     PUserGrp          B                   Export
     P*
     DUserGrp          PI             3  0
     D user                                like(UPUSR) const
     D grp_name                      60                options(*nopass)

     D group#          s                   like(UPFND)

     C                   callp     getRecord(user)
     C  n99              eval      group# = UPFND

     C                   return    group#

     PUserGrp          E
    P*====================================================================
    P* getRecord - get group record only if key values have not changed
    P*
    P*====================================================================
     PgetRecord        b
     DgetRecord        PI
     D user                          10    const
     C
     C     theKey        klist
     C                   kfld                    user
     C
     C                   if        user  <> *Blanks
     C                   setoff                                       99
     C                   if        user <> savUser Or user <> UPuser
     C     thekey        chain     pup00                              99
     C                   eval      savUser = user
     C                   endif
     C                   else
     C                   seton                                        99
     C                   eval      savUser = user
     C                   endif
      /Free
       If *in99;
          Clear UPnewUsr;
          Chain (user) sxupl;
          If UPnewUsr <> *blank;
             Chain (UPnewUsr) wmupl;
             *in99 = not %found;
          Endif;
       Endif;
      /End-free
     PgetRecord        e
    P*====================================================================
    P* getStfRcd - get record only if key values have not changed
    P*
    P*====================================================================
     PgetStfRcd        b
     DgetStfRcd        PI
     D staff                          3    const
     C
     C     theKey        klist
     C                   kfld                    staff
     C
     C                   if        staff  <> *Blanks
     C                   setoff                                       99
     C                   if        staff  <> savStaff Or
     C                             staff  <> UPstaf
     C     thekey        chain     pup01                              99
     C                   eval      savStaff = staff
     C                   endif
     C                   else
     C                   eval      savStaff = staff
     C                   seton                                        99
     C                   endif
     PgetStfRcd        e
     P*=====================================================================
     P* GetUsrEmail - returns user email
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     PGetUsrEmail      B                   Export
     P*
     DGetUsrEmail      PI            60
     D user                                like(UPUSR) const

     D userEmail       s                   like(UPMAIL)

     C                   callp     getRecord(user)
     C  n99              eval      userEmail= UPMAIL

     C                   return    userEmail

     PGetUsrEmail      E
     P*=====================================================================
     P* ValidStaff - returns '1' for valid staff and '0 for invalid
     P*  staffCode - passed in
     P*=====================================================================
     PValidStaff       B                   Export
     P*
     DValidStaff       pi              n
     D staffCode                           like(UPSTAF) const
      /free
        Chain staffCode pup01;
        If %Found(wmupl1);
          Return *On;
        Else;
          Return *Off;
        Endif;
      /End-free
     PValidStaff       E
     P*=====================================================================
     P* GetRsvAutLim - returns reserve authority limit
     P*  user        - passed in for gathering info (required)
     P*=====================================================================
     PGetRsvAutLim     B                   Export
     DGetRsvAutLim     PI                  like(UPral)
     D user                                like(UPusr) const
     D rsvAutLim       s                   like(UPral) inz
      /Free

       getRecord(user);
       If *in99 = *Off;
          rsvAutLim=UPral;
       EndIf;

       Return rsvAutLim;

      /End-Free
     PGetRsvAutLim     E
     P*=====================================================================
     P* GetChgAddrFlag - returns change address flag
     P*  user          - passed in for gathering info (required)
     P*=====================================================================
     PGetChgAddrFlag   B                   Export
     P*
     DGetChgAddrFlag   PI             1
     D user                                like(UPUSR) const

     D chgAddressFlag  s                   like(UPFL2) Inz('N')

      /free
       getRecord(user);
       If *in99=*Off;
          chgAddressFlag=UPfl2;
       EndIf;

       Return chgAddressFlag;

      /end-free

     PGetChgAddrFlag   E
     p*** End   Add *** 11/01/12 ******************************************
     p*** Begin Add *** 04/23 14 *******************************************
     P*=====================================================================
     P* GetGrpAuthSql - Returns group Number
     P*  user         - passed in for gathering info (required)
     P*  grpSelect    - returns the "Select" SQL statement
     P*  grpFrom      - returns the "From" SQL statement
     P*  grpWhere     - returns the "Where" SQL statement
     P*  mode         - Add process or change/inquiry process (A/I)
     P*=====================================================================
     PGetGrpAuthSql    B                   Export
     P*
     DGetGrpAuthSql    PI             3  0
     D user                                like(UPUSR) const
     D grpSelect                    100
     D grpFrom                      200
     D grpWhere                     200
     D mode                           1     const

     D group#          s                   like(UPFND) inz(0)
     D hldGroup#       s                   like(UPFND) inz(0)
     D moreRecords     s              7p 0 inz
     D company         s              3p 0

      /Free
       Callp GetRecord(user);

       // Get the Company Number
       company = UserCo#(user);

       // Determine if Multi-group Restrictions Exist
       Setll (company:user) SMAZL;
       Dou %eof(SMAZL);
          Reade (company:user) SMAZL;
          If not %eof(SMAZL) and AZfnd<>HldGroup# and AZappl='W'
            and ((mode='A' And AZauth='A') Or mode='I') and AZstat<>'I';
             hldGroup# = AZfnd;
             moreRecords = moreRecords + 1;
             If moreRecords>1;
                Leave;
             EndIf;
          Endif;
       Enddo;

       //Select Statement
       grpSelect = 'Digits(FNfnd) || ''-'' || FNname';
       //*** Beg Add *** 03/14/22 **********************************************
       // Use Group Short Name for Policy Entry if not blank and set to do so
       isPolicyEntry = PgmInStack('WTPOLQTE');
       If isPolicyEntry;
          Chain (company) ppa01;
          If %Found(WMPAL1) and PAentGshNm = 'Y';
            grpSelect = 'Digits(FNfnd) || ''-'' || (case when FNSHNM <> ' +
              ' '' '' then FNSHNM else FNNAME end)';
          Endif;
       Endif;
       //*** End Add *** 03/14/22 **********************************************

       // Now determine what kind of authority is in play
       Select;
       When UPxFnd = 'Y' and moreRecords >= 1;

          // From Statement
          // Add Mode
          If Mode = 'A';
             grpFrom='WMFNP join SMAZP A on FNco#=AZco# and FNfnd=AZfnd' +
               ' and AZauth=''A'' and AZappl=''W'' and AZusr=''' +
               user + ''' and AZstat<>''I''';
          // Display or Change Mode
          Else;
             grpFrom='WMFNP join SMAZP A on FNco#=AZco# and FNfnd=AZfnd' +
               ' and AZappl=''W'' and AZusr=''' +
               user + ''' and AZstat<>''I''';
          EndIf;

          // Where Statement
          // Add Mode
          If Mode = 'A';
             grpWhere = 'FNstat<>''I'' And  Rrn(a) = (Select Max(Rrn(b)) ' +
              'From SMAZP b Where b.AZappl=''W'' And b.AZauth=''A'' And ' +
              'b.AZstat<>''I'' And a.AZfnd=b.AZfnd And ' +
              'b.AZusr=''' + user + ''')';
          // Change and Display Mode
          Else;
             grpWhere = 'FNstat<>''I'' And  Rrn(a) = (Select Max(Rrn(b)) ' +
              'From SMAZP b Where b.AZappl=''W'' And b.AZstat<>''I'' And ' +
              'a.AZfnd=b.AZfnd And b.AZusr=''' + user + ''')';
          EndIf;

          // Not Multi Group
          If moreRecords = 1;
             group# = hldGroup#;
          EndIf;

       Other;

          // From Statement
          grpFrom = 'WMFNP';

          // Where Statement
          grpWhere = 'FNstat<>''I''';

          // Group Restricted
          If UPxFnd = 'Y';
             group# = UPfnd;
             grpWhere = %trim(grpWhere) + 'And FNfnd=' + %EditC(UPfnd:'X');
          EndIf;

       Endsl;

       // Return Group If Single Group Restriction
       Return group#;

      /End-Free

     PGetGrpAuthSql    E
     P*=====================================================================
     P* GetRsvLimType - Returns Reserve Limit Type
     P*                 I=Individual C=Cumulative N=None
     P*  user         - passed in for gathering info (required)
     P*=====================================================================
     PGetRsvLimType    B                   Export
     P*
     DGetRsvLimType    PI             1
     D user                                like(UPUSR) const

     D group#          s                   like(UPFND) inz(0)
     D hldGroup#       s                   like(UPFND) inz(0)
     D moreRecords     s              7p 0 inz
     D company         s              3p 0

      /Free
       Callp GetRecord(user);

       Select;
       When UPmRal<>0 Or UPcRal<>0 Or UPeRal<>0 Or UPlRal<>0;
          Return 'I';
       When UPral<>0;
          Return 'C';
       Other;
          Return 'N';
       EndSl;

      /End-Free

     PGetRsvLimType    E
     P*=====================================================================
     P*  ValRsvType    - returns valid/invalid flag
     P*  user          - passed in for gathering info (required)
     p*  rsv type      - passed in for gathering info (required)
     P*=====================================================================
     PValRsvType       B                   Export
     DValRsvType       PI              N
     D user                                like(UPusr)  const
     D rsvtype                             like(UPrsv1) const
     D n               s              3  0 INZ
      /Free
       getRecord(user);
       If *in99 = *Off;
          n = %LookUp(rsvtype:@Rsvtyp);
          select;
             when n > *zero;
                return *on;
             When n = *zero;
                return *off;
             Other;
                return *off;
          endsl;
       else;
          return *off;
       EndIf;
      /End-Free
     PValRsvType       E
