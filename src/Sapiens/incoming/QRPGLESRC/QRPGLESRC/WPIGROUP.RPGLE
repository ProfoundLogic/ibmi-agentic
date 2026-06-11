     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
      **************************************************************************************
      * Revisions:                                                                         *
      *
      * 04/04/22 C:136 Project: CN028
      *                Add more info to SqlWithGroupPortalInfo
      *                - As portalGroupsClaims
      *          C:33  - As portalGroupsPolicy
      *                - As portalGroupsQuote
      *                - As portalGroupsSR
      *                - As portalGroupsFROI
      *
     * 08/02/21 C:318 Project: WC2820                                                     *
     *                Added New Procedure to get group scoring vendor                     *
     *                                                                                    *
      * 02/19/20 C:33  Project: SY406
      *                Add SqlWithGroupPortalInfo
     *                                                                                    *
     * 08/14/19 C:33  Project: SY406                                                      *
     *                Add UseGroupInPortal.                                               *
     *                                                                                    *
     * 08/10/17 C:062 Project: 150WAP                                                     *
     *                Add routine to get the state peo type.                              *
     *                                                                                    *
     * 05/22/17 C:136 Project: WC2497                                                     *
     *                Add logic to GroupAuth to look at User Parm Group (UPXFND, UPFND)   *
     *                in addition to records in the Group Restriction file (SMAZP)        *
     *                                                                                    *
      * 09/07/16 C:308 Project: C114WAA                                                    *
      *                - To add a new procedure GetGroupAssociation to get FNgrpAsc field. *
     *                                                                                    *
      * 10/09/13 C:295 Support 82970                                                       *
      *                - ExpiredGrp - Determine if the group is expired.                   *
     *                                                                                    *
     *
      * 05/31/13 C:33  Redmine #2644
      *                - GrpCarrier - Retrieve group carrier code
      *                - SameCarrier - Determine if the groups are the same
      *                                 carrier
      *                                                                                    *
      * 04/23/13  C:189 Project: 147WBQ                                                    *
      *                 Added function to get the Group Assosiation Flag                   *
      *                                                                                    *
      * 04/17/13  C:65  Project: Redmine 2730                                              *
      *                 Uncomment out RRN setting in GetGrpName procedure.                 *
      *                                                                                    *
      * 01/31/13  C:279 Project: 146WFJ                                                    *
      *                 Add function to get Third party Billing Flag.                      *
      *                                                                                    *
      * 06/22/12  C:191 Project: Transform                                                 *
      *                 Add function to get PEO flag                                       *
      *                                                                                    *
      * 11/05/03  C:136 Project: WC1156                                                    *
      *                 Add function to validate group policy year.                        *
      *                                                                                    *
      * 03/15/02  C:69  Support: 29710                                                     *
      *                 Rewrote service program to only get record when the key fields     *
      *                 have changed. (Changes were not commented.)                        *
      *                                                                                    *
     H**************************************************************************************
     H nomain
    F*
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00) infds(fnfds)
      *** beg add ***** 01/31/13 ***********************************************
     Fwmexl     if   e           k disk    rename(pex01:pex00)
      *** end add ***** 01/31/13 ***********************************************
      *** beg add ***** 11/05/03 ***********************************************
     Fwdf1l     if   e           k disk    rename(pf101:pf100) infds(f1fds)
      ***  Begin Add  ***  05/31/13  **************************************
     Fwdf2l     if   e           k disk    rename(pf201:pf200)
      ***  End   Add  ***  05/31/13  **************************************
      ***  Begin Add  ***  08/10/17  **************************************
     Fwdrrl     if   e           k disk    rename(prr01:prr00)
      ***  End   Add  ***  08/10/17  **************************************
      ***  Begin Add  ***  08/14/19  **************************************
     Fwmfip     if   e           k disk    rename(pfi01:pfi0p)
      ***  End   Add  ***  08/14/19  **************************************
      *** beg add ***** 08/02/21 ***********************************************
     Fwinotip1  if   e           k disk    rename(pn101:pn100)
      *** end add ***** 08/02/21 ***********************************************
     *
     D f1fds           ds
     D f1_rrn                397    400b 0
      *** end add ***** 11/05/03 ***********************************************
     *
     D fnfds           ds
     D fn_rrn                397    400b 0

      ***  Begin Add  ***  08/14/19  **************************************
     D/copy sPrString
      ***  End   Add  ***  08/14/19  **************************************
     D/copy wprusrprms
     D/copy wprgroup
     D*** Begin Add *** 08/10/17 *****************
     D/copy wprrateset
     D*** End   Add *** 08/10/17 *****************
     D*** Begin Add *** 08/02/21 *****************
     D/copy sprDate
     D*** End   Add *** 08/02/21 *****************
     D
     DgetRecord        PR
     D                                3  0 const
     D                                3  0 const

     D psds          esds                  extname(szq1p)
     D
     D savCompany      s                   like(FNCO#)
     D savGroup        s                   like(FNFND)
      *** Beg add ***** 10/09/13 ***********************************************
     D Expireddat      S                   like(FNEXDT)    Inz
      *** End add ***** 10/09/13 ***********************************************



    P*====================================================================
    P* CommGroup - Determine if group is commercial
    P*
    P*  CommGroup - returns *on if group is commercial, otherwise off
    P*   co#       - accepts the company #     (required)
    P*   group     - passed in for validation  (required)
    P*   fnrrn     - returns the RRN of WMFNP
    P*====================================================================
    P*
     PCommGroup        b                   export
    P*
     DCommGroup        PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)

     D commercial      s              1

     C                   callp     getRecord(co#:group)
     C                   if        *in60 = *off
     C                   eval      commercial = FNCGFL = 'Y'
     C*
     C                   if        %parms >= 3
     C                   eval      fnrrn = fn_rrn
     C                   endif
     C                   endif
     C*
     C                   return    commercial

     PCommGroup        e



    P*====================================================================
    P* GetAltConm - Get alternate company name
    P*
    P*  GetAltConm - returns group name
    P*   co#       - accepts the company #     (required)
    P*   group     - passed in for validation  (required)
    P*   fnrrn     - returns the RRN of WMFNP
    P*====================================================================
    P*
     PGetAltConm       b                   export
    P*
     DGetAltConm       PI            60
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)

     D name            s             60

     C                   callp     getRecord(co#:group)
     C                   if        *in60 = *off
     C                   eval      name = FNANAM
     C*
     C                   if        %parms >= 3
     C                   eval      fnrrn = fn_rrn
     C                   endif
     C                   endif
     C*
     C                   return    name

     PGetAltConm       e



    P*====================================================================
    P* GetGrpName - Get group name
    P*
    P*  GetGrpName - returns group name
    P*   group     - passed in for validation  (required)
    P*   fnrrn     - returns the RRN of WMFNP
    P*   co#       - accepts the company # to search over
    P*   flgPEO    - return PEO policy group.
    P*====================================================================
    P*
     PGetGrpName       b                   export
    P*
     DGetGrpName       PI            60
     D group                               like(FNFND)                  const
     D fnrrn                          9  0             options(*nopass)
     D co#                                 like(FNCO#) options(*nopass) const
      *** beg add ***** 03/07/11 ***********************************************
     D flgPEO                         1          options(*nopass)
      *** end add ***** 03/07/11 ***********************************************

     Dcompany          s                   like(FNCO#)
     Dname             s                   like(FNNAME)

     C                   eval      company = UserCo#(q1user)
     C*
      *** beg add ***** 03/07/11 ***********************************************
     C                   if        %parms >= 4
     C                   eval      flgPEO= FNpeof
     C                   endif
      *** end add ***** 03/07/11 ***********************************************
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:group)
     C                   if        *in60 = *off
     C                   eval      name = FNNAME
     C*
     C                   if        %parms >= 2
     c*** begin chg ***  04/17/13  ***********************************************
     C*                  eval      fnrrn = fn_rrn
     C                   eval      fnrrn = fn_rrn
     c*** end chg *****  04/17/13  ***********************************************
     C                   endif
     C                   endif
     C*
     C                   return    name

     PGetGrpName       e



    P*====================================================================
    P* GetGrpRRN - Get group RRN
    P*
    P*  GetGrpRRN - returns the RRN of WMFNP
    P*   co#      - passed in for validation (required)
    P*   group    - passed in for validation (required)
    P*====================================================================
    P*
     PGetGrpRRN        b                   export
    P*
     DGetGrpRRN        PI             9  0
     D co#                            3  0 const
     D group                          3  0 const

     D rrn             s                   like(FN_rrn)
     C                   callp     getRecord(co#:group)
     C  n60              eval      rrn = FN_rrn
     C*
     C                   return    rrn

     PGetGrpRRN        e



    P*====================================================================
    P* GroupAuth - Determine if user is authorized to the group
    P*
    P*  GroupAuth  - returns *on if the user is authorized, otherwise *off
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   userEntry - accepts the user to search over (defaults to Q1USER)
    P*   accessLvl - returns access level (ie. 'I'nquiry or 'A'll)
    P*====================================================================
    P*
     PGroupAuth        b                   export
    P*
     DGroupAuth        PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D userEntry                     10    const options(*nopass)
     D accessLvl                      1          options(*nopass)

     DSA008R           PR                  extpgm('SA008R')
     D ##co#                          3  0 const
     D ##user                        10    const
     D ##appl                         1    const
     D ##rtcd                         1
     D ##auth                         1          options(*nopass)
     D ##grp                          3  0 const options(*nopass)

     D user            s                   like(q1user)
     D records         s              1
     D authority       s              1
     D access          s              1
     D authorized      s              1
      *** beg add ***** 05/22/17 ***************
     D flgGrpRstr      s               n
     D intDfltGrp      s              3  0
      *** end add ***** 05/22/17 ***************

     C                   eval      user = q1user
     C*
     C                   if        %parms >= 3 and userEntry <> *blanks
     C                   eval      user = userEntry
     C                   endif
     C*
     C                   callp     SA008R(co#:user:'W':records)
     C                   callp     SA008R(co#:user:'W':authority:access:
     C                                    group)
     C*
     C                   eval      *in60      = records   = *on and
     C                                          authority = *on
     C                   eval      authorized = not *in60
     C*
      *** beg add ***** 05/22/17 *************************************
      /Free
       // User has total access to default group if
       //  group restriction records or group restriction
       //  for the default group doesn't exist.
       // User is not authorized to any other group
       //  if group restriction flag is set and no
       //  group restriction records exist
       flgGrpRstr = UsrRstFlg(user);
       intDfltGrp = UserGrp(user);
       Select;
       When records = *On and group = intDfltGrp and authorized = *Off
        Or records = *Blank and group = intDfltGrp;
          authorized = *On;
          access     = 'A';
       When records = *Blank and group <> intDfltGrp and flgGrpRstr  = 'Y';
          authorized = *Off;
       Endsl;
      /End-Free
      *** end add ***** 05/22/17 *************************************

     C                   if        %parms >= 4
     C                   eval      accessLvl = access
     C                   endif
     C*
     C                   return    authorized
     C*
     PGroupAuth        e



      ***  Begin Add  ***  05/31/13  **************************************
    P*====================================================================
    P* GrpCarrier - Retrieve the group carrier code
    P*
    P*  GrpCarrier - returns the group carrier code, if found
    P*   co#    - passed in for validation (required)
    P*   group  - passed in for validation (required)
    P*====================================================================
     PGrpCarrier       b                   export
     dGrpCarrier       pi             5
     d inCo#                          3  0 const
     d inGroup                        3  0 const

     C     keyWDF2L      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
      // WDF2P
      /Free
       Clear F2car#;
       Chain keyWDF2L wdf2l;
       Return %char(F2car#);
      /End-free
     PGrpCarrier       e
      ***  End   Add  ***  05/31/13  **************************************



    P*====================================================================
    P* GrpSrch - Search groups
    P*
    P*  GrpSrch - returns RRN of WMFNP
    P*   inkc   - returns *on if F3 was pressed from search  (required)
    P*   inkl   - returns *on if F12 was pressed from search (required)
    P*   group# - returns the group number
    P*   co#    - accepts company number to search over
    P*====================================================================
    P*
     PGrpSrch          b                   export
    P*
     DGrpSrch          PI             9  0
     D inkc                           1
     D inkl                           1
     D group                          3  0 options(*nopass)
     D co#                            3  0 options(*nopass)

     DW0010R           PR                  extpgm('W0010R')
     D ##kc                           1
     D ##kl                           1
     D ##call                         1    const
     D ##co#                          3    const
     D ##grp                          3    const

     D co#_a           s              3    inz
     D group_a         s              3    inz
     D company         s              3  0 inz
     D group#          s              3  0 inz

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 4 and co# <> *zero
     C                   eval      company = co#
     C                   endif
     C*
     C                   move      company       co#_a
     C*
     C                   callp     W0010R(inkc:inkl:'2':co#_a:group_a)
     C*
     C                   move      group_a       group#
     C*
     C                   if        %parms >= 3
     C                   eval      group = group#
     C                   endif
     C*
     C                   return    getGrpRRN(company:group#)
     C*
     PGrpSrch          e



      ***  Begin Add  ***  05/31/13  **************************************
    P*====================================================================
    P* SameCarrier - Determine if the groups have the same carrier code
    P*
    P*  SameCarrier - returns *on if both groups have the same carrier code
    P*   co#    - passed in for validation (required)
    P*   group1 - passed in for validation (required)
    P*   group2 - passed in for validation (required)
    P*====================================================================
     PSameCarrier      b                   export
     dSameCarrier      pi             1n
     d inCo#                          3  0 const
     d inGroup1                       3  0 const
     d inGroup2                       3  0 const
      /Free
       Select;
       When inGroup1 = inGroup2;
          Return *on;
       When GrpCarrier(inCo#:inGroup1) = GrpCarrier(inCo#:inGroup2);
          Return *on;
       Other;
          Return *off;
       Endsl;
      /End-free
     PSameCarrier      e
      ***  End   Add  ***  05/31/13  **************************************



    P*====================================================================
    P* TestGrp - Determine if the group is a TEST group
    P*
    P*  TestGrp - returns *on if group is a TEST group
    P*   co#    - passed in for validation (required)
    P*   group  - passed in for validation (required)
    P*   fnrrn  - returns the RRN of WMFNP
    P*====================================================================
    P*
     PTestGrp          b                   export
    P*
     DTestGrp          PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)

     C                   callp     getRecord(co#:group)
     C*
     C                   if        *in60 = *off
     C                   eval      *in99 = FNTEST = 'Y'
     C*
     C                   if        %parms >= 3
     C                   eval      fnrrn = fn_rrn
     C                   endif
     C                   endif
     C*
     C                   return    *in99

     PTestGrp          e



    P*====================================================================
    P* ValidGrp - Valid group
    P*
    P*  ValidGrp - return *on if group is valid
    P*   group   - passed in for validation  (required)
    P*   fnrrn   - returns the RRN of WMFNP
    P*   co#     - passed in for validation
    P*====================================================================
    P*
     PValidGrp         b                   export
    P*
     DValidGrp         PI             1
     D group                               like(FNFND)                  const
     D fnrrn                          9  0             options(*nopass)
     D co#                                 like(FNCO#) options(*nopass) const

     Dcompany          s                   like(FNCO#)

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:group)
     C*
     C                   if        *in60 = *off
     C                   if        %parms >= 2
     C                   eval      fnrrn = fn_rrn
     C                   endif
     C                   endif
     C*
     C                   return    not *in60

     PValidGrp         e
    
    
    
    P*====================================================================
    P* getRecord - get record only if key values have changed
    P*====================================================================
     PgetRecord        b
     DgetRecord        PI
     D company#                       3  0 const
     D group#                         3  0 const
     C
     C     theKey        klist
     C                   kfld                    company#
     C                   kfld                    group#
     C
     C                   if        company# <> savCompany
     C                             or group# <> savGroup
     C     thekey        chain     pfn00                              60
     C                   eval      savGroup = group#
     C                   eval      savCompany = company#
      *** Beg add ***** 10/09/13 ***********************************************
     C                   eval      Expireddat = Fnexdt
      *** End add ***** 10/09/13 ***********************************************
     C                   endif
     PgetRecord        e
      *** beg add ***** 11/05/03 ***********************************************
    P*====================================================================
    P* ValidGrpYear  - Valid group year
    P*
    P*  ValidGrpYear - return *on if group year is valid
    P*   co#         - passed in for validation  (required)
    P*   group       - passed in for validation  (required)
    P*   year        - passed in for validation  (required)
    P*   f1rrn       - returns the RRN of WDF1P
    P*====================================================================
     PValidGrpYear     b                   export
    P*
     DValidGrpYear     PI             1
     D group                               like(F1FND)                  const
     D year                                like(F1FYR)                  const
     D f1rrn                          9  0             options(*nopass)
     D co#                                 like(F1CO#) options(*nopass) const

     Dcompany          s                   like(F1CO#)

     C     f1ky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    year
     C
     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif
     C*
     C     f1ky          chain     pf100
     C*
     C                   if        %found
     C                   if        %parms >= 3
     C                   eval      f1rrn = f1_rrn
     C                   endif
     C                   endif
     C*
     C                   return    %found

     PValidGrpYear     e
      *** end add ***** 11/05/03 ***********************************************
    P*====================================================================
    P* GetPEOFlag - Get group PEO flag
    P*
    P*  GetPEOFlag  - returns PEO flag (1A)
    P*   group     - passed in for validation  (required)
    P*   co#       - accepts the company # to search over (optional)
    P*====================================================================
    P*
     PGetPEOFlag       b                   export
    P*
     DGetPEOFlag       pi             1
     D group                               like(FNFND)                  const
     D co#                                 like(FNCO#) options(*nopass) const

     Dcompany          s                   like(FNCO#)
     DflgPEO           s                   like(FNpeof)

     C                   eval      company = UserCo#(q1user)
     C*
     C*
     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:group)
     C                   if        *in60 = *off
     C                   eval      flgPEO= FNpeof
     C                   endif
     C*
     C                   return    flgPEO

     PGetPEOFlag       e
      *** Begin Add *** 08/10/17 ***********************************************
    P*====================================================================
    P* GetStatePeoFlag - Get the PEO State Flag
    P*
    P*  GetStatePeoFlag  - returns PEO State flag (1A)
    P*   group           - passed in for retrieval   (required)
     P*   rateSetCode     - passed in for retrieval   (required)
     P*   effectiveDate   - passed in for retrieval   (required)
    P*   co#             - accepts the company # to search over (optional)
    P*====================================================================
    P*
     PGetStatePeoFlag  b                   export
     DGetStatePeoFlag  pi             1
     D group#                              Like(FNfnd)                  Const
     D rateSetCode                    3                                 Const
     D effectiveDate                       like(FNEfDt)                 Const
     D co#                                 like(FNco#) Options(*nopass) Const

     D company         s                   like(FNCO#)
     d state           s              2    Inz(' ')
     d peoPolicyType   s              1    Inz(' ')

      /free
       company = UserCo#(q1user);
       If %parms >= 4;
          company = co#;
       EndIf;

       // Get the rating state
       state = RatingSt(company:rateSetCode);

       // Retrieve State PEO Policy Type
       SetGT  (company:group#:state:effectiveDate) prr00;
       ReadPE (company:group#:state)               prr00;
       If Not %Eof(wdrrl);
          peoPolicyType = RRpeoF;
       EndIf;

       Return peoPolicyType;
      /end-free


     PGetStatePeoFlag  e
      *** End   Add *** 08/10/17 ***********************************************
      *** beg add ***** 01/31/13 ***********************************************
    P*====================================================================
    P* Get3rdPtyFlag - Get Third Party Flag
    P*
    P*  Get3rdPtyFlag - returns Third Party Flag
    P*   co#       - accepts the company # (required
    P*   group     - accepts the group #   (required)
    P*====================================================================
     PGet3rdPtyFlag    b                   export
    
     DGet3rdPtyFlag    pi             1
     D co#                                 like(EXco#)                  const
     D group                               like(EXfnd)                  const

     Dcompany          s                   like(EXco#)
     Dflg3PtBl         s                   like(EX3rdb)

     C     keyWMEXL      klist
     C                   kfld                    company
     C                   kfld                    group

     C                   eval      company = UserCo#(q1user)

     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif

     C     keyWMEXL      chain     pex00
     C                   if        %found(WMEXL)
     C                   eval      flg3PtBl = EX3rdb
     C                   endif
     C*
     C                   return    flg3PtBl

     PGet3rdPtyFlag    e
      *** end add ***** 01/31/13 ***********************************************
      *** beg add ***** 04/23/13 ***********************************************
    P*====================================================================
    P* GetGrpAssflg - Get Group Assosiation value
    P*
    P*  GetGrpAssflg  - returns Group Assosiation Value
    P*   co#       - accepts the company # (required
    P*   group     - accepts the group #   (required
    P*====================================================================
     PGetGrpAssflg     b                   export
    
     DGetGrpAssflg     pi             1
     D co#                                 like(FNco#)                  const
     D group                               like(FNfnd)                  const

     Dcompany          s                   like(FNco#)
     DisOptaComp       s                   like(EX3rdb)

     C     keyWMFNL      klist
     C                   kfld                    company
     C                   kfld                    group

     C                   eval      company = UserCo#(q1user)

     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif

     C     keyWMFNL      chain     pfn00
     C                   if        %found(WMFNL)
     C                   if        FNgrpasc = 'OPTACOMP'
     C                   eval      isOptaComp = *on
     C                   endif
     C                   endif
     C*
     C                   return    isOptaComp

     PGetGrpAssFlg     e
      *** end add ***** 04/23/13 ***********************************************
      *** Beg add ***** 10/09/13 ***********************************************
      *** Beg add ***** 09/07/16 ***********************************************
    P*====================================================================
    P* GetGroupAssociation - to get group Association field FNgrpAsc
    P*
    P*  GetGroupAssoc - returns Group Assosiation field value
    P*   co#       - accepts the company # (required
    P*   group     - accepts the group #   (required
    P*   GrpAssociation - returns the Group Association
    P*====================================================================
     PGetGroupAssoc    b                   export
    
     DGetGroupAssoc    pi            10
     D co#                                 like(FNco#)                  const
     D group                               like(FNfnd)                  const

     Dcompany          s                   like(FNco#)
     DGrpAssociation   s                   like(FNgrpasc)

     C     keyWMFNL      klist
     C                   kfld                    company
     C                   kfld                    group

     C                   eval      company = UserCo#(q1user)

     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif

     C     keyWMFNL      chain     pfn00
     C                   if        %found(WMFNL)
     C                   eval      GrpAssociation = FNgrpasc
     C                   else
     C                   eval      GrpAssociation = *Blanks
     C                   endif
     C*
     C                   return    GrpAssociation

     PGetGroupAssoc    e
      *** End add ***** 09/07/16 ***********************************************
    P*====================================================================
    P* ExpiredGrp -- Expired Group
    P*
    P*  Expired Grp - Return *on if group is Expired
    P*  co#         - Passed in for validation  (required)
    P*  Group       - Passed in for validation  (required)
    P*====================================================================
    P*
     PExpiredGrp       b                   export
    P*
     DExpiredGrp       PI             1
     D co#                                 like(FNCO#)                  const
     D group                               like(FNFND)                  const

      /Free
           getRecord(co#:group);
           if  Expireddat > 0 and Expireddat < %DEC(%date() : *CYMD);

           return *On;
           Else;
           return *Off;
           endif;

      /End-Free
     PExpiredGrp       e
    
      *** End add ***** 10/09/13 ***********************************************
      *** beg add ***** 08/02/21 ***********************************************
    P*====================================================================
    P* GetGrScoringVnd   - Return the scoring vendor for the group
    P*   co#             - accepts the company # (required)
    P*   group           - accepts the group #   (required)
     P*   Effective Date  - accepts the Effective Date (Optional)
    P*====================================================================
     PGetGrScoringVnd  b                   export
    P*
     DGetGrScoringVnd  pi            25
     d co#                                 like(FNco#)                  const
     d group                               like(FNfnd)                  const
     d effDate                             like(FNEfDt)Options(*nopass) Const

     d scoreEfDt       s                   like(FNEfDt)

     c     keyWMEXL      klist
     c                   kfld                    co#
     c                   kfld                    group

      /free
       scoreEfDt= Dateto7(SystemDate());
       If %parms >= 3;
          scoreEfDt = effDate;
       Endif;

       Chain keyWMEXL pex00;
       If %Found(wmexl) and scoreEfDt >= EXscrVndEf and (scoreEfDt < EXscrVndEx
          or EXscrVndEx = *Zeros) and EXscrVnd <> *Blanks ;
          Select;
          When EXscrVnd = '*NONE';
             Return *Blank;
          Other;
             Return EXscrVnd;
          Endsl;
       Else;
          Chain co# pn100;
          If %Found(winotip1) and scoreEfDt >= N1scrVndEf;
             Return N1scrVnd;
          Else;
             Return *Blank;
          Endif;
       Endif;
      /end-free

     PGetGrScoringVnd  e
      *** end add ***** 08/02/21 ***********************************************
      *** beg add ***** 10/07/13 ***********************************************
    P*====================================================================
    P* GetPrmContflg - Get Premium/Contribution verbiage flag.
    P*
    P* GetPrmContflg - returns Premium/Contribution verbiage flag
    P*   co#       - accepts the company # (required
    P*   group     - accepts the group #   (required
    P*====================================================================
     PGetPrmContflg    b                   export
    
     DGetPrmContflg    pi             1
     d co#                                 like(FNco#)                  const
     d group                               like(FNfnd)                  const

     d company         s                   like(FNco#)
     d isContribution  s                   like(EX3rdb)

     c     keyWMFNL      klist
     c                   kfld                    company
     c                   kfld                    group

      /free
       company = UserCo#(q1user);
       If %parms >= 2;
          company = co#;
       Endif;

       Chain keyWMFNL pfn00;
       If %found(WMFNL);
          isContribution = FNprmv = 'C';
       Endif;
       return isContribution;
      /end-free

     PGetPrmContflg    e
      *** end add ***** 10/07/13 ***********************************************

       //***  Begin Add  ***  02/19/20  *************************
       //===================================================================
       // Create 'with' conditions to build group portal information
       //             - Returns a partial SQL string to be included in 'with'
       //==================================================================
     pSqlWithGroupPortalInfo...
     p                 b                   export
     dSqlWithGroupPortalInfo...
     d                 pi          5000    varying
     d
     dstrWith          s           5000    varying
      /Free
       strWith = ' ' +
        'portalInfo as ' +
        '(select FNCO# co#, FNFND fnd, ' +
        '  FIASOF grpAsOf, CIASOF coAsOf, ' +
        '  case ' +
        '   when (CIASOF is not null and CIASOF <> 0) ' +
        '    then CIASOF ' +
        '   when (CIASOF is null or CIASOF = 0) and ' +
        '    (FIASOF is not null and FIASOF <> 0) ' +
        '    then FIASOF ' +
        '   else DEC(current_date,8,0)-19000000 end as AsOf, ' +
        '  FIDAYS grpDays, CIDAYS coDays, ' +
        '  case ' +
        '   when (FIDAYS is null and CIDAYS is null) then 0 ' +
        '   when ((FIDAYS is null or (FIASOF=0 and FIDAYS=0)) ' +
        '    and CIDAYS is not null) then CIDAYS ' +
        '   else FIDAYS end as Days ' +
        ' from WMFNP ' +
        '  left join SMCIP on FNCO#=CICO# ' +
        '  left join WMFIP on FNCO#=FICO# and FNFND=FIFND), ' +
        'portalDates as ' +
        '(select p.co# co#, p.fnd fnd, p.grpAsOf, p.coAsOf, ' +
        '  DATE(SUBSTR(CHAR((p.AsOf)+19000000),1,4) || ''-'' || ' +
        '   SUBSTR(CHAR(p.AsOf),4,2) || ''-'' || SUBSTR(CHAR(p.AsOf),6,2)) ' +
        '   as AsOfDt, ' +
        '  p.grpDays, p.coDays, ' +
        '  (current_date - p.Days days) as calcDate ' +
        ' from portalInfo p), ' +
        'portalDate as ' +
        '(select p.co#, p.fnd, p.calcDate, p.AsOfDt, ' +
        '  (DEC(case ' +
        '        when (p.calcDate = current_date and p.AsOfDt = current_date) '+
        '         then cast(null as date) ' +
        '        when (p.calcDate <> current_date and p.calcDate > p.AsOfDt) ' +
        '         then p.calcDate ' +
        '        else p.AsOfDt end,8,0)-19000000) as compareDt ' +
        ' from portalDates p)';
       //*** beg add ***** 04/04/22 *************************
       strWith += ', ' +
        'portalGroupsClaims as ( ' +
        'Select FIco# co#, FIfnd fnd ' +
        'From WMFIP ' +
        'Where FIclaim = ''Y''' +
        ') '
        ;
       strWith += ', portalGroupsPolicy as ' +
        '(select FIco# co#, FIfnd fnd ' +
        '  from WMFIP ' +
        '  where FIpolicy = ''Y'')';
       strWith += ', portalGroupsQuote as ' +
        '(select FIco# co#, FIfnd fnd ' +
        '  from WMFIP ' +
        '  where FIquote = ''Y'')';
       strWith += ', portalGroupsSR as ' +
        '(select FIco# co#, FIfnd fnd ' +
        '  from WMFIP ' +
        '  where FIsr = ''Y'')';
       strWith += ', portalGroupsFROI as ' +
        '(select FIco# co#, FIfnd fnd ' +
        '  from WMFIP ' +
        '  where FIfroi = ''Y'')';
       //*** end add ***** 04/04/22 *************************
       Return strWith;
      /End-free
     pSqlWithGroupPortalInfo...
     p                 e
       //***  End   Add  ***  02/19/20  *************************
      ***  Begin Add  ***  08/14/19  **************************************
    P*====================================================================
    P* UseGroupInPortal - Returns *on if group is selected for component
    P*   co#       - accepts the company # (required)
    P*   group     - accepts the group #   (required)
    P*   component - accepts the component (required) {not case sensitive}
     P*                - Quote
     P*                - FROI
     P*                - S/R Online
     P*                - Policy
     P*                - Claim
    P*====================================================================
     PUseGroupInPortal...
     P                 b                   export
     DUseGroupInPortal...
     D                 pi             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inComponent                   25    const varying

     ddsWMFIP          ds                  likerec(pfi0p:*input) inz
      /Free
       Reset dsWMFIP;
       Chain (inCo#:inGroup) pfi0p dsWMFIP;
       Select;
       When Upper(inComponent) = 'QUOTE' and dsWMFIP.FIquote = 'Y';
          Return *on;
       When Upper(inComponent) = 'FROI' and dsWMFIP.FIfroi = 'Y';
          Return *on;
       When Upper(inComponent) = 'S/R ONLINE' and dsWMFIP.FIsr = 'Y';
          Return *on;
       When Upper(inComponent) = 'POLICY' and dsWMFIP.FIpolicy = 'Y';
          Return *on;
       When Upper(inComponent) = 'CLAIM' and dsWMFIP.FIclaim = 'Y';
          Return *on;
       Other;
          Return *off;
       Endsl;
       return *off;
      /End-free
     PUseGroupInPortal...
     P                 e
      ***  End   Add  ***  08/14/19  **************************************
