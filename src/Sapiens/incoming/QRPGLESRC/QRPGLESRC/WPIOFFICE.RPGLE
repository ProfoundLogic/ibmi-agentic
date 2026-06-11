     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 02/01/20  C:062 Project: SY409                                                     *
     H*                 Add Office Validation                                              *
     H*                                                                                    *
     H* 05/21/03  C:132 Support: 37011                                                     *
     H*                 Recompiled to include WPIUSRPRMS change  (no chg to this module)   *
     H*                                                                                    *
     H* 03/15/02  C:69  Support: 29710                                                     *
     H*                 Rewrote service program to only get record when the key fields     *
     H*                 have changed. (Changes were not commented.)                        *
     H*                                                                                    *
     H**************************************************************************************
     H copyright('Progressive Data Solutions, Inc 2002')
     H nomain
    F*
     Fwmofl     if   e           k disk    rename(pof01:pof00) infds(offds)
    F*
     D offds           ds
     D ofrrn                 397    400b 0

     D/copy wprusrprms
     D/copy wproffice
     D
     DgetRecord        PR
     D                                3  0 const
     D                                5    const

     D psds          esds                  extname(szq1p)
     D
     D savCompany      s                   like(OFCO#)
     D savOffice       s                   like(OFOFC#)



    P*====================================================================
    P* GetOffName - Get office name
    P*
    P*  GetOffName - returns office name
    P*   office    - accepts the office (required)
    P*   of_rrn    - returns the RRN of WMOFP
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetOffName       b                   export
    P*
     DGetOffName       PI            30
     D office                         5    const
     D of_rrn                         9  0       options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(OFCO#)
     D name            s                   like(OFNAME)

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:office)
     C                   if        *in60 = *off
     C                   eval      name = OFNAME
     C*
     C                   if        %parms >= 2
     C                   eval      of_rrn = ofrrn
     C                   endif
     C                   endif
     C*
     C                   return    name

     PGetOffName       e



    P*====================================================================
    P* GetOffRRN - Get office RRN
    P*
    P*  GetOffRRN - returns the RRN of WMOFP
    P*   office   - accepts the office (required)
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PGetOffRRN        b                   export
    P*
     DGetOffRRN        PI             9  0
     D office                         5    const
     D co#                            3  0 const options(*nopass)

     D company         s                   like(OFCO#)
     D rrn             s                   like(ofrrn)

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:office)
     C  n60              eval      rrn = ofrrn
     C*
     C                   return    ofrrn
     C*
     PGetOffRRN        e



    P*====================================================================
    P* getRecord - get record only if key values have not changed
    P*
    P*====================================================================
     PgetRecord        b
     DgetRecord        PI
     D company#                       3  0 const
     D office#                        5    const
     C
     C     theKey        klist
     C                   kfld                    company#
     C                   kfld                    office#
     C
     C                   if        company# <> savCompany
     C                             or office# <> savOffice
     C     thekey        chain     pof00                              60
     C                   eval      savCompany = company#
     C                   eval      savOffice = office#
     C                   endif
     PgetRecord        e
    P*====================================================================
    P* ValidOffice - Validate Office
    P*
    P*  ValidOffice - returns if valid office or not
    P*   office     - accepts the office (required)
    P*   co#        - accepts the company # to search over
    P*====================================================================
     PValidOffice      b                   export
    P*
     DValidOffice      PI              n
     D office                         5    const
     D co#                            3  0 const options(*nopass)

     D company         s                   like(OFCO#)
     D name            s                   like(OFNAME)

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 2
     C                   eval      company = co#
     C                   endif
     C*
     C                   callp     getRecord(company:office)
     C                   if        *in60 = *off
     C                   Return    *On
     C                   Else
     C                   Return    *Off
     C                   endif

     PValidOffice      e
