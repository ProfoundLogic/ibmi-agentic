     H copyright('Progressive Data Solutions, Inc 2000')
     H nomain
    F*
     Fwmcpl     if   e           k disk    rename(pcp01:pcp00) infds(cpfds)
     Fwmcpp     if   e             disk    rename(pcp01:pcp0p)
    F*
     D cpfds           ds
     D cprrn                 397    400b 0

     D/copy wprusrprms
     D/copy wprchapter

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* GetChapDes - Get chapter description
    P*
    P*  GetChapDes - returns chapter description
    P*   chapter   - accepts the region # (required)
    P*   cp_rrn    - returns the RRN of WMCPP
    P*   group#    - accepts the group to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetChapDes       b                   export
    P*
     DGetChapDes       PI            30
     D chapter                        3  0 const
     D cp_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(CPCO#)
     D group           s                   like(CPFND)
     D desc            s             30

     C     cpky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    chapter

     C                   eval      company = UserCo#(q1user)
     C                   eval      group   = UserGrp(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      group = group#
     C                   endif
     C*
     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif
     C*
     C                   clear                   CPDESC
     C*
     C     cpky          chain     pcp00                              60
     C*
     C                   eval      desc = CPDESC
     C*
     C                   if        %parms >= 2
     C                   eval      cp_rrn = cprrn
     C                   endif
     C*
     C                   return    desc

     PGetChapDes       e





    P*====================================================================
    P* GetChapRRN - Get chapter RRN
    P*
    P*  GetChapRRN - returns the RRN of WMCPP
    P*   chapter   - accepts the chapter (required)
    P*   group#    - accepts the group to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetChapRRN       b                   export
    P*
     DGetChapRRN       PI             9  0
     D chapter                        3  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(CPCO#)
     D group           s                   like(CPFND)

     C     cpky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    chapter

     C                   eval      company = UserCo#(q1user)
     C                   eval      group   = UserGrp(q1user)
     C*
     C                   if        %parms >= 2
     C                   eval      group = group#
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     cpky          chain     pcp00                              60
     C*
     C                   return    cprrn
     C*
     PGetChapRRN       e





