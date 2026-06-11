     H copyright('Progressive Data Solutions, Inc 2000, 2003')
     H nomain
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 05/21/03  C:132 Support: 37011                                                     *
     H*                 Recompiled to include WPRUSRPRMS change  (no chg to this module)   *
     H*                                                                                    *
     H**************************************************************************************
    F*
     Fwmrel     if   e           k disk    rename(pre01:pre00) infds(refds)
     Fwmrep     if   e             disk    rename(pre01:pre0p)
    F*
     D refds           ds
     D rerrn                 397    400b 0

     D/copy wprusrprms
     D/copy wprregion

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* GetRgnDesc - Get region description
    P*
    P*  GetRgnDesc - returns region description
    P*   region#   - accepts the region # (required)
    P*   re_rrn    - returns the RRN of WMREP
    P*   group#    - accepts the group to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetRgnDesc       b                   export
    P*
     DGetRgnDesc       PI            30
     D region#                        3  0 const
     D re_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(RECO#)
     D group           s                   like(REFND)
     D desc            s             30

     C     reky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    region#

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
     C                   clear                   REDESC
     C*
     C     reky          chain     pre00                              60
     C*
     C                   eval      desc = REDESC
     C*
     C                   if        %parms >= 2
     C                   eval      re_rrn = rerrn
     C                   endif
     C*
     C                   return    desc

     PGetRgnDesc       e





    P*====================================================================
    P* GetRgnRRN - Get region RRN
    P*
    P*  GetRgnRRN - returns the RRN of WMREP
    P*   region#  - accepts the region # (required)
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PGetRgnRRN        b                   export
    P*
     DGetRgnRRN        PI             9  0
     D region#                        3  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(RECO#)
     D group           s                   like(REFND)

     C     reky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    region#

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
     C     reky          chain     pre00                              60
     C*
     C                   return    rerrn
     C*
     PGetRgnRRN        e





