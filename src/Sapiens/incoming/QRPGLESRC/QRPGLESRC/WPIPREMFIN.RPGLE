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
     Fwmpfl     if   e           k disk    rename(ppf01:ppf00) infds(pffds)
     Fwmpfp     if   e             disk    rename(ppf01:ppf0p)
    F*
     D pffds           ds
     D pfrrn                 397    400b 0

     D/copy wprusrprms
     D/copy wprpremfin

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* GetPrmFNm - Get premium finance company name
    P*
    P*  GetPrmFNm - returns premium finance company name
    P*   premFin  - accepts the premium finance company code  (required)
    P*   effDate  - accepts the effective date to search over (required)
    P*   pf_rrn   - pfturns the RRN of WMPFP
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PGetPrmFNm        b                   export
    P*
     DGetPrmFNm        PI            40
     D premFin                        5    const
     D effDate                        7  0 const
     D pf_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(pfCO#)
     D group           s                   like(pfFND)
     D name            s             40

     C     pfky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    premFin
     C*
     C     pfky_1        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    premFin
     C                   kfld                    effDate

     C                   eval      company = UserCo#(q1user)
     C                   eval      group   = UserGrp(q1user)
     C*
     C                   if        %parms >= 4
     C                   eval      group = group#
     C                   endif
     C*
     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif
     C*
     C                   clear                   PFNAME
     C*
     C     pfky_1        setgt     ppf00
     C     pfky          readpe    ppf00                                  60
     C*
     C                   eval      name = PFNAME
     C*
     C                   if        %parms >= 2
     C                   eval      pf_rrn = pfrrn
     C                   endif
     C*
     C                   return    name

     PGetPrmFNm        e





    P*====================================================================
    P* GetPrmFRRN - Get premium finance company RRN
    P*
    P*  GetPrmFRRN - pfturns the RRN of WMPFP
    P*   premFin  - accepts the premium finance company code  (required)
    P*   effDate  - accepts the effective date to search over (required)
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PGetPrmFRRN       b                   export
    P*
     DGetPrmFRRN       PI             9  0
     D premFin                        5    const
     D effDate                        7  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)

     D company         s                   like(pfCO#)
     D group           s                   like(pfFND)

     C     pfky          klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    premFin
     C*
     C     pfky_1        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    premFin
     C                   kfld                    effDate

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
     C     pfky_1        setgt     ppf00
     C     pfky          readpe    ppf00                                  60
     C*
     C                   return    pfrrn
     C*
     PGetPrmFRRN       e





