     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H nomain
    H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H* 01/07/21  C:33  Project: 129WWK                                   *
     H*                 Translate client codes.                           *
     H*                                                                   *
     H* 04/04/16 C:136 Phaseware: 40576                                   *
     H*                Add edit to Agt Master from being inactivated if   *
     H*                active Agent Assignment record exists              *
     H*                                                                   *
     H* 08/31/15  C:33  35971                                             *
     H*                 Evaluate the environment parm passed to GetEmpAgt *
     H*                                                                   *
     H* 02/11/15  C:062 Project: 114WGU                                   *
     H*                 Add parameters to the Agent Searches              *
      *
      * 10/14/14 C:33  Project: 114WPE
      *                Add GetRatePlanDesc for agent rate plans.
      *
     H*                                                                   *
     H* 08/20/13  C:295 Project: 147WBV                                   *
     H*                  To check the parameters are passed in or not     *
     H*                  on GetAgtInfo procedure.                         *
     H*                                                                   *
     H* 07/30/13  C:287 Project: 147WBV                                   *
     H*                  Included additional parameters to return info    *
     H*                  on GetAgtInfo procedure.                         *
     H*                                                                   *
     H* 05/03/13  C:062 Project: WA114 82250                              *
     H*                  Create stored functions to get agt#.             *
     H*                                                                   *
     H* 03/07/12  C:190 Project: WC2116                                   *
     H*                  Added GetAgtInfo1, reterive values on basis      *
     H*                  of RRN.                                          *
     H*                                                                   *
     H* 03/06/12  C:189 Project: WC2115                                   *
     H*                  Added GetAgtInfo (Name,Email,Phone#)             *
     H*                                                                   *
    H*  11/05/04  C:33  Project: WC1033                                  *
    H*                  Add GetAgent#                                    *
    H*                                                                   *
    H*  10/10/03  C:33  Project: WC1017                                  *
    H*                  Add agent validity (ValidAgt#)                   *
    H*********************************************************************
    F*
     Fwmagl     if   e           k disk    rename(pag01:pag00) infds(agfds)
     Fwmaal     if   e           k disk    rename(paa01:paa00)
     Fwmaal1    if   e           k disk                        infds(aafds)
     Fwmahl     if   e           k disk    rename(pah01:pah00) infds(ahfds)
     Fwmahlq    if   e           k disk    rename(pah01:pah0q) usropn
     F                                     infds(ahqfds)
     Fwmahl1    if   e           k disk    rename(pah01:pah01) infds(ah1fds)
     Fwmarl     if   e           k disk    rename(par01:par00) infds(arfds)
     Fwtabl     if   e           k disk                        infds(tbfds)
     Fwmagp     if   e             disk    rename(pag01:pag0p)
     Fwmaap     if   e             disk    rename(paa01:paa0p)
     Fwmahp     if   e             disk    rename(pah01:pah0p)
    F*
     D agfds           ds
     D agrrn                 397    400b 0
     D aafds           ds
     D aarrn                 397    400b 0
     D ahfds           ds
     D ahrrn                 397    400b 0
     D arfds           ds
     D arrrn                 397    400b 0
     D tbfds           ds
     D tb_rrn                397    400b 0
     D ah1fds          ds
     D ah1rrn                397    400b 0
     D ahqfds          ds
     D ahqrrn                397    400b 0

     D***  Begin Add  ***  01/07/21  ************************
     DGetClientID      pr                  ExtPgm('RTVCLTID')
     D cltId                          3
     D***  End   Add  ***  01/07/21  ************************

     D/copy sprdate

     D/copy wprusrprms
     D/copy wprgroup
     D/copy wpremplr
     D/copy wpragent

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* AgtSrchAA - Search agents
    P*
    P*  AgtSrchAA - Returns RRN of WMAAP
    P*   inkc     - returns *on if F3 was pressed from search  (required)
    P*   inkl     - returns *on if F12 was pressed from search (required)
    P*   error    - returns a '1' if group passed in is invalid
    P*   group    - accepts the group # to search over
    P*   co#      - accepts the company # to search over
    P*   view     - sets the view
    P*   quote    - indicates if called from quote or not
    P*   mode     - used to indicate if called from Agent History
    P*   name     - subsets to the name that matches
    P*====================================================================
    P*
     PAgtSrchAA        b                   export
    P*
     DAgtSrchAA        PI             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D group                               options(*nopass) const like(AAFND)
     D co#                                 options(*nopass) const like(AACO#)
     D view                           1  0 options(*nopass) const
     D quote                          1    options(*nopass) const
     D mode                           1    options(*nopass) const
     D name                          32    options(*nopass) const

     D err             s              1
     D group#          s                   like(AAFND)
     D company         s                   like(AACO#)
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D fnrrn           s              9  0
     D inView          s              1  0 Inz(0)
     D inQuote         s              1    Inz(' ')
     D inMode          s              1    Inz(' ')
     D inName          s             32    Inz(' ')

     C                   eval      err     = *blank
     C                   eval      group#  = UserGrp(q1user)
     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif
     C*
     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif
     C*
     C                   if        ValidGrp(group#:fnrrn:company) = *off
     C                   eval      err = '1'
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      error = err
     C                   endif
     C*
     C                   If        %parms >= 6
     C                   eval      inView = view
     C                   EndIf
     C*
     C                   If        %parms >= 7
     C                   eval      inQuote = quote
     C                   EndIf
     C*
     C                   If        %parms >= 8
     C                   eval      inMode = mode
     C                   EndIf
     C*
     C                   If        %parms >= 9
     C                   eval      inName = name
     C                   EndIf
     C*
     C                   if        err = *blank
     C*
     C                   call      'W0040R'
     C     inkc          parm                    ##kc
     C     inkl          parm                    ##kl
     C                   parm      '2'           ##call
     C                   parm      fnrrn         ##rrn
     C                   parm                    inView
     C                   parm                    inQuote
     C                   parm                    inMode
     C                   parm                    inName
     C*
     C                   endif
     C*
     C                   return    ##rrn
     C*
     PAgtSrchAA        e





    P*====================================================================
    P* AgtSrchAG - Search agents
    P*
    P*  AgtSrchAG - Returns RRN of WMAGP
    P*   inkc     - returns *on if F3 was pressed from search  (required)
    P*   inkl     - returns *on if F12 was pressed from search (required)
    P*   error    - returns a '1' if group passed in is invalid
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PAgtSrchAG        b                   export
    P*
     DAgtSrchAG        PI             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D co#                                 options(*nopass) const like(AGCO#)
     D view                           1  0 options(*nopass) const
     D quote                          1    options(*nopass) const
     D mode                           1    options(*nopass) const
     D name                          32    options(*nopass) const

     D err             s              1
     D company         s                   like(AGCO#)
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D inView          s              1  0 Inz(0)
     D inQuote         s              1    Inz(' ')
     D inMode          s              1    Inz(' ')
     D inName          s             32    Inz(' ')

     C                   eval      err     = *blank
     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      error = err
     C                   endif
     C*
     C                   If        %parms >= 5
     C                   eval      inView = view
     C                   EndIf
     C*
     C                   If        %parms >= 6
     C                   eval      inQuote = quote
     C                   EndIf
     C*
     C                   If        %parms >= 7
     C                   eval      inMode = mode
     C                   EndIf
     C*
     C                   If        %parms >= 8
     C                   eval      inName = name
     C                   EndIf
     C*
     C                   if        err = *blank
     C*
     C                   call      'W0040R'
     C     inkc          parm                    ##kc
     C     inkl          parm                    ##kl
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm                    inView
     C                   parm                    inQuote
     C                   parm                    inMode
     C                   parm                    inName
     C*
     C                   endif
     C*
     C                   return    ##rrn
     C*
     PAgtSrchAG        e





    P*====================================================================
    P* AgtHisSrch - Search employer agent history records
    P*
    P*  AgtHisSrch - returns RRN of WMAHP
    P*   inkc     - returns *on if F3 was pressed from search   (required)
    P*   inkl     - returns *on if F12 was pressed from search  (required)
    P*   error    - returns a '1' if group passed in is invalid (required)
    P*              returns a '2' if employer passed in is invalid
    P*              returns a '3' if employer year passed in is invalid
    P*   quote    - accepts the environment to search over      (required)
    P*   emp#     - accepts the employer # to search over       (required)
    P*   div      - accepts the divsion to search over          (required)
    P*   yr       - accepts the employer year to search over
    P*   group    - accepts the group # to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     PAgtHisSrch       b                    export
    P*
     DAgtHisSrch       PI             9  0
     D inkc                           1
     D inkl                           1
     D error                          1
     D quote                          1                     const
     D emp#                                                 const like(AHEMP#)
     D div                                                  const like(AHDIV)
     D yr                                  options(*nopass) const like(AHFYR)
     D group                               options(*nopass) const like(AHFND)
     D co#                                 options(*nopass) const like(AHCO#)

     D err             s              1
     D employer        s                   like(AHEMP#)
     D division        s                   like(AHDIV)
     D year            s                   like(AHFYR)
     D group#          s                   like(AHFND)
     D company         s                   like(AHCO#)
     D rrn             s              9  0
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D ##qte           s              1
     D ##mode          s              1
     D fnrrn           s              9  0
     D emrrn           s              9  0
     D elrrn           s              9  0

     C                   eval      err      = *blank
     C                   eval      ##rrn    = *zeros
     C                   eval      company  = UserCo#(q1user)
     C                   eval      group#   = UserGrp(q1user)
     C                   eval      employer = emp#
     C                   eval      division = div
     C                   eval      year     = *zeros
     C*
     C                   if        %parms >= 7
     C                   eval      year = yr
     C                   endif
     C*
     C                   if        %parms >= 8
     C                   eval      group# = group
     C                   endif
     C*
     C                   if        %parms >= 9
     C                   eval      company = co#
     C                   endif
     C*
     C                   if        ValidEmp(employer:division:emrrn:group#:
     C                              company) = *off
     C                   eval      err = '2'
     C                   endif
     C*
     C                   if        ValidEmpYr(year:employer:division:elrrn:
     C                              group#:company) = *off and year <> *zero
     C                   eval      err = '3'
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      error = err
     C                   endif
     C*
     C                   if        err = *blank
     C*
     C                   select
     C                   when      year = *zero
     C                   eval      ##mode = 'E'
     C                   eval      ##rrn  = emrrn
     C                   when      year <> *zero
     C                   eval      ##mode = 'Y'
     C                   eval      ##rrn  = elrrn
     C                   endsl
     C*
     C                   call      'W0553R'
     C     inkc          parm                    ##kc
     C     inkl          parm                    ##kl
     C                   parm      '2'           ##call
     C                   parm                    ##rrn
     C                   parm      quote         ##qte
     C                   parm                    ##mode
     C*
     C                   endif
     C*
     C                   return    ##rrn
     C*
     PAgtHisSrch       e





    P*====================================================================
    P* GetAgent# - Get agent #
    P*
    P*  GetAgent#  - returns agent assignment #
    P*   co#       - accepts the company #  (required)
    P*   fein      - accepts the agent fein (required)
    P*   mod       - accepts the agent mod  (required)
    P*   group#    - accepts the group #
    P*====================================================================
    P*
     PGetAgent#        b                   export
    P*
     DGetAgent#        PI             9  0
     D co#                            3  0 const
     D fein                           9  0 const
     D mod                            3  0 const
     D group#                         3  0 const options(*nopass)

     D group           s                   like(AAFND) inz

     C     aaky          klist
     C                   kfld                    co#
     C                   kfld                    fein
     C                   kfld                    mod
     C                   kfld                    group
     C*
     C     aaky1         klist
     C                   kfld                    co#
     C                   kfld                    fein
     C                   kfld                    mod

     C                   if        %parms >= 4
     C                   eval      group = group#
     C                   endif
     C*
     C                   clear                   AAAGT#
     C     aaky          setll     paa00
     C     aaky1         reade     paa00
     C*
     C                   return    AAAGT#

     PGetAgent#        e





    P*====================================================================
    P* GetAgtFein - Get agent fed id # (mod)
    P*
    P*  GetAgtFein - returns agent fed id #
    P*   co#       - accepts the company # (required)
    P*   group#    - accepts the group #   (required)
    P*   agent#    - accepts the agent #   (required)
    P*   mod       - returns the modifier of the agent's fein
    P*   aa_rrn    - returns the RRN of WMAAP
    P*====================================================================
    P*
     PGetAgtFein       b                   export
    P*
     DGetAgtFein       PI             9  0
     D co#                            3  0 const
     D group#                         3  0 const
     D agent#                         5  0 const
     D mod                            3  0       options(*nopass)
     D aa_rrn                         9  0       options(*nopass)

     D fein            s                   like(AGFEIN)

     C     aaky1         klist
     C                   kfld                    co#
     C                   kfld                    group#
     C                   kfld                    agent#

     C*
     C                   clear                   AAFEIN
     C                   clear                   AAMOD
     C     aaky1         chain     paa01                              60
     C                   eval      fein = AAFEIN
     C*
     C                   if        %parms >= 4
     C                   eval      mod = AAMOD
     C                   endif
     C*
     C                   if        %parms >= 5
     C                   eval      aa_rrn = aarrn
     C                   endif
     C*
     C                   return    fein

     PGetAgtFein       e





    P*====================================================================
    P* GetAgtName - Get agent name
    P*
    P*  GetAgtName - returns agent name
    P*   agent#    - passed in for validation  (required)
    P*   ag_rrn    - returns the RRN of WMAGP
    P*   aa_rrn    - returns the RRN of WMAAP
    P*   group#    - accepts the group # to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetAgtName       b                   export
    P*
     DGetAgtName       PI            40
     D agent#                         5  0 const
     D ag_rrn                         9  0              options(*nopass)
     D aa_rrn                         9  0              options(*nopass)
     D group#                         3  0 const        options(*nopass)
     D co#                            3  0 const        options(*nopass)

     D company         s                   like(AACO#)
     D group           s                   like(AAFND)
     D name            s                   like(AGNAME)

     C     aaky1         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    agent#
     c*
     C     agky          klist
     C                   kfld                    company
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD

     C                   clear                   agrrn
     C                   clear                   aarrn
     C*
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
     C                   clear                   AAFEIN
     C                   clear                   AAMOD
     C                   clear                   AGNAME
     C*
     C     aaky1         chain     paa01                              60
     C     agky          chain     pag00                              60
     C                   eval      name = AGNAME
     C*
     C                   if        %parms >= 2
     C                   eval      ag_rrn = agrrn
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      aa_rrn = aarrn
     C                   endif
     C*
     C                   return    name

     PGetAgtName       e





    P*====================================================================
    P* GetAgtRRN - Get agent RRN
    P*
    P*  GetAgtRRN - returns the RRN of WMAAP
    P*   group    - passed in for validation  (required)
    P*   agent#   - passed in for validation  (required)
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     PGetAgtRRN        b                   export
    P*
     DGetAgtRRN        PI             9  0
     D group                          3  0                  const
     D agent#                         5  0                  const
     D co#                            3  0 options(*nopass) const

     D company         s                   like(AACO#)

     C     aaky1         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    agent#

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     aaky1         chain     paa01                              60
     C*
     C                   return    aarrn
     C*
     PGetAgtRRN        e





    P*====================================================================
    P* GetEmpAgt - Get employers agent
    P*
    P*  GetEmpAgt  - returns the employers agent #
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the division to search over  (required)
    P*   yr        - accepts the year to search over
    P*   efdt      - accepts the eff date to search over
    P*   agtName   - returns the agent name
    P*   AH_rrn    - returns the RRN of WMAHP
    P*   AA_rrn    - returns the RRN of WMAAP
    P*   AG_rrn    - returns the RRN of WMAGP
    P*   env       - accepts the environment Quote/live
    P*====================================================================
    P*
     PGetEmpAgt        b                   export
    P*
     DGetEmpAgt        PI             5  0
     D co#                                 like(AHCO#)  const
     D group                               like(AHFND)  const
     D emp#                                like(AHEMP#) const
     D div                                 like(AHDIV)  const
     D yr                                  like(AHFYR)  const options(*nopass)
     D efdt                                like(AHEFDT) const options(*nopass)
     D agtName                       40                       options(*nopass)
     D AH_rrn                         9  0                    options(*nopass)
     D AA_rrn                         9  0                    options(*nopass)
     D AG_rrn                         9  0                    options(*nopass)
     D env                            1                       options(*nopass)

     D empAgt          s              5  0
     D year            s                   like(AHFYR)
     D date            s                   like(AHEFDT)
     D AA_rrn1         s                   like(AA_rrn)
     D AG_rrn1         s                   like(AG_rrn)
     D isQuote         s              1n   inz
     D strCommand      s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     C     ahky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    year
     C*
     C     ahky_1        klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    year
     C                   kfld                    date
     C*
     C     ahky_2        klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

     C                   clear                   ahrrn
     C                   clear                   ahqrrn
     C                   clear                   agrrn
     C                   clear                   aarrn
     C                   clear                   empAgt
     C                   clear                   year
     C*
      /free
       If %parms >= 11;
          isQuote = env = 'Q';
       Endif;

       If isQuote;
          strCommand = 'OVRDBF FILE(WMAHLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMAHLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMAHLQ;
       Endif;
      /End-free
     C                   eval      date = *hival
     C* If Quote environment
     C                   If        %parms >=11 and env = 'Q'
     C     ahky_2        setgt     pah0q
     C                   dou       *in60 or AHSTAT <> 'I'
     C     ahky_2        readpe    pah0q                                  60
     C                   enddo
     C                   Else
     C* If Live environment
     C     ahky_2        setgt     pah00
     C                   dou       *in60 or AHSTAT <> 'I'
     C     ahky_2        readpe    pah00                                  60
     C                   enddo
     C                   Endif
     C*
     C                   if        not *in60
     C                   eval      year = AHFYR
     C                   else
     C                   clear                   ahrrn
     C                   endif
     C*
     C                   if        %parms >= 5 and yr <> *zero
     C                   eval      year = yr
     C                   endif
     C*
     C                   if        %parms >= 6 and efdt <> *zero
     C                   eval      date = efdt
     C                   endif
     C*
     C* If Quote environment
     C                   If        %parms >=11 and env = 'Q'
     C     ahky_1        setgt     pah0q
     C                   dou       *in60 or AHSTAT <> 'I'
     C     ahky          readpe    pah0q                                  60
     C                   enddo
     C                   Else
     C* If Live environment
     C     ahky_1        setgt     pah00
     C                   dou       *in60 or AHSTAT <> 'I'
     C     ahky          readpe    pah00                                  60
     C                   enddo
     C                   Endif
     C*
     C                   if        not *in60
     C                   eval      empAgt = AHAGT#
     C                   else
     C                   clear                   ahrrn
     C                   clear                   ahqrrn
     C                   endif
     C*
     C                   if        %parms >= 7
     C                   eval      agtName = GetAgtName(empAgt:AG_rrn1:
     C                                                  AA_rrn1:group:co#)
     C                   endif
     C*
     C                   if        %parms >= 8
     C                   if        %parms >=11 and env = 'Q'
     C                   eval      AH_rrn = ahqrrn
     C                   else
     C                   eval      AH_rrn = ahrrn
     C                   endif
     C                   endif
     C*
     C                   if        %parms >= 9
     C                   eval      AA_rrn = AA_rrn1
     C                   endif
     C*
     C                   if        %parms >= 10
     C                   eval      AG_rrn = AG_rrn1
     C                   endif
     C*
     C                   If        %parms >= 11
     C                   Close     WMAHLQ
     C                   Endif
     C
     C                   return    empAgt
     C*
     PGetEmpAgt        e





    P*====================================================================
    P* GetEmpAgtSF - Get employers agent - Stored Function
    P*
    P*  GetEmpAgt  - returns the employers agent #
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the division to search over  (required)
    P*   env       - accepts the environment Quote/live   (required)
    P*
    P**========================================================================***
    P*   Create Function must occur on the clients machine as this is installed****
    P*
    P*   Create function pdsobj/getEmpAgt#
    P*          ( co#  decimal(3, 0),
    P*            fnd  decimal(3, 0),
    P*            emp# decimal(9, 0),
    P*            div  decimal(5, 0),
    P*            env  varchar(1) )
    P*          returns decimal(5,0)
    P*          language rpgle
     p*          deterministic
     p*          no sql
    P*          external name WPIAGENT(GetEmpAgtSF)
    P*          parameter style general
    P*          program type sub
    P*
    P*====================================================================
     PGetEmpAgtSF      b                   export
     DGetEmpAgtSF      PI             5p 0
     D co#                                 like(AHCO#)  const
     D group                               like(AHFND)  const
     D emp#                                like(AHEMP#) const
     D div                                 like(AHDIV)  const
     D env                            1                 const varying

     D empAgt          s                   like(AHagt#) Inz(0)
     D year            s                   like(AHfYr)
     D date            s                   like(AHefDt) Inz(*Hival)
     D strCommand      s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const
     C*
     C     keyWMAHL      klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

     C     key1WMAHL     klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    year
     C*
     C     key2WMAHL     klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    year
     C                   kfld                    date

      /free

       // Override to quote file if policy is from quote environment
       If env='Q';
          strCommand = 'ADDLIBLE LIB(' + %trim(RtvQuoteLib('*QTE')) +
           ') POSITION(*AFTER QTEMP)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMAHLQ;
       Endif;

       // Get the policy year
       If env='Q';
          SetGT keyWMAHL pah0q;
          DoU %Eof(WMAHLQ);
             ReadPE keyWMAHL pah0q;
             If Not %Eof(WMAHLQ) And AHstat<>'I';
                year = AHfYr;
                Leave;
             EndIf;
          EndDo;
       Else;
          SetGT keyWMAHL pah00;
          DoU %Eof(WMAHL);
             ReadPE keyWMAHL pah00;
             If Not %Eof(WMAHL) And AHstat<>'I';
                year = AHfYr;
                Leave;
             EndIf;
          EndDo;
       EndIf;

       // Get the agent #
       If env='Q';
          SetGT key2WMAHL pah0q;
          DoU %Eof(WMAHLQ);
             ReadPE key1WMAHL pah0q;
             If Not %Eof(WMAHLQ) And AHstat<>'I';
                empAgt = AHagt#;
                Leave;
             EndIf;
          EndDo;
       Else;
          SetGT key2WMAHL pah00;
          DoU %Eof(WMAHL);
             ReadPE key1WMAHL pah00;
             If Not %Eof(WMAHL) And AHstat<>'I';
                empAgt = AHagt#;
                Leave;
             EndIf;
          EndDo;
       EndIf;

       // Remove overrides
       If env='Q';
          Close WMAHLQ;
          strCommand = 'RMVLIBLE LIB(' + %trim(RtvQuoteLib('*QTE')) +  ')';
          Callp QCMDEXC(strCommand:%len(strCommand));
       Endif;

       // Return the agent #
       Return empAgt;

      /End-free
     C*
     PGetEmpAgtSF      e





    P*====================================================================
    P* GetEmpPlan - Get employers commission plan
    P*
    P*  GetEmpPlan - returns the employers commission plan
    P*   co#       - accepts the company # to search over (required)
    P*   group     - accepts the group to search over     (required)
    P*   emp#      - accepts the employer to search over  (required)
    P*   div       - accepts the division to search over  (required)
    P*   yr        - accepts the year to search over
    P*   efdt      - accepts the eff date to search over
    P*====================================================================
    P*
     PGetEmpPlan       b                   export
    P*
     DGetEmpPlan       PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D yr                             3  0 const
     D efdt                           7  0 const

     D plan            s              1

     D year            s                   like(yr)
     D effDate         s                   like(efdt)
     D agt#            s                   like(AHAGT#)
     D agtName         s             40
     D AH_rrn          s              9  0
     D sysDate         s               d

     C     arky          klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    AHRT#
     C*
     C     arky1         klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    AHRT#
     C                   kfld                    AHEFDT

     C                   clear                   plan
     C                   eval      sysDate = systemDate
     C                   eval      year    = *hival
     C     *cymd         move      sysDate       effDate
     C*
     C                   if        yr >= *zero
     C                   eval      year = yr
     C                   endif
     C*
     C                   if        efdt <> *zero
     C                   eval      effDate = efdt
     C                   endif
     C*
     C                   eval      agt# = GetEmpAgt(co#:group:emp#:div:year:
     C                                    effDate:agtName:AH_rrn)
     C*
     C     AH_rrn        chain     wmahp                              60
     C*
     C     arky1         setgt     wmarl
    C*
     C                   dou       *in60 or ARSTAT <> 'I'
     C     arky          readpe    wmarl                                  60
     C                   enddo
    C*
     C  n60              eval      plan = ARPLAN
     C*
     C                   return    plan
     C*
     PGetEmpPlan       e





    P*====================================================================
    P* GetMstrRRN - Get agent master RRN
    P*
    P*  GetMstrRRN - return agent master RRN (WMAGP)
    P*   fein      - passed in for validation  (required)
    P*   mod       - passed in for validation  (required)
    P*   co#       - passed in for validation
    P*====================================================================
    P*
     PGetMstrRRN       b                   export
    P*
     DGetMstrRRN       PI             9  0
     D fein                                like(AGFEIN)                  const
     D mod                                 like(AGMOD)                   const
     D co#                                 like(AGCO#)  options(*nopass) const

     D company         s                   like(AGCO#)

     C     agky          klist
     C                   kfld                    company
     C                   kfld                    fein
     C                   kfld                    mod

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C     agky          chain     pag00                              60
     C*
     C                   return    agrrn

     PGetMstrRRN       e





    P*====================================================================
    P* ValidAgt# - Validate Agent #
    P*
    P*  ValidAgt# - returns *on if the agent is valid
    P*   agent#   - passed in for validation  (required)
    P*   group    - passed in for validation
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     PValidAgt#        b                   export
    P*
     DValidAgt#        PI                  like(*in60)
     D agent#                         5  0                  const
     D group#                         3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const

     D company         s                   like(AACO#)
     D group           s                   like(AAFND)

     C     aaky1         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    agent#

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
     C     aaky1         chain     paa01                              60
     C*
     C                   select
     C                   when      %found
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl
     C*
     PValidAgt#        e





    P*====================================================================
    P* ValidFein - Validate Agent Fein/Mod
    P*
    P*  ValidFein - returns *on if the agent is valid
    P*   fein     - passed in for validation  (required)
    P*   mod      - passed in for validation
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     PValidFein        b                   export
    P*
     DValidFein        PI                  like(*in60)
     D fein                           9  0                  const
     D mod                            3  0 options(*nopass) const
     D co#                            3  0 options(*nopass) const

     D company         s                   like(AACO#)

     C     keyWMAGL      klist
     C                   kfld                    company
     C                   kfld                    fein
     C                   kfld                    mod

     C     key2WMAGL     klist
     C                   kfld                    company
     C                   kfld                    fein

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      company = co#
     C                   endif
     C*
     C                   if        %parms >= 2
     C     keyWMAGL      chain     pag00                              60
     C                   else
     C     key2WMAGL     chain     pag00                              60
     C                   endif
     C*
     C                   select
     C                   when      %found
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl
     C*
     PValidFein        e

    P*====================================================================
    P* GetAgtInfo - Get the agent information AgtPhn#, AgtEmail, AgtName
    P* Added agent address AgtAd1, AgtCty, AgtSt, AgtZip
    P*
    P*   co#      - passed co#                (required)
    P*   fnd      - passed fnd                (required)
    P*   emp#     - passed emp#               (required)
    P*   div      - passed div#               (required)
    P*  AgtName   - returns Agent Name
    P*  AgtEmail  - returns Agent Email
    P*  AgtPhn#   - returns Agent Phone no#
     P* AgtAddr   - returns Agent Address 1
     P* AgtCity   - returns Agent City
     P* AgtState  - returns Agent State
     P* AgtZip    - returns Agent Zip Code
    P*====================================================================
    P*
     PGetagtInfo       b                   export
     P*
     DGetAgtInfo       PI
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D xxAgent                       40
     D xxAgEm                        60
     D xxAgtPhn#                     15
     D AgtAddr                       40    options(*nopass)
     D AgtCity                       25    options(*nopass)
     D AgtState                       2    options(*nopass)
     D AgtZip                         9  0 options(*nopass)
      *
     C     keyWMAHL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    emp#
     C                   Kfld                    div
      *
     C     keyWMAAL1     Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    AHagt#
      *
     C     keyWMAGL      Klist
     C                   Kfld                    co#
     C                   Kfld                    AAfein
     C                   Kfld                    AAmod
      *
      /Free

       // Agent Information

       SetGt keyWMAHL pah00;
       ReadPe keyWMAHL pah00;
       Chain keyWMAAL1 paa01;
       Chain keyWMAGL pag00;
       If %Found(wmagl);

          If AGphn1<>*Zeros;
             xxAgtPhn#='(' + %EditC(AGare1:'X') + ') ' +
              %Subst(%EditC(AGphn1:'X'):1:3) + '-' +
              %Subst(%EditC(AGphn1:'X'):4:4);
          EndIf;
          xxAgEm=AGeMal;
          xxAgent=AGname;
       EndIf;

          If %parms > 7;
          If AGMAd1 <> *Blanks ; //If Mailing Address contains data
            AgtAddr = AGMAd1 ;   // return mailing address information
            AgtCity = AGMCty ;
            AgtState = AGMSt ;
            AgtZip = AGMZip ;
          Else ;                //If no mailing address then,
            AgtAddr = AGPAd1 ;  // return physical address information
            AgtCity = AGPCty ;
            AgtState = AGPSt ;
            AgtZip = AGPZip ;
          EndIf ;
          EndIf ;

      /End-Free

     PGetagtInfo       e

    P*====================================================================
    P* GetAgtInfo1 - Get the agent information based on RRN
    P*
    P*   RRN      - passed rrn                (required)
    P*  AgtCo#    - returns Company Number
    P*  AgtFnd    - returns Agent Group Number
    P*  AgtAgt#   - returns Agent# assigned by Fumd.
    P*  AgtAgrc   - returns Agent# of Record.
    P*  AgtPrrc   - returns Producer of Record.
    P*  AgtProd   - returns Producer of Agent.
    P*====================================================================
    P*
     PGetagtInfo1      b                   export
    P*
     DGetAgtInfo1      PI
     D AHrrn1                         9  0                  const
     D xxAgCo#                        3  0
     D xxAgFnd                        3  0
     D xxAgAgt#                       5  0
     D xxAgAgrc                       5  0
     D xxAgPrrc                       5  0
     D xxAgProd                       5  0
      *
      /Free
       //
          Chain AHrrn1 PAH0P;
          xxAgCo# = AHco#;
          xxAgFnd = AHfnd;
          xxAgAGT#= AHagt#;
          xxAgagrc= AHagrc;
          xxAgPrrc= AHprrc;
          xxAgProd= AHprod;
       //

      /End-Free
     PGetagtInfo1      e
    P*====================================================================
    P* GetHAgtRRN  - Get the agent RRN based on information
    P*   agent#   - accepts the agent# # to search over         (required)
    P*   co#      - accepts the company # to search over        (required)
    P*   group    - accepts the group # to search over          (required)
    P*   emp#     - accepts the employer # to search over       (required)
    P*   div      - accepts the divsion to search over          (required)
    P*   yr       - accepts the employer year to search over    (required)
    P*
    P*====================================================================
    P*
     PGetHAgtRRN       b                   export
    P*
     DGetHAgtRRN       PI             9  0
     D agent#                              like(AHAGT#) const
     D co#                                 like(AHCO#)  const
     D group                               like(AHFND)  const
     D emp#                                like(AHEMP#) const
     D div                                 like(AHDIV)  const
     D yr                                  like(AHFYR)  const
      *

     c     ahky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    agent#
     C                   kfld                    emp#
     C                   kfld                    div
     c*

     c                   clear                   ah1rrn
     C     ahky          chain     pah01                              60
     C*

     c                   return    AH1rrn


     PGetHAgtRRN       e


     pGetPlanTypeDesc  b                   export
     dGetPlanTypeDesc  pi            30
     d inPlan                         1

     D***  Begin Add  ***  01/07/21  *********************
     DcltId            s              3
     D***  End   Add  ***  01/07/21  *********************

      /Free
       //***  Begin Add  ***  01/07/21  *********************
       GetClientID(cltId);

       If cltId = '129';
          Select;
          When inPlan = 'D';
            inPlan = ' ';
          When inPlan = 'B';
            inPlan = 'W';
          Endsl;
       Endif;
       //***  End   Add  ***  01/07/21  *********************
       Select;
       When inPlan = 'A';
          Return 'Agency Bill';
       When inPlan = 'W';
          Return 'Written Premium';
       Other;
          Return 'Cash Basis';
       Endsl;
      /End-free
     pGetPlanTypeDesc  e
      *=====================================================
      * Return *on if active agent assignment record exists
      *=====================================================
     pGetActAgtAsg     b                   export
     dGetActAgtAsg     pi              n
     d inCo#                               like(AAco#)  const
     d inFein                              like(AAfein) const
     d inMod                               like(AAmod)  const
      /free
       Setll (inCo#:inFein:inMod) paa00;
       Reade (inCo#:inFein:inMod) paa00;
       Dow not(%eof);
          If AAstat = 'A';
             Return *on;
          Endif;
       Reade (inCo#:inFein:inMod) paa00;
       Enddo;
       Return *off;
      /end-free
     pGetActAgtAsg     e

