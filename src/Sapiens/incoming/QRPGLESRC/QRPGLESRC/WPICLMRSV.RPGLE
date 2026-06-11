     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H option(*nodebugio:*srcstmt)
      *===================================================================
      *
      *===================================================================
     Fwmrhl     if   e           k disk    rename(prh01:prh00) infds(rhfds)

     DdsFile           ds
     D rrn                   397    400b 0
     Drhfds            ds                  likeds(dsFile)

     D/copy sprdate
     D/copy wprclmrsv

     D psds          esds                  extname(szq1p)

     P*====================================================================
     P* GetInitRsvFlag - Determine the value of the initial reserve flag
     P*
     P*  GetInitRsvFlag - Returns 'Y' or blank
     P*   co#       - accepts the company #                (required)
     P*   group     - accepts the group                    (required)
     P*   fyr       - accepts the fyr                      (required)
     P*   claim     - accepts the claim                    (required)
     P*====================================================================

     PGetInitRsvFlag   b                   export

     DGetInitRsvFlag   pi                  like(RHinitRsv)
     D co#                                 like(RHco#)  const
     D group                               like(RHfnd)  const
     D fyr                                 like(RHfyr)  const
     D claim                               like(RHcase) const

     D outFlag         s                   like(RHinitRsv) inz
      /Free
       Setll (co#:group:fyr:claim) wmrhl;
       Reade(n) (co#:group:fyr:claim) wmrhl;
       If %eof;
          outFlag = 'Y';
       Endif;
       Return outFlag;
      /End-free
     PGetInitRsvFlag   e
