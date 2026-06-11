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
     Fwmrsl     if   e           k disk    rename(prs01:prs00) infds(rsfds)

     D rsfds           ds
     D rs_rrn                397    400b 0

     D/copy wprrateset

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* RatingSt - Get the rating state
    P*
    P*  RatingSt - returns the rating state for a rate set code
    P*   co#     - accepts the company #     (required)
    P*   code    - accepts the rate set code (required)
    P*   RSrrn   - returns the RRN of WMRSP
    P*====================================================================
    P*
     PRatingSt         b                   export
    P*
     DRatingSt         PI             2
     D co#                            3  0 const
     D code                           3    const
     D RSrrn                          9  0       options(*nopass)

     D state           s              2

     C     rsky          klist
     C                   kfld                    co#
     C                   kfld                    code

     C                   clear                   RSJRST
     C     rsky          chain     prs00                              60
     C                   eval      state = RSJRST
     C*
     C                   if        %parms >= 3
     C                   eval      RSrrn = RS_rrn
     C                   endif
     C*
     C                   return    state
     C*
     PRatingSt         e





    P*====================================================================
    P* RateCdDesc - Get the rate set code description
    P*
    P*  RateCdDesc - returns the rate set code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the rate set code (required)
    P*   RSrrn      - returns the RRN of WMRSP
    P*====================================================================
    P*
     PRateCdDesc       b                   export
    P*
     DRateCdDesc       PI            30
     D co#                            3  0 const
     D code                           3    const
     D RSrrn                          9  0       options(*nopass)

     D desc            s             30

     C     rsky          klist
     C                   kfld                    co#
     C                   kfld                    code

     C                   clear                   RSDESC
     C     rsky          chain     prs00                              60
     C                   eval      desc = RSDESC
     C*
     C                   if        %parms >= 3
     C                   eval      RSrrn = RS_rrn
     C                   endif
     C*
     C                   return    desc
     C*
     PRateCdDesc       e





