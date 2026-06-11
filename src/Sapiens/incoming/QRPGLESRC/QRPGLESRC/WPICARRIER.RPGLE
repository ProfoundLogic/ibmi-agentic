     H copyright('Progressive Data Solutions, Inc 2000')
     H nomain
    F*
     Fwmrpl     if   e           k disk    rename(prp01:prp00) infds(rpfds)
     Fwmrpp     if   e             disk    rename(prp01:prp0p)
    F*
     D rpfds           ds
     D rprrn                 397    400b 0

     D/copy wprusrprms
     D/copy wprcarrier

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* GetCarrPol - Get carrier policy #
    P*
    P*  GetCarrPol - returns carrier policy #
    P*   co#       - accepts the company #   (required)
    P*   group     - accepts the group       (required)
    P*   employer  - accepts the employer    (required)
    P*   division  - accepts the division    (required)
    P*   yr        - accepts the policy year (required)
    P*   rp_rrn    - returns the RRN of WMRPP
    P*====================================================================
    P*
     PGetCarrPol       b                   export
    P*
     DGetCarrPol       PI            25
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const
     D rp_rrn                         9  0              options(*nopass)

     D policy#         s             25

     C     rpky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division
     C                   kfld                    yr

     C                   clear                   RPCAP#
     C*
     C     rpky          chain     prp00                              60
     C*
     C                   eval      policy# = RPCAP#
     C*
     C                   if        %parms >= 6
     C                   eval      rp_rrn = rprrn
     C                   endif
     C*
     C                   return    policy#

     PGetCarrPol       e





    P*====================================================================
    P* GetCarrRRN - Get carrier/state reporting RRN
    P*
    P*  GetCarrRRN - returns the RRN of WMRPP
    P*   co#       - accepts the company #   (required)
    P*   group     - accepts the group       (required)
    P*   employer  - accepts the employer    (required)
    P*   division  - accepts the division    (required)
    P*   yr        - accepts the policy year (required)
    P*====================================================================
    P*
     PGetCarrRRN       b                   export
    P*
     DGetCarrRRN       PI             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const

     C     rpky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division
     C                   kfld                    yr

     C*
     C     rpky          chain     prp00                              60
     C*
     C                   return    rprrn
     C*
     PGetCarrRRN       e





