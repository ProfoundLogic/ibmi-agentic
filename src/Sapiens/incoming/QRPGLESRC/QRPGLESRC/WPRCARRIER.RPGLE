     D*=====================================================================
     D* Carrier/State reporting processing procedure prototypes
     D*=====================================================================
    D*
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
     DGetCarrPol       PR            25
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const
     D rp_rrn                         9  0              options(*nopass)
    P*
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
     DGetCarrRRN       PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const
