     D*=====================================================================
     D* Claim processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetAdjState - Get Adjudication State
    P*
    P*  GetAdjState - returns claim adjudication state
    P*   co#         - accepts the company #                       (required)
    P*   group       - accepts the group                           (required)
    P*   yr          - accepts the policy year                     (required)
    P*   claim#      - accepts the claim#                          (required)
    P*====================================================================
    P*
     DGetAdjState      PR             2
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
    P*====================================================================
    P* GetFmcmpRrn - Get FMCMP relative record number
    P*
    P*  GetFmcmpRrn - returns FMCMP rrn
    P*   co#         - accepts the company #                       (required)
    P*   group       - accepts the group                           (required)
    P*   yr          - accepts the policy year                     (required)
    P*   claim#      - accepts the claim#                          (required)
    P*====================================================================
    P*
     DGetFmcmpRrn      PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
    P*====================================================================
    P* GetFaeapRrn - Get FAEAP relative record number
    P*
    P*  GetFaeapRrn - returns FAEAP rrn
    P*   co#         - accepts the company #                       (required)
    P*   group       - accepts the group                           (required)
    P*   yr          - accepts the policy year                     (required)
    P*   claim#      - accepts the claim#                          (required)
    P*   eob#        - accepts the eob#                            (required)
    P*   recon       - accepts the recon #                         (required)
    P*====================================================================
    P*
     DGetFaeapRrn      PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D eob#                           9  0 const
     D recon                          3  0 const
    P*====================================================================
    P* GetFaeopRrn - Get FAEOP relative record number
    P*
    P*  GetFaeopRrn - returns FAEAP rrn
    P*   co#         - accepts the company #                       (required)
    P*   group       - accepts the group                           (required)
    P*   yr          - accepts the policy year                     (required)
    P*   claim#      - accepts the claim#                          (required)
    P*   eob#        - accepts the eob#                            (required)
    P*====================================================================
    P*
     DGetFaeopRrn      PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D eob#                           9  0 const
