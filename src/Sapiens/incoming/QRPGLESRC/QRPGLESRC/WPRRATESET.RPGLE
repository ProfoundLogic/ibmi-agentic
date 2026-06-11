     D*=====================================================================
     D* Rate set code processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* RatingSt - Get the rating state
    P*
    P*  RatingSt - returns the rating state for a rate set code
    P*   co#     - accepts the company #     (required)
    P*   code    - accepts the rate set code (required)
    P*   RSrrn   - returns the RRN of WMRSP
    P*====================================================================
    P*
     DRatingSt         PR             2
     D co#                            3  0 const
     D code                           3    const
     D RSrrn                          9  0       options(*nopass)
    D*
    P*====================================================================
    P* RateCdDesc - Get the rate set code description
    P*
    P*  RateCdDesc - returns the rate set code description
    P*   co#       - accepts the company #     (required)
    P*   code      - accepts the rate set code (required)
    P*   RSrrn      - returns the RRN of WMRSP
    P*====================================================================
    P*
     DRateCdDesc       PR            30
     D co#                            3  0 const
     D code                           3    const
     D RSrrn                          9  0       options(*nopass)
