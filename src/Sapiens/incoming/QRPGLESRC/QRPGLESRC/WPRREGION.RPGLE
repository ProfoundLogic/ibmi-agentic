     D*=====================================================================
     D* Region processing procedure prototypes
     D*=====================================================================
    D*
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
     DGetRgnDesc       PR            30
     D region#                        3  0 const
     D re_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
    P*
    P*====================================================================
    P* GetRgnRRN - Get region RRN
    P*
    P*  GetRgnRRN - returns the RRN of WMREP
    P*   region#  - accepts the region # (required)
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     DGetRgnRRN        PR             9  0
     D region#                        3  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
