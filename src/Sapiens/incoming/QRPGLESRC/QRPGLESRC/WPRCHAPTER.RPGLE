     D*=====================================================================
     D* Chapter processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetChapDes - Get chapter description
    P*
    P*  GetChapDes - returns chapter description
    P*   chapter   - accepts the region # (required)
    P*   cp_rrn    - returns the RRN of WMCPP
    P*   group#    - accepts the group to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     DGetChapDes       PR            30
     D chapter                        3  0 const
     D cp_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
    P*
    P*====================================================================
    P* GetChapRRN - Get chapter RRN
    P*
    P*  GetChapRRN - returns the RRN of WMCPP
    P*   chapter   - accepts the chapter (required)
    P*   group#    - accepts the group to search over
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     DGetChapRRN       PR             9  0
     D chapter                        3  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
