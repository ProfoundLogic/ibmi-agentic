     D*=====================================================================
     D* Office processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetOffName - Get office name
    P*
    P*  GetOffName - returns office name
    P*   office    - accepts the office (required)
    P*   of_rrn    - returns the RRN of WMOFP
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     DGetOffName       PR            30
     D office                         5    const
     D of_rrn                         9  0       options(*nopass)
     D co#                            3  0 const options(*nopass)
    P*
    P*====================================================================
    P* GetOffRRN - Get office RRN
    P*
    P*  GetOffRRN - returns the RRN of WMOFP
    P*   office   - accepts the office (required)
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     DGetOffRRN        PR             9  0
     D office                         5    const
     D co#                            3  0 const options(*nopass)
    P*====================================================================
    P* ValidOffice - Validate Office #
    P*
    P*  ValidOffice - returns Office Valid or Not
    P*   office     - accepts the office (required)
    P*   co#        - accepts the company # to search over
    P*====================================================================
    P*
     DValidOffice      PR              n
     D office                         5    const
     D co#                            3  0 const options(*nopass)
