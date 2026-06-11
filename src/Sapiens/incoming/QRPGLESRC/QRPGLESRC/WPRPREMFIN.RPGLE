     D*=====================================================================
     D* Premium finance company processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetPrmFNm - Get premium finance company name
    P*
    P*  GetPrmFNm - returns premium finance company name
    P*   premFin  - accepts the premium finance company code  (required)
    P*   effDate  - accepts the effective date to search over (required)
    P*   pf_rrn   - pfturns the RRN of WMPFP
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     DGetPrmFNm        PR            40
     D premFin                        5    const
     D effDate                        7  0 const
     D pf_rrn                         9  0       options(*nopass)
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
    P*
    P*====================================================================
    P* GetPrmFRRN - Get premium finance company RRN
    P*
    P*  GetPrmFRRN - pfturns the RRN of WMPFP
    P*   premFin  - accepts the premium finance company code  (required)
    P*   effDate  - accepts the effective date to search over (required)
    P*   group#   - accepts the group to search over
    P*   co#      - accepts the company # to search over
    P*====================================================================
    P*
     DGetPrmFRRN       PR             9  0
     D premFin                        5    const
     D effDate                        7  0 const
     D group#                         3  0 const options(*nopass)
     D co#                            3  0 const options(*nopass)
