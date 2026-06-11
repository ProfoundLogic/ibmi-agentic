     D*=====================================================================
     D* S/R processing procedure prototypes
     D*=====================================================================

    P*====================================================================
    P* GetSRReportStatus
    P*   co#      - accepts the company #      (required)
    P*   group    - accepts the group          (required)
    P*   employer - accepts the employer       (required)
    P*   division - accepts the division       (required)
    P*   year     - accepts the year           (required)
     P*   reportDt - accetps the reporting date (required)
    P*====================================================================
    P*
     DGetSRReportStatus...
     D                 pr            25    varying
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inReportDt                     7  0 const
     D inPeriod                      25    const varying options(*nopass)

    P*====================================================================
    P* IsValidReportStatus
     P*   process  - accepts the process        (required)
    P*   co#      - accepts the company #      (required)
    P*   group    - accepts the group          (required)
    P*   employer - accepts the employer       (required)
    P*   division - accepts the division       (required)
    P*   year     - accepts the year           (required)
     P*   reportDt - accetps the reporting date (required)
    P*====================================================================
    P*
     DIsValidReportStatus...
     D                 pr             1n
     D inProcess                    256    const varying
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inReportDt                     7  0 const
