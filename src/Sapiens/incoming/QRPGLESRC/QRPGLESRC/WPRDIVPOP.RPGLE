      *********************************************************************
      * REVISIONS:
     *
     *
      *********************************************************************
     D*=====================================================================
     D* Dividend Popup processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* DivdPlanEdit - Display and process popup window for dividend records.
    P*
    P*   inCO#     - passed in for validation                  (required)
    P*   inRRN     - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DDivdPlanEdit     PR
     D inCO#                          3  0 const
     D inRRN                          9  0 const
    D*
    P*====================================================================
    P* DivPremLevel - Display and process popup window for Dividend Premiun Level records.
    P*
    P*   inCO#     - passed in for validation                  (required)
    P*   inRRN     - passed in for validation                  (required)
    P*   inPlanID  - passed in for validation                  (required)
    P*   inqteMode   passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DDivPremLevel     PR
     D inCO#                          3  0 const
     D inRRN                          9  0 const
     D inPlanID                      10    const
     D quoteMode                      1    const
    D*
