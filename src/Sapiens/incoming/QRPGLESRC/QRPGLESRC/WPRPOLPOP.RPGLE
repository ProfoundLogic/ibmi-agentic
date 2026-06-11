      *********************************************************************
      * REVISIONS:
      //                                                               *
      // 11/08/17 C:33  Support: 50956                                 *
      //                Default carrier policy # on add mode           *
     *
     *
      *********************************************************************
     D*=====================================================================
     D* Policy Popup processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* PayrollEdit - Display and process popup window for payroll records.
    P*
    P*   EPrrn     - passed in for validation                  (required)
    P*   inRTCD    - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DPayrollEdit      PR
     D EPrrn                          9  0 const
     D inRTCD                         3    const
     D ELrrn                          9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)
    D*
    P*====================================================================
    P* ExcludeEdit - Display and process popup window for excluded states records.
    P*
    P*   XSrrn     - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DExcludeEdit      PR
     D XSrrn                          9  0 const
     D ELrrn                          9  0 const options(*nopass)
    D*
    P*====================================================================
    P* RatingEdit - Display and process popup window for rating info.
    P*
    P*   ELrrn     - passed in for validation                  (required)
    P*   chgMode   - passed in for mode                        (optional)
    P*
    P*====================================================================
    P*
     DRatingEdit       PR
     D ELrrn                          9  0 const
     D chgMode                         n   options(*nopass)
    D*
    P*====================================================================
    P* LargeDedEdit - Display and process popup window for rating info.
    P*
    P*   ELrrn     - passed in for validation                  (required)
    P*   chgMode   - passed in for mode                        (optional)
    P*
    P*====================================================================
    P*
     DLargeDedEdit     PR
     D ELrrn                          9  0 const
     D chgMode                         n   options(*nopass)
    D*
    P*====================================================================
    P* RetroEdit - Display and process popup window for rating info.
    P*
    P*   ELrrn     - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DRetroEdit        PR
     D ELrrn                          9  0 const
    P*====================================================================
    P* AddInfoEdit - Display and process popup window for Additional Info.
    P*
    P*   ELrrn     - passed in for validation                  (required)
    P*   quoteMode - passed in for validation                  (required)
    P*   chgMode   - passed in for mode                        (optional)
    P*
    P*====================================================================
    P*
     DAddInfoEdit      PR
     D ELrrn                          9  0 const
     D quoteMode                      1    const
     D chgMode                         n   options(*nopass) const
     D***  Begin Add  ***  11/08/17  **************************************
     D inDefaultMode                   n   options(*nopass) const
     D***  End   Add  ***  11/08/17  **************************************
    D*
    P*====================================================================
    P* AgentEdit - Display and process popup window for agent history records.
    P*
    P*   AHrrn     - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*   inMode    - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DAgentEdit        PR
     D AHrrn                          9  0 const
     D quoteMode                      1    const
     D ELrrn                          9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)
    D*
    P*====================================================================
    P* BillAuxiliary - Call the billing auxiliary window and return BSCD.
    P*
    P*   type      - passed in for validation                  (required)
    P*   begDate   - passed in for validation                  (required)
    P*   endDate   - passed in for validation                  (required)
    P*   fnRRN     - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DBillAuxiliary    PR             3  0
     D type                           1    const
     D begDate                        8  0 const
     D endDate                        8  0 const
     D fnRRN                          9  0 const
    D*
    P*====================================================================
    P* DemoPopup - Call the demograhpics popup window and return RRN.
    P*
    P*   co#       - passed in for validation                  (required)
    P*   fnd       - passed in for validation                  (required)
    P*   state     - passed in for validation                  (required)
    P*   city      - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DDemoPopup        PR             9  0
     D co#                            3  0 const
     D fnd                            3  0 const
     D state                          2    const
     D city                          25    const
    D*
    P*====================================================================
    P* SchdRatingEdit - Display and process popup window for scheduled rating info.
    P*
    P*   ELrrn     - passed in for validation                  (required)
    P*
    P*====================================================================
    P*
     DSchdRatingEdit   PR
     D ELrrn                          9  0 const
     D quoteMode                      1    const
    P*====================================================================
    P* DisplayPremium - display a popup window displaying the unbilled premium from
    P*                   a calculate print bill.
    P*
    P*   inRRN      - passed in for validation                  (required)
    P*   inFlag     - passed in for validation                  (required)
    P*                (flag denotes which file the RRN# applies to)
    P*====================================================================
    P*
     DDisplayPremium   PR
     D inRRN                          9  0 const
     D inFlag                         1    const
    P*====================================================================
      * Premium Info - returns premium info information
    P*   inRRN      - passed in for validation                  (required)
    P*   inFlag     - passed in for validation                  (required)
    P*                (flag denotes which file the RRN# applies to)
      *   outHeadings - Headings for premium line items          (required)
      *   outFactors  - Factors for premium line items           (required)
      *   outAmounts  - Amounts of premium line items            (required)
      *   outInfPrem  - Inforce amounts of premium line items    (required)

    P*====================================================================
     DPremiumInfo      PR
     d inRRN                          9  0 const
     d inFlag                         1    const
     d outHeadings                 1100
     d outFactors                  1100
     d outAmounts                   550
     d outInfPrem                   550
