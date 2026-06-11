      *********************************************************************
      * REVISIONS:
     *
      * 01/21/20 C:180 Project WC2507
      *                Add div column to Estimated Payroll Edit Pop Up.
      *                and allow call from Policy review.
      *
     * 10/21/19 C:180 Project: WC2702
      *                Added new procedure EstPyrlEdit for Estimated Payroll
      *
     * 07/28/17 C:267 Project: WC2259
      *                Added new procedure EmpPayEdit for Employee Payroll
     *
     * 04/08/13 C:136 Project: 46WGO (Retrofit for CompSuite)
      *                Add parm to selfRptEdit for Late Report flag
      *********************************************************************
     D*=====================================================================
     D* Final Audit Payroll  Popup processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* FinAudtEdit - Display and process popup window for payroll records.
    P*
    P*   EPrrn     - passed in for validation                  (required)
    P*   inRTCD    - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DFinAudtEdit      PR
     D EPrrn                          9  0 const
     D inRTCD                         3    const
     D ELrrn                          9  0 const options(*nopass)
    D*
    P*====================================================================
    P*====================================================================
    P* SelfRptEdit - Display and process popup window for SR payroll records.
    P*
    P*   inDate      passed in for validation                  (required)
    P*   inRTCD    - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DSelfRptEdit      PR
     D EPrrn                          9  0 const
     D inRTCD                         3    const
     D ELrrn                          9  0 const
     D inDate                         8  0 const
     D inschdTyp                      1    const options(*nopass)
      *** beg add ***** 04/08/13 ******************************************
     D inSrLateRpt                    1    const options(*nopass)
      *** end add ***** 04/08/13 ******************************************
    D*
    P*====================================================================
    P*====================================================================
    P* VendorPEdit - Display and process popup window for Payroll Vendors.
    P*
    P*   VHrrn     - passed in for validation                  (required)
    P*   inRTCD    - passed in for validation                  (required)
    P*   ELrrn     - passed in for validation                  (optional)
    P*
    P*====================================================================
    P*
     DVendorPEdit      PR
     D Vhrrn                          9  0 const
     D ELrrn                          9  0 const options(*nopass)
    D*
    P*====================================================================
    P*====================================================================
    P* PrvEmpXRef  - Display and process popup window for X Ref Employers.
     P*               Load Previous Cross Ref Policies for Action panel.
    P*
    P*   inCO#     - passed in for validation                  (required)
    P*   inFND     - passed in for validation                  (required)
    P*   inEMP     - passed in for validation                  (required)
    P*   inEMP     - passed in for validation                  (required)
    P*
    P*====================================================================
     d PrvEmpXRef      pr
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inEMP#                         9  0 const
     d inDIV                          5  0 const
      *** Beg add ***** 07/28/17 ******************************************
    P*====================================================================
    P* EmpPayEdit  - Add/Edit/Delete procedure for Employee Payroll grid
    P*
    P*   inCO#     - passed in for validation                  (required)
    P*   inFND     - passed in for validation                  (required)
    P*   inEMP     - passed in for validation                  (required)
    P*   inEMP     - passed in for validation                  (required)
    P*
    P*====================================================================
     d EmpPayEdit      pr
     D eeRRN                          9  0 const
     D elRRN                          9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)
      *** End add ***** 07/28/17 ******************************************
      *** Beg add ***** 10/21/19 ******************************************
     D EstPyrEdit      PR
     D EPrrn                          9  0 const
     D inRTCD                         3    const
     D ELrrn                          9  0 const options(*nopass)
     D inPayMode                     10    const options(*nopass)
      *** Beg add ***** 01/21/20 ******************************************
      *** End add ***** 01/21/20 ******************************************
      *** End add ***** 10/21/19 ******************************************
