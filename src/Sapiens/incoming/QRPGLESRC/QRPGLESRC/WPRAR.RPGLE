     D*=====================================================================
     D* A/R processing procedure prototypes
     D*=====================================================================
    D*
      //***  Begin Add  ***  01/22/19  *************************
    P*====================================================================
    P* Accumulate all A/R for specific purposes
    P*
    P*  AccumulateAR - Returns the to A/R posted for a particular code
     P*   code      - A/R code for posted A/R   (required)
     P*   from date - A/R posted on or after    (required)
     P*   to date   - A/R posted on or before   (required)
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (optional)
    P*   employer - accepts the employer   (optional)
    P*   division - accepts the division   (optional)
    P*====================================================================
     DAccumulateAR     pr            15  2
     D inCode                         2    const
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
      //***  End   Add  ***  01/22/19  *************************
      //*** Begin Add *** 05/25/21 *****************************************
     //===================================================================
     // Return the balance for a policy based upon the posting dates passed
     //
     // ARBalance       - Returns the A/R Balance
      //  from date      - A/R posted on or after    (required)
      //  to date        - A/R posted on or before   (required)
     //  co#            - accepts the company #     (required)
     //  group          - accepts the group         (required)
     //  year           - accepts the year          (optional)
     //  employer       - accepts the employer      (optional)
     //  division       - accepts the division      (optional)
     //  mode           - accepts the mode          (optional)
     //  invoice #      - accepts the invoice #     (optional)
     //  includeBatch   - not transferred(Y/N)      (optional)
     //  ExcludeDeposit - exclude deposits (Y/N)    (optional)
     //===================================================================
     DARBalance        pr            15  2
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     D inInvoice#                    10  0 const options(*nopass)
     D inIncludeBatch                 1    const options(*nopass)
     D inExcludeDeposit...
     D                                1    const options(*nopass)
      //*** End   Add *** 05/25/21 *****************************************
      //***  Begin Add  ***  07/08/20  *************************
    P*====================================================================
    P* Accumulate all Billed A/R
    P*
    P*  ARBilled - Returns the ammount of Billed A/R
     P*   from date - A/R posted on or after    (required)
     P*   to date   - A/R posted on or before   (required)
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (optional)
    P*   employer - accepts the employer   (optional)
    P*   division - accepts the division   (optional)
    P*   invoice# - accepts the invoice #  (optional)
    P*====================================================================
     DARBilled         pr            15  2
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D inMode                        25    const options(*nopass)
     D*** Begin Add *** 05/25/21 ****************************************  *
     D inInvoice#                    10  0 const options(*nopass)
     D inIncludeBatch                 1    const options(*nopass)
     D inExcludeDeposit...
     D                                1    const options(*nopass)
     D*** End   Add *** 05/25/21 ****************************************  *

    P*====================================================================
    P* Accumulate all Paid A/R
    P*
    P*  ARPaid - Returns the ammount of Paid A/R
     P*   from date - A/R posted on or after    (required)
     P*   to date   - A/R posted on or before   (required)
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (optional)
    P*   employer - accepts the employer   (optional)
    P*   division - accepts the division   (optional)
    P*   invoice# - accepts the invoice #  (optional)
    P*====================================================================
     DARPaid           pr            15  2
     D inDt7From                      7  0 const
     D inDt7To                        7  0 const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const options(*nopass)
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
      //***  End   Add  ***  07/08/20  *************************
     D inMode                        25    const options(*nopass)
     D*** Begin Add *** 05/25/21 ****************************************  *
     D inInvoice#                    10  0 const options(*nopass)
     D inIncludeBatch                 1    const options(*nopass)
     D inExcludeDeposit...
     D                                1    const options(*nopass)
     D*** End   Add *** 05/25/21 ****************************************  *
    P*
    P*====================================================================
    P* ARValue - Determine if A/R amount is positive or negative
    P*
    P*  ARValue   - Returns the value based on the type
    P*   ARAmount - passed in for validation  (required)
    P*   ARType   - passed in for validation  (required)
    P*====================================================================
    P*
     DARValue          PR             9  2
     D ARAmount                       9  2  const
     D ARType                         1     const
    P*
    P*====================================================================
    P* ARCatSrch - Searh A/R categories
    P*
    P*  ARCatSrch - Returns the RRN of WTCTP
    P*   inkc     - returns *on if F3 was pressed from search  (required)
    P*   inkl     - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     DARCatSrch        PR             9  0
     D inkc                           1
     D inkl                           1
    P*
    P*====================================================================
    P* ARCodeSrch - Searh A/R codes
    P*
    P*  ARCodeSrch - Returns the the A/R code
    P*   inkc      - returns *on if F3 was pressed from search  (required)
    P*   inkl      - returns *on if F12 was pressed from search (required)
    P*====================================================================
    P*
     DARCodeSrch       PR            10
     D inkc                           1
     D inkl                           1
    P*
    P*====================================================================
    P* BillType - Check for type as a bill type
    P*
    P*  BillType - Returns *on if A/R type is a bill type
    P*   type    - passed in for checking  (required)
    P*====================================================================
     DBillType         pr             1n
     D inType                         1    const
    P*
     P*===================================================
     P* Return *On if condition exists that warrants removing
     P* unpaid invoices at Invoice Cloud.
     P* outInvType describes the condition.
     D DeleteUnpaidInvoices...
     D                 pr             1n
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inInv#                        10  0 const
     D outInvType                    25

    P*====================================================================
    P* GetARCat - Get the A/R category for a specific A/R code
    P*
    P*  GetARCat - Returns the A/R category
    P*   code    - passed in for validation  (required)
    P*   TT_rrn  - returns the RRN of WTTTP
    P*   co#     - accepts the company #
    P*====================================================================
    P*
     DGetARCat         PR             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
    P*
      //***  Begin Add  ***  01/22/19  *************************
    P*====================================================================
    P* GetARCode - Get the appropriate A/R code
    P*
    P*  GetARCode - Returns the A/R code
     P*   type     - A/R code to be returned   (required)
     P*            -- paymentSR
     P*            -- billSR
    P*   co#      - accepts the company #  (required)
    P*   group    - accepts the group      (required)
    P*   year     - accepts the year       (required)
    P*====================================================================
     DGetARCode        pr             2
     D inType                        25    const
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inYear                         3  0 const
      //***  End   Add  ***  01/22/19  *************************
    P*
    P*====================================================================
    P* GetARComm - Get the A/R exclude commission flag for an A/R code
    P*
    P*  GetARComm - Returns the A/R exclude commission flag
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     DGetARComm        PR             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
    P*
    P*====================================================================
    P* GetARDate - Get A/R transaction or posing date
    P*
    P*  GetARDate - Returns the date used for any particular transaction
    P*   co#      - accepts the company #        (required)
    P*   group    - accepts the group #          (required)
    P*   year     - accepts the policy year      (required)
    P*   employer - accepts the employer         (required)
    P*   division - accepts the division         (required)
    P*   type     - accepts the transaction type (required)
    P*   postDate - accepts the posting date     (required)
    P*   tranDate - accepts the transaction date (required)
    P*====================================================================
    P*
     DGetARDate        PR             7  0
     D co#                            3  0 const
     D group                          3  0 const
     D year                           3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D type                           1    const
     D postDate                       7  0 const
     D tranDate                       7  0 const
    P*
    P*====================================================================
    P* GetARDesc - Get the A/R description for a specific A/R code
    P*
    P*  GetARDesc - Returns the A/R code descriptio0n
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     DGetARDesc        PR            30
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
    P*
    P*====================================================================
    P* GetARType - Get the A/R type for a specific A/R code
    P*
    P*  GetARType - Returns the A/R type
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     DGetARType        PR             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
     P*
    P*====================================================================
    P* GetCatDesc - Get the A/R category description
    P*
    P*  GetCatDesc - Returns the A/R category description
    P*   co#       - accepts the company #     (required)
    P*   category  - passed in for validation  (required)
    P*   CT_rrn    - returns the RRN of WTCTP
    P*====================================================================
    P*
     DGetCatDesc       PR            30
     D co#                            3  0 const
     D category                       1    const
     D CT_rrn                         9  0       options(*nopass)
    P*
    P*====================================================================
    P* PayComm - Detemine if the code will pay commission
    P*
    P*  PayComm - Returns *on if code will pay commission, otherwise *off
    P*   co#    - accepts the company #     (required)
    P*   code   - passed in for validation  (required)
    P*   TT_rrn - returns the RRN of WTTTP
    P*====================================================================
    P*
     DPayComm          PR             1
     D co#                            3  0 const
     D code                           2    const
     D TT_rrn                         9  0       options(*nopass)
    P*
    P*====================================================================
    P* PayType - Check for type as a payment type
    P*
    P*  PayType - Returns *on if A/R type is a payment type
    P*   type   - passed in for checking  (required)
    P*====================================================================
     DPayType          pr             1n
     D inType                         1    const
    P*
    P*====================================================================
    P* ValidCat - Valid category
    P*
    P*  ValidCat  - Returns *on if category valid
    P*   categroy - passed in for validation  (required)
    P*   CT_rrn   - returns the RRN of WTCTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     DValidCat         PR             1
     D category                       1                     const
     D CT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
    P*
    P*====================================================================
    P* ValidCode - Valid A/R code
    P*
    P*  ValidCode - Returns *on if A/R code valid
    P*   code     - passed in for validation  (required)
    P*   TT_rrn   - returns the RRN of WTTTP
    P*   co#      - accepts the company #
    P*====================================================================
    P*
     DValidCode        PR             1
     D code                           2                     const
     D TT_rrn                         9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
    P*====================================================================
    P* ARHistBal - A/R Historical Balance
    P*
    P*  ARHistBal  - Returns the RRN of WDELP
    P*   inkc      - returns *on if F3 was pressed from search  (required)
    P*   inkl      - returns *on if F12 was pressed from search (required)
    P*====================================================================
     DARHistBal        PR             9  0
     D inCall                         1
     D inKC                           1
     D inKL                           1
     D inRRN                          9  0
      //*** beg add ***** 09/28/20 *************************
      //====================================================
      // GetLast3rdPartySeq#
      // Parms: inType - Determines how to parse key
      //        inKey  - Stores info associated with type
      //                 appended by seq#
      //====================================================
       Dcl-Pr GetLast3rdPartySeq# Packed(10:0);
        inKey Char(20) Const;
       End-Pr;
      //====================================================
      // GetNext3rdPartySeq#
      // Parms: inType - Determines how to parse key
      //        inKey  - Stores info associated with type
      //                 appended by seq#
      //====================================================
       Dcl-Pr GetNext3rdPartySeq# Packed(10:0);
        inKey Char(20) Const;
       End-Pr;
      //=====================================================
      //*** Begin Add *** 05/25/21 *****************************************
    P*====================================================================
    P* Return the balance for a policy based upon the posting dates passed
    P*
    P*  TransARToLive   - Returns nothing
     P*   fileName       - Incoming file name        (required)
     P*   rrn            - WDEAP_HLD rrn             (required)
    P*====================================================================
     DTransferARToLive...
     D                 pr
     D inFile                        10    const
     D inRRN                          9  0 const
      //*** End   Add *** 05/25/21 *****************************************
      //*** Begin Add *** 06/12/24 *****************************************
    P*====================================================================
    P* Return the default A/R code based upon type and category.
    P*
    P*  GetDefaultARCode  - Returns Default A/R Code
     P*   type             - A/R Type      (required)
     P*   category         - A/R Category  (required)
     P*   company#         - company #     (optional)
    P*====================================================================
     DGetDefaultARCode...
     D                 pr             2
     D inType                         1    const
     D inCategory                     1    const
     D inCompany                      3  0 options(*nopass) const
      //*** End   Add *** 06/12/24 *****************************************
