     D*=====================================================================
     D* Claim processing procedure prototypes
     D*=====================================================================
     D*
     P*====================================================================
     P* GetClmSts - Get claim status
     P*
     P*  GetClmSts - returns claim status for claim
     P*   co#       - accepts the company #                       (required)
     P*   group     - accepts the group                           (required)
     P*   yr        - accepts the policy year                     (required)
     P*   claim#    - accepts the claim#                          (required)
     P*   date      - accepts the date to determine the status
     P*====================================================================
     P*
     DGetClmSts        PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D date                           7  0
     D*
     P*====================================================================
     P* GetIncLoss - Get total inccurred loss for the employer
     P*
     P*  GetIncLoss - returns the total incurred loss for the employer
     P*   co#       - accepts the company #   (required)
     P*   group     - accepts the group       (required)
     P*   employer  - accepts the employer    (required)
     P*   division  - accepts the division    (required)
     P*   yr        - accepts the policy year
     P*   openClose - accepts 'O' for open claims, 'C' for closed claims
     P*   fromDate  - accepts the from date
     P*   toDate    - accepts the to date
     P*   payments  - returns the total claim payments for the employer
     P*   reserve   - returns the total claim reserves for the employer
     P*====================================================================
     P*
     DGetIncLoss       PR            11  2
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const options(*nopass)
     D openClose                      1    const options(*nopass)
     D fromDate                       7  0 const options(*nopass)
     D toDate                         7  0 const options(*nopass)
     D payments                      11  2       options(*nopass)
     D reserves                      11  2       options(*nopass)
     P*====================================================================
     P* GetClaimantn- accepts the SS# (key information) and returns the name in
     P*              Last, First MI format  (blank if not found).
     P* GetClaimantn - Returns the name in Last, First MI format
     P*  ss#         - accepts the ss#   (required)
     P*====================================================================
     P*
     DGetClaimantn     PR            40
     D ss#                            9    const
     P*====================================================================
     P* GetClaimRRN - Return Claim RRN
     P*   co#       - accepts the company #   (required)
     P*   group     - accepts the group       (required)
     P*   yr        - accepts the policy year
     P*   Case      - accepts the Case#
     P*====================================================================
     DGetClaimRRN      PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const

     P*====================================================================
     P* GetmultiClm - Check whether multi claim# exists
     P*   co#       - accepts the company #   (required)
     P*   group     - accepts the group       (required)
     P*   yr        - accepts the policy year (required)
     P*   Case      - accepts the Case#       (required)
     P*             - Returns whether mutli claim exists
     P*====================================================================
     P*
     DGetmultiClm      PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     P*====================================================================
     P* ChkAuthClm  - Determines if authorized to override add claim
     P*   company#  - accepts the company #   (required)
     P*   group#    - group number
     P*             - Returns *On if User has authority
     P*====================================================================
     D ChkAuthClm      PR              n
     D company#                       3  0
     D group#                         3  0
     P*====================================================================
     P* ChkDupClm   - Determines if Claims exist with that SS#
     P*   company#  - accepts the company #   (required)
     P*   socialSec#- Social Security #
     P*             - Returns *On if claim found with that SS#
     P*====================================================================
     D ChkDupClm       PR              n
     D company#                       3  0
     D socialSec#                     9
     P*====================================================================
     P* GetFroiState- Retrieves FROI State
     P*   company#  - accepts the company #   (required)
     P*   state     - jurisdiction state
     P*             - Returns Froi State
     P*====================================================================
     D GetFroiState    PR             3
     D company#                       3  0
     D state                          2
     P*====================================================================
     P* GetNextClm# - Retrieves the next available claim #
     P*   company#  - accepts the company #   (required)
     P*   group#    - group number
     P*             - Returns Claim #
     P*====================================================================
     D GetNextClm#     PR             7  0
     D company#                       3  0 const
     D group#                         3  0 const
     P*====================================================================
     P* ChkDupSS#   - Determines if duplicate SS# exist.
     P*   company#  - accepts the company #   (required)
     P*   socialSec#- Social Security #
     P*             - Returns *On if claim found with that SS#
     P*====================================================================
     D ChkDupSS#       PR              n
     D company#                       3  0
     D socialSec#                     9
     P*====================================================================
     P*====================================================================
     P* GetClaimName - Return Claim Name
     P*   co#       - accepts the company #
     P*   group     - accepts the group
     P*   yr        - accepts the policy year
     P*   Case      - accepts the Case#
     P*====================================================================
     DGetClaimName     PR            50
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D rrn                            9  0       options(*nopass)
     P*====================================================================
     P* ClaimCount  - Return Claim Count
     P*   co#       - accepts the company #
     P*   group     - accepts the group
     P*   employer  - accepts the employer #
     P*   division  - accepts the division (optional)
     P*====================================================================
     DClaimCount       PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const options(*nopass)
     D** division                       3  0 const options(*nopass)

     P*====================================================================
     P* ClaimDOB    - Get claimaint date of birth
     P* inss#       - accepts the SSN #
     P*====================================================================
     DClaimDOB         PR             7  0
     D inss#                          9

      //***  Begin Add  ***  07/27/20  ****************************
      *====================================================================
      * ClaimJSONData returns JSON string of general claim data
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inFyr  - accepts the current year      (required)
      *  inCase - accepts the current case      (required)
      *====================================================================
     dClaimJSONData    pr              a   len(5000) varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inFyr                          3  0 const
     d inCase                         7  0 const
      //***  End   Add  ***  07/27/20  ****************************
      //***  Begin Add ***** 11/03/21 ****************************
      *====================================================================
      * Create 'with' conditions to build Claim information including data from:
      *  SqlWithPolicyInfo(WDELP, WMEMP, WMAHP, WMAAP) and WMCMP
      *    - Returns a partial SQL string to be included in 'with'
      *====================================================================
     dSqlWithClaimInfo...
     d                 pr          5000    varying
      //***  End   Add ***** 11/03/21 ****************************
      *** beg add ***** 12/15/21 ***************************
     d inAsOfDate                      d   options(*nopass)
      *** end add ***** 12/15/21 ***************************
