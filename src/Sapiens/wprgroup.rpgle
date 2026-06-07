      *********************************************************************
      * REVISIONS:
      *
      * 08/02/21 C:318 Project: WC2820
      *                Add GetGrScoringVnd
      *                                                                                    *
      * 02/19/20 C:33  Project: SY406
      *                Add SqlWithGroupPortalInfo
      *                                                                                    *
      * 08/14/19 C:33  Project: SY406                                                      *
      *                Add UseGroupInPortal.                                               *
      *
      * 08/10/17 C:062 Project: 150WAP
      *                GetStatePeoFlag - Get the state epo flag
      *
      * 09/07/16 C:308 PROJECT: C114WAA
      *                - GetGroupAssoc - Get the Group association field
      *
      * 10/09/13 C:295 Support 82970
      *                - ExpiredGrp - Determines if the group is Expired Group
      *
      * 05/31/13 C:33  Redmine #2644
      *                - GrpCarrier - Retrieve group carrier code
      *                - SameCarrier - Determine if the groups are the same
      *                                 carrier
      *********************************************************************
     D*=====================================================================
     D* Group processing procedure prototypes
     D*=====================================================================
     D*
     P*====================================================================
     P* CommGroup - Determine if group is commercial
     P*
     P*  CommGroup - returns *on if group is commercial, otherwise off
     P*   co#       - accepts the company #     (required)
     P*   group     - passed in for validation  (required)
     P*   fnrrn     - returns the RRN of WMFNP
     P*====================================================================
     DCommGroup        PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)
     D*
     P*====================================================================
     P* GetAltConm - Get alternate company name
     P*
     P*  GetAltName - returns group name
     P*   co#       - accepts the company #     (required)
     P*   group     - passed in for validation  (required)
     P*   fnrrn     - returns the RRN of WMFNP
     P*====================================================================
     DGetAltConm       PR            60
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)
     D*
     P*====================================================================
     P* GetGrpName - Get group name
     P*
     P*  GetGrpName - returns group name
     P*   group     - passed in for validation  (required)
     P*   fnrrn     - returns the RRN of WMFNP
     P*   co#       - accepts the company # to search over
     P*   flgPEO    - return PEO policy group.
     P*====================================================================
     DGetGrpName       PR            60
     D group                          3  0                  const
     D fnrrn                          9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
      *** beg add ***** 11/05/03 ***********************************************
     D flgPEO                         1          options(*nopass)
      *** end add ***** 11/05/03 ***********************************************
     D*
     P*====================================================================
     P* GetGrpRRN - Get group RRN
     P*
     P*  GetGrpRRN - returns the RRN of WMFNP
     P*   co#      - passed in for validation (required)
     P*   group    - passed in for validation (required)
     P*====================================================================
     P*
     DGetGrpRRN        PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D*
     P*====================================================================
     P* GroupAuth - Determine if user is authorized to the group
     P*
     P*  GroupAuth  - returns *on if the user is authorized, otherwise *off
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   userEntry - accepts the user to search over (defaults to Q1USER)
     P*   accessLvl - returns access level (ie. 'I'nquiry or 'A'll)
     P*====================================================================
     P*
     DGroupAuth        PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D userEntry                     10    const options(*nopass)
     D accessLvl                      1          options(*nopass)
     D*
      ***  Begin Add  ***  05/31/13  **************************************
     P*====================================================================
     P* GrpCarrier - Retrieve the group carrier code
     P*
     P*  GrpCarrier - returns the group carrier code, if found
     P*   co#    - passed in for validation (required)
     P*   group  - passed in for validation (required)
     P*====================================================================
     dGrpCarrier       pr             5
     d inCo#                          3  0 const
     d inGroup                        3  0 const
      ***  End   Add  ***  05/31/13  **************************************

     P*====================================================================
     P* GrpSrch - Search groups
     P*
     P*  GrpSrch - returns RRN of WMFNP
     P*   inkc   - returns *on if F3 was pressed from search  (required)
     P*   inkl   - returns *on if F12 was pressed from search (required)
     P*   group# - returns the group number
     P*   co#    - accepts company number to search over
     P*====================================================================
     P*
     DGrpSrch          PR             9  0
     D inkc                           1
     D inkl                           1
     D group                          3  0 options(*nopass)
     D co#                            3  0 options(*nopass)

      ***  Begin Add  ***  05/31/13  **************************************
     P*====================================================================
     P* SameCarrier - Determine if the groups have the same carrier code
     P*
     P*  SameCarrier - returns *on if both groups have the same carrier code
     P*   co#    - passed in for validation (required)
     P*   group1 - passed in for validation (required)
     P*   group2 - passed in for validation (required)
     P*====================================================================
     dSameCarrier      pr             1n
     d inCo#                          3  0 const
     d inGroup1                       3  0 const
     d inGroup2                       3  0 const
      ***  End   Add  ***  05/31/13  **************************************

     P*====================================================================
     P* TestGrp - Determine if the group is a TEST group
     P*
     P*  TestGrp - returns *on if group is a TEST group
     P*   co#    - passed in for validation (required)
     P*   group  - passed in for validation (required)
     P*   fnrrn  - returns the RRN of WMFNP
     P*====================================================================
     P*
     DTestGrp          PR             1
     D co#                            3  0 const
     D group                          3  0 const
     D fnrrn                          9  0       options(*nopass)
     D*
     P*====================================================================
     P* ValidGrp - Valid group
     P*
     P*  ValidGrp - return *on if group is valid
     P*   group   - passed in for validation  (required)
     P*   fnrrn   - returns the RRN of WMFNP
     P*   co#     - passed in for validation
     P*====================================================================
     DValidGrp         PR             1
     D group                          3  0                  const
     D fnrrn                          9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
      ***  Begin Add  ***  10/09/13  **************************************
     P*====================================================================
     P* ExpiredGrp -Expired Group
     P*
     P* ExpiredGrp - return *on if group is Expired.
     P*   co#     - passed in for validation  (required)
     P*   group   - passed in for validation  (required)
     P*====================================================================
     DExpiredGrp       PR             1
     D co#                            3  0                  const
     D group                          3  0                  const
      ***  End Add  ***  10/09/13  **************************************
     P*====================================================================
     P* ValidGrpYear - Validates group year
     P*
     P*  ValidGrpYear - return *on if group year is valid
     P*   group       - passed in for validation  (required)
     P*   year        - passed in for validation  (required)
     P*   fnrrn       - returns the RRN of WMFNP
     P*   co#         - passed in for validation
     P*====================================================================
     DValidGrpYear     PR             1
     D group                          3  0                  const
     D year                           3  0                  const
     D f1rrn                          9  0 options(*nopass)
     D co#                            3  0 options(*nopass) const
     P*====================================================================
     P* GetPEOFlag - Get group PEO flag
     P*
     P*  GetPEOFlag - returns PEO flag
     P*   group     - passed in for validation  (required)
     P*   co#       - accepts the company # to search over (Optional
     P*====================================================================
     DGetPEOFlag       PR             1
     D group                          3  0                  const
     D co#                            3  0 options(*nopass) const
     P*====================================================================
     P* Get3rdPtyFlag - Get Third Party Flag
     P*
     P*  Get3rdPtyFlag - returns Third Party Flag
     P*   co#       - accepts the company # (required
     P*   group     - accepts the group #   (required)
     P*====================================================================
     DGet3rdPtyFlag    PR             1
     D co#                            3  0                  const
     D group                          3  0                  const
     D*
     P*====================================================================
     P* GetGrpAssFlg - Get Group Assosiation flag value
     P*
     P*  GetGrpAssFlg  - returns Group Assosiation Value
     P*   co#       - accepts the company # (required
     P*   group     - accepts the group #   (required
     P*====================================================================
     DGetGrpAssFlg     PR             1
     D co#                            3  0                  const
     D group                          3  0                  const
      *** Beg Add ***** 09/07/16 ******************************************
     P*====================================================================
     P* GetGroupAssociation  - Get Group Assosiation field valiue
     P*
     P*  GetGroupAssoc - returns Group Assosiation field Value
     P*   co#       - accepts the company # (required
     P*   group     - accepts the group #   (required
     P*   GrpAssociation - returns the Group Association of WMFNP
     P*====================================================================
     DGetGroupAssoc    PR            10
     D co#                            3  0                  const
     D group                          3  0                  const
      *** End Add ***** 09/07/16 ******************************************
      *** beg add ***** 08/02/21 ***********************************************
     P*====================================================================
     P* GetGrScoringVnd   - Return the scoring vendor for the group
     P*   co#             - accepts the company # (required)
     P*   group           - accepts the group #   (required)
     P*   Effective Date  - accepts the Effective Date (Optional)
     P*====================================================================
     DGetGrScoringVnd  pr            25
     D co#                            3  0                  Const
     D group                          3  0                  Const
     D effectiveDate                  7  0 Options(*nopass) Const
     p*** End   Add *** 08/02/21 ********************************************
     P*====================================================================
     P* GetPrmContflg - Get Premium/Contribution verbiage flag.
     P*
     P*  GetPrmContflg - returns Premium/Contribution verbiage flag
     P*   co#       - accepts the company # (required
     P*   group     - accepts the group #   (required
     P*====================================================================
     DGetPrmContflg    PR             1
     D co#                            3  0                  const
     D group                          3  0                  const
     p*** Begin Add *** 08/10/17 ********************************************
     P*====================================================================
     P* GetStatePeoFlag - Get the PEO State Flag
     P*
     P*  GetStatePeoFlag  - returns PEO State flag (1A)
     P*   group           - passed in for retrieval   (required)
     P*   rateSetCode     - passed in for retrieval   (required)
     P*   effectiveDate   - passed in for retrieval   (required)
     P*   co#             - accepts the company # to search over (optional)
     P*====================================================================
     DGetStatePeoFlag  pr             1
     D group#                         3  0                  Const
     D rateSetCode                    3                     Const
     D effectiveDate                  7  0                  Const
     D co#                            3  0 Options(*nopass) Const
     p*** End   Add *** 08/10/17 ********************************************
       //***  Begin Add  ***  02/19/20  *************************
       //===================================================================
       // Create 'with' conditions to build group portal information
       //             - Returns a partial SQL string to be included in 'with'
       //==================================================================
     dSqlWithGroupPortalInfo...
     d                 pr          5000    varying
       //***  Emd   Add  ***  02/19/20  *************************
      ***  Begin Add  ***  08/14/19  **************************************
     P*====================================================================
     P* UseGroupInPortal - Returns *on if group is selected for component
     P*   co#       - accepts the company # (required)
     P*   group     - accepts the group #   (required)
     P*   component - accepts the component (required) {not case sensitive}
     P*                - Quote
     P*                - FROI
     P*                - S/R Online
     P*                - Policy
     P*                - Claim
     P*====================================================================
     DUseGroupInPortal...
     D                 pr             1n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inComponent                   25    const varying
      ***  End   Add  ***  08/14/19  **************************************
