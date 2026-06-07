     D*=====================================================================
     D* User Parm processing procedure prototypes
     D*=====================================================================

     P*=====================================================================
     P* GetUserId  - returns the user id for the staff code
     P*  staffCode - passed in for gathering info (required)
     P*=====================================================================
     DGetUserId        PR            10
     D staffCode                      3    const

     P*=====================================================================
     P* GetUsrName - returns user name
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrName       PR            50
     D user                          10    const

     P*=====================================================================
     P* GetUsrOutq - returns user output queue
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrOutq       PR            10
     D user                          10    const

     P*=====================================================================
     P* GetUsrRRN - returns RRN for WMUPP
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrRRN        PR             9  0
     D user                          10    const

     P*=====================================================================
     P* UsrDivRstFlg - returns user restrict division flag
     P*                returns user parm restrict flag ('Y' or 'N')
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DUsrDivRstFlg     PR             1
     D user                          10    const

     P*=====================================================================
     P* UsrEmpRstFlg  - returns user restricted employer flag
     P*                 returns user parm restrict flag ('N','Y','S' or 'M')
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DUsrEmpRstFlg     PR             1
     D user                          10    const

      ***** BEG ADD ***** 04/29/03 *********************************************
     P*=====================================================================
     P* UsrRstFlg  - returns user restricted group flag
     P*  user      - passed in for gathering info (required)
     P*  GrpRstFlg - returns user restricted group flag
     P*=====================================================================
     DUsrRstFlg        PR             1
     D user                          10    const

      ***** END ADD ***** 04/29/03 *********************************************
     P*=====================================================================
     P* GetUsrLvl - Returns user security level
     P*  user     - passed in for gathering info (required)
     P*  type     - returns the user security type
     P*=====================================================================
     DGetUsrLvl        PR             3  0
     D user                          10    const
     D type                           1    options(*nopass)

     P*=====================================================================
     P* GetUsrType - Returns user security type
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrType       PR             1
     D user                          10    const

     P*=====================================================================
     P* GetUsrStaf - returns user staff code
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrStaf       PR             3
     D user                          10    const

     P*=====================================================================
     P* GetUsrTitl - returns user title
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrTitl       PR            15
     D user                          10    const

     P*=====================================================================
     P* UserCo#  - returns user parm company #
     P*  user    - passed in for gathering info (required)
     P*  co_name - returns user parm company name
     P*=====================================================================
     DUserCo#          PR             3  0
     D user                          10    const
     D co_name                       40    options(*nopass)

     P*=====================================================================
     P* UserDiv   - returns user parm division
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DUserDiv          PR             9  0
     D user                          10     const

     P*=====================================================================
     P* UserEmp#  - returns user parm employer
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DUserEmp#         PR             9  0
     D user                          10    const

     P*=====================================================================
     P* UserGrp   - returns user parm group
     P*  user     - passed in for gathering info (required)
     P*  grp_name - returns user parm company name
     P*=====================================================================
     DUserGrp          PR             3  0
     D user                          10    const
     D grp_name                      60    options(*nopass)
      *
     P*=====================================================================
     P* GetUsrEmail - returns user email
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrEmail      PR            60
     D user                          10    const
     P*=====================================================================
     P* ValidStaff - returns '1' for valid staff and '0 for invalid
     P*  staffCode - passed in
     P*=====================================================================
     DValidStaff       pr              n
     D staffCode                      3    const
     P*=====================================================================
     P* GetRsvAutLim - returns reserve authority limit
     P*  user        - passed in for gathering info (required)
     P*=====================================================================
     DGetRsvAutLim     PR            11  2
     D user                          10    const
     P*=====================================================================
     P* GetChgAddrFlag - returns Change Address Flag
     P*  user          - passed in for gathering info (required)
     P*=====================================================================
     DGetChgAddrFlag   PR             1
     D user                          10    const
     P*=====================================================================
     P* GetGrpAuthSql - Returns group Number
     P*  user         - passed in for gathering info (required)
     P*  grpSelect    - returns the "Select" SQL statement
     P*  grpFrom      - returns the "From" SQL statement
     P*  grpWhere     - returns the "Where" SQL statement
     P*  mode         - Add process or change/inquiry process (A/I)
     P*=====================================================================
     DGetGrpAuthSql    PR             3  0
     D user                          10    const
     D grpSelect                    100
     D grpFrom                      200
     D grpWhere                     200
     D mode                           1    const
     P*=====================================================================
     P* GetRsvLimType - Returns Reserve Limit Type
     P*                 I=Individual C=Cumulative N=None
     P*  user         - passed in for gathering info (required)
     P*=====================================================================
     DGetRsvLimType    PR             1
     D user                          10    const
     P*=====================================================================
     P*  ValRsvType    - returns valid/invalid flag
     P*  user          - passed in for gathering info (required)
     p*  rsv type      - passed in for gathering info (required)
     P*=====================================================================
     DValRsvType       PR              N
     D user                          10    const
     D rsvtype                        1    const
