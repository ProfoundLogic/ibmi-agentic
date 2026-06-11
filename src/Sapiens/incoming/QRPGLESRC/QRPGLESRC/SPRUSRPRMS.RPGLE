     D*=====================================================================
     D* User parameters procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* getNbrUsrs - retrieves the number of non-PDS users
     P*  Returns - number of users
     P*=====================================================================
     DgetNbrUsrs       PR             3  0
     D
     P*=====================================================================
     P* GetUserName - Returns UserName for given User ID
     P*  Returns - User Name
     P*=====================================================================
     DGetUserName      PR            50
     D inUserID                      10    const

     P*=====================================================================
     P* GetUserInfo - Returns User company number along with general information
     P*  Returns - Company #
     P*=====================================================================
     DGetUserInfo      PR             3  0
1    D inUserID                      10    const
2    D outUserName                   50    options(*nopass)
3    D outUserOutQ                   10    options(*nopass)
4    D outUserStaff                   3    options(*nopass)
5    D outUserType                    1    options(*nopass)
6    D outUserLvl                     3  0 options(*nopass)

     P*=====================================================================
     P* GetUserEmail- Returns UserEmail for given User ID
     P*  Returns - User Email
     P*=====================================================================
     DGetUserEmail     PR            60
     D inUserID                      10    const

     P*=====================================================================
     P* GetUserId - Returns User ID for given Staff Code
     P*  Returns - User ID
     P*=====================================================================
     DGetUserId        PR            10
     D inStaffCode                    3    const

     P*=====================================================================
     P* GetUsrStaf- Returns User Staff code for given User ID
     P*  Returns - User Staff code
     P*=====================================================================
     DGetUsrStaf       pr             3
     D inUserID                      10    const
     P*=====================================================================
     P* GetSecLvl- Returns User Security Level for given User ID
     P*  Returns - User Security Level
     P*=====================================================================
     DGetSecLvl        pr             3  0
     D inUserID                      10    const
     P*=====================================================================
     P* GetSecType- Returns User Security Type for given User ID
     P*  Returns - User Security Type
     P*=====================================================================
     DGetSecType       pr             1
     D inUserID                      10    const

     P*=====================================================================
     P* GetUsrRRN - returns RRN for WMUPP
     P*  user     - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrRRN        PR             9  0
     D user                          10    const

     P*=====================================================================
     P* GetUsrTitl - returns user title
     P*  user      - passed in for gathering info (required)
     P*=====================================================================
     DGetUsrTitl       PR            15
     D user                          10    const

     P*=====================================================================
     P* GetContSec- Returns Contacts Security which allows Contacts to be
     P*             updated in inquiry mode.
     P*  Returns -  Contacts Security
     P*=====================================================================
     DGetContUpd       PR             1
     D inUserID                      10    const
