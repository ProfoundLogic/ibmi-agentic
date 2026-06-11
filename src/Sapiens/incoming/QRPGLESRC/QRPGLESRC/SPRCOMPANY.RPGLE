     D*=====================================================================
     D* Company processing procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* getCompNam - get company name
     P*  Parameter 1 - 3 numeric
     P*  Returns - 40 character (blanks if invalid)
     P*=====================================================================
     DgetCompNam       PR            40
     D compNum                        3  0 Const
     D
     P*=====================================================================
     P* GetOnlyCo# - Returns co# if only one exists, otherwise *zero
     P*=====================================================================
     DGetOnlyCo#       pr             3  0

     P*=====================================================================
    P*  ValidCo# - return *on if company is valid
    P*   group   - passed in for validation  (required)
    P*   coRRN   - returns the RRN of SMCOP
     P*=====================================================================
     DValidCo#         PR             1n
     D co#                            3  0 const
     D coRRN                          9  0 options(*nopass)
     P*=====================================================================
     P* getInqAuthOvr - get Inquiry Authority Override
     P*  Parameter 1 - 3 numeric
     P*  Returns - 1 character
     P*=====================================================================
     DGetInqAuthOvr    PR             1
     D compNum                        3  0 Const
     P*=====================================================================
     P* GetCompAdr - get company address
     P*  Parameters - Company number (3,0), Address type (1)
     P*    Returns:   Address line 1  (40), Address Line 2 (40), City (25),
     P*               State (2), Zip code (9,0).
     P*=====================================================================
     DGetCompAdr       PR
     D compNum                        3P 0 Const
     D AdrType                        1    Const
     D Adr1                          40
     D Adr2                          40
     D City                          25
     D State                          2
     D Zip                            9P 0
     P*=====================================================================
     P* GetCompFein - Returns FEIN if it exists, otherwise *zero
     P*=====================================================================
     DGetCompFein      PR             9P 0
     D intCo#                         3P 0 Const
     P*=====================================================================
     P* GetCompPhon - Returns Area code and Phone number.
     P*=====================================================================
     DGetCompPhon      PR            10A
     D intCo#                         3P 0 Const
     D PhoneType                      1A   Const
