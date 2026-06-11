     D*=====================================================================
     D* PEO procedure prototypes
     D*=====================================================================
     P* GetCltInfo - Get Client Information (Name, address, FEIN, phone#)
     P*  Parameters:   Company, fund, employer, division, fund year
     P*  Returns:      500 alpha (blanks if invalid)
     P*              1 - 100 Name
     P*            101 - 200 Federal name
     P*            201 - 250 Address line 1
     P*            251 - 300 Address line 2
     P*            301 - 350 Address line 3
     P*            351 - 375 City
     P*            376 - 377 State
     P*            378 - 386 Zip
     P*            387 - 395 Fein
     P*            396 - 398 Phone area
     P*            399 - 405 Phone number
     P*            406 - 417 Phone edited
     P*            418 - 500 Filler
     P*=====================================================================
     DGetCltInfo       PR           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const
     D qqemp#                         9  0 Const
     D qqdiv                          5  0 Const
     D qqfyr                          3  0 Const
