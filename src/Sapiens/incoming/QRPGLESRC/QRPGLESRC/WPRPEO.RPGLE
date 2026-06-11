      * Data Structures
     D dsPeoInfo       DS           500    qualified
     D  name                        100    Overlay(dsPeoInfo:1)
     D  fedName                     100    Overlay(dsPeoInfo:*next)
     D  address1                     50    Overlay(dsPeoInfo:*next)
     D  address2                     50    Overlay(dsPeoInfo:*next)
     D  address3                     50    Overlay(dsPeoInfo:*next)
     D  city                         25    Overlay(dsPeoInfo:*next)
     D  state                         2    Overlay(dsPeoInfo:*next)
     D  zip                           9    Overlay(dsPeoInfo:*next)
     D   zip5                         5    Overlay(zip)
     D   zip4                         4    Overlay(zip:6)
     D  fein                          9    Overlay(dsPeoInfo:*next)
     D  PhoneAreaCode                 3    Overlay(dsPeoInfo:*next)
     D  phone#                        7    Overlay(dsPeoInfo:*next)
     D  phone#Full                   12    Overlay(dsPeoInfo:*next)
     D  empLink                      15    Overlay(dsPeoInfo:*next)
     D  filler1                      68    Overlay(dsPeoInfo:*next)

     D dsCltInfo       DS           500    qualified
     D  name                        100    Overlay(dsCltInfo:1)
     D  fedName                     100    Overlay(dsCltInfo:*next)
     D  address1                     50    Overlay(dsCltInfo:*next)
     D  address2                     50    Overlay(dsCltInfo:*next)
     D  address3                     50    Overlay(dsCltInfo:*next)
     D  city                         25    Overlay(dsCltInfo:*next)
     D  state                         2    Overlay(dsCltInfo:*next)
     D  zip                           9    Overlay(dsCltInfo:*next)
     D   zip5                         5    Overlay(zip)
     D   zip4                         4    Overlay(zip:6)
     D  fein                          9    Overlay(dsCltInfo:*next)
     D  PhoneAreaCode                 3    Overlay(dsCltInfo:*next)
     D  phone#                        7    Overlay(dsCltInfo:*next)
     D  phone#Full                   12    Overlay(dsCltInfo:*next)
     D  empLink                      15    Overlay(dsCltInfo:*next)
     D  filler1                      68    Overlay(dsCltInfo:*next)

     D*=====================================================================
     D* PEO procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* GetCltInfo - Get Client Information (Name, address, FEIN, phone#)
     P*=====================================================================
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
      *
     DGetCltInfo       PR           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const
     D qqemp#                         9  0 Const
     D qqdiv                          5  0 Const
     D qqfyr                          3  0 Const
      *
     P*=====================================================================
     P* GetPeoInfo - Get PEO Information (Name, address, FEIN, phone#)
     P*=====================================================================
     P*  Parameters:   Company, fund
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

     DGetPeoInfo       PR           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const
