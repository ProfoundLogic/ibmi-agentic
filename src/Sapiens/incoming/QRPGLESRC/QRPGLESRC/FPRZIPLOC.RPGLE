     D*=====================================================================
     D* Zip Code handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* GetZip9Loc - Get the locality by state for 9 digit zip code
     P*  Parameter 1 - 9 character zip code
     P*  Parameter 2 - 2 character state code
     P*  Parameter 3 - 7 digit effective date (cyymmdd)
     P*  Returns - digit locality for the zip code and state
     P*=====================================================================
     DGetZip9Loc       PR             2  0
     D inZip9                         9
     D inState                        2
     D inEfDt                         7  0
     D
