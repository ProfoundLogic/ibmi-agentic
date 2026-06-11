     D*=====================================================================
     D* Location procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetLocInfo  - Get Location Information
    P*
    P*            - Accepts co# ,fnd , emp, div, Location, mode
    P*            - returns indicator telling if rocord was found or not
    P*            - returns Location Name
    P*            - returns Physical Address 1 , City and  Zip  if mode = 'P'
    P*            - returns Mailing  Address 1 , City and  Zip  if mode = 'M'
    P*
    P*
     DGetLocInfo       PR                  like(*in60)
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D ctg1                          10                     const
     D mode                           1                     const
     D name                          40
     D Ad1                           40
     D Zip                            9  0
     D Cty                           25
     D St                             2
     D phn#                           7  0
     D area                           3  0
    P*====================================================================
    P* GetLocInfo2  - Get Location Information 2
    P*
    P*            - Accepts co# ,fnd , emp, div, Location
    P*            - returns indicator telling if record was found or not
    P*            - returns Location Name, Phn# and Area Code
    P*            - returns Physical Address 1 , City and  Zip
    P*            - returns Mailing  Address 1 , City and  Zip
    P*
    P*====================================================================
     DGetLocInfo2      PR              n
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D ctg1                          10                     const
     D name                          40
     D mAd1                          40
     D mAd2                          40
     D mZip                           9  0
     D mVty                          25
     D mSt                            2
     D pAd1                          40
     D pAd2                          40
     D pZip                           9  0
     D pVty                          25
     D pSt                            2
     D phn#                           7  0
     D area                           3  0
     D efdt                           7  0
     D dldt                           7  0
