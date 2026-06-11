     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H**************************************************************************************
     H* Created
     H*
     H* 03/06/12  C:189 Project: WC2115
     H*                 Added a procedure to fetch the Location details
     H**************************************************************************************
     H* Revision:
     H*
     H* 07/10/12 c:62 Project WC2115
     H*               Add a new procedure to gather more info.
     H*
     H**************************************************************************************
    F*
     FWMALL     if   e           k disk    rename(pal01:pal00)

     D/copy wprloc

     D psds          esds                  extname(szq1p)

     P*=====================================================================
    P*
    P*            - Accepts co# ,fnd , emp, div, Location, mode
    P*            - returns indicator telling if record was found or not
    P*            - returns Location Name, Phn# and Area Code
    P*            - returns Physical Address 1 , City and  Zip  if mode = 'P'
    P*            - returns Mailing  Address 1 , City and  Zip  if mode = 'M'
    P*            - If mode is blank defaulted to Physical Address
     P*=====================================================================
     PGetLocInfo       b                   export
     DGetLocInfo       PI                  like(*in60)
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
     D Locfound        s                   like(*in60)
      *
      *
     C     keyWMALL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    emp#
     C                   Kfld                    div
     C                   Kfld                    ctg1
      *
      *
      /Free
       Chain keyWMALL pal00;
       If %Found(wmall);
          locFound=*on;

        Name = ALName;
        Zip = ALMzip;
        Cty = ALMcty;

         If mode = 'P' or mode = ' ' ;
              Ad1 = ALPad1;
              Zip = ALPzip;
              Cty = ALPcty;
              St  = ALPst ;
            Else;
              Ad1 = ALMad1;
              Zip = ALMzip;
              Cty = ALMcty;
              St  = ALMst ;
         EndIf;

       EndIf;
            return  locfound ;
      /End-Free

     PGetLocInfo       e
     P*** Begin Add *** 07/10/2012 *****************************************
     P*=====================================================================
    P*
    P*            - Accepts co# ,fnd , emp, div, Location
    P*            - returns indicator telling if rocord was found or not
    P*            - returns Location Name, Phn# and Area Code
    P*            - returns Physical Address 1 , City and  Zip
    P*            - returns Mailing  Address 1 , City and  Zip
     P*=====================================================================
     PGetLocInfo2      b                   export
     DGetLocInfo2      PI              n
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D ctg1                          10                     const
     D name                          40
     D mAd1                          40
     D mAd2                          40
     D mZip                           9  0
     D mCty                          25
     D mSt                            2
     D pAd1                          40
     D pAd2                          40
     D pZip                           9  0
     D pCty                          25
     D pSt                            2
     D phn#                           7  0
     D area                           3  0
     D efdt                           7  0
     D dldt                           7  0
     D locFound        s                   like(*in60)
      *
     C     keyWMALL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    emp#
     C                   Kfld                    div
     C                   Kfld                    ctg1
      *
      *
      /Free
       Chain keyWMALL pal00;
       If %Found(WMALL);
          locFound=*On;
          Name=ALname;
          mAd1=ALmAd1;
          mAd2=ALmAd2;
          mZip=ALmZip;
          mCty=ALmCty;
          mSt =ALmSt;
          pAd1=ALpAd1;
          pAd2=ALpAd2;
          pZip=ALpZip;
          pCty=ALpCty;
          pSt =ALpSt;
          phn#=ALphn#;
          area=ALarea;
          efdt=ALefDt;
          dldt=ALdlDt;
       Else;
          locFound=*Off;
       EndIf;

       return  locFound;

      /End-Free
     PGetLocInfo2      e
     P*** End   Add *** 07/10/2012 *****************************************
