     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H/TITLE WA101R - API to Retrieve Territorial Differential Factor
      //============================================================
      //  Revisions:
      //
      //  01/03/20 C:267 Support: 84971
      //                 Fix division work field definition to be 5 0
      //
      //  12/13/19 C:267 Support: 84971
      //                 Fix incorrect division being returned to calling program
      //
      //  08/27/19 C:291 Project 150WEA
      //                 Recalculate TDF. Removed Zip code range logic.
      //
      //  07/16/19 C:136 Support: 80959
      //                 Allow TDF to be recalculated. Changes not commented
      //
      //  05/23/19 C:62  Project 150WEA
      //                 Check to see if we should use the state reporting
      //                 division.
      //
      //  Created on 07/25/17  C:62  Project: 150WAL                     *
      //============================================================
     Fwmtzl1    if   e           k Disk    Rename(ptz01:ptz01)
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
     Fwdf2l     if   e           k Disk    Rename(pf201:pf200)
     F*** Begin Add ** 05/23/19 ***************************************
     Fwdrrl     if   e           k Disk    Rename(prr01:prr00)
     F*** End   Add ** 05/23/19 ***************************************

     d*** Begin Add ** 05/23/19 ***************************************
     d/copy WprEmpLr
     d*** End   Add ** 05/23/19 ***************************************
     d inCompany#      s              3  0
     d inGroup#        s              3  0
     d inEmployer#     s              9  0
     d inDivision      s              5  0
     d inYear          s              3  0
     d outTDF          s              5  4
     d zipCode         s              5  0
     d zeroGroup#      s              3  0 Inz(0)
     d*** Begin Add ** 05/23/19 ***************************************
     d ratingState     s                   Like(RRst)
     d polFromDate     s                   Like(RRefdt)
     d*** End   Add ** 05/23/19 ***************************************
     d*** Beg Add ***** 08/27/19 ***************************************
     d zeroCar#        s              3  0 Inz(0)
     d*** End Add ***** 08/27/19 ***************************************

     d*** Beg Add ***** 12/13/19 ***************************************
     d*** Beg Chg ***** 01/03/20 ***************************************
      *** svDivision      s              3  0 Inz
     d svDivision      s              5  0 Inz
     d*** End Chg ***** 01/03/20 ***************************************
     d*** End Add ***** 12/13/19 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *Entry        Plist
     C                   Parm                    inCompany#
     C                   Parm                    inGroup#
     C                   Parm                    inEmployer#
     C                   Parm                    inDivision
     C                   Parm                    inYear
     C                   Parm                    outTDF
     C*
      /free

       // Initialize the out territorial differential
       outTDF = 0;

       //*** Begin Add *** 05/23/19 ****************************************
       // Get the Rating State.
       ratingState = GetRatingState(inCompany#:inGroup#:inEmployer#:
                        inDivision:inYear);

       // Get the Policy Effective Date
       polFromDate = RtvPolFrDt(inCompany#:inGroup#:inEmployer#:inDivision:
                        inYear);

       // Access the State Statistical File
       Clear  RRpullSRD;
       SetGt  (inCompany#:inGroup#:ratingState:polFromDate) prr00;
       ReadPE (inCompany#:inGroup#:ratingState)             prr00;

       //*** Beg Add ***** 12/13/19 *******************************************
       // save original division coming in so it won't be inadvertantly changed
       svDivision = inDivision;
       //*** End Add ***** 12/13/19 *******************************************

       // Should we use the state reporting division?
       If RRpullSRD = 'Y';
          inDivision = GetStRptDv(inCompany#:inGroup#:inEmployer#);
       EndIf;
       //*** End   Add *** 05/23/19 ****************************************

       // Access Employer Master
       Chain (inCompany#:inGroup#:inEmployer#:inDivision) pem00;

       // Access Group Auxiliary
       Chain (inCompany#:inGroup#) pf200;

       // Get the Zip code
       If EMpZip <> 0;
          zipCode = %Dec(%Subst(%EditC(EMpZip:'X'):1:5):5:0);
       Else;
          zipCode = %Dec(%Subst(%EditC(EMmZip:'X'):1:5):5:0);
       EndIf;

       //*** Beg Chg ***** 08/27/19 *******************************
       // Look for group specific entry
       //SetGt  (F2Car#:F2fnd:zipCode) ptz01;
       //ReadPe (F2Car#:F2fnd) ptz01;
       //If Not %Eof(wmtzl1) And zipCode <= TZtoZip;
       //   outTDF = TZtdf;
       //   Exsr LeavePgm;
       //Endif;

       // Look for generic entry (grp = 0)
       //SetGt  (F2Car#:zeroGroup#:zipCode) ptz01;
       //ReadPe (F2Car#:zeroGroup#) ptz01;
       //If Not %Eof(wmtzl1) And zipCode <= TZtoZip;
       //   outTDF = TZtdf;
       //EndIf;

       // Look for specific entry by Carr#/Group/Zip code
       SetGt  (F2Car#:F2fnd:zipCode:polFromDate) ptz01;
       ReadPe (F2Car#:F2fnd:zipCode) ptz01;
       If Not %Eof(wmtzl1) And (TZexdt = 0 or TZexdt > polFromDate);
          outTDF = TZtdf;
          Exsr LeavePgm;
       Endif;

       // Look for Group specific record
       SetGt  (ZeroCar#:F2fnd:zipCode:polFromDate) ptz01;
       ReadPe (ZeroCar#:F2fnd:zipCode) ptz01;
       If Not %Eof(wmtzl1) And (TZexdt = 0 or TZexdt > polFromDate);
          outTDF = TZtdf;
          Exsr LeavePgm;
       Endif;

       // Look for Carrier# specific record
       SetGt  (F2Car#:zeroGroup#:zipCode:polFromDate) ptz01;
       ReadPe (F2Car#:zeroGroup#:zipCode) ptz01;
       If Not %Eof(wmtzl1) And (TZexdt = 0 or TZexdt > polFromDate);
          outTDF = TZtdf;
          Exsr LeavePgm;
       Endif;

       // Look for default record by zip code
       SetGt  (ZeroCar#:zeroGroup#:zipCode:polFromDate) ptz01;
       ReadPe (ZeroCar#:zeroGroup#:zipCode) ptz01;
       If Not %Eof(wmtzl1) And (TZexdt = 0 or TZexdt > polFromDate);
          outTDF = TZtdf;
          Exsr LeavePgm;
       Endif;
       //*** End Chg ***** 08/27/19 *******************************
       Exsr LeavePgm;

       //--------------------------
       Begsr LeavePgm;

          //*** Beg Add ***** 12/13/19 *****************************************
          // be sure to return to the calling pgm the original incoming division
          inDivision = svDivision;
          //*** End Add ***** 12/13/19 *****************************************

          *Inlr = *On;
          Return;
       Endsr;
       //--------------------------

      /end-free
