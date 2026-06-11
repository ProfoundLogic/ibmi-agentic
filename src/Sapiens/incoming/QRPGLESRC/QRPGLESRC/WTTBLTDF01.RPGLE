     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Territorial Differential Factors by Zip code
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTTBLTDF
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 07/25/2017 by C:062 for Project 150WAL
     h**********************************************************************
     h* Revisions:
     h*
      * 08/20/19 C:291 Project: 150WEA                                     *
      *                Add Effective and Expiration date. Add Zip Code.    *
      *                Delete From Zip code and To Zip code.               *
     h**********************************************************************
     fwmtzp     if   e             disk    Rename(ptz01:ptz0p)
     fwmfnl     if   e           k disk    Rename(pfn01:pfn00)

      //*** Beg Add ***** 08/20/19 *********************************
      /copy sPrDate
      //*** Beg Add ***** 08/20/19 *********************************
      /copy sPrAtrium
      /copy wPrUsrPrms

      * *ENTRY
     dwtTblTdf01       pr                  extpgm('WTTBLTDF01')
     d                                9  0
     d                            32767
     d                                1

     dwtTblTdf01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d psds          esds                  extname(szq1p)

     d strGroup        s             70
     d strStat         s             10
     d intCo#          s              3  0
      *** Beg Add ***** 08/20/19 *********************************
     d strCar#         s             10
      *** End Add ***** 08/20/19 *********************************

      /Free

       Chain inRRN ptz0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       //*** Beg Chg ***** 08/20/19 *********************************
       //BuildJSON(outJSON :'OutDtlCarrier#':%EditC(TZcar#:'4'));
       BuildJSON(outJSON :'OutDtlCarrier#':%trim(strCar#));
       //*** End Chg ***** 08/20/19 *********************************
       BuildJSON(outJSON :'OutDtlGroup':%trim(strGroup));
       //*** Beg Chg ***** 08/20/19 *********************************
       //BuildJSON(outJSON :'OutDtlFromZip':%EditC(TZfrmZip:'4'));
       //BuildJSON(outJSON :'OutDtlToZip':%EditC(TZtoZip:'4'));
       BuildJSON(outJSON :'OutDtlZipCode':%EditC(TZZip:'4'));
       BuildJSON(outJSON :'OutDtlEffDate':%EditW(dtMDYY(TZefdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlExpDate':%EditW(dtMDYY(TZexdt):'  /  /    '));
       //*** End Chg ***** 08/20/19 *********************************
       BuildJSON(outJSON :'OutDtlTDF':%EditC(TZtdf:'4'));
       BuildJSON(outJSON :'OutDtlTDFStat':%Trim(strStat));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

          // Build Group # and Name
          If TZfnd = 0;
             strGroup = '*All';
          Else;
             intCo# = UserCo#(Q1user);
             Chain (intCo#:TZfnd) pfn00;
             If %Found(wmfnl);
                strGroup = %Char(TZfnd) + '-' + %Trim(FNname);
             EndIf;
          EndIf;

          //*** Beg Add ***** 08/20/19 *********************************
          // Carrier Number
          If TZCar# = 0;
             strCar# = '*All';
          Else;
             strCar# = %Char(TZCar#);
          EndIf;
          //*** End Add ***** 08/20/19 *********************************

          // Build Status
          If TZstat = 'A';
             strStat = 'Active';
          Else;
             strStat = 'InActive';
          EndIf;

       endsr;
      /End-Free
