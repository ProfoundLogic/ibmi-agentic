     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Defualt county code
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLLMST
     h*  Calls to:
     h*
     h*  Created on 10/12/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  09/10/25 C:300 Support: 119663
     h*                 Return Billing plan code from State Reporting
     h*                 Division, if SRD present
     h*
     h*  01/14/18 C:136 Project: 150WCJ
     h*                -Add default for PEO Named Insured
     h*                -Change logic of program so that default fields
     h*                 will clear if a record doesn't exist in their
     h*                 respective file
     h*
     h*  09/13/17 C:65  Project: 150WAM
     h*                 Return Default billing plan to StateChange() function
     h*
     h*  08/03/17 C:291 Project: 2504
     h*                 Return Leasing ID to StateChange() function
     h*
     h*  07/31/14 C:291 Support: 85791
     h*                 Populate NAIC Required asterisk based on
     h*                 RRnaic Y/N value
     h*
     h**********************************************************************
      * Zip Codes by County (CO#/STATE/ZIPCODE)
     Fwmczl     if   E           K DISK    rename(pcz01:pcz00)
     Fwdrrl1    if   E           K DISK
     Fwmemp     if   E             DISK    rename(pem01:pem0p)
      *** beg add ***** 01/14/18 *************************
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      *** end add ***** 01/14/18 *************************
      *** Beg Add ***** 09/10/25 *************************
     Fwdell     if   e           k disk    rename(pel01:pel00)
      *** End Add ***** 09/10/25 *************************

      /copy sPrAtrium
      //*** Beg Add ***** 09/10/25 *************************
     d/copy wPrEmplr
      //*** End Add ***** 09/10/25 *************************
      * *ENTRY
     DDefaultCounty    pr                  ExtPgm('WTPOLLMS03')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DDefaultCounty    pi
     D inRrn                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1
      *
     D intCo#          s              3  0 inz
     D phyState        s              2    inz
     D mailState       s              2    inz
     D phyZip          s              5  0 inz
     D mailZip         s              5  0 inz
     D state           s              2    inz
     D xxdate          s              7  0 inz
     D zip             s              5  0 inz
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D dashPos         s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz
     D zip5            s              5    inz
      //*** Beg Add ***** 09/10/25 *************************
     d xxStRptDiv      s                   like(ELdiv)
     d xxBscd          s                   like(ELbscd)
      //*** End Add ***** 09/10/25 *************************
      /Free

       Chain(n) inRrn pem0p;
       //                          1   5  8  9
       // input data is in format: ,ccc,pp,mm,xxxxx-xxxx,zzzzz-zzzz
       // ccc - Co#; pp - physical state; mm - mailing state; xxxxx - phy zip and zzzzz - mail zip

       intCo# = 0;
       phyState = *Blanks;
       mailState = *Blanks;
       phyZip = 0;
       mailZip = 0;
       outJSON = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

        // set key values
       If phyState = *Blanks;
         state = mailState;
         zip = mailZip;
       Else;
         state = phyState;
         zip = phyZip;
       Endif;

       Chain(n) (intCo#:state:zip) pcz00;
       //*** beg chg ***** 01/14/18 **************
       //*** If %found(wmczl);
       //***   BuildJSON(outJSON :'CountyCode' :CZcnty);
       //*** Endif;
       BuildJSON(outJSON :'CountyCode' :CZcnty);
       //*** end chg ***** 01/14/18 **************

       //*** beg add ***** 01/14/18 **************
       Chain (EMco#:EMfnd) pfn00;
       Clear RRdrat;
       Clear RRnaic;
       RRlsid = '1';
       Clear RRbscd;
       Clear RRNmInDft;
       //*** end add ***** 01/14/18 **************
       Eval xxdate = EMefd1;
       Setll (EMco#:EMfnd:state:xxdate) prr01;
       Reade (EMco#:EMfnd:state) prr01;
       BuildJSON(outJSON:'RateSetCode':RRdrat);
       BuildJSON(outJSON:'NAICRequired':RRnaic);
       // Return Leasing Id
       //***** beg add ****** 08/03/17 ********
       If RRLsID <> *Blank;
          BuildJSON(outJSON:'LeasingID':RRLsID);
       Else;
          BuildJSON(outJSON:'LeasingID':'1');
       Endif;
       //***** end add ****** 08/03/17 ********
       //*** begin add ****** 09/13/17 ********
       //*** Beg Chg ***** 09/10/25 *************************
       //*** If RRBSCD <> 0;
       //***    BuildJSON(outJSON:'BSCD':%editc(RRBSCD:'X'));
       //*** Endif;

       If RRbscd <> 0;
          xxBscd = RRbscd;
       Endif;

       // Get State Reporting Division
       xxStRptDiv = GetStRptDv(EMco#:EMfnd:EMemp#);

       // Read WDELP Employer Detail File
       // to Get Billing Plan for State Reporting Division
       Chain (EMco#:EMfnd:EMemp#:xxStRptDiv) pel00;
       If %found(wdell) and ELbscd <> 0;
          xxBscd = ELbscd;
       Endif;

       If xxBscd <> 0;
          BuildJSON(outJSON:'BSCD':%editc(xxBscd:'X'));
       Endif;
       //*** End Chg ***** 09/10/25 *************************
       //*** end add ******** 09/13/17 ********
       //*** beg add ******** 01/14/18 ********
       BuildJSON(outJSON:'PEONamedInsured':%Editc(RRNmInDft:'Z'));
       //*** end add ******** 01/14/18 ********

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = 'X';
       Monitor;
       // extract company number
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         Monitor;
          intCo# = %Int(%trim(value));
         On-Error;
          intCo# = 0;
         EndMon;
       // extract phy state
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         phyState = %trim(value);
       // extract mail state
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         mailState = %trim(value);
       // extract phy zip
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         dashPos = %scan('-':value);
         If dashPos > 0;
           zip5 = %subst(value:1:dashPos-1);
         Else;
           zip5 = %subst(value:1:5);
         Endif;
         Monitor;
           phyZip= %Int(%trim(zip5));
         On-Error;
          phyZip = 0;
         EndMon;
       // extract Mail zip
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         dashPos = %scan('-':value);
         If dashPos > 0;
           zip5 = %subst(value:1:dashPos-1);
         Else;
           zip5 = %subst(value:1:5);
         Endif;
         Monitor;
          mailZip= %Int(%trim(zip5));
         On-Error;
          mailZip = 0;
         EndMon;
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;
      /End-Free
