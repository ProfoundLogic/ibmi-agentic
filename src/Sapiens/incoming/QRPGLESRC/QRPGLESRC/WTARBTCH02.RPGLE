     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Employer Info
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTARBTCH
     h*  Calls to:
     h*
     h*  Created on 08/04/2022 by C:342 for WC2675
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*   04/15/24 C:300 Project: WC2675, Support: 115391              *
     h*                  On Add Batch Details info screen, If no values*
     h*                  then 'Activty Status' field should be Blank.  *
     h*                  It should not look like typeahead input field *
     h*                                                                *
     h*   12/27/23 C:300 Project: WC2675                               *
     h*                  Correct read of Premium Finance Co.           *
     h*                                                                *
     h*                                                                *
     h**********************************************************************
      * employer details - old
     fwzexl     if   e           k disk    rename(pex01:pex01l)
     fwmeml     if   e           k disk
     fwdell2    if   E           K DISK    RENAME(Pel01:Pel0L2)
     fwdell     if   E           K DISK
     fwmpfl     if   E           K DISK
      *** Begin Add ***** 04/15/24 *****************************
     FWTAVL     IF   E           K DISK
      *** End   Add ***** 04/15/24 *****************************
     fwtabl     if   e           k disk
     f                                     INCLUDE(PSY01)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrModule

      *entry
     dWTARBTCH02       pr                  ExtPgm('WTARBTCH02')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dWTARBTCH02       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d comp1           s                   like(EXCO#)
     d fnd1            s                   like(EXFND)
     d emp1            s                   like(EXEMP#)
     d div1            s                   like(EXDIV)
     d year1           s                   like(ELFYR)

     d svcash1         s              1    inz(*blanks)
     d svtrdt1         s              7  0 inz(*zeros)
     d svachf1         s              1    inz(*blanks)
     d renwcash1       s              1    inz(*blanks)

     d SYSDT7          s                   like(EMEFD1)

     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d value           s             20    inz

     d*visPriInfo      s              1    inz
     d visVip          s              1    inz
     d mydate1         s               d   datfmt(*usa)
     d mydate2         s             10A

     d psds          esds                  extname(szq1p)

     d                 DS                  INZ
     d  ECA                    1     35  0
     d                                     DIM(5)                               CX DATES/FILE
     d  EMCAD1                 1      7  0
     d  EMCAD2                 8     14  0
     d  EMCAD3                15     21  0
     d  EMCAD4                22     28  0
     d  EMCAD5                29     35  0
      *
     d                 DS                  INZ
     d  EEF                    1     35  0
     d                                     DIM(5)                               EFF DATES
     d  EMEFD1                 1      7  0
     d  EMEFD2                 8     14  0
     d  EMEFD3                15     21  0
     d  EMEFD4                22     28  0
     d  EMEFD5                29     35  0
      *
     d CANCDT          s                   like(EMCAD1)
     d XXPFDT          s                   like(EMCAD1)
     d IX              s              1P 0 inz(*zeros)
     d SysTyp          s             10A   inz(*blanks)
     d ARCode          s             10A   inz(*blanks)
     d xxofnd          s                   like(EXofnd)
     d xxoemp          s                   like(EXoemp)
     d xxodiv          s                   like(EXodiv)


     d ActStsE         s             50A   inz(*blanks)
      *** Begin Add ***** 04/15/24 *****************************
     d AvKeyf          s             10A   inz(*blanks)
      *** End   Add ***** 04/15/24 *****************************
     d PremFinE        s             50A   inz(*blanks)
     d CanDateE        s              8  0 inz(*zeros)
     d vip             s              3A   inz(*blanks)


     d currYear        s                   like(ELfyr)
     d rnewYear        s                   like(ELfyr)

     c     emky          klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    emp1
     c                   kfld                    div1

     c     elky          klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    emp1
     c                   kfld                    div1
     c                   kfld                    year1

     c     elky2         klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    emp1
     c                   kfld                    div1
     c                   kfld                    sysDt7

     c     tbky          klist
     c                   kfld                    comp1
     c                   kfld                    kkkey

     c     pfky          klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    ELpfin
      *
     c     pfky2         klist
     c                   kfld                    comp1
     c                   kfld                    fnd1
     c                   kfld                    ELpfin
     c                   kfld                    XXPFDT



     c     *LIKE         DEFINE    SYKEY         KKKEY

      /Free
       //                          1   5  8  9
       // input data is in format: ,ccc,acct
        // ccc = Co#, acct = Bank Account

       Exsr ProcessGenParm;
       if RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       endif;

       xxofnd = 0;
       xxoemp = 0;
       xxodiv = 0;
       chain(n) (comp1:fnd1:emp1:div1) pex01l;
      // Check if employer details found
       if %found(wzexl);
          xxofnd = EXofnd;
          xxoemp = EXoemp;
          xxodiv = EXodiv;
       // visPriInfo= 'Y';
       endif;

       BuildJSON(outJSON :'Points' :%char(xxofnd));
       BuildJSON(outJSON :'Points1' :%char(xxoemp));
       BuildJSON(outJSON :'Points2' :%char(xxodiv));
       // BuildJSON(outJSON :'Points3' :visPriInfo);

       //  year1 = 999 represent that year filed on screen is hidden
       if svcash1 = 'Y';

          sysDt7 = DateTo7(SystemDate);
          if svachf1= 'L' and renwcash1=' ';
             sysDt7 = svtrdt1;
          endif;

          if year1 = 999;
             currYear = 0;
             rnewYear = 0;
             // Get current year
             Setgt elky2 pel0l2;
             Readpe emky pel0l2;
             if not(%eof);
                currYear = ELfyr;
             endif;
             // Get renewal year
             Setll elky2  pel0l2;
             Reade emky pel0l2;
             if not(%eof);
                rnewYear = ELfyr;
             endif;
             //Use the renewal year as the current year if current year doesn't exist.
             if currYear = 0;
                year1 = rnewYear;
             endif;
             //Use the current year as the renewal year if renewal year doesn't exist.
             if rnewYear = 0;
                year1 = currYear;
             endif;
          endif;
       endif;

       Chain emky wmeml;
       if %found(wmeml);
          BuildJSON(outJSON :'Points4' :EMname);
          CanDateE = *ZEROS;

          // determine cancellation date
          CancDt = *zeros;
          ELpfdt = *zeros;
          ELptdt = *zeros;
          IX = *zeros;
          IX = 5    ;
          Dow EEF(IX) = *zeros and IX >=1;
             IX -= 1;
          Enddo;
          CancDt =  ECA(IX);
          CanDateE = DtMDYY(CancDt);
          if Cancdt <> *zeros;
             mydate1 = %date(CanDateE: *usa);
             mydate2 = %CHAR(mydate1 : *usa);
          endif ;
          BuildJSON(outJSON :'Points6' :mydate2);

          // get activity status
          //***  Beg Chg  **** 12/27/23 *******************************
          //*** chain elky wdell;
          setgt elky wdell;
          readpe elky wdell;
          //***  End Chg  **** 12/27/23 *******************************
          ActStsE = ELascd;
          //***  Beg Add  **** 04/15/24 *******************************
          // Get Activity Status
          If ELascd <> *blanks;
             AvKeyf = ELascd;
             Chain(n) (ELco#:AvKeyf) WTAVL;
             If %found(WTAVL);
                ActStsE = %trim(AVkey) + ' - ' + %trim(AVdesc);
             Endif;
          Endif;
          //***  End Add  **** 04/15/24 *******************************
          BuildJSON(outJSON :'Points5' :ActStsE);

          vip = *blanks;
          if emvip = 'Y';
             vip = 'VIP';
             visVip = 'Y';
             BuildJSON(outJSON :'Points7' :vip);
             BuildJSON(outJSON :'Points10' :visVip);
          endif;

          //If a Premium finance co. has been assigned to the policy, load
          PremFinE = *blanks;
          XXPFDT = ELPFDT;

          if ELpfin <> *blanks;
             setgt pfky2 WMPFL;
             readpe pfky  WMPFL;
             //***  Beg Chg  **** 12/27/23 *******************************
             //*** if %found(wmpfl);
             //***   PremFinE = %trim(ELpfin) + ' - ' + %trim(PFname);
             PremFinE = %trim(ELpfin);
             if not %eof(wmpfl);
                PremFinE = %trim(PremFinE) + ' - ' + %trim(PFname);
             //***  End Chg  **** 12/27/23 *******************************
             endif;
          endif ;
          BuildJSON(outJSON :'Points8' :PremFinE);
       endif;

       RtnCode = ' ';
       Return;

       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;
       value = *blanks;
       if startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         if endPos > 0;
           len = endPos-startPos;
           if len > 0;
             value = %Subst(inGenParm:startPos:len);
           endif;
         endif;
       endif;
       EndSr;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract Company No.
         startPos = %Scan(',':inGenParm)+1;
         if startPos <= 1;
           LeaveSr;
         endif;
         Exsr ExtractValue;
         comp1= %int(%trim(value));
       // extract fund number
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         fnd1 = %int(%trim(value));
       // extract employer number
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         emp1 = %int(%trim(value));
       // extract division
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         div1 = %int(%trim(value));
       // extract policy year
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         year1= %int(%trim(value));

       // extract svachf1
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         svachf1  = %trim(value);

       // extract renwcash1
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         renwcash1= %trim(value);

       // extract svcash1
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         svcash1  = %trim(value);

       // extract svtrdt1
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         svtrdt1  = %int(%trim(value));

         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
