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
     h*  Called from: WTRFBTCH
     h*  Calls to:
     h*
     h*  Created on 08/04/2022 by C:342 for WC2675
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmeml     if   e           k disk
     fwdell     if   E           K DISK
     fwmpfl     if   E           K DISK

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrModule

      *entry
     dWTRFBTCH02       pr                  ExtPgm('WTRFBTCH02')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dWTRFBTCH02       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d comp1           s                   like(EMco#)
     d fnd1            s                   like(EMfnd)
     d emp1            s                   like(EMemp#)
     d div1            s                   like(EMdiv)
     d XXPFDT          s                   like(EMCAD1)
     d PremFinE        s             50A   inz(*blanks)
     d AgtNameE        s             70A   inz(*blanks)

     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d value           s             20    inz

     d @@co#           s              3  0 inz
     d @@fnd           s              3  0 inz
     d @@emp#          s              9  0 inz
     d @@div           s              5  0 inz
     d @@fyr           s              3  0 inz
     d @@agt#          s              5  0 inz
     d @@anam          s             60    inz

      // Get Agent Name
     d GetAgtName      pr                  extpgm('WA002R')
     d @@co#                          3  0
     d @@fnd                          3  0
     d @@emp#                         9  0
     d @@div                          5  0
     d @@fyr                          3  0
     d @@agt#                         5  0
     d @@anam                        60

     d psds          esds                  extname(szq1p)

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

       Chain emky wmeml;
       if %found(wmeml);
          BuildJSON(outJSON :'Points1' :EMname);
          BuildJSON(outJSON :'Points5' :EMcust);

          // get employer details info
          setgt elky wdell;
          readpe elky wdell;

          //If a Premium finance co. has been assigned to the policy, load
          PremFinE = *blanks;
          XXPFDT = ELpfdt;

          if ELpfin <> *blanks;
             PremFinE = %trim(ELpfin);
             setgt pfky2 WMPFL;
             readpe pfky  WMPFL;
             if not %eof(wmpfl);
                PremFinE = %trim(PremFinE) + ' - ' + %trim(PFname);
             endif;
          endif ;
          BuildJSON(outJSON :'Points2' :PremFinE);
       endif;

       // Autofill contested audit flag in Add mode
       if ELctst = 'Y';
          BuildJSON(outJSON :'Points3' :ELctst);
       endif;

       // Get Agent Name
       @@co# = comp1;
       @@fnd = fnd1;
       @@emp#= emp1;
       @@div = div1;
       @@fyr = *zeros;
       @@agt#= *zeros;
       @@anam = *blanks;
       GetAgtName(@@co#:@@fnd:@@emp#:@@div:@@fyr:@@agt#:@@anam);
       AgtNameE = *blanks;
       If @@agt# <> 0;
          AgtNameE = %trim(%Char(@@agt#));
       Endif;
       If @@anam <> *blanks;
          AgtNameE = %trim(AgtNameE) + ' - ' +%trim(@@anam);
       Endif;
       BuildJSON(outJSON :'Points4' :AgtNameE);

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

         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
