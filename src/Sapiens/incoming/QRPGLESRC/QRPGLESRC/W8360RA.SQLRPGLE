     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W8360RA - Info Window for Prior Self Reports
     H Bnddir('WTBNDDIR')
      //==============================================================
      // Revisions
      //
      //
      // Created by C:136 for 150WEG
      //==============================================================
      /Free
       Dcl-Pr W8360RA ExtPgm;
        *N Char(3);
        *N Char(5);
        *N Char(1);
       End-Pr;
       Dcl-Pi W8360RA;
        inFnd Char(3);
        inVendor Char(5);
        outResponse Char(1);
       End-Pi;
       Dcl-S chrIsoReportDate Char(8);
       Dcl-S intCYMD Packed(7:0);
       Dcl-S intPayrollCount Packed(7:0);
       Dcl-S intRPMY Packed(5:0);
       Dcl-S intUserCo# Packed(3:0);
       Dcl-S intVendorCount Packed(3:0);
      /End-Free
     d psds          esds                  ExtName(SZq1p)

      /Copy SprGenPop
      /Copy WprUsrPrms
      /Free
       intUserCo# = UserCo#(Q1user);
       If inVendor <> *Blanks;
          Exec Sql
           Select Count(*)
           Into :intVendorCount
           From WDVHP
           Where VHco# = :intUserCo#
            And VHgrp = :inFnd
            And VHvdr# = :inVendor;
       Endif;
       Exec Sql
        Declare C1 Cursor For
        Select Distinct PUrpdt from WXPUPA;
       Exec Sql
        Open C1;
       Dou sqlStt = '02000';
          Exec Sql
           Fetch Next
           From C1
           Into :chrIsoReportDate;
          If sqlStt = '02000';
             Leave;
          Endif;
          Monitor;
             intCYMD = %Dec(%Char(%Date(chrIsoReportDate:*Iso0):*Cymd0):7:0);
          On-Error;
             Iter;
          Endmon;
          intRPMY = intCYMD/100;
          If intVendorCount = *Zero;
             Exec Sql
              Select Count(*)
              Into :intPayRollCount
              From WASRP
              Where SRco# = :intUserCo#
               And SRfnd = :inFnd
               And SRipgm = 'W8361R'
               And (SRtodt = :intCYMD
                Or SRrpmy = :intRPMY);
          Else;
             Exec Sql
              Select Count(*)
              Into :intPayRollCount
              From WASRP Join WDVHP
               On SRco#   = VHco#
               And SRfnd  = VHgrp
               And SRemp# = VHemp#
               And SRdiv  = VHdiv
               And SRfyr  = VHfyr
              Where SRco# = :intUserCo#
               And SRfnd  = :inFnd
               And VHvdr# = :inVendor
               And SRipgm = 'W8361R'
               And (SRtodt = :intCYMD
                Or SRrpmy = :intRPMY);
          Endif;
          If intPayrollCount > 0;
             Leave;
          Endif;
       Enddo;
       Exec Sql
        Close C1;
       If intPayrollCount > 0;
          outResponse =
           PopTwoBtn('Payrolls have been previously imported for one or ' +
           'more report dates indicated in the import file. Press OK to '  +
           'Continue or Cancel to Exit':
           'Informational Message');
       Else;
          outResponse = '1';
       Endif;
       Return;
      /End-Free
