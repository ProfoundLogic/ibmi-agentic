     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  F6402R - Laod ICD10 Codes
     H*
     H*  Description: This pgm will laod ICD10 codes from Holding File
     H*               into System File
     H*
     H*  Created    : 02/23/2015  Programmer: 212  Project: MC377
     H*
     H*  Called By  : F6402C
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H*
     H*  06/19/15 C:212 Prpject : MC377
     H*                 Fix to make type O do not have a decimal in them
     H*
     H****************************************************************
    F*
     f** file definitions
     f*
    f* PCS ICD10 File
     ffaicd10pcsif   e           k disk
     f*
    f* CM ICD10 File
     ffaicd10cm if   e           k disk
     f*
    f* ICD10 Code File
     ffmicd10l1 uf a e           k disk
     D*
     d** data structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     d** constants
     d*
     d digits          c                   const('0123456789')
     d*
     d** standalone variables
     d*
     d SysDtUsa        s              7  0 inz
     d SysTmHms        s              6  0 inz
     C*
     c/free

        // initializaton subroutine
        exsr SrInit;

        // read PCS code file
        read ppcs01;
        Dow not %eof(faicd10pcs);
           Chain (PCSCode:'O') pic01;

           ICVld  = PCSSpec;
           ICDsc1 = PCSDsc1;
           ICDsc2 = PCSDsc2;
           ICUser = q1user;
           ICPgm  = q1pgm;
           ICDt   = sysdtusa;
           ICTime = systmhms;
           //***** Beg Add *** 06/19/15 ***********************************
           ICFCode = PCSCode;
           //***** End Add *** 06/19/15 ***********************************

           if %found(fmicd10l1);
              update pic01;
           else;

           //***** Beg Del *** 06/19/15 ***********************************
           // // check for digits and convert to xxx.xx if len > 3
           // // 123 stays 123 ; 12345 becomes 123.45 ; 1234A stays 1234A
           // if %check(digits:%trim(PCSCode)) = 0 and
           //    %len(%trim(PCSCode)) > 3;
           //    ICFCode = %subst(PCSCode:1:3) + '.' +
           //              %subst(PCSCode:4:4);
           // else;
           //    ICFCode = PCSCode;
           // endif;
           //***** End Del *** 06/19/15 ***********************************

              ICCode = PCSCode;
              ICInd  = 'O';
              ICStat = 'A';
              ICIUsr = q1user;
              ICIPgm = q1pgm;
              ICIDt  = sysdtusa;
              ICITim = systmhms;

              write pic01;
           endif;

           read ppcs01;
        Enddo;

        // read CM code file
        read pcm01;
        Dow not %eof(faicd10cm);
           Chain (CMCode:'D') pic01;

           ICVld  = CMSpec;
           ICDsc1 = CMDsc1;
           ICDsc2 = CMDsc2;
           ICUser = q1user;
           ICPgm  = q1pgm;
           ICDt   = sysdtusa;
           ICTime = systmhms;

           //***** Beg Add *** 06/19/15 ***********************************

           // check for digits and convert to xxx.xx if len > 3
           // 123 stays 123 ; 12345 becomes 123.45
           if %len(%trim(CMCode)) > 3;
              ICFCode = %subst(CMCode:1:3) + '.' +
                        %subst(CMCode:4:4);
           else;
              ICFCode = CMCode;
           endif;
           //***** End Add *** 06/19/15 ***********************************

           if %found(fmicd10l1);
              update pic01;
           else;

           //***** Beg Del *** 06/19/15 ***********************************
           // // check for digits and convert to xxx.xx if len > 3
           // // 123 stays 123 ; 12345 becomes 123.45 ; 1234A stays 1234A
           // if %check(digits:%trim(CMCode)) = 0 and
           //    %len(%trim(CMCode)) > 3;
           //    ICFCode = %subst(CMCode:1:3) + '.' +
           //              %subst(CMCode:4:4);
           // else;
           //    ICFCode = CMCode;
           // endif;
           //***** Beg Del *** 06/19/15 ***********************************

              ICCode = CMCode;
              ICInd  = 'D';
              ICStat = 'A';
              ICIUsr = q1user;
              ICIPgm = q1pgm;
              ICIDt  = sysdtusa;
              ICITim = systmhms;

              write pic01;
           endif;

           read pcm01;
        Enddo;

        *inlr = *on;

        //================================================================
        // SrInit - initialization subroutine
        //================================================================
         begsr srinit;

           sysdtusa = %dec(%char(%date():*cymd0):7:0);
           systmhms = %dec(%time());

         endsr;
        //===============================================================

      /end-free
