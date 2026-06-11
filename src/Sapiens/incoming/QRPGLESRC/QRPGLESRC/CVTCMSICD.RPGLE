     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  CVTCMSICD - Load ICD10 Codes
     H*
     H*  Description: This pgm will laod ICD10 codes from Holding File
     H*               into System File
     H*
     H*  Created    : 04/27/2015  Programmer: 53   Project: WC2261
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F*
     f** file definitions
     f*
    f* CMS ICD10 File
     fwaicd10p  if   e           k disk
     f*
    f* ICD10 Code File
     fwmicd10l1 uf a e           k disk
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

        // read CMS code file
        read pcm01;
        Dow not %eof(waicd10p);
           Chain (CMCode:'D') pic01;

       //  ICvld  = CMSpec;  //
           ICDsc1 = CMDsc1;
           ICDsc2 = CMDsc2;
           ICUser = q1user;
           ICPgm  = q1pgm;
           ICDt   = sysdtusa;
           ICTime = systmhms;

           if %found(wmicd10l1);
              update pic01;
           else;
              // check for digits and convert to xxx.xx if len > 3
              // 123 stays 123 ; 12345 becomes 123.45 ; 1234A stays 1234A
              if %check(digits:%trim(CMCode)) = 0 and
                 %len(%trim(CMCode)) > 3;
                 ICFCode = %subst(CMCode:1:3) + '.' +
                           %subst(CMCode:4:4);
              else;
                 ICFCode = CMCode;
              endif;
              ICCode = CMCode;
              ICLost = 'D';
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
