     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h BNDDIR('STBNDDIR')
     h dftactgrp(*no) actgrp(*caller)
     h*
     h/TITLE IDC Quarterly - Prompt
     h*********************************************************************
     h* LODGNTBLFL: Load generic table flags for IDC table values
     h*---------------------------------------------------------------------
     h* Created by C:079 8/19/19 for WC2604
     h*---------------------------------------------------------------------
     h* Revisions:
     h*
     h* 12/05/25 C:079 Project: 126WFX
     h*                Update per the latest NCCI IDC Guidebook
     h*
     h* 02/24/20 C:079 Project: WC2604
     h*                Get the system date to load to maintenance date
     h*
     h* 12/16/19 C:079 Support: 99/85994
     h*                Update Xref for EMST
     h*---------------------------------------------------------------------
     f*
     fsmcop     if   e           k disk
     fwtgtl     uf   e           k disk
     d* Copybooks
     d*---------------------------------------------------------------------
     d/copy sprUsrPrms
     d/copy sprDate
     d*---------------------------------------------------------------------
     d* Begin Standalone fields
     d*---------------------------------------------------------------------
     d xxCo#           s              3s 0
     d xxTbId          s              4a
     d*---------------------------------------------------------------------
     d* End Standalone fields
     d*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
     d*---------------------------------------------------------------------
      /Free

       //* IDC Jurisdiction States

       Read pco01;
       Dow not %Eof(smcop);

       xxCo# = COco#;
       xxTbId = 'IJST';

       Setll (xxCo#:xxTbId) pgt01;
       Reade (xxCo#:xxTbId) pgt01;
       Dow Not %Eof(wtgtl);
          Select;
          When GTkey = 'AL';
             GTcF01 = '01';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'AK';
             GTcF01 = '54';
             UpdGenTable();

          When GTkey = 'AZ';
             GTcF01 = '02';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'AR';
             GTcF01 = '03';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'CO';
             GTcF01 = '05';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'CT';
             GTcF01 = '06';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'DC';
             GTcF01 = '08';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'FL';
             GTcF01 = '09';
             UpdGenTable();

          When GTkey = 'GA';
             GTcF01 = '10';
             UpdGenTable();

          When GTkey = 'HI';
             GTcF01 = '52';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'ID';
             GTcF01 = '11';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'IL';
             GTcF01 = '12';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'IN';
             GTcF01 = '13';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'IA';
             GTcF01 = '14';
             //*** Beg Add *** 12/05/25 ************************************
             GTfg01 = 'L';
             //*** End Add *** 12/05/25 ************************************
             UpdGenTable();

          When GTkey = 'KS';
             GTcF01 = '13';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'KY';
             GTcF01 = '16';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'LA';
             GTcF01 = '17';
             UpdGenTable();

          When GTkey = 'ME';
             GTcF01 = '18';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'MD';
             GTcF01 = '19';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'MN';
             GTcF01 = '22';
             UpdGenTable();

          When GTkey = 'MS';
             GTcF01 = '23';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'MO';
             GTcF01 = '24';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'MT';
             GTcF01 = '25';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'NE';
             GTcF01 = '26';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'NV';
             GTcF01 = '27';
             UpdGenTable();

          When GTkey = 'NH';
             GTcF01 = '28';
             //*** Beg Add *** 12/05/25 ************************************
             GTfg01 = 'L';
             //*** End Add *** 12/05/25 ************************************
             UpdGenTable();

          When GTkey = 'NM';
             GTcF01 = '30';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'NC';
             GTcF01 = '32';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'OK';
             GTcF01 = '35';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'OR';
             GTcF01 = '36';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'RI';
             GTcF01 = '38';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'SC';
             GTcF01 = '39';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'SD';
             GTcF01 = '40';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'TN';
             GTcF01 = '41';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'TX';
             GTcF01 = '42';
             UpdGenTable();

          When GTkey = 'UT';
             GTcF01 = '43';
             UpdGenTable();

          When GTkey = 'VA';
             GTcF01 = '45';
             GTfg01 = 'L';
             UpdGenTable();

          When GTkey = 'WV';
             GTcF01 = '47';
             UpdGenTable();

          When GTkey = 'WI';
             GTcF01 = '48';
             GTfg01 = 'L';
             UpdGenTable();

          //*** Beg Add *** 12/05/25 ***************************************

          When GTkey = 'VT';
             GTcF01 = '44';
             UpdGenTable();

          //*** End Add *** 12/05/25 ***************************************
          Endsl;

          Reade (xxCo#:xxTbId) pgt01;

       Enddo;

       //*** Beg Add *** 12/16/19 ******************************************
       xxTbId = 'EMST';

       Setll (xxCo#:xxTbId) pgt01;
       Reade (xxCo#:xxTbId) pgt01;
       Dow Not %Eof(wtgtl);
          Select;
          When GTkey = 'FT';
             GTfg04 = '1';
             UpdGenTable();
          When GTkey = 'PT';
             GTfg04 = '2';
             UpdGenTable();
          When GTkey = 'SL';
             GTfg04 = '8';
             UpdGenTable();
          When GTkey = 'VO';
             GTfg04 = '9';
             UpdGenTable();
          When GTkey = 'AD' or GTkey = 'AP' or GTkey = 'DS' or
               GTkey = 'NE' or GTkey = 'OS' or GTkey = 'PW' or GTkey = 'RT';
             GTfg04 = 'X';
             UpdGenTable();
          When GTkey = '9';
             GTfg04 = '9';
             UpdGenTable();
          When GTkey = '8';
             GTfg04 = '8';
             UpdGenTable();
          When GTkey = '1';
             GTfg04 = '1';
             UpdGenTable();
          When GTkey = '2';
             GTfg04 = '2';
             UpdGenTable();
          Other;
             GTfg04 = 'X';
             UpdGenTable();
          Endsl;
          Reade (xxCo#:xxTbId) pgt01;
       Enddo;
       //*** End Add *** 12/16/19 ******************************************

       Read pco01;
       Enddo;

       *inlr = *on;
       Return;
      /Free
     P*=====================================================================
     P* UpdGenTable - Update Generic Table
     P*=====================================================================
     PUpdGenTable      B                   Export
     DUpdGenTable      PI
     d sys_date        s               D   datfmt(*USA)
     d sys_time        s               T
     D
      /Free
       GTuser = Q1user;
       GTpgm  = Q1pgm;
       sys_time = %time();
       //*** Beg Add *** 02/24/20 ******************************************
       sys_Date = %Date();
       //*** End Add *** 02/24/20 ******************************************
       GTdt   = Dateto7(sys_date);
       GTtime = TimeTo6(SystemTime());
       Update pgt01;
      /End-free

     PUpdGenTable      E
