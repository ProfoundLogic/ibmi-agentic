     HCopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE Fix80573 - Write WAQTP for MTE Dlt or Res Division
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('WTBNDDIR')
      // Created by C:136 for PHW80573
      // Program writes a CX or RE type Quote Tracking record for division
      //  deleted or rescinded by Midterm Endorsement
      /Free
       Dcl-F WAQTL16 Usage(*Input:*Output) Keyed Rename(pqt01:pqt16);
       Dcl-F WDCXL Usage(*Update) Keyed Rename(pcx01:pcx00);
       Dcl-F WDELL Keyed Rename(pel01:pel00);
       Dcl-F WDXHL Usage(*Update) Keyed Rename(pxh01:pxh00);
       Dcl-F WMAHL Keyed Rename(pah01:pah00);
       Dcl-Ds pgmStat Ext Psds ExtName('SZQ1P');
       End-Ds;
       Dcl-S sysDate7 Zoned(7:0);
       Dcl-S sysTime6 Zoned(6:0);
       sysDate7 = %Dec(%Date():*Cymd);
       sysTime6 = %Dec(%Time():*Hms);
       Dou %Eof(WDELL);
          Read pel00;
          If %Eof;
             Leave;
          Endif;
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pcx00;
          Dou %Eof(WDCXL);
             Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pcx00;
             If %Eof(WDCXL);
                Leave;
             Endif;
             If CXipgm <> 'WTPOLDLTDV';
                Iter;
             EndIf;
             Setgt (CXco#:CXfnd:CXemp#:CXdiv:CXfyr:CXcdat) pah00;
             Dou %Eof(WMAHL);
                Readpe (CXco#:CXfnd:CXemp#:CXdiv:CXfyr) pah00;
                If %Eof;
                   Leave;
                Endif;
                If AHstat = 'A';
                   Leave;
                Endif;
             Enddo;
             If Not %Eof(WMAHL);
                CXagt# = AHagt#;
                CXpgm = Q1pgm;
                Update pcx00;
                Setgt (CXco#:CXfnd:CXfyr:CXemp#:CXdiv:CXreq#) pxh00;
                Dou %Eof(WDXHL);
                   Readpe (CXco#:CXfnd:CXfyr:CXemp#:CXdiv:CXreq#) pxh00;
                   If %Eof;
                      Leave;
                   Endif;
                   XHagt# = AHagt#;
                   XHpgm = Q1pgm;
                   Update pxh00;
                Enddo;
             EndIf;
             If CXredt <> 0;
               // Write Rein record
                Setgt (CXco#:CXfnd:CXemp#:CXdiv:CXrdat) pqt16;
                Readpe (CXco#:CXfnd:CXemp#:CXdiv) pqt16;
                If %Eof Or QTtype = 'R';
                   Leave;
                Endif;
                QTwkdt = CXrdat;
                QTtype = 'R';
                QTpgm = Q1pgm;
                QTuser = Q1user;
                QTdt = sysDate7;
                QTtime = sysTime6;
                Write pqt16;
             Endif;
            // Write Cx record
             Setgt (CXco#:CXfnd:CXemp#:CXdiv:CXcdat) pqt16;
             Readpe (CXco#:CXfnd:CXemp#:CXdiv) pqt16;
             If %Eof Or QTtype = 'C';
                Leave;
             Endif;
             QTwkdt = CXcdat;
             QTtype = 'C';
             QTpgm = Q1pgm;
             QTuser = Q1user;
             QTdt = sysDate7;
             QTtime = sysTime6;
             Write pqt16;
            // Write WAQTP for latest Dlt and Rescind Only
             Leave;
          Enddo;
       Enddo;
       *Inlr = *On;
      /End-Free
