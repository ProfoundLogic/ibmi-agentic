     hcopyright('(c) 2019 Sapiens, Inc. All rights reserved.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIX86605 - Fix bad data in WDAEP
     h*****************************************************************
     h* Description:  Fix bad data in WDAEP                           *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     fwdaep     uf   e             Disk

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
       //===================================================================
       // Main Program Logic
       //===================================================================

       // Read through the WDAEP and fix Data Decimal Errors for packed
       // and zoned numeric fields. Fix AEproj data field.
      /free
       Read pae01;
       Dow not %Eof(wdaep);
          AEproj = %Subst(AEproj:1:200);
          Update pae01;
          Read pae01;
       Enddo;
       *inLR = *On;
      /end-free
