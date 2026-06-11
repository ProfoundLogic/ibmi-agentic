     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
      *========================================================================
      * CVTLDDATA - Copy records from a Group, Year, Employer, Div to other
      *             divisions for the same group, year, employer
      *========================================================================
      * pel01 - Employer Detail File
     fwdell1    if   e           k disk                                         CO#/FND/FYR/EMP#/DIV
      * pld01 - Large Deductible Discount Master File
     fwmldl1    uf a e           k disk                                         CO#/FND/EMP#/DIV/FYR

     d wmldl1ds      e ds                         extname(wmldp) inz
     d wmldl1sv      e ds                         extname(wmldp) prefix(s) inz

     d PSDS          esds                  extname(SZQ1P)
     d/copy sPrDate

      * Entry Parm list
     c     *Entry        PList
     c                   Parm                    @@CO#             3 0          From Company
     c                   Parm                    @@FND             3 0          From Group No.
     c                   Parm                    @@EMP#            9 0          From Employer No.
     c                   Parm                    @@DIV             5 0          From Division
     c                   Parm                    @@Fyr             3 0          From Policy Year

      /free
        // validate the input parameters
        Chain (@@CO#:@@FND:@@FYR:@@EMP#:@@DIV) pel01;        //wdell1 - Employer Detail File
        if %found;
           // retrieve and save entry parm division record
           Chain (@@CO#:@@FND:@@EMP#:@@DIV:@@FYR) pld01;     //wmldl1 - Large Deductible Discount
           if %found;
              wmldl1sv = wmldl1ds;     //save copy from fields
              // Copy to all other divisions
              Setll (@@CO#:@@FND:@@FYR:@@EMP#) pel01;           //wdell1 - Employer Detail File
              Reade (@@CO#:@@FND:@@FYR:@@EMP#) pel01;           //wdell1 - Employer Detail File
              Dow not(%eof);
                 If eldiv <> @@DIV;       //skip current entry parm division number
                    // Add or update?
                    Chain (elCO#:elFND:elEMP#:elDIV:elFYR) pld01;  //wmldl1 - Large Ded. Discount
                    wmldl1ds = wmldl1sv;     //restore copy from fields
                    lddiv    = elDIV;
                    lduser   = Q1USER;
                    ldpgm    = Q1PGM;
                    ldtime   = Timeto6(SystemTime());
                    lddt     = Dateto7(SystemDate());
                    //ldiusr   =
                    //ldtime   =
                    //ldiusr   =
                    //ldipgm   =
                    //lditim   =
                    //ldidt    =
                    if Not %found;
                       write pld01;
                    Else;
                       Update pld01;
                    Endif;
                 Endif;
                 Reade (@@CO#:@@FND:@@FYR:@@EMP#) pel01;        //wdell1 - Employer Detail File
              Enddo;
           Endif;
        Endif;

        *Inlr = *on;
      /End-free
