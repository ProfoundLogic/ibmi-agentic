     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title QRY118040B - 2nd Query for Incorrect Cancellation Date
     h dftname(QRY118040B)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 11/29/24                                         *
      //  Programmer: C:300                                            *
      //  Support   : 118040 (UBIC) -- Find Incorrect Cancellation Date*
      //  Description: This program will find policies having Incorrect*
      //               Cancellation Date                               *
      //===============================================================*
      //  Revisions:                                                   *
      *                                                                *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Employer Master file
     fwmeml     if   e           k disk    rename(pem01:pem00)
      * Employer Detail file
     fwdell     if   e           k disk    rename(pel01:pel00)
      * Cancellation Detail File
     fwdcxl1    if   e           k disk    rename(pcx01:pcx00)
     fwdcxl10   if   e           k disk    rename(pcx01:pcx10)
     f                                     prefix(L10_)
      * Query Report File for Incorrect Cancellation Date
     fqrycxp    o    e             disk

     d psds          esds                  extname(szq1p)

     d xxFyr           s              3  0 inz

      *
     c     keyWdcxl10    klist
     c                   kfld                    CXco#
     c                   kfld                    CXfnd
     c                   kfld                    CXemp#
     c                   kfld                    CXdiv
     c                   kfld                    xxFyr
     c                   kfld                    CXcxdt


      /free

       // we need to find is Cancellation history records (WDXHP) where there is a Cancellation
       // record in a given year, but no corresponding Reinstatement record (so, it is not marked as
       // reinstated), with a matching Cancellation history record in the following policy year
       // (matching cancellation issue date and cancellation effective date) that also has a
       // corresponding reinstatement record (so, it is marked as reinstated)

       xxFyr = 0;

       // Read Cancellation Detail File in loop
       Setll *loval pcx00;
       Read pcx00;
       Dow not %EOF(WDCXL1);
          If CXredt = 0;                     // Reinstatement Date is 0 means Cancel policy
             // Check Policy Cancel Date in WMEMP
             Chain (CXco#:CXfnd:CXemp#:CXdiv) pem00;
             If %found(WMEML);
                If EMcad1 = 0;               // policy has been reinstated
                   xxFyr = CXfyr + 1;              // Read next policy year record in WDCXP
                   Setll keyWdcxl10 pcx10;
                   Reade keyWdcxl10 pcx10;
                   If not %EOF(WDCXL10);
                      If CXcxdt = L10_CXcxdt and CXcxdt = L10_CXredt;
                         Exsr WrtRpt;
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
          Read pcx00;
       Enddo;

       *InLr = *On;


       Begsr WrtRpt;                        // Write Report
        Clear pqx01;
        QXco#  = CXco#;
        QXfnd  = CXfnd;
        QXemp# = CXemp#;
        QXdiv  = CXdiv;
        QXfyr  = CXfyr;
        QXcxdt = CXcxdt;
        QXpgm  = CXipgm;

        Setgt (CXco#:CXfnd:CXemp#:CXdiv) pel00;
        Readpe (CXco#:CXfnd:CXemp#:CXdiv) pel00;
        If not %EOF(WDELL);
           QXefdt = ELpfdt;
           QXexdt = ELptdt;
        Endif;

        QXemcad1 = EMcad1;

        Write pqx01;
       Endsr;

      /end-free
