     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h/title FIXWDEHPS - WDEHP update for missing Minimum Premium on SUNZ
     h dftname(FIXWDEHPS)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 02/10/25                                         *
      //  Programmer: C:300                                            *
      //  Support   : 118189 (SUNZ) -- V24 update appears to have cleared
      //            : data in WDMPP and WDEHP                          *
      //  Description: This program will update Missing Premiumn fields*
      //               in WDEHP file for 1 week's missing data         *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Employer Detail History File
     fwdehp     uf   e             disk    rename(peh01:peh0p)
      * Employer Premium Detail file
     fwdmpl     if   e           k disk    rename(pmp01:pmp00)
      * Output file for WDMPP file updates
     ffixwdehps o    e             disk

     d psds          esds                  extname(szq1p)


     d/copy sPrDate
      *

     c     *entry        plist
     c                   parm                    @@mode            1


      /free

       // We need to read WDMPP file for the missing 1 week's data, which could not be recovered
       // from backup. Using calculation logic found in 80100R, W8011R and W7720R programs,
       // we will update missing field (EHWBAL,  EHWAMAN, EHWASTD , EHWANORM, EHWAANN) in WDEHP file
       //
       // @@mode = 'U' for Update and @@mode = 'A' for audit/run/report


       // Read WDEHP File in loop
       Setll 1 peh0p;
       Read peh0p;
       Dow not %EOF(WDEHP);
          If EHtrdt > 1241109 and EHtrdt < 1241116;     // Run report for 1 week's missing data
             // Find its record in WDMPP file
             Setll (EHco#:EHfnd:EHfyr:EHemp#:Ehdiv) pmp00;
             Reade (EHco#:EHfnd:EHfyr:EHemp#:Ehdiv) pmp00;
             Dow not %EOF(WDMPL);
                If EHtrdt = MPdt;
                   If EHfaf  = 'E' or EHfaf  = 'R';   // Estimated or Revised Bill (non-audited)
                      EHWBAL   = MPEBAL;
                      EHWAMAN  = MPMAPR;
                      EHWASTD  = MPSTPR;
                      EHWANORM = MPENMP;
                      EHWAANN  = MPANPR;
                   Endif;

                   If EHfaf  = 'A';                   // Audited
                      EHWBAL   = MPABAL;
                      EHWAMAN  = MPAMPR;
                      EHWASTD  = MPASPR;
                      EHWANORM = MPANMP;
                      EHWAANN  = MPAAPR;
                   Endif;

                   If @@mode = 'A';
                      Exsr WrtEhp;
                   Endif;
                   If @@mode = 'U';
                      Update peh0p;
                      Exsr WrtEhp;
                   Endif;

                Endif;
                Reade (EHco#:EHfnd:EHfyr:EHemp#:Ehdiv) pmp00;
             Enddo;

          Endif;
          Read peh0p;
       Enddo;

       *InLr = *On;




       Begsr WrtEhp;                        // Write FIXWDEHPS
        Clear pxh01;
        XHco#  = EHco#;
        XHfnd  = EHfnd;
        XHemp# = EHemp#;
        XHdiv  = EHdiv;
        XHfyr  = EHfyr;
        XHdt   = EHTRdt;
        XHfaf  = EHfaf;
        XHWBAL   = EHWBAL;
        XHWAMAN  = EHWAMAN;
        XHWASTD  = EHWASTD;
        XHWANORM = EHWANORM;
        XHWAANN  = EHWAANN;

        Write pxh01;
       Endsr;

      /end-free
