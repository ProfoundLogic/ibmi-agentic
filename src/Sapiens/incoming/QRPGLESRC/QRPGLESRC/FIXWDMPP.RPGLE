     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h/title FIXWDMPP - WDMPP update for missing Minimum Premium on SUNZ
     h dftname(QRY118040B)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 12/24/24                                         *
      //  Programmer: C:300                                            *
      //  Support   : 118189 (SUNZ) -- V24 update appears to have cleared
      //            : data in WDMPP and WDEHP                          *
      //  Description: This program will update Minimum Premiumn fields*
      //               in WDMPP file for 1 week's missing data         *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  08/06/24 C:300 Support: 118189                               *
      //                 Modify to update WDMPP file only for unauduted*
      //                 policies                                      *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Employer Detail file
     fwdell     if   e           k disk    rename(pel01:pel00)
      * Employer Premium Detail file
     fwdmpp     uf   e             disk    rename(pmp01:pmp0p)
     fwdmpl     if   e           k disk    rename(pmp01:pmp00)
     f                                     prefix(L1_)
      * Output file for WDMPP file updates
     ffixwdmpp  o    e             disk

     d psds          esds                  extname(szq1p)

     d xxEbal2         s             11  2 inz
     d xxAbal2         s             11  2 inz
     d sum_anpr        s             11  2 inz
     d sum_aapr        s             11  2 inz

     d/copy sPrDate
      *

     c     *entry        plist
     c                   parm                    @@mode            1


      /free

       // We need to read WDMPP file for the missing 1 week's data, which could not be recovered
       // from backup. Using calculation logic found in 80100R and W7720R programs, we will be
       // re-calculating Estimated and Audited Minimum premium to update in WDMPP file
       //
       // @@mode = 'U' for Update and @@mode = 'A' for audit/run/report

       xxEbal2 = 0;
       xxAbal2 = 0;
       sum_anpr = 0;
       sum_aapr = 0;

       // Read WDMPP File in loop
       Setll 1 pmp0p;
       Read pmp0p;
       Dow not %EOF(WDMPP);
       // If MPebal <> 0 or MPabal <> 0;            // Est-Aud Balance to Minimum premium
          If MPdt > 1241109 and MPdt < 1241116;     // Run report for 1 week's missing data
             // Check for Minimum Premium flag in WDELP file
             Chain (MPco#:MPfnd:MPemp#:MPdiv:MPfyr) pel00;
             If %found(WDELL);
             // If ELemnp = 'Y' or ELamnp = 'Y';   // Est/Aud Minimum premium flag
                If ELfaf  = 'E' or ELfaf  = 'R';   // Estimated or Revised Bill (non-audited)
                   Exsr GetSum;                    // Get sum of MPanpr/MPaapr from all div
                // If MPebal <> 0;
                   If ELemnp = 'Y';                   // Est/Aud Minimum premium flag
                      xxEbal2 = (ELemna + ELein1 + ELein2 + ELewav) - sum_anpr;
                   Endif;

                   // ELAMNA field already contains the increased limits and waiver charge
                // If MPabal <> 0;
                   If ELamnp = 'Y';                   // Est/Aud Minimum premium flag
                      xxAbal2 = ELamna - sum_aapr;
                   Endif;

                // If xxEbal2 > 0 or xxAbal2 > 0;
                   If @@mode = 'A';
                      Exsr WrtMpp;
                   Endif;
                   If @@mode = 'U' and xxEbal2 > 0;
                      MPebal = xxEbal2;
                //    MPabal = xxAbal2;
                      MPuser = q1user;
                //    MPpgm  = q1pgm;
                //    MPtime = Timeto6(SystemTime());
                //    MPdt   = Dateto7(SystemDate());
                      Update pmp0p;

                      Exsr WrtMpp;
                   Endif;
                // Endif;
                Endif;
             Endif;
          Endif;
          Read pmp0p;
       Enddo;

       *InLr = *On;


       Begsr GetSum;                        // Get sum of MPanpr/MPanpr from all div
        sum_anpr = 0;
        sum_aapr = 0;

        Setll (MPco#:MPfnd:MPfyr:MPemp#) pmp00;
        Reade (MPco#:MPfnd:MPfyr:MPemp#) pmp00;
        Dow not %EOF(WDMPL);
           sum_anpr = sum_anpr + L1_MPanpr;
           sum_aapr = sum_aapr + L1_MPaapr;
           Reade (MPco#:MPfnd:MPfyr:MPemp#) pmp00;
        Enddo;
       Endsr;


       Begsr WrtMpp;                        // Write FIXWDMPP
        Clear pxm01;
        XMco#  = MPco#;
        XMfnd  = MPfnd;
        XMemp# = MPemp#;
        XMdiv  = MPdiv;
        XMfyr  = MPfyr;
        XMdt   = MPdt;
        XManpr = MPanpr;
        XMaapr = MPaapr;
        XMebal = MPebal;
        XMabal = MPabal;
        XMfaf  = ELfaf;
        XMemnp = ELemnp;
        XMamnp = ELamnp;
        XMmapr = ELmapr;
        XMampr = ELampr;
        XM1apr = EL1apr;
        XMemna = ELemna;
        XMamna = ELamna;
        XMebal2= xxEbal2;
        XMabal2= xxAbal2;

        Write pxm01;

        xxEbal2 = 0;
        xxAbal2 = 0;
       Endsr;

      /end-free
