     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      *================================================================
     H* Update Experience Mod Rates to numerice 8.3
     H*
     H* Project : WC2952 C:291  Date: 06/20/24.
     H*
     H* Called from: SYPOSTPGM2
     H* Calls to:    None
     H*
     H*================================================================
     H* Revisions:
     H*
     H*****************************************************************
     Fexxtp     uf   e             disk    rename(pxx01:pxx0p)
     Femmrp     uf   e             disk    rename(pmr01:pmr0p)
     Femexp     uf   e             disk    rename(pex01:pex0p)

     D intIdt          s              7  0
     D intItim         s              6  0
     D psds          esds                  extname(szq1p)

     D/copy sprdate

      /Free
        //Employer Experience Modifier Work File
        Read exxtp;
        Dow not %Eof(exxtp);
           If XMnrt <> 0;
              XMnrt = XMnrt/100;
           Endif;
           If XElrt <> 0;
              XElrt = XElrt/100;
           Endif;
           XUser  = q1user;
           XPgm   = q1pgm;
           XTime  = %Dec(%Time():*hms);
           XDt    = %Dec(%Date():*cymd);
           Update pxx0p;
           Read exxtp;
        Enddo;

        //Manual Rate Master File
        Read emmrp;
        Dow not %Eof(emmrp);
           If MRRate <> 0;
              MRRate = MRRate/100;
           Endif;
           MRUser  = q1user;
           MRPgm   = q1pgm;
           MRTime  = %Dec(%Time():*hms);
           MRDt    = %Dec(%Date():*cymd);
           Update pmr0p;
           Read emmrp;
        Enddo;

        //Expected Loss Rate & Discount Ratio
        Read emexp;
        Dow not %Eof(emexp);
           If EXLsRt <> 0;
              EXLsRt = EXLsRt/100;
           Endif;
           EXUser  = q1user;
           EXPgm   = q1pgm;
           EXTime  = %Dec(%Time():*hms);
           EXDt    = %Dec(%Date():*cymd);
           Update pex0p;
           Read emexp;
        Enddo;

       *Inlr = *On;
      /End-free
