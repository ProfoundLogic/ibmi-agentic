    HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE Automate Rate Set Data Upload Process
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H*  CALLED FROM: W8621C                                              *
    H*                                                                   *
    H*  CALLS TO:  W8622R                                                *
    H*                                                                   *
    H*  CREATED: 06/29/23      C:291     PROJECT: WC2915                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
     H* 11/14/24 C:291 Support: 118020                                    *
     H*                Clear all the numeric fields before Write/Update   *
     H*                wmprp record to prevent loading data from prior    *
     H*                record when these fields are blank or zero in      *
     H*                upload file.                                       *
     H*                                                                   *
     H* 11/13/24 C:344 Project: WC2952                                    *
     H*                Increase Rate field (E8RT) to 8.3.                 *
     H*                                                                   *
     H* 06/29/23 C:291 Project: WC2915                                    *
     H*                Convert to CompSuite and rewrite.                  *
     H*                                                                   *
    H*********************************************************************

       Dcl-F wxprp  Disk Usage(*Input);
       Dcl-F wmprl1 Disk Keyed Usage(*Update);
       Dcl-F wxpxp1 Disk Usage(*Input);
       Dcl-F wmpxl1 Disk Keyed Usage(*Update:*output);
       Dcl-F wmprp  Disk Usage(*Output) Rename(ppr01:ppr0p);

       // *Entry
       Dcl-Pr W8622R ExtPgm('W8622R');
        inUpdWmprp char(1);
        inUpdWmpxp char(1);
       End-Pr;
       Dcl-Pi W8622R;
        inUpdWmprp char(1);
        inUpdWmpxp char(1);
       End-Pi;

       //Program Status Data Structure
       Dcl-Ds SZQ1P ExtName('SZQ1P') psds;
       End-Ds;

       //Stand alone fields
       Dcl-s xxCO# packed(3:0);

      /copy wprusrprms

       //===============================================================
       // Main Processing
       //===============================================================
       xxCo# = userCo#(q1user);

       If inUpdWmprp = 'Y';
       //Add processing for Payroll class code Rates
          Setll 1 wxprp;
          Read wxprp;
          Dow not %eof(wxprp);
             Chain (xxCo#:E8rtcd:%Int(E8clas):%Int(E8EfDt)) ppr01;
             If %Found(wmprl1);
                Exsr MoveToWMPRP;
                Update ppr01;
             Else;
                Exsr MoveToWMPRP;
                Write ppr0p;
             Endif;
             Read wxprp;
          Enddo;
          //Add processing for class code description
          If inUpdWmpxp = 'Y';
             Read wxpxp1;
             Dow not %eof;
                Chain (xxCo#:%Int(P1CLAS):P1EXT:P1st:%Int(P1Efdt)) ppx01;
                If %Found(wmpxl1);
                   Exsr UpdWMPXP;
                Else;
                   Exsr WrtWMPXP;
                Endif;
                Read wxpxp1;
             Enddo;
          Endif;
       Endif;
       *Inlr = *On;

       //**********************************************************************
       // Move Field Values from WXPRP TO WMPRP
       //**********************************************************************
        Begsr MoveToWMPRP;
         //*** Beg Add ***** 11/14/24 ************************
         Clear PRclas;
         Clear PRrt;
         Clear PRmnpr;
         Clear PRexdt;
         Clear PRxfer;
         Clear PRloss;
         Clear PRlcm;
         Clear PRlcex;
         //*** End Add ***** 11/14/24 ************************
         PRco# = XXCo#;
         If E8clas <> *Blank and E8clas <> '0';
            PRclas = %Int(E8clas);
         Endif;
         PRefdt = %Int(E8efdt);
         PRdesc = E8desc;
         If E8rt <> *Blank and E8rt <> '0';
            //*** Beg Chg ***** 11/13/24 ****************************
            //***PRrt = %Dec(E8rt:7:2);
            PRrt = %Dec(E8rt:8:3);
            //*** End Chg ***** 11/13/24 ****************************
         Endif;
         If E8mne8 <> *Blank and E8mne8 <> '0';
            PRmnpr = %Dec(E8mne8:7:2);
         Endif;
         PRpcf = E8pcf;
         PRstat = 'A';
         PRuser = Q1user;
         PRpgm  = Q1PGM;
         PRtime = %Dec(%Char(%Time():*iso0):6:0);
         PRdt = %Dec(%Char(%Date():*cymd0):7:0);
         PRlhcd = E8lhcd;
         PRccfl = E8ccfl;
         PRrtcd = E8rtcd;
         PRhzgr = E8hzgr;
         PRsic  = E8sic;
         PReact = E8eact;
         PRtier = E8tier;
         If E8exdt <> *Blank and E8exdt <> '0';
            PRexdt = %Int(E8exdt);
         Endif;
         If E8xfer <> *Blank and E8xfer <> '0';
            PRxfer = %Int(E8xfer);
         Endif;
         PRgvex = E8gvex;
         If E8loss <> *Blank and E8loss <> '0';
            PRloss = %Dec(E8loss:7:2);
         Endif;
         PRarfl = E8arfl;
         PRsccd = E8sccd;
         PRoccd = E8occd;
         If E8lcm <> *Blank and E8lcm <> '0';
            PRlcm  = %Dec(E8lcm:5:4);
         Endif;
         If E8lcex <> *Blank and E8lcex <> '0';
            PRlcex = %Int(E8lcex);
         Endif;
         PRlogf = E8logf;
         PRncig = E8ncig;
         PRnrat = E8nrat;
        Endsr;

        //**********************************************************************
        // Move Field Values from WXPXP TO WMPXP
        //**********************************************************************
        Begsr MoveToWMPXP;
         //*** Beg Add ***** 11/14/24 ************************
         Clear PXcode;
         Clear PXexdt;
         //*** End Add ***** 11/14/24 ************************
         PXco#  = XXCo#;
         If P1clas <> *Blank and P1clas <> '0';
            PXclas = %Int(P1clas);
         Endif;
         PXext  = P1ext;
         PXdesc = P1desc;
         PXsic  = P1sic;
         PXstat = 'A';
         PXst   = P1st;
         If P1efdt <> *Blank and P1efdt <> '0';
            PXefdt = %Int(P1efdt);
         Endif;
         If P1code <> *Blank and P1code <> '0';
            PXcode = %Int(P1code);
         Endif;
         If P1exdt <> *Blank and P1exdt <> '0';
            PXexdt = %Int(P1exdt);
         Endif;
        Endsr;
        //**********************************************************************
        // Update WMPXP
        //**********************************************************************
        Begsr UpdWMPXP;
         Exsr MoveToWMPXP;
         PXuser = Q1user;
         PXpgm  = Q1pgm;
         PXtime = %Dec(%Char(%Time():*iso0):6:0);
         PXdt   = %Dec(%Char(%Date():*cymd0):7:0);
         Update ppx01;
        Endsr;
        //**********************************************************************
        // Write WMPXP
        //**********************************************************************
        Begsr WrtWMPXP;
         Exsr MoveToWMPXP;
         PXuser = Q1user;
         PXpgm  = Q1pgm;
         PXtime = %Dec(%Char(%Time():*iso0):6:0);
         PXdt   = %Dec(%Char(%Date():*cymd0):7:0);
         PXiusr = Q1user;
         PXipgm = Q1pgm;
         PXitm  = %Dec(%Char(%Time():*iso0):6:0);
         PXidt  = %Dec(%Char(%Date():*cymd0):7:0);
         Write  ppx01;
        Endsr;

