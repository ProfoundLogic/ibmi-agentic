     H Copyright('(C) Copyright Progressive Data Solutions, Inc., 2017')
     H nomain
    H**********************************************************************************************
     H* REVISIONS:                                                                                 *
     H*                                                                                            *
    H**********************************************************************************************
      * Employer Master
     Fwmeml     if   e           k disk    usropn

     D/copy sprpeo

     P*=====================================================================
     P* GetPEOinfo - Get PEO name, address,fein, phone number
     P*  Parameter Company, fund, employer, division, fund year
     P*  Returns - 500 alpha client info or blanks if invalid.
     P*=====================================================================
     PGetCltInfo       B                   Export
     DGetCltInfo       PI           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const
     D qqemp#                         9  0 Const
     D qqdiv                          5  0 Const
     D qqfyr                          3  0 Const

      *
      * Data Structures
      *
     D CltINfo         DS           500
     D  CltName                     100    Overlay(Cltinfo:1)
     D  CltFedName                  100    Overlay(Cltinfo:101)
     D  CltAdrln1                    50    Overlay(Cltinfo:201)
     D  CltAdrln2                    50    Overlay(Cltinfo:251)
     D  CltAdrln3                    50    Overlay(Cltinfo:301)
     D  CltCity                      25    Overlay(Cltinfo:351)
     D  CltState                      2    Overlay(Cltinfo:376)
     D  CltZip                        9    Overlay(Cltinfo:378)
     D    Cltzip5                     5    Overlay(CltZip)
     D    CltZip4                     4    Overlay(CltZip:6)
     D  CltFein                       9    Overlay(Cltinfo:387)
     D  CltPhArea                     3    Overlay(Cltinfo:396)
     D  CltPhNbr                      7    Overlay(Cltinfo:399)
     D  CltPhone                     12    Overlay(Cltinfo:406)
     D  CltFiller                    83    Overlay(Cltinfo:418)

      *
      * Stand Alone
      *
      /Free
        If not %Open(Wmeml);
           Open Wmeml;
        Endif;

        CltName    = *blanks;
        CltFedName = *blanks;
        CltAdrln1  = *blanks;
        CltAdrln2  = *blanks;
        CltAdrln3  = *blanks;
        CltCity    = *blanks;
        CltState   = *blanks;
        CltZip     = *zeros;
        CltFein    = *zeros;
        CltPharea  = *zeros;
        CltPhnbr   = *zeros;
        CltPhone   = *zeros;
        CltFiller  = *blanks;

        Chain (qqco#:qqfnd:qqemp#:qqdiv) pem01;
        If %found;
          CltName    = emname + emnam2;
          CltFedname = emfdnm + emfdn2;

          If emmpfl = 'P';
             CltAdrLn1 = empad1;
             CltAdrLn2 = empad2;
             CltCity   = empcty;
             CltState  = empst;
             CltZip    = %editc(empzip:'X');
          else;
             CltAdrLn1 = emmad1;
             CltAdrLn2 = emmad2;
             CltCity   = emmcty;
             CltState  = emmst;
             CltZip    = %editc(emmzip:'X');
          Endif;

          If CltZip4 <> '0000';
             CltAdrln3 = %Trim(CltCity) + ', ' + %Trim(CltState) + '  ' +
                         %Trim(CltZip);
          Else;
             CltAdrln3 = %Trim(CltCity) + ', ' + %Trim(CltState) + '  ' +
                         %Trim(CltZip5);
          Endif;

          If CltAdrLn2 = *blanks;
             CltAdrLn2 = CltAdrLn3;
             CltAdrLn3 = *blanks;
          Endif;

          CltPhArea = %editc(emarea:'X');
          CltPhNbr  = %editc(emphn#:'X');

          If CltPhNbr <> '0000000';
             If CltPhArea <> '000';
                CltPhone = CltPhArea + ' ' + %subst(CltPhNbr:1:3) + '-' +
                            %subst(CltPhNbr:4:4);
             Else;
                CltPhone =  %subst(CltPhNbr:1:3) + '-' + %subst(CltPhNbr:4:4);
             Endif;
          Else;
             CltPhone = *blanks;
          Endif;

          CltFein   = %editc(emfein:'X');

          If %Open(Wmeml);
             Close Wmeml;
          Endif;

          Return CltInfo;

        Else;

           If %Open(Wmeml);
              Close Wmeml;
           Endif;

          Return *blanks;
        Endif;
      /End-Free
     PGetCltInfo       E
