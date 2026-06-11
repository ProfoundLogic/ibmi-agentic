     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H nomain
     *=========================================================================
      * REVISIONS:
      *
      * 08/12/20 C:136 WC2549
      *                Return the physical address first.
      *
     *=========================================================================
      * Employer Master
     Fwmeml     if   e           k disk    usropn
     D/copy wprpeo
     P*=====================================================================
     P* GetCltinfo  - Get Clt name, address,fein, phone number
     P*  Parameters - Company, fund, employer, division, fund year
     P*  Returns    - 500 alpha client info or blanks if invalid.
     P*=====================================================================
     PGetCltInfo       B                   Export
     DGetCltInfo       PI           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const
     D qqemp#                         9  0 Const
     D qqdiv                          5  0 Const
     D qqfyr                          3  0 Const

      /Free
        If not %Open(Wmeml);
           Open Wmeml;
        Endif;

        dsCltinfo.Name     = *blanks;
        dsCltinfo.FedName  = *blanks;
        dsCltinfo.address1 = *blanks;
        dsCltinfo.address2 = *blanks;
        dsCltinfo.address3 = *blanks;
        dsCltinfo.City     = *blanks;
        dsCltInfo.State    = *blanks;
        dsCltInfo.Zip      = *zeros;
        dsCltInfo.Fein     = *zeros;
        dsCltInfo.PhoneAreaCode = *zeros;
        dsCltInfo.Phone#        = *zeros;
        dsCltInfo.Phone#Full    = *zeros;
        dsCltInfo.EmpLink       = *blanks;
        dsCltInfo.Filler1       = *blanks;

        Chain (qqco#:qqfnd:qqemp#:qqdiv) pem01;
        If %found;
          dsCltInfo.Name    = emname + emnam2;
          dsCltInfo.Fedname = emfdnm + emfdn2;

         //*** beg chg ***** 08/12/20 ***************
         //*** If emmpfl = 'P';
         //***    dsCltInfo.Address1  = empad1;
         //***    dsCltInfo.Address2  = empad2;
         //***    dsCltInfo.City      = empcty;
         //***    dsCltInfo.State     = empst;
         //***    dsCltInfo.Zip       = %editc(empzip:'X')
         //*** else;
         //***    dsCltInfo.Address1  = emmad1;
         //***    dsCltInfo.Address2  = emmad2;
         //***    dsCltInfo.City   = emmcty;
         //***    dsCltInfo.State  = emmst;
         //***    dsCltInfo.Zip    = %editc(emmzip:'X');
         //*** Endif;
          Select;
          When EMpad1 <> *Blanks;
             dsCltInfo.Address1  = EMpad1;
             dsCltInfo.Address2  = EMpad2;
             dsCltInfo.City      = EMpcty;
             dsCltInfo.State     = EMpst;
             dsCltInfo.Zip       = %Editc(EMpzip:'X');
          When EMmad1 <> *Blanks;
             dsCltInfo.Address1  = EMmad1;
             dsCltInfo.Address2  = EMmad2;
             dsCltInfo.City   = EMmcty;
             dsCltInfo.State  = EMmst;
             dsCltInfo.Zip    = %Editc(EMmzip:'X');
          Endsl;
         //*** end chg ***** 08/12/20 ***************

          If dsCltInfo.Zip4 <> '0000';
             dsCltInfo.Address3 = %Trim(dsCltInfo.City) + ', ' +
                                  %Trim(dsCltInfo.State) + '  ' +
                                  %Trim(dsCltInfo.Zip5) + '-' +
                                  %Trim(dsCltInfo.Zip4);
          Else;
             dsCltInfo.Address3 = %Trim(dsCltInfo.City) + ', ' +
                                  %Trim(dsCltInfo.State) + '  ' +
                                  %Trim(dsCltInfo.Zip5);
          Endif;

          If dsCltInfo.Address2 = *blanks;
             dsCltInfo.Address2 = dsCltInfo.Address3;
             dsCltInfo.Address3 = *blanks;
          Endif;

          dsCltInfo.PhoneAreaCode = %editc(emarea:'X');
          dsCltInfo.Phone#        = %editc(emphn#:'X');

          If dsCltInfo.Phone# <> '0000000';
             If dsCltInfo.PhoneAreaCode <> '000';
                dsCltInfo.Phone#Full = dsCltInfo.PhoneAreaCode + ' ' +
                                       %subst(dsCltInfo.Phone#:1:3) + '-' +
                                       %subst(dsCltInfo.Phone#:4:4);
             Else;
                dsCltInfo.Phone#Full = %subst(dsCltInfo.Phone#:1:3) + '-' +
                                       %subst(dsCltInfo.Phone#:4:4);
             Endif;
          Else;
             dsCltInfo.Phone#Full = *blanks;
          Endif;

          dsCltInfo.Fein    = %editc(emfein:'X');
          dsCltInfo.EmpLink = EMlink;

          If %Open(Wmeml);
             Close Wmeml;
          Endif;

          Return dsCltInfo;

        Else;

           If %Open(Wmeml);
              Close Wmeml;
           Endif;

          Return *blanks;
        Endif;
      /End-Free
     PGetCltInfo       E

     P*=====================================================================
     P* GetPeoinfo  - Get Peo name, address,fein, phone number
     P*  Parameters - Company, fund, employer, division
     P*  Returns    - 500 alpha Peo info or blanks if invalid.
     P*=====================================================================
     PGetPeoInfo       B                   Export
     DGetPeoInfo       PI           500
     D qqco#                          3  0 Const
     D qqfnd                          3  0 Const

      *
      * Stand Alone
      *
     D @@emp#          S              9  0 INZ(1)
     D @@div           S              5  0 INZ

      /Free
        If not %Open(Wmeml);
           Open Wmeml;
        Endif;

        dsPeoinfo.Name     = *blanks;
        dsPeoinfo.FedName  = *blanks;
        dsPeoinfo.address1 = *blanks;
        dsPeoinfo.address2 = *blanks;
        dsPeoinfo.address3 = *blanks;
        dsPeoinfo.City     = *blanks;
        dsPeoInfo.State    = *blanks;
        dsPeoInfo.Zip      = *zeros;
        dsPeoInfo.Fein     = *zeros;
        dsPeoInfo.PhoneAreaCode = *zeros;
        dsPeoInfo.Phone#        = *zeros;
        dsPeoInfo.Phone#Full    = *zeros;
        dsPeoInfo.EmpLink       = *blanks;
        dsPeoInfo.Filler1       = *blanks;

        Chain (qqco#:qqfnd:@@emp#:@@div) pem01;
        If %found;
          dsPeoInfo.Name    = emname + emnam2;
          dsPeoInfo.Fedname = emfdnm + emfdn2;

          If emmpfl = 'P';
             dsPeoInfo.Address1  = empad1;
             dsPeoInfo.Address2  = empad2;
             dsPeoInfo.City      = empcty;
             dsPeoInfo.State     = empst;
             dsPeoInfo.Zip       = %editc(empzip:'X');
          else;
             dsPeoInfo.Address1  = emmad1;
             dsPeoInfo.Address2  = emmad2;
             dsPeoInfo.City   = emmcty;
             dsPeoInfo.State  = emmst;
             dsPeoInfo.Zip    = %editc(emmzip:'X');
          Endif;

          If dsPeoInfo.Zip4 <> '0000';
             dsPeoInfo.Address3 = %Trim(dsPeoInfo.City) + ', ' +
                                  %Trim(dsPeoInfo.State) + '  ' +
                                  %Trim(dsPeoInfo.Zip5) + '-' +
                                  %Trim(dsPeoInfo.Zip4);
          Else;
             dsPeoInfo.Address3 = %Trim(dsPeoInfo.City) + ', ' +
                                  %Trim(dsPeoInfo.State) + '  ' +
                                  %Trim(dsPeoInfo.Zip5);
          Endif;

          If dsPeoInfo.Address2 = *blanks;
             dsPeoInfo.Address2 = dsPeoInfo.Address3;
             dsPeoInfo.Address3 = *blanks;
          Endif;

          dsPeoInfo.PhoneAreaCode = %editc(emarea:'X');
          dsPeoInfo.Phone#        = %editc(emphn#:'X');

          If dsPeoInfo.Phone# <> '0000000';
             If dsPeoInfo.PhoneAreaCode <> '000';
                dsPeoInfo.Phone#Full = dsPeoInfo.PhoneAreaCode + ' ' +
                                       %subst(dsPeoInfo.Phone#:1:3) + '-' +
                                       %subst(dsPeoInfo.Phone#:4:4);
             Else;
                dsPeoInfo.Phone#Full = %subst(dsPeoInfo.Phone#:1:3) + '-' +
                                       %subst(dsPeoInfo.Phone#:4:4);
             Endif;
          Else;
             dsPeoInfo.Phone#Full = *blanks;
          Endif;

          dsPeoInfo.Fein    = %editc(emfein:'X');
          dsPeoInfo.EmpLink = EMlink;

          If %Open(Wmeml);
             Close Wmeml;
          Endif;

          Return dsPeoInfo;

        Else;

           If %Open(Wmeml);
              Close Wmeml;
           Endif;

          Return *blanks;
        Endif;
      /End-Free
     PGetPeoInfo       E
