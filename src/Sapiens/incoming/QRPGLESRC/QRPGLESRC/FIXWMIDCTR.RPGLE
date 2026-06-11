     H* (C) Sapiens Inc, 2021
     H/TITLE FIXWMIDCTR - Fix TRWKBENAMT
     H**************************************************************************************
     H*  Created: 03/18/21     C:53     Ticket: 93297                                      *
     H**************************************************************************************
     Fwmcml1    if   e           k disk
     Fwmidctr   uf   e           k disk
     Fwmawl3    if   e           k disk
     D*------------------------------------------------------------------------------
     D Date7           S              7  0 INZ
     D intToday7       S              7  0 INZ
     D TDate8          S              8  0 INZ
     C*
     C*====================================================================
     C*  Read WMIDCTR and update TRWKBENAMT with the proper value
     C*====================================================================
     C*
      /Free
      //Today's date
       intToday7 = %dec(%date():*cymd);

             Read PTR01;
             Dow not %eof(WMIDCTR);
             If TRWkBenAmt = '000000000';
                // Translate TRTrnDate;
                TDate8 = %dec(TRTrnDate:8:0);
                Date7 = %dec(%char(%date(TDate8:*iso):*cymd0):7:0);

                Chain (TRco#:TRfnd:TRfyr:TRcase) PCM01;
                If %found(WMCML1);
                //Weekly Benefit Amount
                   Setgt (TRco#:TRfnd:TRfyr:TRcase:TRbenTyCd) PAW01;
                   Readpe (TRco#:TRfnd:TRfyr:TRcase:TRbenTyCd:Date7) PAW01;
                   If not %eof(WMAWL3);
                      TRwkBenAmt = %Editc(AWcrt:'X');
                   Else;
                      %Subst(TRwkBenAmt:3) = %Editc(CMcrt:'X');
                   Endif;
                truser = 'PDSPGMR   ';
                trpgm = 'FIXWMIDCTR';
                trdt = intToday7;
                trtime = 000000;
                Update PTR01;
                Endif;

             Endif;
             Read PTR01;
             Enddo;
       // End of pgm

        eval  *inlr = *on;
        return;

      /End-Free
