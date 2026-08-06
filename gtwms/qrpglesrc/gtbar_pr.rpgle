       // ==================================================================
       //  GTBAR -- barcode identification and GS1 parsing.
       //
       //  Every scan in the application goes through gtbar_parse: the
       //  operator is never asked "are you scanning a location or an item?",
       //  the server works it out.
       //
       //  Formatting note: 7-space indent and no conditional directives,
       //  matching the existing prototype members in this repo. A copy
       //  member laid out any other way is read as fixed-format and fails
       //  with RNF0257 / RNF3304.
       // ==================================================================

       // What a scan resolved to. kind drives screen routing.
       dcl-ds gtbar_result_t qualified template;
         kind          char(6);       // ITEM / LOC / PALLET / RCPT / OPER / UNKNWN
         symbology     char(12);      // inferred from length and content
         isGs1         ind;           // carried GS1 application identifiers
         checkOk       ind;           // mod-10 check digit verified
         rawLen        int(10);
         raw           varchar(120);
         sku           char(15);
         locationId    char(10);
         sscc          char(18);
         receiptId     char(10);
         gtin          char(14);      // AI 01
         lot           char(20);      // AI 10
         expiry        char(6);       // AI 17, YYMMDD
         serial        char(20);      // AI 21
         qtyCount      packed(8 : 0); // AI 30 or 37
         descEn        varchar(60);
         descFr        varchar(60);
         packQty       packed(7 : 0);
         barLevel      char(2);       // EA / CS / PL
       end-ds;

       // Parse, classify and resolve a scanned payload.
       dcl-pr gtbar_parse;
         payload varchar(120) const;
         result  likeds(gtbar_result_t);
       end-pr;

       // GS1 mod-10 check digit test. Returns *off for anything non-numeric
       // or shorter than 8 digits.
       dcl-pr gtbar_checkOk ind;
         barcode varchar(120) const;
       end-pr;

       // Compute the mod-10 check digit for a payload without one.
       dcl-pr gtbar_checkDigit char(1);
         payload varchar(120) const;
       end-pr;
