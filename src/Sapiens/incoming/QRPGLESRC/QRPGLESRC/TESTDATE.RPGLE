     D date            S               d   DatFmt(*usa)
     d decDate         s              8  0
     C     *ENTRY        PLIST
     C                   PARM                    @@DATE            8
      /free
       decDate = %Int(@@date);
       date = %Date(@@date:*iso0);
       *inLR = *On;
      /end-free
