     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwmawl1    if a e           k Disk

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     d/copy sPrDate

      /free
       SetLL 1 pcm0p;
       Read  pcm0p;
       DoW Not %Eof(wmcmp);
          // Does a history record exist?
          Chain (CMco#:CMfnd:CMfYr:CMcase) paw01;
          If Not %Found(wmawl1) And CMltFl = 'I';
             Clear paw01;
             AWco#  = CMco#;
             AWfnd  = CMfnd;
             AWfyr  = CMfyr;
             AWcase = CMcase;
             AWtrDt = CMiDt;
             AWaww  = CMaww;
             AWcrt  = CMcrt;
             AWefDt = 0;
             AWstat = 'A';
             AWuser = q1User;
             AWpgm  = q1Pgm;
             AWtime = TimeTo6(SystemTime());
             AWdt   = DateTo7(SystemDate());
             Write paw01;
          EndIf;
          Read  pcm0p;
       EndDo;

       *inLR = *On;
       Return;
      /end-free
