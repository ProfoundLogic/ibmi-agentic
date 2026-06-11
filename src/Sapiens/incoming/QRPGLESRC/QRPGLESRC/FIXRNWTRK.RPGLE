     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/Title FixRnwTrk - Fix Renewal Tracking
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
      * Reads through WDELP, if ELnwrw = 'N' it deletes the renewal tracking records
     fWdell1    if   e           k disk
     fWdrtl     uf   e           k disk
     fFixRnwTrk uf a e           k disk    rename(prt01:saveFile)

     c     *entry        plist
     c                   parm                    flgUpdate         1

      /free
       // Clear FixRnwTrk
       Read saveFile;
       Dow not(%eof);
       Delete saveFile;
       Read saveFile;
       Enddo;

       Read pel01;
       Dow not(%eof);
         If ELnwrw = 'N';
            Setll (ELco#:ELfnd:ELemp#:ELfyr) prt01;
            Reade (ELco#:ELfnd:ELemp#:ELfyr) prt01;
            Dow not(%eof);
               Write saveFile;
               If flgUpdate = 'Y';
                  Delete prt01;
               Endif;
            Reade (ELco#:ELfnd:ELemp#:ELfyr) prt01;
            Enddo;
         Endif;
       Setgt (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       Read pel01;
       Enddo;
       *Inlr = *On;
       Return;
      /end-free
