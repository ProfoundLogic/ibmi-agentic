     Fdata01d   cf   e             workstn
     Fbigfile   if   e             disk
         read bigfile;
           dow not %eof(bigfile);
           nbrrcd += 1;
           read bigfile;
         enddo;
         exfmt fm01;
         *inlr = *on;
         return;
