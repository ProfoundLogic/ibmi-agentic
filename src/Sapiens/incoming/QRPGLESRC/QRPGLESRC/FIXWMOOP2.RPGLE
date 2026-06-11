     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h option(*nodebugio:*srcstmt) bnddir('STBNDDIR')
     h dftactgrp(*no)
      //********************************************************************
      /TITLE FIXWMOOP2 - Fix Duplicate Sequence# Occurrence In WMOOP File.
      //********************************************************************
      //********************************************************************
      // Created: 05/18/23    C:344   Support:
      //********************************************************************
      // Revisions:
      //
      //********************************************************************
     fwmool1    uf   e           k Disk    rename(poo01:poo00)
       Dcl-s @seq like(OOseq#) inz;

       Setll *loval poo00;
       Read poo00;
       Dow not %eof(wmool1);
          clear @seq;

          Dow not %eof(wmool1);
             @seq += 1;
             OOseq# = @seq;
             update poo00;
             Reade (OOco#:OOfnd:OOemp#:OOdiv) poo00;
          Enddo;

       Setgt (OOco#:OOfnd:OOemp#:OOdiv) poo00;
       Read poo00;
       Enddo;
       *inlr = *on;
