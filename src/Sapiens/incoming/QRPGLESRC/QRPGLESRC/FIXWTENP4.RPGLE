     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/TITLE FIXWTENP4 - Fix Adoption date in Endorsement Form Table
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     //==========================================================
      // Notes:
     //==========================================================
     // Created: 01/27/20    C:300   Ticket: 86839
     //==========================================================
      // Revisions:
      //
     //==========================================================
     Fwtenp     uf   e             disk
      *
      /Free
       Setll 1 pen01;
       Read pen01;
       Dow Not %Eof(wtenp);
          If ENADTE > 0;
          Else;
             ENADTE = 0;
             Update pen01;
          Endif;
       Read pen01;
       Enddo;

       *Inlr = *On;

      /End-free
