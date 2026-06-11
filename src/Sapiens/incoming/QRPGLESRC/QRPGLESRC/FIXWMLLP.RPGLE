     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/TITLE FIXWMLLP - Copy WCAB# from LLWCAB to LLWCABN
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     //==========================================================
      // Notes: Copy WCAB# from LLWCAB (10A) to LLWCABN (15A)
     //==========================================================
     // Created: 11/13/17    C:291   Project: WC2502
     //==========================================================
      // Revisions:
      //
     //==========================================================
     Fwmllp     uf   e             disk

      /Free
         Read  pll01;
         Dow Not %Eof(wmllp);
           If llWcabN = *Blanks;
              llWcabN = llWcab;
              Clear llWcab;
           Endif;
           Update pll01;
         Read  pll01;
         Enddo;

         *Inlr = *On;

      /End-free
