     Hcopyright('(c) 2018 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H* Update the Scriptura Module/Setting File for Allow Inquiry
     H* Flags SCKEY3.
     H**
     H* Created: 06/20/18 Project: WC2600   C: 291
     H**
     H*****************************************************************
     H*
     Fstscp     uf   e             disk

     D psds          esds                  extname(SZQ1P)

     D/copy sPrDate

      /free
       Setll 1 psc01;
       Read psc01;
       If %found(stscp);
          SCkey3  = 'C ';
          SCUser  = Q1USER;
          SCPgm   = Q1PGM;
          SCDt    = Dateto7(SystemDate());
          SCTime  = Timeto6(SystemTime());
          Update psc01;
       Endif;

       *Inlr = *On;
      /End-free
