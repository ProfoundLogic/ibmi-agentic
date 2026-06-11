     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE CVTWMECP - Copy Employer Contact Info from Quote to Live.
     H*****************************************************************
     H* DESCRIPTION:
     H*   Copy all policy contacts in quote to live for new business quotes.
     H*
     H*****************************************************************
      *
     FWDELLQ    IF   E           K DISK    rename(pel01:pel0q)
     FWMECP     O  A E             DISK    rename(pec01:pec0p)
     FWMECLQ    IF   E           K DISK    rename(pec01:pec0q)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
       /free
        Setll (000) pel0q;
        Read pel0q;

        //* Read each quote employer detail record and process is not renewal to quote.
        Dow not %eof;

           If ELRQFL <> 'Y';

              Setll (ELCo#:ELFnd:ELEmp#:ELDiv) pec0q;
              Reade (ELCo#:ELFnd:ELEmp#:ELDiv) pec0q;

           //* Copy each contact to live file.
              Dow not %eof;

                 ECPgm = Q1Pgm;
                 Write pec0p;

                 Reade (ELCo#:ELFnd:ELEmp#:ELDiv) pec0q;
              Enddo;

           Endif;

           Read pel0q;

        Enddo;

        *inlr = *on;
       /end-free
      *
