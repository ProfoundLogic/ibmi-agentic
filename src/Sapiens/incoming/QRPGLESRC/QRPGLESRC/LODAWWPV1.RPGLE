     Hcopyright('(c) 2019 Sapiens International. All rights reserved. ')
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE LODAWWPV1  Load default PV1 code for release
     H*****************************************************************
     H*                                                               *
     H* CREATED BY:   C 180                                           *
     H* PROJECT:      WC2711            12/09/19                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     fWMAWP     uf   e             Disk

       Read PAW01;
       Dow not %eof(WMAWP);
          If AWbr2 = *blanks;
             AWbr2 = 'TT';
             Update PAW01;
          Endif;
          Read PAW01;
       Enddo;

       *inlr = *On;
