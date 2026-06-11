     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      *================================================================
     H* Update Transaction Date and Time in WIPSPHIST
     H*
     H* Project : WC2920 C:291  Date: 09/12/23.
     H*
     H* Called from: SYPOSTPGM2
     H* Calls to:    None
     H*
     H*================================================================
     H* Revisions:
     H*
     H*****************************************************************
     Fwipsphist uf   e             disk    rename(pps01:ppsh01) Prefix(PSH:2)

     D intIdt          s              7  0
     D intItim         s              6  0
     D psds          esds                  extname(szq1p)

     D/copy sprdate

      /Free
       Exec SQL
        Set option commit = *none;

        Read wipsphist;
        Dow not %eof(wipsphist);
           If PSHTrDt = 0;
              Exec SQL
               Select psidt, psitim desc
                Into :intIdt, :intItim
                From wipsp
                Where PSco# = :PSHco# and PSfnd = :PSHfnd and PSemp# = :PSHemp#
                Limit 1;
              PSHTrDt = intIdt;
              PSHTrTm = intItim;
              PSHTrDtDsp = %Date(DtMDYY(intIdt):*usa);
              PSHuser  = q1user;
              PSHpgm   = q1pgm;
              PSHtime  = %Dec(%Time():*hms);
              PSHdt    = %Dec(%Date():*cymd);
              Update ppsh01;
           Endif;
           Read wipsphist;
        Enddo;

       *Inlr = *On;
      /End-free
