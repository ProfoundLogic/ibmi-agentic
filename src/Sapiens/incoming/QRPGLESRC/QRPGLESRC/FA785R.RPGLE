     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H****************************************************************
     H*  FA785R - Check Rightcoding
     H*
     H*  Description: This program determines if a line hits a rightcoding
     H*               condition
     H*
     H*  Created    : 08/01/2014  Programmer: 212  Project: 114MNV
     H*
     H*  Called By  :
     H*  Calls To   :
     H*
     H**********************************************************************
     H*  Revisions:
     H**********************************************************************
     H*
     H*
     H**********************************************************************
    F*
     ffaeop     if   e             disk
     f*
     ffmcxl     if   e           k disk
     f*
     fstabl     if   e           k disk
     f*
     ffdcal5    if   e           k disk
     f*
     ffddhl     if   e           k disk
     f*
     ffddul     if   e           k disk
     f*
     ffarcl     if   e           k disk
     f*
     fftgtl     if   e           k disk
    D*
     d** Procedure definitions
     d*
     d* *entry
     d FA785R          pr                  ExtPgm('FA785R')
     d ##rrn                          9
     d ##rtncd                        1                                         MC INST
     d*
     d FA785R          pi
     d ##rrn                          9
     d ##rtncd                        1                                         MC INST
     d*
     D** Standalone variables
     D*
     d xxRrn           s              9  0 inz
     d xxKey           s                   like(stkey) inz
     d CodTyp          s              1    inz
     d indRCfnd        s               n   inz(*off)
     d xxEffIso        s              8    inz
     d xxRCEffIso      s              8    inz
    C*
      /free
        //================================================================
        // mainline
        //================================================================

        // initialization routine
         exsr srinit;

        // get bill details
         Chain(n) xxRrn peo01;
         if not %found(faeop);
           exsr pgmend;
         endif;

        // verify if group uses Procura audit process
         Chain(n) (EOco#:EOsub#:EOfnd) pcx01;
         if not %found(fmcxl);
           exsr pgmend;
         else;
           if CXpauf = *blanks;
             exsr pgmend;
           endif;
         endif;

        // verify if state uses Procura audit process
         xxKey = EOast;
         Chain(n) xxKey pst01;
         if not %found(stabl);
           exsr pgmend;
         else;
           if STpauf = *blanks;
             exsr pgmend;
           endif;
         endif;

         if EOrcn <> *zeros;
           exsr pgmend;
         endif;

         // get the bill detail
         Setll (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
         Dow not *inlr;

           ReadE(n) (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;

           if %eof(fdcal5);
             leave;
           endif;

           // determine code type
           select;

             when EObtyp = 'N';
              chain(n)
              (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:CAdseq) pdh01;
              if %found(fddhl);
                codtyp = *blanks;
                select;
                 when (dhprcd >= '00100' and dhprcd <= '01999');
                   codtyp = 'A';
                 when (dhprcd >= '10000' and dhprcd <= '99999');
                   codtyp = 'P';
                 when (dhprcd >= 'A0001' and dhprcd <= 'Z9999');
                   codtyp = 'H';
                 when (dhprcd >= '    1' and dhprcd <= '  999');
                   codtyp = 'D';
                endsl;
              endif;

             when EObtyp = 'H';
              chain(n)
              (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:CAdseq) pdu01;
              if %found(fddul);
                codtyp = *blanks;
                select;
                 when (durate >= '00100' and durate <= '01999');
                   codtyp = 'A';
                 when (durate >= '10000' and durate <= '99999');
                   codtyp = 'P';
                 when (durate >= 'A0001' and durate <= 'Z9999');
                   codtyp = 'H';
                 when (durate >= '    1' and durate <= '  999');
                   codtyp = 'D';
                endsl;
              endif;

           endsl;

           // verify if the bill line details match Righcoding setup in file
           indRCfnd = *off;
           Setgt (EOast:codtyp) prc01;
           ReadpE (EOast:codtyp) prc01;
           Dow not %eof(farcl);
             xxRCEffIso = %char(%date(rcefdt:*cymd):*iso0);
             xxEffIso = %char(%date(cafdat:*cymd):*iso0);
             if xxRCEffIso <= xxEffIso and
                cacpt4 >= rcfcod and cacpt4 <= rctcod and
                rcstat = 'A';
               indRCfnd = *on;
               leave;
             endif;

             ReadpE (EOast:codtyp) prc01;
           EndDo;
           If indRCfnd = *off;
             iter;
           Endif;

           If codtyp = 'A';

            // see if modifier is present in file
             select;

               when EObtyp = 'N';
                xxKey = DHmod1;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DHmod2;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DHmod3;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DHmod4;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;

               when EObtyp = 'H';
                xxKey = DUpmod;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DUmod2;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DUmod3;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;
                xxKey = DUmod4;
                Setll (CAco#:'ANMD':xxKey) pgt01;
                if %equal(ftgtl);
                  ##rtncd = 'R';
                  leave;
                endif;

             endsl;

            // verify hours
             if DHunts > RCaneh;
               ##rtncd = 'R';
               leave;
             endif;

           Endif;

           If RCpmtt <> *zeros;
             if CApaid > RCpmtt;
               ##rtncd = 'R';
               leave;
             endif;
           Else;
             ##rtncd = 'R';
             leave;
           Endif;

         Enddo;

         exsr pgmend;

        //==================================================================
        // end of mainline
        //==================================================================


        //================================================================
        // PgmEnd - end of progam subroutine
        //================================================================
         begsr pgmend;

           *inlr = *on;
           return;

         endsr;

        //================================================================
        // SRINIT - initialization subroutine
        //================================================================
         begsr srinit;

           ##rtncd = *blanks;

           monitor;
             xxRrn = %int(##rrn);
           on-error;
             exsr pgmend;
           endmon;

         endsr;

        //================================================================

