     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program will be used to update and add records to FAEAP  *
     H* based on records in FDDAP for each level of recon             *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*  03/20/13  C:079 Redmine: 2518                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     Ffddal     if   e           K disk
     Ffaeal1    uf a e           k disk
     Ffaeolg    if   e           k disk
     Ffmf1l     if   e           k disk
     FQSYSPRT   O    F  198        PRINTER OFLIND(ovrflw)
     F*
     D ovrflw          s               n
     D psds          esds                  extname(szq1p)
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*
     D kkco#           s                   like(eaco#)
     D kksub#          s                   like(easub#)
     D kkfnd           s                   like(eafnd)
     D kkfyr           s                   like(eafyr)
     D kkcase          s                   like(eacase)
     D kkeob#          s                   like(eaeob#)
     D kkrcn           s                   like(earcn)
     D sum_chrg        s                   like(eachrg)
     D sum_mra         s                   like(eamra)
     D sum_ndis        s                   like(eandis)
     D sum_othr        s                   like(eaothr)
     D sum_oth2        s                   like(eaoth2)
     D sum_paid        s                   like(eaoth2)
     D xxrcn           s                   like(earcn)
     D xxeob#          s                   like(eaeob#)
     D xxFirst         s              1A
     D xxCount         s              9  0
     D*
     D*
     D*
     C*
     C     *entry        plist
     C                   parm                    @@mode            1
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     f1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     *
     C     eaky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     *
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
      /Free
         //Main Processing
         //Read through FDDAP for each recon
           read fddal;
             exsr initialize;
             eval xxFirst = 'Y';
             eval xxCount = 0;
           Dow not %Eof(fddal);
             if xxrcn <> darcn# or xxeob# <> daeob#;
               exsr upd_faeap;
             else;
               xxrcn = darcn#;
               xxeob# = daeob#;
             endif;
             exsr sum_amounts;
             exsr init_keys;
             read fddal;
           enddo;
           if xxCount > 0;
             except Totals;
           endif;
           *inlr = *on;
           Return;
    
         //Subroutines
    
       //inititalize key fields
       Begsr init_keys;

         eval kkco# = daco#;
         eval kksub# = dasub#;
         eval kkfnd = dafnd;
         eval kkfyr = dafyr;
         eval kkcase = dacase;
         eval kkeob# = daeob#;
         eval kkrcn = darcn#;

       Endsr;
    
       //upd_faeap - Write/Update FAEAP
       Begsr upd_faeap;

       chain eaky faeal1;
       chain eoky faeolg;

         If not %found(faeal1) and %found(faeolg)
            and eoblst = 'C' and eoipgm <> 'F8042R';
           eachrg = sum_chrg;
           eamra  = sum_mra;
           eandis = sum_ndis;
           eaothr = sum_othr;
           eaoth2 = sum_oth2;
           eapaid = sum_paid;

           eval eaco#  = eoco#;
           eval easub# = eosub#;
           eval eafnd  = eofnd;
           eval eafyr  = eofyr;
           eval eacase = eocase;
           eval eaeob# = eoeob#;
           eval earcn  = xxrcn;
           eval eass#  = eoss#;
           eval eaacdt = eoacdt;
           eval easys  = eosys;
           eval eafein = eofein;
           eval eamod  = eomod;
           eval eaesdt = eowcck;
           chain f1ky pf101;
             if %found(fmf1l);
               if f1outa = 'Y';
                 eval eaplnk = '1';
               else;
                 if eopaid = 0;
                   eval eaplnk = '2';
                 else;
                   eval eaplnk = '3';
                 endif;
               endif;
             endif;
           eval eaepfl = 'E';
           if eoblst = 'X' or eoblst = 'D';
             eval eaepfl = 'X';
           endif;
           eval eabnpi = eobnpi;
           eval eaesdt = eowcck;
           eval eapnam = eopnam;
           eval eaadr1 = eoadr1;
           eval eaadr2 = eoadr2;
           eval eacty  = eocty;
           eval east   = eost;
           eval eazip  = eozip;
           eval eatax  = eotax;
           eval eatax  = eotax;
           eval eauser = q1user;
           eval eaiusr = q1user;
           eval eapgm  = q1pgm;
           eval eaipgm  = q1pgm;
           eval eastat = 'A';
           eatime  = %Dec(%Time(): *ISO);
           eadt    = %dec(%date():*CYMD);
           eaitim  = %Dec(%Time(): *ISO);
           eaidt   = %dec(%date():*CYMD);
           if @@mode = 'U';
             write pea01;
           endif;

         // Write to the report

           if xxFirst = 'Y';
             except header;
             eval xxFirst = 'N';
           endif;
           if ovrflw;
             except header;
             clear ovrflw;
           endif;
           except detail;
          eval xxCount = xxCount + 1;
         endif;

         If %found(faeal1) and %found(faeolg) and eoblst = 'C'
            and eoipgm <> 'F8042R';
           if eachrg <> sum_chrg or eamra <> sum_mra or eandis <> sum_ndis
           or eaothr <> sum_othr or eaoth2 <> sum_oth2 or
           eapaid <> sum_paid;
             eachrg = sum_chrg;
             eamra  = sum_mra;
             eandis = sum_ndis;
             eaothr = sum_othr;
             eaoth2 = sum_oth2;
             eapaid = sum_paid;
             eval eauser = q1user;
             eval eapgm  = q1pgm;
             eatime  = %Dec(%Time(): *ISO);
             eadt    = %dec(%date():*CYMD);
             if @@mode = 'U';
               update pea01;
             endif;

         // Write to the report

             if xxFirst = 'Y';
               except header;
               eval xxFirst = 'N';
             endif;
             if ovrflw;
               except header;
               clear ovrflw;
             endif;
             except detail;
             eval xxCount = xxCount + 1;
           endif;
          endif;
         // Initialize summary fields
         exsr initialize;

       Endsr;
       //initialize - Initialize fields
       Begsr initialize;
         sum_chrg = 0;
         sum_mra  = 0;
         sum_ndis = 0;
         sum_othr = 0;
         sum_oth2 = 0;
         sum_paid = 0;
         xxrcn = darcn#;
         xxeob# = daeob#;
       endsr;
       //sum_amounts - Sum up amounts in FDDAP
       Begsr sum_amounts;
         sum_chrg = sum_chrg + dachrg;
         sum_mra  = sum_mra + damra;
         sum_ndis = sum_ndis + dandis;
         sum_othr = sum_othr + daothr;
         sum_oth2 = sum_oth2 + daoth2;
         sum_paid = sum_paid + dapaid;
       Endsr;

     /End-Free
     OQSYSPRT   E            HEADER
     O                                           25 'EOB NUMBER'
     O                                           45 'ENTRY DATE'
     O          E            DETAIL
     O                       EAEOB#              25
     O                       EOIDT               45
     O          E            Totals
     O                                           25 'Total number of EOBs:'
     O                       xxCount             36
