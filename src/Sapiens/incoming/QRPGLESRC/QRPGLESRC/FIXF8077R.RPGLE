     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXF8077R- Correct the Payment Requests having transaction date of zero.
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 09/18/15                                         *
     h*  Programmer: C:298                                            *
     h*  Support   : 37276                                            *
     h*===============================================================*
     h*  Revisions:                                                   *
     h*                                                               *
     h*                                                               *
     h* ------------------------------------------------------------- *
     h*  NOTE: Please add comments to the top of Revisions.           *
     h* ------------------------------------------------------------- *
     h*===============================================================*
     f*
     ffaeop     uf   e             disk    rename(peo01:peo0p)
     ffaeap     uf   e             disk    rename(pea01:pea0p)
     ffaesp     uf   e             disk    rename(pes01:pes0p)
     ffaepp     uf   e             disk    rename(pep01:pep0p)
     ffdcap     uf   e             disk    rename(pca01:pca0p)
     ffddap     uf   e             disk    rename(pda01:pda0p)
     ffddep     uf   e             disk    rename(pde01:pde0p)
     ffddpp     uf   e             disk    rename(pdp01:pdp0p)
     ffmcmp     if   e           k disk    rename(pcm01:pcm0p)
     ffmcml1    if   e           k disk    rename(pcm01:pcm00)
     ffac1l2    if   e           k disk    rename(pc101:pc100)
     ffaeol31   if   e           k disk    rename(peo01:peo00)
     fwarppm    uf   e             disk    rename(prp01:prp0p)

     d*
     dpsds           esds                  extname(szQ1p)
     d*

      *
      * Main program processing

      /Free
       Read peo0p;
       Dow not %eof(faeop);
         If EOipgm = 'F8077R';
            If EOcrdt = 0;
               EOcrdt = EOidt;
            Endif;
            If EOtrdt = 0;
               EOtrdt = EOidt;
            Endif;
            If EOacdt = 0;
               Chain (EOco# :EOsub# :EOfnd :EOfyr :EOcase) pcm00;
               If %found(fmcml1);
                  EOacdt = CMacdt;
               Endif;
            Endif;
            EOpgm  = Q1pgm;
            EOtime = %Dec(%Time():*hms);
            EOdt   = %Dec(%Date():*cymd);
            Update peo0p;
         Endif;
         Read peo0p;
       Enddo;

       Read pea0p;
       Dow not %eof(faeap);
         If EAacdt = 0 and EAipgm = 'F8077R';
            Chain (EAco# :EAsub# :EAfnd :EAfyr :EAcase) pcm00;
            If %found(fmcml1);
               EAacdt = CMacdt;
               EApgm  = Q1pgm;
               EAtime = %Dec(%Time():*hms);
               EAdt   = %Dec(%Date():*cymd);
               Update pea0p;
            Endif;
         Endif;
         Read pea0p;
       Enddo;

       Read pes0p;
       Dow not %eof(faesp);
         If ESacdt = 0 and ESipgm = 'F8077R';
            Chain (ESco# :ESsub# :ESfnd :ESfyr :EScase) pcm00;
            If %found(fmcml1);
               ESacdt = CMacdt;
               ESpgm  = Q1pgm;
               EStime = %Dec(%Time():*hms);
               ESdt   = %Dec(%Date():*cymd);
               Update pes0p;
            Endif;
         Endif;
         Read Pes0p;
       Enddo;

       Read pep0p;
       Dow not %eof(faepp);
         If EPacdt = 0 and EPipgm = 'F8077R';
            Chain (EPco# :EPsub# :EPfnd :EPfyr :EPcase) pcm00;
            If %found(fmcml1);
               EPacdt = CMacdt;
               EPpgm  = Q1pgm;
               EPtime = %Dec(%Time():*hms);
               EPdt   = %Dec(%Date():*cymd);
               Update pep0p;
            Endif;
         Endif;
         Read pep0p;
       Enddo;

       Read pca0p;
       Dow not %eof(fdcap);
         If CAacdt = 0 and CAipgm = 'F8077R';
            Chain (CAco# :CAsub# :CAfnd :CAfyr :CAcase) pcm00;
            If %found(fmcml1);
               CAacdt = CMacdt;
               CApgm  = Q1pgm;
               CAtime = %Dec(%Time():*hms);
               CAdt   = %Dec(%Date():*cymd);
               Update pca0p;
            Endif;
         Endif;
         Read pca0p;
       Enddo;

       Read pda0p;
       Dow not %eof(fddap);
         If DAacdt = 0 and DAipgm = 'F8077R';
            Chain (DAco# :DAsub# :DAfnd :DAfyr :DAcase) Pcm00;
            If %found(fmcml1);
               DAacdt = CMacdt;
               DApgm  = Q1pgm;
               DAtime = %Dec(%Time():*hms);
               DAdt   = %Dec(%Date():*cymd);
               Update pda0p;
            Endif;
         Endif;
         Read pda0p;
       Enddo;

       Read pde0p;
       Dow not %eof(fddep);
         If DEacdt = 0 and DEipgm = 'F8077R';
            Chain (DEco# :DEsub# :DEfnd :DEfyr :DEcase) pcm00;
            If %found(fmcml1);
               DEacdt = CMacdt;
               DEpgm  = Q1pgm;
               DEtime = %Dec(%Time():*hms);
               DEdt   = %Dec(%Date():*cymd);
               Update pde0p;
            Endif;
         Endif;
         Read pde0p;
       Enddo;

       Read pdp0p;
       Dow not %eof(fddpp);
         If DPipgm = 'F8077R';
            If DPacdt = 0;
               Chain (DPco# :DPsub# :DPfnd :DPfyr :DPcase) pcm00;
               If %found(fmcml1);
                  DPacdt = CMacdt;
               Endif;
            Endif;
            If DPrxfd = 0;
               Chain DPeob# Pc100;
               If %found(fac1l2);
                  DPrxfd =  %dec(%char(%date(C1rxfd:*iso0):*cymd0):7:0);
               Endif;
            Endif;
            DPpgm  = Q1pgm;
            DPtime = %Dec(%Time():*hms);
            DPdt   = %Dec(%Date():*cymd);
            Update pdp0p;
         Endif;
         Read pdp0p;
       Enddo;

       Read prp0p;
       Dow not %eof(warppm);
         If RPtrdt = 0 and RPipgm = 'F8077R';
            Chain (RPco# :RPeob#) peo00;
            If %found(faeol31);
               RPtrdt = EOtrdt;
               RPpgm  = Q1pgm;
               RPtime = %Dec(%Time():*hms);
               RPdt   = %Dec(%Date():*cymd);
               Update prp0p;
            Endif;
         Endif;
         Read prp0p;
       Enddo;

       *inlr = *on;
      /End-Free

