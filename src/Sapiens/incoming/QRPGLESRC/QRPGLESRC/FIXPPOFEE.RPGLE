     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIXPPOFEE - Add missing PPO Fees for Bill Review
     h*****************************************************************
     h* Description:  Add Missing PPO Fees to FAU1P and FAEAP         *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     ffxppop    if   e             Disk
     ffau1l6    uf   e           k Disk
     ffaeal1    uf   e           k Disk
     ffaeol31   if   e           k Disk
     fqsysprt   o    f  132        Printer oflInd(*inOf)

     d/copy sprDate

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
     d xxDesc          s             80a
     d xxErrCnt        s              9s 0 Inz(0)
     d xxRcn           s                   Like(EArcn)
      //================================================================
      //  Plist definitions
      //================================================================
     c     *entry        Plist
     c                   Parm                    @@Mode            1
     c                   Parm                    @@Rcn             1

      /free
       //===================================================================
       // Main Program Logic
       //===================================================================
       *inOf = *on;
       Read ppo01;
       DoW Not %Eof(fxppop);

       // Only process if the EOB is processed and a Fee was not in the
       // original file
          Chain POrvw# pu101;
          If %Found(fau1l6) and U1prFl = 'Y' and U1eob# <> 0 and U1cost = 0
           and U1ntwk = 'S1MED';
           If @@Rcn = U1rcnF;
             U1cost = POfee;
             If @@Mode = 'U';
                Update pu101;
             Endif;

       // Update the fee in the EOB file
             Chain (U1co#:U1eob#) peo01;
             If %Found(faeol31);
       // Set the recon
                If U1rcnF = 'Y';
                   xxRcn = 1;
                Else;
                   xxRcn = 0;
                Endif;

                Chain (U1co#:U1sub#:U1fnd:U1fYr:U1Case:U1eob#:xxRcn) pea01;
                if %Found(faeal1) and EAnAmt = 0;
                   EAePdt = 0;
                   EAePfl = ' ';
                   EAaPpF = ' ';
                   EAnAmt = U1cost;
                   EAnCam = U1cost;
                   EAbHrF = 'H';
                   EAnHrF = 'H';

                   EAuser = Q1user;
                   EApgm  = Q1pgm;
                   sys_date = %date();
                   EAdt = Dateto7(sys_date);
                   EAtime = TimeTo6(SystemTime());

                   If @@Mode = 'U';
                      Update pea01;
                   Endif;
                Else;
                   xxErrCnt = xxErrCnt + 1;
                   xxDesc = 'FAEAP Recon not found or already has fee';
                   If *InOf= *On;
                       Except Hdg;
                   Endif;
                   Except Det;
                EndIf;
             Else;
                xxErrCnt = xxErrCnt + 1;
                xxDesc = 'Record not in FAEOP';
                If *InOf= *On;
                    Except Hdg;
                Endif;
                Except Det;
             EndIf;
           EndIf;
          Else;
             xxErrCnt = xxErrCnt + 1;
             xxDesc = 'Record in FAU1P does not qualify';
             If *InOf= *On;
                 Except Hdg;
                 *inOf = *off;
             Endif;
             Except Det;
          EndIf;
          Read ppo01;
       EndDo;
       If xxErrCnt > 0;
          Except Summary;
       Endif;

       *inLR = *On;
      /end-free
     Oqsysprt   E            HDG              02
     O                                           82 'Report for Bill Import Fee'
     O                       PAGE          2   +001
      *
     O          E            HDG         3
     O                                            8 'Review#'
     O                                           25 'Claim Number'
     O                                           40 'PPO Fee'
     O                                           60 'Description'
      *
     O          E            DET         1
     O                       POrvw#              15
     O                       POclm#              25
     O                       POfee         J     40
     O                       xxDesc             132
     O          E            Summary     3
     O                                           22 'Total Number of Errors'
     O          E            Summary     1
     O                       xxErrCnt            22
      *
