     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIXBKFLPPO - Add missing PPO Fees for Bill Review
     h BNDDIR('STBNDDIR')
     h*****************************************************************
     h* Description:  Add Missing PPO Fees                            *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     fFAU1PBKFL if   e             Disk
     ffau1l17   uf   e           k Disk
     ffaeal1    uf   e           k Disk
     ffaeol31   if   e           k Disk
     fqsysprt   o    f  132        Printer oflInd(*inOf)

     d/copy sprDate
     d*
     d CreatePPOCheck  pr                  ExtPgm('W0081PPOR')
     d  inCall                        3a
     d  inCo#                         3a
     d  inSub#                        5a
     d  inFnd                         3a
     d  inFyr                         3a
     d  inCase                        7a
     d  inEob#                        9a
     d  outRtCd                       1a
     d  inNamt                             Like(EAnAmt)
     d  inCall         s              3a
     d  inCo#          s              3a
     d  inSub#         s              5a
     d  inFnd          s              3a
     d  inFyr          s              3a
     d  inCase         s              7a
     d  inEob#         s              9a
     d  outRtCd        s              1a
     d  inNamt         s                   Like(EAnAmt)

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
     d xxDesc          s             45a
     d xxErrCnt        s              9s 0 Inz(0)
     d xxRcn           s                   Like(EArcn)
     d xxInvoice#      s                   Like(U1trId)
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
       Read pbk01;
       DoW Not %Eof(fau1pbkfl);

       // Only process if the EOB is processed and a Fee was not in the
       // original file
          xxInvoice# = %Char(BKinv#);
          Chain xxInvoice# pu101;
          If %Found(FAU1L17);
             If @@Rcn = U1rcnF;
             // If the Fee matches then skip the record
                If U1cost = BKppoFee;
                   xxErrCnt = xxErrCnt + 1;
                   xxDesc = 'Backfill Fee Matches FAU1P Fee';
                   If *InOf= *On;
                      Except Hdg;
                      *inOf = *off;
                   Endif;
                   Except Det;
                Else;
               // Update the fee in the EOB file
                   Chain (U1co#:U1eob#) peo01;
                   If %Found(faeol31);
               // Set the recon
                      xxRcn = BKrcn;

                      Chain (EOco#:EOsub#:EOfnd:EOfYr:EoCase:EOeob#:xxRcn)
                       pea01;
                      if %Found(faeal1) and EAnAmt <> BKppoFee;
                         EAePdt = 0;
                         EAePfl = ' ';
                         EAaPpF = ' ';
                         EAnAmt = BKppoFee;
                         EAnCam = BKppoFee;
                         EAbHrF = 'H';
                         EAnHrF = 'H';

                         EAuser = Q1user;
                         EApgm  = Q1pgm;
                         sys_date = %date();
                         EAdt = Dateto7(sys_date);
                         EAtime = TimeTo6(SystemTime());

                         If @@Mode = 'U';
                            Update pea01;
                            U1cost = BKppoFee;
                            Update pu101;
                            inCall = *blank;
                            inCo# = %EditC(EAco#:'X');
                            inSub# = %EditC(EAsub#:'X');
                            inFnd  = %EditC(EAfnd:'X');
                            inFyr  = %EditC(EAfyr:'X');
                            inCase = %EditC(EAcase:'X');
                            inEob# = %EditC(EAeob#:'X');
                            outRtCd = *blank;
                            inNamt = EAnAmt;
                            CreatePPOCheck(inCall:inCo#:inSub#:inFnd:inFyr
                              :inCase:inEob#:outRtCd:inNamt);
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
                         *inOf = *off;
                      Endif;
                      Except Det;
                   EndIf;
                Endif;
             Endif;
          Else;
             xxErrCnt = xxErrCnt + 1;
             xxDesc = 'Record in FAU1P does not qualify';
             If *InOf= *On;
                 Except Hdg;
                 *inOf = *off;
             Endif;
             Except Det;
          Endif;
          Read pbk01;
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
     O                                           20 'Review#'
     O                                           30 'Year'
     O                                           42 'Case'
     O                                           55 'PPO Fee'
     O                                           71 'Description'
      *
     O          E            DET         1
     O                       BKinv#              20
     O                       BKyear              30
     O                       BKcase              45
     O                       BKppoFee      J     55
     O                       xxDesc             105
     O          E            Summary     3
     O                                           22 'Total Number of Errors'
     O          E            Summary     1
     O                       xxErrCnt            22
      *
