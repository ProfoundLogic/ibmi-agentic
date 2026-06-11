     hcopyright('(c) 2019 Sapiens, Inc. All rights reserved.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIX87234 - Back out duplicate recons
     h*****************************************************************
     h* Description:  Back out duplicate recons from backfill file    *
     h*               that was placed in production by client mistake *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     f* Header files
     ffau1l6    uf   e           k Disk
     ffaeol31   uf   e           k Disk
     ffaeal1    uf   e           k Disk
     f*
     f* Detail files
     ffdcal24   uf   e           k Disk
     ffddal4    uf   e           k Disk
     f*
     fqsysprt   o    f  132        Printer oflInd(*inOf)

     d/copy sprDate

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
     d xxCnt           s              9s 0 Inz(0)
     d xxNamt          s                   Like(EANAMT)
     d xxRcn           s                   Like(EARCN)
     d xxChrg          s                   Like(EACHRG)
     d xxMra           s                   Like(EAMRA)
     d xxNdis          s                   Like(EANDIS)
     d xxOth2          s                   Like(EAOTH2)
     d xxPaid          s                   Like(EAPAID)
      //================================================================
      //  Prototype definitions
      //================================================================
      //================================================================
      //  Plist definitions
      //================================================================
     c     *entry        Plist
     c                   Parm                    @@Mode            1

      /free
       //===================================================================
       // Main Program Logic
       //===================================================================
       *inOf = *on;

       // Read through header file for member B202003000
       Read pu101;
       DoW Not %Eof(fau1l6);
          If U1prFl = 'Y' and U1rcnF = 'Y' and U1mbr# = 'B202003000';
          // Remove the most recent recon for this EOB in header files
             Chain (U1co#:U1EOB#) peo01;
             If %Found(faeol31);
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:EORcn)
                       pea01;
                If %Found(faeal1);
                   EOchrg = EOchrg - EAchrg;
                   EOmra  = EOmra  - EAmra;
                   EOnDis = EOnDis - EAnDis;
                   EOoth2 = EOoth2 - EAoth2;
                   EOpaid = EOpaid - EApaid;
                   EOrcn  = EOrcn  - 1;
                   If @@mode = 'U';
                      EOpgm  = Q1pgm;
                      sys_date = %date();
                      EOdt = Dateto7(sys_date);
                      EOtime = TimeTo6(SystemTime());
                      Update peo01;
                   Endif;
          // Store the amounts from FAEAP
                   xxNamt = EAnAmt;
                   xxRcn  = EArcn;
                   xxChrg = EAchrg;
                   xxMra  = EAmra;
                   xxnDis = EAnDis;
                   xxOth2 = EAoth2;
                   xxPaid = EApaid;

                   If @@mode = 'U';
                      Delete pea01;
                   Endif;
                Endif;
          // Update Detail files FDCAP and FDDAP
                Setll (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:EOrcn)
                       pda01;
                Reade (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:EOrcn)
                       pda01;
                Dow Not %Eof(fddal4);
                   Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:DAdseq)
                          pca01;
                   If %Found(fdcal24);
                      CAchrg = CAchrg - DAchrg;
                      CAmra  = CAmra  - DAmra;
                      CAnDis = CAnDis - DAnDis;
                      CAoth2 = CAoth2 - DAoth2;
                      CApaid = CApaid - DApaid;
                      CAuser = Q1user;
                      CApgm  = Q1pgm;
                      sys_date = %date();
                      CAdt = Dateto7(sys_date);
                      CAtime = TimeTo6(SystemTime());
                      If @@Mode = 'U';
                         Update pca01;
                      Endif;
                   Endif;
                   If @@Mode = 'U';
                      Delete pda01;
                   Endif;
                   Reade (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:EOrcn)
                          pda01;
                Enddo;
                // Print to report
                xxCnt = xxCnt + 1;
                If *InOf= *On;
                   Except Hdg;
                  *InOf = *off;
                Endif;
                Except Det;
             Endif;
          Endif;

          Read pu101;
       Enddo;

       If xxCnt > 0;
          Except Summary;
       Endif;

       *inLR = *On;
       Return;

      /End-free

     Oqsysprt   E            HDG              02
     O                                           82 'Remove Duplicate Recons'
     O                       PAGE          2   +001
      *
     O          E            HDG         3
     O                                           12 'EOB Number'
     O                                           29 'Review Number'
     O                                           44 'PPO Fee'
     O                                           52 'Recon'
     O                                           67 'Charged'
     O                                           82 'MRA Red'
     O                                           97 'NTWK Red'
     O                                          112 'Othr2 Red'
     O                                          127 'Paid'
      *
     O          E            DET         1
     O                       U1EOB#        J     12
     O                       U1TRID              29
     O                       XXNAMT        J     44
     O                       XXRCN         J     52
     O                       XXCHRG        J     67
     O                       XXMRA         J     82
     O                       XXNDIS        J     97
     O                       XXOTH2        J    112
     O                       XXPAID        J    127
     O*
     O          E            Summary     3
     O                                           22 'Total Count'
     O          E            Summary     1
     O                       xxCnt               22
