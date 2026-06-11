     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIXIMPDTA - Backfill missing/incorrect Bill Import data
     h*****************************************************************
     h* Description:  Backfill missing/incorrect Bill Import data     *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     f* Header files
     ffxu1p     if   e             Disk
     ffau1l6    uf   e           k Disk    Rename(pu101:pu102) Prefix(X1:2)
     ffaeol31   uf   e           k Disk
     ffaeal1    uf   e           k Disk
     ffaehl     uf   e           k Disk
     ffaeul     uf   e           k Disk
     ffaepl     uf   e           k Disk
     ffaedl     uf   e           k Disk
     f*
     f* Detail files
     ffxu2l1    if   e           k Disk
     ffau2l3    uf   e           k Disk    Rename(pu201:pu202) Prefix(X2:2)
     ffdcal24   uf   e           k Disk
     ffddal2    uf   e           k Disk
     f*
     fqsysprt   o    f  132        Printer oflInd(*inOf)

     d/copy sprDate

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
     d xxCnt           s              9s 0 Inz(0)
     d xxRcn           s                   Like(EOrcn)
     d hdOth2          s                   Like(EOoth2)
     d dtOth2          s                   Like(EOoth2)
     d isUpdFaeop      s               n
     d isUpdFau1p      s               n
     d xxRvw#          s             15a
     d xxtype          s             10a

      //================================================================
      //  Prototype definitions
      //================================================================
     d* Backfill Character Data Fields
     d BackfillChrDta  pr
     d*
     d* Backfill Numeric Data Fields
     d BackfillNumDta  pr
      //================================================================
      //  Plist definitions
      //================================================================
     c     *entry        Plist
     c                   Parm                    @@Mode            1
     c                   Parm                    @@Rcn             1
     c                   Parm                    @@Type            1

      /free
       //===================================================================
       // Main Program Logic
       //===================================================================
       *inOf = *on;

       Select;
       When @@Type = 'A';
          xxType = 'Amounts';
       When @@Type = 'N';
          xxType = 'Net Desc';
       When @@Type = 'L';
          xxType = 'Lic Num';
       When @@Type = 'R';
          xxType = 'Reas Code';
       Endsl;

       // Read through backfill header file
       Read pu101;
       DoW Not %Eof(fxu1p);
          Chain (u1trid) pu102;
          // Update various files based on character data or numeric data
          If %Found(fau1l6) and @@Rcn = U1rcnF;
             Chain (X1co#:x1EOB#) peo01;
             If %Found(faeol31);
                isUpdFaeop = *off;
                isUpdFau1p = *off;
                xxRvw# = U1trId;
                If @@Rcn = 'Y';
                   xxRcn = 1;
                Else;
                   xxRcn = 0;
                Endif;

                Chain(N) (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:xxRcn)
                       pea01;
                Select;
                // Updating Network Description or Patient Account
                // Number or License Number
                when @@Type = 'N' or @@Type = 'A' or @@Type = 'L'
                 or @@Type = 'R';
                   BackfillChrDta();
                // Updating data
                when @@Type = 'D';
                   BackfillNumDta();
                Endsl;

          // Update bill import header file and EOB file
                If isUpdFau1p = *on and @@Mode = 'U';
                   Update pu102;
                Endif;
                If isUpdFaeop = *on and @@Mode = 'U';
                   EOpgm  = Q1pgm;
                   sys_date = %date();
                   EOdt = Dateto7(sys_date);
                   EOtime = TimeTo6(SystemTime());
                   Update peo01;
                Endif;

             // Print to report
                If isUpdFaeop = *on or isUpdFau1p = *on;
                   xxCnt = xxCnt + 1;
                   If *InOf= *On;
                      Except Hdg;
                      *InOf = *off;
                   Endif;
                   Except Det;
                Endif;
             Endif;
          Endif;
          Read pu101;
       Enddo;

       If xxCnt > 0;
          Except Summary;
       Endif;

       *inLR = *On;
       Return;
     Oqsysprt   E            HDG              02
     O                                           82 'Backfill Bill Import Data'
     O                       PAGE          2   +001
      *
     O          E            HDG         3
     O                                           12 'EOB Number'
     O                                           27 'Review Number'
     o                                           38 'Type'
     O                                           54 'PPO Fee'
     O                                           67 'Recon'
     O                                           80 'Charged'
     O                                           93 'MRA Red'
     O                                          106 'NTWK Red'
     O                                          119 'Othr2 Red'
     O                                          132 'Paid'
      *
     O          E            DET         1
     O                       EAEOB#        J     12
     O                       XXRVW#              29
     O                       XXtype              44
     O                       EANAMT        J     54
     O                       EARCN         J     67
     O                       EACHRG        J     80
     O                       EAMRA         J     93
     O                       EANDIS        J    106
     O                       EAOTH2        J    119
     O                       EAPAID        J    132
     O*
     O          E            Summary     3
     O                                           22 'Total Count'
     O          E            Summary     1
     O                       xxCnt               22

       //===================================================================
       // Procedures
       //===================================================================

       //===================================================================
       // BackfillChrDta - Backfill Character Data Fields
       //===================================================================
     p BackfillChrDta  B
     d                 PI
      /free

       // Network Name (U1NDSC). Map to FAEOP/EONDSC
       // Patient Account Number (U1ACCT).  Map to Header files.
       // License Number (U1BLIC).  Map to Header files and EOB Header
       Select;
       When @@Type = 'N' and U1nDsc <> *blanks;
          isUpdFau1p = *on;
          X1nDsc = U1nDsc;
          EOnDsc = U1nDsc;
          isUpdFaeop = *on;
       When @@Type = 'A' and U1acct <> ' ' or @@Type = 'L'
        and U1bLic <> *blanks;
          Select;
          When EObtyp = 'N';
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peh01;
             If %Found(faehl);
                EHacct = U1acct;
                X1acct = U1acct;
                If U1bLic <> ' ' and @@Type = 'L';
                   EHfLic = U1bLic;
                   X1bLic = U1bLic;
                Endif;
                EHuser = Q1user;
                EHpgm  = Q1pgm;
                sys_date = %date();
                EHdt = Dateto7(sys_date);
                EHtime = TimeTo6(SystemTime());

                If @@Mode = 'U';
                   Update peh01;
                Endif;
             Endif;

          When EObtyp = 'H';
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peu01;
             If %Found(faeul);
                If U1acct <> ' ';
                   EUacct = U1acct;
                   X1acct = U1acct;
                Endif;
                If U1bLic <> ' ' and @@Type = 'L';
                   isUpdFaeop = *on;
                   EOfLic = U1bLic;
                   EUfLic = U1bLic;
                   X1bLic = U1bLic;
                Endif;
                EUuser = Q1user;
                   EUpgm  = Q1pgm;
                   sys_date = %date();
                   EUdt = Dateto7(sys_date);
                   EUtime = TimeTo6(SystemTime());

                   If @@Mode = 'U';
                      Update peu01;
                   Endif;
                Endif;

          When EObtyp = 'X';
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pep01;
             If %Found(faepl);
                If U1acct <> ' ';
                   EPacct = U1acct;
                   X1acct = U1acct;
                Endif;
                If U1bLic <> ' ' and @@Type = 'L';
                   EPfLic = U1bLic;
                   X1bLic = U1bLic;
                Endif;
                EPuser = Q1user;
                EPpgm  = Q1pgm;
                sys_date = %date();
                EPdt = Dateto7(sys_date);
                EPtime = TimeTo6(SystemTime());

                If @@Mode = 'U';
                   Update pep01;
                Endif;
                Endif;
          When EObtyp = 'D';
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) ped01;
             If %Found(faedl);
                If U1acct <> ' ';
                   EDacct = U1acct;
                   X1acct = U1acct;
                Endif;
                If U1bLic <> ' ' and @@Type = 'L';
                   EDfLic = U1bLic;
                   X1bLic = U1bLic;
                Endif;
                EDuser = Q1user;
                EDpgm  = Q1pgm;
                sys_date = %date();
                EDdt = Dateto7(sys_date);
                EDtime = TimeTo6(SystemTime());

                If @@Mode = 'U';
                   Update ped01;
                Endif;
             Endif;
          Endsl;
       Endsl;

       // Nurse Notes (U1BACD). Map to FAU1P/X1BACD
       If U1bAcd <> ' ' and @@Type = 'R';
          X1bAcd = U1bAcd;
          isUpdFau1p = *on;
       Endif;

      /end-free
     p BackfillChrDta  E
       //===================================================================
       // BackfillNumDta - Backfill Numeric Data Fields
       //===================================================================
     p BackfillNumDta  B
     d                 PI
      /free

       // PPO Fees (U1COST).  Map to FAEAP/EANAMT for recon level. Other 2
       // map to other 2 fields.
       hdOth2 = %Dec(U1oth2:11:0)/100;

       If U1cost <> 0 or hdoth2 <> 0;
             isUpdFau1p = *on;
             isUpdFaeop = *on;

             // PPO Fees (U1COST).  Map to FAEAP/EANAMT for recon level.
             If U1cost <> 0;
                X1cost = U1cost;
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:xxRcn) pea01;
                If %Found(faeal1);
                   EAnAmt = U1cost;
                   EAuser = Q1user;
                   EApgm  = Q1pgm;
                   sys_date = %date();
                   EAdt = Dateto7(sys_date);
                   EAtime = TimeTo6(SystemTime());
                   If @@Mode = 'U';
                      Update pea01;
                   Endif;
                Endif;
             Endif;

             // Other 2 amount.  Map to Other 2 fields in FAEAP,FDDAP,FDCAP
             If hdoth2 <> 0;
                X1oth2 = U1oth2;

             // Faeop
             // Subtract other 2 from MRA
                If @@Rcn <> 'Y';
                   EOoth2 = hdOth2;
                Else;
                   EOoth2 = EOoth2 + hdOth2;
                Endif;
                EOmra = EOmra - hdOth2;
                EOpgm  = Q1pgm;
                sys_date = %date();
                EOdt = Dateto7(sys_date);
                EOtime = TimeTo6(SystemTime());

             // Faeap
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:xxRcn) pea01;
                If %Found(faeal1);
                   EAoth2 = hdOth2;
                // Subtract from MRA field
                   EAmra  = EAmra - hdOth2;
                   EAuser = Q1user;
                   EApgm  = Q1pgm;
                   sys_date = %date();
                   EAdt = Dateto7(sys_date);
                   EAtime = TimeTo6(SystemTime());
                   If @@Mode = 'U';
                      Update pea01;
                   Endif;
                Endif;

             // Read from detail backfill file and update amounts

                Setll (u1Trid) pu201;
                Reade (u1Trid) pu201;
                Dow Not %Eof(fxu2l1);
                   dtOth2 = %Dec(U2oth2:11:0)/100;

             // Read from detail production file and update amount
                   Chain (u2Trid:U2lin#) pu202;
                   If %found(fau2l3);
                      x2Oth2 = U2oth2;
                      If @@Mode = 'U';
                         Update pu202;
                      Endif;
                   Endif;

             // Read from detail production amount file and update amount
                   Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:x2Dseq:
                          xxRcn) pda01;
                   If %found(fddal2);
                      DAoth2 = dtOth2;
                   // Subtract from MRA
                      DAmra  = DAmra - dtOth2;
                      DAuser = Q1user;
                      DApgm  = Q1pgm;
                      sys_date = %date();
                      DAdt = Dateto7(sys_date);
                      DAtime = TimeTo6(SystemTime());
                      If @@Mode = 'U';
                         Update pda01;
                      Endif;
                   Endif;
             // Update the Detail Charge file
                   Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#:x2Dseq)
                          pca01;
                   If %Found(fdcal24);
                      If xxRcn = 0;
                         CAoth2 = dtOth2;
                      Else;
                         CAoth2 = dtOth2;
                      Endif;
                      CAmra = CAmra - dtOth2;

                      CAuser = Q1user;
                      CApgm  = Q1pgm;
                      sys_date = %date();
                      CAdt = Dateto7(sys_date);
                      CAtime = TimeTo6(SystemTime());
                      If @@Mode = 'U';
                         Update pca01;
                      Endif;
                   Endif;
                   Reade (u1Trid) pu201;
                Enddo;
             Endif;
       Endif;

      /end-free
     p BackfillNumDta  E

      /end-free
      *
