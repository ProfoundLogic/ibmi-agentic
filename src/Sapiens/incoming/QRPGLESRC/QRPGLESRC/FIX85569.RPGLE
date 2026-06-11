     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     h/define CONVERT
     h/Title FIX85569 - Fix Mitchell Bill Import Header Amounts.
     h*****************************************************************
     h* Description:  Fix Mitchell Bill Import Header Amounts         *
     h*                                                               *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     ffxu1pfix  if   e             Disk
     ffau1l6    uf   e           k Disk
     ffaeal1    uf   e           k Disk
     ffaeol31   uf   e           k Disk
     fqsysprt   o    f  132        Printer oflInd(*inOf)

     d/copy sprDate

     d psds          esds                  ExtName(SZQ1P)
     d sys_date        s               D   datfmt(*USA)
     d xxCnt           s              9s 0 Inz(0)
     d xxAmt           s             11s 2
     d xxAmt1          s             10s 2
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

       // Read through the Mitchell Bill Import fix file to fix the header
       // amounts
       Read px101;
       DoW Not %Eof(fxu1pFix);

       // Update Bill Import Header FAU1P
          Chain X1trId pu101;
          If %Found(fau1l6) and U1prFl = 'Y' and U1eob# <> 0;
             // Get rid of the decimal point
             xxAmt  = X1chrg * 100;
             xxAmt1 = X1chrg * 100;
             If xxAmt < 0;
                U1chrg = '-' + %Editc(-xxAmt1:'X');
             Else;
                U1chrg = %Editc(xxAmt:'X');
             Endif;

             xxAmt  = X1nDis * 100;
             xxAmt1 = X1nDis * 100;
             If xxAmt < 0;
                U1nDis = '-' + %Editc(-xxAmt1:'X');
             Else;
                U1nDis = %Editc(xxAmt:'X');
             Endif;

             xxAmt  = X1mra * 100;
             xxAmt1 = X1mra * 100;
             If xxAmt < 0;
                U1mra  = '-' + %Editc(-xxAmt1:'X');
             Else;
                U1mra  = %Editc(xxAmt:'X');
             Endif;

             xxAmt  = X1oth2 * 100;
             xxAmt1 = X1oth2 * 100;
             If xxAmt < 0;
                U1oth2 = '-' + %Editc(-xxAmt1:'X');
             Else;
                U1oth2 = %Editc(xxAmt:'X');
             Endif;

             xxAmt  = X1paid * 100;
             xxAmt1 = X1paid * 100;
             If xxAmt < 0;
                U1paid = '-' + %Editc(-xxAmt1:'X');
             Else;
                U1paid = %Editc(xxAmt:'X');
             Endif;

             If @@Mode = 'U';
                Update pu101;
             Endif;

            // Update EOB Header FAEOP
             Chain (U1co#:U1eob#) peo01;
             If %Found(faeol31) and U1prFl = 'Y' and U1eob# <> 0;
                EOchrg = X1chrg;
                EOnDis = X1nDis;
                EOmra  = X1mra;
                EOoth2 = X1oth2;
                EOpaid = X1paid;

                If @@Mode = 'U';
                   EOuser = Q1user;
                   EOpgm  = Q1pgm;
                   sys_date = %date();
                   EOdt = Dateto7(sys_date);
                   EOtime = TimeTo6(SystemTime());
                   Update peo01;
                Endif;

               // Update EOB Header Amounts file FAEAP
                Chain (EOco#:EOsub#:EOfnd:EOfYr:EOCase:EOeob#) pea01;
                if %Found(faeal1);
                   EAchrg = X1chrg;
                   EAnDis = X1nDis;
                   EAmra  = X1mra;
                   EAoth2 = X1oth2;
                   EApaid = X1paid;

                   If @@Mode = 'U';
                      EAuser = Q1user;
                      EApgm  = Q1pgm;
                      sys_date = %date();
                      EAdt = Dateto7(sys_date);
                      EAtime = TimeTo6(SystemTime());
                      Update pea01;
                   Endif;
                Endif;
             xxCnt = xxCnt + 1;
             If *InOf= *On;
                 Except Hdg;
                 *inOf = *off;
             Endif;
             Except Det;
             Endif;
          EndIf;
          Read px101;
       EndDo;
       If xxCnt > 0;
          Except Summary;
       Endif;

       *inLR = *On;
      /end-free
     Oqsysprt   E            HDG              02
     O                                           82 'Fix Mitchell Header File'
     O                       PAGE          2   +001
      *
     O          E            HDG         3
     O                                            8 'Trans ID'
     O                                           40 'Charges'
     O                                           55 'Network'
     O                                           70 'MRA'
     O                                           85 'Other 2'
     O                                          100 'Paid'
      *
     O          E            DET         1
     O                       U1trid              15
     O                       U1chrg              40
     O                       U1nDis              55
     O                       U1mra               70
     O                       U1oth2              85
     O                       U1Paid             100
     O          E            Summary     3
     O                                           22 'Total Number of Bills'
     O          E            Summary     1
     O                       xxCnt               22
      *
