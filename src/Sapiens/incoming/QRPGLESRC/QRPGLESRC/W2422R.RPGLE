       CTL-OPT COPYRIGHT('(c) 2020 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no)  ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  W2422R - Bank Account Master Maintenance Pass-Through          *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program has been converted to be a pass-through for       *
       //  the new Banck Account Master File Maintenance Program.         *
       //                                                                 *
       //  Created: 08/14/20  C:180  Project:  WC2731                     *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //                                                                 *
       //******************************************************************

       //Traditional parms from old W2422R
       DCL-PI *N;
           @@KC   char(1);
           @@KL   char(1);
           @@CALL char(1);
           @@CO#  char(3);
           @@ACCT char(20);
       END-PI;

       //Files
       DCL-F wsckp disk usage(*INPUT) ;

       //Bank Account Master program prototype.
       DCL-PR WTBNKACCT extpgm;
          @@p1 char(1);
          @@p2 char(1);
          @@mode char(1);
          @@RRNA char(9);
       END-PR;

       DCL-S ##KC char(1);
       DCL-S ##KL char(1);
       DCL-S ##mode char(1);
       DCL-S ##rrna char(9) INZ('000000000');
       DCL-S ##rrn  packed(9:0);

       DCL-S ##Co#  packed(3:0);
       DCL-S ##acct char(20);

       //Load incoming parameters
       ##KC = @@KC;
       ##KL = @@KL;
       ##mode  = @@CALL;
       If %parms() > 3 and @@CO# <> *blanks;
          ##Co# = %int(@@CO#);
       Endif;

       //Bank Account Master
       WTBNKACCT(##KC:##KL:##mode:##RRNA);
       ##rrn = %int(##RRNA);

       //Outgoing parameters
       If ##rrn > *Zero;
          chain ##rrn PCK01;
          If %found(WSCKP);
             If %parms() > 3;
                @@CO# =  %editc(CKco#:'X');
             Endif;
             If %parms() > 4;
                @@ACCT = CKacct;
             Endif;
          Endif;
       Endif;
       *inlr = *On;
