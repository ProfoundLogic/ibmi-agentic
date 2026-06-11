       CTL-OPT COPYRIGHT('(c) 2021 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no)  ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  W0320RW Multiple Claim Maintenence - Wrapper                   *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  Wrapper pgm for Mult Claim Maint -                             *
       //                                                                 *
       //  Created: 02/01/21  C:180  Project:  WC2732                     *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //                                                                 *
       //******************************************************************

       //Traditional parms from old W0320R
       DCL-PI *N;
           @@Rrn char(9);
           @@PrDs char(40);
           @@Mode char(1);
       END-PI;

       //Multi-claim Maintenance program prototype.
       DCL-PR WTMLTCLM extpgm;
          @@p1 char(40);
          @@p2 char(1);
          @@p3 packed (9:0);
       END-PR;

       DCL-S ##Rrn packed(9:0);
       DCL-S ##PrDs char(40);
       DCL-S ##Mode char(1);



       //Load incoming parameters
       ##Rrn   = %Dec(@@Rrn:9:0);
       ##PrDs  = @@PrDs;

       //Green Panel Link Filter Mode
       If @@Mode = *blanks and %subst(##PrDs:1:1) = 'F';
          ##Mode = 'F';
       Endif;

       if ##Mode = *blank;      //Inquiry Mode
          ##Mode  = '1';
       Endif;

       //Multi-claim Maintenance
       WTMLTCLM(##PrDs:##Mode:##Rrn);
       *inlr = *On;
