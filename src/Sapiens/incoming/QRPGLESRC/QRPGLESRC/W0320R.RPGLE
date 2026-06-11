       CTL-OPT COPYRIGHT('(c) 2021 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no)  ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  W0320R Multiple Claim Maintenence - Reins Sys - Pass Through   *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  Pass through pgm for Mult Claim Maint - Reinsurance System     *
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
           @@PrDs char(40);
           @@Mode char(1);
       END-PI;

       //Multi-claim Maintenance program prototype.
       DCL-PR WTMLTCLM extpgm;
          @@p1 char(40);
          @@p2 char(1);
       END-PR;

       DCL-S ##PrDs char(40);
       DCL-S ##Mode char(1);


       //Load incoming parameters
       ##PrDs  = @@PrDs;
       ##Mode  = @@Mode;

       //Multi-claim Maintenance
       WTMLTCLM(##PrDs:##Mode);
       *inlr = *On;
