     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  FIXFMRCP - FIX - Update RCMRA vd R1MR80 if they are differ
     H*             This program updates the  Maximum Reimburse Amount
     H*             RCMRA if it differ from MRA 80th Perc (R1MR80)
     H*             of UCR Auxiliary File FMR1P.
     H*****************************************************************
     H*                                                               *
     H*  Called from: None                                            *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 05/21/2013 by C:277  SUPPORT: RED2317             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/25/13 C: 079 Support: MC114 82125                         *
     H*                  Correct the rental amount for HCPCS codes    *
     H*                                                               *
     H*****************************************************************
    f*  UCR Auxiliary File
     ffmr1p     if   e             disk
    f*
    f*  National Reasonable and Customary File
     ffmrcl     uf   e           k disk
     d*
     d psds          esds                  extname(szq1p)
     d*
     D*** Beg Add *** 06/25/13 *********************************
     D isFound         S              1A
     D*** End Add *** 06/25/13 *********************************
     DTimeTo6          pr             6  0
     D time                            t   Const
     D*
     DDateTo7          pr             7  0
     D date                            d   Const
     D*
     DsystemDate       pr              d
     D*
     DsystemTime       pr              t
      *
     c     fmrclky       klist
     c                   kfld                    r1co#
     c                   kfld                    r1zipa
     c                   kfld                    r1prcd
     c                   kfld                    r1efdt
      /Free
          //Program starts here
         Read  pr101;
         Dow not %eof(fmr1p);

            If r1prcd >= 'A0001' and
               r1prcd <= 'Z9999';

           Chain fmrclky prc01;
              If %found(fmrcl);

          //*** Beg Add *** 06/25/13 *********************************
                 isFound = '0';
                 If rcanam > 0;
                    rcanam = rcanam * 100;
                    isFound = '1';
                 Endif;
          //*** End Add *** 06/25/13 *********************************
                 If r1mr80 <> rcmra;
                    rcmra = r1mr80;
          //*** Beg Add *** 06/25/13 *********************************
                    isFound = '1';
                 Endif;
          //*** End Add *** 06/25/13 *********************************

          //*** Beg Add *** 06/25/13 *********************************
                 If isFound = '1';
          //*** End Add *** 06/25/13 *********************************
                    rcpgm  = Q1pgm;
                    rcuser = Q1user;
                    rctime = TimeTo6(SystemTime());
                    rcdt   = DateTo7(SystemDate());
                    update  prc01;
                 Endif;
              Endif;
            Endif;
         Read pr101;
         Enddo;

         *inlr = *on;
      /End-Free
