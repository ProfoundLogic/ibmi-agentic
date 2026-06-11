     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
      //
     h/TITLE FIXIDCBNAM - Fix TRBENOFAMT if TRBENOFCD = 2 or 3
     h dftactgrp(*no) actgrp(*caller)
      *
      * CREATED:
      * 12/01/25 C:053  Support 120089
      *                 Fix TRBENOFAMT
      *
      * DESCRIPTION :  Fix TRBENOFAMT if TRBENOFCD = 2 or 3
      *
      * REVISIONS:
      * 00/00/00 C:000  Support 000000
      *
     FWMIDCTRL1 UF   E           K DISK
     FWMBNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *Entry
     dFIXIDCBNAM       pr
     d eUpdt                          1
     d inBchId                       14
     dFIXIDCBNAM       pi
     d eUpdt                          1
     d inBchId                       14

      * Stand Alone
     D D7trndate       S              7  0 INZ
     D NewAmt          S             11A   INZ
     D OldAmt          S             11A   INZ
     D strBchId        S             14A   inz
     D intRecCnt       S              7  0 INZ

      /Free
       strBchId = inBchId;
       Setll strBchId PTR01;
       Reade strBchId PTR01;
       Except HDG;
       Dow not %eof(WMIDCTRL1);
          If TRbchid <> '00000000000000';
             If TRbenofcd = '2' and TRbenOfAmt = '00000000000'
                or TRbenofcd = '3' and TRbenOfAmt = '00000000000';
                OldAmt  = TRbenOfAmt;
                NewAmt  = TRbenOfAmt = '00000000000';

                //Benefit Offset records
                Setll (TRco#:TRfnd:TRfyr:TRcase) PBN01;
                If %equal(WMBNL);
                   Reade (TRco#:TRfnd:TRfyr:TRcase) PBN01;
                   Dow not %eof(WMBNL);

                      If BNstat = 'A';
       D7TrnDate = %dec(%char(%date(TRtrnDate: *ISO0): *CYMD0): 7: 0);
                         If (D7trndate >= BNstdt) and  //Within Range
                            (D7trndate <= BNendt or BNendt = *Zeros);

                //Benefit Offset Amount
                            %Subst(TRbenOfAmt:3) = %Editc(BNamt:'X');
                            NewAmt  = TRbenOfAmt;
                            Leave;
                         Endif;
                      Endif;

                     Reade (TRco#:TRfnd:TRfyr:TRcase) PBN01;
                   Enddo;
                Endif;

                Exsr FixCase;
             Endif;
          Endif;
          Reade strBchId PTR01;
       Enddo;

       If intRecCnt > *Zero;
          If *InOf = *On;
             Except Hdg;
          Endif;
          Except Tot;
       Endif;

       *inlr = *on;

       //*--------------------------------------------------------------
       //* Fix IDC data for specific case
       //*--------------------------------------------------------------
       Begsr FixCase;

          If NewAmt <> '00000000000';
             Exsr Print;
             intRecCnt = intRecCnt + 1;
          Endif;

          If eUpdt = 'Y' and NewAmt <> '00000000000';
             Update PTR01;
          Endif;
       Endsr;

       Begsr Print;

          If *InOf = *On;
             Except Hdg;
          Endif;

          Except Det;
       Endsr;
      /End-Free
     OQSYSPRT   E            HDG            2 02
     O                                              'PGM:FIXIDCBNAM'
     O                                           74 'FIX BEN OFFSET AMT'
     O          E            HDG         1
     O                                           62 'BATCH ID'
     O                       TRBCHID          +0001
     O          E            HDG         2
     O                                              'CLAIM NUMBER'
     O                                           39 'OLD AMOUNT'
     O                                         +005 'NEW AMOUNT'
     O          E            DET         1
     O                       TRCO#
     O                                              '-'
     O                       TRFND
     O                                              '-'
     O                       TRFYR
     O                                              '-'
     O                       TRCASE
     O                       OldAmt              39
     O                       NewAmt            +004
     O
      *
     O          E            TOT         2
     O                                              'Total records processed:'
     O                       intRecCnt     4   +001
