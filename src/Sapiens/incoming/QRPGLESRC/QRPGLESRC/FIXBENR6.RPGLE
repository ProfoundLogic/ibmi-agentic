     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
      //
     h/TITLE FIXBENR6 -  Fix TRWKBNAMT = .66 OR 1.00
     h dftactgrp(*no) actgrp(*caller)
      *
      * DESCRIPTION :  Fix IDC transactional file records that have
      *                .66 or 1.00 in the IDC weekly benefit amount.
      **********************************************************************
      * REVISIONS:
      *
      * 02/03/22 C:079  Support 146/100715
      *                 Change criteria to 0
      *
      * 04/09/21 C:180  Support 95350
      *                 Print Batch Id on report headings.
      *
      **********************************************************************
      *
      *
     FWMIDCTRL1 UF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWMAWL3    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWTABL     IF   E           K DISK    ignore(pat01)
     F                                     ignore(pba01)
     F                                     ignore(pbc01)
     F                                     ignore(pb701)
     F                                     ignore(pca01)
     F                                     ignore(pcc01)
     F                                     ignore(pcn01)
     F                                     ignore(pcs01)
     F                                     ignore(phy01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     F                                     ignore(plr01)
     F                                     ignore(pna01)
     F                                     ignore(pnc01)
     F                                     ignore(ppa01)
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(prs01)
     F                                     ignore(psp01)
     F                                     ignore(psy01)
     F                                     ignore(ptr01)
     F                                     ignore(ptt01)
     F                                     ignore(pvs01)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *Entry
     dFIXBENR6         pr
     d eUpdt                          1
     d inBchId                       14
     dFIXBENR6         pi
     d eUpdt                          1
     d inBchId                       14

      * Stand Alone
     D kkBR2           S             10A   inz
     D kkTrDt          S                   like(PAtrdt) inz
     D kkFrDt          S                   like(PAfrdt) inz
     D kkToDt          S                   like(PAtodt) inz
     D kkTrnAmt        S                   like(TRTRNAMT)
     D x2Amt           S              9  2 INZ
     D CompRate        S              9  2 INZ
     D OldRate         S              9  2 INZ
     D Adjuster        S             30A   inz
     D strBchId        S             14A   inz
     D intRecCnt       S              7  0 INZ

      /Free
       strBchId = inBchId;
       Setll strBchId PTR01;
       Reade strBchId PTR01;
       Except HDG;
       Dow not %eof(WMIDCTRL1);
          If TRbchid <> '00000000000000';
             If TRWkBenAmt = '000000000' and TRlmpSmInd <> 'Y';
                Chain (TRco#:TRfnd:TRfyr:TRcase) PCM01;
                Chain CMclEx pup01;
                If %Found(wmupl1);
                   Adjuster=UPusNm;
                Endif;
                OldRate = %int(TrWkBenAmt)/100;
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
        kkTrDt =  %dec(%char(%date(TRtrndate:*ISO0):*cymd0):7:0);
        kkFrDt =  %dec(%char(%date(TRtrnfrdt:*ISO0):*cymd0):7:0);
        kkToDt =  %dec(%char(%date(TRtrntodt:*ISO0):*cymd0):7:0);

        Setll (TRco#:TRfnd:TRfyr:TRcase:kkTrDT) PPA01;
        Reade (TRco#:TRfnd:TRfyr:TRcase:kkTrDT) PPA01;

             Dow not %eof(WDPAL5);
                If PAretp = 'C' and kkFrDt = PAfrdt and kkToDt = PAtodt;
                   kkTrnAmt = *Zeros;

                   If PAtype = 'R' or PAtype ='S' or PAtype = 'V';
                      PAamt = PAamt * -1;
                   Endif;

                   If PAamt >= *Zeros;
                      %Subst(kkTrnAmt:4) = %Editc(PAamt:'X');
                   Else;
                      x2Amt = PAamt * -1;
                      %Subst(kkTrnAmt:4) = %Editc(x2Amt:'X');
                      %subst(kkTrnAmt:1:1) = '-';
                   Endif;

                   If kkTrnAmt = TRtrnAmt;
                     kkBR2 = PAbr2;
                     Chain (TRco#:kkBR2) PB201;
                     If %found(WTABL);
                        Exsr FixWkBenAmt;
                        Leave;
                     Endif;
                   Endif;
                Endif;

                Reade (TRco#:TRfnd:TRfyr:TRcase:kkTrDT) PPA01;
             Enddo;
       Endsr;

       //*--------------------------------------------------------------
       //* Fix IDC weekly Ben amt
       //*--------------------------------------------------------------
          Begsr FixWkBenAmt;
          // Fix Weekly Benefit Amount
          CompRate = *Zeros;
          Setgt (TRco#:TRfnd:TRfyr:TRcase:kkBr2:kktrdt) PAW01;
          Readpe (TRco#:TRfnd:TRfyr:TRcase:kkBr2) PAW01;
          If not %eof(WMAWL3) and AWcrt > *Zero;
             TRwkBenAmt = %Editc(AWcrt:'X');
             CompRate = AWcrt;
          Else;
             Chain (TRco#:TRfnd:TRfyr:TRcase) PCM01;
             TRwkBenAmt = '00' + %Editc(CMcrt:'X');
             CompRate = CMcrt;
          Endif;

          If *InOf = *On;
             Except Hdg;
          Endif;

          Except Det;
          intRecCnt = intRecCnt + 1;

          If eUpdt = 'Y' and CompRate <> *Zeros;
             Update PTR01;
          Endif;
       Endsr;
      /End-Free
     OQSYSPRT   E            HDG              02
     O                                              'PGM:FIXBENR6'
     O                                           56 'FIX IDC WEEKLY BENEFIT'
     O                                        +0001 'AMOUNT LESS THAN THE STATE'
     O                                        +0001 'MINIMUM AMOUNT'
     O          E            HDG         1
     O                                           62 'BATCH ID'
     O                       TRBCHID          +0001
     O          E            HDG         2
     O                                              'CLAIM NUMBER'
     O                                           39 'OLD COMP RATE'
     O                                         +001 'NEW COMP RATE'
     O                                           63 'ADJUSTER'
     O          E            DET         1
     O                       TRCO#
     O                                              '-'
     O                       TRFND
     O                                              '-'
     O                       TRFYR
     O                                              '-'
     O                       TRCASE
     O                       OldRate       J     39
     O                       CompRate      J   +001
     O                       Adjuster            85
      *
     O          E            TOT         2
     O                                              'Total records processed:'
     O                       intRecCnt     4   +001
