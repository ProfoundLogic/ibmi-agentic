     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      //
     h/TITLE FIXIDCPV1 -
     h dftactgrp(*no) actgrp(*caller)
      *
      * Created :  C: 053  Support 118614
      *
      * DESCRIPTION :  Fix IDC transactional file records that
      *                do not have TRBENTYCD populated
      *
      * REVISIONS:
      * 00/00/00 C:000  Support 000000
      *
      *
     FWMIDCTRL1 UF   E           K DISK
     FWDPAL5    IF   E           K DISK
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
     dFIXIDCPV1        pr
     d eUpdt                          1
     d inBchId                       14
     dFIXIDCPV1        pi
     d eUpdt                          1
     d inBchId                       14

      * Stand Alone
     D kkBR2           S             10A   inz
     D kkTrDt          S                   like(PAtrdt) inz
     D kkFrDt          S                   like(PAfrdt) inz
     D kkToDt          S                   like(PAtodt) inz
     D kkTrnAmt        S                   like(TRTRNAMT)
     D x2Amt           S              9  2 INZ
     D NewCode         S              2A   INZ
     D OldCode         S              2A   INZ
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
             If TRbentycd  = '  ';
                Chain (TRco#:TRfnd:TRfyr:TRcase) PCM01;
                Chain CMclEx pup01;
                If %Found(wmupl1);
                   Adjuster=UPusNm;
                Endif;
                OldCode = TRbentycd;
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
                        Exsr FixBenTyCd;
                        Leave;
                     Else;
                        Exsr Print;
                     Endif;
                   Else;
                     Exsr Print;
                   Endif;
                Endif;

                Reade (TRco#:TRfnd:TRfyr:TRcase:kkTrDT) PPA01;
             Enddo;
       Endsr;

       //*--------------------------------------------------------------
       //* Fix IDC weekly Ben amt
       //*--------------------------------------------------------------
       Begsr FixBenTyCd;
          // Fix Benefit Type code
          NewCode = '  ';
          If B2ibtc <> '  ' and TRBenTyCd = '  ';
             TRBenTyCd = B2IBTC;
             NewCode  = B2IBTC;
          Endif;

          Exsr Print;
          intRecCnt = intRecCnt + 1;

          If eUpdt = 'Y' and NewCode <> '  ';
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
     O                                              'PGM:FIXIDCPV1'
     O                                           73 'FIX IDC PV1 VALUE'
     O          E            HDG         1
     O                                           62 'BATCH ID'
     O                       TRBCHID          +0001
     O          E            HDG         2
     O                                              'CLAIM NUMBER'
     O                                           39 'OLD CODE'
     O                                         +002 'NEW CODE'
     O                                         +002 'PV1'
     O                                           66 'ADJUSTER'
     O          E            DET         1
     O                       TRCO#
     O                                              '-'
     O                       TRFND
     O                                              '-'
     O                       TRFYR
     O                                              '-'
     O                       TRCASE
     O                       OldCode             39
     O                       NewCode           +002
     O                       PAbr2             +008
     O                       Adjuster            88
     O
      *
     O          E            TOT         2
     O                                              'Total records processed:'
     O                       intRecCnt     4   +001
