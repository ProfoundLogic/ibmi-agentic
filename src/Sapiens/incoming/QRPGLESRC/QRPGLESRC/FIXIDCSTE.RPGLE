     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      //
     h/TITLE FIXIDCSTE - Fix IDC states
     h dftactgrp(*no) actgrp(*caller)
      *
      * Created :  C: 053  Support 119861
      *
      * DESCRIPTION :  Fix IDC quarterly file records that
      *                do not have the IDC State translation.
      *
      * REVISIONS:
      * 00/00/00 C:000  Support 000000
      *
      *
     FWMIDCQTL1 UF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *Entry
     dFIXIDCSTE        pr
     d eUpdt                          1
     d*inBchId                       14
     dFIXIDCSTE        pi
     d eUpdt                          1
     d*inBchId                       14

      * Stand Alone
     D NewExpSt        S              2A   INZ
     D OldExpSt        S              2A   INZ
     D NewJrStCd       S              2A   INZ
     D OldJrStCd       S              2A   INZ
     D NewAccStCd      S              2A   INZ
     D OldAccStCd      S              2A   INZ
     D Adjuster        S             30A   inz
     D*strBchId        S             14A   inz
     D intRecCnt       S              7  0 INZ
     D kkTbId          S              4    INZ('IJST')
     D kkTbKey         S             10    INZ
     D FieldChg        S              1A   INZ
     D StrIDCState     S              2A   INZ

      /Free
       Setll *loval PQT01;
       Read PQT01;
       Except HDG;
       Dow not %eof(WMIDCQTL1);

             FieldChg = ' ';

          // First field
             OldExpSt = QTExpSt;
             kkTbKey = QTExpSt;
             Exsr GetStateCd;
             If strIdcState <> '  ';
                NewExpSt = strIdcState;
                QTExpSt = strIdcState;
                FieldChg = 'Y';
             Endif;

          // Second field
             OldJrStCd = QTJrStCd;
             kkTbKey = QTJrStCd;
             Exsr GetStateCd;
             If strIdcState <> '  ';
                NewJrStCd = strIdcState;
                QTJrStCd = strIdcState;
                FieldChg = 'Y';
             Endif;

          // Third field
             OldAccStCd = QTAccStCd;
             kkTbKey = QTAccStCd;
             Exsr GetStateCd;
             If strIdcState <> '  ';
                NewAccStCd = strIdcState;
                QTAccStCd = strIdcState;
                FieldChg = 'Y';
             Endif;

             Exsr FixStateCd;

             Read PQT01;
          Enddo;

       If intRecCnt > *Zero;
          If *InOf = *On;
             Except Hdg;
          Endif;
          Except Tot;
       Endif;

       *inlr = *on;

       //*--------------------------------------------------------------
       //* Fix IDC State code
       //*--------------------------------------------------------------
       Begsr FixStateCd;
          // Fix State code 1
          // Fix State code 2
          // Fix State code 3

          If FieldChg = 'Y';
             Exsr Print;
             intRecCnt = intRecCnt + 1;
          Endif;

          If eUpdt = 'Y' and FieldChg = 'Y';
             Update PQT01;
          Endif;
       Endsr;
       Begsr Print;

          If *InOf = *On;
             Except Hdg;
          Endif;

          Except Det;
       Endsr;
       //*--------------------------------------------------------------
       //* Get IDC State code
       //*--------------------------------------------------------------
       Begsr GetStateCd;
          // Get State IDC code

           Chain (QTCo#:kkTbId:kkTbKey) PGT01;
           If %found(WTGTL);
              strIdcState =  %subst(GTcf01:1:2);
           Else;
              strIdcState = *blanks;
           Endif;

       Endsr;
      /End-Free
     OQSYSPRT   E            HDG            2 02
     O                                              'PGM:FIXIDCSTE'
     O                                           73 ' FIX STATE VALUE'
     O          E            HDG         1
     O                                           62 'BATCH ID'
     O                       QTBCHID          +0001
     O          E            HDG         2
     O                                           39 'QQExpSt '
     O                                         +002 'QQJrStCd'
     O                                         +002 'QQAccStCd'
     O          E            HDG         1
     O                                              'CLAIM NUMBER'
     O                                           39 'OLD  NEW'
     O                                         +002 'OLD  NEW'
     O                                         +002 'OLD  NEW'
     O          E            DET         1
     O                       QTCO#
     O                                              '-'
     O                       QTFND
     O                                              '-'
     O                       QTFYR
     O                                              '-'
     O                       QTCASE
     O                       OldExpSt            33
     O                       NewExpSt          +003
     O                       OldJrStCd         +003
     O                       NewJrStCd         +003
     O                       OldAccStCd        +003
     O                       NewAccStCd        +003
     O
      *
     O          E            TOT         2
     O                                              'Total records processed:'
     O                       intRecCnt     4   +001
