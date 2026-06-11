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
     FWMCML1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *Entry
     dFIXIDCCLS        pr
     d eUpdt                          1
     d inBchId                       14
     dFIXIDCCLS        pi
     d eUpdt                          1
     d inBchId                       14

      * Stand Alone
     D NewCls          S              4A   INZ
     D OldCls          S              4A   INZ
     D NewInd          S              1A   INZ
     D OldInd          S              1A   INZ
     D intCls          S              5  0 INZ
     D strBchId        S             14A   inz
     D intRecCnt       S              7  0 INZ
     D kkTbId          S              4    INZ('IJST')
     D kkTbKey         S             10    INZ
     D FieldChg        S              1A   INZ
     D StrIDCState     S              2A   INZ

      /Free
       strBchId = inBchId;
       Setll strBchId PQT01;
       Reade strBchId PQT01;
       Except HDG;
       Dow not %eof(WMIDCQTL1);
       // If QTbchid <> '00000000000000';
                OldCls = QTclscd;
                Chain (QTco#:QTfnd:QTfyr:QTcase) PCM01;
                If %Found(wmcml1);
                   NewCls = %Subst(%editc(CMclas:'X'):2:4);
                   QTclscd = %Subst(%editc(CMclas:'X'):2:4);
                Endif;

                NewInd = ' ';
                OldInd = QTindclmcd;
             If QTindclmcd = ' ';
                QTindclmcd = '0';
                NewInd = QTindclmcd;
             Endif;

                Exsr FixCls;

       // Endif;
          Reade strBchId PQT01;
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
       Begsr FixCls;

          If eUpdt = 'Y';
             Update PQT01;
          Endif;

          Exsr Print;
          intRecCnt = intRecCnt + 1;

       Endsr;
       Begsr Print;

          If *InOf = *On;
             Except Hdg;
          Endif;

          Except Det;
       Endsr;
      /End-Free
     OQSYSPRT   E            HDG            2 02
     O                                              'PGM:FIXIDCCLS'
     O                                           73 ' FIX CLASS CODE '
     O          E            HDG         1
     O                                              'UPDATE: '
     O                       EUPDT            +0001
     O                                           62 'BATCH ID'
     O                       QTBCHID          +0001
     O          E            HDG         2
     O                                              'CLAIM NUMBER'
     O                                           39 'Old Class'
     O                                         +002 'New Class'
     O                                         +008 'Old ClInd'
     O                                         +002 'New ClInd'
     O          E            DET         1
     O                       QTCO#
     O                                              '-'
     O                       QTFND
     O                                              '-'
     O                       QTFYR
     O                                              '-'
     O                       QTCASE
     O                       OldCls              34
     O                       QTclscd           +007
     O                       OldInd            +013
     O                       QTindclmcd        +010
     O
      *
     O          E            TOT         2
     O                                              'Total records processed:'
     O                       intRecCnt     4   +001
