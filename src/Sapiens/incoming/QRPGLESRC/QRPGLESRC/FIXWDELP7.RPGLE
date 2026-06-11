     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H/TITLE FIXWDELP7 - Check estimated WDELP fields do not match WDEHP.
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK    rename(pel01:pel00)
     FWDEHL6    IF   E           K DISK    rename(peh01:peh06)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     D/copy sPrDate

     C*
     C*  EMPLOYER History FILE
     C*
     C     keyWDEHL6     KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    xxFND
     C                   KFLD                    xxEMP#
     C                   KFLD                    xxDIV
     C                   KFLD                    xxFYR
     C*
     C*  EMPLOYER Detail FILE
     C*
     C     keyWDELL      KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    xxFND
     C                   KFLD                    xxEMP#
     C                   KFLD                    xxDIV
     C                   KFLD                    xxFYR
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELANPR        XXDIFF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C                   TIME                    DSDTTM
     C                   Clear                   xxTRDT            8 0
     C*
      /free

       *inOF = *on;
       *in66 = *off;

       If QQUpd = 'Y';
          *in66 = *on;
       Endif;

       Read pel00;
       Dow not %eof;
          xxCO#  = ELCO#;
          xxFND  = ELFND;
          xxEMP# = ELEMP#;
          xxDIV  = ELDIV;
          xxFYR  = ELFYR;

          Setgt keyWDEHL6 peh06;
          Readpe keyWDEHL6 peh06;
          Dow not %eof and EHFAF = 'A';
             Readpe keyWDEHL6 peh06;
          Enddo;

          If not %eof and (ELANPR <> EHWAPR or ELSTPR <> EHWSPR
            or ELMAPR <> EHWMPR or ELESBJ <> EHWSBJ);

             If *inOF = *on;
                *inOF = *off;
                Except Head;
             Endif;

             xxDiff = ELANPR - EHWAPR;
             xxTrdt = DtMDYY(EHTRDT);

             Except Detail;

             If QQUpd = 'Y';
                Exsr Update;
             Endif;

          Endif;

          Read pel00;
       Enddo;

       *inLR = *on;

       //********************************************************
       Begsr Update;

          ELEMNP = EHWMNP;
          ELMAPR = EHWMPR;
          ELESBJ = EHWSBJ;
          ELSTPR = EHWSPR;
          ELEMPM = EHWMPM;
          ELENMP = EHWNMP;
          ELANPR = EHWAPR;
          ELEDC1 = EHWDC1;
          ELEDC2 = EHWDC2;
          ELEDC3 = EHWDC3;
          ELEDV2 = EHWDV2;
          ELEXP1 = EHWXP1;
          ELEXP2 = EHWXP2;
          ELEXP3 = EHWXP3;
          ELEIN1 = EHWIN1;
          ELEIN2 = EHWIN2;
          ELEDCT = EHWDCT;
          ELEDCM = EHWDCM;
          ELEDCS = EHWDCS;
          ELEDCR = EHWDCR;
          ELEDC  = EHWDC;
          ELEFRS = EHWFRS;
          ELEWAV = EHWWAV;
          ELETER = EHWTER;
          ELRPE1 = EHWRP1;
          ELRPE2 = EHWRP2;
          ELRPE3 = EHWRP3;
          ELEDD2 = EHWDD2;
          ELEDTR = EHWDTR;
          ELEARC = EHWARC;
          ELD1E2 = EHD1W2;
          ELD2E2 = EHD2W2;
          ELEHCN = EHWHCN;

          ELPGM = Q1PGM;
          ELUSER = Q1USER;
          Update pel00;

       Endsr;
       //********************************************************

      /end-free
     C*
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           24 'FIXWDELP7'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF WDELP '
     O                                           45 'DOES NOT MATCH WDEHP'
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           59 'ELANPR'
     O                                           76 'EHWAPR'
     O                                           93 'Difference'
     O                                          110 'EHTRDT'
     O          E            DETAIL         1
     O                       ELCO#         4      5
     O                       ELFND         4     11
     O                       ELFYR         4     16
     O                       ELEMP#        4     27
     O                       ELDIV         4     34
     O                       ELFAF               39
     O                       ELANPR        J     59
     O                       EHWAPR        J     76
     O                       xxDIFF        J     93
     O                       xxTRDT             110 '  /  /    '
