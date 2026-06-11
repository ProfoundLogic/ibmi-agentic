     HCopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
      *================================================================
     H* Description:  Generate wcpols work data for submission history
     H*               records.  Call policy page program to build data
     H*               so deleted names/addresses will not report again.
     H*
     H* CREATED: 10/30/13  C:65   Project: 147WAC - Optacomp Conversion
     H*                                    (Supp# WA147/84011)
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     fWMSHP     uf   e             disk
     F                                     rename(psh01:psh0p)
     fWMSHL1    if   e           k disk
     F                                     rename(psh01:psh01)
     F                                     INFDS(DBFDB)
     fWXNAL     uf   e           k disk
     F                                     rename(pna01:pna00)
     fWDSHL     if a e           k disk
     F                                     rename(ps101:ps100)
      *
     d xxupdate        s              1    inz
     d xxtype          s              1    inz
     d xxco#           s              3  0 inz
     d xxfnd           s              3  0 inz
     d xxemp#          s              9  0 inz
     d xxdiv           s              5  0 inz
     d xxfyr           s              3  0 inz
     d svfyr           s              3  0 inz
     d svemp#          s              9  0 inz
     d svrrn           s              9  0 inz
     D svdate          s              7  0
     D sysDt7          s              7  0
     d x               s              3  0 inz
      *
     D DBFDB           DS
     D  DBRRB                397    400B 0
     D*
     D                 ds
     D  sst                    1     30    dim(15)
     D  shst01                 1      2
     D  shst02                 3      4
     D  shst03                 5      6
     D  shst04                 7      8
     D  shst05                 9     10
     D  shst06                11     12
     D  shst07                13     14
     D  shst08                15     16
     D  shst09                17     18
     D  shst10                19     20
     D  shst11                21     22
     D  shst12                23     24
     D  shst13                25     26
     D  shst14                27     28
     D  shst15                29     30
     D                 ds
     D  sss                    1     15    dim(15)
     D  shss01                 1      1
     D  shss02                 2      2
     D  shss03                 3      3
     D  shss04                 4      4
     D  shss05                 5      5
     D  shss06                 6      6
     D  shss07                 7      7
     D  shss08                 8      8
     D  shss09                 9      9
     D  shss10                10     10
     D  shss11                11     11
     D  shss12                12     12
     D  shss13                13     13
     D  shss14                14     14
     D  shss15                15     15
     D                 ds
     D  ssc                    1     15    dim(15)
     D  shcd01                 1      1
     D  shcd02                 2      2
     D  shcd03                 3      3
     D  shcd04                 4      4
     D  shcd05                 5      5
     D  shcd06                 6      6
     D  shcd07                 7      7
     D  shcd08                 8      8
     D  shcd09                 9      9
     D  shcd10                10     10
     D  shcd11                11     11
     D  shcd12                12     12
     D  shcd13                13     13
     D  shcd14                14     14
     D  shcd15                15     15
      *
     D                 DS
     D  SSD                    1    105  0
     D                                     DIM(15)
     D  SHDT01                 1      7  0
     D  SHDT02                 8     14  0
     D  SHDT03                15     21  0
     D  SHDT04                22     28  0
     D  SHDT05                29     35  0
     D  SHDT06                36     42  0
     D  SHDT07                43     49  0
     D  SHDT08                50     56  0
     D  SHDT09                57     63  0
     D  SHDT10                64     70  0
     D  SHDT11                71     77  0
     D  SHDT12                78     84  0
     D  SHDT13                85     91  0
     D  SHDT14                92     98  0
     D  SHDT15                99    105  0
     D                 DS
     D  STI                    1     90  0
     D                                     DIM(15)
     D  SHTM01                 1      6  0
     D  SHTM02                 7     12  0
     D  SHTM03                13     18  0
     D  SHTM04                19     24  0
     D  SHTM05                25     30  0
     D  SHTM06                31     36  0
     D  SHTM07                37     42  0
     D  SHTM08                43     48  0
     D  SHTM09                49     54  0
     D  SHTM10                55     60  0
     D  SHTM11                61     66  0
     D  SHTM12                67     72  0
     D  SHTM13                73     78  0
     D  SHTM14                79     84  0
     D  SHTM15                85     90  0
     D*
     D psds          esds                  extname(szq1p)
     D*
     D/copy sprString
     D/copy sprDate
     D/copy wPrEmplr
     D/copy wPrRateSet
    C*
    C*  KEY LIST FOR SUBMISSION HISTORY FILE
    C*
     C     shky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxfyr
     C                   kfld                    xxtype
    C*
     C     shky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    C*
    C*  KEY LIST FOR NAME/ADDRESS WORK FILE
    C*
     C     naky          klist
     C                   kfld                    shco#
     C                   kfld                    shfnd
     C                   kfld                    shemp#
     C                   kfld                    shfyr
     C                   kfld                    shunid
      *
      * Entry List
      *
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdate            7
      *
     C                   Move      qqco#         xxco#
     C                   Move      qqfnd         xxfnd
     C                   Z-add     0             xxemp#
     C                   Z-add     0             xxfyr
     C                   Z-add     0             svemp#
     C                   Z-add     0             svfyr
     C                   Z-add     0             svdate
      *
     C                   If        q1prms >= 3
     C                   Move      qqfyr         svfyr
     C                   Move      qqfyr         xxfyr
     C                   Endif
      *
     C                   If        q1prms >= 4
     C                   Move      qqemp#        svemp#
     C                   Move      qqemp#        xxemp#
     C                   Endif
      *
     C                   If        q1prms >= 5
     C                   Move      qqdate        svdate
     C                   Endif
      *
     C                   Move      'P'           xxtype
     C                   eval      sysDt7 = DateTo7(SystemDate)
      *
      /Free

       If svemp# <> 0;
          Setll shky psh01;
          Reade shky psh01;
       Else;
          Setll shky1 psh01;
          Reade shky1 psh01;
       Endif;

       Dow not %eof;

          If SHType = 'P';

             If shfyr = svfyr or svfyr = 0;

                If shemp# = svemp# or svemp# = 0;

                   If shidt <= svdate or svdate = 0;

                      xxEmp# = SHEmp#;
                      xxDiv = SHDiv;
                      xxFyr = SHFyr;

                      Exsr policyPage;

                   Endif;

                Endif;

             Endif;

          Endif;

          If svEmp# <> 0;
             Leave;
          Else;
             Reade shky1 psh01;
          Endif;

       Enddo;

      /End-Free
     C                   Move      *on           *inlr
     C***************************************************************************************
     C*
     C* Call policy page program to generate work data.
     C*
     C     policyPage    Begsr
     C*
     C                   Z-add     dbrrb         svrrn
     C     svrrn         Chain     psh0p
     C                   Move      '6'           SHStat
     C                   Movel(p)  q1pgm         shpgm
     C                   Movel(p)  q1user        shuser
     C                   Update    psh0p
     C*
     C                   MOVE      SHCO#         Q#CO#
     C                   MOVE      SHFND         Q#FND
     C                   MOVE      SHEMP#        Q#EMP#
     C                   MOVE      SHDIV         Q#DIV
     C                   MOVE      SHFYR         Q#FYR
     C
     C                   CALL      'W7413R'
     C                   PARM                    Q#CO#             3
     C                   PARM                    Q#FND             3
     C                   PARM                    Q#EMP#            9
     C                   PARM                    Q#DIV             5
     C                   PARM                    Q#FYR             3
     C                   PARM      'Y'           Q#REIS            1
     C                   PARM      *BLANKS       Q#QUE            10
     C                   PARM      '0'           Q#STEP            1
     C                   PARM      'N'           Q#DART            1
     C                   PARM      'N'           Q#HARD            1
     C                   PARM      'N'           Q#AGTO            1
     C                   PARM      '01'          Q#CPY             2
     C                   PARM      'B'           Q#RNB             1
     C                   PARM      *BLANKS       Q#DMBR           10
     C                   PARM      *BLANKS       Q#FINA            1
     C                   PARM      'N'           Q#PDF             1
     C                   PARM      *BLANKS       Q#PRT            10
     C                   PARM      *BLANKS       Q#PRT1           10
     C
     C     svrrn         Chain     psh0p
     C
     C                   Move      '4'           SHStat
     C                   eval      SHDT = sysDt7
     C                   eval      SHTIME = TimeTo6(SystemTime)
     C                   eval      SHSBDT = sysDt7
     C                   eval      SHSBTM = TimeTo6(SystemTime)
     C
     C     1             Do        15            x
     C                   If        sst(x) <> '  '
     C                   Move      '4'           sss(x)
     C                   Z-add     shsbdt        ssd(x)
     C                   Z-add     shsbtm        sti(x)
     C                   Exsr      sbmDetail
     C                   Endif
     C                   Enddo
     C
     C                   Movel(p)  q1pgm         shpgm
     C                   Movel(p)  q1user        shuser
     C                   Update    psh0p
     C
      /Free

       Setll naky pna00;
       Reade naky pna00;

       Dow not %eof;

          xxupdate = ' ';

          If napfdt = naptdt;
             nanamd = 'Y';
             xxupdate = 'Y';
          Endif;

          If napfd2 = naptd2;
             naadrd = 'Y';
             xxupdate = 'Y';
          Endif;

          If xxupdate = 'Y';
             napgm = q1pgm;
             nauser = q1user;
             Update pna00;
          Endif;

          Reade naky pna00;
       Enddo;

      /End-Free
     C
     C                   Endsr                                                  policyPage
     C***************************************************************************************
      *================================================================
      *   SUBROUTINE TO WRITE SUBMISSION HISTORY DETAIL RECORDS.
      *================================================================
     C     sbmDetail     BEGSR
     C*
     C*  CLEAR AND WRITE RECORD TO WDSHP FILE.
     C*
     C                   CLEAR                   PS100
     C*
     C                   MOVE      SHCO#         S1CO#
     C                   MOVE      SHFND         S1FND
     C                   MOVE      SHEMP#        S1EMP#
     C                   MOVE      SHDIV         S1DIV
     C                   MOVE      SHFYR         S1FYR
     C                   MOVE      SHTYPE        S1TYPE
     C                   MOVE      sst(x)        S1ST
     C                   MOVE      ssd(x)        S1SBDT
     C                   MOVE      sti(x)        S1SBTM
     C                   MOVE      ssc(x)        S1CODE
     C                   MOVE      SHUNID        S1UNID
     C                   MOVE      SHPFDT        S1PFDT
     C                   MOVE      SHAGT#        S1AGT#
     C                   MOVE      SHDOC         S1DOC
     C                   MOVE      SHFOLD        S1FOLD
     C*
     C                   Z-ADD     SHDT          S1DT
     C                   Z-ADD     SHTIME        S1TIME
     C                   MOVEL(P)  Q1PGM         S1PGM
     C                   MOVEL(P)  Q1USER        S1USER
     C                   Z-ADD     SHDT          S1IDT
     C                   Z-ADD     SHTIME        S1ITIM
     C                   MOVEL(P)  Q1PGM         S1IPGM
     C                   MOVEL(P)  Q1USER        S1IUSR
     C*
     C                   WRITE     PS100
     C*
     C                   ENDSR                                                  SUBR sbmDetail
