     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR21402C - MI Endorsement WC210402C
       //===============================================================*
       //                                                               *
       //    PROGRAM DESCRIPTION:                                       *
       //                                                               *
       //       THIS PROGRAM IS CALLED TO:                              *
       //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
       //            ATTACHED TO THE POLICY.                            *
       //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
       //            (WXFMP) PRIOR TO A FORM MERGE.                     *
       //                                                               *
       //                                                               *
       //****************************************************************
       //                                                               *
       // REVISIONS:                                                    *
       //                                                               *
       // 11/03/22 C:333 Project: WC2834                                *
       //                Endorsement Recognition of Cancellation        *
       //                                                               *
       //****************************************************************

      /Copy ScopyEn01F

      //* Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
      //* Workers' Comp Assesable Forms Data Fill File 3
     Fwxfmp3    o    e             disk    usropn

      /Copy ScopyEn01D

     D qqForm          s             15A   Inz('WC210402C ')
     D addPgm          c                   const('SRXXXXXXXX')

     D                 DS
     D  St                     1    104
     D                                     DIM(52) ASCEND
     D  fmCd01                 1      2
     D  fmCd02                 3      4
     D  fmCd03                 5      6
     D  fmCd04                 7      8
     D  fmCd05                 9     10
     D  fmCd06                11     12
     D  fmCd07                13     14
     D  fmCd08                15     16
     D  fmCd09                17     18
     D  fmCd10                19     20
     D  fmCd11                21     22
     D  fmCd12                23     24
     D  fmCd13                25     26
     D  fmCd14                27     28
     D  fmCd15                29     30
     D  fmCd16                31     32
     D  fmCd17                33     34
     D  fmCd18                35     36
     D  fmCd19                37     38
     D  fmCd20                39     40
     D  fmCd21                41     42
     D  fmCd22                43     44
     D  fmCd23                45     46
     D  fmCd24                47     48
     D  fmCd25                49     50
     D  fmCd26                51     52
     D  fmCd27                53     54
     D  fmCd28                55     56
     D  fmCd29                57     58
     D  fmCd30                59     60
     D  fmCd31                61     62
     D  fmCd32                63     64
     D  fmCd33                65     66
     D  fmCd34                67     68
     D  fmCd35                69     70
     D  fmCd36                71     72
     D  fmCd37                73     74
     D  fmCd38                75     76
     D  fmCd39                77     78
     D  fmCd40                79     80
     D  fmCd41                81     82
     D  fmCd42                83     84
     D  fmCd43                85     86
     D  fmCd44                87     88
     D  fmCd45                89     90
     D  fmCd46                91     92
     D  fmCd47                93     94
     D  fmCd48                95     96
     D  fmCd49                97     98
     D  fmCd50                99    100
     D  fmCd51               101    102
     D  fmCd52               103    104

     D                 DS
     D  Rt                     1    260  3
     D                                     DIM(52) ASCEND
     D  fmRt01                 1      5  3
     D  fmRt02                 6     10  3
     D  fmRt03                11     15  3
     D  fmRt04                16     20  3
     D  fmRt05                21     25  3
     D  fmRt06                26     30  3
     D  fmRt07                31     35  3
     D  fmRt08                36     40  3
     D  fmRt09                41     45  3
     D  fmRt10                46     50  3
     D  fmRt11                51     55  3
     D  fmRt12                56     60  3
     D  fmRt13                61     65  3
     D  fmRt14                66     70  3
     D  fmRt15                71     75  3
     D  fmRt16                76     80  3
     D  fmRt17                81     85  3
     D  fmRt18                86     90  3
     D  fmRt19                91     95  3
     D  fmRt20                96    100  3
     D  fmRt21               101    105  3
     D  fmRt22               106    110  3
     D  fmRt23               111    115  3
     D  fmRt24               116    120  3
     D  fmRt25               121    125  3
     D  fmRt26               126    130  3
     D  fmRt27               131    135  3
     D  fmRt28               136    140  3
     D  fmRt29               141    145  3
     D  fmRt30               146    150  3
     D  fmRt31               151    155  3
     D  fmRt32               156    160  3
     D  fmRt33               161    165  3
     D  fmRt34               166    170  3
     D  fmRt35               171    175  3
     D  fmRt36               176    180  3
     D  fmRt37               181    185  3
     D  fmRt38               186    190  3
     D  fmRt39               191    195  3
     D  fmRt40               196    200  3
     D  fmRt41               201    205  3
     D  fmRt42               206    210  3
     D  fmRt43               211    215  3
     D  fmRt44               216    220  3
     D  fmRt45               221    225  3
     D  fmRt46               226    230  3
     D  fmRt47               231    235  3
     D  fmRt48               236    240  3
     D  fmRt49               241    245  3
     D  fmRt50               246    250  3
     D  fmRt51               251    255  3
     D  fmRt52               256    260  3

     d x1              s              3  0

     d inCo#           s              3    inz(*blanks)
     d inFnd           s              3    inz(*blanks)
     d outRtCd         s              3    inz(*blanks)
     d inClas          s              5    inz(*blanks)
     d inEfDt          s              7    inz(*blanks)
     d outLcM          s              5    inz(*blanks)
     d numLcM          s              5  0 inz(*blanks)
     d xxLcM           s              5  4
     d stsav           s                   like(fmCd01)
     d rtsav           s              5  2
     d sttmp           s                   like(fmCd01)
     d rttmp           s                   like(fmRt01)

     D Sr21402C        pr
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D Sr21402C        pi
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     d GetlcMult       pr                  Extpgm('SRLCM')
     d inCo#                          3
     d inFnd                          3
     d outRtCd                        3
     d inClas                         5
     d inEfDt                         7
     d outLcM                         5

      /Copy ScopyEn01C

      //========================================================================
      //  OthInf - Subroutine to load form specific information & write record
      //========================================================================
       Begsr Othinf;

       open WXFMP3;

       Clear pfm03;
       fmForm = %Trim(xxForm);
       //* Get state / rate information
       Setll elky2 pel02;
       Reade elky2 pel02;
       Dow Not %Eof(WDELL1);

       //***** Beg Add *** 11/03/22 ******************************
          xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
          If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 11/03/22 ******************************
             Chain rsky prs01;

             Setgt (Elco#:ELfnd:RSjrSt:ELpfDt) prr01;
             Readpe (ELco#:ELfnd:RSjrSt) prr01;
             If Not %Eof(WDRRL) and RRtErr <> 0;
                inCo# = %EditC(RRco#:'X');
                inFnd = %EditC(RRfnd:'X');
                outRtCd = ELrtCd;
                inClas = %EditC(RRteRc:'X');
                inEfDt = %EditC(ELpfDt:'X');

                Callp GetLcMult(inCo#:inFnd:outRtCd:inClas:inEfDt:outLcM);
                Monitor;
                numLcM = %Dec(outLcm:5:0);
                xxLcM = numLcm/10000;
                On-Error;
                Endmon;
                If RRterL <> 'Y';
                  eval(h) rtsav  = RRTERR * xxlcm;
                Else;
                  eval(h) rtsav  = RRTERR;
                Endif;
                stSav = %Trim(RSjrSt);
                Exsr SrIns;
             EndIf;

          //***** Beg Add *** 11/03/22 ******************************
          Endif;
          //***** End Add *** 11/03/22 ******************************
          ReadE elky2 pel02;
       EndDo;

       Write pfm03;

       Close WXFMP3;

       Endsr;
       //=======================================================================
       //  PgmStr - Beginning of program processing
       //=======================================================================
       Begsr PgmStr;
       Endsr;
       //=======================================================================
       //  PgmEnd - End of program processing
       //=======================================================================
       Begsr PgmEnd;
       Endsr;
       //=======================================================================
       //  SrIns - Insertion sort
       //=======================================================================
       Begsr SrIns;

       x1 = 1;

       Dow x1 <= 52;

          Select;

          When st(x1) = *blanks;
             st(x1) = %Trim(stSav);
             rt(x1) = rtSav;
             leave;
           When stSav > st(x1);
              x1 = x1 + 1;
             iter;
           When stSav = st(x1);
              If rtSav > rt(x1);
                 rt(x1)= rtSav;
              Endif;
              leave;
           Other;
              stTmp  = %Trim(st(x1));
              rtTmp  = rt(x1);
              st(x1) = %Trim(stSav);
              rt(x1) = rtSav;
              stSav  = stTmp;
              rtSav  = rtTmp;
              x1     = x1 + 1;
              iter;
           Endsl;

       EndDo;

       Endsr;
       //=======================================================================
       //  Valid - Subroutine to attach this form to the policy
       //=======================================================================
       Begsr Valid;

         Setgt (ELco#:ELfnd:RSjrSt:ELpfDt) prr01;
         Readpe (ELco#:ELfnd:RSjrSt) prr01;

         //  Attach endorsement if non-zero terror rate for jurisdiction state
         If Not %Eof(wdrrl) and (RRterC <> *zeros);
            crtFrm = *on;
         Endif;

       Endsr;
