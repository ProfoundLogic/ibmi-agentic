     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR09403A - FL - Terrorism Risk Insurance Ext Act Endorsement
      //===============================================================*
      //                                                               *
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 01/10/15                                          *
      // Programmer: C:302                                             *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/31/22 C:333 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F

     Fwdrrl     if   e           k disk
     Fwxfmp3    o    e             disk    usropn

      /COPY SCOPYEN01D

     D qqForm          s             15A   Inz('WC090403B ')
     D addpgm          c                   const('SRXXXXXXXX')

     D                 DS
     D  ST                     1    104
     D                                     DIM(52) ASCEND
     D  FMCD01                 1      2
     D  FMCD02                 3      4
     D  FMCD03                 5      6
     D  FMCD04                 7      8
     D  FMCD05                 9     10
     D  FMCD06                11     12
     D  FMCD07                13     14
     D  FMCD08                15     16
     D  FMCD09                17     18
     D  FMCD10                19     20
     D  FMCD11                21     22
     D  FMCD12                23     24
     D  FMCD13                25     26
     D  FMCD14                27     28
     D  FMCD15                29     30
     D  FMCD16                31     32
     D  FMCD17                33     34
     D  FMCD18                35     36
     D  FMCD19                37     38
     D  FMCD20                39     40
     D  FMCD21                41     42
     D  FMCD22                43     44
     D  FMCD23                45     46
     D  FMCD24                47     48
     D  FMCD25                49     50
     D  FMCD26                51     52
     D  FMCD27                53     54
     D  FMCD28                55     56
     D  FMCD29                57     58
     D  FMCD30                59     60
     D  FMCD31                61     62
     D  FMCD32                63     64
     D  FMCD33                65     66
     D  FMCD34                67     68
     D  FMCD35                69     70
     D  FMCD36                71     72
     D  FMCD37                73     74
     D  FMCD38                75     76
     D  FMCD39                77     78
     D  FMCD40                79     80
     D  FMCD41                81     82
     D  FMCD42                83     84
     D  FMCD43                85     86
     D  FMCD44                87     88
     D  FMCD45                89     90
     D  FMCD46                91     92
     D  FMCD47                93     94
     D  FMCD48                95     96
     D  FMCD49                97     98
     D  FMCD50                99    100
     D  FMCD51               101    102
     D  FMCD52               103    104

     D                 DS
     D  RT                     1    260  3
     D                                     DIM(52) ASCEND
     D  FMRT01                 1      5  3
     D  FMRT02                 6     10  3
     D  FMRT03                11     15  3
     D  FMRT04                16     20  3
     D  FMRT05                21     25  3
     D  FMRT06                26     30  3
     D  FMRT07                31     35  3
     D  FMRT08                36     40  3
     D  FMRT09                41     45  3
     D  FMRT10                46     50  3
     D  FMRT11                51     55  3
     D  FMRT12                56     60  3
     D  FMRT13                61     65  3
     D  FMRT14                66     70  3
     D  FMRT15                71     75  3
     D  FMRT16                76     80  3
     D  FMRT17                81     85  3
     D  FMRT18                86     90  3
     D  FMRT19                91     95  3
     D  FMRT20                96    100  3
     D  FMRT21               101    105  3
     D  FMRT22               106    110  3
     D  FMRT23               111    115  3
     D  FMRT24               116    120  3
     D  FMRT25               121    125  3
     D  FMRT26               126    130  3
     D  FMRT27               131    135  3
     D  FMRT28               136    140  3
     D  FMRT29               141    145  3
     D  FMRT30               146    150  3
     D  FMRT31               151    155  3
     D  FMRT32               156    160  3
     D  FMRT33               161    165  3
     D  FMRT34               166    170  3
     D  FMRT35               171    175  3
     D  FMRT36               176    180  3
     D  FMRT37               181    185  3
     D  FMRT38               186    190  3
     D  FMRT39               191    195  3
     D  FMRT40               196    200  3
     D  FMRT41               201    205  3
     D  FMRT42               206    210  3
     D  FMRT43               211    215  3
     D  FMRT44               216    220  3
     D  FMRT45               221    225  3
     D  FMRT46               226    230  3
     D  FMRT47               231    235  3
     D  FMRT48               236    240  3
     D  FMRT49               241    245  3
     D  FMRT50               246    250  3
     D  FMRT51               251    255  3
     D  FMRT52               256    260  3

      // counters
     d x1              s              3  0

     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4
     d stsav           s                   like(fmcd01)
     d rtsav           s              5  2
     d sttmp           s                   like(fmcd01)
     d rttmp           s                   like(fmrt01)
     d temp            s              5  0

     D SR09403B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR09403B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

     D GetLossCostM    pr                  extpgm('SRLCM')
     D  qqCo#                         3
     D  qqFmd                         3
     D  ELrtcd                        3
     D  ##clas                        5
     D  ##efdt                        7
     D  ##Lcm                         5

      /Free

      /COPY SCOPYEN01C

       //=========================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=========================================================================
       Begsr othinf;
          // open work file
          open wxfmp3;

          //    initialize
          clear pfm03;
          FMform = xxForm;
          //  Get state / rate information
          Setll (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          Dow Not %EOF(WDELL1);
       //***** Beg Add *** 10/31/22 ******************************
             xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
             If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
       //***** End Add *** 10/31/22 ******************************
                Chain (xxCo#:ELrtcd) WMRSL;
                If %Found(WMRSL) and RSjrst = ENst;
                   Setgt (ELco#:ELfnd:RSjrst:ELpfdt) WDRRL;
                   Readpe (ELco#:ELfnd:RSjrst) WDRRL;

                   If Not %EOF(wdrrl) And RRterr <> 0;
                      ##clas = %EditC(RRterc:'X');
                      ##efdt = %EditC(ELpfdt:'X');
                      GetLossCostM(qqCo#:qqFnd:ELrtcd:##clas:##efdt:##Lcm);
                      temp = %dec(##Lcm:5:0);
                      xxLcm = Temp / 10000;
                      eval(h) rtsav  = RRTERR * xxlcm;
                      stSav = RSjrst;
                      exsr srins;
                   EndIf;

                EndIf;

          //***** Beg Add *** 10/31/22 ******************************
             Endif;
          //***** End Add *** 10/31/22 ******************************
             Reade (xxCo#:xxFnd:xxFyr:xxEmp#) WDELL1;
          EndDo;

          // Write the output record
          Write pfm03;

          // Close the endorsement form workfile
          Close wxfmp3;

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       begsr pgmstr;
       endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       begsr pgmend;
       endsr;

       //=========================================================================
       //  srins - Insertion sort
       //=========================================================================
       Begsr srins;

          x1 = 1;
          Dow x1 <= 52;
             Select;
             When st(x1) = *blanks;
                st(x1) = stSav;
                rt(x1) = rtSav;
                Leave;
             When stSav > st(x1);
                x1 = x1 + 1;
                Iter;
             when stSav = st(x1);
                If rtSav > rt(x1);
                   rt(x1) = rtSav;
                Endif;
                Leave;
             Other;
                stTmp = st(x1);
                rtTmp = rt(x1);
                st(x1)= stSav;
                rt(x1)= rtSav;
                stSav= stTmp;
                rtSav= rtTmp;
                x1 = x1 + 1;
                Iter;
             Endsl;
          EndDo;

       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr valid;

          Chain (xxCo#:ELrtcd) WMRSL;
          If %Found  and RSjrst = ENst;

             Setgt (ELco#:ELfnd:RSjrst:ELpfdt) WDRRL;
             Readpe (ELco#:ELfnd:RSjrst) WDRRL;
             //  Attach endorsement if non-zero domestic code for jurisdictio   n state
             If Not %EOF And RRterc <> *zero;
                crtfrm = *on;
             Endif;
          Endif;

       Endsr;

      /End-Free
