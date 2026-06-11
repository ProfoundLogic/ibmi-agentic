     * (C) Copyright Progressive Data Solutions, Inc., 2003
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00420 - Terrorism Risk Insurance Act Endorsement
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 01/09/03                                          *
     * Programmer: C:61                                              *
     * Project   : WC1120                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 09/15/08 C:189 PROJECT: WC1644                                *
      *                Add an Option to exclude terrorism from LCM    *
      *                                                               *
     *                                                               *
     * 07/18/08 C:190 SEQ#64396                                      *
      *                Changed the processing for the Terrorism Endor-*
     *                sement so that it attaches without checking    *
     *                Foreign or Domestic Terrorism End Stat Code    *
      *                in Stat Rpt/Rating.                            *
     *                                                               *
      * 11/30/05 C:61  support wa114 50097
     *                Sunset 01/01/06 Terror Risk Act
     *
      * 08/10/05 C:136  Project: WC1136
      *                 Add Loss Cost Multiplier to Payroll Rate.
      *
     * 11/18/04 c:61  support wa114 44239                            *
     *                Only for non-Pennsylvania rating state         *
     *                                                               *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
    
     * Copybook F specs
     F/COPY SCOPYEN01F
    
      ** Statistical Reporting/Rating File
     Fwdrrl     if   e           k disk
    
      ** Statistical Reporting/Rating File
     Fwxfmp3    o    e             disk    usropn
    
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     * Copybook D specs
     D/COPY SCOPYEN01D

      *
      *
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
      *
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
    
      * counters
     d x1              s              3  0
    
      * temp
     d stsav           s                   like(fmcd01)
     d rtsav           s                   like(fmrt01)
     d sttmp           s                   like(fmcd01)
     d rttmp           s                   like(fmrt01)
    
      *** beg add ***** 08/10/05 **************************************
     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4
      *** end add ***** 08/10/05 **************************************


     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC000420  ')

     * Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')



     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
    
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
      *  Statistical Reporting/Rating
      *
     C     rrky          KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
     C                   KFld                    ELPFDT
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
    
     ** open work file
     C                   open      wxfmp3
    
     *    initialize
     C                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
    
     ** Get state / rate information
     C     elky2         Chain     pel02                              60
     *
     C                   Dow       *in60 = *off
     C     rsky          Chain     prs01                              60
     *
     ****** beg del **** 11/18/04 ***************************************
     C**** *in60         Ifeq      *off
     ****** end del **** 11/18/04 ***************************************
     ****** beg add **** 11/18/04 ***************************************
     C                   if        (*in60 = *off)
     C                             and (rsjrst <> 'PA')
     ****** end add **** 11/18/04 ***************************************
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     *
     C     *in60         Ifeq      *off
     C     rrterr        andne     0
     C**** rrtedt *****  andne     0
     C**** elpfdt *****  andge     rrtedt
      *** beg add ***** 08/10/05 ***************************************
     c                   move(p)   RRCO#         #1co#
     c                   move(p)   RRFND         #1fnd
     c                   move(p)   ELRTCD        ##rtcd
     c                   move(p)   RRTERC        ##clas
     c                   move(p)   ELPFDT        ##efdt

     c                   call      'SRLCM'
     c                   parm                    #1co#
     c                   parm                    #1fnd
     c                   parm                    ##rtcd
     c                   parm                    ##clas
     c                   parm                    ##efdt
     c                   parm                    ##lcm

     c                   move      ##lcm         xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   if        RRTERL <> 'Y'
     C*** END ADD *****  09/15/08  ************************************
     c                   eval(h)   RRTERR = RRTERR * xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   endif                                                      RRTERL IFNE
     C*** END ADD *****  09/15/08  ************************************
      *** end add ***** 08/10/05 ***************************************
     c                   movel(p)  rsjrst        stsav
     c                   z-add     rrterr        rtsav
     c                   exsr      srins
     C                   EndIf
     *
     C                   EndIf
     *
     C     elky2         ReadE     pel02                                  60
     C                   EndDo
    
     ** Write the output record
     C                   write     pfm03
     C                   write     pfm01
     *
     ** Close the endorsement form workfile
     C                   close     wxfmp3
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
    
     C                   endsr
    
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     *
     C                   endsr
    
    
    
     *=========================================================================
     *  srins - Insertion sort
     *=========================================================================
     C     srins         begsr
    
     c                   z-add     1             x1
    
     C     x1            Dowle     52
      *
     c                   select
      *
     C     st(x1)        wheneq    *blanks
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   leave
      *
     C     stsav         whengt    st(x1)
     c                   add       1             x1
     c                   iter
      *
     C     stsav         wheneq    st(x1)
     c     rtsav         ifgt      rt(x1)
     c                   z-add     rtsav         rt(x1)
     c                   endif
     c                   leave
      *
     C                   other
     c                   movel(p)  st(x1)        sttmp
     c                   z-add     rt(x1)        rttmp
     c                   movel(p)  stsav         st(x1)
     c                   z-add     rtsav         rt(x1)
     c                   movel(p)  sttmp         stsav
     c                   z-add     rttmp         rtsav
     c                   add       1             x1
     c                   iter
      *
     c                   endsl
      *
     C                   EndDo
    
     C                   endsr
    
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     ****** beg add **** 11/30/05 ***************************************
     C                   if        (elpfdt < 1060101)
     ****** end add **** 11/30/05 ***************************************
     C     rsky          chain     prs01                              60
     *
     ****** beg del **** 11/18/04 ***************************************
     C**************     if        *in60 = *off
     ****** end del **** 11/18/04 ***************************************
     ****** beg add **** 11/18/04 ***************************************
     C                   if        (*in60 = *off)
     C                             and (rsjrst <> 'PA')
     ****** end add **** 11/18/04 ***************************************
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
     C     *in60         ifeq      *off
    C*** BEG CHG ******* 07/18/08 ******************************************
     C     rrterc        andne     *zero
     C*********rrterr        andne     *zero
    C*** END CHG ******* 07/18/08 ******************************************
     C**** rrtedt ****** andne     0
     C**** elpfdt ****** andge     rrtedt
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
     ****** beg add **** 11/30/05 ***************************************
     *
     C                   endif
     ****** end add **** 11/30/05 ***************************************
     *
     C                   endsr
    
    
    
