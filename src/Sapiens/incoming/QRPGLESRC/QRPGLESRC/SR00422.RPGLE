     * (C) Copyright Progressive Data Solutions, Inc., 2003
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00422 - Foriegn Terrorism Premium Endorsement
     *===============================================================*
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 11/15/05                                          *
     * Programmer: C:61                                              *
     * Project   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
      * 09/15/08 C:189 PROJECT: WC1644                                *
      *                Add an Option to exclude terrorism from LCM    *
      *                                                               *
     *                                                               *
     * 08/04/08 C:190 WC1657                                         *
     *                Change the "logic" to look for a Stat Code     *
     *                (instead of a percentage or Terrorism amount)  *
     *                                                               *
     * 07/03/08 C:61 PROJECT WC1530                                  *
     *               Remove references to WTESL
     *                                                               *
     * 06/05/08 C:197 Project wc1530 priss #6                        *
     *              - Endorsements - Generic end Approved/Excluded   *
     *                States                                         *
     *                                                               *
     * 04/24/08 C:197 Support 62990                                  *
     *              - State and Rate per $100 of payroll information *
     *                is not printing on the form                    *
     *                                                               *
     * 03/29/08 C:197 Project wc1667                                 *
     *              - Terrorism End updates -State-specific          *
     *                                                               *
     * 11/01/07 C:184 Project wc1530                                 *
     *              - Endorsements - Generic end Approved/Excluded   *
     *                States                                         *
     *                                                               *
     * 10/23/07 C:184 support WA099 59651                            *
     *              - New source version (that have all prior commen-*
     *                ts removed)                                    *
     *                                                               *
     * 03/28/07 C:184 PROJECT WC1518                                 *
     *              - Endorsement "Approved by State" Edit           *
     *                                                               *
     * 02/06/07 C:61  support wa139 56430                            *
     *              - Attach for policies with NY coverage.          *
     *                                                               *
     * 01/17/07 C:61  support wa091 54752                            *
     *              - Adjust rounding of printed rate.               *
     *                                                               *
     * 11/02/06 C:61  support wa091 54752                            *
     *              - Only attach for approved states.               *
     *              - Only print rates for approved states.          *
     *
      * 04/07/06 C:61  support wc099 52133
     *                change to attach to forms with NJ coverage.
     *
      * 02/01/06 C:61  project cm027
     *              - Include PA rates.
     *
      * 01/20/06 C:61  project cm027
     *              - Approved North Carolina.
     *
      * 01/17/06 C:61  project cm027
     *              - Update states approved for attach.
     *
      * 11/30/05 C:61  support wa114 50097
     *              - allow for PA, DE, NJ
     *                                                               *
     * 11/28/05 SUPPORT CM027 CLT091                                 *
     *          Only attach to policies in states where wc000422     *
     *          has been approved.                                   *
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
    *** BEG ADD ******* 11/01/07 ******************************************
     *
     ****** beg del ****** 07/03/08 **********************************
      ** Endorsement States Approved
     F*****wtesl     if   e           k disk
     ****** end del ****** 07/03/08 **********************************
    *** END ADD ******* 11/01/07 ******************************************
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
     d rtsav           s              5  2
     d sttmp           s                   like(fmcd01)
     d rttmp           s                   like(fmrt01)
    
     d #1co#           s              3    inz(*blanks)
     d #1fnd           s              3    inz(*blanks)
     d ##rtcd          s              3    inz(*blanks)
     d ##clas          s              5    inz(*blanks)
     d ##efdt          s              7    inz(*blanks)
     d ##lcm           s              5    inz(*blanks)
     d xxlcm           s              5  4

    *** BEG ADD ******* 03/29/08 ******************************************
     d kkefdt          s                   like(elpfdt)
    *** BEG ADD ******* 03/29/08 ******************************************

     *=========================================================================
     *  /COPY required constants
     *=========================================================================

     * Form ID
     D qqform          c                   const('WC000422  ')
    *** BEG ADD ******* 11/01/07 ******************************************
     d x1form          s             15a   inz(*blanks)
    *** END ADD ******* 11/01/07 ******************************************

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
    *** BEG DEL ******* 03/29/08 ******************************************
     C**********         KFld                    ELPFDT
    *** END DEL ******* 03/29/08 ******************************************
      *
     C     rrky1         KList
     C                   KFld                    ELCO#
     C                   KFld                    ELFND
     C                   KFld                    RSJRST
    *** BEG ADD ******* 03/29/08 ******************************************
     C                   KFld                    kkefdt
    *** END ADD ******* 03/29/08 ******************************************
    
    *** BEG ADD ******* 11/01/07 ******************************************
     *
     ****** beg del ****** 07/03/08 **********************************
    C*****esky          KList
    C*****              KFld                    ELCO#
    C*****              KFld                    X1FORM
    C*****              KFld                    RSJRST
     ****** end del ****** 07/03/08 **********************************
    *** END ADD ******* 11/01/07 ******************************************
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     * Copybook C specs
     C/COPY SCOPYEN01C
    
     *=========================================================================
     *  CKSTAT - Check if endorsement approved for state                   d
     *=========================================================================
     c     ckstat        begsr
    
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              move      'N'           approved_state    1
    *** END DEL ******* 06/05/08 ******************************************
    
    *** BEG DEL ******* 11/01/07 ******************************************
     c*****              if        (rsjrst <> 'CA')
     c*****                        and (rsjrst <> 'FL')
     c*****                        and (rsjrst <> 'MA')
     c*****                        and (rsjrst <> 'PA')
     c*****                        and (rsjrst <> 'VA')
    *** END DEL ******* 11/01/07 ******************************************
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              eval      approved_state = 'Y'
    *** END DEL ******* 06/05/08 ******************************************

    *** BEG DEL ******* 11/01/07 ******************************************
     c*****              endif
    *** END DEL ******* 11/01/07 ******************************************

     c                   endsr
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
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              exsr      ckstat
    *** END DEL ******* 06/05/08 ******************************************
     *
     ****** beg del ****** 07/03/08 **********************************
    *** BEG ADD ******* 11/01/07 ******************************************
    c*****esky          Chain     pes01                              60
    ****END ADD ******* 11/01/07 ******************************************
    ****BEG ADD ******* 03/29/08 ******************************************
    c*****              if        *in60 =*on
    c*****elco#         chain     pes01                              60
    C*****              if        *in60 = *on
    C*****              eval      *in60=*off
    c*****              else
    C*****              eval      *in60=*on
    C*****              endif                                                  *in60=*on
    c*****              endif                                                  *in60=*on
    ****END ADD ******* 03/29/08 ******************************************
     ****** end del ****** 07/03/08 **********************************
     C                   if        (*in60 = *off)
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****                        and (approved_state = 'Y')
    *** END DEL ******* 06/05/08 ******************************************
    *** BEG CHG ******* 04/24/08 ******************************************
     C*****rrky          setgt     prr01
     C*****rrky1         readpe    prr01                                  60
     C                   eval      kkefdt = elpfdt
     C     rrky1         setgt     prr01
     C     rrky          readpe    prr01                                  60
    *** END CHG ******* 04/24/08 ******************************************
     *
     C     *in60         Ifeq      *off
     C     rrterr        andne     0
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
     c                   eval(h)   rtsav  = RRTERR * xxlcm
     C*** BEG ADD *****  09/15/08  ************************************
     C                   else
     c                   eval(h)   rtsav  = RRTERR
     C                   endif                                                      RRTERL IFNE
     C*** END ADD *****  09/15/08  ************************************
     c                   movel(p)  rsjrst        stsav
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
    *** BEG ADD ******* 11/01/07 ******************************************
    c                   movel(p)  xxform        x1form
    *** END ADD ******* 11/01/07 ******************************************
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
     C     rsky          chain     prs01                              60
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****              exsr      ckstat
    *** END DEL ******* 06/05/08 ******************************************
     *
     ****** beg del ****** 07/03/08 **********************************
    ****BEG ADD ******* 11/01/07 ******************************************
    c*****esky          Chain     pes01                              60
    ****END ADD ******* 11/01/07 ******************************************
    ****BEG ADD ******* 03/29/08 ******************************************
    c*****              if        *in60 =*on
    c*****elco#         chain     pes01                              60
    C*****              if        *in60 = *on
    C*****              eval      *in60=*off
    c*****              else
    C*****              eval      *in60=*on
    C*****              endif                                                  *in60=*on
    c*****              endif                                                  *in60=*on
     ****** end del ****** 07/03/08 **********************************
    *** END ADD ******* 03/29/08 ******************************************
     *
     C                   if        (*in60 = *off)
    *** BEG ADD ******* 03/29/08 ******************************************
     c                             and (elpfdt >= 1060101)
    *** END ADD ******* 03/29/08 ******************************************
    *** BEG DEL ******* 06/05/08 ******************************************
     c*****                        and (approved_state = 'Y')
    *** END DEL ******* 06/05/08 ******************************************
    *** BEG CHG ******* 03/29/08 ******************************************
     C*****rrky          setgt     prr01
     C*****rrky1         readpe    prr01                                  60
     c                   eval      kkefdt = elpfdt
     c     rrky1         setgt     prr01
     c     rrky          readpe    prr01                                  60
    *** END CHG ******* 03/29/08 ******************************************
     *
     *  Attach endorsement if non-zero terror rate for jurisdiction state
     *
     C     *in60         ifeq      *off
    C*** BEG CHG ******* 08/04/08 ******************************************
     C     rrterc        andne     *zero
     C*****rrterr        andne     *zero
    C*** END CHG ******* 08/04/08 ******************************************
     C                   move      *on           crtfrm
     C                   endif
     *
     C                   endif
     *
     C                   endsr
    
    
    
