     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     Hdftactgrp(*no)
     H BNDDIR('WTBNDDIR')
      *============================================================================================*
      /title W0093R - 3rd Party Premium Update Program                                             *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      * Revisions
      *
      * 12/31/20 C:136 Support: 92607
      *                Correct audited manual premium calc
      *
      *
      * 10/07/13 C:190 Project: 25WSS
      *                Change the program to determine if
      *                Premium or Contribution should be printed
      *                based on Group setup.
      *
      * 05/12/09 C:136 Support: WA146 68335
      *                Correct Premium Total Logic
      *
      * 05/11/09 C:136 Support: WA146 68081
      *                Correct edit for Short Rate Penalty applied to Standard
      *============================================================================================*
     Fw0093f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*  Physical file definition (for maintaining record)
     Fwae3p     uf a e             disk
     F                                     rename(pel01:dbrcdf)
      * Employer Detail File
     fwdelp     if   e             disk    rename(pel01 : pel0p)
      * Rate Set Code File
     fwmrsl     if   e           k disk    rename(prs01 : prs0l)
      * Ratings and Statistics File
     fwdrrl     if   e           k disk    rename(prr01 : prr0l)
      * Group Master
     fwmfnl     if   e           k disk    rename(pfn01 : pfn0l)
      * Employer Master
     fwmeml     if   e           k disk    rename(pem01 : pem0l)
      * 3rd Party Premium - co/fnd/emp/div/yr
     fwae3l     if   e           k disk    rename(pel01 : pe30l)
     F                                     infds(dbwae3l)
     FWMEPL     if   e           k disk
     FWMPRL1    IF   E           K DISK
     FWMPXL1    IF   E           K DISK
    D*=====================================================================
    D*  /COPY required constants
    D*=====================================================================
      ***************** Beg Chg ************** 10/07/13 **********************
     D***************** optnm           c                   '3rd Party Premium Update'
     D optnm           s             40
      ***************** End Chg ************** 10/07/13 **********************
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*=====================================================================
    D* Stand Alone Fields - TOP
    D*=====================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@elrrn         s              9  0
     D fldsize         s              6
     D lvlsize         s              1
     D a               s              3  0
     D b               s              3  0
     D c               s              3  0
      ***************** Beg Add ************** 10/07/13 **********************
     DstrHdg           s             22
     DisContri         s              1
     D/copy wPrGroup
      ***************** End Add ************** 10/07/13 **********************
     d xxst            s                   like(RSJRST)
     d xxexco          s                   like(ELPRTD)
     d xxanpr          s                   like(ELANPR)
     d prmLvlTotal     s                   like(ELANPR)
     d prmLvlAmt       s                   like(ELANPR)
     d xxTotal         s                   like(ELANPR)
     d NonDisplay      c                   const(x'A7')
     d Editable        c                   const(x'24')
     d LightEmUp       c                   const(x'22')
     d LightEmUpLine   c                   const(x'26')
     d displayOnly     c                   const(x'A2')
    D*=====================================================================
    D* Arrays and Data Structures
    D*=====================================================================
      *
      * Default Headings
      *
      ***************** Beg Chg ************** 10/07/13 **********************
     D***************** con             s             22    dim(60) ctdata perrcd(1)
     D con             s             22    dim(65) ctdata perrcd(1)
      ***************** End Chg ************** 10/07/13 **********************
      *
      * Screen Descriptions
      *
     D                 ds
     D dsc                     1    612
     D                                     dim(34)
     D  scds01                 1     18
     D  scds02                19     36
     D  scds03                37     54
     D  scds04                55     72
     D  scds05                73     90
     D  scds06                91    108
     D  scds07               109    126
     D  scds08               127    144
     D  scds09               145    162
     D  scds10               163    180
     D  scds11               181    198
     D  scds12               199    216
     D  scds13               217    234
     D  scds14               235    252
     D  scds15               253    270
     D  scds16               271    288
     D  scds17               289    306
     D  scds18               307    324
     D  scds19               325    342
     D  scds20               343    360
     D  scds21               361    378
     D  scds22               379    396
     D  scds23               397    414
     D  scds24               415    432
     D  scds25               433    450
     D  scds26               451    468
     D  scds27               469    486
     D  scds28               487    504
     D  scds29               505    522
     D  scds30               523    540
     D  scds31               541    558
     D  scds32               559    576
     D  scds33               577    594
     D  scds34               595    612
      *
      * Plus or Minus
      *
     D                 ds
     D sin                     1     34
     D                                     dim(34)
     D  scsn01                 1      1
     D  scsn02                 2      2
     D  scsn03                 3      3
     D  scsn04                 4      4
     D  scsn05                 5      5
     D  scsn06                 6      6
     D  scsn07                 7      7
     D  scsn08                 8      8
     D  scsn09                 9      9
     D  scsn10                10     10
     D  scsn11                11     11
     D  scsn12                12     12
     D  scsn13                13     13
     D  scsn14                14     14
     D  scsn15                15     15
     D  scsn16                16     16
     D  scsn17                17     17
     D  scsn18                18     18
     D  scsn19                19     19
     D  scsn20                20     20
     D  scsn21                21     21
     D  scsn22                22     22
     D  scsn23                23     23
     D  scsn24                24     24
     D  scsn25                25     25
     D  scsn26                26     26
     D  scsn27                27     27
     D  scsn28                28     28
     D  scsn29                29     29
     D  scsn30                30     30
     D  scsn31                31     31
     D  scsn32                32     32
     D  scsn33                33     33
     D  scsn34                34     34
      *
      * Prem Amount
      *
     D                 ds
     D amt                     1    374  2
     D                                     dim(34)
     D  scam01                 1     11  2 inz(0)
     D  scam02                12     22  2 inz(0)
     D  scam03                23     33  2 inz(0)
     D  scam04                34     44  2 inz(0)
     D  scam05                45     55  2 inz(0)
     D  scam06                56     66  2 inz(0)
     D  scam07                67     77  2 inz(0)
     D  scam08                78     88  2 inz(0)
     D  scam09                89     99  2 inz(0)
     D  scam10               100    110  2 inz(0)
     D  scam11               111    121  2 inz(0)
     D  scam12               122    132  2 inz(0)
     D  scam13               133    143  2 inz(0)
     D  scam14               144    154  2 inz(0)
     D  scam15               155    165  2 inz(0)
     D  scam16               166    176  2 inz(0)
     D  scam17               177    187  2 inz(0)
     D  scam18               188    198  2 inz(0)
     D  scam19               199    209  2 inz(0)
     D  scam20               210    220  2 inz(0)
     D  scam21               221    231  2 inz(0)
     D  scam22               232    242  2 inz(0)
     D  scam23               243    253  2 inz(0)
     D  scam24               254    264  2 inz(0)
     D  scam25               265    275  2 inz(0)
     D  scam26               276    286  2 inz(0)
     D  scam27               287    297  2 inz(0)
     D  scam28               298    308  2 inz(0)
     D  scam29               309    319  2 inz(0)
     D  scam30               320    330  2 inz(0)
     D  scam31               331    341  2 inz(0)
     D  scam32               342    352  2 inz(0)
     D  scam33               353    363  2 inz(0)
     D  scam34               364    374  2 inz(0)
      *
      * Field Name
      *
     D                 ds
     D fld                     1    204
     D                                     dim(34)
     D  fld01                  1      6
     D  fld02                  7     12
     D  fld03                 13     18
     D  fld04                 19     24
     D  fld05                 25     30
     D  fld06                 31     36
     D  fld07                 37     42
     D  fld08                 43     48
     D  fld09                 49     54
     D  fld10                 55     60
     D  fld11                 61     66
     D  fld12                 67     72
     D  fld13                 73     78
     D  fld14                 79     84
     D  fld15                 85     90
     D  fld16                 91     96
     D  fld17                 97    102
     D  fld18                103    108
     D  fld19                109    114
     D  fld20                115    120
     D  fld21                121    126
     D  fld22                127    132
     D  fld23                133    138
     D  fld24                139    144
     D  fld25                145    150
     D  fld26                151    156
     D  fld27                157    162
     D  fld28                163    168
     D  fld29                169    174
     D  fld30                175    180
     D  fld31                181    186
     D  fld32                187    192
     D  fld33                193    198
     D  fld34                199    204
      *
      * Premium Level
      *
     D                 ds
     D lvl                     1     34
     D                                     dim(34)
     D  lvl01                  1      1
     D  lvl02                  2      2
     D  lvl03                  3      3
     D  lvl04                  4      4
     D  lvl05                  5      5
     D  lvl06                  6      6
     D  lvl07                  7      7
     D  lvl08                  8      8
     D  lvl09                  9      9
     D  lvl10                 10     10
     D  lvl11                 11     11
     D  lvl12                 12     12
     D  lvl13                 13     13
     D  lvl14                 14     14
     D  lvl15                 15     15
     D  lvl16                 16     16
     D  lvl17                 17     17
     D  lvl18                 18     18
     D  lvl19                 19     19
     D  lvl20                 20     20
     D  lvl21                 21     21
     D  lvl22                 22     22
     D  lvl23                 23     23
     D  lvl24                 24     24
     D  lvl25                 25     25
     D  lvl26                 26     26
     D  lvl27                 27     27
     D  lvl28                 28     28
     D  lvl29                 29     29
     D  lvl30                 30     30
     D  lvl31                 31     31
     D  lvl32                 32     32
     D  lvl33                 33     33
     D  lvl34                 34     34

     D                 ds
     D dsp                     1     34
     D                                     dim(34)
     D  am01                   1      1
     D  am02                   2      2
     D  am03                   3      3
     D  am04                   4      4
     D  am05                   5      5
     D  am06                   6      6
     D  am07                   7      7
     D  am08                   8      8
     D  am09                   9      9
     D  am10                  10     10
     D  am11                  11     11
     D  am12                  12     12
     D  am13                  13     13
     D  am14                  14     14
     D  am15                  15     15
     D  am16                  16     16
     D  am17                  17     17
     D  am18                  18     18
     D  am19                  19     19
     D  am20                  20     20
     D  am21                  21     21
     D  am22                  22     22
     D  am23                  23     23
     D  am24                  24     24
     D  am25                  25     25
     D  am26                  26     26
     D  am27                  27     27
     D  am28                  28     28
     D  am29                  29     29
     D  am30                  30     30
     D  am31                  31     31
     D  am32                  32     32
     D  am33                  33     33
     D  am34                  34     34

     d  svamt                              like(amt) dim(34)

     D dbwae3l         ds
     D  e3lrrn               397    400B 0
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d setScreen       pr
     d   defaultHdg                  22    const
     d   customHdg                   20    const
     d   amount                            like(ELANPR) const
     d   sign                         1    const
     d   level                        1    const
     d   field                        6    const

     d resetData       pr                  extpgm('W0093C02')
     D qqfrFile                      10    const
     D qqtoFile                      10    const
     D qqco                           3    const
     D qqfnd                          3    const
     D qqemp#                         9    const
     D qqdiv                          5    const
     D qqfyr                          3    const

     C/COPY SCOPYMT01C
    C* ====================================================================
    C*  PLIST definitions
    C*=====================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@elrrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     c     elky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c     emky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c     fnky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c     rsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
     c     rrky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    RSJRST
     c                   kfld                    ELPFDT
     c     rrky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    RSJRST
     C     epky1         klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C     prky          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELRTCD
     C                   kfld                    EPCLAS
     C                   kfld                    EPEFDT
     C     PRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr

     c                   if        b <> 0
     c                   eval      dsp(b) = Editable
     c                   write     scr01
     c                   endif
      *
      *  Ensure subject premium equals all premium at the manual level. Premium amounts
      *  at the Manual level are designated by an 'M' and subject premium is a 'W'.
      *
     c                   eval      prmLvlTotal = 0
     c                   eval      prmLvlAmt   = 0
     c                   eval      a = 1
     c                   eval      b = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'M'
     c                   select
     c                   when      sin(a) = '+'
     c                   add       amt(a)        prmLvlTotal
     c                   when      sin(a) = '-'
     c                   sub       amt(a)        prmLvlTotal
     c                   other
     c                   add       amt(a)        prmLvlTotal
     c                   endsl
     c                   endif
     c                   if        lvl(a) = 'V'
     c                   add       amt(a)        prmLvlAmt
     c                   eval      b = a
     c                   endif
     c                   eval      a = a + 1
     c                   enddo

     c                   if        prmLvlTotal <> prmLvlAmt
     c                   eval      dsp(b) = lightEmUpLine
     c                   eval      c = b + 25
     c                   eval      *in(c) = *on
     c                   eval      *in90 = *on
     c                   eval      *in31 = *on
     c                   endif
      *
      *  Ensure standard premium equals all premium experience mod level premium amounts 'E' and
      *  premium discounts applied to subject premium 'J'. Standard premium is a 'W'
      *
     c                   if        not(*in90)
     c                   eval      prmLvlTotal = 0
     c                   eval      prmLvlAmt   = 0
     c                   eval      a = 1
     c                   eval      b = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'E'
     c                   select
     c                   when      sin(a) = '+'
     c                   add       amt(a)        prmLvlTotal
     c                   when      sin(a) = '-'
     c                   sub       amt(a)        prmLvlTotal
     c                   other
     c                   add       amt(a)        prmLvlTotal
     c                   endsl
     c                   endif
     c                   if        lvl(a) = 'W'
     c                   add       amt(a)        prmLvlAmt
     c                   eval      b = a
     c                   endif
     c                   eval      a = a + 1
     c                   enddo

     c                   if        prmLvlTotal <> prmLvlAmt
     c                   eval      dsp(b) = lightEmUpLine
     c                   eval      c = b + 25
     c                   eval      *in(c) = *on
     c                   eval      *in90 = *on
     c                   endif
     c                   endif
      *
      *  Ensure normal premium equals standard premium 'W' plus the discounts applied to
      *  standard 'S'. Normal premium is an 'X'.
      *
     c                   if        not(*in90)
     c                   eval      prmLvlTotal = 0
     c                   eval      prmLvlAmt   = 0
     c                   eval      a = 1
     c                   eval      b = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'W' or
     c                             lvl(a) = 'S'
     c                   select
     c                   when      sin(a) = '+'
     c                   add       amt(a)        prmLvlTotal
     c                   when      sin(a) = '-'
     c                   sub       amt(a)        prmLvlTotal
     c                   other
     c                   add       amt(a)        prmLvlTotal
     c                   endsl
     c                   endif
     c                   if        lvl(a) = 'X'
     c                   add       amt(a)        prmLvlAmt
     c                   eval      b = a
     c                   endif
     c                   eval      a = a + 1
     c                   enddo

     c                   if        prmLvlTotal <> prmLvlAmt
     c                   eval      dsp(b) = lightEmUpLine
     c                   eval      c = b + 25
     c                   eval      *in(c) = *on
     c                   eval      *in90 = *on
     c                   endif
     c                   endif
      *
      *  Ensure annual premium equals normal 'X' plus any discounts applied to normal 'D' if
      *  the policy is audited.
      *
     c                   if        not(*in90)
     c                   eval      prmLvlTotal = 0
     c                   eval      prmLvlAmt   = 0
     c                   eval      a = 1
     c                   eval      b = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'X' or
     c                             lvl(a) = 'D'
     c                   select
     c                   when      sin(a) = '+'
     c                   add       amt(a)        prmLvlTotal
     c                   when      sin(a) = '-'
     c                   sub       amt(a)        prmLvlTotal
     c                   other
     c                   add       amt(a)        prmLvlTotal
     c                   endsl
     c                   endif
     c                   if        lvl(a) = 'Y'
     c                   add       amt(a)        prmLvlAmt
     c                   eval      b = a
     c                   endif
     c                   eval      a = a + 1
     c                   enddo

     c                   if        ELFAF = 'A'
     c                   if        prmLvlTotal <> prmLvlAmt
     c                   eval      dsp(b) = lightEmUpLine
     c                   eval      c = b + 25
     c                   eval      *in(c) = *on
     c                   eval      *in90 = *on
     c                   endif
     c                   else
     c                   eval      amt(b) = prmLvlTotal
     c                   write     scr01
     c                   endif                                                  ELFAF = 'A'
     c                   endif
      *
      *  Ensure annual premium 'Y' equals Prorated Annual premium 'Z' if audited or if the policy
      *  has a prorate factor of 1.
      *
     c                   if        not(*in90)
     c                   if        ELFAF = 'A' or ELPROR = 1
     c                   eval      prmLvlTotal = 0
     c                   eval      prmLvlAmt   = 0
     c                   eval      a = 1
     c                   eval      b = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'Y'
     c                   add       amt(a)        prmLvlTotal
     c                   endif
     c                   if        lvl(a) = 'Z'
     c                   add       amt(a)        prmLvlAmt
     c                   eval      b = a
     c                   endif
     c                   eval      a = a + 1
     c                   enddo

     c                   if        prmLvlTotal <> prmLvlAmt
     c                   eval      dsp(b) = lightEmUpLine
     c                   eval      c = b + 25
     c                   eval      *in(c) = *on
     c                   eval      *in90 = *on
     c                   endif
     c                   endif                                                  ELFAF = 'A'
     c                   endif
      *
      *  Reset total premium
      *
     c                   eval      a = 1
     c                   eval      prmLvlTotal = 0
     c                   dow       a < %elem(amt)
     c                   if        lvl(a) = 'Z' or lvl(a) = 'I'
     c                   add       amt(a)        prmLvlTotal
     c                   endif
     c                   if        lvl(a) = 'T'
     c                   eval      amt(a) = prmLvlTotal
     c                   write     scr01
     c                   leave
     c                   endif
     c                   eval      a = a + 1
     c                   enddo
      *
      *  If any premiums were changed, redisplay the screen one more time to show new
      *  calculated totals before exiting.
      *
     c                   if        %xfoot(svamt) <> %xfoot(amt)
     c                   eval      *in90 = *on
     c                   eval      svamt = amt
     c                   endif

     C                   endsr                                                  errck1
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
      *
      * Initialize all line items as Editable fields
      *
     c                   eval      a = 1
     c                   dow       a <= %elem(fld)
     c                   eval      dsp(a) = Editable
     c                   eval      a = a + 1
     c                   enddo
      *
      *  The following premium breakdown was borrowed from W0051R10 and modified accordingly.
      *
      *
      *  Manual Premium
      *
     C                   eval      a = 1
     C     ELFAF         ifeq      'A'
     C     ELAMNP        ifeq      'Y'
     C                   sub       ELAIN1        ELAMPR
     C                   sub       ELAIN2        ELAMPR
     C                   sub       ELAWAV        ELAMPR
     C                   endif                                                  ELAMNP=Y
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(1)  :
     c*****************                       ' '     :
     c*****************                       ELMAPR  :
     c*****************                       ' '     :
     c*****************                       'M'     :
     c*****************                       'ELMAPR'  )
      /Free
       If isContri = '1';
          strHdg = con(54);
       Else;
          strHdg = con(1);
       Endif;
        //*** beg chg ***** 12/31/20 *************************************
         //*** Callp  setScreen( strHdg : ' ' : ELMAPR : ' ' : 'M' : 'ELMAPR' );
       Callp  setScreen( strHdg : ' ' : ELAMPR : ' ' : 'M' : 'ELAMPR' );
        //*** beg chg ***** 12/31/20 *************************************
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   else
     C     ELEMNP        ifeq      'Y'
     C                   sub       ELEIN1        ELMAPR
     C                   sub       ELEIN2        ELMAPR
     C                   sub       ELEWAV        ELMAPR
     C                   endif                                                  ELEMNP=Y
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(1)  :
     c*****************                       ' '     :
     c*****************                       ELMAPR  :
     c*****************                       ' '     :
     c*****************                       'M'     :
     c*****************                      'ELMAPR'  )
      /Free
       If isContri = '1';
          strHdg = con(54);
       Else;
          strHdg = con(1);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELMAPR : ' ' : 'M' : 'ELMAPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   endif                                                  ELFAF=A

     C     ELEWAV        ifne      *ZERO
     C     ELWVFL        andeq     *BLANK
     C     ELAWAV        orne      *ZERO
     C     ELWVFL        andeq     *BLANK
     C                   move      'M'           ELWVFL
     C                   endif
     C*
     C* Add short rate penalty premium to manual
     C*
     C     RRSRAF        ifeq      'M'
     C     ELSRAM        andne     *ZERO
     C     RRSRAF        oreq      ' '
     C     ELSRAM        andne     *ZERO
     C                   callp     setScreen( con(45)  :
     c                                        ' '     :
     c                                        ELSRAM  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELSRAM'  )
     C                   endif                                                  RRSRAF IFEQ
     C*
     C* Deviation #1 applied to manual.
     C*
     C     RRDV1L        ifeq      'M'
     C     ELASP2        andne     *zero

     C     ELFAF         ifeq      'A'

     C                   if        ELADC2 < 0
     c                   eval      ELADC2 = ELADC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELADC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELADC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   else

     C                   if        ELEDC2 < 0
     c                   eval      ELEDC2 = ELEDC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELEDC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELEDC2'  )
     c                   endif                                                  ELEDC2 < 0

     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRDV1L ifeq
      *
      *  Load 2nd deviation if flagged at manual
      *
     C     RRDV2L        ifeq      'M'
     C     ELDEV2        andne     *zero

     C     ELFAF         ifeq      'A'

     C                   if        ELADV2 < 0
     c                   eval      ELADV2 = ELADV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELADV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELADV2'  )
     c                   endif                                                  ELADV2 < 0

     C                   else

     C                   if        ELEDV2 < 0
     c                   eval      ELEDV2 = ELEDV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELEDV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELEDV2'  )
     c                   endif                                                  ELEDV2 < 0

     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRDV2L IFEQ
      *
      *  WAIVER SURCHARGE
      *
     C     ELWVFL        ifeq      'M'
     C     ELWVTP        andne     *blank

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELAWAV  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELAWAV'  )
     C                   else
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELEWAV  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELEWAV'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRWVFL IFEQ
      *
      *  Increased limits
      *
     C     ELILCD        IFNE      *BLANKS

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(2)  :
     c                                        '  '    :
     c                                        ELAIN1  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELAIN1'  )
     C                   else
     C                   callp     setScreen( con(2)  :
     c                                        '  '    :
     c                                        ELEIN1  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELEIN1'  )
     C                   endif                                                   ELFAF=A

     C                   endif                                                  ELILCD IFNE
      *
      *  2nd Increased Limits
      *
     C     ELINL2        ifne      *blanks

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(20) :
     c                                        '  '    :
     c                                        ELAIN2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELAIN2'  )
     C                   else
     C                   callp     setScreen( con(20) :
     c                                        '  '    :
     c                                        ELEIN2  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELEIN2'  )
     C                   endif                                                   ELFAF=A

     C                   endif                                                  ELINL2 IFNE
      *
      *  Voluntary Labor Charge
      *
     C     ELVLCA        ifne      *zero
     C     ELEMNP        andne     'Y'
     C                   callp     setScreen( con(26) :
     c                                        RRVLCH  :
     c                                        ELVLCA  :
     c                                        '+'     :
     c                                        'M'     :
     c                                        'ELVLCA'  )
     C                   endif                                                  ELVLCA IFNE
      *
      *  Deductible discount - manual level
      *
     C     RRRDPR        ifeq      'M'
     C     ELDDCT        andne     *zero

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(3)  :
     c                                        ' '     :
     c                                        ELADCT  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELADCT'  )
     C                   else
     C                   callp     setScreen( con(3)  :
     c                                        ' '     :
     c                                        ELEDCT  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELEDCT'  )
     C                   endif                                                  ELFAF=A

     C     ELDD2P        ifne      *zero

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(41) :
     c                                        ' '     :
     c                                        ELADD2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELADD2'  )
     C                   else
     C                   callp     setScreen( con(41) :
     c                                        ' '     :
     c                                        ELEDD2  :
     c                                        '-'     :
     c                                        'M'     :
     c                                        'ELEDD2'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRRDPR IFEQ
      *
      *  Subject Premium
      *
     C     ELFAF         IFEQ      'A'
     C     ELAMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C                   SUB       ELAWAV        ELASBJ
     C                   ENDIF
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(22) :
     c*****************                       ' '     :
     c*****************                       ELASBJ  :
     c*****************                       '='     :
     c*****************                       'V'     :
     c*****************                       'ELASBJ'  )
      /Free
       If isContri = '1';
          strHdg = con(59);
       Else;
          strHdg = con(22);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELASBJ : '=' : 'V' : 'ELASBJ' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ELSE
     C     ELEMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C                   SUB       ELEWAV        ELESBJ
     C                   ENDIF
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(22) :
     c*****************                       ' '     :
     c*****************                       ELESBJ  :
     c*****************                       '='     :
     c*****************                       'V'     :
     c*****************                       'ELESBJ'  )
      /Free
       If isContri = '1';
          strHdg = con(59);
       Else;
          strHdg = con(22);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELESBJ :'=' : 'V' : 'ELESBJ' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ENDIF                                                  ELFAF IFEQ
      *
      *  MANAGED CARE CREDIT
      *
     C     ELASPM        ifne      *zero

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(4)  :
     c                                        RR1AHD  :
     c                                        ELADCM  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELADCM'  )
     C                   else
     C                   callp     setScreen( con(4)  :
     c                                        RR1AHD  :
     c                                        ELEDCM  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELEDCM'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELASPM IFNE
      *
      *  SAFETY CREDIT
      *
     C     ELASPS        ifne      *zero

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(5)  :
     c                                        RR1BHD  :
     c                                        ELADCS  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELADCS'  )
     C                   else
     C                   callp     setScreen( con(5)  :
     c                                        RR1BHD  :
     c                                        ELEDCS  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELEDCS'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELASPS<>*zero
      *
      *  Load first deviation to subject premium level
      *
     C     RRDV1L        ifeq      'J'
     C     ELASP2        andne     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADC2 < 0
     c                   eval      ELADC2 = ELADC2 * -1
     C                   callp     setScreen( con(6)  :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELADC2'  )
     c                   else
     C                   callp     setScreen( con(6)  :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELADC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   else

     C                   if        ELEDC2 < 0
     c                   eval      ELEDC2 = ELEDC2 * -1
     C                   callp     setScreen( con(6)  :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELEDC2'  )
     c                   else
     C                   callp     setScreen( con(6)  :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELEDC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRDV1L=M
      *
      *  Load 2nd Deviation to Subject Premium Level
      *
     C     RRDV2L        ifeq      'J'
     C     ELDEV2        andne     *zero

     C     ELFAF         ifeq      'A'

     C                   if        ELADV2 < 0
     c                   eval      ELADV2 = ELADV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELADV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELADV2'  )
     c                   endif                                                  ELADV2 < 0

     C                   else

     C                   if        ELEDV2 < 0
     c                   eval      ELEDV2 = ELEDV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELEDV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '-'     :
     c                                        'J'     :
     c                                        'ELEDV2'  )
     c                   endif                                                  ELEDV2 < 0

     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRDV2L=J
      *
      *  Waiver surcharge at subject premium level
      *
     C     ELWVFL        ifeq      'J'
     C     ELWVTP        andne     *BLANK

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELAWAV  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELAWAV'  )
     C                   else
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELEWAV  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELEWAV'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'J'

     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2

     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF

     C     EPSTAT        IFNE      'I'

     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF

     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ

     C     PXCODE        IFEQ      *ZERO
     C     XXOCCD        ANDEQ     *BLANK
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ

     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ

     C     XTADLD        IFGT      *ZERO
     C                   callp     setScreen( con(9)  :
     c                                        ' '     :
     c                                        XTADLD  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'XTADLD'  )
     C                   ENDIF                                                  XTADLD IFGT

     C                   ENDIF                                                  ELAMNP<>Y

     C                   ENDIF                                                  RRASFL IFEQ
      *
      * ADD SHORT RATE PENALTY PREMIUM TO SUBJECT PREMIUM.
      *
     C     RRSRAF        ifeq      'J'
     C     ELSRAM        andne     *ZERO
     C                   callp     setScreen( con(45)  :
     c                                        ' '     :
     c                                        ELSRAM  :
     c                                        '+'     :
     c                                        'J'     :
     c                                        'ELSRAM'  )
     C                   endif                                                  RRSRAF IFEQ
      *
      *  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
      *
     C     ELFAF         ifeq      'A'
     c                   select
     c                   when      ELAXP1 <> 0 or
     c                             ELAXP2 <> 0 or
     c                             ELAXP3 <> 0
     c                   if        ELAXP1 <> 0
     C                   callp     setScreen( con(47)  :
     c                                        ' '     :
     c                                        ELAXP1  :
     c                                        ' '     :
     c                                        'E'     :
     c                                        'ELAXP1'  )
     c                   endif
     c                   if        ELAXP2 <> 0
     C                   callp     setScreen( con(48)  :
     c                                        ' '     :
     c                                        ELAXP2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELAXP2'  )
     c                   endif
     c                   if        ELAXP3 <> 0
     C                   callp     setScreen( con(49)  :
     c                                        ' '     :
     c                                        ELAXP3  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELAXP3'  )
     c                   endif
     c                   other
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(53)  :
     c*****************                       ' '     :
     c*****************                       ELAMPM  :
     c*****************                       ' '     :
     c*****************                       'E'     :
     c*****************                       'ELAMPM'  )
      /Free
       If isContri = '1';
          strHdg = con(61);
       Else;
          strHdg = con(53);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELAMPM : ' ' : 'E' : 'ELAMPM' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     c                   endsl
     C                   else
     c                   select
     c                   when      ELEXP1 <> 0 or
     c                             ELEXP2 <> 0 or
     c                             ELEXP3 <> 0
     c                   if        ELEXP1 <> 0
     C                   callp     setScreen( con(47)  :
     c                                        ' '     :
     c                                        ELEXP1  :
     c                                        ' '     :
     c                                        'E'     :
     c                                        'ELEXP1'  )
     c                   endif
     c                   if        ELEXP2 <> 0
     C                   callp     setScreen( con(48)  :
     c                                        ' '     :
     c                                        ELEXP2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEXP2'  )
     c                   endif
     c                   if        ELEXP3 <> 0
     C                   callp     setScreen( con(49)  :
     c                                        ' '     :
     c                                        ELEXP3  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEXP3'  )
     c                   endif
     c                   other
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(53)  :
     c*****************                       ' '     :
     c*****************                       ELEMPM  :
     c*****************                       ' '     :
     c*****************                       'E'     :
     c*****************                       'ELEMPM'  )
      /Free
       If isContri = '1';
          strHdg = con(61);
       Else;
          strHdg = con(53);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELEMPM : ' ' : 'E' : 'ELEMPM' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     c                   endsl
     c                   endif                                                  ELFAF=A

      *
      *  CONTRACTOR CREDIT
      *
     C     ELASP1        ifne      *zero
     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(10)  :
     c                                        ' '     :
     c                                        ELADC1  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELADC1'  )
     C                   else
     C                   callp     setScreen( con(10)  :
     c                                        ' '     :
     c                                        ELEDC1  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELEDC1'  )
     C                   endif                                                  ELFAF=A
     C                   endif                                                  ELASP1 <> 0
      *
      *  SCHEDULE DB/CR
      *
     C     RRDCAP        ifeq      ' '

     C     ELFAF         ifne      'A'
     C     ELEMNP        andne     'Y'
     C     ELFAF         oreq      'A'
     C     ELAMNP        andne     'Y'

     C     ELFAF         ifeq      'A'

     C                   if        ELADC <> 0
     C                   if        ELADC < 0
     c                   eval      ELADC = ELADC * -1
     C                   callp     setScreen( con(19)  :
     c                                        RRCRHD  :
     c                                        ELADC   :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELADC'   )
     c                   else
     C                   callp     setScreen( con(19)  :
     c                                        RRDBHD  :
     c                                        ELADC   :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELADC'   )
     c                   endif
     c                   endif

     C                   else

     C                   if        ELEDC <> 0
     C                   if        ELEDC < 0
     c                   eval      ELEDC = ELEDC * -1
     C                   callp     setScreen( con(19)  :
     c                                        RRCRHD  :
     c                                        ELEDC   :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELEDC'   )
     c                   else
     C                   callp     setScreen( con(19)  :
     c                                        RRDBHD  :
     c                                        ELEDC   :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEDC'   )
     c                   endif
     c                   endif

     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELFAF ifne 'A'

     C                   endif                                                  RRDCAP IFNE
      *
      *
      *  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
      *
     C     RRDV1L        IFEQ      'E'
     C     ELASP2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADC2 < 0
     c                   eval      ELADC2 = ELADC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELADC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELADC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   else

     C                   if        ELEDC2 < 0
     c                   eval      ELEDC2 = ELEDC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEDC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELEDC2'  )
     c                   endif                                                  ELEDC2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV1L=E
      *
      *  LOAD 2nd DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
      *
     C     RRDV2L        IFEQ      'E'
     C     ELDEV2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADV2 < 0
     c                   eval      ELADV2 = ELADV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELADV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELADV2'  )
     c                   endif                                                  ELADV2 < 0

     C                   else

     C                   if        ELEDV2 < 0
     c                   eval      ELEDV2 = ELEDV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEDV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELEDV2'  )
     c                   endif                                                  ELEDV2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV2L=E
      *
      * Schedule DB/CR applied to Modified level
      *
     C     RRDCAP        IFEQ      'Y'
     C     ELFAF         ifne      'A'
     C     ELEMNP        andne     'Y'
     C     ELFAF         oreq      'A'
     C     ELAMNP        andne     'Y'

     C     ELFAF         ifeq      'A'

     C                   if        ELADC <> 0
     C                   if        ELADC < 0
     c                   eval      ELADC = ELADC * -1
     C                   callp     setScreen( con(19)  :
     c                                        RRCRHD  :
     c                                        ELADC   :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELADC'   )
     c                   else
     C                   callp     setScreen( con(19)  :
     c                                        RRDBHD  :
     c                                        ELADC   :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELADC'   )
     c                   endif
     c                   endif

     C                   else

     C                   if        ELEDC <> 0
     C                   if        ELEDC < 0
     c                   eval      ELEDC = ELEDC * -1
     C                   callp     setScreen( con(19)  :
     c                                        RRCRHD  :
     c                                        ELEDC   :
     c                                        '-'     :
     c                                        'E'     :
     c                                        'ELEDC'   )
     c                   else
     C                   callp     setScreen( con(19)  :
     c                                        RRDBHD  :
     c                                        ELEDC   :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEDC'   )
     c                   endif
     c                   endif

     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELFAF ifne 'A'
     C                   ENDIF
      *
      *  WAIVER SURCHARGE
      *
     C     ELWVFL        IFEQ      'E'
     C     ELWVTP        ANDNE     *BLANK

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELAWAV  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELAWAV'  )
     C                   else
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELEWAV  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'ELEWAV'  )
     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRWVFL IFEQ
      *
      *  ADDITIONAL LOAD APPLIED TO MODIFIED
      *
     C     RRASFL        IFEQ      'E'
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2

     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'

     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF

     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C     XXOCCD        ANDEQ     *BLANK
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ

     C     XTADLD        IFGT      *ZERO
     C                   callp     setScreen( con(9)  :
     c                                        ' '     :
     c                                        XTADLD  :
     c                                        '+'     :
     c                                        'E'     :
     c                                        'XTADLD'  )
     C                   ENDIF                                                  XTADLD IFGT

     C                   ENDIF                                                  ELAMNP<>Y

     C                   ENDIF                                                  RRASFL IFEQ
      *
      *  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
      *
     C     RRRDPR        IFEQ      'S'
     C     ELDDCT        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(3)  :
     c                                        ' '     :
     c                                        ELADCT  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADCT'  )
     C                   else
     C                   callp     setScreen( con(3)  :
     c                                        ' '     :
     c                                        ELEDCT  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDCT'  )
     C                   endif                                                  ELFAF=A

     C     ELDD2P        ifne      *zero

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(41) :
     c                                        ' '     :
     c                                        ELADD2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADD2'  )
     C                   else
     C                   callp     setScreen( con(41) :
     c                                        ' '     :
     c                                        ELEDD2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDD2'  )
     C                   endif                                                  ELFAF=A

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRRDPR=S
      *
      *  Loss Constant
      *
     C     ELLOSS        IFNE      *ZERO
     C                   callp     setScreen( con(26) :
     c                                        ' '     :
     c                                        ELLOSS  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELLOSS'  )
     C                   ENDIF                                                  ELLOSS
      *
      *  Occupational Disease Charge
      *
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2

     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF

     C     EPSTAT        IFNE      'I'

     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF

     C     XXOCCD        IFEQ      'Y'
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF

     C                   ENDIF

     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ

     C     XTADLD        IFGT      *ZERO
     C                   callp     setScreen( con(33) :
     c                                        ' '     :
     c                                        XTADLD  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'XTADLD'  )
     C                   ENDIF                                                  XTADLD IFGT

     C                   ENDIF                                                  ELAMNP<>Y
     C*
     C* ADD SHORT RATE PENALTY PREMIUM TO STANDARD PREMIUM.
     C*
     C     RRSRAF        IFEQ      'S'
     C     ELSRAM        ANDNE     *ZERO
     C                   callp     setScreen( con(45)  :
     c                                        ' '     :
     c                                        ELSRAM  :
     c                                        '+'     :
      *** beg chg ***** 05/11/09 *******************************************************
     c***********                             'S'     :
     c                                        'E'     :
      *** end chg ***** 05/11/09 *******************************************************
     c                                        'ELSRAM'  )
     C                   ENDIF                                                  RRSRAF IFEQ
      *
      *  STANDARD PREMIUM
      *
     C     ELFAF         IFEQ      'A'

     C     ELAMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
     C                   SUB       ELAWAV        ELASPR
     C                   ENDIF
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(11)  :
     c*****************                       ' '     :
     c*****************                       ELASPR  :
     c*****************                       '='     :
     c*****************                       'W'     :
     c*****************                       'ELASPR'  )
      /Free
       If isContri = '1';
          strHdg = con(55);
       Else;
          strHdg = con(11);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELASPR : '= ' : 'W' : 'ELASPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ELSE
     C     ELEMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
     C                   SUB       ELEWAV        ELSTPR
     C                   ENDIF
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(11)  :
     c*****************                       ' '     :
     c*****************                       ELSTPR  :
     c*****************                       '='     :
     c*****************                       'W'     :
     c*****************                       'ELSTPR'  )
      /Free
       If isContri = '1';
          strHdg = con(55);
       Else;
          strHdg = con(11);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELSTPR : '= ' : 'W' : 'ELSTPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ENDIF                                                  ELFAF=A
      *
      *  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
      *
     C     RRDV1L        IFEQ      'S'
     C     ELASP2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADC2 < 0
     c                   eval      ELADC2 = ELADC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELADC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   else

     C                   if        ELEDC2 < 0
     c                   eval      ELEDC2 = ELEDC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEDC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDC2'  )
     c                   endif                                                  ELEDC2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV1L=S
      *
      *  LOAD 2nd DEVIATION IF APPLIED TO STANDARD PREMIUM
      *
     C     RRDV2L        IFEQ      'S'
     C     ELDEV2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADV2 < 0
     c                   eval      ELADV2 = ELADV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELADV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADV2'  )
     c                   endif                                                  ELADV2 < 0

     C                   else

     C                   if        ELEDV2 < 0
     c                   eval      ELEDV2 = ELEDV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEDV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDV2'  )
     c                   endif                                                  ELEDV2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV2L=S
      *
      *  Waiver surcharge applied to standard
      *
     C     ELWVFL        IFEQ      'S'
     C     ELWVTP        ANDNE     *BLANK

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELAWAV  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELAWAV'  )
     C                   else
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELEWAV  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEWAV'  )
     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'S'

     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2

     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'

     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF

     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C     XXOCCD        ANDEQ     *BLANK
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ

     C     XTADLD        IFGT      *ZERO
     C                   callp     setScreen( con(9)  :
     c                                        ' '     :
     c                                        XTADLD  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'XTADLD'  )
     C                   ENDIF                                                  XTADLD IFGT

     C                   ENDIF                                                  ELAMNP<>Y

     C                   ENDIF                                                  RRASFL IFEQ
      *
      *  STOCK DISCOUNT
      *
     C     ELEDC3        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELADC3        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'

     C     ELFAF         IFEQ      'A'
     C                   callp     setScreen( con(12) :
     c                                        RRDSHD  :
     c                                        ELADC3  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADC3'  )
     C                   ELSE
     C                   callp     setScreen( con(12) :
     c                                        RRDSHD  :
     c                                        ELEDC3  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDC3'  )
     C                   ENDIF                                                  ELFAF=A

     C                   ENDIF                                                  ELASP3 IFNE
      *
      *  LOAD DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
      *
     C     RRDV1L        IFEQ      'D'
     C     ELASP2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADC2 < 0
     c                   eval      ELADC2 = ELADC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELADC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELADC2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADC2'  )
     c                   endif                                                  ELADC2 < 0

     C                   else

     C                   if        ELEDC2 < 0
     c                   eval      ELEDC2 = ELEDC2 * -1
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEDC2'  )
     c                   else
     C                   callp     setScreen( con(6)   :
     c                                        RRDV1H  :
     c                                        ELEDC2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDC2'  )
     c                   endif                                                  ELEDC2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV1L=D
      *
      *  LOAD 2nd DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
      *
     C     RRDV2L        IFEQ      'D'
     C     ELDEV2        ANDNE     *ZERO

     C     ELFAF         ifeq      'A'

     C                   if        ELADV2 < 0
     c                   eval      ELADV2 = ELADV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELADV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELADV2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELADV2'  )
     c                   endif                                                  ELADV2 < 0

     C                   else

     C                   if        ELEDV2 < 0
     c                   eval      ELEDV2 = ELEDV2 * -1
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEDV2'  )
     c                   else
     C                   callp     setScreen( con(18) :
     c                                        RRDV2H  :
     c                                        ELEDV2  :
     c                                        '-'     :
     c                                        'S'     :
     c                                        'ELEDV2'  )
     c                   endif                                                  ELEDV2 < 0

     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRDV2L=D
      *
      *  WAIVER SURCHARGE
      *
     C     ELWVFL        IFEQ      'D'
     C     ELWVTP        ANDNE     *BLANK

     C     ELFAF         ifeq      'A'
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELAWAV  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELAWAV'  )
     C                   else
     C                   callp     setScreen( con(25) :
     c                                        RRWVHD  :
     c                                        ELEWAV  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELEWAV'  )
     C                   endif                                                  ELFAF=A

     C                   ENDIF                                                  RRWVFL IFEQ
      *
      * ADDITIONAL LOAD
      *
     C     RRASFL        IFEQ      'D'

     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   Z-ADD     *ZERO         XTADLD           11 2

     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'

     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF

     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C     XXOCCD        ANDEQ     *BLANK
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C                   ENDIF
     C     EPKY1         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ

     C     XTADLD        IFGT      *ZERO
     C                   callp     setScreen( con(9)  :
     c                                        ' '     :
     c                                        XTADLD  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'XTADLD'  )
     C                   ENDIF                                                  XTADLD IFGT

     C                   ENDIF                                                  ELAMNP<>Y

     C                   ENDIF                                                  RRASFL IFEQ
      *
      *  MASS ARAP SURCHARGE
      *
     C     ELFAF         IFNE      'A'

     c                   if        ELRPE1 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPE1  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPE1'  )
     c                   endif

     c                   if        ELRPE2 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPE2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPE2'  )
     c                   endif

     c                   if        ELRPE3 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPE3  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPE3'  )
     c                   endif

     C                   ELSE

     c                   if        ELRPA1 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPA1  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPA1'  )
     c                   endif

     c                   if        ELRPA2 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPA2  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPA2'  )
     c                   endif

     c                   if        ELRPA3 <> 0
     C                   callp     setScreen( con(32) :
     c                                        ' '     :
     c                                        ELRPA3  :
     c                                        '+'     :
     c                                        'S'     :
     c                                        'ELRPA3'  )
     c                   endif

     C                   ENDIF
      *
      *  NORMAL PREMIUM
      *
     C     ELFAF         IFEQ      'A'
     C                   callp     setScreen( con(13)  :
     c                                        ' '     :
     c                                        ELANMP  :
     c                                        '='     :
     c                                        'X'     :
     c                                        'ELANMP'  )
     C                   ELSE
     C                   callp     setScreen( con(13)  :
     c                                        ' '     :
     c                                        ELENMP  :
     c                                        '='     :
     c                                        'X'     :
     c                                        'ELENMP'  )
     C                   ENDIF                                                  ELFAF=A
      *
      *  NORMAL PREMIUM
      *
     C     ELHXMN        IFGT      *ZERO
     C                   callp     setScreen( con(46)  :
     c                                        ' '     :
     c                                        ELHXMN  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELHXMN'  )
     C                   ENDIF                                                  ELHXMN
      *
      * Reinsurance Charge
      *
     C     ELEARC        IFNE      *ZEROS
     C     ELFAF         ANDNE     'A'
     C     ELAARC        ORNE      *ZEROS
     C     ELFAF         ANDEQ     'A'

     C     ELFAF         IFEQ      'A'
     C                   callp     setScreen( con(44)  :
     c                                        ' '     :
     c                                        ELAARC  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELAARC'  )
     C                   ELSE
     C                   callp     setScreen( con(44)  :
     c                                        ' '     :
     c                                        ELEARC  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELEARC'  )
     C                   ENDIF                                                  ELFAF=A

     C                   ENDIF
      *
      *  EXPENSE CONSTANT
      *
     C     ELFAF         IFEQ      'A'
     C     ELFFL9        OREQ      'Y'
     c                   z-add     ELPRTD        xxexco
     C                   callp     setScreen( con(14)  :
     c                                        ' '     :
     c                                        ELPRTD  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELPRTD'  )
     C                   ELSE
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
     c                   z-add     ELPRTD        xxexco
     C                   callp     setScreen( con(14)  :
     c                                        ' '     :
     c                                        ELPRTD  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELPRTD'  )
     C                   ELSE
     c                   z-add     ELPRTF        xxexco
     C                   callp     setScreen( con(14)  :
     c                                        ' '     :
     c                                        ELPRTF  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELPRTF'  )
     C                   ENDIF                                                  ELEMNP=Y
     C                   ENDIF                                                  ELFAF=A

     C     ELETER        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELATER        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'

     C     ELFAF         IFEQ      'A'
     C                   callp     setScreen( con(31)  :
     c                                        ' '     :
     c                                        ELATER  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELATER'  )

     C                   ELSE
     C                   callp     setScreen( con(31)  :
     c                                        ' '     :
     c                                        ELETER  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELETER'  )

     C                   ENDIF                                                  ELFAF=A

     C                   ENDIF                                                  ELETER IFEQ

     C     ELEDTR        IFNE      *ZERO
     C     ELFAF         ANDNE     'A'
     C     ELADTR        ORNE      *ZERO
     C     ELFAF         ANDEQ     'A'

     C     ELFAF         IFEQ      'A'
     C                   callp     setScreen( con(37)  :
     c                                        ' '     :
     c                                        ELADTR  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELADTR'  )
     C                   ELSE
     C                   callp     setScreen( con(37)  :
     c                                        ' '     :
     c                                        ELEDTR  :
     c                                        '+'     :
     c                                        'D'     :
     c                                        'ELEDTR'  )
     C                   ENDIF                                                  ELFAF=A

     C                   ENDIF                                                  ELETER IFEQ
      *
      *  ANNUAL PREMIUM
      *
     C     ELFAF         IFEQ      'A'
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(15)  :
     c*****************                       ' '     :
     c*****************                       ELAAPR  :
     c*****************                       '='     :
     c*****************                       'Y'     :
     c*****************                       'ELAAPR'  )
      /Free
       If isContri = '1';
          strHdg = con(56);
       Else;
          strHdg = con(15);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELAAPR : '= ' : 'Y' : 'ELAAPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ELSE
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
     c                   eval      xxanpr = ELENMP + ELETER + ELEDTR + ELEARC
     c                   eval      dsp(a) = displayOnly
     C     ELEMNP        IFEQ      'Y'
     C                   Z-ADD     ELANPR        xxanpr
     C                   ENDIF
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(15)  :
     c*****************                       ' '     :
     c*****************                       xxanpr  :
     c*****************                       '='     :
     c*****************                       'Y'     :
     c*****************                       'xxanpr'  )
      /Free
       If isContri = '1';
          strHdg = con(56);
       Else;
          strHdg = con(15);
       Endif;
       Callp  setScreen( strHdg : ' ' : XXANPR : '= ' : 'Y' : 'XXANPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ELSE
     c                   eval      dsp(a) = displayOnly
     c                   eval      xxanpr = ELENMP + xxexco + ELETER
     c                                             + ELEDTR + ELEARC
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(15)  :
     c*****************                       ' '     :
     c*****************                       xxanpr  :
     c*****************                       '='     :
     c*****************                       'Y'     :
     c*****************                       'xxanpr'  )
      /Free
       If isContri = '1';
          strHdg = con(56);
       Else;
          strHdg = con(15);
       Endif;
       Callp  setScreen( strHdg : ' ' : XXANPR : '= ' : 'Y' : 'XXANPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ENDIF                                                  ELEMNP=Y
     C                   ENDIF                                                  ELFAF=A
      *
      *  Prorated Annual Premium
      *
     C     ELFAF         IFEQ      'A'
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(16)  :
     c*****************                       ' '     :
     c*****************                       ELAAPR  :
     c*****************                       ' '     :
     c*****************                       'Z'     :
     c*****************                       'ELAAPR'  )
      /Free
       If isContri = '1';
          strHdg = con(57);
       Else;
          strHdg = con(16);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELAAPR : ' ' : 'Z' : 'ELAAPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ELSE
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(16)  :
     c*****************                       ' '     :
     c*****************                       ELANPR  :
     c*****************                       ' '     :
     c*****************                       'Z'     :
     c*****************                       'ELANPR'  )
      /Free
       If isContri = '1';
          strHdg = con(57);
       Else;
          strHdg = con(16);
       Endif;
       Callp  setScreen( strHdg : ' ' : ELANPR : ' ' : 'Z' : 'ELANPR' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ENDIF
      *
      * LOAD RETROSPECTIVE PREMIUM VALUES (Not used for SCS - Note in Issues in Project Spec's)
      *
     C*                  Z-ADD     0             XXRETA           11 2
     C*    ELSTAM        IFNE      *ZERO
     C*    A             ANDLT     25
     C*                  MOVEL     CON(42)       HDG(A)
     C*    ELFAF         IFEQ      'A'
     C*    ELSTAM        SUB       ELAAPR        TAM(A)
     C*                  ELSE
     C*    ELSTAM        SUB       ELANPR        TAM(A)
     C*                  ENDIF                                                  ELFAF IFEQ
     C*                  Z-ADD     TAM(A)        XXRETA
     C*                  ADD       1             A
     C*                  MOVEL     CON(43)       HDG(A)
     C*                  Z-ADD     ELSTAM        TAM(A)
     C*                  ENDIF                                                  ELSTAM IFNE
      *
      * LOAD STATE TAX FIELD
      *
     C     ELTXDU        IFNE      *ZERO
     C                   callp     setScreen( con(17)  :
     c                                        RRTXHD  :
     c                                        ELTXDU  :
     c                                        '+'     :
     c                                        'I'     :
     c                                        'ELTXDU'  )
     C                   ENDIF
      *
      * LOAD TOTAL PREMIUM FIELD
      *
      *** beg del ***** 05/12/09 **********************************************
     c***********        eval      dsp(a) = displayOnly
      *** end del ***** 05/12/09 **********************************************
     C     ELTXDU        IFNE      *ZERO
      *** beg add ***** 05/12/09 **********************************************
     c                   eval      dsp(a) = displayOnly
      *** end add ***** 05/12/09 **********************************************
     C     ELFAF         IFEQ      'A'
     C     ELAAPR        ADD       ELTXDU        xxTotal
     C                   ELSE
     C     ELANPR        ADD       ELTXDU        xxTotal
     C                   ENDIF                                                  ELFAF IFEQ
      ***************** Beg Chg ************** 10/07/13 **********************
     C*****************  callp     setScreen( con(21)  :
     c*****************                       ' '     :
     c*****************                       xxTotal :
     c*****************                       '='     :
     c*****************                       'T'     :
     c*****************                       'xxTotal' )
      /Free
       If isContri = '1';
          strHdg = con(58);
       Else;
          strHdg = con(21);
       Endif;
       Callp  setScreen( strHdg : ' ' : XXTOTAL : '= ' : 'T' : 'XXTOTAL' );
      /End-free
      ***************** End Chg ************** 10/07/13 **********************
     C                   ENDIF                                                  ELTXDU IFEQ
     C*                  ENDIF                                                  A IFLT
      *
     C*                  Z-ADD     ELTCLP        SCTCLP
     C*                  Z-ADD     ELOUTS        SCOUTS
     C*                  Z-ADD     ELTCLC        SCTCLC
     C*                  Z-ADD     ELT#CL        SCT#CL
     C*                  Z-ADD     ELMFAM        SCMFAM
     C*                  Z-ADD     ELSFAM        SCSFAM
     c                   if        EL1AAM <> 0
     C                   callp     setScreen( con(28)  :
     c                                        RR1STH  :
     c                                        EL1AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL1AAM' )
     c                   endif
     c                   if        EL2AAM <> 0
      *** beg chg ***** 12/31/20 **************************
     C***                callp     setScreen( con(29)  :
     c***                                     RR2NDH  :
     c***                                     EL2AAM  :
     c***                                     ' '     :
     c***                                     ' '     :
     c***                                     'E21AAM' )
     c                   callp     setScreen( con(29)  :
     c                                        RR2NDH  :
     c                                        EL2AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL2AAM' )
      *** end chg ***** 12/31/20 **************************
     c                   endif
     c                   if        EL3AAM <> 0
     C                   callp     setScreen( con(30)  :
     c                                        RR3RDH  :
     c                                        EL3AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL3AAM' )
     c                   endif
     c                   if        EL4AAM <> 0
     C                   callp     setScreen( con(34)  :
     c                                        RR4THH  :
     c                                        EL4AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL4AAM' )
     c                   endif
     c                   if        EL5AAM <> 0
     C                   callp     setScreen( con(35)  :
     c                                        RR5THH  :
     c                                        EL5AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL5AAM' )
     c                   endif
     c                   if        EL6AAM <> 0
     C                   callp     setScreen( con(39)  :
     c                                        RR6THH  :
     c                                        EL6AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL6AAM' )
     c                   endif
     c                   if        EL7AAM <> 0
     C                   callp     setScreen( con(40)  :
     c                                        RR7THH  :
     c                                        EL7AAM  :
     c                                        ' '     :
     c                                        ' '     :
     c                                        'EL7AAM' )
     c                   endif
      *
      * Light up second column headings if enough premium line items exist
      *
     c                   if        a < 18
     c                   eval      itHd = nonDisplay
     c                   eval      prHd = nonDisplay
     c                   else
     c                   eval      itHd = lightEmUp
     c                   eval      prHd = lightEmUp
     c                   endif
      *
      * Do not display the unused screen lines
      *
     c                   dow       a <= %elem(fld)
     c                   eval      dsp(a) = nonDisplay
     c                   eval      a = a + 1
     c                   enddo
      *
      * Save off amount array for a later comparison to see if any changes were made.
      *
     c                   eval      svamt = amt

     C                   endsr                                                  MVDBSC
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     c                   eval      a = 1
     c                   dow       a <= %elem(fld)

     c                   select
     c                   when      fld(a) = 'ELAMPR'
     c                   eval      ELAMPR =  amt(a)
     c                   if        ELAMNP = 'Y'
     c                   eval      ELAMPR = ELAMPR +
     c                                      ELAIN1 +
     c                                      ELAIN2 +
     c                                      ELAWAV
     c                   endif

     c                   when      fld(a) = 'ELMAPR'
     c                   eval      ELMAPR =  amt(a)
     c                   if        ELEMNP = 'Y'
     c                   eval      ELMAPR = ELMAPR +
     c                                      ELEIN1 +
     c                                      ELEIN2 +
     c                                      ELEWAV
     c                   endif

     c                   when      fld(a) = 'ELSRAM'
     c                   eval      ELSRAM =  amt(a)

     c                   when      fld(a) = 'ELADC2'
     c                   if        sin(a) = '-'
     c                   eval      ELADC2 = amt(a)
     c                   else
     c                   eval      ELADC2 = amt(a) * -1
     c                   endif

     c                   when      fld(a) = 'ELEDC2'
     c                   if        sin(a) = '-'
     c                   eval      ELEDC2 = amt(a)
     c                   else
     c                   eval      ELEDC2 = amt(a) * -1
     c                   endif

     c                   when      fld(a) = 'ELADV2'
     c                   if        sin(a) = '-'
     c                   eval      ELADV2 = amt(a)
     c                   else
     c                   eval      ELADV2 = amt(a) * -1
     c                   endif

     c                   when      fld(a) = 'ELEDV2'
     c                   if        sin(a) = '-'
     c                   eval      ELEDV2 = amt(a)
     c                   else
     c                   eval      ELEDV2 = amt(a) * -1
     c                   endif

     c                   when      fld(a) = 'ELAWAV'
     c                   eval      ELAWAV =  amt(a)

     c                   when      fld(a) = 'ELEWAV'
     c                   eval      ELEWAV =  amt(a)

     c                   when      fld(a) = 'ELAIN1'
     c                   eval      ELAIN1 =  amt(a)

     c                   when      fld(a) = 'ELEIN1'
     c                   eval      ELEIN1 =  amt(a)

     c                   when      fld(a) = 'ELAIN2'
     c                   eval      ELAIN2 =  amt(a)

     c                   when      fld(a) = 'ELEIN2'
     c                   eval      ELEIN2 =  amt(a)

     c                   when      fld(a) = 'ELVLCA'
     c                   eval      ELVLCA =  amt(a)

     c                   when      fld(a) = 'ELADCT'
     c                   eval      ELADCT =  amt(a)

     c                   when      fld(a) = 'ELEDCT'
     c                   eval      ELEDCT =  amt(a)

     c                   when      fld(a) = 'ELADD2'
     c                   eval      ELADD2 =  amt(a)

     c                   when      fld(a) = 'ELEDD2'
     c                   eval      ELEDD2 =  amt(a)

     C                   when      fld(a) = 'ELASBJ'
     c                   eval      ELASBJ =  amt(a)
     c                   if        ELAMNP = 'Y' and ELWVFL = 'M'
     c                   eval      ELASBJ = ELASBJ + ELAWAV
     c                   endif

     C                   when      fld(a) = 'ELESBJ'
     c                   eval      ELESBJ =  amt(a)
     c                   if        ELEMNP = 'Y' and ELWVFL = 'M'
     c                   eval      ELESBJ = ELESBJ + ELEWAV
     c                   endif

     c                   when      fld(a) = 'ELAXP1'
     c                   eval      ELAXP1 =  amt(a)
     c                   eval      ELAMPM =  ELAXP1

     c                   when      fld(a) = 'ELAXP2'
     c                   eval      ELAXP2 =  amt(a)
     c                   eval      ELAMPM =  ELAXP1 + ELAXP2

     c                   when      fld(a) = 'ELAXP3'
     c                   eval      ELAXP3 =  amt(a)
     c                   eval      ELAMPM =  ELAXP1 + ELAXP2 + ELAXP3

     c                   when      fld(a) = 'ELAMPM'
     c                   eval      ELAMPM =  amt(a)

     c                   when      fld(a) = 'ELEXP1'
     c                   eval      ELEXP1 =  amt(a)
     c                   eval      ELEMPM =  ELEXP1

     c                   when      fld(a) = 'ELEXP2'
     c                   eval      ELEXP2 =  amt(a)
     c                   eval      ELEMPM =  ELEXP1 + ELEXP2

     c                   when      fld(a) = 'ELEXP3'
     c                   eval      ELEXP3 =  amt(a)
     c                   eval      ELEMPM =  ELEXP1 + ELEXP2 + ELEXP3

     c                   when      fld(a) = 'ELEMPM'
     c                   eval      ELEMPM =  amt(a)

     c                   when      fld(a) = 'ELADC'
     c                   if        sin(a) = '-'
     c                   eval      ELADC  = amt(a) * -1
     c                   else
     c                   eval      ELADC  = amt(a)
     c                   endif

     c                   when      fld(a) = 'ELEDC'
     c                   if        sin(a) = '-'
     c                   eval      ELEDC  = amt(a) * -1
     c                   else
     c                   eval      ELEDC  = amt(a)
     c                   endif

     c                   when      fld(a) = 'ELLOSS'
     c                   eval      ELLOSS =  amt(a)

     c                   when      fld(a) = 'ELADC3'
     c                   eval      ELADC3 =  amt(a)

     c                   when      fld(a) = 'ELEDC3'
     c                   eval      ELEDC3 =  amt(a)

     c                   when      fld(a) = 'ELASPR'
     c                   eval      ELASPR =  amt(a)
     c                   if        ELAMNP = 'Y' and ELWVFL <> 'M'
     C                                          and ELWVFL <> 'J'
     C                                          and ELWVFL <> 'E'
     c                   eval      ELASPR = ELASPR + ELAWAV
     c                   endif

     c                   when      fld(a) = 'ELSTPR'
     c                   eval      ELSTPR =  amt(a)
     c                   if        ELEMNP = 'Y' and ELWVFL <> 'M'
     C                                          and ELWVFL <> 'J'
     C                                          and ELWVFL <> 'E'
     c                   eval      ELSTPR = ELSTPR + ELEWAV
     c                   endif

     c                   when      fld(a) = 'ELANMP'
     c                   eval      ELANMP =  amt(a)

     c                   when      fld(a) = 'ELENMP'
     c                   eval      ELENMP =  amt(a)

     c                   when      fld(a) = 'ELHXMN'
     c                   eval      ELHXMN =  amt(a)

     c                   when      fld(a) = 'ELRPE1'
     c                   eval      ELRPE1 =  amt(a)

     c                   when      fld(a) = 'ELRPE2'
     c                   eval      ELRPE2 =  amt(a)

     c                   when      fld(a) = 'ELRPE3'
     c                   eval      ELRPE3 =  amt(a)

     c                   when      fld(a) = 'ELRPA1'
     c                   eval      ELRPA1 =  amt(a)

     c                   when      fld(a) = 'ELRPA2'
     c                   eval      ELRPA2 =  amt(a)

     c                   when      fld(a) = 'ELRPA3'
     c                   eval      ELRPA3 =  amt(a)

     c                   when      fld(a) = 'ELEARC'
     c                   eval      ELEARC =  amt(a)

     c                   when      fld(a) = 'ELAARC'
     c                   eval      ELAARC =  amt(a)

     c                   when      fld(a) = 'ELPRTD'
     c                   eval      ELPRTD =  amt(a)

     c                   when      fld(a) = 'ELPRTF'
     c                   eval      ELPRTF =  amt(a)

     c                   when      fld(a) = 'ELETER'
     c                   eval      ELETER =  amt(a)

     c                   when      fld(a) = 'ELATER'
     c                   eval      ELATER =  amt(a)

     c                   when      fld(a) = 'ELEDTR'
     c                   eval      ELEDTR =  amt(a)

     c                   when      fld(a) = 'ELADTR'
     c                   eval      ELADTR =  amt(a)

     c                   when      fld(a) = 'ELANPR'
     c                   eval      ELANPR =  amt(a)

     c                   when      fld(a) = 'ELAAPR'
     c                   eval      ELAAPR =  amt(a)

     c                   when      fld(a) = 'ELTXDU'
     c                   eval      ELTXDU =  amt(a)

     c                   when      fld(a) = 'EL1AAM'
     c                   eval      EL1AAM =  amt(a)

     c                   when      fld(a) = 'EL2AAM'
     c                   eval      EL2AAM =  amt(a)

     c                   when      fld(a) = 'EL3AAM'
     c                   eval      EL3AAM =  amt(a)

     c                   when      fld(a) = 'EL4AAM'
     c                   eval      EL4AAM =  amt(a)

     c                   when      fld(a) = 'EL5AAM'
     c                   eval      EL5AAM =  amt(a)

     c                   when      fld(a) = 'EL6AAM'
     c                   eval      EL6AAM =  amt(a)

     c                   when      fld(a) = 'EL7AAM'
     c                   eval      EL7AAM =  amt(a)

     C                   endsl

     c                   eval      a = a + 1
     c                   enddo

     C                   endsr                                                  mvscdb
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   select
     c                   when      *inkg = *on
     c                   eval      *in90 = *on
     c     @@rrn         chain     dbrcdf
     c     @@elrrn       chain     pel0p
     c                   update    dbrcdf
     c                   exsr      mvdbsc
     c                   exfmt     scr01
     c                   endsl
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C*
    C*================================================================
     C     pgmstr        begsr
      *
      *  Get policy info and ratings and statistics record
      *
     c     @@elrrn       chain     pel0p
     c                   eval      scfnd = ELFND
     c                   eval      scfyr = ELFYR
     c     rsky          setgt     prs0l
     c     rsky          readpe    prs0l
     c     rrky1         setgt     prr0l
     c     rrky          readpe    prr0l
     c     fnky          chain     pfn0l
     c     emky          chain     pem0l
     c                   call      'WA005R'
     c                   parm                    ELEMP#
     c                   parm                    ELDIV
     c                   parm                    EMNAME
     c                   parm                    scemdn
     c                   eval      scfndn = FNNAME
     c     elky          chain     pe30l                              60
     c                   eval      @@rrn = e3lrrn
      ***************** Beg Add ************** 10/07/13 *********************
      /Free
       isContri = GetPrmContFlg(elCo#:elFnd);
       If isContri = '1';
          optnm = '3rd Party Contribuiton Update';
          flxHeading = 'Contribution';
       Else;
          optnm = '3rd Party Premium Update';
          flxHeading = 'Premium';
       Endif;
      /End-free
      ***************** End Add ************** 10/07/13 *********************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wae3p
     C                   endsr
      *===========================================================================
      * setScreen - send premium values to screen
      *===========================================================================
     p setScreen       b
     d setScreen       pi
     d   defaultHdg                  22    const
     d   customHdg                   20    const
     d   amount                            like(ELANPR) const
     d   sign                         1    const
     d   level                        1    const
     d   field                        6    const

     c                   if        customHDG <> *blanks
     c                   eval      dsc(a) = customHdg
     c                   else
     c                   eval      dsc(a) = defaultHdg
     c                   endif
     c                   eval      amt(a) = amount
     c                   eval      sin(a) = sign
     c                   eval      lvl(a) = level
     c                   eval      fld(a) = field
     c                   eval      a = a + 1

     p setScreen       e
      *===========================================================================

**  CON
Manual premium
Incr limit
Deductible
MC credit
Safety credit
Deviation
Subtotal
Exp mod
Addl load
Contr credit
Standard prem
Discount
Total normal
Expense const
Annual prem
Prorated prem
State tax
2nd deviation
Schedule db/cr
2nd Inc limits
Total premium
Subject prem
Surcharge
2nd surcharge
Waiver surcharge
Loss Constant
Safety Fee
1st Assessment
2nd Assessment
3rd Assessment
Foreign Terr Chg
Mass ARAP Surcharge
Occ Disease Cvrge
4th Assessment
5th Assessment
Vol Labor Chg
Domestic Terr Chg
Company Factor
6th Assessment
7th Assessment
2nd Deductible
Retro adjustment
Retrospective premium
Facultative Reinsurance                                     07/06/06
Short rate penalty
Harbor Exposure Fee
Exp Mod #1
Exp Mod #2
Exp Mod #3
Mass ARAP #1
Mass ARAP #2
Mass ARAP #3
Modified Premium
Manual Contri.
Standard Contri.
Annual Contri.
Prorated Contri.
Total Contri.
Subject Contri.
Retrospective Contri.
Modified Contri.
Not Used
Not Used
