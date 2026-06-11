     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H*                                                                   *
     H*  06/07/16  C:65  Project: SY180/Phw#41866                         *
     H*                  Add processing for two additional license keys.  *
     H*                                                                   *
     H*  04/25/02  C:69  Support:                                         *
     H*                  Changed the number of array elements for the     *
     H*                  options and functions arrays from 12 to 14       *
     H*                                                                   *
     H*  03/15/02  C:69  Support: 31640                                   *
     H*                  Redid program so that F23 and F24 would show on  *
     H*                  the first page if necessary. (Changes were not   *
     H*                  identified.)                                     *
     H*                                                                   *
     H*  11/01/00  C:33  PROJECT: SY043                                   *
     H*                  CHANGE PROGRAM ALLOW FOR MULTIPLE ENTRIES OF THE *
     H*                  OPTION/FUNCTION SECURITY FILE.                   *
     H*                                                                   *
     H*  11/11/99  C:69  PROJECT:                                         *
     H*                  Changed logic when invalid call type parameter   *
     H*                  is passed, then no options/functions will show   *
     H*                  instead of all showing.                          *
     H*********************************************************************
     Fsdofl     if   e           k disk    usropn
     Fsmupl     if   e           k disk    usropn
     Fssypf     if   e             disk    usropn
     *
     D ar              s             80    dim(20)
     D pos             s              3  0 dim(19) descend
     D va              s              1    dim(99)
     D of              s              1    dim(100)                             Options, Function
     D ofd             s             35    dim(100)
     *
     D ol              s             80    dim(20)                              Option lines
     D fl              s             80    dim(20)                              Function lines
     D vo              s              1    dim(99)                              Valid options
     D vf              s              1    dim(24)                              Valid functions
     *
     D psds          esds                  extname(szq1p)
     *
     D***  BEGIN ADD  ***  11/01/00  **************************************
     Dsrccsc           PR                  extpgm('SRCCSC')
     D userType                       1    const
     D userLevel                      3    const
     D clntType                       1    const
     D clntLevel                      3    const
     D type                           1    const
     D failed                         1    const
     D clntType2                      1    const options(*nopass)
     D clntType3                      1    const options(*nopass)
     D clntType4                      1    const options(*nopass)
     D clntType5                      1    const options(*nopass)
     D clntType6                      1    const options(*nopass)
     D***  END   ADD  ***  11/01/00  **************************************
     *
     D                 ds
     D  ty                     1      6
     D                                     dim(6)
     D  ofty01                 1      1
     D  ofty02                 2      2
     D  ofty03                 3      3
     D  ofty04                 4      4
     D  ofty05                 5      5
     D  ofty06                 6      6
     D                 ds
     D data                          35
     D  id                            2  0 overlay(data)
     D  sfds                         25    overlay(data:3)
     D  sfmntf                        1    overlay(data:28)
     D  sfinqf                        1    overlay(data:29)
     D  sfself                        1    overlay(data:30)
     D  sfns                          1    overlay(data:31)
     *
     D f23             c                   const('F23=More options')
     D f24             c                   const('F24=More keys')

     D##pasWrd         s             28
     D##serNum         s              7
     D##expDate        s               d
     D##entDate        s               d
     D##days           s              5  0
     D##model          s              4
     D##error          s              1

     Dpassword         ds
     D  syal01                       10
     D  syal02                       10
     D  syal03                       10
     D  syal04                       10
      ****** beg add ***** 06/07/16 ***************************************

     DpassWordQA       ds
     D  syqa01                       10
     D  syqa02                       10
     D  syqa03                       10
     D  syqa04                       10

     DpassWordDR       ds
     D  sydr01                       10
     D  sydr02                       10
     D  sydr03                       10
     D  sydr04                       10
      ****** end add ***** 06/07/16 ***************************************

     *---------------------------------------------------------------------
     * BEGIN of work fields
     *---------------------------------------------------------------------
     D @@pgm           s             10
     D @@wwmo          s              1
     D a               s              3  0
     D chkupf          s              1
     D disp            s              1
     D dsl             s              3  0
     D funcl           s              3  0
     D holdsfds        s                   like(sfds)
     D holds           s                   like(s)
     D holddsl         s                   like(dsl)
     D htsfty          s              1
     D ix              s              3  0
     D l               s              3  0
     D len             s              3  0
     D more2           s              1
     D n               s              1  0
     D odd             s              1
     D omore2          s              1
     D opnof           s              1
     D opnup           s              1
     D chkPasWrd       s              1
     D s               s              3  0
     D t               s              3  0
     D currlngth       s              3  0
     D rsrvSpcs        s                   like(currlngth)
     D xx              s                   like(ix)
     D***  BEGIN ADD  ***  11/01/00  **************************************
     D failed          s              1
     D userLevel       s              3
     D clntLevel       s              3
     D***  END   ADD  ***  11/01/00  **************************************
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     DchkPasswrd       PR             1
     D inPasWrd                      32    Const
     D outSerNbr                      7
     D outExpDate                      d
     D outEntDate                      d
     D outDays                        5  0
     D outModel                       4
     DgetSpaces        PR             3  0
     D                                3  0 Value
     D

     C     *entry        plist
     C                   parm                    @@wwmo
     C                   parm                    of
     C                   parm                    ofd
     C                   parm                    ol
     C                   parm                    fl
     C                   parm                    vo
     C                   parm                    vf
     C                   parm                    @@pgm
     *
     C     ofkey         klist
     C                   kfld                    @@pgm
     C                   kfld                    htsfty
     C                   kfld                    id
     *
B01  C                   if        chkPasWrd = *blanks
   4 C                   open      ssypf
     C                   movel     *blanks       passWord
     C     1             chain     ssypf                              99
     C                   movel     passWord      ##pasWrd
     C                   eval      ##error = chkPasswrd(##paswrd : ##serNum :
     C                             ##expDate : ##entDate : ##days : ##model)
      ****** beg add ***** 06/07/16 ***************************************
     c                   if        ##error = *on
     C                   movel     passWordQA    ##pasWrd
     C                   eval      ##error = chkPasswrd(##paswrd : ##serNum :
     C                             ##expDate : ##entDate : ##days : ##model)
     c                   if        ##error = *on
     C                   movel     passWordDR    ##pasWrd
     C                   eval      ##error = chkPasswrd(##paswrd : ##serNum :
     C                             ##expDate : ##entDate : ##days : ##model)
     c                   endif
     c                   endif
      ****** end add ***** 06/07/16 ***************************************
   4 C                   close     ssypf
   3 C                   move      *on           chkPasWrd
E013 C                   endif
     C
B01  C                   if        ##error = *on OR
     C                             cvtToDate(*date) >= ##expDate
     C                   move      *off          vo
     C                   move      *off          vf
     C                   move      *on           vf(3)
     C*                  move      *on           vf(12)
     C                   return
E01  C                   endif
     C
     C                   move      *off          chkupf
B011 C                   if        q1prms > 7
B022 C                   if        opnof = *blanks
   2 C                   open      sdofl                                98
B033 C                   if        *in98 = *off
   3 C                   move      *on           opnof
E033 C                   endif
E022 C                   endif
 001 *
B022 C                   if        opnof = *on
   2 C     @@pgm         chain     sdofl                              80
B033 C                   if        *in80 = *off
B044 C                   if        opnup = *blanks
   4 C                   open      smupl
   4 C                   move      *on           opnup
E044 C                   endif
   3 C                   eval      uptype = *blanks
   3 C                   eval      uplvl = *zeros
   3 C     q1user        chain     smupl                              81
   3 C                   move      *on           chkupf
E033 C                   endif
E022 C                   endif
E011 C                   endif
     *
     C                   move      *off          omore2
     *
     C                   move      '1'           htsfty
     C                   eval      len = 76
     C                   exsr      bldlin
     C                   movel     ar            ol
     C                   movel     va            vo
     *
     C                   move      '2'           htsfty
     C                   eval      len = 78
     C                   exsr      bldlin
     C                   movel     ar            fl
     C                   movel     va            vf
     C                   return
     *=====================================================================
     *  ADDLIN - Add description to current line
     *=====================================================================
     C     addlin        begsr
     C     ' '           checkr    sfds          dsl
     *
     C                   exsr      dtrmnSpcs
     C                   eval      currlngth = currlngth + s + dsl
     C                   if        omore2 = *on
   1 C                             and odd = *off
   1 C                             and dsl < 16
     C                   eval      rsrvSpcs = rsrvSpcs - dsl + 17
     C                   endif
     C                   if        htsfty = '1'
     C                   eval      rsrvSpcs = 4
     C                   endif
     *
B011 C*******            if        currlngth + rsrvSpcs >= len
B011 C                   if        currlngth + rsrvSpcs > len
B022 C                   if        odd = *on
   2 C                   exsr      push
   2 C                   move      *off          odd
   2 C                   eval      l = l + 1
   2 C                   eval      s = s - s
   2 C                   eval      currlngth = s + dsl
B022 C                   if        htsfty = '2'
     C                   eval      rsrvSpcs = 0
     C                   if        omore2 = *on                                 reserve for F23
     C                   eval      rsrvSpcs = rsrvSpcs + 16 + 1
     C                   endif
     C                   endif
X022 C                   else
   2 C                   eval      currlngth = currlngth - dsl
     C                   select
B033 C                   when      htsfty = '1'
   3 C                   eval      currlngth = currlngth - s
B033 C                   when      htsfty = '2'
   3 C                   eval      rsrvSpcs = rsrvSpcs + 13
E033 C                   endsl
B033 C                   if        currlngth + rsrvSpcs < len
     C                   select
B044 C                   when      htsfty = '1'
   4 C     ar(l)         cat       '...':1       ar(l)
B044 C                   when      htsfty = '2'
   4 C     ar(l)         cat       f24:s         ar(l)
E044 C                   endsl
   3 C                   eval      l = l + 1
   3 C                   eval      s = 0
   3 C                   eval      currlngth = s + dsl
     C                   eval      rsrvSpcs = 0
X033 C                   else
   3 C                   eval      a = holddsl
   3 C     holdsfds:a    scan      ar(l)         funcl
   3 C                   eval      funcl = funcl - 1
   3 C     funcl         subst(p)  ar(l):1       ar(l)
     C                   select
B044 C                   when      htsfty = '1'
   4 C     ar(l)         cat       '...':1       ar(l)
B044 C                   when      htsfty = '2'
     C                   if        omore2 = *off
   4 C     ar(l)         cat       f24:holds     ar(l)
     C                   else
   4 C     ar(l)         cat       f23:holds     ar(l)
     C                   exsr      resetCurr
     C                   eval      rsrvSpcs = rsrvSpcs - 17
     C                   eval      s = 3
     C                   exsr      dtrmnSpcs
   4 C     ar(l)         cat       f24:s         ar(l)
     C                   endif
E044 C                   endsl
E044 C
   3 C                   eval      l = l + 1
   3 C     ar(l)         cat       holdsfds:0    ar(l)
   3 C                   eval      s = 3
   3 C                   eval      currlngth = holddsl + s
   3 C                   eval      currlngth = currlngth + dsl
B011 C                   if        htsfty = '2'
     C                   eval      rsrvSpcs = 0
E033 C                   endif
E033 C                   endif
   2 C                   move      *on           more2
   2 C                   exsr      reset
E022 C                   endif
E011 C                   endif
     *
B011 C                   if        odd = *on
   1 C                   exsr      push
E011 C                   endif
     *
     C     ar(l)         cat       sfds:s        ar(l)
     C                   move      sfds          holdsfds
     C                   move      dsl           holddsl
     C                   move      s             holds
B011 C                   if        s <> 3
   1 C                   eval      s = 3
E011 C                   endif
     C                   endsr
     *=====================================================================
     *  BLDLIN - Build entire set of lines for options or functions
     *=====================================================================
     C     bldlin        begsr
     C                   eval      ar = *blanks
     C                   move      *off          va
     C                   eval      l = 1
     C                   eval      s = 0
     C                   eval      currlngth = 0
     C                   eval      rsrvSpcs = 0
     C                   move      *off          more2
     C                   exsr      reset
     *
     C                   eval      t = 1
B011 C                   dou       *in99 = *off
   1 C     htsfty        lookup    of(t)                                  99
B022 C                   if        *in99 = *on
   2 C                   movel     ofd(t)        data
   2 C                   exsr      chksf
B033 C                   if        disp = 'Y'
   3 C                   exsr      addlin
E033 C                   endif
   2 C                   eval      t = t + 1
E022 C                   endif
E011 C                   enddo
     *
B011 C                   if        omore2 = *on
   1 C                   movel(p)  f23           sfds
     C                   eval      rsrvSpcs = 0
   1 C                   exsr      addlin
   1 C                   move      *on           va(23)
E011 C                   endif
     *
B011 C                   if        more2 = *on
     C                   select
B022 C                   when      htsfty = '1'
   2 C     ar(l)         cat       '...':1       ar(l)
   2 C                   move      *on           omore2
B022 C                   when      htsfty = '2'
   2 C                   movel(p)  f24           sfds
   2 C                   exsr      addlin
   2 C                   move      *on           va(24)
E022 C                   endsl
E011 C                   endif
     C                   endsr
     *================================================================
     *  CHKSF  - Check if option/function should in line and valid
     *================================================================
     C     chksf         begsr
     C                   move      'Y'           disp
     **
     C                   select
B011 C                   when      @@wwmo = ' '
   1 C                   if        sfmntf <> 'Y'
   1 C                   eval      disp = *blanks
E011 C                   endif
     *
B011 C                   when      @@wwmo = '1'
   1 C                   if        sfinqf <> 'Y'
   1 C                   eval      disp = *blanks
E011 C                   endif
     *
B011 C                   when      @@wwmo = '2'
   1 C                   if        sfself <> 'Y'
   1 C                   eval      disp = *blanks
E011 C                   endif
     *
     C                   other
   1 C                   eval      disp = *blanks
     C                   endsl
     **
B011 C                   if        disp = 'Y'
   1 C                             and chkupf = *on
     C***  BEGIN DEL  ***  11/01/00  **************************************
   1 C**********                   and @@wwmo <> '1'
     C***  END   DEL  ***  11/01/00  **************************************
   1 C                   exsr      upchk
E011 C                   endif
     *
B011 C                   if        disp = 'Y'
   1 C                   move      *on           va(id)
E011 C                   endif
     *
B011 C                   if        sfns = 'Y'
   1 C                   eval      disp = *blanks
E011 C                   endif
     C                   endsr
     *================================================================
     *  PUSH   - Push first line position value into stack
     *================================================================
     C     push          begsr
     C                   eval      ix = ix - 1
     C                   eval      pos(ix) = currlngth + 1 - dsl
     C                   endsr
     *================================================================
     *  RESET  - Reset valid for new pair of lines
     *================================================================
     C     reset         begsr
     C                   move      *on           odd
     C                   eval      ix = 20
     C                   eval      pos = *zeros
     C                   endsr
     *================================================================
     *  UPCHK  - Check user profile for special authority
     *================================================================
     C     upchk         begsr
     C     ofkey         chain     sdofl                              71
     C*
     C***  BEGIN CHG  ***  11/01/00  **************************************
B011 C**********         if        *in71 = *off
 001 **********
B022 C**********         if        uptype <> '*'                                All types
B033 C**********         if        uptype = *blanks
   3 C**********         eval      disp = *blanks
E033 C**********         endif
 002 **********
B033 C**********         if        uplvl < oflvl
   3 C**********         eval      disp = *blanks
E033 C**********         endif
 002 **********
   2 C*****uptype        lookup    ty                                     88
B033 C**********         if        *in88 = *off                                 No match on typ
   3 C**********         eval      disp = *blanks
   3 C**********         eval      n = 1
B044 C**********         dou       n > 6
   4 C**********                   or disp = 'Y'
B055 C**********         select
S015 C**********         when      uptype = '#'
B066 C**********         if        ty(n) >= 'A'
   6 C**********                   and ty(n) <= 'M'
   6 C**********         move      'Y'           disp
E066 C**********         endif
S025 C**********         when      uptype = '$'
B066 C**********         if        ty(n) >= 'N'
   6 C**********                   and ty(n) <= 'Z'
   6 C**********         move      'Y'           disp
E066 C**********         endif
S035 C**********         when      uptype = '&'
B066 C**********         if        ty(n) >= 'A'
   6 C**********                   and ty(n) <= 'Z'
   6 C**********                   or ty(n) = '#'
   6 C**********                   or ty(n) = '$'
   6 C**********         move      'Y'           disp
E066 C**********         endif
S045 C**********         when      uptype = '%'
B066 C**********         if        ty(n) >= '0'
   6 C**********                   and ty(n) <= '9'
   6 C**********         move      'Y'           disp
E066 C**********         endif
E055 C**********         endsl
   4 C**********         eval      n = n + 1
E044 C**********         enddo
E033 C**********         endif
 002 **********
E022 C**********         endif
E011 C**********         endif
B011 C                   dow       not *in71
     C*
     C                   eval      disp = *blank
     C*
     C                   move      uplvl         userLevel
     C                   move      oflvl         clntLevel
     C*
     C                   callp     srccsc(uptype:userLevel:ofty01:clntLevel:
     C                                    '1':failed:ofty02:ofty03:ofty04:
     C                                    ofty05:ofty06)
     C*
     C                   if        failed = *off
     C                   eval      disp = 'Y'
     C                   leave
     C                   endif
     C*
     C     ofkey         reade     sdofl                                  71
     C*
     C                   enddo
     C***  END   CHG  ***  11/01/00  **************************************
     C                   endsr
     *=====================================================================
     *  resetCurr - resets field back to actual current length
     *=====================================================================
     C     resetCurr     begsr
     C     ' '           checkr    ar(l)         currlngth
     C                   endsr
     *=====================================================================
     *  dtrmnSpcs - determine spaces for next item on even line
     *=====================================================================
     C     dtrmnSpcs     begsr
B011 C                   if        odd = *off
   1 C                   if        currlngth + rsrvSpcs < pos(ix)
     C                   exsr      resetCurr
     C                   eval      s = getSpaces(currlngth)
   1 C                   eval      currlngth = currlngth - 1
E011 C                   endif
E011 C                   endif
     C                   endsr
     P*=====================================================================
     PgetSpaces        B
     DgetSpaces        PI             3  0
     D startPos                       3  0 value
     D
     D firstItem       s              3  0
     D spaces          s              3  0
     C
   1 C                   eval      firstItem = ix
   1 C                   eval      startPos = startPos + 4
   1 C     startPos      lookup    pos(firstItem)                     91  91
     C                   eval      spaces = pos(firstItem) - startPos + 3
     C                   return    spaces
     PgetSpaces        E

