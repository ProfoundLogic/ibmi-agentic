     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 11/10/10  C:33  Project: 139WPL                                   *
     H*                 Allow for a single function key line              *
     H*********************************************************************
     Fsdofl     if   e           k disk    usropn
     Fsmupl     if   e           k disk    usropn
     *
     D ar              s             78    dim(14)
     D pos             s              3  0 dim(19) descend
     D va              s              1    dim(99)
     D sf              s              1    dim(100)                             Options, Function
     D sfd             s             35    dim(100)
     *
     D fl              s             78    dim(14)                              Function lines
     D vf              s              1    dim(24)                              Valid functions
     *
     D psds          esds                  extname(szq1p)
     *
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
     D  sfChgF                        1    overlay(data:28)
     D  sfDspF                        1    overlay(data:29)
     D  sfAddF                        1    overlay(data:30)
     D  sfns                          1    overlay(data:31)
     *
     D f24             c                   const('F24=More keys')

     *---------------------------------------------------------------------
     * BEGIN of work fields
     *---------------------------------------------------------------------
     D @@pgm           s             10
     D @@maintMode     s              1
     D @@scr#          s              1
     D***  Begin Add  ***  11/10/10  **************************************
     D @@#ofLines      s              1  0
     D***  End   Add  ***  11/10/10  **************************************
     D a               s              3  0
     D chkupf          s              1n
     D disp            s              1n
     D dsl             s              3  0
     D funcl           s              3  0
     D holdsfds        s                   like(sfds)
     D holds           s                   like(s)
     D holddsl         s                   like(dsl)
     D htSfTy          s              1
     D ix              s              3  0
     D l               s              3  0
     D len             s              3  0
     D more2           s              1n
     D n               s              1  0
     D odd             s              1
     D openSDOFL       s              1n
     D openSMUPL       s              1n
     D chkPasWrd       s              1
     D s               s              3  0
     D t               s              3  0
     D currlngth       s              3  0
     D rsrvSpcs        s                   like(currlngth)
     D xx              s                   like(ix)
     D failed          s              1
     D userLevel       s              3
     D clntLevel       s              3
     D***  Begin Add  ***  11/10/10  **************************************
     D #ofLines        s              1  0 inz(2)
     D***  End   Add  ***  11/10/10  **************************************
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     DgetSpaces        PR             3  0
     D                                3  0 Value
     D

     C     *entry        plist
     C                   parm                    @@maintMode
     C                   parm                    sf
     C                   parm                    sfd
     C                   parm                    fl
     C                   parm                    vf
     C                   parm                    @@scr#
     C                   parm                    @@pgm
     C***  Begin Add  ***  11/10/10  **************************************
     C                   parm                    @@#ofLines
     C*
     C                   if        %parms >= 8
     C                              and @@#ofLines > 0
     C                   eval      #ofLines = @@#ofLines
     C                   endif
     C***  End   Add  ***  11/10/10  **************************************
     *
     C     ofkey         klist
     C                   kfld                    @@pgm
     C                   kfld                    htSfTy
     C                   kfld                    id
     *
     C                   eval      chkUpF = *off
B011 C                   if        q1prms >= 7
B022 C                   if        not openSDOFL
   2 C                   open      sdofl                                98
B033 C                   if        *in98 = *off
   3 C                   eval      openSDOFL = *on
E033 C                   endif
E022 C                   endif
 001 *
B022 C                   if        openSDOFL
   2 C     @@pgm         chain     sdofl                              80
B033 C                   if        *in80 = *off
B044 C                   if        not openSMUPL
   4 C                   open      smupl
   4 C                   eval      openSMUPL = *on
E044 C                   endif
   3 C                   eval      uptype = *blanks
   3 C                   eval      uplvl = *zeros
   3 C     q1user        chain     smupl                              81
     C                   eval      chkUpF = *on
E033 C                   endif
E022 C                   endif
E011 C                   endif
     *
     C                   eval      htSfTy = @@scr#
     C                   eval      len = 78
     C                   exsr      bldlin
     C                   movel     ar            fl
     C                   movel     va            vf
     C                   return
     *
     *=====================================================================
     *  Add description to current line
     *=====================================================================
     C     AddLin        begsr
     C     ' '           checkr    sfds          dsl
     *
     C                   exsr      dtrmnSpcs
     C                   eval      currlngth = currlngth + s + dsl
     C                   eval      rsrvSpcs = 2
     *
B011 C                   if        currlngth + rsrvSpcs > len
     C***  Begin Add  ***  11/10/10  **************************************
     C                   select
     C                   when      #ofLines = 1
     C                   eval      odd = *off
     C                   endsl
     C***  End   Add  ***  11/10/10  **************************************
B022 C                   if        odd = *on
   2 C                   exsr      push
   2 C                   move      *off          odd
   2 C                   eval      l = l + 1
   2 C                   eval      s = 0
   2 C                   eval      currlngth = dsl
     C                   eval      rsrvSpcs = 0
X022 C                   else
   2 C                   eval      currlngth = currlngth - dsl
   3 C                   eval      rsrvSpcs = rsrvSpcs + 13
B033 C                   if        currlngth + rsrvSpcs < len
   4 C     ar(l)         cat       f24:s         ar(l)
   3 C                   eval      l = l + 1
   3 C                   eval      s = 0
   3 C                   eval      currlngth = s + dsl
     C                   eval      rsrvSpcs = 0
X033 C                   else
   3 C                   eval      a = holddsl
   3 C     holdsfds:a    scan      ar(l)         funcl
   3 C                   eval      funcl = funcl - 1
   3 C     funcl         subst(p)  ar(l):1       ar(l)
     C                   exsr      resetCurr
     C                   eval      rsrvSpcs = rsrvSpcs - 15
     C                   eval      s = 3
     C                   exsr      dtrmnSpcs
   4 C     ar(l)         cat       f24:s         ar(l)
E044 C
   3 C                   eval      l = l + 1
   3 C     ar(l)         cat       holdsfds:0    ar(l)
   3 C                   eval      s = 3
   3 C                   eval      currlngth = holddsl + s
   3 C                   eval      currlngth = currlngth + dsl
     C                   eval      rsrvSpcs = 0
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
     C                   eval      va = *off
     C                   eval      l = 1
     C                   eval      s = 0
     C                   eval      currlngth = 0
     C                   eval      rsrvSpcs = 0
     C                   move      *off          more2
     C                   exsr      reset
     *
     C                   eval      t = 1
B011 C                   dou       *in99 = *off
   1 C     htSfTy        lookup    sf(t)                                  99
B022 C                   if        *in99 = *on
   2 C                   movel     sfd(t)        data
   2 C                   exsr      ChkSf
B033 C                   if        disp
   3 C                   exsr      AddLin
E033 C                   endif
   2 C                   eval      t = t + 1
E022 C                   endif
E011 C                   enddo
     *
B011 C                   if        more2 = *on
   2 C                   movel(p)  f24           sfds
   2 C                   exsr      AddLin
   2 C                   move      *on           va(24)
E011 C                   endif
     C                   endsr
     *================================================================
     *  Check if function should in line and valid
     *================================================================
     C     ChkSf         begsr
     *
     C                   select
B011 C                   when      @@maintMode = 'C' and sfChgF = 'Y'
   1 C                   eval      disp = *on
B011 C                   when      @@maintMode = 'D' and sfDspF = 'Y'
   1 C                   eval      disp = *on
B011 C                   when      @@maintMode = 'A' and sfAddF = 'Y'
   1 C                   eval      disp = *on
     C                   other
   1 C                   eval      disp = *off
     C                   endsl
     *
B011 C                   if        disp and chkupf
   1 C                   exsr      upchk
E011 C                   endif
     *
B011 C                   if        disp
     C                   eval      va(id) = *on
E011 C                   endif
     *
B011 C                   if        sfns = 'Y'
   1 C                   eval      disp = *off
E011 C                   endif
     *
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
     C*
     C     ofkey         chain     sdofl                              71
     C*
B011 C                   dow       not *in71
     C*
     C                   eval      disp = *off
     C*
     C                   move      uplvl         userLevel
     C                   move      oflvl         clntLevel
     C*
     C                   callp     srccsc(uptype:userLevel:ofty01:clntLevel:
     C                                    '1':failed:ofty02:ofty03:ofty04:
     C                                    ofty05:ofty06)
     C*
     C                   if        failed = *off
     C                   eval      disp = *on
     C                   leave
     C                   endif
     C*
     C     ofkey         reade     sdofl                                  71
     C*
     C                   enddo
     C*
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
     C   91              eval      spaces = pos(firstItem) - startPos + 3
     C  n91              eval      spaces = 3
     C                   return    spaces
     PgetSpaces        E

