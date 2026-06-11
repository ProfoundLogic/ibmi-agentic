     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/title NCIGF Month Driver Program
    H*********************************************************************
    H*                                                                   *
    H*  Called from: W8165C                                              *
    H*  Calls to   : W8166C                                              *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 06/10/10 by c:220 for Project 114WWL                  *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                               *
     H* 12/14/10 C:191 PROJECT: 114WXR                                *
     H*                Add processing for inception-to-date extract   *
     H*                processing                                     *
    H*                                                                   *
    H*********************************************************************
     D pMend           s              5
     D xxmeDt          s              7  0
     D xxqeDt1         s              7  0
     D xxqeDt2         s              7  0
     D xxqeDt3         s              7  0
     D xxqeDt4         s              7  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xmnbr           s              2  0
     D@@date           s              8  0
     D@@months         s              3  0
     D@@err            s              1
     D @dsdt           ds
     D @mth                    1      2
     D @day                    3      4
     D @fyr                    5      8
     C*********************************************************************
     C* Main Loop
     C*********************************************************************
     C*
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@medt            7
     C                   parm                    @@naic            5
     C                   parm                    @@mth             2
     C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C                   move      @@mth         @1mth             2 0
     C                   move      @@medt        @1medt            7
     C*
    C* Loop the call number of months entered
     C*
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C* Store input value for later use
     C                   move      @@mth         @omth             2 0
     C* Reset loop variable to 1, to execute routine once if inception-to-date is requested
     C                   if        @1mth = 0
     C                   eval      @1mth = 1
     C                   endif
     C*** END   ADD ****** 12/14/10 ************************************
     C                   eval      xmnbr = @1mth
     C*
     C                   do        @1mth
     C*
     C                   if        @1mth > 1
     C*
     C* Decrement counter for next month end date
     C*
     C                   eval      xmnbr = xmnbr - 1
     C*
     C* Determine the month-end date
     C*
     C                   movel     @@medt        xxedt             7 0
     C                   z-add     xxedt         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         x1medt            8 0
     C*
     C                   call      'SRPMTH8'
     C                   parm      x1medt        @@date
     C                   parm      xmnbr         @@months
     C                   parm                    @@err
     C*
     C                   Move      @@date        @dsdt
     C*
     C*
     C                   select
     c                   when      @mth ='01'
     c                   movel     '0131'        @@date
     c                   when      @mth ='02'
     c                   movel     '0229'        @@date
     c                   z-add     @@date        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr='1'
     c                   movel     '0228'        @@date
     c                   endif
     c                   when      @mth ='03'
     c                   movel     '0331'        @@date
     c                   when      @mth ='04'
     c                   movel     '0430'        @@date
     c                   when      @mth ='05'
     c                   movel     '0531'        @@date
     c                   when      @mth ='06'
     c                   movel     '0630'        @@date
     c                   when      @mth ='07'
     c                   movel     '0731'        @@date
     c                   when      @mth ='08'
     c                   movel     '0831'        @@date
     c                   when      @mth ='09'
     c                   movel     '0930'        @@date
     c                   when      @mth ='10'
     c                   movel     '1031'        @@date
     c                   when      @mth ='11'
     c                   movel     '1130'        @@date
     c                   when      @mth ='12'
     c                   movel     '1231'        @@date
     c                   endsl
     C*
     C                   z-add     @@date        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         @1medt            7
     C*
     C                   endif
     C*
     C*
     C* Determine if the month-end is also the quarter-end
     C*
     C*
     C                   move      @@co#         @1co#             3
     C                   movel     @1medt        xxmeDt
     C                   move      @@naic        @1naic            5
     C*
     C                   eval      pMend='N'
     C                   move      xxmeDt        xxqeDt1
     C                   move      0331          xxqeDt1
     C                   move      xxmeDt        xxqeDt2
     C                   move      0630          xxqeDt2
     C                   move      xxmeDt        xxqeDt3
     C                   move      0930          xxqeDt3
     C                   move      xxmeDt        xxqeDt4
     C                   move      1231          xxqeDt4
     C*
     C                   if        xxmeDt=xxqedt1 or xxmeDt=xxqedt2 or
     C                             xxmeDt=xxqedt3 or xxmeDt=xxqedt4
     C                   eval      pMend='Y'
     C                   endif
     C*
     C                   eval      @1mend = pMend
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C* If @omth = 0, then inception-to-date extract is requested
     C                   if        @omth = 0
     C                   eval      @1itdt = 'Y'
     C                   else
     C                   eval      @1itdt = ' '
     C                   endif
     C*** END   ADD ****** 12/14/10 ************************************
     C*
     C                   call      'W8166C'
     C                   parm                    @1co#             3
     C                   parm                    @1medt            7
     C                   parm                    @1naic            5
     C                   parm                    @1mend            2
     C*** BEGIN ADD ****** 12/14/10 ************************************
     C                   parm                    @1itdt            1
     C*** END   ADD ****** 12/14/10 ************************************
     C*
     C                   eval      pMend='N'
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
