     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE New York State Specific Information Entry Program
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 07/14/2005 by C: 132 for Project 129WIX               *
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H*  10/12/10 C:257 Support:  Seq # 74482                             *
     H*                 Checkout for Recompilation.                       *
    H*                                                                   *
    H*  06/05/06 C:132 Support:  Seq # 51973                             *
     H*                 Added check to see if additional program calls    *
    H*                 should be made when depending on MODE (from       *
    H*                 program W0070RGA).                                *
    H*                                                                   *
    H* 05/23/06 C: 132 Support: WC132/52838                              *
    H*                 Correct Allow Add/UpDate when called in Inquiry   *
    H*                 Mode, which can be either a "1" or "Z" in xxcall. *
    H*                                                                   *
    H*********************************************************************
     FW0070FNY  cf   e             workstn
    F*
     Fwmcml1    if   e           k disk
     F*
     Fwmfnl     if   e           k disk
     F*
     Fwmeml     if   e           k disk
     F*
     Fwmnil     uf a e           k disk
     F*
     Fwmcxl     if   e           k disk
     D/COPY SCOPYPR01D
      *** BEGIN ADD *** 06/05/06 *******************************************
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0
      *** END ADD ***** 06/05/06 *******************************************
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('New York State Specific -
     D                                     Info')
     D comapi          c                   const('WA001R')
     D pos1            s              2  0
     D pos2            s              2  0
     D length          s              2  0
     D rzip            s              9
     D tzip            s              9
     D @@call          s              1
     D @@prds          s             40
     D @@rtcd          s              1
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D xxss#           ds             9
     D  ss1                    1      3
     D  ss2                    4      5
     D  ss3                    6      9
     D xxdiv           s              5
     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     D xxacdt          s              7
     D x               s              3  0
     D xxdate          ds             8
     D  mm                     1      2
     D  dd                     4      5
     D  ccyy                   6      8
     D xxtime          ds             4
     D  hr                            2  0
     D  mn                            2  0
     D*
     D kkco#           s                   like(cmco#)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkcase          s                   like(cmcase)
     D kkst            s                   like(cmjrst)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@rtcd
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     acdpl         plist
     C                   parm                    xxprds           40
     C                   parm                    xxcall
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm      *zero         xaacdt            8
     C                   parm                    xxrtcd            1
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
    C*  KEY LIST FOR CLAIM/DARTS CONTROL FILE
    C*
     C     cxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C* State NOI record file
     C*
     C     niky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkst
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
    C*
     C                   setoff                                       707172
     C                   setoff                                       737475
     C                   setoff                                       767778
     C                   setoff                                       798081
     C                   setoff                                       828384
     C                   setoff                                       858687
     C                   setoff                                       8889
    C*
     C                   do
     C*
     C* If F3 or F12, exit program
     C                   IF        *INKL OR *INKC
     C                   LEAVE
     C                   ENDIF
    C*
     C                   if        not *in09
     C*
     C* Normal Start Time range must be valid if entered & include the AM/PM field value
     C*
     C                   if        sctim2  < *zeros  OR
     C                             sctim2  > 235959
     C                   seton                                        7090
     C                   leave
     C                   endif
     C*
     C* Injured Employee work week
     C*
     C                   IF        scmon <> 'X' and  scmon <> *blanks
     C                   seton                                        8390
     C                   leave
     C                   EndIf
     C*
     C                   IF        sctue <> 'X' and  sctue <> *blanks
     C                   seton                                        8490
     C                   leave
     C                   EndIf
     C*
     C                   IF        scwed <> 'X' and  scwed <> *blanks
     C                   seton                                        8590
     C                   leave
     C                   EndIf
     C*
     C                   IF        scthu <> 'X' and  scthu <> *blanks
     C                   seton                                        8690
     C                   leave
     C                   EndIf
     C*
     C                   IF        scfri <> 'X' and  scfri <> *blanks
     C                   seton                                        8790
     C                   leave
     C                   EndIf
     C*
     C                   IF        scsat <> 'X' and  scsat <> *blanks
     C                   seton                                        8890
     C                   leave
     C                   EndIf
     C*
     C                   IF        scsun <> 'X' and  scsun <> *blanks
     C                   seton                                        8990
     C                   leave
     C                   EndIf
     C*
     C* Empolyer provided medical care ?
     C*
     C                   IF        sc1a1 <> *blanks  AND
     C                             sc1a1 <> 'Y' AND sc1a1 <> 'N'
     C                   seton                                        7690
     C                   leave
     C                   EndIf
     C*
     C* Medical Care Date must be valid if entered, must be entered if sc1a1 = Y
     C*
     C                   IF        scdt1 <> *zeros
     C                   z-add     scdt1         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7790
     C                   leave
     C                   else
     C                   movel     srdto         scdt1
     C                   endif
     C                   EndIf                                                  scdt1 <> *zeros
     C*
     C                   IF        scdt1 =  *zeros AND sc1a1 = 'Y'
     C                   seton                                        7890
     C                   leave
     C                   EndIf                                                  scdt1 = *zeros
     C*
     C* Employee treated in ER flag must be Y or N
     C*
     C                   if        sc1a2 <> *blank AND
     C                             sc1a2 <> 'Y' AND sc1a2 <> 'N'
     C                   seton                                        7990
     C                   leave
     C                   endif
     C*
     C* Employee in hospital overnight
     C*
     C                   if        sc1a3 <> *blank AND
     C                             sc1a3 <> 'N'  AND sc1a3 <> 'Y'
     C                   seton                                        8090
     C                   leave
     C*
     C                   endif
     C*
     C* Specific activity employee was preforming
     C*
     C*                  IF        sc60a5 = *blanks
     C*                  seton                                        8190
     C*                  leave
     C*                  EndIf
     C*
     C* How did the accident or exposure occur ?
     C*
     C                   IF        sc60a3  = *blanks
     C                   seton                                        8290
     C                   leave
     C                   EndIf
     C*
     C                   exsr      upwrtr
     C                   endif
     C*
     C                   dou       not *in90
     C                   exfmt     scr02
     C                   setoff                                       90
     C                   if        *inkl
     C                   seton                                        90
     C                   setoff                                       kl
     C                   leave
     C                   else
     C                   exsr      errck2
     C                   endif
     C                   enddo
     C*
     C                   enddo
     C*
     C                   endsr
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
    C*
     C     ERRCK2        begsr
    C*
     C                   do
     C*
     C* If F3 or F12, exit program
     C                   IF        *INKL OR *INKC
     C                   LEAVE
     C                   ENDIF
     C*
     C                   if        not *in09
     C*
     C* If Nearest Relative info entered, require Relationship be entered
     C*
     C                   IF        sc80a5a <> *blanks AND sc20a3 = *blanks
     C                   seton                                        7190
     C                   leave
     C                   EndIF
     C*
     C                   If        scprnm = *blanks
     C                   seton                                        7290
     C                   leave
     C                   endIF
     C*
     C                   If        scprti = *blanks
     C                   seton                                        7390
     C                   leave
     C                   endIF
     C*
     C                   If        scprph = *zeros
     C                   seton                                        7490
     C                   leave
     C                   endIF
     C*
     C                   exsr      upwrtr
     C                   endif
     C*
     C                   dou       not *in90
     C                   exfmt     scr03
     C                   setoff                                       90
     C                   if        *inkl
     C                   seton                                        90
     C                   setoff                                       kl
     C                   leave
     C                   else
     C                   exsr      errck3
     C                   endif
     C                   enddo
     C*
     C                   enddo
     C*
     C                   endsr
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
    C*
     C     ERRCK3        begsr
    C*
     C                   do
     C*
     C* If F3 or F12, exit program
     C                   IF        *INKL OR *INKC
     C                   LEAVE
     C                   ENDIF
     C*
     C                   if        not *in09
     C*
     C* If have 3rd Party Company Name, Address, must enter contact name & phone #
     C*
     C                   IF        sc80a7a <> *blanks AND sc40a5 = *blanks
     C                   seton                                        7090
     C                   leave
     C                   EndIF
     C*
     C                   IF        sc80a7a <> *blanks AND sc30a5 = *blanks
     C                   seton                                        7190
     C                   leave
     C                   EndIF
     C*
     C                   exsr      upwrtr
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr
    C*
    C*
     C                   endsr
    C*====================================================================
    C* MVDBSC - Move database to screen filed
    C*====================================================================
     C     MVDBSC        begsr
    C*
     C     niky          chain(n)  pni01
     C                   if        %found
     C*
     C                   movel     ni20a2        scscl#                         UI Employer Reg. #
     C*
     C                   eval      sctim2 = nitim2                              Normal Start Time
     C*
     C                   eval      sc1a1  = ni1a1                               Empr prov MED Care
     C                   eval      sc1a2  = ni1a2                               Empl Treat in EM?
     C                   eval      sc1a3  = ni1a3                               Empl hosp overnight?
     C*
     C                   z-add     ni5n1         sc5n1                          Tot Earn - 52 weeks
     C*
     C                   eval      x = %scan('1':ni10a1)
     C                   if        x <> 0
     c                   eval      scmon = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('2':ni10a1)
     C                   if        x <> 0
     c                   eval      sctue = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('3':ni10a1)
     C                   if        x <> 0
     c                   eval      scwed = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('4':ni10a1)
     C                   if        x <> 0
     c                   eval      scthu = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('5':ni10a1)
     C                   if        x <> 0
     c                   eval      scfri = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('6':ni10a1)
     C                   if        x <> 0
     c                   eval      scsat = 'X'
     C                   endif
     C*
     C                   eval      x = %scan('7':ni10a1)
     C                   if        x <> 0
     c                   eval      scsun = 'X'
     C                   endif
     C*
     C                   eval      sc20a3 = ni20a3                              Relationship
     C                   movel     ni30a5        sc30a5                         3rd Prty Cont Phone
     C*
     C                   eval      sc60a1 = ni60a1                              Work Activity
     C                   eval      sc60a2 = ni60a2
     C                   eval      sc60a3 = ni60a3                              How Injury Occurred
     C                   eval      sc60a4 = ni60a4
     C                   eval      sc60a5 = ni60a5                              How Injury Occurred
     C                   eval      sc60a6 = ni60a6
     C                   eval      sc30a3 = ni30a3
     C                   eval      sc40a4 = ni40a4
     C                   eval      sc40a5 = ni40a5
     C*
     C                   eval      sctim2 = nitim2                              Normal Start Time
     C*
     C                   movel     ni80a5        sc80a5a                        Nam Addr-Near Relatv
     C                   move      ni80a5        sc80a5b
     C*
     C                   movel     ni80a6        sc80a6a                        Nam Addr-Near Rel-2
     C                   eval      scrcty = %subst(ni80a6:41:25)
     C                   eval      scrst  = %subst(ni80a6:66:2)
     C                   eval      rzip   = %subst(ni80a6:68:9)
     C                   movel     rzip          scrzp1
     C                   move      rzip          scrzp2
     C*
     C                   movel     ni80a7        sc80a7a                        3rd Prty-Co Name &
     C                   move      ni80a7        sc80a7b
     C*
     C                   movel     ni80a8        sc80a8a                        3rd Prty-Co Addr -2
     C                   eval      sctcty = %subst(ni80a8:41:25)
     C                   eval      sctst  = %subst(ni80a8:66:2)
     C                   eval      tzip   = %subst(ni80a8:68:9)
     C                   movel     tzip          sctzp1
     C                   move      tzip          sctzp2
     C*
     C                   z-add     nidt1         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt1                          Med Care Date-Empr
     C*
     C                   if        niprnm <> *blanks
     C*
     C                   eval      niprnm = %trim(niprnm)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':niprnm:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      scprnm = %subst(niprnm:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      scprti = %subst(niprnm:pos1:19)
     C*
     C                   else
     c                   clear                   scprnm
     c                   clear                   scprti
     c                   endif
     C*
     C                   move      niprph        scprph
     C*
     C                   else
     C*
     C                   clear                   sc1a1
     C                   clear                   sc1a2
     C                   clear                   sc1a3
     C                   clear                   scmon
     C                   clear                   sctue
     C                   clear                   scwed
     C                   clear                   scthu
     C                   clear                   scfri
     C                   clear                   scsat
     C                   clear                   scsun
     C                   clear                   sc30a3
     C                   clear                   sc40a4
     C                   clear                   sc20a3
     C                   clear                   sc30a5
     C                   clear                   sc60a1
     C                   clear                   sc60a2
     C                   clear                   sc60a3
     C                   clear                   sc60a4
     C                   clear                   sc60a5
     C                   clear                   sc60a6
     C                   clear                   sc80a5a
     C                   clear                   sc80a5b
     C                   clear                   sc80a6a
     C                   clear                   scrcty
     C                   clear                   scrst
     C                   clear                   scrzp1
     C                   clear                   scrzp2
     C                   clear                   sc80a7a
     C                   clear                   sc80a7b
     C                   clear                   sc80a8a
     C                   clear                   sctcty
     C                   clear                   sctst
     C                   clear                   sctzp1
     C                   clear                   sctzp2
     C                   clear                   sctim2
     C                   clear                   scdt1
     C                   clear                   scprnm
     C                   clear                   scprti
     C                   clear                   scprph
     C                   clear                   sc40a5
    C*
     C                   endif
    C*
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen to Database filed
    C*====================================================================
    C*
     C     MVSCDB        begsr
     C
     C                   eval      ni1a1  = sc1a1
     C                   eval      ni1a2  = sc1a2
     C                   eval      ni1a3  = sc1a3
     C*
     C                   clear                   ni10a1
     C                   if        scmon ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '1'
     C                   endif
     C*
     C                   if        sctue ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '2'
     C                   endif
     C*
     C                   if        scwed ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '3'
     C                   endif
     C*
     C                   if        scthu ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '4'
     C                   endif
     C*
     C                   if        scfri ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '5'
     C                   endif
     C*
     C                   if        scsat ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '6'
     C                   endif
     C*
     C                   if        scsun ='X'
     C                   eval      ni10a1 = %trim(ni10a1) + '7'
     C                   endif
     C*
     C                   eval      niprnm = %trim(scprnm) + ',' +
     C                                      %trim(scprti)
     C                   move      scprph        niprph
     C*
     C                   eval      ni20a3 = sc20a3
     C                   movel     sc30a5        ni30a5
     C                   eval      ni60a1 = sc60a1
     C                   eval      ni60a2 = sc60a2
     C                   eval      ni60a3 = sc60a3
     C                   eval      ni60a4 = sc60a4
     C                   eval      ni60a5 = sc60a5
     C                   eval      ni30a3 = sc30a3
     C                   eval      ni40a4 = sc40a4
     C                   eval      ni40a5 = sc40a5
     C*
     C                   z-add     sc5n1         ni5n1
     C*
     C                   eval      ni80a5 = sc80a5a + sc80a5b
     C                   movel     scrzp1        rzip
     C                   move      scrzp2        rzip
     C                   eval      ni80a6 = sc80a6a + scrcty + scrst + rzip
     c*
     C                   eval      ni80a7 = sc80a7a + sc80a7b
     C                   movel     sctzp1        tzip
     C                   move      sctzp2        tzip
     C                   eval      ni80a8 = sc80a6a + sctcty + sctst +tzip
     C*
     C                   eval      nitim2  = sctim2
     C*
     C                   z-add     scdt1         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt1
     C*
     C                   time                    dsdttm
     C*
     C                   eval      niuser = q1user
     C                   eval      nipgm  = q1pgm
     C                   eval      nitime = dstime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt
     C*
     C                   endsr
      *** BEGIN ADD *** 06/05/06 *******************************************
    C*====================================================================
    C* NXTPGM - Call next programs if set up to do so
    C*====================================================================
    C*
     C     NXTPGM        begsr
     C*
     C*  IF @@call = A, GO ON TO THE NEXT PROGRAM IN ADD MODE.
     C*  IF @@call = Y, GO ON TO THE NEXT PROGRAM IN CHANGE MODE.
     C*  IF @@call = Z, GO ON TO THE NEXT PROGRAM IN DISPLAY MODE.
     C*
     C                   do
     C                   if        @@call = 'Y' or @@call = 'Z'
     C                   move      '1'           xxrtcd
     C                   call      'W0190R'      acdpl                  60
     C                   if        xxrtcd = 'B'
     C                   seton                                        90
     C                   leave
     C                   endif
     C                   if        xxrtcd = 'A'
     C                   setoff                                       90
     C                   leave
     C                   endif
     C                   endif
      *
     C                   if        xxcall = 'A'
     C     cxky          chain     pcx01                              60
     C                   if        *in60 = *off
     C                   select
     C                   when      cxdben = 'Y'
     C                   move      '1'           xxrtcd
     C                   call      'W0190R'      acdpl                  60
     C                   when      cxdart = 'Y'
     C                   move      *blanks       qakey
     C                   call      'SC010R'
     C                   parm      qckey         qakey            40
     C                   parm      'W0070R'      qqpgm            10
     C                   endsl
      *
     C                   if        xxrtcd = 'B'
     C                   seton                                        90
     C                   leave
     C                   endif
      *
     C                   if        xxrtcd = 'A'
     C                   setoff                                       90
     C                   leave
     C                   endif
     C                   endif
     C                   endif
      *
     C                   leave
     C                   enddo
     C                   endsr
      *** END ADD ***** 06/05/06 *******************************************
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
    C*
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
      ***** BEG ADD ***** 05/23/06 *******************************************
     C                   If        xxcall = '1' OR xxcall = 'Z'                 call is Inquiry
     C                   Eval      *IN09  = *ON
     C                   EndIf
      ***** END ADD ***** 05/23/06 *******************************************
    C*
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
    C*
     C     cmky          chain     pcm01                              60
     C                   move      cmfnd         scfnd
     C                   move      *blanks       scfnam
     C     fnky          chain     pfn01                              60
     C                   if        *in60 = *off
     C                   movel     fnname        scfnam
     C                   endif
      *
     C                   if        cmss# <> *blank
     C                   move      cmss#         xxss#
     C     ss1           cat       '-':0         scss#
     C     scss#         cat       ss2:0         scss#
     C     scss#         cat       '-':0         scss#
     C     scss#         cat       ss3:0         scss#
     C                   endif
     C*
     C                   move      *blank        sccnam
     C     cmlsn         cat       ',':0         sccnam
     C     sccnam        cat       cmfsn:1       sccnam
     C     sccnam        cat       cmmid:1       sccnam
     C*
     C                   move      *blank        scedn
     C     emky          chain     pem01                              60
     C                   movel     ememp#        scedn
     C                   move      emdiv         xxdiv
     C     scedn         cat       xxdiv:1       scedn
     C     scedn         cat       emname:1      scedn
     C*
     C                   movel     @@fyr         scyrcs
     C     scyrcs        cat       '-':0         scyrcs
     C     scyrcs        cat       @@case:0      scyrcs
    C*
     C                   move      *blank        scscl#
     C                   movel     cmacl#        scscl#
     C*
     C                   move      cmacdt        xxacdt
     C                   move      xxacdt        srdti
    C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt                         Accident Date
     C*
     C                   movel     cmjrst        kkst
     C*
     C                   exsr      mvdbsc
     C                   endsr
     C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     PREDSP        begsr
     C                   endsr
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     PRSCTL        begsr
      *** BEGIN ADD *** 06/05/06 *******************************************
     C                   exsr      nxtpgm
      *** END ADD ***** 06/05/06 *******************************************
     C                   endsr
    C*====================================================================
    C* UPWRTR - Update/Write Database file
    C*====================================================================
    C*
     C     UPWRTR        begsr
     C*
     C     niky          chain     pni01
     C*
     C                   exsr      mvscdb
     C*
     C                   if        %found
     C                   update    pni01
     C                   else
     C                   eval      nico# = cmco#
     C                   eval      nifnd = cmfnd
     C                   eval      nifyr = cmfyr
     C                   eval      nicase = cmcase
     C                   eval      nist = 'NY'
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
     C*
     C                   endsr
