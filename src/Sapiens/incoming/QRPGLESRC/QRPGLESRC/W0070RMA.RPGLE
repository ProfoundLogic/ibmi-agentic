     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE Massachusetts State Specific Information Entry Program
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 08/18/2005 by C: 132 for Project 129WIZ               *
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
     FW0070FMA  cf   e             workstn
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
    D*
      *** BEGIN ADD *** 06/05/06 *******************************************
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0
      *** END ADD ***** 06/05/06 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Massachusetts Specific I-
     D                                     nfo')
     D comapi          c                   const('WA001R')
     D pos1            s              2  0
     D pos2            s              2  0
     D length          s              2  0
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
     C* AWW Indicator 1 or 2 only
     C*
     C                   IF        sc1a2 <> *blanks  AND
     C                             sc1a2 <> 'E' AND sc1a2 <> 'A'
     C                   seton                                        7090
     C                   leave
     C                   EndIf
     C*
     C* Self Insured ? Y or N
     C*
     C                   IF        sc1a1 <> *blanks  AND
     C                             sc1a1 <> 'Y' AND sc1a1 <> 'N'
     C                   seton                                        7390
     C                   leave
     C                   EndIf
     C*
     C* Self-Insured number
     C*
     C                   IF        sc40a3 = *blanks AND sc1a1 = 'Y'
     C                   seton                                        7490
     C                   leave
     C                   EndIf
     C*
     C                   exsr      upwrtr
     c                   endif
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
     C* Fifth day of total or partial incapacity
     C*
     C                   IF        scdt1 <> *zeros
     C                   z-add     scdt1         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7090
     C                   leave
     C                   else
     C                   movel     srdto         scdt1
     C                   endif
     C                   EndIf                                                  scdt1 <> *zeros
     C*
     C* Description of how injury occurred and body parts.
     C*
     C                   IF        sc80aa = *blanks
     C                   seton                                        7290
     C                   leave
     C                   EndIf
     C*
     C* Date reported as work related
     C*
     C                   IF        scdt2 <> *zeros
     C                   z-add     scdt2         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7490
     C                   leave
     C                   else
     C                   movel     srdto         scdt2
     C                   endif
     C                   EndIf                                                  scdt1 <> *zeros
     C*
     C                   exsr      upwrtr
     c                   endif
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
     C* Employee Returned to same Occupation?
     C*
     C                   IF        sc1a4 <> *blanks  AND
     C                             sc1a4 <> 'Y' AND sc1a4 <> 'N'
     C                   seton                                        7290
     C                   leave
     C                   EndIf
     C*
     C                   if        scprnm = *blanks
     C                   seton                                        7390
     c                   leave
     c                   endif
     C*
     C                   if        scprti = *blanks
     C                   seton                                        7490
     c                   leave
     c                   endif
     C*
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scprep
     c                   if        srerr = '1'
     c                   seton                                        7590
     c                   endif
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
     C* Screen One fields
     C                   eval      sc1a1  = ni1a1                               Self-Insured?
     C                   eval      sc1a2  = ni1a2                               AWW Indicator
     C                   eval      sc2a2  = ni2a2                               Industry Code
     C                   eval      sc20a7  = ni20a7                             Industry Code
     C*
     C                   eval      sc40a3 = ni40a3                              Self-Insured Number
     C                   eval      sccont = nicont                              Self-Insured Number
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
     C                   z-add     niprep        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scprep
     C*
     C* Screen Two fields
     C                   z-add     nidt1         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt1                          5th Day of t/p Inc.
     C*
     C                   eval      sc60a1 = ni60a1                              Injury Activity
     C                   eval      sc60a2 = ni60a2
     C                   eval      sc80aa = %subst(ni80a1:1:60)
     C                   eval      sc80ab = %subst(ni80a1:61:20) +
     C                                      %subst(ni80a2:1:40)
     C                   eval      sc80ac = %subst(ni80a2:41:40)
     C*
     C                   movel     ni10a1        sc10a1                         Injury Codes
     C                   movel     ni10a2        sc10a2
     C                   movel     ni10a3        sc10a3
     C*
     C                   movel     ni10a4        sc10a4                         Body Part Codes
     C                   movel     ni10a5        sc10a5
     C                   movel(P)  NI20A6        sc10a6
     C*
     C                   z-add     nidt2         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt2                          Date Reptd=Wrk Rltd
     C*
     C* Screen Three fields
     C*
     C                   eval      sc40a4 = %subst(ni80a3:1:40)                 Witnesses
     C                   eval      sc40a5 = %subst(ni80a3:41:40)
     C*
     C                   eval      sc1a4  = ni1a4                               Return to Reg occpt?
     C*
     C                   else
     C*
     c                   clear                   scprnm
     c                   clear                   scprti
     c                   clear                   scprep
     C                   clear                   sc1a1
     C                   clear                   sc1a2
     C                   clear                   sc1a4
     C                   clear                   sc2a2
    C*
     C                   clear                   sc10a1
     C                   clear                   sc10a2
     C                   clear                   sc10a3
     C                   clear                   sc10a4
     C                   clear                   sc10a5
     C                   clear                   sc10a6
     C                   clear                   sccont
     C                   clear                   sc40a3
     C                   clear                   sc40a4
     C                   clear                   sc40a5
     C                   clear                   sc60a1
     C                   clear                   sc60a2
     C                   clear                   sc80aa
     C                   clear                   sc80ab
     C                   clear                   sc80ac
     C                   clear                   scdt1
     C                   clear                   scdt2
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
     C                   eval      ni1a4  = sc1a4
     C                   eval      ni2a2  = sc2a2
     C                   eval      ni10a1 = sc10a1
     C                   eval      ni10a2 = sc10a2
     C                   eval      ni10a3 = sc10a3
     C                   eval      ni10a4 = sc10a4
     C                   eval      ni10a5 = sc10a5
     C                   MOVEL(P)  SC10A6        NI20A6
     C                   eval      ni20a7 = sc20a7
     C                   eval      nicont = sccont
     C                   eval      ni40a3 = sc40a3
     C                   eval      ni80a3 = sc40a4 + sc40a5
     C                   eval      ni60a1 = sc60a1
     C                   eval      ni60a2 = sc60a2
     C                   eval      ni80a1 = sc80aa + %subst(sc80ab:1:20)
     C                   eval      ni80a2 = %subst(sc80ab:21:40) + sc80ac
     C*
     C                   eval      niprnm = %trim(scprnm) + ',' +
     C                                      %trim(scprti)
     C*
     C                   z-add     scprep        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         niprep
     C*
     C                   z-add     scdt1         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt1
     C*
     C                   z-add     scdt2         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt2
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
     C                   If        @@call = '1'
     C                   move      *ON           *IN09
     C                   else
     C                   move      *OFF          *IN09
     C                   endif
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
     C                   If        NOT *IN09
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
     C                   eval      nist = 'MA'
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
     C*
     C                   EndIF                                                  NOT *IN09
     C*
     C                   endsr
