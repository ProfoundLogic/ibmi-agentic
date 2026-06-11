     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H*********************************************************************
     H actgrp(*caller)
    H/TITLE Pennsylvania State Specific Information Entry Program
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 11/01/2005 by C: 132 for Project 114WKO               *
    H*********************************************************************
    H*  Revisions:                                                       *
     H*
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 02/03/10 C:237 SEQ#71314                                          *
     H*                Fix for "Witness Phone number".                    *
     H*                                                                   *
     H* 10/22/09 C:189 SEQ#70040                                          *
     H*                Additional screens being displayed which           *
     H*                has not been set up in Group set up.               *
    H*                                                                   *
     H* 08/28/09 C:189 Project: 133WHE                                    *
     H*                Add the call to Accident Info program              *
     H*                W0070R08 and not call W0190R and SC010R.           *
     H*                                                                   *
    H*  07/22/09 C:198 Project: 114WUN                                   *
     H*                 Added SCR03 to populate WMNIL fields.             *
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
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070FPA  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     Fwmcml1    if   e           k disk
     F*** BEG ADD ***** 08/28/09 **************************************
     F                                     infds(DBFDS3)
     F*** END ADD ***** 08/28/09 **************************************
     F*
     Fwmfnl     if   e           k disk
     F*
     Fwmeml     if   e           k disk
     F*
     Fwmnil     uf a e           k disk
     Fwmcxl     if   e           k disk
     F*
     Fwtgtl     if   e           k disk
     F*
     D/COPY SCOPYPR01D
      *** BEGIN ADD *** 06/05/06 *******************************************
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0
      *** END ADD ***** 06/05/06 *******************************************
      *
     D*** BEG ADD ***** 08/28/09 **************************************
     DDBFDS3           ds
     D DBRR3B                397    400b 0
     D*** END ADD ***** 08/28/09 **************************************
      *
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Pennsylvania State Speci-
     D                                     fic Info')
     D comapi          c                   const('WA001R')
     D pos1            s              2  0
     D pos2            s              2  0
     D length          s              2  0
     D phone           s             10
      ***** BEG ADD ******* 02/03/10 **********************************
     D value           s              1    dim(30)
     D y               s              2  0
     d val#            s             10
     d val             s              1
     d phone#          s             30
      ***** END ADD ******* 02/03/10 **********************************
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
      ***** BEG ADD ******* 10/22/09 **********************************
ks01 C                   parm                    @@cont
      ***** END ADD ******* 10/22/09 **********************************
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

     C*** BEG ADD ***** 08/28/09 **************************************
     C     acdpl4        plist
     C     *inkc         parm                    @@KC              1
     C     *inkl         parm                    @@KL              1
     C                   parm                    @@MODE            1
     C                   parm                    @@RRN             9 0
      ***** BEG CHG ******* 10/22/09 **********************************
     C************       parm      'Y'           @@CONT            1
     C                   parm                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************

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
    C*  KEY List for Initial Treatment Table File
    C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    gttbid
     C                   kfld                    gtkey
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
     C                   setoff                                       82
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
     C* Normal Start Time range must be valid if entered, cannot be zeros
     C*
     C                   if        sctim2  < *zeros  OR
     C                             sctim2  > 235959
     C                   seton                                        7090
     C                   leave
     C                   endif
     C*
     C* Were Safeguards Provided ? and Used ?
     C*
     C                   IF        sc1a1 <> *blanks  AND
     C                             sc1a1 <> 'Y' AND sc1a1 <> 'N'
     C                   seton                                        7590
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a2 <> *blanks  AND
     C                             sc1a2 <> 'Y' AND sc1a2 <> 'N'
     C                   seton                                        7790
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   If        cltid = '133' and *inkc
     C                   seton                                        90
     C                   leave
     C                   endif
     C*** END ADD ***** 08/28/09 **************************************
     C                   exsr      errck2
     C*** Beg Add *** 07/22/09 ***************************************************
     C                   if        *inkc
     C                   seton                                        90
     C                   leave
     C                   endif
     C*** End Add *** 07/22/09 ***************************************************
     C                   endif
     C*** Beg Add *** 07/22/09 ***************************************************
     C                   dou       not *in90
     C                   exfmt     scr03
     C                   setoff                                       90
     C                   if        *inkl
     C                   seton                                        90
     C                   setoff                                       kl
     C                   leave
     C                   else
     C                   exsr      errck3
     C                   if        *inkc
     C                   seton                                        90
     C                   leave
     C                   endif
     C                   endif
     C                   enddo
     C                   if        *inkc
     C                   seton                                        90
     C                   leave
     C                   endif
     C*** END ADD *** 07/22/09 ********************************************
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
     C* Date FROI Prepared
     C*
     C                   IF        scprep <> *zeros
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        8090
     C                   leave
     C                   else
     C                   movel     srdto         scprep
     C                   endif
     C                   EndIf                                                  scdt1 <> *zeros
     C*
     C* If Accident date > FROI Date, is error
     C*
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxprep            7 0          FROI Prepare Date
     C*
     C                   IF        xxprep <  cmacdt
     C                   seton                                        8190
     C                   leave
     C                   EndIf                                                  scdt1 = *zeros
     C*
     C                   exsr      upwrtr
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr
     C*
     C*** BEG ADD *** 07/22/09 ********************************************
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
     C                   IF        sc1a3 <> *blanks  AND
     C                             sc1a3 <> 'Y'  and sc1a3 <> 'N'
     C                   seton                                        8990
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a4 <> *blanks  AND
     C                             sc1a4 <> 'Y'  and sc1a4 <> 'N'
     C                   seton                                        8390
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a5 <> *blanks  AND
     C                             sc1a5 <> 'Y' and sc1a5 <> 'N'
     C                   seton                                        8490
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a6 <> *blanks  AND
     C                             sc1a6 <> 'Y' and sc1a6 <> 'N'
     C                   seton                                        8590
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a7 <> *blanks  AND
     C                             sc1a7 <> 'Y' and sc1a7 <> 'N'
     C                   seton                                        8690
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a8 <> *blanks  AND
     C                             sc1a8 <> 'Y' and sc1a8 <> 'N'
     C                   seton                                        8790
     C                   leave
     C                   EndIf
     C*
     C                   IF        sc1a9 <> *blanks  AND
     C                             sc1a9 <> 'Y' and sc1a9 <> 'N'
     C                   seton                                        8890
     C                   leave
     C                   EndIf
     C*
     C                   exsr      upwrtr
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr
     C*
     C*** END ADD *** 07/22/09 ********************************************
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
     C                   if        not %found
     C                   clear                   pni01
     C                   endif
     C*
     C                   z-add     nitim2        sctim2                         Normal Start Time
     C*
     C                   if        nicont <> *blanks
     C*
     C                   eval      nicont = %trim(nicont)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':nicont:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      sccfsn = %subst(nicont:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      scclsn = %subst(nicont:pos1:27)
     C*
     C                   else
     c                   clear                   sccfsn
     c                   clear                   scclsn
     c                   endif
     C*
     C                   move      nicar         sccar
     c                   move      nicphn        sccphn
     C*
     C                   eval      sc1a1  = ni1a1
     C                   eval      sc1a2  = ni1a2
     C*
     C*
     C                   eval      sc60a1 = ni60a1                              Materials Used
     C                   eval      sc60a2 = ni60a2
     C*
     C                   movel     ni80a1        sc80a1                         How Injury Occurred
     C                   move      ni80a2        sc80a1
     C*
     C* Screen Two
     C*
     C                   if        ni80a3 <> *blanks
     C*
     C                   eval      ni80a3 = %trim(ni80a3)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':ni80a3:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      scwfsn = %subst(ni80a3:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      pos2 = %scan(',':ni80a3:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      scwlsn = %subst(ni80a3:pos1:length)
     c                   endif
     c*
     c                   eval      pos1 = pos2 + 1
      ***** BEG CHG ******* 02/03/10 **********************************
     c******             eval      phone = %subst(ni80a3:pos1:10)
     c                   eval      phone#= %subst(ni80a3:pos1:30)
      ***** END CHG ******* 02/03/10 **********************************
      ***** BEG ADD ******* 02/03/10 **********************************
     c                   eval      y = 1
     c                   movea     phone#        value(y)
     c                   dow       y <= 30
     c                   testn                   value(y)             40
     c                   if        *in40 = '1'
     c                   movea     value(y)      val
     c                   eval      val#= %trim(val#) + val
     c                   endif
     c                   eval      y = y+1
     c                   enddo
     c                   move      VAL#          phone
      ***** END ADD ******* 02/03/10 **********************************
     c                   move      phone         scwphn
     C*
     C                   else
     c                   clear                   scwfsn
     c                   clear                   scwlsn
     c                   clear                   scwphn
     c                   endif
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
     c                   eval      scprnt = %subst(niprnm:pos1:19)
     C*
     C                   else
     c                   clear                   scprnm
     c                   clear                   scprnt
     c                   endif
     C*
     C                   movel     niprph        scprph
     C*
     C                   movel     ni20a4        sc20a4
     C*
     C                   z-add     niprep        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprep                         FROI Prepare Date
     C*
     C                   movel     ni30a4        sc30a4
    C*
     C*** BEG ADD *** 07/22/09 *******************************************
     C*** Screen Three
     C                   eval      sc1a3 = ni1a3
     C                   eval      sc1a4 = ni1a4
     C                   eval      sc1a5 = ni1a5
     C                   eval      sc1a6 = ni1a6
     C                   eval      sc1a7 = ni1a7
     C                   eval      sc1a8 = ni1a8
     C                   eval      sc1a9 = ni1a9
     C*** END ADD *** 07/22/09 *******************************************
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen to Database filed
    C*====================================================================
    C*
     C     MVSCDB        begsr
     C
     C                   z-add     sctim2        nitim2                         Normal Start Time
     C*
     C                   eval      nicont = %trim(sccfsn) + ',' +
     C                                      %trim(scclsn)
     C*
     C                   eval      nicar = sccar
     C                   eval      nicphn = sccphn
     C*
     C                   eval      ni1a1  = sc1a1
     C                   eval      ni1a2  = sc1a2
     C*
     C                   eval      ni60a1 = sc60a1
     C                   eval      ni60a2 = sc60a2
     C*
     C                   movel(p)  sc80a1        ni80a1
     C                   move(p)   sc80a1        ni80a2
     C*
     C                   move      scwphn        phone
     C                   eval      ni80a3 = %trim(scwfsn) + ',' +
     C                                      %trim(scwlsn) + ',' +
     C                                      %trim(phone)
     C*
     C                   eval      niprnm = %trim(scprnm) + ',' +
     C                                      %trim(scprnt)
     C*
     C                   move      scprph        niprph
     C                   move      sc20a4        ni20a4
     C*
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         niprep                         FROI Prepare Date
     C*
     C                   movel     sc30a4        ni30a4
      *** BEG ADD *** 07/22/09 *******************************************
     C                   eval      ni1a3 = sc1a3
     C                   eval      ni1a4 = sc1a4
     C                   eval      ni1a5 = sc1a5
     C                   eval      ni1a6 = sc1a6
     C                   eval      ni1a7 = sc1a7
     C                   eval      ni1a8 = sc1a8
     C                   eval      ni1a9 = sc1a9
      *** END ADD *** 07/22/09 *******************************************
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   If        CLTID <> '133'
     C*** END ADD ***** 08/28/09 **************************************
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
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   endif                                                  CLTID <> 133
      *
     C                   If        CLTID = '133'
     C                             and @@call <> *blanks
      *
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     DBRR3B        @@RRN
      *
     C                   Select
     C                   when      @@call = 'A'
     C                   movel     'A'           @@MODE
     C                   when      @@call = 'Y'
     C                   movel     'C'           @@MODE
     C                   when      @@call = 'Z'
     C                   movel     'D'           @@MODE
     C                   endsl
      *
     C                   CALL      'W0070R08'    ACDPL4                   99
      *
     C                   If        *inkl
     C                   eval      *in90 = *on
     C                   eval      *inkl = *off
     C                   endif
      *
     C                   endif                                                  CLTID = 133
     C*** END ADD ***** 08/28/09 **************************************
      *
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   if        cltid = '133'
     C   kl              eval      @@rtcd = 'B'
     C   kc              eval      @@rtcd = 'A'
     C                   endif
     C*** END ADD ***** 08/28/09 **************************************
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
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'xxss#'       qqname           25
     C     xxss#         parm      xxss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
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
     C*
     C                   move      cmacdt        xxacdt
     C                   move      xxacdt        srdti
    C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt                         Accident Date
     C*
     C                   movel     cmjrst        kkst
     C*
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
      *
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
     C                   eval      nist = 'PA'
      *** BEG ADD *** 07/22/09 *******************************************
     C                   eval      ni1a3 = sc1a3
     C                   eval      ni1a4 = sc1a4
     C                   eval      ni1a5 = sc1a5
     C                   eval      ni1a6 = sc1a6
     C                   eval      ni1a7 = sc1a7
     C                   eval      ni1a8 = sc1a8
     C                   eval      ni1a9 = sc1a9
      *** END ADD *** 07/22/09 *******************************************
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
     C*
     C                   endsr
