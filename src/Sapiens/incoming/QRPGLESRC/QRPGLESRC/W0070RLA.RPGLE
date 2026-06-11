     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H*********************************************************************
     H actgrp(*caller)
    H/TITLE LOUISIANA STATE SPECIFIC INFORMATION ENTRY PROGRAM
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 07/08/2005 by C: 132 for Project 133WBQ               *
    H*********************************************************************
    H*  Revisions:                                                       *
     H*
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H*                                                                   *
     H* 10/22/09 C:189 SEQ#70040                                          *
     H*                Additional screens being displayed which           *
     H*                has not been set up in Group set up.               *
    H*                                                                   *
    H* 08/28/09 C:189 Project: 133WHE                                    *
    H*                Add the call to Accident Info program              *
    H*                W0070R08 and not call W0190R and SC010R.           *
    H*                                                                   *
    H*  06/05/06 C:132 Support:  Seq # 51973                             *
     H*                 Added check to see if additional program calls    *
    H*                 should be made when depending on MODE (from       *
    H*                 program W0070RGA).                                *
    H*                                                                   *
    H* 05/23/06 C: 132 Support: WC132/52838                              *
    H*                 Correct Allow Add/updtte when called in Inquiry   *
    H*                 Mode, which can be either a "1" or "Z" in xxcall. *
    H*                                                                   *
    H* 03/03/06 C:132 Support: WC090/50955                               *
    H*                Added code for table lookup of Report purpose      *
    H*                                                                   *
    H* 02/13/06 C:15  Support: WC133/51444                               *
    H*                Added code so all entry fields are optional (not   *
    H*                required).                                         *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070FLA  cf   e             workstn
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
     F*
     Fwmcxl     if   e           k disk
     Fstabl     if a e           k disk
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
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Louisiana State Specific -
     D                                     Info')
     D pur01           c                   const('More than 7 days disability')
     D pur02           c                   const('Possible Dispute')
     D pur03           c                   const('Medical Only')
     D pur04           c                   const('Injury resulted in death')
     D pur05           c                   const('Lump Sum Compromise/Sttlmnt')
     D pur06           c                   const('Amputation or disfigurement')
     D pur07           c                   const('Other')
     D comapi          c                   const('WA001R')
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
     D kkkey           s             10
      ***** BEG ADD ***** 03/03/06 **********************************************
     D qqcall          s              1
     D qqid            s              4
     D qqkey           s             10
     D qqprds          s             40
     D qqrtc           s              1
      ***** END ADD ***** 03/03/06 **********************************************
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
     C**********         parm        'Y'         @@CONT            1
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
     C                   setoff                                       7172
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
     C* Report Date must be valid if entered
     C*
     C*** Begin add *** 02/13/06 *******************************************
     C                   if        scprep <> 0
     C*** End add ***** 02/13/06 *******************************************
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7190
     C                   leave
     C                   else
     C                   movel     srdto         scprep
     c                   endif
     C*** Begin add *** 02/13/06 *******************************************
     C                   endif
     C*** End add ***** 02/13/06 *******************************************
     C*
     C* At Same Wage flag must be Y or N if entered or if Back to Work date > zeros
     C*
     C*** Begin dlt *** 02/13/06 *******************************************
     C*****              if        sc1a2 =  *blanks AND scrtw <> *zeros
     C*****              seton                                        7390
     C*****              leave
     c*****              endif
     C*
     C* At Same Wage flag must be BLANK  if Back to Work date = zeros
     C*
     C                   if        sc1a2 =  'Y'  AND scrtw = *zeros OR
     C                             sc1a2 =  'N'  AND scrtw = *zeros OR
     C                             sc1a2 <> *blanks  AND scrtw = *zeros
     C                   seton                                        8090
     C                   leave
     c                   endif
     C*
     C*****              if        sc1a2 <> *blanks AND scrtw <> *zeros AND
     C*****                        sc1a2 <> 'N'     AND
     C*****                        sc1a2 <> 'Y'
     C*****              seton                                        7390
     C*****              leave
     c*****              endif
     C*** End dlt ***** 02/13/06 *******************************************
     C*
     C* Normal Start Time range must be valid if entered & include the AM/PM field value
     C*
     C*** Begin add *** 02/13/06 *******************************************
     C                   if        sctim2 <> *zeros
     C*** End add ***** 02/13/06 *******************************************
     C                   if        sctim2  = *zeros  OR
     C                             sctim2  < *zeros  OR
     C                             sctim2  > 235959
     C                   seton                                        7490
     C                   leave
     C                   endif
     C*** Begin add *** 02/13/06 *******************************************
     C                   endif
     C*** End add ***** 02/13/06 *******************************************
     C*
     C* Last Full Day Paid Date must be valid if entered
     C*
     C                   IF        SCDT2 > *zeros
     C                   z-add     scdt2         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7790
     C                   leave
     C                   else
     C                   movel     srdto         scdt2
     c                   endif
     C                   endIF                                                  End SCDT2 > Zeros
     C*
     C* Report purpose must be between 1 and 7 if entered.
     C*
     C                   if        scrp01 <> *blanks and scrp01 <> 'Y' and
     C                             scrp01 <> 'N'
     c                   seton                                        8190
     C                   leave
     c                   endif
     C*
     C                   if        scrp02 <> *blanks and scrp02 <> 'Y' and
     C                             scrp02 <> 'N'
     c                   seton                                        8290
     C                   leave
     c                   endif
     C*
     C                   if        scrp03 <> *blanks and scrp03 <> 'Y' and
     C                             scrp03 <> 'N'
     c                   seton                                        8390
     C                   leave
     c                   endif
     C*
     C                   if        scrp04 <> *blanks and scrp04 <> 'Y' and
     C                             scrp04 <> 'N'
     c                   seton                                        8490
     C                   leave
     c                   endif
     C*
     C                   if        scrp05 <> *blanks and scrp05 <> 'Y' and
     C                             scrp05 <> 'N'
     c                   seton                                        8590
     C                   leave
     c                   endif
     C*
     C                   if        scrp06 <> *blanks and scrp06 <> 'Y' and
     C                             scrp06 <> 'N'
     c                   seton                                        8690
     C                   leave
     c                   endif
     C*
     C                   if        scrp07 <> *blanks and scrp07 <> 'Y' and
     C                             scrp07 <> 'N'
     c                   seton                                        8790
     C                   leave
     c                   endif
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
     C                   leave
     C                   endif
     C*** END ADD ***** 08/28/09 **************************************
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
     C                   exsr      upwrtr
     c                   endif
     C                   dou       not *in90
     C                   exfmt     scr03
     C                   setoff                                       90
     C                   if        *inkl
     C                   seton                                        90
     C                   setoff                                       kl
     C                   leave
     C                   else
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   If        cltid = '133' and *inkc
     C                   leave
     C                   endif
     C*** END ADD ***** 08/28/09 **************************************
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
     C                   if        not *in09
     C*
     C* Occupational disease diagnois date must be valid if entered
     C*
     C                   If        scdt3 <> *zeros
     C                   z-add     scdt3         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7190
     C                   leave
     C                   else
     C                   movel     srdto         scdt3
     c                   endif
     C                   endIF                                                  End SCDT3 > Zeros
     C*
     C* State  must be valid if entered
     C*
     C                   if        sc2a1 <> *blanks
     c                   movel(p)  sc2a1         kkkey
     C     kkkey         chain     pst01
     C                   if        not %found
     C                   seton                                        7290
     C                   endif
     C                   endif
     C*
     C                   exsr      upwrtr
     c                   endif
     C*
     C                   enddo
     C*
     C                   endsr
    C*====================================================================
    C* MVDBSC - Move database to screen filed
    C*====================================================================
     C     MVDBSC        begsr
    C*
     C     niky          chain(n)  pni01
     C                   if        not %found
     c                   clear                   pni01
     c                   endif
     c*
     c* Screen 1
     C*
     C                   z-add     niprep        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprep                         Report Date
     C*
     C                   eval      sc1a2  = ni1a2                               Return at same wage?
     C                   eval      sctim2 = nitim2                              Normal Start Time
     C                   eval      sc20a2 = ni20a2
     c*
     C                   z-add     nidt2         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt2                          Last Full Day Paid
     C*
     c                   eval      scrp01 = 'N'
     c                   eval      scrp02 = 'N'
     c                   eval      scrp03 = 'N'
     c                   eval      scrp04 = 'N'
     c                   eval      scrp05 = 'N'
     c                   eval      scrp06 = 'N'
     c                   eval      scrp07 = 'N'
     C*
     C                   if        %scan('1':ni20a4:1) <> 0
     C                   eval      scrp01 = 'Y'
     C                   endif
     C*
     C                   if        %scan('2':ni20a4:1) <> 0
     C                   eval      scrp02 = 'Y'
     C                   endif
     C*
     C                   if        %scan('3':ni20a4:1) <> 0
     C                   eval      scrp03 = 'Y'
     C                   endif
     C*
     C                   if        %scan('4':ni20a4:1) <> 0
     C                   eval      scrp04 = 'Y'
     C                   endif
     C*
     C                   if        %scan('5':ni20a4:1) <> 0
     C                   eval      scrp05 = 'Y'
     C                   endif
     C*
     C                   if        %scan('6':ni20a4:1) <> 0
     C                   eval      scrp06 = 'Y'
     C                   endif
     C*
     C                   if        %scan('7':ni20a4:1) <> 0
     C                   eval      scrp07 = 'Y'
     C                   endif
     c*
     c* Screen 2
     c*
     C                   eval      sc20a3 = ni20a3                              Parish of Injury
     C                   eval      sc30a3 = ni30a3                              Parish of Injury
     C                   eval      sc60a3 = ni60a3
     C                   eval      sc60a4 = ni60a4
     C                   eval      sc40a4 = ni40a4
     C*
     C                   movel     ni80a1        sc160a                         Cause of Injury
     C                   move      ni80a2        sc160a
     c*
     c* Screen 3
     C*
     C                   z-add     nidt3         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt3                          Occ. Disease Date Dg
     C*
     c                   eval      scprnm = niprnm
    C*
     C                   eval      sc40a1 = ni40a1
     C                   eval      sc25a1 = ni25a1
     C                   eval      sc2a1  = ni2a1
     C                   movel     ni9n2         sc9n2a
     C                   move      ni9n2         sc9n2b
    C*
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen to Database filed
    C*====================================================================
    C*
     C     MVSCDB        begsr
     C*
     C* Screen 1
     C*
     C                   z-add     scprep        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         niprep                         Report Date
     C*
     C                   eval      ni1a2  = sc1a2                               Return at same wage?
     C                   eval      nitim2 = sctim2                              Normal Start Time
     c*
     C                   z-add     scdt2         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt2                          Last Full Day Paid
     C*
     C                   clear                   ni20a4
     C*
     c                   if        scrp01 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '1'
     C                   endif
     C*
     c                   if        scrp02 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '2'
     C                   endif
     C*
     c                   if        scrp03 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '3'
     C                   endif
     C*
     c                   if        scrp04 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '4'
     C                   endif
     C*
     c                   if        scrp05 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '5'
     C                   endif
     C*
     c                   if        scrp06 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '6'
     C                   endif
     C*
     c                   if        scrp07 = 'Y'
     C                   eval      ni20a4 = %trim(ni20a4) + '7'
     C                   endif
     C*
     C* Screen 2
     C*
     C                   eval      ni20a3 = sc20a3                              Parish of Injury
     C                   eval      ni30a3 = sc30a3                              Parish of Injury
     C                   eval      ni60a3 = sc60a3
     C                   eval      ni60a4 = sc60a4
     C                   eval      ni20a2 = sc20a2
     C                   eval      ni40a4 = sc40a4
     C*
     C                   movel     sc160a        ni80a1                         Cause of Injury
     C                   move      sc160a        ni80a2
     C*
     C* Screen 3
     C*
     C                   z-add     scdt3         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt3                          Occ. Disease Date Dg
     C*
     c                   eval      niprnm = scprnm
    C*
     C                   eval      ni40a1 = sc40a1
     C                   eval      ni25a1 = sc25a1
     C                   eval      ni2a1  = sc2a1
     C                   movel     sc9n2a        ni9n2
     C                   move      sc9n2b        ni9n2
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   endif                                                  CLTID <> 133
      *
     C                   If        CLTID = '133'
     C                             and @@call <> *blanks
     C                             and not *inkc and not *inkl
      *
     C     CMKY          chain     pcm01                              99
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
    C*
     C                   move      *blank        scscl#
     C                   movel     cmacl#        scscl#
     C*
     C                   move      cmacdt        xxacdt
     C                   move      xxacdt        srdti
    C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt                         Accident Date
     C*
     C                   move      cmrtw         xxacdt
     C                   move      xxacdt        srdti
    C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scrtw                          Back to work date
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
     C                   eval      nist = 'LA'
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
     C*
     C                   endsr
