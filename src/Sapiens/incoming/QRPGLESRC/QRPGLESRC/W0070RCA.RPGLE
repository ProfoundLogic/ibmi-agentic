     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H*********************************************************************
     H actgrp(*caller)
    H/TITLE CALIFORNIA STATE SPECIFIC INFORMATION ENTRY PROGRAM
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 01/04/2005 by c:62 for Project 132DAW                 *
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
    H*  06/30/10 C:235 Support:  Seq # 70759                             *
     H*                 Pass the values in DS qckey used to call W0070R   *
    H*                 next program in add mode                          *
     H*                                                                   *
     H* 10/22/09 C:189 SEQ#70040                                          *
     H*                Additional screens being displayed which           *
     H*                has not been set up in Group set up.               *
    H*                                                                   *
     H* 08/28/09 C:189 Project: 133WHE                                    *
     H*                Add the call to Accident Info program              *
     H*                W0070R08 and not call W0190R and SC010R.           *
     H*                                                                   *
    H*  06/05/06 C:132 Support:  Seq # 51973                             *
     H*                 Added check to see if additional program calls    *
    H*                 should be made when depending on MODE (from       *
    H*                 program W0070RGA).                                *
    H*                                                                   *
    H* 05/23/06 C: 132 Support: WC132/52838                              *
    H*                 Correct Allow Add/UpDate when called in Inquiry   *
    H*                 Mode, which can be either a "1" or "Z" in xxcall. *
    H*                                                                   *
    H* 04/17/06 C:62  Support: CI132/52316                               *
    H*                Correct field mappings                             *
    H*                                                                   *
    H* 02/27/06 C:15  Support: WC136/50984                               *
    H*                Added code to condition the "Date employee ...'    *
    H*                edit, so it is only invoked if a date is entered.  *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070FCA  cf   e             workstn
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
     D/COPY SCOPYPR01D
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
     D optnm           c                   const('California State Specific -
     D                                     Info')
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
     D xxdate          ds             8
     D  mm                     1      2
     D  dd                     4      5
     D  ccyy                   6      8
     D xxtime          ds             4
     D  hr                            2  0
     D  mn                            2  0
     D kkco#           s                   like(cmco#)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkcase          s                   like(cmcase)
     D kkst            s                   like(cmjrst)
      *** BEGIN ADD *** 06/05/06 *******************************************
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0
      *** END ADD ***** 06/05/06 *******************************************

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
     C*********          parm      'Y'           @@CONT            1
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
     C* Date must be valid if entered
     C*
     C*** begin add *** 02/27/06 ***********************************************
     C                   if        scdt1 <> 0
     C*** end add ***** 02/27/06 ***********************************************
     C                   z-add     scdt1         srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = *on
     C                   seton                                        7190
     C                   leave
     C                   else
     C                   movel     srdto         scdt1
     c                   endif
     C*** begin add *** 02/27/06 ***********************************************
     C                   endif
     C*** end add ***** 02/27/06 ***********************************************
     C*
     C* Inpatient flag must be Y or N if entered.
     C*
     c*** begin chg *** 04/17/06 *******************************************
     C**********         if        sc1a3 <> *blanks and
     C**********                   sc1a3 <> 'N'     and
     C**********                   sc1a3 <> 'Y'
     C                   if        sc1a5 <> *blanks and
     C                             sc1a5 <> 'N'     and
     C                             sc1a5 <> 'Y'
     c*** end   chg *** 04/17/06 *******************************************
     C                   seton                                        7290
     C                   leave
     c                   endif
     C*
     C* Emergency room flag must be Y or N if entered.
     C*
     c*** begin chg *** 04/17/06 *******************************************
     C**********         if        sc1a4 <> *blanks and
     C**********                   sc1a4 <> 'N'     and
     C**********                   sc1a4 <> 'Y'
     C                   if        sc1a6 <> *blanks and
     C                             sc1a6 <> 'N'     and
     C                             sc1a6 <> 'Y'
     c*** end   chg *** 04/17/06 *******************************************
     C                   seton                                        7390
     C                   leave
     c                   endif
     C*
     C                   exsr      upwrtr
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
     C*
     C                   if        not *inkc
     C                   exsr      upwrtr
     c                   endif
     C*
     C                   enddo
     C*
     C                   endsr
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
     c*** begin chg *** 04/17/06 *******************************************
     C**********         eval      sc1a3  = ni1a3
     C**********         eval      sc1a4  = ni1a4
     C                   eval      sc1a5  = ni1a5
     C                   eval      sc1a6  = ni1a6
     c*** end   chg *** 04/17/06 *******************************************
     C                   eval      sc20a1 = ni20a1
     c*** begin chg *** 04/17/06 *******************************************
     C**********         eval      sc30a1 = ni30a1
     C**********         eval      sc30a2 = ni30a2
     C**********         eval      sc40a1 = ni40a1
     C                   eval      sc30a2 = ni30a2
     C                   eval      sc30a3 = ni30a3
     C                   eval      sc40a4 = ni40a4
     c*** end   chg *** 04/17/06 *******************************************
     C*
     c*** begin chg *** 04/17/06 *******************************************
     C**********         movel     ni80a1        sc80a1a
     C**********         move      ni80a1        sc80a1b
     C*
     C**********         movel     ni80a2        sc80a2a
     C**********         move      ni80a2        sc80a2b
     C                   movel     ni80a6        sc80a6a
     C                   move      ni80a6        sc80a6b
     C*
     C                   movel     ni80a7        sc80a7a
     C                   move      ni80a7        sc80a7b
     c*** end   chg *** 04/17/06 *******************************************
     C*
     C                   movel     ni80a4        sc80a4a
     C                   move      ni80a4        sc80a4b
     C*
     C                   movel     ni80a5        sc80a5a
     C                   move      ni80a5        sc80a5b
     C*
     C                   movel     nitim2        sctim2
     C*
     C                   z-add     nidt1         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdt1
     C*
     C                   move      ni9n3         sc9n3
     C                   movel     ni40a6        sc40a6
     C*
     C                   else
     c*** begin chg *** 04/17/06 *******************************************
     C**********         clear                   sc1a3
     C**********         clear                   sc1a4
     C                   clear                   sc1a5
     C                   clear                   sc1a6
     c*** end   chg *** 04/17/06 *******************************************
     C                   clear                   sc20a1
     c*** begin chg *** 04/17/06 *******************************************
     C**********         clear                   sc30a1
     C**********         clear                   sc30a2
     C**********         clear                   sc40a1
     C**********         clear                   sc80a1a
     C**********         clear                   sc80a2a
     C                   clear                   sc30a2
     C                   clear                   sc30a3
     C                   clear                   sc40a4
     C                   clear                   sc80a6a
     C                   clear                   sc80a7a
     c*** end   chg *** 04/17/06 *******************************************
     C                   clear                   sc80a4a
     C                   clear                   sc80a5a
     c*** begin chg *** 04/17/06 *******************************************
     C**********         clear                   sc80a1b
     C**********         clear                   sc80a2b
     C                   clear                   sc80a6b
     C                   clear                   sc80a7b
     c*** end   chg *** 04/17/06 *******************************************
     C                   clear                   sc80a4b
     C                   clear                   sc80a5b
     C                   clear                   sctim2
     C                   clear                   scdt1
     C                   clear                   sc9n3
     C                   clear                   sc40a6
     C                   endif
    C*
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen to Database filed
    C*====================================================================
    C*
     C     MVSCDB        begsr
     C
     c*** begin chg *** 04/17/06 *******************************************
     C**********         eval      ni1a3  = sc1a3
     C**********         eval      ni1a4  = sc1a4
     C                   eval      ni1a5  = sc1a5
     C                   eval      ni1a6  = sc1a6
     c*** end   chg *** 04/17/06 *******************************************
     C                   eval      ni20a1 = sc20a1
     c*** begin chg *** 04/17/06 *******************************************
     C**********         eval      ni30a1 = sc30a1
     C**********         eval      ni30a2 = sc30a2
     C**********         eval      ni40a1 = sc40a1
     C*
     C**********         movel(p)  sc80a1a       ni80a1
     C**********         move      sc80a1b       ni80a1
     C*
     C**********         movel(p)  sc80a2a       ni80a2
     C**********         move      sc80a2b       ni80a2
     C                   eval      ni30a2 = sc30a2
     C                   eval      ni30a3 = sc30a3
     C                   eval      ni40a4 = sc40a4
     C*
     C                   movel(p)  sc80a6a       ni80a6
     C                   move      sc80a6b       ni80a6
     C*
     C                   movel(p)  sc80a7a       ni80a7
     C                   move      sc80a7b       ni80a7
     c*** end   chg *** 04/17/06 *******************************************
     C*
     C                   movel(p)  sc80a4a       ni80a4
     C                   move      sc80a4b       ni80a4
     C*
     C                   movel(p)  sc80a5a       ni80a5
     C                   move      sc80a5b       ni80a5
     C*
     C                   movel     sctim2        nitim2
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
     C                   move      sc9n3         ni9n3
     C                   movel     sc40a6        ni40a6
     C*
     C                   endsr
    C*
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
      *** BEGIN ADD *** 06/30/10 *******************************************
     c                   move      @@co#         xxco#
     c                   move      @@fnd         xxfnd
     c                   move      @@fyr         xxfyr
     c                   move      @@case        xxcase
      *** END   ADD *** 06/30/10 *******************************************
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
      *
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     DBRR3B        @@RRN
      *
     C                   Select
     C                   when      @@call = 'A'
     C                   MOVEL     'A'           @@MODE
     C                   when      @@call = 'Y'
     C                   MOVEL     'C'           @@MODE
     C                   when      @@call = 'Z'
     C                   MOVEL     'D'           @@MODE
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
    C*
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
     C                   movel     srdto         scacdt
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
     C                   eval      nist = 'CA'
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
     C*
     C                   endsr
