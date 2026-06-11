     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H*********************************************************************
     H actgrp(*caller)
    H/TITLE GEORGIA STATE SPECIFIC INFORMATION ENTRY PROGRAM
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM: W0070R12                                            *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 12/30/99      C:112     PROJECT: 91WAF                  *
    H*********************************************************************
    H*  REVSIONS:                                                        *
     H*                                                                   *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 10/22/09 C:189 SEQ#70040                                          *
     H*                Additional screens being displayed which           *
     H*                has not been set up in Group set up.               *
    H*                                                                   *
    H*                                                                   *
    H* 08/28/09 C:189 Project: 133WHE                                    *
    H*                Add the call to Accident Info program              *
    H*                W0070R08 and not call W0190R and SC010R.           *
    H*                                                                   *
    H* 07/21/09 C: 062 Support: WC139/69160                              *
    H*                 Lift edit on to date                              *
    H*                                                                   *
    H* 05/23/06 C: 132 Support: WC132/52838                              *
    H*               - Correct Allow Add/UpDate when called in Inquiry   *
    H*                 Mode, which can be either a "1" or "Z" in xxcall. *
    H*               - Change W0070FGA to add Indicator 09 to fields.    *
    H*        NOTE - Must send W0070FGA with this program for Installs   *
    H*                                                                   *
    H*  01/26/06 C:168 Project: 91WMT                                    *
    H*                 Remove field "Time workday began" and Did Emp     *
    H*                 work next day" and added the following filds:     *
    H*                 - Claims office e-mail                            *
    H*                 - SBWC ID#                                        *
    H*                 - Person filling form e-mail                      *
    H*                                                                   *
    H*  02/16/00 C:112 Support:  Seq # 21878                             *
     H*                 Added check to see if additional program calls    *
    H*                 should be made when depending on MODE             *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070FGA  cf   e             workstn
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
      *** BEGIN ADD *** 02/16/00 *******************************************
     Fwmcxl     if   e           k disk
      *** END ADD ***** 02/16/00 *******************************************
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
     D optnm           c                   const('Georgia State Specific Info')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxdt1           s                   like(nidt1)
     D xxdt2           s                   like(nidt1)
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
     D xxst            s                   like(cmjrst)
      *** BEGIN ADD *** 02/16/00 *******************************************
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0
      *** END ADD ***** 02/16/00 *******************************************
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
      *** BEGIN ADD *** 02/16/00 *******************************************
     C     acdpl         plist
     C                   parm                    xxprds           40
     C                   parm                    xxcall
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm      *zero         xaacdt            8
     C                   parm                    xxrtcd            1
      *** END ADD ***** 02/16/00 *******************************************
     C*** BEG ADD ***** 08/28/09 **************************************
     C     acdpl4        plist
     C     *inkc         parm                    @@KC              1
     C     *inkl         parm                    @@KL              1
     C                   parm                    @@MODE            1
     C                   parm                    @@RRN             9 0
      ***** BEG CHG ******* 10/22/09 **********************************
     C**********         parm      'Y'           @@CONT            1
     C                   parm                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C*** BEGIN ADD *** 02/16/00 *******************************************
     C*
    C*  KEY LIST FOR CLAIM/DARTS CONTROL FILE
    C*
     C     cxky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
      *** END ADD ***** 02/16/00 *******************************************
     C*
     C* State NOI record file
     C*
     C     niky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C                   kfld                    xxst
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
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
     C                   setoff                                       888990
      *
     C                   exsr      othfks
    C*
     C                   do
    C*
      *** BEG DEL ***** 01/26/06 *******************************************
     C***                if        sctime <> *zeros
     C***                move      sctime        xxtime
     C***                if        hr > 12 or hr < *zero
     C***                          or mn > 59 or mn < *zero
     C***                seton                                        7290
     C***                leave
     C***                endif
      ***
     C***                if        scampm = ' '
     C***                seton                                        8890
     C***                leave
     C***                endif
    C***
     C***                if        scampm <> 'A' and scampm <> 'P'
     C***                seton                                        7390
     C***                leave
     C***                endif
     C***                else
     C***                if        scampm <> ' '
     C***                seton                                        8890
     C***                leave
     C***                endif
     C***                endif
     C***
     C***                if        scewrk <> 'Y' and scewrk <> 'N'
     C***                          and scewrk <> ' '
     C***                seton                                        7490
     C***                leave
     C***                endif
      *** END DEL ***** 01/26/06 *******************************************
      *** BEG ADD ***** 01/26/06 *******************************************
      *
      * Employee e-mail
     C                   if        sc40a3 = *blanks
     C*                  seton                                        7290
     C*                  leave
     C                   endif
      *
      * Claims office e-mail
     C                   if        sc40a4 = *blanks
     C*                  seton                                        7390
     C*                  leave
     C                   endif
      *
      * SBWC ID #
     C                   if        sc5n3 = *zeros
     C*                  seton                                        7190
     C*                  leave
     C                   endif
      *
      * Person filling form e-mail
     C                   if        sc40a5 = *blanks
     C*                  seton                                        7890
     C*                  leave
     C                   endif
     *
      *** END ADD ***** 01/26/06 *******************************************
    C*
     C                   if        scbamt < *zero
     C                   seton                                        7690
     C                   leave
     C                   endif
    C*
     C                   if        scbpfr > *zero
     C                   z-add     scbpfr        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scbpfr
     C                   if        srerr = '1'
     C                   seton                                        7790
     C                   leave
     C                   endif
     C                   endif
    C*
     C                   if        sccdtt <> 'X' and sccdtt <> ' '
     C                   seton                                        7890
     C                   leave
     C                   endif
    C*
     C                   if        sccdtp <> 'X' and sccdtp <> ' '
     C                   seton                                        7990
     C                   leave
     C                   endif
    C*
     C                   if        sccdpp <> 'X' and sccdpp <> ' '
     C                   seton                                        8090
     C                   leave
     C                   endif
    C*
     C                   if        (sccdtt = 'X' and sccdtp = 'X') or
     C                             (sccdtt = 'X' and sccdpp = 'X') or
     C                             (sccdtp = 'X' and sccdpp = 'X')
     C                   seton                                        8690
     C                   leave
     C                   endif
    C*
     C                   if        scperc > 100 or scperc < 0
     C                   seton                                        8190
     C                   leave
     C                   endif
    C*
     C                   if        scweek < 0
     C                   seton                                        8390
     C                   leave
     C                   endif
    C*
     C                   if        scundt > *zero
     C                   z-add     scundt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scundt
     C                   if        srerr = '1'
     C                   seton                                        8490
     C                   leave
     C                   endif
     C                   endif
     C
     C                   z-add     scbpfr        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt1
     C
     C                   z-add     scundt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt2
    C*
     C*** begin chg *** 07/21/09 *******************************************
     C**********         if        xxdt1 > xxdt2
     C                   if        xxdt1 > xxdt2 and scundt<>0
     C*** end   chg *** 07/21/09 *******************************************
     C                   seton                                        7590
     C                   leave
     C                   endif
     C*
     C                   if        (scbamt > 0 or scbpfr > 0 or
     C                             sccdtt <> *blanks or  sccdtp <> *blanks or
     C                             sccdpp <> *blanks or scbprt <> *blanks or
     C                             scperc > 0 or scweek > 0 or scundt > 0) and
     C                             scnopy <> *blanks
     C                   seton                                        8790
     C                   leave
     C                   endif
     C*
     C                   leave
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move database to screen filed
    C*====================================================================
    C*
     C     MVDBSC        begsr
      *
      *** BEGIN DEL *** 01/26/06 *******************************************
     C*                  move      ni5n1         sctime
     C*                  move      ni1a1         scampm
     C*                  move      ni1a2         scewrk
      *** BEGIN DEL *** 01/26/06 *******************************************
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   movel     ni40a3        sc40a3
     C                   movel     ni40a4        sc40a4
     C                   move      ni5n3         sc5n3
     C                   movel     ni40a5        sc40a5
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   move      ni20a1        scdyof
     C                   move      ni5n2         scbamt
     C                   z-add     nidt1         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scbpfr
     C                   if        ni10a2 = 'TT'
     C                   move      'X'           sccdtt
     C                   move      *blanks       sccdtp
     C                   move      *blanks       sccdpp
     C                   else
     C                   if        ni10a2 = 'TP'
     C                   move      'X'           sccdtp
     C                   move      *blanks       sccdtt
     C                   move      *blanks       sccdpp
     C                   else
     C                   if        ni10a2 = 'PP'
     C                   move      'X'           sccdpp
     C                   move      *blanks       sccdtt
     C                   move      *blanks       sccdtp
     C                   else
     C                   move      *blanks       sccdtt
     C                   move      *blanks       sccdtp
     C                   move      *blanks       sccdpp
     C                   endif
     C                   endif
     C                   endif
     C                   move      ni3n1         scperc
     C                   move      ni10a3        scbprt
     C                   move      ni3n2         scweek
     C                   z-add     nidt2         srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scundt
     C                   move      ni60a1        scnopy
      *
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen to Database filed
    C*====================================================================
    C*
     C     MVSCDB        begsr
     C
      *
      *    write or update WMNIP State NOI Record file
      *
     C                   move      xxco#         nico#
     C                   move      xxfnd         nifnd
     C                   move      xxfyr         nifyr
     C                   move      xxcase        nicase
      *** BEGIN DEL *** 01/26/06 *******************************************
     C*                  move      sctime        ni5n1
     C*                  move      scampm        ni1a1
     C*                  move      scewrk        ni1a2
      *** BEGIN DEL *** 01/26/06 *******************************************
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   movel     sc40a3        ni40a3
     C                   movel     sc40a4        ni40a4
     C                   move      sc5n3         ni5n3
     C                   movel     sc40a5        ni40a5
     C                   movel     xxst          nist
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   move      scdyof        ni20a1
     C                   move      scbamt        ni5n2
     C                   z-add     scbpfr        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt1
     C                   if        sccdtt = 'X'
     C                   movel     'TT'          ni10a2
     C                   else
     C                   if        sccdtp = 'X'
     C                   movel     'TP'          ni10a2
     C                   else
     C                   if        sccdpp = 'X'
     C                   movel     'PP'          ni10a2
     C                   else
     C                   move      *blanks       ni10a2
     C                   endif
     C                   endif
     C                   endif
     C                   move      scperc        ni3n1
     C                   move      scbprt        ni10a3
     C                   move      scweek        ni3n2
     C                   z-add     scundt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt2
      *
     C                   move      q1user        niuser
     C                   move      q1pgm         nipgm
     C                   time                    dsdttm
     C                   z-add     dstime        nitime
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt
     C                   move      scnopy        ni60a1
     C
     C                   if        *in20 = *on
     C                   update    pni01                                60
     C                   else
     C                   write     pni01                                60
     C                   endif
     C                   endsr
    C*
      *** BEGIN ADD *** 02/16/00 *******************************************
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
      *** END ADD ***** 02/16/00 *******************************************
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
    C*
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
     C                   move      @@co#         xxco#
     C                   move      @@fnd         xxfnd
     C                   move      @@fyr         xxfyr
     C                   move      @@case        xxcase
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
      *
      *** BEGIN DEL *** 01/26/06 *******************************************
     C*                  move      *zero         sctime
     C*                  move      *blank        scampm
      *** BEGIN DEL *** 01/26/06 *******************************************
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   move      *blanks       sc40a3
     C                   move      *blanks       sc40a4
     C                   move      *zeros        sc5n3
     C                   move      *blanks       sc40a5
      *** BEGIN ADD *** 01/26/06 *******************************************
     C                   move      *blank        scdyof
     C                   move      *zero         scbamt
     C                   move      *blank        scbpfr
     C                   movel     cmjrst        xxst
     C                   setoff                                       20
     C     niky          chain     pni01                              60
     C     *in60         ifne      '1'
     C                   exsr      mvdbsc
      * set indicator to show record in WMNIP exists
     C                   seton                                        20
     C                   endif
     C                   endsr
     C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
     C                   exsr      mvscdb
      *** BEGIN ADD *** 02/16/00 *******************************************
     C                   exsr      nxtpgm
      *** END ADD ***** 02/16/00 *******************************************
     C                   endsr
    C*
     O
