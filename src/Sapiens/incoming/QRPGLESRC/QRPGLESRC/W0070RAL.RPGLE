     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*
     H actgrp(*caller)
     H/TITLE AL State Specific Information Entry Program
     H*********************************************************************
     H* REVISIONS:
     H*                                                                   *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
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
     H*
     H* 07/22/05  C:61 Project 71WFY
     H*                Created.
     H*
     H*********************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070FAL  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      * Claim
     Fwmcml1    if   e           k disk
     F*** BEG ADD ***** 08/28/09 **************************************
     F                                     infds(DBFDS3)
     F*** END ADD ***** 08/28/09 **************************************
      *
      * Group
     Fwmfnl     if   e           k disk
      *
      * Employer
     Fwmeml     if   e           k disk
      *
      * Notice of Injury
     Fwmnil     uf a e           k disk
      *
      *** BEGIN ADD *** 06/05/06 *******************************************
     Fwmcxl     if   e           k disk
      *** END ADD ***** 06/05/06 *******************************************
      *
     D/COPY SCOPYPR01D
      *
     D*** BEG ADD ***** 08/28/09 **************************************
     DDBFDS3           ds
     D DBRR3B                397    400b 0
     D*** END ADD ***** 08/28/09 **************************************
      *
      *---------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *---------------------------------------------------------------------
     D optnm           c                   const('Alabama State Specific In-
     D                                     fo')
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

      *---------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *---------------------------------------------------------------------
    
     C/COPY SCOPYDTPL
    
    
      *================================================================
      *  PLIST definitions
      *================================================================
    
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
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    
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


      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Case/claim
      *
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *
     C*** BEGIN ADD *** 06/05/06 *******************************************
     C*
    C*  KEY LIST FOR CLAIM/DARTS CONTROL FILE
    C*
     C     cxky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
      *** END ADD ***** 06/05/06 *******************************************
      *
      * State NOI record
      *
     C     niky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkst
      *
      * Group
      *
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
      *
      * Employer
      *
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv


     C/COPY SCOPYPR01C


      *====================================================================
      * DSCAPI - Call API for getting descriptions (use 91 on error)
      *====================================================================
     C     DSCAPI        begsr
      *
     C                   call      comapi        compl                  91
      *
     C                   endsr
    

      *====================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *====================================================================
     C     ERRCK1        begsr

     c                   do

      *  Work Comp Provided by

     c                   if        (*in90 = *off)

     c                   if        (sc1a1 <> ' ')
     c                             and (sc1a1  <> '1')
     c                             and (sc1a1  <> '2')
     c                             and (sc1a1  <> '3')
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   leave
     c                   endif

     c                   endif

      *******   passed errck1

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

     C                   enddo

     C                   endsr


      *====================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *====================================================================
      *
     C     ERRCK2        begsr
      *
      *
      *
     C                   if        not *inkc and NOT *inkl
     C                   exsr      upwrtr
     c                   endif
      *
     C                   endsr


      *====================================================================
      * KJDO   - Processing for F10
      *====================================================================
     C     KJDO          begsr
     C                   endsr


      *====================================================================
      * MVDBSC - Move database to screen filed
      *====================================================================
     C     MVDBSC        begsr

     C     niky          chain(n)  pni01

     C                   if        %found
     C                   eval      sc20a2 = ni20a2                              Purpose of Report
     C                   eval      sc40a7 = ni40a7                              Purpose of Report
     C                   eval      sc1a1  = ni1a1                               Purpose of Report
     C                   eval      sc30a4 = ni30a4                              Purpose of Report
     C                   eval      sc10a1 = ni10a1                              Purpose of Report
     C                   eval      sc60a3 = ni60a3                              Work Activity
     C                   eval      sc60a4 = ni60a4
     C                   eval      sc60a1 = ni60a1                              Work Activity
     C                   eval      sc60a2 = ni60a2
     C                   eval      sc80a1 = ni80a1                              Work Activity
     C                   eval      sc80a2 = ni80a2
     C                   eval      sc40a8 = ni40a8
     C                   eval      sc3n2  = ni3n2
     C                   eval      sc3n3  = ni3n3
     C                   eval      sc3n4  = ni3n4
     C                   eval      sc3n5  = ni3n5
     C                   eval      sc9n1  = ni9n1

     C                   else

     C                   clear                   sc20a2                         Purpose of Report
     C                   clear                   sc40a7                         Purpose of Report
     C                   clear                   sc1a1                          Purpose of Report
     C                   clear                   sc30a4                         Purpose of Report
     C                   clear                   sc10a1                         Purpose of Report
     C                   clear                   sc60a3                         Work Activity
     C                   clear                   sc60a4
     C                   clear                   sc60a1                         Work Activity
     C                   clear                   sc60a2
     C                   clear                   sc80a1                         Work Activity
     C                   clear                   sc80a2
     C                   clear                   sc40a8
     C                   clear                   sc3n2
     C                   clear                   sc3n3
     C                   clear                   sc3n4
     C                   clear                   sc3n5
     C                   clear                   sc9n1
      *
     C                   endif
      *
     C                   endsr


      *====================================================================
      * MVSCDB - Move screen to Database filed
      *====================================================================
      *
     C     MVSCDB        begsr

     C                   eval      ni20a2 = sc20a2                              Purpose of Report
     C                   eval      ni40a7 = sc40a7                              Purpose of Report
     C                   eval      ni1a1  = sc1a1                               Purpose of Report
     C                   eval      ni30a4 = sc30a4                              Purpose of Report
     C                   eval      ni10a1 = sc10a1                              Purpose of Report
     C                   eval      ni60a3 = sc60a3                              Work Activity
     C                   eval      ni60a4 = sc60a4
     C                   eval      ni60a1 = sc60a1                              Work Activity
     C                   eval      ni60a2 = sc60a2
     C                   eval      ni80a1 = sc80a1                              Work Activity
     C                   eval      ni80a2 = sc80a2
     C                   eval      ni40a8 = sc40a8
     C                   eval      ni3n2  = sc3n2
     C                   eval      ni3n3  = sc3n3
     C                   eval      ni3n4  = sc3n4
     C                   eval      ni3n5  = sc3n5
     C                   eval      ni9n1  = sc9n1
      *
     C                   time                    dsdttm
      *
     C                   eval      niuser = q1user
     C                   eval      nipgm  = q1pgm
     C                   eval      nitime = dstime
      *
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nidt
      *
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
      *
     C     cmky          chain     pcm01                              99
     C                   z-add     dbrr3b        @@RRN
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
     C                   endsr
      *** END ADD ***** 06/05/06 *******************************************

      *====================================================================
      * OTHFKS - Processing for other functions keys
      *====================================================================
      *
     C     OTHFKS        begsr
      *
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
      *
     C                   endsr
    

      *====================================================================
      * PGMEND - End of program processing
      *====================================================================
      *
     C     PGMEND        begsr
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   if        cltid = '133'
     C   kl              eval      @@rtcd = 'B'
     C   kc              eval      @@rtcd = 'A'
     C                   endif
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   endsr
    

      *====================================================================
      * PGMSTR - Used for any start of program processing (one-time)
      *====================================================================
     C     PGMSTR        begsr

      *  Check for Display mode

     c                   if        q1prms >= 1
     c                   if        (@@CALL = '1')
     c                             or (@@Call = 'D')
     c                             or (@@Call = 'Z')
     c                   eval      *in09 = *on
     c                   endif
     c                   endif

      *
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
      *
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
      *
     C                   movel     ##data        scprds
      *
     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
      *
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
      *
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
      *
     C                   move      *blank        sccnam
     C     cmlsn         cat       ',':0         sccnam
     C     sccnam        cat       cmfsn:1       sccnam
     C     sccnam        cat       cmmid:1       sccnam
      *
     C                   move      *blank        scedn
     C     emky          chain     pem01                              60
     C                   movel     ememp#        scedn
     C                   move      emdiv         xxdiv
     C     scedn         cat       xxdiv:1       scedn
     C     scedn         cat       emname:1      scedn
      *
     C                   movel     @@fyr         scyrcs
     C     scyrcs        cat       '-':0         scyrcs
     C     scyrcs        cat       @@case:0      scyrcs
      *
     C                   move      *blank        scscl#
     C                   movel     cmacl#        scscl#
      *
     C                   move      cmacdt        xxacdt
     C                   move      xxacdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt                         Accident Date
      *
     C                   movel     cmjrst        kkst
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
     C                   exsr      mvdbsc
     C                   endsr


      *====================================================================
      * PREDSP - Pre-processing before displaying (every time)
      *====================================================================
     C     PREDSP        begsr
     C                   endsr


      *====================================================================
      * PRSCTL - Process control after no errors found (Call Submitter)
      *====================================================================
     C     PRSCTL        begsr
      *** BEGIN ADD *** 06/05/06 *******************************************
     C                   exsr      nxtpgm
      *** END ADD ***** 06/05/06 *******************************************
     C                   endsr


      *====================================================================
      * UPWRTR - Update/Write Database file
      *====================================================================
      *
     C     UPWRTR        begsr
      *
     C     niky          chain     pni01
      *
     C                   exsr      mvscdb
      *
     C                   if        %found
     C                   update    pni01
     C                   else
     C                   eval      nico# = cmco#
     C                   eval      nifnd = cmfnd
     C                   eval      nifyr = cmfyr
     C                   eval      nicase = cmcase
     C                   eval      nist = 'AL'
     C                   eval      niiusr = niuser
     C                   eval      niitm  = nitime
     C                   eval      niidt  = nidt
     C                   write     pni01
     C                   endif
      *
     C                   endsr


