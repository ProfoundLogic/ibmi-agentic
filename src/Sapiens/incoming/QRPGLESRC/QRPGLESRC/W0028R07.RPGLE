     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/TITLE W0028R07 - EFT Account Information                       *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: EFT Account Information                  *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 08/09/07  C:191 PROJECT WC1453                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H* 08/02/11 C:136 Support: WA099 77593                           *
     H*                Ensure that DFI ID, Ck Digit, and Acct #       *
     H*                populate with digits only                      *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                Add field "Account type"                       *
     H*                                                               *
     H* 06/29/09 C:191 Project: WC1806                                *
     H*               -Update params to match W0028R                  *
     H*               -Add employer mode when @@type = 'E'            *
     H*                                                               *
     H* 10/08/07 C:136 Project: WC997 PRISS 4                         *
     H*                Correct search logic                           *
     H*                                                               *
     H* 09/20/07 C:136 Project: WC997                                 *
     H*                EFT - Agent Commossions                        *
     H*****************************************************************
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0028f07  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmefp     uf a e             disk
     F                                     rename(pef01:dbrcdf)
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    F*
     F*** BEGIN DEL ****** 06/29/09 ************************************
     F****************           (for getting dependent name)
     F************wdacl     if   e           k disk
     F*************              (for getting provider name)
     F***********wmpdl     if   e           k disk
    F*  Claims physical file definition (for search)
     F********wmcmp     if   e             disk
    F**********
     F********wtgtl     if   e           k disk
    F**************                     (for search)
     F***********wdacp     if   e             disk    rename(pac01:pac01p)
     F*** END   DEL ****** 06/29/09 ************************************
      *** beg add ***** 09/20/07 **************************************
     Fwmaal1    if   e           k disk    rename(paa01:paa0l1)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
      *** end add ***** 09/20/07 **************************************
     F*** BEGIN ADD ****** 06/29/09 ************************************
     Fwmcml1    if   e           k disk
     F*** END   ADD ****** 06/29/09 ************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EFT Bank Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
      *** beg add ***** 10/08/07 **********************************************
     D in              s              2  0
      *** end add ***** 10/08/07 **********************************************
     D xxco#           s                   like(efco#)
     D kkco#           s                   like(efco#)
     D kkfnd           s                   like(effnd)
     D*** BEGIN DEL ****** 06/29/09 ************************************
     D********* kkfein          s                   like(effein)
     D********* kktbid          s                   like(gttbid)
     D********* kkkey           s                   like(gtkey)
     D*** END   DEl ****** 06/29/09 ************************************
     D kkfyr           s                   like(effyr)
     D kkmod           s                   like(efmod)
     D kkcase          s                   like(efcase)
     D*** BEGIN DEL ****** 06/29/09 ************************************
     D********* kkseq#          s                   like(efseq#)
     D*** END   DEl ****** 06/29/09 ************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   parm                    @@type            1
     C*** END   ADD ****** 06/29/09 ************************************
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*  to retrieve date in DTMDYY format
    C*
     C     dtmdyp        plist
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     dtcmdp        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *blanks       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
     C*
    C*================================================================
    C*  KLIST definitions
     C*
     C*** BEGIN DEL ****** 06/29/09 ************************************
     C**** pdky          klist
     C**************     kfld                    kkfein
     C**************     kfld                    kkmod
     C**************
     C*****acky          klist
     C**************     kfld                    kkco#
     C**************     kfld                    kkfnd
     C**************     kfld                    kkfyr
     C**************     kfld                    kkcase
     C**************     kfld                    kkseq#
    C**************
     C**** gtky          klist
     C**************     kfld                    kkco#
     C**************     kfld                    kktbid
     C**************     kfld                    kkkey
     C*** END   DEL ****** 06/29/09 ************************************

      *** beg add ***** 09/20/07 **************************************
     C     aaky          klist
     C                   kfld                    kkco#
     C                   kfld                    s1fnd
     C                   kfld                    s1agt#

     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    s1fnd
      *** end add ***** 09/20/07 **************************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*** END   ADD ****** 06/29/09 ************************************
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C*** BEGIN DEL ****** 06/29/09 ************************************
     C* ACH transaction code
     C*************      if        sctrcd = *blanks
     C*************      eval      *in71 = *on
     C*************      eval      *in90 = *on
     C*************      leavesr
     C*************      endif                                                  sctrcd = *blanks
     C*************
     C*************      eval      kkco# = xxco#
     C*************      eval      kktbid = ' ACH'
     C*************      eval      kkkey = sctrcd
     C*****gtky          chain     pgt01
     C*************      if        not %found(wtgtl)
     C*************      eval      *in76 = *on
     C*************      eval      *in90 = *on
     C*************      leavesr
     C*************      endif                                                  not %found(wtgtl)
     C*************
     C* ACH pre-note transaction code
     C*************      if        scpre  = *blanks
     C*************      eval      *in72 = *on
     C*************      eval      *in90 = *on
     C*************      leavesr
     C*************      endif                                                  scpre = *blanks
     C*************
     C*************      eval      kkkey = scpre
     C**** gtky          chain     pgt01
     C*************      if        not %found(wtgtl)
     C*************      eval      *in77 = *on
     C*************      eval      *in90 = *on
     C*************      leavesr
     C*************      endif                                                  not %found(wtgtl)
     C*** END   DEL ****** 06/29/09 ************************************
     C*
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C* Check / Saving Validation
     C                   if        scactp = *blanks or
     C                             (scactp <> 'C' and scactp <> 'S')
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scpre = *blanks
     C*** END   CHG ****** 06/29/09 ************************************
     C* Recieving DFI identification
     C                   if        scid#  = *blanks
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scid# = *blanks
     C* Recieving DFI check digit
     C                   if        scdgit = *blanks
     C                   eval      *in74 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scdigit = *blanks
     C* DFI account number
     C                   if        scacct = *blanks
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  scacct = *blanks
     C*
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   setoff                                       414243
     C                   setoff                                       4445
     C*** END   ADD ****** 06/29/09 ************************************
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   select
     C                   when      eftype = 'P'
     C                   eval      scfein =  effein
     C                   eval      scmod =  efmod
     C                   eval      scpnam =  efpnam
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C*****************  eval      *in41 = *off
     C                   eval      *in45 = *on
      *
     C****************   when      eftype = 'C'
     C                   when      eftype = 'C' or eftype = 'D'
     C*** END   CHG ****** 06/29/09 ************************************
     C                   eval      *in41 = *on
     C                   eval      scfnd = effnd
     C                   eval      scfyr = effyr
     C                   eval      sccase = efcase
     C                   eval      scss#  = %char(effein)
     C******  Begin Add  ***  06/09/20  **************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scss#'       qqname           25
     C     scss#         parm      scss#         qqval           100
     C******  End Add  ***  06/09/20  ****************
     C                   eval      scclmn =  efpnam
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   if        eftype = 'D'
     C*** END   ADD ****** 06/29/09 ************************************
     C                   eval      kkco# = efco#
     C                   eval      kkfnd = effnd
     C                   eval      kkfyr = effyr
     C                   eval      kkcase = efcase
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C****************   eval      kkseq# = efseq#
     C*****acky          chain     pac01
     C****************   if        %found(wdacl)
     C****************   eval      scdnam =  acpnam
     C*
     C                   eval      *in43 = *on
     C                   eval      scdnam =  efpnam
     C     cmky          chain     pcm01
     C                   if        %found(wmcml1)
     C                   eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C                   endif                                                  %found
     C                   endif                                                  eftype='D'
     C*** END   CHG ****** 06/29/09 ************************************
      *** beg add ***** 09/20/07 *******************************************************
     C                   when      eftype = 'A'
     c                   eval      *in42 = *on
     C                   eval      kkco# = efco#
     C                   eval      s1fnd = effnd
     C     fnky          chain     pfn0l
     c                   eval      s1fndm = fnname
     c                   eval      s1agt# = efagt#
     c                   eval      s1fein = effein
     c                   eval      s1mod  = efmod
     c     aaky          chain     paa0l1
     c                   eval      s1agtn = aaname
      *** end add ***** 09/20/07 *******************************************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   when      eftype = 'E'
     c                   eval      *in44 = *on
     C                   eval      kkco# = efco#
     C                   eval      s1fnd = effnd
     C     fnky          chain     pfn0l
     c                   eval      s1fndm = fnname
     c*
     c                   eval      scemp# = efemp#
     c                   eval      scdiv  = efdiv
     c                   eval      s1fein = effein
     c                   eval      s1mod  = efmod
     c                   eval      scempn = efpnam
     C*** BEG   ADD ****** 04/21/11 ************************************
     c                   eval      sceatp = efeatp
     C*** END   ADD ****** 04/21/11 ************************************
     C*** END   ADD ****** 06/29/09 ************************************
     C                   endsl
     C*
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C****************   eval      sctrcd = eftrcd
     C****************   eval      scpre = efpre
     C                   eval      scactp = efactp
     C*** END   CHG ****** 06/29/09 ************************************
     C                   eval      scid# = efid#
     C                   eval      scdgit = efdgit
     C                   eval      scacct = efacct
      *
     C                   z-add     efefdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         SCEFDT
     C                   move      *blanks       SRDTO
     C*
     C                   z-add     efexdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         SCEXDT
     C                   move      *blanks       SRDTO
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      efpgm = q1pgm
     C                   eval      efuser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         efdt
     C                   eval      eftime = dstime
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C***************    movel     sctrcd        eftrcd
     C***************    movel     scpre         efpre
     C                   movel(p)  scactp        efactp
     C*** END   CHG ****** 06/29/09 ************************************
     C                   movel     scid#         efid#
     C                   movel     scdgit        efdgit
     C                   movel     scacct        efacct
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C***
     C* Process search key
     C                   select
     C                   when      *inkj = *on
     C                   exsr      kjdo
     C                   endsl
     C***
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      xxco# = upco#
     C                   endif                                                  %found
     C*
     C                   eval      @@mode = 'C'
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmefp
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr
     C***
     C*** BEGIN DEL ****** 06/29/09 ************************************
     C*************      select
     C*************
     C*************      when      hfld = 'SCTRCD'  or hfld = 'SCPRE'
     C*************
     C*************      CALL      'W2493R'
     C*************      PARM      OPTNM         ##PRDS           40
     C*************      PARM      *BLANKS       ##CALL            1
     C*************      PARM      ' ACH'        ##ID              4
     C*************      PARM      ' '           ##KY             10
     C*************      PARM      *BLANK        ##RTCD            1
     C*************
      *** beg add ***** 10/08/07 **********************************************
     c*************      select
     c*************      when      hfld = 'SCTRCD'
     c*************      eval      in = 80
     c*************      when      hfld = 'SCPRE'
     c*************      eval      in = 81
     c*************      endsl
      *** end add ***** 10/08/07 **********************************************
     C*
     C*************      select
     C*************      when      ##RTCD = 'A'
     C*************      move      '1'           *INKC
     C*************
     C*************      when      ##RTCD = 'B'
     C*************      move      '1'           *INKL
     C*************
     C*************      when      ##RTCD =  '*'
      *** beg add ***** 10/08/07 **********************************************
     c*************      eval      in = in + 1
      *** end add ***** 10/08/07 **********************************************
     C*************      if        hfld = 'SCTRCD'
     C*************      movel(p)  ##KY          sctrcd
     C*************      else
     C*************      movel(p)  ##KY          scpre
     C*************      endif                                                  hfld ='SCTRCD'
     C*************      endsl
     c*************
      *** beg add ***** 10/08/07 **********************************************
     c*************      eval      *in(in) = *on
      *** end add ***** 10/08/07 **********************************************
     C*** END   DEL ****** 06/29/09 ************************************
     C*
      *** beg del ***** 10/08/07 **********************************************
     C***********        when      hfld = 'SCFND' or
     C***********                  hfld = 'SCFYR' or
     C***********                  hfld = 'SCCASE'
     C***********        call      'W0070R'
     C***********        parm      '2'           ##call            1
     C******inkc*        parm      *blanks       ##kc              1
     C******inkl*        parm      *blanks       ##kl              1
     C***********        parm      ' '           ##mode            1
     C***********        parm      *zeros        ##rrn             9 0
     C***********        parm      'CN'          ##sort            2
     C***********        parm      *zeros        ##rrn2            9 0
     C***********        parm      *zeros        ##fyr             3 0
     C***********
     C***********        if        *inkl <> *on and ##rrn <> *zeros
     C*****##rrn*        chain     pcm01
     C***********        if        %found
     C***********        eval      scfnd = cmfnd
     C***********        eval      scfyr = cmfyr
     C***********        eval      sccase = cmcase
     C***********        eval      scss#  = cmss#
     C***********        eval      scclmn = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C***********                         + ' ' + %trim(cmmid)
     C***********        seton                                        25
     C***********        endif                                                  %found
     C***********        else
     C***********        seton                                        21
     C***********        endif                                                  *inkl <> *on
     C***********
     C*****for*searching dependent
     C***********        when      hfld = 'SCDNAM'
     C***********        If        sccase <> 0
     C***********        call      'W0573R'
     C***********        parm      '2'           ##call            1
     C***********        parm      'Y'           ##pmtf            1
     C******inkc*        parm      *blanks       ##kc              1
     C******inkl*        parm      *blanks       ##kl              1
     C***********        parm      ##rrn         ##rrn             9 0
     C***********
     C***********        if        *inkl <> *on and ##rrn <> *zeros
     C*****##rrn*        chain     pac01p
     C***********        if        %found
     C***********        movel     acpnam        scpnam
     C***********        seton                                        26
     C***********        endif                                                  %found
     C***********        else
     C***********        seton                                        24
     C***********        endif                                                  *inkl <> *on
     C***********        endif
     C***********
     C*****payee*and its fed id
     C***********        when      hfld = 'SCFEIN' or
     C***********                  hfld = 'SCMOD'
     C***********        call      'W0030R'
     C***********        parm      '2'           ##call            1
     C***********        parm      *blanks       ##fein            9
     C***********        parm      *blanks       ##mod             3
     C***********        parm      *blanks       ##rtcd            1
     C***********        movel     ##fein        kkfein
     C***********        movel     ##mod         kkmod
     C***********
     C*****pdky**        chain     wmpdl
     C***********        if        %found
     C***********        movel     pdname        scpnam
     C***********        movel     pdfein        scfein
     C***********        movel     pdmod         scmod
     C***********        seton                                            26
     C***********        else
     C***********        seton                                            28
     C***********        endif                                                  %found
      *** end del ***** 10/08/07 **********************************************
     C*** BEGIN DEL ****** 06/29/09 ************************************
     C*************      endsl
     C*** END   DEL ****** 06/29/09 ************************************
     C                   seton                                        90
     C
     C                   endsr
