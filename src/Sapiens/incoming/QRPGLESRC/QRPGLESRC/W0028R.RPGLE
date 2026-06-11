     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0028R - Work With EFT Account Information               *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Work With EFT Account Information        *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W0028R02                                            *
     H*                                                               *
     H* CREATED: 08/09/07  C:191 PROJECT WC1453                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 05/19/17 C:136 Project: WC2497                                *
     H*                Add Group Restriction logic                    *
     H*                                                               *
     H* 07/24/14 C:136 Support: 36216                                 *
     H*                Correct filtering for providers in subset      *
     H*                routine                                        *
     H*                                                               *
     H* 08/30/11 C:136 Support: WA099 77821                           *
     H*                Add grp value to subfile screen for Employers  *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                ACH for Premium Payments via Agent.            *
     H*                                                               *
     H* 10/26/09 C:191 Project: WC1806 Priss                          *
     H*                - Show err message if is pre-note is bypassed  *
     H*                                                               *
     H* 06/25/09 C:136 Project: WC1806                                *
     H* 06/29/09 C:191 Project: WC1806                                *
     H*               -Pull in subset info by type and RRN            *
     H*               -Add employer view when @@type = 'E'            *
     H*               -Add option 12 to create prenote request        *
     H*                                                               *
     H* 05/14/08 C:33  Project: WC997  Priss# 11                      *
     H*                Use the group passed in instead of the user    *
     H*                 parm group.                                   *
     H*                                                               *
     H* 12/13/07 C: 53 Project: WC1453 priss 3                        *
     H*                                                               *
     H* 09/20/07 C:136 Project: WC997                                 *
     H*                EFT - Agent Commossions                        *
     H*****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0028f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmefp     if   e             disk
     F                                     rename(pef01:pfrcdf)
    F*
    F*  First logical file definition (for Ist View)
     Fwmefl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pef01:dbrcdf)
    F*
    F*  Second logical file definition (for 2nd View)
     Fwmefl2    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pef01:dbrcdf2)
    F*
      *** beg add ***** 06/25/09 **********************************************
    F*  1st Logical file definition (for Employer View)
     Fwmefl5    if   e           k disk
     F                                     infds(dbremp1)
     F                                     rename(pef01:pef05)
    F*
    F*  2nd Logical file definition (for Employer View)
     Fwmefl6    if   e           k disk
     F                                     infds(dbremp2)
     F                                     rename(pef01:pef06)
      *** end add ***** 06/25/09 **********************************************
    F*
      *** beg del ***** 06/25/09 **********************************************
     F*  User id file definition (for getting user param)
     F*********** wmupl     if   e           k disk
      *** end del ***** 06/25/09 **********************************************
      *** beg add ***** 06/25/09 **********************************************
     fwmaap     if   e             disk    rename(paa01:paa0p)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwdacp     if   e             disk    rename(pac01:pac0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmpdp     if   e             disk    rename(ppd01:ppd0p)
      *** end add ***** 06/25/09 **********************************************

    D*
     D hd              s             78    dim(4) ctdata perrcd(1)
     D*** BEGIN CHG ****** 06/29/09 ************************************
     D************* of              s              1    dim(14) ctdata perrcd(1)
     D************* ofd             s             35    dim(14) alt(of)
     D of              s              1    dim(15) ctdata perrcd(1)
     D ofd             s             35    dim(15) alt(of)
     D*** END   CHG ****** 06/29/09 ************************************
      *** beg add ***** 06/25/09 **********************************************
     D ed              s             78    dim(4) ctdata perrcd(1)
      *** end add ***** 06/25/09 **********************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EFT Account Information'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0028R02'
     D eftpgm          c                   'W0028R07'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   2
     D viewmx          c                   2
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(efco#)
     D kkfein          s                   like(effein)
     D kkmod           s                   like(efmod)
     D kkefdt          s                   like(efefdt)
     D kkpnam          s                   like(efpnam)
      *** beg add ***** 09/20/07 *******************************************
     d kktype          s                   like(eftype)
     d xxtype          s                   like(eftype)
     d xxagt#          s                   like(efagt#)
     d x1fnd           s                   like(effnd)
      *** end add ***** 09/20/07 *******************************************
      *** beg add ***** 06/25/09 *******************************************
     d NonDisplay      c                   const(x'A7')
     d EditwLine       c                   const(x'24')
     d EditHI          c                   const(x'22')
     d EditHIwLine     c                   const(x'26')
     d protectHI       c                   const(x'A2')
     d protectNORM     c                   const(x'A0')
     D string1         s                   like(EFPNAM)
     D string2         s                   like(EFPNAM)
     d strlen          s              3  0
     d start           s              3  0
     d patlen          s              3  0
     d trans           s              1
     d trim            s              1
     d wild            s              1
     d scnres          s              3  0
     d workDate        s               d
     d kkfnd           s                   like(EFFND)
     d kkemp           s                   like(EFEMP#)
     d kkdiv           s                   like(EFDIV)
     d xxadt6          s              6  0
     d xxefd6          s              6  0
     d xxexd6          s              6  0
     d xxfdt6          s              6  0
     d xxldt6          s              6  0
     d xxpdt6          s              6  0
     D ##type          s              1
     D ##rrn1          s              9  0
      *** end add ***** 06/25/09 *******************************************
      *** beg add ***** 04/21/11 *******************************************
     DprIsActive       pr             1n
      *** end add ***** 04/21/11 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
      *** beg chg ***** 10/05/07 *************************************
     D***********  @@clm            DS
     D***********  xxclm                          13
     D***********  xxfnd                           3  0 overlay(xxclm)
     D***********  xxfyr                           3  0 overlay(xxclm : 4)
     D***********  xxcase                          7  0 overlay(xxclm : 7)
     Dxxclm            DS            13
     Dxxfnd                           3  0 overlay(xxclm)
     Dxxfyr                           3  0 overlay(xxclm : 4)
     Dxxcase                          7  0 overlay(xxclm : 7)
      *** end chg ***** 10/05/07 *************************************
    C*
     D@@fein           DS
     D##fein                         12
     Dxxfein                          9  0 overlay(##fein)
     Dxxmod                           3  0 overlay(##fein : 10)
      *** beg add ***** 06/25/09 ***********************************************
     D dbremp1         ds
     D emrrn1                397    400b 0
     D dbremp2         ds
     D emrrn2                397    400b 0
     D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
      *** end add ***** 06/25/09 ***********************************************
    C*
      *** beg add ***** 06/25/09 *************************************
     C/copy sprstring
      *** end add ***** 06/25/09 *************************************
      *** beg add ***** 05/19/17 *************************************
      /copy wprgroup
      *** end add ***** 05/19/17 *************************************
     C/COPY SCOPYWW01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@rrn             9 0
     C                   parm                    @@clm            13
     C                   parm                    @@fein           12
     C                   parm                    @@seq#            5 0
      *** beg add ***** 09/20/07 *********************************************
     C                   parm                    @@type            1
     C                   parm                    @@fnd             3 0
     C                   parm                    @@agt#            5 0
      *** end add ***** 09/22/07 *********************************************
    C*
    C*  Add/change/display program parameter list (Restrict user to adding records for a particular
    C*  type, depending on how this program is called.
     *
      *** beg chg ***** 06/25/09 *********************************************
    C*****acdpl         plist
    C******inkc         parm                    ##kc
    C******inkl         parm                    ##kl
    C**********         parm                    ##mode
    C**********         parm      scdbrr        ##rrn
     ****beg*add ***** 09/20/07 *********************************************
    C**********         parm                    ##clm            13
    C**********         parm                    ##fein
    C**********         parm                    ##seq#            5 0
    C**********         parm                    ##type            1
    C**********         parm                    ##fnd             3 0
    C**********         parm                    ##agt#            5 0
      *** end add ***** 09/20/07 *********************************************
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      @@type        ##type
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn1
      *** end chg ***** 06/25/09 *********************************************
    C*
    C*  EFT Account Information parameter list
    C*
     C     eftpl         plist
     C     *inkc         parm                    ##kc                           ???
     C     *inkl         parm                    ##kl                           ???
     C                   parm                    ##mode
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   parm      @@type        ##type
     C*** END   ADD ****** 06/29/09 ************************************
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     DTMDYP        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkpnam
     C                   kfld                    kkfein
     C                   kfld                    kkMOD
     C                   kfld                    kkEFDT
      *** beg add ***** 09/20/07 ******************************************************
    C                   kfld                    kktype
      *** end add ***** 09/20/07 ******************************************************
    C*
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfein
     C                   kfld                    kkMOD
     C                   kfld                    kkEFDT
      *** beg add ***** 09/20/07 ******************************************************
    C                   kfld                    kktype
      *** end add ***** 09/20/07 ******************************************************
    C*
      *** beg add ***** 06/25/09 ******************************************************
     C     emky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkpnam
     C                   kfld                    kkfnd
     C                   kfld                    kkemp
     C                   kfld                    kkdiv
     C     emky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp
     C                   kfld                    kkdiv
      *** end add ***** 06/25/09 ******************************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
      *** beg chg ***** 06/25/09 ***************************************
     C***********        if        sstype <> *blank or ssfyr <> 0 or
     C***********                  ssfein <> 0
     C***********        seton                                        11
     C***********        endif
     C                   if        sstype <> *blank or
     c                             sspnam <> *blank or
     c                             ssfyr <> 0 or
     C                             ssfein <> 0 or
     c                             ssmod <> 0 or
     c                             ssagt# <> 0 or
     c                             ssfnd <> 0 or
     c                             ssemp# <> 0 or
     c                             ssdiv <> 0 or
     c                             sscase <> 0 or
     c                             ssseq# <> 0 or
      *** beg add ***** 04/21/11 *********************************************
     c                             ssactv <> *blank
      *** end add ***** 04/21/11 *********************************************
     C                   seton                                        11
     C                   endif
      *** end chg ***** 06/25/09 ***************************************
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   eval      *in90 = *off
     C                   eval      *in66 = *off
     C                   If        scsel = '10'
     C                   If        scadt  <> 0 or scftdt <> 0
     C                   eval      *in66 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   endif
     C                   endif
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   if        scsel = '12'
     C                   if        scpdt <> 0
     C                   eval      *in67 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   leavesr
     C                   endif                                                  scpdt <> 0
     C*
     C*** BEGIN ADD ****** 10/26/09 ************************************
     C                   if        scid# = *blanks
     C                   eval      *in64 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   leavesr
     C                   endif                                                  ##pre <> *on
     C*** END   ADD ****** 10/26/09 ************************************
     C*
     C* Check validity for prenote processing
     C                   move      ##co#         #1co#
     C                   move      sctype        ##type
     C                   move      scfnd         #1fnd
     C                   move      sckey         ##key
     C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ##date
     C*
     C                   call      'WA023R3'
     C                   parm                    #1co#             3
     C                   parm                    ##type            1
     C                   parm                    #1fnd             3
     C                   parm                    ##key            30
     C                   parm                    ##date            7
     C                   parm                    ##eft             1
     C                   parm                    ##pre             1
     C*
     C                   if        ##eft <> *on
     C                   eval      *in68 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   leavesr
     C                   endif                                                  ##pre <> *on
     C*** BEGIN ADD ****** 10/26/09 ************************************
     C                   if        ##pre <> *on
     C                   eval      *in65 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   leavesr
     C                   endif                                                  ##pre <> *on
     C*** END   ADD ****** 10/26/09 ************************************
     C*
     C                   endif                                                  scsel = '12'
     C*** END   ADD ****** 06/29/09 ************************************
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
      *** beg add ***** 04/21/11 *********************************************
     c                   if        ssactv <> ' ' and ssactv <> 'A'
     c                             and ssactv <> 'I'
     C                   eval      *in89 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
      *** end add ***** 04/21/11 *********************************************
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkfein = *loval
     C                   eval      kkpnam = *loval
     C                   eval      kkefdt = *loval
     C                   eval      kkmod  = *loval
      *** beg add ***** 09/20/07 *************************************
     C                   eval      kktype = *loval
      *** end add ***** 09/20/07 *************************************
      *** beg add ***** 06/25/09 *************************************
     C                   eval      kkco# = ##co#
      *** end add ***** 06/25/09 *************************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   select
     C                   when      *in21
     C                   eval      kkpnam = scpst1
     C                   eval      kkfein = *loval
     C                   when      *in22
     C                   movel     scpst2        kkfein
      *** beg add ***** 06/25/09 **************************************
     c                   if        @@type = 'E'
     c                   move(p)   scpst9        kkfnd
     c                   eval      kkemp  = scpsta
     c                   eval      kkdiv  = scpstb
     c                   endif
      *** end add ***** 06/25/09 **************************************
     C                   endsl
     C*
     C                   eval      kkefdt = *loval
     C                   eval      kkmod  = *loval
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   movel(p)  scpnam        kkpnam
      *** beg chg ***** 06/09/20 **************************************
     C*                  eval      kkfein = scfein
     C                   move      scfein        kkfein
      *** end chg ***** 06/09/20 **************************************
     C                   eval      kkmod  = scmod
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         kkefdt
      *** beg add ***** 09/20/07 **************************************
      *
      *  A small possibility of duplicate fein and mods, this will prevent paging errors in the
      *  subfile processing.
      *
     c                   eval      kktype = sctype
      *** end add ***** 09/20/07 **************************************
      *** beg add ***** 06/25/09 **************************************
     c                   eval      kkfnd  = scfnd
     c                   eval      kkemp  = scemp#
     c                   eval      kkdiv  = scdiv
      *** end add ***** 06/25/09 **************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     efpnam        scpnam
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  eval      scfein = effein
     C                   eval      scfein = %char(effein)
     C***  End Chg  ***  06/09/20  *******************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scfein'      qqname           25
     C     scfein        parm      scfein        qqval           100
     C***  End Add  ***  06/09/20  *******************************
     C                   eval      sctype = eftype
     C                   eval      scfnd  = effnd
     C                   eval      scmod  = efmod
      *** beg add ***** 06/25/09 **************************************
     c                   eval      scid#  = efid#
     C                   eval      sckey  = efkey
     c                   if        @@type = 'E'
     c                   eval      schdl1 = ed(hd#)
     c                   endif
     c                   eval      scemp# = efemp#
     c                   eval      scdiv  = efdiv
      *** beg add ***** 04/21/11 **************************************
     c                   eval      sceatp = efeatp
      *** end add ***** 04/21/11 **************************************
      *** end add ***** 06/25/09 **************************************
     C*    call up conversion SR for date
     C                   z-add     efefdt        srdti
     C                   call      'SRCVTDAT'    DTMDYP
     C                   MOVEL     SRDTO         scefdt
     C                   move      *blanks       SRDTO
     C
     C                   z-add     efexdt        srdti
     C                   call      'SRCVTDAT'    DTMDYp
     C                   MOVEL     SRDTO         SCEXDT
     C                   move      *blanks       SRDTO
     C
     C                   z-add     efpdt         srdti
     C                   call      'SRCVTDAT'    DTMDYp
     C                   MOVEL     SRDTO         scpdt
     C                   move      *blanks       SRDTO
     C
     C                   z-add     efadt         srdti
     C                   call      'SRCVTDAT'    DTMDYp
     C                   MOVEL     SRDTO         scadt
     C                   move      *blanks       SRDTO
     C
     C                   z-add     efftdt        srdti
     C                   call      'SRCVTDAT'    DTMDYp
     C                   MOVEL     SRDTO         scftdt
     C                   move      *blanks       SRDTO
     C
     C                   z-add     efltdt        srdti
     C                   call      'SRCVTDAT'    DTMDYp
     C                   MOVEL     SRDTO         scltdt
     C                   move      *blanks       SRDTO
     C
      *** beg chg ***** 06/25/09 **************************************
     C**********         select
     C**********         when      info# = 1
    C**********
     C**When*View 1 (Payee view) is on and Info 1 is on
     C**********         If        *in21 = *on
     C**********         eval      scdata = ' ' + %triml(scpnam) + '  '
     C**********                          + %editc(scfein:'4') + '  '
     C**********                          + %editw(scefdt:'  /  /    ') + '  '
     C**********                          + %editw(SCEXDT:'  /  /    ') + '    '
     C**********                          + %editw(scpdt :'  /  /    ')
     C**********         endif
    C**********
     C**********         If        *in22 = *on
     C**When*View 2 (SS#/FEIN view) is on and Info 1 is on
     C**********         eval      scdata = ' ' +  %editc(scfein:'4') + ' '
     C**********                          + %triml(scpnam) + '  '
     C**********                          + %editw(scefdt:'  /  /    ') + '  '
     C**********                          + %editw(SCEXDT:'  /  /    ') + '    '
     C**********                          + %editw(scpdt :'  /  /    ')
     C**********         endif
    C**********
     C**********         when      info# = 2
    C**********
     C**********         If        *in21 = *on
     C**When*View 1 (Payee view) is on and Info 1 is on
     C**********         eval      scdata = ' ' + %triml(scpnam) + ' '
     C**********                          + %editc(scfein:'4') + ' '
     C**********                          + %editw(scadt :'  /  /    ') + '   '
     C**********                       + %editw(scftdt:'  /  /    ') + '     '
     C**********                          + %editw(scltdt:'  /  /    ')
     C**********         endif
    C**********
     C**********         If        *in22 = *on
     C**When*View 2 (SS#/FEIN view) is on and Info 2 is on
     C**********         eval      scdata = ' ' +  %editc(scfein:'4') + ' '
     C**********                          + %triml(scpnam) + ' '
     C**********                          + %editw(scadt :'  /  /    ') + '   '
     C**********                       + %editw(scftdt:'  /  /    ') + '      '
     C**********                          + %editw(scltdt:'  /  /    ')
     C**********         endif
    C**********
     C**********         endsl

     c                   if        efefdt <> 0
     c     *cymd         test(de)                efefdt
     c                   if        not(%error)
     c     *cymd         move      efefdt        workDate
     c     *mdy          move      workDate      xxefd6
     c                   else
     c                   eval      xxefd6 = 0
     c                   endif
     c                   else
     c                   eval      xxefd6 = 0
     c                   endif

     c                   if        efexdt <> 0
     c     *cymd         test(de)                efexdt
     c                   if        not(%error)
     c     *cymd         move      efexdt        workDate
     c     *mdy          move      workDate      xxexd6
     c                   else
     c                   eval      xxexd6 = 0
     c                   endif
     c                   else
     c                   eval      xxexd6 = 0
     c                   endif

     c                   if        efpdt  <> 0
     c     *cymd         test(de)                efpdt
     c                   if        not(%error)
     c     *cymd         move      efpdt         workDate
     c     *mdy          move      workDate      xxpdt6
     c                   else
     c                   eval      xxpdt6 = 0
     c                   endif
     c                   else
     c                   eval      xxpdt6 = 0
     c                   endif

     c                   if        efadt  <> 0
     c     *cymd         test(de)                efadt
     c                   if        not(%error)
     c     *cymd         move      efadt         workDate
     c     *mdy          move      workDate      xxadt6
     c                   else
     c                   eval      xxadt6 = 0
     c                   endif
     c                   else
     c                   eval      xxadt6 = 0
     c                   endif

     c                   if        efftdt <> 0
     c     *cymd         test(de)                efftdt
     c                   if        not(%error)
     c     *cymd         move      efftdt        workDate
     c     *mdy          move      workDate      xxfdt6
     c                   else
     c                   eval      xxfdt6 = 0
     c                   endif
     c                   else
     c                   eval      xxfdt6 = 0
     c                   endif

     c                   if        efltdt <> 0
     c     *cymd         test(de)                efltdt
     c                   if        not(%error)
     c     *cymd         move      efltdt        workDate
     c     *mdy          move      workDate      xxldt6
     c                   else
     c                   eval      xxldt6 = 0
     c                   endif
     c                   else
     c                   eval      xxldt6 = 0
     c                   endif

     c                   select
     c                   when      @@type = 'E'
      *** beg chg ***** 08/30/11 ******************************
     c***********        select
     c***********        when      *in21 and info# = 1
     C***********        eval      scdata = ' '
     C***********                  + %triml(scpnam)
     c***********                  + '  '
     C***********                  + %editc(scemp#:'4')
     c***********                  + ' '
     C***********                  + %editc(scdiv:'3')
     c***********                  + ' '
     C***********                  + %editw(xxefd6:'  /  /  ')
     c***********                  + '  '
     C***********                  + %editw(xxexd6:'  /  /  ')
     c***********                  + '    '
     C***********                  + %editw(xxpdt6:'  /  /  ')
      ****beg*add ***** 04/21/11 **************************************
     c***********                  + '   '
     C***********                  + %triml(sceatp)
      ****end*add ***** 04/21/11 **************************************
     c***********        when      *in21 and info# = 2
     C***********        eval      scdata = ' '
     C***********                  + %triml(scpnam)
     c***********                  + ' '
     C***********                  + %editc(scemp#:'4')
     c***********                  + ' '
     C***********                  + %editc(scdiv:'3')
     c***********                  + '  '
     C***********                  + %editw(xxadt6:'  /  /  ')
     c***********                  + '  '
     C***********                  + %editw(xxfdt6:'  /  /  ')
     c***********                  + '    '
     C***********                  + %editw(xxldt6:'  /  /  ')
      ****beg*add ***** 04/21/11 **************************************
     c***********                  + '   '
     C***********                  + %triml(sceatp)
      ****end*add ***** 04/21/11 **************************************
     c***********        when      *in22 and info# = 1
     C***********        eval      scdata = ' '
     C***********                  + %editc(scemp#:'4')
     c***********                  + ' '
     C***********                  + %editc(scdiv:'3')
     c***********                  + ' '
     C***********                  + %triml(scpnam)
     c***********                  + '  '
     C***********                  + %editw(xxefd6:'  /  /  ')
     c***********                  + '  '
     C***********                  + %editw(xxexd6:'  /  /  ')
     c***********                  + '    '
     C***********                  + %editw(xxpdt6:'  /  /  ')
      ****beg*add ***** 04/21/11 **************************************
     c***********                  + '   '
     C***********                  + %triml(sceatp)
      ****end*add ***** 04/21/11 **************************************
     c***********        when      *in22 and info# = 2
     C***********        eval      scdata = ' '
     C***********                  + %editc(scemp#:'4')
     c***********                  + ' '
     C***********                  + %editc(scdiv:'3')
     c***********                  + '  '
     C***********                  + %triml(scpnam)
     c***********                  + '  '
     C***********                  + %editw(xxadt6:'  /  /  ')
     c***********                  + '  '
     C***********                  + %editw(xxfdt6:'  /  /  ')
     c***********                  + '    '
     C***********                  + %editw(xxldt6:'  /  /  ')
      ****beg*add ***** 04/21/11 **************************************
     c***********                  + '  '
     C***********                  + %triml(sceatp)
      ****end*add ***** 04/21/11 **************************************
     c***********        endsl

     c                   select
     c                   when      *in21 and info# = 1
     c                   eval      scdata = ' '
     c                             + %triml(scpnam)
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     c                             + %editc(scemp#:'4')
     c                             + ' '
     c                             + %editc(scdiv:'3')
     c                             + ' '
     c                             + %editw(xxefd6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxexd6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxpdt6:'  /  /  ')
     c                             + '  '
     c                             + %triml(sceatp)
     c                   when      *in21 and info# = 2
     c                   eval      scdata = ' '
     c                             + %triml(scpnam)
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     c                             + %editc(scemp#:'4')
     c                             + ' '
     c                             + %editc(scdiv:'3')
     c                             + ' '
     c                             + %editw(xxadt6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxfdt6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxldt6:'  /  /  ')
     c                             + '  '
     c                             + %triml(sceatp)
     c                   when      *in22 and info# = 1
     c                   eval      scdata = ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     c                             + %editc(scemp#:'4')
     c                             + ' '
     c                             + %editc(scdiv:'3')
     c                             + ' '
     c                             + %triml(scpnam)
     c                             + ' '
     c                             + %editw(xxefd6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxexd6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxpdt6:'  /  /  ')
     c                             + '  '
     c                             + %triml(sceatp)
     c                   when      *in22 and info# = 2
     c                   eval      scdata = ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     c                             + %editc(scemp#:'4')
     c                             + ' '
     c                             + %editc(scdiv:'3')
     c                             + ' '
     c                             + %triml(scpnam)
     c                             + ' '
     c                             + %editw(xxadt6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxfdt6:'  /  /  ')
     c                             + ' '
     c                             + %editw(xxldt6:'  /  /  ')
     c                             + '  '
     c                             + %triml(sceatp)
     c                   endsl
      *** end chg ***** 08/30/11 ******************************
     c                   other
     c                   select
     c                   when      *in21 and info# = 1
     C                   eval      scdata = ' '
     C                             + %triml(scpnam)
     c                             + ' '
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                            + %editc(scfein:'4')
     C                             + %trim(scfein)
     C***  End Chg  ***  06/09/20  *******************************
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     C                             + %editw(scefdt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scexdt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scpdt :'  /  /    ')
     c                   when      *in21 and info# = 2
     C                   eval      scdata = ' '
     C                             + %triml(scpnam)
     c                             + ' '
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                            + %editc(scfein:'4')
     C                             + %trim(scfein)
     C***  End Chg  ***  06/09/20  *******************************
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     C                             + %editw(scadt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scftdt:'  /  /    ')
     c                             + '  '
     C                             + %editw(scltdt:'  /  /    ')
     c                   when      *in22 and info# = 1
     C                   eval      scdata = ' '
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                            + %editc(scfein:'4')
     C                             + %trim(scfein)
     C***  End Chg  ***  06/09/20  *******************************
     c                             + ' '
     C                             + %triml(scpnam)
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     C                             + %editw(scefdt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scexdt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scpdt :'  /  /    ')
     c                   when      *in22 and info# = 2
     C                   eval      scdata = ' '
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                            + %editc(scfein:'4')
     C                             + %trim(scfein)
     C***  End Chg  ***  06/09/20  *******************************
     c                             + ' '
     C                             + %triml(scpnam)
     c                             + ' '
     c                             + %editc(scfnd:'4')
     c                             + ' '
     C                             + %editw(scadt:'  /  /    ')
     c                             + ' '
     C                             + %editw(scftdt:'  /  /    ')
     c                             + '  '
     C                             + %editw(scltdt:'  /  /    ')
     c                   endsl
     c                   endsl
      *** end chg ***** 06/25/09 **************************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'scfein'      qqname           25
     C     scfein        parm      scfein        qqval           100
     C***  End Add  ***  06/09/20  *******************************
    C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C     scsel         caseq     '7'           opt07
     C     scsel         caseq     '10'          opt10
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C     scsel         caseq     '12'          opt12
     C*** END   ADD ****** 06/29/09 ************************************
     C                   endcs
     C                   endsr
    C*================================================================
    C* OPT07  - Process option 07
    C*================================================================
     C     opt07         begsr
    C*
     C                   eval      ##mode = ' '
      **** call W0028R07
     C                   call      eftpgm        eftpl
    C*
     C                   endsr
    C*================================================================
    C* OPT10  - Process option 10
    C*================================================================
     C     opt10         begsr
      **** process it within this program
     C                   move      'S'           ##mode
     C                   call      acdpgm        acdpl
     C                   endsr
     C*** BEGIN CHG ****** 06/29/09 ************************************
    C*================================================================
    C* OPT12  - Process option 12
    C*================================================================
     C     opt12         begsr
      *
      **** Call API to write pre-note to Central Holding file
     C                   call      'WA073R'
     C                   parm      sctype        ##type
     C                   parm      *zeros        ##rrn1
     C                   parm      scdbrr        ##rrn
     C                   parm      *blanks       ##rtn             1
      *
     C                   if        ##rtn <> ' '
     C                   eval      *in69 = *on
     C                   eval      *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   endif                                                  ##rtn <> ' '
     C*
      **** Call API again to cleanup
     C                   call      'WA073R'
     C                   parm      'X'           ##type
     C                   parm      *zeros        ##rrn1
     C                   parm      *zeros        ##rrn
     C                   parm      *blanks       ##rtn             1
     C*
     C                   endsr
     C*** END   CHG ****** 06/29/09 ************************************
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
      *** beg del ***** 06/25/09 **************************************
     C* get company no. from user parameter
     C*****q1user        chain     pup01
     C*********          if        %found
     C*********          eval      kkco# = upco#
     C*********          endif                                                  %found
     C*********
      ****beg*add ***** 09/20/07 **************************************
      *********
      ***if*type is passed in, subset on it; otherwise, allow claims and provider entries to show.
      ***This*should make this feature backwards compatible with WC1453.
      *********
     c*********          if        Q1PRMS > 7
     c****beg*add *** 12/13/07 *********************************
     c*********          if        @@type <> *blank
     c****end*add *** 12/13/07 *********************************
     c*********          eval      *in59 = *on
     c****beg*add *** 12/13/07 *********************************
     c*********          endif
     c****end*add *** 12/13/07 *********************************
     c*********          eval      xxtype = @@type
     c*********          eval      sstype = @@type
     c*********          eval      ##type = @@type
     c*********          eval      ##fein = @@fein
     c*********          if        @@fein <> *blanks
     c*********          eval      *in56 = *on
     c*********          movel(p)  @@fein        ssfein
     c*********          endif
     c*********          eval      ##clm  = @@clm
     c*********          if        @@clm <> *blanks
     c*********          eval      xxclm  = @@clm
     c*********          else
     c*********          eval      xxclm  = *zeros
     c*********          endif
     c*********          eval      ##seq# = @@seq#
      *********
     c*********          if        xxtype = 'C'
     c*********          eval      *in57  = *on
     c*********          if        xxfyr  <> 0
     c*********          eval      ssfyr = xxfyr
     c*********          eval      *in58  = *on
     c*********          endif
     c*********          endif
      *********
     c*********          else
     c*********          eval      xxtype = 'B'
     c*********          eval      ##type = *blank
     c*********          endif
      *********
     c*********          if        Q1PRMS > 8
     c*********          eval      x1fnd  = @@fnd
     C*****Begin Add  ***  05/14/08  **************************************
     c*********          eval      ##Fnd  = @@fnd
     C*****End*  Add  ***  05/14/08  **************************************
     c*********          eval      xxagt# = @@agt#
     c*********          eval      ##agt# = @@agt#
     c*********          endif
      ****end*add ***** 09/20/07 **************************************
      *********
      ***Disable F12 If W0028R is called straight from menu
      *********
     C*********          if        @@clm = *blanks and @@seq# = 0
     C*********                     and @@fein = *blanks
      ****BEG*ADD ***** 09/20/07 *************************************
     C*********                     and xxagt# = *zero  and x1fnd = *zero
      ****END*ADD ***** 09/20/07 *************************************
     C*********          move      '2'           XXOPFN
     C*********          move      'NNN'         XXMIS
     C*********          move      '12'          XXNMBR
     C*********          exsr      UPDOFA
     C*********          endif
     C*********
     C*********          if        @@clm = *blanks
     C*********          eval      @@clm = '0000000000000'
     C*********          endif
     C*********
     C*********          if        @@fein = *blanks
     C*********          eval      @@fein = '000000000000'
     C*********          endif
      *** end del ***** 06/25/09 **************************************
     C*
      *** beg add ***** 06/25/09 **************************************
      *
      * Set up subset screen
      *
     c                   select
     c                   when      @@type = 'A'
     c                   eval      sstype = 'A'
     c                   eval      at01 = protectNORM
     c                   eval      sstyd1 = '(A=Agent)'
     c                   eval      sstyd2 = *blanks
     c                   eval      sssshd = 'Fein/mod . . :'
     c                   eval      ad07 = nonDisplay
     c                   eval      at07 = nonDisplay
     c                   eval      ad08 = nonDisplay
     c                   eval      at08 = nonDisplay
     c                   eval      ad09 = nonDisplay
     c                   eval      at09 = nonDisplay
     c                   eval      ad10 = nonDisplay
     c                   eval      at10 = nonDisplay
     c                   if        @@rrn <> *zero
     c     @@rrn         chain     wmaap
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   else
     c                   eval      ssfnd  = AAFND
     c                   eval      ssfein = AAFEIN
     c                   eval      ssmod = aamod
     c                   eval      ssagt# = AAAGT#
     c                   endif
     c                   endif
     c                   when      @@type = 'C'
     c                   eval      sstype = 'C'
     c                   eval      at01 = protectNORM
     c                   eval      sstyd1 = '(C=Claims)'
     c                   eval      sstyd2 = *blanks
     c                   eval      sssshd = 'SSN  . . . . :'
     c                   eval      at04 = nonDisplay
     c                   eval      ad06 = nonDisplay
     c                   eval      at06 = nonDisplay
     c                   if        @@rrn <> *zero
     c     @@rrn         chain     wmcmp
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   else
     c                   eval      ssfnd  = CMFND
     c                   eval      sscase = CMCASE
     c                   eval      ssfyr  = CMFYR
     c                   endif
     c                   endif
     c                   when      @@type = 'D'
     c                   eval      sstype = 'D'
     c                   eval      at01 = protectNORM
     c                   eval      sstyd1 = '(D=Additional Address)'
     c                   eval      sstyd2 = *blanks
     c                   eval      sssshd = 'SSN  . . . . :'
     c                   eval      at04 = nonDisplay
     c                   eval      ad06 = nonDisplay
     c                   eval      at06 = nonDisplay
     c                   if        @@rrn <> *zero
     c     @@rrn         chain     wdacp
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   else
     c                   eval      ssfnd  = ACFND
     c                   eval      sscase = ACCASE
     c                   eval      ssfyr  = ACFYR
     c                   eval      ssseq# = ACSEQ#
     c                   endif
     c                   endif
     c                   when      @@type = 'E'
     c                   eval      sstype = 'E'
     c                   eval      at01 = protectNORM
     c                   eval      sstyd1 = '(E=Employer)'
     c                   eval      sstyd2 = *blanks
     c                   eval      ad03 = nonDisplay
     c                   eval      at03 = nonDisplay
     c                   eval      at04 = nonDisplay
     c                   eval      ad06 = nonDisplay
     c                   eval      at06 = nonDisplay
     c                   eval      ad07 = nonDisplay
     c                   eval      at07 = nonDisplay
     c                   eval      ad08 = nonDisplay
     c                   eval      at08 = nonDisplay
     c                   eval      ad09 = nonDisplay
     c                   eval      at09 = nonDisplay
     c                   if        @@rrn <> *zero
     c     @@rrn         chain     wmemp
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   else
     c                   eval      ssfein = EMFEIN
     c                   eval      ssfnd  = EMFND
     c                   eval      ssemp# = EMEMP#
     c                   eval      ssdiv  = EMDIV
     c                   endif
     c                   endif
     c                   when      @@type = 'P'
     c                   eval      sstype = 'P'
     c                   eval      at01 = protectNORM
     c                   eval      sstyd1 = '(P=Provider)'
     c                   eval      sstyd2 = *blanks
     c                   eval      sssshd = 'Fein/mod . . :'
     c                   eval      ad05 = nonDisplay
     c                   eval      at05 = nonDisplay
     c                   eval      ad06 = nonDisplay
     c                   eval      at06 = nonDisplay
     c                   eval      ad07 = nonDisplay
     c                   eval      at07 = nonDisplay
     c                   eval      ad08 = nonDisplay
     c                   eval      at08 = nonDisplay
     c                   eval      ad09 = nonDisplay
     c                   eval      at09 = nonDisplay
     c                   eval      ad10 = nonDisplay
     c                   eval      at10 = nonDisplay
     c                   if        @@rrn <> *zero
     c     @@rrn         chain     wmpdp
     c                   if        not(%found)
     c                   seton                                        lr
     c                   return
     c                   else
     c                   eval      ssfein = PDFEIN
     c                   eval      ssmod  = pdmod
     c                   endif
     c                   endif
     c                   other
     c                   eval      sstyd1 = '(A=Agent, C=Claims, D=Additional-
     c                              Address,'
     c                   eval      sstyd2 = ' E=Employer, P=Provider)'
     c                   eval      sssshd = 'SSN/Fein/Mod :'
      *
     c                   endsl
      *
      * Set up the position to fields for the Employer type
      *
     c                   if        @@type = 'E'
     c                   eval      *in61 = *on
     c                   endif
      *** end add ***** 06/25/09 **************************************
      *** beg add ***** 04/21/11 *********************************************
     c                   if        @@call = '2'
     c                   eval      ssactv = 'A'
     c                   endif
      *** end add ***** 04/21/11 *********************************************
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
      *** beg chg ***** 06/25/09 **************************************
     C**********         if        *in21 = *on
     C*****kkco#         reade     dbrcdf                                 95
     C**********         endif
     C**********         if        *in22 = *on
     C*****kkco#         reade     dbrcdf2                                95
     C****************** read      dbrcdf2                                95
     C**********         endif

     c                   select
     c                   when      @@type = 'E'
     c                   select
     c                   when      *in21 = *on
     c     kkco#         reade     pef05                                  95
     c                   eval      dbrrn = emrrn1
     c                   when      *in22 = *on
     c     kkco#         reade     pef06                                  95
     c                   eval      dbrrn2 = emrrn2
     c                   endsl
     c                   other
     c                   select
     c                   when      *in21 = *on
     c     kkco#         reade     dbrcdf                                 95
     c                   when      *in22 = *on
     c     kkco#         reade     dbrcdf2                                95
     C                   endsl
     c                   endsl
      *** end chg ***** 06/25/09 **************************************
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr

      *** beg chg ***** 06/25/09 **************************************
     C**********         if        *in21 = *on
     C*****kkco#         readpe    dbrcdf                                 95
     C****************   readp     dbrcdf                                 95
     C**********         endif
     C**********         if        *in22 = *on
     C*****kkco#         readpe    dbrcdf2                                95
     C****************   readp     dbrcdf2                                95
     C**********         endif

     c                   select
     c                   when      @@type = 'E'
     c                   select
     c                   when      *in21 = *on
     c     kkco#         readpe    pef05                                  95
     c                   eval      dbrrn = emrrn1
     c                   when      *in22 = *on
     c     kkco#         readpe    pef06                                  95
     c                   eval      dbrrn2 = emrrn2
     c                   endsl
     c                   other
     c                   select
     c                   when      *in21 = *on
     c     kkco#         readpe    dbrcdf                                 95
     c                   when      *in22 = *on
     c     kkco#         readpe    dbrcdf2                                95
     c                   endsl
     c                   endsl
      *** end chg ***** 06/25/09 **************************************
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
      *** beg chg ***** 06/25/09 **************************************
     C*********          if        *in21 = *on
     C*****key1          setgt     dbrcdf                             96
     C*********          endif
     C*********          if        *in22 = *on
     C*****key2          setgt     dbrcdf2                            96
     C*********          endif

     c                   select
     c                   when      @@type = 'E'
     c                   select
     c                   when      *in21 = *on
     c     emky1         setgt     pef05                              96
     c                   when      *in22 = *on
     c     emky2         setgt     pef06                              96
     c                   endsl
     c                   other
     c                   select
     c                   when      *in21 = *on
     c     key1          setgt     dbrcdf                             96
     c                   when      *in22 = *on
     c     key2          setgt     dbrcdf2                            96
     c                   endsl
     c                   endsl
      *** end chg ***** 06/25/09 **************************************
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
      *** beg chg ***** 06/25/09 **************************************
     C**********         if        *in21 = *on
     C*****key1*         setll     dbrcdf= *on                        96
     C**********         endif     *in21 = *on
     C**********         if        *in22 = *on
     C*****key2*         setll     dbrcdf2 *on                        96
     C**********         endif     *in21 = *on

     c                   select
     c                   when      @@type = 'E'
     c                   select
     c                   when      *in21 = *on
     c     emky1         setll     pef05                              96
     c                   when      *in22 = *on
     c     emky2         setll     pef06                              96
     c                   endsl
     c                   other
     c                   select
     c                   when      *in21 = *on
     c     key1          setll     dbrcdf                             96
     c                   when      *in22 = *on
     c     key2          setll     dbrcdf2                            96
     c                   endsl
     c                   endsl
      *** end chg ***** 06/25/09 **************************************
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
      *** beg add ***** 04/21/11 *********************************************
     c                   move      scdbrr        @@rrn
      *** end add ***** 04/21/11 *********************************************
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr

      *** beg add ***** 06/25/09 **************************************
     c                   select
      *** beg add ***** 05/19/17 **************************************
     c                   When      GroupAuth(EFco#:EFfnd) = *Off
     c                   eval      meetss = *off
      *** end add ***** 05/19/17 **************************************
     c                   when      sstype = 'C' and
     c*** BEGIN CHG ****** 06/29/09 ************************************
     c*****************            (eftype <> 'C' or eftype <> 'D')
     c                             (eftype <> 'C' and eftype <> 'D')
     c*** END   CHG ****** 06/29/09 ************************************
     c                   eval      meetss = *off
     c                   when      sstype <> *blank and
     c                             sstype <> eftype
     c                   eval      meetss = *off
     c                   when      sspnam <> *blanks
     C                   eval      string1 = upper(efpnam)
     C                   eval      string2 = upper(sspnam)
     c                   call      'QCLSCAN'
     c                   parm                    string1
     c                   parm      40            strlen
     c                   parm      1             start
     c                   parm                    string2
     c                   parm      40            patlen
     c                   parm      '0'           trans
     c                   parm      '1'           trim
     c                   parm      ' '           wild
     c                   parm                    scnres
     c                   if        scnres = 0
     c                   eval      meetss = *off
     c                   endif
      *** beg chg ***** 07/24/15 ***************************************
     c***********        when      ssfein <> 0 and ssfein <> effein and
     c***********                  ssmod  = 0
     c***********        eval      meetss = *off
     c***********        when      ssfein <> 0 and ssfein <> effein and
     c***********                  ssmod  <> 0 and ssmod <> efmod
     c***********        eval      meetss = *off
     c                   when      ssfein <> 0 and ssfein <> effein
     c                   eval      meetss = *off
     c                   when      ssfein <> 0 and ssfein = effein
     c                   if        ssmod <> 0 and ssmod <> efmod
     c                   eval      meetss = *off
     c                   endif
      *** end chg ***** 07/24/15 ***************************************
     c                   when      ssfnd  <> 0 and ssfnd  <> effnd
     c                   eval      meetss = *off
     c                   when      ssagt# <> 0 and ssagt# <> efagt#
     c                   eval      meetss = *off
     c                   when      sscase <> 0 and sscase <> efcase
     c                   eval      meetss = *off
     c                   when      ssseq# <> 0 and ssseq# <> efseq#
     c                   eval      meetss = *off
     c                   when      ssfyr  <> 0 and ssfyr  <> effyr
     c                   eval      meetss = *off
     c                   when      ssemp# <> 0 and ssemp# <> efemp#
     c                   eval      meetss = *off
     c                   when      ssdiv <> 0 and ssdiv  <> efdiv
     c                   eval      meetss = *off
      *** beg add ***** 04/21/11 *********************************************
     c                   when      (ssactv = 'A' and not prIsActive) OR
     c                             (ssactv = 'I' and prIsActive)
     c*****              time                    dsdttm
     C*****              eval      srdti = dsdate
     C*****              call      'SRCVTDAT'    dtcymd
     C*****              movel     srdto         ##date
     c*****              move      ##date        #@date            7 0
     c*****              if        (efefdt > #@date  or
     c*****                        (efexdt < #@date and efexdt <> 0))
     c                   eval      meetss = *off
     c*****              endif
      *** end add ***** 04/21/11 *********************************************
     c                   endsl
      *** end add ***** 06/25/09 **************************************
      *** beg del ***** 06/25/09 **************************************
     C*****************  if        xxfyr <> *zeros and xxfyr <> effyr
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        xxfnd <> *zeros and xxfnd <> effnd
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        xxcase <> *zeros and xxcase <> efcase
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        xxfein <> *zeros and xxfein <> effein
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        xxmod <> *zeros and xxmod <> efmod
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        ssseq# <> *zeros and ssseq# <> efseq#
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        ssfein <> *zeros and ssfein <> effein
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        sstype <> *blank and sstype <> eftype
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     C*****************  if        ssfyr <> *zeros and ssfyr <> effyr
     C*****************  eval      meetss = *off
     C*****************  endif
     C*****************
      ****beg*add****** 09/20/07 **********************************************
     c*****************  if        x1fnd  <> *zeros and effnd  <> x1fnd
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     c*****************  if        xxagt# <> *zeros and efagt# <> xxagt#
     C*****************  eval      meetss = *off
     C*****************  endif
      *****************
     c*****************  if            xxtype <> *blank
     c*****************            and xxtype <> 'B'
     c*****************            and eftype <> xxtype
     c*****************  eval      meetss = *off
     c*****************  endif
      *****************
     c*****************  if            xxtype <> *blank
     c*****************            and xxtype = 'B'
     c*****************            and eftype = 'A'
     c*****************  eval      meetss = *off
     c*****************  endif
      *** end del ***** 06/25/09 **************************************
      *** end add ***** 09/20/07 **********************************************
      *
     C                   endsr
      *** beg add ***** 04/21/11 *********************************************
    P*================================================================
    P* Check Status
    P*================================================================
     PprIsActive       b
     DprIsActive       pi             1n

     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         ##date
     c                   move      ##date        #@date            7 0
     c                   if        (efefdt <= #@date  and
     c                             (efexdt > #@date or  efexdt = 0))
     c                   return    *on
     c                   else
     c                   return    *off
     c                   endif

     PprIsActive       e
      *** end add ***** 04/21/11 *********************************************

** Column headings
Opt Payee name            SS#/FEIN Grp   Eff Date   Exp Date  Prenote Dt
Opt  SS#/FEIN Payee name           Grp   Eff Date   Exp Date  Prenote Dt
Opt Payee name            SS#/FEIN Grp  Approval  1st Sbmt Dt  Last Sbmt Dt
Opt  SS#/FEIN Payee name           Grp  Approval  1st Sbmt Dt  Last Sbmt Dt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1077=EFT Bank Info          YNN
11010=Clear pre-note date   YNN
11212=Request pre-note      YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change Info           YYN
211F11=Change View          YYN
212F12=Cancel               YYY
217F17=Subset               YYY
3
** Employer Headings
Opt Payee name           Fnd  Employer   Div   Eff Dt   Exp Dt  Prenote Typ
Opt Fnd  Employer   Div Payee name             Eff Dt   Exp Dt  Prenote Typ
Opt Payee name           Fnd  Employer   Div  Appr Dt 1st Trfr Lst Trfr Typ
Opt Fnd  Employer   Div Payee name            Appr Dt 1st Trfr Lst Trfr Typ
