     H** (C) Copyright Progressive Data Solutions, Inc., 2002             **
    H**                                                                  **
     H**********************************************************************
     H** CodeReview Group/State Information                               **
     H**                                                                  **
     H** Created on 05/29/02 by C:062 for Project 114MDC                  **
     H**********************************************************************
     H** REVISIONS:                                                       **
     H**                                                                  **
     H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Ff0123f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffdsxp     uf a e             disk
     F                                     rename(psx01:dbrcdf)
     Ffdsxl1    if   e           k disk
     Ffdgxp     if   e             disk
     Fststp     if   e             disk
     Fstabl     if   e           k disk
     F                                     rename(pst01:pst02)
     Fftgtl     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmsml     if   e           k disk
     Fhprcntl   if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CR Group/State Information'
     D comapi          c                   'SA005R'
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
     D @@rrn2          s              9  0
     D kkco#           s                   like(gxco#)
     D kksub#          s                   like(gxsub#)
     D kkfnd           s                   like(gxfnd)
     D kkcrvw          s                   like(gxcrvw)
     D kkefdt          s                   like(gxefdt)
     D kkst            s                   like(sxst)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D kkcntl01        s                   like(cntl01) inz('003')
     D kkcntl02        s                   like(cntl02)
     D kkcntl03        s                   like(cntl03)
     D xxcntl03        s                   like(cntl03)
     D xxefdt          s                   like(sxefdt)
     D xxexdt          s                   like(sxexdt)
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================

     C     cntlky1       klist
     C                   kfld                    kkcntl01
     C                   kfld                    kkcntl02

     C     cntlky2       klist
     C                   kfld                    kkcntl01
     C                   kfld                    kkcntl02
     C                   kfld                    kkcntl03

     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
    
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey

     C     smky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#

     C     stky          klist
     C                   kfld                    kkkey

     C     sxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkcrvw
     C                   kfld                    kkefdt
     C                   kfld                    kkst
     C*================================================================
     C* CHGSTS - Subroutine for changing status
     C*================================================================
     C     chgsts        begsr

     C     @@rrn         chain     dbrcdf                             99

     C                   if        *in99 = *off

     C                   if        sxstat = 'I'
     C                   eval      sxstat = 'A'
     C                   else
     C                   eval      sxstat = 'I'
     C                   endif

     C                   eval      sxuser = q1user
     C                   eval      sxpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sxtime = dstime

     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sxdt

     C                   update    dbrcdf

     C                   endif

     C                   endsr
    C*================================================================
    C* DATCVT - Converts dates to MDYY when searches are used
    C*================================================================
     C     datcvt        begsr

     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scexdt

     C                   endsr
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

     C* State must be valid

     C                   if        *in90 = *off and *in40 = *on
     C                   movel(p)  scst          kkkey
     C     stky          chain     pst02                              99
     C   99              seton                                        7190
     C                   endif

     C* CodeReview Group/State Information cannot be duplicated

     C                   if        *in90 = *off and *in40 = *on
     C                   eval      kkst = scst
     C     sxky          chain     psx01                              99
     C  n99              seton                                        7290
     C                   endif

     C* Default the KBID

     C                   if        *in40 = *on and sckbid = *blanks
     C                   if        gxkbid = '99'
     C     2             subst     st7001:3      sckbid
     C                   else
     C                   eval      sckbid = gxkbid
     C                   endif
     C                   endif

     C* Knowledge Base ID Must be valid.

     C                   if        *in90 = *off
     C                   eval      kkcntl02=sckbid
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxcntl03
     C                   eval      kkcntl03 = 99999999 - xxcntl03
     C     cntlky2       setll     hprcntlr
     C     cntlky1       reade     hprcntlr                               99
     C                   if        *in99=*on
     C                   seton                                        7390
     C                   endif
     C                   endif

     C* Expiration date must be valid

     C                   if        *in90 = *off and scexdt <> 0
     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scexdt
     C                   if        srerr = '1'
     C                   seton                                        7490
     C                   endif
     C                   endif

     C* Expiration date must be greater than or equal to Effective date

     C                   if        *in90 = *off and scexdt <> 0

     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxefdt

     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxexdt

     C                   if        xxexdt < xxefdt
     C                   seton                                        7590
     C                   endif

     C                   endif

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
    C* KJDO - F10=Search
    C*================================================================
     C     kjdo          begsr

     C                   exsr      datcvt

     C                   select
     C     hfld          wheneq    'SCST'
     C                   call      'SY601R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm      '0'           ##kc              1
     C     *inkl         parm      '0'           ##kl              1
     C                   parm      *zeros        ##rrn             9 0

     C                   if        *inkl = *on or ##rrn = *zeros
     C                   seton                                        31
     C                   else
     C     ##rrn         chain     pst01                              99
     C                   movel(p)  stkey         scst
     C                   movel(p)  stname        scstnm
     C                   seton                                        32
     C                   endif

     C                   endsl

     C                   seton                                        90

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   if        *in40 = *on
     C                   movel     *blanks       scst
     C                   movel     *blanks       scstnm
     C                   movel     *blanks       sckbid
     C                   z-add     *zeros        scexdt
     C                   movel     'A'           scstat
     C                   else
     C                   movel(p)  sxst          scst
     C                   movel(p)  scst          kkkey
     C     stky          chain     pst02                              99
     C                   if        *in99=*off
     C                   eval      scstnm=stname
     C                   else
     C                   eval      scstnm=*blanks
     C                   endif
     C                   z-add     sxexdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scexdt
     C                   movel     sxkbid        sckbid
     C                   movel     sxstat        scstat
     C                   endif

     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     C                   eval      sxco#=kkco#
     C                   eval      sxsub#=kksub#
     C                   eval      sxfnd=kkfnd
     C                   eval      sxcrvw=sccrvw
     C                   eval      sxefdt=gxefdt
     C                   eval      sxst=scst
     C                   z-add     scexdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sxexdt
     C                   movel(p)  sckbid        sxkbid
     C                   movel(p)  scstat        sxstat

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

     C     *inkj         caseq     *on           kjdo
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40

     C     @@rrn2        chain     pgx01                              99
     C                   eval      kkco#=gxco#
     C                   eval      kksub#=gxsub#
     C                   eval      kkfnd=gxfnd
     C                   eval      kkcrvw=gxcrvw
     C                   eval      kkefdt=gxefdt
     C                   eval      scsub#=gxsub#
     C                   eval      scfnd=gxfnd
     C                   eval      sccrvw=gxcrvw

     C     smky          chain     psm01                              99
     C                   if        *in99=*off
     C                   eval      scsnam=smname
     C                   else
     C                   eval      scsnam=*blanks
     C                   endif

     C     fnky          chain     pfn01                              99
     C                   if        *in99=*off
     C                   eval      scgnam=fnname
     C                   else
     C                   eval      scgnam=*blanks
     C                   endif

     C                   movel(p)  'CRVW'        kktbid
     C                   movel(p)  gxcrvw        kkkey
     C     gtky          chain     pgt01                              99
     C                   if        *in99=*off
     C                   eval      sccrvd=gtdesc
     C                   else
     C                   eval      sccrvd=*blanks
     C                   endif

     C                   z-add     gxefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt

     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr

     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl

     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr

     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      sxuser=q1user
     C                   eval      sxpgm=q1pgm
     C                   time                    dsdttm
     C                   z-add     dstime        sxtime
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sxdt
     C                   update    dbrcdf
E01  C                   endif

     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr

     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      sxuser=q1user
     C                   eval      sxpgm=q1pgm
     C                   eval      sxiusr=q1user
     C                   eval      sxipgm=q1pgm
     C                   time                    dsdttm
     C                   z-add     dstime        sxtime
     C                   z-add     dstime        sxitim
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sxdt
     C                   movel     srdto         sxidt
     C                   write     dbrcdf
     C                   feod      fdsxp

     C                   endsr
