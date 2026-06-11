     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE F2374R01 : NEW, PROVIDER PARISH SALES TAX MAINTENANCE
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: NEW, PROVIDER PARISH SALES TAX MAINTENANCE      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F2374R                                                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 09/29/10  PROGRAMMER: C:257 PROJECT: C133MJN        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/07/12  C:251  Project: 133MMH Priss 11                    *
     H*                   Change to fetch from table value 'LAPA'     *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2374f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmp3pla   uf a e             disk
     F                                     rename(pp301:dbrcdf)
     Ffmpdp     if   e             disk
     F*** Beg Add *** 05/07/12 ****************************************
     FFTGTL     if   e           k disk
     F*** End Add *** 05/07/12 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Provider Parish Sales Tax'
     D comapi          c                   'FA007R'
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
     D @@rrn1          s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Data structure
    D*---------------------------------------------------------------------
     D                 ds
     D dsprovider                    55
     D  dfein                  1      9a
     D  dmod                  11     14a
     D  dname                 16     55a
    D*
    D*
    D*
    D*---------------------------------------------------------------------
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
     C                   parm                    @@rrn1
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
     C     SYSTPL        PLIST
     C                   PARM      optnm         ##PRDS           40
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    XXTBID            4
     C                   PARM      *BLANKS       XXKEY1           10
     C                   PARM                    ##RTCD            1
     C*** Beg Add *** 05/07/12 ****************************************
     C     Gtky          Klist
     C                   kfld                    ##co#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey1
     C*** End Add *** 05/07/12 ****************************************
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
     C*                  if        @@mode = 'A' and *inkj = '0'
     C                   if        *inkj = '0'
     C                   select
     C     scstatus      wheneq    *blanks
     C                   seton                                        3090
     C                   eval      *in35 = '1'
     C     scprsh        wheneq    *blanks
     C                   seton                                        3190
     C                   eval      *in36 = '1'
     C*** Beg Add *** 05/07/12 *********************************************
     C*
     C     scprsh        whenne    *blanks
     C                   movel(P)  'LAPA'        xxtbid
     C                   movel(P)  scprsh        xxkey1
     C     Gtky          chain     pgt01
     C                   if        not %found (ftgtl)
     C                             or gtstat = 'I'
     C                   eval      *in71 = *on
     C                   eval      *in36 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*** End Add *** 05/07/12 *********************************************
     C     scefdt        wheneq    *zeros
     C                   seton                                        3290
     C                   eval      *in37 = '1'
     C     sccty         wheneq    *blanks
     C                   seton                                        3390
     C                   eval      *in38 = '1'
     C     sctax         wheneq    *zeros
     C                   seton                                        3490
     C                   eval      *in39 = '1'
     C                   endsl
     C*
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
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     p3prsh        scprsh
     C                   movel     p3cty         sccty
     C*
     C                   z-add     p3efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C*
     C                   Movel     srdto         scefdt
     C*
     C                   movel     p3tax         sctax
     C                   if        @@mode = 'A'
     C                   eval      scstatus = 'A'
     C                   else
     C                   eval      scstatus = p3stat
     C                   endif
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     pdfein        p3fein
     C                   movel     pdmod         p3mod
     C                   movel     scprsh        p3prsh
     C                   movel     sccty         p3cty
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         p3efdt
     C*
     C                   movel     sctax         p3tax
     C                   eval      p3stat   = scstatus
     C*
     C                   movel     q1user        p3user
     C                   movel     q1pgm         p3pgm
     C                   movel     dstime        p3time
     C                   movel     dstime        p3itim
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         p3dt
     C                   movel     srdto         p3idt
     C                   movel     q1user        p3iusr
     C*                  movel     qqpgm         p3ipgm
     C*
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
     C                   if        @@mode = 'A'
     C                   select
     C* When cursor is on Parish
     C     HFLD          wheneq    'SCPRSH'
     C     *inkj         andeq     '1'
     C                   movel     'LAPA'        xxtbid
     C                   call      'F2493R'      SYSTPL
     C*** Beg Add *** 05/07/12 *********************************************
     C*
     C     Gtky          chain     pgt01
     C                   if        %found (ftgtl)
     C                             and gtstat <> 'I'
     C*** End Add *** 05/07/12 *********************************************
     C                   movel     xxkey1        SCPRSH
     C                   seton                                          90
     C*** Beg Add *** 05/07/12 *********************************************
     C                   seton                                          37
     C                   else
     C                   movel     *blanks       SCPRSH
     C                   seton                                        3690
     C                   endif
     C*** End Add *** 05/07/12 *********************************************
     C                   if        xxkey1 <> *blanks
     C                   eval      *in88 = '1'
     C                   endif
     C     HFLD          whenne    'SCPRSH'
     C     *inkj         andeq     '1'
     C                   seton                                          90
     C                   endsl
     C                   endif
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
     C                   if        @@mode = 'D'
     C                   eval      *in10 = '1'
     C                   endif
     C* Protect Parish and effective date from being updated.
     C                   if        @@mode = 'C' or @@mode = 'D'
     C                   eval      *in11 = '1'
     C                   else
     C                   eval      *in11 = '0'
     C                   endif
     C* Load iformation of Provider/Provider type/State in header
     C     @@rrn1        setll     PPD01
     C                   read      PPD01
     C                   movel     pdfein        dfein
     C                   movel     pdmod         dmod
     C                   movel     pdname        dname
     C                   movel     dsprovider    scprvdr                        provider
     C                   movel     pdst          scst                           state
     C                   movel     pdtype        scprvdt                        provider type
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
     C                   feod      fmp3pla
     C                   endsr
