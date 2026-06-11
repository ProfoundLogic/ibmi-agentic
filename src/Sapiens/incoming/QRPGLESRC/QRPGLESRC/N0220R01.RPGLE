     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*
     H/TITLE N0220R01 - WORK WITH H/R FOR INVOICING
     H*
     H*****************************************************************
     H* Description: This program will allow the user to display or   *
     H*              change Uninvoiced EOMBs for Hold and Release.    *
     H*                                                               *
     H* Called by: N0220R                                             *
     H* Calls    : None                                               *
     H*                                                               *
     H* Created: 05/10/00  C:62  Project: AR028                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fn0220f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffaeap     uf a e             disk
     F                                     rename(pea01:dbrcdf)
     Ffaeol4    if a e           k disk
     F                                     infds(dbfds)
     Fftgtl     if a e           k disk
     I*
     D dbfds           ds
     D  dbrrn                397    400b 0
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'H/R for Invoicing'
     D hold            c                   'Hold'
     D release         c                   'Release'
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
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@mode
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     eoky          klist
     c                   kfld                    eaco#
     c                   kfld                    easub#
     c                   kfld                    eafnd
     c                   kfld                    eafyr
     c                   kfld                    eacase
     c                   kfld                    eaeob#
     c*
     C     gtky          klist
     c                   kfld                    eaco#
     c                   kfld                    kktbid
     c                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     c     *like         define    gttbid        kktbid
     c     *like         define    gtkey         kkkey
    C*================================================================
    C* CHGSTS - Change the invoice status.
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        eabhrf = 'H'
     C                   eval      eabhrf = 'R'
     C                   else
     C                   eval      eabhrf = 'H'
     C                   endif
     C*
     C                   eval      eapgm = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C*
     C                   movel     dstime        eatime
     C*
     C                   update    dbrcdf
     C*
     C                   endif
     c*
     c                   endsr
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
     C  N90              if        (scbhrf <> *BLANKS) and
     C                             (scbhrf <> 'H') and
     C                             (scbhrf <> 'R')
     C*
     C                   seton                                        7190
     C*
     C                   endif
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
    C* KRDO - F17=T/M Activity
    C*================================================================
     C     krdo          begsr
     c*
     c                   if        @@mode = 'D'
     c                   eval      ##call = '1'
     c                   else
     c                   eval      ##call = ' '
     c                   endif
     C*
     C                   call      'F3040R'
     C                   parm                    ##call            1
     C     *inkc         parm      '0'           ##kc              1
     C     *inkl         parm      '0'           ##kl              1
     C                   parm      *zeros        ##rrn             9 0
     C                   parm      dbrrn         ##rrn2            9 0
     C                   parm      *zeros        ##rrn3            9 0
     C                   parm      *blanks       ##cert           15
     C                   parm      *zeros        ##ext             3 0
     C                   parm      'Y'           ##eob             1
     C                   parm      earcn         ##rcn             3 0
     c*
     c     @@rrn         chain(n)  dbrcdf                             99
     c*
     c                   exsr      mvdbsc
     c*
     c                   seton                                        90
     c*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scbhrf = eabhrf
     C*
     C                   select
     C     scbhrf        wheneq    'H'
     c                   eval      schrds = hold
     C     scbhrf        wheneq    'R'
     c                   eval      schrds = release
     C                   endsl
     C*
     C                   eval      scbcam = eabcam
     C                   eval      scbamt = eabamt
     C                   eval      sctmam = eabamt - eabcam
     C                   eval      scsys  = easys
     c*
     c                   movel(p)  'SYS '        kktbid
     c                   movel(p)  easys         kkkey
     c*
     c     gtky          chain     pgt01                              99
     c*
     c  n99              movel(p)  gtdesc        scsysd
     c   99              movel     *blanks       scsysd
     c*
     C                   eval      sceob# = eaeob#
     c*
     c     eoky          chain     peo01                              99
     c*
     c  n99              movel(p)  eocert        sccert
     c   99              movel     *blanks       sccert
     C*
     C                   select
     C     eatmfl        wheneq    'Y'
     c                   eval      sctmfl = 'Y'
     C     eatmfl        wheneq    ' '
     c                   eval      sctmfl = 'N'
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c*
     c                   eval      eabhrf = scbhrf
     c                   eval      eabcam = scbcam
     c                   eval      eabamt = scbcam +sctmam
     c*
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
     c*
     c     *inkr         caseq     *on           krdo
     c                   endcs
     c*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     c*
     c     @@mode        comp      'D'                                    09
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     c*
     c     @@rrn         chain(n)  dbrcdf                             99
     C*
     C                   move      eaco#         #1co#             3
     C                   move      easub#        #1sub#            5
     C                   move      eafnd         #1fnd             3
     C                   move      eafyr         #1fyr             3
     C                   move      eacase        #1case            7
     C*
     C                   call      'FA702R'
     C                   parm      *blanks       ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C     schd1         parm      *blanks       ##l1             78
     C     schd2         parm      *blanks       ##l2             78
     c*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C*
     C                   eval      eapgm = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C*
     C                   movel     dstime        eatime
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
     C                   feod      faeap
     C                   endsr
