     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
    H*********************************************************************
    H/TITLE SC995R  - MULTPLE DIARY RECIPIENTS INFO                      *
    H*********************************************************************
    H*  DESCRIPTION:  Multiple DARTS Diary Recipients Information        *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    SC995R                                                         *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 05/15/09  PROGRAMMER: C:189  Project : 139WLJ           *
    H*********************************************************************
    H*
    H*
     Fsc995f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmdrp     uf a e             disk
     F                                     rename(pdr01:dbrcdf)
     Fsmupp     if   e             disk
     F                                     rename(pup01:pup03)
     Fsmupl1    if   e           k disk
     Fsmupl     if   e           k disk
     F                                     rename(pup01:pup02)
     Fstctl1    if   e           k disk
     Fwmdrl     uf   e           k disk    rename(pdr01:pdr02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Multiple DARTS Diary Recipient Info'
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
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rmcd            5
     C                   parm                    @@rcid           10
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     uprmpl1       plist
     C                   parm      '2'           ##call            1
     C                   parm      *zeros        ##rrn             9 0
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     drky          klist
     C                   kfld                    @@rmcd
     C                   kfld                    @@rcid
     C     drky1         klist
     C                   kfld                    @@rmcd
     C                   kfld                    s1rcid
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
     C                   if        s1rmcd <> *blanks
     C*
     C     s1rmcd        chain     pct01                              60
     C*
     C     *IN60         ifeq      *on
     C                   seton                                        6390
     C                   endif
     C                   endif
     C*
     C*
     C                   if        s1rcsc <> *blanks
     C*
     C     s1rcsc        chain     pup01                              60
     C     *IN60         ifeq      *on
     C                   seton                                        6490
     C                   endif
     C                   endif
     C*
     C                   if        s1rcid <> *blanks
     C*
     C     s1rcid        chain     pup02                              60
     C     *in60         ifeq      *on
     C                   seton                                        6690
     C                   endif
     C                   endif
     C*
     C*
     C                   if        s1rcid <> *blanks  and s1rmcd <> *blanks
     C*
     C     drky1         chain     pdr02                              60
     C     *in60         ifne      *on
     C                   seton                                        7590
     C                   endif
     C                   endif
     C*
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
     C                   if        q1prms >  4
     C                   if        @@rmcd <> *blanks
     C                   seton                                        696768
     C                   movel     @@rmcd        s1rmcd
     C                   endif
     C                   else
     C                   Movel     drrmcd        s1rmcd
     C                   endif
     C*
     C                   Movel     drrcid        s1rcid
     C                   Movel     drrcsc        s1rcsc
     C                   Movel     drrcds        s1rcds
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   Movel     s1rmcd        drrmcd
     C                   Movel     s1rcid        drrcid
     C                   Movel     s1rcsc        drrcsc
     C                   Movel     s1rcds        drrcds
     C                   Movel     'A'           drstat
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
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C                   If        @@mode = 'S'
     C                   Exsr      Chgsts
     C                   Return
     C                   Endif

     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   Movel     q1pgm         drpgm
     C                   Movel     dstime        drtime
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         drdt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr

     C                   exsr      mvscdb
     C                   Movel     ##co#         drco#
     C                   Movel     q1user        druser
     C                   Movel     q1pgm         drpgm
     C                   Movel     dstime        drtime
     C                   Movel     q1user        driusr
     C                   Movel     q1pgm         dripgm
     C                   Movel     upusnm        drrcds
     C                   Movel     upstaf        drrcsc
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         drdt
     C                   movel     srdto         dridt
     C*
     C                   write     dbrcdf
     C                   feod      wmdrp
     C                   endsr
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
     C     kjdo          begsr
     C                   select
     C                   when      hfld = 'S1RMCD'
      *
     C  n69              call      'SC305R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm      *blanks       ##rmcd            5
     C                   parm      *blanks       ##desc           40
     C                   parm      *blanks       ##rtcd            1
      *
      *
     C                   If        ##rmcd <> ' '
     C                   Movel     ##rmcd        s1rmcd
     C                   seton                                        35
     C                   endif
      *
     C                   when      hfld = 'S1RCID'
      *
     C                   call      'S0005R'      uprmpl1
      *
     C     ##rrn         ifne      *zeros
     C     ##rrn         chain     pup03                              99
     C*
     C                   movel     upusr         s1rcid
     C                   movel     upusnm        s1rcds
     C                   movel     upstaf        s1rcsc
     C                   endif
     C*
     C                   endsl
     C
     C                   seton                                        90
     C                   endsr
    C*================================================================
    C* CHGSTS - Update WMDRP with New Status Flag
    C*================================================================
     C     Chgsts        Begsr

     C     drky          Chain     pdr02

     C                   If        %found

     C                   If        drstat = 'A'
     C                   Eval      drstat = 'I'

     c                   else

     C                   Eval      drstat = 'A'
     C                   Endif

     C                   Update    pdr02
     C                   Endif

     C                   endsr

    C*================================================================
     C*===============================================================
