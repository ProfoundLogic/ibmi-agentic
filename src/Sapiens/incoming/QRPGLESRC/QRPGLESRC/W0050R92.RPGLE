     H* (c) Copyright FISERV Insurance Solutions, Inc., 2006
     H*
     H/TITLE W0050R92 - Employer MPN Provider Maintenance
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R - Work With Employers                               *
     H*    W0076R - Work With Entity/Locations                        *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/06/06 c:61  PROJECT 132WFM                                 *
     H*                Created.                                       *
     H*                                                               *
     H*****************************************************************

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0050f92  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF


      *  Physical file definition (for maintaining record)
     Fwmmpp     uf a e             disk
     F                                     rename(pmp01:dbrcdf)

      *  State code
     Fststl3    if   e           k disk

      *  Employer Master
     Fwmemp     if   e             disk
     Fwmeml     if   e           k disk
     F                                     rename(pem01:pem02)

      *  Employer Entity/Location
     Fwmalp     if   e             disk

      *  Group Master
     Fwmfnl     if   e           k disk




      *================================================================
      *  /cOPY required constants
      *================================================================
     D optnm           c                   'Employer MPN Provider'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


      * Employer Effective/Cancel date arrays
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0

     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *

     D                 DS
     D sczip                   1      9  0
     D  sczip1                 1      5  0
     D  sczip2                 6      9  0

     D                 DS
     D scname                  1    100
     D  scnam1                 1     50
     D  scnam2                51    100


      *---------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@pgm           s             10


      *---------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *---------------------------------------------------------------------

     c/cOPY ScOPYMT01c


      *================================================================
      *  PLIST definitions
      *================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn
     c                   parm                    @@rrn1
     c                   parm                    @@pgm
      *
      *   @@rrn - WMMPP
      *   @@rrn1 - WMEMP if called from W0050R
      *          - WMALP if called from W0076R
      *   @@pgm - Name of program called from.
      *
      *
      *
      *  company file/fund (group) retrieve program parameter list
      *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================

      *  Group Master

     C     fnky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd

      *  State Code Table

     c     stky          klist
     c                   kfld                    xxkey            10

      *  Employer Master

     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv


      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     c     *like         define    mpco#         kkco#
     c     *like         define    mpfnd         kkfnd
     c     *like         define    mpemp#        kkemp#
     c     *like         define    mpdiv         kkdiv
     c     *like         define    mpelfl        kkelfl
     c     *like         define    mpelid        kkelid

     C     *like         define    emefd1        xxefdt
     C     *like         define    emcad1        xxcxdt


      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     c     dltpre        begsr
     c                   endsr
    
    
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     c     dsplyr        begsr
     c                   endsr
    
    
      *================================================================
      * ERRcK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     c     errck1        begsr

      *  MPN Provider Name must be entered

     c  n90              if        scname = *blanks
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif

      *  State Code must be valid (if entered)

     c  n90              if        scst <> *blanks
     c                   movel(p)  scst          xxkey            10
     c     stky          chain     pst01

     c                   if        not %found
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      *  Check to be sure screen has not been updated since entry

     c                   if        *in90 = *off
     C     @@rrn         chain(n)  dbrcdf                             99

     c                   if        (mpdt > savedt) or
     c                             ((mpdt = savedt) and (mptim > savetm))
     c                   eval      *in90 = *on
     c                   eval      *in93 = *on
     c                   endif

     c                   endif

     c                   endsr
    
    
      *================================================================
      * ERRcK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     c     errck2        begsr
     c                   endsr
    
    
      *================================================================
      * ERRcK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     c     errck3        begsr
     c                   endsr
    
    
      *================================================================
      * gethdr - get header information
      *================================================================
     c     gethdr        begsr

     c                   select

     c                   when      @@pgm = 'W0050R'
     c     @@rrn1        chain     pem01
     c                   eval      kkco#  = emco#
     c                   eval      kkfnd  = emfnd
     c                   eval      kkemp# = ememp#
     c                   eval      kkdiv  = emdiv
     c                   eval      kkelfl = 'N'
     c                   eval      kkelid = *blanks
     c                   eval      *in30 = *off

     c                   when      @@pgm = 'W0076R'
     c     @@rrn1        chain     pal01
     c                   eval      kkco#  = alco#
     c                   eval      kkfnd  = alfnd
     c                   eval      kkemp# = alemp#
     c                   eval      kkdiv  = aldiv
     c                   eval      kkelfl = 'Y'
     c                   eval      kkelid = alelid
     c                   eval      scelid = alelid
     c                   eval      *in30 = *on
     c     emky          chain     pem02

     c                   other
     c                   eval      *inlr = *on
     c                   return

     c                   endsl

      * Get CX date

     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     ED(X)         XXEFDT
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
      *
     C     SRERR         COMP      *ON                                    50
     C                   Z-ADD     EMFND         SCFND
      *
      *  Compress Emp#/div/name
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        #2EMP#            9 0
     C                   PARM      EMDIV         #2DIV             5 0
     C                   PARM      EMNAME        #2NAME           40
     C                   PARM                    #2EDN            80
      *
     C                   MOVEL     #2EDN         SCEDN
      *
      *  Get Group Name
      *
     C     FNKY          CHAIN(N)  PFN01                              60
     C                   MOVEL     FNNAME        scfnam
      *
      *  Get Agent/Producer Information
      *
     C                   CALL      'WA002R'
     C                   PARM      emco#         #1CO#             3 0
     C                   PARM      EMFND         #1FND             3 0
     C                   PARM      EMEMP#        #1EMP#            9 0
     C                   PARM      EMDIV         #1DIV             5 0
     C                   PARM      *zeros        #1FYR             3 0
     C     SCAGT#        PARM      *zeros        #1AGT#            5 0
     C                   PARM      *blanks       #1ANAM           60
     C     SCPROD        PARM      *zeros        #1PROD            5 0
     C                   PARM      *blanks       #1PNAM           60
      *
     C                   MOVEL     #1ANAM        SCANAM
     C                   MOVEL     #1PNAM        SCPNAM
      *
     c                   endsr
    
    
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     c     mvdbsc        begsr

     c                   eval      scname = mpname
     c                   eval      scadr1 = mpadr1
     c                   eval      scadr2 = mpadr2
     c                   eval      sccity = mpcity
     c                   eval      scst   = mpst
     c                   z-add     mpzip         sczip
     c                   eval      scptyp = mpptyp

     c                   endsr
    
    
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     c     mvscdb        begsr

     c                   eval      mpco#  = kkco#
     c                   eval      mpfnd  = kkfnd
     c                   eval      mpemp# = kkemp#
     c                   eval      mpdiv  = kkdiv
     c                   eval      mpelfl = kkelfl
     c                   eval      mpelid = kkelid
     c                   eval      mpname = scname
     c                   eval      mpadr1 = scadr1
     c                   eval      mpadr2 = scadr2
     c                   eval      mpcity = sccity
     c                   eval      mpst   = scst
     c                   z-add     sczip         mpzip
     c                   eval      mpptyp = scptyp

     c                   endsr
    
    
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     c     otherr        begsr
     c                   endsr
    
    
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     c     othfks        begsr
     c                   endsr
    
    
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     c     pgmend        begsr
     c                   eval      *inlr = *on
     c                   endsr
    
    
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     c     pgmstr        begsr

     c                   if        @@mode = 'A'
     c                   eval      *in40  = *on
     c                   endif

      *  get header information

     c                   exsr      gethdr

     c                   endsr
    
    
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     c     preoth        begsr
     c                   endsr
    
    
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     C                   eval      mpusr  = q1user
     C                   eval      mppgm = q1pgm
     C                   time                    dsdttm
     C                   eval      mptim  = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         mpdt
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
    
    
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     C                   eval      mpusr  = q1user
     C                   eval      mpiusr = q1user
     C                   eval      mppgm  = q1pgm
     C                   eval      mpipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      mptim  = dstime
     C                   eval      mpitim = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         mpdt
     C                   movel(p)  srdto         mpidt
     c                   write     dbrcdf
     c                   feod      wmmpp
     c                   eval      outflg = *on
     c                   endsr
    
    
