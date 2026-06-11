     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)

      *
      /title SR48316I02 - "Info/Maint Clt Termitn End" Additional Info program
      *****************************************************************
      * DESCRIPTION:
      *
      *
      * WC480316 - WI Employee Leasing Company Client Termination Endorsement
      *
      * "Info/Maint" - Endorsement "Addtional Information" program
      *              - Multiple, Manual with Additional Information.
      *
      * CALLED FROM: SR48316I
      * CALLS TO:    None
      *
      *****************************************************************
      * REVISED:
      *
      *
      * 12/20/10 C:220 PROJECT WC1962
      *                Created.
      *
      *****************************************************************

      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fsr48316f02cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     *  Physical file definition (for maintaining record)
      *
     Fwledp     uf a e             disk
      *
     F                                     rename(ped01:dbrcdf)
     Fwledl     if a e           k disk    prefix(p_) usropn
      *
    fwdrrl     if   e           k disk
    
      * group
     Fwmfnl     if   e           k disk
    
      * employer
     Fwmeml     if   e           k disk
    
      * employer policy year
     Fwdelp     if   e             disk
    
      * employer cross reference
     Fwzexl     if   e           k disk
     F                                     rename(pex01:pex02)
     Fwmrsl     if   e           k disk
    

     D efDS            ds
     D  EMEFD1
     D  EMEFD2
     D  EMEFD3
     D  EMEFD4
     D  EMEFD5
     D ef                                  OVERLAY(efDS)
     D                                     LIKE(EMEFD1) DIM(5)

     D cxDS            ds
     D  EMCAD1
     D  EMCAD2
     D  EMCAD3
     D  EMCAD4
     D  EMCAD5
     D cx                                  OVERLAY(cxDS)
     D                                     LIKE(EMCAD1) DIM(5)
      * name returned from SRADDR
     d emnam           ds
     d  ##emn1                       50
     d  ##emn2                       50
     d  ##emn3                       50
     d  ##emn4                       50
     d  ##emn5                       50
     d  ##emn6                       50
     d  ##emn7                       50
    d nam                                 OVERLAY(emnam)
     d                                     LIKE(##emn1) DIM(7)



    D*================================================================
    D*  /COPY required constants
    D*================================================================

      *
     D optnm           c                   'WI Client Termination Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   2


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
     D @@qte           s              1
     d ##emp#          s                   like(ememp#)
     d ##div           s                   like(emdiv)
     d ##name          s                   like(emname)
     d ##fyr           s                   like(elfyr)
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##prod          s              5  0
     d ##pnam          s             60
    D #1co#           s              3
    D #1fnd           s              3
    D #1fyr           s              3
    D #1emp#          s              9
    D #1div           s              5
    D ##odr           s              1
    D ##nodr          s              1
    D ##aodr          s              1
    D ##adrl          s              3  0
    D ##naml          s              3  0
    D ##fedl          s              3  0
    D ##fedl1         s              3  0
    D ##peo1          s             57
    D ##peo2          s             57
    D ##pgfl          s              1
     d x               s              7  0
     d sysdt           s              7  0
     d xxdat           s              7s 0
     d xxdat2          s              8s 0
     d xxstdt          s                   like(elpfdt)
     d x1div           s                   like(eldiv)
     d x1pfdt          s                   like(elpfdt)
     d x1andt          s                   like(elandt)
     d xspfdt          s                   like(elpfdt)
     d xsfyr           s                   like(elfyr)
     d xsandt          s                   like(elandt)
     d xsdiv           s                   like(eldiv)
     d xxseq#          s                   like(fmseq#)
     d xxform          s                   like(fmform)


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------


     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
      *                                     endorsement specific data file
     C                   parm                    @@rrn
      *                                     wdelp
     C                   parm                    @@rrn2
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     c     agtpl         plist
     c                   PARM                    ##CO#
     c                   PARM      elfnd         ##FND
     c                   PARM      elemp#        ##EMP#
     c                   PARM      eldiv         ##DIV
     c                   PARM      elfyr         ##FYR
     c     scagt#        PARM                    ##AGT#
     c                   PARM                    ##ANAM
     c     scprod        PARM                    ##PROD
     c                   PARM                    ##PNAM


     *================================================================
     *  KLIST definitions
     *================================================================
     c*
     c*  Add keys for WLEDL key list for seq# retrieval
     c*
     c     edky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c                   kfld                    xxform
     c                   kfld                    xxseq#

     c     edky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c                   kfld                    xxform

     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     zexky         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
    c*
     c     rrky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
     c                   kfld                    elpfdt
    c*
     c     rrky2         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    rsjrst
      ** Rate Set Code file
     c     rsky          KList
     c                   KFld                    elco#
     c                   KFld                    elrtcd
    


     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for screen 1 (SCR01)
     *================================================================
     C     errck1        begsr
     c*
     c                   if        sccln1 = *blanks and sccln2 = *blanks
     c                             and sccln3 = *blanks
     c                   seton                                        7490
     c                   endif
     c*
      *  Coverage terminated date and date sent should be a valid date
     c*
     c                   if        (sctdt <>*zeros)
     c                   eval      srdti = sctdt
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         sctdt
     c*
     c                   if        srerr = '1'
     c                   seton                                        7290
     c                   endif
     c                   endif
     c*
     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     c*
     c                   if        (scsdt <>*zeros)
     c                   eval      srdti = scsdt
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scsdt
     c*
     c                   if        srerr = '1'
     c                   seton                                        7390
     c                   endif
     c                   endif
     c*
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for screen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     c* Move datebase fields:-
     c*
     c                   eval      scnam1 = fm1f01
     c                   eval      scnam2 = fm1f02
     c                   eval      scnam3 = fm1f03
     c                   eval      scnam4 = fm1f04
     c*
     c                   eval      sccln1 = fm2001
     c                   eval      sccln2 = fm2002
     c                   eval      sccln3 = fm2003
     c*
     c*                  convert date into MDYY format to store in file
     c*
     c                   eval      xxdat2= *zero
     c                   eval      srdti = fma701
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdat2
     c                   eval      sctdt  = xxdat2
     c*
     c*                  convert date into MDYY format to store in file
     c*
     c                   eval      xxdat2= *zero
     c                   eval      srdti = fma702
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdat2
     c                   eval      scsdt  = xxdat2
     c*
     c                   eval      scens1 = fm2004
     c                   eval      scens2 = fm2005
     c                   eval      scens3 = fm2006
     c                   eval      scens4 = fm2007
     c                   eval      scens5 = fm2008
     c                   eval      scens6 = fm2009
     c*
     c                   if        *in40 = *on
    c*
    c* Get employer address if mailing / physical
    c*
     c                   if        ##adrl > 1
     c                   eval      x =  ##adrl - 1
     c                   endif
    c*
     c                   select
    c*
     c                   when      x =  1
     c                   movel(p)  nam(1)        scnam1
    c*
     c                   when      x =  2
     c                   movel(p)  nam(1)        scnam1
     c                   movel(p)  nam(2)        scnam2
    c*
     c                   when      x =  3
     c                   movel(p)  nam(1)        scnam1
     c                   movel(p)  nam(2)        scnam2
     c                   movel(p)  nam(3)        scnam3
     c*
     c                   when      x =  4
     c                   movel(p)  nam(1)        scnam1
     c                   movel(p)  nam(2)        scnam2
     c                   movel(p)  nam(3)        scnam3
     c                   movel(p)  nam(4)        scnam4
     c*
     c                   endsl
     c                   endif
     c*
     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

    c*
     c                   eval      fmco#  = elco#
     c                   eval      fmfnd  = elfnd
     c                   eval      fmfyr  = elfyr
     c                   eval      fmemp# = elemp#
     c                   eval      fmdiv  = eldiv
     c                   eval      fm1f01 = scnam1
     c                   eval      fm1f02 = scnam2
     c                   eval      fm1f03 = scnam3
     c                   eval      fm1f04 = scnam4
     c*
     c                   eval      fm2001 = sccln1
     c                   eval      fm2002 = sccln2
     c                   eval      fm2003 = sccln3
     c*
     c*                  convert date into CYMD format to store in file
     c*
     c                   eval      xxdat = *zero
     c                   eval      srdti = sctdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdat
     c                   eval      fma701 = xxdat
     c*
     c*                  convert date into CYMD format to store in file
     c*
     c                   eval      xxdat = *zero
     c                   eval      srdti = scsdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdat
     c                   eval      fma702 = xxdat
     c*
     c                   eval      fm2004 = scens1
     c                   eval      fm2005 = scens2
     c                   eval      fm2006 = scens3
     c                   eval      fm2007 = scens4
     c                   eval      fm2008 = scens5
     c                   eval      fm2009 = scens6
     c*
     c* Findout the latest endorsement seq#
     c*
     c                   if        *in40 = *on
     c*
     c                   open      wledl
     c*
     c                   eval      xxform = 'WC480316'
     c                   eval      xxseq# = *hival
     c     edky          setgt     ped01
     c     edky1         readpe(n) ped01                                  60
     c*
     c                   if        *in60 = *off
     c                   eval      xxseq# = p_fmseq# + 1
     c                   else
     c                   eval      xxseq# = 1
     c                   endif
     c                   close     wledl
     c*
     c                   eval      fmseq# = p_fmseq# + 1
     c                   eval      fmform = 'WC480316'
     c*
     c                   endif
     c*

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
      *
     C                   time                    dsdttm
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         sysdat            7 0
      *

     c                   select

     c                   when      @@mode = 'A'
     c                   eval      *in40 = *on

     c                   when      @@mode = 'D'
     c                   eval      *in09 = *on

     c                   endsl

      *  Load Screen Headings

     c     @@rrn2        chain     pel01                              60
     c     emky          chain     pem01                              60
     c                   eval      scfyr  = elfyr
     c                   eval      scfnd  = emfnd

      * get fund name

     c                   eval      scfnam = *blanks
     c     fnky          chain     pfn01
     c                   if        %found
     c                   if        fnanam <> *blanks
     c                   eval      scfnam = fnanam
     c                   else
     c                   eval      scfnam = fnname
     c                   endif
     c                   endif

     c                   if        elfaf = 'A'
     c                   eval      scpyst = 'Audited'
     c                   else
     c                   eval      scpyst = 'Estimated'
     c                   endif
      *
      *  Concat Employer/Division/Name
      *
     c                   CALL      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c                   parm                    ##edn            80
      *
     c                   eval      scedn = ##edn

      *  get Cancel date

     c                   eval      x = 5
      *
     c                   dow       (ef(x) = 0) and (x > 1)
     c                   eval      x = x - 1
     c                   enddo
      *
     c                   eval      *in41 = *off
      *
     c                   if        (cx(x) <> 0)
     c                   eval      srdti = cx(x)
     c                   CALL      'SRCVTDAT'    DTMDYY
     c                   movel     srdto         sccxdt
     c                   eval      *in41 = *on
     c                   endif

     c                   time                    DSDTTM
     c                   z-add     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt

      *  Get Agent/Producer Information

     c                   call      'WA002R'      AGTPL
     c                   eval      scanam = ##anam
     c                   eval      scpnam = ##pnam

      *  Get Employer Cross Reference info for header

     c     zexky         chain     pex02                              63

     c                   if        %found
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv
     c                   endif
    c*
     c                   if        *in40 = *on
    c*
     c     rsky          chain     prs01
     c                   move      elco#         #1CO#
     c                   move      elfnd         #1FND
     c                   move      elemp#        #1EMP#
     c                   move      eldiv         #1DIV
    c*
    c* Get employer name
    c*
     C                   call      'SRADDR'
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1emp#
     C                   parm                    #1div
     C                   parm      ' '           ##odr
     C                   parm      ' '           ##nodr
     C                   parm      ' '           ##aodr
     C                   parm      *blanks       ##emn1
     C                   parm      *blanks       ##emn2
     C                   parm      *blanks       ##emn3
     C                   parm      *blanks       ##emn4
     C                   parm      *blanks       ##emn5
     C                   parm      *blanks       ##emn6
     C                   parm      *blanks       ##emn7
     C                   parm      *zero         ##adrl
     C                   parm      *zero         ##naml
     C                   parm      *zero         ##fedl
     C                   parm      *blanks       ##peo1
     C                   parm      *blanks       ##peo2
     C                   parm      '1'           ##pgfl
     C*
     c                   endif
    c*
     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb

      * Update maintinance fields

     C                   eval      fmuser = q1user
     C                   eval      fmpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      fmtime = dstime

     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         fmdt

     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb

      * Update maintinance fields

     C                   eval      fmuser = q1user
     C                   eval      fmiusr = q1user
     C                   eval      fmpgm  = q1pgm
     C                   eval      fmipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      fmtime = dstime
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         fmdt
     C                   movel(p)  srdto         fmidt
     C                   write     dbrcdf
      *
     C                   feod      wledp
     C                   eval      outflg = *on
      *
     C                   endsr

