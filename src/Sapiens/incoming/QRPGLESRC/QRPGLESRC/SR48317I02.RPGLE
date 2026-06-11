     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)

      *
      /title SR48317I02 - "Info/Maint Clt Exclusion End" Additional Info program
      *****************************************************************
      * DESCRIPTION:
      *
      *
      * WC480317 - WI Employee Leasing Company Client Exclusion Endorsement
      *
      * "Info/Maint" - Endorsement "Addtional Information" program
      *              - Multiple, Manual with Additional Information.
      *
      * CALLED FROM: SR48317I
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
     Fsr48317f02cf   e             workstn
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


    D*================================================================
    D*  /COPY required constants
    D*================================================================

      *
     D optnm           c                   'WI Client Exclusion Information'
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
     d x               s              7  0
     d sysdt           s              7  0
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
     c*  At least one name should be enetered
     c*
     c                   if        sccnm1 = *blanks and sccnm2 = *blanks
     c                             and sccnm3 = *blanks and sccnm4 = *blanks
     c                             and sccnm5 = *blanks and sccnm6 = *blanks
     c                             and sccnm7 = *blanks and sccnm8 = *blanks
     c                             and sccnm9 = *blanks and sccnm10= *blanks
     c                             and sccnm11= *blanks and sccnm12= *blanks
     c                   seton                                        7290
     c                   endif
     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *================================================================
     C     errck2        begsr
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
     c                   eval      sccnm1  = fm2011
     c                   eval      sccnm2  = fm2012
     c                   eval      sccnm3  = fm2013
     c                   eval      sccnm4  = fm2014
     c                   eval      sccnm5  = fm2015
     c                   eval      sccnm6  = fm2016
     c                   eval      sccnm7  = fm2017
     c                   eval      sccnm8  = fm2018
     c                   eval      sccnm9  = fm2019
     c                   eval      sccnm10 = fm2020
     c                   eval      sccnm11 = fm2021
     c                   eval      sccnm12 = fm2022
     c*

     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   eval      fmco#  = elco#
     c                   eval      fmfnd  = elfnd
     c                   eval      fmfyr  = elfyr
     c                   eval      fmemp# = elemp#
     c                   eval      fmdiv  = eldiv
     c*
     c                   eval      fm2011  = sccnm1
     c                   eval      fm2012  = sccnm2
     c                   eval      fm2013  = sccnm3
     c                   eval      fm2014  = sccnm4
     c                   eval      fm2015  = sccnm5
     c                   eval      fm2016  = sccnm6
     c                   eval      fm2017  = sccnm7
     c                   eval      fm2018  = sccnm8
     c                   eval      fm2019  = sccnm9
     c                   eval      fm2020  = sccnm10
     c                   eval      fm2021  = sccnm11
     c                   eval      fm2022  = sccnm12
     c*
     c                   if        *in40 = *on
     c*
     c                   open      wledl
     c*
     c                   eval      xxform = 'WC480317'
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
     c                   eval      fmseq# = xxseq#
     c                   eval      fmform = 'WC480317'
     c*
     c                   endif
     c*
     c                   endsr


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

