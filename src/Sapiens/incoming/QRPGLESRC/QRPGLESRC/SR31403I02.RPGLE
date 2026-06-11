     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)

      /title SR31403I02 - "Info/Maint" NY PPO PremiumEnd Information
      *****************************************************************
      * DESCRIPTION:
      *
      * WC310403A - NY PPO Premium Endorsement
      *
      * "Info/Maint" - Endorsement "Addtional Information" program
      *              - Multiple, Manual with Additional Information.
      *
      * CALLED FROM: SR31403AI
      * CALLS TO:    None
      *
      * 02/22/12 C:282 PROJECT WC2127
      *                Created.
      *
      *****************************************************************
      * REVISED:
      *
      *
      *****************************************************************

      * zzz change display file name
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fsr31403f02cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     *  Physical file definition (for maintaining record)
     Fwledp     uf a e             disk
     F                                     rename(ped01:dbrcdf)
    
     Fwledl     if a e           k disk    prefix(p_) usropn
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

      * zzz change program title displayed
     D optnm           c                   'NY PPO Premium Information'
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

     d kkseq#          s                   like(fmseq#)
     d kkform          s                   like(fmform)
     d @@form          s                   like(fmform)
      *
     d ##dti           s              8  0
     d ##dto           s             10
     d ##err           s              1
     d ##ffmt          s              8
     d ##tfmt          s              8
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
     C                   parm                    @@form
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

      *
      *
     *================================================================
     *  KLIST definitions
     *================================================================
     c*
     c*  WLEDL key list for seq# retrieval
     c*
     c     edky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c                   kfld                    kkform
     c                   kfld                    kkseq#

     c     edky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c                   kfld                    kkform

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

      *  Premium reduction must be entered.
      *
     c                   if        (scprrd = *zero)
     c                   seton                                        7490
     c                   leavesr
     c                   endif
      *  Insured Name must be entered.
     c                   if        (scindnm1 = *blanks)
     c                   seton                                        7590
     c                   leavesr
     c                   endif
      *
     c                   endsr


     *================================================================
     * ERRCK2 - Error checking for screen 2 (SCR02)
     *================================================================
     C     errck2        begsr
      *
      *  PPO must entered.
     c                   if        (scpponm1 = *blanks)
     c                   seton                                        7690
     c                   leavesr
     c                   endif
      *
      *  Effective date must be entered.
     c                   movel     sceffdt       srdti
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = *on
     c                   seton                                        7790
     c                   leavesr
     c                   endif
      *
      *  Effective date must be entered between policy date range.
     c                   movel     sceffdt       srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxeedt            7 0
     c                   if        xxeedt < elpfdt or
     c                             xxeedt > elptdt
     c                   seton                                        7890
     c                   leavesr
     c                   endif
     c
      *
      *  Union Employees can be (Y/N).
     c                   if        (scunemp = 'Y'  or
     c                              scunemp = 'N')
     c                   setoff                                       7990
     c                   else
     c                   seton                                        7990
     c                   leavesr
     c                   endif
      *
      *  Union Employees in program must be entered based on Union Employees
     c                   select
     c                   when      (scunemp = 'Y')
      *
     c                   if        (scunempp = 'Y'  or
     c                              scunempp = 'N')
     c                   setoff                                       8090
     c                   else
     c                   seton                                        8090
     c                   leavesr
     c                   endif
      *
     c                   when      (scunemp = 'N')
      *
     c                   if        (scunempp = 'Y'  or
     c                              scunempp = 'N')
     c                   setoff                                       8190
     c                   else
     c                   seton                                        8190
     c                   leavesr
     c                   endif
      *
     c                   if         scunempp <> 'N'
     c                   seton                                        8290
     c                   leavesr
     c                   endif
      *
     c                   endsl
      *
      *  Total Estimated Number of Employees covered by PPO must be entered.
     c                   if        (sctotemp = *zero)
     c                   seton                                        8390
     c                   leavesr
     c                   endif
      *
     c                   endsr


     *================================================================
     * ERRCK3 - Error checking for screen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * GetSeq# - Get next Seq#
     *================================================================
     C     GetSeq#       begsr

     c                   open      wledl
     c                   eval      kkform = @@form
     c                   eval      kkseq# = *hival
     c     edky          setgt     ped01
     c     edky1         readpe(n) ped01
     c*
     c                   if        %found
     c                   eval      fmseq# = p_fmseq# + 1
     c                   else
     c                   eval      fmseq# = 1
     c                   endif

     c                   close     wledl
     c                   eval      fmform = @@form

     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

      *
      * Populate Premium Reduction %
     c                   eval      scprrd = fma521
      *
      * Populate Insured Name, Address(es)
     c                   eval      scindnm1 = fm2001
     c                   eval      scindnm2 = fm2002
     c                   eval      scindnm3 = fm2003
     c                   eval      scindnm4 = fm2004
     c                   eval      scindnm5 = fm2005
     c                   eval      scindnm6 = fm2006
     c                   eval      scindnm7 = fm2007
     c                   eval      scindnm8 = fm2008
     c                   eval      scindnm9 = fm2009
      *
      * Populate PPO Name(s)
     c                   eval      scpponm1 = fm2011
     c                   eval      scpponm2 = fm2012
     c                   eval      scpponm3 = fm2013
     c                   eval      scpponm4 = fm2014
     c                   eval      scpponm5 = fm2015
     c                   eval      scpponm6 = fm2016
      *
      * Populate Effective Date of Employers participation
     c                   eval      sceffdt = fmdt01
      *
      * Populate Union Employees
     c                   eval      scunemp = fmfl01
      *
      * Populate Union Employees in program
     c                   eval      scunempp = fmfl02
      *
      * Total Estimated Number of Employees covered by PPO.
     c                   eval      sctotemp = fma701
      *

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
     c                   eval      fmform = @@form
      *
      * Populate Premium Reduction %
     c                   eval      fma521 = scprrd
      *
      * Populate Insured Name, Address(es)
     c                   eval      fm2001 = scindnm1
     c                   eval      fm2002 = scindnm2
     c                   eval      fm2003 = scindnm3
     c                   eval      fm2004 = scindnm4
     c                   eval      fm2005 = scindnm5
     c                   eval      fm2006 = scindnm6
     c                   eval      fm2007 = scindnm7
     c                   eval      fm2008 = scindnm8
     c                   eval      fm2009 = scindnm9
      *
      * Populate PPO Name(s)
     c                   eval      fm2011 = scpponm1
     c                   eval      fm2012 = scpponm2
     c                   eval      fm2013 = scpponm3
     c                   eval      fm2014 = scpponm4
     c                   eval      fm2015 = scpponm5
     c                   eval      fm2016 = scpponm6
      *
      * Populate Effective Date of Employers participation
     c                   eval      fmdt01 = sceffdt
      *
      * Populate Union Employees
     c                   eval      fmfl01 = scunemp
      *
      * Populate Union Employees in program
     c                   eval      fmfl02 = scunempp
      *
      * Total Estimated Number of Employees covered by PPO.
     c                   eval      fma701 = sctotemp
      *
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
     C                   exsr      getseq#
     C                   eval      fmuser = q1user
     C                   eval      fmiusr = q1user
     C                   eval      fmpgm  = q1pgm
     C                   eval      fmipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      fmtime = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         fmdt
     C                   movel(p)  srdto         fmidt
     C                   write     dbrcdf
     C                   feod      wledp
     C                   endsr

