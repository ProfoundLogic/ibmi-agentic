      *  (C) Copyright Progressive Data Solutions, Inc., 2005

      /title SR99FR10M - CA Approved Form 10
      *****************************************************************
      *
      * CALLED FROM: SR99FR10I - CA Approved Form 10
      *
      * CALLED TO:   None
      *
      *****************************************************************
      * Revised:
      *
      * 04/12/06 C:61 PROJECT 132WFF
      *               Created.
      *
      *****************************************************************

     Fsr99FR10g cf   e             workstn
    
     *  Physical file definition (for maintaining record)
     Fwmfrp     uf a e             disk
     F                                     rename(pfr01:dbrcdf)
    
      * Form 10
     Fwmfrl     if   e           k disk
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
      * Carrier Reporting Master
     Fwmrpl     if   e           k disk
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


     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'CA Approved Form 10'
     D comapi          c                   'WA001R'
     D #ofscr          c                   2


     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------

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

     d svpol#          s                   like(frpol#)


     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------


     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
      *                                     wmfrp
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

      * Carrier

     c     rpky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr

      * Form 10

     c     frky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    elfyr

      * Group

     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd

      * Employer

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

      * Employer Cross Reference

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
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

      *  Name must be entered

     c  n90              if        (scin01 = *blanks)
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif

      * default policy number

     c  n90              if        (@@mode = 'A') and (scpol# = *blanks)
     c                   movel(p)  svpol#        scpol#
     c                   endif

     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr

     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     c     mvdbsc        begsr

     c                   eval      scin01 = frin01
     c                   eval      scin02 = frin02
     c                   eval      scin03 = frin03
     c                   eval      scin04 = frin04
     c                   eval      scin05 = frin05
     c                   eval      scin06 = frin06
     c                   eval      scin07 = frin07
     c                   eval      scin08 = frin08
     c                   eval      scin09 = frin09
     c                   eval      scin10 = frin10
     c                   eval      scin11 = frin11
     c                   eval      scin12 = frin12
     c                   eval      scpol# = frpol#
     c                   eval      scissu = frissu
     c                   eval      scto   = frto
     c                   eval      scof   = frof
     c                   eval      scdaat = frdaat
     c                   eval      scthis = frthis
     c                   eval      scdaof = frdaof
     c                   z-add     fryear        scyear

     c                   if        @@mode = 'A'
     c     frky          setgt     pfr01
     c     frky          readpe    pfr01
     c
     c                   if        not(%eof)
     c     frseq#        add       1             scseq#
     c                   else
     c                   z-add     1             scseq#
     c                   endif

     c                   else

     c                   z-add     frseq#        scseq#
     c                   endif

     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   eval      frco#  = elco#
     c                   eval      frfnd  = elfnd
     c                   eval      frfyr  = elfyr
     c                   eval      fremp# = elemp#
     c                   eval      frseq# = scseq#

     c                   eval      frin01 = scin01
     c                   eval      frin02 = scin02
     c                   eval      frin03 = scin03
     c                   eval      frin04 = scin04
     c                   eval      frin05 = scin05
     c                   eval      frin06 = scin06
     c                   eval      frin07 = scin07
     c                   eval      frin08 = scin08
     c                   eval      frin09 = scin09
     c                   eval      frin10 = scin10
     c                   eval      frin11 = scin11
     c                   eval      frin12 = scin12
     c                   eval      frpol# = scpol#
     c                   eval      frissu = scissu
     c                   eval      frto   = scto
     c                   eval      frof   = scof
     c                   eval      frdaat = scdaat
     c                   eval      frthis = scthis
     c                   eval      frdaof = scdaof
     c                   z-add     scyear        fryear

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

      *  Get Carrier #

     c     rpky          chain     prp01

     c                   if        (%found)
     c                             and (rpcap# <> *blanks)
     c                   movel(p)  rpcap#        svpol#

     c                   else

     c                   movel(p)  elfnd         svpol#
     c                   movel(p)  elemp#        xxtmp1            9
     c                   cat(p)    xxtmp1:0      svpol#
     c                   movel(p)  elfyr         xxtmp1            9
     c                   cat(p)    xxtmp1:0      svpol#
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
     C                   eval      fruser = q1user
     C                   eval      frpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      frtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         frdt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      fruser = q1user
     C                   eval      friusr = q1user
     C                   eval      frpgm  = q1pgm
     C                   eval      fripgm = q1pgm
     C                   time                    dsdttm
     C                   eval      frtime = dstime
     C                   eval      fritm  = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         frdt
     C                   movel(p)  srdto         fridt
     C                   write     dbrcdf
     C                   feod      wmfrp
     c                   move      *on           outflg
     C                   endsr

