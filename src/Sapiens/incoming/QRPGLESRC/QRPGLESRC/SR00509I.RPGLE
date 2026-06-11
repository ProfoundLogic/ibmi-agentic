      * (C) Copyright FISERV Insurance Solutions, Inc., 2007

      /title SR00509A - W/W Addtional Input - WC000509A
      *****************************************************************
      * Description:
      *
      * "Work With" - Multiple Manual Additional Input Program
      *               Endorsement specific data file storage
      *
      * Called From: SR00509A
      * Calls To:    SR00509I2
      *
      *****************************************************************
      * Revised:
      *
      * 07/04/08  C:61 Project: 114WQG
      *                Created.
      *
      *****************************************************************

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fsr00509f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    
     *  Physical file definition (for refreshing subfile record)
     Fwlrpp     if   e             disk
     F                                     rename(prp01:pfrcdf)
    
     *  Primary logical file definition (for displaying items)
     Fwlrpl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(prp01:dbrcdf)
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
      * Rating State
     Fwmrsl     if   e           k disk
    
    
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)


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

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

    
      *================================================================
      *  /COPY required constants
      *================================================================
      *
     D optnm           c                   'Retro Premium Changes End'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'SR00509I2'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   8


      *---------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *---------------------------------------------------------------------
     D ##conm          s             40
     D ##fnam          s             40
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     d ##rrn           s                   like(@@rrn)
     d ##rrn2          s                   like(@@rrn2)
     D @@call          s              1

     D kkco#           s                   like(rpco#)
     D kkfnd           s                   like(rpfnd)
     D kkfyr           s                   like(rpfyr)
     D kkemp#          s                   like(rpemp#)
     D kkdiv           s                   like(rpdiv)
     D kkseq#          s                   like(rpseq#)

     d ##co#           s                   like(elco#)
     d ##fnd           s                   like(elfnd)
     d ##emp#          s                   like(ememp#)
     d ##div           s                   like(emdiv)
     d ##name          s                   like(emname)
     d ##fyr           s                   like(elfyr)
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##prod          s              5  0
     d ##pnam          s             60
     d x               s              7  0
     d xxst            s                   like(rsjrst)
     d xxstdt          s                   like(elpfdt)
     d x1div           s                   like(eldiv)
     d x1pfdt          s                   like(elpfdt)
     d x1andt          s                   like(elandt)
     d xspfdt          s                   like(elpfdt)
     d xsfyr           s                   like(elfyr)
     d xsandt          s                   like(elandt)
     d xsdiv           s                   like(eldiv)


      *---------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *---------------------------------------------------------------------

    
     c/copy scopyww01c
    

      *================================================================
      *  PLIST definitions
      *================================================================

     c     *entry        plist
     c                   parm      *INKC         @@KC              1
     c                   parm      *INKL         @@KL              1
     c                   parm                    @@call
      ******                               Additional input data file
     c                   parm                    @@RRN             9 0
      ******                               WDELP
     c                   parm                    @@RRN2            9 0
      *
      *  Add/change/display program parameter list
      *
     c     acdpl         plist
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode
      *****                                Additional input data file
     c                   parm      scdbrr        ##rrn
      *****                                wdelp
     c                   parm      @@rrn2        ##rrn2
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam

     c     dtmdy         plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      '*MDY'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

      *
     c     agtpl         plist
     c                   PARM      elco#         ##CO#
     c                   PARM      elfnd         ##FND
     c                   PARM      elemp#        ##EMP#
     c                   PARM      eldiv         ##DIV
     c                   PARM      elfyr         ##FYR
     c     scagt#        PARM                    ##AGT#
     c                   PARM                    ##ANAM
     c     scprod        PARM                    ##PROD
     c                   PARM                    ##PNAM
      *


      *================================================================
      *  KLIST definitions
      *================================================================

     c     key           klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#

     c     key1          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#
     c                   kfld                    kkseq#

     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     emky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#

     c     zexky         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv


      *================================================================
      * cHKSSM - Set indicator 11 for subset message
      *================================================================
     c     chkssm        begsr
     c                   endsr


      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     c     errck1        begsr
     c                   endsr


      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     c     errck2        begsr
     c                   endsr


      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     c     inzsst        begsr
     c                   endsr


      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     c     keyinz        begsr
     c                   eval      kkco#  = elco#
     c                   eval      kkfnd  = elfnd
     c                   eval      kkfyr  = elfyr
     c                   eval      kkemp# = elemp#
     c                   eval      kkseq# = 1
     c                   endsr


      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     c     keyset        begsr

     c                   eval      kkco#  = elco#
     c                   eval      kkfnd  = elfnd
     c                   eval      kkfyr  = elfyr
     c                   eval      kkemp# = elemp#
     c                   move      scpst1        kkseq#

     c                   endsr


      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     c     keysfl        begsr
     c                   eval      kkco#  = elco#
     c                   eval      kkfnd  = elfnd
     c                   eval      kkfyr  = elfyr
     c                   eval      kkemp# = elemp#
     c                   eval      kkseq# = scseq#
     c                   endsr


      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     c     mvdbsc        begsr

     c                   eval      scseq# = rpseq#

     c                   select

     c                   when      (*in21 = *on)
     c                   eval      scdata = %editc(rpseq#:'4')

     c                   endsl

     c                   endsr


      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     c     othfks        begsr
     c                   endsr


      *================================================================
      * OTHOPT - Process other options
      *================================================================
     c     othopt        begsr
     c                   endsr


      *================================================================
      * PGMEND - End of program processing
      *================================================================
     c     pgmend        begsr
     c                   eval      *inlr = *on
     c                   endsr


      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     c     pgmstr        begsr

      *  Load Screen Headings

     c     @@rrn2        chain     pel01                              60
     c     emky          chain     pem01                              60
     c                   eval      kkco#  = elco#
     c                   eval      kkfnd  = elfnd
     c                   eval      kkemp# = elemp#
     c                   eval      kkdiv  = eldiv
     c                   eval      kkfyr  = elfyr
     c                   eval      scfyr  = elfyr
     c                   eval      scfnd  = elfnd

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

      *  Audited/Estimated

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

      *  Get Agent/Producer Information

     c                   call      'WA002R'      AGTPL
     c                   eval      scanam = ##anam
     c                   eval      scpnam = ##pnam

      *  Get Employer Cross Reference info for header

     c                   clear                   pex02
     c     zexky         chain     pex02                              63
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv

     c                   z-add     1             kkseq#
     c                   endsr


      *================================================================
      * READ   - Read next record from file
      *================================================================
     c     read          begsr

     c                   select

     c                   when      *in21 = *on
     c     key           reade     dbrcdf                                 95

     c                   endsl

     c                   endsr


      *================================================================
      * READP  - Read previous record from file
      *================================================================
     c     readp         begsr

     c                   select

     c                   when      *in21 = *on
     c     key           readpe    dbrcdf                                 95

     c                   endsl

     c                   endsr


      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     c     setgt         begsr

     c                   select

     c                   when      *in21 = *on
     c     key1          setgt     dbrcdf                             96

     c                   endsl

     c                   endsr


      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     c     setll         begsr

     c                   select

     c                   when      *in21 = *on
     c     key1          setll     dbrcdf                             96

     c                   endsl

     c                   endsr


      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     c     sf1upd        begsr
     c                   eval      scdata = '  *Deleted'
     c                   endsr


      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     c     slctrd        begsr
     c                   eval      @@rrn = scdbrr
     c                   endsr


      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     c     subset        begsr
     c                   endsr


** column headings
Opt  Sequence #
** Description--------------MISN
1011=Select                 NNY
1022=change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=change view          YYY
212F12=cancel               YYY
217F17=Subset               NNN
3
