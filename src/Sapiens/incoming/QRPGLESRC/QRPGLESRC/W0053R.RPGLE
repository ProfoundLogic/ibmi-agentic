     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
      *==================================================================================*
      *  DESCRIPTION:  Work With Vendor History                                          *
      *  CALLED FROM: W0051R                                                             *
      *                                                                                  *
      *  CREATED: 01/09/2004  Matt Dawson  Project: WC1115                               *
      *==================================================================================*
      *  REVISIONS:                                                                      *
      *                                                                                  *
      *  06/04/03 C:136 Support: WA129 42766                                             *
      *                 Add vendor name to subfile                                       *
      *                                                                                  *
      *==================================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0053f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdvhp     if   e             disk
     F                                     rename(pvh01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdvhl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pvh01:dbrcdf)
     Fwdelp     if   e             disk
     Fwmfnl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwzexl     if   e           k disk
      *** beg add ***** 06/04/04 **************************************
     Fwmvpl     if   e           k disk
      *** end add ***** 06/04/04 **************************************
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Vendor History'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0053R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   10
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##emp#          s                   like(ememp#)
     D ##div           s                   like(emdiv)
     D ##name          s                   like(emname)
     D ##edn           s             80
     D @@call          s              1
     D @@rrn           s              9  0
     D @@inkc          s              1
     D @@inkl          s              1
     D effdate         s               d   datfmt(*usa)
     D expDate         s               d   datfmt(*usa)
     D expDateChar     s             10
     D kkco#           s                   like(vhco#)
     D kkgrp           s                   like(vhgrp)
     D kkemp#          s                   like(vhemp#)
     D kkdiv           s                   like(vhdiv)
     D kkfyr           s                   like(vhfyr)
     D kkvdr#          s                   like(vhvdr#)
     D kkefdt          s                   like(vhefdt)
      *** beg add ***** 06/04/04 **************************************
     D xxname          s             25
      *** end add ***** 06/04/04 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@inkc
     C                   parm      *inkl         @@inkl
     C                   parm                    @@call
     C                   parm                    @@rrn                          wdelp
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2                         wdelp
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
   68C     dtcymd        plist
   68C                   parm                    srdti
   68C                   parm      *blanks       srffmt
   68C                   parm      '*CYMD'       srtfmt
   68C                   parm                    srdto
   69C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkgrp
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkgrp
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkvdr#

     c     emky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv

     c     fnky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd

      *** beg add ***** 06/04/04 **************************************
     c     vpky          klist
     C                   kfld                    kkco#
     C                   kfld                    vhvdr#
      *** edg add ***** 06/04/04 **************************************
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr

     C     @@rrn         chain     wdelp
     C                   if        %found
     C                   eval      scgrp = elfnd
     C                   eval      scfyr = elfyr
     C                   eval      kkco#  = elco#
     C                   eval      kkgrp  = elfnd
     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv  = eldiv
     C                   eval      kkfyr  = elfyr
     C                   eval      kkvdr# = *loval
     C                   else
     C                   exsr      pgmend
     C                   endif

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move(p)   scpst1        kkvdr#
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkvdr# = scvdr#
     C                   move      scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      scefdt = vhefdt
     C                   eval      scvdr# = vhvdr#
     C     *cymd         move      vhefdt        effdate

     c                   if        vhexdt > 0
     C     *cymd         move      vhexdt        expDate
     c                   move      expDate       expDateChar
     c                   else
     c                   move      *blanks       expDateChar
     c                   endif

      *** beg add ***** 06/04/04 **************************************
     c     vpky          chain     pvp01
     c                   if        %found
     c                   eval      xxname = vpname
     c                   else
     c                   eval      xxname = *blanks
     c                   endif
      *** end add ***** 06/04/04 **************************************

     C   21              eval      scdata = %editc(vhvdr#:'4') + '      '  +
      *** beg add ***** 06/04/04 **************************************
     C                                      xxname + '      ' +
      *** end add ***** 06/04/04 **************************************
     C                                      %char(effdate) + '     ' +
     C                                      expDateChar
     c
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     fnky          chain     wmfnl
     c                   if        %found
     c                   eval      scname = fnname
     c                   else
     c                   eval      scname = *blanks
     C                   endif

     C     emky          chain     wmeml
     c                   if        %found
     c                   call      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c     scedn         parm                    ##edn
     C                   endif

     C     emky          chain     wzexl                              63
     c                   if        not *in63
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv
     C                   endif

     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1          readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key2          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key2          setll     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt  Vendor#     Vendor Name                      Eff Date       Exp Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YNN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
