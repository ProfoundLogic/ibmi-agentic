     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
      *==================================================================================*
      *  DESCRIPTION:  Work With Direct Debit Information                                *
      *  CALLED FROM: W0050R                                                             *
      *                                                                                  *
      *  CREATED: 01/14/2004  Matt Dawson  Project: WC1116                               *
      *==================================================================================*
      *  REVISIONS:                                                                      *
      *                                                                                  *
      *==================================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw1251f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmdbp     if   e             disk
     F                                     rename(pdb01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmdbl2    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pdb01:dbrcdf)
     Fwmemp     if   e             disk
     Fwmfnl     if   e           k disk
     Fwzexl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Direct Debit Information'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W1251R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   10
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@call          s              1
     D @@inkc          s              1
     D @@inkl          s              1
     D @@rrn           s              9  0
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
     D expDateChar     s             10
     D kkco#           s                   like(dbco#)
     D kkfnd           s                   like(dbfnd)
     D kkemp#          s                   like(dbemp#)
     D kkdiv           s                   like(dbdiv)
     D kkdfia          s                   like(dbdfia)
     D kkcsfl          s                   like(dbcsfl)
     D kkstdt          s                   like(dbefdt)
     D effDate         s               d   datfmt(*usa)
     D expDate         s               d   datfmt(*usa)
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
     C                   parm                    @@inkc
     C                   parm                    @@inkl
     C                   parm                    @@call
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
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
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkdfia
     C                   kfld                    kkcsfl
     C                   kfld                    kkstdt

     c     emky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#
     C                   kfld                    emdiv

     c     fnky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
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

     C     @@rrn         chain     wmemp
     C                   if        %found
     C                   eval      kkco#  = emco#
     C                   eval      kkfnd  = emfnd
     C                   eval      kkemp# = ememp#
     C                   eval      kkdiv  = emdiv
     C                   eval      kkdfia = *loval
     C                   eval      kkcsfl = *loval
     C                   eval      kkstdt = *loval
     C                   else
     C                   exsr      pgmend
     C                   endif

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   eval      kkdfia = scpst1
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkdfia = scdfia
     C                   eval      kkcsfl = sccsfl
     C                   eval      kkstdt = scefdt
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     c                   eval      scdfia = dbdfia
     C                   eval      sccsfl = dbcsfl
     C                   eval      scefdt = dbefdt
     C                   eval      scdfii = dbdfii
     C                   eval      sctrcd = dbtrcd
     C     *cymd         move      dbefdt        effDate

     c                   if        dbexdt > 0
     C     *cymd         move      dbexdt        expDate
     c                   move      expDate       expDateChar
     c                   else
     c                   move      *blanks       expDateChar
     c                   endif


     C   21              eval      scdata = scdfia + '      ' +
     C                                      sccsfl + '   ' +
     C                                      %editc(scdfii:'X') + '   ' +
     C                                      %editc(sctrcd:'X') + '     ' +
     C                                      %char(effDate) + '   ' +
     C                                      expDateChar

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
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     fnky          chain     wmfnl
     c                   eval      scgrp = fnfnd
     c                   if        %found
     c                   eval      scname = fnname
     c                   else
     c                   eval      scname = *blanks
     C                   endif

     c                   call      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c     scedn         parm                    ##edn

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
Opt   Bank Account #       Type  Routing# CkDgt    Eff Date     Exp Date
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
