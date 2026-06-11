     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE M0021R - Work with Environment Library List
     H*********************************************************************
     H* Called from: WWENV command                                        *
     H*********************************************************************
    H*
     Fm0021f    cf   e             workstn infds(fids01)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fmmllp     if   e             disk    rename(pll01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fmmlll     if   e           k disk    rename(pll01:dbRcdF)
     F                                     infds(dbfds)
     Fmmlll1    if   e           k disk    rename(pll01:dbRcdF2)
     F                                     infds(dbfds2)
     Fstgtp     if   e             disk    rename(pgt01:pgt0p)
    D*
     D #ofHD           c                   2
     D #ofOF           c                   13
     D hd              s             78    dim(#ofHD) ctdata perrcd(1)
     D of              s              1    dim(#ofOF) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOF) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Environment Library Lists'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'M0021R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   12
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
     D @@call          s              1
     D @@kc            s              1n
     D @@kl            s              1n
     D @@rrn           s              9  0
     D command         s            255
     D inqMode         s              1n   inz
     D kkApp           s                   like(LLAPP)
     D kkClt#          s                   like(LLCLT#)
     D kkEnv           s                   like(LLENV)
     D kkRls           s                   like(LLRLS)
     D maintMode       s              1n   inz
     D selectMode      s              1n   inz
     D srchRRN         s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D SrchRls         pr                  extPgm('S2380R')
     D  call                          1    const
     D  kc                            1n
     D  kl                            1n
     D  rrn                           9  0
     D  tableID                       4    const
     D*
     DGetDesc          pr            30
     D inDataArea                    10    const
     D*
     D ValidClt#       pr             1n
     D  clt#                               like(ssClt#) const
     D*
     D ValidNum        pr             1n
     D  inAlpha                            like(ssClt#) const
     D*
     D ValidSubset     pr             1n
     D*
     D/copy mPrMenu
     D/copy sPrRunCmd
    C*
     C/COPY SCOPYWW01C
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scDbRR        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     key           klist
     C                   kfld                    kkClt#
     C                   kfld                    kkApp
     C                   kfld                    kkEnv
     C                   kfld                    kkRls
     C*
     C     key2          klist
     C                   kfld                    kkRls
     C                   kfld                    kkClt#
     C                   kfld                    kkApp
     C                   kfld                    kkEnv
     C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
     C     chkssm        begsr
     C*
     C                   eval      *in11 = ssStat <> *blank
     C                                  or ssRls  <> *blank
     C                                  or ssClt# <> *blank
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk1 - Additional errors on subfile selection
    C*====================================================================
     C     ErrCk1        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk2 - Error checking for the subset screen
    C*====================================================================
     C     ErrCk2        begsr
     C*
     C                   exsr      OthFks
     C*
     C                   endsr
     C*
    C*====================================================================
    C* InzSst - Initialize the subset fields
    C*====================================================================
     C     InzSst        begsr
     C*
     C                   if        selectMode
     C                   eval      ssStat = 'A'
     C                   eval      ssApp  = 'LIV'
     C                   eval      ssClt# = q1User
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeyInz - Initialize key values for program start
    C*====================================================================
     C     KeyInz        begsr
     C*
     C                   clear                   kkApp
     C                   clear                   kkClt#
     C                   clear                   kkEnv
     C                   clear                   kkRls
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySet - Set key values when doing "position to"
    C*====================================================================
     C     KeySet        begsr
     C*
     C                   clear                   kkApp
     C                   clear                   kkClt#
     C                   clear                   kkEnv
     C                   clear                   kkRls
     C*
     C                   select
     C                   when      view# = 1
     C                   eval      kkClt# = scPst1
     C                   when      view# = 2
     C                   eval      kkClt# = scPst2
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KeySfl - Set key values from subfile records for paging
    C*====================================================================
     C     KeySfl        begsr
     C*
     C                   eval      kkApp  = scApp
     C                   eval      kkClt# = scClt#
     C                   eval      kkEnv  = scEnv
     C                   eval      kkRls  = scRls
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr
     C*
     C                   select
     C                   when      hFld = 'SSRLS'
     C                   exsr      SearchRls
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C                   eval      scApp  = LLAPP
     C                   eval      scClt# = LLCLT#
     C                   eval      scEnv  = LLENV
     C                   eval      scRls  = LLRLS
     C                   eval      scDtaA = LLDTAA
     C*
     C                   select
     C                   when      view# = 1
     C                   eval      scData = LLCLT# + '  '
     C                                    + LLAPP  + '   '
     C                                    + LLENV  + '   '
     C                                    + LLRLS  + '  '
     C                                    + LLDTAA + '  '
     C                                    + GetDesc(LLDTAA) + '   '
     C                                    + LLSTAT + '   '
     C                   when      view# = 2
     C                   eval      scData = LLRLS  + '  '
     C                                    + LLCLT# + '  '
     C                                    + LLAPP  + '   '
     C                                    + LLENV  + '   '
     C                                    + LLDTAA + '  '
     C                                    + GetDesc(LLDTAA) + '   '
     C                                    + LLSTAT + '   '
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Opt20 - Process option 20 = Change Status
    C*====================================================================
     C     Opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C*
     C                   call      ACDPgm        ACDPl                  99
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OthFks - Process other function keys
    C*====================================================================
     C     OthFks        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
     C*
    C*====================================================================
    C* OthOpt - Process other options
    C*====================================================================
     C     OthOpt        begsr
     C*
     C     scSel         caseq     '20'          Opt20
     C                   endcs
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C*
     C                   if        %parms >= 2
     C                   eval      @@kc = *inkc
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      @@kl = *inkl
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   if        %parms >= 1
     C                   eval      maintMode  = @@call = ' '
     C                   eval      inqMode    = @@call = '1'
     C                   eval      selectMode = @@call = '2'
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Read - Read next record from file
    C*====================================================================
     C     Read          begsr
     C*
     C                   select
     C                   when      view# = 1
     C                   read      dbRcdF                                 95
     C                   when      view# = 2
     C                   read      dbRcdF2                                95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ReadP - Read previous record from file
    C*====================================================================
     C     ReadP         begsr
     C*
     C                   select
     C                   when      view# = 1
     C                   readp     dbRcdF                                 95
     C                   when      view# = 2
     C                   readp     dbRcdF2                                95
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* SearchRls - Search for release
    C*====================================================================
     C     SearchRls     begsr
     C*
     C                   callp     SrchRls('2':*inKC:*inKL:srchRRN:'CDDT')
     C*
     C     srchRRN       chain     stgtp
     C*
     C                   select
     C                   when      not %found or *inKL
     C                   eval      *in84 = *on
     C                   when      %found
     C                   eval      ssRls  = GTKEY
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetGT - Set greater than on file
    C*====================================================================
     C     SetGT         begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key           setgt     dbRcdF                             96
     C                   when      view# = 2
     C     key2          setgt     dbRcdF2                            96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SetLL - Set lower limits on file
    C*====================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key           setll     dbRcdF                             96
     C                   when      view# = 2
     C     key2          setll     dbRcdF2                            96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* Sf1Upd - Update subfile record when item is deleted
    C*====================================================================
     C     Sf1Upd        begsr
     C*
     C                   eval      scData = '* * * Deleted * * *'
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SlctRd - Move fields for selected record to parameters
    C*====================================================================
     C     SlctRd        begsr
     C*
     C                   eval      @@rrn = scDbRR
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
     C     subset        begsr
     C*
     C                   eval      meetSS = ValidSubset
     C*
     C                   endsr





    P*====================================================================
    P* GetDesc - Get the description from the data area passed in
    P*====================================================================
     PGetDesc          b
     DGetDesc          pi            30
     D inDataArea                    10    const

     DdtaAraInfo       ds           450    inz
     D dtaAraDesc                    30    overlay(dtaAraInfo:401)

     D rtnCode         s              4

      /Free
       LoadDtaAra('*IN':inDataArea:dtaAraInfo:rtnCode);

       Return dtaAraDesc;
      /End-free

     PGetDesc          e





    C*====================================================================
    C* ValidClt# - Validate the client #
    C*====================================================================
     PValidClt#        b
     D ValidClt#       pi             1n
     D  clt#                               like(ssClt#) const

     C                   select
     C                   when      clt# = 'BASE'
     C                   return    *on
     C** **              when      %subst(clt#:1:1) <> 'C'
     C** **              return    *off
     C                   when      not ValidNum(clt#)
     C                   return    *off
     C                   endsl
     C*
     C                   return    *on

     PValidClt#        e





    C*====================================================================
    C* ValidNum - Validate for a numeric alpha
    C*====================================================================
     PValidNum         b
     D ValidNum        pi             1n
     D  inAlpha                            like(ssClt#) const

     D alpha           s              1
     D size            s              3  0
     D x               s              3  0

     C                   eval      size = %size(inAlpha)
     C*
     C                   do        size          x
     C                   if        %subst(inAlpha:x:1) <> '0' and
     C                             %subst(inAlpha:x:1) <> '1' and
     C                             %subst(inAlpha:x:1) <> '2' and
     C                             %subst(inAlpha:x:1) <> '3' and
     C                             %subst(inAlpha:x:1) <> '4' and
     C                             %subst(inAlpha:x:1) <> '5' and
     C                             %subst(inAlpha:x:1) <> '6' and
     C                             %subst(inAlpha:x:1) <> '7' and
     C                             %subst(inAlpha:x:1) <> '8' and
     C                             %subst(inAlpha:x:1) <> '9' and
     C                             %subst(inAlpha:x:1) <> ' '
     C                   return    *off
     C                   endif
     C                   enddo
     C*
     C                   return    *on

     PValidNum         e





    C*====================================================================
    C* ValidSubset - Validate the subsets
    C*====================================================================
     PValidSubset      b
     D ValidSubset     pi             1n

     C                   if        ssStat <> *blank and
     C                             ssStat <> LLSTAT
     C                   return    *off
     C                   endif
     C*
     C                   if        ssRls  <> *blank and
     C                             ssRls  <> LLRLS
     C                   return    *off
     C                   endif
     C*
     C                   if        ssApp  <> *blank and
     C                             ssApp  <> LLAPP
     C                   return    *off
     C                   endif
     C*
     C                   if        ssClt# <> *blank
     C                   select
     C                   when      ssClt# = 'BASE' and
     C                             ssClt# <> LLCLT#
     C                   return    *off
     C                   when      ValidClt#(ssClt#) and
     C                             ssClt# <> LLCLT#
     C                   return    *off
     C                   when      not ValidClt#(ssClt#) and
     C                             (ssClt# <> LLCLT# and
     C                              not ValidClt#(LLCLT#))
     C                   return    *off
     C                   endsl
     C                   endif
     C*
     C                   return    *on

     PValidSubset      e





** Column headings
Opt  Clt#  App  Env  Rls    DataArea    Description                     Sts
Opt  Rls    Clt#  App  Env  DataArea    Description                     Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   nNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Sts            YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
