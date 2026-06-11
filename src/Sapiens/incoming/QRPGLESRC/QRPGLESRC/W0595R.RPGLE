     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/24/12 C:190 PROJECT: 25WPW Priss# 150                      *
     H*                Correct subfile paging.                        *
     H*                                                               *
     H* ***************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0595f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmwcp     if   e             disk
     F                                     rename(pwc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmwcl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pwc01:dbrcdf)
     Fwmcmp     if   e             disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Wage Calculation'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0595R01'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
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
     D ##rrn1          s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@call          s              1
     D kkco#           s                   like(wcco#)
     D kkfnd           s                   like(wcfnd)
     D kkfyr           s                   like(wcfyr)
     D kkcase          s                   like(wccase)
     D kkseq#          s                   like(wcseq#)
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
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
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
     C                   parm      @@rrn         ##rrn1
    C*
     C     acdpl2        plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
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
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C     key           klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C******** temporary code remove after testing ** beg *******
     C****               if        scsel = '4'
      ****
     C****               eval      ##mgid = '0'
      ****
     C****               exsr      msgapi
      ****
     C****               endif
     C******** temporary code remove after testing ** end *******
     C*
     C* sts <> P
     C                   if        wcsts  <> 'P'
     C                   if        scsel = '2' or
     C                             scsel = '4' or
     C                             scsel = '7'
     C                   move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C* rvsd <> blank
     C                   if        wcrvsd  <> *blank and
     C**************************   wcsts  <> 'A' and
     C                             scsel = '9'
     C                   move      *on           *in72
     C     *in72         cabeq     *on           ender1                   90
     C                   endif
     C* sts <> A
     C                   if        wcsts  <> 'A' and
     C                             scsel = '9'
     C                   move      *on           *in73
     C     *in73         cabeq     *on           ender1                   90
     C                   endif
     C*
     C     ender1        endsr
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
     C*** BEG ADD ***** 01/24/12 ********************************
     C                   Eval      KKseq# = *zeros
     C*** END ADD ***** 01/24/12 ********************************
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*** BEG ADD ***** 01/24/12 ********************************
     C                   Eval      KKseq# = scseq#
     C*** END ADD ***** 01/24/12 ********************************
     C                   endsr
    C*================================================================
    C* MSGAPI - Call the message display API
    C*================================================================
     C     msgapi        begsr
     C*
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   z-add     wcstdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scstdt
     C                   z-add     wclsdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sclsdt
     C                   z-add     wcendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scendt
     C                   movel     wcsts         scsts
     C                   z-add     wcdedt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scdedt
     C                   if        wctaww > 0
     C                   z-add     wctaww        scaww
     C                   else
     C                   z-add     wciaww        scaww
     C                   endif
     C                   movel     wctype        sctype
     C                   movel     wcrvsd        scrvsd
     C*** BEG ADD ***** 01/24/12 ********************************
     c                   eval      scseq# = wcseq#
     C*** END ADD ***** 01/24/12 ********************************
     C*
     C                   eval      scdata = %editw(scstdt:'  /  /    ') +
     C                             ' ' + %editw(sclsdt:'  /  /    ') +
     C                             ' ' + %editw(scendt:'  /  /    ') +
     C                             '  ' + scsts +
     C                             '   ' + %editw(scdedt:'  /  /    ') +
     C                             ' ' + %editw(scaww:'  ,   .  ') +
     C                             '   ' + sctype +
     C                             '      ' + scrvsd
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
      *
     C     scsel         caseq     '7'           opt07
     C     scsel         caseq     '9'           opt09
     C                   endcs
      *
     C                   endsr
     *================================================================
     * Option 07 processing
     *================================================================
     C     opt07         begsr
      *
     C                   eval      ##mgid = '5'
      *
     C                   exsr      msgapi
      *
      * Approve mode
     C                   if        ##rtcd = 'Y'
     C                   move      'V'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   endif
     C                   endsr
     *================================================================
     * Option 09 processing
     *================================================================
     C     opt09         begsr
      *
     C                   call      'W0595R06'    acdpl2
      *
     C                   eval      ##mgid = '9'
      *
     C                   exsr      msgapi
      *
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C*
     C     @@rrn         chain(n)  pcm01                              99
     C                   move      cmco#         kkco#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
      *
     C                   call      'WA006R1'
     C                   parm                    kkco#
     C                   parm                    kkfnd
     C                   parm                    kkfyr
     C                   parm                    kkcase
     C                   parm                    sccs01
     C                   parm                    sccs02
     C                   parm                    sccs03
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   eval      scdata = ' * * * Deleted * * *'
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
Opt   Start dt    Last dt   Enter dt  Stat Decided dt     AWW    Type  Revised
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Approve                YNN
1099=Revise                 YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYN
217F17=Subset               NNN
3
