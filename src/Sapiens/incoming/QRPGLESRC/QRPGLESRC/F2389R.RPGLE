     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
    H**********************************************************************
    H* PROGRAM: F2389R
    H* DESCRIPTION: Work with Medicare Zip Codes + 4
    H* CALLED FROM: F6401R
    H* CALLS TO   : F2389R01
    H**********************************************************************
    H*  CREATED: 09/18/12  C:284  PROJECT: TX079
    H**********************************************************************
    H*  REVISIONS:
     H*
    H*  01/21/22 C:079 Project: FS629
     H*                 Use new logical to correct paging issue
     H*
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
     Ff2389f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftzpp     if   e             disk
     F                                     rename(pzp01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     f*** Beg Chg *** 01/21/22 *************************************************
     F****ftzpl1    if   e           k disk
     Fftzpl2    if   e           k disk
     f*** End Chg *** 01/21/22 *************************************************
     F                                     infds(dbfds)
     F                                     rename(pzp01:dbrcdf)
     F*
     Fststl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D*of              s              1    dim(10) ctdata perrcd(1)
     D*ofd             s             35    dim(10) alt(of)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Zip Codes + 4'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2389R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
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
     D x1data          s             30
     D x2data          s             40
     D xxdt            s               d   datfmt(*usa)
     D xxdt1           s             10a
     D kkkey           s                   like(stkey)
     D*
     D kkst            s                   like(zpst)
     D kkzip           s                   like(zpzip)
     D kkpls4          s                   like(zppls4)
     D kkefdt          s                   like(zpefdt)
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
     C                   parm                    @@call
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9 0
    C*
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
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
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*** Beg Chg *** 01/21/22 *************************************************
     C**** key1          klist
     C****               kfld                    kkst
     C****               kfld                    kkzip
     C****               kfld                    kkpls4
     C****               kfld                    kkefdt
     C     key1          klist
     C                   kfld                    kkst
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     C*** End Chg *** 01/21/22 *************************************************
     C*
     C     stky          klist
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssst <> *blanks or ssefdt <> 0 or
     C                             ssstat <> ' '  or sszip <> *zeros
     C                   seton                                        11
     C                   endif
     C*
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
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C*
     C                   if        ssefdt <> *zeros
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssefdt
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C*
     C                   endif
     C                   endif
     C*
     C                   if        ssstat <> ' '
     C                             and ssstat <> 'A' and ssstat <> 'I'
     C                   eval      *in58 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCKP - Error checking
    C*================================================================
     C     errckp        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkst = *loval
     C                   eval      kkzip = *loval
     C                   eval      kkpls4 = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkst = *blanks
     C                   eval      kkzip = *zeros
     C                   eval      sczipc = *zeros
     C*
     C   21              eval      kkst   = scpst1
     C                   If        Scpst2 <> *blanks
     C                   Eval      Sczipc = %dec(Scpst2:5:0)
     C                   Endif
     C   21              eval      kkzip  = sczipc
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkst   = scst
     C                   eval      kkzip  = sczip
     C                   eval      kkpls4 = scpls4
     C                   eval      kkefdt = scefdt
     C*
     C                   endsr
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
     C     kvdo          begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scst   = zpst
     C                   eval      sczip  = zpzip
     C                   eval      sccarr = zpcarr
     C                   eval      scloc  = zploc
     C                   eval      scrur  = zprur
     C                   eval      sc4flg = zp4flg
     C                   eval      scpls4 = zppls4
     C                   eval      scefdt = zpefdt
     c*
     C     *cymd         move      zpefdt        xxdt
     C                   movel     xxdt          xxdt1
     c*
     C                   eval      scdata = scst  + '     '
     C                             + %editc(sczip:'X') + '  '
     C                             + %editc(sccarr:'X') + '     '
     C                             + %editc(scloc:'X') + '        '
     C                             + scrur   + '       '
     C                             + sc4flg  + '       '
     C                             + %editc(scpls4:'X')  + '   '
     C                             + xxdt1  + '      '
     C*
     C                   eval      scstat = zpstat
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   seton                                        LR
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
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
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   eval      @@rrn = scdbrr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        ssst <> *blanks and ssst <> zpst
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        sszip <> *zeros and sszip <> zpzip
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> zpefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> zpstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  State  Zip   Carrier  Locality  Rural   Plus 4   Plus 4  Eff. Date  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           NNN
3
