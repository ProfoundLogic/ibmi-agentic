     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F2549R
    H* DESCRIPTION: Work with IL Region to Zip for Non Hospitals
    H* CALLED FROM: F0601R
    H* CALLS TO   : F2549R01
    H**********************************************************************
    H*  CREATED: 02/21/12  C:277  PROJECT: IL004
    H**********************************************************************
    H*  REVISIONS:
    H*  08/08/15 C: 079 Project: SY360
    H*                  Correct function keys
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
    H*
     Ff2549f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftnzpil   if   e             disk
     F                                     rename(pnz01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items for view1)
     Fftnzl1il  if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pnz01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items for view2)
     Fftnzl2il  if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pnz01:dbrcdf2)
     Fstabl     if   e           k disk
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IL Zip Codes to Regions (Non-Hosp)'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2549R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   2
     D viewmx          c                   2
     D sf1pag          c                   9
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
     D kkcnty          s                   like(nzcnty)
     D kkrgn#          s                   like(nzrgn#)
     D kkzip           s                   like(nzzip)
     D kkefdt          s                   like(nzefdt)
     D kkst            s                   like(stkey)
     D xxrgn#          s              3
     D xxcode          s              5
     D xxcnty          s             30
     D x1data          s             45
     D x2data          s             16
     D x3data          s             32
     D x4data          s             28
     D xxdt            s               d   datfmt(*usa)
     D u1pdsc          s            100
     D u2pdsc          s            100
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
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
     C                   parm                    @@st              2
     C                   parm                    @@rrn             9 0
     C                   parm                    @@view            1
     C                   parm                    @@zip             5 0
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C*                  parm      scst          ##st              2
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
     C     key1          klist
     C                   kfld                    kkzip
     C                   kfld                    kkcnty
     C                   kfld                    kkrgn#
     C                   kfld                    kkefdt
     C*
     C     key2          klist
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkst
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssrgn# <> *blanks or sscode <> *blanks or
     C                             ssefdt <> 0 or ssstat <> ' '
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
     C                   eval      kkzip  = *loval
     C                   eval      kkcnty = *loval
     C                   eval      kkrgn# = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              movel     scpst1        kkzip
     C   21              movel(p)  scpsct        kkcnty
     C   21              movel     scpsrg        kkrgn#
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         kkefdt
     C                   endif
     C                   endif
     C*
     C   22              movel     scpst2        kkzip
     C                   if        *in22 = *on
     C                   z-add     scpsdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C   21              movel     sccode        kkzip
     C   21              eval      kkcnty = sccnty
     C   21              movel     scrgn#        kkrgn#
     C   21              eval      kkefdt = scefdt
     C*
     C                   endsr
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   movel     @@st          kkst
     C                   movel     @@st          scst
     C*
     C     stky          chain     pst01
     C*
     C                   movel     stname        scstnm
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     nzrgn#        scrgn#
     C                   Move      nzzip         sccode
     C                   eval      sccnty = nzcnty
     C                   eval      scefdt = nzefdt
     C*
     C   21              eval      x1data = '   ' + sccode  + '        ' +
     C                             %subst(sccnty:1 :20) + '     ' + scrgn#
     C*
     C* Load the current record for display
     C*
     C     *cymd         move      nzefdt        xxdt
     C   21              movel     xxdt          x2data
     C*
     C   21              movel(P)  x1data        scdata
     C   21              move      x2data        scdata
     C*
     C   22              eval      x3data = '   ' + (sccode) + '        '
     C                             + %char(xxdt) + '      '
     C   22              move      scrgn#        x3data
     C   22              eval      x4data = %subst(sccnty: 1: 20) + '  '
     C*
     C   22              movel(P)  x3data        scdata
     C   22              move      x4data        scdata
     C*
     C                   eval      scstat = nzstat
     C*
     C                   endsr
    C*================================================================
    C* OPT20 - Subroutine for option 20 = Change status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
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
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   eval      @@kc = *inkc
     C                   eval      @@kl = *inkl
     C*
     C                   seton                                        LR
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   if        %parms > 4
     C                   if        @@view = '2'
     C                   eval      view# = 2
     C                   endif
     C                   endif
     C*
     C                   exsr      mksthd
    C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C   21              read      dbrcdf                                 95
     C   22              read      dbrcdf2                                95
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C   21              readp     dbrcdf                                 95
     C   22              readp     dbrcdf2                                95
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C   21key1          setgt     dbrcdf                             96
     C   22key2          setgt     dbrcdf2                            96
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C   21key1          setll     dbrcdf                             96
     C   22key2          setll     dbrcdf2                            96
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
     C                   if        @@call = '2'
     C                   movel     @@zip         scpst1
     C                   movel     @@zip         scpst2
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   eval      xxrgn# = *blanks
     C                   movel     nzrgn#        xxrgn#
     C                   movel     nzzip         xxcode
     C                   movel     nzcnty        xxcnty
     C*
     C                   if        (scpst1 <> *blanks and scpst1 <> xxcode)
     C                   eval      meetss = *off
     C                   leavesr
     C                   endif
     C*
     C                   if        (scpst2 <> *blanks and scpst2 <> xxcode)
     C                   eval      meetss = *off
     C                   leavesr
     C                   endif
     C*
     C                   if        scpsct <> *blanks and scpsct <> xxcnty
     C                   eval      meetss = *off
     C                   leavesr
     C                   endif
     C*
     C                   if        ssrgn# <> *blanks and ssrgn# <> xxrgn#
     C                   eval      meetss = *off
     C                   leavesr
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> nzefdt
     C                   eval      meetss = *off
     C                   leavesr
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> nzstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     string        begsr
     C*
     C                   call      'QCLSCAN'
     C                   parm                    U1PDSC
     C                   parm      50            STRLEN            3 0
     C                   parm      1             START             3 0
     C                   parm                    U2PDSC
     C                   parm      50            PATLEN            3 0
     C                   parm      '0'           TRANS             1
     C                   parm      '1'           TRIM              1
     C                   parm      ' '           WILD              1
     C                   parm                    SCNRES            3 0
     C*
     C                   if        scnres = 0
     C                   move      *off          meetss
     C                   endif                                                  scrnes=0
     C*
     C                   endsr                                                  string
     C*===============================================================
** Column headings
Opt     Zip code     County                   Region     Eff.Date          Sts
Opt     Zip code     Eff.Date     Region     County                        Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           NNN
3
