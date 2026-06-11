     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2378R
    H* DESCRIPTION: Work with IRF Wage Index Table
    H* CALLED FROM: F0601R
    H* CALLS TO   :
    H**********************************************************************
    H*  CREATED: 10/20/11  C:280  PROJECT: 114MAR
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
     H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FF2378F    CF   E             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
    F*
     FFMIWP     if   e             disk
     F                                     rename(PIW01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
    F*
     FFMIWL1    Uf   e           k disk
     F                                     infds(dbfds)
     F                                     rename(PIW01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
     D optnm           c                   'IRF Wage Index Table'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2378R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkcbsa          s                   like(iwcbsa)
     D kkefdt          s                   like(iwefdt)
     D x1data          s             30
     D x2data          s             40
     D xxdt            s               d   datfmt(*usa)
     D xxdt1           s             10a
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9 0
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
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     iwky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C* none used
    C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
    C*
     C     chkssm        begsr
     C*
     C                   if        ssefdt <> 0 or
     C                             ssstat <> ' '
     C                   eval      *in11 = *on
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
    C*
     C     errck1        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
    C*
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
    C*
    C*================================================================
    C* ERRCKP - Error checking printing
    C*================================================================
    C*
     C     errckp        begsr
     C*
     C                   eval      *IN90 = *OFF
     C                   eval      *IN59 = *OFF
     C*
     C                   if        scefdt <> 0
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   eval      *IN90 = *On
     C                   eval      *IN59 = *On
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
    C*
     C     inzsst        begsr
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
    C*
     C     keyinz        begsr
     C*
     C                   eval      kkcbsa = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
    C*
     C     keyset        begsr
     C*
     C   21              eval      kkcbsa = scpst1
     C*
     C                   if        *in21 = *on
     C                   z-add     scpsdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
    C*
     C     keysfl        begsr
     C*
     C                   eval      kkcbsa = scbsa
     C*
     C                   z-add     scedt         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   MOVEL     SRDTO         kkefdt
     C*
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      sccbsa = iwcbsa
     C                   eval      scbsa = iwcbsa
     C                   eval      scefdt = iwefdt
     C                   eval      scedt = iwefdt
     c*
     C     *cymd         move      iwefdt        xxdt
     C                   movel     xxdt          xxdt1
     C                   movel     xxdt          scefdt
     C                   movel     xxdt          scedt
     c*
     C                   eval      scarea = sccbsa + '  '
     C                             + xxdt1 + '  '
     C                             + scarea
     C*
     C                   eval      scarea = iwarea
     C                   eval      scwind = iwwind
     C                   eval      scstat = iwstat
     C*
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
    C*
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C                   endcs
    C*
     C                   endsr
    C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
    C*
     C     othopt        begsr
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
    C*
     C     pgmend        begsr
     C*
     C                   if        *inkc=*on or  *inkl=*on
     C                   eval      *inlr = *on
     C                   else
     C                   eval      rstpgm= *on
     C                   endif
     C*
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
    C*
     C     pgmstr        begsr
     C                   endsr
    C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
    C*
     C     read          begsr
    C*
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
    C*
     C     readp         begsr
    C*
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
    C*
     C     setgt         begsr
    C*
     C                   if        *in21 = *on
    C*
     C     iwky          setgt     dbrcdf                             96
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
    C*
     C     setll         begsr
    C*
     C                   if        *in21 = *on
    C*
     C     iwky          setll     dbrcdf                             96
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
    C*
     C     sf1upd        begsr
     C                   endsr
    C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
    C*
     C     slctrd        begsr
    C*
     C                   eval      @@rrn = scdbrr
    C*
     C                   endsr
    C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
    C*
     C     subset        begsr
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> iwefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> iwstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
    C*
** Column headings
Opt  CBSA   Description                               Eff. Date   Wage    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
