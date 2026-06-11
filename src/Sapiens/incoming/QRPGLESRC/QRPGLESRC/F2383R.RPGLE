     H*copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H*StoneRiver is a trademark of StoneRiver, Inc.')
      **************************************************************************
     H/TITLE - Work With IRF Provider Specific File                            *
     H*                                                                        *
     H*  DESCRIPTION: This program Shows the W/W  Screen for FMIRP             *
     H*                                                                        *
     H*   CALLED BY: MENU                                                      *
     H*                                                                        *
     H**************************************************************************
     H*  CREATED: 10/17/2011  C:251 PROJECT 114MAR                             *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H**************************************************************************
    F*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2383f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmirp     if   e             disk
     F                                     rename(pir01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmirl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pir01:dbrcdf)
    F*  Secondary logical file definition (for subset option)
     Ffmirl2    if   e           k disk
     F*                                     rename(pir01:dbrcdf1)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IRF Provider Specific File'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2383R01'
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
     D kkprov          s                   like(irprov)
     D kkefdt          s                   like(irffdt)
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
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@rrn             9 0
    C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
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
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkprov
     C                   kfld                    kkefdt
     C*
     C     key2          klist
     C                   kfld                    kkefdt
     C                   kfld                    kkprov
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssprov <> *blank or ssefdt <> 0 or
     C                             ssstat <> ' '
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
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
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
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkprov = *loval
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              eval      kkprov = scpst1
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   movel(p)  scprov        kkprov
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C*
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     irprov        scprov
     C                   z-add     irffdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scefdt
     C                   Movel     irstat        scstat
     C*
     C                   eval      scdata = scprov  + '                '
     C                                    + %editw(scefdt:'  /  /    ')
     C                             + '                               '
     C                                          + %trim(scstat)
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
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
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
     C                   MOVE      SCDBRR        @@RRN
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   if        ssprov <>  ' ' and ssprov <> irprov
     C                   eval      meetss = *off
     C                   endif
      *
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            8 0
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> irffdt
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> irstat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
** Column headings
Opt  Provider number       Effective date                            Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
