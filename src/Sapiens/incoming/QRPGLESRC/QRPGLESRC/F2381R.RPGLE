     H copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H******************************************************************
     H/TITLE F2381R                                                    *
     H******************************************************************
     H*  Description:  SNF Wage Index Table                            *
     H*                                                                *
     H*  Created: 10/06/11   Programmer C:251   Project:114MZO         *
     H******************************************************************
     H*  Revisions:                                                    *
     H*                                                                *
     H******************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2381f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmwgp     if   e             disk
     F                                     rename(pwg01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmwgl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pwg01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'SNF Wage Index Table'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2381R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
     D Lo              c                   'abcdefghijklmnopqrstuvwxyz'
     D Hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
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
     D kkarea          s                   like(wgarea)
     D kkefdt          s                   like(wgefdt)
     D kkcbsa          s                   like(wgcbsa)
     D u1pdsc          s                   like(wgarea)
     D u2pdsc          s                   like(wgarea)
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
     C                   parm                    @@KC              1
     C                   parm                    @@KL              1
     C                   parm                    @@RRN             9 0          INZ
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
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkarea
     C                   kfld                    kkefdt
     C                   kfld                    kkcbsa
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssefdt <> 0 or
     C                             ssstat <> ' '  or ssarea <> *blank
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
     C*
     C                   if        @@call = '2'
     C                   movel     'A'           ssstat
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkarea = *loval
     C                   eval      kkefdt = *loval
     C                   eval      kkcbsa = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              movel(p)  scpst1        kkarea
     C*
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C                   endif
     C*
     C                   eval      kkcbsa = *loval
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkarea = scarea
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C*
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         kkefdt
     C                   endif
     C*
     C                   eval      kkcbsa = sccbsa
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     wgarea        scarea
     C                   movel     wgcbsa        sccbsa
     C                   movel     wgstat        scstat
     C*
     C                   z-add     wgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C*
     C*
     C                   eval      scdata = wgarea  +
     C                             '      ' + %editw(scefdt:'  /  /    ') +
     C                               '   '   + wgcbsa + '     ' + wgstat
     C*
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
     C*
     C                   eval      @@rrn = scdbrr
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        ssarea <>  *blanks
     C     Lo:Hi         xlate(p)  wgarea        u1pdsc
     C     Lo:Hi         xlate(p)  ssarea        u2pdsc
     C                   exsr      chkstr
     C*
     C                   if        scnres = 0
     C                   move      *off          meetss
     C                   endif                                                  scrnes=0
     C*
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            7 0
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> wgefdt
     C                   eval      meetss = *off
     C                   endif

     C                   if        ssstat <> ' ' and ssstat <> wgstat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     ChkStr        Begsr
     C*
     C                   call      'QCLSCAN'
     C                   parm                    U1PDSC
     C                   parm      40            STRLEN            3 0
     C                   parm      1             START             3 0
     C                   parm                    U2PDSC
     C                   parm      40            PATLEN            3 0
     C                   parm      '0'           TRANS             1
     C                   parm      '1'           TRIM              1
     C                   parm      ' '           WILD              1
     C                   parm                    SCNRES            3 0
     C*
     C                   endsr
     C*
     C*================================================================
** Column headings
Opt  Area                                      Effective date   CBSA   Status
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
