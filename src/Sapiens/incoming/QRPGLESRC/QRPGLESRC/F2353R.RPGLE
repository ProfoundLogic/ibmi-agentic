     H**************************************************************************************
     H copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* H/TITLE - Work with IPPS Hospital Impacts                                          *
     H*                                                                                    *
     H* DESCRIPTION: This program shows the subfile containing the records from FTIIP      *
     H*  Prov#, status and eff.date  are shown on the screen.                              *
     H*                                                                                    *
     H* CALLED BY: MENU                                                                    *
     H*                                                                                    *
     H* CALLS TO : F2353R01                                                                *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/03/08   Programmer C:189   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  05/15/09 C:198 Support: 68468                                                     *
     H*                 Compilation only                                                   *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2353f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftiip     if   e             disk
     F                                     rename(pii01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fftiil     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pii01:dbrcdf)
    F*  Secondary logical file definition (for subset option)
     Fftiil1    if   e           k disk
     F                                     rename(pii01:dbrcdf1)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IPPS Hospital Impacts'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2353R01'
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
     D kkprov          s                   like(iiprov)
     D kkefdt          s                   like(iiefdt)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
      ****
      ****
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
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkprov
     C                   kfld                    kkefdt
      *
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
     C*********          move      '0'           *in57
     C*********          if        scpstd <> 0
     C*
     C*********          z-add     scpstd        srdti
     C*********          call      'SRCVTDAT'    datchk
     C*
     C*********          if        srerr = '1'
     C*********          seton                                        57
     C*********          endif
     C*
     C*********          endif                                                  scpstd <> 0
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
     C                   call      'SRCVTDAT'    dtcymd
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
     C                   call      'SRCVTDAT'    dtcymd
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
     C                   movel     iiprov        scprov
     C                   z-add     iiefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   Movel     iistat        scstat
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
     C                   if        ssprov <>  ' ' and ssprov <> iiprov
     C                   eval      meetss = *off
     C                   endif
      *
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            7 0
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> iiefdt
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> iistat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
** Column headings
Opt  Provider number       Effective date                            Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
