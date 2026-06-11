     H**************************************************************************************
     H copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* H/TITLE - Work with MS-DRG's                                                       *
     H*                                                                                    *
     H* DESCRIPTION: This program shows the subfile containing the records from FTDGP      *
     H*  DRG# , status and eff.date  are shown on the screen.                              *
     H*                                                                                    *
     H* CALLED BY: MENU                                                                    *
     H*                                                                                    *
     H* CALLS TO : F2354R01                                                                *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/03/08   Programmer C:189   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Removed option 5 and function 17 for GS
     H*                                                                                    *
     H*  03/16/11 C:257 Support: 76148                                                     *
     H*                 The search on DRG code default to Active codes                     *
     H*                                                                                    *
     H*  05/15/09 C:198 Support: 68468                                                     *
     H*                 Compilation only                                                   *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H**sy266_ovr**** DFTACTGRP(*NO)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Ff2354f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftdgp     if   e             disk
     F                                     rename(pdg01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fftdgl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pdg01:dbrcdf)
    F*  Secondary logical file definition (for subset option)
     Fftdgl1    if   e           k disk
     F                                     rename(pdg01:dbrcdf1)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'MS-DRGs'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2354R01'
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
     D kkdrg#          s                   like(dgdrg#)
     D kkefdt          s                   like(dgefdt)
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
     C                   parm                    @@drg#            5 0
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
     C                   kfld                    kkdrg#
     C                   kfld                    kkefdt
      *
     C     key2          klist
     C                   kfld                    kkefdt
     C                   kfld                    kkdrg#
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssdrg# <> 0      or ssefdt <> 0 or
     C                             ssstat <> ' '
     C                   seton                                        11
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C
     C
     C*
     C
     C
     C
     C
     C
     C
     C
     C
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
     C*** BEGIN ADD *** 03/16/11 **************************************
     C* Assume 'A' when subset is requested
     C     @@call        ifeq      '2'
     C                   move      'A'           ssstat
     C                   endif
     C*** END   ADD *** 03/16/11 **************************************
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkdrg# = *loval
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              Move      scpst1        xxdrg#            5 0
     C                   eval      kkdrg#    =   xxdrg#
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
     C                   movel(p)  scdrg#        kkdrg#
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
     C                   movel     dgdrg#        scdrg#
     C                   z-add     dgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   Movel     dgstat        scstat
     C                   Movel     dgrelw        screlw
     C                   Movel     dgdesc        scdesc
     C*
     C                   eval      scdata = %editc(scdrg#: '4')  + ' ' +
     C                               scdesc + '  '
     C                                    + %editw(scefdt:'  /  /    ')
     C                             + '         ' + %editc(screlw : '4')
     C                             + '         '   + scstat
     C
     C*
     C
     C*
     C
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
     C*** Beg Add *** MC367 *** 11/15/12 *****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 *****************************
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
     C                   Z-add     scdrg#        @@drg#
     C                   MOVE      SCDBRR        @@RRN
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   if        ssdrg# <>  0   and ssdrg# <> dgdrg#
     C                   eval      meetss = *off
     C                   endif
      *
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   MOVEL     SRDTO         xxefdt            7 0
     C                   endif
      *
     C                   if        ssefdt <>  *zeros and xxefdt <> dgefdt
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssstat <> ' ' and ssstat <> dgstat
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
** Column headings
Opt   DRG# Description            Effective date      Relative wt.      Status
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
