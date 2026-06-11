     H*  (C) Copyright Progressive Data Solutions, Inc., 2001
    H*
     H/TITLE W0017R - Work With Loss Constants
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* WORK WITH PROGRAM FOR LOSS CONSTANTS                          *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: E0017R02                                            *
     H*                                                               *
     H* CREATED: 09/28/01
     H*                                                               *
     H* 09/28/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SSEFDT - INVALID DATE                               *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0017f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdscp     if   e             disk
     F                                     rename(psc01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdscl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(psc01:dbrcdf)
     F*  Group file (for getting group info in pgmstr)
     Fwmfnp     if   e             disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Loss Constants'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0017R02'
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
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D kkco#           s                   like(scco#)
     D kkfnd           s                   like(scfnd)
     D kkst            s                   like(scst)
     D kkefdt          s                   like(scefdt)
     D kkprbs          s                   like(scprbs)
     D xxefdt          s                   like(scefdt)
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
     C                   parm                    @@kc
     C                   parm                    @@kl
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
     C                   parm      sc_fnd        ##fnd
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
      *
     C     scky1         klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
      *
     C     scky2         klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt
     C                   kfld                    kkprbs
      *
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
      *
     C                   if        ssst <> *blanks or ssefdt <> 0
     C                   seton                                        11
     C                   endif
      *
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
      *
     C                   if        *in90 = *off and ssefdt <> 0
     C                   eval      srdti = ssefdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel(p)  srdto         ssefdt
     C                   if        srerr = '1'
     C                   seton                                        7190
     C                   endif
     C                   endif
      *
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
     C     @@rrn         chain     pfn01                              60
     C                   if        not *in60
     C                   eval      kkfnd = fnfnd
     C                   else
     C                   eval      kkfnd = 0
     C                   endif
     C                   eval      kkst = *blanks
     C                   eval      kkefdt = 0
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move(p)   scpst1        kkst
     C                   eval      srdti = scpsdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkst = sc_st
     C                   eval      srdti = sc_efdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C                   eval      kkprbs = sc_prbs
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
      *
     C                   if        *in21
     C                   eval      sc_st = scst
     C                   eval      srdti = scefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel(p)  srdto         sc_efdt
     C                   eval      sc_prbs = scprbs
     C                   eval      sc_stat = scstat
     C                   endif
      *
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
     C                   if        not *in60                                    from chain operation
     C                   eval      sc_fnd = fnfnd                               in KEYINZ subroutine
     C                   eval      scfname = fnname
     C                   else
     C                   eval      sc_fnd = 0
     C                   eval      scfname = *blanks
     C                   endif
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     scky1         reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     scky1         readpe    dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     scky2         setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     scky2         setll     dbrcdf                             96
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
      *
     C                   if        ssst <> *blanks and ssst <> scst
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssefdt <> 0
     C                   eval      srdti = ssefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         xxefdt
     C                   if        xxefdt <> scefdt
     C                   eval      meetss = *off
     C                   endif
     C                   endif
      *
     C                   endsr
** Column headings
Opt  St    Eff. Date   Prem Basis Amt    Stat
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
