     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2003-2008
    H**********************************************************************
    H* PROGRAM: F2536R
    H* DESCRIPTION: WORK WITH PHYSICAL THERAPY PER VISIT RATES
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 04/07/03  C:79   PROJECT: PA020
    H**********************************************************************
    H*  REVISIONS:
     H*
     H* 11/18/08 C:198 Project: PA046
     H*                Added NPI# after Effective Date.
     H*
    H**********************************************************************
    F*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2536f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffaptppa   if   e             disk
     F                                     rename(ppt01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffaptlpa   if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ppt01:dbrcdf)
     Fststl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Physical Therapy Per Visit'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2536R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   10
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
     D kkmed#          s                   like(ptmed#)
     D kkefdt          s                   like(ptefdt)
     D s1efdt          s                   like(ptefdt)
     D sys_date        s               D   datfmt(*USA)
     D kkkey           s                   like(stkey)
     D*** BEG CHG *** 11/18/08 ***********************************
     D*v1data*****     ds            68
     D v1data          ds            36
     D*** END CHG *** 11/18/08 ***********************************
     D  scmed#                 1     10
     D  x1efdt                14     23D   datfmt(*usa)
     D*** BEG CHG *** 11/18/08 ***********************************
     D**scname*****           32     68
     D  scnpi#                25     36
     D*** END CHG *** 11/18/08 ***********************************
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
     C                   parm                    @@call            1
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@st              2
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      scst          ##st
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
     C                   kfld                    kkmed#
     C                   kfld                    kkefdt
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
     C                   if        ssstat <> ' ' or ssmed# <> ' '
     C                             or s1efdt <> 0
     C                   eval      *in11 = *on
     C                   endif
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   if        *in90 <> *on
     C*
     C                   eval      s1efdt = ssefdt
     C*
     C                   if        ssefdt <> 0
     C*
     C                   eval      srdti = ssefdt
     C                   call      'SRCVTDAT'    DATCHK
     C                   movel     srdto         ssefdt
     C*
     C                   if        srerr = *on
     C                   seton                                        9071
     C                   goto      ck2end
     C*
     C                   else
     C     *usa          move      ssefdt        sys_date
     C     *cymd         move      sys_date      s1efdt
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   endif
     C*
     C     ck2end        endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkmed# = *blanks
     C                   eval      kkefdt = 0
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkmed# = scpst1
     C*
     C                   if        scpsta <> 0
     C*
     C                   eval      srdti = scpsta
     C                   call      'SRCVTDAT'    DATCHK
     C                   movel     srdto         scpsta
     C*
     C     *usa          move      scpsta        sys_date
     C     *cymd         move      sys_date      kkefdt
     C*
     C                   else
     C                   eval      kkefdt = 0
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkmed# = scmed#
     C                   eval      kkefdt = scefdt
     C                   endsr
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
     C     kvdo          begsr
     C*
     C                   exfmt     wdw01
     C*
     C                   if        *inkl = *off
     C                   move      ##co#         #1co
     C                   call      'F7078C'
     C                   parm                    #1co              3
     C                   parm      'QPRINT'      #1que            10
     C                   parm      'D'           #1mode            1
     C                   parm      @@ST          ##ST              2
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* Display state code and name in header
     C*
     C                   eval      kkkey = @@st
     C                   eval      scst  = @@st
     C     stky          chain     pst01                              99
     C                   eval      scstds = stname
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scmed# = ptmed#
     C*** BEG DEL *** 11/18/08 ****************************************
     C*********          eval      scname = ptname
     C*** END DEL *** 11/18/08 ****************************************
     C                   eval      scefdt = ptefdt
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   eval      scnpi# = ptnpi#
     C*** END ADD *** 11/18/08 ****************************************
     C     *ymd          move      scefdt        x1efdt
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   movel     v1data        scdata
     C                   movel     ptname        scname           30
     C                   move      scname        scdata
     C*** END ADD *** 11/18/08 ****************************************
     C                   eval      scstat = ptstat
     C*** BEG DEL *** 11/18/08 ****************************************
     C*********          eval      scdata = v1data
     C*** END DEl *** 11/18/08 ****************************************
     C                   endsr
    C*================================================================
    C* OPT20 - Subroutine for option 20
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval       ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C                   eval       ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C*
     C                   if        *inkv = *on
     C                   exsr      kvdo
     C                   endif
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
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   exsr      mksthd
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
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        ptmed# <> ssmed# and ssmed# <> ' '
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ptstat <> ssstat and ssstat <> ' '
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ptefdt <> s1efdt and s1efdt <> 0
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  Medicare#    Eff. Date  NPI#          Name                            Sts
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNN
217F17=Subset               YYY
221F21=Print list           YYN
3
