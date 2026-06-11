     H*  (C) Copyright Fiserv, Inc., 2007
    H*
     H/TITLE W0067R - Work With Claim User Security
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Work With Claim User Security           *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W0067R02                                            *
     H*                                                               *
     H* CREATED: 05/25/07  C:191 PROJECT WC1515                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0067f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmcup     if   e             disk
     F                                     rename(pcu01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmcul     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pcu01:dbrcdf)
    F*  logical file definition (for change view)
     Fwmcul1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pcu01:dbrcdf1)
    F*  logical file definition (for change view)
     Fwmcul2    if   e           k disk
     F                                     infds(dbfds3)
     F                                     rename(pcu01:dbrcdf2)
    F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    D*
     D hd              s             78    dim(3) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Claims User Security'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0067R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   3
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
     D xxco#           s                   like(cuco#)
     D kkco#           s                   like(cuco#)
     D kkfnd           s                   like(cufnd)
     D kkfyr           s                   like(cufyr)
     D kkcase          s                   like(cucase)
     D kkausr          s                   like(cuausr)
     D kkcnam          s                   like(cucnam)
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkausr
    C*
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcnam
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkausr
    C*
     C     key3          klist
     C                   kfld                    kkco#
     C                   kfld                    kkausr
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
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
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkco# = xxco#
     C                   eval      kkfnd = *loval
     C                   eval      kkfyr = *loval
     C                   eval      kkcase = *loval
     C                   eval      kkausr = *loval
     C                   eval      kkcnam = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   eval      kkco# = xxco#
     C                   move      scpst1        kkfnd
     C                   eval      kkfyr = scpst1a
     C                   eval      kkcase = scpst1b
     C                   eval      kkcnam = scpst2
     C                   eval      kkausr = scpst3
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkco# = scco#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
     C                   eval      kkausr = scausr
     C                   eval      kkcnam = sccnam
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scco# = cuco#
     C                   eval      scfnd = cufnd
     C                   eval      scfyr = cufyr
     C                   eval      sccase = cucase
     C                   eval      scausr = cuausr
     C                   eval      sccnam = cucnam
     C                   eval      scstat = custat
     C*
     C                   select
     C                   when      *in21 = *on
     C                   eval      scdata = ' ' + %editc(scfnd:'2') + '   '
     C                                    + %editc(scfyr:'2') + '  '
     C                                    + %editc(sccase:'2') + '   '
     C                                    + sccnam + '  '
     C                                    + scausr + '    '
     C                                    + scstat
     C                   when      *in22 = *on
     C                   eval      scdata = ' ' + sccnam + '  '
     C                                    + %editc(scfnd:'2') + '   '
     C                                    + %editc(scfyr:'2') + '  '
     C                                    + %editc(sccase:'2') + '   '
     C                                    + scausr + '    '
     C                                    + scstat
     C                   when      *in23 = *on
     C                   eval      scdata = ' ' + scausr + '   '
     C                                    + %editc(scfnd:'2') + '   '
     C                                    + %editc(scfyr:'2') + '  '
     C                                    + %editc(sccase:'2') + '   '
     C                                    + sccnam + '   '
     C                                    + scstat
     C                   endsl
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
     C                   select
     C                   when      scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl
     C                   endsl
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
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      xxco# = upco#
     C                   endif                                                  %found
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C                   read      dbrcdf1                                95
     C                   endif
     C                   if        *in23 = *on
     C                   read      dbrcdf2                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif
     C                   if        *in22 = *on
     C                   readp     dbrcdf1                                95
     C                   endif
     C                   if        *in23 = *on
     C                   readp     dbrcdf2                                95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setgt     dbrcdf1                            96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setgt     dbrcdf2                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C                   if        *in22 = *on
     C     key2          setll     dbrcdf1                            96
     C                   endif
     C                   if        *in23 = *on
     C     key3          setll     dbrcdf2                            96
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '   *Deleted   '
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C***********        eval      @rrn = scdbrr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt   Grp   Year   Claim #   Claimant Name              User ID      Sts
Opt   Claimant Name              Grp   Year   Claim #   User ID      Sts
Opt   User ID      Grp   Year   Claim #   Claimant Name              Sts
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNN
217F17=Subset               NNN
3
