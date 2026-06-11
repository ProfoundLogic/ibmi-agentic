    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE Work with Agent Commission Processing History
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: MENU                                                *
    H*                                                                   *
    H*  CALLS TO:  W0685R02                                              *
    H*                                                                   *
    H*  CREATED: 12/20/06      C:183     PROJECT: 114WND                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
    H*********************************************************************
      *
     *********************************************************************
    *  File Specification                                               *
     *********************************************************************
      *
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0685f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmphp     if   e             disk
     F                                     rename(pph01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmphl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pph01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fwmphl1    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pph01:dbrcdf2)
    D*
     D hd              s             74    dim(2) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Agt Commission Processing Hist'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0685R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   2
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
     D kkco#           s                   like(phco#)
     D kkfnd           s                   like(phfnd)
     D kkdt            s                   like(phdt)
     D kktime          s                   like(phtime)
     D kkuser          s                   like(phuser)
     D kkproc          s                   like(phtype)
     D @kkdt           s              8  0
     D @scdate         s              8  0
     D@@scdate         s               D   DATFMT(*USA)
     D @sctime         s              8
     D @scuser         s                   like(phuser)
     D @proctyp        s             27
     D @scgrp          s              3
      *
     *********************************************************************
    *  Copybooks for Prototypes                                         *
     *********************************************************************
      *
     D/COPY SPRDATE
     D/COPY SPRSTRING
     D/COPY SPRWRKWTH
     D/COPY SPRMESSAGE
     D/COPY SPRRUNCMD
      *
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW41C
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
     C                   kfld                    kkdt
     C                   kfld                    kktime
     C                   kfld                    kkfnd
     C                   kfld                    kkuser
     C                   kfld                    kkproc
    C*================================================================
     C     key1a         klist
     C                   kfld                    kkco#
    C*================================================================
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkdt
     C                   kfld                    kktime
     C                   kfld                    kkuser
     C                   kfld                    kkproc
    C*================================================================
     C     key2a         klist
     C                   kfld                    kkco#
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
     C                   eval      kkco# = *loval
     C                   eval      kkdt = *hival
     C                   eval      kktime = *hival
     C                   eval      kkfnd = *loval
     C                   eval      kkuser = *loval
     C                   eval      kkproc = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   eval      kkco# = scco#
     C                   select
     C                   when      *in21 = *on
     C                   move      scpst1        @kkdt
     C                   eval      kkdt = dtcymd(@kkdt)
     C                   move      scpst2        kktime
     C                   if        kktime = 0
     C                   eval      kktime = *hival
     C                   endif
     C                   eval      kkfnd = *loval
     C                   when      *in22 = *on
     C                   move      scpst4        @kkdt
     C                   eval      kkdt = dtcymd(@kkdt)
     C                   move      scpst3        kkfnd
     C                   eval      kktime = *hival
     C                   endsl
     C                   if        kkdt = 0
     C                   eval      kkdt = *hival
     C                   endif
     C                   eval      kkuser = *loval
     C                   eval      kkproc = *loval
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkco# = scco#
     C                   eval      kkfnd = scfnd
     C                   eval      kkdt = scrndt
     C                   eval      kktime = scrntm
     C                   eval      kkuser = scrusr
     C                   eval      kkproc = scrpro
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scco# = phco#
     C                   eval      scfnd = phfnd
     C                   eval      scrndt = phdt
     C                   eval      scrntm = phtime
     C                   eval      scrusr = phuser
     C                   eval      scrpro = phtype
     C                   eval      @scdate = dtmdyy(phdt)
     C                   move      @scdate       @@scdate
     C                   eval      @sctime = %editw(phtime:'  :  :  ')
     C                   if        %subst(@sctime:1:1) = ' '
     C                   eval      %subst(@sctime:1:1) = '0'
     C                   endif
     C                   eval      @scuser = phuser
     C                   select
     C                   when      phtype = 'E'
     C                   eval      @proctyp = 'Commission Earnings        '
     C                   when      phtype = 'R'
     C                   eval      @proctyp = 'Commission Checks Requested'
     C                   when      phtype = 'P'
     C                   eval      @proctyp = 'Commission Payments        '
     C                   when      phtype = 'A'
     C                   eval      @proctyp = 'Commission Adjustments     '
     C                   endsl
     C                   eval      @scgrp = %editc(phfnd:'X')
     C                   select
     C                   when      view# = 1
     C                   eval      scdata = %char(@@scdate) + '  ' +
     C                             @sctime + '  ' + @scuser + '  ' +
     C                             @proctyp + '  ' + @scgrp
     C                   when      view# = 2
     C                   eval      scdata = @scgrp + '  ' +
     C                             %char(@@scdate) + '  ' +
     C                             @sctime + '  ' + @scuser + '  ' +
     C                             @proctyp
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
     C                   select
     C                   when      *in21 = *on
     C                   read      dbrcdf                                 95
     C                   when      *in22 = *on
     C                   read      dbrcdf2                                95
     C                   endsl
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   select
     C                   when      *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   when      *in22 = *on
     C                   readp     dbrcdf2                                95
     C                   endsl
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   select
     C                   when      *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   when      *in22 = *on
     C     key2          setgt     dbrcdf2                            96
     C                   endsl
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   select
     C                   when      *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   when      *in22 = *on
     C     key2          setll     dbrcdf2                            96
     C                   endsl
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
     C                   endsr
** Column headings
Opt Run date    Time      User        Process type                 Grp
Opt Grp  Run date    Time      User        Process type
** Description--------------MISN
1011=Select                 NNY
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
212F12=Cancel               NNY
211F11=Change View          YYY
3
