     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/TITLE M0021R - Work with Environment Library List
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  12/01/99  C:69  Support:                                     *
    H*                  Changed program to use SA005R, S0006R APIs   *
    H*                                                               *
    H*****************************************************************
     Fs0010f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fsdclp     if   e             disk
     F                                     rename(pcl01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fsdcll     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pcl01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(9) ctdata perrcd(1)
     D ofd             s             35    dim(9) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('PDS/Client Log-in')
     D slmsgi          c                   const('SYS0001')
     D slmsgf          c                   const('SYMESG')
     D acdpgm          c                   const('S0010R1')
     D comapi          c                   const('SA005R')
     D uspapi          c                   const('S0006R')
     D infomx          c                   const(1)
     D viewmx          c                   const(1)
     D sf1pag          c                   const(12)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D date8           s              8  0
     D kkidt           s                   like(clidt)
     D kkitim          s                   like(clitim)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     D/copy sprdate
     D/copy sprstring
     D/copy sprmessage
     D/copy sprruncmd
     D/copy sprwrkwth
     DgetQrySel        PR           512
     D
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
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkidt
     C                   kfld                    kkitim
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCKC - Additional errors on subfile control
    C*================================================================
     C     errckc        begsr
     C                   if        scpst1 <> *blanks
     C                   move      scpst1        date8
     C                   eval      date8 = dtCheck(date8)
     C                   move      date8         scpst1
     C                   if        date8 = *zero
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   endif
     C                   endif
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
     C                   if        ssmode <> *blanks
     C                             and ssmode <> '1'
     C                             and ssmode <> '2'
     C                   eval      *in89 = *on
     C                   eval      *in90 = *on
     C                   endif
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
     C                   movel     *hival        kkidt
     C                   movel     *loval        kkitim
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move      scpst1        date8
     C                   eval      kkidt = dtCYMD(date8)
     C                   movel     scpsta        kkitim
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkidt = dtCYMD(scidt)
     C                   movel     scitim        kkitim
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      scidt = dtMDYY(clidt)
     C                   movel     clitim        scitim
     C                   movel     clrsys        scrsys
     C                   select
     C                   when      clmode = '1'
     C                   movel     'Start'       scmodd
     C                   when      clmode = '2'
     C                   movel(p)  'End'         scmodd
     C                   endsl
     C                   movel     cliusr        sciusr
     C                   movel     clrmk         scrmk
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
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   move      *on           *in12
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
     C                   if        ssrsys <> *blanks
     C                             and ssrsys <> clrsys
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssiusr <> *blanks
     C                             and ssiusr <> cliusr
     C                   move      *off          meetss
     C                   endif
    C*
     C                   if        ssmode <> *blanks
     C                             and ssmode <> clmode
     C                   move      *off          meetss
     C                   endif
     C                   endsr
** Column headings
Opt     Date       Time    System      Mode   User ID
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
215F15=Fold/truncate        YYY
217F17=Subset               YYY
3
