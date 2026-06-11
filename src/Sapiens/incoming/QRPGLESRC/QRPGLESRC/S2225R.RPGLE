     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE S2225R - Work with Contacts
     H*******************************************************************
     H* Created on 02/01/2010 by c:62 for Project 25WQK                 *
     H*******************************************************************
     H* Revisions:                                                      *
     H*                                                                 *
     H*******************************************************************
     Fs2225f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fsdcnp     if   e             disk
     F                                     rename(pcn01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fsdcnl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pcn01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fsdcnl2    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pcn01:dbrcd2)
     Fsmchp     if   e             disk
     Fsms2p     if   e             disk
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Contacts'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'S2225R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
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
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D ##call          s              1
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@rrn3          s              9  0
     D xxfnd           s              3
     D kkco#           s                   like(cnco#)
     D kktype          s                   like(cntype)
     D kkseqh          s                   like(cnseqh)
     D kkseqs          s                   like(cnseqs)
     D kkfnd           s                   like(cnfnd)
     D kkname          s                   like(cnname)
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
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C                   parm                    @@rrn3
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn2        ##rrn2
     C                   parm      @@rrn3        ##rrn3
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C                   kfld                    kkseqs
     C*
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C                   kfld                    kkseqs
     C                   kfld                    kkfnd
     C*
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kktype
     C                   kfld                    kkseqh
     C                   kfld                    kkseqs
     C                   kfld                    kkname
     C                   kfld                    kkfnd
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
     C*
     C     @@rrn3        chain     ps201
     C*
     C                   eval      kkco#=s2co#
     C                   eval      kktype=s2type
     C                   eval      kkseqh=s2seqh
     C                   eval      kkseqs=s2seqs
     C                   eval      kkfnd=0
     C                   eval      kkname=*loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              move      scpst1        kkfnd
     C   22              move      scpst2        kkname
     C   22              move      *zeros        kkfnd
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkfnd=scfnd
     C                   eval      kkname=scname
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scfnd=cnfnd
     C                   eval      scname=cnname
     C*
     C                   movel(p)  cnfnd         xxnum
     C*
     C                   call      'SA020R'
     C                   parm                    xxnum            80
     C                   parm      '4'           edtmsk           10
     C*
     C                   eval      xxfnd=%trim(xxnum)
     C*
     C                   select
     C                   when      *in21
     C                   movel(p)  xxfnd         scdata
     C                   move      cnname        scdata
     C                   when      *in22
     C                   movel(p)  cnname        scdata
     C                   move      xxfnd         scdata
     C                   endsl
     C*
     C                   eval      scstat=cnstat
     C*
     C                   endsr
    C*================================================================
    C* OPT20 - Change Status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode='S'
     C                   call      acdpgm        acdpl                  99
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
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     c*
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
     C*
     C     @@rrn2        chain     pch01
     c*
     C                   movel(p)  chhdg1        scmhdg
     C                   movel(p)  s2hdg1        scshdg
     C                   if        scshdg=*blanks
     C                   eval      scshdg='* * Blank line * *'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C   21key           reade     dbrcdf                                 95
     C   22key           reade     dbrcd2                                 95
     C*
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C   21key           readpe    dbrcdf                                 95
     C   22key           readpe    dbrcd2                                 95
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C   21key1          setgt     dbrcdf                             96
     C   22key2          setgt     dbrcd2                             96
     C*
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C   21key1          setll     dbrcdf                             96
     C   22key2          setll     dbrcd2                             96
     C*
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
Opt  Grp  Name                                                         Sts
Opt  Name                                                         Grp  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Chg View             YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
