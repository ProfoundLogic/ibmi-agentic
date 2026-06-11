    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE F8285R
     H**************************************************************************
    H*  F8285R - First Health EOR Financial Work with Transfer History
    H*
    H*  Called from: Menu
    H*  Calls to   : F8285R01 - First Health EOR Financial Work with Transfer H
    H*                          Update/Display
    H*                                                               *
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8265R from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff8285f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffax9p     if   e             disk
     F                                     rename(px901:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffax9l1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(px901:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*
     D******/COPY SCOPYWW01E
    D*
    D*
     D******/COPY SCOPYWW01I
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Bill Transfer Histor-
     D                                     y')
     D slmsgi          c                   const('MC49998')
     D slmsgf          c                   const('FSMESG')
     D acdpgm          c                   const('F8285R01')
     D comapi          c                   const('FA007R')
     D uspapi          c                   const('F0006R')
     D infomx          c                   const(1)
     D viewmx          c                   const(1)
     D sf1pag          c                   const(11)
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
     D kkdcid          s                   like(x9dcid)
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
    C*
     C     key1          klist
     C                   kfld                    kkdcid
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
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
    C*
     C                   clear                   kkdcid
    C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
    C*
     C                   movel(p)  scpst1        kkdcid
    C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
    C*
     C                   movel(p)  scdcid        kkdcid
    C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   movel(p)  x9dcid        scdcid
     C                   movel(p)  x9mbr#        scmbr#
    C*
     C                   eval      srdti = x9cvdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sccvdt
    C*
     C                   eval      sccvtm = x9cvtm
    C*
     C                   eval      srdti = x9prdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprdt
    C*
     C                   eval      scprtm = x9prtm
    C*
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
     C                   endsr
** Column headings
Opt Document ID    Member #      Date       Time       Date       Time
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               NNN
3
