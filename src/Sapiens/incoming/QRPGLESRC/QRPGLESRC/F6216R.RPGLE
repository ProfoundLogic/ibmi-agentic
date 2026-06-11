     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6216R - 997 Error codes
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*     THIS PROGRAM SHOWS ERROR CODES                            *
     H*                                                               *
     H*  CALLED BY: F6213R                                            *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*****************************************************************
     H*  PROGRAMMER: 296                                              *
     H*  CREATED:   05/19/14           PROJECT: MC376                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*
     H
     Ff6216f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffeae997p  if   e             disk
     F                                     rename(p99701:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffeae997l1 if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(p99701:dbrcdf)
     Ffemfp     if   e             disk
    D*
      /COPY SCOPYWW01D
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   '997 Ack Errors'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F6216R01'
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
     D kk997tstid      s                   like(ae997tstid)
     D @@RRN           s              5  0
     D @@997SgId       s                   like(ae997SgId)
     D @@997ErCd       s                   like(ae997ErCd)
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
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@RRN
     C                   parm                    @@997SgId
     C                   parm                    @@997ErCd
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
     C                   kfld                    kk997tstid
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
     C                   eval      kk997Tstid = *blanks
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   move(P)   scPst1        kk997tstid
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   move(P)   sc997tstId    kk997tstid
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   move(P)   AE997TstId    sc997TstId
     C                   z-add     AE997PrDt     srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         sc997PrDt
     C                   movel     AE997ErCd     sc997ErCd
     C                   movel     'Error'       sc997EDsc
     C                   Movel     AE997Stat     scStat
     C*
     C                   eval      scdata = sc997TstId + '     ' +
     C                                     %editw(sc997PrDt:'  /  /    ')
     C                             + '      '  + sc997ErCd
     C                             + ' '   + sc997edsc
     C                             + '         '   + scstat
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
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *InLr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C     @@RRN         chain     femfp                              99
     C*
     C                   if        *in99 = *off
     C                   eval      kk997TstId = MFBch#
     C                   Endif
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key1          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key1          readpe    dbrcdf                                 95
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
Opt  Trans Set ID  Process Dt Err Code Description                       Sts
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   NNN
212F12=Cancel               YYN
217F17=Subset               NNN
3
