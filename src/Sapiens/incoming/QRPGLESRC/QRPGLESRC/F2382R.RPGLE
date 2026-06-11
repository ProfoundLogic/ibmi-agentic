     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H******************************************************************
     H/TITLE F2381R                                                    *
     H******************************************************************
     H*  Description:  Work With Payment Limit Buckets                 *
     H*                                                                *
     H*  Created: 12/14/11   Programmer C:277   Project:114MBB         *
     H******************************************************************
     H*  Revisions:                                                    *
     H*
     H*  02/04/13 C:280 Project: 114MBB
     H*                 Added to v13r2                                  when payment
     H*                                                                *
     H******************************************************************
    H*
     Ff2382f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffatfp     if   e             disk
     F                                     rename(ptf01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffatfl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(ptf01:dbrcdf)
     Ffmcml1    if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Payment Limit Buckets'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2381R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
     D Lo              c                   'abcdefghijklmnopqrstuvwxyz'
     D Hi              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D @@co#           s              3  0
     D ##fnd           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D @@kc            s              1
     D @@kl            s              1
     D ##mode          s              1
     D @@call          s              1
     D ##rrn           s              9  0
     D @@sub#          s                   like(tfsub#)
     D @@fnd           s                   like(tffnd )
     D @@fyr           s                   like(tffyr )
     D @@case          s                   like(tfcase)
     D kkco#           s                   like(cmco#)
     D kksub#          s                   like(tfsub#)
     D kkfnd           s                   like(tffnd )
     D kkfyr           s                   like(tffyr )
     D kkcase          s                   like(tfcase)
     D kkbgdt          s                   like(tfbgdt)
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
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    @@kc
     C     *inkl         parm                    @@kl
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
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     key1          klist
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkbgdt
     C*
     C     key2          klist
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     key3          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        ssefdt <> 0 or
     C                             ssstat <> ' '  or ssarea <> *blank
     C                   seton                                        11
     C                   endif
     C*
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
     C                   move      cmsub#        kksub#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
     C                   eval      kkbgdt = *hival
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   z-add     scbgdt        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         kkbgdt
     C
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scclnm = %char(tfsub#) + '-'+ %char(tffnd) +
     C                             '-' + %char(tffyr) + '-' + %char(tfcase)
     C*
     C                   movel     tfclm#        scclm#
     C*
     C                   z-add     cmacdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scacdt
     C                   eval      sclfpy = cmlfpy
     C*
     C                   z-add     tfbgdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scbgdt
     C*
     C                   z-add     tfendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scendt
     C*
     C                   z-add     tfpmnt        scpmnt
     C*
     C                   eval      scdata = %editw(scbgdt:'  /  /    ') +
     C                             '      ' + %editw(scendt:'  /  /    ')
     C                             + '         ' + %editw(scpmnt:'       .  ')
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
    C*
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C***                call      acdpgm        acdpl                    99
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C                   seton                                        LR
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   eval      kkco#  = 777
     C                   eval      kksub# = @@sub#
     C                   eval      kkfnd  = @@fnd
     C                   eval      kkfyr  = @@fyr
     C                   eval      kkcase = @@case
     C*
     C     key3          chain     pcm01                              99
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C     key2          reade     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C     key2          readpe    dbrcdf                                 95
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
     C*
     C                   eval      ##rrn = scdbrr
     C*
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     ChkStr        Begsr
     C                   endsr
     C*
     C*================================================================
** Column headings
     FY Begin        FY End            Amount Paid
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1055=Display                NNN
12020=Change status         NNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
