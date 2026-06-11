    H**********************************************************************
     H Copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F6300R
    H* DESCRIPTION: NCCI Data Call Submission
    H**********************************************************************
    H*  CREATED: 04/02/09  C:227  PROJECT: MC331
    H**********************************************************************
    H*  REVISIONS:
     H*
     H*  08/04/09  C:227  Project:MC331 Priss
     H*                   Compilation only
     H*
    H**********************************************************************
    H*                                                                    *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff6300f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffeu9p     if   e             disk
     F                                     rename(pu901:pfrcdf)
    F*  Primary logical file definition (for displaying items)
     Ffeu9l1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pu901:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(13) ctdata perrcd(1)
     D ofd             s             35    dim(13) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'NCCI Data Call Submissions'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F6300R01'
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
     D kkfile          s                   like(u9file)
     D kksbdt          s                   like(u9sbdt)
     D kksbtm          s                   like(u9sbtm)
     D kkftcd          s                   like(u9ftcd)
     D kkprfl          s                   like(u9prfl)
     D @@co#           s              3
     D @@mth           s              2
     D @@qtr           s              1
     D @@year          s              4
     D @@ftcd          s              1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      '*MDY'        srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
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
     C                   kfld                    kkfile
     C                   kfld                    kksbdt
     C                   kfld                    kksbtm
     C                   kfld                    kkftcd
     C     key2          klist
     C                   kfld                    kkfile
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C                   if        sssbdt <> 0 or
     C                             ssprfl <> *blank
     C                   seton                                        11
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   eval      *in71 = *off
     C                   if        scsel = '6' and *in90 = *off
     C                   eval      kkfile = scfile
     C     key2          chain(n)  dbrcdf
     C                   if        %found
     C                   if        u9prfl <> 'P'
     C                   seton                                        7190
     C                   leavesr
     C                   endif
     C                   endif
     C                   endif
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   eval      *in57 = *off
     C*
     C                   if        sssbdt <> *zeros
     C                   z-add     sssbdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endif
     C*
     C                   if        ssprfl <> ' ' and ssprfl <> 'P'
     C                             and ssprfl <> 'D' and ssprfl <> 'F'
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
     C                   eval      kkfile = *loval
     C                   eval      kksbdt = *loval
     C                   eval      kksbtm = *loval
     C                   eval      kkftcd = *loval
     C                   eval      kkprfl = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              movel(p)  scpst1        kkfile
     C                   if        *in21 = *on
     C                   z-add     scpstd        srdti
     C                   call      'SRCVTDAT'    dtmdy
     C                   if        srerr = '0'
     C                   movel     srdto         kksbdt
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   movel(p)  scfile        kkfile
     C                   z-add     scsbdt        srdti
     C                   call      'SRCVTDAT'    dtmdy
     C*
     C                   if        srerr = '0'
     C                   movel     srdto         kksbdt
     C                   endif
     C*
     C                   move      scsbtm        kksbtm
     C                   movel(p)  scftcd        kkftcd
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   movel     u9file        scfile
     C                   z-add     u9sbdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         scsbdt
     C                   move      u9sbtm        scsbtm
     C                   movel     u9ftcd        scftcd
     C                   movel     u9prfl        scprfl
     C*
     C                   eval      scdata = scfile + ' ' +
     C                             %editw(scsbdt:'  /  /    ') + '     ' +
     C                             scftcd + '            ' +
     C                             scprfl
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
     C                   select
     C                   when      scsel = '6'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl
     C                   when      scsel = '7'
     C                   eval      ##mode = 'X'
     C                   call      acdpgm        acdpl
     C                   when      scsel = '8'
     C                   eval      ##mode = 'F'
     C                   call      acdpgm        acdpl
     C                   endsl
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
     C                   call      'SA001R'
     C                   parm                    ##wcif            1
     C                   parm                    ##fbif            1
     C                   parm                    ##fl01            1
     C                   parm      *blank        ##ar              1
     C                   parm      *blank        ##ntwk            1
     C                   parm      *blank        ##ur              1
     C                   parm      *blank        ##med             1
     C                   parm      *blank        ##edi             1
     C                   parm      *blank        ##wcdk            1
     C                   parm      *blank        ##rhab            1
     C                   parm      *blank        ##mss             1
     C                   parm      *blank        ##fsch            1
     C                   parm      *blank        ##prvb            1
     C                   parm      *blank        ##npwc            1
     C                   parm      *blank        ##os              1
     C                   parm      *blank        ##arls            1
     C                   parm      *blank        ##ccod            1
     C                   parm      *blank        ##crvw            1
     C                   parm      *blank        ##prom            1
     C                   parm      *blank        ##mcrv            1
     C                   parm      *blank        ##freq            1
     C                   parm      *blank        ##crac            1
     C                   parm      *blank        ##preo            1
     C                   parm      *blank        ##parm            1
     C                   parm      *blank        ##mgri            1
     C                   parm      *blank        ##kcif            1
     C                   parm      *blank        ##curs            1
     C                   parm      *blank        ##pcib            1
     C                   parm      *blank        ##clt#            1
     C                   parm      *blank        ##ucpf            1
     C                   parm      *blank        ##dcf             1
     C                   parm      *blank        ##autb            1
     C                   parm      *blank        ##deif            1
     C                   parm      *blank        ##aure            1
     C                   parm      *blank        ##cred            1
     C                   parm      *blank        ##idrt            1
     C                   parm      *blank        ##cprv            1
     C                   parm      *blank        ##eft             1
     C                   parm      *blank        ##govr            1
     C                   parm      *blank        ##ncdc            1
     C
     C                   if        ##ncdc <> 'Y'
     C                   exfmt     wdw01
     C                   return
     C                   endif
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
     C                   z-add     sssbdt        srdti
     C                   call      'SRCVTDAT'    dtmdy
     C                   if        srerr = '0'
     C                   movel     srdto         xxsbdt            8 0
     C                   else
     C                   call      'SRCVTDAT'    dtmdy1
     C                   if        srerr = '0'
     C                   movel     srdto         xxsbdt            8 0
     C                   endif
     C                   endif
     C*
     C                   if        sssbdt <> 0 and xxsbdt <> u9sbdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssprfl <> ' ' and ssprfl <> u9prfl
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  File Name                      Submission Dt  File Type Cd  Process Sts
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Replace                YNN
1077=Delete                 YNN
1088=File Failure Delete    YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Original Submission   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
