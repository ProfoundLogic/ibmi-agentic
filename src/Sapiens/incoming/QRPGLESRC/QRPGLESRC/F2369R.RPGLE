     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2369R
    H* DESCRIPTION: Work with Medicare Ambulance Rates
    H* CALLED FROM: F0601R
    H* CALLS TO   : F2369R01
    H**********************************************************************
    H*  CREATED: 08/12/10  C:251  PROJECT: 114MWS
    H**********************************************************************
    H*  REVISIONS:
     H*                                                                                    *
     H*  10/11/10  C:280  Project: seq78229                                                *
     H*                   Added option 20 for status change                                *
     H*                                                                                    *
    H**********************************************************************
    H*  INDICATORS
     H*
    H**********************************************************************
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2369f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Ffmamp     if   e             disk
     F                                     rename(pam01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Ffmaml1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pam01:dbrcdf)
     F*
     Fststl     if   e           k disk
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D*** BEG ADD *** 10/11/10 ****************************************
     D*of              s              1    dim(10) ctdata perrcd(1)
     D*ofd             s             35    dim(10) alt(of)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     D*** END ADD *** 10/11/10 ****************************************
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Ambulance Rates'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2369R01'
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
     D x1data          s             30
     D x2data          s             40
     D xxdt            s               d   datfmt(*usa)
     D xxdt1           s             10a
     D kkkey           s                   like(stkey)
     D*
     D kkst            s                   like(amst)
     D kkloc           s                   like(amloc)
     D kkhcpc          s                   like(amhcpc)
     D kkefdt          s                   like(amefdt)
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
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@rrn             9 0
    C*
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
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkhcpc
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
     C*
     C                   if        ssst <> *blanks or ssefdt <> 0 or
     C                             ssstat <> ' '
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
     C*
     C                   eval      *in57 = *off
     C                   eval      *in58 = *off
     C*
     C                   if        ssefdt <> *zeros
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssefdt
     C*
     C                   if        srerr = '1'
     C                   eval      *in57 = *on
     C                   eval      *in90 = *on
     C*
     C                   endif
     C                   endif
     C*
     C                   if        ssstat <> ' '
     C                             and ssstat <> 'A' and ssstat <> 'I'
     C                   eval      *in58 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* ERRCKP - Error checking
    C*================================================================
     C     errckp        begsr
     C*
     C                   eval      *IN90 = *OFF
     C                   eval      *IN60 = *OFF
     C                   eval      *IN61 = *OFF
     C*
     C                   if        slst <> *blanks
     C                   eval      kkkey = slst
     C     stky          chain     pst01                              99
     C                   if        not %found
     C                   seton                                        6090
     C                   endif
     C                   endif
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         slefdt
     C                   if        srerr = '1'
     C                   seton                                        6190
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C                   if        @@call = '2'
     C                   eval      ssstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkst = *loval
     C                   eval      kkloc = *loval
     C                   eval      kkhcpc = *loval
     C                   eval      kkefdt = *loval
     C*
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C   21              eval      kkst   = scpst1
     C   21              eval      kkloc  = scploc
     C   21              eval      kkhcpc = scphcpc
     C*
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkst   = scst
     C                   eval      kkloc  = scloc
     C                   eval      kkhcpc = schcpc
     C                   eval      kkefdt = scefdt
     C*
     C                   endsr
    C*================================================================
    C* KVDO - F21=Print list
    C*================================================================
     C     kvdo          begsr
     C*
     C                   move      *blanks       slst
     C                   move      *zeros        slefdt
     C*
     C                   dou       *in90 = *off or *inkl = *on
     C*
     C                   exfmt     wdw01
     C*
     C                   if        *inkl = *off
     C*
     C                   exsr      errckp
     C*
     c                   if        *in90 = *off
     C*
     C                   if        slefdt <> 0
     C                   z-add     slefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ##efdt            7
     C                   else
     C                   movel     *blanks       ##efdt
     C                   endif
     C*
     C                   move      ##co#         #1co
     C*
     C                   call      'F7105C'
     C                   parm                    #1co              3
     C                   parm      'QPRINT'      #1que            10
     C                   parm      'D'           #1mode            1
     C                   parm      SLST          ##ST              2
     C                   parm                    ##EFDT            7
     C*
     C                   endif
     C*
     C                   endif
     C                   enddo
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scst   = amst
     C                   eval      scloc  = amloc
     C                   eval      schcpc = amhcpc
     C                   eval      scefdt = amefdt
     c*
     C     *cymd         move      amefdt        xxdt
     C                   movel     xxdt          xxdt1
     c*
     C                   eval      scdata = scst  + '           '
     C                             + %editc(scloc:'X') + '  '
     C                             + schcpc  + '  '
     C                             + xxdt1  + '  '
     C                             + amdesc
     C*
     C                   eval      scstat = amstat
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkc         caseq     *on           kcdo
     C                   endcs
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
    C*
    C*** BEG ADD *** 10/11/11 ****************************************
    C*
     C                   If        Scsel = '20'
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99
     C                   endif
    C*
    C*** END ADD *** 10/11/11 ****************************************
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
     C                   eval      @@rrn = scdbrr
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        ssst <> *blanks and ssst <> amst
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   z-add     ssefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         xxefdt            7 0
     C                   endif
     C*
     C                   if        ssefdt <> *zeros and xxefdt <> amefdt
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   if        ssstat <> ' ' and ssstat <> amstat
     C                   eval      meetss = *off
     C                   endif
     C*
     C                   endsr
** Column headings
Opt  State  Locality  HCPCS   Eff. Date  Description                     Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
12020=Change status         YNN
221F21=Print list           YYN
3
